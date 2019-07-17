**********************************************************************
*
* Local class for handling messages of bookings
*
**********************************************************************
CLASS lcl_message_helper DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES tt_booking_failed   TYPE TABLE FOR FAILED   /dmo/i_booking_u.
    TYPES tt_booking_reported TYPE TABLE FOR REPORTED /dmo/i_booking_u.

    CLASS-METHODS handle_booking_messages
      IMPORTING iv_cid        TYPE string OPTIONAL
                iv_travel_id  TYPE /dmo/travel_id OPTIONAL
                iv_booking_id TYPE /dmo/booking_id OPTIONAL
                it_messages   TYPE /dmo/if_flight_legacy=>tt_message
      CHANGING
                failed   TYPE tt_booking_failed
                reported TYPE tt_booking_reported.

ENDCLASS.

CLASS lcl_message_helper IMPLEMENTATION.

  METHOD handle_booking_messages.

    LOOP AT it_messages INTO DATA(ls_message) WHERE msgty = 'E' OR msgty = 'A'.
      INSERT VALUE #( %cid      = iv_cid
                      travelid  = iv_travel_id
                      bookingid = iv_booking_id ) INTO TABLE failed.

      INSERT /dmo/cl_travel_auxiliary=>map_booking_message(
                                          iv_travel_id  = iv_travel_id
                                          iv_booking_id = iv_booking_id
                                          is_message    = ls_message ) INTO TABLE reported.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


**********************************************************************
*
* Handler class for managing bookings
*
**********************************************************************
CLASS lhc_booking DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    TYPES:
        tt_booking_update           TYPE TABLE FOR UPDATE  /dmo/i_booking_u,
        tt_bookingsupplement_create TYPE TABLE FOR CREATE  /dmo/i_bookingsupplement_u.

    METHODS:
        update_booking FOR MODIFY
                                IMPORTING   it_booking_update       FOR UPDATE booking,
        delete_booking FOR MODIFY
                                IMPORTING   it_booking_delete       FOR DELETE booking,
        cba_supplement FOR MODIFY
                                IMPORTING   it_supplement_create_ba FOR CREATE booking\_booksupplement.

    METHODS:
        _fill_booking_inx
                IMPORTING is_booking_update     TYPE LINE OF tt_booking_update
                RETURNING VALUE(rs_booking_inx) TYPE /dmo/if_flight_legacy=>ts_booking_inx,
        _fill_bookingsupplement_inx
                IMPORTING is_bookingsupplement_create     TYPE LINE OF tt_bookingsupplement_create
                RETURNING VALUE(rs_bookingsupplement_inx) TYPE /dmo/if_flight_legacy=>ts_booking_supplement_inx.

ENDCLASS.


CLASS lhc_booking IMPLEMENTATION.

**********************************************************************
*
* Implements the UPDATE operation for a set of booking instances
*
**********************************************************************
  METHOD update_booking.

    DATA lt_messages TYPE /dmo/if_flight_legacy=>tt_message.

    LOOP AT it_booking_update ASSIGNING FIELD-SYMBOL(<fs_booking_update>).
      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = VALUE /dmo/if_flight_legacy=>ts_travel_in( travel_id = <fs_booking_update>-travelid )
          is_travelx  = VALUE /dmo/if_flight_legacy=>ts_travel_inx( travel_id = <fs_booking_update>-travelid )
          it_booking  = VALUE /dmo/if_flight_legacy=>tt_booking_in( ( /dmo/cl_travel_auxiliary=>map_booking_cds_to_db( CORRESPONDING #( <fs_booking_update> ) ) ) )
          it_bookingx = VALUE /dmo/if_flight_legacy=>tt_booking_inx( ( _fill_booking_inx( <fs_booking_update> ) ) )
        IMPORTING
          et_messages = lt_messages.


      lcl_message_helper=>handle_booking_messages(
        EXPORTING
          iv_cid        = <fs_booking_update>-%cid_ref
          iv_travel_id  = <fs_booking_update>-travelid
          iv_booking_id = <fs_booking_update>-bookingid
          it_messages   = lt_messages
        CHANGING
          failed   = failed-booking
          reported = reported-booking ).

    ENDLOOP.

  ENDMETHOD.

**********************************************************************
* Helper method:
* Indicates the booking fields that have been changed by the client
*
**********************************************************************
  METHOD _fill_booking_inx.

    CLEAR rs_booking_inx.
    rs_booking_inx-booking_id    = is_booking_update-bookingid.
    rs_booking_inx-action_code   = /dmo/if_flight_legacy=>action_code-update.

    rs_booking_inx-booking_date  = xsdbool( is_booking_update-%control-bookingdate  = if_abap_behv=>mk-on ).
    rs_booking_inx-customer_id   = xsdbool( is_booking_update-%control-customerid   = if_abap_behv=>mk-on ).
    rs_booking_inx-carrier_id    = xsdbool( is_booking_update-%control-airlineid    = if_abap_behv=>mk-on ).
    rs_booking_inx-connection_id = xsdbool( is_booking_update-%control-connectionid = if_abap_behv=>mk-on ).
    rs_booking_inx-flight_date   = xsdbool( is_booking_update-%control-flightdate   = if_abap_behv=>mk-on ).
    rs_booking_inx-flight_price  = xsdbool( is_booking_update-%control-flightprice  = if_abap_behv=>mk-on ).
    rs_booking_inx-currency_code = xsdbool( is_booking_update-%control-currencycode = if_abap_behv=>mk-on ).
  ENDMETHOD.


**********************************************************************
*
* Implements the DELETE operation for a set of booking instances
*
**********************************************************************
  METHOD delete_booking.

    DATA lt_messages TYPE /dmo/if_flight_legacy=>tt_message.

    LOOP AT it_booking_delete INTO DATA(ls_booking_delete).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = VALUE /dmo/if_flight_legacy=>ts_travel_in( travel_id = ls_booking_delete-travelid )
          is_travelx  = VALUE /dmo/if_flight_legacy=>ts_travel_inx( travel_id = ls_booking_delete-travelid )
          it_booking  = VALUE /dmo/if_flight_legacy=>tt_booking_in( ( booking_id = ls_booking_delete-bookingid ) )
          it_bookingx = VALUE /dmo/if_flight_legacy=>tt_booking_inx( ( booking_id  = ls_booking_delete-bookingid
                                                                       action_code = /dmo/if_flight_legacy=>action_code-delete ) )
        IMPORTING
          et_messages = lt_messages.

      IF lt_messages IS NOT INITIAL.

        lcl_message_helper=>handle_booking_messages(
         EXPORTING
           iv_cid        = ls_booking_delete-%cid_ref
           iv_travel_id  = ls_booking_delete-travelid
           iv_booking_id = ls_booking_delete-bookingid
           it_messages   = lt_messages
         CHANGING
           failed   = failed-booking
           reported = reported-booking ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

***********************************************************************
*
* Create associated booking supplements
*
***********************************************************************
  METHOD cba_supplement.

    DATA lt_messages               TYPE /dmo/if_flight_legacy=>tt_message.
    DATA lt_booksupplement_old     TYPE /dmo/if_flight_legacy=>tt_booking_supplement.
    DATA ls_booksupplement         TYPE LINE OF /dmo/if_flight_legacy=>tt_booking_supplement_in.
    DATA lv_last_booksupplement_id TYPE /dmo/booking_supplement_id.

    " Loop at parent - booking
    LOOP AT it_supplement_create_ba ASSIGNING FIELD-SYMBOL(<fs_supplement_create_ba>).
      DATA(ls_parent_key) = <fs_supplement_create_ba>-%key.

      " Retrieve booking supplements related to the imported travel ID
      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id          = ls_parent_key-travelid
        IMPORTING
          et_booking_supplement = lt_booksupplement_old
          et_messages           = lt_messages.

      IF lt_messages IS INITIAL.

        " Look up for maximum booking supplement ID for a given travel/booking
        " lt_booksupplement_old provides sorted values, therefore the last value is maximum value
        lv_last_booksupplement_id = REDUCE #( INIT res = 0
                                              FOR old IN lt_booksupplement_old
                                                USING KEY primary_key
                                                WHERE ( travel_id  = ls_parent_key-travelid
                                                    AND booking_id = ls_parent_key-bookingid )
                                              NEXT res = old-booking_supplement_id ).

        LOOP AT <fs_supplement_create_ba>-%target INTO DATA(ls_supplement_create).
          ls_supplement_create-bookingid = ls_parent_key-bookingid.

          " Increase value of booking supplement ID with 1
          lv_last_booksupplement_id += 1.
          ls_supplement_create-bookingsupplementid = lv_last_booksupplement_id.

          " Do mapping between the element names of the CDS view and the original table fields
          ls_booksupplement = /dmo/cl_travel_auxiliary=>map_bookingsupplemnt_cds_to_db( CORRESPONDING #( ls_supplement_create ) ) .

          " Create a new booking supplement and update a booking instance
          CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
            EXPORTING
              is_travel              = VALUE /dmo/if_flight_legacy=>ts_travel_in( travel_id = ls_parent_key-travelid )
              is_travelx             = VALUE /dmo/if_flight_legacy=>ts_travel_inx( travel_id = ls_parent_key-travelid )
              it_bookingx            = VALUE /dmo/if_flight_legacy=>tt_booking_inx( ( booking_id  = ls_parent_key-bookingid
                                                                                      action_code = /dmo/if_flight_legacy=>action_code-update ) )
              it_booking_supplement  = VALUE /dmo/if_flight_legacy=>tt_booking_supplement_in( ( ls_booksupplement ) )
              it_booking_supplementx = VALUE /dmo/if_flight_legacy=>tt_booking_supplement_inx( ( _fill_bookingsupplement_inx( ls_supplement_create ) ) )
            IMPORTING
              et_messages = lt_messages.

          IF lt_messages IS INITIAL.
            INSERT VALUE #( %cid = ls_supplement_create-%cid
                            travelid = ls_parent_key-travelid
                            bookingid = ls_parent_key-bookingid
                            bookingsupplementid = ls_booksupplement-booking_supplement_id )
                   INTO TABLE mapped-bookingsupplement.
          ELSE.

            " Issue a message in case of error ('E') or abort ('A')
            LOOP AT lt_messages INTO DATA(ls_message) WHERE msgty = 'E' OR msgty = 'A'.
              INSERT VALUE #( %cid = ls_supplement_create-%cid ) INTO TABLE failed-bookingsupplement.
              INSERT /dmo/cl_travel_auxiliary=>map_bookingsupplemnt_message(
                                                        iv_cid     = ls_supplement_create-%cid
                                                        is_message = ls_message ) INTO TABLE reported-bookingsupplement.
            ENDLOOP.

          ENDIF.

        ENDLOOP.

      ELSE.
        lcl_message_helper=>handle_booking_messages(
          EXPORTING
            iv_cid       = <fs_supplement_create_ba>-%cid_ref
            iv_travel_id = ls_parent_key-travelid
            it_messages  = lt_messages
          CHANGING
            failed   = failed-booking
            reported = reported-booking ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

**********************************************************************
* Helper method:
* Indicates the booking supplemnt fields that have been changed by the client
*
**********************************************************************
  METHOD _fill_bookingsupplement_inx.

    CLEAR rs_bookingsupplement_inx.
    rs_bookingsupplement_inx-booking_supplement_id = is_bookingsupplement_create-bookingsupplementid.
    rs_bookingsupplement_inx-action_code           = /dmo/if_flight_legacy=>action_code-create.
    rs_bookingsupplement_inx-booking_id            = is_bookingsupplement_create-bookingid.

    rs_bookingsupplement_inx-supplement_id         = xsdbool( is_bookingsupplement_create-%control-supplementid = if_abap_behv=>mk-on ).
    rs_bookingsupplement_inx-price                 = xsdbool( is_bookingsupplement_create-%control-price = if_abap_behv=>mk-on ).
    rs_bookingsupplement_inx-currency_code         = xsdbool( is_bookingsupplement_create-%control-currencycode = if_abap_behv=>mk-on ).

  ENDMETHOD.

ENDCLASS.
