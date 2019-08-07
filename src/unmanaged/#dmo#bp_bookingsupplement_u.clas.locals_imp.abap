**********************************************************************
*
* Local class for handling messages of booking supplements
*
**********************************************************************
CLASS lcl_message_helper DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES tt_bookingsupplement_failed   TYPE TABLE FOR FAILED   /dmo/i_bookingsupplement_u.
    TYPES tt_bookingsupplement_reported TYPE TABLE FOR REPORTED /dmo/i_bookingsupplement_u.

    CLASS-METHODS handle_bookingsuppl_messages
      IMPORTING iv_cid                  TYPE string OPTIONAL
                iv_travel_id            TYPE /dmo/travel_id OPTIONAL
                iv_booking_id           TYPE /dmo/booking_id OPTIONAL
                iv_bookingsupplement_id TYPE /dmo/booking_supplement_id OPTIONAL
                it_messages             TYPE /dmo/if_flight_legacy=>tt_message
      CHANGING failed   TYPE tt_bookingsupplement_failed
               reported TYPE tt_bookingsupplement_reported.

ENDCLASS.

CLASS lcl_message_helper IMPLEMENTATION.

  METHOD handle_bookingsuppl_messages.

    LOOP AT it_messages INTO DATA(ls_message) WHERE msgty = 'E' OR msgty = 'A'.
      INSERT VALUE #( %cid                = iv_cid
                      travelid            = iv_travel_id
                      bookingid           = iv_booking_id
                      bookingsupplementid = iv_bookingsupplement_id ) INTO TABLE failed.

      INSERT /dmo/cl_travel_auxiliary=>map_bookingsupplemnt_message(
                                          iv_travel_id            = iv_travel_id
                                          iv_booking_id           = iv_booking_id
                                          iv_bookingsupplement_id = iv_bookingsupplement_id
                                          is_message              = ls_message ) INTO TABLE reported.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


**********************************************************************
*
* Handler class implements UPDATE and DELETE for booking supplements
*
**********************************************************************
CLASS lhc_supplement DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.
    TYPES:
        tt_booking_update           TYPE TABLE FOR UPDATE    /dmo/i_booking_u,
        tt_bookingsupplement_update TYPE TABLE FOR UPDATE    /dmo/i_bookingsupplement_u.

    METHODS:
        update_bookingsupplement FOR MODIFY
                                    IMPORTING it_bookingsupplement_update FOR UPDATE bookingsupplement,
        delete_bookingsupplement FOR MODIFY
                                    IMPORTING it_bookingsupplement_delete FOR DELETE bookingsupplement.

    METHODS _fill_bookingsupplement_inx
                                    IMPORTING is_bookingsupplement_update     TYPE LINE OF tt_bookingsupplement_update
                                    RETURNING VALUE(rs_bookingsupplement_inx) TYPE /dmo/if_flight_legacy=>ts_booking_supplement_inx.
ENDCLASS.


CLASS lhc_supplement IMPLEMENTATION.


**********************************************************************
*
* Implements the UPDATE operation for a set of booking supplements
*
**********************************************************************
  METHOD update_bookingsupplement.

    DATA lt_messages TYPE /dmo/if_flight_legacy=>tt_message.

    LOOP AT it_bookingsupplement_update ASSIGNING FIELD-SYMBOL(<fs_bookingsupplement_update>).
      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel              = VALUE /dmo/if_flight_legacy=>ts_travel_in( travel_id = <fs_bookingsupplement_update>-travelid )
          is_travelx             = VALUE /dmo/if_flight_legacy=>ts_travel_inx( travel_id = <fs_bookingsupplement_update>-travelid )
          it_booking_supplement  = VALUE /dmo/if_flight_legacy=>tt_booking_supplement_in( ( /dmo/cl_travel_auxiliary=>map_bookingsupplemnt_cds_to_db( CORRESPONDING #( <fs_bookingsupplement_update> ) ) ) )
          it_booking_supplementx = VALUE /dmo/if_flight_legacy=>tt_booking_supplement_inx( ( _fill_bookingsupplement_inx( <fs_bookingsupplement_update> ) ) )
        IMPORTING
          et_messages = lt_messages.


      lcl_message_helper=>handle_bookingsuppl_messages(
        EXPORTING
          iv_cid                  = <fs_bookingsupplement_update>-%cid_ref
          iv_travel_id            = <fs_bookingsupplement_update>-travelid
          iv_booking_id           = <fs_bookingsupplement_update>-bookingid
          iv_bookingsupplement_id = <fs_bookingsupplement_update>-bookingsupplementid
          it_messages             = lt_messages
        CHANGING
            failed   = failed-bookingsupplement
            reported = reported-bookingsupplement ).

    ENDLOOP.

  ENDMETHOD.


**********************************************************************
* Helper method:
* Indicates the booking supplement fields that have been changed by the client
**********************************************************************

  METHOD _fill_bookingsupplement_inx.

    CLEAR rs_bookingsupplement_inx.
    rs_bookingsupplement_inx-booking_supplement_id = is_bookingsupplement_update-bookingsupplementid.
    rs_bookingsupplement_inx-action_code           = /dmo/if_flight_legacy=>action_code-update.
    rs_bookingsupplement_inx-booking_id            = is_bookingsupplement_update-bookingid.

    rs_bookingsupplement_inx-supplement_id         = xsdbool( is_bookingsupplement_update-%control-supplementid = if_abap_behv=>mk-on ).
    rs_bookingsupplement_inx-price                 = xsdbool( is_bookingsupplement_update-%control-price        = if_abap_behv=>mk-on ).
    rs_bookingsupplement_inx-currency_code         = xsdbool( is_bookingsupplement_update-%control-currencycode = if_abap_behv=>mk-on ).
  ENDMETHOD.


**********************************************************************
*
* Implements the DELETE operation for a set of booking supplements
*
**********************************************************************
  METHOD delete_bookingsupplement.

    DATA lt_messages TYPE /dmo/if_flight_legacy=>tt_message.

    LOOP AT it_bookingsupplement_delete INTO DATA(ls_bookingsupplement_delete).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel              = VALUE /dmo/if_flight_legacy=>ts_travel_in( travel_id = ls_bookingsupplement_delete-travelid )
          is_travelx             = VALUE /dmo/if_flight_legacy=>ts_travel_inx( travel_id = ls_bookingsupplement_delete-travelid )
          it_booking             = VALUE /dmo/if_flight_legacy=>tt_booking_in( ( booking_id = ls_bookingsupplement_delete-bookingid ) )
          it_bookingx            = VALUE /dmo/if_flight_legacy=>tt_booking_inx( ( booking_id  = ls_bookingsupplement_delete-bookingid ) )
          it_booking_supplement  = VALUE /dmo/if_flight_legacy=>tt_booking_supplement_in( (  booking_supplement_id = ls_bookingsupplement_delete-bookingSupplementid
                                                                                             booking_id            = ls_bookingsupplement_delete-BookingID ) )
          it_booking_supplementx = VALUE /dmo/if_flight_legacy=>tt_booking_supplement_inx( ( booking_supplement_id = ls_bookingsupplement_delete-bookingsupplementid
                                                                                             booking_id            = ls_bookingsupplement_delete-bookingid
                                                                                             action_code           = /dmo/if_flight_legacy=>action_code-delete ) )
        IMPORTING
          et_messages = lt_messages.

      IF lt_messages IS NOT INITIAL.

        lcl_message_helper=>handle_bookingsuppl_messages(
         EXPORTING
           iv_cid                  = ls_bookingsupplement_delete-%cid_ref
           iv_travel_id            = ls_bookingsupplement_delete-travelid
           iv_booking_id           = ls_bookingsupplement_delete-bookingid
           iv_bookingsupplement_id = ls_bookingsupplement_delete-bookingsupplementid
           it_messages = lt_messages
         CHANGING
           failed   = failed-bookingsupplement
           reported = reported-bookingsupplement ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
