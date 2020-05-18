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
        IMPORTING it_booking_update FOR UPDATE booking,
      delete_booking FOR MODIFY
        IMPORTING it_booking_delete FOR DELETE booking,
      read_booking FOR READ
        IMPORTING it_booking_read FOR READ booking
        RESULT    et_booking,

      cba_supplement FOR MODIFY
        IMPORTING it_supplement_create_ba FOR CREATE booking\_booksupplement,
      read_supplement_ba FOR READ
        IMPORTING it_booking   FOR READ booking\_booksupplement FULL iv_full_requested
        RESULT    et_booksuppl LINK et_link_table,
      read_travel_ba FOR READ
        IMPORTING it_booking FOR READ booking\_travel FULL iv_full_requested
        RESULT    et_travel LINK et_link_table.



*    METHODS:
*      _fill_booking_inx
*        IMPORTING is_booking_update     TYPE LINE OF tt_booking_update
*        RETURNING VALUE(rs_booking_inx) TYPE /dmo/s_booking_inx.

ENDCLASS.


CLASS lhc_booking IMPLEMENTATION.

**********************************************************************
*
* Implements the UPDATE operation for a set of booking instances
*
**********************************************************************
  METHOD update_booking.

    DATA lt_messages TYPE /dmo/t_message.
    DATA ls_booking  TYPE /dmo/booking.
    DATA ls_bookingx TYPE /dmo/s_booking_inx.

    LOOP AT it_booking_update ASSIGNING FIELD-SYMBOL(<fs_booking_update>).

      ls_booking = CORRESPONDING #( <fs_booking_update> MAPPING FROM ENTITY ).

      ls_bookingx-booking_id = <fs_booking_update>-BookingID.
      ls_bookingx-_intx      = CORRESPONDING #( <fs_booking_update> MAPPING FROM ENTITY ).
      ls_bookingx-action_code = /dmo/if_flight_legacy=>action_code-update.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = VALUE /dmo/s_travel_in( travel_id = <fs_booking_update>-travelid )
          is_travelx  = VALUE /dmo/s_travel_inx( travel_id = <fs_booking_update>-travelid )
          it_booking  = VALUE /dmo/t_booking_in( ( CORRESPONDING #( ls_booking ) ) )
          it_bookingx = VALUE /dmo/t_booking_inx( ( ls_bookingx ) )
        IMPORTING
          et_messages = lt_messages.


      /dmo/cl_travel_auxiliary=>handle_booking_messages(
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

***********************************************************************
** Helper method:
** Indicates the booking fields that have been changed by the client
**
***********************************************************************
*  METHOD _fill_booking_inx.
*
*    CLEAR rs_booking_inx.
*    rs_booking_inx-booking_id    = is_booking_update-bookingid.
*    rs_booking_inx-action_code   = /dmo/if_flight_legacy=>action_code-update.
*
*    rs_booking_inx-booking_date  = xsdbool( is_booking_update-%control-bookingdate  = if_abap_behv=>mk-on ).
*    rs_booking_inx-customer_id   = xsdbool( is_booking_update-%control-customerid   = if_abap_behv=>mk-on ).
*    rs_booking_inx-carrier_id    = xsdbool( is_booking_update-%control-airlineid    = if_abap_behv=>mk-on ).
*    rs_booking_inx-connection_id = xsdbool( is_booking_update-%control-connectionid = if_abap_behv=>mk-on ).
*    rs_booking_inx-flight_date   = xsdbool( is_booking_update-%control-flightdate   = if_abap_behv=>mk-on ).
*    rs_booking_inx-flight_price  = xsdbool( is_booking_update-%control-flightprice  = if_abap_behv=>mk-on ).
*    rs_booking_inx-currency_code = xsdbool( is_booking_update-%control-currencycode = if_abap_behv=>mk-on ).
*  ENDMETHOD.


**********************************************************************
*
* Implements the DELETE operation for a set of booking instances
*
**********************************************************************
  METHOD delete_booking.

    DATA lt_messages TYPE /dmo/t_message.

    LOOP AT it_booking_delete INTO DATA(ls_booking_delete).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = VALUE /dmo/s_travel_in( travel_id = ls_booking_delete-travelid )
          is_travelx  = VALUE /dmo/s_travel_inx( travel_id = ls_booking_delete-travelid )
          it_booking  = VALUE /dmo/t_booking_in( ( booking_id = ls_booking_delete-bookingid ) )
          it_bookingx = VALUE /dmo/t_booking_inx( ( booking_id  = ls_booking_delete-bookingid
                                                    action_code = /dmo/if_flight_legacy=>action_code-delete ) )
        IMPORTING
          et_messages = lt_messages.

      IF lt_messages IS NOT INITIAL.

        /dmo/cl_travel_auxiliary=>handle_booking_messages(
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

**********************************************************************
*
* Read booking data from buffer
*
**********************************************************************

  METHOD read_booking.

    DATA: ls_travel_out  TYPE /dmo/travel,
          lt_booking_out TYPE /dmo/t_booking,
          lt_message     TYPE /dmo/t_message.

    "Only one function call for each requested travelid
    LOOP AT it_booking_read ASSIGNING FIELD-SYMBOL(<fs_travel_read>)
                            GROUP BY <fs_travel_read>-travelid .

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = <fs_travel_read>-travelid
        IMPORTING
          es_travel    = ls_travel_out
          et_booking   = lt_booking_out
          et_messages  = lt_message.

      IF lt_message IS INITIAL.
        "For each travelID find the requested bookings
        LOOP AT GROUP <fs_travel_read> ASSIGNING FIELD-SYMBOL(<fs_booking_read>).

          READ TABLE lt_booking_out INTO DATA(ls_booking) WITH KEY travel_id  = <fs_booking_read>-%key-TravelID
                                                                   booking_id = <fs_booking_read>-%key-BookingID .
          "if read was successfull
          IF sy-subrc = 0.

            "fill result parameter with flagged fields
            INSERT
              VALUE #( travelid      =   ls_booking-travel_id
                       bookingid     =   ls_booking-booking_id
                       bookingdate   =   COND #( WHEN <fs_booking_read>-%control-BookingDate      = cl_abap_behv=>flag_changed THEN ls_booking-booking_date   )
                       customerid    =   COND #( WHEN <fs_booking_read>-%control-CustomerID       = cl_abap_behv=>flag_changed THEN ls_booking-customer_id    )
                       airlineid     =   COND #( WHEN <fs_booking_read>-%control-AirlineID        = cl_abap_behv=>flag_changed THEN ls_booking-carrier_id     )
                       connectionid  =   COND #( WHEN <fs_booking_read>-%control-ConnectionID     = cl_abap_behv=>flag_changed THEN ls_booking-connection_id  )
                       flightdate    =   COND #( WHEN <fs_booking_read>-%control-FlightDate       = cl_abap_behv=>flag_changed THEN ls_booking-flight_date    )
                       flightprice   =   COND #( WHEN <fs_booking_read>-%control-FlightPrice      = cl_abap_behv=>flag_changed THEN ls_booking-flight_price   )
                       currencycode  =   COND #( WHEN <fs_booking_read>-%control-CurrencyCode     = cl_abap_behv=>flag_changed THEN ls_booking-currency_code  )
*                       lastchangedat =   COND #( WHEN <fs_booking_read>-%control-LastChangedAt    = cl_abap_behv=>flag_changed THEN ls_travel_out-lastchangedat   )
                     ) INTO TABLE et_booking.
          ELSE.
            "BookingID not found
            INSERT
              VALUE #( travelid    = <fs_booking_read>-TravelID
                       bookingid   = <fs_booking_read>-BookingID
                       %fail-cause = if_abap_behv=>cause-not_found )
              INTO TABLE failed-booking.
          ENDIF.
        ENDLOOP.
      ELSE.
        "TravelID not found or other fail cause
        LOOP AT GROUP <fs_travel_read> ASSIGNING <fs_booking_read>.
          failed-booking = VALUE #(  BASE failed-booking
                                     FOR msg IN lt_message ( %key-TravelID    = <fs_booking_read>-TravelID
                                                             %key-BookingID   = <fs_booking_read>-BookingID
                                                             %fail-cause      = COND #( WHEN msg-msgty = 'E' AND msg-msgno = '016'
                                                                                        THEN if_abap_behv=>cause-not_found
                                                                                        ELSE if_abap_behv=>cause-unspecific ) ) ).
        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

***********************************************************************
*
* Create associated booking supplements
*
***********************************************************************
  METHOD cba_supplement.

    DATA lt_messages               TYPE /dmo/t_message.
    DATA lt_booksupplement_old     TYPE /dmo/t_booking_supplement.
    DATA ls_booksupplement         TYPE /dmo/book_suppl.
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

          " Increase value of booking supplement ID with 1
          lv_last_booksupplement_id += 1.

          " Do mapping between the element names of the CDS view and the original table fields
          ls_booksupplement = CORRESPONDING #( ls_supplement_create MAPPING FROM ENTITY USING CONTROL ).

          " Assign the key elements
          ls_booksupplement-travel_id = ls_parent_key-TravelID.
          ls_booksupplement-booking_id = ls_parent_key-BookingID.
          ls_booksupplement-booking_supplement_id = lv_last_booksupplement_id.

          " Create a new booking supplement and update a booking instance
          CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
            EXPORTING
              is_travel              = VALUE /dmo/s_travel_in( travel_id = ls_parent_key-travelid )
              is_travelx             = VALUE /dmo/s_travel_inx( travel_id = ls_parent_key-travelid )
              it_booking_supplement  = VALUE /dmo/t_booking_supplement_in( ( CORRESPONDING #( ls_booksupplement ) ) )
              it_booking_supplementx = VALUE /dmo/t_booking_supplement_inx( ( VALUE #(
                                                                                                             booking_id = ls_booksupplement-booking_id
                                                                                                             booking_supplement_id = ls_booksupplement-booking_supplement_id
                                                                                                             action_code = /dmo/if_flight_legacy=>action_code-create )
                                                                                                 ) )
            IMPORTING
              et_messages            = lt_messages.

          IF lt_messages IS INITIAL.
            INSERT VALUE #( %cid      = ls_supplement_create-%cid
                            travelid  = ls_parent_key-travelid
                            bookingid = ls_parent_key-bookingid
                            bookingsupplementid = ls_booksupplement-booking_supplement_id )
                   INTO TABLE mapped-bookingsupplement.
          ELSE.

            " Issue a message in case of error ('E') or abort ('A')
            LOOP AT lt_messages INTO DATA(ls_message) WHERE msgty = 'E' OR msgty = 'A'.
              INSERT VALUE #( %cid      = ls_supplement_create-%cid
                              travelid  = ls_supplement_create-TravelID
                              bookingid = ls_supplement_create-BookingID )
                     INTO TABLE failed-bookingsupplement.

              INSERT VALUE #( %cid      = ls_supplement_create-%cid
                              TravelID  = ls_supplement_create-TravelID
                              BookingID = ls_supplement_create-BookingID
                              %msg      = new_message(
                                            id       = ls_message-msgid
                                            number   = ls_message-msgno
                                            severity = if_abap_behv_message=>severity-error
                                            v1       = ls_message-msgv1
                                            v2       = ls_message-msgv2
                                            v3       = ls_message-msgv3
                                            v4       = ls_message-msgv4 ) )
                    INTO TABLE reported-bookingsupplement.


            ENDLOOP.

          ENDIF.

        ENDLOOP.

      ELSE.
        /dmo/cl_travel_auxiliary=>handle_booking_messages(
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
*
* Read travel  by association
*
**********************************************************************
  METHOD read_travel_ba.
    DATA: ls_travel_out  TYPE /dmo/travel,
          lt_booking_out TYPE /dmo/t_booking,
          ls_travel      LIKE LINE OF et_travel,
          lt_message     TYPE /dmo/t_message.

    "Only one function call for each requested travelid
    LOOP AT it_booking ASSIGNING FIELD-SYMBOL(<fs_travel>)
                                 GROUP BY <fs_travel>-TravelID.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = <fs_travel>-%key-TravelID
        IMPORTING
          es_travel    = ls_travel_out
          et_messages  = lt_message.

      IF lt_message IS INITIAL.

        LOOP AT GROUP <fs_travel> ASSIGNING FIELD-SYMBOL(<fs_booking>).
          "fill link table with key fields
          INSERT VALUE #( source-%key = <fs_booking>-%key
                          target-%key = ls_travel_out-travel_id )
           INTO TABLE et_link_table.

          IF iv_full_requested = abap_true.
            "fill result parameter with flagged fields
            ls_travel = CORRESPONDING #(  ls_travel_out MAPPING TO ENTITY ).
            INSERT ls_travel INTO TABLE et_travel.
          ENDIF.
        ENDLOOP.

      ELSE. "fill failed table in case of error
        failed-booking = VALUE #(  BASE failed-booking
                              FOR msg IN lt_message ( %key-TravelID    = <fs_travel>-%key-TravelID
                                                      %key-BookingID   = <fs_travel>-%key-BookingID
                                                      %fail-cause      = COND #( WHEN msg-msgty = 'E' AND msg-msgno = '016'
                                                                                 THEN if_abap_behv=>cause-not_found
                                                                                ELSE if_abap_behv=>cause-unspecific ) ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

**********************************************************************
*
* Read booking supplement by association
*
**********************************************************************
  METHOD read_supplement_ba.
    DATA: ls_travel_out    TYPE /dmo/travel,
          lt_booksuppl_out TYPE /dmo/t_booking_supplement,
          lt_message       TYPE /dmo/t_message.

    "Only one function call for each travelid
    LOOP AT it_booking ASSIGNING FIELD-SYMBOL(<fs_travel>)
                       GROUP BY <fs_travel>-TravelID.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id          = <fs_travel>-travelid
        IMPORTING
          es_travel             = ls_travel_out
          et_booking_supplement = lt_booksuppl_out
          et_messages           = lt_message.

      IF lt_message IS INITIAL.
        "Get BookingSupplements for each travel
        LOOP AT GROUP <fs_travel> ASSIGNING FIELD-SYMBOL(<fs_booking>).
          LOOP AT lt_booksuppl_out ASSIGNING FIELD-SYMBOL(<fs_booksuppl>) WHERE  travel_id  = <fs_booking>-%key-TravelID
                                                                          AND    booking_id = <fs_booking>-%key-BookingID .
            "Fill link table with key fields
            INSERT
                 VALUE #( source-%key = VALUE #( TravelID            = <fs_booking>-%key-TravelID
                                                 BookingID           = <fs_booking>-%key-BookingID )
                          target-%key = VALUE #( TravelID            = <fs_booksuppl>-travel_id
                                                 BookingID           = <fs_booksuppl>-booking_id
                                                 BookingSupplementID = <fs_booksuppl>-booking_supplement_id  ) )
                 INTO TABLE et_link_table.

            "fill result parameter with flagged fields
            IF iv_full_requested = abap_true.
              INSERT
                 VALUE #( travelid            = SWITCH #( <fs_booking>-%control-TravelID            WHEN cl_abap_behv=>flag_changed THEN <fs_booksuppl>-travel_id )
                          bookingid           = SWITCH #( <fs_booking>-%control-BookingID           WHEN cl_abap_behv=>flag_changed THEN <fs_booksuppl>-booking_id )
                          bookingsupplementid = SWITCH #( <fs_booking>-%control-BookingSupplementID WHEN cl_abap_behv=>flag_changed THEN <fs_booksuppl>-booking_supplement_id )
                          supplementid        = SWITCH #( <fs_booking>-%control-SupplementID        WHEN cl_abap_behv=>flag_changed THEN <fs_booksuppl>-supplement_id )
                          price               = SWITCH #( <fs_booking>-%control-Price               WHEN cl_abap_behv=>flag_changed THEN <fs_booksuppl>-price )
                          currencycode        = SWITCH #( <fs_booking>-%control-CurrencyCode        WHEN cl_abap_behv=>flag_changed THEN <fs_booksuppl>-currency_code ) )
*                          lastchangedat       = SWITCH #( <fs_booking>-%control-LastChangedAt       WHEN cl_abap_behv=>flag_changed THEN ls_travel_out-lastchangedat ) )
                 INTO TABLE et_booksuppl.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

      ELSE.
        "Fill failed table in case of error
        LOOP AT GROUP <fs_travel> ASSIGNING <fs_booking>.
          failed-booking = VALUE #(  BASE failed-booking
                                FOR msg IN lt_message ( %key-TravelID    = <fs_booking>-%key-TravelID
                                                        %key-BookingID   = <fs_booking>-%key-BookingID
                                                        %fail-cause      = COND #( WHEN msg-msgty = 'E' AND msg-msgno = '016'
                                                                                   THEN if_abap_behv=>cause-not_found
                                                                                   ELSE if_abap_behv=>cause-unspecific ) ) ).

        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.



ENDCLASS.
