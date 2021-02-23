**********************************************************************
*
* Handler class for managing bookings
*
**********************************************************************
CLASS lhc_booking DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS:
      update FOR MODIFY
        IMPORTING entities FOR UPDATE booking,
      delete FOR MODIFY
        IMPORTING keys FOR DELETE booking,
      read FOR READ
        IMPORTING keys FOR READ booking
        RESULT    result,
      rba_travel FOR READ
        IMPORTING keys_rba FOR READ booking\_travel FULL result_requested
        RESULT    result LINK association_links,

      cba_booksupplement FOR MODIFY
        IMPORTING entities FOR CREATE booking\_booksupplement,
      rba_booksupplement FOR READ
        IMPORTING keys_rba   FOR READ booking\_booksupplement FULL result_requested
        RESULT    result LINK association_links.







ENDCLASS.


CLASS lhc_booking IMPLEMENTATION.

**********************************************************************
*
* Implements the UPDATE operation for a set of booking instances
*
**********************************************************************
  METHOD update.

    DATA messages TYPE /dmo/t_message.
    DATA booking  TYPE /dmo/booking.
    DATA bookingx TYPE /dmo/s_booking_inx.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<booking>).

      booking = CORRESPONDING #( <booking> MAPPING FROM ENTITY ).

      bookingx-booking_id  = <booking>-BookingID.
      bookingx-_intx       = CORRESPONDING #( <booking> MAPPING FROM ENTITY ).
      bookingx-action_code = /dmo/if_flight_legacy=>action_code-update.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = VALUE /dmo/s_travel_in( travel_id = <booking>-travelid )
          is_travelx  = VALUE /dmo/s_travel_inx( travel_id = <booking>-travelid )
          it_booking  = VALUE /dmo/t_booking_in( ( CORRESPONDING #( booking ) ) )
          it_bookingx = VALUE /dmo/t_booking_inx( ( bookingx ) )
        IMPORTING
          et_messages = messages.


      /dmo/cl_travel_auxiliary=>handle_booking_messages(
        EXPORTING
          cid        = <booking>-%cid_ref
          travel_id  = <booking>-travelid
          booking_id = <booking>-bookingid
          messages   = messages
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
  METHOD delete.

    DATA messages TYPE /dmo/t_message.

    LOOP AT keys INTO DATA(booking).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = VALUE /dmo/s_travel_in( travel_id = booking-travelid )
          is_travelx  = VALUE /dmo/s_travel_inx( travel_id = booking-travelid )
          it_booking  = VALUE /dmo/t_booking_in( ( booking_id = booking-bookingid ) )
          it_bookingx = VALUE /dmo/t_booking_inx( ( booking_id  = booking-bookingid
                                                    action_code = /dmo/if_flight_legacy=>action_code-delete ) )
        IMPORTING
          et_messages = messages.

      IF messages IS NOT INITIAL.

        /dmo/cl_travel_auxiliary=>handle_booking_messages(
         EXPORTING
           cid        = booking-%cid_ref
           travel_id  = booking-travelid
           booking_id = booking-bookingid
           messages   = messages
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

  METHOD read.

    DATA: travel_out   TYPE /dmo/travel,
          bookings_out TYPE /dmo/t_booking,
          messages     TYPE /dmo/t_message.

    "Only one function call for each requested travelid
    LOOP AT keys ASSIGNING FIELD-SYMBOL(<booking_by_travel>)
                               GROUP BY <booking_by_travel>-travelid .

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = <booking_by_travel>-travelid
        IMPORTING
          es_travel    = travel_out
          et_booking   = bookings_out
          et_messages  = messages.

      IF messages IS INITIAL.
        "For each travelID find the requested bookings
        LOOP AT GROUP <booking_by_travel> ASSIGNING FIELD-SYMBOL(<booking>)
                                          GROUP BY <booking>-%key.

          READ TABLE bookings_out INTO DATA(booking_out) WITH KEY travel_id  = <booking>-%key-TravelID
                                                                  booking_id = <booking>-%key-BookingID .
          "if read was successful
          IF sy-subrc = 0.

            "fill result parameter with flagged fields
            INSERT
              VALUE #( travelid      =   booking_out-travel_id
                       bookingid     =   booking_out-booking_id
                       bookingdate   =   COND #( WHEN <booking>-%control-BookingDate      = cl_abap_behv=>flag_changed THEN booking_out-booking_date   )
                       customerid    =   COND #( WHEN <booking>-%control-CustomerID       = cl_abap_behv=>flag_changed THEN booking_out-customer_id    )
                       airlineid     =   COND #( WHEN <booking>-%control-AirlineID        = cl_abap_behv=>flag_changed THEN booking_out-carrier_id     )
                       connectionid  =   COND #( WHEN <booking>-%control-ConnectionID     = cl_abap_behv=>flag_changed THEN booking_out-connection_id  )
                       flightdate    =   COND #( WHEN <booking>-%control-FlightDate       = cl_abap_behv=>flag_changed THEN booking_out-flight_date    )
                       flightprice   =   COND #( WHEN <booking>-%control-FlightPrice      = cl_abap_behv=>flag_changed THEN booking_out-flight_price   )
                       currencycode  =   COND #( WHEN <booking>-%control-CurrencyCode     = cl_abap_behv=>flag_changed THEN booking_out-currency_code  )
*                       lastchangedat =   COND #( WHEN <booking>-%control-LastChangedAt    = cl_abap_behv=>flag_changed THEN ls_travel_out-lastchangedat   )
                     ) INTO TABLE result.
          ELSE.
            "BookingID not found
            INSERT
              VALUE #( travelid    = <booking>-TravelID
                       bookingid   = <booking>-BookingID
                       %fail-cause = if_abap_behv=>cause-not_found )
              INTO TABLE failed-booking.
          ENDIF.
        ENDLOOP.
      ELSE.
        "TravelID not found or other fail cause
        LOOP AT GROUP <booking_by_travel> ASSIGNING <booking>.
          failed-booking = VALUE #(  BASE failed-booking
                                     FOR msg IN messages ( %key-TravelID    = <booking>-TravelID
                                                           %key-BookingID   = <booking>-BookingID
                                                           %fail-cause      = COND #( WHEN msg-msgty = 'E' AND ( msg-msgno = '016' OR msg-msgno = '009' )
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
  METHOD cba_booksupplement.

    DATA messages               TYPE /dmo/t_message.
    DATA booksupplements_old     TYPE /dmo/t_booking_supplement.
    DATA booksupplement         TYPE /dmo/book_suppl.
    DATA last_bookingsupplement_id TYPE /dmo/booking_supplement_id.

    " Loop at parent - booking
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<bookingsupplement>).
      DATA(parent_key) = <bookingsupplement>-%key.

      " Retrieve booking supplements related to the imported travel ID
      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id          = parent_key-travelid
        IMPORTING
          et_booking_supplement = booksupplements_old
          et_messages           = messages.

      IF messages IS INITIAL.

        " Look up for maximum booking supplement ID for a given travel/booking
        " lt_booksupplement_old provides sorted values, therefore the last value is maximum value
        last_bookingsupplement_id = REDUCE #( INIT res = 0
                                              FOR old IN booksupplements_old
                                                USING KEY primary_key
                                                WHERE ( travel_id  = parent_key-travelid
                                                    AND booking_id = parent_key-bookingid )
                                              NEXT res = old-booking_supplement_id ).

        LOOP AT <bookingsupplement>-%target INTO DATA(supplement).

          " Increase value of booking supplement ID with 1
          last_bookingsupplement_id += 1.

          " Do mapping between the element names of the CDS view and the original table fields
          booksupplement = CORRESPONDING #( supplement MAPPING FROM ENTITY USING CONTROL ).

          " Assign the key elements
          booksupplement-travel_id = parent_key-TravelID.
          booksupplement-booking_id = parent_key-BookingID.
          booksupplement-booking_supplement_id = last_bookingsupplement_id.

          " Create a new booking supplement and update a booking instance
          CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
            EXPORTING
              is_travel              = VALUE /dmo/s_travel_in( travel_id = parent_key-travelid )
              is_travelx             = VALUE /dmo/s_travel_inx( travel_id = parent_key-travelid )
              it_booking_supplement  = VALUE /dmo/t_booking_supplement_in( ( CORRESPONDING #( booksupplement ) ) )
              it_booking_supplementx = VALUE /dmo/t_booking_supplement_inx( ( VALUE #(
                                                                                                             booking_id = booksupplement-booking_id
                                                                                                             booking_supplement_id = booksupplement-booking_supplement_id
                                                                                                             action_code = /dmo/if_flight_legacy=>action_code-create )
                                                                                                 ) )
            IMPORTING
              et_messages            = messages.

          IF messages IS INITIAL.
            INSERT VALUE #( %cid      = supplement-%cid
                            travelid  = parent_key-travelid
                            bookingid = parent_key-bookingid
                            bookingsupplementid = booksupplement-booking_supplement_id )
                   INTO TABLE mapped-bookingsupplement.
          ELSE.

            " Issue a message in case of error ('E') or abort ('A')
            LOOP AT messages INTO DATA(message) WHERE msgty = 'E' OR msgty = 'A'.
              INSERT VALUE #( %cid      = supplement-%cid
                              travelid  = supplement-TravelID
                              bookingid = supplement-BookingID )
                     INTO TABLE failed-bookingsupplement.

              INSERT VALUE #( %cid      = supplement-%cid
                              TravelID  = supplement-TravelID
                              BookingID = supplement-BookingID
                              %msg      = new_message(
                                            id       = message-msgid
                                            number   = message-msgno
                                            severity = if_abap_behv_message=>severity-error
                                            v1       = message-msgv1
                                            v2       = message-msgv2
                                            v3       = message-msgv3
                                            v4       = message-msgv4 ) )
                    INTO TABLE reported-bookingsupplement.


            ENDLOOP.

          ENDIF.

        ENDLOOP.

      ELSE.
        /dmo/cl_travel_auxiliary=>handle_booking_messages(
           EXPORTING
             cid       = <bookingsupplement>-%cid_ref
             travel_id = parent_key-TravelID
             booking_id = parent_key-BookingID
             messages  = messages
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
  METHOD rba_travel.
    DATA: travel_out     TYPE /dmo/travel,
          travel         LIKE LINE OF result,
          messages       TYPE /dmo/t_message.

    "Only one function call for each requested travelid
    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<travel>)
                                 GROUP BY <travel>-TravelID.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = <travel>-%key-TravelID
        IMPORTING
          es_travel    = travel_out
          et_messages  = messages.

      IF messages IS INITIAL.

        LOOP AT GROUP <travel> ASSIGNING FIELD-SYMBOL(<booking>).
          "fill link table with key fields
          INSERT VALUE #( source-%key = <booking>-%key
                          target-%key = travel_out-travel_id )
           INTO TABLE association_links.

          IF result_requested = abap_true.
            "fill result parameter with flagged fields
            travel = CORRESPONDING #(  travel_out MAPPING TO ENTITY ).
            INSERT travel INTO TABLE result.
          ENDIF.
        ENDLOOP.

        DELETE ADJACENT DUPLICATES FROM association_links COMPARING ALL FIELDS.
        DELETE ADJACENT DUPLICATES FROM result COMPARING ALL FIELDS.

      ELSE. "fill failed table in case of error
        failed-booking = VALUE #(  BASE failed-booking
                              FOR msg IN messages ( %key-TravelID    = <travel>-%key-TravelID
                                                    %key-BookingID   = <travel>-%key-BookingID
                                                    %fail-cause      = COND #( WHEN msg-msgty = 'E' AND ( msg-msgno = '016' OR msg-msgno = '009' )
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
  METHOD rba_booksupplement.
    DATA: ls_travel_out    TYPE /dmo/travel,
          lt_booksuppl_out TYPE /dmo/t_booking_supplement,
          lt_message       TYPE /dmo/t_message.

    "Only one function call for each travelid
    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<fs_travel>)
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
        LOOP AT GROUP <fs_travel> ASSIGNING FIELD-SYMBOL(<fs_booking>) GROUP BY <fs_booking>-%key.
          LOOP AT lt_booksuppl_out ASSIGNING FIELD-SYMBOL(<fs_booksuppl>) WHERE  travel_id  = <fs_booking>-%key-TravelID
                                                                          AND    booking_id = <fs_booking>-%key-BookingID .
            "Fill link table with key fields
            INSERT
                 VALUE #( source-%key = VALUE #( TravelID            = <fs_booking>-%key-TravelID
                                                 BookingID           = <fs_booking>-%key-BookingID )
                          target-%key = VALUE #( TravelID            = <fs_booksuppl>-travel_id
                                                 BookingID           = <fs_booksuppl>-booking_id
                                                 BookingSupplementID = <fs_booksuppl>-booking_supplement_id  ) )
                 INTO TABLE association_links.

            "fill result parameter with flagged fields
            IF result_requested = abap_true.
              INSERT
                 VALUE #( travelid            = SWITCH #( <fs_booking>-%control-TravelID            WHEN cl_abap_behv=>flag_changed THEN <fs_booksuppl>-travel_id )
                          bookingid           = SWITCH #( <fs_booking>-%control-BookingID           WHEN cl_abap_behv=>flag_changed THEN <fs_booksuppl>-booking_id )
                          bookingsupplementid = SWITCH #( <fs_booking>-%control-BookingSupplementID WHEN cl_abap_behv=>flag_changed THEN <fs_booksuppl>-booking_supplement_id )
                          supplementid        = SWITCH #( <fs_booking>-%control-SupplementID        WHEN cl_abap_behv=>flag_changed THEN <fs_booksuppl>-supplement_id )
                          price               = SWITCH #( <fs_booking>-%control-Price               WHEN cl_abap_behv=>flag_changed THEN <fs_booksuppl>-price )
                          currencycode        = SWITCH #( <fs_booking>-%control-CurrencyCode        WHEN cl_abap_behv=>flag_changed THEN <fs_booksuppl>-currency_code ) )
*                          lastchangedat       = SWITCH #( <fs_booking>-%control-LastChangedAt       WHEN cl_abap_behv=>flag_changed THEN ls_travel_out-lastchangedat ) )
                 INTO TABLE result.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

      ELSE.
        "Fill failed table in case of error
        LOOP AT GROUP <fs_travel> ASSIGNING <fs_booking>.
          failed-booking = VALUE #(  BASE failed-booking
                                FOR msg IN lt_message ( %key-TravelID    = <fs_booking>-%key-TravelID
                                                        %key-BookingID   = <fs_booking>-%key-BookingID
                                                        %fail-cause      = COND #( WHEN msg-msgty = 'E' AND ( msg-msgno = '016' OR msg-msgno = '009' )
                                                                                   THEN if_abap_behv=>cause-not_found
                                                                                   ELSE if_abap_behv=>cause-unspecific ) ) ).

        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.



ENDCLASS.
