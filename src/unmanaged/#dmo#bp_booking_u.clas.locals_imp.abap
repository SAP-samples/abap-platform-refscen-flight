CLASS ltcl_handler DEFINITION DEFERRED FOR TESTING.
CLASS lhc_booking DEFINITION
  INHERITING FROM cl_abap_behavior_handler
  FRIENDS ltcl_handler
  .
  PRIVATE SECTION.

    TYPES tt_booking_failed   TYPE TABLE FOR FAILED   /dmo/i_booking_u.
    TYPES tt_booking_reported TYPE TABLE FOR REPORTED /dmo/i_booking_u.

    TYPES tt_bookingsupplement_failed   TYPE TABLE FOR FAILED   /dmo/i_bookingsupplement_u.
    TYPES tt_bookingsupplement_reported TYPE TABLE FOR REPORTED /dmo/i_bookingsupplement_u.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE booking.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE booking.

    METHODS read FOR READ
      IMPORTING keys FOR READ booking RESULT result.

    METHODS rba_Booksupplement FOR READ
      IMPORTING keys_rba FOR READ booking\_Booksupplement FULL result_requested RESULT result LINK association_links.

    METHODS rba_Travel FOR READ
      IMPORTING keys_rba FOR READ booking\_Travel FULL result_requested RESULT result LINK association_links.

    METHODS cba_Booksupplement FOR MODIFY
      IMPORTING entities_cba FOR CREATE booking\_Booksupplement.

    METHODS map_messages
      IMPORTING
        cid          TYPE string OPTIONAL
        travel_id    TYPE /dmo/travel_id OPTIONAL
        booking_id   TYPE /dmo/booking_id OPTIONAL
        messages     TYPE /dmo/t_message
      EXPORTING
        failed_added TYPE abap_bool
      CHANGING
        failed       TYPE tt_booking_failed
        reported     TYPE tt_booking_reported.

    METHODS map_messages_assoc_to_booksupp
      IMPORTING
        cid          TYPE string
        is_dependend TYPE abap_bool DEFAULT abap_false
        messages     TYPE /dmo/t_message
      EXPORTING
        failed_added TYPE abap_bool
      CHANGING
        failed       TYPE tt_bookingsupplement_failed
        reported     TYPE tt_bookingsupplement_reported.

ENDCLASS.

CLASS lhc_booking IMPLEMENTATION.

**********************************************************************
*
* Implements the UPDATE operation for a set of booking instances
*
**********************************************************************
  METHOD update.
    DATA: messages TYPE /dmo/t_message,
          booking  TYPE /dmo/booking,
          bookingx TYPE /dmo/s_booking_inx.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<booking>).

      booking = CORRESPONDING #( <booking> MAPPING FROM ENTITY ).

      bookingx-_intx       = CORRESPONDING #( <booking> MAPPING FROM ENTITY ).
      bookingx-booking_id  = <booking>-BookingID.
      bookingx-action_code = /dmo/if_flight_legacy=>action_code-update.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = VALUE /dmo/s_travel_in( travel_id = <booking>-travelid )
          is_travelx  = VALUE /dmo/s_travel_inx( travel_id = <booking>-travelid )
          it_booking  = VALUE /dmo/t_booking_in( ( CORRESPONDING #( booking ) ) )
          it_bookingx = VALUE /dmo/t_booking_inx( ( bookingx ) )
        IMPORTING
          et_messages = messages.

      map_messages(
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


**********************************************************************
*
* Implements the DELETE operation for a set of booking instances
*
**********************************************************************
  METHOD delete.
    DATA messages TYPE /dmo/t_message.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<booking>).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = VALUE /dmo/s_travel_in( travel_id = <booking>-travelid )
          is_travelx  = VALUE /dmo/s_travel_inx( travel_id = <booking>-travelid )
          it_booking  = VALUE /dmo/t_booking_in( ( booking_id = <booking>-bookingid ) )
          it_bookingx = VALUE /dmo/t_booking_inx( ( booking_id  = <booking>-bookingid
                                                    action_code = /dmo/if_flight_legacy=>action_code-delete ) )
        IMPORTING
          et_messages = messages.

      map_messages(
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

      map_messages(
        EXPORTING
          travel_id  = <booking_by_travel>-travelid
          booking_id = <booking_by_travel>-bookingid
          messages   = messages
        IMPORTING
          failed_added = DATA(failed_added)
        CHANGING
          failed   = failed-booking
          reported = reported-booking ).


      IF failed_added = abap_false.
        "For each travelID find the requested bookings
        LOOP AT GROUP <booking_by_travel> ASSIGNING FIELD-SYMBOL(<booking>)
                                          GROUP BY <booking>-%tky.

          READ TABLE bookings_out INTO DATA(booking_out) WITH KEY travel_id  = <booking>-%key-TravelID
                                                                  booking_id = <booking>-%key-BookingID .
          "if read was successful
          IF sy-subrc = 0.
            INSERT CORRESPONDING #( booking_out MAPPING TO ENTITY ) INTO TABLE result.
          ELSE.
            "BookingID not found
            INSERT VALUE #( travelid    = <booking>-TravelID
                            bookingid   = <booking>-BookingID
                            %fail-cause = if_abap_behv=>cause-not_found )
              INTO TABLE failed-booking.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


***********************************************************************
*
* Read associated booking supplements
*
***********************************************************************
  METHOD rba_Booksupplement.
    DATA: bookingsupplements_out TYPE /dmo/t_booking_supplement,
          messages               TYPE /dmo/t_message.

    "Only one function call for each requested travelid
    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<bookingsupplement_by_travel>)
                               GROUP BY <bookingsupplement_by_travel>-travelid .

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id          = <bookingsupplement_by_travel>-travelid
        IMPORTING
          et_booking_supplement = bookingsupplements_out
          et_messages           = messages.

      LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<bookingsuppl_by_travel_book>)
                                 USING KEY entity
                                 WHERE TravelID = <bookingsupplement_by_travel>-TravelID.

        map_messages(
          EXPORTING
            travel_id  = <bookingsuppl_by_travel_book>-travelid
            booking_id = <bookingsuppl_by_travel_book>-bookingid
            messages   = messages
          IMPORTING
            failed_added = DATA(failed_added)
          CHANGING
            failed   = failed-booking
            reported = reported-booking ).

        IF failed_added = abap_false.
          LOOP AT bookingsupplements_out ASSIGNING FIELD-SYMBOL(<bookingsupplement>)
                                                   WHERE travel_id  = <bookingsuppl_by_travel_book>-TravelID
                                                     AND booking_id = <bookingsuppl_by_travel_book>-BookingID.

            IF result_requested = abap_true.
              INSERT CORRESPONDING #( <bookingsupplement> MAPPING TO ENTITY ) INTO TABLE result.
            ENDIF.

            INSERT VALUE #(
                source-travelid            = <bookingsupplement>-travel_id
                source-bookingid           = <bookingsupplement>-booking_id
                target-travelid            = <bookingsupplement>-travel_id
                target-bookingid           = <bookingsupplement>-booking_id
                target-bookingsupplementid = <bookingsupplement>-booking_supplement_id
              ) INTO TABLE association_links.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    SORT association_links BY target ASCENDING.
    DELETE ADJACENT DUPLICATES FROM association_links COMPARING ALL FIELDS.

    SORT result BY %tky ASCENDING.
    DELETE ADJACENT DUPLICATES FROM result COMPARING ALL FIELDS.
  ENDMETHOD.


***********************************************************************
*
* Read associated travels
*
***********************************************************************
  METHOD rba_Travel.
    DATA: travel   TYPE /dmo/travel,
          messages TYPE /dmo/t_message.

    "Only one function call for each requested travelid
    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<booking_by_travel>)
                               GROUP BY <booking_by_travel>-travelid .

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = <booking_by_travel>-travelid
        IMPORTING
          es_travel    = travel
          et_messages  = messages.

      map_messages(
        EXPORTING
          travel_id  = <booking_by_travel>-travelid
          booking_id = <booking_by_travel>-bookingid
          messages   = messages
        IMPORTING
          failed_added = DATA(failed_added)
        CHANGING
          failed   = failed-booking
          reported = reported-booking ).


      IF failed_added = abap_false.
        LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<travel>) USING KEY entity WHERE TravelID = <booking_by_travel>-TravelID.
          INSERT VALUE #(
              source-%tky     = <travel>-%tky
              target-travelid = <travel>-TravelID
            ) INTO TABLE association_links.

          IF result_requested = abap_true.
            APPEND CORRESPONDING #( travel MAPPING TO ENTITY ) TO result.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    SORT association_links BY source ASCENDING.
    DELETE ADJACENT DUPLICATES FROM association_links COMPARING ALL FIELDS.

    SORT result BY %tky ASCENDING.
    DELETE ADJACENT DUPLICATES FROM result COMPARING ALL FIELDS.
  ENDMETHOD.


***********************************************************************
*
* Create associated booking supplements
*
***********************************************************************
  METHOD cba_Booksupplement.
    DATA: message                   TYPE LINE OF /dmo/t_message,
          messages                  TYPE /dmo/t_message,
          booksupplements_old       TYPE /dmo/t_booking_supplement,
          booksupplement            TYPE /dmo/book_suppl,
          last_bookingsupplement_id TYPE /dmo/booking_supplement_id.

    " Loop at parent - booking
    LOOP AT entities_cba ASSIGNING FIELD-SYMBOL(<booking>).
      DATA(parent_key) = <booking>-%tky.

      " Retrieve booking supplements related to the imported travel ID
      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id          = parent_key-travelid
        IMPORTING
          et_booking_supplement = booksupplements_old
          et_messages           = messages.

      map_messages(
        EXPORTING
          cid        = <booking>-%cid_ref
          travel_id  = <booking>-travelid
          booking_id = <booking>-bookingid
          messages   = messages
        IMPORTING
          failed_added = DATA(failed_added)
        CHANGING
          failed   = failed-booking
          reported = reported-booking ).

      IF failed_added = abap_true.
        LOOP AT <booking>-%target ASSIGNING FIELD-SYMBOL(<bookingsupplement>).
          map_messages_assoc_to_booksupp(
            EXPORTING
              cid          = <bookingsupplement>-%cid
              is_dependend = abap_true
              messages     = messages
            CHANGING
              failed       = failed-bookingsupplement
              reported     = reported-bookingsupplement
          ).
        ENDLOOP.

      ELSE.

        " Look up for maximum booking supplement ID for a given travel/booking
        last_bookingsupplement_id = REDUCE #( INIT res TYPE /dmo/booking_supplement_id
                                              FOR old IN booksupplements_old
                                                USING KEY primary_key
                                                WHERE ( travel_id  = parent_key-travelid
                                                    AND booking_id = parent_key-bookingid )
                                              NEXT res = COND #( WHEN old-booking_supplement_id > res THEN old-booking_supplement_id ELSE res ) ).

        LOOP AT <booking>-%target ASSIGNING FIELD-SYMBOL(<supplement>).

          " Increase value of booking supplement ID with 1
          last_bookingsupplement_id += 1.

          " Do mapping between the element names of the CDS view and the original table fields
          booksupplement = CORRESPONDING #( <supplement> MAPPING FROM ENTITY USING CONTROL ).

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


          map_messages_assoc_to_booksupp(
            EXPORTING
              cid          = <supplement>-%cid
              is_dependend = abap_true
              messages     = messages
            IMPORTING
              failed_added = failed_added
            CHANGING
              failed       = failed-bookingsupplement
              reported     = reported-bookingsupplement
          ).

          IF failed_added = abap_false.
            INSERT VALUE #( %cid                = <supplement>-%cid
                            travelid            = parent_key-travelid
                            bookingid           = parent_key-bookingid
                            bookingsupplementid = booksupplement-booking_supplement_id )
                 INTO TABLE mapped-bookingsupplement.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


***********************************************************************
*
* Map messages
*
***********************************************************************
  METHOD map_messages.
    failed_added = abap_false.
    LOOP AT messages INTO DATA(message).
      IF message-msgty = 'E' OR message-msgty = 'A'.
        APPEND VALUE #( %cid        = cid
                        travelid    = travel_id
                        bookingid   = booking_id
                        %fail-cause = /dmo/cl_travel_auxiliary=>get_cause_from_message(
                                        msgid = message-msgid
                                        msgno = message-msgno
                                      ) )
            TO failed.
        failed_added = abap_true.
      ENDIF.

      APPEND VALUE #( %msg = new_message(
                                id       = message-msgid
                                number   = message-msgno
                                severity = if_abap_behv_message=>severity-error
                                v1       = message-msgv1
                                v2       = message-msgv2
                                v3       = message-msgv3
                                v4       = message-msgv4 )
                      %cid          = cid
                      TravelID      = travel_id
                      BookingID     = booking_id )
        TO reported.
    ENDLOOP.
  ENDMETHOD.

  METHOD map_messages_assoc_to_booksupp.
    ASSERT cid IS NOT INITIAL.  "In a create case, the %cid has to be present
    failed_added = abap_false.
    LOOP AT messages INTO DATA(message).
      IF message-msgty = 'E' OR message-msgty = 'A'.
        APPEND VALUE #( %cid        = cid
                        %fail-cause = /dmo/cl_travel_auxiliary=>get_cause_from_message(
                                        msgid        = message-msgid
                                        msgno        = message-msgno
                                        is_dependend = is_dependend
                                      ) )
            TO failed.
        failed_added = abap_true.
      ENDIF.

      APPEND VALUE #( %msg = new_message(
                                id       = message-msgid
                                number   = message-msgno
                                severity = if_abap_behv_message=>severity-error
                                v1       = message-msgv1
                                v2       = message-msgv2
                                v3       = message-msgv3
                                v4       = message-msgv4 )
                      %cid = cid )
        TO reported.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
