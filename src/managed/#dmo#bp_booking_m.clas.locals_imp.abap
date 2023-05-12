CLASS ltcl_booking DEFINITION DEFERRED FOR TESTING.
CLASS lhc_booking DEFINITION INHERITING FROM cl_abap_behavior_handler
  FRIENDS ltcl_booking.
  PRIVATE SECTION.

    METHODS calculatetotalprice FOR DETERMINE ON MODIFY
       IMPORTING keys FOR booking~calculatetotalprice.
    METHODS validateStatus FOR VALIDATE ON SAVE
       IMPORTING keys FOR booking~validatestatus.
    METHODS get_features FOR INSTANCE FEATURES
       IMPORTING keys REQUEST requested_features FOR booking RESULT result.
    METHODS validate_currencycode FOR VALIDATE ON SAVE
      IMPORTING keys FOR booking~validatecurrencycode.
    METHODS earlynumbering_cba_booksupplem FOR NUMBERING
       IMPORTING entities FOR CREATE booking\_booksupplement.




ENDCLASS.

CLASS lhc_booking IMPLEMENTATION.

  METHOD calculatetotalprice.
    DATA: travel_ids TYPE STANDARD TABLE OF /dmo/i_travel_m WITH UNIQUE HASHED KEY key COMPONENTS travel_id.

    travel_ids = CORRESPONDING #( keys DISCARDING DUPLICATES MAPPING travel_id = travel_id ).

    MODIFY ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY Travel
        EXECUTE ReCalcTotalPrice
        FROM CORRESPONDING #( travel_ids ).

  ENDMETHOD.



  METHOD validateStatus.

    READ ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
      ENTITY booking
        FIELDS ( booking_status )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).

    LOOP AT bookings INTO DATA(booking).
      CASE booking-booking_status.
        WHEN 'N'.  " New
        WHEN 'X'.  " Canceled
        WHEN 'B'.  " Booked

        WHEN OTHERS.
          APPEND VALUE #( %tky = booking-%tky ) TO failed-booking.

          APPEND VALUE #( %tky = booking-%tky
                          %msg = NEW /dmo/cm_flight_messages(
                                     textid = /dmo/cm_flight_messages=>status_invalid
                                     status = booking-booking_status
                                     severity = if_abap_behv_message=>severity-error )
                          %element-booking_status = if_abap_behv=>mk-on
                          %path = VALUE #(  travel-travel_id    = booking-travel_id )
                        ) TO reported-booking.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_features.

    READ ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
      ENTITY booking
         FIELDS ( booking_id booking_status )
         WITH CORRESPONDING #( keys )
      RESULT DATA(bookings)
      FAILED failed.

    result = VALUE #( FOR booking IN bookings
                       ( %tky                   = booking-%tky
                         %assoc-_booksupplement = COND #( WHEN booking-booking_status = 'B'
                                                          THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  ) ) ).

  ENDMETHOD.



  METHOD earlynumbering_cba_booksupplem.
    DATA: max_booking_suppl_id TYPE /dmo/booking_supplement_id .

    READ ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
      ENTITY booking BY \_booksupplement
        FROM CORRESPONDING #( entities )
        LINK DATA(booking_supplements).

    " Loop over all unique tky (TravelID + BookingID)
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<booking_group>) GROUP BY <booking_group>-%tky.

      " Get highest bookingsupplement_id from bookings belonging to booking
      max_booking_suppl_id = REDUCE #( INIT max = CONV /dmo/booking_supplement_id( '0' )
                                       FOR  booksuppl IN booking_supplements USING KEY entity
                                                                             WHERE (     source-travel_id  = <booking_group>-travel_id
                                                                                     AND source-booking_id = <booking_group>-booking_id )
                                       NEXT max = COND /dmo/booking_supplement_id( WHEN   booksuppl-target-booking_supplement_id > max
                                                                          THEN booksuppl-target-booking_supplement_id
                                                                          ELSE max )
                                     ).
      " Get highest assigned bookingsupplement_id from incoming entities
      max_booking_suppl_id = REDUCE #( INIT max = max_booking_suppl_id
                                       FOR  entity IN entities USING KEY entity
                                                               WHERE (     travel_id  = <booking_group>-travel_id
                                                                       AND booking_id = <booking_group>-booking_id )
                                       FOR  target IN entity-%target
                                       NEXT max = COND /dmo/booking_supplement_id( WHEN   target-booking_supplement_id > max
                                                                                     THEN target-booking_supplement_id
                                                                                     ELSE max )
                                     ).


      " Loop over all entries in entities with the same TravelID and BookingID
      LOOP AT entities ASSIGNING FIELD-SYMBOL(<booking>) USING KEY entity WHERE travel_id  = <booking_group>-travel_id
                                                                            AND booking_id = <booking_group>-booking_id.

        " Assign new booking_supplement-ids
        LOOP AT <booking>-%target ASSIGNING FIELD-SYMBOL(<booksuppl_wo_numbers>).
          APPEND CORRESPONDING #( <booksuppl_wo_numbers> ) TO mapped-booksuppl ASSIGNING FIELD-SYMBOL(<mapped_booksuppl>).
          IF <booksuppl_wo_numbers>-booking_supplement_id IS INITIAL.
            max_booking_suppl_id += 1 .
            <mapped_booksuppl>-booking_supplement_id = max_booking_suppl_id .
          ENDIF.
        ENDLOOP.

      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

  METHOD validate_currencycode.
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY booking
        FIELDS ( currency_code )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).

    DATA: currencies TYPE SORTED TABLE OF I_Currency WITH UNIQUE KEY currency.

    currencies = CORRESPONDING #( bookings DISCARDING DUPLICATES MAPPING currency = currency_code EXCEPT * ).
    DELETE currencies WHERE currency IS INITIAL.

    IF currencies IS NOT INITIAL.
      SELECT FROM I_Currency FIELDS currency
        FOR ALL ENTRIES IN @currencies
        WHERE currency = @currencies-currency
        INTO TABLE @DATA(currency_db).
    ENDIF.


    LOOP AT bookings INTO DATA(booking).
      IF booking-currency_code IS INITIAL.
        " Raise message for empty Currency
        APPEND VALUE #( %tky                   = booking-%tky ) TO failed-booking.
        APPEND VALUE #( %tky                   = booking-%tky
                        %msg                   = NEW /dmo/cm_flight_messages(
                                                        textid    = /dmo/cm_flight_messages=>currency_required
                                                        severity  = if_abap_behv_message=>severity-error )
                        %element-currency_code = if_abap_behv=>mk-on
                        %path                  = VALUE #(  travel-travel_id    = booking-travel_id )
                      ) TO reported-booking.
      ELSEIF NOT line_exists( currency_db[ currency = booking-currency_code ] ).
        " Raise message for not existing Currency
        APPEND VALUE #( %tky                   = booking-%tky ) TO failed-booking.
        APPEND VALUE #( %tky                   = booking-%tky
                        %msg                   = NEW /dmo/cm_flight_messages(
                                                        textid    = /dmo/cm_flight_messages=>currency_not_existing
                                                        severity  = if_abap_behv_message=>severity-error
                                                        currency_code = booking-currency_code )
                        %path                  = VALUE #(  travel-travel_id    = booking-travel_id )
                        %element-currency_code = if_abap_behv=>mk-on
                      ) TO reported-booking.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
