CLASS ltcl_booking DEFINITION DEFERRED FOR TESTING.
CLASS lhc_booking DEFINITION INHERITING FROM cl_abap_behavior_handler
  FRIENDS ltcl_booking.
  PRIVATE SECTION.

    METHODS calculatetotalprice FOR DETERMINE ON MODIFY
       IMPORTING keys FOR booking~calculatetotalprice.
    METHODS validateStatus FOR VALIDATE ON SAVE
       IMPORTING keys FOR booking~validatestatus.
    METHODS validate_customer FOR VALIDATE ON SAVE
      IMPORTING keys FOR booking~validateCustomer.
    METHODS validate_connection FOR VALIDATE ON SAVE
      IMPORTING keys FOR booking~validateConnection.
    METHODS get_features FOR INSTANCE FEATURES
       IMPORTING keys REQUEST requested_features FOR booking RESULT result.
    METHODS validate_currencycode FOR VALIDATE ON SAVE
      IMPORTING keys FOR booking~validatecurrencycode.
    METHODS validate_flight_price FOR VALIDATE ON SAVE
      IMPORTING keys FOR booking~validateFlightPrice.
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

  METHOD validate_customer.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY Booking
        FIELDS ( customer_id )
        WITH CORRESPONDING #( keys )
    RESULT DATA(bookings).

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY Booking BY \_Travel
        FROM CORRESPONDING #( bookings )
      LINK DATA(travel_booking_links).

    DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    customers = CORRESPONDING #( bookings DISCARDING DUPLICATES MAPPING customer_id = customer_id EXCEPT * ).
    DELETE customers WHERE customer_id IS INITIAL.

    IF  customers IS NOT INITIAL.
      " Check if customer ID exists
      SELECT FROM /dmo/customer FIELDS customer_id
                                FOR ALL ENTRIES IN @customers
                                WHERE customer_id = @customers-customer_id
      INTO TABLE @DATA(valid_customers).
    ENDIF.

    " Raise message for non existing and initial customer id
    LOOP AT bookings INTO DATA(booking).
      IF booking-customer_id IS  INITIAL.

        APPEND VALUE #( %tky = booking-%tky ) TO failed-booking.
        APPEND VALUE #( %tky                 = booking-%tky
                        %msg                 = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>enter_customer_id
                                                                severity = if_abap_behv_message=>severity-error )
                        %path                = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = booking-%tky ]-target-%tky )
                        %element-customer_id = if_abap_behv=>mk-on
                       ) TO reported-booking.

      ELSEIF booking-customer_id IS NOT INITIAL AND NOT line_exists( valid_customers[ customer_id = booking-customer_id ] ).

        APPEND VALUE #( %tky = booking-%tky ) TO failed-booking.
        APPEND VALUE #( %tky                 = booking-%tky
                        %msg                 = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>customer_unkown
                                                                customer_id = booking-customer_id
                                                                severity = if_abap_behv_message=>severity-error )
                        %path                = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = booking-%tky ]-target-%tky )
                        %element-customer_id = if_abap_behv=>mk-on
                       ) TO reported-booking.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD validate_connection.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY Booking
        FIELDS ( booking_id carrier_id connection_id flight_date )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY Booking BY \_Travel
        FROM CORRESPONDING #( bookings )
      LINK DATA(travel_booking_links).

    LOOP AT bookings ASSIGNING FIELD-SYMBOL(<booking>).
      " Raise message for non existing airline ID
      IF <booking>-carrier_id IS INITIAL.

        APPEND VALUE #( %tky = <booking>-%tky ) TO failed-booking.
        APPEND VALUE #( %tky                = <booking>-%tky
                        %msg                = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>enter_airline_id
                                                                severity = if_abap_behv_message=>severity-error )
                        %path               = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = <booking>-%tky ]-target-%tky )
                        %element-carrier_id = if_abap_behv=>mk-on
                       ) TO reported-booking.
      ENDIF.
      " Raise message for non existing connection ID
      IF <booking>-connection_id IS INITIAL.

        APPEND VALUE #( %tky = <booking>-%tky ) TO failed-booking.
        APPEND VALUE #( %tky                   = <booking>-%tky
                        %msg                   = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>enter_connection_id
                                                                severity = if_abap_behv_message=>severity-error )
                        %path                  = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = <booking>-%tky ]-target-%tky )
                        %element-connection_id = if_abap_behv=>mk-on
                       ) TO reported-booking.
      ENDIF.
      " Raise message for non existing flight date
      IF <booking>-flight_date IS INITIAL.

        APPEND VALUE #( %tky = <booking>-%tky ) TO failed-booking.
        APPEND VALUE #( %tky                 = <booking>-%tky
                        %msg                 = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>enter_flight_date
                                                                severity = if_abap_behv_message=>severity-error )
                        %path                = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = <booking>-%tky ]-target-%tky )
                        %element-flight_date = if_abap_behv=>mk-on
                       ) TO reported-booking.
      ENDIF.
      " check if flight connection exists
      IF <booking>-carrier_id IS NOT INITIAL AND
         <booking>-connection_id IS NOT INITIAL AND
         <booking>-flight_date IS NOT INITIAL.

        SELECT SINGLE Carrier_ID, Connection_ID, Flight_Date   FROM /dmo/flight  WHERE  carrier_id    = @<booking>-carrier_id
                                                               AND  connection_id = @<booking>-connection_id
                                                               AND  flight_date   = @<booking>-flight_date
                                                               INTO  @DATA(flight).

        IF sy-subrc <> 0.

          APPEND VALUE #( %tky = <booking>-%tky ) TO failed-booking.
          APPEND VALUE #( %tky                   = <booking>-%tky
                          %msg                   = NEW /dmo/cm_flight_messages(
                                                                textid      = /dmo/cm_flight_messages=>no_flight_exists
                                                                carrier_id  = <booking>-carrier_id
                                                                flight_date = <booking>-flight_date
                                                                severity    = if_abap_behv_message=>severity-error )
                          %path                  = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = <booking>-%tky ]-target-%tky )
                          %element-flight_date   = if_abap_behv=>mk-on
                          %element-carrier_id    = if_abap_behv=>mk-on
                          %element-connection_id = if_abap_behv=>mk-on
                        ) TO reported-booking.

        ENDIF.

      ENDIF.

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

  METHOD validate_flight_price.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY booking
        FIELDS ( flight_price )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY Booking BY \_Travel
        FROM CORRESPONDING #( bookings )
      LINK DATA(travel_booking_links).

    LOOP AT bookings INTO DATA(booking) WHERE flight_price < 0.
      " Raise message for flight price < 0
      APPEND VALUE #( %tky                  = booking-%tky ) TO failed-booking.
      APPEND VALUE #( %tky                  = booking-%tky
                      %msg                  = NEW /dmo/cm_flight_messages(
                                                     textid      = /dmo/cm_flight_messages=>flight_price_invalid
                                                     severity    = if_abap_behv_message=>severity-error )
                      %element-flight_price = if_abap_behv=>mk-on
                      %path                 = VALUE #( travel-%tky = travel_booking_links[ KEY id source-%tky = booking-%tky ]-target-%tky )
                    ) TO reported-booking.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
