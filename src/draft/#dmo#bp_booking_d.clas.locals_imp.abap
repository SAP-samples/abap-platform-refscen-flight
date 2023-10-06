CLASS ltc_booking DEFINITION DEFERRED FOR TESTING.
CLASS lhc_booking DEFINITION INHERITING FROM cl_abap_behavior_handler
 FRIENDS ltc_booking.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF booking_status,
        new    TYPE c LENGTH 1 VALUE 'N', "New
        booked TYPE c LENGTH 1 VALUE 'B', "Booked
      END OF booking_status.

    METHODS setBookingNumber FOR DETERMINE ON SAVE
      IMPORTING keys FOR Booking~setBookingNumber.
    METHODS setBookingDate FOR DETERMINE ON SAVE
      IMPORTING keys FOR Booking~setBookingDate.
    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Booking~calculateTotalPrice.

    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Booking~validateCustomer.
    METHODS validateConnection FOR VALIDATE ON SAVE
      IMPORTING keys FOR Booking~validateConnection.
    METHODS validateCurrencyCode FOR VALIDATE ON SAVE
      IMPORTING keys FOR Booking~validateCurrencyCode.

ENDCLASS.

CLASS lhc_booking IMPLEMENTATION.

  METHOD setBookingNumber.
    DATA:
      max_bookingid   TYPE /dmo/booking_id,
      bookings_update TYPE TABLE FOR UPDATE /DMO/R_Travel_D\\Booking,
      booking         TYPE STRUCTURE FOR READ RESULT /DMO/R_Booking_D.

    "Read all travels for the requested bookings
    " If multiple bookings of the same travel are requested, the travel is returned only once.
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Booking BY \_Travel
        FIELDS ( TravelUUID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    " Read all bookings for all affected travels
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel BY \_Booking
        FIELDS ( BookingID )
        WITH CORRESPONDING #( travels )
        LINK DATA(booking_links)
      RESULT DATA(bookings).

    " Process all affected travels.
    LOOP AT travels INTO DATA(travel).

      " find max used bookingID in all bookings of this travel
      max_bookingid = '0000'.
      LOOP AT booking_links INTO DATA(booking_link) USING KEY id WHERE source-%tky = travel-%tky.
        " Short dump occurs if link table does not match read table, which must never happen
        booking = bookings[ KEY id  %tky = booking_link-target-%tky ].
        IF booking-BookingID > max_bookingid.
          max_bookingid = booking-BookingID.
        ENDIF.
      ENDLOOP.

      "Provide a booking ID for all bookings of this travel that have none.
      LOOP AT booking_links INTO booking_link USING KEY id WHERE source-%tky = travel-%tky.
        " Short dump occurs if link table does not match read table, which must never happen
        booking = bookings[ KEY id  %tky = booking_link-target-%tky ].
        IF booking-BookingID IS INITIAL.
          max_bookingid += 1.
          APPEND VALUE #( %tky      = booking-%tky
                          BookingID = max_bookingid
                        ) TO bookings_update.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " Provide a booking ID for all bookings that have none.
    MODIFY ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY booking
        UPDATE FIELDS ( BookingID )
        WITH bookings_update.

  ENDMETHOD.

  METHOD setBookingDate.

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Booking
        FIELDS ( BookingDate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).

    DELETE bookings WHERE BookingDate IS NOT INITIAL.
    CHECK bookings IS NOT INITIAL.

    LOOP AT bookings ASSIGNING FIELD-SYMBOL(<booking>).
      <booking>-BookingDate = cl_abap_context_info=>get_system_date( ).
    ENDLOOP.

    MODIFY ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Booking
        UPDATE  FIELDS ( BookingDate )
        WITH CORRESPONDING #( bookings ).

  ENDMETHOD.

  METHOD calculateTotalPrice.

    " Read all parent UUIDs
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Booking BY \_Travel
        FIELDS ( TravelUUID  )
        WITH CORRESPONDING #(  keys  )
      RESULT DATA(travels).

    " Trigger Re-Calculation on Root Node
    MODIFY ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        EXECUTE reCalcTotalPrice
          FROM CORRESPONDING  #( travels ).

  ENDMETHOD.


  METHOD validateCustomer.

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Booking
        FIELDS (  CustomerID )
        WITH CORRESPONDING #( keys )
    RESULT DATA(bookings).

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Booking BY \_Travel
        FROM CORRESPONDING #( bookings )
      LINK DATA(travel_booking_links).

    DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    customers = CORRESPONDING #( bookings DISCARDING DUPLICATES MAPPING customer_id = CustomerID EXCEPT * ).
    DELETE customers WHERE customer_id IS INITIAL.

    IF  customers IS NOT INITIAL.
      " Check if customer ID exists
      SELECT FROM /dmo/customer FIELDS customer_id
                                FOR ALL ENTRIES IN @customers
                                WHERE customer_id = @customers-customer_id
      INTO TABLE @DATA(valid_customers).
    ENDIF.

    " Raise message for non existing customer id
    LOOP AT bookings INTO DATA(booking).
      APPEND VALUE #(  %tky               = booking-%tky
                       %state_area        = 'VALIDATE_CUSTOMER' ) TO reported-booking.

      IF booking-CustomerID IS  INITIAL.
        APPEND VALUE #( %tky = booking-%tky ) TO failed-booking.

        APPEND VALUE #( %tky                = booking-%tky
                        %state_area         = 'VALIDATE_CUSTOMER'
                         %msg                = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>enter_customer_id
                                                                severity = if_abap_behv_message=>severity-error )
                        %path               = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = booking-%tky ]-target-%tky )
                        %element-CustomerID = if_abap_behv=>mk-on
                       ) TO reported-booking.

      ELSEIF booking-CustomerID IS NOT INITIAL AND NOT line_exists( valid_customers[ customer_id = booking-CustomerID ] ).
        APPEND VALUE #(  %tky = booking-%tky ) TO failed-booking.

        APPEND VALUE #( %tky                = booking-%tky
                        %state_area         = 'VALIDATE_CUSTOMER'
                         %msg                = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>customer_unkown
                                                                customer_id = booking-customerId
                                                                severity = if_abap_behv_message=>severity-error )
                        %path               = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = booking-%tky ]-target-%tky )
                        %element-CustomerID = if_abap_behv=>mk-on
                       ) TO reported-booking.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD validateConnection.

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Booking
        FIELDS ( BookingID AirlineID ConnectionID FlightDate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Booking BY \_Travel
        FROM CORRESPONDING #( bookings )
      LINK DATA(travel_booking_links).

    LOOP AT bookings ASSIGNING FIELD-SYMBOL(<booking>).
      "overwrite state area with empty message to avoid duplicate messages
      APPEND VALUE #(  %tky               = <booking>-%tky
                       %state_area        = 'VALIDATE_CONNECTION' ) TO reported-booking.

      " Raise message for non existing airline ID
      IF <booking>-AirlineID IS INITIAL.
        APPEND VALUE #( %tky = <booking>-%tky ) TO failed-booking.

        APPEND VALUE #( %tky                = <booking>-%tky
                        %state_area         = 'VALIDATE_CONNECTION'
                         %msg                = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>enter_airline_id
                                                                severity = if_abap_behv_message=>severity-error )
                        %path              = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = <booking>-%tky ]-target-%tky )
                        %element-AirlineID = if_abap_behv=>mk-on
                       ) TO reported-booking.
      ENDIF.
      " Raise message for non existing connection ID
      IF <booking>-ConnectionID IS INITIAL.
        APPEND VALUE #( %tky = <booking>-%tky ) TO failed-booking.

        APPEND VALUE #( %tky                = <booking>-%tky
                        %state_area         = 'VALIDATE_CONNECTION'
                        %msg                = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>enter_connection_id
                                                                severity = if_abap_behv_message=>severity-error )
                        %path               = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = <booking>-%tky ]-target-%tky )
                        %element-ConnectionID = if_abap_behv=>mk-on
                       ) TO reported-booking.
      ENDIF.
      " Raise message for non existing flight date
      IF <booking>-FlightDate IS INITIAL.
        APPEND VALUE #( %tky = <booking>-%tky ) TO failed-booking.

        APPEND VALUE #( %tky                = <booking>-%tky
                        %state_area         = 'VALIDATE_CONNECTION'
                        %msg                = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>enter_flight_date
                                                                severity = if_abap_behv_message=>severity-error )
                        %path               = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = <booking>-%tky ]-target-%tky )
                        %element-FlightDate = if_abap_behv=>mk-on
                       ) TO reported-booking.
      ENDIF.
      " check if flight connection exists
      IF <booking>-AirlineID IS NOT INITIAL AND
         <booking>-ConnectionID IS NOT INITIAL AND
         <booking>-FlightDate IS NOT INITIAL.

        SELECT SINGLE Carrier_ID, Connection_ID, Flight_Date   FROM /dmo/flight  WHERE  carrier_id    = @<booking>-AirlineID
                                                               AND  connection_id = @<booking>-ConnectionID
                                                               AND  flight_date   = @<booking>-FlightDate
                                                               INTO  @DATA(flight).

        IF sy-subrc <> 0.
          APPEND VALUE #( %tky = <booking>-%tky ) TO failed-booking.

          APPEND VALUE #( %tky                 = <booking>-%tky
                          %state_area          = 'VALIDATE_CONNECTION'
                          %msg                 = NEW /dmo/cm_flight_messages(
                                                                textid      = /dmo/cm_flight_messages=>no_flight_exists
                                                                carrier_id  = <booking>-AirlineID
                                                                flight_date = <booking>-FlightDate
                                                                severity    = if_abap_behv_message=>severity-error )
                          %path                  = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = <booking>-%tky ]-target-%tky )
                          %element-FlightDate    = if_abap_behv=>mk-on
                          %element-AirlineID     = if_abap_behv=>mk-on
                          %element-ConnectionID  = if_abap_behv=>mk-on
                        ) TO reported-booking.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD validateCurrencyCode.
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY booking
        FIELDS ( currencycode )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).

      READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Booking BY \_Travel
        FROM CORRESPONDING #( bookings )
      LINK DATA(travel_booking_links).

    DATA: currencies TYPE SORTED TABLE OF I_Currency WITH UNIQUE KEY currency.

    currencies = CORRESPONDING #( bookings DISCARDING DUPLICATES MAPPING currency = currencycode EXCEPT * ).
    DELETE currencies WHERE currency IS INITIAL.

    IF currencies IS NOT INITIAL.
      SELECT FROM I_Currency FIELDS currency
        FOR ALL ENTRIES IN @currencies
        WHERE currency = @currencies-currency
        INTO TABLE @DATA(currency_db).
    ENDIF.


    LOOP AT bookings INTO DATA(booking).
      APPEND VALUE #(  %tky               = booking-%tky
                       %state_area        = 'VALIDATE_CURRENCYCODE'
                    ) TO reported-booking.
      IF booking-currencycode IS INITIAL.
        " Raise message for empty Currency
        APPEND VALUE #( %tky                   = booking-%tky ) TO failed-booking.
        APPEND VALUE #( %tky                   = booking-%tky
                        %state_area            = 'VALIDATE_CURRENCYCODE'
                        %msg                   = NEW /dmo/cm_flight_messages(
                                                        textid    = /dmo/cm_flight_messages=>currency_required
                                                        severity  = if_abap_behv_message=>severity-error )
                        %path                  = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = booking-%tky ]-target-%tky )
                        %element-currencycode = if_abap_behv=>mk-on
                      ) TO reported-booking.
      ELSEIF NOT line_exists( currency_db[ currency = booking-currencycode ] ).
        " Raise message for not existing Currency
        APPEND VALUE #( %tky                   = booking-%tky ) TO failed-booking.
        APPEND VALUE #( %tky                   = booking-%tky
                        %state_area            = 'VALIDATE_CURRENCYCODE'
                        %msg                   = NEW /dmo/cm_flight_messages(
                                                        textid        = /dmo/cm_flight_messages=>currency_not_existing
                                                        severity      = if_abap_behv_message=>severity-error
                                                        currency_code = booking-currencycode )
                        %path                  = VALUE #( travel-%tky = travel_booking_links[ KEY id  source-%tky = booking-%tky ]-target-%tky )
                        %element-currencycode = if_abap_behv=>mk-on
                      ) TO reported-booking.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
