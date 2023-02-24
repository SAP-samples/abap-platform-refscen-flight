CLASS ltc_BookingSupplement DEFINITION DEFERRED FOR TESTING.
CLASS lhc_BookingSupplement DEFINITION INHERITING FROM cl_abap_behavior_handler
 FRIENDS ltc_BookingSupplement.

  PRIVATE SECTION.

    METHODS setBookSupplNumber FOR DETERMINE ON SAVE
      IMPORTING keys FOR BookingSupplement~setBookSupplNumber.
    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR BookingSupplement~calculateTotalPrice.

    METHODS validateSupplement FOR VALIDATE ON SAVE
      IMPORTING keys FOR BookingSupplement~validateSupplement.
    METHODS validateCurrencyCode FOR VALIDATE ON SAVE
      IMPORTING keys FOR BookingSupplement~validateCurrencyCode.

ENDCLASS.

CLASS lhc_bookingsupplement IMPLEMENTATION.

  METHOD setBookSupplNumber.
    DATA:
      max_bookingsupplementid   TYPE /dmo/booking_supplement_id,
      bookingsupplements_update TYPE TABLE FOR UPDATE /DMO/R_Travel_D\\BookingSupplement,
      bookingsupplement         TYPE STRUCTURE FOR READ RESULT /DMO/R_BookingSupplement_D.

    "Read all bookings for the requested booking supplements
    " If multiple booking supplements of the same booking are requested, the booking is returned only once.
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Booking
        FIELDS (  BookingUUID  )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).

    " Read all booking supplements for the affected bookings
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Booking BY \_BookingSupplement
        FIELDS ( BookingSupplementID )
        WITH CORRESPONDING #( bookings )
        LINK DATA(bookingsupplement_links)
      RESULT DATA(bookingsupplements).

    " Process all affected bookings.
    LOOP AT bookings INTO DATA(booking).

      " find max used booking supplement ID in all booking supplements of this booking
      max_bookingsupplementid = '00'.
      LOOP AT bookingsupplement_links INTO DATA(bookingsupplement_link) USING KEY id WHERE source-%tky = booking-%tky.
        bookingsupplement = bookingsupplements[ KEY id %tky = bookingsupplement_link-target-%tky ].
        IF bookingsupplement-BookingSupplementID > max_bookingsupplementid.
          max_bookingsupplementid = bookingsupplement-BookingSupplementID.
        ENDIF.
      ENDLOOP.

      "Provide a booking supplement ID for all booking supplements of this booking that have none.
      LOOP AT bookingsupplement_links INTO bookingsupplement_link USING KEY id WHERE source-%tky = booking-%tky.
        bookingsupplement = bookingsupplements[ KEY id %tky = bookingsupplement_link-target-%tky ].
        IF bookingsupplement-BookingSupplementID IS INITIAL.
          max_bookingsupplementid += 1.
          APPEND VALUE #( %tky                = bookingsupplement-%tky
                          bookingsupplementid = max_bookingsupplementid
                        ) TO bookingsupplements_update.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " Provide a booking supplement ID for all booking supplements that have none.
    MODIFY ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement
        UPDATE FIELDS ( BookingSupplementID ) WITH bookingsupplements_update.

  ENDMETHOD.

  METHOD calculateTotalPrice.
    " Read all parent UUIDs
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Travel
        FIELDS ( TravelUUID  )
        WITH CORRESPONDING #(  keys  )
      RESULT DATA(travels).

    " Trigger Re-Calculation on Root Node
    MODIFY ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        EXECUTE reCalcTotalPrice
          FROM CORRESPONDING  #( travels ).

  ENDMETHOD.

  METHOD validateSupplement.

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement
        FIELDS ( SupplementID )
        WITH CORRESPONDING #(  keys )
      RESULT DATA(bookingsupplements)
      FAILED DATA(read_failed).

    failed = CORRESPONDING #( DEEP read_failed ).

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Booking
        FROM CORRESPONDING #( bookingsupplements )
      LINK DATA(booksuppl_booking_links).

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Travel
        FROM CORRESPONDING #( bookingsupplements )
      LINK DATA(booksuppl_travel_links).


    DATA supplements TYPE SORTED TABLE OF /dmo/supplement WITH UNIQUE KEY supplement_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    supplements = CORRESPONDING #( bookingsupplements DISCARDING DUPLICATES MAPPING supplement_id = SupplementID EXCEPT * ).
    DELETE supplements WHERE supplement_id IS INITIAL.

    IF  supplements IS NOT INITIAL.
      " Check if customer ID exists
      SELECT FROM /dmo/supplement FIELDS supplement_id
                                  FOR ALL ENTRIES IN @supplements
                                  WHERE supplement_id = @supplements-supplement_id
      INTO TABLE @DATA(valid_supplements).
    ENDIF.

    LOOP AT bookingsupplements ASSIGNING FIELD-SYMBOL(<bookingsupplement>).

      APPEND VALUE #(  %tky        = <bookingsupplement>-%tky
                       %state_area = 'VALIDATE_SUPPLEMENT'
                    ) TO reported-bookingsupplement.

      IF <bookingsupplement>-SupplementID IS  INITIAL.
        APPEND VALUE #( %tky = <bookingsupplement>-%tky ) TO failed-bookingsupplement.

        APPEND VALUE #( %tky                  = <bookingsupplement>-%tky
                        %state_area           = 'VALIDATE_SUPPLEMENT'
                        %msg                  = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>enter_supplement_id
                                                                severity = if_abap_behv_message=>severity-error )
                        %path                 = VALUE #( booking-%tky = booksuppl_booking_links[ KEY id  source-%tky = <bookingsupplement>-%tky ]-target-%tky
                                                         travel-%tky  = booksuppl_travel_links[  KEY id  source-%tky = <bookingsupplement>-%tky ]-target-%tky )
                        %element-SupplementID = if_abap_behv=>mk-on
                       ) TO reported-bookingsupplement.


      ELSEIF <bookingsupplement>-SupplementID IS NOT INITIAL AND NOT line_exists( valid_supplements[ supplement_id = <bookingsupplement>-SupplementID ] ).
        APPEND VALUE #(  %tky = <bookingsupplement>-%tky ) TO failed-bookingsupplement.

        APPEND VALUE #( %tky                  = <bookingsupplement>-%tky
                        %state_area           = 'VALIDATE_SUPPLEMENT'
                        %msg                  = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>supplement_unknown
                                                                severity = if_abap_behv_message=>severity-error )
                        %path                 = VALUE #( booking-%tky = booksuppl_booking_links[ KEY id  source-%tky = <bookingsupplement>-%tky ]-target-%tky
                                                          travel-%tky = booksuppl_travel_links[  KEY id  source-%tky = <bookingsupplement>-%tky ]-target-%tky )
                        %element-SupplementID = if_abap_behv=>mk-on
                       ) TO reported-bookingsupplement.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD validateCurrencyCode.
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement
        FIELDS ( currencycode )
        WITH CORRESPONDING #( keys )
      RESULT DATA(booking_supplements).

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Booking
        FROM CORRESPONDING #( booking_supplements )
      LINK DATA(booksuppl_booking_links).

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Travel
        FROM CORRESPONDING #( booking_supplements )
      LINK DATA(booksuppl_travel_links).

    DATA: currencies TYPE SORTED TABLE OF I_Currency WITH UNIQUE KEY currency.

    currencies = CORRESPONDING #( booking_supplements DISCARDING DUPLICATES MAPPING currency = currencycode EXCEPT * ).
    DELETE currencies WHERE currency IS INITIAL.

    IF currencies IS NOT INITIAL.
      SELECT FROM I_Currency FIELDS currency
        FOR ALL ENTRIES IN @currencies
        WHERE currency = @currencies-currency
        INTO TABLE @DATA(currency_db).
    ENDIF.


    LOOP AT booking_supplements INTO DATA(booking_supplement).
      APPEND VALUE #(  %tky               = booking_supplement-%tky
                       %state_area        = 'VALIDATE_CURRENCYCODE'
                    ) TO reported-bookingsupplement.
      IF booking_supplement-currencycode IS INITIAL.
        " Raise message for empty Currency
        APPEND VALUE #( %tky                   = booking_supplement-%tky ) TO failed-bookingsupplement.
        APPEND VALUE #( %tky                   = booking_supplement-%tky
                        %state_area            = 'VALIDATE_CURRENCYCODE'
                        %msg                   = NEW /dmo/cm_flight_messages(
                                                        textid    = /dmo/cm_flight_messages=>currency_required
                                                        severity  = if_abap_behv_message=>severity-error )
                        %path                 = VALUE #( booking-%tky = booksuppl_booking_links[ KEY id  source-%tky = booking_supplement-%tky ]-target-%tky
                                                         travel-%tky  = booksuppl_travel_links[  KEY id  source-%tky = booking_supplement-%tky ]-target-%tky )
                        %element-currencycode = if_abap_behv=>mk-on
                      ) TO reported-bookingsupplement.
      ELSEIF NOT line_exists( currency_db[ currency = booking_supplement-currencycode ] ).
        " Raise message for not existing Currency
        APPEND VALUE #( %tky                   = booking_supplement-%tky ) TO failed-bookingsupplement.
        APPEND VALUE #( %tky                   = booking_supplement-%tky
                        %state_area            = 'VALIDATE_CURRENCYCODE'
                        %msg                   = NEW /dmo/cm_flight_messages(
                                                        textid        = /dmo/cm_flight_messages=>currency_not_existing
                                                        severity      = if_abap_behv_message=>severity-error )
                        %path                 = VALUE #( booking-%tky = booksuppl_booking_links[ KEY id  source-%tky = booking_supplement-%tky ]-target-%tky
                                                         travel-%tky  = booksuppl_travel_links[  KEY id  source-%tky = booking_supplement-%tky ]-target-%tky )
                        %element-currencycode = if_abap_behv=>mk-on
                      ) TO reported-bookingsupplement.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
