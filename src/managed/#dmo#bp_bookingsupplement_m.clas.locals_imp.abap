CLASS ltcl_bookingsupplement DEFINITION DEFERRED FOR TESTING.
CLASS lhc_bookingsupplement DEFINITION INHERITING FROM cl_abap_behavior_handler
  FRIENDS ltcl_bookingsupplement.
  PRIVATE SECTION.

    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY IMPORTING keys FOR booksuppl~calculateTotalPrice.
    METHODS validate_currencycode FOR VALIDATE ON SAVE
      IMPORTING keys FOR booksuppl~validateCurrencyCode.

ENDCLASS.

CLASS lhc_bookingsupplement IMPLEMENTATION.

  METHOD calculateTotalPrice.
    DATA: travel_ids TYPE STANDARD TABLE OF /dmo/i_travel_m WITH UNIQUE HASHED KEY key COMPONENTS travel_id.

    travel_ids = CORRESPONDING #( keys DISCARDING DUPLICATES MAPPING travel_id = travel_id ).

    MODIFY ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY Travel
        EXECUTE ReCalcTotalPrice
        FROM CORRESPONDING #( travel_ids ).
  ENDMETHOD.

  METHOD validate_currencycode.
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY booksuppl
        FIELDS ( currency_code )
        WITH CORRESPONDING #( keys )
      RESULT DATA(booking_supplements).

    DATA: currencies TYPE SORTED TABLE OF I_Currency WITH UNIQUE KEY currency.

    currencies = CORRESPONDING #( booking_supplements DISCARDING DUPLICATES MAPPING currency = currency_code EXCEPT * ).
    DELETE currencies WHERE currency IS INITIAL.

    IF currencies IS NOT INITIAL.
      SELECT FROM I_Currency FIELDS currency
        FOR ALL ENTRIES IN @currencies
        WHERE currency = @currencies-currency
        INTO TABLE @DATA(currency_db).
    ENDIF.


    LOOP AT booking_supplements INTO DATA(booking_supplement).
      IF booking_supplement-currency_code IS INITIAL.
        " Raise message for empty Currency
        APPEND VALUE #( %tky                   = booking_supplement-%tky ) TO failed-booksuppl.
        APPEND VALUE #( %tky                   = booking_supplement-%tky
                        %msg                   = NEW /dmo/cm_flight_messages(
                                                        textid    = /dmo/cm_flight_messages=>currency_required
                                                        severity  = if_abap_behv_message=>severity-error )
                        %element-currency_code = if_abap_behv=>mk-on
                        %path                  = VALUE #( travel-travel_id    = booking_supplement-travel_id
                                                          booking-booking_id  = booking_supplement-booking_id )
                      ) TO reported-booksuppl.
      ELSEIF NOT line_exists( currency_db[ currency = booking_supplement-currency_code ] ).
        " Raise message for not existing Currency
        APPEND VALUE #( %tky                   = booking_supplement-%tky ) TO failed-booksuppl.
        APPEND VALUE #( %tky                   = booking_supplement-%tky
                        %msg                   = NEW /dmo/cm_flight_messages(
                                                        textid        = /dmo/cm_flight_messages=>currency_not_existing
                                                        severity      = if_abap_behv_message=>severity-error
                                                        currency_code = booking_supplement-currency_code )
                        %element-currency_code = if_abap_behv=>mk-on
                      ) TO reported-booksuppl.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
