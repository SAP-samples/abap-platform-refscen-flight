CLASS lthc_carrier DEFINITION DEFERRED FOR TESTING.
CLASS lhc_carrier DEFINITION
  INHERITING FROM cl_abap_behavior_handler
  FRIENDS lthc_carrier
  .

  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Carrier RESULT result.

    METHODS validatecurrencycode FOR VALIDATE ON SAVE
      IMPORTING keys FOR carrier~validatecurrencycode.

    METHODS validatename FOR VALIDATE ON SAVE
      IMPORTING keys FOR carrier~validatename.

ENDCLASS.

CLASS lhc_carrier IMPLEMENTATION.

  METHOD get_instance_features.

    DATA: airline_ids TYPE SORTED TABLE OF /dmo/i_carrier WITH UNIQUE KEY AirlineID.

    READ ENTITIES OF /DMO/I_CarriersLockSingleton_S IN LOCAL MODE
      ENTITY Carrier
        FIELDS ( AirlineID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(Carrier)
      FAILED failed.

    airline_ids = CORRESPONDING #( Carrier DISCARDING DUPLICATES MAPPING AirlineID = AirlineID EXCEPT * ).

    IF airline_ids IS NOT INITIAL.
      SELECT DISTINCT AirlineID
      FROM /DMO/I_Connection
        FOR ALL ENTRIES IN @airline_ids
        WHERE AirlineID = @airline_ids-AirlineID
        INTO TABLE @DATA(connections_db).
    ENDIF.

    LOOP AT Carrier ASSIGNING FIELD-SYMBOL(<carrier>).
      " Delete is not allowed if any Connection exists
      IF line_exists( connections_db[ AirlineID = <carrier>-AirlineID ] ).
        APPEND VALUE #( %tky    = <carrier>-%tky
                        %delete = if_abap_behv=>fc-o-disabled ) TO result.
        APPEND VALUE #( %tky                        = <carrier>-%tky
                        %msg                        = NEW /dmo/cx_carriers_s(
                                                       textid        = /dmo/cx_carriers_s=>airline_still_used
                                                       severity      = if_abap_behv_message=>severity-information
                                                       airline_id    = <carrier>-AirlineID )
                        %delete                     = if_abap_behv=>mk-on
                        %path-carrierslocksingleton = CORRESPONDING #( <carrier> )
                      ) TO reported-carrier.
      ELSE.
        APPEND VALUE #( %tky    = <carrier>-%tky
                        %delete = if_abap_behv=>fc-o-enabled ) TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD validateCurrencyCode.

    DATA: currency_codes TYPE SORTED TABLE OF I_Currency WITH UNIQUE KEY Currency.

    READ ENTITIES OF /DMO/I_CarriersLockSingleton_S IN LOCAL MODE
      ENTITY Carrier
        FIELDS ( CarrierSingletonID CurrencyCode )
        WITH CORRESPONDING #( keys )
      RESULT DATA(Carrier).

    currency_codes = CORRESPONDING #( Carrier DISCARDING DUPLICATES MAPPING Currency = CurrencyCode EXCEPT * ).

    IF currency_codes IS NOT INITIAL.
      SELECT FROM I_Currency FIELDS Currency
        FOR ALL ENTRIES IN @currency_codes
        WHERE Currency = @currency_codes-Currency
        INTO TABLE @DATA(currency_codes_db).
    ENDIF.

    " Raise message for not existing Currency Codes
    LOOP AT Carrier ASSIGNING FIELD-SYMBOL(<carrier>).

      APPEND VALUE #( %tky        = <carrier>-%tky
                      %state_area = 'VALIDATE_CURRENCY_CODE'
                    ) TO reported-carrier.

      IF <carrier>-CurrencyCode IS INITIAL.
        APPEND VALUE #( %tky                             = <carrier>-%tky ) TO failed-carrier.
        APPEND VALUE #( %tky                             = <carrier>-%tky
                        %msg                             = NEW /dmo/cx_carriers_s(
                                                               textid     = /dmo/cx_carriers_s=>currency_code_required
                                                               severity   = if_abap_behv_message=>severity-error
                                                               airline_id = <carrier>-AirlineID )
                        %element-currencycode            = if_abap_behv=>mk-on
                        %state_area                      = 'VALIDATE_CURRENCY_CODE'
                        %path-carrierslocksingleton      = CORRESPONDING #( <carrier> )
                      ) TO reported-carrier.

      ELSEIF NOT line_exists( currency_codes_db[ Currency = <carrier>-CurrencyCode ] ).
        APPEND VALUE #( %tky                             = <carrier>-%tky ) TO failed-carrier.
        APPEND VALUE #( %tky                             = <carrier>-%tky
                        %msg                             = NEW /dmo/cx_carriers_s(
                                                               textid        = /dmo/cx_carriers_s=>invalid_currency_code
                                                               severity      = if_abap_behv_message=>severity-error
                                                               currency_code = <carrier>-CurrencyCode )
                        %element-currencycode            = if_abap_behv=>mk-on
                        %state_area                      = 'VALIDATE_CURRENCY_CODE'
                        %path-carrierslocksingleton      = CORRESPONDING #( <carrier> )
                      ) TO reported-carrier.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD validateName.

    READ ENTITIES OF /DMO/I_CarriersLockSingleton_S IN LOCAL MODE
      ENTITY Carrier
        FIELDS ( CarrierSingletonID Name )
        WITH CORRESPONDING #( keys )
      RESULT DATA(Carrier).

    " Raise message for empty Airline Name
    LOOP AT Carrier ASSIGNING FIELD-SYMBOL(<carrier>).

      APPEND VALUE #( %tky        = <carrier>-%tky
                      %state_area = 'VALIDATE_NAME'
                    ) TO reported-carrier.

      IF <carrier>-name IS INITIAL.
        APPEND VALUE #( %tky                        = <carrier>-%tky ) TO failed-carrier.
        APPEND VALUE #( %tky                        = <carrier>-%tky
                        %msg                        = NEW /dmo/cx_carriers_s(
                                                         textid     = /dmo/cx_carriers_s=>name_required
                                                         severity   = if_abap_behv_message=>severity-error
                                                         airline_id = <carrier>-AirlineID )
                        %element-name               = if_abap_behv=>mk-on
                        %state_area                 = 'VALIDATE_NAME'
                        %path-carrierslocksingleton = CORRESPONDING #( <carrier> )
                      ) TO reported-carrier.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lthc_CarriersLockSingleton DEFINITION DEFERRED FOR TESTING.
CLASS lhc_CarriersLockSingleton DEFINITION
  INHERITING FROM cl_abap_behavior_handler
  FRIENDS lthc_carrierslocksingleton
  .
  PRIVATE SECTION.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR CarriersLockSingleton RESULT result.

ENDCLASS.

CLASS lhc_CarriersLockSingleton IMPLEMENTATION.

  METHOD get_global_authorizations.
  ENDMETHOD.

ENDCLASS.


CLASS ltsc_I_CARRIERSLOCKSINGLETON_S DEFINITION DEFERRED FOR TESTING.
CLASS lsc_I_CARRIERSLOCKSINGLETON_S DEFINITION
  INHERITING FROM cl_abap_behavior_saver
  FRIENDS ltsc_I_CARRIERSLOCKSINGLETON_S
  .
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_I_CARRIERSLOCKSINGLETON_S IMPLEMENTATION.

  METHOD save_modified.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
