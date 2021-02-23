CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF travel_status,
        open     TYPE c LENGTH 1 VALUE 'O', "Open
        accepted TYPE c LENGTH 1 VALUE 'A', "Accepted
        rejected TYPE c LENGTH 1 VALUE 'X', "Rejected
      END OF travel_status.

    METHODS acceptTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~acceptTravel RESULT result.
    METHODS rejectTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~rejectTravel RESULT result.
    METHODS deductDiscount FOR MODIFY
      IMPORTING keys FOR ACTION Travel~deductDiscount RESULT result.
    METHODS reCalcTotalPrice FOR MODIFY
      IMPORTING keys FOR ACTION Travel~reCalcTotalPrice.

    METHODS setTravelNumber FOR DETERMINE ON SAVE
      IMPORTING keys FOR Travel~setTravelNumber.
    METHODS setStatusToNew FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~setStatusToNew.
    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~calculateTotalPrice.

    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateCustomer.
    METHODS validateAgency FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateAgency.
    METHODS validateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateDates.
    METHODS validateauthoncreate FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validateauthoncreate.

    METHODS get_instance_features FOR FEATURES
      IMPORTING keys REQUEST requested_features FOR Travel RESULT result.
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR travel RESULT result.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR travel RESULT result.


    METHODS is_create_granted
      IMPORTING country_code          TYPE land1 OPTIONAL
      RETURNING VALUE(create_granted) TYPE abap_bool.
    METHODS is_update_granted
      IMPORTING country_code          TYPE land1 OPTIONAL
      RETURNING VALUE(update_granted) TYPE abap_bool.
    METHODS is_delete_granted
      IMPORTING country_code          TYPE land1 OPTIONAL
      RETURNING VALUE(delete_granted) TYPE abap_bool.




ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

  METHOD acceptTravel.

    "Modify travel instance
    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS (  OverallStatus )
        WITH VALUE #( FOR key IN keys ( %tky          = key-%tky
                                        OverallStatus = travel_status-accepted ) )
    FAILED failed
    REPORTED reported.

    "Read changed data for action result
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH
        CORRESPONDING #( keys )
      RESULT DATA(lt_travel).

    result = VALUE #( FOR travel IN lt_travel ( %tky   = travel-%tky
                                                %param = travel ) ).

  ENDMETHOD.

  METHOD rejectTravel.

    "Modify travel instance
    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS (  OverallStatus )
        WITH VALUE #( FOR key IN keys ( %tky          = key-%tky
                                        OverallStatus = travel_status-rejected ) )
    FAILED failed
    REPORTED reported.

    "Read changed data for action result
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH
        CORRESPONDING #( keys )
      RESULT DATA(lt_travel).

    result = VALUE #( FOR travel IN lt_travel ( %tky   = travel-%tky
                                                %param = travel ) ).

  ENDMETHOD.

  METHOD deductDiscount.
    DATA lt_update_travel TYPE TABLE FOR UPDATE /DMO/I_Travel_D.
    DATA(lt_keys) = keys.

    LOOP AT lt_keys ASSIGNING FIELD-SYMBOL(<fs_key>) WHERE %param-discount_percent IS INITIAL
                                                        OR %param-discount_percent > 100
                                                        OR %param-discount_percent <= 0.

      APPEND VALUE #( %tky                = <fs_key>-%tky ) TO failed-travel.

      APPEND VALUE #( %tky                = <fs_key>-%tky
                      %msg                = NEW /dmo/cm_flight_messages(
                             textid = /dmo/cm_flight_messages=>discount_invalid
                             severity = if_abap_behv_message=>severity-error )
                             %element-TotalPrice = if_abap_behv=>mk-on ) TO reported-travel.

      DELETE lt_keys.
    ENDLOOP.

    CHECK lt_keys IS NOT INITIAL.

    "get total price
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS ( BookingFee )
        WITH CORRESPONDING #( lt_keys )
      RESULT DATA(lt_travel)
      FAILED DATA(read_failed).

    failed = CORRESPONDING #( DEEP read_failed ).

    LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<fs_travel>).
      DATA lv_percentage TYPE decfloat16.
      DATA(lv_discount_percent) = lt_keys[  %tky = <fs_travel>-%tky ]-%param-discount_percent.
      lv_percentage =  lv_discount_percent / 100 .
      DATA(lv_reduced_fee) = <fs_travel>-BookingFee * ( 1 - lv_percentage ) .

      APPEND VALUE #( %tky       = <fs_travel>-%tky
                      BookingFee = lv_reduced_fee  ) TO lt_update_travel.
    ENDLOOP.

    "update total price with reduced price
    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
       UPDATE FIELDS ( BookingFee )
       WITH lt_update_travel
    FAILED DATA(update_failed)
    REPORTED DATA(update_reported).

    "Read changed data for action result
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH
        CORRESPONDING #( lt_travel )
      RESULT DATA(lt_travel_with_discount).

    result = VALUE #( FOR travel IN lt_travel_with_discount ( %tky   = travel-%tky
                                                              %param = travel ) ).

  ENDMETHOD.

  METHOD reCalcTotalPrice.
    TYPES: BEGIN OF ty_amount_per_currencycode,
             amount        TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
           END OF ty_amount_per_currencycode.

    DATA: amount_per_currencycode TYPE STANDARD TABLE OF ty_amount_per_currencycode.

    " Read all relevant travel instances.
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
         ENTITY Travel
            FIELDS ( BookingFee CurrencyCode )
            WITH CORRESPONDING #( keys )
         RESULT DATA(lt_travel)
         FAILED failed.

    DELETE lt_travel WHERE CurrencyCode IS INITIAL.

    LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<fs_travel>).
      " Set the start for the calculation by adding the booking fee.
      amount_per_currencycode = VALUE #( ( amount        = <fs_travel>-BookingFee
                                           currency_code = <fs_travel>-CurrencyCode ) ).

      " Read all associated bookings and add them to the total price.
      READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
        ENTITY Travel BY \_Booking
          FIELDS ( FlightPrice CurrencyCode )
        WITH VALUE #( ( %tky = <fs_travel>-%tky ) )
        RESULT DATA(lt_booking).

      LOOP AT lt_booking INTO DATA(booking) WHERE CurrencyCode IS NOT INITIAL.
        COLLECT VALUE ty_amount_per_currencycode( amount        = booking-FlightPrice
                                                  currency_code = booking-CurrencyCode ) INTO amount_per_currencycode.
      ENDLOOP.

      " Read all associated booking supplements and add them to the total price.
      READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
        ENTITY Booking BY \_BookingSupplement
          FIELDS ( BookSupplPrice CurrencyCode )
        WITH VALUE #( FOR rba_booking IN lt_booking ( %tky = rba_booking-%tky ) )
        RESULT DATA(lt_bookingsupplement).

      LOOP AT lt_bookingsupplement INTO DATA(bookingsupplement) WHERE CurrencyCode IS NOT INITIAL.
        COLLECT VALUE ty_amount_per_currencycode( amount        = bookingsupplement-BookSupplPrice
                                                  currency_code = bookingsupplement-CurrencyCode ) INTO amount_per_currencycode.
      ENDLOOP.

      CLEAR <fs_travel>-TotalPrice.
      LOOP AT amount_per_currencycode INTO DATA(single_amount_per_currencycode).
        " If needed do a Currency Conversion
        IF single_amount_per_currencycode-currency_code = <fs_travel>-CurrencyCode.
          <fs_travel>-TotalPrice += single_amount_per_currencycode-amount.
        ELSE.
          /dmo/cl_flight_amdp=>convert_currency(
             EXPORTING
               iv_amount                   =  single_amount_per_currencycode-amount
               iv_currency_code_source     =  single_amount_per_currencycode-currency_code
               iv_currency_code_target     =  <fs_travel>-CurrencyCode
               iv_exchange_rate_date       =  cl_abap_context_info=>get_system_date( )
             IMPORTING
               ev_amount                   = DATA(total_booking_price_per_curr)
            ).
          <fs_travel>-TotalPrice += total_booking_price_per_curr.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " write back the modified total_price of travels
    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY travel
        UPDATE FIELDS ( TotalPrice )
        WITH CORRESPONDING #( lt_travel ).

  ENDMETHOD.

  METHOD setTravelNumber.
    "Ensure idempotence
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travel).

    DELETE lt_travel WHERE TravelID IS NOT INITIAL.
    CHECK lt_travel IS NOT INITIAL.

    "Get max travelID
    SELECT SINGLE FROM /dmo/a_travel_d FIELDS MAX( travel_id ) INTO @DATA(lv_max_travelid).

    "update involved instances
    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS ( TravelID )
        WITH VALUE #( FOR ls_travel IN lt_travel INDEX INTO i (
                           %tky      = ls_travel-%tky
                           TravelID  = lv_max_travelid + i ) )
    REPORTED DATA(lt_reported).

    "fill reported
    reported = CORRESPONDING #( DEEP lt_reported ).
  ENDMETHOD.

  METHOD setStatusToNew.

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
     ENTITY Travel
       FIELDS ( OverallStatus )
       WITH CORRESPONDING #( keys )
     RESULT DATA(lt_travel)
     FAILED DATA(lt_failed).

    "If Status is already set, do nothing
    DELETE lt_travel WHERE OverallStatus IS NOT INITIAL.
    CHECK lt_travel IS NOT INITIAL.

    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        UPDATE SET FIELDS
        WITH VALUE #( FOR key IN keys ( %tky          = key-%tky
                                        OverallStatus = travel_status-open ) )
    REPORTED DATA(lt_reported).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

  METHOD calculateTotalPrice.

    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        EXECUTE reCalcTotalPrice
        FROM CORRESPONDING #( keys )
    REPORTED DATA(lt_reported).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

  METHOD validateCustomer.

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS ( CustomerID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travel)
      FAILED DATA(lt_failed).

    failed =  CORRESPONDING #( DEEP lt_failed  ).

    DATA lt_customer TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    lt_customer = CORRESPONDING #( lt_travel DISCARDING DUPLICATES MAPPING customer_id = CustomerID EXCEPT * ).
    DELETE lt_customer WHERE customer_id IS INITIAL.

    IF  lt_customer IS NOT INITIAL.
      " Check if customer ID exists
      SELECT FROM /dmo/customer FIELDS customer_id
                                FOR ALL ENTRIES IN @lt_customer
                                WHERE customer_id = @lt_customer-customer_id
      INTO TABLE @DATA(lt_customer_db).
    ENDIF.

    " Raise message for non existing customer id
    LOOP AT lt_travel INTO DATA(ls_travel).

      APPEND VALUE #(  %tky                 = ls_travel-%tky
                       %state_area          = 'VALIDATE_CUSTOMER' ) TO reported-travel.

      IF ls_travel-CustomerID IS  INITIAL.
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky                = ls_travel-%tky
                        %state_area         = 'VALIDATE_CUSTOMER'
                        %msg                = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>enter_customer_id
                                                                severity = if_abap_behv_message=>severity-error )
                        %element-CustomerID = if_abap_behv=>mk-on ) TO reported-travel.

      ELSEIF ls_travel-CustomerID IS NOT INITIAL AND NOT line_exists( lt_customer_db[ customer_id = ls_travel-CustomerID ] ).
        APPEND VALUE #(  %tky = ls_travel-%tky ) TO failed-travel.

        APPEND VALUE #(  %tky                = ls_travel-%tky
                         %state_area         = 'VALIDATE_CUSTOMER'
                         %msg                = NEW /dmo/cm_flight_messages(
                                                                customer_id = ls_travel-customerid
                                                                textid = /dmo/cm_flight_messages=>customer_unkown
                                                                severity = if_abap_behv_message=>severity-error )
                         %element-CustomerID = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD validateAgency.
    DATA: modification_granted TYPE abap_boolean,
          agency_country_code  TYPE land1.

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS ( AgencyID TravelID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels)
      FAILED DATA(read_failed).

    failed =  CORRESPONDING #( DEEP read_failed  ).

    DATA agencies TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id.

    " Optimization of DB select: extract distinct non-initial agency IDs
    agencies = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING agency_id = AgencyID EXCEPT * ).
    DELETE agencies WHERE agency_id IS INITIAL.

    IF  agencies IS NOT INITIAL.
      " Check if customer ID exists
      SELECT FROM /dmo/agency FIELDS agency_id, country_code
                              FOR ALL ENTRIES IN @agencies
                              WHERE agency_id = @agencies-agency_id
        INTO TABLE @DATA(agencies_db).
    ENDIF.

    " Raise message for non existing customer id
    LOOP AT travels INTO DATA(travel).
      APPEND VALUE #(  %tky               = travel-%tky
                       %state_area        = 'VALIDATE_AGENCY'
                    ) TO reported-travel.

      IF travel-AgencyID IS  INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky                = travel-%tky
                        %state_area         = 'VALIDATE_AGENCY'
                        %msg                = NEW /dmo/cm_flight_messages(
                                                  textid   = /dmo/cm_flight_messages=>enter_agency_id
                                                  severity = if_abap_behv_message=>severity-error )
                        %element-AgencyID   = if_abap_behv=>mk-on
                       ) TO reported-travel.

      ELSEIF travel-AgencyID IS NOT INITIAL AND NOT line_exists( agencies_db[ agency_id = travel-AgencyID ] ).
        APPEND VALUE #(  %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #(  %tky               = travel-%tky
                         %state_area        = 'VALIDATE_AGENCY'
                         %msg               = NEW /dmo/cm_flight_messages(
                                                                agency_id = travel-agencyid
                                                                textid = /dmo/cm_flight_messages=>agency_unkown
                                                                severity = if_abap_behv_message=>severity-error )
                         %element-AgencyID  = if_abap_behv=>mk-on
                      ) TO reported-travel.
        " If Agency ID is valis, check authorization
      ELSE.

        modification_granted = abap_false.

        READ TABLE agencies_db WITH KEY agency_id = travel-AgencyID
             ASSIGNING FIELD-SYMBOL(<agency_country_code>).

        modification_granted = is_update_granted( <agency_country_code>-country_code ).

        IF modification_granted = abap_false.
          APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

          APPEND VALUE #(  %tky               = travel-%tky
                           %state_area        = 'VALIDATE_AGENCY'
                           %msg               = NEW /dmo/cm_flight_messages(
                                                    agency_id = travel-agencyid
                                                    textid    = /dmo/cm_flight_messages=>not_authorized_for_agencyid
                                                    severity  = if_abap_behv_message=>severity-error )
                           %element-AgencyID  = if_abap_behv=>mk-on
                        ) TO reported-travel.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD validateDates.
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS (  BeginDate EndDate TravelID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travel)
      FAILED DATA(lt_failed).

    failed =  CORRESPONDING #( DEEP lt_failed  ).

    LOOP AT lt_travel INTO DATA(ls_travel).

      APPEND VALUE #(  %tky               = ls_travel-%tky
                       %state_area          = 'VALIDATE_DATES' ) TO reported-travel.

      IF ls_travel-BeginDate IS INITIAL.
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = ls_travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                         %msg                = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>enter_begin_date
                                                                severity = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
      IF ls_travel-EndDate IS INITIAL.
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = ls_travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                         %msg                = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>enter_end_date
                                                                severity = if_abap_behv_message=>severity-error )
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
      IF ls_travel-EndDate < ls_travel-BeginDate AND ls_travel-BeginDate IS NOT INITIAL
                                                 AND ls_travel-EndDate IS NOT INITIAL.
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = ls_travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>begin_date_bef_end_date
                                                                begin_date = ls_travel-BeginDate
                                                                end_date   = ls_travel-EndDate
                                                                severity = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
      IF ls_travel-BeginDate < cl_abap_context_info=>get_system_date( ) AND ls_travel-BeginDate IS NOT INITIAL.
        APPEND VALUE #( %tky               = ls_travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = ls_travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                         %msg                = NEW /dmo/cm_flight_messages(
                                                                begin_date = ls_travel-BeginDate
                                                                textid = /dmo/cm_flight_messages=>begin_date_on_or_bef_sysdate
                                                                severity = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_features.

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS ( OverallStatus )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travel)
      FAILED failed.


    result = VALUE #( FOR ls_travel IN lt_travel
                          ( %tky                   = ls_travel-%tky

                            %field-BookingFee      = COND #( WHEN ls_travel-OverallStatus = travel_status-accepted
                                                             THEN if_abap_behv=>fc-f-read_only
                                                             ELSE if_abap_behv=>fc-f-unrestricted )
                            %action-acceptTravel   = COND #( WHEN ls_travel-OverallStatus = travel_status-accepted
                                                             THEN if_abap_behv=>fc-o-disabled
                                                             ELSE if_abap_behv=>fc-o-enabled )
                            %action-rejectTravel   = COND #( WHEN ls_travel-OverallStatus = travel_status-rejected
                                                             THEN if_abap_behv=>fc-o-disabled
                                                             ELSE if_abap_behv=>fc-o-enabled )
                            %action-deductDiscount = COND #( WHEN ls_travel-OverallStatus = travel_status-accepted
                                                             THEN if_abap_behv=>fc-o-disabled
                                                             ELSE if_abap_behv=>fc-o-enabled )
                            %assoc-_Booking        = COND #( WHEN ls_travel-OverallStatus = travel_status-rejected
                                                            THEN if_abap_behv=>fc-o-disabled
                                                            ELSE if_abap_behv=>fc-o-enabled )
                          ) ).

  ENDMETHOD.

  METHOD get_global_authorizations.
    IF requested_authorizations-%create EQ if_abap_behv=>mk-on.
      IF is_create_granted( ) = abap_true.
        result-%create = if_abap_behv=>auth-allowed.
      ELSE.
        result-%create = if_abap_behv=>auth-unauthorized.
        APPEND VALUE #( %msg    = NEW /dmo/cm_flight_messages(
                                       textid   = /dmo/cm_flight_messages=>not_authorized
                                       severity = if_abap_behv_message=>severity-error )
                        %global = if_abap_behv=>mk-on ) TO reported-travel.

      ENDIF.
    ENDIF.

    "Actions are treated like update
    IF requested_authorizations-%update                =  if_abap_behv=>mk-on OR
       requested_authorizations-%action-Edit           =  if_abap_behv=>mk-on OR
       requested_authorizations-%action-Prepare        =  if_abap_behv=>mk-on OR
       requested_authorizations-%action-acceptTravel   =  if_abap_behv=>mk-on OR
       requested_authorizations-%action-rejectTravel   =  if_abap_behv=>mk-on OR
       requested_authorizations-%action-deductDiscount =  if_abap_behv=>mk-on.

      IF  is_update_granted( ) = abap_true.
        result-%update                =  if_abap_behv=>auth-allowed.
        result-%action-Edit           =  if_abap_behv=>auth-allowed.
        result-%action-Prepare        =  if_abap_behv=>auth-allowed.
        result-%action-acceptTravel   =  if_abap_behv=>auth-allowed.
        result-%action-rejectTravel   =  if_abap_behv=>auth-allowed.
        result-%action-deductDiscount =  if_abap_behv=>auth-allowed.

      ELSE.
        result-%update                =  if_abap_behv=>auth-unauthorized.
        result-%action-Edit           =  if_abap_behv=>auth-unauthorized.
        result-%action-Prepare        =  if_abap_behv=>auth-unauthorized.
        result-%action-acceptTravel   =  if_abap_behv=>auth-unauthorized.
        result-%action-rejectTravel   =  if_abap_behv=>auth-unauthorized.
        result-%action-deductDiscount =  if_abap_behv=>auth-unauthorized.

        APPEND VALUE #( %msg    = NEW /dmo/cm_flight_messages(
                                       textid   = /dmo/cm_flight_messages=>not_authorized
                                       severity = if_abap_behv_message=>severity-error )
                        %global = if_abap_behv=>mk-on )
          TO reported-travel.

      ENDIF.
    ENDIF.


    IF requested_authorizations-%delete =  if_abap_behv=>mk-on.
      IF is_delete_granted( ) = abap_true.
        result-%delete = if_abap_behv=>auth-allowed.
      ELSE.
        result-%delete = if_abap_behv=>auth-unauthorized.
        APPEND VALUE #( %msg    = NEW /dmo/cm_flight_messages(
                                       textid   = /dmo/cm_flight_messages=>not_authorized
                                       severity = if_abap_behv_message=>severity-error )
                        %global = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD is_create_granted.

    "For validation
    IF country_code IS SUPPLIED.
      AUTHORITY-CHECK OBJECT '/DMO/TRVL'
        ID '/DMO/CNTRY' FIELD country_code
        ID 'ACTVT'      FIELD '01'.
      create_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      "Simulation for full authorization
      "(not to be used in productive code)
      create_granted = abap_true.

      " simulation of auth check for demo,
      " auth granted for country_code US, else not
*      CASE country_code.
*        WHEN 'US'.
*          create_granted = abap_true.
*        WHEN OTHERS.
*          create_granted = abap_false.
*      ENDCASE.

      "For global auth
    ELSE.
      AUTHORITY-CHECK OBJECT '/DMO/TRVL'
        ID '/DMO/CNTRY' DUMMY
        ID 'ACTVT'      FIELD '01'.
      create_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      "Simulation for full authorization
      "(not to be used in productive code)
      create_granted = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD is_update_granted.
    "For instance auth
    IF country_code IS SUPPLIED.
      AUTHORITY-CHECK OBJECT '/DMO/TRVL'
        ID '/DMO/CNTRY' FIELD country_code
        ID 'ACTVT'      FIELD '02'.
      update_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      "Simulation for full authorization
      "(not to be used in productive code)
      update_granted = abap_true.

      " simulation of auth check for demo,
      " auth granted for country_code US, else not
*      CASE country_code.
*        WHEN 'US'.
*          update_granted = abap_true.
*        WHEN OTHERS.
*          update_granted = abap_false.
*      ENDCASE.

      "For global auth
    ELSE.
      AUTHORITY-CHECK OBJECT '/DMO/TRVL'
        ID '/DMO/CNTRY' DUMMY
        ID 'ACTVT'      FIELD '02'.
      update_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      "Simulation for full authorization
      "(not to be used in productive code)
      update_granted = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD is_delete_granted.
    "For instance auth
    IF country_code IS SUPPLIED.
      AUTHORITY-CHECK OBJECT '/DMO/TRVL'
        ID '/DMO/CNTRY' FIELD country_code
        ID 'ACTVT'      FIELD '06'.
      delete_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      "Simulation for full authorization
      "(not to be used in productive code)
      delete_granted = abap_true.

*      " simulation of auth check for demo,
*      " auth granted for country_code US, else not
*      CASE country_code.
*        WHEN 'US'.
*          delete_granted = abap_true.
*        WHEN OTHERS.
*          delete_granted = abap_false.
*      ENDCASE.

      "For global auth
    ELSE.
      AUTHORITY-CHECK OBJECT '/DMO/TRVL'
        ID '/DMO/CNTRY' DUMMY
        ID 'ACTVT'      FIELD '06'.
      delete_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      "Simulation for full authorization
      "(not to be used in productive code)
      delete_granted = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance_authorizations.

    DATA: update_requested TYPE abap_bool,
          delete_requested TYPE abap_bool,
          update_granted   TYPE abap_bool,
          delete_granted   TYPE abap_bool.

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS ( AgencyID )
        WITH CORRESPONDING #( keys )
    RESULT DATA(travels)
    FAILED failed.

    CHECK travels IS NOT INITIAL.

    "Select country_code and agency of corresponding persistent travel instance
    "authorization  only checked against instance that have active persistence
    SELECT  FROM /dmo/a_travel_d AS travel  INNER JOIN /dmo/agency AS agency
          ON travel~agency_id = agency~agency_id
          FIELDS travel~travel_uuid , travel~agency_id, agency~country_code
          FOR ALL ENTRIES IN @travels WHERE travel_uuid EQ @travels-TravelUUID
          INTO  TABLE @DATA(travel_agency_country).


    "all actions are treated like update
    update_requested = COND #( WHEN requested_authorizations-%update                = if_abap_behv=>mk-on OR
                                    requested_authorizations-%assoc-_Booking        = if_abap_behv=>mk-on OR
                                    requested_authorizations-%action-Edit           = if_abap_behv=>mk-on OR
                                    requested_authorizations-%action-Prepare        = if_abap_behv=>mk-on OR
                                    requested_authorizations-%action-acceptTravel   = if_abap_behv=>mk-on OR
                                    requested_authorizations-%action-deductDiscount = if_abap_behv=>mk-on OR
                                    requested_authorizations-%action-rejectTravel   = if_abap_behv=>mk-on
                               THEN abap_true ELSE abap_false ).

    delete_requested = COND #( WHEN requested_authorizations-%delete                = if_abap_behv=>mk-on
                               THEN abap_true ELSE abap_false ).


    LOOP AT travels INTO DATA(travel).
      "get country_code of agency in corresponding instance on persistent table
      READ TABLE travel_agency_country WITH KEY travel_uuid = travel-TravelUUID
        ASSIGNING FIELD-SYMBOL(<travel_agency_country_code>).

      "Auth check for active instances that have before image on persistent table
      IF sy-subrc = 0.

        "check auth for update
        IF update_requested = abap_true.
          update_granted = is_update_granted( <travel_agency_country_code>-country_code  ).
          IF update_granted = abap_false.
            APPEND VALUE #( %tky = travel-%tky
                            %msg = NEW /dmo/cm_flight_messages(
                                     textid   = /dmo/cm_flight_messages=>not_authorized_for_agencyid
                                     agency_id = travel-AgencyID
                                     severity = if_abap_behv_message=>severity-error )
                            %element-AgencyID = if_abap_behv=>mk-on
                           ) TO reported-travel.
          ENDIF.
        ENDIF.

        "check auth for delete
        IF delete_requested = abap_true.
          delete_granted = is_delete_granted( <travel_agency_country_code>-country_code ).
          IF delete_granted = abap_false.
            APPEND VALUE #( %tky = travel-%tky
                            %msg = NEW /dmo/cm_flight_messages(
                                     textid   = /dmo/cm_flight_messages=>not_authorized_for_agencyid
                                     agency_id = travel-AgencyID
                                     severity = if_abap_behv_message=>severity-error )
                            %element-AgencyID = if_abap_behv=>mk-on
                           ) TO reported-travel.
          ENDIF.
        ENDIF.

        " operations on draft instances and on active instances that have no persistent before image (eg Update on newly created instance)
        " create authorization is checked, for newly created instances
      ELSE.
        update_granted = delete_granted = is_create_granted( ).
        IF update_granted = abap_false.
          APPEND VALUE #( %tky = travel-%tky
                          %msg = NEW /dmo/cm_flight_messages(
                                   textid   = /dmo/cm_flight_messages=>not_authorized
                                   severity = if_abap_behv_message=>severity-error )
                          %element-AgencyID = if_abap_behv=>mk-on
                        ) TO reported-travel.
        ENDIF.
      ENDIF.

      APPEND VALUE #( LET upd_auth = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed
                                             ELSE if_abap_behv=>auth-unauthorized )
                          del_auth = COND #( WHEN delete_granted = abap_true THEN if_abap_behv=>auth-allowed
                                             ELSE if_abap_behv=>auth-unauthorized )
                      IN
                       %tky = travel-%tky
                       %update                = upd_auth
                       %assoc-_Booking        = upd_auth
                       %action-Edit           = upd_auth
                       %action-Prepare        = upd_auth
                       %action-acceptTravel   = upd_auth
                       %action-deductDiscount = upd_auth
                       %action-rejectTravel   = upd_auth

                       %delete                = del_auth
                    ) TO result.
    ENDLOOP.


  ENDMETHOD.

  METHOD validateAuthOnCreate.
    DATA: create_granted      TYPE abap_boolean,
          agency_country_code TYPE land1.

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
    ENTITY Travel
      FIELDS ( AgencyID )
      WITH CORRESPONDING #( keys )
    RESULT DATA(travels)
    FAILED DATA(read_failed).

    failed = CORRESPONDING #( DEEP read_failed ).
    CHECK travels IS NOT INITIAL.

    SELECT FROM /dmo/agency FIELDS agency_id, country_code
      FOR ALL ENTRIES IN @travels
      WHERE agency_id = @travels-AgencyID
      INTO TABLE @DATA(agency_country_codes).

    LOOP AT travels INTO DATA(travel).

      APPEND VALUE #( %tky = travel-%tky
                      %state_area = 'VALIDATE_AUTHORIZATION' ) TO reported-travel.

      create_granted = abap_false.

      READ TABLE agency_country_codes WITH KEY agency_id = travel-AgencyID
                   ASSIGNING FIELD-SYMBOL(<agency_country_code>).

      "If invalid or initial AgencyID -> validateAgency
      CHECK sy-subrc = 0.
      IF is_create_granted( <agency_country_code>-country_code  ) = abap_false.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky                = travel-%tky
                        %state_area         = 'VALIDATE_AUTHORIZATION'
                        %msg    = NEW /dmo/cm_flight_messages(
                                  textid    = /dmo/cm_flight_messages=>not_authorized_for_agencyid
                                  agency_id = travel-AgencyID
                                  severity  = if_abap_behv_message=>severity-error )
                        %element-AgencyID   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


ENDCLASS.
