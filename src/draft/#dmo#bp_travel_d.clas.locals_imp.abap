CLASS ltc_travel DEFINITION DEFERRED FOR TESTING.
CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler
 FRIENDS ltc_travel.

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
    METHODS GetDefaultsFordeductDiscount FOR READ
      IMPORTING keys FOR FUNCTION Travel~GetDefaultsFordeductDiscount RESULT result.
    METHODS reCalcTotalPrice FOR MODIFY
      IMPORTING keys FOR ACTION Travel~reCalcTotalPrice.

    METHODS setTravelNumber FOR DETERMINE ON SAVE
      IMPORTING keys FOR Travel~setTravelNumber.
    METHODS setstatustoopen FOR DETERMINE ON MODIFY
      IMPORTING keys FOR travel~setStatusToOpen.
    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~calculateTotalPrice.

    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateCustomer.
    METHODS validateAgency FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateAgency.
    METHODS validateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateDates.
    METHODS validateCurrencyCode FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateCurrencyCode.


    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Travel RESULT result.
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR travel RESULT result.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR travel RESULT result.


    METHODS precheck_create FOR PRECHECK
      IMPORTING entities FOR CREATE travel.
    METHODS precheck_update FOR PRECHECK
      IMPORTING entities FOR UPDATE travel.


    METHODS resume FOR MODIFY
      IMPORTING keys FOR ACTION Travel~Resume.



    METHODS is_create_granted
      IMPORTING country_code          TYPE land1 OPTIONAL
      RETURNING VALUE(create_granted) TYPE abap_bool.
    METHODS is_update_granted
      IMPORTING country_code          TYPE land1 OPTIONAL
      RETURNING VALUE(update_granted) TYPE abap_bool.
    METHODS is_delete_granted
      IMPORTING country_code          TYPE land1 OPTIONAL
      RETURNING VALUE(delete_granted) TYPE abap_bool.




    TYPES:
      t_entities_create TYPE TABLE FOR CREATE /dmo/r_travel_d\\travel,
      t_entities_update TYPE TABLE FOR UPDATE /dmo/r_travel_d\\travel,
      t_failed_travel   TYPE TABLE FOR FAILED   EARLY /dmo/r_travel_d\\travel,
      t_reported_travel TYPE TABLE FOR REPORTED EARLY /dmo/r_travel_d\\travel.


    METHODS precheck_auth
      IMPORTING
        entities_create TYPE t_entities_create OPTIONAL
        entities_update TYPE t_entities_update OPTIONAL
      CHANGING
        failed          TYPE t_failed_travel
        reported        TYPE t_reported_travel.

ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

  METHOD acceptTravel.

    "Modify travel instance
    MODIFY ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS (  OverallStatus )
        WITH VALUE #( FOR key IN keys ( %tky          = key-%tky
                                        OverallStatus = travel_status-accepted ) ).

    "Read changed data for action result
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH
        CORRESPONDING #( keys )
      RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels ( %tky   = travel-%tky
                                              %param = travel ) ).

  ENDMETHOD.

  METHOD rejectTravel.

    "Modify travel instance
    MODIFY ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS (  OverallStatus )
        WITH VALUE #( FOR key IN keys ( %tky          = key-%tky
                                        OverallStatus = travel_status-rejected ) ).

    "Read changed data for action result
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH
        CORRESPONDING #( keys )
      RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels ( %tky   = travel-%tky
                                              %param = travel ) ).

  ENDMETHOD.

  METHOD deductDiscount.
    DATA travels_for_update TYPE TABLE FOR UPDATE /DMO/R_Travel_D.
    DATA(keys_with_valid_discount) = keys.

    LOOP AT keys_with_valid_discount ASSIGNING FIELD-SYMBOL(<key_with_valid_discount>) WHERE %param-discount_percent IS INITIAL
                                                        OR %param-discount_percent > 100
                                                        OR %param-discount_percent <= 0.

      APPEND VALUE #( %tky                       = <key_with_valid_discount>-%tky ) TO failed-travel.

      APPEND VALUE #( %tky                       = <key_with_valid_discount>-%tky
                      %msg                       = NEW /dmo/cm_flight_messages(
                                                       textid = /dmo/cm_flight_messages=>discount_invalid
                                                       severity = if_abap_behv_message=>severity-error )
                      %element-TotalPrice        = if_abap_behv=>mk-on
                      %op-%action-deductDiscount = if_abap_behv=>mk-on
                    ) TO reported-travel.

      DELETE keys_with_valid_discount.
    ENDLOOP.

    CHECK keys_with_valid_discount IS NOT INITIAL.

    "get total price
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS ( BookingFee )
        WITH CORRESPONDING #( keys_with_valid_discount )
      RESULT DATA(travels).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).
      DATA percentage TYPE decfloat16.
      DATA(discount_percent) = keys_with_valid_discount[ KEY id  %tky = <travel>-%tky ]-%param-discount_percent.
      percentage =  discount_percent / 100 .
      DATA(reduced_fee) = <travel>-BookingFee * ( 1 - percentage ) .

      APPEND VALUE #( %tky       = <travel>-%tky
                      BookingFee = reduced_fee
                    ) TO travels_for_update.
    ENDLOOP.

    "update total price with reduced price
    MODIFY ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
       UPDATE FIELDS ( BookingFee )
       WITH travels_for_update.

    "Read changed data for action result
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH
        CORRESPONDING #( travels )
      RESULT DATA(travels_with_discount).

    result = VALUE #( FOR travel IN travels_with_discount ( %tky   = travel-%tky
                                                            %param = travel ) ).

  ENDMETHOD.

  METHOD reCalcTotalPrice.
    TYPES: BEGIN OF ty_amount_per_currencycode,
             amount        TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
           END OF ty_amount_per_currencycode.

    DATA: amounts_per_currencycode TYPE STANDARD TABLE OF ty_amount_per_currencycode.

    " Read all relevant travel instances.
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
         ENTITY Travel
            FIELDS ( BookingFee CurrencyCode )
            WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    DELETE travels WHERE CurrencyCode IS INITIAL.

    " Read all associated bookings and add them to the total price.
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel BY \_Booking
        FIELDS ( FlightPrice CurrencyCode )
      WITH CORRESPONDING #( travels )
      LINK DATA(booking_links)
      RESULT DATA(bookings).

    " Read all associated booking supplements and add them to the total price.
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Booking BY \_BookingSupplement
        FIELDS ( BookSupplPrice CurrencyCode )
      WITH CORRESPONDING #( bookings )
      LINK DATA(bookingsupplement_links)
      RESULT DATA(bookingsupplements).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).
      " Set the start for the calculation by adding the booking fee.
      amounts_per_currencycode = VALUE #( ( amount        = <travel>-bookingfee
                                            currency_code = <travel>-currencycode ) ).

      LOOP AT booking_links INTO DATA(booking_link) USING KEY id WHERE source-%tky = <travel>-%tky.
        " Short dump occurs if link table does not match read table, which must never happen
        DATA(booking) = bookings[ KEY id  %tky = booking_link-target-%tky ].
        COLLECT VALUE ty_amount_per_currencycode( amount        = booking-flightprice
                                                  currency_code = booking-currencycode ) INTO amounts_per_currencycode.

        LOOP AT bookingsupplement_links INTO DATA(bookingsupplement_link) USING KEY id WHERE source-%tky = booking-%tky.
          DATA(bookingsupplement) = bookingsupplements[ KEY id  %tky = bookingsupplement_link-target-%tky ].
          COLLECT VALUE ty_amount_per_currencycode( amount        = bookingsupplement-booksupplprice
                                                    currency_code = bookingsupplement-currencycode ) INTO amounts_per_currencycode.
        ENDLOOP.
      ENDLOOP.

      DELETE amounts_per_currencycode WHERE currency_code IS INITIAL.

      CLEAR <travel>-TotalPrice.
      LOOP AT amounts_per_currencycode INTO DATA(amount_per_currencycode).
        " If needed do a Currency Conversion
        IF amount_per_currencycode-currency_code = <travel>-CurrencyCode.
          <travel>-TotalPrice += amount_per_currencycode-amount.
        ELSE.
          /dmo/cl_flight_amdp=>convert_currency(
             EXPORTING
               iv_amount                   =  amount_per_currencycode-amount
               iv_currency_code_source     =  amount_per_currencycode-currency_code
               iv_currency_code_target     =  <travel>-CurrencyCode
               iv_exchange_rate_date       =  cl_abap_context_info=>get_system_date( )
             IMPORTING
               ev_amount                   = DATA(total_booking_price_per_curr)
            ).
          <travel>-TotalPrice += total_booking_price_per_curr.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " write back the modified total_price of travels
    MODIFY ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY travel
        UPDATE FIELDS ( TotalPrice )
        WITH CORRESPONDING #( travels ).

  ENDMETHOD.

  METHOD setTravelNumber.
    "Ensure idempotence
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    DELETE travels WHERE TravelID IS NOT INITIAL.
    CHECK travels IS NOT INITIAL.

    "Get max travelID
    SELECT SINGLE FROM /dmo/a_travel_d FIELDS MAX( travel_id ) INTO @DATA(max_travelid).

    "update involved instances
    MODIFY ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS ( TravelID )
        WITH VALUE #( FOR travel IN travels INDEX INTO i (
                           %tky      = travel-%tky
                           TravelID  = max_travelid + i ) ).

  ENDMETHOD.

  METHOD setStatusToOpen.

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
     ENTITY Travel
       FIELDS ( OverallStatus )
       WITH CORRESPONDING #( keys )
     RESULT DATA(travels).

    "If Status is already set, do nothing
    DELETE travels WHERE OverallStatus IS NOT INITIAL.
    CHECK travels IS NOT INITIAL.

    MODIFY ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS ( OverallStatus )
        WITH VALUE #( FOR travel IN travels ( %tky          = travel-%tky
                                              OverallStatus = travel_status-open ) ).

  ENDMETHOD.

  METHOD calculateTotalPrice.

    MODIFY ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        EXECUTE reCalcTotalPrice
        FROM CORRESPONDING #( keys ).

  ENDMETHOD.

  METHOD validateCustomer.

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS ( CustomerID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    customers = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING customer_id = CustomerID EXCEPT * ).
    DELETE customers WHERE customer_id IS INITIAL.

    IF  customers IS NOT INITIAL.
      " Check if customer ID exists
      SELECT FROM /dmo/customer FIELDS customer_id
                                FOR ALL ENTRIES IN @customers
                                WHERE customer_id = @customers-customer_id
      INTO TABLE @DATA(valid_customers).
    ENDIF.

    " Raise message for non existing customer id
    LOOP AT travels INTO DATA(travel).

      APPEND VALUE #(  %tky                 = travel-%tky
                       %state_area          = 'VALIDATE_CUSTOMER'
                     ) TO reported-travel.

      IF travel-CustomerID IS  INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky                = travel-%tky
                        %state_area         = 'VALIDATE_CUSTOMER'
                        %msg                = NEW /dmo/cm_flight_messages(
                                                                textid   = /dmo/cm_flight_messages=>enter_customer_id
                                                                severity = if_abap_behv_message=>severity-error )
                        %element-CustomerID = if_abap_behv=>mk-on
                      ) TO reported-travel.

      ELSEIF travel-CustomerID IS NOT INITIAL AND NOT line_exists( valid_customers[ customer_id = travel-CustomerID ] ).
        APPEND VALUE #(  %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #(  %tky                = travel-%tky
                         %state_area         = 'VALIDATE_CUSTOMER'
                         %msg                = NEW /dmo/cm_flight_messages(
                                                                customer_id = travel-customerid
                                                                textid      = /dmo/cm_flight_messages=>customer_unkown
                                                                severity    = if_abap_behv_message=>severity-error )
                         %element-CustomerID = if_abap_behv=>mk-on
                      ) TO reported-travel.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD validateAgency.
    DATA: modification_granted TYPE abap_boolean,
          agency_country_code  TYPE land1.

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS ( AgencyID TravelID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    DATA agencies TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id.

    " Optimization of DB select: extract distinct non-initial agency IDs
    agencies = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING agency_id = AgencyID EXCEPT * ).
    DELETE agencies WHERE agency_id IS INITIAL.

    IF  agencies IS NOT INITIAL.
      " Check if Agency ID exists
      SELECT FROM /dmo/agency FIELDS agency_id, country_code
                              FOR ALL ENTRIES IN @agencies
                              WHERE agency_id = @agencies-agency_id
        INTO TABLE @DATA(valid_agencies).
    ENDIF.

    " Raise message for non existing Agency id
    LOOP AT travels INTO DATA(travel).
      APPEND VALUE #(  %tky               = travel-%tky
                       %state_area        = 'VALIDATE_AGENCY'
                    ) TO reported-travel.

      IF travel-AgencyID IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky                = travel-%tky
                        %state_area         = 'VALIDATE_AGENCY'
                        %msg                = NEW /dmo/cm_flight_messages(
                                                          textid   = /dmo/cm_flight_messages=>enter_agency_id
                                                          severity = if_abap_behv_message=>severity-error )
                        %element-AgencyID   = if_abap_behv=>mk-on
                       ) TO reported-travel.

      ELSEIF travel-AgencyID IS NOT INITIAL AND NOT line_exists( valid_agencies[ agency_id = travel-AgencyID ] ).
        APPEND VALUE #(  %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #(  %tky               = travel-%tky
                         %state_area        = 'VALIDATE_AGENCY'
                         %msg               = NEW /dmo/cm_flight_messages(
                                                                agency_id = travel-agencyid
                                                                textid    = /dmo/cm_flight_messages=>agency_unkown
                                                                severity  = if_abap_behv_message=>severity-error )
                         %element-AgencyID  = if_abap_behv=>mk-on
                      ) TO reported-travel.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD validateDates.

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS (  BeginDate EndDate TravelID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    LOOP AT travels INTO DATA(travel).

      APPEND VALUE #(  %tky               = travel-%tky
                       %state_area        = 'VALIDATE_DATES' ) TO reported-travel.

      IF travel-BeginDate IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                         %msg              = NEW /dmo/cm_flight_messages(
                                                                textid   = /dmo/cm_flight_messages=>enter_begin_date
                                                                severity = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
      IF travel-EndDate IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                         %msg                = NEW /dmo/cm_flight_messages(
                                                                textid   = /dmo/cm_flight_messages=>enter_end_date
                                                                severity = if_abap_behv_message=>severity-error )
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
      IF travel-EndDate < travel-BeginDate AND travel-BeginDate IS NOT INITIAL
                                           AND travel-EndDate IS NOT INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               = NEW /dmo/cm_flight_messages(
                                                                textid     = /dmo/cm_flight_messages=>begin_date_bef_end_date
                                                                begin_date = travel-BeginDate
                                                                end_date   = travel-EndDate
                                                                severity   = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
      IF travel-BeginDate < cl_abap_context_info=>get_system_date( ) AND travel-BeginDate IS NOT INITIAL.
        APPEND VALUE #( %tky               = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                         %msg              = NEW /dmo/cm_flight_messages(
                                                                begin_date = travel-BeginDate
                                                                textid     = /dmo/cm_flight_messages=>begin_date_on_or_bef_sysdate
                                                                severity   = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_features.

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS ( OverallStatus )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels)
      FAILED failed.


    result = VALUE #( FOR ls_travel IN travels
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

    "Edit is treated like update
    IF requested_authorizations-%update                =  if_abap_behv=>mk-on OR
       requested_authorizations-%action-Edit           =  if_abap_behv=>mk-on
.

      IF  is_update_granted( ) = abap_true.
        result-%update                =  if_abap_behv=>auth-allowed.
        result-%action-Edit           =  if_abap_behv=>auth-allowed.

      ELSE.
        result-%update                =  if_abap_behv=>auth-unauthorized.
        result-%action-Edit           =  if_abap_behv=>auth-unauthorized.

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

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS ( AgencyID )
        WITH CORRESPONDING #( keys )
        RESULT DATA(travels)
      FAILED failed.

    CHECK travels IS NOT INITIAL.

    "Select country_code and agency of corresponding persistent travel instance
    "authorization  only checked against instance that have active persistence
    SELECT FROM /dmo/a_travel_d AS travel
      INNER JOIN /dmo/agency    AS agency ON travel~agency_id = agency~agency_id
      FIELDS travel~travel_uuid , travel~agency_id, agency~country_code
      FOR ALL ENTRIES IN @travels
      WHERE travel_uuid EQ @travels-TravelUUID
      INTO  TABLE @DATA(travel_agency_country).


    "edit is treated like update
    update_requested = COND #( WHEN requested_authorizations-%update      = if_abap_behv=>mk-on
                                 OR requested_authorizations-%action-Edit = if_abap_behv=>mk-on
                               THEN abap_true ELSE abap_false ).

    delete_requested = COND #( WHEN requested_authorizations-%delete      = if_abap_behv=>mk-on
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
                                                     textid    = /dmo/cm_flight_messages=>not_authorized_for_agencyid
                                                     agency_id = travel-AgencyID
                                                     severity  = if_abap_behv_message=>severity-error )
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

      APPEND VALUE #( LET upd_auth = COND #( WHEN update_granted = abap_true
                                             THEN if_abap_behv=>auth-allowed
                                             ELSE if_abap_behv=>auth-unauthorized )
                          del_auth = COND #( WHEN delete_granted = abap_true
                                             THEN if_abap_behv=>auth-allowed
                                             ELSE if_abap_behv=>auth-unauthorized )
                      IN
                       %tky = travel-%tky
                       %update                = upd_auth
                       %action-Edit           = upd_auth

                       %delete                = del_auth
                    ) TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD precheck_create.
    precheck_auth(
        EXPORTING
          entities_create = entities
        CHANGING
          failed          = failed-travel
          reported        = reported-travel
      ).
  ENDMETHOD.

  METHOD precheck_update.
    precheck_auth(
        EXPORTING
          entities_update = entities
        CHANGING
          failed          = failed-travel
          reported        = reported-travel
      ).
  ENDMETHOD.

  METHOD precheck_auth.
    DATA:
      entities          TYPE t_entities_update,
      operation         TYPE if_abap_behv=>t_char01,
      agencies          TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id,
      is_modify_granted TYPE abap_bool.

    " Either entities_create or entities_update is provided.  NOT both and at least one.
    ASSERT NOT ( entities_create IS INITIAL EQUIV entities_update IS INITIAL ).

    IF entities_create IS NOT INITIAL.
      entities = CORRESPONDING #( entities_create MAPPING %cid_ref = %cid ).
      operation = if_abap_behv=>op-m-create.
    ELSE.
      entities = entities_update.
      operation = if_abap_behv=>op-m-update.
    ENDIF.

    DELETE entities WHERE %control-AgencyID = if_abap_behv=>mk-off.

    agencies = CORRESPONDING #( entities DISCARDING DUPLICATES MAPPING agency_id = AgencyID EXCEPT * ).
    CHECK agencies IS NOT INITIAL.
    SELECT FROM /dmo/agency FIELDS agency_id, country_code
                            FOR ALL ENTRIES IN @agencies
                            WHERE agency_id = @agencies-agency_id
      INTO TABLE @DATA(agency_country_codes).

    LOOP AT entities INTO DATA(entity).
      is_modify_granted = abap_false.

      READ TABLE agency_country_codes WITH KEY agency_id = entity-AgencyID
                   ASSIGNING FIELD-SYMBOL(<agency_country_code>).

      "If invalid or initial AgencyID -> validateAgency
      CHECK sy-subrc = 0.
      CASE operation.
        WHEN if_abap_behv=>op-m-create. is_modify_granted = is_create_granted( <agency_country_code>-country_code ).
        WHEN if_abap_behv=>op-m-update. is_modify_granted = is_update_granted( <agency_country_code>-country_code ).
      ENDCASE.
      IF is_modify_granted = abap_false.
        APPEND VALUE #(
                         %cid      = COND #( WHEN operation = if_abap_behv=>op-m-create THEN entity-%cid_ref )
                         %tky      = entity-%tky
                       ) TO failed.

        APPEND VALUE #(
                         %cid      = COND #( WHEN operation = if_abap_behv=>op-m-create THEN entity-%cid_ref )
                         %tky      = entity-%tky
                         %msg      = NEW /dmo/cm_flight_messages(
                                                 textid    = /dmo/cm_flight_messages=>not_authorized_for_agencyid
                                                 agency_id = entity-AgencyID
                                                 severity  = if_abap_behv_message=>severity-error )
                         %element-AgencyID   = if_abap_behv=>mk-on
                      ) TO reported.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD resume.
    DATA:
      entities_update TYPE t_entities_update.

    READ ENTITIES OF /dmo/r_travel_d IN LOCAL MODE
      ENTITY Travel
        FIELDS ( AgencyID )
        WITH VALUE #(
                      FOR key IN keys
                        %is_draft = if_abap_behv=>mk-on
                        ( %key = key-%key )
                    )
        RESULT DATA(travels).

    " Set %control-AgencyID (if set) to true, so that the precheck_auth checks the permissions.
    entities_update = CORRESPONDING #( travels CHANGING CONTROL ).

    IF entities_update IS NOT INITIAL.
      precheck_auth(
        EXPORTING
          entities_update = entities_update
        CHANGING
          failed          = failed-travel
          reported        = reported-travel
      ).
    ENDIF.
  ENDMETHOD.

  METHOD validateCurrencyCode.
    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY travel
        FIELDS ( currencycode )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    DATA: currencies TYPE SORTED TABLE OF I_Currency WITH UNIQUE KEY currency.

    currencies = CORRESPONDING #(  travels DISCARDING DUPLICATES MAPPING currency = currencycode EXCEPT * ).
    DELETE currencies WHERE currency IS INITIAL.

    IF currencies IS NOT INITIAL.
      SELECT FROM I_Currency FIELDS currency
        FOR ALL ENTRIES IN @currencies
        WHERE currency = @currencies-currency
        INTO TABLE @DATA(currency_db).
    ENDIF.


    LOOP AT travels INTO DATA(travel).
      APPEND VALUE #(  %tky               = travel-%tky
                       %state_area        = 'VALIDATE_CURRENCYCODE'
                    ) TO reported-travel.
      IF travel-currencycode IS INITIAL.
        " Raise message for empty Currency
        APPEND VALUE #( %tky                   = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky                   = travel-%tky
                        %state_area            = 'VALIDATE_CURRENCYCODE'
                        %msg                   = NEW /dmo/cm_flight_messages(
                                                        textid    = /dmo/cm_flight_messages=>currency_required
                                                        severity  = if_abap_behv_message=>severity-error )
                        %element-currencycode = if_abap_behv=>mk-on
                      ) TO reported-travel.
      ELSEIF NOT line_exists( currency_db[ currency = travel-currencycode ] ).
        " Raise message for not existing Currency
        APPEND VALUE #( %tky                   = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky                   = travel-%tky
                        %state_area            = 'VALIDATE_CURRENCYCODE'
                        %msg                   = NEW /dmo/cm_flight_messages(
                                                        textid        = /dmo/cm_flight_messages=>currency_not_existing
                                                        severity      = if_abap_behv_message=>severity-error
                                                        currency_code = travel-currencycode )
                        %element-currencycode = if_abap_behv=>mk-on
                      ) TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.





  METHOD GetDefaultsFordeductDiscount.

    READ ENTITIES OF /DMO/R_TRAVEL_D
      IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TotalPrice )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    LOOP AT travels INTO DATA(travel).
      IF travel-TotalPrice >= 5000.
        APPEND VALUE #( %tky                     = travel-%tky
                        %param-discount_percent  = 20 ) TO result.
      ELSE.
        APPEND VALUE #( %tky                     = travel-%tky
                        %param-discount_percent  = 10 ) TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
