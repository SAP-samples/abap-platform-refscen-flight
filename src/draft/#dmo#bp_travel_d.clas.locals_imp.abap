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

    METHODS get_features FOR FEATURES
      IMPORTING keys REQUEST requested_features FOR Travel RESULT result.




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
                      %msg                = new_message(  id       = '/DMO/CM_FLIGHT_LEGAC'
                                                          number   = '047' "discount invalid
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

    result = VALUE #( FOR travel IN lt_travel ( %tky   = travel-%tky
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

    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY travel
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
                        %msg                = new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                           number   = '044' " Customer is initial
                                                           v1       = ls_travel-TravelID
                                                           severity = if_abap_behv_message=>severity-error )
                        %element-CustomerID = if_abap_behv=>mk-on ) TO reported-travel.

      ELSEIF ls_travel-CustomerID IS NOT INITIAL AND NOT line_exists( lt_customer_db[ customer_id = ls_travel-CustomerID ] ).
        APPEND VALUE #(  %tky = ls_travel-%tky ) TO failed-travel.

        APPEND VALUE #(  %tky                = ls_travel-%tky
                         %state_area         = 'VALIDATE_CUSTOMER'
                         %msg                = new_message( id        = '/DMO/CM_FLIGHT_LEGAC'
                                                             number   = '002' " Customer unknown
                                                             v1       = ls_travel-CustomerID
                                                             severity = if_abap_behv_message=>severity-error )
                         %element-CustomerID = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD validateAgency.

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS (  AgencyID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travel)
      FAILED DATA(lt_failed).

    failed =  CORRESPONDING #( DEEP lt_failed  ).

    DATA lt_agency TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    lt_agency = CORRESPONDING #( lt_travel DISCARDING DUPLICATES MAPPING agency_id = AgencyID EXCEPT * ).
    DELETE lt_agency WHERE agency_id IS INITIAL.

    IF  lt_agency IS NOT INITIAL.
      " Check if customer ID exists
      SELECT FROM /dmo/agency FIELDS agency_id
                              FOR ALL ENTRIES IN @lt_agency
                              WHERE agency_id = @lt_agency-agency_id
      INTO TABLE @DATA(lt_agency_db).
    ENDIF.


    " Raise message for non existing customer id
    LOOP AT lt_travel INTO DATA(ls_travel).
      APPEND VALUE #(  %tky               = ls_travel-%tky
                       %state_area          = 'VALIDATE_AGENCY' ) TO reported-travel.

      IF ls_travel-AgencyID IS  INITIAL.
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky                = ls_travel-%tky
                        %state_area         = 'VALIDATE_AGENCY'
                        %msg                = new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                           number   = '045' " Agency is initial
                                                           v1       = ls_travel-TravelID
                                                           severity = if_abap_behv_message=>severity-error )
                        %element-AgencyID = if_abap_behv=>mk-on ) TO reported-travel.

      ELSEIF ls_travel-AgencyID IS NOT INITIAL AND NOT line_exists( lt_agency_db[ agency_id = ls_travel-AgencyID ] ).
        APPEND VALUE #(  %tky = ls_travel-%tky ) TO failed-travel.

        APPEND VALUE #(  %tky                 = ls_travel-%tky
                         %state_area          = 'VALIDATE_AGENCY'
                         %msg                 = new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                             number   = '001' " Agency unknown
                                                             v1       = ls_travel-AgencyID
                                                             severity = if_abap_behv_message=>severity-error )
                         %element-AgencyID  = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD validateDates.
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS (  BeginDate EndDate )
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
                        %msg               =  new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                           number   = '013' " Enter Begin Date for travel
                                                           v1       = ls_travel-TravelID
                                                           severity = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
      IF ls_travel-EndDate IS INITIAL.
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = ls_travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               =  new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                           number   = '014' " Enter EndDate for travel
                                                           v1       = ls_travel-TravelID
                                                           severity = if_abap_behv_message=>severity-error )
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
      IF ls_travel-EndDate < ls_travel-BeginDate AND ls_travel-BeginDate IS NOT INITIAL
                                                 AND ls_travel-EndDate IS NOT INITIAL.
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = ls_travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               =  new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                           number   = '015' " End date before Begin date
                                                           v1       = ls_travel-BeginDate
                                                           v2       = ls_travel-EndDate
                                                           v3       = ls_travel-TravelID
                                                           severity = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
      IF ls_travel-BeginDate < cl_abap_context_info=>get_system_date( ) AND ls_travel-BeginDate IS NOT INITIAL.
        APPEND VALUE #( %tky               = ls_travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = ls_travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               = new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                          number   = '038' " Begin Date must be in the future
                                                          severity = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_features.

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

ENDCLASS.
