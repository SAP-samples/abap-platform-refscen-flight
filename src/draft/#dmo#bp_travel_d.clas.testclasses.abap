"! @testing BDEF:/DMO/R_Travel_D
CLASS ltc_travel DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_travel,
      cds_test_environment TYPE REF TO if_cds_test_environment,
      sql_test_environment TYPE REF TO if_osql_test_environment.

    CONSTANTS:
      uuid1 TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A1',
      uuid2 TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A2',
      uuid3 TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A3',
      uuid4 TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A4',
      uuid5 TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A5'.

    CLASS-METHODS:
      "! Instantiate class under test and set up test double framework
      class_setup,

      "! Destroy test environment and test double
      class_teardown.


    METHODS:
      "! Reset test double
      setup,

      "! Reset transactional buffer
      teardown.


    METHODS:
      "! Checks if { @link ..lhc_travel.METH:setTravelNumber } draws the correct numbers
      "! when applying without an ID.
      setTravelNumber_idempotence   FOR TESTING,

      "! Checks if { @link ..lhc_travel.METH:setTravelNumber } doesn't draw a number
      "! when applying travels with an ID.
      settravelnumber_newtravelids  FOR TESTING,

      "! Checks if { @link ..lhc_travel.METH:setTravelNumber } draws the correct numbers
      "! when applying travels with and without an ID.
      settravelnumber_mixed         FOR TESTING,

      "! Checks if { @link ..lhc_travel.METH:acceptTravel } sets the status as expected
      "! and returns the entire instance.
      acceptTravel                  FOR TESTING,

      "! Checks if { @link ..lhc_travel.METH:rejectTravel } sets the status as expected
      "! and returns the entire instance.
      rejectTravel                  FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:deductDiscount } with a set of parameter
      "! that are not accepted.
      deductDiscount_invalid_param  FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:deductDiscount } with a correct parameter
      "! and checks if the discount is calculated correctly.
      deductDiscount_success        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:reCalcTotalPrice }
      "! and checks the correct sum using one currency.
      reCalcTotalPrice_one_currency FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:reCalcTotalPrice }
      "! and checks the correct sum using two currencies.
      reCalcTotalPrice_mix_currency FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:setStatusToOpen }
      "! and checks if status is set to open and others not changed.
      setStatusToOpen               FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validateCustomer }
      "! and checks if an existing customer is set.
      validateCustomer_success      FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validateCustomer }
      "! and checks for a message for an initial customer.
      validateCustomer_initial      FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validateCustomer }
      "! and checks for a message for a non-existing customer.
      validateCustomer_not_exist    FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validateAgency }
      "! and checks if an existing agency is set.
      validateAgency_success        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validateAgency }
      "! and checks for a message for an initial Agency.
      validateAgency_initial        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validateAgency }
      "! and checks for a message for a non-existing Agency.
      validateAgency_not_exist      FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validateDates }
      "! and checks if a pair of dates is valid.
      validateDates_success        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validateDates }
      "! and checks if invalid permutations of sets of dates
      "! returns messages.
      validatedates_not_valid      FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:get_instance_features }
      "! using travels with an <em>accepted</em>, <em>rejected</em>,
      "! <em>open</em> and unknown status.
      get_instance_features        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:get_global_authorizations }
      "! and requests the permissions of standard operations
      "! <em>create</em>, <em>update</em>, <em>delete</em> and <em>edit</em>.
      "! As by default we overwrite the authorization checks, so all
      "! operations are always permitted.
      get_global_authorizations     FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:is_create_granted }.
      "! As by default we overwrite the authorization checks, so all
      "! operations are always permitted and <em>abap_true</em> is returned.
      is_create_granted     FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:is_update_granted }.
      "! As by default we overwrite the authorization checks, so all
      "! operations are always permitted and <em>abap_true</em> is returned.
      is_update_granted     FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:is_delete_granted }.
      "! As by default we overwrite the authorization checks, so all
      "! operations are always permitted and <em>abap_true</em> is returned.
      is_delete_granted     FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:get_instance_authorizations }.
      "! As by default we overwrite the authorization checks, so all
      "! operations are always permitted.
      get_instance_authorizations        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:precheck_create }.
      "! Checks successfully data in create case when agency is provided.
      "! Before image is a different agency then the provided one
      precheck_create_provided        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:precheck_create }.
      "! Checks successfully data in create case when agency is empty.
      precheck_create_empty        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:precheck_update }.
      "! Checks successfully data in update case when agency is provided.
      "! Before image is a different agency then the provided one
      precheck_update_provided        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:precheck_update }.
      "! Checks successfully data in update case when agency is empty.
      precheck_update_empty        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:resume }
      "! for a key which can be resumed.
      resume_success               FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validatecurrencycode }
      "! and checks if a pair of status is valid.
      validate_currency_success        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validatecurrencycode }
      "! and checks if invalid permutations of sets of status
      "! returns messages.
      validate_currency_not_valid      FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:GetDefaultsFordeductDiscount }
      "! and checks the defaulting for total prizes
      getdefaultsfordiscount FOR TESTING.




ENDCLASS.


CLASS ltc_travel IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
                               VALUE #(
                                   ( i_for_entity = '/DMO/R_Travel_D'            )
                                   ( i_for_entity = '/DMO/R_Booking_D'           )
                                   ( i_for_entity = '/DMO/R_BookingSupplement_D' )
                                 )
                             ).
    cds_test_environment->enable_double_redirection(  ).
    sql_test_environment = cl_osql_test_environment=>create(
                               VALUE #(
                                   ( 'I_CURRENCY'    )
                                   ( '/DMO/CUSTOMER' )
                                   ( '/DMO/AGENCY'   )
                                 )
                               ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).

    DATA: currencies TYPE STANDARD TABLE OF i_currency.
    currencies = VALUE #(
        ( currency = 'EUR' )
        ( currency = 'USD' )
      ).
    sql_test_environment->insert_test_data( currencies ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
    cds_test_environment->destroy( ).
    sql_test_environment->destroy( ).
  ENDMETHOD.

  METHOD settravelnumber_idempotence.
    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test  TYPE STANDARD TABLE OF /DMO/R_Travel_D WITH KEY TravelUUID,
      exp_travels      TYPE TABLE FOR READ RESULT /DMO/R_Travel_D\\travel,
      reported         TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    travel_mock_data = VALUE #( ( travel_uuid = uuid1  travel_id = '1' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TO ENTITY ).
    exp_travels     = CORRESPONDING #( travel_mock_data MAPPING TO ENTITY ).

    class_under_test->setTravelNumber(
         EXPORTING
           keys     = CORRESPONDING #( travels_to_test )
         CHANGING
           reported = reported
       ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Travel
        FIELDS ( TravelID ) WITH CORRESPONDING #( travels_to_test )
        RESULT DATA(read_result).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_travels
        act = read_result
      ).
  ENDMETHOD.



  METHOD settravelnumber_newtravelids.
    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test  TYPE STANDARD TABLE OF /DMO/R_Travel_D WITH KEY TravelUUID,
      exp_travels      TYPE TABLE FOR READ RESULT /DMO/R_Travel_D\\travel,
      reported         TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    travel_mock_data = VALUE #( ( travel_uuid = uuid1 )
                                ( travel_uuid = uuid2 ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TO ENTITY ).

    class_under_test->setTravelNumber(
       EXPORTING
         keys     = CORRESPONDING #( travels_to_test )
       CHANGING
         reported = reported
     ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Travel
        FIELDS ( TravelID ) WITH CORRESPONDING #( travels_to_test )
        RESULT DATA(read_result).

    SORT read_result BY travelid.

    exp_travels = VALUE #(
        %is_draft = if_abap_behv=>mk-off
          ( traveluuid = uuid1  travelid = '1' )
          ( traveluuid = uuid2  travelid = '2' )
        ).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_travels
        act = read_result
      ).
  ENDMETHOD.



  METHOD settravelnumber_mixed.
    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test  TYPE STANDARD TABLE OF /DMO/R_Travel_D WITH KEY TravelUUID,
      exp_travels      TYPE TABLE FOR READ RESULT /DMO/R_Travel_D\\travel,
      reported         TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    travel_mock_data = VALUE #( ( travel_uuid = uuid1 )
                                ( travel_uuid = uuid2 travel_id = '1' )
                                ( travel_uuid = uuid3 ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TO ENTITY ).

    exp_travels = VALUE #(
        %is_draft = if_abap_behv=>mk-off
          ( traveluuid = uuid1  travelid = '2' )
          ( traveluuid = uuid2  travelid = '1' )
          ( traveluuid = uuid3  travelid = '3' )
        ).

    class_under_test->setTravelNumber(
       EXPORTING
         keys     = CORRESPONDING #( travels_to_test )
       CHANGING
         reported = reported
     ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Travel
        FIELDS ( TravelID ) WITH CORRESPONDING #( travels_to_test )
        RESULT DATA(read_result).

    SORT read_result BY travelid ASCENDING.
    SORT exp_travels BY travelid ASCENDING.

    cl_abap_unit_assert=>assert_equals(
        exp = exp_travels
        act = read_result
      ).
  ENDMETHOD.

  METHOD accepttravel.
    DATA:
      travel_mock_data   TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test    TYPE STANDARD TABLE OF /DMO/R_Travel_D WITH KEY TravelUUID,
      exp_travels_action TYPE TABLE FOR ACTION RESULT /DMO/R_Travel_D\\travel~acceptTravel,
      exp_travel_read    TYPE STRUCTURE FOR READ RESULT /DMO/R_Travel_D\\travel,
      result             TYPE TABLE FOR ACTION RESULT  /DMO/R_Travel_D\\Travel~acceptTravel,
      mapped             TYPE RESPONSE FOR MAPPED EARLY  /DMO/R_Travel_D,
      failed             TYPE RESPONSE FOR FAILED EARLY  /DMO/R_Travel_D,
      reported           TYPE RESPONSE FOR REPORTED EARLY  /DMO/R_Travel_D.

    travel_mock_data = VALUE #( ( travel_uuid = uuid1  overall_status = class_under_test->travel_status-open ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TO ENTITY ).

    exp_travels_action = VALUE #(
        (
          %is_draft     = if_abap_behv=>mk-off
          traveluuid    = uuid1
          %param        = VALUE #(
              %is_draft     = if_abap_behv=>mk-off
              traveluuid    = uuid1
              OverallStatus = class_under_test->travel_status-accepted
            )
        )
      ).

    class_under_test->accepttravel(
        EXPORTING
          keys     = CORRESPONDING #( travels_to_test )
        CHANGING
          result   = result
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped   ).
    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_travels_action
        act = result
      ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Travel
        FIELDS ( TravelID ) WITH CORRESPONDING #( travels_to_test )
        RESULT DATA(read_result).

    cl_abap_unit_assert=>assert_equals( exp = 1  act = lines( read_result ) ).

    exp_travel_read = CORRESPONDING #( exp_travels_action[ 1 ]-%param ).
    cl_abap_unit_assert=>assert_equals(
        exp = exp_travel_read
        act = read_result[ 1 ]
      ).
  ENDMETHOD.

  METHOD rejecttravel.
    DATA:
      travel_mock_data   TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test    TYPE STANDARD TABLE OF /DMO/R_Travel_D WITH KEY TravelUUID,
      exp_travels_action TYPE TABLE FOR ACTION RESULT /DMO/R_Travel_D\\travel~rejectTravel,
      exp_travel_read    TYPE STRUCTURE FOR READ RESULT /DMO/R_Travel_D\\travel,
      result             TYPE TABLE FOR ACTION RESULT  /DMO/R_Travel_D\\Travel~rejectTravel,
      mapped             TYPE RESPONSE FOR MAPPED EARLY  /DMO/R_Travel_D,
      failed             TYPE RESPONSE FOR FAILED EARLY  /DMO/R_Travel_D,
      reported           TYPE RESPONSE FOR REPORTED EARLY  /DMO/R_Travel_D.

    travel_mock_data = VALUE #( ( travel_uuid = uuid1  overall_status = class_under_test->travel_status-open ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TO ENTITY ).

    exp_travels_action = VALUE #(
        (
          %is_draft     = if_abap_behv=>mk-off
          traveluuid    = uuid1
          %param        = VALUE #(
              %is_draft     = if_abap_behv=>mk-off
              traveluuid    = uuid1
              OverallStatus = class_under_test->travel_status-rejected
            )
        )
      ).

    class_under_test->rejecttravel(
        EXPORTING
          keys     = CORRESPONDING #( travels_to_test )
        CHANGING
          result   = result
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped   ).
    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_travels_action
        act = result
      ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Travel
        FIELDS ( TravelID ) WITH CORRESPONDING #( travels_to_test )
        RESULT DATA(read_result).

    cl_abap_unit_assert=>assert_equals( exp = 1  act = lines( read_result ) ).

    exp_travel_read = CORRESPONDING #( exp_travels_action[ 1 ]-%param ).
    cl_abap_unit_assert=>assert_equals(
        exp = exp_travel_read
        act = read_result[ 1 ]
      ).
  ENDMETHOD.

  METHOD deductdiscount_invalid_param.
    TYPES:
      t_keys TYPE TABLE FOR KEY OF /DMO/R_Travel_D\\Travel.
    DATA:
      travels_to_test TYPE TABLE FOR ACTION IMPORT /DMO/R_Travel_D\\travel~deductDiscount,
      result          TYPE TABLE FOR ACTION RESULT  /DMO/R_Travel_D\\Travel~deductDiscount,
      mapped          TYPE RESPONSE FOR MAPPED EARLY  /DMO/R_Travel_D,
      failed          TYPE RESPONSE FOR FAILED EARLY  /DMO/R_Travel_D,
      reported        TYPE RESPONSE FOR REPORTED EARLY  /DMO/R_Travel_D.


    travels_to_test = VALUE #(
        %is_draft = if_abap_behv=>mk-off
        ( TravelUUID = uuid1  %param-discount_percent = '200' )
        ( TravelUUID = uuid3  )
      ).

    class_under_test->deductdiscount(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          result   = result
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( result   ).
    cl_abap_unit_assert=>assert_initial( mapped   ).
    cl_abap_unit_assert=>assert_not_initial( failed   ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = CORRESPONDING t_keys( travels_to_test )
        act = CORRESPONDING t_keys( failed-travel )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = CORRESPONDING t_keys( travels_to_test )
        act = CORRESPONDING t_keys( reported-travel )
      ).
  ENDMETHOD.

  METHOD deductdiscount_success.
    DATA:
      travel_mock_data   TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test    TYPE TABLE FOR ACTION IMPORT /DMO/R_Travel_D\\travel~deductdiscount,
      exp_travels_action TYPE TABLE FOR ACTION RESULT /DMO/R_Travel_D\\travel~deductdiscount,
      exp_travel_read    TYPE STRUCTURE FOR READ RESULT /DMO/R_Travel_D\\travel,
      result             TYPE TABLE FOR ACTION RESULT  /DMO/R_Travel_D\\Travel~deductdiscount,
      mapped             TYPE RESPONSE FOR MAPPED EARLY  /DMO/R_Travel_D,
      failed             TYPE RESPONSE FOR FAILED EARLY  /DMO/R_Travel_D,
      reported           TYPE RESPONSE FOR REPORTED EARLY  /DMO/R_Travel_D.

    travel_mock_data = VALUE #( ( travel_uuid = uuid1  booking_fee = '100'  currency_code = 'EUR' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = VALUE #( ( traveluuid = uuid1  %param-discount_percent  = '50' ) ).

    exp_travels_action = VALUE #(
        (
          %is_draft     = if_abap_behv=>mk-off
          traveluuid    = uuid1
          %param        = VALUE #(
              %is_draft    = if_abap_behv=>mk-off
              traveluuid   = uuid1
              BookingFee   = '50'
              TotalPrice   = '50'
              CurrencyCode = 'EUR'
            )
        )
      ).

    class_under_test->deductdiscount(
        EXPORTING
          keys     = CORRESPONDING #( travels_to_test )
        CHANGING
          result   = result
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped   ).
    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_travels_action
        act = result
      ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Travel
        FIELDS ( TravelID ) WITH CORRESPONDING #( travels_to_test )
        RESULT DATA(read_result).

    exp_travel_read = CORRESPONDING #( exp_travels_action[ 1 ]-%param ).
    cl_abap_unit_assert=>assert_equals(
        exp = exp_travel_read
        act = read_result[ 1 ]
      ).
  ENDMETHOD.

  METHOD recalctotalprice_one_currency.
    CONSTANTS:
      c_currency         TYPE /dmo/currency_code VALUE 'EUR',
      c_booking_fee      TYPE /dmo/booking_fee VALUE '100',
      c_flight_price     TYPE /dmo/flight_price VALUE '10',
      c_supplement_price TYPE /dmo/supplement_price VALUE '1'.

    CONSTANTS:
      booking_uuid1           TYPE /dmo/a_booking_d-booking_uuid VALUE 'B1',
      booking_uuid2           TYPE /dmo/a_booking_d-booking_uuid VALUE 'B2',
      bookingsupplement_uuid1 TYPE /dmo/a_bksuppl_d-booksuppl_uuid VALUE 'C1',
      bookingsupplement_uuid2 TYPE /dmo/a_bksuppl_d-booksuppl_uuid VALUE 'C2',
      bookingsupplement_uuid3 TYPE /dmo/a_bksuppl_d-booksuppl_uuid VALUE 'C3',
      bookingsupplement_uuid4 TYPE /dmo/a_bksuppl_d-booksuppl_uuid VALUE 'C4'.

    DATA:
      travel_mock_data            TYPE STANDARD TABLE OF /dmo/a_travel_d,
      booking_mock_data           TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      travels_to_test             TYPE TABLE FOR ACTION IMPORT /DMO/R_Travel_D\\travel~reCalcTotalPrice,
      exp_travel_read             TYPE TABLE FOR READ RESULT /DMO/R_Travel_D\\travel,
      mapped                      TYPE RESPONSE FOR MAPPED EARLY  /DMO/R_Travel_D,
      failed                      TYPE RESPONSE FOR FAILED EARLY  /DMO/R_Travel_D,
      reported                    TYPE RESPONSE FOR REPORTED EARLY  /DMO/R_Travel_D.

    travels_to_test = VALUE #( ( %is_draft = if_abap_behv=>mk-off  traveluuid = uuid1 ) ).

    travel_mock_data = VALUE #(
        ( travel_uuid = uuid1  booking_fee = c_booking_fee  currency_code = c_currency )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    booking_mock_data = VALUE #(
        flight_price  = c_flight_price
        currency_code = c_currency
        ( booking_uuid = booking_uuid1  parent_uuid = uuid1 )
        ( booking_uuid = booking_uuid2  parent_uuid = uuid1 )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookingsupplement_mock_data = VALUE #(
        price         = c_supplement_price
        currency_code = c_currency
        ( booksuppl_uuid = bookingsupplement_uuid1  parent_uuid = booking_uuid1 )
        ( booksuppl_uuid = bookingsupplement_uuid2  parent_uuid = booking_uuid1 )
        ( booksuppl_uuid = bookingsupplement_uuid3  parent_uuid = booking_uuid2 )
        ( booksuppl_uuid = bookingsupplement_uuid4  parent_uuid = booking_uuid2 )
      ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).


    class_under_test->reCalcTotalPrice(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped   ).
    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Travel
        FIELDS ( TravelID TotalPrice BookingFee CurrencyCode )
        WITH CORRESPONDING #( travels_to_test )
        RESULT DATA(read_result).

    exp_travel_read = VALUE #( (
        %is_draft    = if_abap_behv=>mk-off
        TravelUUID   = uuid1
        BookingFee   = c_booking_fee
        TotalPrice   = c_booking_fee
                         + c_flight_price * lines( booking_mock_data )
                         + c_supplement_price * lines( bookingsupplement_mock_data )
        CurrencyCode = c_currency
      ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_travel_read
        act = read_result
      ).
  ENDMETHOD.

  METHOD recalctotalprice_mix_currency.
    CONSTANTS:
      c_currency_eur     TYPE /dmo/currency_code VALUE 'EUR',
      c_currency_usd     TYPE /dmo/currency_code VALUE 'USD',
      c_booking_fee      TYPE /dmo/booking_fee VALUE '100',
      c_flight_price     TYPE /dmo/flight_price VALUE '10',
      c_supplement_price TYPE /dmo/supplement_price VALUE '1'.

    CONSTANTS:
      booking_uuid1           TYPE /dmo/a_booking_d-booking_uuid VALUE 'B1',
      booking_uuid2           TYPE /dmo/a_booking_d-booking_uuid VALUE 'B2',
      bookingsupplement_uuid1 TYPE /dmo/a_bksuppl_d-booksuppl_uuid VALUE 'C1',
      bookingsupplement_uuid2 TYPE /dmo/a_bksuppl_d-booksuppl_uuid VALUE 'C2',
      bookingsupplement_uuid3 TYPE /dmo/a_bksuppl_d-booksuppl_uuid VALUE 'C3',
      bookingsupplement_uuid4 TYPE /dmo/a_bksuppl_d-booksuppl_uuid VALUE 'C4'.

    DATA:
      travel_mock_data            TYPE STANDARD TABLE OF /dmo/a_travel_d,
      booking_mock_data           TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      travels_to_test             TYPE TABLE FOR ACTION IMPORT /DMO/R_Travel_D\\travel~reCalcTotalPrice,
      exp_travel_read             TYPE TABLE FOR READ RESULT /DMO/R_Travel_D\\travel,
      mapped                      TYPE RESPONSE FOR MAPPED EARLY  /DMO/R_Travel_D,
      failed                      TYPE RESPONSE FOR FAILED EARLY  /DMO/R_Travel_D,
      reported                    TYPE RESPONSE FOR REPORTED EARLY  /DMO/R_Travel_D.

    travels_to_test = VALUE #( ( %is_draft = if_abap_behv=>mk-off  traveluuid = uuid1 ) ).

    travel_mock_data = VALUE #(
        ( travel_uuid = uuid1  booking_fee = c_booking_fee  currency_code = c_currency_eur )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    booking_mock_data = VALUE #(
        flight_price  = c_flight_price
        currency_code = c_currency_eur
        ( booking_uuid = booking_uuid1  parent_uuid = uuid1 )
        ( booking_uuid = booking_uuid2  parent_uuid = uuid1 )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookingsupplement_mock_data = VALUE #(
        price         = c_supplement_price
        currency_code = c_currency_eur
        ( booksuppl_uuid = bookingsupplement_uuid1  parent_uuid = booking_uuid1 )
        ( booksuppl_uuid = bookingsupplement_uuid2  parent_uuid = booking_uuid1 )
        ( booksuppl_uuid = bookingsupplement_uuid3  parent_uuid = booking_uuid2 )

        currency_code = c_currency_usd
        ( booksuppl_uuid = bookingsupplement_uuid4  parent_uuid = booking_uuid2 )
      ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).


    class_under_test->reCalcTotalPrice(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped   ).
    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Travel
        FIELDS ( TravelID TotalPrice BookingFee CurrencyCode )
        WITH CORRESPONDING #( travels_to_test )
        RESULT DATA(read_result).

    /dmo/cl_flight_amdp=>convert_currency(
       EXPORTING
         iv_amount                   =  c_supplement_price
         iv_currency_code_source     =  c_currency_usd
         iv_currency_code_target     =  c_currency_eur
         iv_exchange_rate_date       =  cl_abap_context_info=>get_system_date( )
       IMPORTING
         ev_amount                   = DATA(converted_supplement_price)
      ).


    exp_travel_read = VALUE #( (
        %is_draft    = if_abap_behv=>mk-off
        TravelUUID   = uuid1
        BookingFee   = c_booking_fee
        TotalPrice   = c_booking_fee
                         + c_flight_price * lines( booking_mock_data )
                         + c_supplement_price * ( lines( bookingsupplement_mock_data ) - 1 )
                         + converted_supplement_price
        CurrencyCode = c_currency_eur
      ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_travel_read
        act = read_result
      ).
  ENDMETHOD.

  METHOD setstatustoopen.
    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test  TYPE TABLE FOR DETERMINATION /DMO/R_Travel_D\\travel~setStatusToOpen,
      exp_travel_read  TYPE TABLE FOR READ RESULT /DMO/R_Travel_D\\travel,
      reported         TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    travel_mock_data = VALUE #(
        ( travel_uuid = uuid1 )
        ( travel_uuid = uuid2  overall_status = 'T' )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TravelUUID = travel_uuid ).

    class_under_test->setStatusToOpen(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( reported ).

    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Travel
        FIELDS ( TravelID OverallStatus )
        WITH CORRESPONDING #( travels_to_test )
        RESULT DATA(read_result).

    exp_travel_read = VALUE #(
        %is_draft = if_abap_behv=>mk-off
        ( TravelUUID = uuid1  OverallStatus = class_under_test->travel_status-open )
        ( TravelUUID = uuid2  OverallStatus = 'T' )
      ).

    SORT read_result     BY TravelUUID ASCENDING.
    SORT exp_travel_read BY TravelUUID ASCENDING.

    cl_abap_unit_assert=>assert_equals(
        exp = exp_travel_read
        act = read_result
      ).
  ENDMETHOD.

  METHOD validatecustomer_success.
    CONSTANTS:
      c_customer_id TYPE /dmo/customer_id VALUE '123'.

    DATA:
      customer_mock_data TYPE STANDARD TABLE OF /dmo/customer,
      travel_mock_data   TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test    TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\travel~validateCustomer,
      failed             TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported           TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    travel_mock_data = VALUE #(
        ( travel_uuid = uuid1  customer_id = c_customer_id )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TravelUUID = travel_uuid ).

    class_under_test->validateCustomer(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( reported-travel )
      ).
  ENDMETHOD.

  METHOD validatecustomer_initial.
    CONSTANTS:
      c_customer_id           TYPE /dmo/customer_id VALUE '123',
      c_customer_id_of_travel TYPE /dmo/customer_id VALUE '111'.

    DATA:
      customer_mock_data TYPE STANDARD TABLE OF /dmo/customer,
      travel_mock_data   TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test    TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\travel~validateCustomer,
      failed             TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported           TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    travel_mock_data = VALUE #(
        ( travel_uuid = uuid1  customer_id = c_customer_id_of_travel )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TravelUUID = travel_uuid ).

    class_under_test->validateCustomer(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-travel )
      ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lines( reported-travel )
      ).
  ENDMETHOD.

  METHOD validatecustomer_not_exist.
    CONSTANTS:
      c_customer_id           TYPE /dmo/customer_id VALUE '123',
      c_customer_id_of_travel TYPE /dmo/customer_id VALUE IS INITIAL.

    DATA:
      customer_mock_data TYPE STANDARD TABLE OF /dmo/customer,
      travel_mock_data   TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test    TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\travel~validateCustomer,
      failed             TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported           TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    travel_mock_data = VALUE #(
        ( travel_uuid = uuid1  customer_id = c_customer_id_of_travel )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TravelUUID = travel_uuid ).

    class_under_test->validateCustomer(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-travel )
      ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lines( reported-travel )
      ).
  ENDMETHOD.

  METHOD validateagency_success.
    CONSTANTS:
      c_agency_id TYPE /dmo/agency_id VALUE '123'.

    DATA:
      agency_mock_data TYPE STANDARD TABLE OF /dmo/agency,
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test  TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\travel~validateagency,
      failed           TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported         TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    agency_mock_data = VALUE #( ( agency_id = c_agency_id ) ).
    sql_test_environment->insert_test_data( agency_mock_data ).

    travel_mock_data = VALUE #(
        ( travel_uuid = uuid1  agency_id = c_agency_id )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TravelUUID = travel_uuid ).

    class_under_test->validateagency(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( reported-travel )
      ).
  ENDMETHOD.

  METHOD validateagency_initial.
    CONSTANTS:
      c_agency_id           TYPE /dmo/agency_id VALUE '123',
      c_agency_id_of_travel TYPE /dmo/agency_id VALUE '111'.

    DATA:
      agency_mock_data TYPE STANDARD TABLE OF /dmo/agency,
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test  TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\travel~validateagency,
      failed           TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported         TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    agency_mock_data = VALUE #( ( agency_id = c_agency_id ) ).
    sql_test_environment->insert_test_data( agency_mock_data ).

    travel_mock_data = VALUE #(
        ( travel_uuid = uuid1  agency_id = c_agency_id_of_travel )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TravelUUID = travel_uuid ).

    class_under_test->validateagency(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-travel )
      ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lines( reported-travel )
      ).
  ENDMETHOD.

  METHOD validateagency_not_exist.
    CONSTANTS:
      c_agency_id           TYPE /dmo/agency_id VALUE '123',
      c_agency_id_of_travel TYPE /dmo/agency_id VALUE IS INITIAL.

    DATA:
      agency_mock_data TYPE STANDARD TABLE OF /dmo/agency,
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test  TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\travel~validateagency,
      failed           TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported         TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    agency_mock_data = VALUE #( ( agency_id = c_agency_id ) ).
    sql_test_environment->insert_test_data( agency_mock_data ).

    travel_mock_data = VALUE #(
        ( travel_uuid = uuid1  agency_id = c_agency_id_of_travel )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TravelUUID = travel_uuid ).

    class_under_test->validateagency(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-travel )
      ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lines( reported-travel )
      ).
  ENDMETHOD.

  METHOD validatedates_success.
    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test  TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\travel~validateDates,
      failed           TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported         TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D,
      today            TYPE cl_abap_context_info=>ty_system_date,
      tomorrow         TYPE cl_abap_context_info=>ty_system_date.

    today    = cl_abap_context_info=>get_system_date( ).
    tomorrow = cl_abap_context_info=>get_system_date( ) + 1.

    travel_mock_data = VALUE #(
        ( travel_uuid = uuid1  begin_date = today  end_date = tomorrow )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TravelUUID = travel_uuid ).

    class_under_test->validatedates(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( reported-travel )
      ).
  ENDMETHOD.

  METHOD validatedates_not_valid.
    TYPES: BEGIN OF t_check.
             INCLUDE TYPE /DMO/A_Travel_D.
    TYPES:   exp_amount_reported_entries TYPE i,
             exp_amount_failed_entries   TYPE i,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY travel_uuid.

    CONSTANTS:
      uuid5 TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A5',
      uuid6 TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A6'.

    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_check TYPE t_check_table,
      travel_to_check  TYPE t_check,
      travels_to_test  TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\travel~validateDates,
      failed           TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported         TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D,
      one_week_ago     TYPE cl_abap_context_info=>ty_system_date,
      yesterday        TYPE cl_abap_context_info=>ty_system_date,
      today            TYPE cl_abap_context_info=>ty_system_date,
      tomorrow         TYPE cl_abap_context_info=>ty_system_date.


    one_week_ago = cl_abap_context_info=>get_system_date( ) - 7.
    yesterday    = cl_abap_context_info=>get_system_date( ) - 1.
    today        = cl_abap_context_info=>get_system_date( ).
    tomorrow     = cl_abap_context_info=>get_system_date( ) + 1.

    travels_to_check = VALUE #(
        exp_amount_reported_entries = '2'
        exp_amount_failed_entries   = '1'
        ( travel_uuid = uuid1                             end_date = today         description = 'Begin initial'       )
        ( travel_uuid = uuid2  begin_date = today                                  description = 'End initial'         )
        ( travel_uuid = uuid3  begin_date = tomorrow      end_date = today         description = 'Begin > End'         )
        ( travel_uuid = uuid4  begin_date = yesterday     end_date = today         description = 'Begin < today'       )
        ( travel_uuid = uuid5  begin_date = yesterday     end_date = one_week_ago  description = 'End < Begin < today' )

        exp_amount_reported_entries = '3'
        exp_amount_failed_entries   = '2'
        ( travel_uuid = uuid6  begin_date = one_week_ago  end_date = yesterday     description = 'Begin < End < today' )
      ).
    travel_mock_data = CORRESPONDING #( travels_to_check ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TravelUUID = travel_uuid ).

    class_under_test->validatedates(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR travel IN travels_to_check
                            NEXT sum += travel-exp_amount_failed_entries )
           act  = lines( failed-travel )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT travels_to_check INTO travel_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = travel_to_check-exp_amount_failed_entries
            act  = lines( FILTER #( failed-travel USING KEY entity WHERE TravelUUID = travel_to_check-travel_uuid ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ travel_to_check-exp_amount_failed_entries } line(s) in failed expected for '{ travel_to_check-description }'|
          ).
      ENDLOOP.
    ENDIF.

    cl_abap_unit_assert=>assert_not_initial( reported ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR travel IN travels_to_check
                            NEXT sum += travel-exp_amount_reported_entries )
           act  = lines( reported-travel )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT travels_to_check INTO travel_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = travel_to_check-exp_amount_reported_entries
            act  = lines( FILTER #( reported-travel USING KEY entity WHERE TravelUUID = travel_to_check-travel_uuid ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ travel_to_check-exp_amount_reported_entries } line(s) in reported expected for '{ travel_to_check-description }'|
          ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_instance_features.
    TYPES: t_instance_feature TYPE STRUCTURE FOR INSTANCE FEATURES RESULT /DMO/R_Travel_D\\travel,
           BEGIN OF t_check.
             INCLUDE TYPE t_instance_feature.
    TYPES: overallstatus TYPE /DMO/R_Travel_D-OverallStatus,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY traveluuid.

    DATA:
      check_table        TYPE t_check_table,
      travel_mock_data   TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      travels_to_test    TYPE TABLE FOR INSTANCE FEATURES KEY /DMO/R_Travel_D\\travel,
      requested_features TYPE STRUCTURE FOR INSTANCE FEATURES REQUEST /DMO/R_Travel_D\\travel,
      act_result         TYPE TABLE FOR INSTANCE FEATURES RESULT /DMO/R_Travel_D\\travel,
      exp_result         TYPE TABLE FOR INSTANCE FEATURES RESULT /DMO/R_Travel_D\\travel,
      reported           TYPE RESPONSE FOR REPORTED EARLY /DMO/R_Travel_D,
      failed             TYPE RESPONSE FOR FAILED   EARLY /DMO/R_Travel_D.

    " In current implementation requested_features is not used.
    requested_features = VALUE #( ).

    check_table = VALUE t_check_table(
        (
          %is_draft              = if_abap_behv=>mk-off
          traveluuid             = uuid1
          overallstatus          = class_under_test->travel_status-accepted
          %field-BookingFee      = if_abap_behv=>fc-f-read_only
          %action-acceptTravel   = if_abap_behv=>fc-o-disabled
          %action-rejectTravel   = if_abap_behv=>fc-o-enabled
          %action-deductDiscount = if_abap_behv=>fc-o-disabled
          %assoc-_Booking        = if_abap_behv=>fc-o-enabled
        )
        (
          %is_draft              = if_abap_behv=>mk-off
          traveluuid             = uuid2
          overallstatus          = class_under_test->travel_status-rejected
          %field-BookingFee      = if_abap_behv=>fc-f-unrestricted
          %action-acceptTravel   = if_abap_behv=>fc-o-enabled
          %action-rejectTravel   = if_abap_behv=>fc-o-disabled
          %action-deductDiscount = if_abap_behv=>fc-o-enabled
          %assoc-_Booking        = if_abap_behv=>fc-o-disabled
        )
        (
          %is_draft              = if_abap_behv=>mk-off
          traveluuid             = uuid3
          overallstatus          = class_under_test->travel_status-open
          %field-BookingFee      = if_abap_behv=>fc-f-unrestricted
          %action-acceptTravel   = if_abap_behv=>fc-o-enabled
          %action-rejectTravel   = if_abap_behv=>fc-o-enabled
          %action-deductDiscount = if_abap_behv=>fc-o-enabled
          %assoc-_Booking        = if_abap_behv=>fc-o-enabled
        )
        (
          %is_draft              = if_abap_behv=>mk-off
          traveluuid             = uuid4
          overallstatus          = 'T'
          %field-BookingFee      = if_abap_behv=>fc-f-unrestricted
          %action-acceptTravel   = if_abap_behv=>fc-o-enabled
          %action-rejectTravel   = if_abap_behv=>fc-o-enabled
          %action-deductDiscount = if_abap_behv=>fc-o-enabled
          %assoc-_Booking        = if_abap_behv=>fc-o-enabled
        )
      ).

    travel_mock_data = CORRESPONDING #(
                          check_table
                          MAPPING
                            travel_uuid    = traveluuid
                            overall_status = overallstatus
                          EXCEPT *
                        ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( check_table ).

    exp_result = CORRESPONDING #( check_table ).

    class_under_test->get_instance_features(
      EXPORTING
        keys               = travels_to_test
        requested_features = requested_features
      CHANGING
        result             = act_result
        failed             = failed
        reported           = reported
    ).

    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( failed   ).

    SORT act_result BY TravelUUID ASCENDING.
    SORT exp_result BY TravelUUID ASCENDING.

    cl_abap_unit_assert=>assert_equals(
        exp = exp_result
        act = act_result
      ).
  ENDMETHOD.

  METHOD get_global_authorizations.
    DATA:
      requested_authorizations TYPE STRUCTURE FOR GLOBAL AUTHORIZATION REQUEST /DMO/R_Travel_D\\travel,
      exp_result               TYPE STRUCTURE FOR GLOBAL AUTHORIZATION RESULT /DMO/R_Travel_D\\travel,
      act_result               TYPE STRUCTURE FOR GLOBAL AUTHORIZATION RESULT /DMO/R_Travel_D\\travel,
      reported                 TYPE RESPONSE FOR REPORTED EARLY /DMO/R_Travel_D.

    requested_authorizations = VALUE #(
         %create      = if_abap_behv=>mk-on
         %update      = if_abap_behv=>mk-on
         %delete      = if_abap_behv=>mk-on
         %action-edit = if_abap_behv=>mk-on
      ).

    exp_result = VALUE #(
         %create      = if_abap_behv=>auth-allowed
         %update      = if_abap_behv=>auth-allowed
         %delete      = if_abap_behv=>auth-allowed
         %action-edit = if_abap_behv=>auth-allowed
      ).

    class_under_test->get_global_authorizations(
        EXPORTING
          requested_authorizations = requested_authorizations
        CHANGING
          result                   = act_result
          reported                 = reported
      ).

    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_result
        act = act_result
      ).
  ENDMETHOD.

  METHOD is_create_granted.
    cl_abap_unit_assert=>assert_true( class_under_test->is_create_granted(      ) ).
    cl_abap_unit_assert=>assert_true( class_under_test->is_create_granted( 'DE' ) ).
  ENDMETHOD.

  METHOD is_update_granted.
    cl_abap_unit_assert=>assert_true( class_under_test->is_update_granted(      ) ).
    cl_abap_unit_assert=>assert_true( class_under_test->is_update_granted( 'DE' ) ).
  ENDMETHOD.

  METHOD is_delete_granted.
    cl_abap_unit_assert=>assert_true( class_under_test->is_delete_granted(      ) ).
    cl_abap_unit_assert=>assert_true( class_under_test->is_delete_granted( 'DE' ) ).
  ENDMETHOD.

  METHOD get_instance_authorizations.
    CONSTANTS:
      c_agency1    TYPE /dmo/agency_id VALUE '1337',
      c_agency2    TYPE /dmo/agency_id VALUE '1338',
      c_country_de TYPE /dmo/agency-country_code VALUE 'US' ##NO_TEXT,
      c_country_us TYPE /dmo/agency-country_code VALUE 'DE' ##NO_TEXT.

    DATA:
      travel_mock_data         TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      agency_mock_data         TYPE STANDARD TABLE OF /dmo/agency,
      travels_to_test          TYPE TABLE FOR AUTHORIZATION KEY /DMO/R_Travel_D\\travel,
      requested_authorizations TYPE STRUCTURE FOR AUTHORIZATION REQUEST /DMO/R_Travel_D\\travel,
      act_result               TYPE TABLE FOR AUTHORIZATION RESULT /DMO/R_Travel_D\\travel,
      exp_result               TYPE TABLE FOR AUTHORIZATION RESULT /DMO/R_Travel_D\\travel,
      failed                   TYPE RESPONSE FOR FAILED EARLY /DMO/R_Travel_D,
      reported                 TYPE RESPONSE FOR REPORTED EARLY /DMO/R_Travel_D.

    agency_mock_data = VALUE #(
        ( agency_id = c_agency1  country_code = c_country_de )
        ( agency_id = c_agency2  country_code = c_country_us )
      ).
    sql_test_environment->insert_test_data( agency_mock_data ).

    travel_mock_data = VALUE #(
         ( travel_uuid = uuid1  agency_id = c_agency1 )
         ( travel_uuid = uuid2  agency_id = c_agency2 )
       ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TravelUUID = travel_uuid ).

    requested_authorizations = VALUE #(
        %update      = if_abap_behv=>mk-on
        %delete      = if_abap_behv=>mk-on
        %action-edit = if_abap_behv=>mk-on
      ).

    class_under_test->get_instance_authorizations(
        EXPORTING
          keys                     = travels_to_test
          requested_authorizations = requested_authorizations
        CHANGING
          result                   = act_result
          failed                   = failed
          reported                 = reported
      ).

    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( failed   ).

    exp_result = VALUE #(
        %is_draft    = if_abap_behv=>mk-off
        %update      = if_abap_behv=>auth-allowed
        %delete      = if_abap_behv=>auth-allowed
        %action-edit = if_abap_behv=>auth-allowed
        ( TravelUUID   = uuid1 )
        ( TravelUUID   = uuid2 )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_result
        act = act_result
      ).
  ENDMETHOD.

  METHOD precheck_create_provided.
    CONSTANTS:
      c_agency1    TYPE /dmo/agency_id VALUE '1337',
      c_agency2    TYPE /dmo/agency_id VALUE '1338',
      c_country_de TYPE /dmo/agency-country_code VALUE 'US' ##NO_TEXT,
      c_country_us TYPE /dmo/agency-country_code VALUE 'DE' ##NO_TEXT.

    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      agency_mock_data TYPE STANDARD TABLE OF /dmo/agency,
      travels_to_test  TYPE TABLE FOR CREATE /DMO/R_Travel_D\\travel,
      failed           TYPE RESPONSE FOR FAILED EARLY /DMO/R_Travel_D,
      reported         TYPE RESPONSE FOR REPORTED EARLY /DMO/R_Travel_D.

    agency_mock_data = VALUE #(
        ( agency_id = c_agency1  country_code = c_country_de )
        ( agency_id = c_agency2  country_code = c_country_us )
      ).
    sql_test_environment->insert_test_data( agency_mock_data ).

    travel_mock_data = VALUE #(
         ( travel_uuid = uuid1  agency_id = c_agency1 )
       ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = VALUE #(
        (
          TravelUUID          = uuid1
          AgencyID            = c_agency2
          %control-TravelUUID = if_abap_behv=>mk-on
          %control-AgencyID   = if_abap_behv=>mk-on
        )
      ).

    class_under_test->precheck_create(
        EXPORTING
          entities                 = travels_to_test
        CHANGING
          failed                   = failed
          reported                 = reported
      ).

    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( failed   ).
  ENDMETHOD.

  METHOD precheck_create_empty.
    CONSTANTS:
      c_agency1    TYPE /dmo/agency_id VALUE '1337',
      c_agency2    TYPE /dmo/agency_id VALUE '1338',
      c_country_de TYPE /dmo/agency-country_code VALUE 'US' ##NO_TEXT,
      c_country_us TYPE /dmo/agency-country_code VALUE 'DE' ##NO_TEXT.

    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      agency_mock_data TYPE STANDARD TABLE OF /dmo/agency,
      travels_to_test  TYPE TABLE FOR CREATE /DMO/R_Travel_D\\travel,
      failed           TYPE RESPONSE FOR FAILED EARLY /DMO/R_Travel_D,
      reported         TYPE RESPONSE FOR REPORTED EARLY /DMO/R_Travel_D.

    agency_mock_data = VALUE #(
        ( agency_id = c_agency1  country_code = c_country_de )
        ( agency_id = c_agency2  country_code = c_country_us )
      ).
    sql_test_environment->insert_test_data( agency_mock_data ).

    travel_mock_data = VALUE #(
         ( travel_uuid = uuid1  agency_id = c_agency1 )
       ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = VALUE #( ##NO_TEXT
        (
          TravelUUID           = uuid1
          Description          = 'Test'
          %control-TravelUUID  = if_abap_behv=>mk-on
          %control-Description = if_abap_behv=>mk-on
        )
      ).

    class_under_test->precheck_create(
        EXPORTING
          entities                 = travels_to_test
        CHANGING
          failed                   = failed
          reported                 = reported
      ).

    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( failed   ).
  ENDMETHOD.

  METHOD precheck_update_provided.
    CONSTANTS:
      c_agency1    TYPE /dmo/agency_id VALUE '1337',
      c_agency2    TYPE /dmo/agency_id VALUE '1338',
      c_country_de TYPE /dmo/agency-country_code VALUE 'US' ##NO_TEXT,
      c_country_us TYPE /dmo/agency-country_code VALUE 'DE' ##NO_TEXT.

    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      agency_mock_data TYPE STANDARD TABLE OF /dmo/agency,
      travels_to_test  TYPE TABLE FOR UPDATE /DMO/R_Travel_D\\travel,
      failed           TYPE RESPONSE FOR FAILED EARLY /DMO/R_Travel_D,
      reported         TYPE RESPONSE FOR REPORTED EARLY /DMO/R_Travel_D.

    agency_mock_data = VALUE #(
        ( agency_id = c_agency1  country_code = c_country_de )
        ( agency_id = c_agency2  country_code = c_country_us )
      ).
    sql_test_environment->insert_test_data( agency_mock_data ).

    travel_mock_data = VALUE #(
         ( travel_uuid = uuid1  agency_id = c_agency1 )
       ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = VALUE #(
        (
          TravelUUID          = uuid1
          AgencyID            = c_agency2
          %control-TravelUUID = if_abap_behv=>mk-on
          %control-AgencyID   = if_abap_behv=>mk-on
        )
      ).

    class_under_test->precheck_update(
        EXPORTING
          entities                 = travels_to_test
        CHANGING
          failed                   = failed
          reported                 = reported
      ).

    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( failed   ).
  ENDMETHOD.

  METHOD precheck_update_empty.
    CONSTANTS:
      c_agency1    TYPE /dmo/agency_id VALUE '1337',
      c_agency2    TYPE /dmo/agency_id VALUE '1338',
      c_country_de TYPE /dmo/agency-country_code VALUE 'US' ##NO_TEXT,
      c_country_us TYPE /dmo/agency-country_code VALUE 'DE' ##NO_TEXT.

    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      agency_mock_data TYPE STANDARD TABLE OF /dmo/agency,
      travels_to_test  TYPE TABLE FOR UPDATE /DMO/R_Travel_D\\travel,
      failed           TYPE RESPONSE FOR FAILED EARLY /DMO/R_Travel_D,
      reported         TYPE RESPONSE FOR REPORTED EARLY /DMO/R_Travel_D.

    agency_mock_data = VALUE #(
        ( agency_id = c_agency1  country_code = c_country_de )
        ( agency_id = c_agency2  country_code = c_country_us )
      ).
    sql_test_environment->insert_test_data( agency_mock_data ).

    travel_mock_data = VALUE #(
         ( travel_uuid = uuid1  agency_id = c_agency1 )
       ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = VALUE #( ##NO_TEXT
        (
          TravelUUID           = uuid1
          Description          = 'Test'
          %control-TravelUUID  = if_abap_behv=>mk-on
          %control-Description = if_abap_behv=>mk-on
        )
      ).

    class_under_test->precheck_update(
        EXPORTING
          entities                 = travels_to_test
        CHANGING
          failed                   = failed
          reported                 = reported
      ).

    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( failed   ).
  ENDMETHOD.

  METHOD resume_success.
    CONSTANTS:
      c_agency1    TYPE /dmo/agency_id VALUE '1337',
      c_agency2    TYPE /dmo/agency_id VALUE '1338',
      c_country_de TYPE /dmo/agency-country_code VALUE 'US' ##NO_TEXT,
      c_country_us TYPE /dmo/agency-country_code VALUE 'DE' ##NO_TEXT.

    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D,
      agency_mock_data TYPE STANDARD TABLE OF /dmo/agency,
      travels_to_test  TYPE TABLE FOR ACTION IMPORT /dmo/r_travel_d\\travel~resume,
      failed           TYPE RESPONSE FOR FAILED EARLY /DMO/R_Travel_D,
      reported         TYPE RESPONSE FOR REPORTED EARLY /DMO/R_Travel_D,
      mapped           TYPE RESPONSE FOR MAPPED /dmo/r_travel_d.

    agency_mock_data = VALUE #(
        ( agency_id = c_agency1  country_code = c_country_de )
        ( agency_id = c_agency2  country_code = c_country_us )
      ).
    sql_test_environment->insert_test_data( agency_mock_data ).

    travel_mock_data = VALUE #(
         ( travel_uuid = uuid1  agency_id = c_agency1 )
       ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = VALUE #( ( TravelUUID = uuid1 ) ).

    class_under_test->resume(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped   ).
    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( failed   ).
  ENDMETHOD.

  METHOD validate_currency_success.
    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /dmo/a_travel_d,
      travels_to_test  TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\travel~validatecurrencycode,
      failed           TYPE RESPONSE FOR FAILED   LATE /dmo/r_travel_d,
      reported         TYPE RESPONSE FOR REPORTED LATE /dmo/r_travel_d.

    travel_mock_data = VALUE #(
        ( travel_uuid = uuid1  currency_code = 'EUR' )
        ( travel_uuid = uuid2  currency_code = 'USD' )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING traveluuid = travel_uuid ).

    class_under_test->validatecurrencycode(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = lines( travels_to_test )
        act = lines( reported-travel )
      ).
  ENDMETHOD.

  METHOD validate_currency_not_valid.
    TYPES: BEGIN OF t_check.
             INCLUDE TYPE /dmo/a_travel_d.
    TYPES:   exp_amount_reported_entries TYPE i,
             exp_amount_failed_entries   TYPE i,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY travel_uuid.

    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /dmo/a_travel_d,
      travels_to_check TYPE t_check_table,
      travel_to_check  TYPE t_check,
      travels_to_test  TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\travel~validatecurrencycode,
      failed           TYPE RESPONSE FOR FAILED   LATE /dmo/r_travel_d,
      reported         TYPE RESPONSE FOR REPORTED LATE /dmo/r_travel_d.

    travels_to_check = VALUE #(
        exp_amount_reported_entries = '2'
        exp_amount_failed_entries   = '1'
        ( travel_uuid = uuid1  currency_code = ''    )
        ( travel_uuid = uuid2  currency_code = 'XXX' )
      ).
    travel_mock_data = CORRESPONDING #( travels_to_check ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING traveluuid = travel_uuid ).

    class_under_test->validatecurrencycode(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR travel IN travels_to_check
                            NEXT sum += travel-exp_amount_failed_entries )
           act  = lines( failed-travel )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT travels_to_check INTO travel_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = travel_to_check-exp_amount_failed_entries
            act  = lines( FILTER #( failed-travel USING KEY entity WHERE traveluuid = travel_to_check-travel_uuid ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ travel_to_check-exp_amount_failed_entries } line(s) in failed expected for '{ travel_to_check-description }'|
          ).
      ENDLOOP.
    ENDIF.

    cl_abap_unit_assert=>assert_not_initial( reported ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR travel IN travels_to_check
                            NEXT sum += travel-exp_amount_reported_entries )
           act  = lines( reported-travel )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT travels_to_check INTO travel_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = travel_to_check-exp_amount_reported_entries
            act  = lines( FILTER #( reported-travel USING KEY entity WHERE traveluuid = travel_to_check-travel_uuid ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ travel_to_check-exp_amount_reported_entries } line(s) in reported expected for '{ travel_to_check-description }'|
          ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.



  METHOD getdefaultsfordiscount.

    DATA:
      travel_mock_data   TYPE STANDARD TABLE OF /dmo/a_travel_d,
      travels_to_test    TYPE TABLE FOR FUNCTION IMPORT /dmo/r_travel_d\\travel~GetDefaultsFordeductDiscount,
      exp_travels_action TYPE TABLE FOR FUNCTION RESULT /dmo/r_travel_d\\travel~GetDefaultsFordeductDiscount,
      result             TYPE TABLE FOR FUNCTION RESULT /dmo/r_travel_d\\travel~GetDefaultsFordeductDiscount,
      failed             TYPE RESPONSE FOR FAILED   EARLY /dmo/r_travel_d,
      reported           TYPE RESPONSE FOR REPORTED EARLY /dmo/r_travel_d.

      travel_mock_data = VALUE #( ( travel_uuid = uuid1 total_price = '' )
                                  ( travel_uuid = uuid2 total_price = '0' )
                                  ( travel_uuid = uuid3 total_price = '1' )
                                  ( travel_uuid = uuid4 total_price = '5000' )
                                  ( travel_uuid = uuid5 total_price = '5001' ) ).

      cds_test_environment->insert_test_data( travel_mock_data ).

      exp_travels_action = VALUE #(
        (
          %is_draft     = if_abap_behv=>mk-off
          traveluuid    = uuid1
          %param        = VALUE #(
              discount_percent = '10' ) )
        ( %is_draft     = if_abap_behv=>mk-off
          traveluuid    = uuid2
          %param        = VALUE #(
              discount_percent = '10' ) )
        ( %is_draft   = if_abap_behv=>mk-off
          traveluuid = uuid3
          %param = VALUE #(
              discount_percent = '10' ) )
        ( %is_draft   = if_abap_behv=>mk-off
          traveluuid = uuid4
          %param = VALUE #(
              discount_percent = '20' ) )
        ( %is_draft   = if_abap_behv=>mk-off
          traveluuid = uuid5
          %param = VALUE #(
              discount_percent = '20' ) )
         ).

      travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TravelUUID = travel_uuid EXCEPT * ).

      class_under_test->getdefaultsfordeductdiscount(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          result   = result
          failed   = failed
          reported = reported
      ).

      cl_abap_unit_assert=>assert_initial( failed ).
      cl_abap_unit_assert=>assert_initial( reported ).

      cl_abap_unit_assert=>assert_equals( exp = exp_travels_action act = result ).

  ENDMETHOD.

ENDCLASS.
