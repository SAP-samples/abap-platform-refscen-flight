"! @testing BDEF:/DMO/I_Travel_M
CLASS ltcl_booking DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS:
      travel_id1  TYPE /dmo/travel_id VALUE '11',
      travel_id2  TYPE /dmo/travel_id VALUE '12',
      travel_id3  TYPE /dmo/travel_id VALUE '13',
      travel_id4  TYPE /dmo/travel_id VALUE '14',
      booking_id1 TYPE /dmo/booking_id VALUE '21',
      booking_id2 TYPE /dmo/booking_id VALUE '22',
      booking_id3 TYPE /dmo/booking_id VALUE '23',
      booking_id4 TYPE /dmo/booking_id VALUE '24',
      c_currency  TYPE /dmo/currency_code VALUE 'EUR'.

    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_booking,
      cds_test_environment TYPE REF TO if_cds_test_environment,
      sql_test_environment TYPE REF TO if_osql_test_environment.


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
      "! Calls { @link ..lhc_booking.METH:validate_booking_status }
      "! and checks if a pair of status is valid.
      validatestatus_success        FOR TESTING RAISING cx_static_check,

      "! Calls { @link ..lhc_booking.METH:validate_booking_status }
      "! and checks if invalid permutations of sets of status
      "! returns messages.
      validatestatus_not_valid      FOR TESTING RAISING cx_static_check,

      "! Calls { @link ..lhc_booking.METH:get_instance_features }
      "! using bookings with an <em>accepted</em>, <em>rejected</em>,
      "! <em>open</em> and unknown status.
      get_instance_features        FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_booking.METH:earlynumbering_cba_booksupplem }
      "! draws a new number for instances where no number is drawn yet.
      earlynumbering_cba_new_number     FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_booking.METH:calculatetotalprice }
      "! calculates the total_price correctly by triggering an action.
      calculatetotalprice     FOR TESTING RAISING cx_static_check,

      "! Calls { @link ..lhc_booking.METH:validate_currencycode }
      "! and checks if a pair of status is valid.
      validate_currency_success        FOR TESTING,

      "! Calls { @link ..lhc_booking.METH:validate_currencycode }
      "! and checks if invalid permutations of sets of status
      "! returns messages.
      validate_currency_not_valid      FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateCustomer }
      "! and checks if an existing customer is set.
      validate_customer_success      FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateCustomer }
      "! and checks for a message for an initial customer.
      validate_customer_initial      FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateCustomer }
      "! and checks for a message for a non-existing customer.
      validate_customer_not_exist    FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateConnection }
      "! and checks if an existing Connection is set.
      validate_connection_success      FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateConnection }
      "! and checks for a message for an initial Connection.
      validate_connection_initial      FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateConnection }
      "! and checks for a message for a non-existing Connection.
      validate_connection_not_exist    FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateFlightPrice }
      "! and checks if flight price is valid.
      validate_flight_price_success    FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateFlightPrice }
      "! and checks if flight price is initial.
      validate_flight_price_initial    FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateFlightPrice }
      "! and checks for a message for an invalid flight price.
      validate_flight_price_invalid    FOR TESTING.
ENDCLASS.


CLASS ltcl_booking IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
                               VALUE #(
                                   ( i_for_entity = '/DMO/I_Travel_M'            )
                                   ( i_for_entity = '/DMO/I_Booking_M'           )
                                   ( i_for_entity = '/DMO/I_BookSuppl_M' )
                                 )
                             ).
    cds_test_environment->enable_double_redirection(  ).
    sql_test_environment = cl_osql_test_environment=>create(
                               VALUE #(
                                   ( '/DMO/CUSTOMER' )
                                   ( '/DMO/AGENCY'   )
                                   ( '/DMO/FLIGHT'   )
                                   ( 'I_CURRENCY'    )
                                 )
                               ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).

    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  currency_code = c_currency )
        ( travel_id = travel_id2  currency_code = c_currency )
        ( travel_id = travel_id3  currency_code = c_currency )
        ( travel_id = travel_id4  currency_code = c_currency )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

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


  METHOD validatestatus_success.
    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/booking_m,
      bookings_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booking~validatestatus,
      failed            TYPE RESPONSE FOR FAILED LATE  /dmo/i_travel_m,
      reported          TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    booking_mock_data = VALUE #(
        travel_id = travel_id1
        ( booking_id = booking_id1  booking_status = 'N' )
        ( booking_id = booking_id2  booking_status = 'X' )
        ( booking_id = booking_id3  booking_status = 'B' )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data ).

    class_under_test->validatestatus(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).
    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( failed   ).
  ENDMETHOD.

  METHOD validatestatus_not_valid.
    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/booking_m,
      bookings_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booking~validatestatus,
      failed            TYPE RESPONSE FOR FAILED LATE  /dmo/i_travel_m,
      reported          TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    booking_mock_data = VALUE #(
        ( travel_id = travel_id1  booking_id = booking_id1  booking_status = 'K' )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data ).

    class_under_test->validatestatus(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-booking )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = travel_id1
        act = failed-booking[ 1 ]-travel_id
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = booking_id1
        act = failed-booking[ 1 ]-booking_id
      ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( reported-booking )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = travel_id1
        act = reported-booking[ 1 ]-travel_id
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = booking_id1
        act = reported-booking[ 1 ]-booking_id
      ).

    TYPES: reported_booking LIKE LINE OF reported-booking.
    DATA: exp_reported_element TYPE reported_booking-%element.
    exp_reported_element-booking_status = if_abap_behv=>mk-on.
    cl_abap_unit_assert=>assert_equals(
        exp = exp_reported_element
        act = reported-booking[ 1 ]-%element
      ).
  ENDMETHOD.

  METHOD get_instance_features.
    TYPES: t_instance_feature TYPE STRUCTURE FOR INSTANCE FEATURES RESULT /dmo/i_travel_m\\booking,
           BEGIN OF t_check.
             INCLUDE TYPE t_instance_feature.
    TYPES: booking_status TYPE /dmo/i_booking_m-booking_status,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY travel_id.

    DATA:
      check_table        TYPE t_check_table,
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/booking_m,
      travels_to_test    TYPE TABLE FOR INSTANCE FEATURES KEY /dmo/i_travel_m\\booking,
      requested_features TYPE STRUCTURE FOR INSTANCE FEATURES REQUEST /dmo/i_travel_m\\booking,
      act_result         TYPE TABLE FOR INSTANCE FEATURES RESULT /dmo/i_travel_m\\booking,
      exp_result         TYPE TABLE FOR INSTANCE FEATURES RESULT /dmo/i_travel_m\\booking,
      reported           TYPE RESPONSE FOR REPORTED EARLY /dmo/i_travel_m,
      failed             TYPE RESPONSE FOR FAILED   EARLY /dmo/i_travel_m.

    " In current implementation requested_features is not used.
    requested_features = VALUE #( ).

    check_table = VALUE t_check_table(
        (
          travel_id              = travel_id1
          booking_id             = booking_id1
          booking_status         = 'B'
          %assoc-_booksupplement = if_abap_behv=>fc-o-disabled
        )
        (
          travel_id              = travel_id1
          booking_id             = booking_id2
          booking_status         = 'X'
          %assoc-_booksupplement = if_abap_behv=>fc-o-enabled
        )
        (
          travel_id              = travel_id1
          booking_id             = booking_id3
          booking_status         = 'N'
          %assoc-_booksupplement = if_abap_behv=>fc-o-enabled
        )
        (
          travel_id              = travel_id1
          booking_id             = booking_id4
          booking_status         = 'T'
          %assoc-_booksupplement = if_abap_behv=>fc-o-enabled
        )
      ).

    booking_mock_data = CORRESPONDING #(
                          check_table
                          MAPPING
                            travel_id      = travel_id
                            booking_id     = booking_id
                            booking_status = booking_status
                          EXCEPT *
                        ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    travels_to_test = CORRESPONDING #( check_table ).

    exp_result = CORRESPONDING #( check_table ).

    class_under_test->get_features(
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

    SORT act_result BY travel_id ASCENDING.
    SORT exp_result BY travel_id ASCENDING.

    cl_abap_unit_assert=>assert_equals(
        exp = exp_result
        act = act_result
      ).
  ENDMETHOD.

  METHOD earlynumbering_cba_new_number.
    DATA:
      booking_mock_data            TYPE STANDARD TABLE OF /dmo/booking_m,
      booking_supplement_mock_data TYPE STANDARD TABLE OF /dmo/booksuppl_m,
      exp_travel_read              TYPE TABLE FOR READ RESULT /dmo/i_travel_m\\booking,
      mapped                       TYPE RESPONSE FOR MAPPED EARLY  /dmo/i_travel_m,
      failed                       TYPE RESPONSE FOR FAILED EARLY  /dmo/i_travel_m,
      reported                     TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m,
      entities                     TYPE TABLE FOR CREATE /dmo/i_travel_m\\booking\_booksupplement.

    booking_mock_data = VALUE #(
        travel_id = travel_id1
        ( booking_id = booking_id1 ) "no existing booking_supplement_supplements and no number provided
        ( booking_id = booking_id2 ) "no existing booking_supplement_supplements but some numbers provided
        ( booking_id = booking_id3 ) "existing booking_supplement_supplements
        ( booking_id = booking_id4 ) "existing booking_supplement_supplements and some numbers provided
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    booking_supplement_mock_data = VALUE #(
        travel_id = travel_id1
        ( booking_id   = booking_id3  booking_supplement_id = '1' )
        ( booking_id   = booking_id4  booking_supplement_id = '2' )
      ).
    cds_test_environment->insert_test_data( booking_supplement_mock_data ).

    entities = VALUE #(
        travel_id = travel_id1
        (
          booking_id = booking_id1  "no existing booking_supplements and no number provided
          %target   = VALUE #(
              ( %cid = '1b1' )
            )
        )
        (
          booking_id = booking_id2  "no existing booking_supplements but some numbers provided
          %target   = VALUE #(
              ( %cid = '2b1' )
              ( %cid = '2b2'  booking_supplement_id = '1' )
            )
        )
        (
          booking_id = booking_id3  "existing booking_supplements
          %target   = VALUE #(
              ( %cid = '3b1' )
            )
        )
        (
          booking_id = booking_id4  "existing booking_supplements and some numbers provided
          %target   = VALUE #(
              ( %cid = '4b1' )
              ( %cid = '4b2'  booking_supplement_id = '1' )
              ( %cid = '4b3'  booking_supplement_id = '3' )
            )
        )
      ).

    class_under_test->earlynumbering_cba_booksupplem(
        EXPORTING
          entities = entities
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( mapped-travel ).
    cl_abap_unit_assert=>assert_initial( mapped-booking ).
    cl_abap_unit_assert=>assert_not_initial( mapped-booksuppl ).

    cl_abap_unit_assert=>assert_equals(
        exp = REDUCE #( INIT i = 0
                        FOR booking IN entities
                        NEXT i += lines( booking-%target ) )
        act = lines( mapped-booksuppl )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/booking_supplement_id( '1' )
        act = VALUE /dmo/booking_supplement_id( mapped-booksuppl[ %cid = '1b1' ]-booking_supplement_id )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/booking_supplement_id( '2' )
        act = VALUE /dmo/booking_supplement_id( mapped-booksuppl[ %cid = '2b1' ]-booking_supplement_id )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/booking_supplement_id( '2' )
        act = VALUE /dmo/booking_supplement_id( mapped-booksuppl[ %cid = '3b1' ]-booking_supplement_id )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/booking_supplement_id( '4' )
        act = VALUE /dmo/booking_supplement_id( mapped-booksuppl[ %cid = '4b1' ]-booking_supplement_id )
      ).
  ENDMETHOD.

  METHOD calculatetotalprice.
    CONSTANTS:
      c_flight_price TYPE /dmo/flight_price VALUE '10'.

    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/booking_m,
      reported          TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    booking_mock_data = VALUE #(
        flight_price  = c_flight_price
        currency_code = c_currency
        travel_id     = travel_id1
        ( booking_id = booking_id1 )
        ( booking_id = booking_id2 )
        ( booking_id = booking_id3 )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).


    class_under_test->calculatetotalprice(
       EXPORTING
         keys     = CORRESPONDING #( booking_mock_data )
       CHANGING
         reported = reported
     ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /dmo/i_travel_m
      ENTITY travel
        FIELDS ( total_price ) WITH VALUE #( ( travel_id = travel_id1 ) )
        RESULT DATA(read_result).

    cl_abap_unit_assert=>assert_not_initial( read_result ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/total_price( c_flight_price * lines( booking_mock_data ) )
        act = read_result[ 1 ]-total_price
      ).
  ENDMETHOD.

  METHOD validate_currency_success.
    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/booking_m,
      bookings_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booking~validatecurrencycode,
      failed            TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported          TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    booking_mock_data = VALUE #(
        travel_id = travel_id1
        ( booking_id = booking_id1  currency_code = 'EUR' )
        ( booking_id = booking_id2  currency_code = 'USD' )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING travel_id = travel_id  booking_id = booking_id ).

    class_under_test->validate_currencycode(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD validate_currency_not_valid.
    TYPES: BEGIN OF t_check.
             INCLUDE TYPE /dmo/booking_m.
    TYPES:   exp_amount_reported_entries TYPE i,
             exp_amount_failed_entries   TYPE i,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY travel_id.

    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/booking_m,
      bookings_to_check TYPE t_check_table,
      booking_to_check  TYPE t_check,
      bookings_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booking~validatecurrencycode,
      failed            TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported          TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    bookings_to_check = VALUE #(
        exp_amount_reported_entries = '1'
        exp_amount_failed_entries   = '1'
        travel_id                   = travel_id1
        ( booking_id = booking_id1  currency_code = ''    )
        ( booking_id = booking_id2  currency_code = 'XXX' )
      ).
    booking_mock_data = CORRESPONDING #( bookings_to_check ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING travel_id = travel_id  booking_id = booking_id ).

    class_under_test->validate_currencycode(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR booking IN bookings_to_check
                            NEXT sum += booking-exp_amount_failed_entries )
           act  = lines( failed-booking )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT bookings_to_check INTO booking_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = booking_to_check-exp_amount_failed_entries
            act  = lines( FILTER #( failed-booking USING KEY entity WHERE travel_id = booking_to_check-travel_id AND booking_id = booking_to_check-booking_id ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ booking_to_check-exp_amount_failed_entries } line(s) in failed expected for '{ booking_to_check-booking_id }'|
          ).
      ENDLOOP.
    ENDIF.

    cl_abap_unit_assert=>assert_not_initial( reported ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR booking IN bookings_to_check
                            NEXT sum += booking-exp_amount_reported_entries )
           act  = lines( reported-booking )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT bookings_to_check INTO booking_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = booking_to_check-exp_amount_reported_entries
            act  = lines( FILTER #( reported-booking USING KEY entity WHERE travel_id = booking_to_check-travel_id AND booking_id = booking_to_check-booking_id ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ booking_to_check-exp_amount_reported_entries } line(s) in reported expected for '{ booking_to_check-booking_id }'|
          ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD validate_customer_success.

    CONSTANTS:
      c_customer_id TYPE /dmo/customer_id VALUE '123'.

    DATA:
      customer_mock_data TYPE STANDARD TABLE OF /dmo/customer,
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/booking_m,
      bookings_to_test   TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booking~validateCustomer,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    booking_mock_data = VALUE #(
        travel_id = travel_id1
        ( booking_id = booking_id1  customer_id = c_customer_id )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING travel_id = travel_id  booking_id = booking_id ).

    class_under_test->validate_customer(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).

  ENDMETHOD.

  METHOD validate_customer_initial.

    CONSTANTS:
      c_customer_id            TYPE /dmo/customer_id VALUE '123',
      c_customer_id_of_booking TYPE /dmo/customer_id VALUE IS INITIAL.

    DATA:
      customer_mock_data TYPE STANDARD TABLE OF /dmo/customer,
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/booking_m,
      bookings_to_test   TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booking~validateCustomer,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    booking_mock_data = VALUE #(
        travel_id = travel_id1
        ( booking_id = booking_id1  customer_id = c_customer_id_of_booking )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING travel_id = travel_id  booking_id = booking_id ).

    class_under_test->validate_customer(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-booking )
      ).
    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( reported-booking )
      ).
    cl_abap_unit_assert=>assert_bound( reported-booking[ 1 ]-%msg ).
    cl_abap_unit_assert=>assert_equals(
        exp = '/DMO/CM_FLIGHT'
        act = reported-booking[ 1 ]-%msg->if_t100_message~t100key-msgid
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = '010'
        act = reported-booking[ 1 ]-%msg->if_t100_message~t100key-msgno
      ).

  ENDMETHOD.

  METHOD validate_customer_not_exist.

    CONSTANTS:
      c_customer_id            TYPE /dmo/customer_id VALUE '123',
      c_customer_id_of_booking TYPE /dmo/customer_id VALUE '111'.

    DATA:
      customer_mock_data TYPE STANDARD TABLE OF /dmo/customer,
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/booking_m,
      bookings_to_test   TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booking~validateCustomer,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    booking_mock_data = VALUE #(
        travel_id = travel_id1
        ( booking_id = booking_id1  customer_id = c_customer_id_of_booking )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING travel_id = travel_id  booking_id = booking_id ).

    class_under_test->validate_customer(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-booking )
      ).
    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( reported-booking )
      ).
    cl_abap_unit_assert=>assert_bound( reported-booking[ 1 ]-%msg ).
    cl_abap_unit_assert=>assert_equals(
        exp = '/DMO/CM_FLIGHT'
        act = reported-booking[ 1 ]-%msg->if_t100_message~t100key-msgid
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = '001'
        act = reported-booking[ 1 ]-%msg->if_t100_message~t100key-msgno
      ).
  ENDMETHOD.

  METHOD validate_connection_success.

    CONSTANTS:
      c_airline     TYPE /dmo/carrier_id VALUE 'TS',
      c_connection  TYPE /dmo/connection_id VALUE '123',
      c_flight_date TYPE /dmo/flight_date VALUE '20200202'.

    DATA:
      flight_mock_data  TYPE STANDARD TABLE OF /dmo/flight,
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/booking_m,
      bookings_to_test   TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booking~validateConnection,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

     flight_mock_data = VALUE #( ( carrier_id = c_airline  connection_id = c_connection  flight_date = c_flight_date ) ).
     sql_test_environment->insert_test_data( flight_mock_data ).

    booking_mock_data = VALUE #(
        travel_id = travel_id1
        (
          booking_id = booking_id1
          carrier_id    = c_airline
          connection_id = c_connection
          flight_date   = c_flight_date
        )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING travel_id = travel_id  booking_id = booking_id ).

    class_under_test->validate_connection(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).

  ENDMETHOD.


  METHOD validate_connection_initial.

    CONSTANTS:
      c_airline     TYPE /dmo/carrier_id VALUE 'TS',
      c_connection  TYPE /dmo/connection_id VALUE '123',
      c_flight_date TYPE /dmo/flight_date VALUE '20200202'.

    DATA:
      flight_mock_data  TYPE STANDARD TABLE OF /dmo/flight,
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/booking_m,
      bookings_to_test   TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booking~validateConnection,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

     flight_mock_data = VALUE #( ( carrier_id = c_airline  connection_id = c_connection  flight_date = c_flight_date ) ).
     sql_test_environment->insert_test_data( flight_mock_data ).

    booking_mock_data = VALUE #(
        travel_id = travel_id1
        ( booking_id = booking_id1 )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING travel_id = travel_id  booking_id = booking_id ).

    class_under_test->validate_connection(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 3 " AirlineID + ConnectionID + FlightDate
        act = lines( failed-booking )
      ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 3 " AirlineID + ConnectionID + FlightDate
        act = lines( reported-booking )
      ).
    cl_abap_unit_assert=>assert_bound( reported-booking[ 1 ]-%msg ).
    cl_abap_unit_assert=>assert_equals(
        exp = '/DMO/CM_FLIGHT'
        act = reported-booking[ 1 ]-%msg->if_t100_message~t100key-msgid
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = '016'
        act = reported-booking[ 1 ]-%msg->if_t100_message~t100key-msgno
      ).
    cl_abap_unit_assert=>assert_bound( reported-booking[ 2 ]-%msg ).
    cl_abap_unit_assert=>assert_equals(
        exp = '/DMO/CM_FLIGHT'
        act = reported-booking[ 2 ]-%msg->if_t100_message~t100key-msgid
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = '011'
        act = reported-booking[ 2 ]-%msg->if_t100_message~t100key-msgno
      ).
    cl_abap_unit_assert=>assert_bound( reported-booking[ 3 ]-%msg ).
    cl_abap_unit_assert=>assert_equals(
        exp = '/DMO/CM_FLIGHT'
        act = reported-booking[ 3 ]-%msg->if_t100_message~t100key-msgid
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = '017'
        act = reported-booking[ 3 ]-%msg->if_t100_message~t100key-msgno
      ).
  ENDMETHOD.


  METHOD validate_connection_not_exist.

    CONSTANTS:
      c_airline              TYPE /dmo/carrier_id    VALUE 'TS',
      c_connection           TYPE /dmo/connection_id VALUE '123',
      c_connection_not_exist TYPE /dmo/connection_id VALUE '321',
      c_flight_date          TYPE /dmo/flight_date   VALUE '20200202'.

    DATA:
      flight_mock_data  TYPE STANDARD TABLE OF /dmo/flight,
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/booking_m,
      bookings_to_test   TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booking~validateConnection,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

     flight_mock_data = VALUE #( ( carrier_id = c_airline  connection_id = c_connection  flight_date = c_flight_date ) ).
     sql_test_environment->insert_test_data( flight_mock_data ).

    booking_mock_data = VALUE #(
        travel_id = travel_id1
        (
          booking_id = booking_id1
          carrier_id    = c_airline
          connection_id = c_connection_not_exist
          flight_date   = c_flight_date
        )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING travel_id = travel_id  booking_id = booking_id ).

    class_under_test->validate_connection(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-booking )
      ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( reported-booking )
      ).
    cl_abap_unit_assert=>assert_bound( reported-booking[ 1 ]-%msg ).
    cl_abap_unit_assert=>assert_equals(
        exp = '/DMO/CM_FLIGHT'
        act = reported-booking[ 1 ]-%msg->if_t100_message~t100key-msgid
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = '012'
        act = reported-booking[ 1 ]-%msg->if_t100_message~t100key-msgno
      ).
  ENDMETHOD.

  METHOD validate_flight_price_success.

    DATA:
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/booking_m,
      bookings_to_test   TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booking~validateFlightPrice,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    booking_mock_data = VALUE #(
        travel_id = travel_id1
        (
          booking_id   = booking_id1
          flight_price = '123'
        )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING travel_id = travel_id  booking_id = booking_id ).

    class_under_test->validate_flight_price(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).

  ENDMETHOD.

  METHOD validate_flight_price_initial.

    DATA:
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/booking_m,
      bookings_to_test   TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booking~validateFlightPrice,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    booking_mock_data = VALUE #(
        travel_id = travel_id1
        (
          booking_id   = booking_id1
          flight_price = ''
        )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING travel_id = travel_id  booking_id = booking_id ).

    class_under_test->validate_flight_price(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).

  ENDMETHOD.

  METHOD validate_flight_price_invalid.

    DATA:
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/booking_m,
      bookings_to_test   TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booking~validateFlightPrice,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    booking_mock_data = VALUE #(
        travel_id = travel_id1
        (
          booking_id   = booking_id1
          flight_price = '-123'
        )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING travel_id = travel_id  booking_id = booking_id ).

    class_under_test->validate_flight_price(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-booking )
      ).
    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( reported-booking )
      ).

    cl_abap_unit_assert=>assert_bound( reported-booking[ 1 ]-%msg ).
    cl_abap_unit_assert=>assert_equals(
        exp = '/DMO/CM_FLIGHT'
        act = reported-booking[ 1 ]-%msg->if_t100_message~t100key-msgid
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = '025'
        act = reported-booking[ 1 ]-%msg->if_t100_message~t100key-msgno
      ).

  ENDMETHOD.

ENDCLASS.
