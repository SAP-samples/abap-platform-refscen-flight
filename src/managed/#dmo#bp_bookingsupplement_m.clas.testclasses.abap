"! @testing BDEF:/DMO/I_Travel_M
CLASS ltcl_bookingsupplement DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS:
      travel_id1             TYPE /dmo/travel_id             VALUE '11',
      travel_id2             TYPE /dmo/travel_id             VALUE '12',
      travel_id3             TYPE /dmo/travel_id             VALUE '13',
      travel_id4             TYPE /dmo/travel_id             VALUE '14',
      booking_id1            TYPE /dmo/booking_id            VALUE '21',
      booking_id2            TYPE /dmo/booking_id            VALUE '22',
      booking_id3            TYPE /dmo/booking_id            VALUE '23',
      booking_id4            TYPE /dmo/booking_id            VALUE '24',
      booking_supplement_id1 TYPE /dmo/booking_supplement_id VALUE '31',
      booking_supplement_id2 TYPE /dmo/booking_supplement_id VALUE '32',
      booking_supplement_id3 TYPE /dmo/booking_supplement_id VALUE '33',
      booking_supplement_id4 TYPE /dmo/booking_supplement_id VALUE '34',
      c_currency             TYPE /dmo/currency_code         VALUE 'EUR'.

    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_bookingsupplement,
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
      "! Checks if { @link ..lhc_booking_supplement.METH:calculatetotalprice }
      "! calculates the total_price correctly by triggering an action.
      calculatetotalprice     FOR TESTING RAISING cx_static_check,

      "! Calls { @link ..lhc_bookingsupplement.METH:validate_currencycode }
      "! and checks if a pair of status is valid.
      validate_currency_success        FOR TESTING,

      "! Calls { @link ..lhc_bookingsupplement.METH:validate_currencycode }
      "! and checks if invalid permutations of sets of status
      "! returns messages.
      validate_currency_not_valid      FOR TESTING.
ENDCLASS.


CLASS ltcl_bookingsupplement IMPLEMENTATION.

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
                                   ( 'I_CURRENCY'    )
                                 )
                               ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).

    DATA:
      travel_mock_data  TYPE STANDARD TABLE OF /dmo/travel_m,
      booking_mock_data TYPE STANDARD TABLE OF /dmo/booking_m.

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  currency_code = c_currency )
        ( travel_id = travel_id2  currency_code = c_currency )
        ( travel_id = travel_id3  currency_code = c_currency )
        ( travel_id = travel_id4  currency_code = c_currency )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    booking_mock_data = VALUE #(
        ( travel_id = travel_id1  booking_id = booking_id1  currency_code = c_currency )
        ( travel_id = travel_id1  booking_id = booking_id2  currency_code = c_currency )
        ( travel_id = travel_id1  booking_id = booking_id3  currency_code = c_currency )
        ( travel_id = travel_id1  booking_id = booking_id4  currency_code = c_currency )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

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

  METHOD calculatetotalprice.
    CONSTANTS:
      c_supplement_price TYPE /dmo/supplement_price VALUE '10'.

    DATA:
      booking_supplement_mock_data TYPE STANDARD TABLE OF /dmo/booksuppl_m,
      reported                     TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    booking_supplement_mock_data = VALUE #(
        price         = c_supplement_price
        currency_code = c_currency
        travel_id     = travel_id1
        booking_id    = booking_id1
        ( booking_supplement_id = booking_supplement_id1 )
        ( booking_supplement_id = booking_supplement_id2 )
        ( booking_supplement_id = booking_supplement_id3 )
      ).
    cds_test_environment->insert_test_data( booking_supplement_mock_data ).


    class_under_test->calculatetotalprice(
       EXPORTING
         keys     = CORRESPONDING #( booking_supplement_mock_data )
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
        exp = CONV /dmo/total_price( c_supplement_price * lines( booking_supplement_mock_data ) )
        act = read_result[ 1 ]-total_price
      ).
  ENDMETHOD.

  METHOD validate_currency_success.
    DATA:
      booking_supplement_mock_data TYPE STANDARD TABLE OF /dmo/booksuppl_m,
      booking_supplements_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booksuppl~validatecurrencycode,
      failed                       TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported                     TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    booking_supplement_mock_data = VALUE #(
        travel_id  = travel_id1
        booking_id = booking_id1
        ( booking_supplement_id = booking_supplement_id1  currency_code = 'EUR' )
        ( booking_supplement_id = booking_supplement_id2  currency_code = 'USD' )
      ).
    cds_test_environment->insert_test_data( booking_supplement_mock_data ).

    booking_supplements_to_test = CORRESPONDING #( booking_supplement_mock_data MAPPING travel_id = travel_id  booking_id = booking_id ).

    class_under_test->validate_currencycode(
        EXPORTING
          keys     = booking_supplements_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD validate_currency_not_valid.
    TYPES: BEGIN OF t_check.
             INCLUDE TYPE /dmo/booksuppl_m.
    TYPES:   exp_amount_reported_entries TYPE i,
             exp_amount_failed_entries   TYPE i,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY travel_id.

    DATA:
      booking_supplement_mock_data TYPE STANDARD TABLE OF /dmo/booksuppl_m,
      booking_supplements_to_check TYPE t_check_table,
      booking_supplement_to_check  TYPE t_check,
      booking_supplements_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\booksuppl~validatecurrencycode,
      failed                       TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported                     TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    booking_supplements_to_check = VALUE #(
        exp_amount_reported_entries = '1'
        exp_amount_failed_entries   = '1'
        travel_id                   = travel_id1
        booking_id                  = booking_id1
        ( booking_supplement_id = booking_supplement_id1  currency_code = ''    )
        ( booking_supplement_id = booking_supplement_id2  currency_code = 'XXX' )
      ).
    booking_supplement_mock_data = CORRESPONDING #( booking_supplements_to_check ).
    cds_test_environment->insert_test_data( booking_supplement_mock_data ).

    booking_supplements_to_test = CORRESPONDING #( booking_supplement_mock_data MAPPING travel_id = travel_id ).

    class_under_test->validate_currencycode(
        EXPORTING
          keys     = booking_supplements_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR booking_supplement IN booking_supplements_to_check
                            NEXT sum += booking_supplement-exp_amount_failed_entries )
           act  = lines( failed-booksuppl )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT booking_supplements_to_check INTO booking_supplement_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = booking_supplement_to_check-exp_amount_failed_entries
            act  = lines( FILTER #( failed-booksuppl USING KEY entity
                                      WHERE travel_id             = booking_supplement_to_check-travel_id
                                        AND booking_id            = booking_supplement_to_check-booking_id
                                        AND booking_supplement_id = booking_supplement_to_check-booking_supplement_id ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ booking_supplement_to_check-exp_amount_failed_entries } line(s) in failed expected for '{ booking_supplement_to_check-booking_supplement_id }'|
          ).
      ENDLOOP.
    ENDIF.

    cl_abap_unit_assert=>assert_not_initial( reported ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR booking_supplement IN booking_supplements_to_check
                            NEXT sum += booking_supplement-exp_amount_reported_entries )
           act  = lines( reported-booksuppl )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT booking_supplements_to_check INTO booking_supplement_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = booking_supplement_to_check-exp_amount_reported_entries
            act  = lines( FILTER #( reported-booksuppl USING KEY entity
                                      WHERE travel_id             = booking_supplement_to_check-travel_id
                                        AND booking_id            = booking_supplement_to_check-booking_id
                                        AND booking_supplement_id = booking_supplement_to_check-booking_supplement_id ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ booking_supplement_to_check-exp_amount_reported_entries } line(s) in reported expected for '{ booking_supplement_to_check-booking_supplement_id }'|
          ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
