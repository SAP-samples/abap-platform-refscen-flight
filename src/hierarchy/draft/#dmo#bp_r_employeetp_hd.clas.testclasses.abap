CLASS ltc_abstract DEFINITION ABSTRACT FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PROTECTED SECTION.

    TYPES:
      tt_agency       TYPE STANDARD TABLE OF /dmo/agency_a_hd WITH KEY agency,
      tt_employee     TYPE STANDARD TABLE OF /dmo/employ_a_hd WITH KEY agency employee,
      tt_employee_key TYPE TABLE FOR KEY OF /dmo/r_agencytp_hd\\employee.

    CONSTANTS:
      agency1   TYPE /dmo/agency_id VALUE '10',
      agency2   TYPE /dmo/agency_id VALUE '20',
      agency3   TYPE /dmo/agency_id VALUE '30',
      agency4   TYPE /dmo/agency_id VALUE '40',
      employee1 TYPE /dmo/employee_id VALUE '1',
      employee2 TYPE /dmo/employee_id VALUE '2',
      employee3 TYPE /dmo/employee_id VALUE '3',
      employee4 TYPE /dmo/employee_id VALUE '4',
      employee5 TYPE /dmo/employee_id VALUE '5',
      employee6 TYPE /dmo/employee_id VALUE '6',
      employee7 TYPE /dmo/employee_id VALUE '7',
      employee8 TYPE /dmo/employee_id VALUE '8'.

    CLASS-DATA:
      cds_test_environment TYPE REF TO if_cds_test_environment,
      sql_test_environment TYPE REF TO if_osql_test_environment.


    DATA:
      agencies  TYPE tt_agency,
      employees TYPE tt_employee.


  PRIVATE SECTION.

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


ENDCLASS.


CLASS ltc_abstract IMPLEMENTATION.

  METHOD class_setup.
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
        VALUE #(
            ( i_for_entity = '/DMO/R_AGENCYTP_HD'    i_select_base_dependencies = abap_true  )
            ( i_for_entity = '/DMO/R_EMPLOYEETP_HD'  i_select_base_dependencies = abap_true  )
          )
      ).
    cds_test_environment->enable_double_redirection( ).

    sql_test_environment = cl_osql_test_environment=>create(
        VALUE #(
            ( 'I_CURRENCY' )
          )
      ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).

    agencies = VALUE tt_agency(
        ( agency = agency1  name = 'Test1' )
        ( agency = agency2  name = 'Test2' )
        ( agency = agency3  name = 'Test3' )
        ( agency = agency4  name = 'Test4' )
      ).
    cds_test_environment->insert_test_data( agencies ).

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

ENDCLASS.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""



"! @testing BDEF:/DMO/R_AgencyTP_HD
CLASS ltc_general DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM ltc_abstract.
  PRIVATE SECTION.

    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_general.

    CLASS-METHODS:
      class_setup.

    METHODS:
      setup.

    METHODS:
*      "! Checks that { @link ..lhc_employee.METH:get_global_authorizations } returns initial values
*      "! for <em>result</em> and <em>reported</em>.
*      get_global_authorizations      FOR TESTING RAISING cx_static_check,

      "! Calls { @link ..lhc_booking.METH:validationcurrencyexists }
      "! and checks if valid currencies are accepted.
      validate_currency_success        FOR TESTING RAISING cx_static_check,

      "! Calls { @link ..lhc_booking.METH:validationcurrencyexists }
      "! and checks if invalid and initial currencies get rejected.
      validate_currency_not_valid      FOR TESTING RAISING cx_static_check,

      "! Calls { @link ..lhc_booking.METH:validationnamegiven }
      "! and checks if at least one name is given.
      validation_name_given_success        FOR TESTING RAISING cx_static_check,

      "! Calls { @link ..lhc_booking.METH:validationnamegiven }
      "! and checks if initial name pairs get rejected.
      validation_name_given_inital      FOR TESTING RAISING cx_static_check,

      "! Calls { @link ..lhc_booking.METH:validationsalarypositive }
      "! and checks if a positive salary is accepted.
      validation_salary_pos_success     FOR TESTING RAISING cx_static_check,

      "! Calls { @link ..lhc_booking.METH:validationsalarypositive }
      "! and checks if a negaive salary is rejected.
      validation_salary_pos_fail      FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_general IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
  ENDMETHOD.

  METHOD setup.
    employees = VALUE tt_employee(
        agency = agency1
        ( employee = employee1  last_name = 'Test1-1'  sibling_order_number = '1' )
        ( employee = employee2  last_name = 'Test1-2'  sibling_order_number = '2' )
        ( employee = employee3  last_name = 'Test1-3'  sibling_order_number = '100'     )
        ( employee = employee4  last_name = 'Test1-4'  sibling_order_number = ''        )
        agency = agency2
        ( employee = employee1  last_name = 'Test2-1'  sibling_order_number = '1' )
        ( employee = employee2  last_name = 'Test2-2'  sibling_order_number = '2' )
        ( employee = employee3  last_name = 'Test2-3'  sibling_order_number = ''  )
        ( employee = employee4  last_name = 'Test2-4'  sibling_order_number = ''  )
      ).
    cds_test_environment->insert_test_data( employees ).
  ENDMETHOD.

*  METHOD get_global_authorizations.
*    DATA:
*      requested_authorizations TYPE STRUCTURE FOR GLOBAL AUTHORIZATION REQUEST /dmo/r_agencytp_hd\\employee,
*      result                   TYPE STRUCTURE FOR GLOBAL AUTHORIZATION RESULT /dmo/r_agencytp_hd\\employee,
*      reported                 TYPE RESPONSE FOR REPORTED EARLY /dmo/r_agencytp_hd.
*
*    requested_authorizations-%action-changenextsibling = if_abap_behv=>mk-on.
*
*    class_under_test->get_global_authorizations(
*        EXPORTING
*          requested_authorizations = requested_authorizations
*        CHANGING
*          result                   = result
*          reported                 = reported
*      ).
*
*    cl_abap_unit_assert=>assert_initial( result   ).
*    cl_abap_unit_assert=>assert_initial( reported ).
*  ENDMETHOD.

  METHOD validate_currency_success.
    DATA:
      employee_mock_data TYPE STANDARD TABLE OF /dmo/employ_a_hd,
      employees_to_test  TYPE TABLE FOR VALIDATION /dmo/r_agencytp_hd\\employee~validationcurrencyexists,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/r_agencytp_hd,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/r_agencytp_hd.

    employee_mock_data = VALUE #(
        agency = agency3
        ( employee = employee1  salary_currency = 'EUR' )
        ( employee = employee2  salary_currency = 'USD' )
      ).
    cds_test_environment->insert_test_data( employee_mock_data ).

    employees_to_test = CORRESPONDING #( employee_mock_data MAPPING agency = agency  employee = employee ).

    class_under_test->validationcurrencyexists(
        EXPORTING
          keys     = employees_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = lines( employees_to_test )
        act = lines( reported-employee )
      ).
  ENDMETHOD.

  METHOD validate_currency_not_valid.
    TYPES: BEGIN OF t_check.
             INCLUDE TYPE /dmo/employ_a_hd.
    TYPES:   exp_amount_reported_entries TYPE i,
             exp_amount_failed_entries   TYPE i,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY agency employee.

    DATA:
      employee_mock_data TYPE STANDARD TABLE OF /dmo/employ_a_hd,
      employees_to_check TYPE t_check_table,
      employee_to_check  TYPE t_check,
      employees_to_test  TYPE TABLE FOR VALIDATION /dmo/r_agencytp_hd\\employee~validationcurrencyexists,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/r_agencytp_hd,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/r_agencytp_hd.

    employees_to_check = VALUE #(
        exp_amount_reported_entries = '2'
        exp_amount_failed_entries   = '1'
        ( employee = employee1  salary_currency = ''    )
        ( employee = employee2  salary_currency = 'XXX' )
      ).
    employee_mock_data = CORRESPONDING #( employees_to_check ).
    cds_test_environment->insert_test_data( employee_mock_data ).

    employees_to_test = CORRESPONDING #( employee_mock_data MAPPING employee = employee ).

    class_under_test->validationcurrencyexists(
        EXPORTING
          keys     = employees_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR employee IN employees_to_check
                            NEXT sum += employee-exp_amount_failed_entries )
           act  = lines( failed-employee )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT employees_to_check INTO employee_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = employee_to_check-exp_amount_failed_entries
            act  = lines( FILTER #( failed-employee USING KEY entity WHERE agency = employee_to_check-agency AND employee = employee_to_check-employee ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ employee_to_check-exp_amount_failed_entries } line(s) in failed expected for '{ employee_to_check-employee }'|
          ).
      ENDLOOP.
    ENDIF.

    cl_abap_unit_assert=>assert_not_initial( reported ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR employee IN employees_to_check
                            NEXT sum += employee-exp_amount_reported_entries )
           act  = lines( reported-employee )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT employees_to_check INTO employee_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = employee_to_check-exp_amount_reported_entries
            act  = lines( FILTER #( reported-employee USING KEY entity WHERE agency = employee_to_check-agency AND employee = employee_to_check-employee ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ employee_to_check-exp_amount_reported_entries } line(s) in reported expected for '{ employee_to_check-employee }'|
          ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD validation_name_given_success.
    DATA:
      employee_mock_data TYPE STANDARD TABLE OF /dmo/employ_a_hd,
      employees_to_test  TYPE TABLE FOR VALIDATION /dmo/r_agencytp_hd\\employee~validationnamegiven,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/r_agencytp_hd,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/r_agencytp_hd.

    employee_mock_data = VALUE #(
        agency = agency3
        ( employee = employee1  first_name = 'Test'  last_name = 'Testinger' )
        ( employee = employee2  first_name = 'Test'  last_name = ''          )
        ( employee = employee3  first_name = ''      last_name = 'Testinger' )
      ).
    cds_test_environment->insert_test_data( employee_mock_data ).

    employees_to_test = CORRESPONDING #( employee_mock_data MAPPING agency = agency  employee = employee ).

    class_under_test->validationnamegiven(
        EXPORTING
          keys     = employees_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = lines( employees_to_test )
        act = lines( reported-employee )
      ).
  ENDMETHOD.

  METHOD validation_name_given_inital.
    TYPES: BEGIN OF t_check.
             INCLUDE TYPE /dmo/employ_a_hd.
    TYPES:   exp_amount_reported_entries TYPE i,
             exp_amount_failed_entries   TYPE i,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY agency employee.

    DATA:
      employee_mock_data TYPE STANDARD TABLE OF /dmo/employ_a_hd,
      employees_to_check TYPE t_check_table,
      employee_to_check  TYPE t_check,
      employees_to_test  TYPE TABLE FOR VALIDATION /dmo/r_agencytp_hd\\employee~validationnamegiven,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/r_agencytp_hd,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/r_agencytp_hd.


    employees_to_check = VALUE #(
        exp_amount_reported_entries = '2'
        exp_amount_failed_entries   = '1'
        agency = agency3
        ( employee = employee3  first_name = ''  last_name = '' )
      ).
    employee_mock_data = CORRESPONDING #( employees_to_check ).
    cds_test_environment->insert_test_data( employee_mock_data ).

    employees_to_test = CORRESPONDING #( employee_mock_data MAPPING employee = employee ).

    class_under_test->validationnamegiven(
        EXPORTING
          keys     = employees_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR employee IN employees_to_check
                            NEXT sum += employee-exp_amount_failed_entries )
           act  = lines( failed-employee )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT employees_to_check INTO employee_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = employee_to_check-exp_amount_failed_entries
            act  = lines( FILTER #( failed-employee USING KEY entity WHERE agency = employee_to_check-agency AND employee = employee_to_check-employee ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ employee_to_check-exp_amount_failed_entries } line(s) in failed expected for '{ employee_to_check-employee }'|
          ).
      ENDLOOP.
    ENDIF.

    cl_abap_unit_assert=>assert_not_initial( reported ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR employee IN employees_to_check
                            NEXT sum += employee-exp_amount_reported_entries )
           act  = lines( reported-employee )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT employees_to_check INTO employee_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = employee_to_check-exp_amount_reported_entries
            act  = lines( FILTER #( reported-employee USING KEY entity WHERE agency = employee_to_check-agency AND employee = employee_to_check-employee ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ employee_to_check-exp_amount_reported_entries } line(s) in reported expected for '{ employee_to_check-employee }'|
          ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD validation_salary_pos_success.
    DATA:
      employee_mock_data TYPE STANDARD TABLE OF /dmo/employ_a_hd,
      employees_to_test  TYPE TABLE FOR VALIDATION /dmo/r_agencytp_hd\\employee~validationsalarypositive,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/r_agencytp_hd,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/r_agencytp_hd.

    employee_mock_data = VALUE #(
        agency = agency3
        ( employee = employee1  salary = '123' )
      ).
    cds_test_environment->insert_test_data( employee_mock_data ).

    employees_to_test = CORRESPONDING #( employee_mock_data MAPPING agency = agency  employee = employee ).

    class_under_test->validationsalarypositive(
        EXPORTING
          keys     = employees_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = lines( employees_to_test )
        act = lines( reported-employee )
      ).

  ENDMETHOD.

  METHOD validation_salary_pos_fail.
    TYPES: BEGIN OF t_check.
             INCLUDE TYPE /dmo/employ_a_hd.
    TYPES:   exp_amount_reported_entries TYPE i,
             exp_amount_failed_entries   TYPE i,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY agency employee.

    DATA:
      employee_mock_data TYPE STANDARD TABLE OF /dmo/employ_a_hd,
      employees_to_check TYPE t_check_table,
      employee_to_check  TYPE t_check,
      employees_to_test  TYPE TABLE FOR VALIDATION /dmo/r_agencytp_hd\\employee~validationsalarypositive,
      failed             TYPE RESPONSE FOR FAILED   LATE /dmo/r_agencytp_hd,
      reported           TYPE RESPONSE FOR REPORTED LATE /dmo/r_agencytp_hd.


    employees_to_check = VALUE #(
        exp_amount_reported_entries = '2'
        exp_amount_failed_entries   = '1'
        agency = agency3
        ( employee = employee3  salary = ''     )
        ( employee = employee4  salary = '0'    )
        ( employee = employee5  salary = '-123' )
      ).
    employee_mock_data = CORRESPONDING #( employees_to_check ).
    cds_test_environment->insert_test_data( employee_mock_data ).

    employees_to_test = CORRESPONDING #( employee_mock_data MAPPING employee = employee ).

    class_under_test->validationsalarypositive(
        EXPORTING
          keys     = employees_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR employee IN employees_to_check
                            NEXT sum += employee-exp_amount_failed_entries )
           act  = lines( failed-employee )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT employees_to_check INTO employee_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = employee_to_check-exp_amount_failed_entries
            act  = lines( FILTER #( failed-employee USING KEY entity WHERE agency = employee_to_check-agency AND employee = employee_to_check-employee ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ employee_to_check-exp_amount_failed_entries } line(s) in failed expected for '{ employee_to_check-employee }'|
          ).
      ENDLOOP.
    ENDIF.

    cl_abap_unit_assert=>assert_not_initial( reported ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR employee IN employees_to_check
                            NEXT sum += employee-exp_amount_reported_entries )
           act  = lines( reported-employee )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT employees_to_check INTO employee_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = employee_to_check-exp_amount_reported_entries
            act  = lines( FILTER #( reported-employee USING KEY entity WHERE agency = employee_to_check-agency AND employee = employee_to_check-employee ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ employee_to_check-exp_amount_reported_entries } line(s) in reported expected for '{ employee_to_check-employee }'|
          ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
