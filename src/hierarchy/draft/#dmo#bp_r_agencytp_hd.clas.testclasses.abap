CLASS test_numbering DEFINITION CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      lif_numbering.

    TYPES:
      BEGIN OF ts_responses,
        lowest_reserved_number  TYPE /dmo/agency_id,
        reserve_numbers_message TYPE REF TO if_abap_behv_message,
        reserve_numbers_success TYPE abap_boolean,
      END OF ts_responses.

    METHODS:
      set_responses
        IMPORTING
          responses TYPE ts_responses.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      responses TYPE ts_responses.

ENDCLASS.

CLASS test_numbering IMPLEMENTATION.

  METHOD lif_numbering~get_lowest_reserved_number.
    result = responses-lowest_reserved_number.
  ENDMETHOD.

  METHOD lif_numbering~reserve_numbers.
    success = responses-reserve_numbers_success.
    message = responses-reserve_numbers_message.
  ENDMETHOD.

  METHOD set_responses.
    me->responses = responses.
  ENDMETHOD.

ENDCLASS.

CLASS test_message DEFINITION CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      if_abap_behv_message.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS test_message IMPLEMENTATION.

  METHOD if_message~get_longtext.
    result = 'Longtext'.
  ENDMETHOD.

  METHOD if_message~get_text.
    result = 'Text'.
  ENDMETHOD.

ENDCLASS.


"! @testing BDEF:/DMO/R_AgencyTP_HD
CLASS ltc_agency DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CONSTANTS:
      agency1 TYPE /dmo/agency_id VALUE '1',
      agency2 TYPE /dmo/agency_id VALUE '2',
      agency3 TYPE /dmo/agency_id VALUE '3',
      agency4 TYPE /dmo/agency_id VALUE '4'.

    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_agency,
      sql_test_environment TYPE REF TO if_osql_test_environment,
      numbering            TYPE REF TO test_numbering.

    CLASS-METHODS:
      "! Instantiate class under test and set up test double framework
      class_setup,

      "! Destroy test environment and test double
      class_teardown.


    DATA:
      mapped   TYPE RESPONSE FOR MAPPED EARLY /dmo/r_agencytp_hd,
      failed   TYPE RESPONSE FOR FAILED LATE /dmo/r_agencytp_hd,
      reported TYPE RESPONSE FOR REPORTED LATE /dmo/r_agencytp_hd.


    METHODS:
      "! Reset test double
      setup,

      "! Reset transactional buffer
      teardown.


    METHODS:
      "! Checks if { @link ..lhc_agency.METH:earlynumbering_create } fulfills idempotency
      "! by passing an instance where the number is already drawn.
      earlynumbering_idempotency    FOR TESTING,

      "! Checks if { @link ..lhc_agency.METH:earlynumbering_create }
      "! draws a new number for instances where no number is drawn yet.
      earlynumbering_new_number     FOR TESTING,

      "! Checks if { @link ..lhc_agency.METH:earlynumbering_create }
      "! draws new number and adds message if supplied but still successful.
      earlynumbering_new_number_msg     FOR TESTING,

      "! Checks if { @link ..lhc_agency.METH:earlynumbering_create }
      "! returns failed and messages if numbering fails.
      earlynumbering_failed_number     FOR TESTING,

      "! Checks if { @link ..lhc_agency.METH:earlynumbering_cba_employee }
      "! draws a new number for instances where no number is drawn yet.
      earlynumbering_cba_new_number     FOR TESTING,

      "! Checks if { @link ..lhc_agency.METH:validateName } behaves correctly for an instance
      "! where Name is set correctly.
      validate_name_success        FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_agency.METH:validateName } behaves correctly for instances
      "! where Name is not set correctly.  This should result in filled reported and failed tables.
      validate_name_failed         FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_agency.METH:validateemailaddress } behaves correctly for an instance
      "! where email address is set correctly.
      validate_email_success       FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_agency.METH:validateemailaddress } behaves correctly for instances
      "! where email address is not set correctly.  This should result in filled reported and failed tables.
      "! All permutations are tested here.
      validate_email_failed        FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_agency.METH:validateCountryCode } behaves correctly for an instance
      "! where country code is set correctly.
      validate_countrycode_success FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_agency.METH:validateCountryCode } behaves correctly for instances
      "! where country code is not set correctly.  This should result in filled reported and failed tables.
      "! All permutations are tested here.
      validate_countrycode_failed  FOR TESTING RAISING cx_static_check.

*      "! Checks that { @link ..lhc_agency.METH:get_global_authorizations } returns initial values
*      "! for <em>result</em> and <em>reported</em>.
*      get_global_authorizations    FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_agency IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    sql_test_environment = cl_osql_test_environment=>create(
                               VALUE #(
                                   ( '/DMO/R_AGENCYTP_HD'   )
                                   ( '/DMO/R_EMPLOYEETP_HD' )
                                   ( 'I_COUNTRY' )
                                 )
                             ).
    numbering = NEW test_numbering( ).
    class_under_test->numbering = numbering.
  ENDMETHOD.

  METHOD setup.
    sql_test_environment->clear_doubles( ).

    CLEAR mapped.
    CLEAR failed.
    CLEAR reported.
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
    sql_test_environment->destroy( ).
  ENDMETHOD.


  METHOD earlynumbering_idempotency.
    DATA:
      entity          TYPE STRUCTURE FOR CREATE /dmo/r_agencytp_hd\\agency,
      mapped          TYPE RESPONSE FOR MAPPED /dmo/r_agencytp_hd,
      failed          TYPE RESPONSE FOR FAILED EARLY /dmo/r_agencytp_hd,
      reported        TYPE RESPONSE FOR REPORTED EARLY /dmo/r_agencytp_hd,
      act_mapped_line LIKE LINE OF mapped-agency,
      exp_mapped_line LIKE LINE OF mapped-agency.

    entity = VALUE #( %cid = 'Test'  agency = 'XX123' ).

    numbering->set_responses(
      VALUE test_numbering=>ts_responses(
        lowest_reserved_number  = '4'
        reserve_numbers_success = abap_true
      )
    ).

    class_under_test->earlynumbering_create(
        EXPORTING
          new_agencies = VALUE #( ( entity ) )
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     failed   ).
    cl_abap_unit_assert=>assert_initial(     reported ).
    cl_abap_unit_assert=>assert_not_initial( mapped   ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( mapped-agency ) ).

    act_mapped_line = mapped-agency[ 1 ].
    exp_mapped_line = CORRESPONDING #( entity ).

    cl_abap_unit_assert=>assert_equals( exp = exp_mapped_line  act = act_mapped_line ).
  ENDMETHOD.

  METHOD earlynumbering_new_number.
    DATA:
      entity          TYPE STRUCTURE FOR CREATE /dmo/r_agencytp_hd\\agency,
      mapped          TYPE RESPONSE FOR MAPPED /dmo/r_agencytp_hd,
      failed          TYPE RESPONSE FOR FAILED EARLY /dmo/r_agencytp_hd,
      reported        TYPE RESPONSE FOR REPORTED EARLY /dmo/r_agencytp_hd,
      act_mapped_line LIKE LINE OF mapped-agency,
      exp_mapped_line LIKE LINE OF mapped-agency.

    entity = VALUE #( %cid = 'Test' ).

    numbering->set_responses(
      VALUE test_numbering=>ts_responses(
        lowest_reserved_number  = '4'
        reserve_numbers_success = abap_true
      )
    ).


    class_under_test->earlynumbering_create(
        EXPORTING
          new_agencies = VALUE #( ( entity ) )
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     failed   ).
    cl_abap_unit_assert=>assert_initial(     reported ).
    cl_abap_unit_assert=>assert_not_initial( mapped   ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( mapped-agency ) ).

    act_mapped_line = mapped-agency[ 1 ].
    cl_abap_unit_assert=>assert_not_initial( act_mapped_line-agency ).

    exp_mapped_line = CORRESPONDING #( entity ).
    exp_mapped_line-agency = act_mapped_line-agency.

    cl_abap_unit_assert=>assert_equals( exp = exp_mapped_line  act = act_mapped_line ).
  ENDMETHOD.

  METHOD earlynumbering_cba_new_number.
    DATA:
      agency_mock_data   TYPE STANDARD TABLE OF /dmo/r_agencytp_hd,
      employee_mock_data TYPE STANDARD TABLE OF /dmo/r_employeetp_hd,
      exp_travel_read    TYPE TABLE FOR READ RESULT /dmo/r_agencytp_hd\\agency,
      mapped             TYPE RESPONSE FOR MAPPED EARLY  /dmo/r_agencytp_hd,
      failed             TYPE RESPONSE FOR FAILED EARLY  /dmo/r_agencytp_hd,
      reported           TYPE RESPONSE FOR REPORTED EARLY  /dmo/r_agencytp_hd,
      entities           TYPE TABLE FOR CREATE /dmo/r_agencytp_hd\\agency\_employee.

    agency_mock_data = VALUE #(
        ( agency = agency1 ) "no existing employees and no number provided
        ( agency = agency2 ) "no existing employees but some numbers provided
        ( agency = agency3 ) "existing employees
        ( agency = agency4 ) "existing employees and some numbers provided
      ).
    sql_test_environment->insert_test_data( agency_mock_data ).

    employee_mock_data = VALUE #(
        ( agency   = agency3  employee = '1' )
        ( agency   = agency4  employee = '2' )
      ).
    sql_test_environment->insert_test_data( employee_mock_data ).

    entities = VALUE #(
        (
          agency = agency1  "no existing employees and no number provided
          %target   = VALUE #(
              ( %cid = '1b1' )
            )
        )
        (
          agency = agency2  "no existing employees but some numbers provided
          %target   = VALUE #(
              ( %cid = '2b1' )
              ( %cid = '2b2'  employee = '1' )
            )
        )
        (
          agency = agency3  "existing employees
          %target   = VALUE #(
              ( %cid = '3b1' )
            )
        )
        (
          agency = agency4  "existing employees and some numbers provided
          %target   = VALUE #(
              ( %cid = '4b1' )
              ( %cid = '4b2'  employee = '1' )
              ( %cid = '4b3'  employee = '3' )
            )
        )
      ).

    class_under_test->earlynumbering_cba_employee(
        EXPORTING
          new_employees = entities
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( mapped-agency ).
    cl_abap_unit_assert=>assert_not_initial( mapped-employee ).

    cl_abap_unit_assert=>assert_equals(
        exp = REDUCE #( INIT i = 0
                        FOR travel IN entities
                        NEXT i += lines( travel-%target ) )
        act = lines( mapped-employee )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/employee_id( '1' )
        act = VALUE /dmo/employee_id( mapped-employee[ %cid = '1b1' ]-employee )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/employee_id( '2' )
        act = VALUE /dmo/employee_id( mapped-employee[ %cid = '2b1' ]-employee )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/employee_id( '2' )
        act = VALUE /dmo/employee_id( mapped-employee[ %cid = '3b1' ]-employee )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/employee_id( '4' )
        act = VALUE /dmo/employee_id( mapped-employee[ %cid = '4b1' ]-employee )
      ).
  ENDMETHOD.


  METHOD earlynumbering_failed_number.
    DATA:
      entity            TYPE STRUCTURE FOR CREATE /dmo/r_agencytp_hd\\agency,
      mapped            TYPE RESPONSE FOR MAPPED /dmo/r_agencytp_hd,
      failed            TYPE RESPONSE FOR FAILED EARLY /dmo/r_agencytp_hd,
      reported          TYPE RESPONSE FOR REPORTED EARLY /dmo/r_agencytp_hd,
      act_failed_line   LIKE LINE OF failed-agency,
      exp_failed_line   LIKE LINE OF failed-agency,
      act_reported_line LIKE LINE OF reported-agency,
      exp_reported_line LIKE LINE OF reported-agency,
      test_message      TYPE REF TO test_message.

    test_message = NEW test_message( ).

    entity = VALUE #( %cid = 'Test' ).

    numbering->set_responses(
      VALUE test_numbering=>ts_responses(
        lowest_reserved_number  = '4'
        reserve_numbers_message = test_message
        reserve_numbers_success = abap_false
      )
    ).


    class_under_test->earlynumbering_create(
        EXPORTING
          new_agencies = VALUE #( ( entity ) )
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped ).

    cl_abap_unit_assert=>assert_not_initial( failed ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( failed-agency ) ).

    act_failed_line = failed-agency[ 1 ].
    cl_abap_unit_assert=>assert_not_initial( act_failed_line ).

    cl_abap_unit_assert=>assert_equals(
        exp = if_abap_behv=>cause-unspecific
        act = act_failed_line-%fail-cause
      ).


    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( reported-agency ) ).

    act_reported_line = reported-agency[ 1 ].
    cl_abap_unit_assert=>assert_not_initial( act_reported_line ).
    DATA(test_message_name) = cl_abap_classdescr=>get_class_name( act_reported_line-%msg ).
    cl_abap_unit_assert=>assert_equals(
        exp = '\PROGRAM=/DMO/BP_R_AGENCYTP_HD=========CP\CLASS=TEST_MESSAGE'
        act = test_message_name
      ).

  ENDMETHOD.

  METHOD earlynumbering_new_number_msg.
    DATA:
      entity            TYPE STRUCTURE FOR CREATE /dmo/r_agencytp_hd\\agency,
      mapped            TYPE RESPONSE FOR MAPPED /dmo/r_agencytp_hd,
      failed            TYPE RESPONSE FOR FAILED EARLY /dmo/r_agencytp_hd,
      reported          TYPE RESPONSE FOR REPORTED EARLY /dmo/r_agencytp_hd,
      act_mapped_line   LIKE LINE OF mapped-agency,
      exp_mapped_line   LIKE LINE OF mapped-agency,
      act_reported_line LIKE LINE OF reported-agency,
      exp_reported_line LIKE LINE OF reported-agency,
      test_message      TYPE REF TO test_message.

    test_message = NEW test_message( ).

    entity = VALUE #( %cid = 'Test' ).

    numbering->set_responses(
      VALUE test_numbering=>ts_responses(
        lowest_reserved_number  = '4'
        reserve_numbers_message = test_message
        reserve_numbers_success = abap_true
      )
    ).


    class_under_test->earlynumbering_create(
        EXPORTING
          new_agencies = VALUE #( ( entity ) )
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     failed   ).
    cl_abap_unit_assert=>assert_not_initial( mapped   ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( mapped-agency ) ).

    act_mapped_line = mapped-agency[ 1 ].
    cl_abap_unit_assert=>assert_not_initial( act_mapped_line-agency ).

    exp_mapped_line = CORRESPONDING #( entity ).
    exp_mapped_line-agency = act_mapped_line-agency.

    cl_abap_unit_assert=>assert_equals( exp = exp_mapped_line  act = act_mapped_line ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( reported-agency ) ).


    act_reported_line = reported-agency[ 1 ].
    cl_abap_unit_assert=>assert_not_initial( act_reported_line ).
    DATA(test_message_name) = cl_abap_classdescr=>get_class_name( act_reported_line-%msg ).
    cl_abap_unit_assert=>assert_equals(
        exp = '\PROGRAM=/DMO/BP_R_AGENCYTP_HD=========CP\CLASS=TEST_MESSAGE'
        act = test_message_name
      ).
  ENDMETHOD.


  METHOD validate_name_success.
    DATA:
      agency   TYPE /dmo/r_agencytp_hd,
      agencies TYPE STANDARD TABLE OF /dmo/r_agencytp_hd.

    agency = VALUE /dmo/r_agencytp_hd(
        agency = '123'
        name     = 'FlightBooker'
      ).

    agencies = VALUE #( ( agency ) ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatename(
        EXPORTING
          keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  agency = agency-agency ) )
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     failed   ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

    " One line is expected as this clears the state area.
    cl_abap_unit_assert=>assert_equals(
        msg = 'Reported has more than one messages'
        exp = 1
        act = lines( reported-agency )
      ).
    DATA(reported_agency) = reported-agency[ 1 ].

    " Check that only %tky and %state_area is filled
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off                         act = reported_agency-%is_draft    ).
    cl_abap_unit_assert=>assert_equals( exp = agency-agency                                act = reported_agency-agency ).
    cl_abap_unit_assert=>assert_equals( exp = class_under_test->state_areas-validate_name  act = reported_agency-%state_area ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%op ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%msg ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%element ).
  ENDMETHOD.


  METHOD validate_name_failed.
    DATA:
      agency                    TYPE /dmo/r_agencytp_hd,
      agencies                  TYPE STANDARD TABLE OF /dmo/r_agencytp_hd,
      reported_with_message     TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp_hd,
      reported_clear_state_area TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp_hd.

    agency = VALUE /dmo/r_agencytp_hd(
        agency = '123'
        name     = ''
      ).

    agencies = VALUE #( ( agency ) ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatename(
        EXPORTING
          keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  agency = agency-agency ) )
        CHANGING
          failed   = failed
          reported = reported
      ).

    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals( exp = 1  act = lines( failed-agency ) ).

    LOOP AT failed-agency INTO DATA(failed_line).
      cl_abap_unit_assert=>assert_not_initial(
          msg = 'Failed key was not provided'
          act = VALUE #( agencies[ agency = failed_line-agency ] OPTIONAL )
        ).
    ENDLOOP.


    " check reported
    cl_abap_unit_assert=>assert_not_initial( reported ).
    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals(
        msg = 'Reported has not the correct amount of messages'
        exp = 2
        act = lines( reported-agency )
      ).

    LOOP AT reported-agency INTO DATA(reported_line).
      IF reported_line-%msg IS BOUND.
        reported_with_message     = reported_line.
      ELSE.
        reported_clear_state_area = reported_line.
      ENDIF.

      cl_abap_unit_assert=>assert_equals(
          exp = lhc_agency=>state_areas-validate_name
          act = reported_line-%state_area
        ).
      cl_abap_unit_assert=>assert_equals(
          exp = agency-agency
          act = reported_line-agency
        ).
      cl_abap_unit_assert=>assert_equals(
          exp = if_abap_behv=>mk-off
          act = reported_line-%is_draft
        ).

    ENDLOOP.

    cl_abap_unit_assert=>assert_not_initial( reported_with_message     ).
    cl_abap_unit_assert=>assert_not_initial( reported_clear_state_area ).

    " check message
    cl_abap_unit_assert=>assert_equals(
        exp = if_abap_behv=>mk-on
        act = reported_with_message-%element-name
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = CORRESPONDING symsg( /dmo/cx_agency=>name_required )
        act = CORRESPONDING symsg( reported_with_message-%msg->if_t100_message~t100key )
      ).
  ENDMETHOD.


  METHOD validate_email_success.
    DATA:
      agency   TYPE /dmo/r_agencytp_hd,
      agencies TYPE STANDARD TABLE OF /dmo/r_agencytp_hd.

    agency = VALUE /dmo/r_agencytp_hd( agency     = '123'
                                    emailaddress = 'name@provider.toplevel' ).

    agencies = VALUE #( ( agency ) ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validateemailaddress(
      EXPORTING keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  agency = agency-agency ) )
      CHANGING  failed   = failed
                reported = reported ).

    cl_abap_unit_assert=>assert_initial(     failed   ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

    " One line is expected as this clears the state area.
    cl_abap_unit_assert=>assert_equals(
        msg = 'Reported has more than one messages'
        exp = 1
        act = lines( reported-agency )
      ).
    DATA(reported_agency) = reported-agency[ 1 ].

    " Check that only %tky and %state_area is filled
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off                         act = reported_agency-%is_draft    ).
    cl_abap_unit_assert=>assert_equals( exp = agency-agency                              act = reported_agency-agency ).
    cl_abap_unit_assert=>assert_equals( exp = class_under_test->state_areas-validate_email  act = reported_agency-%state_area ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%op ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%msg ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%element ).
  ENDMETHOD.


  METHOD validate_email_failed.
    CONSTANTS:
      cv_found TYPE /dmo/r_agencytp_hd-name VALUE 'found' ##NO_TEXT.
    DATA:
      agency                    TYPE /dmo/r_agencytp_hd,
      agencies                  TYPE STANDARD TABLE OF /dmo/r_agencytp_hd,
      reported_with_message     TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp_hd,
      reported_clear_state_area TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp_hd.

    agencies = VALUE #(
        ( agency = '1'  emailaddress = 'name@provider' )
        ( agency = '2'  emailaddress = 'name@.toplevel' )
        ( agency = '3'  emailaddress = 'name@provider.' )
        ( agency = '4'  emailaddress = '@provider.toplevel' )
        ( agency = '5'  emailaddress = '' )
        ( agency = '6'  emailaddress = '@.' )
        ( agency = '7'  emailaddress = '@' )
        ( agency = '8'  emailaddress = '.' )
        ( agency = '9'  emailaddress = '@provider.' )
      ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validateemailaddress(
        EXPORTING
          keys     = VALUE #( FOR a IN agencies
                                %is_draft = if_abap_behv=>mk-off
                                ( agency = a-agency )
                              )
         CHANGING
           failed   = failed
           reported = reported
       ).

    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = lines( agencies )
        act = lines( failed-agency )
      ).

    LOOP AT failed-agency INTO DATA(failed_line).
      cl_abap_unit_assert=>assert_not_initial(
          msg = 'Failed key was not provided'
          act = VALUE #( agencies[ agency = failed_line-agency ] OPTIONAL )
        ).
    ENDLOOP.


    " check reported
    cl_abap_unit_assert=>assert_not_initial( reported ).
    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals(
        msg = 'Reported has not the correct amount of messages'
        exp = 2 * lines( agencies )
        act = lines( reported-agency )
      ).

    LOOP AT agencies ASSIGNING FIELD-SYMBOL(<agency>).
      CLEAR reported_with_message.
      CLEAR reported_clear_state_area.

      LOOP AT reported-agency INTO DATA(reported_line) USING KEY entity WHERE agency = <agency>-agency.
        IF reported_line-%msg IS BOUND.
          reported_with_message     = reported_line.
        ELSE.
          reported_clear_state_area = reported_line.
        ENDIF.

        cl_abap_unit_assert=>assert_equals(
            exp = lhc_agency=>state_areas-validate_email
            act = reported_line-%state_area
          ).
        cl_abap_unit_assert=>assert_equals(
            exp = <agency>-agency
            act = reported_line-agency
          ).
        cl_abap_unit_assert=>assert_equals(
            exp = if_abap_behv=>mk-off
            act = reported_line-%is_draft
          ).

        <agency>-name = cv_found.
      ENDLOOP.

      cl_abap_unit_assert=>assert_not_initial( reported_with_message     ).
      cl_abap_unit_assert=>assert_not_initial( reported_clear_state_area ).

      " check message
      cl_abap_unit_assert=>assert_equals(
          exp = if_abap_behv=>mk-on
          act = reported_with_message-%element-emailaddress
        ).
      DATA(elements) = reported_with_message-%element.
      elements-emailaddress = if_abap_behv=>mk-off.
      cl_abap_unit_assert=>assert_initial( elements ).

      cl_abap_unit_assert=>assert_equals(
          exp = CORRESPONDING symsg( /dmo/cx_agency=>email_invalid_format )
          act = CORRESPONDING symsg( reported_with_message-%msg->if_t100_message~t100key )
        ).
    ENDLOOP.

    LOOP AT agencies INTO agency WHERE name IS INITIAL.
      cl_abap_unit_assert=>fail(
          msg  = |EMail Address "{ agency-emailaddress }" was accepted but shouldn't!|
          quit = if_abap_unit_constant=>quit-no
        ).
    ENDLOOP.
  ENDMETHOD.


  METHOD validate_countrycode_success.
    DATA:
      agency    TYPE /dmo/r_agencytp_hd,
      agencies  TYPE STANDARD TABLE OF /dmo/r_agencytp_hd,
      country   TYPE i_country,
      countries TYPE STANDARD TABLE OF i_country.

    country = VALUE i_country( country = 'DE' ).
    countries = VALUE #( ( country ) ).
    sql_test_environment->insert_test_data( countries ).

    agency = VALUE /dmo/r_agencytp_hd(
        agency    = '123'
        countrycode = country-country
      ).

    agencies = VALUE #( ( agency ) ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatecountrycode(
        EXPORTING
          keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  agency = agency-agency ) )
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     failed   ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

    " One line is expected as this clears the state area.
    cl_abap_unit_assert=>assert_equals(
        msg = 'Reported has more than one messages'
        exp = 1
        act = lines( reported-agency )
      ).
    DATA(reported_agency) = reported-agency[ 1 ].

    " Check that only %tky and %state_area is filled
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off                           act = reported_agency-%is_draft    ).
    cl_abap_unit_assert=>assert_equals( exp = agency-agency                                act = reported_agency-agency ).
    cl_abap_unit_assert=>assert_equals( exp = class_under_test->state_areas-validate_country  act = reported_agency-%state_area ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%op ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%msg ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%element ).
  ENDMETHOD.


  METHOD validate_countrycode_failed.
    CONSTANTS:
      cv_found TYPE /dmo/r_agencytp_hd-name VALUE 'found' ##NO_TEXT.
    DATA:
      agency                    TYPE /dmo/r_agencytp_hd,
      agencies                  TYPE STANDARD TABLE OF /dmo/r_agencytp_hd,
      country                   TYPE i_country,
      countries                 TYPE STANDARD TABLE OF i_country,
      reported_with_message     TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp_hd,
      reported_clear_state_area TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp_hd.


    country = VALUE i_country( country = 'DE' ).
    countries = VALUE #( ( country ) ).
    sql_test_environment->insert_test_data( countries ).

    agencies = VALUE #( ( agency = '1'  countrycode = 'XX' )
                        ( agency = '2'  countrycode = '' ) ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatecountrycode(
        EXPORTING
          keys     = VALUE #( FOR a IN agencies
                                 %is_draft = if_abap_behv=>mk-off
                                 ( agency = a-agency )
                               )
        CHANGING
          failed   = failed
          reported = reported
      ).

    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = lines( agencies )
        act = lines( failed-agency )
      ).

    LOOP AT failed-agency INTO DATA(failed_line).
      cl_abap_unit_assert=>assert_not_initial(
          msg = 'Failed key was not provided'
          act = VALUE #( agencies[ agency = failed_line-agency ] OPTIONAL )
        ).
    ENDLOOP.


    " check reported
    cl_abap_unit_assert=>assert_not_initial( reported ).
    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals(
        msg = 'Reported has not the correct amount of messages'
        exp = 2 * lines( agencies )
        act = lines( reported-agency )
      ).

    LOOP AT agencies ASSIGNING FIELD-SYMBOL(<agency>).
      CLEAR reported_with_message.
      CLEAR reported_clear_state_area.

      LOOP AT reported-agency INTO DATA(reported_line) USING KEY entity WHERE agency = <agency>-agency.
        IF reported_line-%msg IS BOUND.
          reported_with_message     = reported_line.
        ELSE.
          reported_clear_state_area = reported_line.
        ENDIF.

        cl_abap_unit_assert=>assert_equals(
            exp = lhc_agency=>state_areas-validate_country
            act = reported_line-%state_area
          ).
        cl_abap_unit_assert=>assert_equals(
            exp = <agency>-agency
            act = reported_line-agency
          ).
        cl_abap_unit_assert=>assert_equals(
            exp = if_abap_behv=>mk-off
            act = reported_line-%is_draft
          ).

        <agency>-name = cv_found.
      ENDLOOP.

      cl_abap_unit_assert=>assert_not_initial( reported_with_message     ).
      cl_abap_unit_assert=>assert_not_initial( reported_clear_state_area ).

      " check message
      cl_abap_unit_assert=>assert_equals(
          exp = if_abap_behv=>mk-on
          act = reported_with_message-%element-countrycode
        ).
      DATA(elements) = reported_with_message-%element.
      elements-countrycode = if_abap_behv=>mk-off.
      cl_abap_unit_assert=>assert_initial( elements ).

      cl_abap_unit_assert=>assert_equals(
          exp = CORRESPONDING symsg( /dmo/cx_agency=>country_code_invalid )
          act = CORRESPONDING symsg( reported_with_message-%msg->if_t100_message~t100key )
        ).
    ENDLOOP.

    LOOP AT agencies INTO agency WHERE name IS INITIAL.
      cl_abap_unit_assert=>fail(
          msg  = |Country Code "{ agency-countrycode }" was accepted but shouldn't!|
          quit = if_abap_unit_constant=>quit-no
        ).
    ENDLOOP.
  ENDMETHOD.


*  METHOD get_global_authorizations.
*    DATA:
*      requested_authorizations TYPE STRUCTURE FOR GLOBAL AUTHORIZATION REQUEST /dmo/r_agencytp_hd\\agency,
*      result                   TYPE STRUCTURE FOR GLOBAL AUTHORIZATION RESULT /dmo/r_agencytp_hd\\agency,
*      reported                 TYPE RESPONSE FOR REPORTED EARLY /dmo/r_agencytp_hd.
*
*    requested_authorizations-%create = if_abap_behv=>mk-on.
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

ENDCLASS.

CLASS ltcl_numbering_by_numberrange DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      get_number FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_numbering_by_numberrange IMPLEMENTATION.

  METHOD get_number.
    DATA:
      numbering TYPE REF TO lif_numbering.

    TRY.
        cl_numberrange_intervals=>read(
            EXPORTING
              nr_range_nr1      = '01'
              object            = '/DMO/AGNHD'
            IMPORTING
              interval     = DATA(interval)
          ).
      CATCH cx_nr_object_not_found.
        cl_abap_unit_assert=>skip( 'Number Range Object is missing.  Please run the data generator!' ).
      CATCH cx_nr_subobject.
        cl_abap_unit_assert=>skip( 'Number Range Subobject is missing.  Please run the data generator!' ).
      CATCH cx_number_ranges INTO DATA(number_range_exception).
        cl_abap_unit_assert=>skip( number_range_exception->get_text( ) ).
    ENDTRY.

    numbering = NEW lcl_numbering_by_numberrange( ).
    numbering->reserve_numbers(
      EXPORTING
        quantity = '1'
      IMPORTING
        message  = DATA(message)
        success  = DATA(success)
    ).

    cl_abap_unit_assert=>assert_initial( message ).
    cl_abap_unit_assert=>assert_true( success ).

    DATA(lowest_reserved_number) = numbering->get_lowest_reserved_number( ).
    cl_abap_unit_assert=>assert_not_initial( lowest_reserved_number ).
  ENDMETHOD.

ENDCLASS.
