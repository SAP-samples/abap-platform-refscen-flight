"! This class tests some read only methods ( get_featues, validate_travel_status)
"! <br/>Data to be read is mocked by the cds test double framework
"! <br/>Additional data is mocked by the osql test double framework
"! <br/>Features used:
"! <ul>
"! <li>CREATE OBJECT FOR TESTING</li>
"! <li>CL_CDS_TEST_ENVIRONMENT</li>
"! <li>CL_OSQL_TEST_ENVIRONMENT</li>
"! </ul>
CLASS test_readonly_methods DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA: class_under_test     TYPE REF TO lhc_travel,  " the class to be tested
                cds_test_environment TYPE REF TO if_cds_test_environment,
                sql_test_environment TYPE REF TO if_osql_test_environment.

    "! setup test double framework
    CLASS-METHODS class_setup.

    "! reset test doubles
    METHODS setup.

    "! Test of validation method validate_travel_status
    METHODS validate_travel_status FOR TESTING.

    METHODS get_features FOR TESTING.

    METHODS validate_customer FOR TESTING.

    "! rollback any changes
    METHODS teardown.

    "! stop test doubles
    CLASS-METHODS class_teardown.
ENDCLASS.

CLASS test_readonly_methods IMPLEMENTATION.

  METHOD class_setup.
    " Create the Class under Test
    " The class is abstract but can be constructed with the FOR TESTING
    CREATE OBJECT class_under_test FOR TESTING.

    " Create test doubles for database dependencies
    " The EML READ operation will then also access the test doubles
    cds_test_environment = cl_cds_test_environment=>create( i_for_entity = '/DMO/I_TRAVEL_M' ).
    cds_test_environment->enable_double_redirection( ).

    sql_test_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( '/DMO/CUSTOMER' ) ) ).
  ENDMETHOD.

  METHOD setup.
    " clear the content of the test double per test
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD validate_travel_status.
    " fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = 42 overall_status = 'A' )
                                ( travel_id = 43 overall_status = 'B' ) " invalid status
                                ( travel_id = 44 overall_status = 'O' ) ).
    cds_test_environment->insert_test_data( i_data = travel_mock_data ).

    " call the method to be tested
    DATA failed TYPE RESPONSE FOR FAILED LATE /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    class_under_test->validate_travel_status(
      EXPORTING
        keys     = CORRESPONDING #( travel_mock_data )
      CHANGING
        failed   = failed
        reported = reported
    ).

    " check that failed has the relevant travel_id
    cl_abap_unit_assert=>assert_not_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'failed-travelid' act = failed-travel[ 1 ]-travel_id exp = 43 ).

    " check that reported also has the correct travel_id, the %element flagged and a message posted
    cl_abap_unit_assert=>assert_not_initial( msg = 'reported' act = reported ).
    DATA(reported_travel) = reported-travel[ 1 ].
    cl_abap_unit_assert=>assert_equals( msg = 'reported-travelid' act = reported_travel-travel_id  exp = 43 ).
    cl_abap_unit_assert=>assert_equals( msg = 'reported-%element' act = reported_travel-%element-overall_status  exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_bound(  msg = 'reported-%msg'     act = reported_travel-%msg ).

  ENDMETHOD.

  METHOD get_features.
    " fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = 42 overall_status = 'A' )
                                ( travel_id = 43 overall_status = 'X' )
                                ( travel_id = 44 overall_status = 'O' ) ).
    cds_test_environment->insert_test_data( i_data = travel_mock_data ).


    " call the method to be tested
    DATA result  TYPE TABLE FOR INSTANCE FEATURES RESULT /dmo/i_travel_m\\travel.
    DATA failed TYPE RESPONSE FOR FAILED EARLY /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    class_under_test->get_features(
      EXPORTING
        keys               = CORRESPONDING #(  travel_mock_data )
        requested_features = VALUE #(  %action-accepttravel = if_abap_behv=>mk-on )
      CHANGING
        result             = result
        failed             = failed
        reported           = reported
    ).

    " get_features should only fail on invalid keys
    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).


    " copy only relevant fields travel_id and %action-accepttravel
    DATA actual LIKE result.
    actual = CORRESPONDING #( result MAPPING travel_id = travel_id
                                  (  %action = %action MAPPING accepttravel = accepttravel EXCEPT * ) EXCEPT * ).

    DATA expected LIKE result.
    expected  = VALUE #( ( travel_id = 42 %action-accepttravel = if_abap_behv=>fc-o-disabled )
                         ( travel_id = 43 %action-accepttravel = if_abap_behv=>fc-o-enabled  )
                         ( travel_id = 44 %action-accepttravel = if_abap_behv=>fc-o-enabled  ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'result' exp = expected act = actual ).
  ENDMETHOD.

  METHOD validate_customer.
    " fill in test data for entity travel
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = 42 customer_id = '' )
                                ( travel_id = 43 customer_id = '1' )
                                ( travel_id = 44 customer_id = '2' ) ).
    cds_test_environment->insert_test_data( i_data = travel_mock_data ).

    " fill in mock data for customer table
    DATA customer_mock_data TYPE STANDARD TABLE OF /dmo/customer.
    customer_mock_data = VALUE #(  (  customer_id = '1' ) ).
    sql_test_environment->insert_test_data( i_data = customer_mock_data ).

    " call the method to be tested
    DATA failed TYPE RESPONSE FOR FAILED LATE /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    class_under_test->validate_customer(
      EXPORTING
        keys               = CORRESPONDING #(  travel_mock_data )
      CHANGING
        failed             = failed
        reported           = reported
    ).

    IF NOT line_exists( failed-travel[ KEY entity travel_id = 42 ] ).
      cl_abap_unit_assert=>fail( msg = 'No failed for empty customer id'  ).
    ENDIF.
    IF NOT line_exists( failed-travel[ KEY entity travel_id = 44 ] ).
      cl_abap_unit_assert=>fail( msg = 'No failed for nonexisting customer'  ).
    ENDIF.
    IF     line_exists( failed-travel[ KEY entity travel_id = 43 ] ).
      cl_abap_unit_assert=>fail( msg = 'Failed for existing customer'  ).
    ENDIF.

    IF NOT line_exists( reported-travel[ KEY entity travel_id = 42 ] ).
      cl_abap_unit_assert=>fail( msg = 'No reported for empty customer id'  ).
    ENDIF.
    IF NOT line_exists( reported-travel[ KEY entity travel_id = 44 ] ).
      cl_abap_unit_assert=>fail( msg = 'No reported for nonexisting customer'  ).
    ENDIF.
    IF     line_exists( reported-travel[ KEY entity travel_id = 43 ] ).
      cl_abap_unit_assert=>fail( msg = 'Reported for existing customer'  ).
    ENDIF.
  ENDMETHOD.

  METHOD teardown.
    " clean up any involved entity
    ROLLBACK ENTITIES.
  ENDMETHOD.

  METHOD class_teardown.
    " stop mocking
    cds_test_environment->destroy( ).
  ENDMETHOD.

ENDCLASS.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


"! This class tests some methods with EML MODIFY
"! <br/>Data to be read is mocked by the cds test double framework
"! <br/>Data is written into the buffer of the entity but not to the database
"!
"! <br/>Features used:
"! <ul><li>CREATE OBJECT FOR TESTING</li><li>CL_CDS_TEST_ENVIRONMENT</li></ul>
"! Drawbacks
"! <ul><li>Determinations might be called during modify, disturbing the test<li>
"! <li>Cannot use IN LOCAL MODE, so reading data back might not work</li></ul>
CLASS test_writing_methods DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA: class_under_test     TYPE REF TO lhc_travel,  " the class to be tested
                cds_test_environment TYPE REF TO if_cds_test_environment.

    "! setup test double framework
    CLASS-METHODS class_setup.

    "! reset test doubles
    METHODS setup.



    METHODS set_status_completed FOR TESTING RAISING cx_static_check.

    "! rollback any changes
    METHODS teardown.

    "! stop test doubles
    CLASS-METHODS class_teardown.
ENDCLASS.

CLASS test_writing_methods IMPLEMENTATION.

  METHOD class_setup.
    " Create the Class under Test
    " The class is abstract but can be constructed with the FOR TESTING
    CREATE OBJECT class_under_test FOR TESTING.

    " Create test doubles for database dependencies
    " The EML READ operation will then also access the test doubles
    cds_test_environment = cl_cds_test_environment=>create( i_for_entity = '/DMO/I_TRAVEL_M' ).
    cds_test_environment->enable_double_redirection( ).
  ENDMETHOD.

  METHOD setup.
    " clear the content of the test double per test
    cds_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD set_status_completed.
    " fill in test data for entity travel
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = 42 overall_status = 'A' )
                                ( travel_id = 43 overall_status = 'B' )
                                ( travel_id = 44 overall_status = 'X' ) ).
    cds_test_environment->insert_test_data( i_data = travel_mock_data ).


    " call the method to be tested
    DATA result TYPE TABLE FOR ACTION RESULT /dmo/i_travel_m\\travel~accepttravel.
    DATA mapped TYPE RESPONSE FOR MAPPED EARLY /dmo/i_travel_m.
    DATA failed TYPE RESPONSE FOR FAILED EARLY /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    class_under_test->set_status_completed(
      EXPORTING
        keys     = CORRESPONDING #(  travel_mock_data )
      CHANGING
        result   = result
        mapped   = mapped
        failed   = failed
        reported = reported
    ).

    cl_abap_unit_assert=>assert_initial( msg = 'mapped' act = mapped ).
    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

    " expect input keys and output keys to be same and overall_status everywhere = 'A'
    DATA exp LIKE result.
    exp = VALUE #(  ( travel_id = 42 %param-travel_id = 42 %param-overall_status = 'A' )
                    ( travel_id = 43 %param-travel_id = 43 %param-overall_status = 'A' )
                    ( travel_id = 44 %param-travel_id = 44 %param-overall_status = 'A' ) ).

    " copy only fields of interest = travel_id, %param-travel_id, %param-overall_status.
    DATA act LIKE result.
    act = CORRESPONDING #( result MAPPING travel_id = travel_id
                                       (  %param = %param MAPPING travel_id = travel_id
                                                                  overall_status = overall_status
                                                                  EXCEPT * )
                                          EXCEPT * ).
    cl_abap_unit_assert=>assert_equals( msg = 'action result' exp = exp act = act ).


    "additionally check by reading entity state
    READ ENTITY /dmo/i_travel_m
      FIELDS ( travel_id overall_status ) WITH CORRESPONDING #( travel_mock_data )
      RESULT DATA(read_result).

    act = VALUE #( FOR t IN read_result ( travel_id = t-travel_id
                                          %param-travel_id = t-travel_id
                                          %param-overall_status = t-overall_status ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'read result' exp = exp act = act ).
  ENDMETHOD.


  METHOD teardown.
    " clean up any involved entity
    ROLLBACK ENTITIES.
  ENDMETHOD.
  METHOD class_teardown.
    " stop mocking
    cds_test_environment->destroy( ).
  ENDMETHOD.

ENDCLASS.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"! This class mocks the entity /DMO/I_TRAVEL_M.
"! All EML calls will go to this class instead of the real entity.
"! We will save the data written during the test method
"! and also return the same data for a read operation
CLASS test_double_for_entity DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES if_abap_behavior_testdouble.
    DATA written TYPE TABLE FOR UPDATE /dmo/i_travel_m.
ENDCLASS.

CLASS test_double_for_entity IMPLEMENTATION.
  METHOD if_abap_behavior_testdouble~modify.
    LOOP AT changes ASSIGNING FIELD-SYMBOL(<change>).
      cl_abap_unit_assert=>assert_equals( msg = 'modify-mock entity' exp = '/DMO/I_TRAVEL_M' act = <change>-entity_name ).
      cl_abap_unit_assert=>assert_equals( msg = 'modify-mock operation' exp = if_abap_behv=>op-m-update act = <change>-op ).
      ASSIGN <change>-instances->* TO FIELD-SYMBOL(<instances>).
      me->written = <instances>.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_abap_behavior_testdouble~read.
    LOOP AT retrievals ASSIGNING FIELD-SYMBOL(<retrieval>).
      cl_abap_unit_assert=>assert_equals( msg = 'read-mock entity' exp = '/DMO/I_TRAVEL_M' act = <retrieval>-entity_name ).
      cl_abap_unit_assert=>assert_equals( msg = 'read-mock operation' exp = if_abap_behv=>op-r-read act = <retrieval>-op ).
      ASSIGN <retrieval>-results->* TO FIELD-SYMBOL(<instances>).
      <instances> = CORRESPONDING #( written ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


CLASS test_using_entity_stub DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA: class_under_test TYPE REF TO lhc_travel,  " the class to be tested
                entity_mock      TYPE REF TO test_double_for_entity.

    "! setup test double framework
    CLASS-METHODS class_setup.

    "! reset test doubles
    METHODS setup.

    METHODS set_status_completed FOR TESTING RAISING cx_static_check.

    "! rollback any changes
    METHODS teardown.
ENDCLASS.

CLASS test_using_entity_stub IMPLEMENTATION.

  METHOD class_setup.
    " Create the Class under Test
    " The class is abstract but can be constructed with the FOR TESTING
    CREATE OBJECT class_under_test FOR TESTING.

    " create the test double for our entity. READ and MODIFIY will call entity_mock
    CREATE OBJECT entity_mock.
    entity_mock->if_abap_behavior_testdouble~root_name = '/DMO/I_TRAVEL_M'.
  ENDMETHOD.

  METHOD setup.
    " activate test double
    cl_abap_behv_test_environment=>set_test_double( object = entity_mock ).
  ENDMETHOD.

  METHOD set_status_completed.
    " call the method to be tested
    DATA result TYPE TABLE FOR ACTION RESULT /dmo/i_travel_m\\travel~accepttravel.
    DATA mapped TYPE RESPONSE FOR MAPPED EARLY /dmo/i_travel_m.
    DATA failed TYPE RESPONSE FOR FAILED EARLY /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    class_under_test->set_status_completed(
      EXPORTING
        keys     = VALUE #( ( travel_id = 42 )
                            ( travel_id = 43 ) )
      CHANGING
        result   = result
        mapped   = mapped
        failed   = failed
        reported = reported
    ).

    cl_abap_unit_assert=>assert_initial( msg = 'mapped' act = mapped ).
    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

    " expect input keys and output keys to be same and overall_status everywhere = 'A'
    DATA exp LIKE result.
    exp = VALUE #(  ( travel_id = 42 %param-travel_id = 42 %param-overall_status = 'A' )
                    ( travel_id = 43 %param-travel_id = 43 %param-overall_status = 'A' ) ).

    " 1. verify result parameter
    " copy only fields of interest = travel_id, %param-travel_id, %param-overall_status.
    DATA act LIKE result.
    act = VALUE #( FOR r IN result ( travel_id             = r-travel_id
                                     %param-travel_id      = r-%param-travel_id
                                     %param-overall_status = r-%param-overall_status ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'action result' exp = exp act = act ).

    " 2. also verify the data that was written into the entity mock during execution of the method
    act = VALUE #( FOR w IN entity_mock->written ( travel_id             = w-travel_id
                                                   %param-travel_id      = w-travel_id
                                                   %param-overall_status = w-overall_status ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'data written' exp = exp act = act ).
  ENDMETHOD.


  METHOD teardown.
    " deactivate test double
    cl_abap_behv_test_environment=>unset_test_double( root = '/DMO/TRAVEL_D' ).

    " clean up any involved entity (should not be necessary as we mocked the entity, just to be safe).
    ROLLBACK ENTITIES.
  ENDMETHOD.


ENDCLASS.
