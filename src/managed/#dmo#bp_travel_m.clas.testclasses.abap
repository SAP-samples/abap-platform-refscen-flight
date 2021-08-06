"! @testing BDEF:/DMO/I_Travel_M
CLASS ltc_managed DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CLASS-DATA: class_under_test     TYPE REF TO lhc_travel,
                cds_test_environment TYPE REF TO if_cds_test_environment,
                sql_test_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      "! Instantiate class under test and setup test double frameworks
      class_setup,

      "! Destroy test environments and test doubles
      class_teardown.

    METHODS:
      "! Reset test doubles
      setup,

      "! Reset transactional buffer
      teardown,

      "! Check instance with overall status 'Open'
      validate_travel_status_open    FOR TESTING,

      "! Check instance with overall status 'Accepted'
      validate_travel_status_acpt    FOR TESTING,

      "! Check instance with overall status 'Rejected'
      validate_travel_status_rej     FOR TESTING,

      "! Check instance with invalid overall status
      validate_travel_status_invalid FOR TESTING,



      "! Check returned features for instance with overall status 'Open'
      get_features_open          FOR TESTING,

      "! Check returned features for instance with overall status 'Accepted'
      get_features_accepted      FOR TESTING,

      "! Check returned features for instance with overall status 'Rejected'
      get_features_rejected      FOR TESTING,

      "! Check returned features for instance with invalid overall status
      get_features_invalidstatus FOR TESTING,

      "! Check returned features for instance with invalid key
      get_features_invalidkey    FOR TESTING,



      "! Check instance with valid customer ID
      validate_customer_valid   FOR TESTING,

      "! Check instance with invalid customer ID
      validate_customer_invalid FOR TESTING,

      "! Check instance with initial customer ID
      validate_customer_initial FOR TESTING,



      "! Check set_status_accepted action
      set_status_accepted FOR TESTING.



ENDCLASS.

CLASS ltc_managed IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    cds_test_environment = cl_cds_test_environment=>create( i_for_entity = '/DMO/I_TRAVEL_M' ).
    sql_test_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( '/DMO/CUSTOMER' ) ) ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES. "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
    cds_test_environment->destroy( ).
    sql_test_environment->destroy( ).
  ENDMETHOD.

  METHOD validate_travel_status_open.

    " Fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = '52' overall_status = 'O' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required structures
    DATA failed TYPE RESPONSE FOR FAILED LATE /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    " Call method to be tested
    class_under_test->validate_travel_status(
      EXPORTING
        keys     = CORRESPONDING #( travel_mock_data )

      CHANGING
        failed   = failed
        reported = reported
    ).

    " Check for content in failed and reported
    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

  ENDMETHOD.


  METHOD validate_travel_status_acpt.

    " Fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = '52' overall_status = 'A' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required structures
    DATA failed TYPE RESPONSE FOR FAILED LATE /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    " Call method to be tested
    class_under_test->validate_travel_status(
      EXPORTING
        keys     = CORRESPONDING #( travel_mock_data )

      CHANGING
        failed   = failed
        reported = reported
    ).

    " Check for content in failed and reported
    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

  ENDMETHOD.


  METHOD validate_travel_status_rej.

    " Fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = '52' overall_status = 'X' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required structures
    DATA failed TYPE RESPONSE FOR FAILED LATE /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    " Call method to be tested
    class_under_test->validate_travel_status(
      EXPORTING
        keys     = CORRESPONDING #( travel_mock_data )

      CHANGING
        failed   = failed
        reported = reported
    ).

    " Check for content in failed and reported
    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

  ENDMETHOD.


  METHOD validate_travel_status_invalid.

    " Fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = '52' overall_status = 'T' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required structures
    DATA failed TYPE RESPONSE FOR FAILED LATE /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    " Call method to be tested
    class_under_test->validate_travel_status(
      EXPORTING
        keys     = CORRESPONDING #( travel_mock_data )

      CHANGING
        failed   = failed
        reported = reported
    ).

    " Check number of returned instances in failed-travel
    cl_abap_unit_assert=>assert_equals( act = lines( failed-travel ) exp = 1 msg = 'lines in failed-travel' ).

    " Check travel id in failed-travel
    cl_abap_unit_assert=>assert_equals( act = failed-travel[ 1 ]-travel_id exp = '52' msg = 'travel id in failed-travel'  ).



    " Check number of returned instances in reported-travel
    cl_abap_unit_assert=>assert_equals( act = lines( reported-travel ) exp = 1 msg = 'lines in reported-travel' ).

    " Check travel id in reported-travel
    cl_abap_unit_assert=>assert_equals( act = reported-travel[ 1 ]-travel_id  exp = '52' msg = 'travel id in reported-travel' ).

    " Check marked field in reported-travel
    cl_abap_unit_assert=>assert_equals( act = reported-travel[ 1 ]-%element-overall_status  exp = if_abap_behv=>mk-on msg = 'field overall status in reported-travel' ).

    " Check message reference in reported-travel
    cl_abap_unit_assert=>assert_bound( act = reported-travel[ 1 ]-%msg msg = 'message reference in reported-travel' ).

  ENDMETHOD.



  METHOD get_features_open.

    " Fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = '43' overall_status = 'O' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required table and structures
    DATA result  TYPE TABLE FOR INSTANCE FEATURES RESULT /dmo/i_travel_m\\travel.
    DATA failed TYPE RESPONSE FOR FAILED EARLY /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    " Call the method to be tested
    class_under_test->get_features(
      EXPORTING
        keys               = CORRESPONDING #(  travel_mock_data )
        requested_features = VALUE #(  %action-accepttravel = if_abap_behv=>mk-on
                                       %action-rejectTravel = if_abap_behv=>mk-on
                                       %assoc-_Booking      = if_abap_behv=>mk-on )

      CHANGING
        result             = result
        failed             = failed
        reported           = reported
    ).

    " Check result
    DATA expected LIKE result.
    expected  = VALUE #( ( travel_id            = '43'
                           %action-accepttravel = if_abap_behv=>fc-o-enabled
                           %action-rejectTravel = if_abap_behv=>fc-o-enabled
                           %assoc-_Booking      = if_abap_behv=>fc-o-enabled ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'result' exp = expected act = result ).

  ENDMETHOD.


  METHOD get_features_accepted.

    " Fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = '43' overall_status = 'A' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required table and structures
    DATA result  TYPE TABLE FOR INSTANCE FEATURES RESULT /dmo/i_travel_m\\travel.
    DATA failed TYPE RESPONSE FOR FAILED EARLY /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    " Call the method to be tested
    class_under_test->get_features(
      EXPORTING
        keys               = CORRESPONDING #(  travel_mock_data )
        requested_features = VALUE #(  %action-accepttravel = if_abap_behv=>mk-on
                                       %action-rejectTravel = if_abap_behv=>mk-on
                                       %assoc-_Booking      = if_abap_behv=>mk-on )

      CHANGING
        result             = result
        failed             = failed
        reported           = reported
    ).

    " Check result
    DATA expected LIKE result.
    expected  = VALUE #( ( travel_id            = '43'
                           %action-accepttravel = if_abap_behv=>fc-o-disabled
                           %action-rejectTravel = if_abap_behv=>fc-o-enabled
                           %assoc-_Booking      = if_abap_behv=>fc-o-enabled ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'result' exp = expected act = result ).

  ENDMETHOD.


  METHOD get_features_rejected.

    " Fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = '43' overall_status = 'X' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required table and structures
    DATA result  TYPE TABLE FOR INSTANCE FEATURES RESULT /dmo/i_travel_m\\travel.
    DATA failed TYPE RESPONSE FOR FAILED EARLY /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    " Call method to be tested
    class_under_test->get_features(
      EXPORTING
        keys               = CORRESPONDING #(  travel_mock_data )
        requested_features = VALUE #(  %action-accepttravel = if_abap_behv=>mk-on
                                       %action-rejectTravel = if_abap_behv=>mk-on
                                       %assoc-_Booking      = if_abap_behv=>mk-on )

      CHANGING
        result             = result
        failed             = failed
        reported           = reported
    ).

    " Check result
    DATA expected LIKE result.
    expected  = VALUE #( ( travel_id            = '43'
                           %action-accepttravel = if_abap_behv=>fc-o-enabled
                           %action-rejectTravel = if_abap_behv=>fc-o-disabled
                           %assoc-_Booking      = if_abap_behv=>fc-o-disabled ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'result' exp = expected act = result ).

  ENDMETHOD.



  METHOD get_features_invalidstatus.

    " Fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = '43' overall_status = 'T' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required table and structures
    DATA result  TYPE TABLE FOR INSTANCE FEATURES RESULT /dmo/i_travel_m\\travel.
    DATA failed TYPE RESPONSE FOR FAILED EARLY /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    " Call the method to be tested
    class_under_test->get_features(
      EXPORTING
        keys               = CORRESPONDING #(  travel_mock_data )
        requested_features = VALUE #(  %action-accepttravel = if_abap_behv=>mk-on
                                       %action-rejectTravel = if_abap_behv=>mk-on
                                       %assoc-_Booking      = if_abap_behv=>mk-on )

      CHANGING
        result             = result
        failed             = failed
        reported           = reported
    ).

    " Check result
    DATA expected LIKE result.
    expected  = VALUE #( ( travel_id            = '43'
                           %action-accepttravel = if_abap_behv=>fc-o-enabled
                           %action-rejectTravel = if_abap_behv=>fc-o-enabled
                           %assoc-_Booking      = if_abap_behv=>fc-o-enabled ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'result' exp = expected act = result ).

  ENDMETHOD.



  METHOD get_features_invalidkey.

    " Declare required table and structures
    DATA result  TYPE TABLE FOR INSTANCE FEATURES RESULT /dmo/i_travel_m\\travel.
    DATA failed TYPE RESPONSE FOR FAILED EARLY /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    " Call the method to be tested with invalid/not existing key
    class_under_test->get_features(
      EXPORTING
        keys               = VALUE #( ( travel_id = '43' ) )
        requested_features = VALUE #(  %action-accepttravel = if_abap_behv=>mk-on
                                       %action-rejectTravel = if_abap_behv=>mk-on
                                       %assoc-_Booking      = if_abap_behv=>mk-on )

      CHANGING
        result             = result
        failed             = failed
        reported           = reported
    ).

    " Check number of returned instances in failed-travel
    cl_abap_unit_assert=>assert_equals( msg = 'lines in failed-travel' act = lines( failed-travel ) exp = 1 ).

    " Check travel id in failed-travel
    cl_abap_unit_assert=>assert_equals( msg = 'travel id in failed-travel' act = failed-travel[ 1 ]-travel_id exp = '43' ).

    " Check fail-cause in failed-travel
    cl_abap_unit_assert=>assert_equals( msg = 'fail-cause in failed-travel' act = failed-travel[ 1 ]-%fail-cause exp = if_abap_behv=>cause-not_found ).

  ENDMETHOD.

  METHOD validate_customer_valid.

    " Fill in test data for entity travel
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = '43' customer_id = '1' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Fill in test data for customer table
    DATA customer_mock_data TYPE STANDARD TABLE OF /dmo/customer.
    customer_mock_data = VALUE #(  (  customer_id = '1' ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    " Declare required structures
    DATA failed TYPE RESPONSE FOR FAILED LATE /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    " Call the method to be tested
    class_under_test->validate_customer(
      EXPORTING
        keys               = CORRESPONDING #(  travel_mock_data )

      CHANGING
        failed             = failed
        reported           = reported
    ).

    " Check for content in failed and reported
    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

  ENDMETHOD.

  METHOD validate_customer_invalid.

    " Fill in test data for entity travel
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = '43' customer_id = '1' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required structures
    DATA failed TYPE RESPONSE FOR FAILED LATE /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    " Call the method to be tested
    class_under_test->validate_customer(
      EXPORTING
        keys               = CORRESPONDING #(  travel_mock_data )

      CHANGING
        failed             = failed
        reported           = reported
    ).

    " Check number of returned instances in failed-travel
    cl_abap_unit_assert=>assert_equals( msg = 'lines in failed-travel' act = lines( failed-travel ) exp = 1 ).

    " Check travel id in failed-travel
    cl_abap_unit_assert=>assert_equals( msg = 'travel id in failed-travel' act = failed-travel[ 1 ]-travel_id exp = '43' ).


    " Check number of returned instances in reported-travel
    cl_abap_unit_assert=>assert_equals( msg = 'lines in reported-travel' act = lines( reported-travel ) exp = 1 ).

    " Check travel id in reported-travel
    cl_abap_unit_assert=>assert_equals( msg = 'travel id in reported-travel' act = reported-travel[ 1 ]-travel_id  exp = '43' ).

    " Check marked field in reported-travel
    cl_abap_unit_assert=>assert_equals( msg = 'field customer id in reported-travel' act = reported-travel[ 1 ]-%element-customer_id  exp = if_abap_behv=>mk-on ).

    " Check message reference in reported-travel
    cl_abap_unit_assert=>assert_bound(  msg = 'message reference in reported-travel' act = reported-travel[ 1 ]-%msg ).

  ENDMETHOD.


  METHOD validate_customer_initial.

    " Fill in test data for entity travel
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = '43' customer_id = '' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required structures
    DATA failed TYPE RESPONSE FOR FAILED LATE /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    " Call the method to be tested
    class_under_test->validate_customer(
      EXPORTING
        keys               = CORRESPONDING #(  travel_mock_data )

      CHANGING
        failed             = failed
        reported           = reported
    ).

    " Check number of returned instances in failed-travel
    cl_abap_unit_assert=>assert_equals( msg = 'lines in failed-travel' act = lines( failed-travel ) exp = 1 ).

    " Check travel id in failed-travel
    cl_abap_unit_assert=>assert_equals( msg = 'travel id in failed-travel' act = failed-travel[ 1 ]-travel_id exp = '43' ).



    " Check number of returned instances in reported-travel
    cl_abap_unit_assert=>assert_equals( msg = 'lines in reported-travel' act = lines( reported-travel ) exp = 1 ).

    " Check travel id in reported-travel
    cl_abap_unit_assert=>assert_equals( msg = 'travel id in reported-travel' act = reported-travel[ 1 ]-travel_id  exp = '43' ).

    " Check marked field in reported-travel
    cl_abap_unit_assert=>assert_equals( msg = 'field customer id in reported-travel' act = reported-travel[ 1 ]-%element-customer_id  exp = if_abap_behv=>mk-on ).

    " Check message reference in reported-travel
    cl_abap_unit_assert=>assert_bound(  msg = 'message reference in reported-travel' act = reported-travel[ 1 ]-%msg ).

  ENDMETHOD.



  METHOD set_status_accepted.

    " Fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m.
    travel_mock_data = VALUE #( ( travel_id = '42' overall_status = 'A' )
                                ( travel_id = '43' overall_status = 'B' )
                                ( travel_id = '44' overall_status = 'X' )
                                ( travel_id = '45' overall_status = '' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required table and structures
    DATA result   TYPE TABLE FOR ACTION RESULT /dmo/i_travel_m\\travel~accepttravel.
    DATA mapped   TYPE RESPONSE FOR MAPPED EARLY /dmo/i_travel_m.
    DATA failed   TYPE RESPONSE FOR FAILED EARLY /dmo/i_travel_m.
    DATA reported TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    " Call the method to be tested
    class_under_test->set_status_accepted(
      EXPORTING
        keys     = CORRESPONDING #(  travel_mock_data )

      CHANGING
        result   = result
        mapped   = mapped
        failed   = failed
        reported = reported
    ).

    " Check for content in mapped, failed and reported
    cl_abap_unit_assert=>assert_initial( msg = 'mapped' act = mapped ).
    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

    " Check action result for fields of interest: travel_id, %param-travel_id, %param-overall_status.
    DATA exp LIKE result.
    exp = VALUE #(  ( travel_id = 42 %param-travel_id = '42' %param-overall_status = 'A' )
                    ( travel_id = 43 %param-travel_id = '43' %param-overall_status = 'A' )
                    ( travel_id = 44 %param-travel_id = '44' %param-overall_status = 'A' )
                    ( travel_id = 45 %param-travel_id = '45' %param-overall_status = 'A' ) ).

    DATA act LIKE result.
    act = CORRESPONDING #( result MAPPING travel_id = travel_id
                                       (  %param = %param MAPPING travel_id = travel_id
                                                                  overall_status = overall_status
                                                                  EXCEPT * )
                                          EXCEPT * ).
    cl_abap_unit_assert=>assert_equals( msg = 'action result' exp = exp act = act ).





    " Additionally check modified instances

    READ ENTITY /dmo/i_travel_m
      FIELDS ( travel_id overall_status ) WITH CORRESPONDING #( travel_mock_data )
      RESULT DATA(read_result).

    act = VALUE #( FOR t IN read_result ( travel_id = t-travel_id
                                          %param-travel_id = t-travel_id
                                          %param-overall_status = t-overall_status ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'read result' exp = exp act = act ).

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

    class_under_test->set_status_accepted(
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
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.


ENDCLASS.
