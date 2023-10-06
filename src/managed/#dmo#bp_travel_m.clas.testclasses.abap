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
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
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






"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""




"! @testing BDEF:/DMO/I_Travel_M
CLASS ltc_travl_not_in_documentation DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CONSTANTS:
      travel_id1 TYPE /dmo/travel_id VALUE '1',
      travel_id2 TYPE /dmo/travel_id VALUE '2',
      travel_id3 TYPE /dmo/travel_id VALUE '3',
      travel_id4 TYPE /dmo/travel_id VALUE '4'.

    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_travel,
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
      "! Checks if { @link ..lhc_travel.METH:set_status_accepted } sets the status as expected
      "! and returns the entire instance.
      accepttravel                  FOR TESTING,

      "! Checks if { @link ..lhc_travel.METH:set_status_rejected } sets the status as expected
      "! and returns the entire instance.
      rejecttravel                  FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:reCalcTotalPrice }
      "! and checks the correct sum using one currency.
      recalctotalprice_one_currency FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:reCalcTotalPrice }
      "! and checks the correct sum using two currencies.
      recalctotalprice_mix_currency FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validate_customer }
      "! and checks if an existing customer is set.
      validatecustomer_success      FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validate_customer }
      "! and checks for a message for an initial customer.
      validatecustomer_initial      FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validate_customer }
      "! and checks for a message for a non-existing customer.
      validatecustomer_not_exist    FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validate_agency }
      "! and checks if an existing agency is set.
      validateagency_success        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validate_agency }
      "! and checks for a message for an initial Agency.
      validateagency_initial        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validate_agency }
      "! and checks for a message for a non-existing Agency.
      validateagency_not_exist      FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validate_dates }
      "! and checks if a pair of dates is valid.
      validatedates_success        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validate_dates }
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

      "! Checks if { @link ..lhc_Travel.METH:earlynumbering_create } fulfills idempotency
      "! by passing an instance where the number is already drawn.
      earlynumbering_idempotency    FOR TESTING,

      "! Checks if { @link ..lhc_Travel.METH:earlynumbering_create }
      "! draws a new number for instances where no number is drawn yet.
      earlynumbering_new_number     FOR TESTING,

      "! Checks if { @link ..lhc_Travel.METH:earlynumbering_cba_booking }
      "! draws a new number for instances where no number is drawn yet.
      earlynumbering_cba_new_number     FOR TESTING,

      "! Checks if { @link ..lhc_Travel.METH:copytravel }
      "! copies a travel and reads the instance.
      copytravel                    FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validate_travel_status }
      "! and checks if a pair of status is valid.
      validatestatus_success        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validate_travel_status }
      "! and checks if invalid permutations of sets of status
      "! returns messages.
      validatestatus_not_valid      FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validate_currencycode }
      "! and checks if a pair of status is valid.
      validate_currency_success        FOR TESTING,

      "! Calls { @link ..lhc_travel.METH:validate_currencycode }
      "! and checks if invalid permutations of sets of status
      "! returns messages.
      validate_currency_not_valid      FOR TESTING.

ENDCLASS.


CLASS ltc_travl_not_in_documentation IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
                               VALUE #(
                                   ( i_for_entity = '/DMO/I_Travel_M'    )
                                   ( i_for_entity = '/DMO/I_Booking_M'   )
                                   ( i_for_entity = '/DMO/I_BookSuppl_M' )
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


  METHOD accepttravel.
    DATA:
      travel_mock_data   TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_test    TYPE STANDARD TABLE OF /dmo/i_travel_m WITH KEY travel_id,
      exp_travels_action TYPE TABLE FOR ACTION RESULT /dmo/i_travel_m\\travel~accepttravel,
      exp_travel_read    TYPE STRUCTURE FOR READ RESULT /dmo/i_travel_m\\travel,
      result             TYPE TABLE FOR ACTION RESULT  /dmo/i_travel_m\\travel~accepttravel,
      mapped             TYPE RESPONSE FOR MAPPED EARLY  /dmo/i_travel_m,
      failed             TYPE RESPONSE FOR FAILED EARLY  /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    travel_mock_data = VALUE #( ( travel_id = travel_id1  overall_status = 'O' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TO ENTITY ).

    exp_travels_action = VALUE #(
        (
          travel_id     = travel_id1
          %param        = VALUE #(
              travel_id      = travel_id1
              overall_status = 'A'
            )
        )
      ).

    class_under_test->set_status_accepted(
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


    READ ENTITIES OF /dmo/i_travel_m
      ENTITY travel
        FIELDS ( travel_id ) WITH CORRESPONDING #( travels_to_test )
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
      travel_mock_data   TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_test    TYPE STANDARD TABLE OF /dmo/i_travel_m WITH KEY travel_id,
      exp_travels_action TYPE TABLE FOR ACTION RESULT /dmo/i_travel_m\\travel~rejecttravel,
      exp_travel_read    TYPE STRUCTURE FOR READ RESULT /dmo/i_travel_m\\travel,
      result             TYPE TABLE FOR ACTION RESULT  /dmo/i_travel_m\\travel~rejecttravel,
      mapped             TYPE RESPONSE FOR MAPPED EARLY  /dmo/i_travel_m,
      failed             TYPE RESPONSE FOR FAILED EARLY  /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    travel_mock_data = VALUE #( ( travel_id = travel_id1  overall_status = 'O' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING TO ENTITY ).

    exp_travels_action = VALUE #(
        (
          travel_id    = travel_id1
          %param        = VALUE #(
              travel_id     = travel_id1
              overall_status = 'X'
            )
        )
      ).

    class_under_test->set_status_rejected(
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


    READ ENTITIES OF /dmo/i_travel_m
      ENTITY travel
        FIELDS ( travel_id ) WITH CORRESPONDING #( travels_to_test )
        RESULT DATA(read_result).

    cl_abap_unit_assert=>assert_equals( exp = 1  act = lines( read_result ) ).

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
      booking_id1           TYPE /dmo/booking_m-booking_id VALUE '10',
      booking_id2           TYPE /dmo/booking_m-booking_id VALUE '20',
      bookingsupplement_id1 TYPE /dmo/booksuppl_m-booking_supplement_id VALUE '1',
      bookingsupplement_id2 TYPE /dmo/booksuppl_m-booking_supplement_id VALUE '2',
      bookingsupplement_id3 TYPE /dmo/booksuppl_m-booking_supplement_id VALUE '3',
      bookingsupplement_id4 TYPE /dmo/booksuppl_m-booking_supplement_id VALUE '4'.

    DATA:
      travel_mock_data            TYPE STANDARD TABLE OF /dmo/travel_m,
      booking_mock_data           TYPE STANDARD TABLE OF /dmo/booking_m,
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/booksuppl_m,
      travels_to_test             TYPE TABLE FOR ACTION IMPORT /dmo/i_travel_m\\travel~recalctotalprice,
      exp_travel_read             TYPE TABLE FOR READ RESULT /dmo/i_travel_m\\travel,
      mapped                      TYPE RESPONSE FOR MAPPED EARLY  /dmo/i_travel_m,
      failed                      TYPE RESPONSE FOR FAILED EARLY  /dmo/i_travel_m,
      reported                    TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    travels_to_test = VALUE #( ( travel_id = travel_id1 ) ).

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  booking_fee = c_booking_fee  currency_code = c_currency )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    booking_mock_data = VALUE #(
        flight_price  = c_flight_price
        currency_code = c_currency
        ( travel_id = travel_id1  booking_id = booking_id1 )
        ( travel_id = travel_id1  booking_id = booking_id2 )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookingsupplement_mock_data = VALUE #(
        price         = c_supplement_price
        currency_code = c_currency
        ( travel_id = travel_id1 booking_id = booking_id1  booking_supplement_id = bookingsupplement_id1 )
        ( travel_id = travel_id1 booking_id = booking_id1  booking_supplement_id = bookingsupplement_id2 )
        ( travel_id = travel_id1 booking_id = booking_id2  booking_supplement_id = bookingsupplement_id3 )
        ( travel_id = travel_id1 booking_id = booking_id2  booking_supplement_id = bookingsupplement_id4 )
      ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).


    class_under_test->recalctotalprice(
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


    READ ENTITIES OF /dmo/i_travel_m
      ENTITY travel
        FIELDS ( travel_id total_price booking_fee currency_code )
        WITH CORRESPONDING #( travels_to_test )
        RESULT DATA(read_result).

    exp_travel_read = VALUE #( (
        travel_id     = travel_id1
        booking_fee   = c_booking_fee
        total_price   = c_booking_fee
                         + c_flight_price * lines( booking_mock_data )
                         + c_supplement_price * lines( bookingsupplement_mock_data )
        currency_code = c_currency
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
      booking_id1           TYPE /dmo/booking_m-booking_id VALUE '10',
      booking_id2           TYPE /dmo/booking_m-booking_id VALUE '20',
      bookingsupplement_id1 TYPE /dmo/booksuppl_m-booking_supplement_id VALUE '1',
      bookingsupplement_id2 TYPE /dmo/booksuppl_m-booking_supplement_id VALUE '2',
      bookingsupplement_id3 TYPE /dmo/booksuppl_m-booking_supplement_id VALUE '3',
      bookingsupplement_id4 TYPE /dmo/booksuppl_m-booking_supplement_id VALUE '4'.

    DATA:
      travel_mock_data            TYPE STANDARD TABLE OF /dmo/travel_m,
      booking_mock_data           TYPE STANDARD TABLE OF /dmo/booking_m,
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/booksuppl_m,
      travels_to_test             TYPE TABLE FOR ACTION IMPORT /dmo/i_travel_m\\travel~recalctotalprice,
      exp_travel_read             TYPE TABLE FOR READ RESULT /dmo/i_travel_m\\travel,
      mapped                      TYPE RESPONSE FOR MAPPED EARLY  /dmo/i_travel_m,
      failed                      TYPE RESPONSE FOR FAILED EARLY  /dmo/i_travel_m,
      reported                    TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    travels_to_test = VALUE #( ( travel_id = travel_id1 ) ).

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  booking_fee = c_booking_fee  currency_code = c_currency_eur )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    booking_mock_data = VALUE #(
        flight_price  = c_flight_price
        currency_code = c_currency_eur
        ( travel_id = travel_id1  booking_id = booking_id1 )
        ( travel_id = travel_id1  booking_id = booking_id2 )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookingsupplement_mock_data = VALUE #(
        price         = c_supplement_price
        currency_code = c_currency_eur
        ( travel_id = travel_id1 booking_id = booking_id1  booking_supplement_id = bookingsupplement_id1 )
        ( travel_id = travel_id1 booking_id = booking_id1  booking_supplement_id = bookingsupplement_id2 )
        ( travel_id = travel_id1 booking_id = booking_id2  booking_supplement_id = bookingsupplement_id3 )

        currency_code = c_currency_usd
        ( travel_id = travel_id1 booking_id = booking_id2  booking_supplement_id = bookingsupplement_id4 )
      ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).


    class_under_test->recalctotalprice(
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


    READ ENTITIES OF /dmo/i_travel_m
      ENTITY travel
        FIELDS ( travel_id total_price booking_fee currency_code )
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
        travel_id     = travel_id1
        booking_fee   = c_booking_fee
        total_price   = c_booking_fee
                         + c_flight_price * lines( booking_mock_data )
                         + c_supplement_price * ( lines( bookingsupplement_mock_data ) - 1 )
                         + converted_supplement_price
        currency_code = c_currency_eur
      ) ).

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
      travel_mock_data   TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_test    TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\travel~validatecustomer,
      failed             TYPE RESPONSE FOR FAILED LATE  /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  customer_id = c_customer_id )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING travel_id = travel_id ).

    class_under_test->validate_customer(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD validatecustomer_initial.
    CONSTANTS:
      c_customer_id           TYPE /dmo/customer_id VALUE '123',
      c_customer_id_of_travel TYPE /dmo/customer_id VALUE '111'.

    DATA:
      customer_mock_data TYPE STANDARD TABLE OF /dmo/customer,
      travel_mock_data   TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_test    TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\travel~validatecustomer,
      failed             TYPE RESPONSE FOR FAILED LATE  /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  customer_id = c_customer_id_of_travel )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING travel_id = travel_id ).

    class_under_test->validate_customer(
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
        exp = 1
        act = lines( reported-travel )
      ).
  ENDMETHOD.

  METHOD validatecustomer_not_exist.
    CONSTANTS:
      c_customer_id           TYPE /dmo/customer_id VALUE '123',
      c_customer_id_of_travel TYPE /dmo/customer_id VALUE IS INITIAL.

    DATA:
      customer_mock_data TYPE STANDARD TABLE OF /dmo/customer,
      travel_mock_data   TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_test    TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\travel~validatecustomer,
      failed             TYPE RESPONSE FOR FAILED LATE  /dmo/i_travel_m,
      reported           TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  customer_id = c_customer_id_of_travel )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING travel_id = travel_id ).

    class_under_test->validate_customer(
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
        exp = 1
        act = lines( reported-travel )
      ).
  ENDMETHOD.

  METHOD validateagency_success.
    CONSTANTS:
      c_agency_id TYPE /dmo/agency_id VALUE '123'.

    DATA:
      agency_mock_data TYPE STANDARD TABLE OF /dmo/agency,
      travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\travel~validateagency,
      failed           TYPE RESPONSE FOR FAILED LATE  /dmo/i_travel_m,
      reported         TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    agency_mock_data = VALUE #( ( agency_id = c_agency_id ) ).
    sql_test_environment->insert_test_data( agency_mock_data ).

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  agency_id = c_agency_id )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING travel_id = travel_id ).

    class_under_test->validate_agency(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD validateagency_initial.
    CONSTANTS:
      c_agency_id           TYPE /dmo/agency_id VALUE '123',
      c_agency_id_of_travel TYPE /dmo/agency_id VALUE '111'.

    DATA:
      agency_mock_data TYPE STANDARD TABLE OF /dmo/agency,
      travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\travel~validateagency,
      failed           TYPE RESPONSE FOR FAILED LATE  /dmo/i_travel_m,
      reported         TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    agency_mock_data = VALUE #( ( agency_id = c_agency_id ) ).
    sql_test_environment->insert_test_data( agency_mock_data ).

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  agency_id = c_agency_id_of_travel )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING travel_id = travel_id ).

    class_under_test->validate_agency(
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
        exp = 1
        act = lines( reported-travel )
      ).
  ENDMETHOD.

  METHOD validateagency_not_exist.
    CONSTANTS:
      c_agency_id           TYPE /dmo/agency_id VALUE '123',
      c_agency_id_of_travel TYPE /dmo/agency_id VALUE IS INITIAL.

    DATA:
      agency_mock_data TYPE STANDARD TABLE OF /dmo/agency,
      travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\travel~validateagency,
      failed           TYPE RESPONSE FOR FAILED LATE  /dmo/i_travel_m,
      reported         TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    agency_mock_data = VALUE #( ( agency_id = c_agency_id ) ).
    sql_test_environment->insert_test_data( agency_mock_data ).

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  agency_id = c_agency_id_of_travel )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING travel_id = travel_id ).

    class_under_test->validate_agency(
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
        exp = 1
        act = lines( reported-travel )
      ).
  ENDMETHOD.

  METHOD validatedates_success.
    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\travel~validatedates,
      failed           TYPE RESPONSE FOR FAILED LATE  /dmo/i_travel_m,
      reported         TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m,
      today            TYPE cl_abap_context_info=>ty_system_date,
      tomorrow         TYPE cl_abap_context_info=>ty_system_date.

    today    = cl_abap_context_info=>get_system_date( ).
    tomorrow = cl_abap_context_info=>get_system_date( ) + 1.

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  begin_date = today  end_date = tomorrow )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING travel_id = travel_id ).

    class_under_test->validate_dates(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD validatedates_not_valid.
    TYPES: BEGIN OF t_check.
             INCLUDE TYPE /dmo/travel_m.
    TYPES:   exp_amount_reported_entries TYPE i,
             exp_amount_failed_entries   TYPE i,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY travel_id.

    CONSTANTS:
      travel_id5 TYPE /dmo/travel_id VALUE '5',
      travel_id6 TYPE /dmo/travel_id VALUE '6'.

    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_check TYPE t_check_table,
      travel_to_check  TYPE t_check,
      travels_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\travel~validatedates,
      failed           TYPE RESPONSE FOR FAILED LATE  /dmo/i_travel_m,
      reported         TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m,
      one_week_ago     TYPE cl_abap_context_info=>ty_system_date,
      yesterday        TYPE cl_abap_context_info=>ty_system_date,
      today            TYPE cl_abap_context_info=>ty_system_date,
      tomorrow         TYPE cl_abap_context_info=>ty_system_date.


    one_week_ago = cl_abap_context_info=>get_system_date( ) - 7.
    yesterday    = cl_abap_context_info=>get_system_date( ) - 1.
    today        = cl_abap_context_info=>get_system_date( ).
    tomorrow     = cl_abap_context_info=>get_system_date( ) + 1.

    travels_to_check = VALUE #(
        exp_amount_reported_entries = '1'
        exp_amount_failed_entries   = '1'
        ( travel_id = travel_id1                             end_date = today         description = 'Begin initial'       )
        ( travel_id = travel_id2  begin_date = today                                  description = 'End initial'         )
        ( travel_id = travel_id3  begin_date = tomorrow      end_date = today         description = 'Begin > End'         )
        ( travel_id = travel_id4  begin_date = yesterday     end_date = today         description = 'Begin < today'       )
        ( travel_id = travel_id5  begin_date = yesterday     end_date = one_week_ago  description = 'End < Begin < today' )
        ( travel_id = travel_id6  begin_date = one_week_ago  end_date = yesterday     description = 'Begin < End < today' )
      ).
    travel_mock_data = CORRESPONDING #( travels_to_check ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING travel_id = travel_id ).

    class_under_test->validate_dates(
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
            act  = lines( FILTER #( failed-travel USING KEY entity WHERE travel_id = travel_to_check-travel_id ) )
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
            act  = lines( FILTER #( reported-travel USING KEY entity WHERE travel_id = travel_to_check-travel_id ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ travel_to_check-exp_amount_reported_entries } line(s) in reported expected for '{ travel_to_check-description }'|
          ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_instance_features.
    TYPES: t_instance_feature TYPE STRUCTURE FOR INSTANCE FEATURES RESULT /dmo/i_travel_m\\travel,
           BEGIN OF t_check.
             INCLUDE TYPE t_instance_feature.
    TYPES: overall_status TYPE /dmo/i_travel_m-overall_status,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY travel_id.

    DATA:
      check_table        TYPE t_check_table,
      travel_mock_data   TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_test    TYPE TABLE FOR INSTANCE FEATURES KEY /dmo/i_travel_m\\travel,
      requested_features TYPE STRUCTURE FOR INSTANCE FEATURES REQUEST /dmo/i_travel_m\\travel,
      act_result         TYPE TABLE FOR INSTANCE FEATURES RESULT /dmo/i_travel_m\\travel,
      exp_result         TYPE TABLE FOR INSTANCE FEATURES RESULT /dmo/i_travel_m\\travel,
      reported           TYPE RESPONSE FOR REPORTED EARLY /dmo/i_travel_m,
      failed             TYPE RESPONSE FOR FAILED   EARLY /dmo/i_travel_m.

    " In current implementation requested_features is not used.
    requested_features = VALUE #( ).

    check_table = VALUE t_check_table(
        (
          travel_id              = travel_id1
          overall_status         = 'A'
          %action-accepttravel   = if_abap_behv=>fc-o-disabled
          %action-rejecttravel   = if_abap_behv=>fc-o-enabled
          %assoc-_booking        = if_abap_behv=>fc-o-enabled
        )
        (
          travel_id              = travel_id2
          overall_status         = 'X'
          %action-accepttravel   = if_abap_behv=>fc-o-enabled
          %action-rejecttravel   = if_abap_behv=>fc-o-disabled
          %assoc-_booking        = if_abap_behv=>fc-o-disabled
        )
        (
          travel_id              = travel_id3
          overall_status         = 'O'
          %action-accepttravel   = if_abap_behv=>fc-o-enabled
          %action-rejecttravel   = if_abap_behv=>fc-o-enabled
          %assoc-_booking        = if_abap_behv=>fc-o-enabled
        )
        (
          travel_id              = travel_id4
          overall_status         = 'T'
          %action-accepttravel   = if_abap_behv=>fc-o-enabled
          %action-rejecttravel   = if_abap_behv=>fc-o-enabled
          %assoc-_booking        = if_abap_behv=>fc-o-enabled
        )
      ).

    travel_mock_data = CORRESPONDING #(
                          check_table
                          MAPPING
                            travel_id    = travel_id
                            overall_status = overall_status
                          EXCEPT *
                        ).
    cds_test_environment->insert_test_data( travel_mock_data ).

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

  METHOD get_global_authorizations.
    DATA:
      requested_authorizations TYPE STRUCTURE FOR GLOBAL AUTHORIZATION REQUEST /dmo/i_travel_m\\travel,
      exp_result               TYPE STRUCTURE FOR GLOBAL AUTHORIZATION RESULT /dmo/i_travel_m\\travel,
      act_result               TYPE STRUCTURE FOR GLOBAL AUTHORIZATION RESULT /dmo/i_travel_m\\travel,
      reported                 TYPE RESPONSE FOR REPORTED EARLY /dmo/i_travel_m.

    requested_authorizations = VALUE #(
         %create      = if_abap_behv=>mk-on
         %update      = if_abap_behv=>mk-on
         %delete      = if_abap_behv=>mk-on
      ).

    exp_result = VALUE #(
         %create      = if_abap_behv=>auth-allowed
         %update      = if_abap_behv=>auth-allowed
         %delete      = if_abap_behv=>auth-allowed
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

  METHOD earlynumbering_idempotency.
    DATA:
      entity          TYPE STRUCTURE FOR CREATE /dmo/i_travel_m\\travel,
      mapped          TYPE RESPONSE FOR MAPPED /dmo/i_travel_m,
      failed          TYPE RESPONSE FOR FAILED EARLY /dmo/i_travel_m,
      reported        TYPE RESPONSE FOR REPORTED EARLY /dmo/i_travel_m,
      act_mapped_line LIKE LINE OF mapped-travel,
      exp_mapped_line LIKE LINE OF mapped-travel.

    entity = VALUE #( %cid = 'Test'  travel_id = 'XX123' ).

    class_under_test->earlynumbering_create(
        EXPORTING
          entities = VALUE #( ( entity ) )
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     failed   ).
    cl_abap_unit_assert=>assert_initial(     reported ).
    cl_abap_unit_assert=>assert_not_initial( mapped   ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( mapped-travel ) ).

    act_mapped_line = mapped-travel[ 1 ].
    exp_mapped_line = CORRESPONDING #( entity ).

    cl_abap_unit_assert=>assert_equals( exp = exp_mapped_line  act = act_mapped_line ).
  ENDMETHOD.

  METHOD earlynumbering_new_number.
    TRY.
        cl_numberrange_intervals=>read(
          EXPORTING
                    nr_range_nr1       = '01'
                    object            = '/DMO/TRV_M'
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

    DATA:
      entity          TYPE STRUCTURE FOR CREATE /dmo/i_travel_m\\travel,
      mapped          TYPE RESPONSE FOR MAPPED /dmo/i_travel_m,
      failed          TYPE RESPONSE FOR FAILED EARLY /dmo/i_travel_m,
      reported        TYPE RESPONSE FOR REPORTED EARLY /dmo/i_travel_m,
      act_mapped_line LIKE LINE OF mapped-travel,
      exp_mapped_line LIKE LINE OF mapped-travel.

    entity = VALUE #( %cid = 'Test' ).

    class_under_test->earlynumbering_create(
        EXPORTING
          entities = VALUE #( ( entity ) )
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     failed   ).
    cl_abap_unit_assert=>assert_initial(     reported ).
    cl_abap_unit_assert=>assert_not_initial( mapped   ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( mapped-travel ) ).

    act_mapped_line = mapped-travel[ 1 ].
    cl_abap_unit_assert=>assert_not_initial( act_mapped_line-travel_id ).

    exp_mapped_line = CORRESPONDING #( entity ).
    exp_mapped_line-travel_id = act_mapped_line-travel_id.

    cl_abap_unit_assert=>assert_equals( exp = exp_mapped_line  act = act_mapped_line ).
  ENDMETHOD.

  METHOD copytravel.
    CONSTANTS:
      booking_id1           TYPE /dmo/booking_m-booking_id VALUE '10',
      booking_id2           TYPE /dmo/booking_m-booking_id VALUE '20',
      bookingsupplement_id1 TYPE /dmo/booksuppl_m-booking_supplement_id VALUE '1',
      bookingsupplement_id2 TYPE /dmo/booksuppl_m-booking_supplement_id VALUE '2',
      bookingsupplement_id3 TYPE /dmo/booksuppl_m-booking_supplement_id VALUE '3',
      bookingsupplement_id4 TYPE /dmo/booksuppl_m-booking_supplement_id VALUE '4'.

    DATA:
      travel_mock_data            TYPE STANDARD TABLE OF /dmo/travel_m,
      booking_mock_data           TYPE STANDARD TABLE OF /dmo/booking_m,
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/booksuppl_m,
      travels_to_test             TYPE TABLE FOR ACTION IMPORT /dmo/i_travel_m\\travel~copytravel,
      exp_travel_read             TYPE TABLE FOR READ RESULT /dmo/i_travel_m\\travel,
      mapped                      TYPE RESPONSE FOR MAPPED EARLY  /dmo/i_travel_m,
      failed                      TYPE RESPONSE FOR FAILED EARLY  /dmo/i_travel_m,
      reported                    TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m.

    travels_to_test = VALUE #( ( %cid = 'Test'  travel_id = travel_id1 ) ).

    travel_mock_data = VALUE #(
        (
          travel_id      = travel_id1
          booking_fee    = '100'
          currency_code  = 'EUR'
          begin_date     = '20220101'
          end_date       = '20220201'
          agency_id      = '42'
          overall_status = 'A'
          customer_id    = '123'
          description    = 'Test'
          total_price    = '508'
        )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    booking_mock_data = VALUE #(
        flight_price   = '200'
        currency_code  = 'EUR'
        booking_date   = '20220115'
        booking_status = 'A'
        carrier_id     = 'XX'
        connection_id  = '12'
        flight_date    = '20220120'
        ( travel_id   = travel_id1  booking_id = booking_id1 )
        ( travel_id   = travel_id1  booking_id = booking_id2 )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookingsupplement_mock_data = VALUE #(
        price         = '2'
        currency_code = 'EUR'
        supplement_id = 'XX123'
        ( travel_id = travel_id1 booking_id = booking_id1  booking_supplement_id = bookingsupplement_id1 )
        ( travel_id = travel_id1 booking_id = booking_id1  booking_supplement_id = bookingsupplement_id2 )
        ( travel_id = travel_id1 booking_id = booking_id2  booking_supplement_id = bookingsupplement_id3 )
        ( travel_id = travel_id1 booking_id = booking_id2  booking_supplement_id = bookingsupplement_id4 )
      ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).


    class_under_test->copytravel(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( mapped-travel ).
    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( mapped-travel )
      ).

    READ ENTITIES OF /dmo/i_travel_m
      ENTITY travel
        ALL FIELDS WITH CORRESPONDING #( mapped-travel )
        RESULT DATA(new_travels).

    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( new_travels )
      ).

    "Travel
    DATA:
      act_travel LIKE LINE OF travel_mock_data,
      exp_travel LIKE LINE OF travel_mock_data.

    cl_abap_unit_assert=>assert_not_initial( new_travels[ 1 ]-travel_id ).
    act_travel = CORRESPONDING #( new_travels[ 1 ] EXCEPT travel_id ).
    exp_travel = CORRESPONDING #( travel_mock_data[ 1 ] EXCEPT travel_id ).
    exp_travel-begin_date     = cl_abap_context_info=>get_system_date( ).
    exp_travel-end_date       = cl_abap_context_info=>get_system_date( ) + 30.
    exp_travel-overall_status = 'O'.

    cl_abap_unit_assert=>assert_equals(
        exp = exp_travel
        act = act_travel
      ).

    "Booking
    READ ENTITIES OF /dmo/i_travel_m
      ENTITY travel BY \_booking
        ALL FIELDS WITH CORRESPONDING #( mapped-travel )
        RESULT DATA(new_bookings).

    cl_abap_unit_assert=>assert_equals(
        exp = lines( booking_mock_data )
        act = lines( new_bookings )
      ).

    DATA:
      act_booking LIKE LINE OF booking_mock_data,
      exp_booking LIKE LINE OF booking_mock_data.

    LOOP AT new_bookings INTO DATA(new_booking).
      CLEAR: act_booking, exp_booking.
      cl_abap_unit_assert=>assert_not_initial( new_booking-travel_id ).
      act_booking = CORRESPONDING #( new_booking EXCEPT travel_id ).
      exp_booking = CORRESPONDING #( booking_mock_data[ booking_id = new_booking-booking_id ] EXCEPT travel_id ).
      exp_booking-booking_status = 'N'.

      cl_abap_unit_assert=>assert_equals(
          exp = exp_booking
          act = act_booking
        ).
    ENDLOOP.

    "Booking Supplement
    READ ENTITIES OF /dmo/i_travel_m
      ENTITY booking BY \_booksupplement
        ALL FIELDS WITH CORRESPONDING #( new_bookings )
        RESULT DATA(new_bookingsupplements).

    cl_abap_unit_assert=>assert_equals(
        exp = lines( bookingsupplement_mock_data )
        act = lines( new_bookingsupplements )
      ).

    DATA:
      act_bookingsupplement LIKE LINE OF bookingsupplement_mock_data,
      exp_bookingsupplement LIKE LINE OF bookingsupplement_mock_data.

    LOOP AT new_bookingsupplements INTO DATA(new_bookingsupplement).
      CLEAR: act_bookingsupplement, exp_bookingsupplement.
      cl_abap_unit_assert=>assert_not_initial( new_bookingsupplement-travel_id ).
      act_bookingsupplement = CORRESPONDING #( new_bookingsupplement EXCEPT travel_id ).
      exp_bookingsupplement = CORRESPONDING #( bookingsupplement_mock_data[
                                                   booking_id            = new_bookingsupplement-booking_id
                                                   booking_supplement_id = new_bookingsupplement-booking_supplement_id
                                                 ]
                                                 EXCEPT travel_id ).

      cl_abap_unit_assert=>assert_equals(
          exp = exp_bookingsupplement
          act = act_bookingsupplement
        ).
    ENDLOOP.

  ENDMETHOD.

  METHOD validatestatus_success.
    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\travel~validatestatus,
      failed           TYPE RESPONSE FOR FAILED LATE  /dmo/i_travel_m,
      reported         TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  overall_status = 'O' )
        ( travel_id = travel_id2  overall_status = 'X' )
        ( travel_id = travel_id3  overall_status = 'A' )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data ).

    class_under_test->validate_travel_status(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).
    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( failed   ).
  ENDMETHOD.

  METHOD validatestatus_not_valid.
    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\travel~validatestatus,
      failed           TYPE RESPONSE FOR FAILED LATE  /dmo/i_travel_m,
      reported         TYPE RESPONSE FOR REPORTED LATE  /dmo/i_travel_m.

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  overall_status = 'K' )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data ).

    class_under_test->validate_travel_status(
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
        exp = 1
        act = lines( reported-travel )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = travel_id1
        act = reported-travel[ 1 ]-travel_id
      ).

    TYPES: reported_travel LIKE LINE OF reported-travel.
    DATA: exp_reported_element TYPE reported_travel-%element.
    exp_reported_element-overall_status = if_abap_behv=>mk-on.
    cl_abap_unit_assert=>assert_equals(
        exp = exp_reported_element
        act = reported-travel[ 1 ]-%element
      ).
  ENDMETHOD.

  METHOD earlynumbering_cba_new_number.
    DATA:
      travel_mock_data  TYPE STANDARD TABLE OF /dmo/travel_m,
      booking_mock_data TYPE STANDARD TABLE OF /dmo/booking_m,
      exp_travel_read   TYPE TABLE FOR READ RESULT /dmo/i_travel_m\\travel,
      mapped            TYPE RESPONSE FOR MAPPED EARLY  /dmo/i_travel_m,
      failed            TYPE RESPONSE FOR FAILED EARLY  /dmo/i_travel_m,
      reported          TYPE RESPONSE FOR REPORTED EARLY  /dmo/i_travel_m,
      entities          TYPE TABLE FOR CREATE /dmo/i_travel_m\\travel\_booking.

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1 ) "no existing bookings and no number provided
        ( travel_id = travel_id2 ) "no existing bookings but some numbers provided
        ( travel_id = travel_id3 ) "existing bookings
        ( travel_id = travel_id4 ) "existing bookings and some numbers provided
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    booking_mock_data = VALUE #(
        ( travel_id   = travel_id3  booking_id = '10' )
        ( travel_id   = travel_id4  booking_id = '20' )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    entities = VALUE #(
        (
          travel_id = travel_id1  "no existing bookings and no number provided
          %target   = VALUE #(
              ( %cid = '1b1' )
            )
        )
        (
          travel_id = travel_id2  "no existing bookings but some numbers provided
          %target   = VALUE #(
              ( %cid = '2b1' )
              ( %cid = '2b2'  booking_id = '10' )
            )
        )
        (
          travel_id = travel_id3  "existing bookings
          %target   = VALUE #(
              ( %cid = '3b1' )
            )
        )
        (
          travel_id = travel_id4  "existing bookings and some numbers provided
          %target   = VALUE #(
              ( %cid = '4b1' )
              ( %cid = '4b2'  booking_id = '10' )
              ( %cid = '4b3'  booking_id = '30' )
            )
        )
      ).

    class_under_test->earlynumbering_cba_booking(
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
    cl_abap_unit_assert=>assert_initial( mapped-booksuppl ).
    cl_abap_unit_assert=>assert_not_initial( mapped-booking ).

    cl_abap_unit_assert=>assert_equals(
        exp = REDUCE #( INIT i = 0
                        FOR travel IN entities
                        NEXT i += lines( travel-%target ) )
        act = lines( mapped-booking )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/booking_id( '10' )
        act = VALUE /dmo/booking_id( mapped-booking[ %cid = '1b1' ]-booking_id )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/booking_id( '20' )
        act = VALUE /dmo/booking_id( mapped-booking[ %cid = '2b1' ]-booking_id )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/booking_id( '20' )
        act = VALUE /dmo/booking_id( mapped-booking[ %cid = '3b1' ]-booking_id )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/booking_id( '40' )
        act = VALUE /dmo/booking_id( mapped-booking[ %cid = '4b1' ]-booking_id )
      ).
  ENDMETHOD.

  METHOD validate_currency_success.
    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\travel~validatecurrencycode,
      failed           TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported         TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    travel_mock_data = VALUE #(
        ( travel_id = travel_id1  currency_code = 'EUR' )
        ( travel_id = travel_id2  currency_code = 'USD' )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING travel_id = travel_id ).

    class_under_test->validate_currencycode(
        EXPORTING
          keys     = travels_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD validate_currency_not_valid.
    TYPES: BEGIN OF t_check.
             INCLUDE TYPE /dmo/travel_m.
    TYPES:   exp_amount_reported_entries TYPE i,
             exp_amount_failed_entries   TYPE i,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY travel_id.

    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /dmo/travel_m,
      travels_to_check TYPE t_check_table,
      travel_to_check  TYPE t_check,
      travels_to_test  TYPE TABLE FOR VALIDATION /dmo/i_travel_m\\travel~validatecurrencycode,
      failed           TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_m,
      reported         TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    travels_to_check = VALUE #(
        exp_amount_reported_entries = '1'
        exp_amount_failed_entries   = '1'
        ( travel_id = travel_id1  currency_code = ''    )
        ( travel_id = travel_id2  currency_code = 'XXX' )
      ).
    travel_mock_data = CORRESPONDING #( travels_to_check ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    travels_to_test = CORRESPONDING #( travel_mock_data MAPPING travel_id = travel_id ).

    class_under_test->validate_currencycode(
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
            act  = lines( FILTER #( failed-travel USING KEY entity WHERE travel_id = travel_to_check-travel_id ) )
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
            act  = lines( FILTER #( reported-travel USING KEY entity WHERE travel_id = travel_to_check-travel_id ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ travel_to_check-exp_amount_reported_entries } line(s) in reported expected for '{ travel_to_check-description }'|
          ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.






"! @testing BDEF:/DMO/I_Travel_M
CLASS ltc_save_not_in_documentation DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      class_under_test     TYPE REF TO lcl_save,
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
      teardown.

    METHODS:
      "! Checks if { @link ..lcl_save.METH:save_modified }
      "! logs all buffered tables of Travel for create, update
      "! and delete correspondingly.
      log_travels FOR TESTING,

      "! Checks if { @link ..lcl_save.METH:save_modified }
      "! process all buffered tables of Booking Supplement
      "! for create, update and delete and persists them.
      unmanaged_save_booksupplement FOR TESTING.
ENDCLASS.


CLASS ltc_save_not_in_documentation IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    sql_test_environment = cl_osql_test_environment=>create(
                               VALUE #(
                                   ( '/DMO/LOG_TRAVEL' )
                                   ( '/DMO/BOOKSUPPL_M' )
                                 )
                               ).
  ENDMETHOD.

  METHOD setup.
    sql_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
    sql_test_environment->destroy( ).
  ENDMETHOD.


  METHOD log_travels.
    TYPES:
      tt_logs TYPE STANDARD TABLE OF /dmo/log_travel WITH KEY travel_id.

    DATA:
      create   TYPE REQUEST FOR CHANGE /dmo/i_travel_m,
      update   TYPE REQUEST FOR CHANGE /dmo/i_travel_m,
      delete   TYPE REQUEST FOR DELETE /dmo/i_travel_m,
      exp_logs TYPE tt_logs,
      reported TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    create-travel = VALUE #(
        ( description = 'Booking Fee'          travel_id = '11'  booking_fee    = '10'  %control-booking_fee    = if_abap_behv=>mk-on )
        ( description = 'Overall Status'       travel_id = '12'  overall_status = 'A'   %control-overall_status = if_abap_behv=>mk-on )
        ( description = 'Booking Fee + Staus'  travel_id = '13'  booking_fee    = '10'  %control-booking_fee    = if_abap_behv=>mk-on
                                                                 overall_status = '10'  %control-overall_status = if_abap_behv=>mk-on )
        ( description = 'Not logged field'     travel_id = '14'  agency_id      = '42'  %control-agency_id      = if_abap_behv=>mk-on )
      ).

    APPEND LINES OF VALUE tt_logs(
        ( travel_id = '11'  changing_operation = 'CREATE'  changed_field_name = 'booking_fee'     changed_value = create-travel[ KEY id  travel_id = '11' ]-booking_fee    )
        ( travel_id = '12'  changing_operation = 'CREATE'  changed_field_name = 'overall_status'  changed_value = create-travel[ KEY id  travel_id = '12' ]-overall_status )
        ( travel_id = '13'  changing_operation = 'CREATE'  changed_field_name = 'booking_fee'     changed_value = create-travel[ KEY id  travel_id = '13' ]-booking_fee    )
        ( travel_id = '13'  changing_operation = 'CREATE'  changed_field_name = 'overall_status'  changed_value = create-travel[ KEY id  travel_id = '13' ]-overall_status )
      ) TO exp_logs.



    update-travel = VALUE #(
        ( description = 'Customer ID'             travel_id = '21'  customer_id = '10'  %control-customer_id = if_abap_behv=>mk-on )
        ( description = 'Description'             travel_id = '22'                      %control-description = if_abap_behv=>mk-on )
        ( description = 'Customer + Description'  travel_id = '23'  customer_id = '10'  %control-customer_id = if_abap_behv=>mk-on
                                                                                        %control-description = if_abap_behv=>mk-on )
        ( description = 'Not logged field'        travel_id = '24'  agency_id   = '42'  %control-agency_id   = if_abap_behv=>mk-on )
      ).

    APPEND LINES OF VALUE tt_logs(
        ( travel_id = '21'  changing_operation = 'UPDATE'  changed_field_name = 'customer_id'  changed_value = update-travel[ KEY id  travel_id = '21' ]-customer_id )
        ( travel_id = '22'  changing_operation = 'UPDATE'  changed_field_name = 'description'  changed_value = update-travel[ KEY id  travel_id = '22' ]-description )
        ( travel_id = '23'  changing_operation = 'UPDATE'  changed_field_name = 'customer_id'  changed_value = update-travel[ KEY id  travel_id = '23' ]-customer_id )
        ( travel_id = '23'  changing_operation = 'UPDATE'  changed_field_name = 'description'  changed_value = update-travel[ KEY id  travel_id = '23' ]-description )
      ) TO exp_logs.




    APPEND LINES OF VALUE tt_logs(
        ( travel_id = '31'  changing_operation = 'DELETE' )
        ( travel_id = '32'  changing_operation = 'DELETE' )
        ( travel_id = '33'  changing_operation = 'DELETE' )
      ) TO exp_logs.

    delete-travel = VALUE #(
        ( travel_id = '31' )
        ( travel_id = '32' )
        ( travel_id = '33' )
      ).

    class_under_test->save_modified(
        EXPORTING
          create   = create
          update   = update
          delete   = delete
        CHANGING
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( reported ).

    SELECT                                              "#EC CI_NOWHERE
      FROM /dmo/log_travel
      FIELDS *                                "#EC CI_ALL_FIELDS_NEEDED
      ORDER BY travel_id ASCENDING
      INTO TABLE @DATA(act_logs).

    cl_abap_unit_assert=>assert_not_initial( act_logs ).

    LOOP AT act_logs ASSIGNING FIELD-SYMBOL(<act_log>).
      CLEAR <act_log>-client.

      cl_abap_unit_assert=>assert_not_initial( <act_log>-change_id ).
      CLEAR <act_log>-change_id.

      cl_abap_unit_assert=>assert_not_initial( <act_log>-created_at ).
      CLEAR <act_log>-created_at.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals( exp = exp_logs act = act_logs ).
  ENDMETHOD.

  METHOD unmanaged_save_booksupplement.
    TYPES:
      tt_db_booking_supplement TYPE STANDARD TABLE OF /dmo/booksuppl_m WITH KEY travel_id booking_id booking_supplement_id.

    DATA:
      create                    TYPE REQUEST FOR CHANGE /dmo/i_travel_m,
      update                    TYPE REQUEST FOR CHANGE /dmo/i_travel_m,
      delete                    TYPE REQUEST FOR DELETE /dmo/i_travel_m,
      setup_booking_supplements TYPE tt_db_booking_supplement,
      exp_booking_supplements   TYPE tt_db_booking_supplement,
      reported                  TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_m.

    create-booksuppl = VALUE #(
        travel_id       = '1'
        booking_id      = '1'
        supplement_id   = 'XX123'
        price           = '10'
        currency_code   = 'EUR'
        ( booking_supplement_id = '1' )
        ( booking_supplement_id = '2' )
      ).

    APPEND LINES OF CORRESPONDING tt_db_booking_supplement( create-booksuppl MAPPING FROM ENTITY )
      TO exp_booking_supplements.

    update-booksuppl = VALUE #(
        travel_id       = '2'
        booking_id      = '2'
        supplement_id   = 'XX123'
        price           = '10'
        currency_code   = 'EUR'
        %control-travel_id             = if_abap_behv=>mk-on
        %control-booking_id            = if_abap_behv=>mk-on
        %control-booking_supplement_id = if_abap_behv=>mk-on
        %control-supplement_id         = if_abap_behv=>mk-on
        %control-currency_code         = if_abap_behv=>mk-on
        ( booking_supplement_id = '1'  %control-price = if_abap_behv=>mk-on  )
        ( booking_supplement_id = '2'  %control-price = if_abap_behv=>mk-off )
      ).

    APPEND LINES OF CORRESPONDING tt_db_booking_supplement( update-booksuppl MAPPING FROM ENTITY USING CONTROL )
      TO exp_booking_supplements.

    APPEND LINES OF VALUE tt_db_booking_supplement(
        travel_id             = '2'
        booking_id            = '2'
        supplement_id         = 'XX123'
*          price                 = '30'
        currency_code         = 'EUR'
        ( booking_supplement_id = '1' )
        ( booking_supplement_id = '2' )
      ) TO setup_booking_supplements.

    delete-booksuppl = VALUE #(
        travel_id       = '3'
        booking_id      = '3'
        ( booking_supplement_id = '1' )
        ( booking_supplement_id = '2' )
      ).

    APPEND LINES OF CORRESPONDING tt_db_booking_supplement( delete-booksuppl MAPPING FROM ENTITY )
      TO setup_booking_supplements.

    sql_test_environment->insert_test_data( setup_booking_supplements ).

    class_under_test->save_modified(
        EXPORTING
          create   = create
          update   = update
          delete   = delete
        CHANGING
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( reported ).

    SELECT                                              "#EC CI_NOWHERE
      FROM /dmo/booksuppl_m
      FIELDS *                                "#EC CI_ALL_FIELDS_NEEDED
      ORDER BY travel_id ASCENDING, booking_id ASCENDING, booking_supplement_id ASCENDING
      INTO TABLE @DATA(act_booking_supplements).

    cl_abap_unit_assert=>assert_not_initial( act_booking_supplements ).

    LOOP AT act_booking_supplements ASSIGNING FIELD-SYMBOL(<act_booking_supplement>).
      CLEAR <act_booking_supplement>-client.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals( exp = exp_booking_supplements act = act_booking_supplements ).
  ENDMETHOD.

ENDCLASS.
