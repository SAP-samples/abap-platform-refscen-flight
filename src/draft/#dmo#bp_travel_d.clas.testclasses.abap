"! @testing BDEF:/DMO/I_TRAVEL_D
CLASS ltc_draft DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CLASS-DATA: class_under_test     TYPE REF TO lhc_travel,
                cds_test_environment TYPE REF TO if_cds_test_environment.

    CONSTANTS: uuid1 TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A1',
               uuid2 TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A2',
               uuid3 TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A3',
               uuid4 TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A4'.

    CLASS-METHODS:
      "! Instantiate class under test and set up CDS Test Double Framework
      class_setup,

      "! Destroy test environment and test double
      class_teardown.


    METHODS:
      "! Reset test double
      setup,

      "! Reset transactional buffer
      teardown,


      "! Check instance with already existing travel id
      setTravelNumber_idempotence  FOR TESTING,

      "! Check instances with new travel ids
      settravelnumber_newtravelids FOR TESTING,

      "! Check instances with and without travel ids
      settravelnumber_mixed        FOR TESTING.

ENDCLASS.


CLASS ltc_draft IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    cds_test_environment = cl_cds_test_environment=>create( i_for_entity = '/DMO/I_TRAVEL_D' ).
    cds_test_environment->enable_double_redirection(  ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES. "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
    cds_test_environment->destroy( ).
  ENDMETHOD.

  METHOD settravelnumber_idempotence.

    " Fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D.
    travel_mock_data = VALUE #( ( travel_uuid = uuid1 travel_id = '1' ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required structure
    DATA reported TYPE RESPONSE FOR REPORTED LATE  /DMO/I_Travel_D.

    " Call the method to be tested
    class_under_test->setTravelNumber(
       EXPORTING
         keys               = VALUE #( ( TravelUUID = travel_mock_data[ 1 ]-travel_uuid ) )

       CHANGING
         reported           = reported
     ).

    " Check for content in reported
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

    " Read instance data
    READ ENTITY /DMO/I_Travel_D
       FIELDS ( TravelID ) WITH VALUE #( ( TravelUUID = travel_mock_data[ 1 ]-travel_uuid ) )
       RESULT DATA(read_result).

    " Check number of returned instances in read_result
    cl_abap_unit_assert=>assert_equals( msg = 'lines in read result' exp = 1 act = lines( read_result ) ).

    " Check idempotence
    cl_abap_unit_assert=>assert_equals( msg = 'idempotence' exp = '1' act = read_result[ 1 ]-TravelID ).

  ENDMETHOD.



  METHOD settravelnumber_newtravelids.

    " Fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D.
    travel_mock_data = VALUE #( ( travel_uuid = uuid1 )
                                ( travel_uuid = uuid2 ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required structure
    DATA reported TYPE RESPONSE FOR REPORTED LATE  /DMO/I_Travel_D.

    " Call the method to be tested
    class_under_test->setTravelNumber(
       EXPORTING
         keys               = VALUE #( FOR key IN travel_mock_data ( TravelUUID = key-travel_uuid ) )

       CHANGING
         reported           = reported
     ).

    " Check for content in reported
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

    " Read and sort instance data
    READ ENTITY /DMO/I_Travel_D
       FIELDS ( TravelID ) WITH VALUE #( FOR key IN travel_mock_data ( TravelUUID = key-travel_uuid ) )
       RESULT DATA(read_result).

    SORT read_result BY travelid.

    " Check number of returned instances in read_result
    cl_abap_unit_assert=>assert_equals( msg = 'lines in read result' exp = 2 act = lines( read_result ) ).

    " Check new travel ids
    cl_abap_unit_assert=>assert_equals( msg = 'new travel id' exp = '1' act = read_result[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'new travel id' exp = '2' act = read_result[ 2 ]-TravelID ).

  ENDMETHOD.



  METHOD settravelnumber_mixed.

    " Fill in test data
    DATA travel_mock_data TYPE STANDARD TABLE OF /DMO/A_Travel_D.
    travel_mock_data = VALUE #( ( travel_uuid = uuid1 )
                                ( travel_uuid = uuid2 travel_id = '1' )
                                ( travel_uuid = uuid3 ) ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    " Declare required structure
    DATA reported TYPE RESPONSE FOR REPORTED LATE  /DMO/I_Travel_D.

    " Call the method to be tested
    class_under_test->setTravelNumber(
       EXPORTING
         keys               = VALUE #( FOR key IN travel_mock_data ( TravelUUID = key-travel_uuid ) )

       CHANGING
         reported           = reported
     ).

    " Check for content in reported
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

    " Read and sort instance data
    READ ENTITY /DMO/I_Travel_D
       FIELDS ( TravelID ) WITH VALUE #( FOR key IN travel_mock_data ( TravelUUID = key-travel_uuid ) )
       RESULT DATA(read_result).

    SORT read_result BY travelid.

    " Check number of returned instances in read result
    cl_abap_unit_assert=>assert_equals( msg = 'lines in read result' exp = 3 act = lines( read_result ) ).

    " Check travel uuid of first instance
    cl_abap_unit_assert=>assert_equals( msg = 'travel uuid' act = read_result[ 1 ]-TravelUUID exp = uuid2 ).

    " Check idempotence
    cl_abap_unit_assert=>assert_equals( msg = 'idempotence' act = read_result[ 1 ]-TravelID exp = '1' ).

    " Check travel uuid of second instance
    cl_abap_unit_assert=>assert_equals( msg = 'travel uuid' act = read_result[ 2 ]-TravelUUID exp = uuid1 ).

    " Check new travel id
    cl_abap_unit_assert=>assert_equals( msg = 'new travel id' act = read_result[ 2 ]-TravelID exp = '2' ).

    " Check travel uuid of third instance
    cl_abap_unit_assert=>assert_equals( msg = 'travel uuid' act = read_result[ 3 ]-TravelUUID exp = uuid3 ).

    " Check new travel id
    cl_abap_unit_assert=>assert_equals( msg = 'new travel id' act = read_result[ 3 ]-TravelID exp = '3' ).

  ENDMETHOD.



ENDCLASS.
