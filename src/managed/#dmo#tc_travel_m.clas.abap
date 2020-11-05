"! @testing BDEF:/DMO/I_TRAVEL_M
"! test entity /DMO/I_TRAVEL_M from the outside
"! by mocking all database artefacts
CLASS /dmo/tc_travel_m DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: cds_test_environment TYPE REF TO if_cds_test_environment,
                sql_test_environment TYPE REF TO if_osql_test_environment,
                begin_date           TYPE /dmo/begin_date,
                end_date             TYPE /dmo/end_date,
                agency_mock_data     TYPE STANDARD TABLE OF /dmo/agency,
                customer_mock_data   TYPE STANDARD TABLE OF /dmo/customer,
                carrier_mock_data    TYPE STANDARD TABLE OF /dmo/carrier,
                flight_mock_data     TYPE STANDARD TABLE OF /dmo/flight,
                supplement_mock_data TYPE STANDARD TABLE OF /dmo/supplement.

    CLASS-METHODS :
      class_setup,
      class_teardown.

    METHODS:
      teardown,
      big_eml FOR TESTING RAISING cx_static_check.

ENDCLASS.



CLASS /dmo/tc_travel_m IMPLEMENTATION.


  METHOD class_setup.

    " Create the stubs/doubles for the underlying CDS entities
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
                      i_for_entities = VALUE #(
                        ( i_for_entity = '/DMO/I_TRAVEL_M' )
                        ( i_for_entity = '/DMO/I_BOOKING_M' )
                        ( i_for_entity = '/DMO/I_BOOKSUPPL_M' ) ) ).
*    cds_test_environment->enable_double_redirection( ).


    begin_date = cl_abap_context_info=>get_system_date( ) + 10.
    end_date = cl_abap_context_info=>get_system_date( ) + 30.

    agency_mock_data     = VALUE #( ( agency_id = '987654' name = 'Agency 987654' ) ).
    customer_mock_data   = VALUE #( ( customer_id = '987653' last_name = 'customer 987653' ) ).
    carrier_mock_data    = VALUE #( ( carrier_id = '123' name = 'carrier 123' ) ).
    flight_mock_data     = VALUE #( ( carrier_id = '123' connection_id = '9876' flight_date = begin_date price = '2000' currency_code = 'EUR' ) ).
    supplement_mock_data = VALUE #( ( supplement_id = '987652' price = 200 currency_code = 'EUR' ) ).

    " stub referenced and additional used tables.
    sql_test_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #(
        ( '/DMO/AGENCY' )
        ( '/DMO/CUSTOMER' )
        ( '/DMO/CARRIER' )
        ( '/DMO/FLIGHT' )
        ( '/DMO/SUPPLEMENT' )

        ( '/DMO/LOG_TRAVEL' )
    ) ).

    sql_test_environment->insert_test_data( agency_mock_data     ).
    sql_test_environment->insert_test_data( customer_mock_data   ).
    sql_test_environment->insert_test_data( carrier_mock_data    ).
    sql_test_environment->insert_test_data( flight_mock_data     ).
    sql_test_environment->insert_test_data( supplement_mock_data ).
  ENDMETHOD.


  METHOD class_teardown.
    " remove test doubles
    cds_test_environment->destroy(  ).
    sql_test_environment->destroy(  ).
  ENDMETHOD.


  METHOD teardown.
    cds_test_environment->clear_doubles(  ).
    ROLLBACK ENTITIES.
  ENDMETHOD.


  METHOD big_eml.
    " create a complete composition and call an action on it.
    MODIFY ENTITIES OF /dmo/i_travel_m
      ENTITY travel
        CREATE SET FIELDS WITH
          VALUE #( (  %cid = 'ROOT1'
                      travel_id = '4711'
                      agency_id = agency_mock_data[ 1 ]-agency_id
                      begin_date = begin_date
                      end_date = end_date
                      description = 'TestTravel 1'
                      booking_fee    = '10.5'
                      currency_code  = 'EUR'
                      overall_status = 'O'
                 ) )
        CREATE BY \_booking SET FIELDS WITH
          VALUE #( ( %cid_ref = 'ROOT1'
                     %target = VALUE #( ( %cid = 'booking1'
                                          booking_id     = '201'
                                          booking_date   = begin_date
                                          customer_id    = customer_mock_data[ 1 ]-customer_id
                                          carrier_id     = flight_mock_data[ 1 ]-carrier_id
                                          connection_id  = flight_mock_data[ 1 ]-connection_id
                                          flight_date    = flight_mock_data[ 1 ]-flight_date
                                          flight_price   = flight_mock_data[ 1 ]-price
                                          currency_code  = flight_mock_data[ 1 ]-currency_code
                                          booking_status = 'N'
                                       ) )
                 ) )
      ENTITY booking
        CREATE BY \_booksupplement SET FIELDS WITH
          VALUE #( ( %cid_ref = 'booking1'
                     %target = VALUE #( ( %cid = 'supplement1'
                                          booking_supplement_id = '01'
                                          supplement_id = supplement_mock_data[ 1 ]-supplement_id
                                          price         = supplement_mock_data[ 1 ]-price
                                          currency_code = supplement_mock_data[ 1 ]-currency_code
                                      ) )
                 ) )

      ENTITY travel
        EXECUTE accepttravel
          FROM VALUE #( ( %cid_ref = 'ROOT1' ) )


      MAPPED   DATA(mapped)
      FAILED   DATA(failed)
      REPORTED DATA(reported).

    " expect no failures and messages
    cl_abap_unit_assert=>assert_initial( msg = 'failed'   act = failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

    " expect a newly created record in mapped tables
    cl_abap_unit_assert=>assert_not_initial( msg = 'mapped-travel' act = mapped-travel ).
    cl_abap_unit_assert=>assert_not_initial( msg = 'mapped-booking' act = mapped-booking ).
    cl_abap_unit_assert=>assert_not_initial( msg = 'mapped-booksuppl' act = mapped-booksuppl ).

    " persist into database (using the mocks)
    COMMIT ENTITIES RESPONSES
      FAILED DATA(commit_failed)
      REPORTED DATA(commit_reported).

    " no failures expected
    cl_abap_unit_assert=>assert_initial( msg = 'commit_failed'   act = commit_failed ).
    cl_abap_unit_assert=>assert_initial( msg = 'commit_reported' act = commit_reported ).

    SELECT * FROM /dmo/i_travel_m INTO TABLE @DATA(lt_travel). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_not_initial( msg = 'travel from db' act = lt_travel ).

    " assert that the action has changed the overall status
    cl_abap_unit_assert=>assert_equals( msg = 'status' exp = 'A' act = lt_travel[ 1 ]-overall_status ).

    " total_price = SUM( flight_price ) + SUM ( supplement price ) + booking_fee
    cl_abap_unit_assert=>assert_equals( msg = 'total price incl. booking_fee' exp = '2210.50' act = lt_travel[ 1 ]-total_price ).

    SELECT * FROM /dmo/log_travel INTO TABLE @DATA(log_travel). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_not_initial( msg = '/DMO/LOG_TRAVEL' act = log_travel ).
  ENDMETHOD.

ENDCLASS.
