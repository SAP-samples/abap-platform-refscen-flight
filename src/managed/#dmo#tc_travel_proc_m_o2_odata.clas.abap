"!@testing SRVB:/DMO/UI_TRAVEL_PROC_M_O2
"! End-to-end testing of a complete service
"! Uses CDS and OSQL Test Environment to stub database dependencies
"! Uses Local OData CLient Proxy to execute OData Calls against local service
CLASS /dmo/tc_travel_proc_m_o2_odata DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: mo_client_proxy      TYPE REF TO /iwbep/if_cp_client_proxy,
                cds_test_environment TYPE REF TO if_cds_test_environment,
                sql_test_environment TYPE REF TO if_osql_test_environment,
                begin_date           TYPE /dmo/begin_date,
                end_date             TYPE /dmo/end_date,
                agency_mock_data     TYPE STANDARD TABLE OF /dmo/agency,
                customer_mock_data   TYPE STANDARD TABLE OF /dmo/customer,
                carrier_mock_data    TYPE STANDARD TABLE OF /dmo/carrier,
                flight_mock_data     TYPE STANDARD TABLE OF /dmo/flight,
                supplement_mock_data TYPE STANDARD TABLE OF /dmo/supplement.

    CLASS-METHODS :
      class_setup RAISING cx_static_check,
      class_teardown,

      create_local_client_proxy
        IMPORTING
          service_key         TYPE /iwbep/if_cp_client_proxy=>ty_s_service_key_v2
        RETURNING
          VALUE(client_proxy) TYPE REF TO /iwbep/if_cp_client_proxy .

    METHODS:
      setup,
      teardown,
      create_travel FOR TESTING RAISING cx_static_check.


ENDCLASS.

CLASS /dmo/tc_travel_proc_m_o2_odata  IMPLEMENTATION.

  METHOD class_setup.

    mo_client_proxy = create_local_client_proxy( VALUE #( service_id      = '/DMO/UI_TRAVEL_PROC_M_O2'
                                                          service_version = '0001' )  ).

    " Create the stubs/doubles for the underlying CDS entities
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
                      i_for_entities = VALUE #(
                        ( i_for_entity = '/DMO/C_TRAVEL_PROCESSOR_M'    i_select_base_dependencies = abap_true )
                        ( i_for_entity = '/DMO/C_BOOKING_PROCESSOR_M'   i_select_base_dependencies = abap_true )
                        ( i_for_entity = '/DMO/C_BOOKSUPPL_PROCESSOR_M' i_select_base_dependencies = abap_true ) ) ).

    " Create the stubs/doubles for referenced and additional used tables.
    sql_test_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #(
*        ( '/DMO/AGENCY' )      "already mocked by cds_test_environment above
*        ( '/DMO/CUSTOMER' )    "already mocked by cds_test_environment above
*        ( '/DMO/CARRIER' )     "already mocked by cds_test_environment above
        ( '/DMO/FLIGHT' )
        ( '/DMO/SUPPLEMENT' )

        ( '/DMO/LOG_TRAVEL' )
    ) ).


    " prepare some test data
    begin_date = cl_abap_context_info=>get_system_date( ) + 10.
    end_date = cl_abap_context_info=>get_system_date( ) + 30.

    agency_mock_data     = VALUE #( ( agency_id = '987654' name = 'Agency 987654' ) ).
    customer_mock_data   = VALUE #( ( customer_id = '987653' last_name = 'customer 987653' ) ).
    carrier_mock_data    = VALUE #( ( carrier_id = '123' name = 'carrier 123' ) ).
    flight_mock_data     = VALUE #( ( carrier_id = '123' connection_id = '9876' flight_date = begin_date price = '2000' currency_code = 'EUR' ) ).
    supplement_mock_data = VALUE #( ( supplement_id = '987652' price = 200 currency_code = 'EUR' ) ).

  ENDMETHOD.

  METHOD setup.
    sql_test_environment->clear_doubles(  ).
    cds_test_environment->clear_doubles(  ).
    cds_test_environment->insert_test_data( agency_mock_data     ).
    cds_test_environment->insert_test_data( customer_mock_data   ).
    cds_test_environment->insert_test_data( carrier_mock_data    ).
    sql_test_environment->insert_test_data( flight_mock_data     ).
    sql_test_environment->insert_test_data( supplement_mock_data ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES. "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
    " remove test doubles
    cds_test_environment->destroy(  ).
    sql_test_environment->destroy(  ).
  ENDMETHOD.

  METHOD create_travel.
    " call a simple create operation and check if the data is available via EML and in the database

    " Prepare business data i.e. the travel instance test data
    DATA(ls_business_data) = VALUE /dmo/c_travel_processor_m(
        agencyid = agency_mock_data[ 1 ]-agency_id
        customerid = customer_mock_data[  1 ]-customer_id
        begindate = begin_date
        enddate = end_date
        bookingfee = '10.50'
        currencycode = 'EUR'
        description = 'TestTravel 1'
        OverallStatus = 'O'
       ).

    " Navigate to the resource and create a request for the create operation
    DATA(lo_request) = mo_client_proxy->create_resource_for_entity_set( 'Travel' )->create_request_for_create( ).

    " Set the business data for the created entity
    lo_request->set_business_data( ls_business_data ).

    " Execute the request
    DATA(lo_response) = lo_request->execute( ).

    cl_abap_unit_assert=>assert_not_initial( lo_response ).

    DATA ls_response_data TYPE /dmo/c_travel_processor_m.
    lo_response->get_business_data( IMPORTING es_business_data = ls_response_data ).

    cl_abap_unit_assert=>assert_equals( msg = 'description' exp = ls_business_data-description act = ls_response_data-description ).

    " Read the created travel entity - the number is now calculated by a number range, so we can't predict it
    READ ENTITIES OF /dmo/c_travel_processor_m
      ENTITY travelprocessor
        FIELDS ( description )
          WITH VALUE #( (  travelid = ls_response_data-travelid ) )
        RESULT DATA(lt_read_travel)
        FAILED DATA(failed)
        REPORTED DATA(reported).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'description' exp = ls_business_data-description act = lt_read_travel[ 1 ]-description ).

    " check data also from Database
    SELECT single @abap_true FROM /dmo/i_travel_m INTO @DATA(lv_travel). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_true( msg = 'travel from db' act = lv_travel ).

    " check also log written by "additional save" functionality
    SELECT single @abap_true FROM /dmo/log_travel INTO @DATA(log_travel). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_true( msg = '/DMO/LOG_TRAVEL' act = log_travel ).

  ENDMETHOD.




  METHOD create_local_client_proxy.

    " as long as CL_WEB_ODATA_CLIENT_FACTORY is not available
    " on all platforms, we have to use dynamic calls
    " to avoid syntax errors
    TRY.
        " the Cloud version
        DATA(class1) = 'CL_WEB_ODATA_CLIENT_FACTORY'.
        CALL METHOD (class1)=>create_v2_local_proxy
          EXPORTING
            is_service_key  = service_key
          RECEIVING
            ro_client_proxy = client_proxy.
      CATCH cx_root.  " cx_sy_dyn_call_illegal_class .
    ENDTRY.

    IF client_proxy IS NOT BOUND.
      TRY.
          " the onPrem version
          DATA(class2) = '/IWBEP/CL_CP_CLIENT_PROXY_FACT'.
          CALL METHOD (class2)=>create_v2_local_proxy
            EXPORTING
              is_service_key  = service_key
            RECEIVING
              ro_client_proxy = client_proxy.
        CATCH cx_root.  " cx_sy_dyn_call_illegal_class .
      ENDTRY.
    ENDIF.

    cl_abap_unit_assert=>assert_bound( msg = 'cannot get client proxy factory or service binding not active' act = client_proxy ).

  ENDMETHOD.

ENDCLASS.
