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
      create_travel FOR TESTING RAISING cx_static_check,
      create_deep FOR TESTING RAISING cx_static_check.

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
    ROLLBACK ENTITIES.
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
        travelid = '101'
        agencyid = agency_mock_data[ 1 ]-agency_id
        customerid = customer_mock_data[  1 ]-customer_id
        begindate = begin_date
        enddate = end_date
        bookingfee = '10.50'
        currencycode = 'EUR'
        description = 'TestTravel 1'
        travelstatus = 'O'
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

    " Read the created travel entity
    READ ENTITIES OF /dmo/c_travel_processor_m
      ENTITY travelprocessor
        FIELDS ( description )
          WITH VALUE #( (  travelid = ls_business_data-travelid ) )
        RESULT DATA(lt_read_travel)
        FAILED DATA(failed)
        REPORTED DATA(reported).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'description' exp = ls_business_data-description act = lt_read_travel[ 1 ]-description ).

    " check data also from Database
    SELECT * FROM /dmo/i_travel_m INTO TABLE @DATA(lt_travel). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_not_initial( msg = 'travel from db' act = lt_travel ).

    " check also log written by "additional save" functionality
    SELECT * FROM /dmo/log_travel INTO TABLE @DATA(log_travel). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_not_initial( msg = '/DMO/LOG_TRAVEL' act = log_travel ).

  ENDMETHOD.

  METHOD create_deep.
    " call a Deep Create (travel with embedded child data) within a $batch request
    " and check that we get back a deep response and that the data was written into the database


    " Need to define a deep data structure
    TYPES: BEGIN OF ty_booking_and_suppl.
             INCLUDE   TYPE /dmo/c_booking_processor_m.
    TYPES:   to_booksupplement TYPE STANDARD TABLE OF /dmo/c_booksuppl_processor_m WITH EMPTY KEY,
           END OF ty_booking_and_suppl.

    TYPES: BEGIN OF ty_travel_and_bookings.
             INCLUDE    TYPE /dmo/c_travel_processor_m.
    TYPES:   to_booking TYPE STANDARD TABLE OF ty_booking_and_suppl WITH EMPTY KEY,
           END OF ty_travel_and_bookings.

    " fill in request data
    DATA(ls_business_data) = VALUE ty_travel_and_bookings(
               travelid     = '101'
               agencyid     = agency_mock_data[ 1 ]-agency_id
               customerid   = customer_mock_data[  1 ]-customer_id
               begindate    = begin_date
               enddate      = end_date
               bookingfee   = '10.50'
               currencycode = 'EUR'
               description  = 'TestTravel 1'
               travelstatus = 'O'

               to_booking = VALUE #( (
                    bookingid     = 10
                    bookingdate   = begin_date
                    customerid    = customer_mock_data[ 1 ]-customer_id
                    carrierid     = flight_mock_data[ 1 ]-carrier_id
                    connectionid  = flight_mock_data[ 1 ]-connection_id
                    flightdate    = flight_mock_data[ 1 ]-flight_date
                    flightprice   = flight_mock_data[ 1 ]-price
                    currencycode  = flight_mock_data[ 1 ]-currency_code
                    bookingstatus = 'N'

                    to_booksupplement = VALUE #(  (
                          bookingsupplementid = '01'
                          supplementid = supplement_mock_data[ 1 ]-supplement_id
                          price        = supplement_mock_data[ 1 ]-price
                          currencycode = supplement_mock_data[ 1 ]-currency_code
                    ) )
               ) )
    ).

    " setup a create request
    DATA(create_travel_request) = mo_client_proxy->create_resource_for_entity_set( 'Travel' )->create_request_for_create( ).

    " extend default data description with compositions
    DATA(description) = create_travel_request->create_data_descripton_node( ).
    " add to_Booking and to_BookingSupplement
    description->add_child( 'TO_BOOKING' )->add_child( 'TO_BOOKSUPPLEMENT' ).

    " fill in the request data
    create_travel_request->set_deep_business_data(
        is_business_data    = ls_business_data
        io_data_description = description
    ).

    " create a $batch with embedded changeset ( as the Fiori UI does )
    DATA(batch) = mo_client_proxy->create_request_for_batch( ).
    DATA(changeset) = batch->create_request_for_changeset( ).
    batch->add_request( changeset ).
    changeset->add_request( io_request = create_travel_request ).

    " Execute the request
    batch->execute( ).

    " check for errors
    batch->check_execution(  ).
    changeset->check_execution( ).

    " get response data back
    DATA(create_travel_response) = create_travel_request->get_response( ).
    cl_abap_unit_assert=>assert_not_initial( create_travel_response ).

    DATA ls_travel_response_data LIKE ls_business_data.
    create_travel_response->get_business_data( IMPORTING es_business_data = ls_travel_response_data ).

    cl_abap_unit_assert=>assert_not_initial( msg = 'response data-travel' act = ls_travel_response_data ).
    cl_abap_unit_assert=>assert_not_initial( msg = 'response data-booking' act = ls_travel_response_data-to_booking ).
    cl_abap_unit_assert=>assert_not_initial( msg = 'response data-booking supplement'
                                             act = ls_travel_response_data-to_booking[ 1 ]-to_booksupplement ).

    " verify also in database table
    SELECT * FROM /dmo/i_travel_m INTO TABLE @DATA(lt_travel). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_not_initial( msg = 'travel from db' act = lt_travel ).

    " check the description
    cl_abap_unit_assert=>assert_equals( msg = 'description' exp = ls_business_data-description  act = lt_travel[ 1 ]-description ).

    " total_price = SUM( flight_price ) + SUM ( supplement price ) + booking_fee
    cl_abap_unit_assert=>assert_equals( msg = 'total price incl. booking_fee' exp = '2210.50' act = lt_travel[ 1 ]-total_price ).

    " check also log written by "additional save" functionality
    SELECT * FROM /dmo/log_travel INTO TABLE @DATA(log_travel). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_not_initial( msg = '/DMO/LOG_TRAVEL' act = log_travel ).


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

    cl_abap_unit_assert=>assert_bound( msg = 'client proxy factory' act = client_proxy ).

  ENDMETHOD.

ENDCLASS.
