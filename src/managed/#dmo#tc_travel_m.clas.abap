"! @testing BDEF:/DMO/I_TRAVEL_M
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

    CLASS-DATA:
      cds_test_environment TYPE REF TO if_cds_test_environment,
      sql_test_environment TYPE REF TO if_osql_test_environment,

      begin_date           TYPE /dmo/begin_date,
      end_date             TYPE /dmo/end_date,

      agency_mock_data     TYPE STANDARD TABLE OF /dmo/agency,
      customer_mock_data   TYPE STANDARD TABLE OF /dmo/customer.

    CONSTANTS cid         TYPE abp_behv_cid    VALUE 'ROOT1'.
    CONSTANTS cid_node    TYPE abp_behv_cid    VALUE 'NODE1'.
    CONSTANTS cid_subnode TYPE abp_behv_cid    VALUE 'SUBNODE1'.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,

      create_validation_negative   FOR TESTING RAISING cx_static_check,
      create_validation_positive   FOR TESTING RAISING cx_static_check,
      deep_create_determination    FOR TESTING RAISING cx_static_check,
      create_fc_action_negative    FOR TESTING RAISING cx_static_check,
      create_fc_action_positive    FOR TESTING RAISING cx_static_check,
      create_fc_cba_negative       FOR TESTING RAISING cx_static_check,
      create_fc_cba_positive       FOR TESTING RAISING cx_static_check,
      create_additional_save       FOR TESTING RAISING cx_static_check.


ENDCLASS.



CLASS /dmo/tc_travel_m IMPLEMENTATION.

  METHOD class_setup.
    agency_mock_data     = VALUE #( ( agency_id   = '987654' name      = 'Miles and More'   ) ).
    customer_mock_data   = VALUE #( ( customer_id = '987653' last_name = 'Buchholm' ) ).

    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
                               VALUE #(
                                   ( i_for_entity = '/DMO/I_TRAVEL_M'            )
                                   ( i_for_entity = '/DMO/I_BOOKING_M'           )
                                   ( i_for_entity = '/DMO/I_BOOKSUPPL_M' )
                                 )
                             ).

    sql_test_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #(
        ( '/DMO/AGENCY' )
        ( '/DMO/CUSTOMER' )
        ( '/DMO/LOG_TRAVEL' )
      ) ) .

    sql_test_environment->insert_test_data( agency_mock_data   ).
    sql_test_environment->insert_test_data( customer_mock_data ).
  ENDMETHOD.


  METHOD setup.
    cds_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
    cds_test_environment->destroy(  ).
    sql_test_environment->destroy(  ).
  ENDMETHOD.



  METHOD create_validation_negative.

    begin_date = cl_abap_context_info=>get_system_date( ) - 10.
    end_date   = cl_abap_context_info=>get_system_date( ) + 30.

    " Create a Travel with invalid begin_date
    MODIFY ENTITIES OF /dmo/i_travel_m
      ENTITY travel
        CREATE FIELDS ( agency_id customer_id begin_date end_date description booking_fee currency_code overall_status ) WITH
        VALUE #( (         %cid = cid
                           agency_id      = agency_mock_data[ 1 ]-agency_id
                           customer_id    = customer_mock_data[ 1 ]-customer_id
                           begin_date     = begin_date
                           end_date       = end_date
                           description    = 'TestTravel 1'
                           booking_fee    = '10.5'
                           currency_code  = 'EUR'
                           overall_status = 'O' ) )
     MAPPED DATA(mapped_create)
     FAILED DATA(failed_create)
     REPORTED DATA(reported_create).

    " Travel was created successfully
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mapped_create-travel ) ).
    DATA(key_of_new_travel) = mapped_create-travel[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = cid act = key_of_new_travel-%cid ).
    cl_abap_unit_assert=>assert_not_initial( act = key_of_new_travel-travel_id ).

    cl_abap_unit_assert=>assert_initial( act = failed_create ).
    cl_abap_unit_assert=>assert_initial( act = reported_create ).

    " Trigger Validations
    COMMIT ENTITIES
    RESPONSE OF /DMO/I_Travel_M
    FAILED DATA(failed_commit)
    REPORTED DATA(reported_commit).

    " Commit failed as begin_date is invalid ( must be after system date )
    cl_abap_unit_assert=>assert_subrc( exp = 4 ).

    cl_abap_unit_assert=>assert_equals( act = lines( failed_commit-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_not_initial( act = failed_commit-travel[ 1 ]-travel_id ).

    cl_abap_unit_assert=>assert_equals( act = lines( reported_commit-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_not_initial( act = reported_commit-travel[ 1 ]-travel_id  ).
    cl_abap_unit_assert=>assert_equals( act = reported_commit-travel[ 1 ]-%element-begin_date  exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = reported_commit-travel[ 1 ]-%element-end_date  exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( exp = 004 act = reported_commit-travel[ 1 ]-%msg->if_t100_message~t100key-msgno ).
    cl_abap_unit_assert=>assert_equals( exp = '/DMO/CM_FLIGHT' act = reported_commit-travel[ 1 ]-%msg->if_t100_message~t100key-msgid ).


  ENDMETHOD.


  METHOD create_validation_positive.

    begin_date = cl_abap_context_info=>get_system_date( ) + 10.
    end_date   = cl_abap_context_info=>get_system_date( ) + 30.

    " Create a Travel with valid begin_date
    MODIFY ENTITIES OF /dmo/i_travel_m
      ENTITY travel
        CREATE FIELDS ( agency_id customer_id begin_date end_date description booking_fee currency_code overall_status ) WITH
        VALUE #( (         %cid = cid
                           agency_id      = agency_mock_data[ 1 ]-agency_id
                           customer_id    = customer_mock_data[ 1 ]-customer_id
                           begin_date     = begin_date
                           end_date       = end_date
                           description    = 'TestTravel 1'
                           booking_fee    = '10.5'
                           currency_code  = 'EUR'
                           overall_status = 'O' ) )
     MAPPED DATA(mapped_create)
     FAILED DATA(failed_create)
     REPORTED DATA(reported_create).

    " Travel was created successfully
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mapped_create-travel ) ).
    DATA(key_of_new_travel) = mapped_create-travel[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = cid act = key_of_new_travel-%cid ).
    cl_abap_unit_assert=>assert_not_initial( act = key_of_new_travel-travel_id ).

    cl_abap_unit_assert=>assert_initial( act = failed_create ).
    cl_abap_unit_assert=>assert_initial( act = reported_create ).

    " Trigger Validations
    COMMIT ENTITIES
    RESPONSE OF /DMO/I_Travel_M
    FAILED DATA(failed_commit)
    REPORTED DATA(reported_commit).

    " Commit was successful
    cl_abap_unit_assert=>assert_subrc( exp = 0 ).
    cl_abap_unit_assert=>assert_initial( act = failed_commit ).
    cl_abap_unit_assert=>assert_initial( act = reported_commit ).

  ENDMETHOD.


  METHOD deep_create_determination.

    begin_date = cl_abap_context_info=>get_system_date( ) + 10.
    end_date   = cl_abap_context_info=>get_system_date( ) + 30.

    " Create a Travel, Booking and Supplement
    MODIFY ENTITIES OF /dmo/i_travel_m
      ENTITY travel
        CREATE FIELDS (    agency_id customer_id begin_date end_date description booking_fee currency_code overall_status ) WITH
        VALUE #( (         %cid = cid
                           agency_id      = agency_mock_data[ 1 ]-agency_id
                           customer_id    = customer_mock_data[ 1 ]-customer_id
                           begin_date     = begin_date
                           end_date       = end_date
                           description    = 'TestTravel 1'
                           booking_fee    = '10.5'
                           currency_code  = 'EUR'
                           overall_status = 'O' ) )
        CREATE BY \_booking FIELDS ( booking_date customer_id carrier_id connection_id flight_date flight_price currency_code booking_status ) WITH
        VALUE #( (         %cid_ref  = cid
                           %target = VALUE #( ( %cid = cid_node
                                                booking_date   = begin_date
                                                customer_id    = customer_mock_data[ 1 ]-customer_id
                                                carrier_id     = '1'
                                                connection_id  = '1'
                                                flight_date    = begin_date
                                                flight_price   = '200'
                                                currency_code  = 'EUR'
                                                booking_status = 'N' ) )
         ) )
      ENTITY booking
          CREATE BY \_booksupplement FIELDS ( supplement_id price currency_code ) WITH
          VALUE #( (  %cid_ref = cid_node
                      %target = VALUE #( ( %cid          = cid_subnode
                                           supplement_id = '100'
                                           price         = '1000'
                                           currency_code = 'EUR' ) )
                       ) )
     MAPPED DATA(mapped_cba)
     FAILED DATA(failed_cba)
     REPORTED DATA(reported_cba).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mapped_cba-travel ) ).

    " Read determination result
    READ ENTITIES OF /dmo/i_travel_m
     ENTITY travel
       FIELDS ( total_price )
       WITH VALUE #( ( %tky = mapped_cba-travel[ 1 ]-%tky ) )
       RESULT DATA(result)
       FAILED DATA(failed_read).

    " Determination calculation was successful
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( result ) ).
    cl_abap_unit_assert=>assert_equals( act = result[ 1 ]-total_price exp = '1210.50'  ).

  ENDMETHOD.


  METHOD create_fc_action_negative.

    begin_date = cl_abap_context_info=>get_system_date( ) + 10.
    end_date   = cl_abap_context_info=>get_system_date( ) + 30.

    " Create a travel
    MODIFY ENTITIES OF /dmo/i_travel_m
     ENTITY travel
       CREATE FIELDS (    agency_id customer_id begin_date end_date description booking_fee currency_code overall_status ) WITH
       VALUE #( (         %cid = cid
                          agency_id      = agency_mock_data[ 1 ]-agency_id
                          customer_id    = customer_mock_data[ 1 ]-customer_id
                          begin_date     = begin_date
                          end_date       = end_date
                          description    = 'TestTravel 1'
                          booking_fee    = '10.5'
                          currency_code  = 'EUR'
                          overall_status = 'A' ) )
    MAPPED DATA(mapped_create)
    FAILED DATA(failed_create)
    REPORTED DATA(reported_create).

    " Travel was created successfully
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mapped_create-travel ) ).
    DATA(key_of_new_travel) = mapped_create-travel[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = cid act = key_of_new_travel-%cid ).
    cl_abap_unit_assert=>assert_not_initial( act = key_of_new_travel-travel_id ).

    cl_abap_unit_assert=>assert_initial( failed_create ).
    cl_abap_unit_assert=>assert_initial( reported_create ).

    " Execute action
    MODIFY ENTITIES OF /dmo/i_travel_m
     ENTITY travel
       EXECUTE acceptTravel FROM VALUE #( ( %tky = key_of_new_travel-%tky ) )
    FAILED DATA(failed_action)
    REPORTED DATA(reported_action).

    " Action failed as overall status is already accepted
    cl_abap_unit_assert=>assert_equals( act = lines( failed_action-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_not_initial( act = failed_action-travel[ 1 ]-travel_id ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on act = failed_action-travel[ 1 ]-%action-accepttravel ).

    cl_abap_unit_assert=>assert_initial( reported_action ).

  ENDMETHOD.


  METHOD create_fc_action_positive.

    begin_date = cl_abap_context_info=>get_system_date( ) + 10.
    end_date   = cl_abap_context_info=>get_system_date( ) + 30.

    " Create a Travel
    MODIFY ENTITIES OF /dmo/i_travel_m
     ENTITY travel
       CREATE FIELDS (    agency_id customer_id begin_date end_date description booking_fee currency_code overall_status ) WITH
       VALUE #( (         %cid = cid
                          agency_id      = agency_mock_data[ 1 ]-agency_id
                          customer_id    = customer_mock_data[ 1 ]-customer_id
                          begin_date     = begin_date
                          end_date       = end_date
                          description    = 'TestTravel 1'
                          booking_fee    = '10.5'
                          currency_code  = 'EUR'
                          overall_status = 'X' ) )
    MAPPED DATA(mapped_create)
    FAILED DATA(failed_create)
    REPORTED DATA(reported_create).

    " Travel was created successfully
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mapped_create-travel ) ).
    DATA(key_of_new_travel) = mapped_create-travel[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = cid act = key_of_new_travel-%cid ).
    cl_abap_unit_assert=>assert_not_initial( act = key_of_new_travel-travel_id ).

    cl_abap_unit_assert=>assert_initial( failed_create ).
    cl_abap_unit_assert=>assert_initial( reported_create ).

    " Execute action
    MODIFY ENTITIES OF /dmo/i_travel_m
     ENTITY travel
       EXECUTE acceptTravel FROM VALUE #( ( %tky = key_of_new_travel-%tky ) )
    FAILED DATA(failed_action)
    REPORTED DATA(reported_action).

    " Read action result
    READ ENTITIES OF /dmo/i_travel_m
     ENTITY travel
       FIELDS ( overall_status )
       WITH VALUE #( ( %tky = key_of_new_travel-%tky ) )
      RESULT DATA(result)
      FAILED DATA(failed_read).

    " Read was executed successfully
    cl_abap_unit_assert=>assert_initial( failed_read ).

    " Action was executed successfully
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( result ) ).
    cl_abap_unit_assert=>assert_equals( act = result[ 1 ]-overall_status exp = 'A'  ).

  ENDMETHOD.


  METHOD create_fc_cba_negative.

    begin_date = cl_abap_context_info=>get_system_date( ) + 10.
    end_date   = cl_abap_context_info=>get_system_date( ) + 30.

    " Create a Travel with overall status "Cancelled"
    MODIFY ENTITIES OF /dmo/i_travel_m
    ENTITY travel
      CREATE FIELDS (    agency_id customer_id begin_date end_date description booking_fee currency_code overall_status ) WITH
      VALUE #( (         %cid = cid
                         agency_id      = agency_mock_data[ 1 ]-agency_id
                         customer_id    = customer_mock_data[ 1 ]-customer_id
                         begin_date     = begin_date
                         end_date       = end_date
                         description    = 'TestTravel 1'
                         booking_fee    = '10.5'
                         currency_code  = 'EUR'
                         overall_status = 'X' ) )
    MAPPED DATA(mapped_create)
    FAILED DATA(failed_create)
    REPORTED DATA(reported_create).

    " Travel was created successfully
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mapped_create-travel ) ).
    DATA(key_of_new_travel) = mapped_create-travel[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = cid act = key_of_new_travel-%cid ).
    cl_abap_unit_assert=>assert_not_initial( act = key_of_new_travel-travel_id ).

    cl_abap_unit_assert=>assert_initial( failed_create ).
    cl_abap_unit_assert=>assert_initial( reported_create ).

    " Create a Booking for the created Travel
    MODIFY ENTITIES OF /dmo/i_travel_m
    ENTITY travel
      CREATE BY \_booking FIELDS ( booking_date customer_id carrier_id connection_id flight_date flight_price currency_code booking_status ) WITH
      VALUE #( (         %tky = key_of_new_travel-%tky
                         %target = VALUE #( ( %cid = cid_node
                                              booking_date   = begin_date
                                              customer_id    = customer_mock_data[ 1 ]-customer_id
                                              carrier_id     = '1'
                                              connection_id  = '1'
                                              flight_date    = begin_date
                                              flight_price   = '200'
                                              currency_code  = 'EUR'
                                              booking_status = 'B' ) )
       ) )
    MAPPED DATA(mapped_cba)
    FAILED DATA(failed_cba)
    REPORTED DATA(reported_cba).

    " Booking was not created as overall status of Travel is "Cancelled" (should be "Open" or "Accepted")
    cl_abap_unit_assert=>assert_initial( act = mapped_cba-booking ).

    cl_abap_unit_assert=>assert_equals( act = lines( failed_cba-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_not_initial( act = failed_cba-travel[ 1 ]-travel_id ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on act = failed_cba-travel[ 1 ]-%assoc-_Booking ).

    cl_abap_unit_assert=>assert_initial( act = reported_cba ).

  ENDMETHOD.


  METHOD create_fc_cba_positive.

    begin_date = cl_abap_context_info=>get_system_date( ) + 10.
    end_date   = cl_abap_context_info=>get_system_date( ) + 30.

    " Create a Travel with overall status "Accepted"
    MODIFY ENTITIES OF /dmo/i_travel_m
    ENTITY travel
      CREATE FIELDS (    agency_id customer_id begin_date end_date description booking_fee currency_code overall_status ) WITH
      VALUE #( (         %cid = cid
                         agency_id      = agency_mock_data[ 1 ]-agency_id
                         customer_id    = customer_mock_data[ 1 ]-customer_id
                         begin_date     = begin_date
                         end_date       = end_date
                         description    = 'TestTravel 1'
                         booking_fee    = '10.5'
                         currency_code  = 'EUR'
                         overall_status = 'A' ) )
    MAPPED DATA(mapped_create)
    FAILED DATA(failed_create)
    REPORTED DATA(reported_create).

    " Travel was created successfully
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mapped_create-travel ) ).
    DATA(key_of_new_travel) = mapped_create-travel[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = cid act = key_of_new_travel-%cid ).
    cl_abap_unit_assert=>assert_not_initial( act = key_of_new_travel-travel_id ).

    cl_abap_unit_assert=>assert_initial( failed_create ).
    cl_abap_unit_assert=>assert_initial( reported_create ).

    " Create a Booking for the created Travel
    MODIFY ENTITIES OF /dmo/i_travel_m
    ENTITY travel
      CREATE BY \_booking FIELDS ( booking_date customer_id carrier_id connection_id flight_date flight_price currency_code booking_status ) WITH
      VALUE #( (         %tky = key_of_new_travel-%tky
                         %target = VALUE #( ( %cid = cid_node
                                              booking_date   = begin_date
                                              customer_id    = customer_mock_data[ 1 ]-customer_id
                                              carrier_id     = '1'
                                              connection_id  = '1'
                                              flight_date    = begin_date
                                              flight_price   = '200'
                                              currency_code  = 'EUR'
                                              booking_status = 'B' ) )
       ) )
    MAPPED DATA(mapped_cba)
    FAILED DATA(failed_cba)
    REPORTED DATA(reported_cba).

    " Booking was created successfully
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mapped_cba-booking ) ).
    DATA(key_of_new_booking) = mapped_cba-booking[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = cid_node act = key_of_new_booking-%cid ).
    cl_abap_unit_assert=>assert_not_initial( act = key_of_new_booking-booking_id ).

    cl_abap_unit_assert=>assert_initial( act = failed_cba ).

    cl_abap_unit_assert=>assert_initial( act = reported_cba ).

  ENDMETHOD.


  METHOD create_additional_save.

    begin_date = cl_abap_context_info=>get_system_date( ) + 10.
    end_date   = cl_abap_context_info=>get_system_date( ) + 30.

    " Create a Travel
    MODIFY ENTITIES OF /dmo/i_travel_m
    ENTITY travel
      CREATE FIELDS (    agency_id customer_id begin_date end_date description booking_fee currency_code overall_status ) WITH
      VALUE #( (         %cid = cid
                         agency_id      = agency_mock_data[ 1 ]-agency_id
                         customer_id    = customer_mock_data[ 1 ]-customer_id
                         begin_date     = begin_date
                         end_date       = end_date
                         description    = 'TestTravel 1'
                         booking_fee    = '10.5'
                         currency_code  = 'EUR'
                         overall_status = 'A' ) )
    MAPPED DATA(mapped_create)
    FAILED DATA(failed_create)
    REPORTED DATA(reported_create).

    " Travel was created successfully
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( mapped_create-travel ) ).
    DATA(key_of_new_travel) = mapped_create-travel[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = cid act = key_of_new_travel-%cid ).
    cl_abap_unit_assert=>assert_not_initial( act = key_of_new_travel-travel_id ).

    cl_abap_unit_assert=>assert_initial( failed_create ).
    cl_abap_unit_assert=>assert_initial( reported_create ).

    " Trigger additional save
    COMMIT ENTITIES
    RESPONSE OF /DMO/I_Travel_M
    FAILED DATA(failed_commit)
    REPORTED DATA(reported_commit).

    cl_abap_unit_assert=>assert_subrc( exp = 0 ).
    cl_abap_unit_assert=>assert_initial( act = failed_commit ).
    cl_abap_unit_assert=>assert_initial( act = reported_commit ).

    " Ensure that a create operation has been executed
    SELECT changed_field_name FROM /dmo/log_travel WHERE changing_operation = 'CREATE' ORDER BY changed_field_name  INTO TABLE @DATA(log_fields).
    cl_abap_unit_assert=>assert_equals( act = lines( log_fields )  exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = log_fields[ 1 ] exp = 'booking_fee' ).
    cl_abap_unit_assert=>assert_equals( act = log_fields[ 2 ] exp = 'overall_status' ).


  ENDMETHOD.

ENDCLASS.
