"! @testing BDEF:/DMO/I_Travel_U
CLASS ltcl_handler DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS:
      fm_create      TYPE sxco_fm_name VALUE '/DMO/FLIGHT_TRAVEL_CREATE',
      fm_delete      TYPE sxco_fm_name VALUE '/DMO/FLIGHT_TRAVEL_DELETE',
      fm_read        TYPE sxco_fm_name VALUE '/DMO/FLIGHT_TRAVEL_READ',
      fm_set_booking TYPE sxco_fm_name VALUE '/DMO/FLIGHT_TRAVEL_SET_BOOKING',
      fm_update      TYPE sxco_fm_name VALUE '/DMO/FLIGHT_TRAVEL_UPDATE'.

    CONSTANTS:
      cid         TYPE abp_behv_cid     VALUE '42',
      travel_id   TYPE /dmo/travel_id   VALUE '1337',
      agency_id   TYPE /dmo/agency_id   VALUE '42',
      customer_id TYPE /dmo/customer_id VALUE '123',
      booking_id  TYPE /dmo/booking_id  VALUE '20',
      carrier_id  TYPE /dmo/carrier_id  VALUE 'XX'.

    CLASS-DATA:
      fm_test_environment TYPE REF TO if_function_test_environment.

    DATA:
      behavior_handler   TYPE REF TO lhc_travel,
      mapped             TYPE RESPONSE FOR MAPPED EARLY /dmo/i_travel_u,
      failed             TYPE RESPONSE FOR FAILED EARLY /dmo/i_travel_u,
      reported           TYPE RESPONSE FOR REPORTED EARLY /dmo/i_travel_u,
      mapped_line_travel LIKE LINE OF mapped-travel,
      failed_line        LIKE LINE OF failed-travel,
      reported_line      LIKE LINE OF reported-travel,
      message            TYPE symsg,
      messages           TYPE /dmo/t_message,
      message_obj        TYPE REF TO if_abap_behv_message.

    CLASS-METHODS:
      class_setup.

    METHODS:
      setup,
      teardown.

    METHODS:
      "! Checks if { @link ..lhc_travel.METH:create } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_CREATE } succeeds.
      create_success            FOR TESTING RAISING cx_static_check,
      "! Checks if { @link ..lhc_travel.METH:create } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_CREATE } fails.
      create_fail               FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_travel.METH:update } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_UPDATE } succeeds.
      update_success            FOR TESTING RAISING cx_static_check,
      "! Checks if { @link ..lhc_travel.METH:update } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_UPDATE } fails.
      update_fail               FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_travel.METH:delete } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_DELETE } succeeds.
      delete_success            FOR TESTING RAISING cx_static_check,
      "! Checks if { @link ..lhc_travel.METH:delete } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_DELETE } fails.
      delete_fail               FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_travel.METH:read } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_READ } succeeds.
      read_success              FOR TESTING RAISING cx_static_check,
      "! Checks if { @link ..lhc_travel.METH:read } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_READ } fails.
      read_fail                 FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_travel.METH:rba_Booking } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_READ } succeeds.
      rba_success               FOR TESTING RAISING cx_static_check,
      "! Checks if { @link ..lhc_travel.METH:rba_Booking } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_READ } fails.
      rba_fail                  FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_travel.METH:cba_Booking } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_READ } and { @link FUNC:/DMO/FLIGHT_TRAVEL_UPDATE } both succeeds.
      cba_success               FOR TESTING RAISING cx_static_check,
      "! Checks if { @link ..lhc_travel.METH:cba_Booking } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_READ } fails.
      cba_fail_read             FOR TESTING RAISING cx_static_check,
      "! Checks if { @link ..lhc_travel.METH:cba_Booking } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_READ } succeeds but { @link FUNC:/DMO/FLIGHT_TRAVEL_UPDATE } fails.
      cba_fail_update           FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_travel.METH:set_status_booked } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_SET_BOOKING } succeeds.
      set_status_booked_success FOR TESTING RAISING cx_static_check,
      "! Checks if { @link ..lhc_travel.METH:set_status_booked } works as expected when { @link FUNC:/DMO/FLIGHT_TRAVEL_SET_BOOKING } fails.
      set_status_booked_fail    FOR TESTING RAISING cx_static_check,

      "! Checks with a check table if { @link ..lhc_travel.METH:get_instance_features } works as expected.
      get_instance_feature      FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_travel.METH:get_global_authorizations } does not fail.
      get_global_authorizations FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_travel.METH:lock } does not fail.
      lock_success              FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_handler IMPLEMENTATION.

  METHOD class_setup.
    fm_test_environment = cl_function_test_environment=>create( function_modules = VALUE #(
          ( fm_create      )
          ( fm_delete      )
          ( fm_read        )
          ( fm_set_booking )
          ( fm_update      )
        )
      ).
  ENDMETHOD.

  METHOD setup.
    CLEAR:
      behavior_handler,
      message,
      messages,
      message_obj,
      mapped,
      failed,
      reported,
      mapped_line_travel,
      failed_line,
      reported_line.

    CREATE OBJECT behavior_handler FOR TESTING.
    message = VALUE symsg(
        msgty = 'E'
        msgid = 'CM_TEST'
        msgno = '123'
      ).
    messages = VALUE /dmo/t_message( ( message ) ).
    message_obj = behavior_handler->new_message(
                      id       = message-msgid
                      number   = message-msgno
                      severity = CONV #( message-msgty )
                    ).
  ENDMETHOD.

  METHOD teardown.
    fm_test_environment->clear_doubles( ).
    CLEAR: behavior_handler.
  ENDMETHOD.

  METHOD create_success.
    DATA:
      entities    TYPE TABLE FOR CREATE /dmo/i_travel_u\\travel.


    DATA(fm_create_double) = fm_test_environment->get_double( fm_create ).

    DATA(travel) = VALUE /dmo/travel( travel_id = travel_id ).

    DATA(fm_create_double_output) = fm_create_double->create_output_configuration( )->set_exporting_parameter( name  = 'ES_TRAVEL'  value = travel ).

    fm_create_double->configure_call( )->ignore_all_parameters( )->then_set_output( fm_create_double_output ).

    entities = VALUE #( ( %cid = cid ) ).

    behavior_handler->create(
        EXPORTING
          entities = entities
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_not_initial( mapped-travel ).
    mapped_line_travel = VALUE #( %cid = cid  TravelID = travel_id ).
    cl_abap_unit_assert=>assert_equals( exp = mapped_line_travel
                                        act = mapped-travel[ 1 ] ).

  ENDMETHOD.

  METHOD create_fail.
    DATA:
      entities      TYPE TABLE FOR CREATE /dmo/i_travel_u\\travel.


    DATA(fm_create_double) = fm_test_environment->get_double( fm_create ).

    DATA(fm_create_double_output) = fm_create_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_create_double->configure_call( )->ignore_all_parameters( )->then_set_output( fm_create_double_output ).

    entities = VALUE #( ( %cid = cid ) ).

    behavior_handler->create(
        EXPORTING
          entities = entities
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped ).

    cl_abap_unit_assert=>assert_not_initial( failed-travel ).
    cl_abap_unit_assert=>assert_not_initial( reported-travel ).

    failed_line = VALUE #( %cid = cid  %fail-cause = if_abap_behv=>cause-unspecific ).
    cl_abap_unit_assert=>assert_equals( exp = failed_line
                                        act = failed-travel[ 1 ] ).

    cl_abap_unit_assert=>assert_equals( exp = cid
                                        act = reported-travel[ 1 ]-%cid ).

    cl_abap_unit_assert=>assert_equals( exp = message-msgty
                                        act = reported-travel[ 1 ]-%msg->if_t100_dyn_msg~msgty ).
  ENDMETHOD.

  METHOD update_success.
    DATA:
      entities    TYPE TABLE FOR UPDATE /dmo/i_travel_u\\travel.

    DATA(fm_update_double) = fm_test_environment->get_double( fm_update ).

    DATA(travel)  = VALUE /dmo/s_travel_in(  travel_id = travel_id  agency_id = agency_id  customer_id = customer_id ).
    DATA(travelx) = VALUE /dmo/s_travel_inx( travel_id = travel_id  agency_id = abap_true  customer_id = abap_true   ).

    DATA(fm_update_double_input)  = fm_update_double->create_input_configuration(
                                      )->set_importing_parameter(
                                        name  = 'IS_TRAVEL'
                                        value = travel
                                      )->set_importing_parameter(
                                        name  = 'IS_TRAVELX'
                                        value = travelx
                                      ).
    DATA(fm_update_double_output_succ) = fm_update_double->create_output_configuration( ).

    DATA(fm_update_double_output_msgs) = fm_update_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_update_double->configure_call( )->when( fm_update_double_input
                                      )->then_set_output( fm_update_double_output_succ
                                      )->for_times( 1
                                      )->then_set_output( fm_update_double_output_msgs
                                      ).

    entities = VALUE #( (
                          TravelID   = travel_id
                          AgencyID   = agency_id
                          CustomerID = customer_id
                          %control   = VALUE #(
                                                  TravelID   = if_abap_behv=>mk-on
                                                  AgencyID   = if_abap_behv=>mk-on
                                                  CustomerID = if_abap_behv=>mk-on
                                              )
                       ) ).

    behavior_handler->update(
        EXPORTING
          entities = entities
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).


  ENDMETHOD.

  METHOD update_fail.
    DATA:
      entities      TYPE TABLE FOR UPDATE /dmo/i_travel_u\\travel.


    DATA(fm_update_double) = fm_test_environment->get_double( fm_update ).

    DATA(fm_update_double_output) = fm_update_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_update_double->configure_call( )->ignore_all_parameters( )->then_set_output( fm_update_double_output ).

    entities = VALUE #( ( TravelID = travel_id ) ).

    behavior_handler->update(
        EXPORTING
          entities = entities
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped ).

    cl_abap_unit_assert=>assert_not_initial( failed-travel ).
    cl_abap_unit_assert=>assert_not_initial( reported-travel ).

    failed_line = VALUE #( TravelID = travel_id  %fail-cause = if_abap_behv=>cause-unspecific ).
    cl_abap_unit_assert=>assert_equals( exp = failed_line
                                        act = failed-travel[ 1 ] ).

    cl_abap_unit_assert=>assert_equals( exp = travel_id
                                        act = reported-travel[ 1 ]-TravelID ).

    cl_abap_unit_assert=>assert_equals( exp = message-msgty
                                        act = reported-travel[ 1 ]-%msg->if_t100_dyn_msg~msgty ).

  ENDMETHOD.

  METHOD delete_success.
    DATA:
      keys    TYPE TABLE FOR DELETE /dmo/i_travel_u\\travel.


    DATA(fm_delete_double) = fm_test_environment->get_double( fm_delete ).

    DATA(fm_delete_double_input)  = fm_delete_double->create_input_configuration(
                                      )->set_importing_parameter(
                                        name  = 'IV_TRAVEL_ID'
                                        value = travel_id
                                      ).
    DATA(fm_delete_double_output_succ) = fm_delete_double->create_output_configuration( ).

    DATA(fm_delete_double_output_msgs) = fm_delete_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_delete_double->configure_call( )->when( fm_delete_double_input
                                      )->then_set_output( fm_delete_double_output_succ
                                      )->for_times( 1
                                      )->then_set_output( fm_delete_double_output_msgs
                                      ).

    keys = VALUE #( ( TravelID = travel_id ) ).

    behavior_handler->delete(
        EXPORTING
          keys = keys
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped ).
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD delete_fail.
    DATA:
      keys      TYPE TABLE FOR DELETE /dmo/i_travel_u\\travel.


    DATA(fm_delete_double) = fm_test_environment->get_double( fm_delete ).

    DATA(fm_delete_double_output) = fm_delete_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_delete_double->configure_call( )->ignore_all_parameters( )->then_set_output( fm_delete_double_output ).

    keys = VALUE #( ( TravelID = travel_id ) ).

    behavior_handler->delete(
        EXPORTING
          keys = keys
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped ).

    cl_abap_unit_assert=>assert_not_initial( failed-travel ).
    cl_abap_unit_assert=>assert_not_initial( reported-travel ).

    failed_line = VALUE #( TravelID = travel_id  %fail-cause = if_abap_behv=>cause-unspecific ).
    cl_abap_unit_assert=>assert_equals( exp = failed_line
                                        act = failed-travel[ 1 ] ).

    cl_abap_unit_assert=>assert_equals( exp = travel_id
                                        act = reported-travel[ 1 ]-TravelID ).

    cl_abap_unit_assert=>assert_equals( exp = message-msgty
                                        act = reported-travel[ 1 ]-%msg->if_t100_dyn_msg~msgty ).

  ENDMETHOD.

  METHOD read_success.
    DATA:
      keys        TYPE TABLE FOR READ IMPORT /dmo/i_travel_u\\travel,
      result      TYPE TABLE FOR READ RESULT /dmo/i_travel_u\\travel,
      result_line TYPE STRUCTURE FOR READ RESULT /dmo/i_travel_u\\travel.

    DATA(travel) = VALUE /dmo/travel(
                       travel_id   = travel_id
                       agency_id   = agency_id
                       customer_id = customer_id
                     ).
    result_line = CORRESPONDING #( travel MAPPING TO ENTITY ).

    DATA(fm_read_double) = fm_test_environment->get_double( fm_read ).

    DATA(fm_read_double_input)  = fm_read_double->create_input_configuration(
                                      )->set_importing_parameter(
                                        name  = 'IV_TRAVEL_ID'
                                        value = travel_id
                                      ).
    DATA(fm_read_double_output_succ) = fm_read_double->create_output_configuration(
                                       )->set_exporting_parameter(
                                         name  = 'ES_TRAVEL'
                                         value = travel
                                       ).

    DATA(fm_read_double_output_msgs) = fm_read_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_read_double->configure_call( )->when( fm_read_double_input
                                      )->then_set_output( fm_read_double_output_succ
                                      )->for_times( 1
                                      )->then_set_output( fm_read_double_output_msgs
                                      ).

    keys = VALUE #( ( TravelID = travel_id ) ).

    behavior_handler->read(
        EXPORTING
          keys = keys
        CHANGING
          result   = result
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_not_initial( result ).
    cl_abap_unit_assert=>assert_equals(
        act = result[ 1 ]
        exp = result_line
      ).
  ENDMETHOD.

  METHOD read_fail.
    DATA:
      keys   TYPE TABLE FOR READ IMPORT /dmo/i_travel_u\\travel,
      result TYPE TABLE FOR READ RESULT /dmo/i_travel_u\\travel.


    DATA(fm_read_double) = fm_test_environment->get_double( fm_read ).

    DATA(fm_read_double_output) = fm_read_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_read_double->configure_call( )->ignore_all_parameters( )->then_set_output( fm_read_double_output ).

    keys = VALUE #( ( TravelID = travel_id ) ).

    behavior_handler->read(
        EXPORTING
          keys = keys
        CHANGING
          result   = result
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped ).

    cl_abap_unit_assert=>assert_not_initial( failed-travel ).
    cl_abap_unit_assert=>assert_not_initial( reported-travel ).

    failed_line = VALUE #( TravelID = travel_id  %fail-cause = if_abap_behv=>cause-unspecific ).
    cl_abap_unit_assert=>assert_equals( exp = failed_line
                                        act = failed-travel[ 1 ] ).

    cl_abap_unit_assert=>assert_equals( exp = travel_id
                                        act = reported-travel[ 1 ]-TravelID ).

    cl_abap_unit_assert=>assert_equals( exp = message-msgty
                                        act = reported-travel[ 1 ]-%msg->if_t100_dyn_msg~msgty ).

  ENDMETHOD.

  METHOD rba_success.
    DATA:
      keys_rba               TYPE TABLE     FOR READ IMPORT /dmo/i_travel_u\\travel\_Booking,
      result                 TYPE TABLE     FOR READ RESULT /dmo/i_travel_u\\travel\_Booking,
      result_line            TYPE STRUCTURE FOR READ RESULT /dmo/i_travel_u\\travel\_Booking,
      association_links      TYPE TABLE     FOR READ LINK   /dmo/i_travel_u\\travel\_Booking,
      association_links_line TYPE STRUCTURE FOR READ LINK   /dmo/i_travel_u\\travel\_Booking,
      bookings               TYPE /dmo/t_booking.

    DATA(travel) = VALUE /dmo/travel(
                       travel_id   = travel_id
                       agency_id   = agency_id
                       customer_id = customer_id
                     ).
    DATA(booking) = VALUE /dmo/booking(
                       travel_id   = travel_id
                       booking_id  = booking_id
                       customer_id = customer_id
                       carrier_id  = carrier_id
                     ).
    APPEND booking TO bookings.
    result_line = CORRESPONDING #( booking MAPPING TO ENTITY ).
    association_links_line = VALUE #( source-TravelID = travel_id  target-TravelID = travel_id  target-BookingID = booking_id ).

    DATA(fm_rba_double) = fm_test_environment->get_double( fm_read ).

    DATA(fm_rba_double_input)  = fm_rba_double->create_input_configuration(
                                      )->set_importing_parameter(
                                        name  = 'IV_TRAVEL_ID'
                                        value = travel_id
                                      ).
    DATA(fm_rba_double_output_succ) = fm_rba_double->create_output_configuration(
                                       )->set_exporting_parameter(
                                         name  = 'ES_TRAVEL'
                                         value = travel
                                       )->set_exporting_parameter(
                                         name  = 'ET_BOOKING'
                                         value = bookings
                                       ).

    DATA(fm_rba_double_output_msgs) = fm_rba_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_rba_double->configure_call( )->when( fm_rba_double_input
                                      )->then_set_output( fm_rba_double_output_succ
                                      )->for_times( 1
                                      )->then_set_output( fm_rba_double_output_msgs
                                      ).

    keys_rba = VALUE #( ( TravelID = travel_id ) ).

    behavior_handler->rba_booking(
        EXPORTING
          keys_rba          = keys_rba
          result_requested  = abap_true
        CHANGING
          result            = result
          association_links = association_links
          failed            = failed
          reported          = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_not_initial( result ).
    cl_abap_unit_assert=>assert_equals(
        act = result[ 1 ]
        exp = result_line
      ).

    cl_abap_unit_assert=>assert_not_initial( association_links ).
    cl_abap_unit_assert=>assert_equals(
        act = association_links[ 1 ]
        exp = association_links_line
      ).
  ENDMETHOD.

  METHOD rba_fail.
    DATA:
      keys_rba          TYPE TABLE FOR READ IMPORT /dmo/i_travel_u\\travel\_Booking,
      result            TYPE TABLE FOR READ RESULT /dmo/i_travel_u\\travel\_Booking,
      association_links TYPE TABLE FOR READ LINK   /dmo/i_travel_u\\travel\_Booking.


    DATA(fm_rba_double) = fm_test_environment->get_double( fm_read ).

    DATA(fm_rba_double_output) = fm_rba_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_rba_double->configure_call( )->ignore_all_parameters( )->then_set_output( fm_rba_double_output ).

    keys_rba = VALUE #( ( TravelID = travel_id ) ).

    behavior_handler->rba_booking(
        EXPORTING
          keys_rba          = keys_rba
          result_requested  = abap_true
        CHANGING
          result            = result
          association_links = association_links
          failed            = failed
          reported          = reported
      ).

    cl_abap_unit_assert=>assert_initial( result ).
    cl_abap_unit_assert=>assert_initial( association_links ).

    cl_abap_unit_assert=>assert_not_initial( failed-travel ).
    cl_abap_unit_assert=>assert_not_initial( reported-travel ).

    failed_line = VALUE #( TravelID = travel_id  %fail-cause = if_abap_behv=>cause-unspecific ).
    cl_abap_unit_assert=>assert_equals( exp = failed_line
                                        act = failed-travel[ 1 ] ).

    cl_abap_unit_assert=>assert_equals( exp = travel_id
                                        act = reported-travel[ 1 ]-TravelID ).

    cl_abap_unit_assert=>assert_equals( exp = message-msgty
                                        act = reported-travel[ 1 ]-%msg->if_t100_dyn_msg~msgty ).

  ENDMETHOD.

  METHOD cba_success.
    DATA:
      entities_cba        TYPE TABLE FOR CREATE /dmo/i_travel_u\\travel\_booking,
      mapped_line_booking LIKE LINE OF mapped-booking,
      travel              TYPE /dmo/s_travel_in,
      travelx             TYPE /dmo/s_travel_inx,
      old_booking         TYPE /dmo/booking,
      new_booking         TYPE /dmo/s_booking_in,
      new_bookingx        TYPE /dmo/s_booking_inx,
      old_bookings        TYPE /dmo/t_booking,
      new_bookings        TYPE /dmo/t_booking_in,
      new_bookingsx       TYPE /dmo/t_booking_inx.

    travel  = VALUE /dmo/s_travel_in(  travel_id = travel_id ).
    travelx = VALUE /dmo/s_travel_inx( travel_id = travel_id ).

    new_booking  = VALUE /dmo/s_booking_in(
                       travel_id   = travel_id
                       booking_id  = booking_id  + 1
                       customer_id = customer_id + 1
                       carrier_id  = carrier_id
                     ).
    APPEND new_booking  TO new_bookings.

    old_booking  = VALUE /dmo/booking(
                       travel_id   = travel_id
                       booking_id  = booking_id
                       customer_id = customer_id
                       carrier_id  = carrier_id
                     ).
    APPEND old_booking  TO old_bookings.

    new_bookingx = VALUE /dmo/s_booking_inx(
                       booking_id  = booking_id
                       action_code = /dmo/if_flight_legacy=>action_code-create
                     ).
    APPEND new_bookingx TO new_bookingsx.

    " Read double
    DATA(fm_read_double) = fm_test_environment->get_double( fm_read ).

    DATA(fm_read_double_input)  = fm_read_double->create_input_configuration(
                                      )->set_importing_parameter(
                                        name  = 'IV_TRAVEL_ID'
                                        value = travel_id
                                      ).
    DATA(fm_read_double_output_succ) = fm_read_double->create_output_configuration(
                                         )->set_exporting_parameter(
                                           name  = 'ET_BOOKING'
                                           value = old_bookings
                                         ).

    DATA(fm_read_double_output_msgs) = fm_read_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_read_double->configure_call( )->when( fm_read_double_input
                                      )->then_set_output( fm_read_double_output_succ
                                      )->for_times( 1
                                      )->then_set_output( fm_read_double_output_msgs
                                      ).


    " Update double
    DATA(fm_update_double) = fm_test_environment->get_double( fm_update ).

    DATA(fm_update_double_input)  = fm_update_double->create_input_configuration(
                                      )->set_importing_parameter(
                                        name  = 'IS_TRAVEL'
                                        value = travel
                                      )->set_importing_parameter(
                                        name  = 'IS_TRAVELX'
                                        value = travelx
                                      )->set_importing_parameter(
                                        name  = 'IT_BOOKING'
                                        value = new_bookings
                                      )->set_importing_parameter(
                                        name  = 'IT_BOOKINGX'
                                        value = new_bookingsx
                                      ).
    DATA(fm_update_double_output_succ) = fm_update_double->create_output_configuration( ).

    DATA(fm_update_double_output_msgs) = fm_update_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_update_double->configure_call( )->when( fm_update_double_input
                                      )->then_set_output( fm_update_double_output_succ
                                      )->for_times( 1
                                      )->then_set_output( fm_update_double_output_msgs
                                      ).


    " Handler Call

    entities_cba = VALUE #( (
                       TravelID = travel_id
                       %target  = VALUE #( (
                                      %cid       = cid
                                      TravelID   = new_booking-travel_id
                                      BookingID  = new_booking-booking_id + 20  "on purpose to test the overwriting
                                      CustomerID = new_booking-customer_id
                                      AirlineID  = new_booking-carrier_id
                                    ) )
                     ) ).

    behavior_handler->cba_booking(
        EXPORTING
          entities_cba = entities_cba
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    mapped_line_booking = VALUE #(
        %cid       = cid
        TravelID   = new_booking-travel_id
        BookingID  = new_booking-booking_id
      ).

    cl_abap_unit_assert=>assert_not_initial( mapped ).
    cl_abap_unit_assert=>assert_equals(
        act = mapped-booking[ 1 ]
        exp = mapped_line_booking
      ).
  ENDMETHOD.

  METHOD cba_fail_read.
    DATA:
      entities_cba  TYPE TABLE FOR CREATE /dmo/i_travel_u\\travel\_booking.


    DATA(fm_read_double) = fm_test_environment->get_double( fm_read ).

    DATA(fm_read_double_output) = fm_read_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_read_double->configure_call( )->ignore_all_parameters( )->then_set_output( fm_read_double_output ).

    DATA(new_booking) = VALUE /dmo/s_booking_in(
                            travel_id   = travel_id
                            booking_id  = booking_id  + 1
                            customer_id = customer_id + 1
                            carrier_id  = carrier_id
                          ).

    entities_cba = VALUE #( (
                       TravelID = travel_id
                       %target  = VALUE #( (
                                      %cid       = cid
                                      TravelID   = new_booking-travel_id
                                      BookingID  = new_booking-booking_id + 20  "on purpose to test the overwriting
                                      CustomerID = new_booking-customer_id
                                      AirlineID  = new_booking-carrier_id
                                    ) )
                     ) ).

    behavior_handler->cba_booking(
        EXPORTING
          entities_cba = entities_cba
        CHANGING
          mapped       = mapped
          failed       = failed
          reported     = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped ).

    cl_abap_unit_assert=>assert_not_initial( failed-travel ).
    cl_abap_unit_assert=>assert_not_initial( reported-travel ).

    failed_line = VALUE #( TravelID = travel_id  %fail-cause = if_abap_behv=>cause-unspecific ).
    cl_abap_unit_assert=>assert_equals( exp = failed_line
                                        act = failed-travel[ 1 ] ).

    cl_abap_unit_assert=>assert_equals( exp = travel_id
                                        act = reported-travel[ 1 ]-TravelID ).

    cl_abap_unit_assert=>assert_equals( exp = message-msgty
                                        act = reported-travel[ 1 ]-%msg->if_t100_dyn_msg~msgty ).

  ENDMETHOD.

  METHOD cba_fail_update.
    DATA:
      entities_cba        TYPE TABLE FOR CREATE /dmo/i_travel_u\\travel\_booking,
      failed_line_booking LIKE LINE OF failed-booking,
      old_booking         TYPE /dmo/booking,
      old_bookings        TYPE /dmo/t_booking.

    old_booking  = VALUE /dmo/booking(
                       travel_id   = travel_id
                       booking_id  = booking_id
                       customer_id = customer_id
                       carrier_id  = carrier_id
                     ).
    APPEND old_booking  TO old_bookings.


    " Read double
    DATA(fm_read_double) = fm_test_environment->get_double( fm_read ).

    DATA(fm_read_double_input)  = fm_read_double->create_input_configuration(
                                      )->set_importing_parameter(
                                        name  = 'IV_TRAVEL_ID'
                                        value = travel_id
                                      ).
    DATA(fm_read_double_output_succ) = fm_read_double->create_output_configuration(
                                         )->set_exporting_parameter(
                                           name  = 'ET_BOOKING'
                                           value = old_bookings
                                         ).

    DATA(fm_read_double_output_msgs) = fm_read_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_read_double->configure_call( )->when( fm_read_double_input
                                    )->then_set_output( fm_read_double_output_succ
                                    )->for_times( 1
                                    )->then_set_output( fm_read_double_output_msgs
                                    ).

    " Update Double
    DATA(fm_update_double) = fm_test_environment->get_double( fm_update ).

    DATA(fm_update_double_output) = fm_update_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_update_double->configure_call( )->ignore_all_parameters( )->then_set_output( fm_update_double_output ).


    "Handler Call
    entities_cba = VALUE #( (
                       TravelID = travel_id
                       %target  = VALUE #( (
                                      %cid       = cid
                                      CustomerID = customer_id
                                      AirlineID  = carrier_id
                                    ) )
                     ) ).

    behavior_handler->cba_booking(
        EXPORTING
          entities_cba = entities_cba
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).


    cl_abap_unit_assert=>assert_initial( mapped ).

    cl_abap_unit_assert=>assert_not_initial( failed-booking ).
    cl_abap_unit_assert=>assert_not_initial( reported-booking ).

    failed_line_booking = VALUE #(
                              %cid        = cid
                              %fail-cause = if_abap_behv=>cause-unspecific
                            ).
    cl_abap_unit_assert=>assert_equals( exp = failed_line_booking
                                        act = failed-booking[ 1 ] ).

    cl_abap_unit_assert=>assert_equals( exp = cid
                                        act = reported-booking[ 1 ]-%cid ).

    cl_abap_unit_assert=>assert_equals( exp = message-msgty
                                        act = reported-booking[ 1 ]-%msg->if_t100_dyn_msg~msgty ).

  ENDMETHOD.

  METHOD get_instance_feature.
    TYPES:
      BEGIN OF check_line,
        travel_id                     TYPE /dmo/travel-travel_id,
        status                        TYPE /dmo/travel-status,
        exp_feature_set_status_booked TYPE abp_behv_feature,
        exp_feature_assoc_booking     TYPE abp_behv_feature,
      END OF check_line,
      check_table TYPE STANDARD TABLE OF check_line WITH DEFAULT KEY.

    DATA:
      keys               TYPE TABLE     FOR INSTANCE FEATURES KEY     /dmo/i_travel_u\\travel,
      requested_features TYPE STRUCTURE FOR INSTANCE FEATURES REQUEST /dmo/i_travel_u\\travel,
      result             TYPE TABLE     FOR INSTANCE FEATURES RESULT  /dmo/i_travel_u\\travel,
      result_line        TYPE STRUCTURE FOR INSTANCE FEATURES RESULT  /dmo/i_travel_u\\travel,
      import_read        TYPE STRUCTURE FOR READ              IMPORT  /dmo/i_travel_u\\travel,
      import_read_tab    TYPE TABLE     FOR READ              IMPORT  /dmo/i_travel_u\\travel,
      result_read        TYPE TABLE     FOR READ              RESULT  /dmo/i_travel_u\\travel.

    DATA(check_table) = VALUE check_table(
        ( travel_id  = '1'  status = 'O'  exp_feature_set_status_booked = if_abap_behv=>fc-o-enabled   exp_feature_assoc_booking = if_abap_behv=>fc-o-enabled  )
        ( travel_id  = '2'  status = 'B'  exp_feature_set_status_booked = if_abap_behv=>fc-o-disabled  exp_feature_assoc_booking = if_abap_behv=>fc-o-disabled )
        ( travel_id  = '3'  status = 'X'  exp_feature_set_status_booked = if_abap_behv=>fc-o-enabled   exp_feature_assoc_booking = if_abap_behv=>fc-o-disabled )
      ).


***
*
*  Volker: I have commented this out, because it seems that the API is not yet finalized, and thus changes lead to syntax errors in our code.
*
*
***

*    requested_features = VALUE #(
*                             %action-set_status_booked = if_abap_behv=>mk-on
*                             %assoc-_Booking           = if_abap_behv=>mk-on
*                           ).
*    DATA(env_config) = cl_abap_behv_test_environment=>create_environment_config(  )->set_bdef_dependencies( bdef_dependencies = VALUE #( ( '/DMO/I_TRAVEL_U' ) ) ).
*    DATA(bo_double_environment) = cl_abap_behv_test_environment=>create( environment_config = env_config ).
*
*    DATA(bo_double) = bo_double_environment->get_test_double( '/DMO/I_TRAVEL_U' ).
*
*    LOOP AT check_table INTO DATA(check_line).
**      DATA(bo_double_input)  = bo_double->create_read_input_config( ).
*      import_read_tab = VALUE #( ( TravelID = check_line-travel_id  %control-TravelID = if_abap_behv=>mk-on  %control-Status = if_abap_behv=>mk-on ) ).
**      bo_double_input->add_input_for_entity( import_read ).   "bo_double_input->set_instance( import_read ).
*      data(bo_double_input) = bo_double->create_read_input_for_entity( '/DMO/I_TRAVEL_U'   "can accept the entity name
*                                         )->set_instances_for_read( import_read_tab ).
*      data(input) = bo_double->create_read_input_config(  )->add_input_for_entity( bo_double_input ).
*
**      DATA(bo_double_output) = bo_double->create_read_output_config( ).
*      result_read = VALUE #( ( TravelID = check_line-travel_id  Status = check_line-status ) ).
**      bo_double_output->set_result( result_read ).
**      bo_double_output->set_result_for_read( result = result_read ).
*      data(output) = bo_double->create_read_output_config( )->set_result_for_read( result_read ).
*
*
*      bo_double->configure_call( )->for_read( )->when( input )->then( output ).
*
*
*      keys = VALUE #( ( TravelID = check_line-travel_id ) ).
*
*      behavior_handler->get_instance_features(
*          EXPORTING
*            keys               = keys
*            requested_features = requested_features
*          CHANGING
*            result             = result
*            failed             = failed
*            reported           = reported
*        ).
*
*      cl_abap_unit_assert=>assert_initial( act = failed    quit = if_abap_unit_constant=>quit-no ).
*      cl_abap_unit_assert=>assert_initial( act = reported  quit = if_abap_unit_constant=>quit-no ).
*
*      result_line = VALUE #(
*                         TravelID                  = check_line-travel_id
*                         %action-set_status_booked = check_line-exp_feature_set_status_booked
*                         %assoc-_Booking           = check_line-exp_feature_assoc_booking
*                       ).
*
*      cl_abap_unit_assert=>assert_not_initial( act = result  quit = if_abap_unit_constant=>quit-no ).
*      cl_abap_unit_assert=>assert_equals(
*          act  = result[ 1 ]
*          exp  = result_line
*          quit = if_abap_unit_constant=>quit-no
*        ).
*
*
*      bo_double->clear_double( ).
*    ENDLOOP.

  ENDMETHOD.

  METHOD set_status_booked_success.
    DATA:
      result      TYPE TABLE     FOR ACTION RESULT /dmo/i_travel_u\\travel~set_status_booked,
      result_line TYPE STRUCTURE FOR ACTION RESULT /dmo/i_travel_u\\travel~set_status_booked.

    DATA(travel) = VALUE /dmo/travel(
                       travel_id   = travel_id
                       agency_id   = agency_id
                       customer_id = customer_id
                       status      = 'B'
                     ).

    result_line-TravelID        = travel_id.
    result_line-%param          = CORRESPONDING #( travel MAPPING TO ENTITY ).
    result_line-%param-TravelID = travel_id.

    " Set_Status_Booked Double
    DATA(fm_set_booking_double) = fm_test_environment->get_double( fm_set_booking ).

    DATA(fm_set_booking_double_input)  = fm_set_booking_double->create_input_configuration(
                                      )->set_importing_parameter(
                                        name  = 'IV_TRAVEL_ID'
                                        value = travel_id
                                      ).

    DATA(fm_set_booking_double_out_succ) = fm_set_booking_double->create_output_configuration( ).
    DATA(fm_set_booking_double_out_msgs) = fm_set_booking_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_set_booking_double->configure_call( )->when( fm_set_booking_double_input
                                      )->then_set_output( fm_set_booking_double_out_succ
                                      )->for_times( 1
                                      )->then_set_output( fm_set_booking_double_out_msgs
                                      ).


    " Read Double
    DATA(fm_read_double) = fm_test_environment->get_double( fm_read ).

    DATA(fm_read_double_input)  = fm_read_double->create_input_configuration(
                                      )->set_importing_parameter(
                                        name  = 'IV_TRAVEL_ID'
                                        value = travel_id
                                      ).
    DATA(fm_read_double_output_succ) = fm_read_double->create_output_configuration(
                                       )->set_exporting_parameter(
                                         name  = 'ES_TRAVEL'
                                         value = travel
                                       ).

    DATA(fm_read_double_output_msgs) = fm_read_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_read_double->configure_call( )->when( fm_read_double_input
                                      )->then_set_output( fm_read_double_output_succ
                                      )->for_times( 1
                                      )->then_set_output( fm_read_double_output_msgs
                                      ).


    " Handler Call
    behavior_handler->set_status_booked(
        EXPORTING
          keys     = VALUE #( ( TravelID = travel_id ) )
        CHANGING
          result   = result
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped ).
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_not_initial( result ).
    cl_abap_unit_assert=>assert_equals(
        act = result[ 1 ]
        exp = result_line
      ).
  ENDMETHOD.

  METHOD set_status_booked_fail.
    DATA:
      result      TYPE TABLE     FOR ACTION RESULT /dmo/i_travel_u\\travel~set_status_booked.

    " Set_Status_Booked Double
    DATA(fm_set_booking_double) = fm_test_environment->get_double( fm_set_booking ).


    DATA(fm_set_booking_double_out_msgs) = fm_set_booking_double->create_output_configuration( )->set_exporting_parameter( name  = 'ET_MESSAGES'  value = messages ).

    fm_set_booking_double->configure_call( )->ignore_all_parameters(
                                      )->then_set_output( fm_set_booking_double_out_msgs
                                      ).

    behavior_handler->set_status_booked(
        EXPORTING
          keys     = VALUE #( ( TravelID = travel_id ) )
        CHANGING
          result   = result
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( result ).
    cl_abap_unit_assert=>assert_initial( mapped ).

    cl_abap_unit_assert=>assert_not_initial( failed-travel ).
    cl_abap_unit_assert=>assert_not_initial( reported-travel ).

    failed_line = VALUE #( TravelID = travel_id  %fail-cause = if_abap_behv=>cause-unspecific ).
    cl_abap_unit_assert=>assert_equals( exp = failed_line
                                        act = failed-travel[ 1 ] ).

    cl_abap_unit_assert=>assert_equals( exp = travel_id
                                        act = reported-travel[ 1 ]-TravelID ).

    cl_abap_unit_assert=>assert_equals( exp = message-msgty
                                        act = reported-travel[ 1 ]-%msg->if_t100_dyn_msg~msgty ).
  ENDMETHOD.

  METHOD lock_success.
    behavior_handler->lock(
      EXPORTING
        keys     = VALUE #( ( TravelID = travel_id ) )
      CHANGING
        failed   = failed
        reported = reported
    ).
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_lock_object_factory=>dequeue_all( ).
  ENDMETHOD.

  METHOD get_global_authorizations.
    DATA:
      requested_authorizations TYPE STRUCTURE FOR GLOBAL AUTHORIZATION REQUEST /dmo/i_travel_u\\travel,
      result                   TYPE STRUCTURE FOR GLOBAL AUTHORIZATION RESULT /dmo/i_travel_u\\travel.

    requested_authorizations = VALUE #(
        %create                   = if_abap_behv=>mk-on
        %update                   = if_abap_behv=>mk-on
        %delete                   = if_abap_behv=>mk-on
        %action-set_status_booked = if_abap_behv=>mk-on
      ).

    behavior_handler->get_global_authorizations(
      EXPORTING
        requested_authorizations = requested_authorizations
      CHANGING
        result                   = result
        reported                 = reported
    ).

    cl_abap_unit_assert=>assert_initial( result ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

ENDCLASS.


"! @testing BDEF:/DMO/I_Travel_U
CLASS ltcl_saver DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS:
      fm_initialize     TYPE sxco_fm_name VALUE '/DMO/FLIGHT_TRAVEL_INITIALIZE',
      fm_adjust_numbers TYPE sxco_fm_name VALUE '/DMO/FLIGHT_TRAVEL_ADJ_NUMBERS',
      fm_save           TYPE sxco_fm_name VALUE '/DMO/FLIGHT_TRAVEL_SAVE'.

    CLASS-DATA:
      fm_test_environment TYPE REF TO if_function_test_environment.

    DATA:
      save_handler TYPE REF TO lsc_i_travel_u,
      mapped       TYPE RESPONSE FOR MAPPED   LATE /dmo/i_travel_u,
      failed       TYPE RESPONSE FOR FAILED   LATE /dmo/i_travel_u,
      reported     TYPE RESPONSE FOR REPORTED LATE /dmo/i_travel_u.

    CLASS-METHODS:
      class_setup.

    METHODS:
      setup,
      teardown,
      _check_all_initial.

    METHODS:
      "! Checks if { @link ..lsc_i_travel_u.METH:finalize } returns initial failed and reported.
      finalize          FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lsc_i_travel_u.METH:check_before_save } returns initial failed and reported.
      check_before_save FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lsc_i_travel_u.METH:adjust_numbers } mapping from { @link FUNC:/DMO/FLIGHT_TRAVEL_ADJ_NUMBERS } works as expected.
      adjust_numbers    FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lsc_i_travel_u.METH:save } calls { @link FUNC:/DMO/FLIGHT_TRAVEL_SAVE } and does not raise any exception.
      save              FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lsc_i_travel_u.METH:cleanup } calls { @link FUNC:/DMO/FLIGHT_TRAVEL_INITIALIZE } and does not raise any exception.
      cleanup           FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lsc_i_travel_u.METH:cleanup_finalize } does not raise any exception.
      cleanup_finalize  FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_saver IMPLEMENTATION.
  METHOD class_setup.
    fm_test_environment = cl_function_test_environment=>create( function_modules = VALUE #(
          ( fm_initialize     )
          ( fm_save           )
          ( fm_adjust_numbers )
        )
      ).
  ENDMETHOD.

  METHOD setup.
    CLEAR:
      save_handler,
      mapped,
      failed,
      reported.

    CREATE OBJECT save_handler FOR TESTING.
  ENDMETHOD.

  METHOD teardown.
    fm_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD finalize.
    save_handler->finalize(
        CHANGING
*        mapped   = mapped
          failed   = failed
          reported = reported
      ).
    _check_all_initial( ).
  ENDMETHOD.

  METHOD check_before_save.
    save_handler->check_before_save(
        CHANGING
*        mapped   = mapped
          failed   = failed
          reported = reported
      ).
    _check_all_initial( ).
  ENDMETHOD.

  METHOD adjust_numbers.
    CONSTANTS:
      travel_pre_1               TYPE /dmo/travel_id VALUE '9990',
      travel_pre_2               TYPE /dmo/travel_id VALUE '9995',
      travel_final_1             TYPE /dmo/travel_id VALUE '6310',
      travel_final_2             TYPE /dmo/travel_id VALUE '6315',
      booking_pre_1              TYPE /dmo/booking_id VALUE '990',
      booking_pre_2              TYPE /dmo/booking_id VALUE '995',
      booking_final_1            TYPE /dmo/booking_id VALUE '410',
      booking_final_2            TYPE /dmo/booking_id VALUE '415',
      booking_supplement_pre_1   TYPE /dmo/booking_supplement_id VALUE '90',
      booking_supplement_pre_2   TYPE /dmo/booking_supplement_id VALUE '95',
      booking_supplement_final_1 TYPE /dmo/booking_supplement_id VALUE '10',
      booking_supplement_final_2 TYPE /dmo/booking_supplement_id VALUE '15'.


    DATA:
      travel_mapping       TYPE /dmo/if_flight_legacy=>tt_ln_travel_mapping,
      booking_mapping      TYPE /dmo/if_flight_legacy=>tt_ln_booking_mapping,
      bookingsuppl_mapping TYPE /dmo/if_flight_legacy=>tt_ln_bookingsuppl_mapping.

    travel_mapping = VALUE /dmo/if_flight_legacy=>tt_ln_travel_mapping(
        ( preliminary = VALUE /dmo/if_flight_legacy=>ts_ln_travel( travel_id = travel_pre_1   )
          final       = VALUE /dmo/if_flight_legacy=>ts_ln_travel( travel_id = travel_final_1 ) )
        ( preliminary = VALUE /dmo/if_flight_legacy=>ts_ln_travel( travel_id = travel_pre_2   )
          final       = VALUE /dmo/if_flight_legacy=>ts_ln_travel( travel_id = travel_final_2 ) )
      ).

    booking_mapping = VALUE /dmo/if_flight_legacy=>tt_ln_booking_mapping(
        ( preliminary = VALUE /dmo/if_flight_legacy=>ts_ln_booking( travel_id = travel_pre_1    booking_id = booking_pre_1   )
          final       = VALUE /dmo/if_flight_legacy=>ts_ln_booking( travel_id = travel_final_1  booking_id = booking_final_1 ) )
        ( preliminary = VALUE /dmo/if_flight_legacy=>ts_ln_booking( travel_id = travel_pre_1    booking_id = booking_pre_2   )
          final       = VALUE /dmo/if_flight_legacy=>ts_ln_booking( travel_id = travel_final_1  booking_id = booking_final_2 ) )
      ).

    bookingsuppl_mapping = VALUE /dmo/if_flight_legacy=>tt_ln_bookingsuppl_mapping(
        ( preliminary = VALUE /dmo/if_flight_legacy=>ts_ln_bookingsuppl( travel_id = travel_pre_1    booking_id = booking_pre_1    booking_supplement_id = booking_supplement_pre_1   )
          final       = VALUE /dmo/if_flight_legacy=>ts_ln_bookingsuppl( travel_id = travel_final_1  booking_id = booking_final_1  booking_supplement_id = booking_supplement_final_1 ) )
        ( preliminary = VALUE /dmo/if_flight_legacy=>ts_ln_bookingsuppl( travel_id = travel_pre_1    booking_id = booking_pre_1    booking_supplement_id = booking_supplement_pre_2   )
          final       = VALUE /dmo/if_flight_legacy=>ts_ln_bookingsuppl( travel_id = travel_final_1  booking_id = booking_final_1  booking_supplement_id = booking_supplement_final_2 ) )
      ).


    DATA(fm_adjust_numbers_double) = fm_test_environment->get_double( fm_adjust_numbers ).

    DATA(fm_adjust_numbers_double_out) = fm_adjust_numbers_double->create_output_configuration(
                                       )->set_exporting_parameter( name = 'et_travel_mapping'        value = travel_mapping
                                       )->set_exporting_parameter( name = 'et_booking_mapping'       value = booking_mapping
                                       )->set_exporting_parameter( name = 'et_bookingsuppl_mapping'  value = bookingsuppl_mapping
                                       ).

    fm_adjust_numbers_double->configure_call( )->ignore_all_parameters( "when( fm_adjust_numbers_double_input
                                      )->then_set_output( fm_adjust_numbers_double_out
                                      ).


    save_handler->adjust_numbers(
        CHANGING
          mapped   = mapped
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        act = lines( mapped-travel )
        exp = 2
      ).
    LOOP AT travel_mapping INTO DATA(travel_map).
      cl_abap_unit_assert=>assert_equals(
          act = mapped-travel[ KEY entity  %key = CORRESPONDING #( travel_map-final MAPPING TravelID = travel_id ) ]-%tmp
          exp = travel_map-preliminary
        ).
    ENDLOOP.


    cl_abap_unit_assert=>assert_equals(
        act = lines( mapped-booking )
        exp = 2
      ).
    LOOP AT booking_mapping INTO DATA(booking_map).
      cl_abap_unit_assert=>assert_equals(
          act = mapped-booking[ KEY entity  %key = CORRESPONDING #( booking_map-final MAPPING TravelID = travel_id  BookingID = booking_id ) ]-%tmp
          exp = booking_map-preliminary
        ).
    ENDLOOP.


    cl_abap_unit_assert=>assert_equals(
        act = lines( mapped-bookingsupplement )
        exp = 2
      ).
    LOOP AT bookingsuppl_mapping INTO DATA(bookingsuppl_map).
      cl_abap_unit_assert=>assert_equals(
          act = mapped-bookingsupplement[ KEY entity  %key = CORRESPONDING #( bookingsuppl_map-final MAPPING TravelID = travel_id  BookingID = booking_id  BookingSupplementID = booking_supplement_id ) ]-%tmp
          exp = bookingsuppl_map-preliminary
        ).
    ENDLOOP.
  ENDMETHOD.

  METHOD save.
    save_handler->save(
        CHANGING
          reported = reported
      ).
    _check_all_initial( ).
  ENDMETHOD.

  METHOD cleanup.
    TRY.
        save_handler->cleanup( ).
      CATCH cx_root INTO DATA(root).
        cl_abap_unit_assert=>fail( msg = root->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD cleanup_finalize.
    TRY.
        save_handler->cleanup_finalize( ).
      CATCH cx_root INTO DATA(root).
        cl_abap_unit_assert=>fail( msg = root->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD _check_all_initial.
    cl_abap_unit_assert=>assert_initial( mapped ).
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

ENDCLASS.
