"! <p class="shorttext synchronized" lang="en">Demonstrate use of MOCKEMLAPI variant to isolate EML dependencies</p>
"! <p> This class demonstrates the use of RAP BO test double framework to isolate BO dependencies via EML involving draft instances in code under test, by creating test
"! doubles with API support to mock EML APIs.</p>
"!<p> With this variant, the framework provides a rich set of APIs to build the expected EML structure with input instances in code under test and configure
"! the desired output configuration for the expected EML statement.</p>
"! <ul>
"! <li>Operation names in test method names, signify EML operations in CUT (Code Under Test).</li>
"! <li>Involved instances are mostly draft instances i.e. with %is_draft field marked with if_abap_behv=>mk-on.</li>
"! <li>The dependent BO is /dmo/r_travel_d and the code under test is its consumer {@link /dmo/tc_travel_d_bo_consumer}</li>
"! <li>The expected input and output should be configured on double via {@link if_botd_mockemlapi_test_double.Meth:configure_call} API.</li>
"! <li>The expected input structure of EML should be built using {@link cl_botd_mockemlapi_bldrfactory}~{@link cl_botd_mockemlapi_bldrfactory.Meth:get_input_config_builder}.</li>
"! <li>The expected output structure of EML should be built using {@link cl_botd_mockemlapi_bldrfactory}~{@link cl_botd_mockemlapi_bldrfactory.Meth:get_output_config_builder}.</li>
"! </ul>
"! For examples with isolating EML dependencies on active instances, refer to the class {@link /dmo/tc_botd_travel_m_demos}.
"! Cases with specific examples requiring draft instances of a draft enabled BO will be demonstrated here."
class ltcl_using_mockemlapi_variant definition final for testing
  duration short
  risk level harmless.

  private section.

    methods isolate_create for testing raising cx_static_check.
    methods isolate_create_ba for testing raising cx_static_check.
    methods isolate_update for testing raising cx_static_check.
    methods isolate_delete for testing raising cx_static_check.
    methods isolate_actions for testing raising cx_static_check.
    methods isolate_read for testing raising cx_static_check.
    methods isolate_read_ba for testing raising cx_static_check.
    methods isolate_deep_create for testing raising cx_static_check.

    "Additional configurations
    methods for_times_configuration for testing raising cx_static_check.
    methods chaining_outputs for testing raising cx_static_check.
    methods ignore_input_configuration for testing raising cx_static_check.
    methods partial_input_configuration for testing raising cx_static_check.
    methods behavior_verification for testing raising cx_static_check.

    methods setup.
    class-methods class_setup.
    class-methods class_teardown.
    class-data double type ref to if_botd_mockemlapi_test_double.
    class-data environment type ref to if_botd_mockemlapi_bo_test_env.
    class-data cut type ref to /dmo/tc_travel_d_bo_consumer.

endclass.

class ltcl_using_mockemlapi_variant implementation.

  method class_setup.
  "Create doubles for BO '/DMO/R_TRAVEL_D'.
  "a. Prepare environment configuration with bdef dependencies for which doubles are to be created
  data(env_config) = cl_botd_mockemlapi_bo_test_env=>prepare_environment_config(  )->set_bdef_dependencies( bdef_dependencies = value #( ( '/DMO/R_TRAVEL_D' ) )
                                                                                  )->handle_draft( bdef_dependencies = value #( ( '/DMO/R_TRAVEL_D' ) ) ).

  "b. Create the test doubles for bdefs from the environment configuration and get the environment instance
  environment = cl_botd_mockemlapi_bo_test_env=>create( environment_config = env_config ).

  "Code under test
  cut = new #(  ).
  endmethod.

  method class_teardown.
  "destroy the environment and all the test doubles
    environment->destroy(  ).
  endmethod.

  method setup.
  "Clear the configurations done on all test doubles from the environment
    environment->clear_doubles(  ).
  endmethod.

  method isolate_create.
  "Test Goal: Isolate Modify EML with Create in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only create ).
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "For Create on Travel entity
   data create_travel_instances type table for create /DMO/R_TRAVEL_D.
   create_travel_instances = value #( ( %cid = 'Travel_987' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel_987' %control-AgencyID = if_abap_behv=>mk-on
                                                                                                                     %control-CustomerID = if_abap_behv=>mk-on
                                                                                                                     %control-description = if_abap_behv=>mk-on )
                                      ( %cid = 'Travel_988' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel_988'
                                        BeginDate = cl_abap_context_info=>get_system_date( ) + 10 EndDate = cl_abap_context_info=>get_system_date( ) ) ).
                                                                                                                                        "BeginDate > EndDate. Should fail due to FEATURE CONTROL
                                      "%control structure is ignored in MOCKEMLAPI variant, while matching the EML input with configured input.

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/R_TRAVEL_D.
    data failed type response for failed early /DMO/R_TRAVEL_D.

    "Setup mapped and failed according to the response to be returned
    mapped-travel = value #( ( TravelUUID = '987'  %cid = 'Travel_987' %is_draft = if_abap_behv=>mk-on ) ). "Implies that a travel with travelUUid 987 was created for 1st instance
    failed-travel = value #( ( TravelUUID = '988'  %cid = 'Travel_988' %is_draft = if_abap_behv=>mk-on %create = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-unspecific ) ).
                                                                                                              "Create on 2nd instance failed as the BeginDate > EndDate. //DUE TO FEATURE CONTROL

  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts and set instances for all operations on that entity
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/R_TRAVEL_D'
                                   )->set_instances_for_create( create_travel_instances ).

    "Input configuration for EML
    "Input should match the EML structure and instances in code under test
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped )->set_failed( failed ).

  "Step 4: Configure the Modify EML in CUT via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/R_TRAVEL_D' ).
    double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  cut->create_travel(
    exporting
      travel   =  create_travel_instances
    importing
      mapped   = data(mapped_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-%cid exp = 'Travel_987' ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%cid exp = 'Travel_988' ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_create_ba.
  "Test Goal: Isolate the Modify EML with Create by association (create_ba) in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only create_ba ).
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only create_ba ).
   data cba_booking_instances type table for create /DMO/R_TRAVEL_D\_Booking.
   cba_booking_instances = value #( ( TravelUUID = '987' %is_draft = if_abap_behv=>mk-on
                                      %target = value #( ( %is_draft = if_abap_behv=>mk-on %cid = 'Travel_987_Booking_001' AirlineID = '000111' CustomerID = '000006' FlightDate = cl_abap_context_info=>get_system_date( ) + 10 )
                                                         ( %is_draft = if_abap_behv=>mk-on %cid = 'Travel_987_Booking_002' AirlineID = '000112' CustomerID = '000007' FlightDate = cl_abap_context_info=>get_system_date( ) + 10 ) )  )
                                                                                                      "Creation of bookings for TravelUUID 987 should pass
                                     ( TravelUUID = '988' %is_draft = if_abap_behv=>mk-on
                                       %target = value #( ( %is_draft = if_abap_behv=>mk-on %cid = 'Travel_988_Booking_001' AirlineID = '000111' CustomerID = '000006' FlightDate = cl_abap_context_info=>get_system_date( ) + 10 ) )
                                     ) ). "Creation of booking for TravelUUID 988 should fail, assuming TravelUUID 988 did not exist

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/R_TRAVEL_D.
    data failed type response for failed early /DMO/R_TRAVEL_D.

    "Setup mapped and failed according to the response to be returned
    mapped-booking = value #( ( %cid = 'Travel_987_Booking_001' BookingUUID = '001' %is_draft = if_abap_behv=>mk-on ) ( %cid = 'Travel_987_Booking_002' BookingUUID = '002' %is_draft = if_abap_behv=>mk-on ) ).

    "For failure of create_ba for travelUUID 988
    failed-travel = value #( ( %is_draft = if_abap_behv=>mk-on TravelUUID = '988' %assoc-_Booking = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).
    failed-booking = value #( ( %is_draft = if_abap_behv=>mk-on %cid = 'Travel_988_Booking_001' %fail-cause = if_abap_behv=>cause-dependency ) ).

  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/R_TRAVEL_D'   "can accept the entity name or alias name
                                   )->set_instances_for_create_ba( cba_booking_instances ).

    "Input configuration for EML
    "Input should match the EML structure and instances in code under test
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped )->set_failed( failed ).

  "Step 4: Configure the Modify EML via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/R_TRAVEL_D' ).
    double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  cut->create_booking_by_assoc(
    exporting
      booking_by_travel = cba_booking_instances
    importing
      mapped   = data(mapped_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-booking ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-booking ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-%cid exp = 'Travel_987_Booking_001' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 2 ]-%cid exp = 'Travel_987_Booking_002' ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-TravelUUID exp = conv sysuuid_x16( '988' ) ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-booking[ 1 ]-%cid exp = 'Travel_988_Booking_001' ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.


  method behavior_verification.
  "Test Goal: Isolate the multiple Modify EML statements with multiple operations in CUT (Code under Test) and configure responses via API.
  "And verify if the executions were as per the configuration

  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.
  "Step 5: Verify the executions on the double.

  "*************Configure first EML in Code under test*************
  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "For Create on Travel entity
   data create_travel_instances type table for create /DMO/R_TRAVEL_D.
   create_travel_instances = value #( ( %cid = 'Travel_987' CustomerID = '000006' description = 'Travel_987'  %control-CustomerID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on %is_draft = if_abap_behv=>mk-on )
                                      ( %cid = 'Travel_988' CustomerID = '000007' description = 'Travel_988' %is_draft = if_abap_behv=>mk-on ) ).

  "For Creating Booking by Association on Travel entity
   data cba_booking_instances type table for create /DMO/R_TRAVEL_D\_Booking.
   cba_booking_instances = value #( ( %cid_ref = 'Travel_987' TravelUUID = '987' %is_draft = if_abap_behv=>mk-on
                                      %target = value #( (  %cid = 'Travel_987_Booking_001' %is_draft = if_abap_behv=>mk-on
                                                            connectionid = 'C1' %control-connectionid = if_abap_behv=>mk-on
                                                            CustomerID = '000006' %control-CustomerID = if_abap_behv=>mk-on
                                                            FlightDate = cl_abap_context_info=>get_system_date( ) + 10 %control-FlightDate = if_abap_behv=>mk-on
                                                            bookingstatus = 'B' %control-bookingstatus = if_abap_behv=>mk-on )
                                                         (  %cid = 'Travel_987_Booking_002' %is_draft = if_abap_behv=>mk-on
                                                            connectionid = 'C1' %control-connectionid = if_abap_behv=>mk-on
                                                            CustomerID = '000007' %control-CustomerID = if_abap_behv=>mk-on
                                                            FlightDate = cl_abap_context_info=>get_system_date( ) + 10 %control-FlightDate = if_abap_behv=>mk-on
                                                            bookingstatus = 'B' %control-bookingstatus = if_abap_behv=>mk-on
                                                            ) )  )
                                                                                                      "Creation of bookings for TravelUUID 987 should pass
                                     ).
  "For Update on Booking entity
   data update_booking_instances type table for update /DMO/R_TRAVEL_D\\booking.
   update_booking_instances = value #( ( TravelUUID = '987' BookingUUID = '001' flightprice = 100 %control-flightprice = if_abap_behv=>mk-on %is_draft = if_abap_behv=>mk-on )
                                       ( TravelUUID = '988' BookingUUID = '003' flightprice = 200 %control-flightprice = if_abap_behv=>mk-on %is_draft = if_abap_behv=>mk-on ) ).


  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/R_TRAVEL_D.
    data failed type response for failed early /DMO/R_TRAVEL_D.

    "Setup mapped and failed according to the response to be returned
    "For create travel instances
    mapped-travel = value #( ( %cid = 'Travel_987' TravelUUID = 987 %is_draft = if_abap_behv=>mk-on  ) ( %cid = 'Travel_988' TravelUUID = 988 %is_draft = if_abap_behv=>mk-on  )  ). "Implies that a travel with travel UUid 987 and 988 was created

    "For create booking by association instances
    mapped-booking = value #( ( %cid = 'Travel_987_Booking_001' BookingUUID = '001' %is_draft = if_abap_behv=>mk-on )
                              ( %cid = 'Travel_987_Booking_002' BookingUUID = '002' %is_draft = if_abap_behv=>mk-on ) ).

    "For update booking instances
    failed-booking = value #( (  BookingUUID = '003' %update = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found %is_draft = if_abap_behv=>mk-on ) ).


  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/R_TRAVEL_D'   "can accept the entity name
                                   )->set_instances_for_create( create_travel_instances
                                   )->set_instances_for_create_ba( cba_booking_instances ).

    "For booking entity
    data(eml_booking_input) = input_config_builder_4_modify->build_entity_part( 'booking'            "can accept the alias name
                                   )->set_instances_for_update( update_booking_instances ).

    "Input configuration for EML
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input
                                                                       )->add_entity_part( eml_booking_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped )->set_failed( failed ).

  "Step 4: Configure the Modify EML via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/R_TRAVEL_D' ).
    double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output ).


  "*************Configure second EML in Code under test*************
    "create travel instances for 2nd EML
    data create_travel_instances2 type table for create /DMO/R_TRAVEL_D.
    create_travel_instances2 = value #( ( %cid = 'Travel_987' CustomerID = '000006' description = 'Travel_987' %is_draft = if_abap_behv=>mk-on ) ).

    "travel input part for 2nd EML
    data(eml_travel_input2) = input_config_builder_4_modify->build_entity_part( '/DMO/R_TRAVEL_D'   "can accept the entity name
                                   )->set_instances_for_create( create_travel_instances2
                                   )->set_instances_for_create_ba( cba_booking_instances ).

    "Input configuration for 2nd EML
    data(input2) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input2
                                                                        )->add_entity_part( eml_booking_input ).

    """""NOTE that input2 is a subset of 'input' set for first EML configuration

    "Reuse response structure

    delete mapped-travel where %cid = 'Travel_988' .
    output->set_mapped( mapped ).


    "Configure double for 2nd EML
    double->configure_call(  )->for_modify(  )->when_input( input2 )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "Two calls to same EML with different input instances.

  "CALL 1
  cut->mod_eml_w_mult_ents_nd_ops(
    exporting
      create_travel     = create_travel_instances
      create_ba_booking = cba_booking_instances
      update_booking    = update_booking_instances
   importing
      mapped   = data(mapped_cut_1)
      reported = data(reported_cut_1)
      failed   = data(failed_cut_1)
  ).

  "CALL 2
  cut->mod_eml_w_mult_ents_nd_ops(
    exporting
      create_travel     = create_travel_instances2
      create_ba_booking = cba_booking_instances
      update_booking    = update_booking_instances
   importing
      mapped   = data(mapped_cut_2)
      reported = data(reported_cut_2)
      failed   = data(failed_cut_2)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
    "Setup mapped and failed according to the response to be returned
    "For create travel instances
    mapped-travel = value #( ( %cid = 'Travel_987' TravelUUID = 987 %is_draft = if_abap_behv=>mk-on ) ( %cid = 'Travel_988' TravelUUID = 988 %is_draft = if_abap_behv=>mk-on )  ). "Implies that a travel with travel id 987 and 988 was created

    "For create booking by association instances
    mapped-booking = value #( ( %cid = 'Travel_987_Booking_001' BookingUUID = '001' %is_draft = if_abap_behv=>mk-on ) ( %cid = 'Travel_987_Booking_002' BookingUUID = '002' %is_draft = if_abap_behv=>mk-on ) ).

    "For update booking instances
    failed-booking = value #( (  BookingUUID = '003' %update = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found %is_draft = if_abap_behv=>mk-on ) ).

    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    "CALL 1 asserts
    cl_abap_unit_assert=>assert_not_initial( mapped_cut_1 ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut_1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut_1-travel ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut_1-booking ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut_1-booking ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut_1-travel[ 1 ]-%cid exp = 'Travel_987' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut_1-travel[ 2 ]-%cid exp = 'Travel_988' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut_1-booking[ 1 ]-%cid exp = 'Travel_987_Booking_001' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut_1-booking[ 2 ]-%cid exp = 'Travel_987_Booking_002' ).

    cl_abap_unit_assert=>assert_equals( act = failed_cut_1-booking[ 1 ] exp = failed-booking[ 1 ] ).


    "CALL 2 asserts
    cl_abap_unit_assert=>assert_not_initial( mapped_cut_2 ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut_2 ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut_2-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut_2-travel[ 1 ]-%cid exp = 'Travel_987' ).


  """""""""""""""""""""""""""Verifications"""""""""""""""""""""""""""""""""""""""
  "Step 5: Verify the executions on the double.

"  double->verify( )->is_called_times( times = 2 ). "Since we see 2 EML calls, the double should be invoked for both.
  double->verify( )->is_called_at_least( times = 2 ). "Since we see 2 EML calls, the double should be invoked for both.
  double->verify( )->is_called_at_least( times = 1 ).

"  double->verify( )->modify( )->is_called_times( times = 2 ). "Since the 2 EML calls are modify, the double should be invoked for both modify EML statements.
  double->verify( )->modify( )->is_called_at_least( times = 2 ). "Since the 2 EML calls are modify, the double should be invoked for both modify EML statements.
  double->verify( )->modify( )->is_called_at_least( times = 1 ).

"  double->verify( )->read( )->is_never_called( ). "Since no READ EML in code under test

  "For specific input configurations

*  double->verify( )->modify( input )->is_called_times( times = 1 ).
*  double->verify( )->modify( input2 )->is_called_times( times = 2 ). "Since input2 is a subset of input. Thus both EMLs are infact executed with input2

  double->verify( )->modify( input )->is_called_at_least( times = 1 ).
  double->verify( )->modify( input2 )->is_called_at_least( times = 2 ). "Since input2 is a subset of input. Thus both EMLs are infact executed with input2

  "Since there are 2 EMls with CREATE for travel with %cid = 'Travel_987' in both, a query to verify if CREATE with %cid = 'Travel_987' was called twice should be true
  "Input configuration for verification
  data(verification_input) = input_config_builder_4_modify->build_input_for_eml(
                                                                )->add_entity_part(
                                                                     input_config_builder_4_modify->build_entity_part( '/DMO/R_TRAVEL_D'
                                                                            )->set_instances_for_create( create_travel_instances2 ) ).

"  double->verify( )->modify( verification_input )->is_called_times( times = 2 ).
  double->verify( )->modify( verification_input )->is_called_at_least( times = 2 ).

  endmethod.

  method chaining_outputs.
  "Test Goal: Isolate multiple occurrences of the Modify EML with create operation on same instances in CUT (Code under Test) and configure responses via API.
  " 1st Modify-create EML should pass i.e. configure mapped
  " 2nd Modify-create EML should fail i.e. configure failed

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only create ).
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only create ).
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "For Create on Travel entity
   data create_travel_instances type table for create /DMO/R_TRAVEL_D.
   create_travel_instances = value #( (  %cid = 'Travel_987' AgencyID = '000111' CustomerID = '000006' description = 'Travel_987' %is_draft = if_abap_behv=>mk-on  ) ).


  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/R_TRAVEL_D.
    data failed type response for failed early /DMO/R_TRAVEL_D.

  "Fill the expected response in the response structure.
    failed-travel = value #( ( %cid = 'Travel_987' %fail-cause = if_abap_behv=>cause-unauthorized %is_draft = if_abap_behv=>mk-on ) ).
    mapped-travel = value #( ( %cid = 'Travel_987' TravelUUID = 987 %is_draft = if_abap_behv=>mk-on  ) ).

  "Step 3: Create input and output configurations for the Modify EML.
   data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
   data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts and set instances for all operations on that entity
    "For travel entity
   data(travel_part) = input_config_builder_4_modify->build_entity_part( '/DMO/R_TRAVEL_D'   "can accept the entity name
                                   )->set_instances_for_create( create_travel_instances ).

    "Input configuration for EML
   data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( travel_part ).

    "Output configuration for EML
    data(output_1) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped ).
    data(output_2) = output_config_builder_4_modify->build_output_for_eml( )->set_failed( failed ).

  "Step 4: Configure the Modify EML via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/R_TRAVEL_D' ).
    double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output_1 )->then_set_output( output_2 ).


  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
    "First EML call with Create operation on instances, returns output_1
    cut->create_travel(
        exporting
          travel   =  create_travel_instances
        importing
          mapped   = data(mapped_cut_1)
          reported = data(reported_cut_1)
          failed   = data(failed_cut_1)
      ).

    "Second EML call with Create operation on same instances, returns output_2
    cut->create_travel(
        exporting
          travel   =  create_travel_instances
        importing
          mapped   = data(mapped_cut_2)
          reported = data(reported_cut_2)
          failed   = data(failed_cut_2)
      ).

    " output 2 is repeated for all Modify Create EML occurrences for same instances after initial 2 occurrences
    cut->create_travel(
        exporting
          travel   =  create_travel_instances
        importing
          mapped   = data(mapped_cut_3)
          reported = data(reported_cut_3)
          failed   = data(failed_cut_3)
      ).
  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

  "Asserts
      cl_abap_unit_assert=>assert_initial( failed_cut_1 ).
      cl_abap_unit_assert=>assert_not_initial( mapped_cut_1 ).

      cl_abap_unit_assert=>assert_equals( act = mapped_cut_1 exp = mapped  ).

      cl_abap_unit_assert=>assert_initial( mapped_cut_2 ).
      cl_abap_unit_assert=>assert_not_initial( failed_cut_2 ).

      cl_abap_unit_assert=>assert_equals( act = failed_cut_2 exp = failed  ).

      cl_abap_unit_assert=>assert_initial( mapped_cut_3 ).
      cl_abap_unit_assert=>assert_not_initial( failed_cut_3 ).

      cl_abap_unit_assert=>assert_equals( act = failed_cut_3 exp = failed  ).

  endmethod.

  method for_times_configuration.
  "Test Goal: Isolate multiple occurrences of the modify-update EML (same instance) in CUT (Code under Test) and configure responses via API.
  " First 2 occurrences of Modify-Update EML should fail i.e. configure failed (assume the instances did not exist)
  " Next 3 occurrences of Modify-Update EML should pass i.e. configure mapped

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only update ).
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.


  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only update ).
  "For Update on Travel entity
   data update_travel_instances type table for update /DMO/R_TRAVEL_D.
   update_travel_instances = value #( ( TravelUUID = '987'  description = 'Travel_987_updated' %control-description = if_abap_behv=>mk-on
                                        %is_draft = if_abap_behv=>mk-on  ) )."Should pass

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/R_TRAVEL_D.
    data failed type response for failed early /DMO/R_TRAVEL_D.

    "Setup mapped and failed according to the response to be returned
    mapped-travel = value #( ( TravelUUID = '987'  %is_draft = if_abap_behv=>mk-on  ) ). "Implies that TravelUUID 987 was updated
    failed-travel = value #( ( TravelUUID = '987'  %is_draft = if_abap_behv=>mk-on %update = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).

  "Step 3: Create input and output configurations for the Modify EML.
   "Get input and output configuration builder.
   data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
   data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts and set instances for all operations on that entity
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/R_TRAVEL_D'
                                   )->set_instances_for_update( update_travel_instances ).

    "Input configuration for EML
    "Input should match the EML structure and instances in code under test
    data(update_input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(update_output_1) = output_config_builder_4_modify->build_output_for_eml( )->set_failed( failed ).
    data(update_output_2) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped ).

  "Step 4: Configure the Modify EML in CUT via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/R_TRAVEL_D' ).
    double->configure_call(
         )->for_modify(
         )->when_input( update_input
         )->then_set_output( update_output_1 )->for_times( 2
         )->then_set_output( update_output_2 )->for_times( 3 ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
    "First EML with update operation on instances returns update_output_1
    cut->update_travel(
    exporting
      travel   = update_travel_instances
    importing
      mapped   = data(mapped_cut_1)
      reported = data(reported_cut_1)
      failed   = data(failed_cut_1)
    ).


    "Second EML with update operation on same instances, returns update_output_1
    cut->update_travel(
    exporting
      travel   = update_travel_instances
    importing
      mapped   = data(mapped_cut_2)
      reported = data(reported_cut_2)
      failed   = data(failed_cut_2)
    ).

    "Third EML with update operation on same instances, returns update_output_2
    cut->update_travel(
    exporting
      travel   = update_travel_instances
    importing
      mapped   = data(mapped_cut_3)
      reported = data(reported_cut_3)
      failed   = data(failed_cut_3)
    ).

    "Fourth EML with update operation on same instances, returns update_output_2
    cut->update_travel(
    exporting
      travel   = update_travel_instances
    importing
      mapped   = data(mapped_cut_4)
      reported = data(reported_cut_4)
      failed   = data(failed_cut_4)
    ).

    "Fifth EML with update operation on same instances, returns update_output_2
    cut->update_travel(
    exporting
      travel   = update_travel_instances
    importing
      mapped   = data(mapped_cut_5)
      reported = data(reported_cut_5)
      failed   = data(failed_cut_5)
    ).

    "Sixth EML with update operation on same instances, returns empty because for times was configured for the output configuration.
    cut->update_travel(
    exporting
      travel   = update_travel_instances
    importing
      mapped   = data(mapped_cut_6)
      reported = data(reported_cut_6)
      failed   = data(failed_cut_6)
    ).
  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
      cl_abap_unit_assert=>assert_initial( mapped_cut_1 ).
      cl_abap_unit_assert=>assert_not_initial( failed_cut_1 ).

      cl_abap_unit_assert=>assert_equals( act = failed_cut_1 exp = failed  ).

      cl_abap_unit_assert=>assert_initial( mapped_cut_2 ).
      cl_abap_unit_assert=>assert_not_initial( failed_cut_2 ).

      cl_abap_unit_assert=>assert_equals( act = failed_cut_2 exp = failed  ).

      cl_abap_unit_assert=>assert_initial( failed_cut_3 ).
      cl_abap_unit_assert=>assert_not_initial( mapped_cut_3 ).

      cl_abap_unit_assert=>assert_equals( act = mapped_cut_3 exp = mapped  ).

      cl_abap_unit_assert=>assert_initial( failed_cut_4 ).
      cl_abap_unit_assert=>assert_not_initial( mapped_cut_4 ).

      cl_abap_unit_assert=>assert_equals( act = mapped_cut_4 exp = mapped  ).

      cl_abap_unit_assert=>assert_initial( failed_cut_5 ).
      cl_abap_unit_assert=>assert_not_initial( mapped_cut_5 ).

      cl_abap_unit_assert=>assert_equals( act = mapped_cut_5 exp = mapped  ).

      cl_abap_unit_assert=>assert_initial( failed_cut_6 ).
      cl_abap_unit_assert=>assert_initial( mapped_cut_6 ).

  endmethod.

  method ignore_input_configuration.
  "Test Goal: Isolate the Modify EML in CUT (Code under Test) and configure responses via API when the only information a user knows is the existence of a Modify EML statement in CUT,
  "without knowing the operations and instances or when user does not care about EML operations and inputs.

  "In this example, MODIFY EML statement in CUT has a CREATE on TRAVEL and CREATE_BA on Travel for booking and UPDATE on booking,
  "But this input is not known to the user

  "Step 1: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 2: Create output configuration for the Modify EML.
  "Step 3: Configure the Modify EML via configure_call and "ignore_input" API on the double.

  "Step 1: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/R_TRAVEL_D.
    data failed type response for failed early /DMO/R_TRAVEL_D.

    "Setup mapped and failed according to the response to be returned
    mapped-travel = value #( ( TravelUUID = '987' %is_draft = if_abap_behv=>mk-on ) ).
    mapped-booking = value #( ( BookingUUID = '001' %is_draft = if_abap_behv=>mk-on ) ).
    failed-booking = value #( ( BookingUUID = '002' %update = if_abap_behv=>mk-on %is_draft = if_abap_behv=>mk-on ) ).

  "Step 2: Create output configuration for the Modify EML.
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped )->set_failed( failed ).

  "Step 3: Configure the Modify EML via configure_call and "ignore_input" API on the double.
    double =  environment->get_test_double( '/DMO/R_TRAVEL_D' ).
    double->configure_call(  )->for_modify(  )->ignore_input(  )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
   data create_travel_instances type table for create /DMO/R_TRAVEL_D.
   create_travel_instances = value #( ( %cid = 'Travel_987' CustomerID = '000006' description = 'Travel_987' %is_draft = if_abap_behv=>mk-on ) ).

  "For Creating Booking by Association on Travel entity
   data cba_booking_instances type table for create /DMO/R_TRAVEL_D\_Booking.
   cba_booking_instances = value #( ( %cid_ref = 'Travel_987' TravelUUID = '987' %is_draft = if_abap_behv=>mk-on
                                      %target = value #( (  %cid = 'Travel_987_Booking_001' %is_draft = if_abap_behv=>mk-on
                                                            connectionid = 'C1' %control-connectionid = if_abap_behv=>mk-on
                                                            CustomerID = '000006' %control-CustomerID = if_abap_behv=>mk-on
                                                            FlightDate = cl_abap_context_info=>get_system_date( ) + 10 %control-FlightDate = if_abap_behv=>mk-on
                                                            bookingstatus = 'B' %control-bookingstatus = if_abap_behv=>mk-on )
                                                           )
                                                                                                      "Creation of bookings for TravelUUID 987 should pass
                                    ) ).
  "For Update on Booking entity
   data update_booking_instances type table for update /DMO/R_TRAVEL_D\\booking.
   update_booking_instances = value #( ( TravelUUID = '988' BookingUUID = '003' flightprice = 200 %control-flightprice = if_abap_behv=>mk-on %is_draft = if_abap_behv=>mk-on ) ).

    cut->mod_eml_w_mult_ents_nd_ops(
      exporting
        create_travel     = create_travel_instances
        create_ba_booking = cba_booking_instances
        update_booking    = update_booking_instances
      importing
        mapped   = data(mapped_cut)
        reported = data(reported_cut)
        failed   = data(failed_cut)
    ).
  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-booking ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-booking ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-TravelUUID exp = conv sysuuid_x16( '987' ) ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-%key-BookingUUID exp = conv sysuuid_x16( '001' ) ).

    cl_abap_unit_assert=>assert_equals( act = failed_cut-booking[ 1 ]-%tky-BookingUUID exp = conv sysuuid_x16( '002' ) ).


  endmethod.

  method isolate_actions.
  "Test Goal: Isolate Modify EML with Action in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only Action ).
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "For accept travel action on Travel entity
   data accept_travel_act_insts type table for action import /DMO/R_TRAVEL_D~acceptTravel.
   accept_travel_act_insts = value #( ( TravelUUID = '987' %is_draft = if_abap_behv=>mk-on ) "Should pass
                                      ( TravelUUID = '988' %is_draft = if_abap_behv=>mk-on ) ).
                                                                                "Should fail, assuming Travel 988 does not exist

  "For deduct discount action on Travel entity
   data deduct_discount_act_insts type table for action import /DMO/R_TRAVEL_D~deductDiscount.
   deduct_discount_act_insts = value #( ( TravelUUID = '987' %param-discount_percent = 10 %is_draft = if_abap_behv=>mk-on ) )."Should pass

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped_accept_travel type response for mapped early /DMO/R_TRAVEL_D.
    data mapped_deductDiscount type response for mapped early /DMO/R_TRAVEL_D.
    data failed type response for failed early /DMO/R_TRAVEL_D.
    data result_accept_travel type table for action result /DMO/R_TRAVEL_D~acceptTravel.
    data result_deductDiscount type table for action result /DMO/R_TRAVEL_D~deductDiscount.

    "Setup mapped and failed according to the response to be returned
    mapped_accept_travel-travel = value #( ( TravelUUID = '987'  %is_draft = if_abap_behv=>mk-on ) ). "Implies that action accept_travel on travelUUid 987 was successful
    result_accept_travel = value #( ( %tky-TravelUUID = '987' %is_draft = if_abap_behv=>mk-on %param-OverallStatus = 'A' %param-TotalPrice = 100 ) ).
    failed-travel = value #( ( TravelUUID = '988'  %is_draft = if_abap_behv=>mk-on %action-acceptTravel = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).
                                                                                   "AcceptTravel failed as the Travel 988 does not exist

    mapped_deductDiscount-travel = value #( ( TravelUUID = '987'  %is_draft = if_abap_behv=>mk-on ) ). "Implies that action deductDiscount on travelUUid 987 was successful
    result_deductDiscount = value #( ( %tky-TravelUUID = '987' %is_draft = if_abap_behv=>mk-on %param-TotalPrice = 90 ) ). "Assuming total price was 100 before and 10% discount is done.


  "Step 3: Create input and output configurations for the Modify EML for Action accept Travel.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts and set instances for all operations on that entity
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/R_TRAVEL_D'
                                   )->set_instances_for_action( instances = accept_travel_act_insts ).

    "Input configuration for EML
    "Input should match the EML structure and instances in code under test
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped_accept_travel )->set_failed( failed )->set_result_for_action( result = result_accept_travel ).

  "Step 4: Configure the Modify EML in CUT via configure_call API on the double for Action accept Travel.
    double =  environment->get_test_double( '/DMO/R_TRAVEL_D' ).
    double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output ).

    "For deduct discount action on travelUUID 987
    data(eml_travel_input_deduct_discnt) = input_config_builder_4_modify->build_entity_part( '/DMO/R_TRAVEL_D'
                                   )->set_instances_for_action( instances = deduct_discount_act_insts ).

    "Input configuration for EML
    "Input should match the EML structure and instances in code under test
    data(input_deduct_discount) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input_deduct_discnt ).
    clear failed.
    output->set_failed( failed )->set_mapped( mapped_deductDiscount )->set_result_for_action( result_deductdiscount ).
    double->configure_call(  )->for_modify(  )->when_input( input_deduct_discount )->then_set_output( output ).


  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "Accept Travel Action
  cut->accept_travel_action(
    exporting
      accept_travel_input = accept_travel_act_insts
    importing
      mapped              = data(mapped_accept_travel_cut)
      reported            = data(reported_accept_travel_cut)
      failed              = data(failed_accept_travel_cut)
      result              = data(result_accept_travel_cut)
  ).


  "Deduct Discount Action
  cut->deduct_discount_action(
    exporting
      deduct_discount_input = deduct_discount_act_insts
    importing
      mapped              = data(mapped_deduct_discount_cut)
      reported            = data(reported_deduct_discount_cut)
      failed              = data(failed_deduct_discount_cut)
      result              = data(result_deduct_discount_cut)
  ).
  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    "Accept Travel asserts
    cl_abap_unit_assert=>assert_not_initial( mapped_accept_travel_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_accept_travel_cut ).
    cl_abap_unit_assert=>assert_initial( reported_accept_travel_cut ).
    cl_abap_unit_assert=>assert_not_initial( result_accept_travel_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_accept_travel_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_accept_travel_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( result_accept_travel_cut ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_accept_travel_cut-travel[ 1 ]-TravelUUID exp = conv sysuuid_x16( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = failed_accept_travel_cut-travel[ 1 ]-TravelUUID exp = conv sysuuid_x16( '988' ) ).
    cl_abap_unit_assert=>assert_equals( act = result_accept_travel_cut[ 1 ]-TravelUUID exp = conv sysuuid_x16( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = result_accept_travel_cut[ 1 ]-%param-TotalPrice exp = 100 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_accept_travel_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = failed_accept_travel_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

    "Deduct discount asserts
    cl_abap_unit_assert=>assert_not_initial( mapped_deduct_discount_cut ).
    cl_abap_unit_assert=>assert_initial( failed_deduct_discount_cut ).
    cl_abap_unit_assert=>assert_not_initial( result_deduct_discount_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_deduct_discount_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( result_deduct_discount_cut ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_deduct_discount_cut-travel[ 1 ]-TravelUUID exp = conv sysuuid_x16( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = result_deduct_discount_cut[ 1 ]-TravelUUID exp = conv sysuuid_x16( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = result_deduct_discount_cut[ 1 ]-%param-TotalPrice exp = 90 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_deduct_discount_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = result_deduct_discount_cut[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_deep_create.
  "Test Goal: Isolate a deep create Modify EML with creation for all entity instances of BO in one EML in CUT (Code under Test) by configuring responses via API.
  "And verify if the executions were as per the configuration

  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "For Create on Travel entity
   data create_travel_instances type table for create /DMO/R_TRAVEL_D.
   create_travel_instances = value #( ( %cid = 'CID_100' %is_draft = if_abap_behv=>mk-on %data-customerid  = '9876' AgencyID = '70006' begindate = '20180101' enddate = '20180101' ) ).

  "For Creating Booking by Association on Travel entity
   data cba_booking_instances type table for create /DMO/R_TRAVEL_D\_Booking.
   cba_booking_instances = value #( ( %cid_ref = 'CID_100' %is_draft = if_abap_behv=>mk-on
                                      %target = value #( (  %cid = 'CID_200' customerid = '9876' flightprice = 100
                                                            %is_draft = if_abap_behv=>mk-on  ) )  )  ).

  "For Creating booking_supplement by association on Booking entity
   data cba_booking_suppl_instances type table for create /DMO/R_TRAVEL_D\\booking\_BookingSupplement.
   cba_booking_suppl_instances = value #( (  %cid_ref = 'CID_200' %is_draft = if_abap_behv=>mk-on
                                             %target  = value #( ( %cid = 'CID_300' %data-supplementid  = '007'  BookSupplPrice  = 100
                                                                   %is_draft = if_abap_behv=>mk-on ) ) ) ).


  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/R_TRAVEL_D.

    "Setup mapped and failed according to the response to be returned
    "For create travel instances
    mapped-travel = value #( ( TravelUUID = '987'  %cid = 'CID_100' %is_draft = if_abap_behv=>mk-on ) ). "Implies that a travel with travelUUid 987 was created for 1st instance

    "For create booking by association instances
    mapped-booking = value #( ( %cid = 'CID_200' BookingUUID = '001' %is_draft = if_abap_behv=>mk-on ) ).

    "For create booking supp by association instances
    mapped-bookingsupplement = value #( ( %cid = 'CID_300' BookSupplUUID = '01' %is_draft = if_abap_behv=>mk-on ) ).


  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/R_TRAVEL_D'   "can accept the entity name
                                   )->set_instances_for_create( create_travel_instances
                                   )->set_instances_for_create_ba( cba_booking_instances ).

    "For booking entity
    data(eml_booking_input) = input_config_builder_4_modify->build_entity_part( 'booking'            "can accept the alias name
                                   )->set_instances_for_create_ba( cba_booking_suppl_instances ).

    "Input configuration for EML
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input
                                                                       )->add_entity_part( eml_booking_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped ).

  "Step 4: Configure the Modify EML via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/R_TRAVEL_D' ).
    double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  cut->deep_create_travel_bo(
   importing
      mapped   = data(mapped_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-booking ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-bookingsupplement ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-%cid exp = 'CID_100' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-%cid exp = 'CID_200' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-bookingsupplement[ 1 ]-%cid exp = 'CID_300' ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-TravelUUID exp = conv sysuuid_x16( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-BookingUUID exp = conv sysuuid_x16( '001' ) ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-bookingsupplement[ 1 ]-BookSupplUUID exp = conv sysuuid_x16( '01' ) ).

  endmethod.

  method isolate_delete.
  "Test Goal: Isolate Modify EML with delete in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only delete ).
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "For delete on Travel entity
   data delete_travel_instances type table for delete /DMO/R_TRAVEL_D.
   delete_travel_instances = value #( ( TravelUUID = '987' %is_draft = if_abap_behv=>mk-on ) "Should pass
                                      ( TravelUUID = '988' %is_draft = if_abap_behv=>mk-on ) ).
                                                                                "Should fail, assuming Travel 988 does not exist

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/R_TRAVEL_D.
    data failed type response for failed early /DMO/R_TRAVEL_D.

    "Setup mapped and failed according to the response to be returned
    mapped-travel = value #( ( TravelUUID = '987'  %is_draft = if_abap_behv=>mk-on ) ). "Implies that travelUUid 987 was deleted
    failed-travel = value #( ( TravelUUID = '988'  %is_draft = if_abap_behv=>mk-on %delete = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).
                                                                                   "delete failed as the Travel 988 does not exist

  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts and set instances for all operations on that entity
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/R_TRAVEL_D'
                                   )->set_instances_for_delete( delete_travel_instances ).

    "Input configuration for EML
    "Input should match the EML structure and instances in code under test
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped )->set_failed( failed ).

  "Step 4: Configure the Modify EML in CUT via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/R_TRAVEL_D' ).
    double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  cut->delete_travel(
    exporting
      travel   = delete_travel_instances
    importing
      mapped   = data(mapped_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-TravelUUID exp = conv sysuuid_x16( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-TravelUUID exp = conv sysuuid_x16( '988' ) ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_read.
  "Test Goal: Isolate the read EML with read operation in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Read EML (Here only read ).
  "Step 2: Define and set up the response structures to be returned for READ EML in CUT
  "Step 3: Create input and output configurations for the READ EML.
  "Step 4: Configure the READ EML via configure_call API on the double.

  "Step 1: Setup test data instances for read operation in READ EML.
  "For read on travel
   data read_travel_instances type table for read import /DMO/R_TRAVEL_D.
   read_travel_instances = value #( ( %is_draft = if_abap_behv=>mk-on TravelUUID = '987' ) "travel returned
                                    ( %is_draft = if_abap_behv=>mk-on TravelUUID = '988' ) ). "read should fail for 988 assuming the instance does not exist

  "Step 2: Define and set up the response structures to be returned for Read EML in CUT
    data result type table for read result /DMO/R_TRAVEL_D.
    result = value #( ( %is_draft = if_abap_behv=>mk-on TravelUUID = '987' %data-BookingFee = 10 TotalPrice = 100 OverallStatus = 'A' ) ).
    data failed type response for failed early /DMO/R_TRAVEL_D.
    failed-travel = value #( ( %is_draft = if_abap_behv=>mk-on TravelUUID = '988' ) ).

  "Step 3: Create input and output configurations for Read EML.
   data(input_config_builder_4_read) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_read(  ).
   data(output_config_builder_4_read) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_read( ).

    "Create input for all entity parts
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_read->build_entity_part( '/DMO/R_TRAVEL_D'   "can accept the entity name
                                   )->set_instances_for_read( read_travel_instances ).

    "input and output configuration
    data(input) = input_config_builder_4_read->build_input_for_eml(  )->add_entity_part( eml_travel_input ).
    data(output) = output_config_builder_4_read->build_output_for_eml( )->set_failed( failed )->set_result_for_read( result ).

  "Step 4: Configure the Read EML via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/R_TRAVEL_D' ).
    double->configure_call(  )->for_read(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  cut->read_travel(
    exporting
      travel   = read_travel_instances
    importing
      result   = data(result_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    cl_abap_unit_assert=>assert_not_initial( result_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( result_cut ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-TravelUUID exp = conv sysuuid_x16( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-TotalPrice exp = 100 ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-TravelUUID exp = conv sysuuid_x16( '988' ) ).

    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_read_ba.
  "Test Goal: Isolate the read by association (read_ba) operation in READ EML in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Read EML (Here only read_ba ).
  "Step 2: Define and set up the response structures to be returned for read_ba operation in READ EML in CUT
  "Step 3: Create input and output configurations for the READ EML.
  "Step 4: Configure the READ EML via configure_call API on the double.

  "Step 1: Setup test data instances for read_ba operation in READ EML.
  "For read booking by association on travel
   data rba_booking_instances type table for read import /DMO/R_TRAVEL_D\_Booking.
   rba_booking_instances = value #( ( %is_draft = if_abap_behv=>mk-on TravelUUID = '987' ) "Bookings returned
                                    ( %is_draft = if_abap_behv=>mk-on TravelUUID = '988' ) ). "read_ba should fail for 988 assuming the instance does not exist

  "Step 2: Define and set up the response structures to be returned for Read EML in CUT
    data result type table for read result /DMO/R_TRAVEL_D\_Booking.
    result = value #( ( %is_draft = if_abap_behv=>mk-on TravelUUID = '987' BookingUUID = '001' %data-FlightPrice = 10  )
                      ( %is_draft = if_abap_behv=>mk-on TravelUUID = '987' BookingUUID = '002' %data-FlightPrice = 20  ) ).
    data failed type response for failed early /DMO/R_TRAVEL_D.
    failed-travel = value #( ( %is_draft = if_abap_behv=>mk-on TravelUUID = '988' ) ).

  "Step 3: Create input and output configurations for Read EML.
   data(input_config_builder_4_read) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_read(  ).
   data(output_config_builder_4_read) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_read( ).

    "Create input for all entity parts
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_read->build_entity_part( 'TRAVEL'   "can accept the entity name
                                   )->set_instances_for_read_ba( rba_booking_instances ).

  "Create input and output configuration for the READ EML in CUT
    data(input) = input_config_builder_4_read->build_input_for_eml(  )->add_entity_part( eml_travel_input ).
    data(output) = output_config_builder_4_read->build_output_for_eml( )->set_failed( failed )->set_result_for_read_ba( result = result  assoc_name = '_Booking' source_entity_name = 'TRAVEL' ).

  "Step 4: Configure the Read EML via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/R_TRAVEL_D' ).
    double->configure_call(  )->for_read(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  cut->read_booking_by_assoc(
    exporting
      travel   = rba_booking_instances
    importing
      result   = data(result_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
      links    = data(links_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    cl_abap_unit_assert=>assert_not_initial( result_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( result_cut ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-TravelUUID exp = conv sysuuid_x16( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-BookingUUID exp = conv sysuuid_x16( '001' ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 2 ]-BookingUUID exp = conv sysuuid_x16( '002' ) ).

    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-TravelUUID exp = conv sysuuid_x16( '988' ) ).

    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_update.
  "Test Goal: Isolate Modify EML with Update in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only update ).
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "For Update on Travel entity
   data update_travel_instances type table for update /DMO/R_TRAVEL_D.
   update_travel_instances = value #( ( TravelUUID = '987' %is_draft = if_abap_behv=>mk-on description = 'Travel_987_updated' %control-description = if_abap_behv=>mk-on ) "Should pass
                                      ( TravelUUID = '988' %is_draft = if_abap_behv=>mk-on description = 'Travel_988_updated' %control-description = if_abap_behv=>mk-on ) ).
                                                                                "Should fail, assuming Travel 988 does not exist

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/R_TRAVEL_D.
    data failed type response for failed early /DMO/R_TRAVEL_D.

    "Setup mapped and failed according to the response to be returned
    mapped-travel = value #( ( TravelUUID = '987'  %is_draft = if_abap_behv=>mk-on ) ). "Implies that travelUUid 987 was updated
    failed-travel = value #( ( TravelUUID = '988'  %is_draft = if_abap_behv=>mk-on %update = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).
                                                                                   "Update failed as the Travel 988 does not exist

  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts and set instances for all operations on that entity
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/R_TRAVEL_D'
                                   )->set_instances_for_update( update_travel_instances ).

    "Input configuration for EML
    "Input should match the EML structure and instances in code under test
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped )->set_failed( failed ).

  "Step 4: Configure the Modify EML in CUT via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/R_TRAVEL_D' ).
    double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  cut->update_travel(
    exporting
      travel   = update_travel_instances
    importing
      mapped   = data(mapped_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-TravelUUID exp = conv sysuuid_x16( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-TravelUUID exp = conv sysuuid_x16( '988' ) ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method partial_input_configuration.
  "Test Goal: Isolate the Modify EML in CUT (Code under Test) and configure responses via API when only partial input/structure of the Modify EML in CUT is known to the user,
  "or user only cares about partial input/structure of the EML in CUT

  "In this example, MODIFY EML statement in CUT has a CREATE on TRAVEL and CREATE_BA on TRAVEL for booking and CREATE_BA on BOOKING for bookingsupplement,
  "But only the input for CREATE on TRAVEL is known to the user

  "Step 1: Setup test data instances for CREATE on TRAVEL in Modify EML.
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call and "when_input_partial_input" API on the double.

  "Step 1: Setup test data instances for CREATE on TRAVEL in Modify EML.
  "For Create on Travel entity
   data create_travel_instances type table for create /DMO/R_TRAVEL_D.
   create_travel_instances = value #( ( %cid = 'CID_100' %data-CustomerID = '9876' AgencyID = '70006' begindate = '20180101' enddate  = '20180101' %is_draft = if_abap_behv=>mk-on ) ).

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/R_TRAVEL_D.

    "Setup mapped according to the response to be returned
    mapped-travel = value #( ( %cid = 'CID_100' TravelUUID = '987' %is_draft = if_abap_behv=>mk-on ) ). "Create on 987 passed

  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/R_TRAVEL_D'
                                   )->set_instances_for_create( create_travel_instances ).

    "Input configuration for EML
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped ).

  "Step 4: Configure the Modify EML via configure_call and "when_input_partial_input" API on the double.
    double =  environment->get_test_double( '/DMO/R_TRAVEL_D' ).
    double->configure_call(  )->for_modify(  )->when_partial_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
    cut->deep_create_travel_bo(
     importing
        mapped   = data(mapped_cut_1)
        reported = data(reported_cut_1)
        failed   = data(failed_cut_1)
    ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    cl_abap_unit_assert=>assert_not_initial( mapped_cut_1 ).
    cl_abap_unit_assert=>assert_initial( failed_cut_1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut_1-travel ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut_1-travel[ 1 ]-TravelUUID exp = conv sysuuid_x16( '987' ) ).

  endmethod.

endclass.

"! <p class="shorttext synchronized" lang="en">Demonstrate use of TXBUFDBL variant to isolate EML dependencies</p>
"! <p> This class demonstrates the use of RAP BO test double framework to isolate BO dependencies via EML involving draft instances in code under test by creating test
"! doubles with a generic implementation and transactional buffer double support.</p>
"!<p> With this variant, the framework provides a generic mock of CRUD operations for Modify and Read EML statements and provides a transactional buffer double
"! which holds all the instances recorded via Create EML and manipulated/queried by other operations of Modify/Read EML statements.</p>
"! <ul>
"! <li>Operation names in test method names, signify EML operations in CUT (Code Under Test).</li>
"! <li>Involved instances are mostly draft instances i.e. with %is_draft field marked with if_abap_behv=>mk-on.</li>
"! <li>The dependent BO is /dmo/r_travel_d and the code under test is its consumer {@link /dmo/tc_travel_d_bo_consumer}</li>
"! <li>The test data should be recorded via Create EML (if required), such that the EML operations in CUT are evaluated
"! as required.</li>
"! <li>The framework takes care of providing the filled response structures (mapped, reported, failed, result)</li>
"! <ul><li>In case the returned responses are not satisfactory, this variant also provides complimentary APIs to set custom responses. <br/>
"! For example check the method {@link ltcl_using_txbufdbl_variant.Meth:isolate_create_set_failed}</li></ul>
"! </ul>
class ltcl_using_txbufdbl_variant definition final for testing
  duration short
  risk level harmless.

  private section.

    "Create
    methods isolate_create_to_pass for testing raising cx_static_check.
    methods isolate_create_to_fail_dup for testing raising cx_static_check.
    methods isolate_create_set_mapped for testing raising cx_static_check.
    methods isolate_create_set_failed for testing raising cx_static_check.

    "Create_ba
    methods isolate_create_ba for testing raising cx_static_check.
    methods isolate_create_ba_set_mapped for testing raising cx_static_check.
    methods isolate_create_ba_set_failed for testing raising cx_static_check.

    "Update
    methods isolate_update_to_pass for testing raising cx_static_check.
    methods isolate_update_to_fail_no_inst for testing raising cx_static_check.
    methods isolate_update_set_mapped for testing raising cx_static_check.
    methods isolate_update_set_failed for testing raising cx_static_check.

    "Delete
    methods isolate_delete_to_pass for testing raising cx_static_check.
    methods isolate_delete_to_fail_no_inst for testing raising cx_static_check.
    methods isolate_delete_set_failed for testing raising cx_static_check.

    "Read
    methods isolate_read for testing raising cx_static_check.
    methods isolate_read_set_failed for testing raising cx_static_check.

    "Read_ba
    methods isolate_read_ba for testing raising cx_static_check.
    methods isolate_read_ba_set_failed for testing raising cx_static_check.

    "Using insert_test_data API, if Create is internal
    methods insert_data_w_internal_create for testing raising cx_static_check.


    "Operations with CID/CID_REF
    "methods modify_entities_operations for testing raising cx_static_check.

    methods setup.
    class-methods class_setup.
    class-methods class_teardown.
    class-data environment type ref to if_botd_txbufdbl_bo_test_env.
    class-data cut type ref to /dmo/tc_travel_d_bo_consumer.

endclass.

class ltcl_using_txbufdbl_variant implementation.

  method class_setup.
  "Create doubles for BO '/DMO/R_TRAVEL_D'.
  "a. Prepare environment configuration with bdef dependencies for which doubles are to be created
    data(env_config) = cl_botd_txbufdbl_bo_test_env=>prepare_environment_config( )->set_bdef_dependencies( bdef_dependencies = value #( ( '/DMO/R_TRAVEL_D' ) )
                                                                                  )->handle_draft( bdef_dependencies = value #( ( '/DMO/R_TRAVEL_D' ) ) ).

  "b. Create the test doubles for bdefs from the environment configuration and get the environment instance
  environment = cl_botd_txbufdbl_bo_test_env=>create( environment_config = env_config ).

  "Code under test
  cut = new #(  ).
  endmethod.

  method class_teardown.
  "destroy the environment and all the test doubles
    environment->destroy(  ).
  endmethod.

  method setup.
  "Clear the configurations done on all test doubles from the environment
    environment->clear_doubles(  ).
  endmethod.


  method isolate_create_to_fail_dup.
  "Test Goal: Isolate the Modify EML with Create draft in CUT (Code under Test) to fail by configuring the transactional buffer double.
  "The Create in code under test should fail due to already existing instance with the same key, as duplicates are not allowed.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML which will have the same key as the instance in code under test.
  "Set numbering support on double (if required).
    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ). "Keys start from 1 for create in test method.

    modify entities of /dmo/r_travel_d
     entity travel
       create fields ( CustomerID description )
        with value #( ( %CID = 'Configured_Travel_1' %is_draft = if_abap_behv=>mk-on CustomerID = '000006' description = 'Configured Travel 1'  ) ) "Since only one instance is inserted, this instance will have key 1.
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-traveluuid exp = conv sysuuid_x16( 1 ) ).

  "Set a new numbering support object on double, in which keys will again start from key 1.
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ). "Keys again start from 1 for create in cut.

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For Create on Travel entity
   data create_travel_instances type table for create /dmo/r_travel_d.
   create_travel_instances = value #( ( %CID = 'CUT_Travel_1' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel 1 in code under test'
                                        %control-AgencyID = if_abap_behv=>mk-on %control-CustomerID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) ).

   cut->create_travel(
    exporting
      travel   =  create_travel_instances
    importing
      mapped   = data(mapped_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "The TXBUFDBL variant, tries to create the instance with key/read-only fields provided via the set_fields_handler.
    "If the operation fails, RAP BO TDF also returns the failed response for eml in code under test.

    "Asserts
    cl_abap_unit_assert=>assert_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%cid exp = 'CUT_Travel_1' ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-traveluuid exp = mapped-travel[ 1 ]-traveluuid ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_create_to_pass.
  "Test Goal: Isolate the Modify EML with Create draft in CUT (Code under Test) to pass by configuring the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Set numbering support on double (if required). (not required if external numbering is enabled)

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "No record step needed.
   "The create in CUT will perform the create operation on the buffer directly.

  "Step 2: Set numbering support on double (if required).
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For Create on Travel entity
   data create_travel_instances type table for create /dmo/r_travel_d.
   create_travel_instances = value #( ( %CID = 'Travel_1' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel 1'
                                        %control-AgencyID = if_abap_behv=>mk-on %control-CustomerID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) ).

   cut->create_travel(
    exporting
      travel   =  create_travel_instances
    importing
      mapped   = data(mapped_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "The TXBUFDBL variant, creates the instance with key/read-only fields provided via the set_fields_handler and inserts into the transactional buffer double
    "and returns a mapped structure.

    "Asserts
    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-%cid exp = 'Travel_1' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

    "Read the data to assert create
    read entity /dmo/r_travel_d
     from value #( ( traveluuid = mapped_cut-travel[ 1 ]-traveluuid %is_draft = if_abap_behv=>mk-on %control-CustomerID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

    cl_abap_unit_assert=>assert_equals( act = lines( result_read ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-CustomerID exp = '000006' ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-description exp = 'Travel 1' ).
    cl_abap_unit_assert=>assert_initial( act = result_read[ 1 ]-totalprice ).
    cl_abap_unit_assert=>assert_initial( act = result_read[ 1 ]-AgencyID ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_create_set_mapped.
  "Test Goal: Isolate the Modify EML with Create in CUT (Code under Test) to pass by configuring additional behavior on the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Setup test data instance for create EML in code under test.
  "Step 3: Define and set up the mapped response structure to be returned for Create EML on test data instance in CUT
  "Step 4: set_mapped using configure_addtional_behavior API on double to configure CREATE EML on test data instance in code under test to pass.
          "The keys for the instance will be set from the mapped structure provided in "set_mapped" complimentary API.
          "set_mapped will be successful only if the create can actually be performed on the current transactional buffer double state.
            "Ex. If instance with traveluuid 987 already exists in the buffer double, setting mapped with traveluuid 987 for another instance will fail as duplicates are not allowed.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "No record step needed.
   "The create in CUT will perform the create operation on the buffer directly.

  "Step 2: Setup test data instance for create EML in code under test.
   data create_travel_instances type table for create /dmo/r_travel_d.
   create_travel_instances = value #( ( %cid = 'Travel_987' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel_987' )
                                      ( %cid = 'Travel_988' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel_988' ) ).
                                      "Configured Input instance should match exactly with the instance in code under test, (%control structure matching is ignored)

  "Step 3: Define and set up the mapped response structure to be returned for Create EML on test data instance in CUT
    data mapped_987 type response for mapped early /dmo/r_travel_d.
    data mapped_988 type response for mapped early /dmo/r_travel_d.

    mapped_987-travel = value #( ( %cid = 'Travel_987' %is_draft = if_abap_behv=>mk-on traveluuid = '987' ) ).  "Create a draft travel with traveluuid 987
    mapped_988-travel = value #( ( %cid = 'Travel_988' %is_draft = if_abap_behv=>mk-on traveluuid = '988' ) ).  "Create a draft travel with traveluuid 988


  "Step 4: set_mapped using configure_addtional_behavior API on double to configure CREATE EML on an instance in code under test to pass.

    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).

    "For travel 987
    data(input) = double->create_modify_input_config(  )->set_instance( create_travel_instances[ 1 ] ).
                              "(Unlike the other variant i.e. MOCKEMLAPI variant of RAP BO TDF, the input configuration here only consists of an input instance on which operation is performed)
    data(output) = double->create_modify_output_config( )->set_mapped( mapped = mapped_987 ).
    double->configure_additional_behavior(  )->for_modify_create(  )->when_input( input )->then_set_output( output ).

    "For travel 988
    input->set_instance( create_travel_instances[ 2 ] ).
    output->set_mapped( mapped = mapped_988 ).
    double->configure_additional_behavior(  )->for_modify_create(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For Create on Travel entity
   data create_travel_instances_cut type table for create /dmo/r_travel_d.
   create_travel_instances_cut = value #( ( %CID = 'Travel_987' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel_987'   "AgencyID should not be filled as it is not marked in %control
                                             %control-CustomerID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on )
                                          ( %CID = 'Travel_988' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel_988'
                                             %control-AgencyID = if_abap_behv=>mk-on %control-CustomerID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) ) .

   cut->create_travel(
    exporting
      travel   =  create_travel_instances_cut
    importing
      mapped   = data(mapped_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-travel ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-%cid exp = 'Travel_987' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 2 ]-%cid exp = 'Travel_988' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 2 ]-%is_draft exp = if_abap_behv=>mk-on ).

    "The TXBUFDBL variant, creates the instance with key fields provided via the set_mapped API and inserts into the transactional buffer double
    "Read the data to assert create
    read entity /dmo/r_travel_d
     from value #( ( traveluuid = mapped_cut-travel[ 1 ]-traveluuid %is_draft = if_abap_behv=>mk-on %control-AgencyID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on )
                   ( traveluuid = mapped_cut-travel[ 2 ]-traveluuid %is_draft = if_abap_behv=>mk-on %control-AgencyID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

    cl_abap_unit_assert=>assert_equals( act = lines( result_read ) exp = 2 ).
    cl_abap_unit_assert=>assert_initial( result_read[ 1 ]-AgencyID ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-description exp = 'Travel_987' ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 2 ]-AgencyID exp = '000111' ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 2 ]-description exp = 'Travel_988' ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 2 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.


  method isolate_create_set_failed.
  "Test Goal: Isolate the Modify EML with Create in CUT (Code under Test) to fail by configuring additional behavior on the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Setup test data instance for create EML in code under test.
  "Step 3: Define and set up the failed response structure to be returned for Create EML on test data instance in CUT
  "Step 4: set_failed using configure_addtional_behavior API on double to configure CREATE EML on test data instance in code under test to fail.
          "set_failed API can be used to force an operation to fail.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "No record step needed.
   "The create in CUT will perform the create operation on the buffer directly.

  "Step 2: Setup test data instance for create EML in code under test.
   data create_travel_instances type table for create /dmo/r_travel_d.
   create_travel_instances = value #( ( %cid = 'Travel_987' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel_987' ) ).
                                      "Configured Input instance should match exactly with the instance in code under test, (%control structure matching is ignored)

  "Step 3: Define and set up the failed response structure to be returned for Create EML on test data instance in CUT
    data failed type response for failed early /dmo/r_travel_d.
    failed-travel = value #( ( %cid = 'Travel_987' %is_draft = if_abap_behv=>mk-on ) ).  "Create travel_987 fails, assuming CustomerID does not exists

  "Step 4: set_failed using configure_addtional_behavior API on double to configure CREATE EML on an instance in code under test to fail.

    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).

    data(input) = double->create_modify_input_config(  )->set_instance( create_travel_instances[ 1 ] ).
                              "(Unlike the other variant i.e. MOCKEMLAPI variant of RAP BO TDF, the input configuration here only consists of an input instance on which operation is performed)
    data(output) = double->create_modify_output_config( )->set_failed( failed ).
    double->configure_additional_behavior(  )->for_modify_create(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For Create on Travel entity
   data create_travel_instances_cut type table for create /dmo/r_travel_d.
   create_travel_instances_cut = value #( ( %CID = 'Travel_987' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel_987'
                                             %control-CustomerID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) ) .

   cut->create_travel(
    exporting
      travel   =  create_travel_instances_cut
    importing
      mapped   = data(mapped_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    cl_abap_unit_assert=>assert_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%cid exp = 'Travel_987' ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_update_to_pass.
  "Test Goal: Isolate the Modify EML with Update for travel in CUT (Code under Test) by configuring the transactional buffer double.
  "Update EML in CUT should successfully update the instance recorded in Framework's Transactional Buffer Double

  "Step 1: Set numbering support on double (if required) via set_fields_handler API. (set_mapped can also be used to define the key fields for an instance)
  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).

  "Step 1: Set numbering support on double (if required).
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).


  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Record a travel instance which is to be updated. ( Will be recorded in Framework's Buffer and an ID will be returned by the framework according to fields_handler )
    modify entities of /dmo/r_travel_d
     entity travel
       create from value #( ( %cid = 'Travel_1' %is_draft = if_abap_behv=>mk-on
                              AgencyID = '000111' CustomerID = '000006' description = 'Travel 1'
                              %control-AgencyID = if_abap_behv=>mk-on
                              %control-CustomerID = if_abap_behv=>mk-on
                              %control-description = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

     "according to the fields_handler, key assigned to travel instance will be 1.
    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-traveluuid exp = conv sysuuid_x16( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  """"""""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""
    " The Framework will evaluate the Update and update the instance recorded in step 2 in Buffer.

    cut->update_travel(
      exporting
        travel   = value #( ( %key-traveluuid = 1 %is_draft = if_abap_behv=>mk-on
                             Description = 'Travel Updated'
                             %control-Description = if_abap_behv=>mk-on ) )
      importing
        mapped   = data(mapped_update)
        reported = data(reported_update)
        failed   = data(failed_update)
    ).
  """"""""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""

    cl_abap_unit_assert=>assert_initial( mapped_update ). "mapped for update is not returned
    cl_abap_unit_assert=>assert_initial( failed_update ).
    cl_abap_unit_assert=>assert_initial( reported_update ).

    "Check if the instance was updated.
    read entity /dmo/r_travel_d
     from value #( ( traveluuid = mapped-travel[ 1 ]-traveluuid %is_draft = if_abap_behv=>mk-on
                     %control-description = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

     cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 'Travel Updated' act = result_read[ 1 ]-Description ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_create_ba.
  "Test Goal: Isolate the Modify EML with Create bookings By Association for travel in CUT (Code under Test) by configuring the transactional buffer double.
   " Creat_BA should pass for travel 1 and insert the booking instances in Transactional Buffer Double of Framework.
   " Creat_BA should fail for travel 2 assuming travel 2 does not exist.

  "Step 1: Set numbering support on double (if required) via set_fields_handler API.
  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).

  "Step 1: Set numbering support on double (if required).
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).

  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Record a travel instance for which booking is to be created. ( Will be recorded in Framework's Buffer and an ID will be returned by the framework according to fields_handler )
    modify entities of /dmo/r_travel_d
     entity travel
       create from value #( ( %cid = 'Travel_1' %is_draft = if_abap_behv=>mk-on
                              AgencyID = '000111' CustomerID = '000006' description = 'Travel 1'
                              %control-AgencyID = if_abap_behv=>mk-on
                              %control-CustomerID = if_abap_behv=>mk-on
                              %control-description = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

     "according to the fields_handler, key assigned to travel instance will be 1.
    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-traveluuid exp = conv sysuuid_x16( 1 ) ).


  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""
   data cba_booking_instances type table for create /dmo/r_travel_d\_Booking.
   cba_booking_instances = value #( ( traveluuid = 1 %is_draft = if_abap_behv=>mk-on
                                      %target = value #( (  %cid = 'Travel_1_Booking_1' %is_draft = if_abap_behv=>mk-on
                                                            connectionid = 'C1' %control-connectionid = if_abap_behv=>mk-on
                                                            CustomerID = '000006' %control-CustomerID = if_abap_behv=>mk-on "For mandatory fields, control structure should be marked.
                                                            FlightDate = cl_abap_context_info=>get_system_date( ) + 10 %control-FlightDate = if_abap_behv=>mk-on
                                                            bookingstatus = 'B' %control-bookingstatus = if_abap_behv=>mk-on )
                                                         (  %cid = 'Travel_1_Booking_2' %is_draft = if_abap_behv=>mk-on
                                                            connectionid = 'C1' %control-connectionid = if_abap_behv=>mk-on
                                                            CustomerID = '000007' %control-CustomerID = if_abap_behv=>mk-on
                                                            FlightDate = cl_abap_context_info=>get_system_date( ) + 10 %control-FlightDate = if_abap_behv=>mk-on
                                                            bookingstatus = 'B' %control-bookingstatus = if_abap_behv=>mk-on
                                                            ) )  )
                                                                                                      "Creation of bookings for traveluuid 1 should pass
                                     ( traveluuid = 2 %is_draft = if_abap_behv=>mk-on
                                       %target = value #( (  %cid = 'Travel_2_Booking_1' %is_draft = if_abap_behv=>mk-on
                                                            connectionid = 'C1' %control-connectionid = if_abap_behv=>mk-on
                                                            CustomerID = '000006' %control-CustomerID = if_abap_behv=>mk-on
                                                            FlightDate = cl_abap_context_info=>get_system_date( ) + 10 %control-FlightDate = if_abap_behv=>mk-on
                                                            bookingstatus = 'B' %control-bookingstatus = if_abap_behv=>mk-on ) )
                                     ) ). "Creation of booking for traveluuid 1 should fail, assuming traveluuid 1 did not exist


      ""Create booking by association
     cut->create_booking_by_assoc(
       exporting
         booking_by_travel = cba_booking_instances
      importing
        mapped   = data(mapped_cut)
        reported = data(reported_cut)
        failed   = data(failed_cut)
    ).

  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""

  "Asserts
    "For success cases
    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-booking ) exp = 2 ).

    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = conv sysuuid_x16( 1 ) act = mapped_cut-booking[ 1 ]-BookingUUID ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = conv sysuuid_x16( 2 ) act = mapped_cut-booking[ 2 ]-BookingUUID ).

    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = 'Travel_1_Booking_1' act = mapped_cut-booking[ 1 ]-%cid ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = 'Travel_1_Booking_2' act = mapped_cut-booking[ 2 ]-%cid ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 2 ]-%is_draft exp = if_abap_behv=>mk-on ).

    "For failed cases
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = conv sysuuid_x16( 2 ) act = failed_cut-travel[ 1 ]-traveluuid ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-not_found act = failed_cut-travel[ 1 ]-%fail-cause ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-booking ) exp = 1 ).

    cl_abap_unit_assert=>assert_initial( failed_cut-booking[ 1 ]-BookingUUID ).
    cl_abap_unit_assert=>assert_equals( exp = 'Travel_2_Booking_1' act = failed_cut-booking[ 1 ]-%cid  ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-dependency act = failed_cut-booking[ 1 ]-%fail-cause ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-booking[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

    "Read the data to assert create by association
    read entity /dmo/r_travel_d
      by \_Booking
     from value #( ( traveluuid = mapped-travel[ 1 ]-traveluuid %is_draft = if_abap_behv=>mk-on %control-bookingstatus = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

    cl_abap_unit_assert=>assert_equals( act = lines( result_read ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-BookingUUID exp = conv sysuuid_x16( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 2 ]-BookingUUID exp = conv sysuuid_x16( 2 ) ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-bookingstatus exp = 'B' ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 2 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method insert_data_w_internal_create.
  "Demonstrate the use of insert_test_data API to insert instances during test configuration, when CREATE operation is internal for the BO.
  """CAN ONLY BE USED WHEN CREATE IS INTERNAL
  """If create is internal for your use case, the following commented code still demonstrates the usage of insert_test_data API.
  """Please implement your test on same lines.

   "Assume update EML in CUT (Code Under Test) which we want to isolate.

*   "Step 1: Record instance. Assume create is internal. Thus CREATE EML statement can't be used to insert test data in transactional buffer double
*   data travel_instances type table for create /dmo/r_travel_d.
*   travel_instances = value #( ( traveluuid = 987 %is_draft = if_abap_behv=>mk-on
*                              begindate = '20180101' enddate = '20180101'
*                              totalprice = 100
*                              %control-traveluuid = if_abap_behv=>mk-on
*                              %control-begindate = if_abap_behv=>mk-on
*                              %control-totalprice = if_abap_behv=>mk-on )
*                          ).
*
*  "Using insert_test_data API to insert test data assuming create is internal.
*    data(double) = environment->get_test_double( root_name = '/dmo/r_travel_d' ).
*    double->insert_test_data( instances  = travel_instances ). "Insert the instance with travelid 987
*
*  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""
*    cut->update_travel(
*      exporting
*        travel   = value #( ( traveluuid = 987
*                              totalprice = 200
*                              %is_draft = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on ) )
*      importing
*        mapped   = data(mapped_cut)
*        reported = data(reported_cut)
*        failed   = data(failed_cut)
*  ).
*  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""
*
*  "Asserts
*    cl_abap_unit_assert=>assert_initial( mapped_cut ).
*    cl_abap_unit_assert=>assert_initial( failed_cut ).
*    cl_abap_unit_assert=>assert_initial( reported_cut ).
*
*    "Check if the instance was updated.
*    read entity /dmo/r_travel_d
*     from value #( ( traveluuid = mapped_cut-travel[ 1 ]-traveluuid %is_draft = if_abap_behv=>mk-on
*                     %control-totalprice = if_abap_behv=>mk-on ) )
*     result   data(result_read)
*     reported data(reported_read)
*     failed   data(failed_read).
*
*     cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 200 act = result_read[ 1 ]-totalprice ).
*
  endmethod.

  method isolate_delete_to_fail_no_inst.
  "Test Goal: Isolate the Modify EML with delete for travel in CUT (Code under Test) by configuring the transactional buffer double to fail.
  "delete EML in CUT should fail assuming the instance does not exist in Framework's Transactional Buffer Double

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Nothing to be recorded.

  """"""""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""
    " The Framework will evaluate the delete and fail as the instance doesn't exist in Buffer. The failed response is returned by the framework.

    cut->delete_travel(
      exporting
        travel   = value #( ( %key-traveluuid = 1 %is_draft = if_abap_behv=>mk-on ) )
      importing
        mapped   = data(mapped_delete)
        reported = data(reported_delete)
        failed   = data(failed_delete)
    ).
  """"""""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""

    cl_abap_unit_assert=>assert_initial( mapped_delete ).
    cl_abap_unit_assert=>assert_not_initial( failed_delete ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_delete-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = conv sysuuid_x16( 1 ) act = failed_delete-travel[ 1 ]-traveluuid ).
    cl_abap_unit_assert=>assert_equals( act = failed_delete-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_delete_to_pass.
  "Test Goal: Isolate the Modify EML with Delete for travel in CUT (Code under Test) to pass by configuring the transactional buffer double.
  "Delete EML in CUT should successfully delete the instance recorded in Framework's Transactional Buffer Double

  "Step 1: Set numbering support on double (if required) via set_fields_handler API. (set_mapped can also be used to define the key fields for an instance)
  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).

  "Step 1: Set numbering support on double (if required).
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).


  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Record a travel instance which is to be deleted. ( Will be recorded in Framework's Buffer and an ID will be returned by the framework according to fields_handler )
   "Use Deep Create to insert a travel and 2 bookings for that travel.
    modify entities of /dmo/r_travel_d
     entity travel
             create fields ( CustomerID begindate enddate description ) with
                         value #( ( %cid        = 'Travel_1'    " Preliminary ID for new travel instance
                                    %is_draft = if_abap_behv=>mk-on
                                    CustomerID  = '000006'
                                    description = 'Travel 1' ) )
             create by \_booking fields ( connectionid CustomerID bookingstatus FlightDate ) with
                         value #( ( %cid_ref  = 'Travel_1'      "refers to the root (travel instance)
                                    %is_draft = if_abap_behv=>mk-on
                                    %target   = value #( ( %cid = 'Travel_1_Booking_1' " Preliminary ID for new booking instance
                                                           %is_draft = if_abap_behv=>mk-on
                                                           connectionid = '1234'
                                                           CustomerID = '000006'
                                                           FlightDate = cl_abap_context_info=>get_system_date( ) + 10
                                                           bookingstatus = 'B' )
                                                         ( %cid = 'Travel_1_Booking_2' " Preliminary ID for new booking instance
                                                           %is_draft = if_abap_behv=>mk-on
                                                           connectionid = '2345'
                                                           CustomerID = '000006'
                                                           FlightDate = cl_abap_context_info=>get_system_date( ) + 10
                                                           bookingstatus = 'B' ) ) ) )
       MAPPED   data(mapped_test_data)
       FAILED   data(failed_test_data)
       REPORTED data(reported_test_data).

      "Check if the instances are actually inserted.
     "according to the fields_handler, key assigned to travel instance will be 1 and for two bookings id 1 and 2 resp.
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-travel[ 1 ]-traveluuid exp = conv sysuuid_x16( 1 ) ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 1 ]-BookingUUID exp = conv sysuuid_x16( 1 ) ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 2 ]-BookingUUID exp = conv sysuuid_x16( 2 ) ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 2 ]-%is_draft exp = if_abap_behv=>mk-on ).

     read entity /dmo/r_travel_d
       from value #( ( traveluuid = mapped_test_data-travel[ 1 ]-traveluuid  %is_draft = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) )
         result   data(result_read_travel)
       by \_Booking from value #( ( traveluuid = mapped_test_data-travel[ 1 ]-traveluuid %is_draft = if_abap_behv=>mk-on %control-connectionid = if_abap_behv=>mk-on ) )
         result   data(result_read_booking)
       reported data(reported_read)
       failed   data(failed_read).


     cl_abap_unit_assert=>assert_equals( act = result_read_travel[ 1 ]-traveluuid exp = conv sysuuid_x16( 1 ) ).
     cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 'Travel 1' act = result_read_travel[ 1 ]-description ).
     cl_abap_unit_assert=>assert_equals( act = result_read_travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

     cl_abap_unit_assert=>assert_equals( act = result_read_booking[ 1 ]-traveluuid exp = conv sysuuid_x16( 1 ) ).
     cl_abap_unit_assert=>assert_equals( act = result_read_booking[ 1 ]-BookingUUID exp = conv sysuuid_x16( 1 ) ).
     cl_abap_unit_assert=>assert_equals( act = result_read_booking[ 2 ]-traveluuid exp = conv sysuuid_x16( 1 ) ).
     cl_abap_unit_assert=>assert_equals( act = result_read_booking[ 2 ]-BookingUUID exp = conv sysuuid_x16( 2 ) ).
     cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = '1234' act = result_read_booking[ 1 ]-connectionid ).
     cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = '2345' act = result_read_booking[ 2 ]-connectionid ).
     cl_abap_unit_assert=>assert_equals( act = result_read_booking[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
     cl_abap_unit_assert=>assert_equals( act = result_read_booking[ 2 ]-%is_draft exp = if_abap_behv=>mk-on ).

  """"""""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""
    " The Framework will evaluate the delete and delete the instance (and its associated instances) from the buffer double recorded in step 2 in Buffer.

      cut->delete_travel(
        exporting
          travel   = value #( ( %key-traveluuid = 1 %is_draft = if_abap_behv=>mk-on ) )
       importing
          mapped   = data(mapped_delete)
          reported = data(reported_delete)
          failed   = data(failed_delete)
    ).
  """"""""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""

    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    cl_abap_unit_assert=>assert_initial( mapped_delete ). "mapped for delete is not returned
    cl_abap_unit_assert=>assert_initial( failed_delete ).
    cl_abap_unit_assert=>assert_initial( reported_delete ).

    "Check if the instance was deleted.
     read entity /dmo/r_travel_d
       from value #( ( traveluuid = mapped_test_data-travel[ 1 ]-traveluuid %is_draft = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) )
         result   data(result_read_travel_deleted)
       by \_Booking from value #( ( traveluuid = mapped_test_data-travel[ 1 ]-traveluuid %is_draft = if_abap_behv=>mk-on %control-connectionid = if_abap_behv=>mk-on ) )
         result   data(result_read_booking_deleted)
       reported data(reported_read_deleted)
       failed   data(failed_read_deleted).

    cl_abap_unit_assert=>assert_initial( result_read_travel_deleted ).
    cl_abap_unit_assert=>assert_initial( result_read_booking_deleted ).

  endmethod.

  method isolate_update_to_fail_no_inst.
  "Test Goal: Isolate the Modify EML with Update for travel in CUT (Code under Test) by configuring the transactional buffer double.
  "Update EML in CUT should fail assuming the instance does not exist in Framework's Transactional Buffer Double

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Nothing to be recorded.

  """"""""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""
    " The Framework will evaluate the Update and fail as the instance doesn't exist in Buffer. The failed response is returned by the framework.

    cut->update_travel(
      exporting
        travel   = value #( ( %key-traveluuid = 1 %is_draft = if_abap_behv=>mk-on
                             description = 'Travel Updated'
                             %control-description = if_abap_behv=>mk-on ) )
      importing
        mapped   = data(mapped_update)
        reported = data(reported_update)
        failed   = data(failed_update)
    ).
  """"""""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""

    cl_abap_unit_assert=>assert_initial( mapped_update ).
    cl_abap_unit_assert=>assert_not_initial( failed_update ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_update-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = conv sysuuid_x16( 1 ) act = failed_update-travel[ 1 ]-traveluuid ).
    cl_abap_unit_assert=>assert_equals( act = failed_update-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

    "Check if the instance was not updated.
    read entity /dmo/r_travel_d
     from value #( ( traveluuid = failed_update-travel[ 1 ]-traveluuid %is_draft = if_abap_behv=>mk-on
                     %control-totalprice = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

    cl_abap_unit_assert=>assert_initial( result_read ).

  endmethod.

  method isolate_create_ba_set_failed.
  "Test Goal: Isolate the Modify EML with Create booking by association on Travel in CUT (Code under Test) to fail by configuring additional behavior on the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Setup test data instance for Create by association EML in code under test.
  "Step 3: Define and set up the failed response structure to be returned for Create by association EML on test data instance in CUT
  "Step 4: set_failed using configure_addtional_behavior API on double to configure Create by association EML on test data instance in code under test to fail.
          "set_failed API can be used to force an operation to fail.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "No record step needed.
   "The create by association in CUT will return failed, configured via set_failed.

  "Step 2: Setup test data instance for Create by association EML in code under test.
   data cba_booking_instances type table for create /dmo/r_travel_d\_Booking.
   cba_booking_instances = value #( ( traveluuid = 987  %is_draft = if_abap_behv=>mk-on
                                      %target = value #( (  %cid = 'Travel_987_Booking_001'  %is_draft = if_abap_behv=>mk-on
                                                            connectionid = 'C1' %control-connectionid = if_abap_behv=>mk-on
                                                            CustomerID = '000006' %control-CustomerID = if_abap_behv=>mk-on "For mandatory fields, control structure should be marked.
                                                            FlightDate = cl_abap_context_info=>get_system_date( ) + 10 %control-FlightDate = if_abap_behv=>mk-on
                                                            bookingstatus = 'B' %control-bookingstatus = if_abap_behv=>mk-on ) ) ) ).


  "Step 3: Define and set up the failed response structure to be returned for Create by association EML on test data instance in CUT.
    data failed type response for failed early /dmo/r_travel_d.
    failed-travel = value #( ( traveluuid = '987'  %is_draft = if_abap_behv=>mk-on  %fail-cause = if_abap_behv=>cause-not_found ) ).
    failed-booking = value #( ( %cid = 'Travel_987_Booking_001'  %is_draft = if_abap_behv=>mk-on  %fail-cause = if_abap_behv=>cause-dependency ) ).


  "Step 4: set_failed using configure_addtional_behavior API on double to configure Create by association EML on an instance in code under test to fail.

    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).

    data(input) = double->create_modify_input_config(  )->set_instance( cba_booking_instances[ 1 ] ).
                                                                                "Note that the input instance should contain only one instance for %target
    data(output) = double->create_modify_output_config( )->set_failed( failed ).

    double->configure_additional_behavior(  )->for_modify_create_ba(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For Create by association on Travel entity

    cut->create_booking_by_assoc(
    exporting
      booking_by_travel   =  cba_booking_instances
    importing
      mapped   = data(mapped_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    cl_abap_unit_assert=>assert_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = conv sysuuid_x16( '987' ) act = failed_cut-travel[ 1 ]-traveluuid ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-not_found act = failed_cut-travel[ 1 ]-%fail-cause ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-booking ) exp = 1 ).
    cl_abap_unit_assert=>assert_initial( failed_cut-booking[ 1 ]-BookingUUID ).
    cl_abap_unit_assert=>assert_equals( exp = 'Travel_987_Booking_001' act = failed_cut-booking[ 1 ]-%cid  ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-dependency act = failed_cut-booking[ 1 ]-%fail-cause ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-booking[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_create_ba_set_mapped.
  "Test Goal: Isolate the Modify EML with Create booking by association on Travel in CUT (Code under Test) to pass by configuring additional behavior on the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Setup test data instance for Create by association EML in code under test.
  "Step 3: Define and set up the mapped response structure to be returned for Create by association EML on test data instance in CUT
  "Step 4: set_mapped using configure_addtional_behavior API on double to configure Create by association EML on test data instance in code under test to pass.
          "The keys for the instance will be set from the mapped structure provided in "set_mapped" complimentary API.
          "set_mapped will be successful only if the create by association can actually be performed on the current transactional buffer double state.
            "Ex. If booking instance for traveluuid 988 is to be created but traveluuid 988 does not already exists in the buffer double,
                    "setting mapped with traveluuid 988 and some BookingUUID will fail as the source instance does not exist.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Insert a travel with traveluuid 987
   "The create in test method will perform the create operation on the buffer double.
   data create_travel_instances type table for create /dmo/r_travel_d.
   create_travel_instances = value #( ( %cid = 'Travel_987' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel_987' ) ).

   "using set mapped API, set the key for the travel as 987
   data mapped_travel_987 type response for mapped early /dmo/r_travel_d.
   mapped_travel_987-travel = value #( ( %cid = 'Travel_987' %is_draft = if_abap_behv=>mk-on traveluuid = 987 ) ).  "Create a travel with traveluuid 987

   "configure the double for create in test method"
   data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).
   data(input) = double->create_modify_input_config(  )->set_instance( create_travel_instances[ 1 ] ).
   data(output) = double->create_modify_output_config( )->set_mapped( mapped = mapped_travel_987 ).
   double->configure_additional_behavior(  )->for_modify_create(  )->when_input( input )->then_set_output( output ).

   "Insert the travel with Create EML
    modify entities of /dmo/r_travel_d
     entity travel
       create fields ( AgencyID CustomerID description )
        with value #( ( %cid = 'Travel_987' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel_987'  ) ) "Since set_mapped is used for the instance,
                                                                                                                                      "key 987 will be assigned to the instance and inserted
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-traveluuid exp = conv sysuuid_x16( 987 ) ).
    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).


  "Step 2: Setup test data instance for Create by association EML in code under test.
   data cba_booking_instances type table for create /dmo/r_travel_d\_Booking.
   cba_booking_instances = value #( ( traveluuid = 987 %is_draft = if_abap_behv=>mk-on
                                      %target = value #( (  %cid = 'Travel_987_Booking_001' %is_draft = if_abap_behv=>mk-on
                                                            connectionid = 'C1' %control-connectionid = if_abap_behv=>mk-on
                                                            CustomerID = '000006' %control-CustomerID = if_abap_behv=>mk-on "For mandatory fields, control structure should be marked.
                                                            FlightDate = cl_abap_context_info=>get_system_date( ) + 10 %control-FlightDate = if_abap_behv=>mk-on
                                                            bookingstatus = 'B' %control-bookingstatus = if_abap_behv=>mk-on ) ) ) ).


  "Step 3: Define and set up the mapped response structure to be returned for Create by association EML on test data instance in CUT, with the key to be assigned for the instance.
    data mapped_booking_001 type response for mapped early /dmo/r_travel_d.
    mapped_booking_001-booking = value #( ( %cid = 'Travel_987_Booking_001' %is_draft = if_abap_behv=>mk-on BookingUUID =  001 ) ).  "Create a booking for traveluuid 987 with BookingUUID 001


  "Step 4: set_mapped using configure_addtional_behavior API on double to configure Create by association EML on an instance in code under test to pass.

    input = double->create_modify_input_config(  )->set_instance( cba_booking_instances[ 1 ] ).
                                                                        "Note that the input instance should contain only one instance for %target
    output = double->create_modify_output_config( )->set_mapped( mapped = mapped_booking_001 ).
    double->configure_additional_behavior(  )->for_modify_create_ba(  )->when_input( input )->then_set_output( output ).


  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For Create by association on Travel entity

    cut->create_booking_by_assoc(
    exporting
      booking_by_travel   =  cba_booking_instances
    importing
      mapped   = data(mapped_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-booking ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-%cid exp = 'Travel_987_Booking_001' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-BookingUUID exp = conv sysuuid_x16( 001 ) ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

    "The TXBUFDBL variant, creates the instance with key fields provided via the set_mapped API and inserts into the transactional buffer double
    "Read the data to assert create by association
    read entity /dmo/r_travel_d
      by \_Booking
     from value #( ( traveluuid = mapped-travel[ 1 ]-traveluuid %is_draft = if_abap_behv=>mk-on %control-bookingstatus = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

    cl_abap_unit_assert=>assert_equals( act = lines( result_read ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-BookingUUID exp = conv sysuuid_x16( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-bookingstatus exp = 'B' ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_update_set_failed.
  "Test Goal: Isolate the Modify EML with Update on Travel in CUT (Code under Test) to fail by configuring additional behavior on the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Setup test data instance for Update EML in code under test.
  "Step 3: Define and set up the failed response structure to be returned for Update EML on test data instance in CUT
  "Step 4: set_failed using configure_addtional_behavior API on double to configure Update EML on test data instance in code under test to fail.
          "set_failed API can be used to force an operation to fail.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "No record step needed.
   "The Update operation on instance in CUT will return failed, configured via set_failed.

  "Step 2: Setup test data instance for Update EML statement in code under test.
   data update_travel_instances type table for update /dmo/r_travel_d.
   update_travel_instances = value #( ( traveluuid = 987 %is_draft = if_abap_behv=>mk-on
                              description = 'Travel Updated'
                              %control-description = if_abap_behv=>mk-on )
                          ).


  "Step 3: Define and set up the failed response structure to be returned for update EML on test data instance in CUT.
    data failed type response for failed early /dmo/r_travel_d.
    failed-travel = value #( ( traveluuid = 987 %is_draft = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found %update = if_abap_behv=>mk-on ) ).


  "Step 4: set_failed using configure_addtional_behavior API on double to configure update EML on an instance in code under test to fail.

    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).
    data(input) = double->create_modify_input_config(  )->set_instance( update_travel_instances[ 1 ] ).
                                                                                "Note that the input instance should contain only one instance
    data(output) = double->create_modify_output_config( )->set_failed( failed ).
    double->configure_additional_behavior(  )->for_modify_update(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For update on Travel entity

    cut->update_travel(
      exporting
        travel   = update_travel_instances
      importing
        mapped   = data(mapped_cut)
        reported = data(reported_cut)
        failed   = data(failed_cut)
    ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    cl_abap_unit_assert=>assert_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = conv sysuuid_x16( 987 ) act = failed_cut-travel[ 1 ]-traveluuid ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-not_found act = failed_cut-travel[ 1 ]-%fail-cause ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_delete_set_failed.
  "Test Goal: Isolate the Modify EML with delete on Travel in CUT (Code under Test) to fail by configuring additional behavior on the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Setup test data instance for delete EML in code under test.
  "Step 3: Define and set up the failed response structure to be returned for delete EML on test data instance in CUT
  "Step 4: set_failed using configure_addtional_behavior API on double to configure delete EML on test data instance in code under test to fail.
          "set_failed API can be used to force an operation to fail.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "No record step needed.
   "The delete operation on instance in CUT will return failed, configured via set_failed.

  "Step 2: Setup test data instance for delete EML statement in code under test.
   data delete_travel_instances type table for delete /dmo/r_travel_d.
   delete_travel_instances = value #( ( traveluuid = 987 %is_draft = if_abap_behv=>mk-on ) ).

  "Step 3: Define and set up the failed response structure to be returned for update EML on test data instance in CUT.
    data failed type response for failed early /dmo/r_travel_d.
    failed-travel = value #( ( traveluuid = 987 %is_draft = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found %delete = if_abap_behv=>mk-on ) ).

  "Step 4: set_failed using configure_addtional_behavior API on double to configure delete EML on an instance in code under test to fail.

    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).
    data(input) = double->create_modify_input_config(  )->set_instance( delete_travel_instances[ 1 ] ).
                                                                                "Note that the input instance should contain only one instance
    data(output) = double->create_modify_output_config( )->set_failed( failed ).
    double->configure_additional_behavior(  )->for_modify_delete(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For delete on Travel entity

    cut->delete_travel(
      exporting
        travel   = delete_travel_instances
      importing
        mapped   = data(mapped_cut)
        reported = data(reported_cut)
        failed   = data(failed_cut)
    ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    cl_abap_unit_assert=>assert_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = conv sysuuid_x16( 987 ) act = failed_cut-travel[ 1 ]-traveluuid ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-not_found act = failed_cut-travel[ 1 ]-%fail-cause ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.


  method isolate_update_set_mapped.
  "Test Goal: Isolate the Modify EML with Update on Travel in CUT (Code under Test) to pass by configuring additional behavior on the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Setup test data instance for Update EML in code under test.
  "Step 3: Define and set up the mapped response structure to be returned for Update EML on test data instance in CUT
  "Step 4: set_mapped using configure_addtional_behavior API on double to configure Update EML on test data instance in code under test to pass.
          "set_mapped will be successful only if the update can actually be performed on the current transactional buffer double state.
            "Ex. If instance for traveluuid 988 does not already exists in the buffer double,
                    "setting mapped with traveluuid 988 for update will fail as instance to be updated does not exist.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Insert a travel with traveluuid 987
   "The create in test method will perform the create operation on the buffer using the key set using set_mapped. (alternatively "set_fields_handler" can be used to set numbering support)
   data create_travel_instances type table for create /dmo/r_travel_d.
   create_travel_instances = value #( ( %cid = 'Travel_987' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel_987' ) ).

   "using set mapped API, set the key for the travel as 987
   data mapped_travel_987 type response for mapped early /dmo/r_travel_d.
   mapped_travel_987-travel = value #( ( %cid = 'Travel_987' %is_draft = if_abap_behv=>mk-on traveluuid = 987 ) ).  "Create a travel with traveluuid 987

   "configure the double for create in test method (below)"
   data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).
   data(input) = double->create_modify_input_config(  )->set_instance( create_travel_instances[ 1 ] ).
   data(output) = double->create_modify_output_config( )->set_mapped( mapped = mapped_travel_987 ).
   double->configure_additional_behavior(  )->for_modify_create(  )->when_input( input )->then_set_output( output ).

   "Insert the travel with Create EML
    modify entities of /dmo/r_travel_d
     entity travel
       create fields ( AgencyID CustomerID description )
        with value #( ( %cid = 'Travel_987' %is_draft = if_abap_behv=>mk-on AgencyID = '000111' CustomerID = '000006' description = 'Travel_987'  ) ) "Since set_mapped is used for the instance,
                                                                                                                                      "key 987 will be assigned to the instance and inserted
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-traveluuid exp = conv sysuuid_x16( 987 ) ).
    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).


  "Step 2: Setup test data instance for Update EML in code under test.
   data update_travel_instances type table for update /dmo/r_travel_d.
   update_travel_instances = value #( ( traveluuid = 987 %is_draft = if_abap_behv=>mk-on
                              description = 'Travel Updated'
                              %control-description = if_abap_behv=>mk-on )
                          ).


  "Step 3: Define and set up the mapped response structure to be returned for update EML on test data instance in CUT.
    data mapped_update type response for mapped early /dmo/r_travel_d.
    mapped_update-travel = value #( ( traveluuid = 987 %is_draft = if_abap_behv=>mk-on ) ).  "Update traveluuid 987 created and inserted in the buffer double above.


  "Step 4: set_mapped using configure_addtional_behavior API on double to configure Update EML on an instance in code under test to pass.

    input = double->create_modify_input_config(  )->set_instance( update_travel_instances[ 1 ] ).
    output = double->create_modify_output_config( )->set_mapped( mapped = mapped_update ).
    double->configure_additional_behavior(  )->for_modify_update(  )->when_input( input )->then_set_output( output ).


  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For Update on Travel entity

    cut->update_travel(
      exporting
        travel   = update_travel_instances
      importing
        mapped   = data(mapped_cut)
        reported = data(reported_cut)
        failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.

    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = conv sysuuid_x16( 987 ) act = mapped_cut-travel[ 1 ]-traveluuid ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

    "Check if the instance was updated.
    read entity /dmo/r_travel_d
     from value #( ( traveluuid = mapped_cut-travel[ 1 ]-traveluuid %is_draft = if_abap_behv=>mk-on
                     %control-description = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

     cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 'Travel Updated' act = result_read[ 1 ]-description ).
     cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-TravelUUID exp = mapped_cut-travel[ 1 ]-traveluuid ).
     cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_read.
  "Test Goal: Isolate the Read EML with read for travel in CUT (Code under Test) by configuring the transactional buffer double.

  "Step 1: Set numbering support on double (if required) via set_fields_handler API (for create during test configuration).
  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).

  "Step 1: Set numbering support on double (if required).
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).

  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Record a travel instance which is to be read in code under test. ( Will be recorded in Framework's Buffer and an ID will be returned by the framework according to fields_handler )
    modify entities of /dmo/r_travel_d
     entity travel
       create from value #( ( %cid = 'Travel_1' %is_draft = if_abap_behv=>mk-on
                              AgencyID = '000111' CustomerID = '000006' description = 'Travel 1'
                              %control-AgencyID = if_abap_behv=>mk-on
                              %control-CustomerID = if_abap_behv=>mk-on
                              %control-description = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

     "according to the fields_handler, key assigned to travel instance will be 1.
    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-traveluuid exp = conv sysuuid_x16( 1 ) ).


  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""
      cut->read_travel(
        exporting
          travel   =  value #( ( traveluuid =  mapped-travel[ 1 ]-traveluuid      "Read should be successful as travel 1 exists
                                 %is_draft = if_abap_behv=>mk-on  %control-description = if_abap_behv=>mk-on )
                               ( traveluuid =  2     "Travel 2 does not exist, read should fail
                                 %is_draft = if_abap_behv=>mk-on  %control-description = if_abap_behv=>mk-on )  )
        importing
          result   = data(result_cut)
          reported = data(reported_cut)
          failed   = data(failed_cut)
      ).

  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""

    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    cl_abap_unit_assert=>assert_not_initial( result_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( result_cut ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-traveluuid exp = conv sysuuid_x16( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-description exp = 'Travel 1' ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-traveluuid exp = conv sysuuid_x16( 2 ) ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_read_ba.
  "Test Goal: Isolate the Read EML with read booking by association operation on travel in CUT (Code under Test) by configuring the transactional buffer double.

  "Step 1: Set numbering support on double (if required) via set_fields_handler API (for create during test configuration).
  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).

  "Step 1: Set numbering support on double (if required).
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).

  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Record a travel instance and bookings for it  which will be read in code under test by association. ( Will be recorded in Framework's Buffer and an ID will be returned by the framework according to fields_handler )
   "Use Deep Create to insert a travel and 2 bookings for that travel.
    modify entities of /dmo/r_travel_d
     entity travel
             create fields ( CustomerID begindate enddate description ) with
                         value #( ( %cid        = 'Travel_1'    " Preliminary ID for new travel instance
                                    %is_draft = if_abap_behv=>mk-on
                                    CustomerID  = '000006'
                                    description = 'Travel 1' ) ) "Travel status open
             create by \_booking fields ( connectionid CustomerID bookingstatus FlightDate ) with
                         value #( ( %cid_ref  = 'Travel_1'      "refers to the root (travel instance)
                                    %is_draft = if_abap_behv=>mk-on
                                    %target   = value #( ( %cid = 'Travel_1_Booking_1' " Preliminary ID for new booking instance
                                                           %is_draft = if_abap_behv=>mk-on
                                                           connectionid = '1234'
                                                           CustomerID = '000006'
                                                           FlightDate = cl_abap_context_info=>get_system_date( ) + 10
                                                           bookingstatus = 'B' )
                                                         ( %cid = 'Travel_1_Booking_2' " Preliminary ID for new booking instance
                                                           %is_draft = if_abap_behv=>mk-on
                                                           connectionid = '2345'
                                                           CustomerID = '000006'
                                                           FlightDate = cl_abap_context_info=>get_system_date( ) + 10
                                                           bookingstatus = 'B' ) ) ) )
       MAPPED   data(mapped_test_data)
       FAILED   data(failed_test_data)
       REPORTED data(reported_test_data).

      "Check if the instances are actually inserted.
     "according to the fields_handler, key assigned to travel instance will be 1 and for two bookings id 1 and 2 resp.
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-travel[ 1 ]-traveluuid exp = conv sysuuid_x16( 1 ) ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 1 ]-BookingUUID exp = conv sysuuid_x16( 1 ) ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 2 ]-BookingUUID exp = conv sysuuid_x16( 2 ) ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 2 ]-%is_draft exp = if_abap_behv=>mk-on ).


  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""
      cut->read_booking_by_assoc(
        exporting
          travel   = value #( ( traveluuid =  mapped_test_data-travel[ 1 ]-traveluuid      "Read_ba should be successful as travel 1 exists and booking instances should be returned
                                %is_draft = if_abap_behv=>mk-on   %control-connectionid = if_abap_behv=>mk-on )
                               ( traveluuid =  2     "Travel 2 does not exist, read_ba should fail
                                 %is_draft = if_abap_behv=>mk-on  %control-connectionid = if_abap_behv=>mk-on )  )
        importing
          result   = data(result_cut)
          reported = data(reported_cut)
          failed   = data(failed_cut)
          links    = data(links_cut)
      ).

  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""

    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    cl_abap_unit_assert=>assert_not_initial( result_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( result_cut ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-traveluuid exp = conv sysuuid_x16( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-BookingUUID exp = conv sysuuid_x16( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 2 ]-BookingUUID exp = conv sysuuid_x16( 2 ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 2 ]-%is_draft exp = if_abap_behv=>mk-on ).

    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-traveluuid exp = conv sysuuid_x16( 2 ) ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

    cl_abap_unit_assert=>assert_equals( act = lines( links_cut ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = links_cut[ 1 ]-source-traveluuid exp = conv sysuuid_x16( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = links_cut[ 1 ]-target-BookingUUID exp = conv sysuuid_x16( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = links_cut[ 2 ]-target-BookingUUID exp = conv sysuuid_x16( 2 ) ).

  endmethod.

  method isolate_read_set_failed.
  "Test Goal: Isolate the Read EML with read on Travel in CUT (Code under Test) to fail by configuring additional behavior on the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Setup test data instance for read EML in code under test.
  "Step 3: Define and set up the failed response structure to be returned for read EML on test data instance in CUT
  "Step 4: set_failed using configure_addtional_behavior API on double to configure read EML on test data instance in code under test to fail.
          "set_failed API can be used to force an operation to fail.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "No record step needed.
   "The read operation on instance in CUT will return failed, configured via set_failed.

  "Step 2: Setup test data instance for read EML statement in code under test.
   data read_travel_instances type table for read import /dmo/r_travel_d.
   read_travel_instances = value #( ( traveluuid = 987 %is_draft = if_abap_behv=>mk-on ) ).


  "Step 3: Define and set up the failed response structure to be returned for read EML on test data instance in CUT.
    data failed type response for failed early /dmo/r_travel_d.
    failed-travel = value #( ( traveluuid = 987 %is_draft = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).


  "Step 4: set_failed using configure_addtional_behavior API on double to configure read EML on an instance in code under test to fail.

    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).
    data(input) = double->create_read_input_config(  )->set_instance( read_travel_instances[ 1 ] ).
                                                                                "Note that the input instance should contain only one instance
    data(output) = double->create_read_output_config( )->set_failed( failed ).
    double->configure_additional_behavior(  )->for_read(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For read on Travel entity

      cut->read_travel(
        exporting
          travel   =  value #( ( traveluuid =  987      "Read should return failed as configured
                                 %is_draft = if_abap_behv=>mk-on  %control-description = if_abap_behv=>mk-on ) )
        importing
          result   = data(result_cut)
          reported = data(reported_cut)
          failed   = data(failed_cut)
      ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    cl_abap_unit_assert=>assert_initial( result_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = conv sysuuid_x16( 987 ) act = failed_cut-travel[ 1 ]-traveluuid ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-not_found act = failed_cut-travel[ 1 ]-%fail-cause ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

  method isolate_read_ba_set_failed.
  "Test Goal: Isolate the Read EML with read booking by association on Travel in CUT (Code under Test) to fail by configuring additional behavior on the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Setup test data instance for read by association EML in code under test.
  "Step 3: Define and set up the failed response structure to be returned for read by association EML on test data instance in CUT
  "Step 4: set_failed using configure_addtional_behavior API on double to configure read by association EML on test data instance in code under test to fail.
          "set_failed API can be used to force an operation to fail.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "No record step needed.
   "The read by association operation on instance in CUT will return failed, configured via set_failed.

  "Step 2: Setup test data instance for read by association EML in code under test.
   data rba_booking_instances type table for read import /dmo/r_travel_d\_Booking.
   rba_booking_instances = value #( ( traveluuid = 987 %is_draft = if_abap_behv=>mk-on ) ).


  "Step 3: Define and set up the failed response structure to be returned for read by association EML on test data instance in CUT
    data failed type response for failed early /dmo/r_travel_d.
    failed-travel = value #( ( traveluuid = 987 %is_draft = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).


  "Step 4: set_failed using configure_addtional_behavior API on double to configure read by association EML on an instance in code under test to fail.

    data(double) =  environment->get_test_double( '/dmo/r_travel_d' ).
    data(input) = double->create_read_input_config(  )->set_instance( rba_booking_instances[ 1 ] ).
                                                                                "Note that the input instance should contain only one instance
    data(output) = double->create_read_output_config( )->set_failed( failed ).
    double->configure_additional_behavior(  )->for_read_ba(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For read on Travel entity

      cut->read_booking_by_assoc(
        exporting
          travel   = rba_booking_instances
        importing
          result   = data(result_cut)
          reported = data(reported_cut)
          failed   = data(failed_cut)
          links    = data(links_cut)
      ).
  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    cl_abap_unit_assert=>assert_initial( result_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = conv sysuuid_x16( 987 ) act = failed_cut-travel[ 1 ]-traveluuid ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-not_found act = failed_cut-travel[ 1 ]-%fail-cause ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-%is_draft exp = if_abap_behv=>mk-on ).

  endmethod.

endclass.
