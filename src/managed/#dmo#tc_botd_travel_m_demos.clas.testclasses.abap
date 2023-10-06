"! <p class="shorttext synchronized" lang="en">Demonstrate use of MOCKEMLAPI variant to isolate EML dependencies</p>
"! <p> This class demonstrates the use of RAP BO test double framework to isolate BO dependencies via EML in code under test, by creating test
"! doubles with API support to mock EML APIs.</p>
"!<p> With this variant, the framework provides a rich set of APIs to build the expected EML structure with input instances in code under test and configure
"! the desired output configuration for the expected EML statement.</p>
"! <ul>
"! <li>Operation names in test method names, signify EML operations in CUT (Code Under Test).</li>
"! <li>The dependent BO is /dmo/i_travel_m and the code under test is its consumer {@link /dmo/tc_travel_m_bo_consumer}</li>
"! <li>The expected input and output should be configured on double via {@link if_botd_mockemlapi_test_double.Meth:configure_call} API.</li>
"! <li>The expected input structure of EML should be built using {@link cl_botd_mockemlapi_bldrfactory}~{@link cl_botd_mockemlapi_bldrfactory.Meth:get_input_config_builder}.</li>
"! <li>The expected output structure of EML should be built using {@link cl_botd_mockemlapi_bldrfactory}~{@link cl_botd_mockemlapi_bldrfactory.Meth:get_output_config_builder}.</li>
"! </ul>
class ltcl_mockemlapi_variant_demos definition final for testing
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
    METHODS isolate_create_ba_to_pass FOR TESTING RAISING cx_static_check.
    METHODS isolate_read_to_pass FOR TESTING RAISING cx_static_check.

    methods setup.
    class-methods class_setup.
    class-methods class_teardown.
    class-data double type ref to if_botd_mockemlapi_test_double.
    class-data environment type ref to if_botd_mockemlapi_bo_test_env.
    class-data cut type ref to /dmo/tc_travel_m_bo_consumer.

endclass.

class ltcl_mockemlapi_variant_demos implementation.

  method class_setup.
  "Create doubles for BO '/DMO/I_TRAVEL_M'.
  "a. Prepare environment configuration with bdef dependencies for which doubles are to be created
  data(env_config) = cl_botd_mockemlapi_bo_test_env=>prepare_environment_config(  )->set_bdef_dependencies( bdef_dependencies = value #( ( '/DMO/I_TRAVEL_M' ) ) ).

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
   data create_travel_instances type table for create /DMO/I_TRAVEL_M.
   create_travel_instances = value #( ( %cid = 'Travel_987' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_987' %control-Agency_ID = if_abap_behv=>mk-on
                                                                                                                     %control-Customer_ID = if_abap_behv=>mk-on
                                                                                                                     %control-description = if_abap_behv=>mk-on )
                                      ( %cid = 'Travel_988' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_988' Begin_Date = cl_abap_context_info=>get_system_date( ) + 10 End_Date = cl_abap_context_info=>get_system_date( ) ) ).
                                                                                                                                        "Begin_Date > End_Date. Should fail due to FEATURE CONTROL
                                      "%control structure is ignored in MOCKEMLAPI variant, while matching the EML input with configured input.

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /dmo/i_travel_m.
    data failed type response for failed early /dmo/i_travel_m.

    "Setup mapped and failed according to the response to be returned
    mapped-travel = value #( ( %cid = 'Travel_987' Travel_ID = 987  ) ). "Implies that a travel with travel id 987 was created
    failed-travel = value #( ( %cid = 'Travel_988' %create = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-unspecific ) ).
                                                                                    "Create on 2nd instance failed as the BeginDate > EndDate. //DUE TO FEATURE CONTROL
  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts and set instances for all operations on that entity
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/dmo/i_travel_m'
                                   )->set_instances_for_create( create_travel_instances ).

    "Input configuration for EML
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped )->set_failed( failed ).

  "Step 4: Configure the Modify EML via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
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

  endmethod.

  method isolate_create_ba.
  "Test Goal: Isolate the Modify EML with Create by association (create_ba) in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only create_ba ).
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only create_ba ).
   data cba_booking_instances type table for create /DMO/I_TRAVEL_M\_Booking.
   cba_booking_instances = value #( ( Travel_ID = '987'
                                      %target = value #( (  %cid = 'Travel_987_Booking_001'
                                                            carrier_id = '000111' %control-carrier_id = if_abap_behv=>mk-on
                                                            connection_id = 'C1' %control-connection_id = if_abap_behv=>mk-on
                                                            Customer_ID = '000006' %control-Customer_ID = if_abap_behv=>mk-on "For mandatory fields, control structure should be marked.
                                                                                                                               "The MOCKEMLAPI variant still ignores the control structure.
                                                            Flight_Date = cl_abap_context_info=>get_system_date( ) + 10 %control-Flight_Date = if_abap_behv=>mk-on
                                                            booking_date = cl_abap_context_info=>get_system_date( ) %control-booking_date = if_abap_behv=>mk-on
                                                            booking_status = 'B' %control-booking_status = if_abap_behv=>mk-on )
                                                         (  %cid = 'Travel_987_Booking_002'
                                                            carrier_id = '000112'  %control-carrier_id = if_abap_behv=>mk-on
                                                            connection_id = 'C1' %control-connection_id = if_abap_behv=>mk-on
                                                            Customer_ID = '000007' %control-Customer_ID = if_abap_behv=>mk-on
                                                            Flight_Date = cl_abap_context_info=>get_system_date( ) + 10 %control-Flight_Date = if_abap_behv=>mk-on
                                                            booking_date = cl_abap_context_info=>get_system_date( )  %control-booking_date = if_abap_behv=>mk-on
                                                            booking_status = 'B' %control-booking_status = if_abap_behv=>mk-on
                                                            ) )  )
                                                                                                      "Creation of bookings for Travel_ID 987 should pass
                                     ( Travel_ID = '988'
                                       %target = value #( (  %cid = 'Travel_988_Booking_001'
                                                            carrier_id = '000111' %control-carrier_id = if_abap_behv=>mk-on
                                                            connection_id = 'C1' %control-connection_id = if_abap_behv=>mk-on
                                                            Customer_ID = '000006' %control-Customer_ID = if_abap_behv=>mk-on
                                                            Flight_Date = cl_abap_context_info=>get_system_date( ) + 10 %control-Flight_Date = if_abap_behv=>mk-on
                                                            booking_date = cl_abap_context_info=>get_system_date( ) %control-booking_date = if_abap_behv=>mk-on
                                                            booking_status = 'B' %control-booking_status = if_abap_behv=>mk-on ) )
                                     ) ). "Creation of booking for Travel_ID 988 should fail, assuming Travel_ID 988 did not exist

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/I_TRAVEL_M.
    data failed type response for failed early /DMO/I_TRAVEL_M.

    "Setup mapped and failed according to the response to be returned
    mapped-booking = value #( ( %cid = 'Travel_987_Booking_001' Booking_ID = '001'  ) ( %cid = 'Travel_987_Booking_002' Booking_ID = '002'  ) ).

    "For failure of create_ba for Travel_ID 988
    failed-travel = value #( (  Travel_ID = '988' %assoc-_Booking = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).
    failed-booking = value #( (  %cid = 'Travel_988_Booking_001' %fail-cause = if_abap_behv=>cause-dependency ) ).

  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/I_TRAVEL_M'   "can accept the entity name or alias name
                                   )->set_instances_for_create_ba( cba_booking_instances ).

    "Input configuration for EML
    "Input should match the EML structure and instances in code under test
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped )->set_failed( failed ).

  "Step 4: Configure the Modify EML via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
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
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-Travel_ID exp = ( '988' ) ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-booking[ 1 ]-%cid exp = 'Travel_988_Booking_001' ).

  endmethod.

  method isolate_create_ba_to_pass.
  "Test Goal: Isolate the Modify EML with Create by association (create_ba) in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only create_ba ).
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only create_ba ).
   data cba_booking_instances type table for create /DMO/I_TRAVEL_M\_Booking.
   cba_booking_instances = value #( ( Travel_ID = 1
                                      %target = value #( (  %cid = 'Travel_1_Booking_1'
                                                            carrier_id = '111' %control-carrier_id = if_abap_behv=>mk-on
                                                            connection_id = '1234' %control-connection_id = if_abap_behv=>mk-on
                                                            Customer_ID = '000006' %control-Customer_ID = if_abap_behv=>mk-on "For mandatory fields, control structure should be marked.
                                                                                                                               "The MOCKEMLAPI variant still ignores the control structure.
                                                            Flight_Date = cl_abap_context_info=>get_system_date( ) + 10 %control-Flight_Date = if_abap_behv=>mk-on
                                                            booking_date = cl_abap_context_info=>get_system_date( ) %control-booking_date = if_abap_behv=>mk-on
                                                            booking_status = 'B' %control-booking_status = if_abap_behv=>mk-on ) ) ) ).

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/I_TRAVEL_M.

    "Setup mapped and failed according to the response to be returned
    mapped-booking = value #( ( %cid = 'Travel_1_Booking_1' Booking_ID = 1  ) ).

  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/I_TRAVEL_M'   "can accept the entity name or alias name
                                   )->set_instances_for_create_ba( cba_booking_instances ).

    "Input configuration for EML
    "Input should match the EML structure and instances in code under test
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped ).

  "Step 4: Configure the Modify EML via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "Execute the Code under test (CUT)
  "The EML in code under test will find a matching configuration with the input configured via configure_call API and return the corresponding output configuration

   cut->create_ba_booking_for_travel_1(
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

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-booking ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-%cid exp = 'Travel_1_Booking_1' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-booking_id exp = 1 ).

    "Verification
    double->verify( )->modify( input )->is_called_times( times = 1 ).
    double->verify( )->modify( input )->is_called_at_least( times = 1 ).

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
   data create_travel_instances type table for create /DMO/I_TRAVEL_M.
   create_travel_instances = value #( ( %cid = 'Travel_987' Customer_ID = '000006' description = 'Travel_987'  %control-Customer_ID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on )
                                      ( %cid = 'Travel_988' Customer_ID = '000007' description = 'Travel_988' ) ).

  "For Creating Booking by Association on Travel entity
   data cba_booking_instances type table for create /DMO/I_TRAVEL_M\_Booking.
   cba_booking_instances = value #( ( %cid_ref = 'Travel_987' Travel_ID = '987'
                                      %target = value #( (  %cid = 'Travel_987_Booking_001'
                                                            carrier_id = '000111' %control-carrier_id = if_abap_behv=>mk-on
                                                            connection_id = 'C1' %control-connection_id = if_abap_behv=>mk-on
                                                            Customer_ID = '000006' %control-Customer_ID = if_abap_behv=>mk-on
                                                            Flight_Date = cl_abap_context_info=>get_system_date( ) + 10 %control-Flight_Date = if_abap_behv=>mk-on
                                                            booking_date = cl_abap_context_info=>get_system_date( ) %control-booking_date = if_abap_behv=>mk-on
                                                            booking_status = 'B' %control-booking_status = if_abap_behv=>mk-on )
                                                         (  %cid = 'Travel_987_Booking_002'
                                                            carrier_id = '000112'  %control-carrier_id = if_abap_behv=>mk-on
                                                            connection_id = 'C1' %control-connection_id = if_abap_behv=>mk-on
                                                            Customer_ID = '000007' %control-Customer_ID = if_abap_behv=>mk-on
                                                            Flight_Date = cl_abap_context_info=>get_system_date( ) + 10 %control-Flight_Date = if_abap_behv=>mk-on
                                                            booking_date = cl_abap_context_info=>get_system_date( )  %control-booking_date = if_abap_behv=>mk-on
                                                            booking_status = 'B' %control-booking_status = if_abap_behv=>mk-on
                                                            ) )  )
                                                                                                      "Creation of bookings for Travel_ID 987 should pass
                                     ).
  "For Update on Booking entity
   data update_booking_instances type table for update /dmo/i_travel_m\\booking.
   update_booking_instances = value #( ( Travel_ID = '987' Booking_ID = '001' flight_price = 100 %control-flight_price = if_abap_behv=>mk-on )
                                       ( travel_Id = '988' Booking_ID = '003' flight_price = 200 %control-flight_price = if_abap_behv=>mk-on ) ).


  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /dmo/i_travel_m.
    data failed type response for failed early /dmo/i_travel_m.

    "Setup mapped and failed according to the response to be returned
    "For create travel instances
    mapped-travel = value #( ( %cid = 'Travel_987' Travel_ID = 987  ) ( %cid = 'Travel_988' Travel_ID = 988  )  ). "Implies that a travel with travel id 987 and 988 was created

    "For create booking by association instances
    mapped-booking = value #( ( %cid = 'Travel_987_Booking_001' Booking_ID = '001'  ) ( %cid = 'Travel_987_Booking_002' Booking_ID = '002'  ) ).

    "For update booking instances
    failed-booking = value #( (  Travel_ID = '988' booking_id = '003' %update = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).


  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/dmo/i_travel_m'   "can accept the entity name
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
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output ).


  "*************Configure second EML in Code under test*************
    "create travel instances for 2nd EML
    data create_travel_instances2 type table for create /dmo/i_travel_m.
    create_travel_instances2 = value #( ( %cid = 'Travel_987' Customer_ID = '000006' description = 'Travel_987' ) ).

    "travel input part for 2nd EML
    data(eml_travel_input2) = input_config_builder_4_modify->build_entity_part( '/dmo/i_travel_m'   "can accept the entity name
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
    mapped-travel = value #( ( %cid = 'Travel_987' Travel_ID = 987  ) ( %cid = 'Travel_988' Travel_ID = 988  )  ). "Implies that a travel with travel id 987 and 988 was created

    "For create booking by association instances
    mapped-booking = value #( ( %cid = 'Travel_987_Booking_001' Booking_ID = '001'  ) ( %cid = 'Travel_987_Booking_002' Booking_ID = '002'  ) ).

    "For update booking instances
    failed-booking = value #( (  Travel_ID = '988' booking_id = '003' %update = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).

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

  double->verify( )->is_called_times( times = 2 ). "Since we see 2 EML calls, the double should be invoked for both.
  double->verify( )->is_called_at_least( times = 1 ).

  double->verify( )->modify( )->is_called_times( times = 2 ). "Since the 2 EML calls are modify, the double should be invoked for both modify EML statements.
  double->verify( )->modify( )->is_called_at_least( times = 1 ).

  double->verify( )->read( )->is_never_called( ). "Since no READ EML in code under test

  "For specific input configurations

  double->verify( )->modify( input )->is_called_times( times = 1 ).
  double->verify( )->modify( input2 )->is_called_times( times = 2 ). "Since input2 is a subset of input. Thus both EMLs are infact executed with input2

  "Since there are 2 EMls with CREATE for travel with %cid = 'Travel_987' in both, a query to verify if CREATE with %cid = 'Travel_987' was called twice should be true
  "Input configuration for verification
  data(verification_input) = input_config_builder_4_modify->build_input_for_eml(
                                                                )->add_entity_part(
                                                                     input_config_builder_4_modify->build_entity_part( '/dmo/i_travel_m'
                                                                            )->set_instances_for_create( create_travel_instances2 ) ).

  double->verify( )->modify( verification_input )->is_called_times( times = 2 ).

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
   data create_travel_instances type table for create /DMO/I_TRAVEL_M.
   create_travel_instances = value #( ( %cid = 'Travel_987' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_987' ) ).


  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /dmo/i_travel_m.
    data failed type response for failed early /dmo/i_travel_m.

  "Fill the expected response in the response structure.
    failed-travel = value #( ( %cid = 'Travel_987' %fail-cause = if_abap_behv=>cause-unauthorized ) ).
    mapped-travel = value #( ( %cid = 'Travel_987' travel_id = 987 ) ).

  "Step 3: Create input and output configurations for the Modify EML.
   data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
   data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts and set instances for all operations on that entity
    "For travel entity
   data(travel_part) = input_config_builder_4_modify->build_entity_part( '/dmo/i_travel_m'   "can accept the entity name
                                   )->set_instances_for_create( create_travel_instances ).

    "Input configuration for EML
   data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( travel_part ).

    "Output configuration for EML
    data(output_1) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped ).
    data(output_2) = output_config_builder_4_modify->build_output_for_eml( )->set_failed( failed ).

  "Step 4: Configure the Modify EML via configure_call API on the double.
    double =  environment->get_test_double( '/dmo/i_travel_m' ).
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
   data update_travel_instances type table for update /DMO/I_TRAVEL_M.
   update_travel_instances = value #( ( Travel_ID = '987'  description = 'Travel_987_updated' %control-description = if_abap_behv=>mk-on ) )."Should pass

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/I_TRAVEL_M.
    data failed type response for failed early /DMO/I_TRAVEL_M.

    "Setup mapped and failed according to the response to be returned
    mapped-travel = value #( ( Travel_ID = '987'   ) ). "Implies that Travel_ID 987 was updated
    failed-travel = value #( ( Travel_ID = '987'   %update = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).

  "Step 3: Create input and output configurations for the Modify EML.
   "Get input and output configuration builder.
   data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
   data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts and set instances for all operations on that entity
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/I_TRAVEL_M'
                                   )->set_instances_for_update( update_travel_instances ).

    "Input configuration for EML
    "Input should match the EML structure and instances in code under test
    data(update_input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(update_output_1) = output_config_builder_4_modify->build_output_for_eml( )->set_failed( failed ).
    data(update_output_2) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped ).

  "Step 4: Configure the Modify EML in CUT via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
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
    data mapped type response for mapped early /dmo/i_travel_m.
    data failed type response for failed early /dmo/i_travel_m.

    "Setup mapped and failed according to the response to be returned
    mapped-travel = value #( ( Travel_ID = '987' ) ).
    mapped-booking = value #( ( Travel_ID = '987'  booking_id = '001' ) ).
    failed-booking = value #( ( Travel_ID = '988'  booking_id = '002' %update = if_abap_behv=>mk-on ) ).

  "Step 2: Create output configuration for the Modify EML.
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped )->set_failed( failed ).

  "Step 3: Configure the Modify EML via configure_call and "ignore_input" API on the double.
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_call(  )->for_modify(  )->ignore_input(  )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
   data create_travel_instances type table for create /DMO/I_TRAVEL_M.
   create_travel_instances = value #( ( %cid = 'Travel_987' Customer_ID = '000006' description = 'Travel_987' ) ).

  "For Creating Booking by Association on Travel entity
   data cba_booking_instances type table for create /DMO/I_TRAVEL_M\_Booking.
   cba_booking_instances = value #( ( %cid_ref = 'Travel_987' Travel_ID = '987'
                                      %target = value #( (  %cid = 'Travel_987_Booking_001'
                                                            carrier_id = '000111' %control-carrier_id = if_abap_behv=>mk-on
                                                            connection_id = 'C1' %control-connection_id = if_abap_behv=>mk-on
                                                            Customer_ID = '000006' %control-Customer_ID = if_abap_behv=>mk-on
                                                            Flight_Date = cl_abap_context_info=>get_system_date( ) + 10 %control-Flight_Date = if_abap_behv=>mk-on
                                                            booking_date = cl_abap_context_info=>get_system_date( ) %control-booking_date = if_abap_behv=>mk-on
                                                            booking_status = 'B' %control-booking_status = if_abap_behv=>mk-on )
                                                           )
                                                                                                      "Creation of bookings for Travel_ID 987 should pass
                                    ) ).
  "For Update on Booking entity
   data update_booking_instances type table for update /dmo/i_travel_m\\booking.
   update_booking_instances = value #( ( travel_Id = '988' Booking_ID = '003' flight_price = 200 %control-flight_price = if_abap_behv=>mk-on ) ).

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

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-travel_id exp = '987' ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-%key-travel_id exp = '987' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-%key-booking_id exp = '001' ).

    cl_abap_unit_assert=>assert_equals( act = failed_cut-booking[ 1 ]-%tky-travel_id exp = '988' ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-booking[ 1 ]-%tky-booking_id exp = '002' ).


  endmethod.

  method isolate_actions.
  "Test Goal: Isolate Modify EML with Action in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only Action ).
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "For accept travel action on Travel entity
   data accept_travel_act_insts type table for action import /DMO/I_TRAVEL_M~acceptTravel.
   accept_travel_act_insts = value #( ( Travel_ID = '987'  ) "Should pass
                                      ( Travel_ID = '988'  ) ).
                                                                                "Should fail, assuming Travel 988 does not exist

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped_accept_travel type response for mapped early /DMO/I_TRAVEL_M.
    data failed type response for failed early /DMO/I_TRAVEL_M.
    data result_accept_travel type table for action result /DMO/I_TRAVEL_M~acceptTravel.

    "Setup mapped and failed according to the response to be returned
    mapped_accept_travel-travel = value #( ( Travel_ID = '987'   ) ). "Implies that action accept_travel on Travel_ID 987 was successful
    result_accept_travel = value #( ( %tky-Travel_ID = '987'  %param-Overall_Status = 'A' %param-Total_Price = 100 ) ).
    failed-travel = value #( ( Travel_ID = '988'   %action-acceptTravel = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).
                                                                                   "AcceptTravel failed as the Travel 988 does not exist

  "Step 3: Create input and output configurations for the Modify EML for Action accept Travel.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts and set instances for all operations on that entity
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/I_TRAVEL_M'
                                   )->set_instances_for_action( instances = accept_travel_act_insts ).

    "Input configuration for EML
    "Input should match the EML structure and instances in code under test
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped_accept_travel )->set_failed( failed )->set_result_for_action( result = result_accept_travel ).

  "Step 4: Configure the Modify EML in CUT via configure_call API on the double for Action accept Travel.
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output ).


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

    cl_abap_unit_assert=>assert_equals( act = mapped_accept_travel_cut-travel[ 1 ]-Travel_ID exp = ( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = failed_accept_travel_cut-travel[ 1 ]-Travel_ID exp = ( '988' ) ).
    cl_abap_unit_assert=>assert_equals( act = result_accept_travel_cut[ 1 ]-Travel_ID exp = ( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = result_accept_travel_cut[ 1 ]-%param-Total_Price exp = 100 ).

  endmethod.

  method isolate_deep_create.
  "Test Goal: Isolate a deep create Modify EML with creation for all entity instances of BO in one EML in CUT (Code under Test) by configuring responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "For Create on Travel entity
   data create_travel_instances type table for create /DMO/I_TRAVEL_M.
   create_travel_instances = value #( ( %cid = 'Travel_987' %data-Customer_ID = '000006' description = 'Travel_987' overall_status = 'O' ) ).

  "For Creating Booking by Association on Travel entity
   data cba_booking_instances type table for create /DMO/I_TRAVEL_M\_Booking.
   cba_booking_instances = value #( ( %cid_ref = 'Travel_987'
                                      %target = value #( (  %cid = 'Travel_987_Booking_001' %data-carrier_id = '000111' connection_id = 'C1' Customer_ID = '000006'
                                                            Flight_Date = cl_abap_context_info=>get_system_date( ) + 10 booking_date = cl_abap_context_info=>get_system_date( ) booking_status = 'B'  ) )  )  ).

  "For Creating booking_supplement by association on Booking entity
   data cba_booking_suppl_instances type table for create /dmo/i_travel_m\\booking\_BookSupplement.
   cba_booking_suppl_instances = value #( (  %cid_ref = 'Travel_987_Booking_001'
                                             %target  = value #( ( %cid = 'Travel_987_Booking_001_BookingSupp_01' %data-supplement_id  = '007' price = 100 ) ) ) ).


  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /dmo/i_travel_m.

    "Setup mapped and failed according to the response to be returned
    "For create travel instances
    mapped-travel = value #( ( %cid = 'Travel_987' Travel_ID = 987  ) ). "Implies that a travel with travel id 987 was created

    "For create booking by association instances
    mapped-booking = value #( ( %cid = 'Travel_987_Booking_001' Travel_ID = 987 Booking_ID = 001  )  ).

    "For create booking supp by association instances
    mapped-booksuppl = value #( (  %cid = 'Travel_987_Booking_001_BookingSupp_01' Travel_ID = 987 Booking_ID = 001 %tky-booking_supplement_id = 01 ) ).


  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/dmo/i_travel_m'   "can accept the entity name
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
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_call(  )->for_modify(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  cut->deep_create_travel_bo(
   importing
      mapped   = data(mapped_cut_1)
      reported = data(reported_cut_1)
      failed   = data(failed_cut_1)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    cl_abap_unit_assert=>assert_not_initial( mapped_cut_1 ).
    cl_abap_unit_assert=>assert_initial( failed_cut_1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut_1-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut_1-booking ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut_1-booksuppl ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut_1-travel[ 1 ]-%cid exp = 'Travel_987' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut_1-booking[ 1 ]-%cid exp = 'Travel_987_Booking_001' ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut_1-booksuppl[ 1 ]-%cid exp = 'Travel_987_Booking_001_BookingSupp_01' ).

    cl_abap_unit_assert=>assert_equals( act = mapped_cut_1-booksuppl[ 1 ]-booking_supplement_id exp = 01 ).
  endmethod.

  method isolate_delete.
  "Test Goal: Isolate Modify EML with delete in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only delete ).
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "For delete on Travel entity
   data delete_travel_instances type table for delete /DMO/I_TRAVEL_M.
   delete_travel_instances = value #( ( %pky-%key-Travel_ID = '987' ) "Should pass
                                      ( %pky-Travel_ID = '988'  ) ).
                                                                                "Should fail, assuming Travel 988 does not exist

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/I_TRAVEL_M.
    data failed type response for failed early /DMO/I_TRAVEL_M.

    "Setup mapped and failed according to the response to be returned
    mapped-travel = value #( ( Travel_ID = '987'   ) ). "Implies that Travel_ID 987 was deleted
    failed-travel = value #( ( Travel_ID = '988'   %delete = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).
                                                                                   "delete failed as the Travel 988 does not exist

  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts and set instances for all operations on that entity
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/I_TRAVEL_M'
                                   )->set_instances_for_delete( delete_travel_instances ).

    "Input configuration for EML
    "Input should match the EML structure and instances in code under test
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped )->set_failed( failed ).

  "Step 4: Configure the Modify EML in CUT via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
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

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-Travel_ID exp = ( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-Travel_ID exp = ( '988' ) ).

  endmethod.

  method isolate_read.
  "Test Goal: Isolate the read EML with read operation in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Read EML (Here only read ).
  "Step 2: Define and set up the response structures to be returned for READ EML in CUT
  "Step 3: Create input and output configurations for the READ EML.
  "Step 4: Configure the READ EML via configure_call API on the double.

  "Step 1: Setup test data instances for read operation in READ EML.
  "For read on travel
   data read_travel_instances type table for read import /DMO/I_TRAVEL_M.
   read_travel_instances = value #( (  Travel_ID = '987' ) "travel returned
                                    (  Travel_ID = '988' ) ). "read should fail for 988 assuming the instance does not exist

  "Step 2: Define and set up the response structures to be returned for Read EML in CUT
    data result type table for read result /DMO/I_TRAVEL_M.
    result = value #( (  Travel_ID = '987' %data-Booking_Fee = 10 Total_Price = 100 Overall_Status = 'A' ) ).
    data failed type response for failed early /DMO/I_TRAVEL_M.
    failed-travel = value #( (  Travel_ID = '988' ) ).

  "Step 3: Create input and output configurations for Read EML.
   data(input_config_builder_4_read) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_read(  ).
   data(output_config_builder_4_read) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_read( ).

    "Create input for all entity parts
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_read->build_entity_part( '/DMO/I_TRAVEL_M'   "can accept the entity name
                                   )->set_instances_for_read( read_travel_instances ).

    "input and output configuration
    data(input) = input_config_builder_4_read->build_input_for_eml(  )->add_entity_part( eml_travel_input ).
    data(output) = output_config_builder_4_read->build_output_for_eml( )->set_failed( failed )->set_result_for_read( result ).

  "Step 4: Configure the Read EML via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
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

    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-Travel_ID exp = ( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-Total_Price exp = 100 ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-Travel_ID exp = ( '988' ) ).

  endmethod.

  method isolate_read_to_pass.
  "Test Goal: Isolate the read EML with read operation in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Read EML (Here only read ).
  "Step 2: Define and set up the response structures to be returned for READ EML in CUT
  "Step 3: Create input and output configurations for the READ EML.
  "Step 4: Configure the READ EML via configure_call API on the double.

  "Step 1: Setup test data instances for read operation in READ EML.
  "For read on travel
   data read_travel_instances type table for read import /DMO/I_TRAVEL_M.
   read_travel_instances = value #( (  Travel_ID = 1 ) ). "code under test has read on travel_id 1

  "Step 2: Define and set up the response structures to be returned for Read EML in CUT
    data result type table for read result /DMO/I_TRAVEL_M.
    result = VALUE #( (  Travel_ID = 1 %data-description = 'Travel 1' ) ).

  "Step 3: Create input and output configurations for Read EML.
   data(input_config_builder_4_read) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_read(  ).
   data(output_config_builder_4_read) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_read( ).

    "Create input for all entity parts
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_read->build_entity_part( '/DMO/I_TRAVEL_M'   "can accept the entity name/alias name
                                   )->set_instances_for_read( read_travel_instances ).

    "input and output configuration
    data(input) = input_config_builder_4_read->build_input_for_eml(  )->add_entity_part( eml_travel_input ).
    data(output) = output_config_builder_4_read->build_output_for_eml( )->set_result_for_read( result ).

  "Step 4: Configure the Read EML via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_call(  )->for_read(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  cut->read_travel_1(
    importing
      result   = data(result_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    cl_abap_unit_assert=>assert_not_initial( result_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( result_cut ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-Travel_ID exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-description exp = 'Travel 1' ).


    "Verify if the EML was actual executed with the configured input
    double->verify( )->read( input )->is_called_times( times = 1 ).
    double->verify( )->read( input )->is_called_at_least( times = 1 ).

  endmethod.

  method isolate_read_ba.
  "Test Goal: Isolate the read by association (read_ba) operation in READ EML in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Read EML (Here only read_ba ).
  "Step 2: Define and set up the response structures to be returned for read_ba operation in READ EML in CUT
  "Step 3: Create input and output configurations for the READ EML.
  "Step 4: Configure the READ EML via configure_call API on the double.

  "Step 1: Setup test data instances for read_ba operation in READ EML.
  "For read booking by association on travel
   data rba_booking_instances type table for read import /DMO/I_TRAVEL_M\_Booking.
   rba_booking_instances = value #( (  Travel_ID = '987' ) "Bookings returned
                                    (  Travel_ID = '988' ) ). "read_ba should fail for 988 assuming the instance does not exist

  "Step 2: Define and set up the response structures to be returned for Read EML in CUT
    data result type table for read result /DMO/I_TRAVEL_M\_Booking.
    result = value #( (  Travel_ID = '987' Booking_ID = '001' %data-Flight_Price = 10  )
                      (  Travel_ID = '987' Booking_ID = '002' %data-Flight_Price = 20  ) ).
    data failed type response for failed early /DMO/I_TRAVEL_M.
    failed-travel = value #( (  Travel_ID = '988' ) ).

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
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
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

    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-Travel_ID exp = ( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-Booking_ID exp = ( '001' ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 2 ]-Booking_ID exp = ( '002' ) ).

    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-Travel_ID exp = ( '988' ) ).

  endmethod.

  method isolate_update.
  "Test Goal: Isolate Modify EML with Update in CUT (Code under Test) and configure responses via API.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML (Here only update ).
  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
  "Step 3: Create input and output configurations for the Modify EML.
  "Step 4: Configure the Modify EML via configure_call API on the double.

  "Step 1: Setup test data instances for all operations on all entities in Modify EML.
  "For Update on Travel entity
   data update_travel_instances type table for update /DMO/I_TRAVEL_M.
   update_travel_instances = value #( ( Travel_ID = '987'  description = 'Travel_987_updated' %control-description = if_abap_behv=>mk-on ) "Should pass
                                      ( Travel_ID = '988'  description = 'Travel_988_updated' %control-description = if_abap_behv=>mk-on ) ).
                                                                                "Should fail, assuming Travel 988 does not exist

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /DMO/I_TRAVEL_M.
    data failed type response for failed early /DMO/I_TRAVEL_M.

    "Setup mapped and failed according to the response to be returned
    mapped-travel = value #( ( Travel_ID = '987'   ) ). "Implies that Travel_ID 987 was updated
    failed-travel = value #( ( Travel_ID = '988'   %update = if_abap_behv=>mk-on %fail-cause = if_abap_behv=>cause-not_found ) ).
                                                                                   "Update failed as the Travel 988 does not exist

  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts and set instances for all operations on that entity
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/DMO/I_TRAVEL_M'
                                   )->set_instances_for_update( update_travel_instances ).

    "Input configuration for EML
    "Input should match the EML structure and instances in code under test
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped )->set_failed( failed ).

  "Step 4: Configure the Modify EML in CUT via configure_call API on the double.
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
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

    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-Travel_ID exp = ( '987' ) ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-Travel_ID exp = ( '988' ) ).

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
   data create_travel_instances type table for create /DMO/I_TRAVEL_M.
   create_travel_instances = value #( ( %cid = 'Travel_987' %data-Customer_ID = '000006' description = 'Travel_987' overall_status = 'O' ) ).

  "Step 2: Define and set up the response structures to be returned for Modify EML in CUT
    data mapped type response for mapped early /dmo/i_travel_m.

    "Setup mapped according to the response to be returned
    mapped-travel = value #( ( %cid = 'Travel_987' Travel_ID = 987 ) ). "Create on 987 passed

  "Step 3: Create input and output configurations for the Modify EML.
    data(input_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_input_config_builder( )->for_modify(  ).
    data(output_config_builder_4_modify) = cl_botd_mockemlapi_bldrfactory=>get_output_config_builder( )->for_modify( ).

    "Create input for all entity parts
    "For travel entity
    data(eml_travel_input) = input_config_builder_4_modify->build_entity_part( '/dmo/i_travel_m'
                                   )->set_instances_for_create( create_travel_instances ).

    "Input configuration for EML
    data(input) = input_config_builder_4_modify->build_input_for_eml(  )->add_entity_part( eml_travel_input ).

    "Output configuration for EML
    data(output) = output_config_builder_4_modify->build_output_for_eml( )->set_mapped( mapped ).

  "Step 4: Configure the Modify EML via configure_call and "when_input_partial_input" API on the double.
    double =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
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

    cl_abap_unit_assert=>assert_equals( act = mapped_cut_1-travel[ 1 ]-travel_id exp = 987 ).

  endmethod.

endclass.


"! <p class="shorttext synchronized" lang="en">Demonstrate use of TXBUFDBL variant to isolate EML dependencies</p>
"! <p> This class demonstrates the use of RAP BO test double framework to isolate BO dependencies via EML in code under test by creating test
"! doubles with a generic implementation and transactional buffer double support.</p>
"!<p> With this variant, the framework provides a generic mock of CRUD operations for Modify and Read EML statements and provides a transactional buffer double
"! which holds all the instances recorded via Create EML and manipulated/queried by other operations of Modify/Read EML statements.</p>
"! <ul>
"! <li>Operation names in test method names, signify EML operations in CUT (Code Under Test).</li>
"! <li>The dependent BO is /dmo/i_travel_m and the code under test is its consumer {@link /dmo/tc_travel_m_bo_consumer}</li>
"! <li>The test data should be recorded via Create EML (if required), such that the EML operations in CUT are evaluated
"! as required.</li>
"! <li>The framework takes care of providing the filled response structures (mapped, reported, failed, result)</li>
"! <ul><li>In case the returned responses are not satisfactory, this variant also provides complimentary APIs to set custom responses. <br/>
"! For example check the method {@link ltcl_txbufdbl_variant_demos.Meth:isolate_create_set_failed}</li></ul>
"! </ul>
class ltcl_txbufdbl_variant_demos definition final for testing
  duration short
  risk level harmless.

  private section.

    "Create
    methods isolate_create_to_pass for testing raising cx_static_check.
    methods isolate_create_to_fail_dup for testing raising cx_static_check.
    methods isolate_create_set_mapped for testing raising cx_static_check. "using complimentary APIs
    methods isolate_create_set_failed for testing raising cx_static_check. "using complimentary APIs

    "Create by association
    methods isolate_create_ba for testing raising cx_static_check.
    methods isolate_create_ba_set_mapped for testing raising cx_static_check."using complimentary APIs
    methods isolate_create_ba_set_failed for testing raising cx_static_check."using complimentary APIs

    "Update
    methods isolate_update_to_pass for testing raising cx_static_check.
    methods isolate_update_to_fail_no_inst for testing raising cx_static_check.
    methods isolate_update_set_mapped for testing raising cx_static_check. "using complimentary APIs
    methods isolate_update_set_failed for testing raising cx_static_check. "using complimentary APIs

    "Delete
    methods isolate_delete_to_pass for testing raising cx_static_check.
    methods isolate_delete_to_fail_no_inst for testing raising cx_static_check.
    methods isolate_delete_set_failed for testing raising cx_static_check. "using complimentary APIs

    "Read
    methods isolate_read for testing raising cx_static_check.
    methods isolate_read_set_failed for testing raising cx_static_check. "using complimentary APIs

    "Read_ba
    methods isolate_read_ba for testing raising cx_static_check.
    methods isolate_read_ba_set_failed for testing raising cx_static_check. "using complimentary APIs

    "Using insert_test_data API, if Create is internal
    methods insert_data_w_internal_create for testing raising cx_static_check.


    METHODS isolate_create_ba_to_pass FOR TESTING RAISING cx_static_check.
    METHODS isolate_create_ba_to_fail FOR TESTING RAISING cx_static_check.


    METHODS isolate_read_to_pass FOR TESTING RAISING cx_static_check.

    methods setup.
    class-methods class_setup.
    class-methods class_teardown.
    class-data environment type ref to if_botd_txbufdbl_bo_test_env.
    class-data cut type ref to /dmo/tc_travel_m_bo_consumer.

endclass.

class ltcl_txbufdbl_variant_demos implementation.

  method class_setup.
  "Create doubles for BO '/DMO/I_TRAVEL_M'.
  "a. Prepare environment configuration with bdef dependencies for which doubles are to be created
    data(env_config) = cl_botd_txbufdbl_bo_test_env=>prepare_environment_config(  )->set_bdef_dependencies( bdef_dependencies = value #( ( '/DMO/I_TRAVEL_M' ) ) ).

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

  method isolate_update_to_pass.
  "Test Goal: Isolate the Modify EML with Update for travel in CUT (Code under Test) by configuring the transactional buffer double.
  "Update EML in CUT should successfully update the instance recorded in Framework's Transactional Buffer Double

  "Step 1: Set numbering support on double (if required) via set_fields_handler API. (set_mapped can also be used to define the key fields for an instance)
  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).

  "Step 1: Set numbering support on double (if required).
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).


  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Record a travel instance which is to be updated. ( Will be recorded in Framework's Buffer and an ID will be returned by the framework according to fields_handler )
    modify entities of /dmo/i_travel_m
     entity travel
       create from value #( ( %cid = 'Travel_1'
                              Agency_ID = '000111' Customer_ID = '000006' description = 'Travel 1'
                              %control-Agency_ID = if_abap_behv=>mk-on
                              %control-Customer_ID = if_abap_behv=>mk-on
                              %control-description = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

     "according to the fields_handler, key assigned to travel instance will be 1.
    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-travel_id exp = 1 ).

  """"""""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""
    " The Framework will evaluate the Update and update the instance recorded in step 2 in Buffer.

    cut->update_travel(
      exporting
        travel   = value #( ( %key-Travel_ID = 1
                             total_price = 200
                             %control-total_price = if_abap_behv=>mk-on ) )
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
    read entity /dmo/i_travel_m
     from value #( ( travel_id = mapped-travel[ 1 ]-travel_id
                     %control-total_price = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

     cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 200 act = result_read[ 1 ]-Total_Price ).

  endmethod.

  method isolate_create_ba.
  "Test Goal: Isolate the Modify EML with Create bookings By Association for travel in CUT (Code under Test) by configuring the transactional buffer double.
   " Creat_BA should pass for travel 1 and insert the booking instances in Transactional Buffer Double of Framework.
   " Creat_BA should fail for travel 2 assuming travel 2 does not exist.

  "Step 1: Set numbering support on double (if required) via set_fields_handler API.
  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).

  "Step 1: Set numbering support on double (if required).
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).

  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Record a travel instance for which booking is to be created. ( Will be recorded in Framework's Buffer and an ID will be returned by the framework according to fields_handler )
    modify entities of /dmo/i_travel_m
     entity travel
       create from value #( ( %cid = 'Travel_1'
                              Agency_ID = '000111' Customer_ID = '000006' description = 'Travel 1'
                              %control-Agency_ID = if_abap_behv=>mk-on
                              %control-Customer_ID = if_abap_behv=>mk-on
                              %control-description = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

     "according to the fields_handler, key assigned to travel instance will be 1.
    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-travel_id exp = 1 ).


  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""
   data cba_booking_instances type table for create /DMO/I_TRAVEL_M\_Booking.
   cba_booking_instances = value #( ( Travel_ID = 1
                                      %target = value #( (  %cid = 'Travel_1_Booking_1'
                                                            carrier_id = '111' %control-carrier_id = if_abap_behv=>mk-on
                                                            connection_id = 'C1' %control-connection_id = if_abap_behv=>mk-on
                                                            Customer_ID = '000006' %control-Customer_ID = if_abap_behv=>mk-on "For mandatory fields, control structure should be marked.
                                                            Flight_Date = cl_abap_context_info=>get_system_date( ) + 10 %control-Flight_Date = if_abap_behv=>mk-on
                                                            booking_date = cl_abap_context_info=>get_system_date( ) %control-booking_date = if_abap_behv=>mk-on
                                                            booking_status = 'B' %control-booking_status = if_abap_behv=>mk-on )
                                                         (  %cid = 'Travel_1_Booking_2'
                                                            carrier_id = '112'  %control-carrier_id = if_abap_behv=>mk-on
                                                            connection_id = 'C1' %control-connection_id = if_abap_behv=>mk-on
                                                            Customer_ID = '000007' %control-Customer_ID = if_abap_behv=>mk-on
                                                            Flight_Date = cl_abap_context_info=>get_system_date( ) + 10 %control-Flight_Date = if_abap_behv=>mk-on
                                                            booking_date = cl_abap_context_info=>get_system_date( )  %control-booking_date = if_abap_behv=>mk-on
                                                            booking_status = 'B' %control-booking_status = if_abap_behv=>mk-on
                                                            ) )  )
                                                                                                      "Creation of bookings for Travel_ID 1 should pass
                                     ( Travel_ID = 2
                                       %target = value #( (  %cid = 'Travel_2_Booking_1'
                                                            carrier_id = '111' %control-carrier_id = if_abap_behv=>mk-on
                                                            connection_id = 'C1' %control-connection_id = if_abap_behv=>mk-on
                                                            Customer_ID = '000006' %control-Customer_ID = if_abap_behv=>mk-on
                                                            Flight_Date = cl_abap_context_info=>get_system_date( ) + 10 %control-Flight_Date = if_abap_behv=>mk-on
                                                            booking_date = cl_abap_context_info=>get_system_date( ) %control-booking_date = if_abap_behv=>mk-on
                                                            booking_status = 'B' %control-booking_status = if_abap_behv=>mk-on ) )
                                     ) ). "Creation of booking for Travel_ID 2 should fail, assuming Travel_ID 2 did not exist


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

    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = 1 act = mapped_cut-booking[ 1 ]-travel_id ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = 1 act = mapped_cut-booking[ 2 ]-travel_id ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = 1 act = mapped_cut-booking[ 1 ]-Booking_ID ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = 2 act = mapped_cut-booking[ 2 ]-Booking_ID ).

    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = 'Travel_1_Booking_1' act = mapped_cut-booking[ 1 ]-%cid ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = 'Travel_1_Booking_2' act = mapped_cut-booking[ 2 ]-%cid ).

    "For failed cases
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = failed_cut-travel[ 1 ]-travel_id ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-not_found act = failed_cut-travel[ 1 ]-%fail-cause ).

    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-booking ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = failed_cut-booking[ 1 ]-travel_id ).
    cl_abap_unit_assert=>assert_initial( failed_cut-booking[ 1 ]-booking_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'Travel_2_Booking_1' act = failed_cut-booking[ 1 ]-%cid  ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-dependency act = failed_cut-booking[ 1 ]-%fail-cause ).

    "Read the data to assert create by association
    read entity /dmo/i_travel_m
      by \_Booking
     from value #( ( travel_id = mapped_cut-booking[ 1 ]-travel_id %control-booking_status = if_abap_behv=>mk-on %control-carrier_id = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

    cl_abap_unit_assert=>assert_equals( act = lines( result_read ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-booking_id exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 2 ]-booking_id exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-booking_status exp = 'B' ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-carrier_id exp = '111' ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 2 ]-carrier_id exp = '112' ).

  endmethod.

  method isolate_create_ba_to_pass.
  "Test Goal: Isolate the Modify EML with Create bookings By Association for travel in CUT (Code under Test) by configuring the transactional buffer double.
   " Create_BA in CUT should pass for travel 1 and insert the booking instances in Transactional Buffer Double of Framework.

  "Step 1: Set numbering support on double (if required) via set_fields_handler API.
  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).

  "Step 1: Set numbering support on double (if required) via set_fields_handler API.
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).

  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Record a travel instance for which booking is to be created. ( Will be recorded in Framework's Buffer and an ID will be returned by the framework according to fields_handler )
    modify entities of /dmo/i_travel_m
     entity travel
       create from value #( ( %cid = 'Travel_1'
                              Agency_ID = '000111' Customer_ID = '000006' description = 'Travel 1'
                              %control-Agency_ID = if_abap_behv=>mk-on
                              %control-Customer_ID = if_abap_behv=>mk-on
                              %control-description = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

     "according to the fields_handler, key assigned to travel instance will be 1.
    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-travel_id exp = 1 ).


  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""
"Execute the code under test (cut) with create by association operation
    ""The code under test has a create booking by association on travel_id '1'. Since a travel with id '1' exists, creating a booking for this instance should be successful.
  "The create_ba operation in cut will be taken care by the generic implementation provided by TXBUFDBL variant and will return the appropriate output responses.
  cut->create_ba_booking_for_travel_1(
      importing
        mapped   = data(mapped_cut)
        reported = data(reported_cut)
        failed   = data(failed_cut)
    ).

  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""

  "Asserts
    "For success cases
    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_equals( act = lines( mapped_cut-booking ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = 1 act = mapped_cut-booking[ 1 ]-travel_id ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = 1 act = mapped_cut-booking[ 1 ]-Booking_ID ).

    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = 'Travel_1_Booking_1' act = mapped_cut-booking[ 1 ]-%cid ).

    "Read the data to assert create by association
    read entity /dmo/i_travel_m
      by \_Booking
     from value #( ( travel_id = mapped_cut-booking[ 1 ]-travel_id %control-booking_status = if_abap_behv=>mk-on %control-carrier_id = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

    cl_abap_unit_assert=>assert_equals( act = lines( result_read ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-booking_id exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-booking_status exp = 'B' ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-carrier_id exp = '111' ).

  endmethod.

  METHOD isolate_create_ba_to_fail.
  "Test Goal: Isolate the Modify EML with Create bookings By Association for travel in CUT (Code under Test) by configuring the transactional buffer double.
   " Create_BA should fail for travel 1 assuming travel 1 does not exist.

  "Step 1: Set numbering support on double (if required) via set_fields_handler API.
  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).

  "Step 1: Set numbering support on double (if required) via set_fields_handler API.
  "Not required, since we are not going to insert any travel instance (code commented)
*  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
*    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
*    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).


  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).
  "We do not perform the following step, i.e. we do not insert the travel 1 for which a booking by association is to be created in CUT.(code commented)
  "Thus we simulate the case, where Create_BA operation fails as the travel instance itself does not already exists.

*   "Record a travel instance for which booking is to be created. ( Will be recorded in Framework's Buffer and an ID will be returned by the framework according to fields_handler )
*    modify entities of /dmo/i_travel_m
*     entity travel
*       create from value #( ( %cid = 'Travel_1'
*                              Agency_ID = '000111' Customer_ID = '000006' description = 'Travel 1'
*                              %control-Agency_ID = if_abap_behv=>mk-on
*                              %control-Customer_ID = if_abap_behv=>mk-on
*                              %control-description = if_abap_behv=>mk-on )
*                          )
*     reported data(reported)
*     failed data(failed)
*     mapped data(mapped).
*
*     "according to the fields_handler, key assigned to travel instance will be 1.
*    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-travel_id exp = 1 ).


  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""
"Execute the code under test (cut) with create by association operation
""The code under test has a create booking by association on travel_id '1'.
""Since a travel with id '1' does not exists, creating a booking for this instance should fail.
""The create_ba operation in cut will be taken care by the generic implementation provided by TXBUFDBL variant and will return the failed output response, with %fail-cause as not found.
  cut->create_ba_booking_for_travel_1(
      importing
        mapped   = DATA(mapped_cut)
        reported = DATA(reported_cut)
        failed   = DATA(failed_cut)
    ).

  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""

  "Asserts
    "For failure case
    cl_abap_unit_assert=>assert_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = failed_cut-travel[ 1 ]-travel_id ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-not_found act = failed_cut-travel[ 1 ]-%fail-cause ).

    cl_abap_unit_assert=>assert_equals( msg = 'create booking should have failed' exp = 'Travel_1_Booking_1' act = failed_cut-booking[ 1 ]-%cid ).

  ENDMETHOD.


  method insert_data_w_internal_create.
  "Demonstrate the use of insert_test_data API to insert instances during test configuration, when CREATE operation is internal for the BO.
  """CAN ONLY BE USED WHEN CREATE IS INTERNAL
  """If create is internal for your use case, the following commented code still demonstrates the usage of insert_test_data API.
  """Please implement your test on same lines.

   "Assume update EML in CUT (Code Under Test) which we want to isolate.

*   "Step 1: Record instance. Assume create is internal. Thus CREATE EML statement can't be used to insert test data in transactional buffer double
*   data travel_instances type table for create /dmo/i_travel_m.
*   travel_instances = value #( ( travel_id = 987
*                              begin_date = '20180101' End_Date = '20180101'
*                              total_price = 100
*                              %control-travel_id = if_abap_behv=>mk-on
*                              %control-begin_date = if_abap_behv=>mk-on
*                              %control-total_price = if_abap_behv=>mk-on )
*                          ).
*
*  "Using insert_test_data API to insert test data assuming create is internal.
*    data(double) = environment->get_test_double( root_name = '/dmo/i_travel_m' ).
*    double->insert_test_data( instances  = travel_instances ). "Insert the instance with travelid 987
*
*  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""
*    cut->update_travel(
*      exporting
*        travel   = value #( ( travel_id = 987
*                              total_price = 200
*                              %control-total_price = if_abap_behv=>mk-on ) )
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
*    read entity /dmo/i_travel_m
*     from value #( ( travel_id = mapped_cut-travel[ 1 ]-travel_id
*                     %control-total_price = if_abap_behv=>mk-on ) )
*     result   data(result_read)
*     reported data(reported_read)
*     failed   data(failed_read).
*
*     cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 200 act = result_read[ 1 ]-Total_Price ).
*
  endmethod.

  method isolate_create_to_fail_dup.
  "Test Goal: Isolate the Modify EML with Create in CUT (Code under Test) to fail by configuring the transactional buffer double.
  "The Create in code under test should fail due to already existing instance with the same key, as duplicates are not allowed.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML which will have the same key as the instance in code under test.
  "Set numbering support on double (if required).
    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ). "Keys start from 1.

    modify entities of /dmo/i_travel_m
     entity travel
       create fields ( customer_id description )
        with value #( ( %CID = 'Configured_Travel_1' Customer_ID = '000006' description = 'Configured Travel 1'  ) ) "Since only one instance is inserted, this instance will have key 1.
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-travel_id exp = 1 ).

  "Set a new numbering support object on double, in which keys will again start from key 1.
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ). "Keys agaon start from 1.

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For Create on Travel entity
   data create_travel_instances type table for create /dmo/i_travel_m.
   create_travel_instances = value #( ( %CID = 'CUT_Travel_1' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel 1 in code under test'
                                        %control-Agency_ID = if_abap_behv=>mk-on %control-Customer_ID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) ).

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
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-travel_id exp = mapped-travel[ 1 ]-travel_id ).

  endmethod.

  method isolate_create_to_pass.
  "Test Goal: Isolate the Modify EML with Create in CUT (Code under Test) to pass by configuring the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Set numbering support on double (if required). (not required if external numbering is enabled)

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "No record step needed.
   "The create in CUT will perform the create operation on the buffer directly.

  "Step 2: Set numbering support on double (if required).
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For Create on Travel entity
   data create_travel_instances type table for create /dmo/i_travel_m.
   create_travel_instances = value #( ( %CID = 'Travel_1' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel 1'
                                        %control-Agency_ID = if_abap_behv=>mk-on %control-Customer_ID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) ).

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

    "Read the data to assert create
    read entity /dmo/i_travel_m
     from value #( ( travel_id = mapped_cut-travel[ 1 ]-travel_id %control-Customer_ID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

    cl_abap_unit_assert=>assert_equals( act = lines( result_read ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-Customer_ID exp = '000006' ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-description exp = 'Travel 1' ).
    cl_abap_unit_assert=>assert_initial( act = result_read[ 1 ]-total_price ).
    cl_abap_unit_assert=>assert_initial( act = result_read[ 1 ]-agency_id ).

  endmethod.

  method isolate_create_set_mapped.
  "Test Goal: Isolate the Modify EML with Create in CUT (Code under Test) to pass by configuring additional behavior on the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Setup test data instance for create EML in code under test.
  "Step 3: Define and set up the mapped response structure to be returned for Create EML on test data instance in CUT
  "Step 4: set_mapped using configure_addtional_behavior API on double to configure CREATE EML on test data instance in code under test to pass.
          "The keys for the instance will be set from the mapped structure provided in "set_mapped" complimentary API.
          "set_mapped will be successful only if the create can actually be performed on the current transactional buffer double state.
            "Ex. If instance with travel_id 987 already exists in the buffer double, setting mapped with travel_id 987 for another instance will fail as duplicates are not allowed.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "No record step needed.
   "The create in CUT will perform the create operation on the buffer directly.

  "Step 2: Setup test data instance for create EML in code under test.
   data create_travel_instances type table for create /DMO/I_TRAVEL_M.
   create_travel_instances = value #( ( %cid = 'Travel_987' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_987' )
                                      ( %cid = 'Travel_988' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_988' ) ).
                                      "Configured Input instance should match exactly with the instance in code under test, (%control structure matching is ignored)

  "Step 3: Define and set up the mapped response structure to be returned for Create EML on test data instance in CUT
    data mapped_987 type response for mapped early /dmo/i_travel_m.
    data mapped_988 type response for mapped early /dmo/i_travel_m.

    mapped_987-travel = value #( ( %cid = 'Travel_987' Travel_ID = '987' ) ).  "Create a travel with travel_id 987
    mapped_988-travel = value #( ( %cid = 'Travel_988' Travel_ID = '988' ) ).  "Create a travel with travel_id 988


  "Step 4: set_mapped using configure_addtional_behavior API on double to configure CREATE EML on an instance in code under test to pass.

    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).

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
   data create_travel_instances_cut type table for create /dmo/i_travel_m.
   create_travel_instances_cut = value #( ( %CID = 'Travel_987' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_987'   "Agency_ID should not be filled as it is not marked in %control
                                             %control-Customer_ID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on )
                                          ( %CID = 'Travel_988' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_988'
                                             %control-Agency_ID = if_abap_behv=>mk-on %control-Customer_ID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) ) .

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

    "The TXBUFDBL variant, creates the instance with key fields provided via the set_mapped API and inserts into the transactional buffer double
    "Read the data to assert create
    read entity /dmo/i_travel_m
     from value #( ( travel_id = mapped_cut-travel[ 1 ]-travel_id %control-agency_id = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on )
                   ( travel_id = mapped_cut-travel[ 2 ]-travel_id %control-agency_id = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

    cl_abap_unit_assert=>assert_equals( act = lines( result_read ) exp = 2 ).
    cl_abap_unit_assert=>assert_initial( result_read[ 1 ]-agency_id ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-description exp = 'Travel_987' ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 2 ]-agency_id exp = '000111' ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 2 ]-description exp = 'Travel_988' ).

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
   data create_travel_instances type table for create /DMO/I_TRAVEL_M.
   create_travel_instances = value #( ( %cid = 'Travel_987' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_987' ) ).
                                      "Configured Input instance should match exactly with the instance in code under test, (%control structure matching is ignored)

  "Step 3: Define and set up the failed response structure to be returned for Create EML on test data instance in CUT
    data failed type response for failed early /dmo/i_travel_m.
    failed-travel = value #( ( %cid = 'Travel_987' ) ).  "Create travel_987 fails, assuming customer_id does not exists

  "Step 4: set_failed using configure_addtional_behavior API on double to configure CREATE EML on an instance in code under test to fail.

    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).

    data(input) = double->create_modify_input_config(  )->set_instance( create_travel_instances[ 1 ] ).
                              "(Unlike the other variant i.e. MOCKEMLAPI variant of RAP BO TDF, the input configuration here only consists of an input instance on which operation is performed)
    data(output) = double->create_modify_output_config( )->set_failed( failed ).
    double->configure_additional_behavior(  )->for_modify_create(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For Create on Travel entity
   data create_travel_instances_cut type table for create /dmo/i_travel_m.
   create_travel_instances_cut = value #( ( %CID = 'Travel_987' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_987'
                                             %control-Customer_ID = if_abap_behv=>mk-on %control-description = if_abap_behv=>mk-on ) ) .

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
        travel   = value #( ( %key-Travel_ID = 1 ) )
      importing
        mapped   = data(mapped_delete)
        reported = data(reported_delete)
        failed   = data(failed_delete)
    ).
  """"""""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""

    cl_abap_unit_assert=>assert_initial( mapped_delete ).
    cl_abap_unit_assert=>assert_not_initial( failed_delete ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_delete-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = failed_delete-travel[ 1 ]-travel_id ).

  endmethod.

  method isolate_delete_to_pass.
  "Test Goal: Isolate the Modify EML with Delete for travel in CUT (Code under Test) to pass by configuring the transactional buffer double.
  "Delete EML in CUT should successfully delete the instance recorded in Framework's Transactional Buffer Double

  "Step 1: Set numbering support on double (if required) via set_fields_handler API. (set_mapped can also be used to define the key fields for an instance)
  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).

  "Step 1: Set numbering support on double (if required).
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).


  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Record a travel instance which is to be deleted. ( Will be recorded in Framework's Buffer and an ID will be returned by the framework according to fields_handler )
   "Use Deep Create to insert a travel and 2 bookings for that travel.
    modify entities of /dmo/i_travel_m
     entity travel
             create fields ( customer_id begin_date end_date overall_status description ) with
                         value #( ( %cid        = 'Travel_1'    " Preliminary ID for new travel instance
                                    customer_id  = '000006'
                                    description = 'Travel 1'
                                    overall_status = 'O' ) ) "Travel status open
             create by \_booking fields ( carrier_id connection_id customer_id Booking_Date booking_status Flight_Date ) with
                         value #( ( %cid_ref  = 'Travel_1'      "refers to the root (travel instance)
                                    %target   = value #( ( %cid = 'Travel_1_Booking_1' " Preliminary ID for new booking instance
                                                           carrier_id = '000111'
                                                           connection_id = '1234'
                                                           Customer_ID = '000006'
                                                           Flight_Date = cl_abap_context_info=>get_system_date( ) + 10
                                                           booking_date = cl_abap_context_info=>get_system_date( )
                                                           booking_status = 'B' )
                                                         ( %cid = 'Travel_1_Booking_2' " Preliminary ID for new booking instance
                                                           carrier_id = '000112'
                                                           connection_id = '2345'
                                                           Customer_ID = '000006'
                                                           Flight_Date = cl_abap_context_info=>get_system_date( ) + 10
                                                           booking_date = cl_abap_context_info=>get_system_date( )
                                                           booking_status = 'B' ) ) ) )
       MAPPED   data(mapped_test_data)
       FAILED   data(failed_test_data)
       REPORTED data(reported_test_data).

      "Check if the instances are actually inserted.
     "according to the fields_handler, key assigned to travel instance will be 1 and for two bookings id 1 and 2 resp.
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-travel[ 1 ]-travel_id exp = 1 ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 1 ]-travel_id exp = 1 ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 1 ]-booking_id exp = 1 ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 2 ]-travel_id exp = 1 ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 2 ]-booking_id exp = 2 ).

     read entity /dmo/i_travel_m
       from value #( ( travel_id = mapped_test_data-travel[ 1 ]-travel_id %control-description = if_abap_behv=>mk-on ) )
         result   data(result_read_travel)
       by \_Booking from value #( ( travel_id = mapped_test_data-travel[ 1 ]-travel_id %control-connection_id = if_abap_behv=>mk-on ) )
         result   data(result_read_booking)
       reported data(reported_read)
       failed   data(failed_read).


     cl_abap_unit_assert=>assert_equals( act = result_read_travel[ 1 ]-travel_id exp = 1 ).
     cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 'Travel 1' act = result_read_travel[ 1 ]-description ).

     cl_abap_unit_assert=>assert_equals( act = result_read_booking[ 1 ]-travel_id exp = 1 ).
     cl_abap_unit_assert=>assert_equals( act = result_read_booking[ 1 ]-booking_id exp = 1 ).
     cl_abap_unit_assert=>assert_equals( act = result_read_booking[ 2 ]-travel_id exp = 1 ).
     cl_abap_unit_assert=>assert_equals( act = result_read_booking[ 2 ]-booking_id exp = 2 ).
     cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = '1234' act = result_read_booking[ 1 ]-connection_id ).
     cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = '2345' act = result_read_booking[ 2 ]-connection_id ).

  """"""""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""
    " The Framework will evaluate the delete and delete the instance (and its associated instances) from the buffer double recorded in step 2 in Buffer.

      cut->delete_travel(
        exporting
          travel   = value #( ( %key-Travel_ID = 1 ) )
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
     read entity /dmo/i_travel_m
       from value #( ( travel_id = mapped_test_data-travel[ 1 ]-travel_id %control-description = if_abap_behv=>mk-on ) )
         result   data(result_read_travel_deleted)
       by \_Booking from value #( ( travel_id = mapped_test_data-travel[ 1 ]-travel_id %control-connection_id = if_abap_behv=>mk-on ) )
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
        travel   = value #( ( %key-Travel_ID = 1
                             total_price = 200
                             %control-total_price = if_abap_behv=>mk-on ) )
      importing
        mapped   = data(mapped_update)
        reported = data(reported_update)
        failed   = data(failed_update)
    ).
  """"""""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""

    cl_abap_unit_assert=>assert_initial( mapped_update ).
    cl_abap_unit_assert=>assert_not_initial( failed_update ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_update-travel ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = failed_update-travel[ 1 ]-travel_id ).

    "Check if the instance was not updated.
    read entity /dmo/i_travel_m
     from value #( ( travel_id = failed_update-travel[ 1 ]-travel_id
                     %control-total_price = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

    cl_abap_unit_assert=>assert_initial( result_read ).

  endmethod.

  method isolate_create_ba_set_failed.
  "Test Goal: Isolate the Modify EML with Create booking by association on Travel in CUT (Code under Test) to fail by configuring additional behavior on the transactional buffer double.
  "The create_ba operation on travel 1 should fail due to no authorization.
  " i.e., the fail cause in the failed response should be "unauthorized"

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Setup test data instance for Create by association EML in code under test.
  "Step 3: Define and set up the failed response structure to be returned for Create by association EML on test data instance in CUT
  "Step 4: set_failed using configure_addtional_behavior API on double to configure Create by association EML on test data instance in code under test to fail.
          "set_failed API can be used to force an operation to fail.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "No record step needed.
   "The create by association in CUT will return failed, configured via set_failed.

  "Step 2: Setup test data instance for Create by association EML in code under test.
   data cba_booking_instances type table for create /DMO/I_TRAVEL_M\_Booking.
   cba_booking_instances = value #( ( Travel_ID = 1
                                      %target = value #( (  %cid = 'Travel_1_Booking_1'
                                                            carrier_id = '111' %control-carrier_id = if_abap_behv=>mk-on
                                                            connection_id = '1234' %control-connection_id = if_abap_behv=>mk-on
                                                            Customer_ID = '000006' %control-Customer_ID = if_abap_behv=>mk-on "For mandatory fields, control structure should be marked.
                                                            Flight_Date = cl_abap_context_info=>get_system_date( ) + 10 %control-Flight_Date = if_abap_behv=>mk-on
                                                            booking_date = cl_abap_context_info=>get_system_date( ) %control-booking_date = if_abap_behv=>mk-on
                                                            booking_status = 'B' %control-booking_status = if_abap_behv=>mk-on ) ) ) ).


  "Step 3: Define and set up the failed response structure to be returned for Create by association EML on test data instance in CUT.
    data failed type response for failed early /dmo/i_travel_m.
    failed-booking = value #( ( %cid = 'Travel_1_Booking_1' Travel_ID = 1 %fail-cause = if_abap_behv=>cause-unauthorized ) ).


  "Step 4: set_failed using configure_addtional_behavior API on double to configure Create by association EML on an instance in code under test to fail.
    "Get the double from environment
    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).

    "Create input configuration with the instance
    data(input) = double->create_modify_input_config(  )->set_instance( cba_booking_instances[ 1 ] ).
                                                                                "Note that the input instance should contain only one instance for %target
    "Create output configuration with failed response
    data(output) = double->create_modify_output_config( )->set_failed( failed ).

    "configure additional behavior for create by association operation
    "by configuring the output configuration for an input configuration
    double->configure_additional_behavior(  )->for_modify_create_ba(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For Create by association on Travel entity

    cut->create_ba_booking_for_travel_1(
    importing
      mapped   = data(mapped_cut)
      reported = data(reported_cut)
      failed   = data(failed_cut)
  ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""

    "Asserts
    cl_abap_unit_assert=>assert_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_equals( act = lines( failed_cut-booking ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = failed_cut-booking[ 1 ]-travel_id ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-unauthorized act = failed_cut-Booking[ 1 ]-%fail-cause ).
    cl_abap_unit_assert=>assert_equals( exp = 'Travel_1_Booking_1' act = failed_cut-booking[ 1 ]-%cid  ).

  endmethod.

  method isolate_create_ba_set_mapped.
  "Test Goal: Isolate the Modify EML with Create booking by association on Travel in CUT (Code under Test) to pass by configuring additional behavior on the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Setup test data instance for Create by association EML in code under test.
  "Step 3: Define and set up the mapped response structure to be returned for Create by association EML on test data instance in CUT
  "Step 4: set_mapped using configure_addtional_behavior API on double to configure Create by association EML on test data instance in code under test to pass.
          "The keys for the instance will be set from the mapped structure provided in "set_mapped" complimentary API.
          "set_mapped will be successful only if the create by association can actually be performed on the current transactional buffer double state.
            "Ex. If booking instance for travel_id 988 is to be created but travel_id 988 does not already exists in the buffer double,
                    "setting mapped with travel_id 988 and some booking_id will fail as the source instance does not exist.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Insert a travel with travel_id 987
   "The create in test method will perform the create operation on the buffer double.
   data create_travel_instances type table for create /DMO/I_TRAVEL_M.
   create_travel_instances = value #( ( %cid = 'Travel_987' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_987' ) ).

   "using set mapped API, set the key for the travel as 987
   data mapped_travel_987 type response for mapped early /dmo/i_travel_m.
   mapped_travel_987-travel = value #( ( %cid = 'Travel_987' Travel_ID = 987 ) ).  "Create a travel with travel_id 987

   "configure the double for create in test method"
   data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
   data(input) = double->create_modify_input_config(  )->set_instance( create_travel_instances[ 1 ] ).
   data(output) = double->create_modify_output_config( )->set_mapped( mapped = mapped_travel_987 ).
   double->configure_additional_behavior(  )->for_modify_create(  )->when_input( input )->then_set_output( output ).

   "Insert the travel with Create EML
    modify entities of /dmo/i_travel_m
     entity travel
       create fields ( Agency_ID customer_id description )
        with value #( ( %cid = 'Travel_987' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_987'  ) ) "Since set_mapped is used for the instance,
                                                                                                                                      "key 987 will be assigned to the instance and inserted
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-travel_id exp = 987 ).


  "Step 2: Setup test data instance for Create by association EML in code under test.
   data cba_booking_instances type table for create /DMO/I_TRAVEL_M\_Booking.
   cba_booking_instances = value #( ( Travel_ID = 987
                                      %target = value #( (  %cid = 'Travel_987_Booking_001'
                                                            carrier_id = '111' %control-carrier_id = if_abap_behv=>mk-on
                                                            connection_id = 'C1' %control-connection_id = if_abap_behv=>mk-on
                                                            Customer_ID = '000006' %control-Customer_ID = if_abap_behv=>mk-on "For mandatory fields, control structure should be marked.
                                                            Flight_Date = cl_abap_context_info=>get_system_date( ) + 10 %control-Flight_Date = if_abap_behv=>mk-on
                                                            booking_date = cl_abap_context_info=>get_system_date( ) %control-booking_date = if_abap_behv=>mk-on
                                                            booking_status = 'B' %control-booking_status = if_abap_behv=>mk-on ) ) ) ).


  "Step 3: Define and set up the mapped response structure to be returned for Create by association EML on test data instance in CUT, with the key to be assigned for the instance.
    data mapped_booking_001 type response for mapped early /dmo/i_travel_m.
    mapped_booking_001-booking = value #( ( %cid = 'Travel_987_Booking_001' Travel_ID = 987 booking_id =  001 ) ).  "Create a booking for travel_id 987 with booking_id 001


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
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-travel_id exp = 987 ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-booking[ 1 ]-booking_id exp = 001 ).

    "The TXBUFDBL variant, creates the instance with key fields provided via the set_mapped API and inserts into the transactional buffer double
    "Read the data to assert create by association
    read entity /dmo/i_travel_m
      by \_Booking
     from value #( ( travel_id = mapped_cut-booking[ 1 ]-travel_id %control-booking_status = if_abap_behv=>mk-on %control-carrier_id = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

    cl_abap_unit_assert=>assert_equals( act = lines( result_read ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-booking_id exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-booking_status exp = 'B' ).
    cl_abap_unit_assert=>assert_equals( act = result_read[ 1 ]-carrier_id exp = '111' ).

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
   data update_travel_instances type table for update /dmo/i_travel_m.
   update_travel_instances = value #( ( travel_id = 987
                              total_price = 200
                              %control-total_price = if_abap_behv=>mk-on )
                          ).


  "Step 3: Define and set up the failed response structure to be returned for update EML on test data instance in CUT.
    data failed type response for failed early /dmo/i_travel_m.
    failed-travel = value #( ( Travel_ID = 987 %fail-cause = if_abap_behv=>cause-not_found %update = if_abap_behv=>mk-on ) ).


  "Step 4: set_failed using configure_addtional_behavior API on double to configure update EML on an instance in code under test to fail.

    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
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
    cl_abap_unit_assert=>assert_equals( exp = 987 act = failed_cut-travel[ 1 ]-travel_id ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-not_found act = failed_cut-travel[ 1 ]-%fail-cause ).

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
   data delete_travel_instances type table for delete /dmo/i_travel_m.
   delete_travel_instances = value #( ( travel_id = 987 ) ).

  "Step 3: Define and set up the failed response structure to be returned for update EML on test data instance in CUT.
    data failed type response for failed early /dmo/i_travel_m.
    failed-travel = value #( ( Travel_ID = 987 %fail-cause = if_abap_behv=>cause-not_found %delete = if_abap_behv=>mk-on ) ).

  "Step 4: set_failed using configure_addtional_behavior API on double to configure delete EML on an instance in code under test to fail.

    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
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
    cl_abap_unit_assert=>assert_equals( exp = 987 act = failed_cut-travel[ 1 ]-travel_id ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-not_found act = failed_cut-travel[ 1 ]-%fail-cause ).

  endmethod.


  method isolate_update_set_mapped.
  "Test Goal: Isolate the Modify EML with Update on Travel in CUT (Code under Test) to pass by configuring additional behavior on the transactional buffer double.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
  "Step 2: Setup test data instance for Update EML in code under test.
  "Step 3: Define and set up the mapped response structure to be returned for Update EML on test data instance in CUT
  "Step 4: set_mapped using configure_addtional_behavior API on double to configure Update EML on test data instance in code under test to pass.
          "set_mapped will be successful only if the update can actually be performed on the current transactional buffer double state.
            "Ex. If instance for travel_id 988 does not already exists in the buffer double,
                    "setting mapped with travel_id 988 for update will fail as instance to be updated does not exist.

  "Step 1: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Insert a travel with travel_id 987
   "The create in test method will perform the create operation on the buffer using the key set using set_mapped. (alternatively "set_fields_handler" can be used to set numbering support)
   data create_travel_instances type table for create /DMO/I_TRAVEL_M.
   create_travel_instances = value #( ( %cid = 'Travel_987' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_987' ) ).

   "using set mapped API, set the key for the travel as 987
   data mapped_travel_987 type response for mapped early /dmo/i_travel_m.
   mapped_travel_987-travel = value #( ( %cid = 'Travel_987' Travel_ID = 987 ) ).  "Create a travel with travel_id 987

   "configure the double for create in test method (below)"
   data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
   data(input) = double->create_modify_input_config(  )->set_instance( create_travel_instances[ 1 ] ).
   data(output) = double->create_modify_output_config( )->set_mapped( mapped = mapped_travel_987 ).
   double->configure_additional_behavior(  )->for_modify_create(  )->when_input( input )->then_set_output( output ).

   "Insert the travel with Create EML
    modify entities of /dmo/i_travel_m
     entity travel
       create fields ( Agency_ID customer_id description )
        with value #( ( %cid = 'Travel_987' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_987'  ) ) "Since set_mapped is used for the instance,
                                                                                                                                      "key 987 will be assigned to the instance and inserted
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-travel_id exp = 987 ).


  "Step 2: Setup test data instance for Update EML in code under test.
   data update_travel_instances type table for update /dmo/i_travel_m.
   update_travel_instances = value #( ( travel_id = 987
                              total_price = 200
                              %control-total_price = if_abap_behv=>mk-on )
                          ).


  "Step 3: Define and set up the mapped response structure to be returned for update EML on test data instance in CUT.
    data mapped_update type response for mapped early /dmo/i_travel_m.
    mapped_update-travel = value #( ( Travel_ID = 987 ) ).  "Update travel_id 987 created and inserted in the buffer double above.


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
    cl_abap_unit_assert=>assert_equals( exp = 987 act = mapped_cut-travel[ 1 ]-travel_id ).

    "Check if the instance was updated.
    read entity /dmo/i_travel_m
     from value #( ( travel_id = mapped_cut-travel[ 1 ]-travel_id
                     %control-total_price = if_abap_behv=>mk-on ) )
     result   data(result_read)
     reported data(reported_read)
     failed   data(failed_read).

     cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 200 act = result_read[ 1 ]-Total_Price ).

  endmethod.

  method isolate_read.
  "Test Goal: Isolate the Read EML with read for travel in CUT (Code under Test) by configuring the transactional buffer double.

  "Step 1: Set numbering support on double (if required) via set_fields_handler API (for create during test configuration).
  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).

  "Step 1: Set numbering support on double (if required).
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).

  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Record a travel instance which is to be read in code under test. ( Will be recorded in Framework's Buffer and an ID will be returned by the framework according to fields_handler )
    modify entities of /dmo/i_travel_m
     entity travel
       create from value #( ( %cid = 'Travel_1'
                              Agency_ID = '000111' Customer_ID = '000006' description = 'Travel 1'
                              %control-Agency_ID = if_abap_behv=>mk-on
                              %control-Customer_ID = if_abap_behv=>mk-on
                              %control-description = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

     "according to the fields_handler, key assigned to travel instance will be 1.
    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-travel_id exp = 1 ).


  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""
      cut->read_travel(
        exporting
          travel   =  value #( ( travel_id =  mapped-travel[ 1 ]-travel_id      "Read should be successful as travel 1 exists
                                   %control-description = if_abap_behv=>mk-on )
                               ( travel_id =  2     "Travel 2 does not exist, read should fail
                                   %control-description = if_abap_behv=>mk-on )  )
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

    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-Travel_ID exp = ( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-description exp = 'Travel 1' ).
    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-Travel_ID exp = ( 2 ) ).

  endmethod.

  method isolate_read_to_pass.
  "Test Goal: Isolate the Read EML with read for travel in CUT (Code under Test) by configuring the transactional buffer double.

  "Step 1: Set numbering support on double (if required) via set_fields_handler API (for create during test configuration).
  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).

  "Step 1: Set numbering support on double (if required).
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).

  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Record a travel instance which is to be read in code under test. ( Will be recorded in Framework's Buffer and an ID will be returned by the framework according to fields_handler )
    modify entities of /dmo/i_travel_m
     entity travel
       create from value #( ( %cid = 'Travel_1'
                              Agency_ID = '000111' Customer_ID = '000006' description = 'Travel 1'
                              %control-Agency_ID = if_abap_behv=>mk-on
                              %control-Customer_ID = if_abap_behv=>mk-on
                              %control-description = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

     "according to the fields_handler, key assigned to travel instance will be 1.
    cl_abap_unit_assert=>assert_equals( act = mapped-travel[ 1 ]-travel_id exp = 1 ).


  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""
  "Execute the code under test (cut) with read operation
  ""The code under test has a read on travel_id '1'. Since a travel with id '1' exists, read for this instance should be successful.
  "The read operation in cut will be taken care by the generic implementation provided by TXBUFDBL variant and will return the appropriate output responses.
       cut->read_travel_1(
        importing
          result   = data(result_cut)
          reported = data(reported_cut)
          failed   = data(failed_cut)
      ).

  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""

    "Asserts
    "Note: Asserts will depend on the use case. These asserts are based on the given code under test and are for demonstration purpose only.
    cl_abap_unit_assert=>assert_not_initial( result_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( act = lines( result_cut ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-Travel_ID exp = ( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-description exp = 'Travel 1' ).

  endmethod.


  method isolate_read_ba.
  "Test Goal: Isolate the Read EML with read booking by association operation on travel in CUT (Code under Test) by configuring the transactional buffer double.

  "Step 1: Set numbering support on double (if required) via set_fields_handler API (for create during test configuration).
  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).

  "Step 1: Set numbering support on double (if required).
  "Since early numbering is enabled for BO and the key fields are read-only, setting a numbering support is required to assign keys to instances for create or create_ba operations
    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    double->configure_additional_behavior(  )->set_fields_handler( fields_handler = new ltd_fields_handler( ) ).

  "Step 2: Insert test data in transactional buffer double using a CREATE EML (if required).
   "Record a travel instance and bookings for it  which will be read in code under test by association. ( Will be recorded in Framework's Buffer and an ID will be returned by the framework according to fields_handler )
   "Use Deep Create to insert a travel and 2 bookings for that travel.
    modify entities of /dmo/i_travel_m
     entity travel
             create fields ( customer_id begin_date end_date overall_status description ) with
                         value #( ( %cid        = 'Travel_1'    " Preliminary ID for new travel instance
                                    customer_id  = '000006'
                                    description = 'Travel 1'
                                    overall_status = 'O' ) ) "Travel status open
             create by \_booking fields ( carrier_id connection_id customer_id Booking_Date booking_status Flight_Date ) with
                         value #( ( %cid_ref  = 'Travel_1'      "refers to the root (travel instance)
                                    %target   = value #( ( %cid = 'Travel_1_Booking_1' " Preliminary ID for new booking instance
                                                           carrier_id = '000111'
                                                           connection_id = '1234'
                                                           Customer_ID = '000006'
                                                           Flight_Date = cl_abap_context_info=>get_system_date( ) + 10
                                                           booking_date = cl_abap_context_info=>get_system_date( )
                                                           booking_status = 'B' )
                                                         ( %cid = 'Travel_1_Booking_2' " Preliminary ID for new booking instance
                                                           carrier_id = '000112'
                                                           connection_id = '2345'
                                                           Customer_ID = '000006'
                                                           Flight_Date = cl_abap_context_info=>get_system_date( ) + 10
                                                           booking_date = cl_abap_context_info=>get_system_date( )
                                                           booking_status = 'B' ) ) ) )
       MAPPED   data(mapped_test_data)
       FAILED   data(failed_test_data)
       REPORTED data(reported_test_data).

      "Check if the instances are actually inserted.
     "according to the fields_handler, key assigned to travel instance will be 1 and for two bookings id 1 and 2 resp.
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-travel[ 1 ]-travel_id exp = 1 ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 1 ]-travel_id exp = 1 ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 1 ]-booking_id exp = 1 ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 2 ]-travel_id exp = 1 ).
        cl_abap_unit_assert=>assert_equals( act = mapped_test_data-booking[ 2 ]-booking_id exp = 2 ).


  """""""""""""""""""""""""""CODE UNDER TEST""""""""""""""""""""""""""""""""""""""
      cut->read_booking_by_assoc(
        exporting
          travel   = value #( ( travel_id =  mapped_test_data-travel[ 1 ]-travel_id      "Read_ba should be successful as travel 1 exists and booking instances should be returned
                                   %control-connection_id = if_abap_behv=>mk-on )
                               ( travel_id =  2     "Travel 2 does not exist, read_ba should fail
                                   %control-connection_id = if_abap_behv=>mk-on )  )
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

    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-Travel_ID exp = ( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 1 ]-Booking_ID exp = ( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = result_cut[ 2 ]-Booking_ID exp = ( 2 ) ).

    cl_abap_unit_assert=>assert_equals( act = failed_cut-travel[ 1 ]-Travel_ID exp = ( 2 ) ).

    cl_abap_unit_assert=>assert_equals( act = lines( links_cut ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = links_cut[ 1 ]-source-travel_id exp = ( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = links_cut[ 1 ]-target-booking_id exp = ( 1 ) ).
    cl_abap_unit_assert=>assert_equals( act = links_cut[ 2 ]-target-Booking_ID exp = ( 2 ) ).

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
   data read_travel_instances type table for read import /dmo/i_travel_m.
   read_travel_instances = value #( ( travel_id = 987 ) ).


  "Step 3: Define and set up the failed response structure to be returned for read EML on test data instance in CUT.
    data failed type response for failed early /dmo/i_travel_m.
    failed-travel = value #( ( Travel_ID = 987 %fail-cause = if_abap_behv=>cause-not_found ) ).


  "Step 4: set_failed using configure_addtional_behavior API on double to configure read EML on an instance in code under test to fail.

    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
    data(input) = double->create_read_input_config(  )->set_instance( read_travel_instances[ 1 ] ).
                                                                                "Note that the input instance should contain only one instance
    data(output) = double->create_read_output_config( )->set_failed( failed ).
    double->configure_additional_behavior(  )->for_read(  )->when_input( input )->then_set_output( output ).

  """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
  "For read on Travel entity

      cut->read_travel(
        exporting
          travel   =  value #( ( travel_id =  987      "Read should return failed as configured
                                   %control-description = if_abap_behv=>mk-on ) )
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
    cl_abap_unit_assert=>assert_equals( exp = 987 act = failed_cut-travel[ 1 ]-travel_id ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-not_found act = failed_cut-travel[ 1 ]-%fail-cause ).

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
   data rba_booking_instances type table for read import /dmo/i_travel_m\_Booking.
   rba_booking_instances = value #( ( travel_id = 987 ) ).


  "Step 3: Define and set up the failed response structure to be returned for read by association EML on test data instance in CUT
    data failed type response for failed early /dmo/i_travel_m.
    failed-travel = value #( ( Travel_ID = 987 %fail-cause = if_abap_behv=>cause-not_found ) ).


  "Step 4: set_failed using configure_addtional_behavior API on double to configure read by association EML on an instance in code under test to fail.

    data(double) =  environment->get_test_double( '/DMO/I_TRAVEL_M' ).
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
    cl_abap_unit_assert=>assert_equals( exp = 987 act = failed_cut-travel[ 1 ]-travel_id ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-not_found act = failed_cut-travel[ 1 ]-%fail-cause ).

  endmethod.


endclass.
