"! @testing BDEF:/DMO/R_Travel_D
CLASS /dmo/tc_travel_d_ktd_examples DEFINITION
  PUBLIC
  FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CONSTANTS:
      cid                         TYPE abp_behv_cid                 VALUE 'Test1',
      existing_customerid         TYPE /dmo/i_travel_d-customerid   VALUE '123',
      another_existing_customerid TYPE /dmo/i_travel_d-customerid   VALUE '321',
      existing_agencyid           TYPE /dmo/i_travel_d-agencyid     VALUE '42',
      another_existing_agencyid   TYPE /dmo/i_travel_d-agencyid     VALUE '24',
      valid_date                  TYPE /dmo/i_travel_d-begindate    VALUE '20230101',
      valid_date_after_being_date TYPE /dmo/i_travel_d-enddate      VALUE '20230204',
      valid_currency_code         TYPE /dmo/i_travel_d-currencycode VALUE 'EUR',
      existing_travel             TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A1'.

    CLASS-DATA:
      cds_test_environment TYPE REF TO if_cds_test_environment,
      sql_test_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      "! Instantiate class under test and set up test double framework
      class_setup,

      "! Destroy test environment and test double
      class_teardown.

    DATA:
      mapped      TYPE RESPONSE FOR MAPPED   EARLY /dmo/i_travel_d,
      failed      TYPE RESPONSE FOR FAILED   EARLY /dmo/i_travel_d,
      reported    TYPE RESPONSE FOR REPORTED EARLY /dmo/i_travel_d,
      read_result TYPE TABLE FOR READ RESULT /dmo/i_travel_d\\travel.

    METHODS:
      "! Read an instance using EML.
      read_via_eml,
      "! Creating a new instance using EML.
      create_via_eml,
      "! Updating the customer for an instance using EML.
      updating_customer_via_eml,
      "! Updating the agency for an instance using EML.
      updating_agency_via_eml,
      "! Deleting an instance using EML.
      delete_via_eml,
      "! Executing the action <em>Accept Travel</em> on an instance with status <em>New</em> using EML.
      action_accept_travel_via_eml.


    METHODS:
      "! Reset test double and set test data for currency and travel.
      setup,

      "! Reset transactional buffer
      teardown.


    METHODS:
      "! Reading an instance.
      test_read_via_eml  FOR TESTING,
      "! Creating a new instance.
      test_create_via_eml  FOR TESTING,
      "! Updating the customer for an instance.
      test_updating_customer_via_eml  FOR TESTING,
      "! Updating the customer for an instance.
      test_updating_agency_via_eml  FOR TESTING,
      "! Deleting an instance.
      test_delete_via_eml  FOR TESTING,
      "! Using the action <em>Accept Tracel</em> on an instance.
      t_action_accept_travel_via_eml  FOR TESTING.


ENDCLASS.


CLASS /dmo/tc_travel_d_ktd_examples IMPLEMENTATION.

  METHOD class_setup.
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
                               VALUE #(
                                   ( i_for_entity = '/DMO/R_Travel_D'            )
                                   ( i_for_entity = '/DMO/R_Booking_D'           )
                                   ( i_for_entity = '/DMO/R_BookingSupplement_D' )
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
    DATA:
      currencies       TYPE STANDARD TABLE OF i_currency,
      travel_mock_data TYPE STANDARD TABLE OF /dmo/a_travel_d.

    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).

    CLEAR mapped.
    CLEAR reported.
    CLEAR failed.

    currencies = VALUE #(
        ( currency = valid_currency_code )
        ( currency = 'USD' )
      ).
    sql_test_environment->insert_test_data( currencies ).

    travel_mock_data = VALUE #(
        (
          travel_uuid    = existing_travel
          customer_id    = existing_customerid
          agency_id      = existing_agencyid
          begin_date     = valid_date
          end_date       = valid_date_after_being_date
          overall_status = 'N'
        )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
    cds_test_environment->destroy( ).
    sql_test_environment->destroy( ).
  ENDMETHOD.

  METHOD test_create_via_eml.
    create_via_eml( ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_not_initial( mapped ).
    cl_abap_unit_assert=>assert_equals(
        exp = '1'
        act =  lines( mapped-travel )
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = cid
        act = mapped-travel[ 1 ]-%cid
      ).
  ENDMETHOD.


  METHOD create_via_eml.
    MODIFY ENTITIES OF /dmo/i_travel_d
      ENTITY travel
        CREATE
          FIELDS ( customerid agencyid begindate enddate currencycode )
          WITH VALUE #(
            (
              %cid         = cid
              customerid   = existing_customerid
              agencyid     = existing_agencyid
              begindate    = valid_date
              enddate      = valid_date_after_being_date
              currencycode = valid_currency_code
            )
          )
      REPORTED reported
      FAILED   failed
      MAPPED   mapped.
  ENDMETHOD.

  METHOD action_accept_travel_via_eml.
    MODIFY ENTITIES OF /dmo/i_travel_d
      ENTITY travel
        EXECUTE accepttravel FROM VALUE #(
            (
              traveluuid = existing_travel
            )
          )
        RESULT DATA(accept_travel_result)
      REPORTED reported
      FAILED   failed
      MAPPED   mapped.
  ENDMETHOD.

  METHOD delete_via_eml.
    MODIFY ENTITIES OF /dmo/i_travel_d
      ENTITY travel
        DELETE FROM VALUE #(
            (
              traveluuid = existing_travel
            )
          )
      REPORTED reported
      FAILED   failed
      MAPPED   mapped.
  ENDMETHOD.

  METHOD read_via_eml.
    READ ENTITIES OF /dmo/i_travel_d
      ENTITY travel
          FIELDS ( customerid agencyid begindate enddate overallstatus )
          WITH VALUE #(
            (
              traveluuid = existing_travel
            )
          )
        RESULT read_result
      REPORTED reported
      FAILED   failed.
  ENDMETHOD.

  METHOD t_action_accept_travel_via_eml.
    action_accept_travel_via_eml( ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    read_via_eml( ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = '1'
        act =  lines( read_result )
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = 'A'
        act = read_result[ 1 ]-overallstatus
      ).
  ENDMETHOD.

  METHOD test_delete_via_eml.
    delete_via_eml( ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    read_via_eml( ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = '1'
        act =  lines( failed-travel )
      ).
  ENDMETHOD.

  METHOD test_read_via_eml.
    read_via_eml( ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = '1'
        act =  lines( read_result )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = existing_customerid
        act = read_result[ 1 ]-customerid
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = existing_agencyid
        act = read_result[ 1 ]-agencyid
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = valid_date
        act = read_result[ 1 ]-begindate
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = valid_date_after_being_date
        act = read_result[ 1 ]-enddate
      ).
  ENDMETHOD.

  METHOD test_updating_customer_via_eml.
    updating_customer_via_eml( ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    read_via_eml( ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = '1'
        act =  lines( read_result )
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = another_existing_customerid
        act = read_result[ 1 ]-customerid
      ).
  ENDMETHOD.

  METHOD updating_customer_via_eml.
    MODIFY ENTITIES OF /dmo/i_travel_d
      ENTITY travel
        UPDATE
          FIELDS ( customerid )
          WITH VALUE #(
            (
              traveluuid = existing_travel
              customerid = another_existing_customerid
            )
          )
      REPORTED reported
      FAILED   failed
      MAPPED   mapped.
  ENDMETHOD.

  METHOD test_updating_agency_via_eml.
    updating_agency_via_eml( ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    read_via_eml( ).

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = '1'
        act =  lines( read_result )
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = another_existing_agencyid
        act = read_result[ 1 ]-agencyid
      ).
  ENDMETHOD.

  METHOD updating_agency_via_eml.
    MODIFY ENTITIES OF /dmo/i_travel_d
      ENTITY travel
        UPDATE
          FIELDS ( agencyid )
          WITH VALUE #(
            (
              traveluuid = existing_travel
              agencyid = another_existing_agencyid
            )
          )
      REPORTED reported
      FAILED   failed
      MAPPED   mapped.
  ENDMETHOD.

ENDCLASS.
