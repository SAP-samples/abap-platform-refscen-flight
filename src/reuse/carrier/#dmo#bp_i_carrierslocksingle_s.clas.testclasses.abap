"! @testing BDEF:/DMO/I_CARRIER
CLASS lthc_carrier DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CONSTANTS airlineid_for_test TYPE /dmo/carrier_id VALUE 'XX4' ##NO_TEXT.

    CLASS-DATA: class_under_test     TYPE REF TO lhc_carrier,
                cds_test_environment TYPE REF TO if_cds_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    DATA:
      failed        TYPE RESPONSE FOR FAILED   EARLY /DMO/I_CarriersLockSingleton_S,
      reported      TYPE RESPONSE FOR REPORTED EARLY /DMO/I_CarriersLockSingleton_S,
      failed_late   TYPE RESPONSE FOR FAILED   LATE  /DMO/I_CarriersLockSingleton_S,
      reported_late TYPE RESPONSE FOR REPORTED LATE  /DMO/I_CarriersLockSingleton_S.


    METHODS:
      setup,
      teardown.

    METHODS:
      "! Checks if { @link ..lhc_carrier.METH:validateCurrencyCode } behaves correctly if an instance
      "! where Currency is set with an existing one.
      validateCurrencyCode_success   FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_carrier.METH:validateCurrencyCode } behaves correctly if instances
      "! where Currency is not set.  This should result in filled reported and failed tables.
      validateCurrencyCode_initial   FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_carrier.METH:validateCurrencyCode } behaves correctly if instances
      "! where Currency does not exists.  This should result in filled reported and failed tables.
      validateCurrencyCode_not_exist FOR TESTING RAISING cx_static_check,


      "! Checks if { @link ..lhc_carrier.METH:validateName } behaves correctly if an instance
      "! where the Name is set.
      validateName_success   FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_carrier.METH:validateName } behaves correctly if instances
      "! where the Name is not set.  This should result in filled reported and failed tables.
      validateName_initial   FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_carrier.METH:get_instance_features } behaves correctly.
      "! The test will hand-over two instances.
      "! One instance will have a connection and therefore will not be deleteable
      "! and one will not have a connection and will be deletable
      get_instance_features  FOR TESTING RAISING cx_static_check.


ENDCLASS.

CLASS lthc_carrier IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
        i_for_entities =  VALUE #(
            ( i_for_entity = '/DMO/I_CARRIER_S'  )
            ( i_for_entity = '/DMO/I_Connection' )
          )
      ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).

    CLEAR:
      failed,
      reported,
      failed_late,
      reported_late.
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
    cds_test_environment->destroy( ).
  ENDMETHOD.

  METHOD validateCurrencyCode_success.
    DATA:
      carrier  TYPE /DMO/I_Carrier,
      carriers TYPE STANDARD TABLE OF /DMO/I_Carrier.

    carrier = VALUE /DMO/I_Carrier(
        AirlineID     = airlineid_for_test
        CurrencyCode  = 'EUR'
      ).

    carriers = VALUE #( ( carrier ) ).
    cds_test_environment->insert_test_data( carriers ).

    class_under_test->validateCurrencyCode(
        EXPORTING
          keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  airlineid = carrier-AirlineID ) )
        CHANGING
          failed   = failed_late
          reported = reported_late
      ).

    cl_abap_unit_assert=>assert_initial(     failed_late   ).
    cl_abap_unit_assert=>assert_not_initial( reported_late ).

    " One line is expected as this clears the state area.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has more than one messages'
                                        exp = 1
                                        act = lines( reported_late-carrier ) ).
    DATA(reported_carrier) = reported_late-carrier[ 1 ].

    "Check that only %tky and %state_area is filled
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off      act = reported_carrier-%is_draft    ).
    cl_abap_unit_assert=>assert_equals( exp = carrier-AirlineID         act = reported_carrier-AirlineID ).
    cl_abap_unit_assert=>assert_equals( exp = 'VALIDATE_CURRENCY_CODE'  act = reported_carrier-%state_area ).
    cl_abap_unit_assert=>assert_initial( reported_carrier-%op ).
    cl_abap_unit_assert=>assert_initial( reported_carrier-%msg ).
    cl_abap_unit_assert=>assert_initial( reported_carrier-%element ).
  ENDMETHOD.

  METHOD validatecurrencycode_initial.
    DATA:
      carrier  TYPE /DMO/I_Carrier,
      carriers TYPE STANDARD TABLE OF /DMO/I_Carrier.

    carrier = VALUE /DMO/I_Carrier(
        AirlineID     = airlineid_for_test
      ).

    carriers = VALUE #( ( carrier ) ).
    cds_test_environment->insert_test_data( carriers ).

    class_under_test->validateCurrencyCode(
        EXPORTING
          keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  airlineid = carrier-AirlineID ) )
        CHANGING
          failed   = failed_late
          reported = reported_late
      ).

    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed_late ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( failed_late-carrier ) ).

    cl_abap_unit_assert=>assert_equals( exp = carrier-AirlineID
                                        act = failed_late-carrier[ 1 ]-AirlineID ).


    cl_abap_unit_assert=>assert_not_initial( reported_late ).
    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has not the correct amount of messages'
                                        exp = 2
                                        act = lines( reported_late-carrier ) ).

    LOOP AT reported_late-carrier INTO DATA(reported_line).
      cl_abap_unit_assert=>assert_equals( exp = carrier-AirlineID
                                          act = reported_line-AirlineID ).

      cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off      act = reported_line-%is_draft   ).
      cl_abap_unit_assert=>assert_equals( exp = 'VALIDATE_CURRENCY_CODE'  act = reported_line-%state_area ).

      IF reported_line-%msg IS BOUND.
        " actual message case
        cl_abap_unit_assert=>assert_equals( act = reported_line-%element-currencycode
                                            exp = if_abap_behv=>mk-on ).

      ELSE.
        " clear state area case
        cl_abap_unit_assert=>assert_initial( reported_line-%op ).
        cl_abap_unit_assert=>assert_initial( reported_line-%msg ).
        cl_abap_unit_assert=>assert_initial( reported_line-%element ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validatecurrencycode_not_exist.
    DATA:
      carrier  TYPE /DMO/I_Carrier,
      carriers TYPE STANDARD TABLE OF /DMO/I_Carrier.

    carrier = VALUE /DMO/I_Carrier(
        AirlineID     = airlineid_for_test
        CurrencyCode  = 'ÄÖÜ'
      ).

    carriers = VALUE #( ( carrier ) ).
    cds_test_environment->insert_test_data( carriers ).

    class_under_test->validateCurrencyCode(
        EXPORTING
          keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  airlineid = carrier-AirlineID ) )
        CHANGING
          failed   = failed_late
          reported = reported_late
      ).

    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed_late ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( failed_late-carrier ) ).

    cl_abap_unit_assert=>assert_equals( exp = carrier-AirlineID
                                        act = failed_late-carrier[ 1 ]-AirlineID ).


    cl_abap_unit_assert=>assert_not_initial( reported_late ).
    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has not the correct amount of messages'
                                        exp = 2
                                        act = lines( reported_late-carrier ) ).

    LOOP AT reported_late-carrier INTO DATA(reported_line).
      cl_abap_unit_assert=>assert_equals( exp = carrier-AirlineID
                                          act = reported_line-AirlineID ).

      cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off      act = reported_line-%is_draft   ).
      cl_abap_unit_assert=>assert_equals( exp = 'VALIDATE_CURRENCY_CODE'  act = reported_line-%state_area ).

      IF reported_line-%msg IS BOUND.
        " actual message case
        cl_abap_unit_assert=>assert_equals( act = reported_line-%element-currencycode
                                            exp = if_abap_behv=>mk-on ).

      ELSE.
        " clear state area case
        cl_abap_unit_assert=>assert_initial( reported_line-%op ).
        cl_abap_unit_assert=>assert_initial( reported_line-%msg ).
        cl_abap_unit_assert=>assert_initial( reported_line-%element ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateName_success.
    DATA:
      carrier  TYPE /DMO/I_Carrier,
      carriers TYPE STANDARD TABLE OF /DMO/I_Carrier.

    carrier = VALUE /DMO/I_Carrier(
        AirlineID = airlineid_for_test
        Name      = 'Testing'
      ).

    carriers = VALUE #( ( carrier ) ).
    cds_test_environment->insert_test_data( carriers ).

    class_under_test->validateName(
        EXPORTING
          keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  airlineid = carrier-AirlineID ) )
        CHANGING
          failed   = failed_late
          reported = reported_late
      ).

    cl_abap_unit_assert=>assert_initial(     failed_late   ).
    cl_abap_unit_assert=>assert_not_initial( reported_late ).

    " One line is expected as this clears the state area.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has more than one messages'
                                        exp = 1
                                        act = lines( reported_late-carrier ) ).
    DATA(reported_carrier) = reported_late-carrier[ 1 ].

    "Check that only %tky and %state_area is filled
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off act = reported_carrier-%is_draft    ).
    cl_abap_unit_assert=>assert_equals( exp = carrier-AirlineID    act = reported_carrier-AirlineID ).
    cl_abap_unit_assert=>assert_equals( exp = 'VALIDATE_NAME'      act = reported_carrier-%state_area ).
    cl_abap_unit_assert=>assert_initial( reported_carrier-%op ).
    cl_abap_unit_assert=>assert_initial( reported_carrier-%msg ).
    cl_abap_unit_assert=>assert_initial( reported_carrier-%element ).
  ENDMETHOD.

  METHOD validateName_initial.
    DATA:
      carrier  TYPE /DMO/I_Carrier,
      carriers TYPE STANDARD TABLE OF /DMO/I_Carrier.

    carrier = VALUE /DMO/I_Carrier(
        AirlineID = airlineid_for_test
      ).

    carriers = VALUE #( ( carrier ) ).
    cds_test_environment->insert_test_data( carriers ).

    class_under_test->validateName(
        EXPORTING
          keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  airlineid = carrier-AirlineID ) )
        CHANGING
          failed   = failed_late
          reported = reported_late
      ).

    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed_late ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( failed_late-carrier ) ).

    cl_abap_unit_assert=>assert_equals( exp = carrier-AirlineID
                                        act = failed_late-carrier[ 1 ]-AirlineID ).


    cl_abap_unit_assert=>assert_not_initial( reported_late ).
    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has not the correct amount of messages'
                                        exp = 2
                                        act = lines( reported_late-carrier ) ).

    LOOP AT reported_late-carrier INTO DATA(reported_line).
      cl_abap_unit_assert=>assert_equals( exp = carrier-AirlineID
                                          act = reported_line-AirlineID ).

      cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off act = reported_line-%is_draft   ).
      cl_abap_unit_assert=>assert_equals( exp = 'VALIDATE_NAME'      act = reported_line-%state_area ).

      IF reported_line-%msg IS BOUND.
        " actual message case
        cl_abap_unit_assert=>assert_equals( act = reported_line-%element-Name
                                            exp = if_abap_behv=>mk-on ).

      ELSE.
        " clear state area case
        cl_abap_unit_assert=>assert_initial( reported_line-%op ).
        cl_abap_unit_assert=>assert_initial( reported_line-%msg ).
        cl_abap_unit_assert=>assert_initial( reported_line-%element ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_instance_features.
    DATA:
      result                     TYPE  TABLE FOR INSTANCE FEATURES RESULT /dmo/i_carrierslocksingleton_s\\carrier,
      carrier_with_connection    TYPE /DMO/I_Carrier,
      carrier_without_connection TYPE /DMO/I_Carrier,
      carriers                   TYPE STANDARD TABLE OF /DMO/I_Carrier,
      connection                 TYPE /dmo/connection,
      connections                TYPE STANDARD TABLE OF /dmo/connection.

    carrier_with_connection = VALUE /DMO/I_Carrier(
        AirlineID = 'AAA'
      ).

    carrier_without_connection = VALUE /DMO/I_Carrier(
        AirlineID = 'BBB'
      ).

    carriers = VALUE #( ( carrier_with_connection ) ( carrier_without_connection ) ).
    cds_test_environment->insert_test_data( carriers ).

    connection = VALUE /dmo/connection(
        carrier_id      = carrier_with_connection-AirlineID
        connection_id   = '1234'
      ).
    connections = VALUE #( ( connection ) ).
    cds_test_environment->insert_test_data( connections ).

    class_under_test->get_instance_features(
        EXPORTING
          keys               = VALUE #(
                                   ( %is_draft = if_abap_behv=>mk-off  airlineid = carrier_with_connection-AirlineID )
                                   ( %is_draft = if_abap_behv=>mk-off  airlineid = carrier_without_connection-AirlineID )
                                 )
          requested_features = VALUE #( %delete = if_abap_behv=>mk-on )
        CHANGING
          result             = result
          failed             = failed
          reported           = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed   ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( reported-carrier ) ).
    cl_abap_unit_assert=>assert_equals( exp = carrier_with_connection-AirlineID
                                        act = reported-carrier[ 1 ]-AirlineID ).

    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( result ) ).

    cl_abap_unit_assert=>assert_not_initial( VALUE #( result[ KEY entity  AirlineID = carrier_with_connection-AirlineID ] OPTIONAL ) ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>fc-o-disabled
                                        act = result[ KEY entity  AirlineID = carrier_with_connection-AirlineID ]-%delete ).

    cl_abap_unit_assert=>assert_not_initial( VALUE #( result[ KEY entity  AirlineID = carrier_without_connection-AirlineID ] OPTIONAL ) ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>fc-o-enabled
                                        act = result[ KEY entity  AirlineID = carrier_without_connection-AirlineID ]-%delete ).

  ENDMETHOD.

ENDCLASS.

CLASS lthc_carrierslocksingleton DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "! Checks that { @link ..lhc_CarriersLockSingleton.METH:get_global_authorizations } returns initial values
      "! for <em>result</em> and <em>reported</em>.
      get_global_authorizations FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS lthc_carrierslocksingleton IMPLEMENTATION.

  METHOD get_global_authorizations.
    DATA:
      class_under_test         TYPE REF TO lhc_CarriersLockSingleton,
      requested_authorizations TYPE STRUCTURE FOR GLOBAL AUTHORIZATION REQUEST /DMO/I_CarriersLockSingleton_S\\CarriersLockSingleton,
      result                   TYPE STRUCTURE FOR GLOBAL AUTHORIZATION RESULT /DMO/I_CarriersLockSingleton_S\\CarriersLockSingleton,
      reported                 TYPE RESPONSE  FOR REPORTED EARLY /DMO/I_CarriersLockSingleton_S.


    CREATE OBJECT class_under_test FOR TESTING.

    requested_authorizations = VALUE #(
        %update      = if_abap_behv=>mk-on
        %action-Edit = if_abap_behv=>mk-on
      ).

    class_under_test->get_global_authorizations(
      EXPORTING
        requested_authorizations = requested_authorizations
      CHANGING
        result                   = result
        reported                 = reported
    ).

    cl_abap_unit_assert=>assert_initial( result   ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

ENDCLASS.

CLASS ltsc_I_CARRIERSLOCKSINGLETON_S DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      save_handler TYPE REF TO lsc_I_CARRIERSLOCKSINGLETON_S,
      mapped       TYPE RESPONSE FOR MAPPED   LATE /DMO/I_CarriersLockSingleton_S,
      failed       TYPE RESPONSE FOR FAILED   LATE /DMO/I_CarriersLockSingleton_S,
      reported     TYPE RESPONSE FOR REPORTED LATE /DMO/I_CarriersLockSingleton_S.

    METHODS:
      setup,
      _check_all_initial.

    METHODS:
      "! Checks if { @link ..lsc_I_CARRIERSLOCKSINGLETON_S.METH:save_modified } calls { @link FUNC:/DMO/FLIGHT_TRAVEL_SAVE }
      "! and does not raise any exception nor reporting anything.
      save_modified     FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lsc_I_CARRIERSLOCKSINGLETON_S.METH:cleanup_finalize } does not raise any exception.
      cleanup_finalize  FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltsc_I_CARRIERSLOCKSINGLETON_S IMPLEMENTATION.

  METHOD setup.
    CLEAR:
      save_handler,
      mapped,
      failed,
      reported.

    CREATE OBJECT save_handler FOR TESTING.
  ENDMETHOD.

  METHOD save_modified.
    TRY.
    save_handler->save_modified(
        EXPORTING
          create   = value #(  )
          update   = value #(  )
          delete   = value #(  )
        CHANGING
          reported = reported
      ).
    _check_all_initial( ).
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
