"! @testing BDEF:/DMO/R_AgencyTP
CLASS ltc_agency_handler DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_agency,
      sql_test_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    DATA:
      mapped        TYPE RESPONSE FOR MAPPED   EARLY /DMO/R_AgencyTP,
      failed        TYPE RESPONSE FOR FAILED   EARLY /DMO/R_AgencyTP,
      reported      TYPE RESPONSE FOR REPORTED EARLY /DMO/R_AgencyTP,
      failed_late   TYPE RESPONSE FOR FAILED   LATE  /DMO/R_AgencyTP,
      reported_late TYPE RESPONSE FOR REPORTED LATE  /DMO/R_AgencyTP.


    METHODS:
      setup,
      teardown.

    METHODS:
      "! Checks if { @link ..lhc_agency.METH:validateName } behaves correctly if an instance
      "! where Name is set correctly and state area is cleared.
      validate_name_success        FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_agency.METH:validateName } behaves correctly if instances
      "! where Name is not set correctly.  This should result in filled reported and failed tables.
      validate_name_failed         FOR TESTING RAISING cx_static_check,


      "! Checks if { @link ..lhc_agency.METH:validateemailaddress } behaves correctly if an instance
      "! where email address is set correctly and state area is cleared.
      validate_email_success        FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_agency.METH:validateemailaddress } behaves correctly if instances
      "! where email address is not set correctly.  This should result in filled reported and failed tables.
      "! All permutations are tested here.
      validate_email_failed         FOR TESTING RAISING cx_static_check,


      "! Checks if { @link ..lhc_agency.METH:validateCountryCode } behaves correctly if an instance
      "! where country code is set correctly and state area is cleared.
      validate_countrycode_success        FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_agency.METH:validateCountryCode } behaves correctly if instances
      "! where country code is not set correctly.  This should result in filled reported and failed tables.
      "! All permutations are tested here.
      validate_countrycode_failed         FOR TESTING RAISING cx_static_check,


      "! Checks that { @link ..lhc_agency.METH:get_global_authorizations } returns initial values
      "! for <em>result</em> and <em>reported</em>.
      get_global_authorizations     FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_agency_handler IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    sql_test_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( '/DMO/R_AgencyTP' ) ( 'I_COUNTRY' ) ) ).
  ENDMETHOD.

  METHOD setup.
    sql_test_environment->clear_doubles( ).

    CLEAR:
      mapped,
      failed,
      reported,
      failed_late,
      reported_late.
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
    sql_test_environment->destroy( ).
  ENDMETHOD.

  METHOD validate_name_success.
    DATA:
      agency   TYPE /DMO/R_AgencyTP,
      agencies TYPE STANDARD TABLE OF /DMO/R_AgencyTP.

    agency = VALUE /DMO/R_AgencyTP(
        AgencyId = '123'
        Name     = 'FlightBooker'
      ).

    agencies = VALUE #( ( agency ) ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatename(
        EXPORTING
          keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  agencyID = agency-AgencyId ) )
        CHANGING
          failed   = failed_late
          reported = reported_late
      ).

    cl_abap_unit_assert=>assert_initial(     failed_late   ).
    cl_abap_unit_assert=>assert_not_initial( reported_late ).

    " One line is expected as this clears the state area.
    cl_abap_unit_assert=>assert_equals(
        msg = 'Reported has more than one messages'
        exp = 1
        act = lines( reported_late-/DMO/Agency )
      ).
    DATA(reported_agency) = reported_late-/DMO/Agency[ 1 ].

    "Check that only %tky and %state_area is filled
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off                        act = reported_agency-%is_draft    ).
    cl_abap_unit_assert=>assert_equals( exp = agency-AgencyId                             act = reported_agency-agencyID ).
    cl_abap_unit_assert=>assert_equals( exp = class_under_test->state_area_validate_name  act = reported_agency-%state_area ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%op ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%msg ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%element ).
  ENDMETHOD.

  METHOD validate_name_failed.
    DATA:
      agency                    TYPE /DMO/R_AgencyTP,
      agencies                  TYPE STANDARD TABLE OF /DMO/R_AgencyTP,
      reported_with_message     TYPE STRUCTURE FOR REPORTED LATE /DMO/R_AgencyTP,
      reported_clear_state_area TYPE STRUCTURE FOR REPORTED LATE /DMO/R_AgencyTP.

    agency = VALUE /DMO/R_AgencyTP(
        AgencyId = '123'
        Name     = ''
      ).

    agencies = VALUE #( ( agency ) ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatename(
        EXPORTING
          keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  agencyID = agency-AgencyId ) )
        CHANGING
          failed   = failed_late
          reported = reported_late
      ).

    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed_late ).
    cl_abap_unit_assert=>assert_equals( exp = 1  act = lines( failed_late-/DMO/Agency ) ).

    LOOP AT failed-/DMO/Agency INTO DATA(failed_line).
      cl_abap_unit_assert=>assert_not_initial(
          msg = 'Failed key was not provided'
          act = VALUE #( agencies[ AgencyId = failed_line-AgencyId ] OPTIONAL )
        ).
    ENDLOOP.


    "check reported
    cl_abap_unit_assert=>assert_not_initial( reported_late ).
    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals(
        msg = 'Reported has not the correct amount of messages'
        exp = 2
        act = lines( reported_late-/DMO/Agency )
      ).

    LOOP AT reported_late-/DMO/Agency INTO DATA(reported_line).
      IF reported_line-%msg IS BOUND.
        reported_with_message     = reported_line.
      ELSE.
        reported_clear_state_area = reported_line.
      ENDIF.

      cl_abap_unit_assert=>assert_equals(
          exp = lhc_agency=>state_area_validate_name
          act = reported_line-%state_area
        ).
      cl_abap_unit_assert=>assert_equals(
          exp = agency-AgencyId
          act = reported_line-AgencyId
        ).
      cl_abap_unit_assert=>assert_equals(
          exp = if_abap_behv=>mk-off
          act = reported_line-%is_draft
        ).

    ENDLOOP.

    cl_abap_unit_assert=>assert_not_initial( reported_with_message     ).
    cl_abap_unit_assert=>assert_not_initial( reported_clear_state_area ).

    "check message
    cl_abap_unit_assert=>assert_equals(
        exp = if_abap_behv=>mk-on
        act = reported_with_message-%element-name
      ).
    cl_abap_unit_assert=>assert_equals(
        exp = CORRESPONDING symsg( /dmo/cx_agency=>name_required )
        act = CORRESPONDING symsg( reported_with_message-%msg->if_t100_message~t100key )
      ).
  ENDMETHOD.

  METHOD validate_email_success.
    DATA:
      agency   TYPE /DMO/R_AgencyTP,
      agencies TYPE STANDARD TABLE OF /DMO/R_AgencyTP.

    agency = VALUE /DMO/R_AgencyTP(
        AgencyId     = '123'
        EmailAddress = 'name@provider.toplevel'
      ).

    agencies = VALUE #( ( agency ) ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validateemailaddress(
        EXPORTING
          keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  agencyID = agency-AgencyId ) )
        CHANGING
          failed   = failed_late
          reported = reported_late
      ).

    cl_abap_unit_assert=>assert_initial(     failed_late   ).
    cl_abap_unit_assert=>assert_not_initial( reported_late ).

    " One line is expected as this clears the state area.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has more than one messages'
                                        exp = 1
                                        act = lines( reported_late-/DMO/Agency ) ).
    DATA(reported_agency) = reported_late-/DMO/Agency[ 1 ].

    "Check that only %tky and %state_area is filled
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off                         act = reported_agency-%is_draft    ).
    cl_abap_unit_assert=>assert_equals( exp = agency-AgencyId                              act = reported_agency-agencyID ).
    cl_abap_unit_assert=>assert_equals( exp = class_under_test->state_area_validate_email  act = reported_agency-%state_area ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%op ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%msg ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%element ).
  ENDMETHOD.

  METHOD validate_email_failed.
    CONSTANTS: cv_found TYPE /DMO/R_AgencyTP-Name VALUE 'found' ##NO_TEXT.
    DATA:
      agency                    TYPE /DMO/R_AgencyTP,
      agencies                  TYPE STANDARD TABLE OF /DMO/R_AgencyTP,
      reported_with_message     TYPE STRUCTURE FOR REPORTED LATE /DMO/R_AgencyTP,
      reported_clear_state_area TYPE STRUCTURE FOR REPORTED LATE /DMO/R_AgencyTP.

    agencies = VALUE #(
        ( AgencyId = '1'  EmailAddress = 'name@provider' )
        ( AgencyId = '2'  EmailAddress = 'name@.toplevel' )
        ( AgencyId = '3'  EmailAddress = 'name@provider.' )
        ( AgencyId = '4'  EmailAddress = '@provider.toplevel' )
        ( AgencyId = '5'  EmailAddress = '' )
        ( AgencyId = '6'  EmailAddress = '@.' )
        ( AgencyId = '7'  EmailAddress = '@' )
        ( AgencyId = '8'  EmailAddress = '.' )
        ( AgencyId = '9'  EmailAddress = '@provider.' )
      ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validateemailaddress(
        EXPORTING
          keys     = VALUE #( FOR a IN agencies
                              %is_draft = if_abap_behv=>mk-off
                              ( agencyID = a-AgencyId )
                            )
        CHANGING
          failed   = failed_late
          reported = reported_late
      ).

    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed_late ).
    cl_abap_unit_assert=>assert_equals(
        exp = lines( agencies )
        act = lines( failed_late-/DMO/Agency )
      ).

    LOOP AT failed-/DMO/Agency INTO DATA(failed_line).
      cl_abap_unit_assert=>assert_not_initial(
          msg = 'Failed key was not provided'
          act = VALUE #( agencies[ AgencyId = failed_line-AgencyId ] OPTIONAL )
        ).
    ENDLOOP.


    "check reported
    cl_abap_unit_assert=>assert_not_initial( reported_late ).
    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals(
        msg = 'Reported has not the correct amount of messages'
        exp = 2 * lines( agencies )
        act = lines( reported_late-/DMO/Agency )
      ).

    LOOP AT agencies ASSIGNING FIELD-SYMBOL(<agency>).
      CLEAR: reported_with_message, reported_clear_state_area.

      LOOP AT reported_late-/DMO/Agency INTO DATA(reported_line) USING KEY entity WHERE AgencyId = <agency>-AgencyId.
        IF reported_line-%msg IS BOUND.
          reported_with_message     = reported_line.
        ELSE.
          reported_clear_state_area = reported_line.
        ENDIF.

        cl_abap_unit_assert=>assert_equals(
            exp = lhc_agency=>state_area_validate_email
            act = reported_line-%state_area
          ).
        cl_abap_unit_assert=>assert_equals(
             exp = <agency>-AgencyId
             act = reported_line-AgencyId
           ).
        cl_abap_unit_assert=>assert_equals(
            exp = if_abap_behv=>mk-off
            act = reported_line-%is_draft
          ).

        <agency>-name = cv_found.
      ENDLOOP.

      cl_abap_unit_assert=>assert_not_initial( reported_with_message     ).
      cl_abap_unit_assert=>assert_not_initial( reported_clear_state_area ).

      "check message
      cl_abap_unit_assert=>assert_equals(
          exp = if_abap_behv=>mk-on
          act = reported_with_message-%element-emailaddress
        ).
      DATA(elements) = reported_with_message-%element.
      elements-emailaddress = if_abap_behv=>mk-off.
      cl_abap_unit_assert=>assert_initial( elements ).

      cl_abap_unit_assert=>assert_equals(
          exp = CORRESPONDING symsg( /dmo/cx_agency=>email_invalid_format )
          act = CORRESPONDING symsg( reported_with_message-%msg->if_t100_message~t100key )
        ).
    ENDLOOP.

    LOOP AT agencies INTO agency WHERE name IS INITIAL.
      cl_abap_unit_assert=>fail(
          msg  = |EMail Address "{ agency-EmailAddress }" was accepted but shouldn't!|
          quit = if_abap_unit_constant=>quit-no
        ).
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_countrycode_success.
    DATA:
      agency    TYPE /DMO/R_AgencyTP,
      agencies  TYPE STANDARD TABLE OF /DMO/R_AgencyTP,
      country   TYPE I_Country,
      countries TYPE STANDARD TABLE OF I_Country.

    country = VALUE I_Country( Country = 'DE' ).
    countries = VALUE #( ( Country ) ).
    sql_test_environment->insert_test_data( countries ).

    agency = VALUE /DMO/R_AgencyTP(
        AgencyId    = '123'
        CountryCode = country-Country
      ).

    agencies = VALUE #( ( agency ) ).
*    cds_test_environment->insert_test_data( agencies ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatecountrycode(
        EXPORTING
          keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  agencyID = agency-AgencyId ) )
        CHANGING
          failed   = failed_late
          reported = reported_late
      ).

    cl_abap_unit_assert=>assert_initial(     failed_late   ).
    cl_abap_unit_assert=>assert_not_initial( reported_late ).

    " One line is expected as this clears the state area.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has more than one messages'
                                        exp = 1
                                        act = lines( reported_late-/DMO/Agency ) ).
    DATA(reported_agency) = reported_late-/DMO/Agency[ 1 ].

    "Check that only %tky and %state_area is filled
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off                           act = reported_agency-%is_draft    ).
    cl_abap_unit_assert=>assert_equals( exp = agency-AgencyId                                act = reported_agency-agencyID ).
    cl_abap_unit_assert=>assert_equals( exp = class_under_test->state_area_validate_country  act = reported_agency-%state_area ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%op ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%msg ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%element ).
  ENDMETHOD.

  METHOD validate_countrycode_failed.
    CONSTANTS: cv_found TYPE /DMO/R_AgencyTP-Name VALUE 'found' ##NO_TEXT.
    DATA:
      agency                    TYPE /DMO/R_AgencyTP,
      agencies                  TYPE STANDARD TABLE OF /DMO/R_AgencyTP,
      country                   TYPE I_Country,
      countries                 TYPE STANDARD TABLE OF I_Country,
      reported_with_message     TYPE STRUCTURE FOR REPORTED LATE /DMO/R_AgencyTP,
      reported_clear_state_area TYPE STRUCTURE FOR REPORTED LATE /DMO/R_AgencyTP.


    country = VALUE I_Country( Country = 'DE' ).
    countries = VALUE #( ( Country ) ).
    sql_test_environment->insert_test_data( countries ).

    agencies = VALUE #(
        ( agencyid = '1'  countrycode = 'XX' )
        ( agencyid = '2'  countrycode = '' )
      ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatecountrycode(
        EXPORTING
          keys     = VALUE #( FOR a IN agencies
                              %is_draft = if_abap_behv=>mk-off
                              ( agencyID = a-agencyid )
                            )
        CHANGING
          failed   = failed_late
          reported = reported_late
      ).

    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed_late ).
    cl_abap_unit_assert=>assert_equals(
        exp = lines( agencies )
        act = lines( failed_late-/DMO/Agency )
      ).

    LOOP AT failed-/DMO/Agency INTO DATA(failed_line).
      cl_abap_unit_assert=>assert_not_initial(
          msg = 'Failed key was not provided'
          act = VALUE #( agencies[ agencyid = failed_line-agencyid ] OPTIONAL )
        ).
    ENDLOOP.


    "check reported
    cl_abap_unit_assert=>assert_not_initial( reported_late ).
    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals(
        msg = 'Reported has not the correct amount of messages'
        exp = 2 * lines( agencies )
        act = lines( reported_late-/DMO/Agency )
      ).

    LOOP AT agencies ASSIGNING FIELD-SYMBOL(<agency>).
      CLEAR: reported_with_message, reported_clear_state_area.

      LOOP AT reported_late-/DMO/Agency INTO DATA(reported_line) USING KEY entity WHERE AgencyId = <agency>-AgencyId.
        IF reported_line-%msg IS BOUND.
          reported_with_message     = reported_line.
        ELSE.
          reported_clear_state_area = reported_line.
        ENDIF.

        cl_abap_unit_assert=>assert_equals(
            exp = lhc_agency=>state_area_validate_country
            act = reported_line-%state_area
          ).
        cl_abap_unit_assert=>assert_equals(
             exp = <agency>-AgencyId
             act = reported_line-AgencyId
           ).
        cl_abap_unit_assert=>assert_equals(
            exp = if_abap_behv=>mk-off
            act = reported_line-%is_draft
          ).

        <agency>-name = cv_found.
      ENDLOOP.

      cl_abap_unit_assert=>assert_not_initial( reported_with_message     ).
      cl_abap_unit_assert=>assert_not_initial( reported_clear_state_area ).

      "check message
      cl_abap_unit_assert=>assert_equals(
          exp = if_abap_behv=>mk-on
          act = reported_with_message-%element-countrycode
        ).
      DATA(elements) = reported_with_message-%element.
      elements-countrycode = if_abap_behv=>mk-off.
      cl_abap_unit_assert=>assert_initial( elements ).

      cl_abap_unit_assert=>assert_equals(
          exp = CORRESPONDING symsg( /dmo/cx_agency=>country_code_invalid )
          act = CORRESPONDING symsg( reported_with_message-%msg->if_t100_message~t100key )
        ).
    ENDLOOP.

    LOOP AT agencies INTO agency WHERE name IS INITIAL.
      cl_abap_unit_assert=>fail(
          msg  = |Country Code "{ agency-CountryCode }" was accepted but shouldn't!|
          quit = if_abap_unit_constant=>quit-no
        ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_global_authorizations.
    DATA:
      requested_authorizations TYPE STRUCTURE FOR GLOBAL AUTHORIZATION REQUEST /DMO/R_AgencyTP\\/DMO/Agency,
      result                   TYPE STRUCTURE FOR GLOBAL AUTHORIZATION RESULT /DMO/R_AgencyTP\\/DMO/Agency,
      reported                 TYPE RESPONSE  FOR REPORTED EARLY /DMO/R_AgencyTP.

    requested_authorizations-%create = if_abap_behv=>mk-on.

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


CLASS ltc_agency_saver DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      class_under_test     TYPE REF TO lsc_agency.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    DATA:
      mapped   TYPE RESPONSE FOR MAPPED   LATE /DMO/R_AgencyTP,
      reported TYPE RESPONSE FOR REPORTED LATE /DMO/R_AgencyTP.

    METHODS:
      setup,
      teardown.


    METHODS:
      "! Checks if { @link ..lhc_agency.METH:earlynumbering_create } fulfills idempotency
      "! by passing an instance where the number is already drawn.
      latenumbering_idempotency    FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_agency.METH:earlynumbering_create }
      "! draws a new number for instances where no number is drawn yet.
      latenumbering_new_number     FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_agency_saver IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
  ENDMETHOD.

  METHOD setup.
    CLEAR:
      mapped,
      reported.
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
  ENDMETHOD.

  METHOD latenumbering_idempotency.
    CONSTANTS: cv_agencyid TYPE /DMO/R_AgencyTP-AgencyId VALUE '123'.

    DATA:
      act_mapped_line LIKE LINE OF mapped-/DMO/Agency,
      exp_mapped_line LIKE LINE OF mapped-/DMO/Agency.

    mapped-/DMO/Agency = VALUE #( ( AgencyId = cv_agencyid ) ).

    class_under_test->adjust_numbers(
        CHANGING
          mapped   = mapped
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     reported ).
    cl_abap_unit_assert=>assert_not_initial( mapped   ).

    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( mapped-/DMO/Agency )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = cv_agencyid
        act = mapped-/DMO/Agency[ 1 ]-AgencyId
      ).
  ENDMETHOD.

  METHOD latenumbering_new_number.
    CONSTANTS: cv_pid TYPE abp_behv_pid VALUE '123'.

    TRY.
        cl_numberrange_intervals=>read(
          EXPORTING
            nr_range_nr1       = '01'
            object            = '/DMO/AGNCY'
          IMPORTING
            interval     = DATA(interval)
        ).
      CATCH cx_nr_object_not_found.
        cl_abap_unit_assert=>skip( 'Number Range Object is missing.  Please run the data generator!' ).
      CATCH cx_number_ranges INTO DATA(number_range_exception).
        cl_abap_unit_assert=>skip( number_range_exception->get_text( ) ).
    ENDTRY.

    mapped-/DMO/Agency = VALUE #( ( %pid = cv_pid ) ).

    class_under_test->adjust_numbers(
        CHANGING
          mapped   = mapped
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     reported ).
    cl_abap_unit_assert=>assert_not_initial( mapped   ).

    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( mapped-/DMO/Agency )
      ).

    cl_abap_unit_assert=>assert_equals(
        exp = cv_pid
        act = mapped-/DMO/Agency[ 1 ]-%pid
      ).

    cl_abap_unit_assert=>assert_not_initial( mapped-/DMO/Agency[ 1 ]-AgencyId ).
  ENDMETHOD.

ENDCLASS.
