"! @testing BDEF:/DMO/R_AgencyTP
CLASS ltc_agency_handler DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA class_under_test     TYPE REF TO lhc_agency.
    CLASS-DATA sql_test_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.

    DATA mapped        TYPE RESPONSE FOR MAPPED EARLY /dmo/r_agencytp.
    DATA failed        TYPE RESPONSE FOR FAILED EARLY /dmo/r_agencytp.
    DATA reported      TYPE RESPONSE FOR REPORTED EARLY /dmo/r_agencytp.
    DATA failed_late   TYPE RESPONSE FOR FAILED LATE /dmo/r_agencytp.
    DATA reported_late TYPE RESPONSE FOR REPORTED LATE /dmo/r_agencytp.

    METHODS setup.
    METHODS teardown.

    "! Checks if { @link ..lhc_agency.METH:validateName } behaves correctly for an instance
    "! where Name is set correctly.
    METHODS validate_name_success        FOR TESTING RAISING cx_static_check.

    "! Checks if { @link ..lhc_agency.METH:validateName } behaves correctly for instances
    "! where Name is not set correctly.  This should result in filled reported and failed tables.
    METHODS validate_name_failed         FOR TESTING RAISING cx_static_check.

    "! Checks if { @link ..lhc_agency.METH:validateemailaddress } behaves correctly for an instance
    "! where email address is set correctly.
    METHODS validate_email_success       FOR TESTING RAISING cx_static_check.

    "! Checks if { @link ..lhc_agency.METH:validateemailaddress } behaves correctly for instances
    "! where email address is not set correctly.  This should result in filled reported and failed tables.
    "! All permutations are tested here.
    METHODS validate_email_failed        FOR TESTING RAISING cx_static_check.

    "! Checks if { @link ..lhc_agency.METH:validateCountryCode } behaves correctly for an instance
    "! where country code is set correctly.
    METHODS validate_countrycode_success FOR TESTING RAISING cx_static_check.

    "! Checks if { @link ..lhc_agency.METH:validateCountryCode } behaves correctly for instances
    "! where country code is not set correctly.  This should result in filled reported and failed tables.
    "! All permutations are tested here.
    METHODS validate_countrycode_failed  FOR TESTING RAISING cx_static_check.

    "! Checks if { @link ..lhc_agency.METH:validatelargeobject } behaves correctly for instances
    "! where attachment, mimetype and filename are set correctly.
    METHODS validate_lob_success         FOR TESTING RAISING cx_static_check.

    "! Checks if { @link ..lhc_agency.METH:validatelargeobject } behaves correctly for instances
    "! where attachment, mimetype and filename are set incorrectly.
    METHODS validate_lob_failed          FOR TESTING RAISING cx_static_check.

    "! Checks that { @link ..lhc_agency.METH:get_global_authorizations } returns initial values
    "! for <em>result</em> and <em>reported</em>.
    METHODS get_global_authorizations    FOR TESTING RAISING cx_static_check.

ENDCLASS.



CLASS ltc_agency_handler IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    sql_test_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( '/DMO/R_AgencyTP' ) ( 'I_COUNTRY' ) ) ).
  ENDMETHOD.


  METHOD setup.
    sql_test_environment->clear_doubles( ).

    CLEAR mapped.
    CLEAR failed.
    CLEAR reported.
    CLEAR failed_late.
    CLEAR reported_late.
  ENDMETHOD.


  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.


  METHOD class_teardown.
    sql_test_environment->destroy( ).
  ENDMETHOD.


  METHOD validate_name_success.
    DATA agency   TYPE /dmo/r_agencytp.
    DATA agencies TYPE STANDARD TABLE OF /dmo/r_agencytp.

    agency = VALUE /dmo/r_agencytp( agencyid = '123'
                                    name     = 'FlightBooker' ).

    agencies = VALUE #( ( agency ) ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatename(
      EXPORTING keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  agencyid = agency-agencyid ) )
      CHANGING  failed   = failed_late
                reported = reported_late ).

    cl_abap_unit_assert=>assert_initial(     failed_late   ).
    cl_abap_unit_assert=>assert_not_initial( reported_late ).

    " One line is expected as this clears the state area.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has more than one messages'
                                        exp = 1
                                        act = lines( reported_late-/dmo/agency ) ).
    DATA(reported_agency) = reported_late-/dmo/agency[ 1 ].

    " Check that only %tky and %state_area is filled
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off                        act = reported_agency-%is_draft    ).
    cl_abap_unit_assert=>assert_equals( exp = agency-agencyid                             act = reported_agency-agencyid ).
    cl_abap_unit_assert=>assert_equals( exp = class_under_test->state_area_validate_name  act = reported_agency-%state_area ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%op ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%msg ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%element ).
  ENDMETHOD.


  METHOD validate_name_failed.
    DATA agency                    TYPE /dmo/r_agencytp.
    DATA agencies                  TYPE STANDARD TABLE OF /dmo/r_agencytp.
    DATA reported_with_message     TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp.
    DATA reported_clear_state_area TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp.

    agency = VALUE /dmo/r_agencytp( agencyid = '123'
                                    name     = '' ).

    agencies = VALUE #( ( agency ) ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatename(
      EXPORTING keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  agencyid = agency-agencyid ) )
      CHANGING  failed   = failed_late
                reported = reported_late ).

    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed_late ).
    cl_abap_unit_assert=>assert_equals( exp = 1  act = lines( failed_late-/dmo/agency ) ).

    LOOP AT failed-/dmo/agency INTO DATA(failed_line).
      cl_abap_unit_assert=>assert_not_initial( msg = 'Failed key was not provided'
                                               act = VALUE #( agencies[ agencyid = failed_line-agencyid ] OPTIONAL ) ).
    ENDLOOP.


    " check reported
    cl_abap_unit_assert=>assert_not_initial( reported_late ).
    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has not the correct amount of messages'
                                        exp = 2
                                        act = lines( reported_late-/dmo/agency ) ).

    LOOP AT reported_late-/dmo/agency INTO DATA(reported_line).
      IF reported_line-%msg IS BOUND.
        reported_with_message     = reported_line.
      ELSE.
        reported_clear_state_area = reported_line.
      ENDIF.

      cl_abap_unit_assert=>assert_equals( exp = lhc_agency=>state_area_validate_name
                                          act = reported_line-%state_area ).
      cl_abap_unit_assert=>assert_equals( exp = agency-agencyid
                                          act = reported_line-agencyid ).
      cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off
                                          act = reported_line-%is_draft ).

    ENDLOOP.

    cl_abap_unit_assert=>assert_not_initial( reported_with_message     ).
    cl_abap_unit_assert=>assert_not_initial( reported_clear_state_area ).

    " check message
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on
                                        act = reported_with_message-%element-name ).
    cl_abap_unit_assert=>assert_equals(
        exp = CORRESPONDING symsg( /dmo/cx_agency=>name_required )
        act = CORRESPONDING symsg( reported_with_message-%msg->if_t100_message~t100key ) ).
  ENDMETHOD.


  METHOD validate_email_success.
    DATA agency   TYPE /dmo/r_agencytp.
    DATA agencies TYPE STANDARD TABLE OF /dmo/r_agencytp.

    agency = VALUE /dmo/r_agencytp( agencyid     = '123'
                                    emailaddress = 'name@provider.toplevel' ).

    agencies = VALUE #( ( agency ) ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validateemailaddress(
      EXPORTING keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  agencyid = agency-agencyid ) )
      CHANGING  failed   = failed_late
                reported = reported_late ).

    cl_abap_unit_assert=>assert_initial(     failed_late   ).
    cl_abap_unit_assert=>assert_not_initial( reported_late ).

    " One line is expected as this clears the state area.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has more than one messages'
                                        exp = 1
                                        act = lines( reported_late-/dmo/agency ) ).
    DATA(reported_agency) = reported_late-/dmo/agency[ 1 ].

    " Check that only %tky and %state_area is filled
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off                         act = reported_agency-%is_draft    ).
    cl_abap_unit_assert=>assert_equals( exp = agency-agencyid                              act = reported_agency-agencyid ).
    cl_abap_unit_assert=>assert_equals( exp = class_under_test->state_area_validate_email  act = reported_agency-%state_area ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%op ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%msg ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%element ).
  ENDMETHOD.


  METHOD validate_email_failed.
    CONSTANTS cv_found TYPE /dmo/r_agencytp-name VALUE 'found' ##NO_TEXT.
    DATA agency                    TYPE /dmo/r_agencytp.
    DATA agencies                  TYPE STANDARD TABLE OF /dmo/r_agencytp.
    DATA reported_with_message     TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp.
    DATA reported_clear_state_area TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp.

    agencies = VALUE #( ( agencyid = '1'  emailaddress = 'name@provider' )
                        ( agencyid = '2'  emailaddress = 'name@.toplevel' )
                        ( agencyid = '3'  emailaddress = 'name@provider.' )
                        ( agencyid = '4'  emailaddress = '@provider.toplevel' )
                        ( agencyid = '5'  emailaddress = '' )
                        ( agencyid = '6'  emailaddress = '@.' )
                        ( agencyid = '7'  emailaddress = '@' )
                        ( agencyid = '8'  emailaddress = '.' )
                        ( agencyid = '9'  emailaddress = '@provider.' ) ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validateemailaddress( EXPORTING keys     = VALUE #( FOR a IN agencies
                                                                          %is_draft = if_abap_behv=>mk-off
                                                                          ( agencyid = a-agencyid ) )
                                            CHANGING  failed   = failed_late
                                                      reported = reported_late ).

    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed_late ).
    cl_abap_unit_assert=>assert_equals( exp = lines( agencies )
                                        act = lines( failed_late-/dmo/agency ) ).

    LOOP AT failed-/dmo/agency INTO DATA(failed_line).
      cl_abap_unit_assert=>assert_not_initial( msg = 'Failed key was not provided'
                                               act = VALUE #( agencies[ agencyid = failed_line-agencyid ] OPTIONAL ) ).
    ENDLOOP.


    " check reported
    cl_abap_unit_assert=>assert_not_initial( reported_late ).
    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has not the correct amount of messages'
                                        exp = 2 * lines( agencies )
                                        act = lines( reported_late-/dmo/agency ) ).

    LOOP AT agencies ASSIGNING FIELD-SYMBOL(<agency>).
      CLEAR reported_with_message.
      CLEAR reported_clear_state_area.

      LOOP AT reported_late-/dmo/agency INTO DATA(reported_line) USING KEY entity WHERE agencyid = <agency>-agencyid.
        IF reported_line-%msg IS BOUND.
          reported_with_message     = reported_line.
        ELSE.
          reported_clear_state_area = reported_line.
        ENDIF.

        cl_abap_unit_assert=>assert_equals( exp = lhc_agency=>state_area_validate_email
                                            act = reported_line-%state_area ).
        cl_abap_unit_assert=>assert_equals( exp = <agency>-agencyid
                                            act = reported_line-agencyid ).
        cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off
                                            act = reported_line-%is_draft ).

        <agency>-name = cv_found.
      ENDLOOP.

      cl_abap_unit_assert=>assert_not_initial( reported_with_message     ).
      cl_abap_unit_assert=>assert_not_initial( reported_clear_state_area ).

      " check message
      cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on
                                          act = reported_with_message-%element-emailaddress ).
      DATA(elements) = reported_with_message-%element.
      elements-emailaddress = if_abap_behv=>mk-off.
      cl_abap_unit_assert=>assert_initial( elements ).

      cl_abap_unit_assert=>assert_equals(
          exp = CORRESPONDING symsg( /dmo/cx_agency=>email_invalid_format )
          act = CORRESPONDING symsg( reported_with_message-%msg->if_t100_message~t100key ) ).
    ENDLOOP.

    LOOP AT agencies INTO agency WHERE name IS INITIAL.
      cl_abap_unit_assert=>fail( msg  = |EMail Address "{ agency-emailaddress }" was accepted but shouldn't!|
                                 quit = if_abap_unit_constant=>quit-no ).
    ENDLOOP.
  ENDMETHOD.


  METHOD validate_countrycode_success.
    DATA agency    TYPE /dmo/r_agencytp.
    DATA agencies  TYPE STANDARD TABLE OF /dmo/r_agencytp.
    DATA country   TYPE i_country.
    DATA countries TYPE STANDARD TABLE OF i_country.

    country = VALUE i_country( country = 'DE' ).
    countries = VALUE #( ( country ) ).
    sql_test_environment->insert_test_data( countries ).

    agency = VALUE /dmo/r_agencytp( agencyid    = '123'
                                    countrycode = country-country ).

    agencies = VALUE #( ( agency ) ).
*    cds_test_environment->insert_test_data( agencies ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatecountrycode(
      EXPORTING keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  agencyid = agency-agencyid ) )
      CHANGING  failed   = failed_late
                reported = reported_late ).

    cl_abap_unit_assert=>assert_initial(     failed_late   ).
    cl_abap_unit_assert=>assert_not_initial( reported_late ).

    " One line is expected as this clears the state area.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has more than one messages'
                                        exp = 1
                                        act = lines( reported_late-/dmo/agency ) ).
    DATA(reported_agency) = reported_late-/dmo/agency[ 1 ].

    " Check that only %tky and %state_area is filled
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off                           act = reported_agency-%is_draft    ).
    cl_abap_unit_assert=>assert_equals( exp = agency-agencyid                                act = reported_agency-agencyid ).
    cl_abap_unit_assert=>assert_equals( exp = class_under_test->state_area_validate_country  act = reported_agency-%state_area ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%op ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%msg ).
    cl_abap_unit_assert=>assert_initial( reported_agency-%element ).
  ENDMETHOD.


  METHOD validate_countrycode_failed.
    CONSTANTS cv_found TYPE /dmo/r_agencytp-name VALUE 'found' ##NO_TEXT.
    DATA agency                    TYPE /dmo/r_agencytp.
    DATA agencies                  TYPE STANDARD TABLE OF /dmo/r_agencytp.
    DATA country                   TYPE i_country.
    DATA countries                 TYPE STANDARD TABLE OF i_country.
    DATA reported_with_message     TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp.
    DATA reported_clear_state_area TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp.


    country = VALUE i_country( country = 'DE' ).
    countries = VALUE #( ( country ) ).
    sql_test_environment->insert_test_data( countries ).

    agencies = VALUE #( ( agencyid = '1'  countrycode = 'XX' )
                        ( agencyid = '2'  countrycode = '' ) ).
    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatecountrycode( EXPORTING keys     = VALUE #( FOR a IN agencies
                                                                         %is_draft = if_abap_behv=>mk-off
                                                                         ( agencyid = a-agencyid ) )
                                           CHANGING  failed   = failed_late
                                                     reported = reported_late ).

    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed_late ).
    cl_abap_unit_assert=>assert_equals( exp = lines( agencies )
                                        act = lines( failed_late-/dmo/agency ) ).

    LOOP AT failed-/dmo/agency INTO DATA(failed_line).
      cl_abap_unit_assert=>assert_not_initial( msg = 'Failed key was not provided'
                                               act = VALUE #( agencies[ agencyid = failed_line-agencyid ] OPTIONAL ) ).
    ENDLOOP.


    " check reported
    cl_abap_unit_assert=>assert_not_initial( reported_late ).
    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has not the correct amount of messages'
                                        exp = 2 * lines( agencies )
                                        act = lines( reported_late-/dmo/agency ) ).

    LOOP AT agencies ASSIGNING FIELD-SYMBOL(<agency>).
      CLEAR reported_with_message.
      CLEAR reported_clear_state_area.

      LOOP AT reported_late-/dmo/agency INTO DATA(reported_line) USING KEY entity WHERE agencyid = <agency>-agencyid.
        IF reported_line-%msg IS BOUND.
          reported_with_message     = reported_line.
        ELSE.
          reported_clear_state_area = reported_line.
        ENDIF.

        cl_abap_unit_assert=>assert_equals( exp = lhc_agency=>state_area_validate_country
                                            act = reported_line-%state_area ).
        cl_abap_unit_assert=>assert_equals( exp = <agency>-agencyid
                                            act = reported_line-agencyid ).
        cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off
                                            act = reported_line-%is_draft ).

        <agency>-name = cv_found.
      ENDLOOP.

      cl_abap_unit_assert=>assert_not_initial( reported_with_message     ).
      cl_abap_unit_assert=>assert_not_initial( reported_clear_state_area ).

      " check message
      cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on
                                          act = reported_with_message-%element-countrycode ).
      DATA(elements) = reported_with_message-%element.
      elements-countrycode = if_abap_behv=>mk-off.
      cl_abap_unit_assert=>assert_initial( elements ).

      cl_abap_unit_assert=>assert_equals(
          exp = CORRESPONDING symsg( /dmo/cx_agency=>country_code_invalid )
          act = CORRESPONDING symsg( reported_with_message-%msg->if_t100_message~t100key ) ).
    ENDLOOP.

    LOOP AT agencies INTO agency WHERE name IS INITIAL.
      cl_abap_unit_assert=>fail( msg  = |Country Code "{ agency-countrycode }" was accepted but shouldn't!|
                                 quit = if_abap_unit_constant=>quit-no ).
    ENDLOOP.
  ENDMETHOD.


  METHOD validate_lob_success.

    DATA agencies TYPE STANDARD TABLE OF /dmo/r_agencytp.

    agencies = VALUE #( ( agencyid   = '1'
                          attachment = '1234567'
                          mimetype   = 'image/jpeg'
                          filename   = 'test.jpg' )
                        ( agencyid   = '2'
                          attachment = VALUE #( )
                          mimetype   = VALUE #( )
                          filename   = VALUE #( ) )
                        ( agencyid   = '3'
                          attachment = '1234567'
                          mimetype   = 'image/jpeg'
                          filename   = VALUE #( ) )  ).

    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatelargeobject( EXPORTING keys     = VALUE #( FOR a IN agencies
                                                                         %is_draft = if_abap_behv=>mk-off
                                                                         ( agencyid  = a-agencyid ) )
                                           CHANGING  failed   = failed_late
                                                     reported = reported_late ).


    cl_abap_unit_assert=>assert_initial( failed_late ).
    cl_abap_unit_assert=>assert_not_initial( reported_late ).

    " Three lines are expected as this clears the state area.
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( reported_late-/dmo/agency ) ).
    DATA(reported_agency) = reported_late-/dmo/agency.

    " Check that only %tky and state_area is filled
    LOOP AT reported_agency ASSIGNING FIELD-SYMBOL(<reported_agency>).
      DATA(agency_number) = sy-tabix.
      cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off                       act = <reported_agency>-%is_draft ).
      cl_abap_unit_assert=>assert_equals( exp = agencies[ agency_number ]-agencyid         act = <reported_agency>-agencyid ).
      cl_abap_unit_assert=>assert_equals( exp = class_under_test->state_area_validate_lob  act = <reported_agency>-%state_area ).
      cl_abap_unit_assert=>assert_initial( <reported_agency>-%op ).
      cl_abap_unit_assert=>assert_initial( <reported_agency>-%msg ).
      cl_abap_unit_assert=>assert_initial( <reported_agency>-%element ).
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_lob_failed.
    CONSTANTS cv_found TYPE /dmo/r_agencytp-name VALUE 'found' ##NO_TEXT.
    DATA agencies                  TYPE STANDARD TABLE OF /dmo/r_agencytp.
    DATA reported_with_message     TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp.
    DATA reported_clear_state_area TYPE STRUCTURE FOR REPORTED LATE /dmo/r_agencytp.

    agencies = VALUE #( ( agencyid   = '1'
                          attachment = '1234567'
                          mimetype   = VALUE #( )
                          filename   = 'test.jpg' )
                        ( agencyid   = '2'
                          attachment = VALUE #( )
                          mimetype   = 'image/jpeg'
                          filename   = 'test.jpg' )
                        ( agencyid   = '3'
                          attachment = VALUE #( )
                          mimetype   = VALUE #( )
                          filename   = 'test.jpg' )
                        ( agencyid   = '4'
                          attachment = '1234567'
                          mimetype   = VALUE #( )
                          filename   = VALUE #( ) )
                        ( agencyid   = '5'
                          attachment = VALUE #( )
                          mimetype   = 'image/jpeg'
                          filename   = VALUE #( ) )
                        ( agencyid   = '6'
                          attachment = '1234567'
                          mimetype   = 'image/jpe'
                          filename   = 'test.jpe' )
                        ( agencyid   = '7'
                          attachment = '1234567'
                          mimetype   = 'image/jpeg'
                          filename   = 'test' )
                        ( agencyid   = '8'
                          attachment = '1234567'
                          mimetype   = 'image/jpe'
                          filename   = 'test.jpg' ) ).

    sql_test_environment->insert_test_data( agencies ).

    class_under_test->validatelargeobject( EXPORTING keys     = VALUE #( FOR a IN agencies
                                                                         %is_draft = if_abap_behv=>mk-off
                                                                         ( agencyid  = a-agencyid ) )
                                           CHANGING  failed   = failed_late
                                                     reported = reported_late ).
    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed_late ).

    LOOP AT failed_late-/dmo/agency INTO DATA(failed_line).
      cl_abap_unit_assert=>assert_not_initial( msg = 'Failed key was not provided'
                                               act = VALUE #( agencies[ agencyid = failed_line-agencyid ] OPTIONAL ) ).
    ENDLOOP.


    " check reported
    cl_abap_unit_assert=>assert_not_initial( reported_late ).

    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals( exp = 2 * lines( agencies )
                                        act = lines( reported_late-/dmo/agency ) ).

    LOOP AT agencies ASSIGNING FIELD-SYMBOL(<agency>).
      CLEAR reported_with_message.
      CLEAR reported_clear_state_area.

      LOOP AT reported_late-/dmo/agency INTO DATA(reported_line) USING KEY entity WHERE agencyid = <agency>-agencyid.
        IF reported_line-%msg IS BOUND.
          reported_with_message     = reported_line.
        ELSE.
          reported_clear_state_area = reported_line.
        ENDIF.

        cl_abap_unit_assert=>assert_equals( exp = lhc_agency=>state_area_validate_lob
                                            act = reported_line-%state_area ).
        cl_abap_unit_assert=>assert_equals( exp = <agency>-agencyid
                                            act = reported_line-agencyid ).
        cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off
                                            act = reported_line-%is_draft ).

        <agency>-name = cv_found.
      ENDLOOP.

      cl_abap_unit_assert=>assert_not_initial( reported_with_message     ).
      cl_abap_unit_assert=>assert_not_initial( reported_clear_state_area ).

    ENDLOOP.


    LOOP AT agencies INTO DATA(agency) WHERE name IS INITIAL.
      cl_abap_unit_assert=>fail( msg  = |Large object fields were accepted but shouldn't!|
                                 quit = if_abap_unit_constant=>quit-no ).
    ENDLOOP.


    " check messages
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on
                                        act = reported_late-/dmo/agency[ 2 ]-%element-mimetype ).
    cl_abap_unit_assert=>assert_equals(
        exp = CORRESPONDING symsg( /dmo/cx_agency=>mimetype_missing )
        act = CORRESPONDING symsg( reported_late-/dmo/agency[ 2 ]-%msg->if_t100_message~t100key ) ).


    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on
                                        act = reported_late-/dmo/agency[ 4 ]-%element-attachment ).
    cl_abap_unit_assert=>assert_equals(
        exp = CORRESPONDING symsg( /dmo/cx_agency=>attachment_empty_missing )
        act = CORRESPONDING symsg( reported_late-/dmo/agency[ 4 ]-%msg->if_t100_message~t100key ) ).


    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on
                                        act = reported_late-/dmo/agency[ 6 ]-%element-attachment ).
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on
                                        act = reported_late-/dmo/agency[ 6 ]-%element-mimetype ).
    cl_abap_unit_assert=>assert_equals(
        exp = CORRESPONDING symsg( /dmo/cx_agency=>only_filename )
        act = CORRESPONDING symsg( reported_late-/dmo/agency[ 6 ]-%msg->if_t100_message~t100key ) ).


    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on
                                        act = reported_late-/dmo/agency[ 8 ]-%element-mimetype ).
    cl_abap_unit_assert=>assert_equals(
        exp = CORRESPONDING symsg( /dmo/cx_agency=>mimetype_missing )
        act = CORRESPONDING symsg( reported_late-/dmo/agency[ 8 ]-%msg->if_t100_message~t100key ) ).


    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on
                                        act = reported_late-/dmo/agency[ 10 ]-%element-attachment ).
    cl_abap_unit_assert=>assert_equals(
        exp = CORRESPONDING symsg( /dmo/cx_agency=>attachment_empty_missing )
        act = CORRESPONDING symsg( reported_late-/dmo/agency[ 10 ]-%msg->if_t100_message~t100key ) ).


    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on
                                        act = reported_late-/dmo/agency[ 12 ]-%element-mimetype ).
    cl_abap_unit_assert=>assert_equals(
        exp = CORRESPONDING symsg( /dmo/cx_agency=>mimetype_not_supported )
        act = CORRESPONDING symsg( reported_late-/dmo/agency[ 12 ]-%msg->if_t100_message~t100key ) ).


    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on
                                        act = reported_late-/dmo/agency[ 14 ]-%element-filename ).
    cl_abap_unit_assert=>assert_equals(
        exp = CORRESPONDING symsg( /dmo/cx_agency=>extension_mimetype_mismatch )
        act = CORRESPONDING symsg( reported_late-/dmo/agency[ 14 ]-%msg->if_t100_message~t100key ) ).


    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on
                                        act = reported_late-/dmo/agency[ 16 ]-%element-mimetype ).
    cl_abap_unit_assert=>assert_equals(
        exp = CORRESPONDING symsg( /dmo/cx_agency=>mimetype_not_supported )
        act = CORRESPONDING symsg( reported_late-/dmo/agency[ 16 ]-%msg->if_t100_message~t100key ) ).

  ENDMETHOD.


  METHOD get_global_authorizations.
    DATA requested_authorizations TYPE STRUCTURE FOR GLOBAL AUTHORIZATION REQUEST /dmo/r_agencytp\\/dmo/agency.
    DATA result                   TYPE STRUCTURE FOR GLOBAL AUTHORIZATION RESULT /dmo/r_agencytp\\/dmo/agency.
    DATA reported                 TYPE RESPONSE FOR REPORTED EARLY /dmo/r_agencytp.

    requested_authorizations-%create = if_abap_behv=>mk-on.

    class_under_test->get_global_authorizations( EXPORTING requested_authorizations = requested_authorizations
                                                 CHANGING  result                   = result
                                                           reported                 = reported ).

    cl_abap_unit_assert=>assert_initial( result   ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

ENDCLASS.



"! @testing BDEF:/DMO/R_AgencyTP
CLASS ltc_agency_saver DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA class_under_test TYPE REF TO lsc_agency.

    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.

    DATA mapped   TYPE RESPONSE FOR MAPPED LATE /dmo/r_agencytp.
    DATA reported TYPE RESPONSE FOR REPORTED LATE /dmo/r_agencytp.

    METHODS setup.
    METHODS teardown.

    "! Checks if { @link ..lhc_agency.METH:earlynumbering_create } fulfills idempotency
    "! by passing an instance where the number is already drawn.
    METHODS latenumbering_idempotency FOR TESTING RAISING cx_static_check.

    "! Checks if { @link ..lhc_agency.METH:earlynumbering_create }
    "! draws a new number for instances where no number is drawn yet.
    METHODS latenumbering_new_number  FOR TESTING RAISING cx_static_check.
ENDCLASS.



CLASS ltc_agency_saver IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
  ENDMETHOD.


  METHOD setup.
    CLEAR mapped.
    CLEAR reported.
  ENDMETHOD.


  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.


  METHOD class_teardown.
  ENDMETHOD.


  METHOD latenumbering_idempotency.
    CONSTANTS cv_agencyid TYPE /dmo/r_agencytp-agencyid VALUE '123'.

    DATA act_mapped_line LIKE LINE OF mapped-/dmo/agency.
    DATA exp_mapped_line LIKE LINE OF mapped-/dmo/agency.

    mapped-/dmo/agency = VALUE #( ( agencyid = cv_agencyid ) ).

    class_under_test->adjust_numbers( CHANGING mapped   = mapped
                                               reported = reported ).

    cl_abap_unit_assert=>assert_initial(     reported ).
    cl_abap_unit_assert=>assert_not_initial( mapped   ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( mapped-/dmo/agency ) ).

    cl_abap_unit_assert=>assert_equals( exp = cv_agencyid
                                        act = mapped-/dmo/agency[ 1 ]-agencyid ).
  ENDMETHOD.


  METHOD latenumbering_new_number.
    CONSTANTS cv_pid TYPE abp_behv_pid VALUE '123'.

    TRY.
        cl_numberrange_intervals=>read( EXPORTING nr_range_nr1 = '01'
                                                  object       = '/DMO/AGNCY'
                                        IMPORTING interval     = DATA(interval) ).
      CATCH cx_nr_object_not_found.
        cl_abap_unit_assert=>skip( 'Number Range Object is missing.  Please run the data generator!' ).
      CATCH cx_number_ranges INTO DATA(number_range_exception).
        cl_abap_unit_assert=>skip( number_range_exception->get_text( ) ).
    ENDTRY.

    mapped-/dmo/agency = VALUE #( ( %pid = cv_pid ) ).

    class_under_test->adjust_numbers( CHANGING mapped   = mapped
                                               reported = reported ).

    cl_abap_unit_assert=>assert_initial(     reported ).
    cl_abap_unit_assert=>assert_not_initial( mapped   ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( mapped-/dmo/agency ) ).

    cl_abap_unit_assert=>assert_equals( exp = cv_pid
                                        act = mapped-/dmo/agency[ 1 ]-%pid ).

    cl_abap_unit_assert=>assert_not_initial( mapped-/dmo/agency[ 1 ]-agencyid ).
  ENDMETHOD.

ENDCLASS.
