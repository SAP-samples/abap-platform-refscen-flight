"! @testing BDEF:/DMO/ZZ_X_REVIEW_R_AGENCYTP
CLASS ltcl_review DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA class_under_test     TYPE REF TO lhc_/dmo/zz_review.
    CLASS-DATA cds_test_environment TYPE REF TO if_cds_test_environment.

    "! Instantiate class under test and set up test double framework
    CLASS-METHODS class_setup.

    "! Destroy test environment and test double
    CLASS-METHODS class_teardown.

    "! Reset test double
    METHODS setup.

    "! Reset transactional buffer
    METHODS teardown.

    "! Calls { @link ..lhc_/DMO/ZZ_Review.METH:get_instance_features }
    "! with all possible in- and outcomes.
    METHODS get_instance_features     FOR TESTING.

    "! Checks that { @link ..lhc_/DMO/ZZ_Review.METH:get_global_authorizations } returns initial values
    "! for <em>result</em> and <em>reported</em>.
    METHODS get_global_authorizations FOR TESTING RAISING cx_static_check.

    "! Checks if { @link ..lhc_agency.METH:ratingInRange } behaves correctly
    "! for all valid combinations.
    METHODS ratinginrange_valid       FOR TESTING RAISING cx_static_check.

    "! Checks if { @link ..lhc_agency.METH:ratingInRange } behaves correctly
    "! for all invalid combinations.
    METHODS ratinginrange_invalid     FOR TESTING RAISING cx_static_check.

    "! Checks if { @link ..lhc_agency.METH:reviewWasHelpful } adds 1 correctly.
    METHODS reviewwashelpful          FOR TESTING RAISING cx_static_check.

    "! Checks if { @link ..lhc_agency.METH:reviewWasNotHelpful } adds 1 correctly.
    METHODS reviewwasnothelpful       FOR TESTING RAISING cx_static_check.

    METHODS _reviewhelpful IMPORTING iv_is_helpful TYPE abap_bool.
ENDCLASS.



CLASS ltcl_review IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
                               VALUE #( ( i_for_entity = '/DMO/I_AGENCYTP'            )
                                        ( i_for_entity = '/DMO/ZZ_I_AGENCY_REVIEWTP'  ) ) ).
  ENDMETHOD.


  METHOD class_teardown.
    cds_test_environment->destroy( ).
  ENDMETHOD.


  METHOD setup.
    cds_test_environment->clear_doubles( ).
  ENDMETHOD.


  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.


  METHOD get_instance_features.
    TYPES: t_instance_feature TYPE STRUCTURE FOR INSTANCE FEATURES RESULT /dmo/r_agencytp\\/dmo/zz_review,
           BEGIN OF t_check.
    INCLUDE TYPE t_instance_feature.
    TYPES: reviewer TYPE /dmo/zz_r_agency_reviewtp-reviewer,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY agencyid reviewid.

    DATA check_table        TYPE t_check_table.
    DATA review_mock_data   TYPE STANDARD TABLE OF /dmo/zz_r_agency_reviewtp.
    DATA reviews_to_test    TYPE TABLE FOR INSTANCE FEATURES KEY /dmo/i_agencytp\\/dmo/zz_review.
    DATA requested_features TYPE STRUCTURE FOR INSTANCE FEATURES REQUEST /dmo/i_agencytp\\/dmo/zz_review.
    DATA act_result         TYPE TABLE FOR INSTANCE FEATURES RESULT /dmo/i_agencytp\\/dmo/zz_review.
    DATA exp_result         TYPE TABLE FOR INSTANCE FEATURES RESULT /dmo/i_agencytp\\/dmo/zz_review.
    DATA reported           TYPE RESPONSE FOR REPORTED EARLY /dmo/i_agencytp.
    DATA failed             TYPE RESPONSE FOR FAILED EARLY /dmo/i_agencytp.

    " In current implementation requested_features is not used.
    requested_features = VALUE #( ).

    check_table = VALUE t_check_table( %is_draft = if_abap_behv=>mk-off
                                       agencyid  = '123'
                                       ( reviewid = '1'
                                         reviewer = cl_abap_context_info=>get_user_technical_name( )
                                         %update  = if_abap_behv=>fc-o-enabled
                                         %delete  = if_abap_behv=>fc-o-enabled )
                                       ( reviewid = '2'
                                         reviewer = 'TEST'
                                         %update  = if_abap_behv=>fc-o-disabled
                                         %delete  = if_abap_behv=>fc-o-disabled ) ).

    review_mock_data = CORRESPONDING #(
                          check_table
                          MAPPING
                            agencyid = agencyid
                            reviewid = reviewid
                            reviewer = reviewer
                          EXCEPT * ).
    cds_test_environment->insert_test_data( review_mock_data ).

    reviews_to_test = CORRESPONDING #( check_table ).

    exp_result = CORRESPONDING #( check_table ).

    class_under_test->get_instance_features( EXPORTING keys               = reviews_to_test
                                                       requested_features = requested_features
                                             CHANGING  result             = act_result
                                                       failed             = failed
                                                       reported           = reported ).

    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( failed   ).

    SORT act_result BY reviewid ASCENDING.
    SORT exp_result BY reviewid ASCENDING.

    cl_abap_unit_assert=>assert_equals( exp = exp_result
                                        act = act_result ).
  ENDMETHOD.


  METHOD get_global_authorizations.
    DATA requested_authorizations TYPE STRUCTURE FOR GLOBAL AUTHORIZATION REQUEST /dmo/i_agencytp\\/dmo/zz_review.
    DATA result                   TYPE STRUCTURE FOR GLOBAL AUTHORIZATION RESULT /dmo/i_agencytp\\/dmo/zz_review.
    DATA reported                 TYPE RESPONSE FOR REPORTED EARLY /dmo/i_agencytp.

    requested_authorizations-%action-/dmo/reviewwashelpful    = if_abap_behv=>mk-on.
    requested_authorizations-%action-/dmo/reviewwasnothelpful = if_abap_behv=>mk-on.

    class_under_test->get_global_authorizations( EXPORTING requested_authorizations = requested_authorizations
                                                 CHANGING  result                   = result
                                                           reported                 = reported ).

    cl_abap_unit_assert=>assert_initial( result   ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.


  METHOD ratinginrange_valid.
    DATA review_mock_data TYPE STANDARD TABLE OF /dmo/zz_r_agency_reviewtp.

    review_mock_data = VALUE #( agencyid = '1'
                                ( reviewid = '1'  rating = '1' )
                                ( reviewid = '2'  rating = '2' )
                                ( reviewid = '3'  rating = '3' )
                                ( reviewid = '4'  rating = '4' )
                                ( reviewid = '5'  rating = '5' ) ).
    cds_test_environment->insert_test_data( review_mock_data ).

    DATA reported TYPE RESPONSE FOR REPORTED LATE /dmo/i_agencytp.

    class_under_test->ratinginrange(
      EXPORTING keys     = CORRESPONDING #( review_mock_data MAPPING agencyid = agencyid  reviewid = reviewid EXCEPT * )
      CHANGING  reported = reported ).

    cl_abap_unit_assert=>assert_not_initial( msg = 'reported' act = reported ).
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has not the correct amount of messages'
                                        exp = lines( review_mock_data )
                                        act = lines( reported-/dmo/zz_review ) ).
  ENDMETHOD.


  METHOD ratinginrange_invalid.
    TYPES BEGIN OF ts_review_test_data.
            INCLUDE TYPE /dmo/zz_r_agency_reviewtp.
    TYPES   t100 LIKE if_t100_message=>t100key.
    TYPES END OF ts_review_test_data.
    TYPES tt_review_test_data TYPE STANDARD TABLE OF ts_review_test_data.

    DATA review_test_data          TYPE tt_review_test_data.
    DATA review_mock_data          TYPE STANDARD TABLE OF /dmo/zz_r_agency_reviewtp.
    DATA reported                  TYPE RESPONSE FOR REPORTED LATE /dmo/i_agencytp.
    DATA reported_with_message     TYPE STRUCTURE FOR REPORTED LATE /dmo/i_agencytp\\/dmo/zz_review.
    DATA reported_clear_state_area TYPE STRUCTURE FOR REPORTED LATE /dmo/i_agencytp\\/dmo/zz_review.

    review_test_data = VALUE #(
        agencyid = '1'
        ( reviewid = '1'  rating = '0'  t100 = CORRESPONDING #( /dmo/zz_cx_agency_review=>rating_invalid ) )
        ( reviewid = '2'  rating = '7'  t100 = CORRESPONDING #( /dmo/zz_cx_agency_review=>rating_invalid ) ) ).
    review_mock_data = CORRESPONDING #( review_test_data ).
    cds_test_environment->insert_test_data( review_mock_data ).


    class_under_test->ratinginrange(
      EXPORTING keys     = CORRESPONDING #( review_test_data MAPPING agencyid = agencyid  reviewid = reviewid EXCEPT * )
      CHANGING  reported = reported ).

    cl_abap_unit_assert=>assert_not_initial( act = reported ).
    cl_abap_unit_assert=>assert_equals( msg  = 'Reported has not the correct amount of messages'
                                        exp  = 2 * lines( review_test_data )
                                        act  = lines( reported-/dmo/zz_review )
                                        quit = if_abap_unit_constant=>quit-no ).

    LOOP AT review_test_data ASSIGNING FIELD-SYMBOL(<reviewy>).
      CLEAR reported_with_message.
      CLEAR reported_clear_state_area.

      LOOP AT reported-/dmo/zz_review INTO DATA(reported_line) USING KEY entity
           WHERE agencyid = <reviewy>-agencyid AND reviewid = <reviewy>-reviewid.
        IF reported_line-%msg IS BOUND.
          reported_with_message     = reported_line.
        ELSE.
          reported_clear_state_area = reported_line.
        ENDIF.

        cl_abap_unit_assert=>assert_equals( exp = lhc_/dmo/zz_review=>rating_in_range
                                            act = reported_line-%state_area ).
        cl_abap_unit_assert=>assert_equals( exp = <reviewy>-agencyid
                                            act = reported_line-agencyid ).
        cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off
                                            act = reported_line-%is_draft ).
      ENDLOOP.

      cl_abap_unit_assert=>assert_not_initial(
          act = reported_with_message
          msg = |Message not found for Review { <reviewy>-agencyid }-{ <reviewy>-reviewid }!| ).
      cl_abap_unit_assert=>assert_not_initial(
          act = reported_clear_state_area
          msg = |Invalidate State not found for Review { <reviewy>-agencyid }-{ <reviewy>-reviewid }!| ).

      " check message
      cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-on
                                          act = reported_with_message-%element-rating ).
      DATA(elements) = reported_with_message-%element.
      elements-rating = if_abap_behv=>mk-off.
      cl_abap_unit_assert=>assert_initial( elements ).

      cl_abap_unit_assert=>assert_equals( exp = <reviewy>-t100
                                          act = reported_with_message-%msg->if_t100_message~t100key ).
    ENDLOOP.
  ENDMETHOD.


  METHOD reviewwashelpful.
    _reviewhelpful( abap_true ).
  ENDMETHOD.


  METHOD reviewwasnothelpful.
    _reviewhelpful( abap_false ).
  ENDMETHOD.


  METHOD _reviewhelpful.
    DATA review_mock_data TYPE STANDARD TABLE OF /dmo/zz_r_agency_reviewtp.
    DATA act_reviews      TYPE TABLE FOR READ RESULT /dmo/i_agencytp\\/dmo/zz_review.
    DATA exp_reviews      TYPE TABLE FOR READ RESULT /dmo/i_agencytp\\/dmo/zz_review.

    review_mock_data = VALUE #(
        agencyid = '1'
        ( reviewid = '1'  helpfulcount =  '10'  helpfultotal = '100'  reviewer = cl_abap_context_info=>get_user_technical_name( ) )
        ( reviewid = '2'  helpfulcount = '123'  helpfultotal = '230'  reviewer = cl_abap_context_info=>get_user_technical_name( ) ) ).
    cds_test_environment->insert_test_data( review_mock_data ).

    exp_reviews = CORRESPONDING #( review_mock_data ).

    DATA reported TYPE RESPONSE FOR REPORTED EARLY /dmo/i_agencytp.

    IF iv_is_helpful = abap_true.
      class_under_test->reviewwashelpful(
        EXPORTING
          keys     = CORRESPONDING #( review_mock_data MAPPING agencyid = agencyid  reviewid = reviewid EXCEPT * )
        CHANGING
          reported = reported ).
    ELSE.
      class_under_test->reviewwasnothelpful(
        EXPORTING
          keys     = CORRESPONDING #( review_mock_data MAPPING agencyid = agencyid  reviewid = reviewid EXCEPT * )
        CHANGING
          reported = reported ).
    ENDIF.

    cl_abap_unit_assert=>assert_initial( reported ).

    LOOP AT exp_reviews ASSIGNING FIELD-SYMBOL(<exp>).
      <exp>-helpfultotal += 1.
      IF iv_is_helpful = abap_true.
        <exp>-helpfulcount += 1.
      ENDIF.
    ENDLOOP.

    READ ENTITIES OF /dmo/i_agencytp
         ENTITY /dmo/zz_review
         FIELDS ( helpfulcount helpfultotal reviewer )
         WITH CORRESPONDING #( review_mock_data )
         RESULT act_reviews.

    cl_abap_unit_assert=>assert_equals( exp = exp_reviews
                                        act = act_reviews ).
  ENDMETHOD.

ENDCLASS.



"! @testing BDEF:/DMO/ZZ_X_REVIEW_R_AGENCYTP
CLASS ltcl_sc_r_agency DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS agency_event  TYPE c length 30 VALUE '/DMO/AGENCYREVIEWCREATED'.
    CONSTANTS agency_entity TYPE abp_entity_name VALUE '/DMO/I_AGENCYTP'.
    CLASS-DATA class_under_test       TYPE REF TO lsc_r_agency.
    CLASS-DATA sql_test_environment   TYPE REF TO if_osql_test_environment.
    CLASS-DATA cds_test_environment   TYPE REF TO if_cds_test_environment.
    CLASS-DATA event_test_environment TYPE REF TO if_rap_event_test_environment.

    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.

    METHODS setup.
    METHODS teardown.

    "! Checks that { @link ..lsc_R_AGENCY.METH:cleanup_finalize } does nothing.
    METHODS cleanup_finalize              FOR TESTING RAISING cx_static_check.

    "! Checks that { @link ..lsc_R_AGENCY.METH:adjust_numbers } calculates
    "! new numbers for <em>ReviewID</em> with all possible permutations.
    METHODS adjust_numbers                FOR TESTING RAISING cx_static_check.

    "! Checks that { @link ..lsc_R_AGENCY.METH:save_modified } raises no
    "! RAP Business Events if no review is created.
    METHODS save_md_no_review_created     FOR TESTING.

    "! Checks that { @link ..lsc_R_AGENCY.METH:save_modified } raises one
    "! RAP Business Event with the expected payload if one review is created.
    METHODS save_md_one_review_created    FOR TESTING.

    "! Checks that { @link ..lsc_R_AGENCY.METH:save_modified } raises one
    "! RAP Business Event with the expected payload if multiple reviews are created.
    METHODS save_md_multi_reviews_created FOR TESTING.

ENDCLASS.



CLASS ltcl_sc_r_agency IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    sql_test_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( '/DMO/ZZ_AGN_REVA' ) ) ).
    cds_test_environment = cl_cds_test_environment=>create( i_for_entity               = agency_entity
                                                            i_select_base_dependencies = abap_true ).
    event_test_environment = cl_rap_event_test_environment=>create( VALUE #( ( entity_name = agency_entity event_name = agency_event ) ) ).
  ENDMETHOD.


  METHOD setup.
    sql_test_environment->clear_doubles( ).
    cds_test_environment->clear_doubles( ).
    event_test_environment->clear( ).
  ENDMETHOD.


  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.


  METHOD class_teardown.
    sql_test_environment->destroy( ).
    cds_test_environment->destroy( ).
    event_test_environment->destroy( ).
  ENDMETHOD.


  METHOD cleanup_finalize.
    TRY.
        class_under_test->cleanup_finalize( ).
      CATCH cx_root INTO DATA(lx).
        cl_abap_unit_assert=>fail( msg = lx->get_longtext( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD adjust_numbers.
    DATA act_mapped       TYPE RESPONSE FOR MAPPED LATE /dmo/i_agencytp.
    DATA exp_mapped       TYPE RESPONSE FOR MAPPED LATE /dmo/i_agencytp.
    DATA review_mock_data TYPE STANDARD TABLE OF /dmo/zz_agn_reva.
    DATA reported         TYPE RESPONSE FOR REPORTED LATE /dmo/i_agencytp.

    review_mock_data = VALUE #( ( agency_id = '1'  review_id = '25' )
                                ( agency_id = '2'  review_id = '10' ) ).
    sql_test_environment->insert_test_data( review_mock_data ).

    exp_mapped-/dmo/zz_review = VALUE #(
        " Existing DB Entry, one new instance
        ( %pid = '1'  %tmp-agencyid = '1'  %key = VALUE #( agencyid = '1'  reviewid = '26' ) )
        " Existing DB Entry, two new instances
        ( %pid = '2'  %tmp-agencyid = '2'  %key = VALUE #( agencyid = '2'  reviewid = '11' ) )
        ( %pid = '3'  %tmp-agencyid = '2'  %key = VALUE #( agencyid = '2'  reviewid = '12' ) )
        " No existing DB Entry, one new instance
        ( %pid = '4'  %tmp-agencyid = '4'  %key = VALUE #( agencyid = '4'  reviewid =  '1' ) )
        " No existing DB Entry, two new instances
        ( %pid = '5'  %tmp-agencyid = '5'  %key = VALUE #( agencyid = '5'  reviewid =  '1' ) )
        ( %pid = '6'  %tmp-agencyid = '5'  %key = VALUE #( agencyid = '5'  reviewid =  '2' ) )
        " No existing DB Entry, overwriting temporary ReviewID
        ( %pid = '7'  %tmp-agencyid = '7'  %key = VALUE #( agencyid = '7'  reviewid =  '1' )  %tmp-reviewid = '123' ) ).

    act_mapped-/dmo/zz_review = CORRESPONDING #( exp_mapped-/dmo/zz_review MAPPING %pid = %pid  %tmp = %tmp EXCEPT * ).
    class_under_test->adjust_numbers( CHANGING mapped   = act_mapped
                                               reported = reported ).

    cl_abap_unit_assert=>assert_initial( act = reported ).

    cl_abap_unit_assert=>assert_equals( exp = exp_mapped
                                        act = act_mapped ).
  ENDMETHOD.

  METHOD save_md_no_review_created.

    " Prepare
    DATA reported TYPE RESPONSE FOR REPORTED LATE /dmo/i_agencytp.

    " Execute
    class_under_test->save_modified( EXPORTING create   = VALUE #( )
                                               update   = VALUE #( )
                                               delete   = VALUE #( )
                                     CHANGING  reported = reported ).

    " Verify
    event_test_environment->get_event( entity_name = agency_entity event_name = agency_event )->verify( )->is_raised_times( 0 ).

  ENDMETHOD.


  METHOD save_md_one_review_created.

    " Prepare
    DATA agency_mock_data TYPE STANDARD TABLE OF /dmo/agency.

    agency_mock_data = VALUE #( ( agency_id = '1' email_address = 'test@test.example' ) ).
    cds_test_environment->insert_test_data( agency_mock_data ).

    DATA create TYPE REQUEST FOR CHANGE /dmo/i_agencytp.
    create-/dmo/zz_review = VALUE #( ( agencyid        = '1'
                                       reviewid        = '1'
                                       rating          = '1'
                                       freetextcomment = 'Bad' ) ).
    DATA reported TYPE RESPONSE FOR REPORTED LATE /dmo/i_agencytp.


    " Execute
    class_under_test->save_modified( EXPORTING create   = create
                                               update   = VALUE #( )
                                               delete   = VALUE #( )
                                     CHANGING  reported = reported ).

    " Verify
    event_test_environment->get_event( entity_name = agency_entity event_name = agency_event )->verify( )->is_raised_times( 1 ).

    DATA event_payload_act TYPE TABLE FOR EVENT /dmo/i_agencytp~/dmo/agencyreviewcreated.
    event_payload_act = event_test_environment->get_event( entity_name  = agency_entity event_name = agency_event )->get_payload( )->*.

    DATA event_payload_exp TYPE TABLE FOR EVENT /dmo/i_agencytp~/dmo/agencyreviewcreated.
    event_payload_exp = VALUE #( ( agencyid        = '1'
                                   reviewid        = '1'
                                   rating          = '1'
                                   freetextcomment = 'Bad'
                                   emailaddress    = 'test@test.example' ) ).

    cl_abap_unit_assert=>assert_equals( act = event_payload_act exp = event_payload_exp ).

  ENDMETHOD.

  METHOD save_md_multi_reviews_created.

    " Prepare
    DATA agency_mock_data TYPE STANDARD TABLE OF /dmo/agency.

    agency_mock_data = VALUE #( ( agency_id = '1' email_address = 'test@test.example' )
                                ( agency_id = '2' email_address = 'test2@test.example' ) ).
    cds_test_environment->insert_test_data( agency_mock_data ).

    DATA create TYPE REQUEST FOR CHANGE /dmo/i_agencytp.
    create-/dmo/zz_review = VALUE #( ( agencyid        = '1'
                                       reviewid        = '1'
                                       rating          = '1'
                                       freetextcomment = 'Bad' )
                                     ( agencyid        = '1'
                                       reviewid        = '2'
                                       rating          = '4'
                                       freetextcomment = 'Good' )
                                     ( agencyid        = '2'
                                       reviewid        = '1'
                                       rating          = '2'
                                       freetextcomment = 'Medium' ) ).
    DATA reported TYPE RESPONSE FOR REPORTED LATE /dmo/i_agencytp.


    " Execute
    class_under_test->save_modified( EXPORTING create   = create
                                               update   = VALUE #( )
                                               delete   = VALUE #( )
                                     CHANGING  reported = reported ).

    " Verify
    event_test_environment->get_event( entity_name = agency_entity event_name = agency_event )->verify( )->is_raised_times( 1 ).

    DATA event_payload_act TYPE TABLE FOR EVENT /dmo/i_agencytp~/dmo/agencyreviewcreated.
    event_payload_act = event_test_environment->get_event( entity_name  = agency_entity event_name = agency_event )->get_payload( )->*.

    DATA event_payload_exp TYPE TABLE FOR EVENT /dmo/i_agencytp~/dmo/agencyreviewcreated.
    event_payload_exp = VALUE #( ( agencyid        = '1'
                                   reviewid        = '1'
                                   rating          = '1'
                                   freetextcomment = 'Bad'
                                   emailaddress    = 'test@test.example' )
                                 ( agencyid        = '1'
                                   reviewid        = '2'
                                   rating          = '4'
                                   freetextcomment = 'Good'
                                   emailaddress    = 'test@test.example' )
                                 ( agencyid        = '2'
                                   reviewid        = '1'
                                   rating          = '2'
                                   freetextcomment = 'Medium'
                                   emailaddress    = 'test2@test.example' ) ).

    cl_abap_unit_assert=>assert_equals( act = event_payload_act exp = event_payload_exp ).

  ENDMETHOD.

ENDCLASS.
