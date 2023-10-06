"! @testing BDEF:/DMO/I_TRAVEL_D
CLASS /dmo/zz_tc_agency_slogan DEFINITION
  PUBLIC
  FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      cds_test_environment TYPE REF TO if_cds_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown.

    METHODS:
      "! Checks if an update on /DMO/ZZ_SLOGAN works
      update_on_slogan       FOR TESTING RAISING cx_static_check.
ENDCLASS.



CLASS /dmo/zz_tc_agency_slogan IMPLEMENTATION.

  METHOD class_setup.
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
                               VALUE #(
                                   i_select_base_dependencies = abap_true
                                   ( i_for_entity = '/DMO/I_AGENCYTP'            )
                                   ( i_for_entity = '/DMO/ZZ_I_AGENCY_REVIEWTP'  )
                                 )
                               ).

  ENDMETHOD.


  METHOD class_teardown.
    cds_test_environment->destroy( ).
  ENDMETHOD.


  METHOD setup.
    cds_test_environment->clear_doubles( ).

    DATA: agencies TYPE STANDARD TABLE OF /dmo/agency.
    agencies = VALUE #( ##NO_TEXT
        (
          agency_id         = '42'
          /dmo/zzsloganzag = 'This company is good'
        )
      ).

    cds_test_environment->insert_test_data( agencies ).
  ENDMETHOD.


  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD update_on_slogan.
    CONSTANTS:
      test_slogan TYPE /dmo/zz_slogan VALUE 'Test Slogan' ##NO_TEXT.

    SELECT SINGLE
      FROM /dmo/i_agencytp
      FIELDS agencyid, /dmo/zzsloganzag
      INTO @DATA(agency)
      .

    cl_abap_unit_assert=>assert_subrc( ).

    MODIFY ENTITIES OF /dmo/i_agencytp
      ENTITY /dmo/agency
        UPDATE FIELDS ( /dmo/zzsloganzag )
          WITH VALUE #(
              (
                %tky = VALUE #(
                           %is_draft = if_abap_behv=>mk-off
                           agencyid  = agency-agencyid
                         )
                /dmo/zzsloganzag = test_slogan
              )
            )
          REPORTED DATA(reported_update)
          FAILED DATA(failed_update)
          .

    cl_abap_unit_assert=>assert_initial( reported_update ).
    cl_abap_unit_assert=>assert_initial( failed_update   ).

    COMMIT ENTITIES RESPONSE OF /dmo/i_agencytp
      REPORTED DATA(reported_commit)
      FAILED DATA(failed_commit).

    cl_abap_unit_assert=>assert_initial( reported_commit ).
    cl_abap_unit_assert=>assert_initial( failed_commit   ).

    cl_abap_unit_assert=>assert_subrc( ).


    SELECT SINGLE
      FROM /dmo/i_agencytp
      FIELDS agencyid, /dmo/zzsloganzag
      WHERE agencyid = @agency-agencyid
      INTO @DATA(agency_after)
      .
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      exp = test_slogan
      act = agency_after-/dmo/zzsloganzag
    ).

  ENDMETHOD.

ENDCLASS.
