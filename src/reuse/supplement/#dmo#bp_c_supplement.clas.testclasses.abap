""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Tests using buffering version of test double framework
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS ltd_fields_handler DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_botd_bufdbl_fields_handler.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      max_supplement_id TYPE /dmo/supplement_id VALUE 0.

ENDCLASS.

CLASS ltd_fields_handler IMPLEMENTATION.

  METHOD if_botd_bufdbl_fields_handler~set_readonly_fields.
    CASE entity_name.
      WHEN '/DMO/I_SUPPLEMENT'.
        CASE operation.
          WHEN if_abap_behv=>op-m-create.
            TYPES: ty_create_instances TYPE TABLE FOR CREATE /dmo/i_supplement.
            FIELD-SYMBOLS: <create_instances> TYPE ty_create_instances.
            ASSIGN instances TO <create_instances>.
            LOOP AT <create_instances> ASSIGNING FIELD-SYMBOL(<instance>).
              max_supplement_id += 1.
              <instance>-supplementid = max_supplement_id.
            ENDLOOP.
        ENDCASE.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.




"! Using RAP BO TDF: Transactional buffer double variant
"! @testing BDEF:/DMO/C_SUPPLEMENT
CLASS ltcl_tdf_buffer DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS:
      cv_i_bdef_name TYPE abp_root_entity_name VALUE '/DMO/I_Supplement' ##NO_TEXT.

    CLASS-DATA:
      environment TYPE REF TO if_botd_txbufdbl_bo_test_env.

    DATA:
      double_supplement TYPE REF TO if_botd_txbufdbl_test_double.

    DATA:
      mapped     TYPE RESPONSE FOR MAPPED   EARLY /dmo/c_supplement,
      failed     TYPE RESPONSE FOR FAILED   EARLY /dmo/c_supplement,
      reported   TYPE RESPONSE FOR REPORTED EARLY /dmo/c_supplement,
      i_mapped   TYPE RESPONSE FOR MAPPED   EARLY /dmo/i_supplement,
      i_failed   TYPE RESPONSE FOR FAILED   EARLY /dmo/i_supplement,
      i_reported TYPE RESPONSE FOR REPORTED EARLY /dmo/i_supplement.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown.



    METHODS:
      "! Checks if { @link ..lhc_Supplement.METH:augment } augments correctly to <em>create</em> a <em>Supplement</em>
      "! and <em>create by association</em> of a <em>SupplementText</em>
      "! when an EML create is triggered.  This leads to a set of { @link ..lhc_Supplement.METH:augment.DATA:entities_create }.
      supplement_create             FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } augments correctly
      "! when a EML <em>create</em> and a EML <p>update</p> on the same instance is triggered.
      "! This should lead to a set of { @link ..lhc_Supplement.METH:augment.DATA:entities_create } and
      "! a correlated set of { @link ..lhc_Supplement.METH:augment.DATA:entities_update }.
      "! The base BO should receive a <em>create</em> and <em>update</em> for <em>Supplement</em>
      "! and a <em>create by association</em> and <em>update</em> for the <em>SupplementText</em>.
      supplement_create_text_update FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } augments correctly
      "! when a EML <em>update</em> is triggered.
      "! This should lead to a set of { @link ..lhc_Supplement.METH:augment.DATA:entities_update }.
      "! The base BO should receive an <em>update</em> for <em>Supplement</em>
      "! and a <em>create by association</em> for the <em>SupplementText</em>.
      supplement_update_text_create FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } augments correctly
      "! when a EML <em>update</em> is triggered on an instance where the text exists in user language.
      "! This should lead to a set of { @link ..lhc_Supplement.METH:augment.DATA:entities_update }.
      "! The base BO should receive an <em>update</em> for <em>Supplement</em>
      "! and a <em>update</em> for the <em>SupplementText</em>.
      supplement_update_text_update FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } augments correctly
      "! when a EML <em>update</em> is triggered with an flags only for <em>description</em> on an instance
      "! where the text does not exists in user language.
      "! This should lead to a set of { @link ..lhc_Supplement.METH:augment.DATA:entities_update }.
      "! The base BO should receive an <em>update</em> for <em>Supplement</em> with initial <em>%control</em>
      "! and a <em>create by association</em> for the <em>SupplementText</em>.
      text_create                   FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } augments correctly
      "! when a EML <em>update</em> is triggered on an instance where the text exists in user language.
      "! This should lead to a set of { @link ..lhc_Supplement.METH:augment.DATA:entities_update }.
      "! The base BO should receive an <em>update</em> for <em>Supplement</em> with initial <em>%control</em>
      "! and a <em>update</em> for the <em>SupplementText</em>.
      text_update                   FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } sets the correct failed cause if
      "! the instance is not found.
      update_with_invalid_keys      FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } sets the correct failed cause if
      "! the instance is not found and does not augment.
      update_invalid_keys_descr     FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_tdf_buffer IMPLEMENTATION.

  METHOD class_setup.
    DATA(env_config) = cl_botd_txbufdbl_bo_test_env=>prepare_environment_config(
                         )->set_bdef_dependencies( VALUE #( ( cv_i_bdef_name ) )
                         )->handle_draft( VALUE #( ( cv_i_bdef_name ) ) ).

    environment = cl_botd_txbufdbl_bo_test_env=>create( env_config ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy(  ).
  ENDMETHOD.

  METHOD setup.
    environment->clear_doubles( ).
    double_supplement =  environment->get_test_double( cv_i_bdef_name ).
    double_supplement->configure_additional_behavior(  )->set_fields_handler( fields_handler = NEW ltd_fields_handler( ) ).
  ENDMETHOD.

  METHOD teardown.
    double_supplement->clear_double( ).
  ENDMETHOD.


  METHOD supplement_create.
    " Test
    DATA:
      c_create TYPE STRUCTURE FOR CREATE /dmo/c_supplement\\supplement,
      i_create TYPE STRUCTURE FOR CREATE /dmo/i_supplement\\supplement.

    c_create = VALUE #(
                   %cid                  = 'TEST_CID'
                   %is_draft             = if_abap_behv=>mk-on
                   supplementcategory    = 'XX'
                   supplementdescription = 'Test'
                   price                 = '42'
                   currencycode          = 'EUR'
                 ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        CREATE AUTO FILL CID
          FIELDS ( supplementcategory supplementdescription price currencycode )
          WITH VALUE #( ( c_create ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    " Verification
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_not_initial( mapped-supplement ).


    READ ENTITIES OF /dmo/i_supplement
      ENTITY supplement
        FIELDS ( supplementcategory price currencycode )
        WITH CORRESPONDING #( mapped-supplement )
        RESULT DATA(supplements)
      ENTITY supplement
        BY \_supplementtext
        FIELDS ( description )
        WITH CORRESPONDING #( mapped-supplement )
        RESULT DATA(supplement_texts)
      FAILED    i_failed
      REPORTED  i_reported.

    cl_abap_unit_assert=>assert_initial( i_failed ).
    cl_abap_unit_assert=>assert_initial( i_reported ).

    " Supplement
    cl_abap_unit_assert=>assert_not_initial( supplements ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( supplements ) ).

    i_create = CORRESPONDING #( c_create ).
    DATA: created_supplement_without_id LIKE LINE OF supplements.
    created_supplement_without_id = CORRESPONDING #( supplements[ 1 ] EXCEPT supplementid ).
    cl_abap_unit_assert=>assert_equals( exp = i_create-%data
                                        act = created_supplement_without_id-%data ).
    cl_abap_unit_assert=>assert_equals( exp = i_create-%is_draft
                                        act = created_supplement_without_id-%is_draft ).

    " Supplement Text
    cl_abap_unit_assert=>assert_not_initial( supplement_texts ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( supplement_texts ) ).
    DATA(act_i_cba) = supplement_texts[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-langu
                                        act = act_i_cba-languagecode ).
    cl_abap_unit_assert=>assert_equals( exp = c_create-supplementdescription
                                        act = act_i_cba-description ).
  ENDMETHOD.

  METHOD supplement_create_text_update.
    " Preparation
    DATA:
      c_create              TYPE STRUCTURE FOR CREATE /dmo/c_supplement\\supplement,
      c_update              TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      c_supplement_act      TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      c_supplement_exp      TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_supplement_act      TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement,
      i_supplement_exp      TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement,
      i_supplement_text_act TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplementtext.

    c_create = VALUE #(
                   %cid                  = 'TESTCID'
                   %is_draft             = if_abap_behv=>mk-on
                   supplementcategory    = 'XX'
                   supplementdescription = 'Test Create'
                   price                 = '42'
                   currencycode          = 'EUR'
                 ).

    c_update = VALUE #(
                   %cid_ref              = c_create-%cid
                   %is_draft             = c_create-%is_draft
                   supplementdescription = 'Test Update'
                   price                 = '123'
                   currencycode          = 'USD'
                 ).

    " Test
    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        CREATE
          FIELDS ( supplementcategory supplementdescription price currencycode )
          WITH VALUE #( ( c_create ) )
        UPDATE
          FIELDS ( supplementdescription price currencycode )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.


    " Verification
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_not_initial( mapped-supplement ).


    READ ENTITIES OF /dmo/i_supplement
      ENTITY supplement
        FIELDS ( supplementcategory price currencycode )
        WITH CORRESPONDING #( mapped-supplement )
        RESULT DATA(i_supplements)
      ENTITY supplement
        BY \_supplementtext
        FIELDS ( description )
        WITH CORRESPONDING #( mapped-supplement )
        RESULT DATA(i_supplement_texts)
      FAILED    i_failed
      REPORTED  i_reported.

    cl_abap_unit_assert=>assert_initial( i_failed ).
    cl_abap_unit_assert=>assert_initial( i_reported ).


    " Supplement
    cl_abap_unit_assert=>assert_not_initial( i_supplements ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( i_supplements ) ).

    i_supplement_act = CORRESPONDING #( i_supplements[ 1 ] EXCEPT %key ).
    i_supplement_exp = CORRESPONDING #( c_update EXCEPT %cid_ref %key supplementcategory ).
    i_supplement_exp-supplementcategory = c_create-supplementcategory.

    cl_abap_unit_assert=>assert_equals( exp = i_supplement_exp
                                        act = i_supplement_act ).


    " Supplement Text
    cl_abap_unit_assert=>assert_not_initial( i_supplement_texts ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( i_supplement_texts ) ).

    i_supplement_text_act = CORRESPONDING #( i_supplement_texts[ 1 ] EXCEPT supplementid ).

    cl_abap_unit_assert=>assert_equals( exp = sy-langu
                                        act = i_supplement_text_act-languagecode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-supplementdescription
                                        act = i_supplement_text_act-description ).

  ENDMETHOD.

  METHOD supplement_update_text_create.
    " Preparation
    DATA:
      i_create TYPE STRUCTURE FOR CREATE /dmo/i_supplement\\supplement.

    i_create = VALUE #(
        %is_draft          = if_abap_behv=>mk-on
        supplementcategory = 'XX'
        price              = '10'
        currencycode       = 'USD'
      ).
    MODIFY ENTITIES OF /dmo/i_supplement
      ENTITY supplement
        CREATE
          AUTO FILL CID
          FIELDS ( supplementcategory price currencycode )
          WITH VALUE #( ( i_create ) )
      MAPPED    i_mapped
      FAILED    i_failed
      REPORTED  i_reported.

    cl_abap_unit_assert=>assert_initial( i_failed ).
    cl_abap_unit_assert=>assert_initial( i_reported ).
    cl_abap_unit_assert=>assert_initial( i_mapped-supplementtext ).

    cl_abap_unit_assert=>assert_not_initial( i_mapped-supplement ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( i_mapped-supplement ) ).

    DATA(supplementid_to_test) = VALUE #( i_mapped-supplement[ 1 ]-supplementid ).

    " Test
    DATA:
      c_update TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_update TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    c_update = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   supplementid          = supplementid_to_test
                   supplementdescription = 'Test'
                   price                 = '42'
                   currencycode          = 'EUR'
                 ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        UPDATE
          FIELDS ( supplementdescription price currencycode )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    " Verification
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( mapped ).


    READ ENTITIES OF /dmo/i_supplement
      ENTITY supplement
        FIELDS ( supplementcategory price currencycode )
        WITH CORRESPONDING #( i_mapped-supplement )
        RESULT DATA(supplements)
      ENTITY supplement
        BY \_supplementtext
        FIELDS ( description )
        WITH CORRESPONDING #( i_mapped-supplement )
        RESULT DATA(supplement_texts)
      FAILED    i_failed
      REPORTED  i_reported.

    cl_abap_unit_assert=>assert_initial( i_failed ).
    cl_abap_unit_assert=>assert_initial( i_reported ).


    " Supplement
    cl_abap_unit_assert=>assert_not_initial( supplements ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( supplements ) ).

    i_update = CORRESPONDING #( c_update ).
    i_update-supplementcategory = i_create-supplementcategory.
    cl_abap_unit_assert=>assert_equals( exp = i_update-%data
                                        act = supplements[ 1 ]-%data ).
    cl_abap_unit_assert=>assert_equals( exp = i_update-%is_draft
                                        act = supplements[ 1 ]-%is_draft ).

    " Supplement Text
    cl_abap_unit_assert=>assert_not_initial( supplement_texts ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( supplement_texts ) ).
    DATA(act_i_cba) = supplement_texts[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-langu
                                        act = act_i_cba-languagecode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-supplementdescription
                                        act = act_i_cba-description ).
  ENDMETHOD.

  METHOD supplement_update_text_update.
    " Preparation
    DATA:
      i_supplement_create      TYPE STRUCTURE FOR CREATE /dmo/i_supplement\\supplement,
      i_supplement_text_create TYPE STRUCTURE FOR CREATE /dmo/i_supplement\\supplement\_supplementtext.

    i_supplement_create = VALUE #(
        %cid               = 'TEST_CID'
        %is_draft          = if_abap_behv=>mk-on
        supplementcategory = 'XX'
        price              = '10'
        currencycode       = 'USD'
      ).
    i_supplement_text_create = VALUE #(
        %cid_ref  = i_supplement_create-%cid
        %is_draft = if_abap_behv=>mk-on
        %target   = VALUE #(
            (
              %is_draft          = if_abap_behv=>mk-on
              languagecode = sy-langu
              description  = 'Before Description'
            )
          )
      ).
    MODIFY ENTITIES OF /dmo/i_supplement
      ENTITY supplement
        CREATE
          FIELDS ( supplementcategory price currencycode )
          WITH VALUE #( ( i_supplement_create ) )
        CREATE BY \_supplementtext
          AUTO FILL CID
          FIELDS ( description )
          WITH VALUE #( ( i_supplement_text_create ) )
      MAPPED    i_mapped
      FAILED    i_failed
      REPORTED  i_reported.

    cl_abap_unit_assert=>assert_initial( i_failed ).
    cl_abap_unit_assert=>assert_initial( i_reported ).

    cl_abap_unit_assert=>assert_not_initial( i_mapped-supplement ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( i_mapped-supplement ) ).
    cl_abap_unit_assert=>assert_not_initial( i_mapped-supplementtext ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( i_mapped-supplementtext ) ).

    DATA(supplementid_to_test) = VALUE #( i_mapped-supplement[ 1 ]-supplementid ).

    " Test
    DATA:
      c_update TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_update TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    c_update = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   supplementid          = supplementid_to_test
                   supplementdescription = 'Updated Description'
                   price                 = '42'
                   currencycode          = 'EUR'
                 ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        UPDATE
          FIELDS ( supplementdescription price currencycode )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( mapped ).


    READ ENTITIES OF /dmo/i_supplement
      ENTITY supplement
        FIELDS ( supplementcategory price currencycode )
        WITH CORRESPONDING #( i_mapped-supplement )
        RESULT DATA(supplements)
      ENTITY supplement
        BY \_supplementtext
        FIELDS ( description )
        WITH CORRESPONDING #( i_mapped-supplement )
        RESULT DATA(supplement_texts)
      FAILED    i_failed
      REPORTED  i_reported.

    cl_abap_unit_assert=>assert_initial( i_failed ).
    cl_abap_unit_assert=>assert_initial( i_reported ).


    " Supplement
    cl_abap_unit_assert=>assert_not_initial( supplements ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( supplements ) ).


    DATA:
      i_supplement_exp TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement,
      i_supplement_act TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    i_supplement_exp = CORRESPONDING #( c_update EXCEPT supplementid ).
    i_supplement_exp-supplementcategory = i_supplement_create-supplementcategory.
    i_supplement_act = CORRESPONDING #( supplements[ 1 ] EXCEPT supplementid ).
    cl_abap_unit_assert=>assert_equals( exp = i_supplement_exp
                                        act = i_supplement_act ).

    " Supplement Text
    cl_abap_unit_assert=>assert_not_initial( supplement_texts ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( supplement_texts ) ).
    cl_abap_unit_assert=>assert_not_initial( supplement_texts[ 1 ] ).
    DATA(act_i_update) = supplement_texts[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-langu
                                        act = act_i_update-languagecode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-supplementdescription
                                        act = act_i_update-description ).
  ENDMETHOD.

  METHOD text_create.
    " Preparation
    DATA:
      i_supplement_create      TYPE STRUCTURE FOR CREATE /dmo/i_supplement\\supplement.

    i_supplement_create = VALUE #(
        %cid               = 'TEST_CID'
        %is_draft          = if_abap_behv=>mk-on
        supplementcategory = 'XX'
        price              = '10'
        currencycode       = 'USD'
      ).
    MODIFY ENTITIES OF /dmo/i_supplement
      ENTITY supplement
        CREATE
          FIELDS ( supplementcategory price currencycode )
          WITH VALUE #( ( i_supplement_create ) )
      MAPPED    i_mapped
      FAILED    i_failed
      REPORTED  i_reported.

    cl_abap_unit_assert=>assert_initial( i_failed ).
    cl_abap_unit_assert=>assert_initial( i_reported ).

    cl_abap_unit_assert=>assert_not_initial( i_mapped-supplement ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( i_mapped-supplement ) ).
    cl_abap_unit_assert=>assert_initial( i_mapped-supplementtext ).

    DATA(supplementid_to_test) = VALUE #( i_mapped-supplement[ 1 ]-supplementid ).

    " Test
    DATA:
      c_update TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_update TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    c_update = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   supplementid          = supplementid_to_test
                   supplementdescription = 'Test'
                 ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        UPDATE
          FIELDS ( supplementdescription )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.


    READ ENTITIES OF /dmo/i_supplement
      ENTITY supplement
        FIELDS ( supplementcategory price currencycode )
        WITH CORRESPONDING #( i_mapped-supplement )
        RESULT DATA(supplements)
      ENTITY supplement
        BY \_supplementtext
        FIELDS ( description )
        WITH CORRESPONDING #( i_mapped-supplement )
        RESULT DATA(supplement_texts)
      FAILED    i_failed
      REPORTED  i_reported.

    cl_abap_unit_assert=>assert_initial( i_failed ).
    cl_abap_unit_assert=>assert_initial( i_reported ).


    " Supplement
    cl_abap_unit_assert=>assert_not_initial( supplements ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( supplements ) ).

    DATA:
      i_supplement_exp TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement,
      i_supplement_act TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    i_supplement_exp = CORRESPONDING #( i_supplement_create EXCEPT supplementid ).
    i_supplement_act = CORRESPONDING #( supplements[ 1 ] EXCEPT supplementid ).
    cl_abap_unit_assert=>assert_equals( exp = i_supplement_exp
                                        act = i_supplement_act ).

    " Supplement Text
    cl_abap_unit_assert=>assert_not_initial( supplement_texts ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( supplement_texts ) ).
    cl_abap_unit_assert=>assert_not_initial( supplement_texts[ 1 ] ).
    DATA(act_i_update) = supplement_texts[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-langu
                                        act = act_i_update-languagecode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-supplementdescription
                                        act = act_i_update-description ).
  ENDMETHOD.

  METHOD text_update.
    " Preparation
    DATA:
      i_supplement_create      TYPE STRUCTURE FOR CREATE /dmo/i_supplement\\supplement,
      i_supplement_text_create TYPE STRUCTURE FOR CREATE /dmo/i_supplement\\supplement\_supplementtext.

    i_supplement_create = VALUE #(
        %cid               = 'TEST_CID'
        %is_draft          = if_abap_behv=>mk-on
        supplementcategory = 'XX'
        price              = '10'
        currencycode       = 'USD'
      ).
    i_supplement_text_create = VALUE #(
        %cid_ref  = i_supplement_create-%cid
        %is_draft = if_abap_behv=>mk-on
        %target   = VALUE #(
            (
              %is_draft          = if_abap_behv=>mk-on
              languagecode = sy-langu
              description  = 'Before Description'
            )
          )
      ).
    MODIFY ENTITIES OF /dmo/i_supplement
      ENTITY supplement
        CREATE
          FIELDS ( supplementcategory price currencycode )
          WITH VALUE #( ( i_supplement_create ) )
        CREATE BY \_supplementtext
          AUTO FILL CID
          FIELDS ( description )
          WITH VALUE #( ( i_supplement_text_create ) )
      MAPPED    i_mapped
      FAILED    i_failed
      REPORTED  i_reported.

    cl_abap_unit_assert=>assert_initial( i_failed ).
    cl_abap_unit_assert=>assert_initial( i_reported ).

    cl_abap_unit_assert=>assert_not_initial( i_mapped-supplement ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( i_mapped-supplement ) ).
    cl_abap_unit_assert=>assert_not_initial( i_mapped-supplementtext ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( i_mapped-supplementtext ) ).

    DATA(supplementid_to_test) = VALUE #( i_mapped-supplement[ 1 ]-supplementid ).

    " Test
    DATA:
      c_update TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_update TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    c_update = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   supplementid          = supplementid_to_test
                   supplementdescription = 'Test'
                 ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        UPDATE
          FIELDS ( supplementdescription )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    " Verification
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_initial( mapped ).


    READ ENTITIES OF /dmo/i_supplement
      ENTITY supplement
        FIELDS ( supplementcategory price currencycode )
        WITH CORRESPONDING #( i_mapped-supplement )
        RESULT DATA(supplements)
      ENTITY supplement
        BY \_supplementtext
        FIELDS ( description )
        WITH CORRESPONDING #( i_mapped-supplement )
        RESULT DATA(supplement_texts)
      FAILED    i_failed
      REPORTED  i_reported.

    cl_abap_unit_assert=>assert_initial( i_failed ).
    cl_abap_unit_assert=>assert_initial( i_reported ).


    " Supplement Text
    cl_abap_unit_assert=>assert_not_initial( supplement_texts ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( supplement_texts ) ).
    DATA(act_i_cba) = supplement_texts[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-langu
                                        act = act_i_cba-languagecode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-supplementdescription
                                        act = act_i_cba-description ).

  ENDMETHOD.

  METHOD update_with_invalid_keys.
    TYPES:
      t_c_failed_supplement     LIKE LINE OF failed-supplement,
      t_c_failed_supplement_tky TYPE t_c_failed_supplement-%tky.

    DATA:
      c_update_no_key      TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      c_update_invalid_key TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_update             TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    c_update_no_key      = VALUE #( %is_draft = if_abap_behv=>mk-on  supplementid = ''     ).
    c_update_invalid_key = VALUE #( %is_draft = if_abap_behv=>mk-on  supplementid = 'TEST' ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        UPDATE
          FIELDS ( supplementdescription price currencycode )
          WITH VALUE #( ( c_update_no_key ) ( c_update_invalid_key ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lines( failed-supplement )
      ).


    cl_abap_unit_assert=>assert_not_initial( VALUE #( failed-supplement[ KEY draft  %tky = CORRESPONDING #( c_update_no_key-%tky     ) ] OPTIONAL ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = if_abap_behv=>cause-not_found
        act = failed-supplement[ KEY draft  %tky = c_update_no_key-%tky      ]-%fail-cause
      ).

    cl_abap_unit_assert=>assert_not_initial( VALUE #( failed-supplement[ KEY draft  %tky = CORRESPONDING #( c_update_invalid_key-%tky ) ] OPTIONAL ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = if_abap_behv=>cause-not_found
        act = failed-supplement[ KEY draft  %tky = c_update_invalid_key-%tky ]-%fail-cause
      ).
  ENDMETHOD.

  METHOD update_invalid_keys_descr.
    TYPES:
      t_c_failed_supplement     LIKE LINE OF failed-supplement,
      t_c_failed_supplement_tky TYPE t_c_failed_supplement-%tky.

    DATA:
      c_update_no_flag     TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      c_update_invalid_key TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_update             TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    c_update_no_flag     = VALUE #( %is_draft = if_abap_behv=>mk-on  supplementid = 'TEST1'  supplementdescription = 'Test'  %control-supplementdescription = if_abap_behv=>mk-off ).
    c_update_invalid_key = VALUE #( %is_draft = if_abap_behv=>mk-on  supplementid = 'TEST2'  supplementdescription = 'Test'  %control-supplementdescription = if_abap_behv=>mk-on  ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        UPDATE
          FIELDS ( supplementdescription price currencycode )
          WITH VALUE #( ( c_update_no_flag ) ( c_update_invalid_key ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lines( failed-supplement )
      ).


    cl_abap_unit_assert=>assert_not_initial( VALUE #( failed-supplement[ KEY draft  %tky = CORRESPONDING #( c_update_no_flag-%tky    ) ] OPTIONAL ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = if_abap_behv=>cause-not_found
        act = failed-supplement[ KEY draft  %tky = c_update_no_flag-%tky     ]-%fail-cause
      ).

    cl_abap_unit_assert=>assert_not_initial( VALUE #( failed-supplement[ KEY draft  %tky = CORRESPONDING #( c_update_invalid_key-%tky ) ] OPTIONAL ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = if_abap_behv=>cause-not_found
        act = failed-supplement[ KEY draft  %tky = c_update_invalid_key-%tky ]-%fail-cause
      ).
  ENDMETHOD.


ENDCLASS.









""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Tests using custom test double
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


CLASS ltd_custom_test_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES if_abap_behavior_testdouble.

    CONSTANTS:
      supplement_entity_name TYPE abp_root_entity_name VALUE '/DMO/I_SUPPLEMENT' ##NO_TEXT,
      text_entity_name       TYPE abp_root_entity_name VALUE '/DMO/I_SUPPLEMENTTEXT' ##NO_TEXT,
      text_sub_name          TYPE abp_behv_changes-sub_name VALUE '_SUPPLEMENTTEXT' ##NO_TEXT.

    DATA:
      supplement_create TYPE TABLE FOR CREATE /dmo/i_supplement\\supplement,
      supplement_update TYPE TABLE FOR UPDATE /dmo/i_supplement\\supplement,
      supplement_failed TYPE RESPONSE FOR FAILED /dmo/i_supplement,

      text_create       TYPE TABLE FOR CREATE /dmo/i_supplement\\supplement\_supplementtext,
      text_update       TYPE TABLE FOR UPDATE /dmo/i_supplement\\supplementtext,

      text_rba_link     TYPE TABLE FOR READ LINK   /dmo/i_supplement\\supplement\_supplementtext,

      text_read_failed  TYPE RESPONSE FOR FAILED /dmo/i_supplement.
ENDCLASS.

CLASS ltd_custom_test_double IMPLEMENTATION.
  METHOD if_abap_behavior_testdouble~modify.
    FIELD-SYMBOLS:
      <supplement_create> LIKE supplement_create,
      <supplement_update> LIKE supplement_update,
      <text_create>       LIKE text_create,
      <text_update>       LIKE text_update.

    LOOP AT changes INTO DATA(change).
      CASE change-op.
        WHEN if_abap_behv=>op-m-create.
          ASSIGN change-instances->* TO <supplement_create>.
          APPEND LINES OF <supplement_create> TO supplement_create.

        WHEN if_abap_behv=>op-m-create_ba.
          ASSIGN change-instances->* TO <text_create>.
          APPEND LINES OF <text_create> TO text_create.

        WHEN if_abap_behv=>op-m-update.
          CASE change-entity_name.
            WHEN supplement_entity_name.
              ASSIGN change-instances->* TO <supplement_update>.
              APPEND LINES OF <supplement_update> TO supplement_update.
            WHEN text_entity_name.
              ASSIGN change-instances->* TO <text_update>.
              APPEND LINES OF <text_update> TO text_update.
            WHEN OTHERS.
              cl_abap_unit_assert=>fail( 'Unexpected Entity' ).
          ENDCASE.

        WHEN OTHERS.
          cl_abap_unit_assert=>fail( 'Unexpected Operation' ).
      ENDCASE.
    ENDLOOP.

    failed = supplement_failed.
  ENDMETHOD.

  METHOD if_abap_behavior_testdouble~read.
    TYPES:
      key         TYPE STRUCTURE FOR READ IMPORT /dmo/i_supplement\\supplement\_supplementtext.

    DATA:
      links_to_be_added  LIKE text_rba_link.

    FIELD-SYMBOLS:
      <key>   TYPE key,
      <links> LIKE text_rba_link.

    LOOP AT retrievals ASSIGNING FIELD-SYMBOL(<retrieval>).
      cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>op-r-read_ba
                                          act = <retrieval>-op ).
      cl_abap_unit_assert=>assert_equals( exp = supplement_entity_name
                                          act = <retrieval>-entity_name ).
      cl_abap_unit_assert=>assert_equals( exp = text_sub_name
                                          act = <retrieval>-sub_name ).
      cl_abap_unit_assert=>assert_not_bound( <retrieval>-results ).
      cl_abap_unit_assert=>assert_bound(     <retrieval>-links   ).


      ASSIGN <retrieval>-links->* TO <links>.

      LOOP AT <retrieval>-instances->* ASSIGNING <key>.
        links_to_be_added = VALUE #(
                                     FOR link IN text_rba_link
                                       USING KEY draft
                                       WHERE ( source-%is_draft = <key>-%is_draft AND source-supplementid = <key>-supplementid )
                                     ( link )
                                   ).
        APPEND LINES OF links_to_be_added TO <links>.
      ENDLOOP.
    ENDLOOP.
    failed = text_read_failed.
  ENDMETHOD.
ENDCLASS.

"! Using custom test double { @link .ltd_custom_test_double }.
"! @testing BDEF:/DMO/C_SUPPLEMENT
CLASS ltcl_custom_test_double DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      entity_double TYPE REF TO ltd_custom_test_double.

    DATA:
      mapped   TYPE RESPONSE FOR MAPPED   EARLY /dmo/c_supplement,
      failed   TYPE RESPONSE FOR FAILED   EARLY /dmo/c_supplement,
      reported TYPE RESPONSE FOR REPORTED EARLY /dmo/c_supplement.

    METHODS:
      setup,
      teardown.

    METHODS:
      "! Checks if { @link ..lhc_Supplement.METH:augment } augments correctly to <em>create</em> a <em>Supplement</em>
      "! and <em>create by association</em> of a <em>SupplementText</em>
      "! when an EML create is triggered.  This leads to a set of { @link ..lhc_Supplement.METH:augment.DATA:entities_create }.
      supplement_create             FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } augments correctly
      "! when a EML <em>create</em> and a EML <p>update</p> on the same instance is triggered.
      "! This should lead to a set of { @link ..lhc_Supplement.METH:augment.DATA:entities_create } and
      "! a correlated set of { @link ..lhc_Supplement.METH:augment.DATA:entities_update }.
      "! The base BO should receive a <em>create</em> and <em>update</em> for <em>Supplement</em>
      "! and a <em>create by association</em> and <em>update</em> for the <em>SupplementText</em>.
      supplement_create_text_update FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } augments correctly
      "! when a EML <em>update</em> is triggered.
      "! This should lead to a set of { @link ..lhc_Supplement.METH:augment.DATA:entities_update }.
      "! The base BO should receive an <em>update</em> for <em>Supplement</em>
      "! and a <em>create by association</em> for the <em>SupplementText</em>.
      supplement_update_text_create FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } augments correctly
      "! when a EML <em>update</em> is triggered on an instance where the text exists in user language.
      "! This should lead to a set of { @link ..lhc_Supplement.METH:augment.DATA:entities_update }.
      "! The base BO should receive an <em>update</em> for <em>Supplement</em>
      "! and a <em>update</em> for the <em>SupplementText</em>.
      supplement_update_text_update FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } augments correctly
      "! when a EML <em>update</em> is triggered with an flags only for <em>description</em> on an instance
      "! where the text does not exists in user language.
      "! This should lead to a set of { @link ..lhc_Supplement.METH:augment.DATA:entities_update }.
      "! The base BO should receive an <em>update</em> for <em>Supplement</em> with initial <em>%control</em>
      "! and a <em>create by association</em> for the <em>SupplementText</em>.
      text_create                   FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } augments correctly
      "! when a EML <em>update</em> is triggered on an instance where the text exists in user language.
      "! This should lead to a set of { @link ..lhc_Supplement.METH:augment.DATA:entities_update }.
      "! The base BO should receive an <em>update</em> for <em>Supplement</em> with initial <em>%control</em>
      "! and a <em>update</em> for the <em>SupplementText</em>.
      text_update                   FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } sets the correct failed cause if
      "! the instance is not found.
      update_with_invalid_keys      FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:augment } sets the correct failed cause if
      "! the instance is not found and does not augment.
      update_invalid_keys_descr     FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_custom_test_double IMPLEMENTATION.

  METHOD setup.
    CLEAR:
      mapped,
      failed,
      reported,
      entity_double.

    CREATE OBJECT entity_double.
    entity_double->if_abap_behavior_testdouble~root_name = ltd_custom_test_double=>supplement_entity_name.
    entity_double->if_abap_behavior_testdouble~handle_draft = abap_true.
    cl_abap_behv_test_environment=>set_test_double( object = entity_double ).
  ENDMETHOD.

  METHOD teardown.
    cl_abap_behv_test_environment=>unset_test_double( root = ltd_custom_test_double=>supplement_entity_name ).
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD supplement_create.
    DATA:
      c_create TYPE STRUCTURE FOR CREATE /dmo/c_supplement\\supplement,
      i_create TYPE STRUCTURE FOR CREATE /dmo/i_supplement\\supplement.

    c_create = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   supplementcategory    = 'XX'
                   supplementdescription = 'Test'
                   price                 = '42'
                   currencycode          = 'EUR'
                 ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        CREATE AUTO FILL CID
          FIELDS ( supplementcategory supplementdescription price currencycode )
          WITH VALUE #( ( c_create ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    " Supplement
    cl_abap_unit_assert=>assert_not_initial( entity_double->supplement_create ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->supplement_create ) ).

    i_create = CORRESPONDING #( c_create ).
    cl_abap_unit_assert=>assert_equals( exp = i_create-%data
                                        act = entity_double->supplement_create[ 1 ]-%data ).
    cl_abap_unit_assert=>assert_equals( exp = i_create-%is_draft
                                        act = entity_double->supplement_create[ 1 ]-%is_draft ).

    " Supplement Text
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_create ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->text_create ) ).
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_create[ 1 ]-%target ).
    DATA(act_i_cba) = entity_double->text_create[ 1 ]-%target[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-langu
                                        act = act_i_cba-languagecode ).
    cl_abap_unit_assert=>assert_equals( exp = c_create-supplementdescription
                                        act = act_i_cba-description ).
  ENDMETHOD.

  METHOD supplement_create_text_update.
    "Create -------------------------------
    DATA:
      c_create TYPE STRUCTURE FOR CREATE /dmo/c_supplement\\supplement,
      i_create TYPE STRUCTURE FOR CREATE /dmo/i_supplement\\supplement,
      c_update TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_update TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    c_create = VALUE #(
                   %cid                  = 'TESTCID'
                   %is_draft             = if_abap_behv=>mk-on
                   supplementcategory    = 'XX'
                   supplementdescription = 'Test Create'
                   price                 = '42'
                   currencycode          = 'EUR'
                 ).

    c_update = VALUE #(
                   %cid_ref              = c_create-%cid
                   %is_draft             = c_create-%is_draft
                   supplementdescription = 'Test Update'
                   price                 = '123'
                   currencycode          = 'USD'
                 ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        CREATE
          FIELDS ( supplementcategory supplementdescription price currencycode )
          WITH VALUE #( ( c_create ) )
        UPDATE
          FIELDS ( supplementdescription price currencycode )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).


    "Create

    " Supplement
    cl_abap_unit_assert=>assert_not_initial( entity_double->supplement_create ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->supplement_create ) ).

    i_create = CORRESPONDING #( c_create ).
    cl_abap_unit_assert=>assert_equals( exp = i_create-%data
                                        act = entity_double->supplement_create[ 1 ]-%data ).
    cl_abap_unit_assert=>assert_equals( exp = i_create-%is_draft
                                        act = entity_double->supplement_create[ 1 ]-%is_draft ).

    " Supplement Text
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_create ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->text_create ) ).
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_create[ 1 ]-%target ).
    DATA(act_i_cba) = entity_double->text_create[ 1 ]-%target[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-langu
                                        act = act_i_cba-languagecode ).
    cl_abap_unit_assert=>assert_equals( exp = c_create-supplementdescription
                                        act = act_i_cba-description ).


    " Update

    " Supplement
    cl_abap_unit_assert=>assert_not_initial( entity_double->supplement_update ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->supplement_update ) ).

    i_update = CORRESPONDING #( c_update ).
    cl_abap_unit_assert=>assert_equals( exp = i_update-%data
                                        act = entity_double->supplement_update[ 1 ]-%data ).
    cl_abap_unit_assert=>assert_equals( exp = i_update-%is_draft
                                        act = entity_double->supplement_update[ 1 ]-%is_draft ).

    " Supplement Text
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_update ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->text_update ) ).
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_update[ 1 ] ).
    DATA(act_i_update) = entity_double->text_update[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-langu
                                        act = act_i_update-languagecode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-supplementdescription
                                        act = act_i_update-description ).
  ENDMETHOD.

  METHOD supplement_update_text_create.
    DATA:
      c_update TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_update TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    c_update = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   supplementid          = 'XX123'
                   supplementdescription = 'Test'
                   price                 = '42'
                   currencycode          = 'EUR'
                 ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        UPDATE
          FIELDS ( supplementdescription price currencycode )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).


    " Supplement
    cl_abap_unit_assert=>assert_not_initial( entity_double->supplement_update ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->supplement_update ) ).

    i_update = CORRESPONDING #( c_update ).
    cl_abap_unit_assert=>assert_equals( exp = i_update-%data
                                        act = entity_double->supplement_update[ 1 ]-%data ).
    cl_abap_unit_assert=>assert_equals( exp = i_update-%is_draft
                                        act = entity_double->supplement_update[ 1 ]-%is_draft ).

    " Supplement Text
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_create ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->text_create ) ).
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_create[ 1 ]-%target ).
    DATA(act_i_cba) = entity_double->text_create[ 1 ]-%target[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-langu
                                        act = act_i_cba-languagecode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-supplementdescription
                                        act = act_i_cba-description ).
  ENDMETHOD.

  METHOD supplement_update_text_update.
    DATA:
      c_update TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_update TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    c_update = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   supplementid          = 'XX123'
                   supplementdescription = 'Test'
                   price                 = '42'
                   currencycode          = 'EUR'
                 ).

    entity_double->text_rba_link = VALUE #( (
        source = VALUE #( %is_draft = c_update-%is_draft  supplementid = c_update-supplementid )
        target = VALUE #( %is_draft = c_update-%is_draft  supplementid = c_update-supplementid  languagecode = 'EN' )
      ) ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        UPDATE
          FIELDS ( supplementdescription price currencycode )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).


    " Supplement
    cl_abap_unit_assert=>assert_not_initial( entity_double->supplement_update ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->supplement_update ) ).

    i_update = CORRESPONDING #( c_update ).
    cl_abap_unit_assert=>assert_equals( exp = i_update-%data
                                        act = entity_double->supplement_update[ 1 ]-%data ).
    cl_abap_unit_assert=>assert_equals( exp = i_update-%is_draft
                                        act = entity_double->supplement_update[ 1 ]-%is_draft ).

    " Supplement Text
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_update ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->text_update ) ).
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_update[ 1 ] ).
    DATA(act_i_update) = entity_double->text_update[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-langu
                                        act = act_i_update-languagecode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-supplementdescription
                                        act = act_i_update-description ).
  ENDMETHOD.

  METHOD text_create.
    DATA:
      c_update TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_update TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    c_update = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   supplementid          = 'XX123'
                   supplementdescription = 'Test'
                 ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        UPDATE
          FIELDS ( supplementdescription )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).


    " Supplement gets an Update but with an initial %control, without changes so to say
    cl_abap_unit_assert=>assert_not_initial( entity_double->supplement_update ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->supplement_update ) ).
    cl_abap_unit_assert=>assert_initial( entity_double->supplement_update[ 1 ]-%control ).

    " Supplement Text
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_create ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->text_create ) ).
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_create[ 1 ]-%target ).
    DATA(act_i_cba) = entity_double->text_create[ 1 ]-%target[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-langu
                                        act = act_i_cba-languagecode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-supplementdescription
                                        act = act_i_cba-description ).
  ENDMETHOD.

  METHOD text_update.
    DATA:
      c_update TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_update TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    c_update = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   supplementid          = 'XX123'
                   supplementdescription = 'Test'
                 ).

    entity_double->text_rba_link = VALUE #( (
        source = VALUE #( %is_draft = c_update-%is_draft  supplementid = c_update-supplementid )
        target = VALUE #( %is_draft = c_update-%is_draft  supplementid = c_update-supplementid  languagecode = 'EN' )
      ) ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        UPDATE
          FIELDS ( supplementdescription )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).


    " Supplement gets an Update but with an initial %control, without changes so to say
    cl_abap_unit_assert=>assert_not_initial( entity_double->supplement_update ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->supplement_update ) ).
    cl_abap_unit_assert=>assert_initial( entity_double->supplement_update[ 1 ]-%control ).

    " Supplement Text
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_update ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( entity_double->text_update ) ).
    cl_abap_unit_assert=>assert_not_initial( entity_double->text_update[ 1 ] ).
    DATA(act_i_update) = entity_double->text_update[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = sy-langu
                                        act = act_i_update-languagecode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-supplementdescription
                                        act = act_i_update-description ).
  ENDMETHOD.

  METHOD update_with_invalid_keys.
    TYPES:
      t_c_failed_supplement     LIKE LINE OF failed-supplement,
      t_c_failed_supplement_tky TYPE t_c_failed_supplement-%tky.

    DATA:
      c_update_no_key      TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      c_update_invalid_key TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_update             TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    c_update_no_key      = VALUE #( %is_draft = if_abap_behv=>mk-on  supplementid = ''     ).
    c_update_invalid_key = VALUE #( %is_draft = if_abap_behv=>mk-on  supplementid = 'TEST' ).

    entity_double->text_read_failed-supplement = VALUE #(
        %fail = VALUE #( cause = if_abap_behv=>cause-not_found )
        ( %tky = CORRESPONDING #( c_update_no_key-%tky )       )
        ( %tky = CORRESPONDING #( c_update_invalid_key-%tky )  )
      ).

    entity_double->supplement_failed = entity_double->text_read_failed.

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        UPDATE
          FIELDS ( supplementdescription price currencycode )
          WITH VALUE #( ( c_update_no_key ) ( c_update_invalid_key ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lines( failed-supplement )
      ).


    cl_abap_unit_assert=>assert_not_initial( VALUE #( failed-supplement[ KEY draft  %tky = CORRESPONDING #( c_update_no_key-%tky     ) ] OPTIONAL ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = if_abap_behv=>cause-not_found
        act = failed-supplement[ KEY draft  %tky = c_update_no_key-%tky      ]-%fail-cause
      ).

    cl_abap_unit_assert=>assert_not_initial( VALUE #( failed-supplement[ KEY draft  %tky = CORRESPONDING #( c_update_invalid_key-%tky ) ] OPTIONAL ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = if_abap_behv=>cause-not_found
        act = failed-supplement[ KEY draft  %tky = c_update_invalid_key-%tky ]-%fail-cause
      ).
  ENDMETHOD.

  METHOD update_invalid_keys_descr.
    TYPES:
      t_c_failed_supplement     LIKE LINE OF failed-supplement,
      t_c_failed_supplement_tky TYPE t_c_failed_supplement-%tky.

    DATA:
      c_update_no_flag     TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      c_update_invalid_key TYPE STRUCTURE FOR UPDATE /dmo/c_supplement\\supplement,
      i_update             TYPE STRUCTURE FOR UPDATE /dmo/i_supplement\\supplement.

    c_update_no_flag     = VALUE #( %is_draft = if_abap_behv=>mk-on  supplementid = 'TEST1'  supplementdescription = 'Test'  %control-supplementdescription = if_abap_behv=>mk-off ).
    c_update_invalid_key = VALUE #( %is_draft = if_abap_behv=>mk-on  supplementid = 'TEST2'  supplementdescription = 'Test'  %control-supplementdescription = if_abap_behv=>mk-on  ).

    entity_double->text_read_failed-supplement = VALUE #(
        %fail = VALUE #( cause = if_abap_behv=>cause-not_found )
        ( %tky = CORRESPONDING #( c_update_no_flag-%tky )       )
        ( %tky = CORRESPONDING #( c_update_invalid_key-%tky )  )
      ).

    entity_double->supplement_failed = entity_double->text_read_failed.

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY supplement
        UPDATE
          FIELDS ( supplementdescription price currencycode )
          WITH VALUE #( ( c_update_no_flag ) ( c_update_invalid_key ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lines( failed-supplement )
      ).


    cl_abap_unit_assert=>assert_not_initial( VALUE #( failed-supplement[ KEY draft  %tky = CORRESPONDING #( c_update_no_flag-%tky    ) ] OPTIONAL ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = if_abap_behv=>cause-not_found
        act = failed-supplement[ KEY draft  %tky = c_update_no_flag-%tky     ]-%fail-cause
      ).

    cl_abap_unit_assert=>assert_not_initial( VALUE #( failed-supplement[ KEY draft  %tky = CORRESPONDING #( c_update_invalid_key-%tky ) ] OPTIONAL ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = if_abap_behv=>cause-not_found
        act = failed-supplement[ KEY draft  %tky = c_update_invalid_key-%tky ]-%fail-cause
      ).

    cl_abap_unit_assert=>assert_initial( entity_double->text_update ).
  ENDMETHOD.

ENDCLASS.
