
CLASS lcl_test_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES if_abap_behavior_testdouble.

    CONSTANTS:
      supplement_entity_name TYPE abp_root_entity_name VALUE '/DMO/I_SUPPLEMENT' ##NO_TEXT,
      text_entity_name       TYPE abp_root_entity_name VALUE '/DMO/I_SUPPLEMENTTEXT' ##NO_TEXT,
      text_sub_name          TYPE abp_behv_changes-sub_name VALUE '_SUPPLEMENTTEXT' ##NO_TEXT.

    DATA:
      supplement_create TYPE TABLE FOR CREATE /DMO/I_Supplement\\Supplement,
      supplement_update TYPE TABLE FOR UPDATE /DMO/I_Supplement\\Supplement,
      text_create       TYPE TABLE FOR CREATE /DMO/I_Supplement\\Supplement\_SupplementText,
      text_update       TYPE TABLE FOR UPDATE /DMO/I_Supplement\\SupplementText,

      text_rba_link     TYPE TABLE FOR READ LINK   /DMO/I_Supplement\\Supplement\_SupplementText.
ENDCLASS.

CLASS lcl_test_double IMPLEMENTATION.
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
  ENDMETHOD.

  METHOD if_abap_behavior_testdouble~read.
    TYPES:
      key         TYPE STRUCTURE FOR READ IMPORT /DMO/I_Supplement\\Supplement\_SupplementText.

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
                                       WHERE ( source-%is_draft = <key>-%is_draft AND source-SupplementID = <key>-SupplementID )
                                     ( link )
                                   ).
        APPEND LINES OF links_to_be_added TO <links>.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_augmentation DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      entity_double TYPE REF TO lcl_test_double.

    DATA:
      mapped   TYPE RESPONSE FOR MAPPED EARLY /dmo/c_supplement,
      failed   TYPE RESPONSE FOR FAILED EARLY /dmo/c_supplement,
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
      text_update                   FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_augmentation IMPLEMENTATION.

  METHOD setup.
    CLEAR:
      mapped,
      failed,
      reported,
      entity_double.

    CREATE OBJECT entity_double.
    entity_double->if_abap_behavior_testdouble~root_name = lcl_test_double=>supplement_entity_name.
    entity_double->if_abap_behavior_testdouble~handle_draft = abap_true.
    cl_abap_behv_test_environment=>set_test_double( object = entity_double ).
  ENDMETHOD.

  METHOD teardown.
    cl_abap_behv_test_environment=>unset_test_double( root = lcl_test_double=>supplement_entity_name ).
    ROLLBACK ENTITIES. "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD supplement_create.
    DATA:
      c_create TYPE STRUCTURE FOR CREATE /DMO/C_Supplement\\Supplement,
      i_create TYPE STRUCTURE FOR CREATE /DMO/I_Supplement\\Supplement.

    c_create = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   SupplementCategory    = 'XX'
                   SupplementDescription = 'Test'
                   Price                 = '42'
                   CurrencyCode          = 'EUR'
                 ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY Supplement
        CREATE AUTO FILL CID
          FIELDS ( SupplementCategory SupplementDescription Price CurrencyCode )
          WITH VALUE #( ( c_create ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.

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
                                        act = act_i_cba-LanguageCode ).
    cl_abap_unit_assert=>assert_equals( exp = c_create-SupplementDescription
                                        act = act_i_cba-Description ).
  ENDMETHOD.

  METHOD supplement_create_text_update.
    "Create -------------------------------
    DATA:
      c_create TYPE STRUCTURE FOR CREATE /DMO/C_Supplement\\Supplement,
      i_create TYPE STRUCTURE FOR CREATE /DMO/I_Supplement\\Supplement,
      c_update TYPE STRUCTURE FOR UPDATE /DMO/C_Supplement\\Supplement,
      i_update TYPE STRUCTURE FOR UPDATE /DMO/I_Supplement\\Supplement.

    c_create = VALUE #(
                   %cid                  = 'TESTCID'
                   %is_draft             = if_abap_behv=>mk-on
                   SupplementCategory    = 'XX'
                   SupplementDescription = 'Test Create'
                   Price                 = '42'
                   CurrencyCode          = 'EUR'
                 ).

    c_update = VALUE #(
                   %cid_ref              = c_create-%cid
                   %is_draft             = c_create-%is_draft
                   SupplementDescription = 'Test Update'
                   price                 = '123'
                   CurrencyCode          = 'USD'
                 ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY Supplement
        CREATE
          FIELDS ( SupplementCategory SupplementDescription Price CurrencyCode )
          WITH VALUE #( ( c_create ) )
        UPDATE
          FIELDS ( SupplementDescription Price CurrencyCode )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.


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
                                        act = act_i_cba-LanguageCode ).
    cl_abap_unit_assert=>assert_equals( exp = c_create-SupplementDescription
                                        act = act_i_cba-Description ).


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
                                        act = act_i_update-LanguageCode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-SupplementDescription
                                        act = act_i_update-Description ).
  ENDMETHOD.

  METHOD supplement_update_text_create.
    DATA:
      c_update TYPE STRUCTURE FOR UPDATE /DMO/C_Supplement\\Supplement,
      i_update TYPE STRUCTURE FOR UPDATE /DMO/I_Supplement\\Supplement.

    c_update = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   SupplementID          = 'XX123'
                   SupplementDescription = 'Test'
                   price                 = '42'
                   CurrencyCode          = 'EUR'
                 ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY Supplement
        UPDATE
          FIELDS ( SupplementDescription Price CurrencyCode )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.


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
                                        act = act_i_cba-LanguageCode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-SupplementDescription
                                        act = act_i_cba-Description ).
  ENDMETHOD.

  METHOD supplement_update_text_update.
    DATA:
      c_update TYPE STRUCTURE FOR UPDATE /DMO/C_Supplement\\Supplement,
      i_update TYPE STRUCTURE FOR UPDATE /DMO/I_Supplement\\Supplement.

    c_update = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   SupplementID          = 'XX123'
                   SupplementDescription = 'Test'
                   price                 = '42'
                   CurrencyCode          = 'EUR'
                 ).

    entity_double->text_rba_link = VALUE #( (
        source = VALUE #( %is_draft = c_update-%is_draft  SupplementID = c_update-SupplementID )
        target = VALUE #( %is_draft = c_update-%is_draft  SupplementID = c_update-SupplementID  LanguageCode = 'EN' )
      ) ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY Supplement
        UPDATE
          FIELDS ( SupplementDescription Price CurrencyCode )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.


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
                                        act = act_i_update-LanguageCode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-SupplementDescription
                                        act = act_i_update-Description ).
  ENDMETHOD.

  METHOD text_create.
    DATA:
      c_update TYPE STRUCTURE FOR UPDATE /DMO/C_Supplement\\Supplement,
      i_update TYPE STRUCTURE FOR UPDATE /DMO/I_Supplement\\Supplement.

    c_update = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   SupplementID          = 'XX123'
                   SupplementDescription = 'Test'
                 ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY Supplement
        UPDATE
          FIELDS ( SupplementDescription )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.


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
                                        act = act_i_cba-LanguageCode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-SupplementDescription
                                        act = act_i_cba-Description ).
  ENDMETHOD.

  METHOD text_update.
    DATA:
      c_update TYPE STRUCTURE FOR UPDATE /DMO/C_Supplement\\Supplement,
      i_update TYPE STRUCTURE FOR UPDATE /DMO/I_Supplement\\Supplement.

    c_update = VALUE #(
                   %is_draft             = if_abap_behv=>mk-on
                   SupplementID          = 'XX123'
                   SupplementDescription = 'Test'
                 ).

    entity_double->text_rba_link = VALUE #( (
        source = VALUE #( %is_draft = c_update-%is_draft  SupplementID = c_update-SupplementID )
        target = VALUE #( %is_draft = c_update-%is_draft  SupplementID = c_update-SupplementID  LanguageCode = 'EN' )
      ) ).

    MODIFY ENTITIES OF /dmo/c_supplement
      ENTITY Supplement
        UPDATE
          FIELDS ( SupplementDescription )
          WITH VALUE #( ( c_update ) )
      MAPPED    mapped
      FAILED    failed
      REPORTED  reported.


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
                                        act = act_i_update-LanguageCode ).
    cl_abap_unit_assert=>assert_equals( exp = c_update-SupplementDescription
                                        act = act_i_update-Description ).
  ENDMETHOD.

ENDCLASS.
