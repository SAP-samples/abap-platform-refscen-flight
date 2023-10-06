"! @testing BDEF:/DMO/I_Supplement
CLASS ltc_supplement DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_supplement,
      cds_test_environment TYPE REF TO if_cds_test_environment,
      sql_test_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    DATA:
      mapped        TYPE RESPONSE FOR MAPPED   EARLY /dmo/i_supplement,
      failed        TYPE RESPONSE FOR FAILED   EARLY /dmo/i_supplement,
      reported      TYPE RESPONSE FOR REPORTED EARLY /dmo/i_supplement,
      failed_late   TYPE RESPONSE FOR FAILED   LATE  /dmo/i_supplement,
      reported_late TYPE RESPONSE FOR REPORTED LATE  /dmo/i_supplement.


    METHODS:
      setup,
      teardown.

    METHODS:
      "! Checks if { @link ..lhc_Supplement.METH:validateprice } behaves correctly if an instance
      "! where Price and Currency is set.
      validate_price_success        FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:validateprice } behaves correctly if instances
      "! where Price and Currency is not set correctly.  This should result in filled reported and failed tables.
      "! All permutations are tested within this test.
      validate_price_failed         FOR TESTING RAISING cx_static_check,



      "! Checks if { @link ..lhc_Supplement.METH:earlynumbering_create } fulfills idempotency
      "! by passing an instance where the number is already drawn.
      earlynumbering_idempotency    FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:earlynumbering_create }
      "! draws a new number for instances where no number is drawn yet.
      earlynumbering_new_number     FOR TESTING RAISING cx_static_check,

      "! Checks if { @link ..lhc_Supplement.METH:earlynumbering_create }
      "! fails when no <em>Number Range Interval</em> exists for the applied <em>SupplementCategory</em>.
      earlynumbering_wrong_category FOR TESTING RAISING cx_static_check,


      "! Checks that { @link ..lhc_Supplement.METH:get_global_authorizations } returns initial values
      "! for <em>result</em> and <em>reported</em>.
      get_global_authorizations     FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_supplement IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.

    cds_test_environment = cl_cds_test_environment=>create(
        i_for_entity = '/DMO/I_SUPPLEMENT'
      ).

    sql_test_environment = cl_osql_test_environment=>create(
        VALUE #(
            ( 'I_CURRENCY' )
          )
      ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).

    CLEAR:
      mapped,
      failed,
      reported,
      failed_late,
      reported_late.


    DATA:
      currencies     TYPE STANDARD TABLE OF i_currency.

    currencies = VALUE #(
        ( currency = 'EUR' )
        ( currency = 'USD' )
      ).

    sql_test_environment->insert_test_data( currencies ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
    cds_test_environment->destroy( ).
    sql_test_environment->destroy( ).
  ENDMETHOD.

  METHOD validate_price_success.
    DATA:
      supplement  TYPE /dmo/supplement,
      supplements TYPE STANDARD TABLE OF /dmo/supplement.

    supplement = VALUE /dmo/supplement(
        supplement_id       = 'XX123'
        supplement_category = 'XX'
        price               = '42'
        currency_code       = 'EUR'
      ).

    supplements = VALUE #( ( supplement ) ).
    cds_test_environment->insert_test_data( supplements ).

    class_under_test->validateprice(
        EXPORTING
          keys     = VALUE #( ( %is_draft = if_abap_behv=>mk-off  supplementid = supplement-supplement_id ) )
        CHANGING
          failed   = failed_late
          reported = reported_late
      ).

    cl_abap_unit_assert=>assert_initial(     failed_late   ).
    cl_abap_unit_assert=>assert_not_initial( reported_late ).

    " One line is expected as this clears the state area.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has more than one messages'
                                        exp = 1
                                        act = lines( reported_late-supplement ) ).
    DATA(reported_supplement) = reported_late-supplement[ 1 ].

    "Check that only %tky and %state_area is filled
    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off      act = reported_supplement-%is_draft    ).
    cl_abap_unit_assert=>assert_equals( exp = supplement-supplement_id  act = reported_supplement-supplementid ).
    cl_abap_unit_assert=>assert_equals( exp = 'VALIDATE_PRICE'          act = reported_supplement-%state_area ).
    cl_abap_unit_assert=>assert_initial( reported_supplement-%op ).
    cl_abap_unit_assert=>assert_initial( reported_supplement-%msg ).
    cl_abap_unit_assert=>assert_initial( reported_supplement-%element ).
  ENDMETHOD.

  METHOD validate_price_failed.
    TYPES:
      BEGIN OF check_table,
        supplement_id    TYPE /dmo/supplement_id,
        clear_state_area TYPE abap_bool,
        message_price    TYPE symsgno,
        message_currency TYPE symsgno,
      END OF check_table.

    DATA:
      supplements     TYPE STANDARD TABLE OF /dmo/supplement WITH KEY supplement_id,
      exp_check_table TYPE STANDARD TABLE OF check_table     WITH KEY supplement_id,
      act_check_table TYPE STANDARD TABLE OF check_table     WITH KEY supplement_id.

    supplements = VALUE #(
        ( supplement_id = 'XX1'  supplement_category = 'XX'  price = '42'  currency_code = ''    )
        ( supplement_id = 'XX2'  supplement_category = 'XX'  price = ''    currency_code = 'EUR' )
        ( supplement_id = 'XX3'  supplement_category = 'XX'  price = ''    currency_code = ''    )
        ( supplement_id = 'XX4'  supplement_category = 'XX'  price = '42'  currency_code = 'XXX' )
        ( supplement_id = 'XX5'  supplement_category = 'XX'  price = ''    currency_code = 'XXX' )
      ).

    exp_check_table = VALUE #(
        clear_state_area = abap_true
        ( supplement_id = 'XX1'  message_price = '000'  message_currency = '002'  )
        ( supplement_id = 'XX2'  message_price = '001'  message_currency = '000'  )
        ( supplement_id = 'XX3'  message_price = '001'  message_currency = '002'  )
        ( supplement_id = 'XX4'  message_price = '000'  message_currency = '006'  )
        ( supplement_id = 'XX5'  message_price = '001'  message_currency = '006'  )
      ).

    act_check_table = VALUE #(
        clear_state_area = abap_false
        message_price    = ''
        message_currency = ''
        ( supplement_id = 'XX1' )
        ( supplement_id = 'XX2' )
        ( supplement_id = 'XX3' )
        ( supplement_id = 'XX4' )
        ( supplement_id = 'XX5' )
      ).

    cds_test_environment->insert_test_data( supplements ).

    class_under_test->validateprice(
        EXPORTING
          keys     = VALUE #( FOR suppl IN supplements (
                                 %is_draft = if_abap_behv=>mk-off
                                 supplementid = suppl-supplement_id ) )
        CHANGING
          failed   = failed_late
          reported = reported_late
      ).

    " check failed
    cl_abap_unit_assert=>assert_not_initial( failed_late ).
    cl_abap_unit_assert=>assert_equals( exp = REDUCE i(
                                                  INIT count = 0
                                                  FOR  exp IN exp_check_table
                                                  NEXT count +=   COND #( WHEN exp-message_price    is not initial THEN 1 ELSE 0 )
                                                                + COND #( WHEN exp-message_currency is not initial THEN 1 ELSE 0 )
                                                )
                                        act = lines( failed_late-supplement ) ).
    LOOP AT failed-supplement INTO DATA(failed_line).
      cl_abap_unit_assert=>assert_not_initial( msg = 'Failed key was not provided'
                                               act = VALUE #( supplements[ supplement_id = failed_line-supplementid ]
                                                              OPTIONAL ) ).
    ENDLOOP.


    "check reported
    cl_abap_unit_assert=>assert_not_initial( reported_late ).
    " One line for clearing the state area plus actual error message each.
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has not the correct amount of messages'
                                        exp = lines( exp_check_table )
                                              + REDUCE i(
                                                  INIT count = 0
                                                  FOR exp IN exp_check_table
                                                  NEXT count +=   COND #( WHEN exp-message_price    <> '000' THEN 1 ELSE 0 )
                                                                + COND #( WHEN exp-message_currency <> '000' THEN 1 ELSE 0 )
                                                )
                                        act = lines( reported_late-supplement ) ).

    LOOP AT reported_late-supplement INTO DATA(reported_line).
      cl_abap_unit_assert=>assert_not_initial( msg = 'Returned key was not in the provided data set!'
                                               act = VALUE #( supplements[ supplement_id = reported_line-supplementid  ] OPTIONAL ) ).
      IF reported_line-%msg IS BOUND.
        " actual message case
        IF reported_line-%element-price = if_abap_behv=>mk-on.
          "check if only once by looking up the check table
          cl_abap_unit_assert=>assert_equals( msg = 'This reported line already appeared'
                                              exp = '000'
                                              act = act_check_table[ supplement_id = reported_line-supplementid ]-message_price ).
          act_check_table[ supplement_id = reported_line-supplementid ]-message_price = reported_line-%msg->if_t100_message~t100key-msgno.
        ELSEIF reported_line-%element-currencycode = if_abap_behv=>mk-on.
          "check if only once by looking up the check table
          cl_abap_unit_assert=>assert_equals( msg = 'This reported line already appeared'
                                              exp = '000'
                                              act = act_check_table[ supplement_id = reported_line-supplementid ]-message_currency ).
          act_check_table[ supplement_id = reported_line-supplementid ]-message_currency = reported_line-%msg->if_t100_message~t100key-msgno.
        ELSE.
          cl_abap_unit_assert=>fail( 'Unexpected element!' ).
        ENDIF.

      ELSE.
        " clear state area case
        "check if only once by looking up the check table
        cl_abap_unit_assert=>assert_equals( msg = 'This reported line already appeared'
                                            exp = abap_false
                                            act = act_check_table[ supplement_id = reported_line-supplementid ]-clear_state_area ).
        act_check_table[ supplement_id = reported_line-supplementid ]-clear_state_area = abap_true.

        "Check that only %tky and %state_area is filled
        cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>mk-off      act = reported_line-%is_draft    ).
        cl_abap_unit_assert=>assert_equals( exp = 'VALIDATE_PRICE'          act = reported_line-%state_area ).
        cl_abap_unit_assert=>assert_initial( reported_line-%op ).
        cl_abap_unit_assert=>assert_initial( reported_line-%msg ).
        cl_abap_unit_assert=>assert_initial( reported_line-%element ).
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals( msg = 'Check Tables differ'
                                        exp = exp_check_table
                                        act = act_check_table ).
  ENDMETHOD.

  METHOD earlynumbering_idempotency.
    DATA:
      entity          TYPE STRUCTURE FOR CREATE /dmo/i_supplement\\supplement,
      act_mapped_line LIKE LINE OF mapped-supplement,
      exp_mapped_line LIKE LINE OF mapped-supplement.

    entity = VALUE #( %is_draft = if_abap_behv=>mk-off  %cid = 'Test'  supplementid = 'XX123' ).

    class_under_test->earlynumbering_create(
        EXPORTING
          entities = VALUE #( ( entity ) )
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     failed   ).
    cl_abap_unit_assert=>assert_initial(     reported ).
    cl_abap_unit_assert=>assert_not_initial( mapped   ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( mapped-supplement ) ).

    act_mapped_line = mapped-supplement[ 1 ].
    exp_mapped_line = CORRESPONDING #( entity ).

    cl_abap_unit_assert=>assert_equals( exp = exp_mapped_line  act = act_mapped_line ).
  ENDMETHOD.

  METHOD earlynumbering_new_number.
    TRY.
        cl_numberrange_intervals=>read(
          EXPORTING
                    nr_range_nr1       = '01'
                    subobject         = CONV #( 'BV' )
                    object            = '/DMO/SUPPL'
          IMPORTING
            interval     = DATA(interval)
        ).
      CATCH cx_nr_object_not_found.
        cl_abap_unit_assert=>skip( 'Number Range Object is missing.  Please run the data generator!' ).
      CATCH cx_nr_subobject.
        cl_abap_unit_assert=>skip( 'Number Range Subobject is missing.  Please run the data generator!' ).
      CATCH cx_number_ranges INTO DATA(number_range_exception).
        cl_abap_unit_assert=>skip( number_range_exception->get_text( ) ).
    ENDTRY.

    DATA:
      entity          TYPE STRUCTURE FOR CREATE /dmo/i_supplement\\supplement,
      act_mapped_line LIKE LINE OF mapped-supplement,
      exp_mapped_line LIKE LINE OF mapped-supplement.

    entity = VALUE #( %is_draft = if_abap_behv=>mk-off  %cid = 'Test'  supplementcategory = 'BV' ).

    class_under_test->earlynumbering_create(
        EXPORTING
          entities = VALUE #( ( entity ) )
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     failed   ).
    cl_abap_unit_assert=>assert_initial(     reported ).
    cl_abap_unit_assert=>assert_not_initial( mapped   ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( mapped-supplement ) ).

    act_mapped_line = mapped-supplement[ 1 ].
    cl_abap_unit_assert=>assert_not_initial( act_mapped_line-supplementid ).

    exp_mapped_line = CORRESPONDING #( entity ).
    exp_mapped_line-supplementid = act_mapped_line-supplementid.

    cl_abap_unit_assert=>assert_equals( exp = exp_mapped_line  act = act_mapped_line ).
  ENDMETHOD.

  METHOD earlynumbering_wrong_category.
    TRY.
        cl_numberrange_intervals=>read(
          EXPORTING
                    nr_range_nr1       = '01'
                    subobject         = CONV #( 'XX' )
                    object            = '/DMO/SUPPL'
          IMPORTING
            interval     = DATA(interval)
        ).
        cl_abap_unit_assert=>skip( 'Unexpected Number Range Interval found!' ).
      CATCH cx_nr_object_not_found cx_nr_subobject cx_number_ranges.
        "Expected
    ENDTRY.

    DATA:
      entity          TYPE STRUCTURE FOR CREATE /dmo/i_supplement\\supplement,
      act_mapped_line LIKE LINE OF mapped-supplement,
      exp_mapped_line LIKE LINE OF mapped-supplement.

    entity = VALUE #( %is_draft = if_abap_behv=>mk-off  %cid = 'Test'  supplementcategory = 'XX' ).

    class_under_test->earlynumbering_create(
        EXPORTING
          entities = VALUE #( ( entity ) )
        CHANGING
          mapped   = mapped
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( mapped ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'Failed has more than one message'
                                        exp = 1
                                        act = lines( failed-supplement ) ).

    DATA(failed_line) = failed-supplement[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = entity-%cid
                                        act = failed_line-%cid ).
    cl_abap_unit_assert=>assert_equals( exp = entity-%is_draft
                                        act = failed_line-%is_draft ).


    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals( msg = 'Reported has more than one message'
                                        exp = 1
                                        act = lines( reported-supplement ) ).

    DATA(reported_line) = reported-supplement[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = entity-%cid
                                        act = reported_line-%cid ).
    cl_abap_unit_assert=>assert_equals( exp = entity-%is_draft
                                        act = reported_line-%is_draft ).
  ENDMETHOD.

  METHOD get_global_authorizations.
    DATA:
      requested_authorizations TYPE STRUCTURE FOR GLOBAL AUTHORIZATION REQUEST /dmo/i_supplement\\supplement,
      result                   TYPE STRUCTURE FOR GLOBAL AUTHORIZATION RESULT /dmo/i_supplement\\supplement,
      reported                 TYPE RESPONSE  FOR REPORTED EARLY /dmo/i_supplement.

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
