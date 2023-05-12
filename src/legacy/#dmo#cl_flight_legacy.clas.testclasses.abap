CLASS ltcl_lock_travel DEFINITION DEFERRED.

CLASS /dmo/cl_flight_legacy DEFINITION LOCAL FRIENDS ltcl_lock_travel.

CLASS ltcl_lock_travel DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      got_lock FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_lock_travel IMPLEMENTATION.
  METHOD got_lock ##NEEDED.
  ENDMETHOD.
ENDCLASS.


CLASS ltc_travel DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PROTECTED SECTION.
    CONSTANTS mc_use_sql_doubles           TYPE abap_bool VALUE abap_false.
    CONSTANTS mc_msgid                     TYPE symsgid   VALUE '/DMO/CM_FLIGHT_LEGAC'.

* SQL doubles currently not available in cloud environment
*   CLASS-DATA mr_test_environment         TYPE REF TO if_osql_test_environment.

    CLASS-DATA gr_cut                      TYPE REF TO /dmo/cl_flight_legacy.

    CLASS-DATA gv_agency_id_1              TYPE /dmo/agency_id.
    CLASS-DATA gv_agency_id_2              TYPE /dmo/agency_id.
    CLASS-DATA gv_agency_id_unknown        TYPE /dmo/agency_id.

    CLASS-DATA gv_customer_id_1            TYPE /dmo/customer_id.
    CLASS-DATA gv_customer_id_2            TYPE /dmo/customer_id.
    CLASS-DATA gv_customer_id_unknown      TYPE /dmo/customer_id.

    METHODS _create_travel IMPORTING is_travel        TYPE /dmo/s_travel_in
                                     iv_save          TYPE abap_bool DEFAULT abap_true
                           RETURNING VALUE(rs_travel) TYPE /dmo/travel.
    METHODS _delete_existing_travel IMPORTING iv_travel_id TYPE /dmo/travel_id.

  PRIVATE SECTION.
    DATA mv_travel_count             TYPE i.
    DATA mv_booking_count            TYPE i.
    DATA mv_booking_supplement_count TYPE i.

    CLASS-METHODS class_setup.
    METHODS setup.
    METHODS teardown.
    CLASS-METHODS class_teardown.

    "! Create and check a single travel
    METHODS create                      FOR TESTING RAISING cx_static_check.
    "! Create and check a single travel in late numbering mode
    METHODS create_late_numbering       FOR TESTING RAISING cx_static_check.
    "! Try to create a travel with an unknown agency -> ERROR
    METHODS c_agency_unknown            FOR TESTING RAISING cx_static_check.
    "! Try to create a travel with an unknown customer -> ERROR
    METHODS c_customer_unknown          FOR TESTING RAISING cx_static_check.
    "! Create 2 travels in the same LUW
    METHODS create_mutiple_calls        FOR TESTING RAISING cx_static_check.

    "! Create a single travel
    METHODS create_single               FOR TESTING RAISING cx_static_check.
    "! Delete a single travel
    METHODS delete_single               FOR TESTING RAISING cx_static_check.
    "! Update a single travel
    METHODS update_single               FOR TESTING RAISING cx_static_check.
    "! Try to delete a single travel with unknown Travel ID -> ERROR
    METHODS d_travel_id_unknown         FOR TESTING RAISING cx_static_check.
    "! Try to update a single travel with unknown Travel ID -> ERROR
    METHODS u_travel_id_unknown         FOR TESTING RAISING cx_static_check.
    "! Delete multiple travels
    METHODS delete_multiple             FOR TESTING RAISING cx_static_check.
    "! Update multiple travels
    METHODS update_multiple             FOR TESTING RAISING cx_static_check.
    "! Update a single travel twice in the same LUW
    METHODS update_twice                FOR TESTING RAISING cx_static_check.
    "! Update, delete a single travel in the same LUW
    METHODS update_delete_single        FOR TESTING RAISING cx_static_check.
    "! Try to update a single travel with an unknown agency -> ERROR
    METHODS u_agency_unknown            FOR TESTING RAISING cx_static_check.
    "! Try to update a single travel with an unknown customer -> ERROR
    METHODS u_customer_unknown          FOR TESTING RAISING cx_static_check.
    "! Try to delete a single travel with an initial Travel ID -> ERROR
    METHODS d_travel_id_initial         FOR TESTING RAISING cx_static_check.
    "! Try to update a single travel with an initial Travel ID -> ERROR
    METHODS u_travel_id_initial         FOR TESTING RAISING cx_static_check.
    "! Call action to set booking status
    METHODS act_set_status_to_booked    FOR TESTING RAISING cx_static_check.
    "! Try to create a travel with faulty dates -> Error
    METHODS c_dates_invalid             FOR TESTING RAISING cx_static_check.
    "! Try to update a travel with faulty dates -> Error
    METHODS u_dates_invalid             FOR TESTING RAISING cx_static_check.
    "! Reset buffer
    METHODS initialize                  FOR TESTING RAISING cx_static_check.
    "! Create, update a single travel in the same LUW
    METHODS create_update_in_one_luw    FOR TESTING RAISING cx_static_check.
    "! Create, delete a single travel in the same LUW
    METHODS create_delete_in_one_luw    FOR TESTING RAISING cx_static_check.
    "! Update, delete a single travel in the same LUW
    METHODS update_delete_in_one_luw    FOR TESTING RAISING cx_static_check.
    "! Try to delete, update a single travel in the same LUW -> ERROR
    METHODS delete_update_in_one_luw    FOR TESTING RAISING cx_static_check.
    "! Delete, delete a single travel in the same LUW -> Error
    METHODS delete_delete_in_one_luw    FOR TESTING RAISING cx_static_check.
    "! Try to update a travel with no control structure -> ERROR
    METHODS u_no_control                FOR TESTING RAISING cx_static_check.
    "! Try to change the travel status to an invalid value -> ERROR
    METHODS u_status_invalid            FOR TESTING RAISING cx_static_check.
    "! Checks if a travel_id is valid otherwise return message
    METHODS check_travel_id             FOR TESTING RAISING cx_static_check.
    "! Checks if a travel_id is drawn in late numbering mode and skipped in early
    METHODS adjust_numbers              FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS /dmo/cl_flight_legacy DEFINITION LOCAL FRIENDS ltc_travel.

CLASS ltc_travel IMPLEMENTATION.
  METHOD class_setup.
    IF mc_use_sql_doubles = abap_true ##BOOL_OK.
*     mr_test_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( '/DMO/TRAVEL' ) ( '/DMO/BOOKING' ) ( '/DMO/BOOK_SUPPL' )
*                                                                                          ( '/DMO/AGENCY' ) ( '/DMO/CUSTOMER' ) ( '/DMO/FLIGHT' ) ( '/DMO/SUPPLEMENT' ) ) ).
*     mr_test_environment->clear_doubles( ).
*     gv_agency_id_1 = '42'.
*     gv_agency_id_2 = '43'.
*     DATA lt_agency TYPE STANDARD TABLE OF /dmo/agency.
*     lt_agency = VALUE #( ( agency_id = gv_agency_id_1 ) ( agency_id = gv_agency_id_2 ) ).
*     mr_test_environment->insert_test_data( lt_agency ).
*
*     gv_customer_id_1 = '42'.
*     gv_customer_id_2 = '43'.
*     DATA lt_customer  TYPE STANDARD TABLE OF /dmo/customer.
*     lt_customer = VALUE #( ( customer_id = gv_customer_id_1 ) ( customer_id = gv_customer_id_2 ) ).
*     mr_test_environment->insert_test_data( lt_customer ).
*
*     gv_agency_id_unknown   = '99'.
*     gv_customer_id_unknown = '99'.
    ELSE.
      DATA lt_agency_id TYPE SORTED TABLE OF /dmo/agency_id     WITH UNIQUE KEY table_line.
      SELECT DISTINCT agency_id FROM /dmo/agency     ORDER BY agency_id   DESCENDING INTO TABLE @lt_agency_id . "#EC CI_NOWHERE

      DATA lt_customer_id TYPE SORTED TABLE OF /dmo/customer_id WITH UNIQUE KEY table_line.
      SELECT DISTINCT customer_id FROM /dmo/customer ORDER BY customer_id DESCENDING INTO TABLE @lt_customer_id . "#EC CI_NOWHERE

      " Select 2 known agency IDs
      IF lines( lt_agency_id ) < 2.
        cl_abap_unit_assert=>abort( msg = 'No agency data!'   ).
      ENDIF.
      gv_agency_id_1 = lt_agency_id[ 1 ].
      gv_agency_id_2 = lt_agency_id[ 2 ].
      cl_abap_unit_assert=>assert_differs( act = gv_agency_id_1  exp = gv_agency_id_2 )." To be totally sure

      " Select 2 known customer IDs
      IF lines( lt_customer_id ) < 2.
        cl_abap_unit_assert=>abort( msg = 'No customer data!' ).
      ENDIF.
      gv_customer_id_1 = lt_customer_id[ 1 ].
      gv_customer_id_2 = lt_customer_id[ 2 ].
      cl_abap_unit_assert=>assert_differs( act = gv_customer_id_1  exp = gv_customer_id_2 )." To be totally sure

      " Determine an unknown agency ID
      gv_agency_id_unknown = lt_agency_id[ 1 ].
      DO.
        gv_agency_id_unknown = gv_agency_id_unknown + 1.
        READ TABLE lt_agency_id   TRANSPORTING NO FIELDS WITH TABLE KEY table_line = gv_agency_id_unknown.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
      ENDDO.

      " Determine an unknown customer ID
      gv_customer_id_unknown = lt_customer_id[ 1 ].
      DO.
        gv_customer_id_unknown = gv_customer_id_unknown + 1.
        READ TABLE lt_customer_id TRANSPORTING NO FIELDS WITH TABLE KEY table_line = gv_customer_id_unknown.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

    gr_cut = NEW #( ).
  ENDMETHOD.


  METHOD setup.
    SELECT COUNT( * ) FROM /dmo/travel     INTO @mv_travel_count. "#EC CI_NOWHERE
    SELECT COUNT( * ) FROM /dmo/booking    INTO @mv_booking_count. "#EC CI_NOWHERE
    SELECT COUNT( * ) FROM /dmo/book_suppl INTO @mv_booking_supplement_count. "#EC CI_NOWHERE
  ENDMETHOD.


  METHOD teardown.
    " Ensure proper cleanup of each individual test method
    SELECT COUNT( * ) FROM /dmo/travel INTO @DATA(lv_travel_count). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_equals( act = lv_travel_count  exp = mv_travel_count ).
    SELECT COUNT( * ) FROM /dmo/booking INTO @DATA(lv_booking_count). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_equals( act = lv_booking_count  exp = mv_booking_count ).
    SELECT COUNT( * ) FROM /dmo/book_suppl INTO @DATA(lv_booking_supplement_count). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_equals( act = lv_booking_supplement_count  exp = mv_booking_supplement_count ).
  ENDMETHOD.


  METHOD class_teardown.
    IF mc_use_sql_doubles = abap_true ##BOOL_OK ##NEEDED.
*     mr_test_environment->destroy( ).
    ENDIF.
  ENDMETHOD.


  METHOD create.
    SELECT COUNT( * ) FROM /dmo/travel INTO @DATA(lv_count1). "#EC CI_NOWHERE

    DATA lv_start TYPE timestampl.
    GET TIME STAMP FIELD lv_start.

    gr_cut->create_travel( EXPORTING is_travel   = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2   begin_date = '20190101'  end_date = '20190201' )
                           IMPORTING es_travel   = DATA(ls_travel_new)
                                     et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel_new-travel_id ).
    gr_cut->save( ).

    DATA lv_end TYPE timestampl.
    GET TIME STAMP FIELD lv_end.

    SELECT COUNT( * ) FROM /dmo/travel INTO @DATA(lv_count2). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_equals( msg = 'create should add a travel'  exp = 1  act = lv_count2 - lv_count1 ).

    SELECT FROM /dmo/travel FIELDS createdby, createdat, status WHERE travel_id = @ls_travel_new-travel_id INTO TABLE @DATA(lt_travel).
    cl_abap_unit_assert=>assert_equals( msg = 'cannot read created travel' exp = 1 act = lines(  lt_travel ) ).
    DATA(ls_travel) = lt_travel[ 1 ].

    cl_abap_unit_assert=>assert_equals( msg = 'createdby' exp = ls_travel-createdby act = sy-uname ).
    cl_abap_unit_assert=>assert_number_between( msg = 'createdat' number = ls_travel-createdat lower = lv_start upper = lv_end ).

    cl_abap_unit_assert=>assert_equals( msg = 'status' act = ls_travel-status exp = CONV /dmo/travel_status( /dmo/if_flight_legacy=>travel_status-new ) ).

    _delete_existing_travel( ls_travel_new-travel_id ).
  ENDMETHOD.


  METHOD create_late_numbering.
    SELECT COUNT( * ) FROM /dmo/travel INTO @DATA(lv_count1). "#EC CI_NOWHERE

    DATA lv_start TYPE timestampl.
    GET TIME STAMP FIELD lv_start.

    gr_cut->create_travel( EXPORTING is_travel         = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2   begin_date = '20190101'  end_date = '20190201' )
                                     iv_numbering_mode = /dmo/if_flight_legacy=>numbering_mode-late
                           IMPORTING es_travel         = DATA(ls_travel_new)
                                     et_messages       = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel_new-travel_id ).
    gr_cut->adjust_numbers(
        IMPORTING
          et_travel_mapping       = DATA(lt_travel_mapping)
          et_booking_mapping      = DATA(lt_booking_mapping)
          et_bookingsuppl_mapping = DATA(lt_bookingsuppl_mapping)
      ).
    cl_abap_unit_assert=>assert_not_initial( lt_travel_mapping       ).
    cl_abap_unit_assert=>assert_initial(     lt_booking_mapping      ).
    cl_abap_unit_assert=>assert_initial(     lt_bookingsuppl_mapping ).
    gr_cut->save( ).

    DATA(lv_travel_id_final) = lt_travel_mapping[ 1 ]-final-travel_id.

    DATA lv_end TYPE timestampl.
    GET TIME STAMP FIELD lv_end.

    SELECT COUNT( * ) FROM /dmo/travel INTO @DATA(lv_count2). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_equals( msg = 'create should add a travel'  exp = 1  act = lv_count2 - lv_count1 ).

    SELECT FROM /dmo/travel FIELDS createdby, createdat, status WHERE travel_id = @lv_travel_id_final INTO TABLE @DATA(lt_travel).
    cl_abap_unit_assert=>assert_equals( msg = 'cannot read created travel' exp = 1 act = lines(  lt_travel ) ).
    DATA(ls_travel) = lt_travel[ 1 ].

    cl_abap_unit_assert=>assert_equals( msg = 'createdby' exp = ls_travel-createdby act = sy-uname ).
    cl_abap_unit_assert=>assert_number_between( msg = 'createdat' number = ls_travel-createdat lower = lv_start upper = lv_end ).

    cl_abap_unit_assert=>assert_equals( msg = 'status' act = ls_travel-status exp = CONV /dmo/travel_status( /dmo/if_flight_legacy=>travel_status-new ) ).

    _delete_existing_travel( lv_travel_id_final ).

    ROLLBACK WORK.                                     "#EC CI_ROLLBACK
  ENDMETHOD.


  METHOD create_mutiple_calls.
    DATA lv_start TYPE timestampl.
    GET TIME STAMP FIELD lv_start.

    gr_cut->create_travel( EXPORTING is_travel             = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2   begin_date = '20190101'  end_date = '20190201' )
                           IMPORTING es_travel             = DATA(ls_travel_1)
                                     et_messages           = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel_1-travel_id ).

    gr_cut->create_travel( EXPORTING is_travel             = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201' )
                           IMPORTING es_travel             = DATA(ls_travel_2)
                                     et_messages           = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel_2-travel_id ).

    cl_abap_unit_assert=>assert_false( xsdbool( ls_travel_1-travel_id = ls_travel_2-travel_id ) ).

    gr_cut->save( ).

    DATA lv_end TYPE timestampl.
    GET TIME STAMP FIELD lv_end.

    SELECT FROM /dmo/travel FIELDS createdby, createdat, status WHERE travel_id = @ls_travel_1-travel_id OR travel_id = @ls_travel_2-travel_id INTO TABLE @DATA(lt_travel) ##SELECT_FAE_WITH_LOB[DESCRIPTION].
    cl_abap_unit_assert=>assert_equals( msg = 'cannot read created travel'  exp = 2  act = lines( lt_travel ) ).

    DATA(ls_travel) = lt_travel[ 1 ].                   "#EC CI_NOORDER

    cl_abap_unit_assert=>assert_equals( msg = 'createdby' exp = ls_travel-createdby act = sy-uname ).
    cl_abap_unit_assert=>assert_number_between( msg = 'createdat' number = ls_travel-createdat lower = lv_start upper = lv_end ).

    cl_abap_unit_assert=>assert_equals( msg = 'status' act = ls_travel-status exp = CONV /dmo/travel_status( /dmo/if_flight_legacy=>travel_status-new ) ).

    _delete_existing_travel( ls_travel_1-travel_id ).
    _delete_existing_travel( ls_travel_2-travel_id ).
  ENDMETHOD.


  METHOD c_agency_unknown.
    SELECT COUNT( * ) FROM /dmo/travel INTO @DATA(lv_count1). "#EC CI_NOWHERE

    gr_cut->create_travel( EXPORTING is_travel   = VALUE #( agency_id = gv_agency_id_unknown  customer_id = gv_customer_id_2 )
                           IMPORTING es_travel   = DATA(ls_travel)
                                     et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( ls_travel-travel_id ).

    gr_cut->save( ).

    SELECT COUNT( * ) FROM /dmo/travel INTO @DATA(lv_count2). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_equals( msg = 'should not create bad travel'  exp = lv_count1  act = lv_count2 ).

    DATA lv_msg_found TYPE abap_bool.
    LOOP AT lt_messages INTO DATA(lr_message) ##INTO_OK.
      IF lr_message->t100key = /dmo/cx_flight_legacy=>agency_unkown.
        lv_msg_found = abap_true.
      ENDIF.
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals( msg = 'missing error' exp = abap_true act = lv_msg_found ).
  ENDMETHOD.


  METHOD c_customer_unknown.
    SELECT COUNT( * ) FROM /dmo/travel INTO @DATA(lv_count1). "#EC CI_NOWHERE

    gr_cut->create_travel( EXPORTING is_travel   = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_unknown )
                           IMPORTING es_travel   = DATA(ls_travel)
                                     et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( ls_travel-travel_id ).

    gr_cut->save( ).

    SELECT COUNT( * ) FROM /dmo/travel INTO @DATA(lv_count2). "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_equals( msg = 'should not create bad travel'  exp = lv_count1  act = lv_count2 ).

    DATA lv_msg_found TYPE abap_bool.
    LOOP AT lt_messages INTO DATA(lr_message) ##INTO_OK.
      IF lr_message->t100key = /dmo/cx_flight_legacy=>customer_unkown.
        lv_msg_found = abap_true.
      ENDIF.
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals( msg = 'missing error' exp = abap_true act = lv_msg_found ).
  ENDMETHOD.


  METHOD create_single.
    DATA lv_timestampl TYPE timestampl.
    GET TIME STAMP FIELD lv_timestampl.

    SELECT MAX( travel_id ) FROM /dmo/travel INTO @DATA(lv_travel_id_max). "#EC CI_NOWHERE
    DATA lv_travel_id_1 TYPE /dmo/travel_id.
    DATA lv_travel_id_2 TYPE /dmo/travel_id.
*    lv_travel_id_1 = lv_travel_id_max + 1.
*    IF lv_travel_id_1 IS INITIAL.
*      cl_abap_unit_assert=>abort( msg = 'Travel ID overflow!' ).
*    ENDIF.
*    lv_travel_id_2 = lv_travel_id_max + 2.
*    IF lv_travel_id_2 IS INITIAL.
*      cl_abap_unit_assert=>abort( msg = 'Travel ID overflow!' ).
*    ENDIF.
*
*    " Create a travel
*    cl_abap_unit_assert=>assert_equals( act = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'
*                                                                       status = /dmo/if_flight_legacy=>travel_status-booked ) )-travel_id  exp = lv_travel_id_1 ).
*
*    " Create a second travel
*    cl_abap_unit_assert=>assert_equals( act = _create_travel( VALUE #( agency_id = gv_agency_id_2  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201'
*                                                                       status = /dmo/if_flight_legacy=>travel_status-booked ) )-travel_id  exp = lv_travel_id_2 ).

*   The Travel ID is now calculated by a number range object - so we can't predict the next number to be used ...
    lv_travel_id_1 = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'
                                              status = /dmo/if_flight_legacy=>travel_status-booked ) )-travel_id.

    lv_travel_id_2 = _create_travel( VALUE #( agency_id = gv_agency_id_2  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201'
                                              status = /dmo/if_flight_legacy=>travel_status-booked ) )-travel_id.

    " Select and check the second travel
    DATA ls_travel_sel TYPE /dmo/travel.
    SELECT SINGLE * FROM /dmo/travel WHERE travel_id = @lv_travel_id_2 INTO @ls_travel_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-agency_id      exp = gv_agency_id_2 ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-customer_id    exp = gv_customer_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-status         exp = CONV /dmo/travel_status( /dmo/if_flight_legacy=>travel_status-new ) )." Provided status overridden by determination
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-createdby      exp = sy-uname ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-lastchangedby  exp = sy-uname ).
    cl_abap_unit_assert=>assert_true( xsdbool( ls_travel_sel-createdat = ls_travel_sel-lastchangedat ) ).
    DATA(lv_diff) = CONV i( cl_abap_tstmp=>subtract( tstmp1 = ls_travel_sel-createdat  tstmp2 = lv_timestampl ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( 0 <= lv_diff AND lv_diff <= 1 ) ).

    _delete_existing_travel( lv_travel_id_1 ).
    _delete_existing_travel( lv_travel_id_2 ).
  ENDMETHOD.


  METHOD delete_single.
    DATA lv_db_exists TYPE abap_bool.

    DATA(ls_travel_new) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Description' ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_new-description  exp = 'My_Description' ).

    SELECT SINGLE description FROM /dmo/travel WHERE travel_id = @ls_travel_new-travel_id INTO @DATA(lv_description).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_description  exp = ls_travel_new-description ).

    _delete_existing_travel( ls_travel_new-travel_id ).

    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/travel FIELDS @abap_true WHERE travel_id = @ls_travel_new-travel_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD update_single.
    DATA lv_timestampl TYPE timestampl.
    GET TIME STAMP FIELD lv_timestampl.

    DATA(ls_travel_new) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Text' ) ).

    DATA ls_travel_sel TYPE /dmo/travel.
    SELECT SINGLE description FROM /dmo/travel WHERE travel_id = @ls_travel_new-travel_id INTO CORRESPONDING FIELDS OF @ls_travel_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-description  exp = ls_travel_new-description ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel_new-travel_id  agency_id = gv_agency_id_unknown  description = 'My_New_Text' )
                                     is_travelx  = VALUE #( travel_id = ls_travel_new-travel_id  description = abap_true  )
                           IMPORTING es_travel   = DATA(ls_travel_updated)
                                     et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_equals(  act = ls_travel_updated-travel_id    exp = ls_travel_new-travel_id ).
    cl_abap_unit_assert=>assert_equals(  act = ls_travel_updated-description  exp = 'My_New_Text' ).
    gr_cut->save( ).

    CLEAR ls_travel_sel.
    SELECT SINGLE * FROM /dmo/travel WHERE travel_id = @ls_travel_new-travel_id INTO @ls_travel_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-agency_id      exp = ls_travel_new-agency_id   ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-customer_id    exp = ls_travel_new-customer_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-description    exp = 'My_New_Text' ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-lastchangedby  exp = sy-uname ).
    cl_abap_unit_assert=>assert_differs( act = ls_travel_sel-createdat  exp = ls_travel_sel-lastchangedat ).
    DATA(lv_diff) = cl_abap_tstmp=>subtract( tstmp1 = ls_travel_sel-lastchangedat  tstmp2 = lv_timestampl ).
    cl_abap_unit_assert=>assert_true( xsdbool( 0 < lv_diff AND lv_diff < 1 ) ).

    _delete_existing_travel( ls_travel_new-travel_id ).
  ENDMETHOD.


  METHOD d_travel_id_unknown.
    DATA(ls_travel_deleted) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201' ) ).
    _delete_existing_travel( ls_travel_deleted-travel_id ).
    gr_cut->delete_travel( EXPORTING iv_travel_id = ls_travel_deleted-travel_id
                           IMPORTING et_messages  = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id  exp = ls_travel_deleted-travel_id ).
  ENDMETHOD.


  METHOD u_travel_id_unknown.
    DATA(ls_travel_deleted) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Old_Text' ) ).
    _delete_existing_travel( ls_travel_deleted-travel_id ).
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel_deleted-travel_id  description = 'My_New_Text' )
                                     is_travelx  = VALUE #( travel_id = ls_travel_deleted-travel_id  description = abap_true )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id  exp = ls_travel_deleted-travel_id ).
  ENDMETHOD.


  METHOD delete_multiple.
    DATA lv_db_exists TYPE abap_bool.

    DATA(ls_travel_new_1) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Text_1' ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_new_1-description  exp = 'My_Text_1' ).
    DATA(ls_travel_new_2) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Text_2' ) ).
    DATA(ls_travel_new_3) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Text_3' ) ).

    " Delete - only in buffer
    gr_cut->delete_travel( EXPORTING iv_travel_id = ls_travel_new_1-travel_id
                           IMPORTING et_messages  = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->delete_travel( EXPORTING iv_travel_id = ls_travel_new_2-travel_id
                           IMPORTING et_messages  = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->delete_travel( EXPORTING iv_travel_id = ls_travel_new_3-travel_id
                           IMPORTING et_messages  = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    DATA lv_description TYPE /dmo/description.

    SELECT SINGLE description FROM /dmo/travel WHERE travel_id = @ls_travel_new_1-travel_id INTO @lv_description.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_description  exp = ls_travel_new_1-description ).

    SELECT SINGLE description FROM /dmo/travel WHERE travel_id = @ls_travel_new_2-travel_id INTO @lv_description.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_description  exp = ls_travel_new_2-description ).

    SELECT SINGLE description FROM /dmo/travel WHERE travel_id = @ls_travel_new_3-travel_id INTO @lv_description.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_description  exp = ls_travel_new_3-description ).

    " Now persist the buffer
    gr_cut->save( ).

    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/travel FIELDS @abap_true WHERE travel_id IN ( @ls_travel_new_1-travel_id, @ls_travel_new_2-travel_id, @ls_travel_new_3-travel_id ) INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD update_multiple.
    DATA(ls_travel_new_1) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Text_1' ) ).
    DATA(ls_travel_new_2) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Text_2' ) ).
    DATA(ls_travel_new_3) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Text_3' ) ).

    DATA lv_description TYPE /dmo/description.

    SELECT SINGLE description FROM /dmo/travel WHERE travel_id = @ls_travel_new_1-travel_id INTO @lv_description.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_description  exp = ls_travel_new_1-description ).

    SELECT SINGLE description FROM /dmo/travel WHERE travel_id = @ls_travel_new_2-travel_id INTO @lv_description.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_description  exp = ls_travel_new_2-description ).

    SELECT SINGLE description FROM /dmo/travel WHERE travel_id = @ls_travel_new_3-travel_id INTO @lv_description.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_description  exp = ls_travel_new_3-description ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel_new_1-travel_id  agency_id = gv_agency_id_unknown  description = 'My_New_Text_1' )
                                     is_travelx  = VALUE #( travel_id = ls_travel_new_1-travel_id  description = abap_true )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel_new_2-travel_id  agency_id = gv_agency_id_unknown  description = 'My_New_Text_2' )
                                     is_travelx  = VALUE #( travel_id = ls_travel_new_2-travel_id  description = abap_true )
                           IMPORTING et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel_new_3-travel_id  agency_id = gv_agency_id_2        description = 'My_New_Text_3' )
                                     is_travelx  = VALUE #( travel_id = ls_travel_new_3-travel_id  agency_id = abap_true )
                           IMPORTING et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->save( ).

    DATA ls_travel_sel TYPE /dmo/travel.

    CLEAR ls_travel_sel.
    SELECT SINGLE agency_id, customer_id, description FROM /dmo/travel WHERE travel_id = @ls_travel_new_1-travel_id INTO CORRESPONDING FIELDS OF @ls_travel_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-agency_id    exp = ls_travel_new_1-agency_id   ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-customer_id  exp = ls_travel_new_1-customer_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-description  exp = 'My_New_Text_1' ).

    CLEAR ls_travel_sel.
    SELECT SINGLE agency_id, customer_id, description FROM /dmo/travel WHERE travel_id = @ls_travel_new_2-travel_id INTO CORRESPONDING FIELDS OF @ls_travel_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-agency_id    exp = ls_travel_new_2-agency_id   ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-customer_id  exp = ls_travel_new_2-customer_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-description  exp = 'My_New_Text_2' ).

    CLEAR ls_travel_sel.
    SELECT SINGLE agency_id, customer_id, description FROM /dmo/travel WHERE travel_id = @ls_travel_new_3-travel_id INTO CORRESPONDING FIELDS OF @ls_travel_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-agency_id    exp = gv_agency_id_2   ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-customer_id  exp = ls_travel_new_3-customer_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-description  exp = 'My_Text_3' ).

    _delete_existing_travel( ls_travel_new_1-travel_id ).
    _delete_existing_travel( ls_travel_new_2-travel_id ).
    _delete_existing_travel( ls_travel_new_3-travel_id ).
  ENDMETHOD.


  METHOD update_twice.
    DATA(ls_travel_new) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Text' ) ).

    DATA ls_travel_sel TYPE /dmo/travel.
    SELECT SINGLE description FROM /dmo/travel WHERE travel_id = @ls_travel_new-travel_id INTO CORRESPONDING FIELDS OF @ls_travel_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-description  exp = ls_travel_new-description ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel_new-travel_id  agency_id = gv_agency_id_unknown  description = 'My_New_Text' )
                                     is_travelx  = VALUE #( travel_id = ls_travel_new-travel_id  description = abap_true )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel_new-travel_id  agency_id = gv_agency_id_2        description = 'WHATEVER' )
                                     is_travelx  = VALUE #( travel_id = ls_travel_new-travel_id  agency_id = abap_true )
                           IMPORTING et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->save( ).

    CLEAR ls_travel_sel.
    SELECT SINGLE agency_id, customer_id, description FROM /dmo/travel WHERE travel_id = @ls_travel_new-travel_id INTO CORRESPONDING FIELDS OF @ls_travel_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-agency_id    exp = gv_agency_id_2 ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-customer_id  exp = ls_travel_new-customer_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-description  exp = 'My_New_Text' ).

    _delete_existing_travel( ls_travel_new-travel_id ).
  ENDMETHOD.


  METHOD update_delete_single.
    DATA lv_db_exists TYPE abap_bool.

    DATA(ls_travel_new) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Text' ) ).

    SELECT SINGLE description FROM /dmo/travel WHERE travel_id = @ls_travel_new-travel_id INTO @DATA(lv_description).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_description  exp = ls_travel_new-description ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel_new-travel_id  agency_id = gv_agency_id_unknown  description = 'My_New_Text' )
                                     is_travelx  = VALUE #( travel_id = ls_travel_new-travel_id  description = abap_true )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->delete_travel( EXPORTING iv_travel_id = ls_travel_new-travel_id
                           IMPORTING et_messages  = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->save( ).

    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/travel FIELDS @abap_true WHERE travel_id = @ls_travel_new-travel_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD u_agency_unknown.
    DATA(ls_travel) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' ) ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel-travel_id  agency_id = gv_agency_id_unknown )
                                     is_travelx  = VALUE #( travel_id = ls_travel-travel_id  agency_id = abap_true )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>agency_unkown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_AGENCY_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_agency_id                   exp = gv_agency_id_unknown ).

    _delete_existing_travel( ls_travel-travel_id ).
  ENDMETHOD.


  METHOD u_customer_unknown.
    DATA(ls_travel) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' ) ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel-travel_id  customer_id = gv_customer_id_unknown )
                                     is_travelx  = VALUE #( travel_id = ls_travel-travel_id  customer_id = abap_true )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>customer_unkown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_CUSTOMER_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_customer_id                 exp = gv_customer_id_unknown ).

    _delete_existing_travel( ls_travel-travel_id ).
  ENDMETHOD.


  METHOD d_travel_id_initial.
    gr_cut->delete_travel( EXPORTING iv_travel_id = '0'
                           IMPORTING et_messages  = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_no_key-msgno ).
  ENDMETHOD.


  METHOD u_travel_id_initial.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( )
                                     is_travelx  = VALUE #( )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_no_key-msgno ).
  ENDMETHOD.


  METHOD act_set_status_to_booked.
    DATA lv_timestampl TYPE timestampl.
    GET TIME STAMP FIELD lv_timestampl.

    " Case A: Known travel ID
    DATA(ls_travel_new) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201' ) ).

    DATA ls_travel_sel TYPE /dmo/travel.
    SELECT SINGLE status FROM /dmo/travel WHERE travel_id = @ls_travel_new-travel_id INTO CORRESPONDING FIELDS OF @ls_travel_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-status  exp = CONV /dmo/travel_status( /dmo/if_flight_legacy=>travel_status-new ) ).

    gr_cut->set_status_to_booked( EXPORTING iv_travel_id = ls_travel_new-travel_id
                                  IMPORTING et_messages  = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->save( ).

    CLEAR ls_travel_sel.
    SELECT SINGLE status, createdat, lastchangedby, lastchangedat FROM /dmo/travel WHERE travel_id = @ls_travel_new-travel_id INTO CORRESPONDING FIELDS OF @ls_travel_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-status  exp = CONV /dmo/travel_status( /dmo/if_flight_legacy=>travel_status-booked ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-lastchangedby  exp = sy-uname ).
    cl_abap_unit_assert=>assert_differs( act = ls_travel_sel-createdat  exp = ls_travel_sel-lastchangedat ).
    DATA(lv_diff) = cl_abap_tstmp=>subtract( tstmp1 = ls_travel_sel-lastchangedat  tstmp2 = lv_timestampl ).
    cl_abap_unit_assert=>assert_true( xsdbool( 0 < lv_diff AND lv_diff < 1 ) ).

    " Case B: Initial travel ID
    gr_cut->set_status_to_booked( EXPORTING iv_travel_id = '0'
                                  IMPORTING et_messages  = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_no_key-msgno ).

    " Case C: Unknown travel ID
    gr_cut->delete_travel( EXPORTING iv_travel_id = ls_travel_new-travel_id
                           IMPORTING et_messages  = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    " -- Deletion only in buffer
    gr_cut->set_status_to_booked( EXPORTING iv_travel_id = ls_travel_new-travel_id
                                  IMPORTING et_messages  = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id  exp = ls_travel_new-travel_id ).

    gr_cut->save( ).

    " -- Deletion send to DB
    gr_cut->set_status_to_booked( EXPORTING iv_travel_id = ls_travel_new-travel_id
                                  IMPORTING et_messages  = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id  exp = ls_travel_new-travel_id ).
  ENDMETHOD.


  METHOD c_dates_invalid.
    " Try with initial begin date
    gr_cut->create_travel( EXPORTING is_travel   = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  end_date = '20190101' )
                              IMPORTING es_travel   = DATA(ls_travel)
                                        et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( ls_travel-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>no_begin_date-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).

    " Try with initial end date
    gr_cut->create_travel( EXPORTING is_travel   = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101' )
                              IMPORTING es_travel   = ls_travel
                                        et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_initial( ls_travel-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>no_end_date-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).

    " Try to have begin date after end date
    gr_cut->create_travel( EXPORTING is_travel   = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190201'  end_date = '20190101' )
                              IMPORTING es_travel   = ls_travel
                                        et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_initial( ls_travel-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>end_date_before_begin_date-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_BEGIN_DATE' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_END_DATE' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr3  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_begin_date                  exp = '20190201' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_end_date                    exp = '20190101' ).
  ENDMETHOD.


  METHOD u_dates_invalid.
    DATA(ls_travel) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' ) ).

    " Try to clear begin date
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel-travel_id )
                                     is_travelx  = VALUE #( travel_id = ls_travel-travel_id  begin_date = abap_true )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>no_begin_date-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = ls_travel-travel_id ).

    " Try to clear end date
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel-travel_id )
                                     is_travelx  = VALUE #( travel_id = ls_travel-travel_id  end_date = abap_true )
                           IMPORTING et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>no_end_date-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = ls_travel-travel_id ).

    " Try to have begin date after end date
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel-travel_id  end_date = '20181201' )
                                     is_travelx  = VALUE #( travel_id = ls_travel-travel_id  end_date = abap_true )
                           IMPORTING et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>end_date_before_begin_date-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_BEGIN_DATE' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_END_DATE' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr3  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_begin_date                  exp = ls_travel-begin_date ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_end_date                    exp = '20181201' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = ls_travel-travel_id ).

    _delete_existing_travel( ls_travel-travel_id ).
  ENDMETHOD.


  METHOD initialize.
    DATA(ls_travel_new) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Old_Text' ) ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel_new-travel_id  description = 'My_New_Text' )
                                     is_travelx  = VALUE #( travel_id = ls_travel_new-travel_id  description = abap_true )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->initialize( ).
    gr_cut->save( ).

    SELECT SINGLE description FROM /dmo/travel WHERE travel_id = @ls_travel_new-travel_id INTO @DATA(lv_description).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_description  exp = 'My_Old_Text' ).

    _delete_existing_travel( ls_travel_new-travel_id ).
  ENDMETHOD.


  METHOD create_update_in_one_luw.
    gr_cut->create_travel( EXPORTING is_travel   = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Old_Text' )
                           IMPORTING es_travel   = DATA(ls_travel)
                                     et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel-travel_id ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel-travel_id  agency_id = gv_agency_id_2  description = 'My_New_Text' )
                                     is_travelx  = VALUE #( travel_id = ls_travel-travel_id  description = abap_true )
                           IMPORTING et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->save( ).

    SELECT SINGLE agency_id, description FROM /dmo/travel WHERE travel_id = @ls_travel-travel_id INTO ( @DATA(lv_agency_id), @DATA(lv_description) ).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_agency_id    exp = gv_agency_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_description  exp = 'My_New_Text' ).

    _delete_existing_travel( ls_travel-travel_id ).
  ENDMETHOD.


  METHOD create_delete_in_one_luw.
    DATA lv_db_exits TYPE abap_bool.

    gr_cut->create_travel( EXPORTING is_travel   = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Old_Text' )
                           IMPORTING es_travel   = DATA(ls_travel)
                                     et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel-travel_id ).

    gr_cut->delete_travel( EXPORTING iv_travel_id = ls_travel-travel_id
                           IMPORTING et_messages  = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->save( ).

    CLEAR lv_db_exits.
    SELECT SINGLE FROM /dmo/travel FIELDS @abap_true WHERE travel_id = @ls_travel-travel_id INTO @lv_db_exits.
    cl_abap_unit_assert=>assert_false( lv_db_exits ).
  ENDMETHOD.


  METHOD update_delete_in_one_luw.
    DATA lv_db_exits TYPE abap_bool.

    DATA(ls_travel_new) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Old_Text' ) ).
    DATA(lv_travel_id) = ls_travel_new-travel_id.

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = lv_travel_id  agency_id = gv_agency_id_2  description = 'My_New_Text' )
                                     is_travelx  = VALUE #( travel_id = lv_travel_id  description = abap_true )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->delete_travel( EXPORTING iv_travel_id = lv_travel_id
                           IMPORTING et_messages  = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->save( ).

    CLEAR lv_db_exits.
    SELECT SINGLE FROM /dmo/travel FIELDS @abap_true WHERE travel_id = @lv_travel_id INTO @lv_db_exits.
    cl_abap_unit_assert=>assert_false( lv_db_exits ).
  ENDMETHOD.


  METHOD delete_update_in_one_luw.
    DATA lv_db_exits TYPE abap_bool.

    DATA(ls_travel_new) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Old_Text' ) ).
    DATA(lv_travel_id) = ls_travel_new-travel_id.

    gr_cut->delete_travel( EXPORTING iv_travel_id = lv_travel_id
                           IMPORTING et_messages  = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = lv_travel_id  agency_id = gv_agency_id_2  description = 'My_New_Text' )
                                     is_travelx  = VALUE #( travel_id = lv_travel_id  description = abap_true )
                           IMPORTING et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id  exp = lv_travel_id ).

    gr_cut->save( ).

    CLEAR lv_db_exits.
    SELECT SINGLE FROM /dmo/travel FIELDS @abap_true WHERE travel_id = @lv_travel_id INTO @lv_db_exits.
    cl_abap_unit_assert=>assert_false( lv_db_exits ).
  ENDMETHOD.


  METHOD delete_delete_in_one_luw.
    DATA lv_db_exits TYPE abap_bool.

    DATA(ls_travel_new) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_2  begin_date = '20190101'  end_date = '20190201'  description = 'My_Old_Text' ) ).
    DATA(lv_travel_id) = ls_travel_new-travel_id.

    gr_cut->delete_travel( EXPORTING iv_travel_id = lv_travel_id
                           IMPORTING et_messages  = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->delete_travel( EXPORTING iv_travel_id = lv_travel_id
                           IMPORTING et_messages  = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id  exp = lv_travel_id ).

    gr_cut->save( ).

    CLEAR lv_db_exits.
    SELECT SINGLE FROM /dmo/travel FIELDS @abap_true WHERE travel_id = @lv_travel_id INTO @lv_db_exits.
    cl_abap_unit_assert=>assert_false( lv_db_exits ).
  ENDMETHOD.


  METHOD u_no_control.
    DATA(ls_travel) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' ) ).

    " No control data at all
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel-travel_id )
                                     is_travelx  = VALUE #( )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_no_control-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = ls_travel-travel_id ).

    _delete_existing_travel( ls_travel-travel_id ).
  ENDMETHOD.


  METHOD u_status_invalid.
    CONSTANTS lc_status_invalid TYPE /dmo/travel_status VALUE 'Z'.

    DATA(ls_travel) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' ) ).

    " No control data at all
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel-travel_id  status = lc_status_invalid )
                                     is_travelx  = VALUE #( travel_id = ls_travel-travel_id  status = abap_true )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_status_invalid-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_STATUS' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_status                      exp = lc_status_invalid ).

    _delete_existing_travel( ls_travel-travel_id ).
  ENDMETHOD.


  METHOD _create_travel.
    CLEAR rs_travel.
    gr_cut->create_travel( EXPORTING is_travel   = is_travel
                           IMPORTING es_travel   = rs_travel
                                     et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_not_initial( rs_travel-travel_id ).
    IF iv_save = abap_true.
      gr_cut->save( ).
    ENDIF.
  ENDMETHOD.


  METHOD _delete_existing_travel.
    gr_cut->delete_travel( EXPORTING iv_travel_id = iv_travel_id
                           IMPORTING et_messages  = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->save( ).
  ENDMETHOD.

  METHOD check_travel_id.
    "Test Travel IDs
    CONSTANTS cv_travel_id_initial          TYPE /dmo/travel_id VALUE IS INITIAL.
    CONSTANTS cv_travel_id_in_delete_buffer TYPE /dmo/travel_id VALUE '123'.
    CONSTANTS cv_travel_id_in_create_buffer TYPE /dmo/travel_id VALUE '321'.
    DATA      lv_travel_id_in_db            TYPE /dmo/travel_id.
    DATA      lv_travel_id_unknown          TYPE /dmo/travel_id.

    "Other variables
    DATA lv_travel_id_is_valid TYPE abap_bool.
    DATA: lt_messages TYPE /dmo/if_flight_legacy=>tt_if_t100_message.

    "setup
    DATA(lo_buffer) = NEW lcl_travel_buffer( ).
    lo_buffer->mt_delete_buffer = VALUE #( ( travel_id = cv_travel_id_in_delete_buffer ) ).
    lo_buffer->mt_create_buffer = VALUE #( ( travel_id = cv_travel_id_in_create_buffer ) ).
    SELECT SINGLE FROM /dmo/travel FIELDS MAX( travel_id ) INTO @lv_travel_id_in_db.
    lv_travel_id_unknown = lv_travel_id_in_db + 1.

    "Check if travel_id is initial
    CLEAR lt_messages.
    CLEAR lv_travel_id_is_valid.
    lv_travel_id_is_valid = lo_buffer->check_travel_id(
                                EXPORTING
                                  iv_travel_id = cv_travel_id_initial
                                CHANGING
                                  ct_messages  = lt_messages
                              ).
    cl_abap_unit_assert=>assert_equals( exp = abap_false  act = lv_travel_id_is_valid ).
    cl_abap_unit_assert=>assert_not_initial( act = lt_messages ).

    "Check if travel_id is in delete buffer
    CLEAR lt_messages.
    CLEAR lv_travel_id_is_valid.
    lv_travel_id_is_valid = lo_buffer->check_travel_id(
                                EXPORTING
                                  iv_travel_id = cv_travel_id_in_delete_buffer
                                CHANGING
                                  ct_messages  = lt_messages
                              ).
    cl_abap_unit_assert=>assert_equals( exp = abap_false  act = lv_travel_id_is_valid ).
    cl_abap_unit_assert=>assert_not_initial( act = lt_messages ).

    "Check if travel_id is unknown
    CLEAR lt_messages.
    CLEAR lv_travel_id_is_valid.
    lv_travel_id_is_valid = lo_buffer->check_travel_id(
                                EXPORTING
                                  iv_travel_id = lv_travel_id_unknown
                                CHANGING
                                  ct_messages  = lt_messages
                              ).
    cl_abap_unit_assert=>assert_equals( exp = abap_false  act = lv_travel_id_is_valid ).
    cl_abap_unit_assert=>assert_not_initial( act = lt_messages ).

    "Check if travel_id is in create buffer
    CLEAR lt_messages.
    CLEAR lv_travel_id_is_valid.
    lv_travel_id_is_valid = lo_buffer->check_travel_id(
                                EXPORTING
                                  iv_travel_id = cv_travel_id_in_create_buffer
                                CHANGING
                                  ct_messages  = lt_messages
                              ).
    cl_abap_unit_assert=>assert_equals( exp = abap_true  act = lv_travel_id_is_valid ).
    cl_abap_unit_assert=>assert_initial( act = lt_messages ).

    "Check if travel_id is in db and therefore valid
    CLEAR lt_messages.
    CLEAR lv_travel_id_is_valid.
    lv_travel_id_is_valid = lo_buffer->check_travel_id(
                                EXPORTING
                                  iv_travel_id = lv_travel_id_in_db
                                CHANGING
                                  ct_messages  = lt_messages
                              ).
    cl_abap_unit_assert=>assert_equals( exp = abap_true  act = lv_travel_id_is_valid ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
  ENDMETHOD.

  METHOD adjust_numbers.
    CONSTANTS cv_travel_id_early       TYPE /dmo/travel_id VALUE '42'.
    DATA      lv_travel_id_preliminary TYPE /dmo/travel_id.

    DATA lt_mapping TYPE /dmo/if_flight_legacy=>tt_ln_travel_mapping.

    DATA(lo_buffer) = NEW lcl_travel_buffer( ).

    "early
    CLEAR lt_mapping.
    CLEAR lo_buffer->mt_create_buffer.
    lo_buffer->mt_create_buffer = VALUE #( ( travel_id = cv_travel_id_early ) ).

    lt_mapping = lo_buffer->adjust_numbers( ).
    cl_abap_unit_assert=>assert_initial( lt_mapping ).


    "late
    CLEAR lt_mapping.
    CLEAR lo_buffer->mt_create_buffer.
    lv_travel_id_preliminary = /dmo/if_flight_legacy=>late_numbering_boundary + 1.
    lo_buffer->mt_create_buffer = VALUE #( ( travel_id = lv_travel_id_preliminary ) ).

    lt_mapping = lo_buffer->adjust_numbers( ).

    cl_abap_unit_assert=>assert_not_initial( lt_mapping ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_mapping )  exp = 1 ).
    cl_abap_unit_assert=>assert_not_initial( lt_mapping[ 1 ]-final-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lt_mapping[ 1 ]-preliminary-travel_id  exp = lv_travel_id_preliminary ).

    ROLLBACK WORK.                                     "#EC CI_ROLLBACK
  ENDMETHOD.

ENDCLASS.


CLASS ltc_booking DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS INHERITING FROM ltc_travel.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_flight,
             carrier_id    TYPE /dmo/carrier_id,
             connection_id TYPE /dmo/connection_id,
             flight_date   TYPE /dmo/flight_date,
             price         TYPE /dmo/flight_price,
             currency_code TYPE /dmo/currency_code,
           END OF ts_flight.

    CLASS-DATA gs_flight_1 TYPE ts_flight.
    CLASS-DATA gs_flight_2 TYPE ts_flight.

    CLASS-DATA gv_carrier_id_unknown       TYPE /dmo/carrier_id.

    CLASS-DATA gv_currency_code_unknown    TYPE /dmo/currency_code.

    CLASS-DATA gv_booking_date             TYPE /dmo/booking_date.

    DATA mv_travel_id         TYPE /dmo/travel_id.
    DATA mv_travel_id_unknown TYPE /dmo/travel_id.

    METHODS _create_booking IMPORTING iv_travel_id      TYPE /dmo/travel_id
                                      is_booking        TYPE /dmo/s_booking_in
                                      iv_save           TYPE abap_bool DEFAULT abap_true
                            RETURNING VALUE(rs_booking) TYPE /dmo/booking.
    METHODS _delete_existing_booking IMPORTING iv_travel_id  TYPE /dmo/travel_id
                                               iv_booking_id TYPE /dmo/booking_id.

  PRIVATE SECTION.
    CLASS-METHODS class_setup.
    METHODS setup.
    METHODS teardown.

    "! Create, Update, Delete a single booking in 3 different LUWs
    METHODS c_u_d_single             FOR TESTING RAISING cx_static_check.
    "! Create, Update a single booking in the same LUW, delete it in a second LUW
    METHODS cu_d_single              FOR TESTING RAISING cx_static_check.
    "! Create, Update, Delete a single booking in the same LUW
    METHODS cud_single               FOR TESTING RAISING cx_static_check.
    "! Create, (Update, Update), Delete a single booking in 3 different LUWs
    METHODS c_uu_d_single            FOR TESTING RAISING cx_static_check.
    "! Try to create a booking with an unknown Travel ID -> ERROR
    METHODS c_travel_id_unknown      FOR TESTING RAISING cx_static_check.
    "! Try to create a booking with an initial Travel ID -> ERROR
    METHODS c_travel_id_initial      FOR TESTING RAISING cx_static_check.
    "! Try to update a booking with an unknown Travel ID -> ERROR
    METHODS u_travel_id_unknown      FOR TESTING RAISING cx_static_check.
    "! Try to update a booking with an initial Travel ID -> ERROR
    METHODS u_travel_id_initial      FOR TESTING RAISING cx_static_check.
    "! Try to delete a booking with an unknown Travel ID -> ERROR
    METHODS d_travel_id_unknown      FOR TESTING RAISING cx_static_check.
    "! Try to delete a booking with an initial Travel ID -> ERROR
    METHODS d_travel_id_initial      FOR TESTING RAISING cx_static_check.
    "! Try to update a booking with an unknown Booking ID -> ERROR
    METHODS u_booking_id_unknown     FOR TESTING RAISING cx_static_check.
    "! Try to delete a booking with an unknown Booking ID -> ERROR
    METHODS d_booking_id_unknown     FOR TESTING RAISING cx_static_check.
    "! Try to update a booking with an initial Booking ID -> ERROR
    METHODS u_booking_id_initial     FOR TESTING RAISING cx_static_check.
    "! Try to delete a booking with an initial Booking ID -> ERROR
    METHODS d_booking_id_initial     FOR TESTING RAISING cx_static_check.
    "! Create, update, delete a single travel with bookings in 3 different LUWs
    METHODS c_u_d_travel_w_bookings  FOR TESTING RAISING cx_static_check.
    "! Try to create a booking with an unknown customer -> ERROR
    METHODS c_customer_unknown       FOR TESTING RAISING cx_static_check.
    "! Try to update a booking with an unknown customer -> ERROR
    METHODS u_customer_unknown       FOR TESTING RAISING cx_static_check.
    "! Try to create a booking with a booking date in the past -> ERROR
    METHODS c_booking_date_past      FOR TESTING RAISING cx_static_check.
    "! Try to clear booking date -> ERROR
    METHODS u_booking_date_initial   FOR TESTING RAISING cx_static_check.
    "! Try to create a booking with invalid flight data -> ERROR
    METHODS c_flight_invalid         FOR TESTING RAISING cx_static_check.
    "! Try to update a booking with different flight data -> ERROR
    METHODS u_flight                 FOR TESTING RAISING cx_static_check.
    "! Try to delete, update a single booking in the same LUW -> ERROR
    METHODS du_single                FOR TESTING RAISING cx_static_check.
    "! Try to delete, delete a single booking in the same LUW -> NO Error
    METHODS dd_single                FOR TESTING RAISING cx_static_check.
    "! Create booking, delete father travel in same LUW -> NO Error, Booking ignored
    METHODS c_booking_d_travel_1_luw FOR TESTING RAISING cx_static_check.
    "! Try to delete father travel, create booking in same LUW -> ERROR
    METHODS d_travel_c_booking_1_luw FOR TESTING RAISING cx_static_check.
    "! Try to create a booking with an initial Booking ID -> ERROR
    METHODS c_booking_id_initial     FOR TESTING RAISING cx_static_check.
    "! Try to create a booking with a known combination Travel ID, Booking ID -> ERROR
    METHODS c_booking_id_exists      FOR TESTING RAISING cx_static_check.
    "! Check that price and currency are derived / not derived
    METHODS c_price_currency         FOR TESTING RAISING cx_static_check.
    "! Try to update price only or currency only -> ERROR
    METHODS u_price_currency         FOR TESTING RAISING cx_static_check.
    "! Try to create a booking with an unknown currency code -> ERROR
    METHODS c_currency_code_unknown  FOR TESTING RAISING cx_static_check.
    "! Try to update a booking with an unknown currency code -> ERROR
    METHODS u_currency_code_unknown  FOR TESTING RAISING cx_static_check.
    "! Checks if a travel_id is drawn in late numbering mode and skipped in early and the booking_id is copied
    METHODS adjust_numbers              FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS /dmo/cl_flight_legacy DEFINITION LOCAL FRIENDS ltc_booking.

CLASS ltc_booking IMPLEMENTATION.
  METHOD class_setup.
    IF mc_use_sql_doubles = abap_true ##BOOL_OK.
*     gs_flight_1 = VALUE #( carrier_id = 'AB'  connection_id = '0001'  flight_date = '20190306'  price = '10818.00'  currency_code = 'SGD' ) ##LITERAL.
*     gs_flight_2 = value #( carrier_id = 'CD'  connection_id = '0002'  flight_date = '20180510'  price = '5950.00'   currency_code = 'SGD' ) ##LITERAL.
*
*     DATA lt_flight TYPE STANDARD TABLE OF /dmo/flight.
*     lt_flight = VALUE #( ( carrier_id = gs_flight_1-carrier_id  connection_id = gs_flight_1-connection_id  flight_date = gs_flight_1-flight_date  price = gs_flight_1-price  currency_code = gs_flight_1-currency_code )
*                          ( carrier_id = gs_flight_2-carrier_id  connection_id = gs_flight_2-connection_id  flight_date = gs_flight_2-flight_date  price = gs_flight_2-price  currency_code = gs_flight_2-currency_code ) ).
*     mr_test_environment->insert_test_data( lt_flight ).
*
*     gv_currency_code_unknown = 'XYZ'.
    ELSE.
      " Select 2 different Flight Dates with their prices
      SELECT SINGLE carrier_id, connection_id, flight_date, price, currency_code FROM /dmo/flight
        INTO CORRESPONDING FIELDS OF @gs_flight_1 ##WARN_OK. "#EC CI_NOORDER
      IF sy-subrc <> 0.
        cl_abap_unit_assert=>abort( |No flight data!| ).
      ENDIF.
      SELECT SINGLE carrier_id, connection_id, flight_date, price, currency_code FROM /dmo/flight WHERE carrier_id <> @gs_flight_1-carrier_id AND connection_id <> @gs_flight_1-connection_id
        INTO CORRESPONDING FIELDS OF @gs_flight_2  ##WARN_OK. "#EC CI_NOORDER
      IF sy-subrc <> 0.
        cl_abap_unit_assert=>abort( |No flight data!| ).
      ENDIF.

      " Determine an unknown Carrier ID
      gv_carrier_id_unknown = 'XX'.
      SELECT SINGLE carrier_id FROM /dmo/carrier WHERE carrier_id = @gv_carrier_id_unknown INTO @DATA(lv_dummy_carrier_id).
      IF sy-subrc = 0.
        cl_abap_unit_assert=>abort( |Carrier ID { gv_carrier_id_unknown } should not be known!| ).
      ENDIF.

      " Invalid currency code
      gv_currency_code_unknown = 'XYZ'.
      " We should use TCURC, but this is not released for "ABAP for SAP Cloud Platform"
      SELECT SINGLE currency FROM i_currency WHERE currency = @gv_currency_code_unknown INTO @DATA(lv_dummy_currency_code).
      IF sy-subrc = 0.
        cl_abap_unit_assert=>abort( |Currency { gv_currency_code_unknown } should not be known!| ).
      ENDIF.
    ENDIF.

    " Use Current date as Booking date
    gv_booking_date = cl_abap_context_info=>get_system_date( ).
  ENDMETHOD.


  METHOD setup.
    mv_travel_id = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' ) )-travel_id.

    mv_travel_id_unknown = mv_travel_id.
    DO.
      mv_travel_id_unknown = mv_travel_id_unknown + 1.
      SELECT SINGLE travel_id FROM /dmo/travel WHERE travel_id = @mv_travel_id_unknown INTO @DATA(lv_dummy_travel_id).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD teardown.
    _delete_existing_travel( mv_travel_id ).
  ENDMETHOD.


  METHOD c_u_d_single.
    DATA(ls_booking) = _create_booking( iv_travel_id = mv_travel_id
                                        is_booking   = VALUE #( booking_id    = '10'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking-customer_id    exp = gv_customer_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking-carrier_id     exp = gs_flight_1-carrier_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking-connection_id  exp = gs_flight_1-connection_id ).

    DATA ls_booking_sel TYPE /dmo/booking.
    SELECT SINGLE customer_id FROM /dmo/booking WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO CORRESPONDING FIELDS OF @ls_booking_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-customer_id  exp = ls_booking-customer_id ).

    DATA(lv_booking_date) = gv_booking_date + 15.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id  booking_date = lv_booking_date ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  booking_date = abap_true ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->save( ).

    CLEAR ls_booking_sel.
    SELECT SINGLE carrier_id, booking_date FROM /dmo/booking WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO CORRESPONDING FIELDS OF @ls_booking_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-carrier_id    exp = ls_booking-carrier_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-booking_date  exp = lv_booking_date ).

    _delete_existing_booking( iv_travel_id = mv_travel_id  iv_booking_id = ls_booking-booking_id ).
    SELECT SINGLE @abap_true FROM /dmo/booking WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO @DATA(lv_found).
    cl_abap_unit_assert=>assert_subrc( exp = 4 ).
  ENDMETHOD.


  METHOD cu_d_single.
    DATA(ls_booking) = _create_booking( iv_travel_id = mv_travel_id
                                        is_booking   = VALUE #( booking_id    = '10'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date )
                                        iv_save      = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking-customer_id    exp = gv_customer_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking-carrier_id     exp = gs_flight_1-carrier_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking-connection_id  exp = gs_flight_1-connection_id ).

    DATA(lv_booking_date) = gv_booking_date + 15.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id  booking_date = lv_booking_date ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  booking_date =  abap_true ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->save( ).

    DATA ls_booking_sel TYPE /dmo/booking.
    SELECT SINGLE carrier_id, booking_date FROM /dmo/booking WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO CORRESPONDING FIELDS OF @ls_booking_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-carrier_id    exp = ls_booking-carrier_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-booking_date  exp = lv_booking_date ).

    _delete_existing_booking( iv_travel_id = mv_travel_id  iv_booking_id = ls_booking-booking_id ).
    SELECT SINGLE @abap_true FROM /dmo/booking WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO @DATA(lv_found).
    cl_abap_unit_assert=>assert_subrc( exp = 4 ).
  ENDMETHOD.


  METHOD cud_single.
    DATA lv_db_exists TYPE abap_bool.

    SELECT SINGLE lastchangedat FROM /dmo/travel WHERE travel_id = @mv_travel_id INTO @DATA(lv_lastchangedat_1).

    DATA(ls_booking) = _create_booking( iv_travel_id = mv_travel_id
                                        is_booking   = VALUE #( booking_id    = '10'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date )
                                        iv_save      = abap_false ).

    DATA lv_booking_date TYPE /dmo/booking_date.
    lv_booking_date = gv_booking_date + 15.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id  booking_date = lv_booking_date ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  booking_date = abap_true ) )
                           IMPORTING et_booking  = DATA(lt_booking_updated)
                                     et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking_updated )  exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_booking_updated[ 1 ]-travel_id     exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lt_booking_updated[ 1 ]-booking_id    exp = ls_booking-booking_id ).
    cl_abap_unit_assert=>assert_equals( act = lt_booking_updated[ 1 ]-booking_date  exp = lv_booking_date ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->save( ).

    " Check that the administrative fields of the root node have been updated
    SELECT SINGLE lastchangedby, lastchangedat FROM /dmo/travel WHERE travel_id = @mv_travel_id INTO ( @DATA(lv_lastchangedby), @DATA(lv_lastchangedat_2) ).
    cl_abap_unit_assert=>assert_equals( act = lv_lastchangedby  exp = sy-uname ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_lastchangedat_2 > lv_lastchangedat_1 ) ).

    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/booking FIELDS @abap_true WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD c_uu_d_single.
    DATA lv_db_exists TYPE abap_bool.

    DATA(ls_booking) = _create_booking( iv_travel_id = mv_travel_id
                                        is_booking   = VALUE #( booking_id    = '10'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date ) ).

    SELECT SINGLE customer_id FROM /dmo/booking WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO @DATA(lv_customer_id).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_customer_id  exp = ls_booking-customer_id ).

    DATA(lv_booking_date) = gv_booking_date + 15.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id  booking_date = lv_booking_date ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  booking_date = abap_true ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id  customer_id = gv_customer_id_2 ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  customer_id = abap_true ) )
                           IMPORTING et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->save( ).

    DATA ls_booking_sel TYPE /dmo/booking.
    SELECT SINGLE booking_date, customer_id FROM /dmo/booking WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO CORRESPONDING FIELDS OF @ls_booking_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-booking_date  exp = lv_booking_date ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-customer_id   exp = gv_customer_id_2 ).

    _delete_existing_booking( iv_travel_id = mv_travel_id  iv_booking_id = ls_booking-booking_id ).
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/booking FIELDS @abap_true WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD c_travel_id_unknown.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id_unknown )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id_unknown )
                                     it_booking  = VALUE #( ( booking_id = '10'  booking_date = '20180701'  customer_id = gv_customer_id_1  carrier_id = gs_flight_1-carrier_id  connection_id = gs_flight_1-connection_id ) )
                                     it_bookingx = VALUE #( ( booking_id = '10'  action_code = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking  = DATA(lt_booking)
                                     et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id_unknown ).
  ENDMETHOD.


  METHOD c_travel_id_initial.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( )
                                     is_travelx  = VALUE #( )
                                     it_booking  = VALUE #( ( booking_id = '10'  booking_date = '20180701'  customer_id = gv_customer_id_1  carrier_id = gs_flight_1-carrier_id  connection_id = gs_flight_1-connection_id ) )
                                     it_bookingx = VALUE #( ( booking_id = '10'  action_code = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking  = DATA(lt_booking)
                                     et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_no_key-msgno ).
  ENDMETHOD.


  METHOD u_travel_id_unknown.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id_unknown )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id_unknown )
                                     it_booking  = VALUE #( ( booking_id = '2'  booking_date = '20180715' ) )
                                     it_bookingx = VALUE #( ( booking_id = '2'  action_code = /dmo/if_flight_legacy=>action_code-update  booking_date = abap_true ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id_unknown ).
  ENDMETHOD.


  METHOD u_travel_id_initial.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( )
                                     is_travelx  = VALUE #( )
                                     it_booking  = VALUE #( ( booking_id = '2'  booking_date = '20180715' ) )
                                     it_bookingx = VALUE #( ( booking_id = '2'  action_code = /dmo/if_flight_legacy=>action_code-update  booking_date = abap_true ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_no_key-msgno ).
  ENDMETHOD.


  METHOD d_travel_id_unknown.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id_unknown )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id_unknown )
                                     it_booking  = VALUE #( ( booking_id = '1' ) )
                                     it_bookingx = VALUE #( ( booking_id = '1'  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id_unknown ).
  ENDMETHOD.


  METHOD d_travel_id_initial.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( )
                                     is_travelx  = VALUE #( )
                                     it_booking  = VALUE #( ( booking_id = '1' ) )
                                     it_bookingx = VALUE #( ( booking_id = '1'  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_no_key-msgno ).
  ENDMETHOD.


  METHOD u_booking_id_unknown.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = '10'  booking_date = '20180715'  ) )
                                     it_bookingx = VALUE #( ( booking_id = '10'  action_code = /dmo/if_flight_legacy=>action_code-update  booking_date = abap_true  ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = '10' ).
  ENDMETHOD.


  METHOD d_booking_id_unknown.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = '10' ) )
                                     it_bookingx = VALUE #( ( booking_id = '10'  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = '10' ).
  ENDMETHOD.


  METHOD u_booking_id_initial.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = '0'  booking_date = '20180715' ) )
                                     it_bookingx = VALUE #( ( booking_id = '0'  action_code = /dmo/if_flight_legacy=>action_code-update  booking_date = abap_true ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_no_key-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
  ENDMETHOD.


  METHOD d_booking_id_initial.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = '0' ) )
                                     it_bookingx = VALUE #( ( booking_id = '0'  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_no_key-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
  ENDMETHOD.


  METHOD c_u_d_travel_w_bookings.
    DATA lv_db_exists TYPE abap_bool.

    " 1. LUW: Create a travel with 2 bookings (the second travel is only for testing)
    DATA(ls_travel_1) = _create_travel( is_travel  = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' )
                                        iv_save           = abap_false ).
    DATA(ls_travel_2) = _create_travel( is_travel  = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' )
                                        iv_save           = abap_false ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel_1-travel_id ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel_2-travel_id ).

    DATA(ls_booking_1) = _create_booking( iv_travel_id = ls_travel_1-travel_id
                                          is_booking   = VALUE #( booking_id    = '10'
                                                                  booking_date  = gv_booking_date
                                                                  customer_id   = gv_customer_id_1
                                                                  carrier_id    = gs_flight_1-carrier_id
                                                                  connection_id = gs_flight_1-connection_id
                                                                  flight_date   = gs_flight_1-flight_date )
                                          iv_save      = abap_false ).
    DATA(ls_booking_2) = _create_booking( iv_travel_id = ls_travel_1-travel_id
                                          is_booking   = VALUE #( booking_id    = '20'
                                                                  booking_date  = gv_booking_date
                                                                  customer_id   = gv_customer_id_1
                                                                  carrier_id    = gs_flight_1-carrier_id
                                                                  connection_id = gs_flight_1-connection_id
                                                                  flight_date   = gs_flight_1-flight_date )
                                          iv_save      = abap_false ).

    gr_cut->save( ).

    SELECT COUNT( * ) FROM /dmo/booking WHERE travel_id = @ls_travel_1-travel_id INTO @DATA(lv_count).
    cl_abap_unit_assert=>assert_equals( act = lv_count  exp = 2 ).

    " 2. LUW: Update the first booking
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_booking_1-travel_id )
                                     is_travelx  = VALUE #( travel_id = ls_booking_1-travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking_1-booking_id  customer_id = gv_customer_id_2 ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking_1-booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  customer_id = abap_true ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->save( ).

    SELECT SINGLE customer_id FROM /dmo/booking WHERE travel_id = @ls_booking_1-travel_id AND booking_id = @ls_booking_1-booking_id INTO @DATA(lv_customer_id).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_customer_id  exp = gv_customer_id_2 ).

    SELECT SINGLE customer_id FROM /dmo/booking WHERE travel_id = @ls_booking_2-travel_id AND booking_id = @ls_booking_2-booking_id INTO @lv_customer_id.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_customer_id  exp = gv_customer_id_1 ).

    " 3. LUW: Delete the first booking - check that the second booking is still there
    _delete_existing_booking( iv_travel_id = ls_booking_1-travel_id  iv_booking_id = ls_booking_1-booking_id ).

    SELECT travel_id, booking_id FROM /dmo/booking WHERE travel_id = @ls_travel_1-travel_id INTO TABLE @DATA(lt_booking_key).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking_key )  exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_booking_key[ 1 ]-booking_id  exp = ls_booking_2-booking_id ).

    " 4. LUW: Delete the travel - check that the second booking has also been deleted
    _delete_existing_travel( ls_travel_1-travel_id ).
    _delete_existing_travel( ls_travel_2-travel_id ).

    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/booking FIELDS @abap_true WHERE travel_id = @ls_travel_1-travel_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD c_customer_unknown.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id    = '10'
                                                              booking_date  = gv_booking_date
                                                              customer_id   = gv_customer_id_unknown
                                                              carrier_id    = gs_flight_1-carrier_id
                                                              connection_id = gs_flight_1-connection_id
                                                              flight_date   = gs_flight_1-flight_date ) )
                                     it_bookingx = VALUE #( ( booking_id  = '10'
                                                              action_code = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking  = DATA(lt_booking)
                                     et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>customer_unkown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_CUSTOMER_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_customer_id                 exp = gv_customer_id_unknown ).
  ENDMETHOD.


  METHOD u_customer_unknown.
    DATA(ls_booking) = _create_booking( iv_travel_id = mv_travel_id
                                        is_booking   = VALUE #( booking_id    = '10'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date ) ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id  customer_id = gv_customer_id_unknown ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  customer_id = abap_true ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>customer_unkown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_CUSTOMER_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_customer_id                 exp = gv_customer_id_unknown ).

    gr_cut->save( ).

    SELECT SINGLE customer_id FROM /dmo/booking WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO @DATA(lv_customer_id).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_customer_id  exp = ls_booking-customer_id ).

    _delete_existing_booking( iv_travel_id = mv_travel_id  iv_booking_id = ls_booking-booking_id ).
  ENDMETHOD.


  METHOD c_booking_date_past.
    gr_cut->update_travel( EXPORTING is_travel    = VALUE #( travel_id = mv_travel_id )
                                     is_travelx   = VALUE #( travel_id = mv_travel_id )
                                     it_booking   = VALUE #( ( booking_id    = '10'
                                                               booking_date  = '20180101'
                                                               customer_id   = gv_customer_id_1
                                                               carrier_id    = gs_flight_1-carrier_id
                                                               connection_id = gs_flight_1-connection_id
                                                               flight_date   = gs_flight_1-flight_date ) )
                                      it_bookingx = VALUE #( ( booking_id  = '10'
                                                               action_code = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking  = DATA(lt_booking)
                                     et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_booking_date_invalid-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr3  exp = 'MV_BOOKING_DATE' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = '10' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_date                exp = '20180101' ).
  ENDMETHOD.


  METHOD u_booking_date_initial.
    DATA(ls_booking) = _create_booking( iv_travel_id = mv_travel_id
                                        is_booking   = VALUE #( booking_id    = '10'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date ) ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  booking_date = abap_true ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_booking_date_invalid-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr3  exp = 'MV_BOOKING_DATE' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = ls_booking-booking_id ).
    cl_abap_unit_assert=>assert_initial( lx->mv_booking_date ).

    gr_cut->save( ).

    SELECT SINGLE booking_date FROM /dmo/booking WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO @DATA(lv_booking_date).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_booking_date  exp = gv_booking_date ).

    _delete_existing_booking( iv_travel_id = mv_travel_id  iv_booking_id = ls_booking-booking_id ).
  ENDMETHOD.


  METHOD c_flight_invalid.
    gr_cut->update_travel( EXPORTING is_travel    = VALUE #( travel_id = mv_travel_id )
                                     is_travelx   = VALUE #( travel_id = mv_travel_id )
                                     it_booking   = VALUE #( ( booking_id    = '10'
                                                               booking_date  = gv_booking_date
                                                               customer_id   = gv_customer_id_1
                                                               carrier_id    = gv_carrier_id_unknown
                                                               connection_id = gs_flight_1-connection_id
                                                               flight_date   = gs_flight_1-flight_date ) )
                                      it_bookingx = VALUE #( ( booking_id    = '10'
                                                               action_code   = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking   = DATA(lt_booking)
                                     et_messages  = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>flight_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_CARRIER_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_CONNECTION_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr3  exp = 'MV_FLIGHT_DATE' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_carrier_id                  exp = gv_carrier_id_unknown ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_connection_id               exp = gs_flight_1-connection_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_flight_date                 exp = gs_flight_1-flight_date ).
  ENDMETHOD.


  METHOD u_flight.
    DATA(ls_booking) = _create_booking( iv_travel_id = mv_travel_id
                                        is_booking   = VALUE #( booking_id    = '10'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date ) ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id  flight_date = gs_flight_2-flight_date ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  flight_date = abap_true ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_flight_u-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = '10' ).

    gr_cut->save( ).

    SELECT SINGLE flight_date FROM /dmo/booking WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO @DATA(lv_flight_date).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_flight_date  exp = gs_flight_1-flight_date ).

    _delete_existing_booking( iv_travel_id = mv_travel_id  iv_booking_id = ls_booking-booking_id ).
  ENDMETHOD.


  METHOD du_single.
    DATA lv_db_exists TYPE abap_bool.

    DATA(ls_booking) = _create_booking( iv_travel_id = mv_travel_id
                                        is_booking   = VALUE #( booking_id    = '10'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date ) ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id  customer_id = gv_customer_id_2 ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  customer_id = abap_true ) )
                           IMPORTING et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = ls_booking-booking_id ).

    gr_cut->save( ).

    SELECT SINGLE FROM /dmo/booking FIELDS @abap_true WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD dd_single.
    DATA lv_db_exists TYPE abap_bool.

    DATA(ls_booking) = _create_booking( iv_travel_id = mv_travel_id
                                        is_booking   = VALUE #( booking_id    = '10'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date ) ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->save( ).

    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/booking FIELDS @abap_true WHERE travel_id = @mv_travel_id AND booking_id = @ls_booking-booking_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD c_booking_d_travel_1_luw.
    DATA lv_db_exists TYPE abap_bool.

    " 1. LUW: Create travel
    DATA(ls_travel) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' ) ).
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/travel FIELDS @abap_true WHERE travel_id = @ls_travel-travel_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_true( lv_db_exists ).

    " 2. LUW: Create booking for the new travel, delete the new travel
    _create_booking( iv_travel_id = ls_travel-travel_id
                     is_booking   = VALUE #( booking_id    = '10'
                                             booking_date  = gv_booking_date
                                             customer_id   = gv_customer_id_1
                                             carrier_id    = gs_flight_1-carrier_id
                                             connection_id = gs_flight_1-connection_id
                                             flight_date   = gs_flight_1-flight_date )
                     iv_save    = abap_false ).

    gr_cut->delete_travel( EXPORTING iv_travel_id = ls_travel-travel_id
                           IMPORTING et_messages  = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->save( ).

    "Check that travel and booking are gone
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/travel FIELDS @abap_true WHERE travel_id = @ls_travel-travel_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).

    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/booking FIELDS @abap_true WHERE travel_id = @ls_travel-travel_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD d_travel_c_booking_1_luw.
    DATA lv_db_exists TYPE abap_bool.

    " 1. LUW: Create travel
    DATA(ls_travel) = _create_travel( VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' ) ).
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/travel FIELDS @abap_true WHERE travel_id = @ls_travel-travel_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_true( lv_db_exists ).

    " 2. LUW: Delete the new travel, create booking for the same travel
    gr_cut->delete_travel( EXPORTING iv_travel_id = ls_travel-travel_id
                           IMPORTING et_messages  = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->update_travel( EXPORTING is_travel    = VALUE #( travel_id = ls_travel-travel_id )
                                     is_travelx   = VALUE #( travel_id = ls_travel-travel_id )
                                     it_booking   = VALUE #( ( booking_id    = '10'
                                                               booking_date  = gv_booking_date
                                                               customer_id   = gv_customer_id_1
                                                               carrier_id    = gs_flight_1-carrier_id
                                                               connection_id = gs_flight_1-connection_id
                                                               flight_date   = gs_flight_1-flight_date ) )
                                      it_bookingx = VALUE #( ( booking_id    = '10'
                                                               action_code   = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking   = DATA(lt_booking)
                                     et_messages  = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_booking ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = ls_travel-travel_id ).

    gr_cut->save( ).

    "Check that travel and booking are gone
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/travel FIELDS @abap_true WHERE travel_id = @ls_travel-travel_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).

    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/booking FIELDS @abap_true WHERE travel_id = @ls_travel-travel_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD c_booking_id_initial.
    gr_cut->update_travel( EXPORTING is_travel    = VALUE #( travel_id = mv_travel_id )
                                     is_travelx   = VALUE #( travel_id = mv_travel_id )
                                     it_booking   = VALUE #( ( booking_id    = '0'
                                                               booking_date  = gv_booking_date
                                                               customer_id   = gv_customer_id_1
                                                               carrier_id    = gs_flight_1-carrier_id
                                                               connection_id = gs_flight_1-connection_id
                                                               flight_date   = gs_flight_1-flight_date ) )
                                      it_bookingx = VALUE #( ( booking_id    = '0'
                                                               action_code = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking   = DATA(lt_booking)
                                     et_messages  = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_no_key-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
  ENDMETHOD.


  METHOD c_booking_id_exists.
    _create_booking( iv_travel_id = mv_travel_id
                     is_booking   = VALUE #( booking_id    = '10'
                                             booking_date  = gv_booking_date
                                             customer_id   = gv_customer_id_1
                                             carrier_id    = gs_flight_1-carrier_id
                                             connection_id = gs_flight_1-connection_id
                                            flight_date   = gs_flight_1-flight_date )
                     iv_save      = abap_false ).

    " Try to create a booking that is already in the buffer
    gr_cut->update_travel( EXPORTING is_travel    = VALUE #( travel_id = mv_travel_id )
                                     is_travelx   = VALUE #( travel_id = mv_travel_id )
                                     it_booking   = VALUE #( ( booking_id    = '10'
                                                               booking_date  = gv_booking_date
                                                               customer_id   = gv_customer_id_1
                                                               carrier_id    = gs_flight_1-carrier_id
                                                               connection_id = gs_flight_1-connection_id
                                                               flight_date   = gs_flight_1-flight_date ) )
                                      it_bookingx = VALUE #( ( booking_id    = '10'
                                                               action_code   = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking   = DATA(lt_booking)
                                     et_messages  = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_exists-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = '10' ).

    gr_cut->save( ).

    " Try to create a booking that is already in the database
    CLEAR lt_messages.
    gr_cut->update_travel( EXPORTING is_travel    = VALUE #( travel_id = mv_travel_id )
                                     is_travelx   = VALUE #( travel_id = mv_travel_id )
                                     it_booking   = VALUE #( ( booking_id    = '10'
                                                               booking_date  = gv_booking_date
                                                               customer_id   = gv_customer_id_1
                                                               carrier_id    = gs_flight_1-carrier_id
                                                               connection_id = gs_flight_1-connection_id
                                                               flight_date   = gs_flight_1-flight_date ) )
                                      it_bookingx = VALUE #( ( booking_id    = '10'
                                                               action_code   = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking   = lt_booking
                                     et_messages  = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_booking ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_exists-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = '10' ).
  ENDMETHOD.


  METHOD c_price_currency.
    " Do not provide price and currency
    DATA(ls_booking_1) = _create_booking( iv_travel_id = mv_travel_id
                                          is_booking   = VALUE #( booking_id    = '10'
                                                                  booking_date  = gv_booking_date
                                                                  customer_id   = gv_customer_id_1
                                                                  carrier_id    = gs_flight_1-carrier_id
                                                                  connection_id = gs_flight_1-connection_id
                                                                  flight_date   = gs_flight_1-flight_date )
                                          iv_save      = abap_false ).

    " Provide price and currency
    CONSTANTS lc_diff TYPE p VALUE '99.99'.
    DATA lv_flight_price  TYPE /dmo/flight_price.
    DATA lv_currency_code TYPE /dmo/currency_code.
    lv_flight_price = gs_flight_1-price + lc_diff.
    lv_currency_code = SWITCH #( gs_flight_1-currency_code WHEN 'EUR' THEN 'USD' ELSE 'EUR' ).
    DATA(ls_booking_2) = _create_booking( iv_travel_id = mv_travel_id
                                          is_booking   = VALUE #( booking_id    = '20'
                                                                  booking_date  = gv_booking_date
                                                                  customer_id   = gv_customer_id_1
                                                                  carrier_id    = gs_flight_1-carrier_id
                                                                  connection_id = gs_flight_1-connection_id
                                                                  flight_date   = gs_flight_1-flight_date
                                                                  flight_price  = lv_flight_price
                                                                  currency_code = lv_currency_code )
                                          iv_save      = abap_true ).

    DATA ls_booking_sel TYPE /dmo/booking.
    SELECT SINGLE flight_price, currency_code FROM /dmo/booking WHERE travel_id = @ls_booking_1-travel_id AND booking_id = @ls_booking_1-booking_id INTO CORRESPONDING FIELDS OF @ls_booking_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-flight_price   exp = gs_flight_1-price ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-currency_code  exp = gs_flight_1-currency_code ).

    CLEAR ls_booking_sel.
    SELECT SINGLE flight_price, currency_code FROM /dmo/booking WHERE travel_id = @ls_booking_1-travel_id AND booking_id = @ls_booking_2-booking_id INTO CORRESPONDING FIELDS OF @ls_booking_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-flight_price   exp = lv_flight_price ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-currency_code  exp = lv_currency_code ).
  ENDMETHOD.


  METHOD u_price_currency.
    DATA(ls_booking) = _create_booking( iv_travel_id = mv_travel_id
                                        is_booking   = VALUE #( booking_id    = '10'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date
                                                                flight_price  = '999.99'
                                                                currency_code = 'EUR' )
                                        iv_save      = abap_false ).

    " Try to change the flight price only
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_booking-travel_id )
                                     is_travelx  = VALUE #( travel_id = ls_booking-travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id  flight_price = '888.88' ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  flight_price = abap_true ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_price_currency_u-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = ls_booking-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = ls_booking-booking_id ).

    " Try to change the currency code only
    CLEAR lt_messages.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_booking-travel_id )
                                     is_travelx  = VALUE #( travel_id = ls_booking-travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id  currency_code = 'XXX' ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  currency_code = abap_true ) )
                           IMPORTING et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_price_currency_u-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = ls_booking-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = ls_booking-booking_id ).
  ENDMETHOD.


  METHOD c_currency_code_unknown.
    gr_cut->update_travel( EXPORTING is_travel    = VALUE #( travel_id = mv_travel_id )
                                     is_travelx   = VALUE #( travel_id = mv_travel_id )
                                     it_booking   = VALUE #( ( booking_id    = '10'
                                                               booking_date  = gv_booking_date
                                                               customer_id   = gv_customer_id_1
                                                               carrier_id    = gs_flight_1-carrier_id
                                                               connection_id = gs_flight_1-connection_id
                                                               flight_date   = gs_flight_1-flight_date
                                                               flight_price  = '999.99'
                                                               currency_code = gv_currency_code_unknown ) )
                                      it_bookingx = VALUE #( ( booking_id  = '10'
                                                               action_code = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>currency_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_CURRENCY_CODE' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_currency_code               exp = gv_currency_code_unknown ).
  ENDMETHOD.


  METHOD u_currency_code_unknown.
    DATA(ls_booking) = _create_booking( iv_travel_id = mv_travel_id
                                        is_booking   = VALUE #( booking_id    = '10'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date
                                                                flight_price  = '999.99'
                                                                currency_code = 'EUR' )
                                        iv_save      = abap_false ).

    " Try to change the flight price only
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_booking-travel_id )
                                     is_travelx  = VALUE #( travel_id = ls_booking-travel_id )
                                     it_booking  = VALUE #( ( booking_id = ls_booking-booking_id  flight_price  = '999.99'  currency_code = gv_currency_code_unknown ) )
                                     it_bookingx = VALUE #( ( booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  flight_price = abap_true  currency_code = abap_true ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>currency_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_CURRENCY_CODE' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_currency_code               exp = gv_currency_code_unknown ).
  ENDMETHOD.


  METHOD _create_booking.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = iv_travel_id )
                                     is_travelx  = VALUE #( travel_id = iv_travel_id )
                                     it_booking  = VALUE #( ( is_booking ) )
                                     it_bookingx = VALUE #( ( booking_id = is_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking  = DATA(lt_booking)
                                     et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking )  exp = 1 ).
    rs_booking = lt_booking[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = rs_booking-travel_id   exp = iv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = rs_booking-booking_id  exp = is_booking-booking_id ).
    IF iv_save = abap_true.
      gr_cut->save( ).
    ENDIF.
  ENDMETHOD.


  METHOD _delete_existing_booking.
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = iv_travel_id )
                                     is_travelx  = VALUE #( travel_id = iv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = iv_booking_id ) )
                                     it_bookingx = VALUE #( ( booking_id = iv_booking_id  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->save( ).
  ENDMETHOD.

  METHOD adjust_numbers.
    CONSTANTS cv_travel_id_preliminary_1 TYPE /dmo/travel_id VALUE '1337'.
    CONSTANTS cv_travel_id_final_1       TYPE /dmo/travel_id VALUE '42'.
    CONSTANTS cv_travel_id_final_2       TYPE /dmo/travel_id VALUE '314'.
    CONSTANTS cv_booking_id_final_1      TYPE /dmo/booking_id VALUE '1'.
    CONSTANTS cv_booking_id_final_2      TYPE /dmo/booking_id VALUE '2'.

    DATA lt_mapping TYPE /dmo/if_flight_legacy=>tt_ln_booking_mapping.

    DATA(lo_buffer) = NEW lcl_booking_buffer( ).


    lo_buffer->mt_create_buffer = VALUE #(
        ( travel_id = cv_travel_id_preliminary_1  booking_id = cv_booking_id_final_1 )
        ( travel_id = cv_travel_id_final_2        booking_id = cv_booking_id_final_2 )
      ).

    lt_mapping = lo_buffer->adjust_numbers(
                   VALUE #( ( preliminary = VALUE #( travel_id = cv_travel_id_preliminary_1 )
                              final       = VALUE #( travel_id = cv_travel_id_final_1       ) ) )
      ).

    cl_abap_unit_assert=>assert_not_initial( lt_mapping ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_mapping )  exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
        act = lt_mapping[
                  preliminary-travel_id  = cv_travel_id_preliminary_1
                  preliminary-booking_id = cv_booking_id_final_1
                ]-final
        exp = VALUE /dmo/if_flight_legacy=>ts_ln_booking(
                  travel_id  = cv_travel_id_final_1
                  booking_id = cv_booking_id_final_1
                )
      ).

    cl_abap_unit_assert=>assert_equals(
        act = lt_mapping[
                  preliminary-travel_id  = cv_travel_id_final_2
                  preliminary-booking_id = cv_booking_id_final_2
                ]-final
        exp = VALUE /dmo/if_flight_legacy=>ts_ln_booking(
                  travel_id  = cv_travel_id_final_2
                  booking_id = cv_booking_id_final_2
                )
      ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_booking_supplement DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS INHERITING FROM ltc_booking.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_supplement,
             supplement_id TYPE /dmo/supplement_id,
             price         TYPE /dmo/supplement_price,
             currency_code TYPE /dmo/currency_code,
           END OF ts_supplement.

    CLASS-DATA gs_supplement_1 TYPE ts_supplement.
    CLASS-DATA gs_supplement_2 TYPE ts_supplement.

    CLASS-DATA gv_supplement_id_unknown      TYPE /dmo/supplement_id.

    DATA mv_booking_id         TYPE /dmo/booking_id.
    DATA mv_booking_id_unknown TYPE /dmo/booking_id.

    CLASS-METHODS class_setup.
    METHODS setup.

    "! Complicated test involving all 3 levels
    METHODS c_u_d_multiple               FOR TESTING RAISING cx_static_check.

    "! Try to create a booking supplement with an unknown Travel ID -> ERROR
    METHODS c_travel_id_unknown          FOR TESTING RAISING cx_static_check.
    "! Try to create a booking supplement with an initial Travel ID -> ERROR
    METHODS c_travel_id_initial          FOR TESTING RAISING cx_static_check.
    "! Try to update a booking supplement with an unknown Travel ID -> ERROR
    METHODS u_travel_id_unknown          FOR TESTING RAISING cx_static_check.
    "! Try to update a booking supplement with an initial Travel ID -> ERROR
    METHODS u_travel_id_initial          FOR TESTING RAISING cx_static_check.
    "! Try to delete a booking supplement with an unknown Travel ID -> ERROR
    METHODS d_travel_id_unknown          FOR TESTING RAISING cx_static_check.
    "! Try to delete a booking supplement with an initial Travel ID -> ERROR
    METHODS d_travel_id_initial          FOR TESTING RAISING cx_static_check.

    "! Try to create a booking supplement with an unknown Booking ID -> ERROR
    METHODS c_booking_id_unknown         FOR TESTING RAISING cx_static_check.
    "! Try to create a booking supplement with an initial Travel ID -> ERROR
    METHODS c_booking_id_initial         FOR TESTING RAISING cx_static_check.
    "! Try to update a booking supplement with an unknown Booking ID -> ERROR
    METHODS u_booking_id_unknown         FOR TESTING RAISING cx_static_check.
    "! Try to update a booking supplement with an initial Booking ID -> ERROR
    METHODS u_booking_id_initial         FOR TESTING RAISING cx_static_check.
    "! Try to delete a booking supplement with an unknown Booking ID -> ERROR
    METHODS d_booking_id_unknown         FOR TESTING RAISING cx_static_check.
    "! Try to delete a booking supplement with an initial Booking ID -> ERROR
    METHODS d_booking_id_initial         FOR TESTING RAISING cx_static_check.

    "! Try to update a booking supplement with an unknown Booking Supplement ID -> ERROR
    METHODS u_booking_suppl_id_unknown   FOR TESTING RAISING cx_static_check.
    "! Try to update a booking supplement with an initial Booking Supplement ID -> ERROR
    METHODS u_booking_suppl_id_initial   FOR TESTING RAISING cx_static_check.
    "! Try to delete a booking supplement with an unknown Booking Supplement ID -> ERROR
    METHODS d_booking_suppl_id_unknown   FOR TESTING RAISING cx_static_check.
    "! Try to delete a booking supplement with an initial Booking Supplement ID -> ERROR
    METHODS d_booking_suppl_id_initial   FOR TESTING RAISING cx_static_check.

    "! Create booking supplement, delete father booking in same LUW -> NO Error, booking supplement ignored
    METHODS c_bsuppl_d_booking_1_luw     FOR TESTING RAISING cx_static_check.
    "! Create booking supplement, delete father travel  in same LUW -> NO Error, booking supplement ignored
    METHODS c_bsuppl_d_travel_1_luw      FOR TESTING RAISING cx_static_check.
    "! Try to delete father travel,  create booking supplement in same LUW -> ERROR
    METHODS d_travel_c_bsuppl_1_luw      FOR TESTING RAISING cx_static_check.
    "! Try to delete father booking, create booking supplement in same LUW -> ERROR
    METHODS d_booking_c_bsuppl_1_luw     FOR TESTING RAISING cx_static_check.
    "! Try to delete, update a single booking in the same LUW -> ERROR
    METHODS du_single                    FOR TESTING RAISING cx_static_check.
    "! Try to delete, delete a single booking in the same LUW -> NO Error
    METHODS dd_single                    FOR TESTING RAISING cx_static_check.

    "! Try to create a Booking Supplement with an unknown supplement ID -> ERROR
    METHODS c_supplement_unknown         FOR TESTING RAISING cx_static_check.

    "! Try to create a Booking Supplement with an initial Booking Supplement ID -> ERROR
    METHODS c_booking_suppl_id_initial  FOR TESTING RAISING cx_static_check.
    "! Try to create a Booking Supplement with a known combination of Travel ID, Booking ID, Booking Supplement ID -> ERROR
    METHODS c_booking_suppl_id_exists   FOR TESTING RAISING cx_static_check.

    "! Do a deep insert involving all 3 levels
    METHODS c_deep_insert               FOR TESTING RAISING cx_static_check.
    "! Try to do a deep insert with a faulty booking supplement -> ERROR (nothing is created)
    METHODS c_deep_insert_suppl_unknown FOR TESTING RAISING cx_static_check.
    "! Do a complicated update in a single call
    METHODS u_single_travel_multiple    FOR TESTING RAISING cx_static_check.

    METHODS read_db                     FOR TESTING RAISING cx_static_check.
    METHODS read_buffer                 FOR TESTING RAISING cx_static_check.
    METHODS read_travel_id_initial      FOR TESTING RAISING cx_static_check.

    "! Try to change the supplement ID -> ERROR
    METHODS u_supplement_id             FOR TESTING RAISING cx_static_check.

    "! Check that price and currency are derived / not derived
    METHODS c_price_currency            FOR TESTING RAISING cx_static_check.
    "! Try to update price only or currency only -> ERROR
    METHODS u_price_currency            FOR TESTING RAISING cx_static_check.
    "! Try to create a booking supplement with an unknown currency code -> ERROR
    METHODS c_currency_code_unknown  FOR TESTING RAISING cx_static_check.
    "! Try to update a booking supplement with an unknown currency code -> ERROR
    METHODS u_currency_code_unknown  FOR TESTING RAISING cx_static_check.

    METHODS _create_booking_suppl IMPORTING iv_travel_id                 TYPE /dmo/travel_id
                                            is_booking_supplement        TYPE /dmo/s_booking_supplement_in
                                            iv_save                      TYPE abap_bool DEFAULT abap_true
                                  RETURNING VALUE(rs_booking_supplement) TYPE /dmo/book_suppl.

    "! Checks if a travel_id is drawn in late numbering mode and skipped in early and the booking_id + booking_supplement_id is copied
    METHODS adjust_numbers              FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS /dmo/cl_flight_legacy DEFINITION LOCAL FRIENDS ltc_booking_supplement.

CLASS ltc_booking_supplement IMPLEMENTATION.
  METHOD class_setup.
    IF mc_use_sql_doubles = abap_true ##BOOL_OK.
*     gs_supplement_1 = VALUE #( supplement_id = 'BV-0001'  price = '2.30'  currency_code = 'EUR' ) ##LITERAL.
*     gs_supplement_2 = VALUE #( supplement_id = 'BV-0002'  price = '7.50'  currency_code = 'EUR' ) ##LITERAL.
*
*     DATA lt_supplement TYPE STANDARD TABLE OF /dmo/supplement.
*     lt_supplement = VALUE #( ( supplement_id = gs_supplement_1-supplement_id  price = gs_supplement_1-price  currency_code = gs_supplement_1-currency_code )
*                              ( supplement_id = gs_supplement_2-supplement_id  price = gs_supplement_2-price  currency_code = gs_supplement_2-currency_code ) ).
*     mr_test_environment->insert_test_data( lt_supplement ).
*
*     gv_supplement_id_unknown      = 'XX-999'.
    ELSE.
      " Select any valid combination of Supplement ID, Price, Currency Code
      SELECT SINGLE supplement_id, price, currency_code FROM /dmo/supplement INTO ( @gs_supplement_1-supplement_id, @gs_supplement_1-price, @gs_supplement_1-currency_code ) ##WARN_OK. "#EC CI_NOORDER
      IF sy-subrc <> 0.
        cl_abap_unit_assert=>abort( 'No supplement data!' ).
      ENDIF.

      " Select a different valid combination of Supplement ID, Price, Currency Code
      SELECT SINGLE supplement_id, price, currency_code FROM /dmo/supplement WHERE supplement_id <> @gs_supplement_1-supplement_id
        INTO ( @gs_supplement_2-supplement_id, @gs_supplement_2-price, @gs_supplement_2-currency_code ) ##WARN_OK. "#EC CI_NOORDER
      IF sy-subrc <> 0.
        cl_abap_unit_assert=>abort( 'No supplement data!' ).
      ENDIF.
      cl_abap_unit_assert=>assert_differs( act = gs_supplement_1-supplement_id  exp = gs_supplement_2-supplement_id ).

      " Determine an unknown Supplement ID
      gv_supplement_id_unknown = 'XX-999'.
      SELECT SINGLE supplement_id FROM /dmo/supplement WHERE supplement_id = @gv_supplement_id_unknown INTO @DATA(lv_dummy_supplement_id).
      IF sy-subrc = 0.
        cl_abap_unit_assert=>abort( |Supplement ID { gv_supplement_id_unknown } should not be known!| ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD setup.
    mv_booking_id = _create_booking( iv_travel_id = mv_travel_id
                                     is_booking   = VALUE #( booking_id    = '10'
                                                             booking_date  = gv_booking_date
                                                             customer_id   = gv_customer_id_1
                                                             carrier_id    = gs_flight_1-carrier_id
                                                             connection_id = gs_flight_1-connection_id
                                                             flight_date   = gs_flight_1-flight_date ) )-booking_id.
    mv_booking_id_unknown = '20'.
  ENDMETHOD.


  METHOD c_u_d_multiple.
    DATA lv_db_exists TYPE abap_bool.

    " 1. LUW:
    " Create a travel
    DATA(ls_travel) = _create_travel( is_travel = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' )
                                      iv_save   = abap_false ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel-travel_id ).

    " Create 2 bookings for the new travel
    DATA(ls_booking_1) = _create_booking( iv_travel_id = ls_travel-travel_id
                                          is_booking   = VALUE #( booking_id    = '20'
                                                                  booking_date  = gv_booking_date
                                                                  customer_id   = gv_customer_id_1
                                                                  carrier_id    = gs_flight_1-carrier_id
                                                                  connection_id = gs_flight_1-connection_id
                                                                  flight_date   = gs_flight_1-flight_date )
                                          iv_save      = abap_false ).
    DATA(ls_booking_2) = _create_booking( iv_travel_id = ls_travel-travel_id
                                          is_booking   = VALUE #( booking_id    = '21'
                                                                  booking_date  = gv_booking_date
                                                                  customer_id   = gv_customer_id_1
                                                                  carrier_id    = gs_flight_1-carrier_id
                                                                  connection_id = gs_flight_1-connection_id
                                                                  flight_date   = gs_flight_1-flight_date )
                                          iv_save      = abap_false ).

    " Create a single booking supplement for the first booking
    _create_booking_suppl( iv_travel_id          = ls_booking_1-travel_id
                           is_booking_supplement = VALUE #( booking_id            = ls_booking_1-booking_id
                                                            booking_supplement_id = '10'
                                                            supplement_id         = gs_supplement_1-supplement_id
                                                            price                 = '10.0'
                                                            currency_code         = 'EUR' )
                           iv_save               = abap_false ).

    " Create 2 booking supplements for the second booking
    DATA(ls_book_suppl_2_1) = _create_booking_suppl( iv_travel_id          = ls_booking_2-travel_id
                                                     is_booking_supplement = VALUE #( booking_id            = ls_booking_2-booking_id
                                                                                      booking_supplement_id = '10'
                                                                                      supplement_id         = gs_supplement_1-supplement_id
                                                                                      price                 = '20.0'
                                                                                      currency_code         = 'EUR' )
                                                     iv_save               = abap_false ).
    DATA(ls_book_suppl_2_2) = _create_booking_suppl( iv_travel_id          = ls_booking_2-travel_id
                                                     is_booking_supplement = VALUE #( booking_id            = ls_booking_2-booking_id
                                                                                      booking_supplement_id = '20'
                                                                                      supplement_id         = gs_supplement_1-supplement_id
                                                                                      price                 = '30.0'
                                                                                      currency_code         = 'EUR' )
                                                     iv_save               = abap_false ).

    gr_cut->save( ).

    SELECT COUNT( * ) FROM /dmo/book_suppl WHERE travel_id = @ls_travel-travel_id INTO @DATA(lv_count).
    cl_abap_unit_assert=>assert_equals( act = lv_count  exp = 3 ).

    DATA ls_book_suppl_sel TYPE /dmo/book_suppl.

    SELECT SINGLE price, currency_code FROM /dmo/book_suppl WHERE travel_id             = @ls_book_suppl_2_1-travel_id
                                                              AND booking_id            = @ls_book_suppl_2_1-booking_id
                                                              AND booking_supplement_id = @ls_book_suppl_2_1-booking_supplement_id
                                                            INTO CORRESPONDING FIELDS OF @ls_book_suppl_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_book_suppl_sel-price          exp = '20.0' ).
    cl_abap_unit_assert=>assert_equals( act = ls_book_suppl_sel-currency_code  exp = 'EUR' ).

    " 2. Update the first booking supplement of the second booking
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = ls_book_suppl_2_1-travel_id )
                                     is_travelx             = VALUE #( travel_id = ls_book_suppl_2_1-travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = ls_book_suppl_2_1-booking_id
                                                                         booking_supplement_id = ls_book_suppl_2_1-booking_supplement_id
                                                                         price                 = '21.0'
                                                                         currency_code         = 'USD'
                                                                         ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = ls_book_suppl_2_1-booking_id
                                                                         booking_supplement_id = ls_book_suppl_2_1-booking_supplement_id
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-update
                                                                         price                 = abap_true
                                                                         currency_code         = abap_true ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    " Check that record already in update buffer is properly updated
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = ls_book_suppl_2_1-travel_id )
                                     is_travelx             = VALUE #( travel_id = ls_book_suppl_2_1-travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = ls_book_suppl_2_1-booking_id
                                                                         booking_supplement_id = ls_book_suppl_2_1-booking_supplement_id
                                                                         price                 = '22.0'
                                                                         currency_code         = 'EUR' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = ls_book_suppl_2_1-booking_id
                                                                         booking_supplement_id = ls_book_suppl_2_1-booking_supplement_id
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-update
                                                                         price                 = abap_true
                                                                         currency_code         = abap_true ) )
                           IMPORTING et_booking_supplement  = DATA(lt_booking_supplement)
                                     et_messages            = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking_supplement )  exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_booking_supplement[ 1 ]-travel_id              exp = ls_book_suppl_2_1-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lt_booking_supplement[ 1 ]-booking_id             exp = ls_book_suppl_2_1-booking_id ).
    cl_abap_unit_assert=>assert_equals( act = lt_booking_supplement[ 1 ]-booking_supplement_id  exp = ls_book_suppl_2_1-booking_supplement_id ).
    cl_abap_unit_assert=>assert_equals( act = lt_booking_supplement[ 1 ]-price                  exp = '22.0' ).
    cl_abap_unit_assert=>assert_equals( act = lt_booking_supplement[ 1 ]-currency_code          exp = 'EUR' ).

    gr_cut->save( ).

    SELECT SINGLE price, currency_code FROM /dmo/book_suppl WHERE travel_id             = @ls_book_suppl_2_1-travel_id
                                                              AND booking_id            = @ls_book_suppl_2_1-booking_id
                                                              AND booking_supplement_id = @ls_book_suppl_2_1-booking_supplement_id
                                                            INTO CORRESPONDING FIELDS OF @ls_book_suppl_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_book_suppl_sel-price          exp = '22.0' ).
    cl_abap_unit_assert=>assert_equals( act = ls_book_suppl_sel-currency_code  exp = 'EUR' ).

    SELECT SINGLE price, currency_code FROM /dmo/book_suppl WHERE travel_id             = @ls_book_suppl_2_2-travel_id
                                                              AND booking_id            = @ls_book_suppl_2_2-booking_id
                                                              AND booking_supplement_id = @ls_book_suppl_2_2-booking_supplement_id
                                                            INTO CORRESPONDING FIELDS OF @ls_book_suppl_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_book_suppl_sel-price          exp = '30.0' ).
    cl_abap_unit_assert=>assert_equals( act = ls_book_suppl_sel-currency_code  exp = 'EUR' ).

    " 3. Delete the first booking - check that the booking supplement has also been deleted
    _delete_existing_booking( iv_travel_id = ls_booking_1-travel_id  iv_booking_id = ls_booking_1-booking_id ).

    SELECT COUNT( * ) FROM /dmo/book_suppl WHERE travel_id = @ls_travel-travel_id INTO @lv_count.
    cl_abap_unit_assert=>assert_equals( act = lv_count  exp = 2 ).

    SELECT COUNT( * ) FROM /dmo/book_suppl WHERE travel_id = @ls_travel-travel_id AND booking_id = @ls_booking_2-booking_id INTO @lv_count.
    cl_abap_unit_assert=>assert_equals( act = lv_count  exp = 2 ).

    " 4. LUW: Delete the second booking supplement of the second booking - here check also travel admin field
    SELECT SINGLE lastchangedat FROM /dmo/travel WHERE travel_id = @ls_travel-travel_id INTO @DATA(lv_lastchangedat_1).
    cl_abap_unit_assert=>assert_subrc( ).

    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/book_suppl FIELDS @abap_true WHERE travel_id             = @ls_book_suppl_2_2-travel_id
                                                           AND booking_id            = @ls_book_suppl_2_2-booking_id
                                                           AND booking_supplement_id = @ls_book_suppl_2_2-booking_supplement_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_true( lv_db_exists ).

    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = ls_book_suppl_2_2-travel_id )
                                     is_travelx             = VALUE #( travel_id = ls_book_suppl_2_2-travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = ls_book_suppl_2_2-booking_id
                                                                         booking_supplement_id = ls_book_suppl_2_2-booking_supplement_id ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = ls_book_suppl_2_2-booking_id
                                                                         booking_supplement_id = ls_book_suppl_2_2-booking_supplement_id
                                                                         action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages            = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->save( ).

    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/book_suppl FIELDS @abap_true WHERE travel_id             = @ls_book_suppl_2_2-travel_id
                                                           AND booking_id            = @ls_book_suppl_2_2-booking_id
                                                           AND booking_supplement_id = @ls_book_suppl_2_2-booking_supplement_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).

    SELECT SINGLE lastchangedat FROM /dmo/travel WHERE travel_id = @ls_travel-travel_id INTO @DATA(lv_lastchangedat_2).
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_true( xsdbool( lv_lastchangedat_2 > lv_lastchangedat_1 ) ).

    SELECT COUNT( * ) FROM /dmo/book_suppl WHERE travel_id = @ls_travel-travel_id AND booking_id = @ls_booking_2-booking_id INTO @lv_count.
    cl_abap_unit_assert=>assert_equals( act = lv_count  exp = 1 ).

    " 5. LUW: Delete the travel - check that all booking (supplements) have also been deleted
    _delete_existing_travel( ls_travel-travel_id ).

    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/booking FIELDS @abap_true WHERE travel_id = @ls_travel-travel_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).

    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/book_suppl FIELDS @abap_true WHERE travel_id = @ls_travel-travel_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD c_travel_id_unknown.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id_unknown )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id_unknown )
                                     it_booking_supplement  = VALUE #( ( booking_id            = '1'
                                                                         booking_supplement_id = '1'
                                                                         price                 = '10.0'
                                                                         currency_code         = 'EUR' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = '1'
                                                                         booking_supplement_id = '1'
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking_supplement  = DATA(lt_booking_supplement)
                                     et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking_supplement ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id_unknown ).
  ENDMETHOD.


  METHOD c_travel_id_initial.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( )
                                     is_travelx             = VALUE #( )
                                     it_booking_supplement  = VALUE #( ( booking_id            = '1'
                                                                         booking_supplement_id = '1'
                                                                         price                 = '10.0'
                                                                         currency_code         = 'EUR' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = '1'
                                                                         booking_supplement_id = '1'
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking_supplement  = DATA(lt_booking_supplement)
                                     et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking_supplement ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_no_key-msgno ).
  ENDMETHOD.


  METHOD u_travel_id_unknown.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id_unknown )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id_unknown )
                                     it_booking_supplement  = VALUE #( ( booking_id = '2'  price = '11.0' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id = '2'  action_code = /dmo/if_flight_legacy=>action_code-update  price = abap_true ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id_unknown ).
  ENDMETHOD.


  METHOD u_travel_id_initial.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( )
                                     is_travelx             = VALUE #( )
                                     it_booking_supplement  = VALUE #( ( booking_id = '2'  booking_supplement_id = '1'  price = '11.0' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id = '2'  booking_supplement_id = '1'  action_code = /dmo/if_flight_legacy=>action_code-update  price = abap_true ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_no_key-msgno ).
  ENDMETHOD.


  METHOD d_travel_id_unknown.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id_unknown )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id_unknown )
                                     it_booking_supplement  = VALUE #( ( booking_id            = '1'
                                                                         booking_supplement_id = '1' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = '1'
                                                                         booking_supplement_id = '1'
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id_unknown ).
  ENDMETHOD.


  METHOD d_travel_id_initial.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( )
                                     is_travelx             = VALUE #( )
                                     it_booking_supplement  = VALUE #( ( booking_id = '1'  booking_supplement_id = '1' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id = '1'  booking_supplement_id = '1'  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_no_key-msgno ).
  ENDMETHOD.


  METHOD c_booking_id_unknown.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = mv_booking_id_unknown
                                                                         booking_supplement_id = '1'
                                                                         price                 = '10.0'
                                                                         currency_code         = 'EUR' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = mv_booking_id_unknown
                                                                         booking_supplement_id = '1'
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking_supplement  = DATA(lt_booking_supplement)
                                     et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking_supplement ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = mv_booking_id_unknown ).
  ENDMETHOD.


  METHOD c_booking_id_initial.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_supplement_id = '1' ) )
                                     it_booking_supplementx = VALUE #( ( booking_supplement_id = '1'  action_code = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking_supplement  = DATA(lt_booking_supplement)
                                     et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking_supplement ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_no_key-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
  ENDMETHOD.


  METHOD u_booking_id_unknown.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id = mv_booking_id_unknown  booking_supplement_id = '1'  price = '11.0' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id = mv_booking_id_unknown  booking_supplement_id = '1'
                                                                         action_code = /dmo/if_flight_legacy=>action_code-update  price = abap_true ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = mv_booking_id_unknown ).
  ENDMETHOD.


  METHOD u_booking_id_initial.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_supplement_id = '1'  price = '11.0' ) )
                                     it_booking_supplementx = VALUE #( ( booking_supplement_id = '1'  action_code = /dmo/if_flight_legacy=>action_code-update  price = abap_true ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_no_key-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
  ENDMETHOD.


  METHOD d_booking_id_unknown.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id = mv_booking_id_unknown  booking_supplement_id = '1' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id = mv_booking_id_unknown  booking_supplement_id = '1'  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = mv_booking_id_unknown ).
  ENDMETHOD.


  METHOD d_booking_id_initial.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_supplement_id = '1' ) )
                                     it_booking_supplementx = VALUE #( ( booking_supplement_id = '1'  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_no_key-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
  ENDMETHOD.


  METHOD u_booking_suppl_id_unknown.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id = mv_booking_id  booking_supplement_id = '10'  price = '11.0' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id = mv_booking_id  booking_supplement_id = '10'  action_code = /dmo/if_flight_legacy=>action_code-update  price = abap_true ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_supplement_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr3  exp = 'MV_BOOKING_SUPPLEMENT_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = mv_booking_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_supplement_id       exp = '10' ).
  ENDMETHOD.


  METHOD u_booking_suppl_id_initial.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id = mv_booking_id  price = '11.0' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id = mv_booking_id  action_code = /dmo/if_flight_legacy=>action_code-update  price = abap_true ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_supplement_no_key-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = mv_booking_id ).
  ENDMETHOD.


  METHOD d_booking_suppl_id_unknown.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id = mv_booking_id  booking_supplement_id = '10' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id = mv_booking_id  booking_supplement_id = '10'
                                                                         action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_supplement_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr3  exp = 'MV_BOOKING_SUPPLEMENT_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = mv_booking_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_supplement_id       exp = '10' ).
  ENDMETHOD.


  METHOD d_booking_suppl_id_initial.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id = mv_booking_id ) )
                                     it_booking_supplementx = VALUE #( ( booking_id = mv_booking_id  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_supplement_no_key-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = mv_booking_id ).
  ENDMETHOD.


  METHOD c_bsuppl_d_booking_1_luw.
    DATA lv_db_exists TYPE abap_bool.

    " 1. LUW: Create: Travel - Booking
    DATA(ls_travel) = _create_travel( is_travel = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' )
                                      iv_save   = abap_false ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel-travel_id ).
    DATA(ls_booking) = _create_booking( iv_travel_id = ls_travel-travel_id
                                        is_booking   = VALUE #( booking_id    = '10'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date )
                                        iv_save      = abap_true ).

    " 2. LUW: Create Booking Supplement, Delete father Booking
    DATA(ls_booking_supplement) = _create_booking_suppl( iv_travel_id          = ls_booking-travel_id
                                                         is_booking_supplement = VALUE #( booking_id            = ls_booking-booking_id
                                                                                          booking_supplement_id = '11'
                                                                                          supplement_id         = gs_supplement_1-supplement_id
                                                                                          price                 = '10.0'
                                                                                          currency_code         = 'EUR' )
                                                         iv_save               = abap_false ).
    _delete_existing_booking( iv_travel_id = ls_booking-travel_id  iv_booking_id = ls_booking-booking_id ).

    " Check that the Booking Supplement was not created
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/book_suppl FIELDS @abap_true WHERE travel_id             = @ls_booking_supplement-travel_id
                                                           AND booking_id            = @ls_booking_supplement-booking_id
                                                           AND booking_supplement_id = @ls_booking_supplement-booking_supplement_id
                                                         INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).

    _delete_existing_travel( ls_travel-travel_id ).
  ENDMETHOD.


  METHOD c_bsuppl_d_travel_1_luw.
    DATA lv_db_exists TYPE abap_bool.

    " 1. LUW: Create: Travel - Booking
    DATA(ls_travel) = _create_travel( is_travel = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' )
                                      iv_save   = abap_false ).
    DATA(ls_booking) = _create_booking( iv_travel_id = ls_travel-travel_id
                                        is_booking   = VALUE #( booking_id    = '11'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date )
                                        iv_save      = abap_true ).

    " 2. LUW: Create Booking Supplement, Delete father Travel
    DATA(ls_booking_supplement) = _create_booking_suppl( iv_travel_id          = ls_booking-travel_id
                                                         is_booking_supplement = VALUE #( booking_id            = ls_booking-booking_id
                                                                                          booking_supplement_id = '11'
                                                                                          supplement_id         = gs_supplement_1-supplement_id
                                                                                          price                 = '10.0'
                                                                                          currency_code         = 'EUR' )
                                                         iv_save               = abap_false ).
    _delete_existing_travel( ls_travel-travel_id ).

    " Check that the Booking Supplement was not created
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/book_suppl FIELDS @abap_true WHERE travel_id             = @ls_booking_supplement-travel_id
                                                           AND booking_id            = @ls_booking_supplement-booking_id
                                                           AND booking_supplement_id = @ls_booking_supplement-booking_supplement_id
                                                         INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD d_travel_c_bsuppl_1_luw.
    " 1. LUW: Create: Travel - Booking
    DATA(ls_travel) = _create_travel( is_travel = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' )
                                      iv_save   = abap_false ).
    DATA(ls_booking) = _create_booking( iv_travel_id = ls_travel-travel_id
                                        is_booking   = VALUE #( booking_id    = '10'
                                                                booking_date  = gv_booking_date
                                                                customer_id   = gv_customer_id_1
                                                                carrier_id    = gs_flight_1-carrier_id
                                                                connection_id = gs_flight_1-connection_id
                                                                flight_date   = gs_flight_1-flight_date )
                                        iv_save      = abap_true ).

    " 2. LUW: Delete Travel, try to create Booking Supplement
    gr_cut->delete_travel( EXPORTING iv_travel_id = ls_travel-travel_id
                           IMPORTING et_messages  = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = ls_booking-travel_id )
                                     is_travelx             = VALUE #( travel_id = ls_booking-travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = ls_booking-booking_id
                                                                         booking_supplement_id = '1'
                                                                         price                 = '10.0'
                                                                         currency_code         = 'EUR' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = ls_booking-booking_id
                                                                         booking_supplement_id = '1'
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking_supplement  = DATA(lt_booking_supplement)
                                     et_messages            = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_booking_supplement ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = ls_travel-travel_id ).
    gr_cut->save( ).
  ENDMETHOD.


  METHOD d_booking_c_bsuppl_1_luw.
    " Delete Booking, try to create Booking Supplement
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = mv_travel_id )
                                     is_travelx  = VALUE #( travel_id = mv_travel_id )
                                     it_booking  = VALUE #( ( booking_id = mv_booking_id ) )
                                     it_bookingx = VALUE #( ( booking_id = mv_booking_id  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '1'
                                                                         price                 = '10.0'
                                                                         currency_code         = 'EUR' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '1'
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking_supplement  = DATA(lt_booking_supplement)
                                     et_messages            = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_booking_supplement ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = mv_booking_id ).
  ENDMETHOD.


  METHOD du_single.
    DATA lv_db_exists TYPE abap_bool.

    DATA(ls_booking_supplement) = _create_booking_suppl( iv_travel_id          = mv_travel_id
                                                         is_booking_supplement = VALUE #( booking_id            = mv_booking_id
                                                                                          booking_supplement_id = '22'
                                                                                          supplement_id         = gs_supplement_1-supplement_id
                                                                                          price                 = '10.0'
                                                                                          currency_code         = 'EUR' )
                                                         iv_save               = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_supplement-travel_id  exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_supplement-booking_id  exp = mv_booking_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_supplement-booking_supplement_id  exp = '22' ).

    " Check existence of Booking Supplement
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/book_suppl FIELDS @abap_true WHERE travel_id = @mv_travel_id AND booking_id = @mv_booking_id AND booking_supplement_id = '22' INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_true( lv_db_exists ).

    " Delete, Update Booking Supplement in the same LUW
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     is_travelx             = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     is_travelx             = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id
                                                                         price                 = '11.0' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-update
                                                                         price                 = abap_true ) )
                           IMPORTING et_messages            = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_supplement_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr3  exp = 'MV_BOOKING_SUPPLEMENT_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = ls_booking_supplement-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = ls_booking_supplement-booking_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_supplement_id       exp = ls_booking_supplement-booking_supplement_id ).
    gr_cut->save( ).

    " Check that the Booking Supplement no longer exists
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/book_suppl FIELDS @abap_true WHERE travel_id             = @ls_booking_supplement-travel_id
                                                           AND booking_id            = @ls_booking_supplement-booking_id
                                                           AND booking_supplement_id = @ls_booking_supplement-booking_supplement_id
                                                         INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD dd_single.
    CONSTANTS lc_booking_supplement_id TYPE /dmo/booking_supplement_id VALUE '34'.
    DATA lv_db_exists TYPE abap_bool.

    _create_booking_suppl( iv_travel_id          = mv_travel_id
                           is_booking_supplement = VALUE #( booking_id            = mv_booking_id
                                                            booking_supplement_id = lc_booking_supplement_id
                                                            supplement_id         = gs_supplement_1-supplement_id
                                                            price                 = '10.0'
                                                            currency_code         = 'EUR' )
                           iv_save               = abap_true ).

    " Check existence of Booking Supplement
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/book_suppl FIELDS @abap_true WHERE travel_id = @mv_travel_id AND booking_id = @mv_booking_id AND booking_supplement_id = @lc_booking_supplement_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_true( lv_db_exists ).

    " Delete Booking Supplement twice in the same LUW
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = lc_booking_supplement_id ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = lc_booking_supplement_id
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = lc_booking_supplement_id ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = lc_booking_supplement_id
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages            = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    gr_cut->save( ).

    " Check that the Booking Supplement no longer exists
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/book_suppl FIELDS @abap_true WHERE travel_id = @mv_travel_id AND booking_id = @mv_booking_id AND booking_supplement_id = @lc_booking_supplement_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD c_supplement_unknown.
    DATA lv_db_exists TYPE abap_bool.

    " Try to create 2 Booking Supplements, one of them is faulty
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '1'
                                                                         supplement_id         = gv_supplement_id_unknown
                                                                         price                 = '10.0'
                                                                         currency_code         = 'EUR' )
                                                                       ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '2'
                                                                         supplement_id         = gs_supplement_1-supplement_id
                                                                         price                 = '10.0'
                                                                         currency_code         = 'EUR' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '1'
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-create )
                                                                       ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '2'
                                                                         supplement_id         = gs_supplement_1-supplement_id
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking_supplement  = DATA(lt_booking_supplement)
                                     et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking_supplement ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>supplement_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_SUPPLEMENT_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_supplement_id               exp = gv_supplement_id_unknown ).

    gr_cut->save( ).

    " Check that no Booking Supplement has been created
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/book_suppl FIELDS @abap_true WHERE travel_id  = @mv_travel_id AND booking_id = @mv_booking_id INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).
  ENDMETHOD.


  METHOD c_booking_suppl_id_initial.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '0'
                                                                         supplement_id         = gs_supplement_1-supplement_id
                                                                         price                 = gs_supplement_1-price
                                                                         currency_code         = gs_supplement_1-currency_code ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '0'
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking_supplement  = DATA(lt_booking_supplement)
                                     et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking_supplement ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_supplement_no_key-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = mv_booking_id ).
  ENDMETHOD.


  METHOD c_booking_suppl_id_exists.
    _create_booking_suppl( iv_travel_id          = mv_travel_id
                           is_booking_supplement = VALUE #( booking_id            = mv_booking_id
                                                            booking_supplement_id = '20'
                                                            supplement_id         = gs_supplement_1-supplement_id
                                                            price                 = gs_supplement_1-price
                                                            currency_code         = gs_supplement_1-currency_code )
                           iv_save               = abap_false ).

    " Try to create a booking supplement that is already in the buffer
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '20'
                                                                         supplement_id         = gs_supplement_1-supplement_id
                                                                         price                 = gs_supplement_1-price
                                                                         currency_code         = gs_supplement_1-currency_code ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '20'
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking_supplement  = DATA(lt_booking_supplement)
                                     et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_booking_supplement ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_supplement_exists-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr3  exp = 'MV_BOOKING_SUPPLEMENT_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = mv_booking_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_supplement_id       exp = '20' ).

    gr_cut->save( ).

    " Try to create a booking supplement that is already in the database
    CLEAR lt_messages.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '20'
                                                                         supplement_id         = gs_supplement_1-supplement_id
                                                                         price                 = gs_supplement_1-price
                                                                         currency_code         = gs_supplement_1-currency_code ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '20'
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking_supplement  = lt_booking_supplement
                                     et_messages            = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_booking_supplement ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_supplement_exists-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr3  exp = 'MV_BOOKING_SUPPLEMENT_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = mv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = mv_booking_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_supplement_id       exp = '20' ).
  ENDMETHOD.


  METHOD c_deep_insert.
    CONSTANTS lc_booking_id_1 TYPE /dmo/booking_id VALUE '20'.
    CONSTANTS lc_booking_id_2 TYPE /dmo/booking_id VALUE '21'.
    CONSTANTS lc_booking_supplement_id_1_1 TYPE /dmo/booking_supplement_id VALUE '10'.
    CONSTANTS lc_booking_supplement_id_2_1 TYPE /dmo/booking_supplement_id VALUE '10'.
    CONSTANTS lc_booking_supplement_id_2_2 TYPE /dmo/booking_supplement_id VALUE '20'.
    gr_cut->create_travel(
      EXPORTING
        is_travel             = VALUE #( agency_id     = gv_agency_id_1
                                         customer_id   = gv_customer_id_1
                                         begin_date    = '20190101'
                                         end_date      = '20190201'
                                         description   = 'My_Deep_Insert'
                                         total_price   = '999.99' "Derivated, so ignored
                                         booking_fee   = '10.00'
                                         currency_code = 'EUR' )
        it_booking            = VALUE #( ( booking_id    = lc_booking_id_1
                                           booking_date  = gv_booking_date
                                           customer_id   = gv_customer_id_1
                                           carrier_id    = gs_flight_1-carrier_id
                                           connection_id = gs_flight_1-connection_id
                                           flight_date   = gs_flight_1-flight_date
                                           flight_price  = '100.00'
                                           currency_code = 'EUR' )
                                         ( booking_id    = lc_booking_id_2
                                           booking_date  = gv_booking_date
                                           customer_id   = gv_customer_id_2
                                           carrier_id    = gs_flight_2-carrier_id
                                           connection_id = gs_flight_2-connection_id
                                           flight_date   = gs_flight_2-flight_date
                                           flight_price  = '200.00'
                                           currency_code = 'EUR' ) )
         it_booking_supplement = VALUE #( ( booking_id            = lc_booking_id_1
                                            booking_supplement_id = lc_booking_supplement_id_1_1
                                            supplement_id         = gs_supplement_1-supplement_id
                                            price                 = '10.0'
                                            currency_code         = 'EUR' )
                                          ( booking_id            = lc_booking_id_2
                                            booking_supplement_id = lc_booking_supplement_id_2_1
                                            supplement_id         = gs_supplement_1-supplement_id
                                            price                 = '20.0'
                                            currency_code         = 'EUR' )
                                          ( booking_id            = lc_booking_id_2
                                            booking_supplement_id = lc_booking_supplement_id_2_2
                                            supplement_id         = gs_supplement_1-supplement_id
                                            price                 = '30.0'
                                            currency_code         = 'USD' ) )
      IMPORTING
         es_travel             = DATA(ls_travel)
         et_booking            = DATA(lt_booking)
         et_booking_supplement = DATA(lt_booking_supplement)
         et_messages           = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking )  exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking_supplement )  exp = 3 ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_booking[ travel_id = ls_travel-travel_id  booking_id = lc_booking_id_1 ] ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_booking[ travel_id = ls_travel-travel_id  booking_id = lc_booking_id_2 ] ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_booking_supplement[ travel_id = ls_travel-travel_id  booking_id = lc_booking_id_1  booking_supplement_id = lc_booking_supplement_id_1_1 ] ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_booking_supplement[ travel_id = ls_travel-travel_id  booking_id = lc_booking_id_2  booking_supplement_id = lc_booking_supplement_id_2_1 ] ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_booking_supplement[ travel_id = ls_travel-travel_id  booking_id = lc_booking_id_2  booking_supplement_id = lc_booking_supplement_id_2_2 ] ) ) ).

    gr_cut->save( ).

    " Check travel
    DATA(lv_exchange_rate_date) = cl_abap_context_info=>get_system_date( ).
    /dmo/cl_flight_amdp=>convert_currency(
      EXPORTING
        iv_amount               = '30.00'
        iv_currency_code_source = 'USD'
        iv_currency_code_target = 'EUR'
        iv_exchange_rate_date   = lv_exchange_rate_date
      IMPORTING
        ev_amount               = DATA(lv_30_usd_as_eur)
    ).
    cl_abap_unit_assert=>assert_not_initial( lv_30_usd_as_eur ).
    DATA ls_travel_sel TYPE /dmo/travel.
    SELECT SINGLE agency_id, customer_id, total_price, currency_code, description FROM /dmo/travel WHERE travel_id = @ls_travel-travel_id INTO CORRESPONDING FIELDS OF @ls_travel_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-agency_id      exp = gv_agency_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-customer_id    exp = gv_customer_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-total_price    exp = '10.00' + '100.00' + '200.00' + '10.00' + '20.00' + lv_30_usd_as_eur ) ##LITERAL.
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-currency_code  exp = 'EUR' ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-description    exp = 'My_Deep_Insert' ).

    " Check booking count, Check one of the bookings
    SELECT COUNT( * ) FROM /dmo/booking WHERE travel_id = @ls_travel-travel_id INTO @DATA(lv_count).
    cl_abap_unit_assert=>assert_equals( act = lv_count  exp = 2 ).
    DATA ls_booking_sel TYPE /dmo/booking.
    SELECT SINGLE customer_id, flight_date FROM /dmo/booking WHERE travel_id  = @ls_travel-travel_id
                                                               AND booking_id = @lc_booking_id_2
                                                             INTO CORRESPONDING FIELDS OF @ls_booking_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-customer_id  exp = gv_customer_id_2 ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-flight_date  exp = gs_flight_2-flight_date ).

    " Check booking supplement count, Check one of the booking supplements
    lv_count = 0.
    SELECT COUNT( * ) FROM /dmo/book_suppl WHERE travel_id = @ls_travel-travel_id INTO @lv_count.
    cl_abap_unit_assert=>assert_equals( act = lv_count  exp = 3 ).
    DATA ls_book_suppl_sel TYPE /dmo/book_suppl.
    SELECT SINGLE price, currency_code FROM /dmo/book_suppl WHERE travel_id             = @ls_travel-travel_id
                                                              AND booking_id            = @lc_booking_id_2
                                                              AND booking_supplement_id = @lc_booking_supplement_id_2_1
                                                            INTO CORRESPONDING FIELDS OF @ls_book_suppl_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_book_suppl_sel-price          exp = '20.0' ).
    cl_abap_unit_assert=>assert_equals( act = ls_book_suppl_sel-currency_code  exp = 'EUR' ).

    _delete_existing_travel( ls_travel-travel_id ).
  ENDMETHOD.


  METHOD c_deep_insert_suppl_unknown.
    " Prepare travel in buffer without save
    DATA(ls_travel) = _create_travel( is_travel = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' )
                                      iv_save   = abap_false ).

    " Now try to put a second travel with a faulty booking supplement into the buffer
    CONSTANTS lc_booking_id_1 TYPE /dmo/booking_id VALUE '20'.
    CONSTANTS lc_booking_id_2 TYPE /dmo/booking_id VALUE '21'.
    CONSTANTS lc_booking_supplement_id_2_1 TYPE /dmo/booking_supplement_id VALUE '10'.
    gr_cut->create_travel(
      EXPORTING
        is_travel             = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201'  description = 'My_Deep_Insert_Fail' )
        it_booking            = VALUE #( ( booking_id    = lc_booking_id_1
                                           booking_date  = gv_booking_date
                                           customer_id   = gv_customer_id_1
                                           carrier_id    = gs_flight_1-carrier_id
                                           connection_id = gs_flight_1-connection_id
                                           flight_date   = gs_flight_1-flight_date )
                                         ( booking_id    = lc_booking_id_2
                                           booking_date  = gv_booking_date
                                           customer_id   = gv_customer_id_2
                                           carrier_id    = gs_flight_2-carrier_id
                                           connection_id = gs_flight_2-connection_id
                                           flight_date   = gs_flight_2-flight_date ) )
         it_booking_supplement = VALUE #( ( booking_id            = lc_booking_id_2
                                            booking_supplement_id = lc_booking_supplement_id_2_1
                                            supplement_id         = gv_supplement_id_unknown " <<< Unknown Supplement ID
                                            price                 = '20.0'
                                            currency_code         = 'EUR' ) )
      IMPORTING
         es_travel             = DATA(ls_travel_fail)
         et_booking            = DATA(lt_booking)
         et_booking_supplement = DATA(lt_booking_supplement)
         et_messages           = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( ls_travel_fail-travel_id ).
    cl_abap_unit_assert=>assert_initial( lt_booking ).
    cl_abap_unit_assert=>assert_initial( lt_booking_supplement ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>supplement_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_SUPPLEMENT_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_supplement_id               exp = gv_supplement_id_unknown ).

    gr_cut->save( ).

    " Delete the first travel, this would fail when first travel had not been created
    _delete_existing_travel( ls_travel-travel_id ).
  ENDMETHOD.


  METHOD u_single_travel_multiple.
    DATA lv_db_exists TYPE abap_bool.

    " Create and persis a travel with 1 booking and 3 booking supplements
    CONSTANTS lc_booking_id_1 TYPE /dmo/booking_id VALUE '20'.
    CONSTANTS lc_booking_supplement_id_1_1 TYPE /dmo/booking_supplement_id VALUE '10'.
    CONSTANTS lc_booking_supplement_id_1_2 TYPE /dmo/booking_supplement_id VALUE '20'.
    CONSTANTS lc_booking_supplement_id_1_3 TYPE /dmo/booking_supplement_id VALUE '30'.
    gr_cut->create_travel(
      EXPORTING
        is_travel             = VALUE #( agency_id     = gv_agency_id_1
                                         customer_id   = gv_customer_id_1
                                         begin_date    = '20190101'
                                         end_date      = '20190201'
                                         description   = 'My_Deep_Insert_1' )
        it_booking            = VALUE #( ( booking_id    = lc_booking_id_1
                                           booking_date  = gv_booking_date
                                           customer_id   = gv_customer_id_1
                                           carrier_id    = gs_flight_1-carrier_id
                                           connection_id = gs_flight_1-connection_id
                                           flight_date   = gs_flight_1-flight_date
                                           flight_price  = '100.00'
                                           currency_code = 'EUR' ) )
         it_booking_supplement = VALUE #( ( booking_id            = lc_booking_id_1
                                            booking_supplement_id = lc_booking_supplement_id_1_1
                                            supplement_id         = gs_supplement_1-supplement_id
                                            price                 = '10.0'
                                            currency_code         = 'EUR' )
                                          ( booking_id            = lc_booking_id_1
                                            booking_supplement_id = lc_booking_supplement_id_1_2
                                            supplement_id         = gs_supplement_1-supplement_id
                                            price                 = '20.0'
                                            currency_code         = 'EUR' )
                                          ( booking_id            = lc_booking_id_1
                                            booking_supplement_id = lc_booking_supplement_id_1_3
                                            supplement_id         = gs_supplement_1-supplement_id
                                            price                 = '30.0'
                                            currency_code         = 'EUR' ) )
      IMPORTING
         es_travel             = DATA(ls_travel)
         et_booking            = DATA(lt_booking)
         et_booking_supplement = DATA(lt_booking_supplement)
         et_messages           = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking )  exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking_supplement )  exp = 3 ).
    DATA(ls_booking_supplement_1_1) = lt_booking_supplement[ travel_id = ls_travel-travel_id  booking_id = lc_booking_id_1  booking_supplement_id = lc_booking_supplement_id_1_1 ].
    gr_cut->save( ).

    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/book_suppl FIELDS @abap_true WHERE travel_id             = @ls_travel-travel_id
                                                           AND booking_id            = @lc_booking_id_1
                                                           AND booking_supplement_id = @lc_booking_supplement_id_1_2
                                                         INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_true( lv_db_exists ).

    " Now do (and save) a complicated update:
    " -- Change a travel attribute
    " -- Change a booking attribute
    " -- Change a booking supplement attribute
    " -- Create a new booking
    " -- Delete a Booking Supplement
    " -- Create a Booking Supplement for an existing Booking
    " -- Create a new Booking Supplement for the new Booking
    CONSTANTS lc_booking_id_2 TYPE /dmo/booking_id VALUE '10'.
    CONSTANTS lc_booking_supplement_id_2_1 TYPE /dmo/booking_supplement_id VALUE '10'.
    CONSTANTS lc_booking_supplement_id_1_4 TYPE /dmo/booking_supplement_id VALUE '40'.
    CONSTANTS lc_diff TYPE /dmo/supplement_price VALUE '123.00'.
    DATA lv_new_price TYPE /dmo/supplement_price.
    lv_new_price = ls_booking_supplement_1_1-price + lc_diff.
    gr_cut->update_travel(
      EXPORTING
        is_travel              = VALUE #( travel_id = ls_travel-travel_id  description = 'My_Deep_Insert_2' )
        is_travelx             = VALUE #( travel_id = ls_travel-travel_id  description = abap_true )
        it_booking             = VALUE #( ( booking_id = lc_booking_id_1  customer_id = gv_customer_id_2 )
                                          ( booking_id    = lc_booking_id_2
                                            booking_date  = gv_booking_date
                                            customer_id   = gv_customer_id_1
                                            carrier_id    = gs_flight_2-carrier_id
                                            connection_id = gs_flight_2-connection_id
                                            flight_date   = gs_flight_2-flight_date
                                            flight_price  = '200.00'
                                            currency_code = 'EUR' ) )
        it_bookingx            = VALUE #( ( booking_id = lc_booking_id_1  action_code = /dmo/if_flight_legacy=>action_code-update  customer_id = abap_true )
                                          ( booking_id = lc_booking_id_2  action_code = /dmo/if_flight_legacy=>action_code-create ) )
        it_booking_supplement  = VALUE #( ( booking_id            = lc_booking_id_1
                                            booking_supplement_id = lc_booking_supplement_id_1_1
                                            price                 = lv_new_price
                                            currency_code         = 'EUR' )
                                          ( booking_id            = lc_booking_id_1
                                            booking_supplement_id = lc_booking_supplement_id_1_2 )
                                          ( booking_id            = lc_booking_id_1
                                            booking_supplement_id = lc_booking_supplement_id_1_4
                                            supplement_id         = gs_supplement_2-supplement_id
                                            price                 = '40.0'
                                            currency_code         = 'EUR' )
                                          ( booking_id            = lc_booking_id_2
                                            booking_supplement_id = lc_booking_supplement_id_2_1
                                            supplement_id         = gs_supplement_2-supplement_id
                                            price                 = '50.0'
                                            currency_code         = 'EUR' ) )
        it_booking_supplementx = VALUE #( ( booking_id            = lc_booking_id_1
                                            booking_supplement_id = lc_booking_supplement_id_1_1
                                            action_code           = /dmo/if_flight_legacy=>action_code-update
                                            price                 = abap_true
                                            currency_code         = 'EUR' )
                                          ( booking_id            = lc_booking_id_1
                                            booking_supplement_id = lc_booking_supplement_id_1_2
                                            action_code           = /dmo/if_flight_legacy=>action_code-delete )
                                          ( booking_id            = lc_booking_id_1
                                            booking_supplement_id = lc_booking_supplement_id_1_4
                                            action_code           = /dmo/if_flight_legacy=>action_code-create )
                                          ( booking_id            = lc_booking_id_2
                                            booking_supplement_id = lc_booking_supplement_id_2_1
                                            action_code           = /dmo/if_flight_legacy=>action_code-create ) )
       IMPORTING
        es_travel              = DATA(ls_travel_aft_update)
        et_booking             = DATA(lt_booking_aft_update)
        et_booking_supplement  = DATA(lt_booking_suppl_aft_update)
        et_messages            = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    cl_abap_unit_assert=>assert_equals( act = ls_travel_aft_update-travel_id  exp = ls_travel-travel_id ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking_aft_update )  exp = 2 ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_booking_aft_update[ travel_id = ls_travel-travel_id  booking_id = lc_booking_id_1 ] ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_booking_aft_update[ travel_id = ls_travel-travel_id  booking_id = lc_booking_id_2 ] ) ) ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking_suppl_aft_update )  exp = 3 ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_booking_suppl_aft_update[ travel_id = ls_travel-travel_id  booking_id = lc_booking_id_1  booking_supplement_id = lc_booking_supplement_id_1_1 ] ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_booking_suppl_aft_update[ travel_id = ls_travel-travel_id  booking_id = lc_booking_id_1  booking_supplement_id = lc_booking_supplement_id_1_4 ] ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_booking_suppl_aft_update[ travel_id = ls_travel-travel_id  booking_id = lc_booking_id_2  booking_supplement_id = lc_booking_supplement_id_2_1 ] ) ) ).

    gr_cut->save( ).

    " Check Travel
    DATA ls_travel_sel TYPE /dmo/travel.
    SELECT SINGLE total_price, currency_code, description FROM /dmo/travel WHERE travel_id = @ls_travel-travel_id INTO CORRESPONDING FIELDS OF @ls_travel_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-total_price    exp = '100.00' + '200.00' + lv_new_price + '30.00' + '40.00' + '50.00' ) ##LITERAL.
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-currency_code  exp = 'EUR' ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_sel-description    exp = 'My_Deep_Insert_2' ).

    " Check Booking(s)
    " -- Updated Booking
    DATA ls_booking_sel TYPE /dmo/booking.
    SELECT SINGLE booking_date, customer_id FROM /dmo/booking WHERE travel_id = @ls_travel-travel_id AND booking_id = @lc_booking_id_1 INTO CORRESPONDING FIELDS OF @ls_booking_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-booking_date  exp = gv_booking_date ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-customer_id   exp = gv_customer_id_2 ).

    " -- New Booking
    CLEAR ls_booking_sel.
    SELECT SINGLE customer_id, flight_date FROM /dmo/booking WHERE travel_id = @ls_travel-travel_id AND booking_id = @lc_booking_id_2 INTO CORRESPONDING FIELDS OF @ls_booking_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-customer_id  exp = gv_customer_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_sel-flight_date  exp = gs_flight_2-flight_date ).

    " Check Booking Supplement(s)
    " -- Updated Booking Supplement
    DATA ls_booking_supplement_sel TYPE /dmo/book_suppl.
    SELECT SINGLE supplement_id, price FROM /dmo/book_suppl WHERE travel_id             = @ls_travel-travel_id
                                                              AND booking_id            = @lc_booking_id_1
                                                              AND booking_supplement_id = @lc_booking_supplement_id_1_1
                                                            INTO CORRESPONDING FIELDS OF @ls_booking_supplement_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_supplement_sel-supplement_id  exp = gs_supplement_1-supplement_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_supplement_sel-price          exp = lv_new_price ).

    " -- Deleted Booking Supplement
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/book_suppl FIELDS @abap_true WHERE travel_id             = @ls_travel-travel_id
                                                           AND booking_id            = @lc_booking_id_1
                                                           AND booking_supplement_id = @lc_booking_supplement_id_1_2
                                                         INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_false( lv_db_exists ).

    " -- Unchanged Booking Supplement
    CLEAR lv_db_exists.
    SELECT SINGLE FROM /dmo/book_suppl FIELDS @abap_true WHERE travel_id             = @ls_travel-travel_id
                                                           AND booking_id            = @lc_booking_id_1
                                                           AND booking_supplement_id = @lc_booking_supplement_id_1_3
                                                         INTO @lv_db_exists.
    cl_abap_unit_assert=>assert_true( lv_db_exists ).

    " -- Created Booking Supplement for the existing Booking
    CLEAR ls_booking_supplement_sel.
    SELECT SINGLE supplement_id FROM /dmo/book_suppl WHERE travel_id             = @ls_travel-travel_id
                                                       AND booking_id            = @lc_booking_id_1
                                                       AND booking_supplement_id = @lc_booking_supplement_id_1_4
                                                     INTO CORRESPONDING FIELDS OF @ls_booking_supplement_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_supplement_sel-supplement_id  exp = gs_supplement_2-supplement_id ).

    " -- Created Booking Supplement for the new Booking
    CLEAR ls_booking_supplement_sel.
    SELECT SINGLE supplement_id FROM /dmo/book_suppl WHERE travel_id             = @ls_travel-travel_id
                                                       AND booking_id            = @lc_booking_id_2
                                                       AND booking_supplement_id = @lc_booking_supplement_id_2_1
                                                     INTO CORRESPONDING FIELDS OF @ls_booking_supplement_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_supplement_sel-supplement_id  exp = gs_supplement_2-supplement_id ).

    _delete_existing_travel( ls_travel-travel_id ).
  ENDMETHOD.


  METHOD read_db.
    DATA(ls_travel) = _create_travel( is_travel = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201' )
                                      iv_save   = abap_false ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel-travel_id ).

    " Create 2 bookings for the new travel
    _create_booking( iv_travel_id = ls_travel-travel_id
                     is_booking   = VALUE #( booking_id    = '20'
                                             booking_date  = gv_booking_date
                                             customer_id   = gv_customer_id_1
                                             carrier_id    = gs_flight_1-carrier_id
                                             connection_id = gs_flight_1-connection_id
                                             flight_date   = gs_flight_1-flight_date )
                     iv_save      = abap_false ).
    _create_booking( iv_travel_id = ls_travel-travel_id
                     is_booking   = VALUE #( booking_id    = '21'
                                             booking_date  = gv_booking_date
                                             customer_id   = gv_customer_id_1
                                             carrier_id    = gs_flight_1-carrier_id
                                             connection_id = gs_flight_1-connection_id
                                             flight_date   = gs_flight_1-flight_date )
                     iv_save      = abap_false ).

    " Create a single booking supplement for the first booking
    _create_booking_suppl( iv_travel_id          = ls_travel-travel_id
                           is_booking_supplement = VALUE #( booking_id            = '20'
                                                            booking_supplement_id = '10'
                                                            supplement_id         = gs_supplement_1-supplement_id
                                                            price                 = '10.0'
                                                            currency_code         = 'EUR' )
                           iv_save               = abap_false ).

    " Create 2 booking supplements for the second booking
    _create_booking_suppl( iv_travel_id          = ls_travel-travel_id
                           is_booking_supplement = VALUE #( booking_id            = '21'
                                                            booking_supplement_id = '10'
                                                            supplement_id         = gs_supplement_1-supplement_id
                                                            price                 = '20.0'
                                                            currency_code         = 'EUR' )
                           iv_save               = abap_false ).
    _create_booking_suppl( iv_travel_id          = ls_travel-travel_id
                           is_booking_supplement = VALUE #( booking_id            = '21'
                                                            booking_supplement_id = '20'
                                                            supplement_id         = gs_supplement_2-supplement_id
                                                            price                 = '30.0'
                                                            currency_code         = 'EUR' )
                           iv_save               = abap_true )." <<< Finally write to DB

    gr_cut->get_travel( EXPORTING iv_travel_id          = ls_travel-travel_id
                                  iv_include_buffer     = abap_true
                        IMPORTING es_travel             = DATA(ls_travel_read)
                                  et_booking            = DATA(lt_booking_read)
                                  et_booking_supplement = DATA(lt_booking_supplement_read)
                                  et_messages           = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_read-travel_id  exp = ls_travel-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_read-agency_id  exp = ls_travel-agency_id ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking_read )  exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking_supplement_read )  exp = 3 ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_booking_supplement_read[ travel_id = ls_travel-travel_id  booking_id = '21'  booking_supplement_id = '20' ] ) ) ).
    cl_abap_unit_assert=>assert_equals( act = lt_booking_supplement_read[ travel_id = ls_travel-travel_id  booking_id = '21'  booking_supplement_id = '20' ]-supplement_id  exp = gs_supplement_2-supplement_id ).

    _delete_existing_travel( ls_travel-travel_id ).

    gr_cut->get_travel( EXPORTING iv_travel_id          = ls_travel-travel_id
                                  iv_include_buffer     = abap_true
                        IMPORTING es_travel             = ls_travel_read
                                  et_booking            = lt_booking_read
                                  et_booking_supplement = lt_booking_supplement_read
                                  et_messages           = lt_messages ).
    cl_abap_unit_assert=>assert_initial( ls_travel_read ).
    cl_abap_unit_assert=>assert_initial( lt_booking_read ).
    cl_abap_unit_assert=>assert_initial( lt_booking_supplement_read ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = ls_travel-travel_id ).
  ENDMETHOD.


  METHOD read_buffer.
    CONSTANTS lc_booking_id_1 TYPE /dmo/booking_id VALUE '20'.
    CONSTANTS lc_booking_id_2 TYPE /dmo/booking_id VALUE '21'.
    CONSTANTS lc_booking_supplement_id_1_1 TYPE /dmo/booking_supplement_id VALUE '10'.
    CONSTANTS lc_booking_supplement_id_2_1 TYPE /dmo/booking_supplement_id VALUE '10'.
    CONSTANTS lc_booking_supplement_id_2_2 TYPE /dmo/booking_supplement_id VALUE '20'.
    gr_cut->create_travel(
      EXPORTING
        is_travel             = VALUE #( agency_id = gv_agency_id_1  customer_id = gv_customer_id_1  begin_date = '20190101'  end_date = '20190201'  description = 'My_Deep_Insert' )
        it_booking            = VALUE #( ( booking_id    = lc_booking_id_1
                                           booking_date  = gv_booking_date
                                           customer_id   = gv_customer_id_1
                                           carrier_id    = gs_flight_1-carrier_id
                                           connection_id = gs_flight_1-connection_id
                                           flight_date   = gs_flight_1-flight_date )
                                         ( booking_id    = lc_booking_id_2
                                           booking_date  = gv_booking_date
                                           customer_id   = gv_customer_id_2
                                           carrier_id    = gs_flight_2-carrier_id
                                           connection_id = gs_flight_2-connection_id
                                           flight_date   = gs_flight_2-flight_date ) )
         it_booking_supplement = VALUE #( ( booking_id            = lc_booking_id_1
                                            booking_supplement_id = lc_booking_supplement_id_1_1
                                            supplement_id         = gs_supplement_1-supplement_id
                                            price                 = '10.0'
                                            currency_code         = 'EUR' )
                                          ( booking_id            = lc_booking_id_2
                                            booking_supplement_id = lc_booking_supplement_id_2_1
                                            supplement_id         = gs_supplement_1-supplement_id
                                            price                 = '20.0'
                                            currency_code         = 'EUR' )
                                          ( booking_id            = lc_booking_id_2
                                            booking_supplement_id = lc_booking_supplement_id_2_2
                                            supplement_id         = gs_supplement_2-supplement_id
                                            price                 = '30.0'
                                            currency_code         = 'EUR' ) )
      IMPORTING
         es_travel             = DATA(ls_travel)
         et_booking            = DATA(lt_booking)
         et_booking_supplement = DATA(lt_booking_supplement)
         et_messages           = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_not_initial( ls_travel-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking )  exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking_supplement )  exp = 3 ).

    " Now delete the first booking from the buffer
    gr_cut->update_travel( EXPORTING is_travel   = VALUE #( travel_id = ls_travel-travel_id )
                                     is_travelx  = VALUE #( travel_id = ls_travel-travel_id )
                                     it_booking  = VALUE #( ( booking_id = lc_booking_id_1 ) )
                                     it_bookingx = VALUE #( ( booking_id = lc_booking_id_1  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                           IMPORTING et_messages = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    gr_cut->get_travel( EXPORTING iv_travel_id          = ls_travel-travel_id
                                  iv_include_buffer     = abap_true
                        IMPORTING es_travel             = DATA(ls_travel_read)
                                  et_booking            = DATA(lt_booking_read)
                                  et_booking_supplement = DATA(lt_booking_supplement_read)
                                  et_messages           = lt_messages ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_read-travel_id  exp = ls_travel-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = ls_travel_read-agency_id  exp = ls_travel-agency_id ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking_read )  exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking_supplement_read )  exp = 2 ).
    cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lt_booking_supplement_read[ travel_id = ls_travel-travel_id  booking_id = '21'  booking_supplement_id = '20' ] ) ) ).
    cl_abap_unit_assert=>assert_equals( act = lt_booking_supplement_read[ travel_id = ls_travel-travel_id  booking_id = '21'  booking_supplement_id = '20' ]-supplement_id  exp = gs_supplement_2-supplement_id ).

    gr_cut->initialize( ).
  ENDMETHOD.


  METHOD read_travel_id_initial.
    gr_cut->get_travel( EXPORTING iv_travel_id          = '0'
                                  iv_include_buffer     = abap_true
                        IMPORTING es_travel             = DATA(ls_travel_read)
                                  et_booking            = DATA(lt_booking_read)
                                  et_booking_supplement = DATA(lt_booking_supplement_read)
                                  et_messages           = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( ls_travel_read ).
    cl_abap_unit_assert=>assert_initial( lt_booking_read ).
    cl_abap_unit_assert=>assert_initial( lt_booking_supplement_read ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>travel_no_key-msgno ).
  ENDMETHOD.


  METHOD u_supplement_id.
    DATA(ls_booking_supplement) = _create_booking_suppl( iv_travel_id          = mv_travel_id
                                                         is_booking_supplement = VALUE #( booking_id            = mv_booking_id
                                                                                          booking_supplement_id = '10'
                                                                                          supplement_id         = gs_supplement_1-supplement_id
                                                                                          price                 = '10.0'
                                                                                          currency_code         = 'EUR' )
                                                         iv_save               = abap_true ).

    " Pretend to change the supplement ID (provide the same value)
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     is_travelx             = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id
                                                                         supplement_id         = gs_supplement_1-supplement_id ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-update
                                                                         supplement_id         = abap_true ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).

    " Try to update the Booking Supplement with an unknown supplement ID
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     is_travelx             = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id
                                                                         supplement_id         = gs_supplement_2-supplement_id
                                                                         price                 = '11.0' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-update
                                                                         supplement_id         = abap_true
                                                                         price                 = abap_true ) )
                           IMPORTING et_messages            = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_supplement_suppl_id_u-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr3  exp = 'MV_BOOKING_SUPPLEMENT_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = ls_booking_supplement-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = ls_booking_supplement-booking_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_supplement_id       exp = ls_booking_supplement-booking_supplement_id ).

    gr_cut->save( ).

    " Check that nothing has changed
    SELECT SINGLE supplement_id, price FROM /dmo/book_suppl WHERE travel_id             = @ls_booking_supplement-travel_id
                                                              AND booking_id            = @ls_booking_supplement-booking_id
                                                              AND booking_supplement_id = @ls_booking_supplement-booking_supplement_id
                                                         INTO ( @DATA(lv_supplement_id), @DATA(lv_price) ).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = lv_supplement_id  exp = gs_supplement_1-supplement_id ).
    cl_abap_unit_assert=>assert_equals( act = lv_price          exp = '10' ).
  ENDMETHOD.


  METHOD c_price_currency.
    " Do not provide price and currency
    _create_booking_suppl( iv_travel_id          = mv_travel_id
                           is_booking_supplement = VALUE #( booking_id            = mv_booking_id
                                                            booking_supplement_id = '10'
                                                            supplement_id         = gs_supplement_1-supplement_id )
                           iv_save               = abap_false ).

    " Provide price and currency
    CONSTANTS lc_diff TYPE /dmo/supplement_price VALUE '9.99'.
    DATA lv_price         TYPE /dmo/supplement_price.
    DATA lv_currency_code TYPE /dmo/currency_code.
    lv_price = gs_supplement_1-price + lc_diff.
    lv_currency_code = SWITCH #( gs_supplement_1-currency_code WHEN 'USD' THEN 'EUR' ELSE 'USD' ).
    _create_booking_suppl( iv_travel_id          = mv_travel_id
                           is_booking_supplement = VALUE #( booking_id            = mv_booking_id
                                                            booking_supplement_id = '20'
                                                            supplement_id         = gs_supplement_1-supplement_id
                                                            price                 = lv_price
                                                            currency_code         = lv_currency_code )
                           iv_save               = abap_true ).

    DATA ls_booking_supplement_sel TYPE /dmo/book_suppl.
    SELECT SINGLE price, currency_code FROM /dmo/book_suppl WHERE travel_id = @mv_travel_id AND booking_id = @mv_booking_id AND booking_supplement_id = '10' INTO CORRESPONDING FIELDS OF @ls_booking_supplement_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_supplement_sel-price          exp = gs_supplement_1-price ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_supplement_sel-currency_code  exp = gs_supplement_1-currency_code ).

    CLEAR ls_booking_supplement_sel.
    SELECT SINGLE price, currency_code FROM /dmo/book_suppl WHERE travel_id = @mv_travel_id AND booking_id = @mv_booking_id AND booking_supplement_id = '20' INTO CORRESPONDING FIELDS OF @ls_booking_supplement_sel.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_supplement_sel-price          exp = lv_price ).
    cl_abap_unit_assert=>assert_equals( act = ls_booking_supplement_sel-currency_code  exp = lv_currency_code ).
  ENDMETHOD.


  METHOD u_price_currency.
    DATA(ls_booking_supplement) = _create_booking_suppl( iv_travel_id          = mv_travel_id
                                                         is_booking_supplement = VALUE #( booking_id            = mv_booking_id
                                                                                          booking_supplement_id = '10'
                                                                                          supplement_id         = gs_supplement_1-supplement_id )
                                                         iv_save               = abap_true ).

    " Try to change only the price
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     is_travelx             = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id
                                                                         price                 = '11.0' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-update
                                                                         price                 = abap_true ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_supplement_pri_curr_u-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr3  exp = 'MV_BOOKING_SUPPLEMENT_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = ls_booking_supplement-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = ls_booking_supplement-booking_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_supplement_id       exp = ls_booking_supplement-booking_supplement_id ).

    " Try to change only the currency
    CLEAR lt_messages.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     is_travelx             = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id
                                                                         currency_code         = 'XXX' ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-update
                                                                         currency_code         = abap_true ) )
                           IMPORTING et_messages            = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>booking_supplement_pri_curr_u-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_TRAVEL_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr2  exp = 'MV_BOOKING_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr3  exp = 'MV_BOOKING_SUPPLEMENT_ID' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_travel_id                   exp = ls_booking_supplement-travel_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_id                  exp = ls_booking_supplement-booking_id ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_booking_supplement_id       exp = ls_booking_supplement-booking_supplement_id ).
  ENDMETHOD.


  METHOD c_currency_code_unknown.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = mv_travel_id )
                                     is_travelx             = VALUE #( travel_id = mv_travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '10'
                                                                         supplement_id         = gs_supplement_1-supplement_id
                                                                         price                 = '9.99'
                                                                         currency_code         = gv_currency_code_unknown ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = mv_booking_id
                                                                         booking_supplement_id = '10'
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>currency_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_CURRENCY_CODE' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_currency_code               exp = gv_currency_code_unknown ).
  ENDMETHOD.


  METHOD u_currency_code_unknown.
    DATA(ls_booking_supplement) = _create_booking_suppl( iv_travel_id          = mv_travel_id
                                                         is_booking_supplement = VALUE #( booking_id            = mv_booking_id
                                                                                          booking_supplement_id = '10'
                                                                                          supplement_id         = gs_supplement_1-supplement_id )
                                                         iv_save               = abap_true ).

    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     is_travelx             = VALUE #( travel_id = ls_booking_supplement-travel_id )
                                     it_booking_supplement  = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id
                                                                         price                 = '9.99'
                                                                         currency_code         = gv_currency_code_unknown ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = ls_booking_supplement-booking_id
                                                                         booking_supplement_id = ls_booking_supplement-booking_supplement_id
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-update
                                                                         price                 = abap_true
                                                                         currency_code         = abap_true ) )
                           IMPORTING et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages )  exp = 1 ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_messages[ 1 ] IS INSTANCE OF /dmo/cx_flight_legacy ) ).
    DATA lx TYPE REF TO /dmo/cx_flight_legacy.
    lx ?= lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgid  exp = mc_msgid ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-msgno  exp = /dmo/cx_flight_legacy=>currency_unknown-msgno ).
    cl_abap_unit_assert=>assert_equals( act = lx->if_t100_message~t100key-attr1  exp = 'MV_CURRENCY_CODE' ).
    cl_abap_unit_assert=>assert_equals( act = lx->mv_currency_code               exp = gv_currency_code_unknown ).
  ENDMETHOD.


  METHOD _create_booking_suppl.
    gr_cut->update_travel( EXPORTING is_travel              = VALUE #( travel_id = iv_travel_id )
                                     is_travelx             = VALUE #( travel_id = iv_travel_id )
                                     it_booking_supplement  = VALUE #( ( is_booking_supplement ) )
                                     it_booking_supplementx = VALUE #( ( booking_id            = is_booking_supplement-booking_id
                                                                         booking_supplement_id = is_booking_supplement-booking_supplement_id
                                                                         action_code           = /dmo/if_flight_legacy=>action_code-create ) )
                           IMPORTING et_booking_supplement  = DATA(lt_booking_supplement)
                                     et_messages            = DATA(lt_messages) ).
    cl_abap_unit_assert=>assert_initial( lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_booking_supplement )  exp = 1 ).
    rs_booking_supplement = lt_booking_supplement[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = rs_booking_supplement-travel_id              exp = iv_travel_id ).
    cl_abap_unit_assert=>assert_equals( act = rs_booking_supplement-booking_id             exp = is_booking_supplement-booking_id ).
    cl_abap_unit_assert=>assert_equals( act = rs_booking_supplement-booking_supplement_id  exp = is_booking_supplement-booking_supplement_id ).
    IF iv_save = abap_true.
      gr_cut->save( ).
    ENDIF.
  ENDMETHOD.

  METHOD adjust_numbers.
    CONSTANTS cv_travel_id_preliminary_1 TYPE /dmo/travel_id VALUE '1337'.
    CONSTANTS cv_travel_id_final_1       TYPE /dmo/travel_id VALUE '42'.
    CONSTANTS cv_travel_id_final_2       TYPE /dmo/travel_id VALUE '314'.
    CONSTANTS cv_booking_id_final_1      TYPE /dmo/booking_id VALUE '61'.
    CONSTANTS cv_booking_id_final_2      TYPE /dmo/booking_id VALUE '62'.
    CONSTANTS cv_booksuppl_id_final_1    TYPE /dmo/booking_supplement_id VALUE '1'.
    CONSTANTS cv_booksuppl_id_final_2    TYPE /dmo/booking_supplement_id VALUE '2'.

    DATA lt_mapping TYPE /dmo/if_flight_legacy=>tt_ln_bookingsuppl_mapping.

    DATA(lo_buffer) = NEW lcl_booking_supplement_buffer( ).


    lo_buffer->mt_create_buffer = VALUE #(
        ( travel_id = cv_travel_id_preliminary_1  booking_id = cv_booking_id_final_1  booking_supplement_id = cv_booksuppl_id_final_1 )
        ( travel_id = cv_travel_id_final_2        booking_id = cv_booking_id_final_2  booking_supplement_id = cv_booksuppl_id_final_2 )
      ).

    lt_mapping = lo_buffer->adjust_numbers(
                   VALUE #( ( preliminary = VALUE #( travel_id = cv_travel_id_preliminary_1  booking_id = cv_booking_id_final_1 )
                              final       = VALUE #( travel_id = cv_travel_id_final_1        booking_id = cv_booking_id_final_1 ) ) )
      ).

    cl_abap_unit_assert=>assert_not_initial( lt_mapping ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_mapping )  exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
        act = lt_mapping[
                  preliminary-travel_id             = cv_travel_id_preliminary_1
                  preliminary-booking_id            = cv_booking_id_final_1
                  preliminary-booking_supplement_id = cv_booksuppl_id_final_1
                ]-final
        exp = VALUE /dmo/if_flight_legacy=>ts_ln_bookingsuppl(
                  travel_id             = cv_travel_id_final_1
                  booking_id            = cv_booking_id_final_1
                  booking_supplement_id = cv_booksuppl_id_final_1
                )
      ).

    cl_abap_unit_assert=>assert_equals(
        act = lt_mapping[
                  preliminary-travel_id             = cv_travel_id_final_2
                  preliminary-booking_id            = cv_booking_id_final_2
                  preliminary-booking_supplement_id = cv_booksuppl_id_final_2
                ]-final
        exp = VALUE /dmo/if_flight_legacy=>ts_ln_bookingsuppl(
                  travel_id             = cv_travel_id_final_2
                  booking_id            = cv_booking_id_final_2
                  booking_supplement_id = cv_booksuppl_id_final_2
                )
      ).
  ENDMETHOD.
ENDCLASS.
