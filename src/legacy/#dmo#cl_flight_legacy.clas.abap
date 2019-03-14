CLASS /dmo/cl_flight_legacy DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS /dmo/cl_flight_data_generator.

  PUBLIC SECTION.
    INTERFACES /dmo/if_flight_legacy.

    TYPES: BEGIN OF ENUM ty_change_mode STRUCTURE change_mode," Key checks are done separately
             create,
             update," Only fields that have been changed need to be checked
           END OF ENUM ty_change_mode STRUCTURE change_mode.

    CLASS-METHODS: get_instance RETURNING VALUE(ro_instance) TYPE REF TO /dmo/cl_flight_legacy.

    "   With respect to the same method call of create/update/delete_travel() we have All or Nothing.
    "   I.e. when one of the levels contains an error, the complete call is refused.
    "   However, the buffer is not cleared in case of an error.
    "   I.e. when the caller wants to start over, he needs to call Initialize() explicitly.

    METHODS set_status_to_booked IMPORTING iv_travel_id TYPE /dmo/travel_id
                                 EXPORTING et_messages  TYPE /dmo/if_flight_legacy=>tt_if_t100_message.

    METHODS create_travel IMPORTING is_travel             TYPE /dmo/if_flight_legacy=>ts_travel_in
                                    it_booking            TYPE /dmo/if_flight_legacy=>tt_booking_in OPTIONAL
                                    it_booking_supplement TYPE /dmo/if_flight_legacy=>tt_booking_supplement_in OPTIONAL
                          EXPORTING es_travel             TYPE /dmo/travel
                                    et_booking            TYPE /dmo/if_flight_legacy=>tt_booking
                                    et_booking_supplement TYPE /dmo/if_flight_legacy=>tt_booking_supplement
                                    et_messages           TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS update_travel IMPORTING is_travel              TYPE /dmo/if_flight_legacy=>ts_travel_in
                                    is_travelx             TYPE /dmo/if_flight_legacy=>ts_travel_inx
                                    it_booking             TYPE /dmo/if_flight_legacy=>tt_booking_in OPTIONAL
                                    it_bookingx            TYPE /dmo/if_flight_legacy=>tt_booking_inx OPTIONAL
                                    it_booking_supplement  TYPE /dmo/if_flight_legacy=>tt_booking_supplement_in OPTIONAL
                                    it_booking_supplementx TYPE /dmo/if_flight_legacy=>tt_booking_supplement_inx OPTIONAL
                          EXPORTING es_travel              TYPE /dmo/travel
                                    et_booking             TYPE /dmo/if_flight_legacy=>tt_booking
                                    et_booking_supplement  TYPE /dmo/if_flight_legacy=>tt_booking_supplement
                                    et_messages            TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS delete_travel IMPORTING iv_travel_id TYPE /dmo/travel_id
                          EXPORTING et_messages  TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS get_travel IMPORTING iv_travel_id           TYPE /dmo/travel_id
                                 iv_include_buffer      TYPE abap_boolean
                                 iv_include_temp_buffer TYPE abap_boolean OPTIONAL
                       EXPORTING es_travel              TYPE /dmo/travel
                                 et_booking             TYPE /dmo/if_flight_legacy=>tt_booking
                                 et_booking_supplement  TYPE /dmo/if_flight_legacy=>tt_booking_supplement
                                 et_messages            TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS save.
    METHODS initialize.
    METHODS convert_messages IMPORTING it_messages TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                             EXPORTING et_messages TYPE /dmo/if_flight_legacy=>tt_message.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA go_instance TYPE REF TO /dmo/cl_flight_legacy.

    CLASS-METHODS:
      "! Calculation of Price <br/>
      "!  <br/>
      "! Price will be calculated using distance multiplied and occupied seats.<br/>
      "! Depending on how many seats in percentage are occupied the formula <br/>
      "! 3/400·x² + 25<br/>
      "! will be applied.<br/>
      "!   0% seats occupied leads to 25% of distance as price.<br/>
      "!  75% seats occupied leads to 50% of distance as price.<br/>
      "! 100% seats occupied leads to 100% of distance as price.<br/>
      "! @parameter iv_seats_occupied_percent | occupied seats
      "! @parameter iv_flight_distance | flight distance in kilometer
      "! @parameter rv_price | calculated flight price
      calculate_flight_price
        IMPORTING
          iv_seats_occupied_percent TYPE /dmo/plane_seats_occupied
          iv_flight_distance        TYPE i
        RETURNING
          VALUE(rv_price)           TYPE /dmo/flight_price ##RELAX.

    METHODS lock_travel IMPORTING iv_lock TYPE abap_bool
                        RAISING   /dmo/cx_flight_legacy ##RELAX ##NEEDED.

    METHODS _resolve_attribute IMPORTING iv_attrname      TYPE scx_attrname
                                         ix               TYPE REF TO /dmo/cx_flight_legacy
                               RETURNING VALUE(rv_symsgv) TYPE symsgv.
    "! Final determinations / derivations after all levels have been prepared, e.g. bottom-up derivations
    METHODS _determine EXPORTING et_messages           TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                       CHANGING  cs_travel             TYPE /dmo/travel
                                 ct_booking            TYPE /dmo/if_flight_legacy=>tt_booking
                                 ct_booking_supplement TYPE /dmo/if_flight_legacy=>tt_booking_supplement.
    METHODS _determine_travel_total_price CHANGING cs_travel             TYPE /dmo/travel
                                                   ct_booking            TYPE /dmo/if_flight_legacy=>tt_booking
                                                   ct_booking_supplement TYPE /dmo/if_flight_legacy=>tt_booking_supplement
                                                   ct_messages           TYPE /dmo/if_flight_legacy=>tt_if_t100_message ##NEEDED.
    METHODS _convert_currency IMPORTING iv_currency_code_source TYPE /dmo/currency_code
                                        iv_currency_code_target TYPE /dmo/currency_code
                                        iv_amount               TYPE /dmo/total_price
                              RETURNING VALUE(rv_amount)        TYPE /dmo/total_price.
ENDCLASS.



CLASS /dmo/cl_flight_legacy IMPLEMENTATION.


  METHOD calculate_flight_price.
    DATA: lv_percentage_of_max_price TYPE i.
    lv_percentage_of_max_price = ( 3 * iv_seats_occupied_percent ** 2 DIV 400 ) + 25 ##OPERATOR[**].
    rv_price = lv_percentage_of_max_price * iv_flight_distance DIV 100.
  ENDMETHOD.


  METHOD convert_messages.
    CLEAR et_messages.
    DATA ls_message TYPE symsg.
    LOOP AT it_messages INTO DATA(lr_error) ##INTO_OK.
      ls_message-msgty = 'E'.
      ls_message-msgid = lr_error->t100key-msgid.
      ls_message-msgno = lr_error->t100key-msgno.
      IF lr_error IS INSTANCE OF /dmo/cx_flight_legacy.
        DATA(lx) = CAST /dmo/cx_flight_legacy( lr_error ).
        ls_message-msgv1 = _resolve_attribute( iv_attrname = lr_error->t100key-attr1  ix = lx ).
        ls_message-msgv2 = _resolve_attribute( iv_attrname = lr_error->t100key-attr2  ix = lx ).
        ls_message-msgv3 = _resolve_attribute( iv_attrname = lr_error->t100key-attr3  ix = lx ).
        ls_message-msgv4 = _resolve_attribute( iv_attrname = lr_error->t100key-attr4  ix = lx ).
      ENDIF.
      APPEND ls_message TO et_messages.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_travel.
    CLEAR: es_travel, et_booking, et_booking_supplement, et_messages.

    " Travel
    lcl_travel_buffer=>get_instance( )->cud_prep( EXPORTING it_travel   = VALUE #( ( CORRESPONDING #( is_travel ) ) )
                                                            it_travelx  = VALUE #( ( travel_id = is_travel-travel_id  action_code = /dmo/if_flight_legacy=>action_code-create ) )
                                                  IMPORTING et_travel   = DATA(lt_travel)
                                                            et_messages = et_messages ).
    IF et_messages IS INITIAL.
      ASSERT lines( lt_travel ) = 1.
      es_travel = lt_travel[ 1 ].
    ENDIF.

    " Bookings
    IF et_messages IS INITIAL.
      DATA lt_booking  TYPE /dmo/if_flight_legacy=>tt_booking.
      DATA lt_bookingx TYPE /dmo/if_flight_legacy=>tt_bookingx.
      LOOP AT it_booking INTO DATA(ls_booking_in).
        DATA ls_booking TYPE /dmo/booking.
        ls_booking = CORRESPONDING #( ls_booking_in ).
        ls_booking-travel_id = es_travel-travel_id.
        INSERT ls_booking INTO TABLE lt_booking.
        INSERT VALUE #( travel_id = ls_booking-travel_id  booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-create ) INTO TABLE lt_bookingx.
      ENDLOOP.
      lcl_booking_buffer=>get_instance( )->cud_prep( EXPORTING it_booking  = lt_booking
                                                               it_bookingx = lt_bookingx
                                                     IMPORTING et_booking  = et_booking
                                                               et_messages = DATA(lt_messages) ).
      APPEND LINES OF lt_messages TO et_messages.
    ENDIF.

    " Booking Supplements
    IF et_messages IS INITIAL.
      DATA lt_booking_supplement  TYPE /dmo/if_flight_legacy=>tt_booking_supplement.
      DATA lt_booking_supplementx TYPE /dmo/if_flight_legacy=>tt_booking_supplementx.
      LOOP AT it_booking_supplement INTO DATA(ls_booking_supplement_in).
        DATA ls_booking_supplement TYPE /dmo/book_suppl.
        ls_booking_supplement = CORRESPONDING #( ls_booking_supplement_in ).
        ls_booking_supplement-travel_id = es_travel-travel_id.
        IF lcl_booking_buffer=>get_instance( )->check_booking_id( EXPORTING iv_travel_id = ls_booking_supplement-travel_id  iv_booking_id = ls_booking_supplement-booking_id CHANGING ct_messages = et_messages ) = abap_false.
          EXIT.
        ENDIF.
        INSERT ls_booking_supplement INTO TABLE lt_booking_supplement.
        INSERT VALUE #( travel_id             = ls_booking_supplement-travel_id
                        booking_id            = ls_booking_supplement-booking_id
                        booking_supplement_id = ls_booking_supplement-booking_supplement_id
                        action_code           = /dmo/if_flight_legacy=>action_code-create ) INTO TABLE lt_booking_supplementx.
      ENDLOOP.
      IF et_messages IS INITIAL.
        lcl_booking_supplement_buffer=>get_instance( )->cud_prep( EXPORTING it_booking_supplement  = lt_booking_supplement
                                                                            it_booking_supplementx = lt_booking_supplementx
                                                                  IMPORTING et_booking_supplement  = et_booking_supplement
                                                                            et_messages            = lt_messages ).
        APPEND LINES OF lt_messages TO et_messages.
      ENDIF.
    ENDIF.

    " Now do any derivations that require the whole business object (not only a single node), but which may in principle result in an error
    IF et_messages IS INITIAL.
      _determine( IMPORTING et_messages           = et_messages
                  CHANGING  cs_travel             = es_travel
                            ct_booking            = et_booking
                            ct_booking_supplement = et_booking_supplement ).
    ENDIF.

    IF et_messages IS INITIAL.
      lcl_travel_buffer=>get_instance( )->cud_copy( ).
      lcl_booking_buffer=>get_instance( )->cud_copy( ).
      lcl_booking_supplement_buffer=>get_instance( )->cud_copy( ).
    ELSE.
      CLEAR: es_travel, et_booking, et_booking_supplement.
      lcl_travel_buffer=>get_instance( )->cud_disc( ).
      lcl_booking_buffer=>get_instance( )->cud_disc( ).
      lcl_booking_supplement_buffer=>get_instance( )->cud_disc( ).
    ENDIF.
  ENDMETHOD.


  METHOD delete_travel.
    CLEAR et_messages.

    get_travel( EXPORTING iv_travel_id           = iv_travel_id
                          iv_include_buffer      = abap_true
                          iv_include_temp_buffer = abap_true
                IMPORTING et_booking             = DATA(lt_booking)
                          et_booking_supplement  = DATA(lt_booking_supplement)
                          et_messages            = et_messages ).

    IF et_messages IS INITIAL.
      lcl_booking_supplement_buffer=>get_instance( )->cud_prep( EXPORTING it_booking_supplement  = CORRESPONDING #( lt_booking_supplement MAPPING travel_id             = travel_id
                                                                                                                                                  booking_id            = booking_id
                                                                                                                                                  booking_supplement_id = booking_supplement_id EXCEPT * )
                                                                          it_booking_supplementx = VALUE #( FOR ls_bs IN lt_booking_supplement ( action_code           = /dmo/if_flight_legacy=>action_code-delete
                                                                                                                                                 travel_id             = ls_bs-travel_id
                                                                                                                                                 booking_id            = ls_bs-booking_id
                                                                                                                                                 booking_supplement_id = ls_bs-booking_supplement_id ) )
                                                                          iv_no_delete_check     = abap_true " No existence check required
                                                                IMPORTING et_messages            = DATA(lt_messages) ).
      APPEND LINES OF lt_messages TO et_messages.
    ENDIF.

    IF et_messages IS INITIAL.
      lcl_booking_buffer=>get_instance( )->cud_prep( EXPORTING it_booking         = CORRESPONDING #( lt_booking MAPPING travel_id = travel_id  booking_id = booking_id EXCEPT * )
                                                               it_bookingx        = VALUE #( FOR ls_b IN lt_booking ( action_code = /dmo/if_flight_legacy=>action_code-delete  travel_id = ls_b-travel_id  booking_id = ls_b-booking_id ) )
                                                               iv_no_delete_check = abap_true " No existence check required
                                                     IMPORTING et_messages        = lt_messages ).
      APPEND LINES OF lt_messages TO et_messages.
    ENDIF.

    IF et_messages IS INITIAL.
      lcl_travel_buffer=>get_instance( )->cud_prep( EXPORTING it_travel          = VALUE #( ( travel_id = iv_travel_id ) )
                                                              it_travelx         = VALUE #( ( travel_id = iv_travel_id  action_code = /dmo/if_flight_legacy=>action_code-delete ) )
                                                              iv_no_delete_check = abap_true " No existence check required
                                                    IMPORTING et_messages        = lt_messages ).
      APPEND LINES OF lt_messages TO et_messages.
    ENDIF.

    IF et_messages IS INITIAL.
      lcl_travel_buffer=>get_instance( )->cud_copy( ).
      lcl_booking_buffer=>get_instance( )->cud_copy( ).
      lcl_booking_supplement_buffer=>get_instance( )->cud_copy( ).
    ELSE.
      lcl_travel_buffer=>get_instance( )->cud_disc( ).
      lcl_booking_buffer=>get_instance( )->cud_disc( ).
      lcl_booking_supplement_buffer=>get_instance( )->cud_disc( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
    go_instance = COND #( WHEN go_instance IS BOUND THEN go_instance ELSE NEW #( ) ).
    ro_instance = go_instance.
  ENDMETHOD.


  METHOD get_travel.
    CLEAR: es_travel, et_booking, et_booking_supplement, et_messages.

    IF iv_travel_id IS INITIAL.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_key ) TO et_messages.
      RETURN.
    ENDIF.

    lcl_travel_buffer=>get_instance( )->get( EXPORTING it_travel              = VALUE #( ( travel_id = iv_travel_id ) )
                                                       iv_include_buffer      = iv_include_buffer
                                                       iv_include_temp_buffer = iv_include_temp_buffer
                                             IMPORTING et_travel              = DATA(lt_travel) ).
    IF lt_travel IS INITIAL.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = iv_travel_id ) TO et_messages.
      RETURN.
    ENDIF.
    ASSERT lines( lt_travel ) = 1.
    es_travel = lt_travel[ 1 ].

    lcl_booking_buffer=>get_instance( )->get( EXPORTING it_booking             = VALUE #( ( travel_id = iv_travel_id ) )
                                                        iv_include_buffer      = iv_include_buffer
                                                        iv_include_temp_buffer = iv_include_temp_buffer
                                              IMPORTING et_booking             = et_booking ).

    lcl_booking_supplement_buffer=>get_instance( )->get( EXPORTING it_booking_supplement  = CORRESPONDING #( et_booking MAPPING travel_id = travel_id  booking_id = booking_id EXCEPT * )
                                                                   iv_include_buffer      = iv_include_buffer
                                                                   iv_include_temp_buffer = iv_include_temp_buffer
                                                         IMPORTING et_booking_supplement  = et_booking_supplement ).
  ENDMETHOD.


  METHOD initialize.
    lcl_travel_buffer=>get_instance( )->initialize( ).
    lcl_booking_buffer=>get_instance( )->initialize( ).
    lcl_booking_supplement_buffer=>get_instance( )->initialize( ).
  ENDMETHOD.


  METHOD lock_travel ##NEEDED.
*   IF iv_lock = abap_true.
*     CALL FUNCTION 'ENQUEUE_/DMO/ETRAVEL'
*       EXCEPTIONS
*         foreign_lock   = 1
*         system_failure = 2
*         OTHERS         = 3.
*     IF sy-subrc <> 0.
*       RAISE EXCEPTION TYPE /dmo/cx_flight_legacy
*         EXPORTING
*           textid = /dmo/cx_flight_legacy=>travel_lock.
*     ENDIF.
*   ELSE.
*     CALL FUNCTION 'DEQUEUE_/DMO/ETRAVEL'.
*   ENDIF.
  ENDMETHOD.


  METHOD save.
    lcl_travel_buffer=>get_instance( )->save( ).
    lcl_booking_buffer=>get_instance( )->save( ).
    lcl_booking_supplement_buffer=>get_instance( )->save( ).
    initialize( ).
  ENDMETHOD.


  METHOD set_status_to_booked.
    lcl_travel_buffer=>get_instance( )->set_status_to_booked( EXPORTING iv_travel_id = iv_travel_id
                                                              IMPORTING et_messages  = et_messages ).
  ENDMETHOD.


  METHOD update_travel.
    CLEAR es_travel.
    CLEAR et_booking.
    CLEAR et_booking_supplement.
    CLEAR et_messages.

    " Travel
    IF is_travel-travel_id IS INITIAL.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_key ) TO et_messages.
      RETURN.
    ENDIF.
    DATA ls_travelx TYPE /dmo/if_flight_legacy=>ts_travelx.
    ls_travelx = CORRESPONDING #( is_travelx ).
    ls_travelx-action_code = /dmo/if_flight_legacy=>action_code-update.
    lcl_travel_buffer=>get_instance( )->cud_prep( EXPORTING it_travel   = VALUE #( ( CORRESPONDING #( is_travel ) ) )
                                                            it_travelx  = VALUE #( ( ls_travelx ) )
                                                  IMPORTING et_travel   = DATA(lt_travel)
                                                            et_messages = et_messages ).

    " We may need to delete Booking Supplements of deleted Bookings
    " Read all Booking Supplements before any Bookings are deleted
    get_travel( EXPORTING iv_travel_id           = is_travel-travel_id
                          iv_include_buffer      = abap_true
                          iv_include_temp_buffer = abap_true
                IMPORTING et_booking_supplement  = DATA(lt_booking_supplement_del) ).

    " Bookings
    IF et_messages IS INITIAL.
      " Ignore provided Travel ID of subnode tables
      DATA lt_booking  TYPE /dmo/if_flight_legacy=>tt_booking.
      DATA lt_bookingx TYPE /dmo/if_flight_legacy=>tt_bookingx.
      LOOP AT it_booking INTO DATA(ls_booking_in).
        DATA ls_booking TYPE /dmo/booking.
        ls_booking = CORRESPONDING #( ls_booking_in ).
        ls_booking-travel_id = is_travel-travel_id.
        INSERT ls_booking INTO TABLE lt_booking.
      ENDLOOP.
      LOOP AT it_bookingx INTO DATA(ls_booking_inx).
        DATA ls_bookingx TYPE /dmo/if_flight_legacy=>ts_bookingx.
        ls_bookingx = CORRESPONDING #( ls_booking_inx ).
        ls_bookingx-travel_id = is_travel-travel_id.
        INSERT ls_bookingx INTO TABLE lt_bookingx.
      ENDLOOP.
      lcl_booking_buffer=>get_instance( )->cud_prep( EXPORTING it_booking  = lt_booking
                                                               it_bookingx = lt_bookingx
                                                     IMPORTING et_booking  = et_booking
                                                               et_messages = DATA(lt_messages) ).
      APPEND LINES OF lt_messages TO et_messages.
    ENDIF.

    " Booking Supplements
    IF et_messages IS INITIAL.
      " Ignore provided Travel ID of subnode tables
      DATA lt_booking_supplement  TYPE /dmo/if_flight_legacy=>tt_booking_supplement.
      DATA lt_booking_supplementx TYPE /dmo/if_flight_legacy=>tt_booking_supplementx.
      LOOP AT it_booking_supplement INTO DATA(ls_booking_supplement_in).
        DATA ls_booking_supplement TYPE /dmo/book_suppl.
        ls_booking_supplement = CORRESPONDING #( ls_booking_supplement_in ).
        ls_booking_supplement-travel_id = is_travel-travel_id.
        IF lcl_booking_buffer=>get_instance( )->check_booking_id( EXPORTING iv_travel_id  = ls_booking_supplement-travel_id
                                                                            iv_booking_id = ls_booking_supplement-booking_id
                                                                  CHANGING  ct_messages   = et_messages ) = abap_false.
          EXIT.
        ENDIF.
        INSERT ls_booking_supplement INTO TABLE lt_booking_supplement.
      ENDLOOP.
      IF et_messages IS INITIAL.
        LOOP AT it_booking_supplementx INTO DATA(ls_booking_supplement_inx).
          DATA ls_booking_supplementx TYPE /dmo/if_flight_legacy=>ts_booking_supplementx.
          ls_booking_supplementx = CORRESPONDING #( ls_booking_supplement_inx ).
          ls_booking_supplementx-travel_id = is_travel-travel_id.
          INSERT ls_booking_supplementx INTO TABLE lt_booking_supplementx.
        ENDLOOP.
        lcl_booking_supplement_buffer=>get_instance( )->cud_prep( EXPORTING it_booking_supplement  = lt_booking_supplement
                                                                            it_booking_supplementx = lt_booking_supplementx
                                                                  IMPORTING et_booking_supplement  = et_booking_supplement
                                                                            et_messages            = lt_messages ).
        APPEND LINES OF lt_messages TO et_messages.
      ENDIF.
    ENDIF.

    " For Bookings to be deleted we also need to delete the Booking Supplements
    IF    et_messages IS INITIAL
      AND lt_booking_supplement_del IS NOT INITIAL
      AND line_exists( lt_bookingx[ action_code = CONV /dmo/action_code( /dmo/if_flight_legacy=>action_code-delete ) ] ).
      " Remove any Bookings from internal table that must not be deleted
      LOOP AT lt_booking_supplement_del ASSIGNING FIELD-SYMBOL(<s_booking_supplement_del>).
        READ TABLE lt_bookingx TRANSPORTING NO FIELDS WITH KEY action_code = CONV /dmo/action_code( /dmo/if_flight_legacy=>action_code-delete )
                                                               travel_id   = <s_booking_supplement_del>-travel_id
                                                               booking_id  = <s_booking_supplement_del>-booking_id.
        IF sy-subrc <> 0.
          DELETE lt_booking_supplement_del.
        ENDIF.
      ENDLOOP.
      lcl_booking_supplement_buffer=>get_instance( )->cud_prep( EXPORTING it_booking_supplement  = CORRESPONDING #( lt_booking_supplement_del MAPPING travel_id             = travel_id
                                                                                                                                                      booking_id            = booking_id
                                                                                                                                                      booking_supplement_id = booking_supplement_id EXCEPT * )
                                                                          it_booking_supplementx = VALUE #( FOR ls_bs IN lt_booking_supplement_del ( action_code           = /dmo/if_flight_legacy=>action_code-delete
                                                                                                                                                     travel_id             = ls_bs-travel_id
                                                                                                                                                     booking_id            = ls_bs-booking_id
                                                                                                                                                     booking_supplement_id = ls_bs-booking_supplement_id ) )
                                                                          iv_no_delete_check     = abap_true " No existence check required
                                                                IMPORTING et_messages            = et_messages ).
    ENDIF.

    IF et_messages IS INITIAL.
      ASSERT lines( lt_travel ) = 1.
      " Now do any derivations that require the whole business object (not only a single node), but which may in principle result in an error
      " The derivation may need the complete Business Object, i.e. including unchanged subnodes
      get_travel( EXPORTING iv_travel_id           = lt_travel[ 1 ]-travel_id
                            iv_include_buffer      = abap_true
                            iv_include_temp_buffer = abap_true
                  IMPORTING es_travel              = es_travel
                            et_booking             = et_booking
                            et_booking_supplement  = et_booking_supplement
                            et_messages            = et_messages ).
      ASSERT et_messages IS INITIAL.
      _determine( IMPORTING et_messages           = et_messages
                  CHANGING  cs_travel             = es_travel
                            ct_booking            = et_booking
                            ct_booking_supplement = et_booking_supplement ).
      IF et_messages IS INITIAL.
        " We do not want to return all subnodes, but only those that have been created or changed.
        " So currently it is not implemented that a determination of a booking changes another booking as the other booking cannot be properly returned.
        LOOP AT et_booking ASSIGNING FIELD-SYMBOL(<s_booking>).
          LOOP AT it_bookingx TRANSPORTING NO FIELDS WHERE booking_id = <s_booking>-booking_id
            AND ( action_code = CONV /dmo/action_code( /dmo/if_flight_legacy=>action_code-create ) OR action_code = CONV /dmo/action_code( /dmo/if_flight_legacy=>action_code-update ) ).
            EXIT.
          ENDLOOP.
          IF sy-subrc <> 0.
            DELETE et_booking.
          ENDIF.
        ENDLOOP.
        LOOP AT et_booking_supplement ASSIGNING FIELD-SYMBOL(<s_booking_supplement>).
          LOOP AT it_booking_supplementx TRANSPORTING NO FIELDS WHERE booking_id = <s_booking_supplement>-booking_id AND booking_supplement_id = <s_booking_supplement>-booking_supplement_id
            AND ( action_code = CONV /dmo/action_code( /dmo/if_flight_legacy=>action_code-create ) OR action_code = CONV /dmo/action_code( /dmo/if_flight_legacy=>action_code-update ) ).
            EXIT.
          ENDLOOP.
          IF sy-subrc <> 0.
            DELETE et_booking_supplement.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF et_messages IS INITIAL.
      lcl_travel_buffer=>get_instance( )->cud_copy( ).
      lcl_booking_buffer=>get_instance( )->cud_copy( ).
      lcl_booking_supplement_buffer=>get_instance( )->cud_copy( ).
    ELSE.
      CLEAR: es_travel, et_booking, et_booking_supplement.
      lcl_travel_buffer=>get_instance( )->cud_disc( ).
      lcl_booking_buffer=>get_instance( )->cud_disc( ).
      lcl_booking_supplement_buffer=>get_instance( )->cud_disc( ).
    ENDIF.
  ENDMETHOD.


  METHOD _convert_currency.
    DATA(lv_exchange_rate_date) = cl_abap_context_info=>get_system_date( )." Do not buffer: Current date may change during lifetime of session
    /dmo/cl_flight_amdp=>convert_currency(
      EXPORTING
        iv_amount               = iv_amount
        iv_currency_code_source = iv_currency_code_source
        iv_currency_code_target = iv_currency_code_target
        iv_exchange_rate_date   = lv_exchange_rate_date
      IMPORTING
        ev_amount               = rv_amount
    ).
  ENDMETHOD.


  METHOD _determine.
    ASSERT cs_travel-travel_id IS NOT INITIAL.
    LOOP AT ct_booking TRANSPORTING NO FIELDS WHERE travel_id <> cs_travel-travel_id.
      EXIT.
    ENDLOOP.
    ASSERT sy-subrc = 4.
    LOOP AT ct_booking_supplement TRANSPORTING NO FIELDS WHERE travel_id <> cs_travel-travel_id.
      EXIT.
    ENDLOOP.
    ASSERT sy-subrc = 4.
    CLEAR et_messages.
    _determine_travel_total_price( CHANGING cs_travel             = cs_travel
                                            ct_booking            = ct_booking
                                            ct_booking_supplement = ct_booking_supplement
                                            ct_messages           = et_messages ).
  ENDMETHOD.


  METHOD _determine_travel_total_price.
    DATA lv_add TYPE /dmo/total_price.
    DATA(lv_currency_code_target) = cs_travel-currency_code.

    " If we do not have a Travel Currency Code yet,
    " we may derive it when all the subnodes have the same non-initial Currency Code
    IF lv_currency_code_target IS INITIAL.
      DATA lv_ok TYPE abap_bool.
      lv_ok = abap_true.
      LOOP AT ct_booking ASSIGNING FIELD-SYMBOL(<s_booking>).
        IF sy-tabix = 1.
          lv_currency_code_target = <s_booking>-currency_code.
        ENDIF.
        IF <s_booking>-currency_code IS INITIAL.
          lv_ok = abap_false.
          EXIT.
        ENDIF.
        IF lv_currency_code_target <> <s_booking>-currency_code.
          lv_ok = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_ok = abap_true.
        LOOP AT ct_booking_supplement ASSIGNING FIELD-SYMBOL(<s_booking_supplement>).
          IF <s_booking_supplement>-currency_code IS INITIAL.
            lv_ok = abap_false.
            EXIT.
          ENDIF.
          IF lv_currency_code_target <> <s_booking_supplement>-currency_code.
            lv_ok = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF lv_currency_code_target IS NOT INITIAL.
      " Total Price = Booking Fee + Booking Flight Prices + Booking Supplement Prices
      cs_travel-total_price   = cs_travel-booking_fee.
      cs_travel-currency_code = lv_currency_code_target.
      LOOP AT ct_booking ASSIGNING <s_booking>
          GROUP BY <s_booking>-currency_code INTO DATA(booking_currency_code).

        lv_add = REDUCE #( INIT sum = 0
                           FOR b IN GROUP booking_currency_code
                           NEXT  sum = sum + b-flight_price  ).

        IF booking_currency_code <> lv_currency_code_target.
          lv_add = _convert_currency( iv_currency_code_source = booking_currency_code
                                      iv_currency_code_target = lv_currency_code_target
                                      iv_amount               = lv_add ).
        ENDIF.
        cs_travel-total_price = cs_travel-total_price + lv_add.
      ENDLOOP.
      LOOP AT ct_booking_supplement ASSIGNING <s_booking_supplement>
          GROUP BY <s_booking_supplement>-currency_code INTO DATA(supplement_currency_code).

        lv_add = REDUCE #( INIT sum = 0
                           FOR s IN GROUP supplement_currency_code
                           NEXT  sum = sum + s-price  ).

        IF supplement_currency_code <> lv_currency_code_target.
          lv_add = _convert_currency( iv_currency_code_source = supplement_currency_code
                                      iv_currency_code_target = lv_currency_code_target
                                      iv_amount               = lv_add ).
        ENDIF.
        cs_travel-total_price = cs_travel-total_price + lv_add.
      ENDLOOP.
      lcl_travel_buffer=>get_instance( )->cud_prep( EXPORTING it_travel   = VALUE #( ( travel_id = cs_travel-travel_id  total_price = cs_travel-total_price  currency_code = cs_travel-currency_code ) )
                                                              it_travelx  = VALUE #( ( action_code = /dmo/if_flight_legacy=>action_code-update  travel_id = cs_travel-travel_id  total_price = abap_true  currency_code = abap_true ) )
                                                    IMPORTING et_messages = DATA(lt_messages) ).
      ASSERT lt_messages IS INITIAL.
    ENDIF.
  ENDMETHOD.


  METHOD _resolve_attribute.
    CLEAR rv_symsgv.
    CASE iv_attrname.
      WHEN ''.
        rv_symsgv = ''.
      WHEN 'MV_TRAVEL_ID'.
        rv_symsgv = |{ ix->mv_travel_id ALPHA = OUT }|.
      WHEN 'MV_BOOKING_ID'.
        rv_symsgv = |{ ix->mv_booking_id ALPHA = OUT }|.
      WHEN 'MV_BOOKING_SUPPLEMENT_ID'.
        rv_symsgv = |{ ix->mv_booking_supplement_id ALPHA = OUT }|.
      WHEN 'MV_AGENCY_ID'.
        rv_symsgv = |{ ix->mv_agency_id ALPHA = OUT }|.
      WHEN 'MV_CUSTOMER_ID'.
        rv_symsgv = |{ ix->mv_customer_id ALPHA = OUT }|.
      WHEN 'MV_CARRIER_ID'.
        rv_symsgv = |{ ix->mv_carrier_id ALPHA = OUT }|.
      WHEN 'MV_CONNECTION_ID'.
        rv_symsgv = |{ ix->mv_connection_id ALPHA = OUT }|.
      WHEN 'MV_SUPPLEMENT_ID'.
        rv_symsgv = ix->mv_supplement_id.
      WHEN 'MV_BEGIN_DATE'.
        rv_symsgv = |{ ix->mv_begin_date DATE = USER }|.
      WHEN 'MV_END_DATE'.
        rv_symsgv = |{ ix->mv_end_date DATE = USER }|.
      WHEN 'MV_BOOKING_DATE'.
        rv_symsgv = |{ ix->mv_booking_date DATE = USER }|.
      WHEN 'MV_FLIGHT_DATE'.
        rv_symsgv = |{ ix->mv_flight_date DATE = USER }|.
      WHEN 'MV_STATUS'.
        rv_symsgv = ix->mv_status.
      WHEN 'MV_CURRENCY_CODE'.
        rv_symsgv = ix->mv_currency_code.
      WHEN 'MV_UNAME'.
        rv_symsgv = ix->mv_uname.
      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
