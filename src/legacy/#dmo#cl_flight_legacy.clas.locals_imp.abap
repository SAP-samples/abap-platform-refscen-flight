CLASS lcl_common_checks DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS is_currency_code_valid IMPORTING iv_currency_code   TYPE /dmo/currency_code
                                         CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                                         RETURNING VALUE(rv_is_valid) TYPE abap_boolean.
    CLASS-METHODS is_customer_id_valid IMPORTING iv_customer_id     TYPE /dmo/customer_id
                                       CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                                       RETURNING VALUE(rv_is_valid) TYPE abap_boolean.
  PRIVATE SECTION.
    CLASS-DATA mt_currency_code TYPE SORTED TABLE OF /dmo/currency_code WITH UNIQUE KEY table_line.
    CLASS-DATA mt_customer_id TYPE SORTED TABLE OF /dmo/customer_id WITH UNIQUE KEY table_line.
ENDCLASS.


CLASS lcl_common_checks IMPLEMENTATION.
  METHOD is_currency_code_valid.
    CLEAR rv_is_valid.
    IF mt_currency_code IS INITIAL.
      " We should use TCURC, but this is not released for "ABAP for SAP Cloud Platform"
      SELECT DISTINCT currency FROM i_currency INTO TABLE @mt_currency_code. "#EC CI_NOWHERE
    ENDIF.
    READ TABLE mt_currency_code TRANSPORTING NO FIELDS WITH TABLE KEY table_line = iv_currency_code.
    IF sy-subrc = 0.
      rv_is_valid = abap_true.
    ELSE.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>currency_unknown  currency_code = iv_currency_code ) TO ct_messages.
    ENDIF.
  ENDMETHOD.


  METHOD is_customer_id_valid.
    CLEAR rv_is_valid.
    IF mt_customer_id IS INITIAL.
      " There may be very many customers, but we only store the ID in the internal table
      SELECT DISTINCT customer_id FROM /dmo/customer INTO TABLE @mt_customer_id. "#EC CI_NOWHERE
    ENDIF.
    READ TABLE mt_customer_id TRANSPORTING NO FIELDS WITH TABLE KEY table_line = iv_customer_id.
    IF sy-subrc = 0.
      rv_is_valid = abap_true.
    ELSE.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>customer_unkown  customer_id = iv_customer_id ) TO ct_messages.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_booking_supplement DEFINITION DEFERRED.
CLASS lcl_booking_supplement_buffer DEFINITION FINAL CREATE PRIVATE FRIENDS ltc_booking_supplement.
  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_booking_supplement_buffer.
    METHODS adjust_numbers IMPORTING it_booking_mapping             TYPE /dmo/if_flight_legacy=>tt_ln_booking_mapping
                           RETURNING VALUE(rt_bookingsuppl_mapping) TYPE /dmo/if_flight_legacy=>tt_ln_bookingsuppl_mapping.
    METHODS save.
    METHODS initialize.
    "! Prepare changes in a temporary buffer
    "! @parameter iv_no_delete_check | In some cases we do not need to check the existence of a record to be deleted, as this check has been done before.
    "!                               | E.g. delete all subnodes of a node to be deleted.  In this case we have read the subnodes to get their keys.
    METHODS cud_prep IMPORTING it_booking_supplement  TYPE /dmo/t_booking_supplement
                               it_booking_supplementx TYPE /dmo/t_booking_supplementx
                               iv_no_delete_check     TYPE abap_bool OPTIONAL
                     EXPORTING et_booking_supplement  TYPE /dmo/t_booking_supplement
                               et_messages            TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    "! Add content of the temporary buffer to the real buffer and clear the temporary buffer
    METHODS cud_copy.
    "! Discard content of the temporary buffer
    METHODS cud_disc.
    "! Get all Booking Supplements for given Bookings
    METHODS get IMPORTING it_booking_supplement  TYPE /dmo/t_booking_supplement
                          iv_include_buffer      TYPE abap_boolean
                          iv_include_temp_buffer TYPE abap_boolean
                EXPORTING et_booking_supplement  TYPE /dmo/t_booking_supplement.

  PRIVATE SECTION.
    CLASS-DATA go_instance TYPE REF TO lcl_booking_supplement_buffer.
    " Main buffer
    DATA: mt_create_buffer TYPE /dmo/t_booking_supplement,
          mt_update_buffer TYPE /dmo/t_booking_supplement,
          mt_delete_buffer TYPE /dmo/t_booking_supplement_key.
    " Temporary buffer valid during create / update / delete Travel
    DATA: mt_create_buffer_2 TYPE /dmo/t_booking_supplement,
          mt_update_buffer_2 TYPE /dmo/t_booking_supplement,
          mt_delete_buffer_2 TYPE /dmo/t_booking_supplement_key.

    DATA mt_supplement TYPE SORTED TABLE OF /dmo/supplement WITH UNIQUE KEY supplement_id.

    METHODS _create IMPORTING it_booking_supplement TYPE /dmo/t_booking_supplement
                    EXPORTING et_booking_supplement TYPE /dmo/t_booking_supplement
                              et_messages           TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS _update IMPORTING it_booking_supplement  TYPE /dmo/t_booking_supplement
                              it_booking_supplementx TYPE /dmo/t_booking_supplementx
                    EXPORTING et_booking_supplement  TYPE /dmo/t_booking_supplement
                              et_messages            TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS _delete IMPORTING it_booking_supplement TYPE /dmo/t_booking_supplement
                              iv_no_delete_check    TYPE abap_bool
                    EXPORTING et_messages           TYPE /dmo/if_flight_legacy=>tt_if_t100_message.

    METHODS _check IMPORTING is_booking_supplement  TYPE /dmo/book_suppl
                             is_booking_supplementx TYPE /dmo/s_booking_supplementx OPTIONAL
                             iv_change_mode         TYPE /dmo/cl_flight_legacy=>ty_change_mode
                   CHANGING  ct_messages            TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                   RETURNING VALUE(rv_is_valid)     TYPE abap_bool.
    METHODS _check_supplement IMPORTING is_booking_supplement  TYPE /dmo/book_suppl
                                        is_booking_supplementx TYPE /dmo/s_booking_supplementx OPTIONAL
                                        iv_change_mode         TYPE /dmo/cl_flight_legacy=>ty_change_mode
                              CHANGING  ct_messages            TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                              RETURNING VALUE(rv_is_valid)     TYPE abap_bool.
    METHODS _check_currency_code IMPORTING is_booking_supplement  TYPE /dmo/book_suppl
                                           is_booking_supplementx TYPE /dmo/s_booking_supplementx OPTIONAL
                                           iv_change_mode         TYPE /dmo/cl_flight_legacy=>ty_change_mode
                                 CHANGING  ct_messages            TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                                 RETURNING VALUE(rv_is_valid)     TYPE abap_bool.
    METHODS _determine IMPORTING iv_change_mode        TYPE /dmo/cl_flight_legacy=>ty_change_mode
                       CHANGING  cs_booking_supplement TYPE /dmo/book_suppl.
ENDCLASS.


CLASS lcl_booking_supplement_buffer IMPLEMENTATION.
  METHOD get_instance.
    go_instance = COND #( WHEN go_instance IS BOUND THEN go_instance ELSE NEW #( ) ).
    ro_instance = go_instance.
  ENDMETHOD.


  METHOD _create.
    CLEAR et_booking_supplement.
    CLEAR et_messages.

    TYPES: BEGIN OF ts_travel_booking_suppl_id,
             travel_id             TYPE /dmo/travel_id,
             booking_id            TYPE /dmo/booking_id,
             booking_supplement_id TYPE /dmo/booking_supplement_id,
           END OF ts_travel_booking_suppl_id,
           tt_travel_booking_suppl_id TYPE SORTED TABLE OF ts_travel_booking_suppl_id WITH UNIQUE KEY travel_id  booking_id  booking_supplement_id.
    DATA lt_travel_booking_suppl_id TYPE tt_travel_booking_suppl_id.

    CHECK it_booking_supplement IS NOT INITIAL.

    SELECT FROM /dmo/book_suppl FIELDS travel_id, booking_id, booking_supplement_id
      FOR ALL ENTRIES IN @it_booking_supplement WHERE travel_id = @it_booking_supplement-travel_id AND booking_id = @it_booking_supplement-booking_id AND booking_supplement_id = @it_booking_supplement-booking_supplement_id
      INTO CORRESPONDING FIELDS OF TABLE @lt_travel_booking_suppl_id.

    LOOP AT it_booking_supplement INTO DATA(ls_booking_supplement_create) ##INTO_OK.
      " Booking_Supplement_ID key must not be initial
      IF ls_booking_supplement_create-booking_supplement_id IS INITIAL.
        APPEND NEW /dmo/cx_flight_legacy( textid     = /dmo/cx_flight_legacy=>booking_supplement_no_key
                                          travel_id  = ls_booking_supplement_create-travel_id
                                          booking_id = ls_booking_supplement_create-booking_id ) TO et_messages.
        RETURN.
      ENDIF.

      " Booking_Supplement_ID key check DB
      READ TABLE lt_travel_booking_suppl_id TRANSPORTING NO FIELDS WITH TABLE KEY travel_id             = ls_booking_supplement_create-travel_id
                                                                                  booking_id            = ls_booking_supplement_create-booking_id
                                                                                  booking_supplement_id = ls_booking_supplement_create-booking_supplement_id.
      IF sy-subrc = 0.
        APPEND NEW /dmo/cx_flight_legacy( textid     = /dmo/cx_flight_legacy=>booking_supplement_exists
                                          travel_id  = ls_booking_supplement_create-travel_id
                                          booking_id = ls_booking_supplement_create-booking_id
                                          booking_supplement_id = ls_booking_supplement_create-booking_supplement_id ) TO et_messages.
        RETURN.
      ENDIF.

      " Booking_Supplement_ID key check Buffer
      READ TABLE mt_create_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id             = ls_booking_supplement_create-travel_id
                                                                        booking_id            = ls_booking_supplement_create-booking_id
                                                                        booking_supplement_id = ls_booking_supplement_create-booking_supplement_id.
      IF sy-subrc = 0.
        APPEND NEW /dmo/cx_flight_legacy( textid     = /dmo/cx_flight_legacy=>booking_supplement_exists
                                          travel_id  = ls_booking_supplement_create-travel_id
                                          booking_id = ls_booking_supplement_create-booking_id
                                          booking_supplement_id = ls_booking_supplement_create-booking_supplement_id ) TO et_messages.
        RETURN.
      ENDIF.

      " Checks
      IF _check( EXPORTING is_booking_supplement = ls_booking_supplement_create
                           iv_change_mode        = /dmo/cl_flight_legacy=>change_mode-create
                 CHANGING  ct_messages           = et_messages ) = abap_false.
        RETURN.
      ENDIF.

      " standard determinations
      _determine( EXPORTING iv_change_mode        = /dmo/cl_flight_legacy=>change_mode-create
                  CHANGING  cs_booking_supplement = ls_booking_supplement_create ).

      INSERT ls_booking_supplement_create INTO TABLE mt_create_buffer_2.
    ENDLOOP.

    et_booking_supplement = mt_create_buffer_2.
  ENDMETHOD.


  METHOD _update.
    CLEAR et_booking_supplement.
    CLEAR et_messages.

    CHECK it_booking_supplement IS NOT INITIAL.

    " Check for empty keys
    LOOP AT it_booking_supplement ASSIGNING FIELD-SYMBOL(<s_booking_supplement_update>) WHERE booking_supplement_id = 0. "#EC CI_SORTSEQ
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_supplement_no_key  travel_id = <s_booking_supplement_update>-travel_id  booking_id = <s_booking_supplement_update>-booking_id ) TO et_messages.
      RETURN.
    ENDLOOP.

    DATA lt_book_suppl TYPE SORTED TABLE OF /dmo/book_suppl WITH UNIQUE KEY travel_id  booking_id  booking_supplement_id.
    SELECT * FROM /dmo/book_suppl FOR ALL ENTRIES IN @it_booking_supplement WHERE travel_id             = @it_booking_supplement-travel_id
                                                                              AND booking_id            = @it_booking_supplement-booking_id
                                                                              AND booking_supplement_id = @it_booking_supplement-booking_supplement_id
                                                                            INTO TABLE @lt_book_suppl.

    FIELD-SYMBOLS <s_buffer_booking_supplement> TYPE /dmo/book_suppl.
    DATA ls_buffer_booking_supplement TYPE /dmo/book_suppl.
    LOOP AT it_booking_supplement ASSIGNING <s_booking_supplement_update>.
      UNASSIGN <s_buffer_booking_supplement>.

      READ TABLE mt_delete_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id             = <s_booking_supplement_update>-travel_id
                                                                        booking_id            = <s_booking_supplement_update>-booking_id
                                                                        booking_supplement_id = <s_booking_supplement_update>-booking_supplement_id.
      IF sy-subrc = 0." Error: Record to be updated marked for deletion
        APPEND NEW /dmo/cx_flight_legacy( textid                = /dmo/cx_flight_legacy=>booking_supplement_unknown
                                          travel_id             = <s_booking_supplement_update>-travel_id
                                          booking_id            = <s_booking_supplement_update>-booking_id
                                          booking_supplement_id = <s_booking_supplement_update>-booking_supplement_id ) TO et_messages.
        RETURN.
      ENDIF.

      IF <s_buffer_booking_supplement> IS NOT ASSIGNED." Special case: record already in temporary create buffer
        READ TABLE mt_create_buffer_2 ASSIGNING <s_buffer_booking_supplement> WITH TABLE KEY travel_id             = <s_booking_supplement_update>-travel_id
                                                                                             booking_id            = <s_booking_supplement_update>-booking_id
                                                                                             booking_supplement_id = <s_booking_supplement_update>-booking_supplement_id.
      ENDIF.

      IF <s_buffer_booking_supplement> IS NOT ASSIGNED." Special case: record already in create buffer
        READ TABLE mt_create_buffer INTO ls_buffer_booking_supplement WITH TABLE KEY travel_id             = <s_booking_supplement_update>-travel_id
                                                                                     booking_id            = <s_booking_supplement_update>-booking_id
                                                                                     booking_supplement_id = <s_booking_supplement_update>-booking_supplement_id.
        IF sy-subrc = 0.
          INSERT ls_buffer_booking_supplement INTO TABLE mt_create_buffer_2 ASSIGNING <s_buffer_booking_supplement>.
        ENDIF.
      ENDIF.

      IF <s_buffer_booking_supplement> IS NOT ASSIGNED." Special case: record already in temporary update buffer
        READ TABLE mt_update_buffer_2 ASSIGNING <s_buffer_booking_supplement> WITH TABLE KEY travel_id             = <s_booking_supplement_update>-travel_id
                                                                                             booking_id            = <s_booking_supplement_update>-booking_id
                                                                                             booking_supplement_id = <s_booking_supplement_update>-booking_supplement_id.
      ENDIF.

      IF <s_buffer_booking_supplement> IS NOT ASSIGNED." Special case: record already in update buffer
        READ TABLE mt_update_buffer INTO ls_buffer_booking_supplement WITH TABLE KEY travel_id             = <s_booking_supplement_update>-travel_id
                                                                                     booking_id            = <s_booking_supplement_update>-booking_id
                                                                                     booking_supplement_id = <s_booking_supplement_update>-booking_supplement_id.
        IF sy-subrc = 0.
          INSERT ls_buffer_booking_supplement INTO TABLE mt_update_buffer_2 ASSIGNING <s_buffer_booking_supplement>.
        ENDIF.
      ENDIF.

      IF <s_buffer_booking_supplement> IS NOT ASSIGNED." Usual case: record not already in update buffer
        READ TABLE lt_book_suppl ASSIGNING FIELD-SYMBOL(<s_booking_supplement_old>) WITH TABLE KEY travel_id             = <s_booking_supplement_update>-travel_id
                                                                                                   booking_id            = <s_booking_supplement_update>-booking_id
                                                                                                   booking_supplement_id = <s_booking_supplement_update>-booking_supplement_id.
        IF sy-subrc = 0.
          INSERT <s_booking_supplement_old> INTO TABLE mt_update_buffer_2 ASSIGNING <s_buffer_booking_supplement>.
          ASSERT sy-subrc = 0.
        ENDIF.
      ENDIF.

      " Error
      IF <s_buffer_booking_supplement> IS NOT ASSIGNED.
        APPEND NEW /dmo/cx_flight_legacy( textid                = /dmo/cx_flight_legacy=>booking_supplement_unknown
                                          travel_id             = <s_booking_supplement_update>-travel_id
                                          booking_id            = <s_booking_supplement_update>-booking_id
                                          booking_supplement_id = <s_booking_supplement_update>-booking_supplement_id ) TO et_messages.
        RETURN.
      ENDIF.

      " Merge fields to be updated
      READ TABLE it_booking_supplementx ASSIGNING FIELD-SYMBOL(<s_booking_supplementx>) WITH KEY travel_id             = <s_booking_supplement_update>-travel_id
                                                                                                 booking_id            = <s_booking_supplement_update>-booking_id
                                                                                                 booking_supplement_id = <s_booking_supplement_update>-booking_supplement_id
                                                                                                 action_code           = /dmo/if_flight_legacy=>action_code-update.
      IF sy-subrc <> 0.
        APPEND NEW /dmo/cx_flight_legacy( textid                = /dmo/cx_flight_legacy=>booking_supplement_no_control
                                          travel_id             = <s_booking_supplement_update>-travel_id
                                          booking_id            = <s_booking_supplement_update>-booking_id
                                          booking_supplement_id = <s_booking_supplement_update>-booking_supplement_id ) TO et_messages.
        RETURN.
      ENDIF.

      IF <s_booking_supplementx>-supplement_id = abap_true AND <s_booking_supplement_update>-supplement_id <> <s_buffer_booking_supplement>-supplement_id.
        " The supplement ID must not be changed (delete the record and create a new one)
        APPEND NEW /dmo/cx_flight_legacy( textid                = /dmo/cx_flight_legacy=>booking_supplement_suppl_id_u
                                          travel_id             = <s_booking_supplement_update>-travel_id
                                          booking_id            = <s_booking_supplement_update>-booking_id
                                          booking_supplement_id = <s_booking_supplement_update>-booking_supplement_id ) TO et_messages.
        RETURN.
      ENDIF.

      IF   ( <s_booking_supplementx>-price = abap_true  AND <s_booking_supplementx>-currency_code = abap_false )
        OR ( <s_booking_supplementx>-price = abap_false AND <s_booking_supplementx>-currency_code = abap_true  ).
        " Price and currency code must be changed together
        APPEND NEW /dmo/cx_flight_legacy( textid                = /dmo/cx_flight_legacy=>booking_supplement_pri_curr_u
                                          travel_id             = <s_booking_supplement_update>-travel_id
                                          booking_id            = <s_booking_supplement_update>-booking_id
                                          booking_supplement_id = <s_booking_supplement_update>-booking_supplement_id ) TO et_messages.
        RETURN.
      ENDIF.

      DATA lv_field TYPE i.
      lv_field = 5.
      DO.
        ASSIGN COMPONENT lv_field OF STRUCTURE <s_booking_supplementx> TO FIELD-SYMBOL(<v_flag>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        IF <v_flag> = abap_true.
          ASSIGN COMPONENT lv_field OF STRUCTURE <s_booking_supplement_update> TO FIELD-SYMBOL(<v_field_new>).
          ASSERT sy-subrc = 0.
          ASSIGN COMPONENT lv_field OF STRUCTURE <s_buffer_booking_supplement> TO FIELD-SYMBOL(<v_field_old>).
          ASSERT sy-subrc = 0.
          <v_field_old> = <v_field_new>.
        ENDIF.
        lv_field = lv_field + 1.
      ENDDO.

      " Checks
      IF _check( EXPORTING is_booking_supplement  = <s_buffer_booking_supplement>
                           is_booking_supplementx = <s_booking_supplementx>
                           iv_change_mode         = /dmo/cl_flight_legacy=>change_mode-update
                 CHANGING  ct_messages            = et_messages ) = abap_false.
        RETURN.
      ENDIF.

      " standard determinations
      DATA(ls_booking_supplement) = <s_buffer_booking_supplement>." Needed, as key fields must not be changed
      _determine( EXPORTING iv_change_mode        = /dmo/cl_flight_legacy=>change_mode-update
                  CHANGING  cs_booking_supplement = ls_booking_supplement ).
      <s_buffer_booking_supplement>-gr_data = ls_booking_supplement-gr_data.

      INSERT <s_buffer_booking_supplement> INTO TABLE et_booking_supplement.
    ENDLOOP.
  ENDMETHOD.


  METHOD _delete.
    CLEAR et_messages.

    CHECK it_booking_supplement IS NOT INITIAL.

    " Check for empty keys
    LOOP AT it_booking_supplement ASSIGNING FIELD-SYMBOL(<s_booking_supplement_delete>) WHERE booking_supplement_id = 0. "#EC CI_SORTSEQ
      APPEND NEW /dmo/cx_flight_legacy( textid     = /dmo/cx_flight_legacy=>booking_supplement_no_key
                                        travel_id  = <s_booking_supplement_delete>-travel_id
                                        booking_id = <s_booking_supplement_delete>-booking_id ) TO et_messages.
      RETURN.
    ENDLOOP.

    DATA(lt_booking_supplement) = it_booking_supplement.

    LOOP AT lt_booking_supplement ASSIGNING <s_booking_supplement_delete>.
      " Special case: record already in create buffer
      READ TABLE mt_create_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id             = <s_booking_supplement_delete>-travel_id
                                                                        booking_id            = <s_booking_supplement_delete>-booking_id
                                                                        booking_supplement_id = <s_booking_supplement_delete>-booking_supplement_id.
      IF sy-subrc = 0." Artificial case: Remove entry from create buffer
        INSERT VALUE #( travel_id             = <s_booking_supplement_delete>-travel_id
                        booking_id            = <s_booking_supplement_delete>-booking_id
                        booking_supplement_id = <s_booking_supplement_delete>-booking_supplement_id ) INTO TABLE mt_delete_buffer_2.
        DELETE lt_booking_supplement.
      ENDIF.
    ENDLOOP.

    IF iv_no_delete_check = abap_false.
      DATA lt_book_suppl_db TYPE /dmo/t_booking_supplement_key.
      SELECT travel_id, booking_id, booking_supplement_id FROM /dmo/book_suppl FOR ALL ENTRIES IN @lt_booking_supplement
        WHERE travel_id = @lt_booking_supplement-travel_id AND booking_id = @lt_booking_supplement-booking_id AND booking_supplement_id = @lt_booking_supplement-booking_supplement_id INTO CORRESPONDING FIELDS OF TABLE @lt_book_suppl_db.
    ENDIF.

    " Check existence and append to delete buffer
    LOOP AT lt_booking_supplement ASSIGNING <s_booking_supplement_delete>.
      IF iv_no_delete_check = abap_false.
        READ TABLE lt_book_suppl_db TRANSPORTING NO FIELDS WITH TABLE KEY travel_id             = <s_booking_supplement_delete>-travel_id
                                                                          booking_id            = <s_booking_supplement_delete>-booking_id
                                                                          booking_supplement_id = <s_booking_supplement_delete>-booking_supplement_id.
        IF sy-subrc <> 0.
          APPEND NEW /dmo/cx_flight_legacy( textid                = /dmo/cx_flight_legacy=>booking_supplement_unknown
                                            travel_id             = <s_booking_supplement_delete>-travel_id
                                            booking_id            = <s_booking_supplement_delete>-booking_id
                                            booking_supplement_id = <s_booking_supplement_delete>-booking_supplement_id ) TO et_messages.
          RETURN.
        ENDIF.
      ENDIF.
      INSERT VALUE #( travel_id             = <s_booking_supplement_delete>-travel_id
                      booking_id            = <s_booking_supplement_delete>-booking_id
                      booking_supplement_id = <s_booking_supplement_delete>-booking_supplement_id ) INTO TABLE mt_delete_buffer_2.
    ENDLOOP.
  ENDMETHOD.


  METHOD adjust_numbers.
    DATA: lt_create_buffer TYPE /dmo/t_booking_supplement.

    ASSERT mt_create_buffer_2 IS INITIAL.
    ASSERT mt_update_buffer_2 IS INITIAL.
    ASSERT mt_delete_buffer_2 IS INITIAL.

    CHECK mt_create_buffer IS NOT INITIAL.    "Any Create to be processed?

    LOOP AT mt_create_buffer INTO DATA(ls_create_buffer).
      READ TABLE it_booking_mapping INTO DATA(ls_booking_mapping) WITH KEY preliminary-travel_id  = ls_create_buffer-travel_id
                                                                           preliminary-booking_id = ls_create_buffer-booking_id.
      " If found we replace the number - otherwise we simply return the mapping a=a
      IF sy-subrc EQ 0.
        APPEND VALUE #( preliminary = VALUE #( travel_id             = ls_create_buffer-travel_id
                                               booking_id            = ls_create_buffer-booking_id
                                               booking_supplement_id = ls_create_buffer-booking_supplement_id )
                        final       = VALUE #( travel_id             = ls_booking_mapping-final-travel_id
                                               booking_id            = ls_booking_mapping-final-booking_id
                                               booking_supplement_id = ls_create_buffer-booking_supplement_id ) ) TO rt_bookingsuppl_mapping.

        ls_create_buffer-travel_id  = ls_booking_mapping-final-travel_id.
        ls_create_buffer-booking_id = ls_booking_mapping-final-booking_id.
      ELSE.
        APPEND VALUE #( preliminary = VALUE #( travel_id             = ls_create_buffer-travel_id
                                               booking_id            = ls_create_buffer-booking_id
                                               booking_supplement_id = ls_create_buffer-booking_supplement_id )
                        final       = VALUE #( travel_id             = ls_create_buffer-travel_id
                                               booking_id            = ls_create_buffer-booking_id
                                               booking_supplement_id = ls_create_buffer-booking_supplement_id ) ) TO rt_bookingsuppl_mapping.
      ENDIF.
      INSERT ls_create_buffer INTO TABLE lt_create_buffer.
    ENDLOOP.
    mt_create_buffer = lt_create_buffer.
  ENDMETHOD.

  METHOD save.
    ASSERT mt_create_buffer_2 IS INITIAL.
    ASSERT mt_update_buffer_2 IS INITIAL.
    ASSERT mt_delete_buffer_2 IS INITIAL.

    " Make sure no preliminary travel_id is remaining (late numbering use-case)
    " If this dump happens, call adjust numbers before executing save
    IF mt_create_buffer IS NOT INITIAL.
      ASSERT mt_create_buffer[ lines( mt_create_buffer ) ]-travel_id < /dmo/if_flight_legacy=>late_numbering_boundary.
    ENDIF.

    INSERT /dmo/book_suppl FROM TABLE @mt_create_buffer.
    UPDATE /dmo/book_suppl FROM TABLE @mt_update_buffer.
    DELETE /dmo/book_suppl FROM TABLE @( CORRESPONDING #( mt_delete_buffer ) ).
  ENDMETHOD.


  METHOD initialize.
    CLEAR: mt_create_buffer, mt_update_buffer, mt_delete_buffer.
  ENDMETHOD.


  METHOD _check.
    rv_is_valid = abap_true.

    IF NOT _check_supplement( EXPORTING is_booking_supplement  = is_booking_supplement
                                        is_booking_supplementx = is_booking_supplementx
                                        iv_change_mode         = iv_change_mode
                              CHANGING ct_messages             = ct_messages ).
      rv_is_valid = abap_false.
    ENDIF.

    IF NOT _check_currency_code( EXPORTING is_booking_supplement  = is_booking_supplement
                                           is_booking_supplementx = is_booking_supplementx
                                           iv_change_mode         = iv_change_mode
                                  CHANGING ct_messages             = ct_messages ).
      rv_is_valid = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD cud_prep.
    CLEAR et_booking_supplement.
    CLEAR et_messages.

    CHECK it_booking_supplement IS NOT INITIAL.

    DATA lt_booking_supplement_c  TYPE /dmo/t_booking_supplement.
    DATA lt_booking_supplement_u  TYPE /dmo/t_booking_supplement.
    DATA lt_booking_supplement_d  TYPE /dmo/t_booking_supplement.
    DATA lt_booking_supplementx_u TYPE /dmo/t_booking_supplementx.
    LOOP AT it_booking_supplement ASSIGNING FIELD-SYMBOL(<s_booking_supplement>).
      READ TABLE it_booking_supplementx ASSIGNING FIELD-SYMBOL(<s_booking_supplementx>) WITH TABLE KEY travel_id             = <s_booking_supplement>-travel_id
                                                                                                       booking_id            = <s_booking_supplement>-booking_id
                                                                                                       booking_supplement_id = <s_booking_supplement>-booking_supplement_id.
      IF sy-subrc <> 0.
        APPEND NEW /dmo/cx_flight_legacy( textid                = /dmo/cx_flight_legacy=>booking_supplement_no_control
                                          travel_id             = <s_booking_supplement>-travel_id
                                          booking_id            = <s_booking_supplement>-booking_id
                                          booking_supplement_id = <s_booking_supplement>-booking_supplement_id ) TO et_messages.
        RETURN.
      ENDIF.
      CASE CONV /dmo/if_flight_legacy=>action_code_enum( <s_booking_supplementx>-action_code ).
        WHEN /dmo/if_flight_legacy=>action_code-create.
          INSERT <s_booking_supplement>  INTO TABLE lt_booking_supplement_c.
        WHEN /dmo/if_flight_legacy=>action_code-update.
          INSERT <s_booking_supplement>  INTO TABLE lt_booking_supplement_u.
          INSERT <s_booking_supplementx> INTO TABLE lt_booking_supplementx_u.
        WHEN /dmo/if_flight_legacy=>action_code-delete.
          INSERT <s_booking_supplement>  INTO TABLE lt_booking_supplement_d.
      ENDCASE.
    ENDLOOP.

    _create( EXPORTING it_booking_supplement = lt_booking_supplement_c
             IMPORTING et_booking_supplement = et_booking_supplement
                       et_messages           = et_messages ).

    _update( EXPORTING it_booking_supplement  = lt_booking_supplement_u
                       it_booking_supplementx = lt_booking_supplementx_u
             IMPORTING et_booking_supplement  = DATA(lt_booking_supplement)
                       et_messages            = DATA(lt_messages) ).
    INSERT LINES OF lt_booking_supplement INTO TABLE et_booking_supplement.
    APPEND LINES OF lt_messages TO et_messages.

    _delete( EXPORTING it_booking_supplement = lt_booking_supplement_d
                       iv_no_delete_check    = iv_no_delete_check
             IMPORTING et_messages           = lt_messages ).
    APPEND LINES OF lt_messages TO et_messages.
  ENDMETHOD.


  METHOD cud_copy.
    LOOP AT mt_create_buffer_2 ASSIGNING FIELD-SYMBOL(<s_create_buffer_2>).
      READ TABLE mt_create_buffer ASSIGNING FIELD-SYMBOL(<s_create_buffer>) WITH TABLE KEY travel_id             = <s_create_buffer_2>-travel_id
                                                                                           booking_id            = <s_create_buffer_2>-booking_id
                                                                                           booking_supplement_id = <s_create_buffer_2>-booking_supplement_id.
      IF sy-subrc <> 0.
        INSERT VALUE #( travel_id             = <s_create_buffer_2>-travel_id
                        booking_id            = <s_create_buffer_2>-booking_id
                        booking_supplement_id = <s_create_buffer_2>-booking_supplement_id ) INTO TABLE mt_create_buffer ASSIGNING <s_create_buffer>.
      ENDIF.
      <s_create_buffer>-gr_data = <s_create_buffer_2>-gr_data.
    ENDLOOP.
    LOOP AT mt_update_buffer_2 ASSIGNING FIELD-SYMBOL(<s_update_buffer_2>).
      READ TABLE mt_update_buffer ASSIGNING FIELD-SYMBOL(<s_update_buffer>) WITH TABLE KEY travel_id             = <s_update_buffer_2>-travel_id
                                                                                           booking_id            = <s_update_buffer_2>-booking_id
                                                                                           booking_supplement_id = <s_update_buffer_2>-booking_supplement_id.
      IF sy-subrc <> 0.
        INSERT VALUE #( travel_id             = <s_update_buffer_2>-travel_id
                        booking_id            = <s_update_buffer_2>-booking_id
                        booking_supplement_id = <s_update_buffer_2>-booking_supplement_id ) INTO TABLE mt_update_buffer ASSIGNING <s_update_buffer>.
      ENDIF.
      <s_update_buffer>-gr_data = <s_update_buffer_2>-gr_data.
    ENDLOOP.
    LOOP AT mt_delete_buffer_2 ASSIGNING FIELD-SYMBOL(<s_delete_buffer_2>).
      DELETE mt_create_buffer WHERE travel_id             = <s_delete_buffer_2>-travel_id
                                AND booking_id            = <s_delete_buffer_2>-booking_id
                                AND booking_supplement_id = <s_delete_buffer_2>-booking_supplement_id.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      DELETE mt_update_buffer WHERE travel_id             = <s_delete_buffer_2>-travel_id
                                AND booking_id            = <s_delete_buffer_2>-booking_id
                                AND booking_supplement_id = <s_delete_buffer_2>-booking_supplement_id.
      INSERT <s_delete_buffer_2> INTO TABLE mt_delete_buffer.
    ENDLOOP.
    CLEAR: mt_create_buffer_2, mt_update_buffer_2, mt_delete_buffer_2.
  ENDMETHOD.


  METHOD cud_disc.
    CLEAR: mt_create_buffer_2, mt_update_buffer_2, mt_delete_buffer_2.
  ENDMETHOD.


  METHOD get.
    CLEAR et_booking_supplement.

    CHECK it_booking_supplement IS NOT INITIAL.

    SELECT * FROM /dmo/book_suppl FOR ALL ENTRIES IN @it_booking_supplement WHERE travel_id  = @it_booking_supplement-travel_id
                                                                              AND booking_id = @it_booking_supplement-booking_id
      INTO TABLE @et_booking_supplement ##SELECT_FAE_WITH_LOB[DESCRIPTION]. "#EC CI_SEL_DEL

    IF iv_include_buffer = abap_true.
      LOOP AT it_booking_supplement ASSIGNING FIELD-SYMBOL(<s_booking_supplement>).
        LOOP AT mt_create_buffer ASSIGNING FIELD-SYMBOL(<s_create_buffer>) WHERE travel_id = <s_booking_supplement>-travel_id AND booking_id = <s_booking_supplement>-booking_id.
          INSERT <s_create_buffer> INTO TABLE et_booking_supplement.
        ENDLOOP.

        LOOP AT mt_update_buffer ASSIGNING FIELD-SYMBOL(<s_update_buffer>) WHERE travel_id = <s_booking_supplement>-travel_id AND booking_id = <s_booking_supplement>-booking_id.
          MODIFY TABLE et_booking_supplement FROM <s_update_buffer>.
        ENDLOOP.

        LOOP AT mt_delete_buffer ASSIGNING FIELD-SYMBOL(<s_delete_buffer>) WHERE travel_id = <s_booking_supplement>-travel_id AND booking_id = <s_booking_supplement>-booking_id.
          DELETE et_booking_supplement WHERE travel_id             = <s_delete_buffer>-travel_id
                                         AND booking_id            = <s_delete_buffer>-booking_id
                                         AND booking_supplement_id = <s_delete_buffer>-booking_supplement_id. "#EC CI_SEL_DEL
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    IF iv_include_temp_buffer = abap_true.
      LOOP AT it_booking_supplement ASSIGNING <s_booking_supplement>.
        LOOP AT mt_create_buffer_2 ASSIGNING <s_create_buffer> WHERE travel_id = <s_booking_supplement>-travel_id AND booking_id = <s_booking_supplement>-booking_id.
          DELETE et_booking_supplement WHERE travel_id             = <s_create_buffer>-travel_id
                                         AND booking_id            = <s_create_buffer>-booking_id
                                         AND booking_supplement_id = <s_create_buffer>-booking_supplement_id. "#EC CI_SEL_DEL
          INSERT <s_create_buffer> INTO TABLE et_booking_supplement.
        ENDLOOP.

        LOOP AT mt_update_buffer_2 ASSIGNING <s_update_buffer> WHERE travel_id = <s_booking_supplement>-travel_id AND booking_id = <s_booking_supplement>-booking_id.
          MODIFY TABLE et_booking_supplement FROM <s_update_buffer>.
        ENDLOOP.

        LOOP AT mt_delete_buffer_2 ASSIGNING <s_delete_buffer> WHERE travel_id = <s_booking_supplement>-travel_id AND booking_id = <s_booking_supplement>-booking_id.
          DELETE et_booking_supplement WHERE travel_id             = <s_delete_buffer>-travel_id
                                         AND booking_id            = <s_delete_buffer>-booking_id
                                         AND booking_supplement_id = <s_delete_buffer>-booking_supplement_id. "#EC CI_SEL_DEL
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD _check_supplement.
    rv_is_valid = abap_true.
    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_booking_supplementx-supplement_id = abap_true ).
      IF mt_supplement IS INITIAL.
        SELECT * FROM /dmo/supplement INTO TABLE @mt_supplement. "#EC CI_NOWHERE
      ENDIF.
      READ TABLE mt_supplement TRANSPORTING NO FIELDS WITH TABLE KEY supplement_id = is_booking_supplement-supplement_id.
      IF sy-subrc <> 0.
        rv_is_valid = abap_false.
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>supplement_unknown  supplement_id = is_booking_supplement-supplement_id ) TO ct_messages.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD _check_currency_code.
    rv_is_valid = abap_true.
    IF   ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create AND is_booking_supplement-currency_code IS NOT INITIAL ) " Will be derived if initial
      OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_booking_supplementx-currency_code = abap_true ).
      rv_is_valid = lcl_common_checks=>is_currency_code_valid( EXPORTING iv_currency_code = is_booking_supplement-currency_code CHANGING ct_messages = ct_messages ).
    ENDIF.
  ENDMETHOD.


  METHOD _determine.
    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create.
      " Derive price and currency code if one of the fields is initial
      IF cs_booking_supplement-price IS INITIAL OR cs_booking_supplement-currency_code IS INITIAL.
        IF mt_supplement IS INITIAL.
          SELECT * FROM /dmo/supplement INTO TABLE @mt_supplement. "#EC CI_NOWHERE
        ENDIF.
        READ TABLE mt_supplement ASSIGNING FIELD-SYMBOL(<s_supplement>) WITH TABLE KEY supplement_id = cs_booking_supplement-supplement_id.
        ASSERT sy-subrc = 0. " Check has been done before
        cs_booking_supplement-price         = <s_supplement>-price.
        cs_booking_supplement-currency_code = <s_supplement>-currency_code.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_booking DEFINITION DEFERRED.
CLASS lcl_booking_buffer DEFINITION FINAL CREATE PRIVATE FRIENDS ltc_booking.
  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_booking_buffer.
    METHODS adjust_numbers IMPORTING it_travel_mapping         TYPE /dmo/if_flight_legacy=>tt_ln_travel_mapping
                           RETURNING VALUE(rt_booking_mapping) TYPE /dmo/if_flight_legacy=>tt_ln_booking_mapping.
    METHODS save.
    METHODS initialize.
    METHODS check_booking_id IMPORTING iv_travel_id       TYPE /dmo/travel_id
                                       iv_booking_id      TYPE /dmo/booking_id
                             CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                             RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    "! Prepare changes in a temporary buffer
    "! @parameter iv_no_delete_check | In some cases we do not need to check the existence of a record to be deleted, as this check has been done before.
    "!                               | E.g. delete all subnodes of a node to be deleted.  In this case we have read the subnodes to get their keys.
    METHODS cud_prep IMPORTING it_booking         TYPE /dmo/t_booking
                               it_bookingx        TYPE /dmo/t_bookingx
                               iv_no_delete_check TYPE abap_bool OPTIONAL
                     EXPORTING et_booking         TYPE /dmo/t_booking
                               et_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    "! Add content of the temporary buffer to the real buffer and clear the temporary buffer
    METHODS cud_copy.
    "! Discard content of the temporary buffer
    METHODS cud_disc.
    "! Get all Bookings for given Travels
    METHODS get IMPORTING it_booking             TYPE /dmo/t_booking
                          iv_include_buffer      TYPE abap_boolean
                          iv_include_temp_buffer TYPE abap_boolean
                EXPORTING et_booking             TYPE /dmo/t_booking.

  PRIVATE SECTION.
    CLASS-DATA go_instance TYPE REF TO lcl_booking_buffer.
    " Main buffer
    DATA: mt_create_buffer TYPE /dmo/t_booking,
          mt_update_buffer TYPE /dmo/t_booking,
          mt_delete_buffer TYPE /dmo/t_booking_key.
    " Temporary buffer valid during create / update / delete Travel
    DATA: mt_create_buffer_2 TYPE /dmo/t_booking,
          mt_update_buffer_2 TYPE /dmo/t_booking,
          mt_delete_buffer_2 TYPE /dmo/t_booking_key.

    TYPES: BEGIN OF ts_flight_key,
             carrier_id    TYPE /dmo/carrier_id,
             connection_id TYPE /dmo/connection_id,
             flight_date   TYPE /dmo/flight_date,
           END OF ts_flight_key.
    TYPES tt_flight_key TYPE SORTED TABLE OF ts_flight_key WITH UNIQUE KEY carrier_id  connection_id  flight_date.
    DATA mt_flight_key TYPE tt_flight_key.

    METHODS _create IMPORTING it_booking  TYPE /dmo/t_booking
                    EXPORTING et_booking  TYPE /dmo/t_booking
                              et_messages TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS _update IMPORTING it_booking  TYPE /dmo/t_booking
                              it_bookingx TYPE /dmo/t_bookingx
                    EXPORTING et_booking  TYPE /dmo/t_booking
                              et_messages TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS _delete IMPORTING it_booking         TYPE /dmo/t_booking
                              iv_no_delete_check TYPE abap_bool
                    EXPORTING et_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message.

    METHODS _check IMPORTING is_booking         TYPE /dmo/booking
                             is_bookingx        TYPE /dmo/s_bookingx OPTIONAL
                             iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
                   CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                   RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    METHODS _check_booking_date IMPORTING is_booking         TYPE /dmo/booking
                                          is_bookingx        TYPE /dmo/s_bookingx OPTIONAL
                                          iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
                                CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                                RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    METHODS _check_customer IMPORTING is_booking         TYPE /dmo/booking
                                      is_bookingx        TYPE /dmo/s_bookingx OPTIONAL
                                      iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
                            CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                            RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    METHODS _check_flight IMPORTING is_booking         TYPE /dmo/booking
                                    is_bookingx        TYPE /dmo/s_bookingx OPTIONAL
                                    iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
                          CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                          RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    METHODS _check_currency_code IMPORTING is_booking         TYPE /dmo/booking
                                           is_bookingx        TYPE /dmo/s_bookingx OPTIONAL
                                           iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
                                 CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                                 RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    METHODS _determine IMPORTING iv_change_mode TYPE /dmo/cl_flight_legacy=>ty_change_mode
                       CHANGING  cs_booking     TYPE /dmo/booking.
ENDCLASS.

CLASS lcl_booking_buffer IMPLEMENTATION.
  METHOD get_instance.
    go_instance = COND #( WHEN go_instance IS BOUND THEN go_instance ELSE NEW #( ) ).
    ro_instance = go_instance.
  ENDMETHOD.


  METHOD _create.
    CLEAR et_booking.
    CLEAR et_messages.

    TYPES: BEGIN OF ts_travel_booking_id,
             travel_id  TYPE /dmo/travel_id,
             booking_id TYPE /dmo/booking_id,
           END OF ts_travel_booking_id,
           tt_travel_booking_id TYPE SORTED TABLE OF ts_travel_booking_id WITH UNIQUE KEY travel_id booking_id.
    DATA lt_travel_booking_id TYPE tt_travel_booking_id.

    CHECK it_booking IS NOT INITIAL.

    SELECT FROM /dmo/booking FIELDS travel_id, booking_id FOR ALL ENTRIES IN @it_booking WHERE travel_id = @it_booking-travel_id AND booking_id = @it_booking-booking_id INTO CORRESPONDING FIELDS OF TABLE @lt_travel_booking_id.

    LOOP AT it_booking INTO DATA(ls_booking_create) ##INTO_OK.
      " Booking_ID key must not be initial
      IF ls_booking_create-booking_id IS INITIAL.
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_no_key  travel_id = ls_booking_create-travel_id ) TO et_messages.
        RETURN.
      ENDIF.

      " Booking_ID key check DB
      READ TABLE lt_travel_booking_id TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = ls_booking_create-travel_id  booking_id = ls_booking_create-booking_id.
      IF sy-subrc = 0.
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_exists  travel_id = ls_booking_create-travel_id  booking_id = ls_booking_create-booking_id ) TO et_messages.
        RETURN.
      ENDIF.

      " Booking_ID key check Buffer
      READ TABLE mt_create_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = ls_booking_create-travel_id  booking_id = ls_booking_create-booking_id.
      IF sy-subrc = 0.
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_exists  travel_id = ls_booking_create-travel_id  booking_id = ls_booking_create-booking_id ) TO et_messages.
        RETURN.
      ENDIF.

      " Checks
      IF _check( EXPORTING is_booking     = ls_booking_create
                           iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create
                 CHANGING  ct_messages    = et_messages ) = abap_false.
        RETURN.
      ENDIF.

      " standard determinations
      _determine( EXPORTING iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create
                  CHANGING cs_booking      = ls_booking_create ).

      INSERT ls_booking_create INTO TABLE mt_create_buffer_2.
    ENDLOOP.

    et_booking = mt_create_buffer_2.
  ENDMETHOD.


  METHOD _update.
    CLEAR et_booking.
    CLEAR et_messages.

    CHECK it_booking IS NOT INITIAL.

    " Check for empty keys
    LOOP AT it_booking ASSIGNING FIELD-SYMBOL(<s_booking_update>) WHERE booking_id = 0. "#EC CI_SORTSEQ
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_no_key  travel_id = <s_booking_update>-travel_id ) TO et_messages.
      RETURN.
    ENDLOOP.

    DATA lt_booking TYPE SORTED TABLE OF /dmo/booking WITH UNIQUE KEY travel_id  booking_id.
    SELECT * FROM /dmo/booking FOR ALL ENTRIES IN @it_booking WHERE travel_id = @it_booking-travel_id AND booking_id = @it_booking-booking_id INTO TABLE @lt_booking.

    FIELD-SYMBOLS <s_buffer_booking> TYPE /dmo/booking.
    DATA ls_buffer_booking TYPE /dmo/booking.
    LOOP AT it_booking ASSIGNING <s_booking_update>.
      UNASSIGN <s_buffer_booking>.

      READ TABLE mt_delete_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id.
      IF sy-subrc = 0." Error: Record to be updated marked for deletion
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_unknown  travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id ) TO et_messages.
        RETURN.
      ENDIF.

      IF <s_buffer_booking> IS NOT ASSIGNED." Special case: record already in temporary create buffer
        READ TABLE mt_create_buffer_2 ASSIGNING <s_buffer_booking> WITH TABLE KEY travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id.
      ENDIF.

      IF <s_buffer_booking> IS NOT ASSIGNED." Special case: record already in create buffer
        READ TABLE mt_create_buffer INTO ls_buffer_booking WITH TABLE KEY travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id.
        IF sy-subrc = 0.
          INSERT ls_buffer_booking INTO TABLE mt_create_buffer_2 ASSIGNING <s_buffer_booking>.
        ENDIF.
      ENDIF.

      IF <s_buffer_booking> IS NOT ASSIGNED." Special case: record already in temporary update buffer
        READ TABLE mt_update_buffer_2 ASSIGNING <s_buffer_booking> WITH TABLE KEY travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id.
      ENDIF.

      IF <s_buffer_booking> IS NOT ASSIGNED." Special case: record already in update buffer
        READ TABLE mt_update_buffer INTO ls_buffer_booking WITH TABLE KEY travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id.
        IF sy-subrc = 0.
          INSERT ls_buffer_booking INTO TABLE mt_update_buffer_2 ASSIGNING <s_buffer_booking>.
        ENDIF.
      ENDIF.

      IF <s_buffer_booking> IS NOT ASSIGNED." Usual case: record not already in update buffer
        READ TABLE lt_booking ASSIGNING FIELD-SYMBOL(<s_booking_old>) WITH TABLE KEY travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id.
        IF sy-subrc = 0.
          INSERT <s_booking_old> INTO TABLE mt_update_buffer_2 ASSIGNING <s_buffer_booking>.
          ASSERT sy-subrc = 0.
        ENDIF.
      ENDIF.

      " Error
      IF <s_buffer_booking> IS NOT ASSIGNED.
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_unknown  travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id ) TO et_messages.
        RETURN.
      ENDIF.

      " Merge fields to be updated
      READ TABLE it_bookingx ASSIGNING FIELD-SYMBOL(<s_bookingx>) WITH KEY travel_id   = <s_booking_update>-travel_id
                                                                           booking_id  = <s_booking_update>-booking_id
                                                                           action_code = /dmo/if_flight_legacy=>action_code-update.
      IF sy-subrc <> 0.
        APPEND NEW /dmo/cx_flight_legacy( textid     = /dmo/cx_flight_legacy=>booking_no_control
                                          travel_id  = <s_booking_update>-travel_id
                                          booking_id = <s_booking_update>-booking_id ) TO et_messages.
        RETURN.
      ENDIF.

      IF   ( <s_bookingx>-carrier_id    = abap_true AND ( <s_booking_update>-carrier_id    <> <s_buffer_booking>-carrier_id    ) )
        OR ( <s_bookingx>-connection_id = abap_true AND ( <s_booking_update>-connection_id <> <s_buffer_booking>-connection_id ) )
        OR ( <s_bookingx>-flight_date   = abap_true AND ( <s_booking_update>-flight_date   <> <s_buffer_booking>-flight_date   ) ).
        " The flight must not be changed (delete the record and create a new one)
        APPEND NEW /dmo/cx_flight_legacy( textid     = /dmo/cx_flight_legacy=>booking_flight_u
                                          travel_id  = <s_booking_update>-travel_id
                                          booking_id = <s_booking_update>-booking_id ) TO et_messages.
        RETURN.
      ENDIF.

      IF   ( <s_bookingx>-flight_price = abap_true  AND <s_bookingx>-currency_code = abap_false )
        OR ( <s_bookingx>-flight_price = abap_false AND <s_bookingx>-currency_code = abap_true  ).
        " Price and currency code must be changed together
        APPEND NEW /dmo/cx_flight_legacy( textid     = /dmo/cx_flight_legacy=>booking_price_currency_u
                                          travel_id  = <s_booking_update>-travel_id
                                          booking_id = <s_booking_update>-booking_id ) TO et_messages.
        RETURN.
      ENDIF.

      DATA lv_field TYPE i.
      lv_field = 4.
      DO.
        ASSIGN COMPONENT lv_field OF STRUCTURE <s_bookingx> TO FIELD-SYMBOL(<v_flag>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        IF <v_flag> = abap_true.
          ASSIGN COMPONENT lv_field OF STRUCTURE <s_booking_update> TO FIELD-SYMBOL(<v_field_new>).
          ASSERT sy-subrc = 0.
          ASSIGN COMPONENT lv_field OF STRUCTURE <s_buffer_booking> TO FIELD-SYMBOL(<v_field_old>).
          ASSERT sy-subrc = 0.
          <v_field_old> = <v_field_new>.
        ENDIF.
        lv_field = lv_field + 1.
      ENDDO.

      " Checks
      IF _check( EXPORTING is_booking     = <s_buffer_booking>
                           is_bookingx    = <s_bookingx>
                           iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update
                 CHANGING  ct_messages    = et_messages ) = abap_false.
        RETURN.
      ENDIF.

      " standard determinations
      DATA(ls_booking) = <s_buffer_booking>." Needed, as key fields must not be changed
      _determine( EXPORTING iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update
                  CHANGING  cs_booking     = ls_booking ).
      <s_buffer_booking>-gr_data = ls_booking-gr_data.

      INSERT <s_buffer_booking> INTO TABLE et_booking.
    ENDLOOP.
  ENDMETHOD.


  METHOD _delete.
    CLEAR et_messages.

    CHECK it_booking IS NOT INITIAL.

    " Check for empty keys
    LOOP AT it_booking ASSIGNING FIELD-SYMBOL(<s_booking_delete>) WHERE booking_id = 0. "#EC CI_SORTSEQ
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_no_key  travel_id = <s_booking_delete>-travel_id ) TO et_messages.
      RETURN.
    ENDLOOP.

    DATA(lt_booking) = it_booking.

    LOOP AT lt_booking ASSIGNING <s_booking_delete>.
      " Special case: record already in create buffer
      READ TABLE mt_create_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = <s_booking_delete>-travel_id  booking_id = <s_booking_delete>-booking_id.
      IF sy-subrc = 0.
        INSERT VALUE #( travel_id = <s_booking_delete>-travel_id  booking_id = <s_booking_delete>-booking_id ) INTO TABLE mt_delete_buffer_2.
        DELETE lt_booking.
      ENDIF.
    ENDLOOP.

    IF iv_no_delete_check = abap_false.
      DATA lt_booking_db TYPE /dmo/t_booking_key.
      SELECT travel_id, booking_id FROM /dmo/booking FOR ALL ENTRIES IN @lt_booking WHERE travel_id = @lt_booking-travel_id AND booking_id = @lt_booking-booking_id INTO CORRESPONDING FIELDS OF TABLE @lt_booking_db.
    ENDIF.

    " Check existence and append to delete buffer
    LOOP AT lt_booking ASSIGNING <s_booking_delete>.
      IF iv_no_delete_check = abap_false.
        READ TABLE lt_booking_db TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = <s_booking_delete>-travel_id  booking_id = <s_booking_delete>-booking_id.
        IF sy-subrc <> 0.
          APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_unknown  travel_id = <s_booking_delete>-travel_id  booking_id = <s_booking_delete>-booking_id ) TO et_messages.
          RETURN.
        ENDIF.
      ENDIF.
      INSERT VALUE #( travel_id = <s_booking_delete>-travel_id  booking_id = <s_booking_delete>-booking_id ) INTO TABLE mt_delete_buffer_2.
    ENDLOOP.
  ENDMETHOD.


  METHOD adjust_numbers.
    DATA: lt_create_buffer TYPE /dmo/t_booking.

    ASSERT mt_create_buffer_2 IS INITIAL.
    ASSERT mt_update_buffer_2 IS INITIAL.
    ASSERT mt_delete_buffer_2 IS INITIAL.

    CHECK mt_create_buffer IS NOT INITIAL.   "Any Create to be processed?

    LOOP AT mt_create_buffer INTO DATA(ls_create_buffer).
      READ TABLE it_travel_mapping INTO DATA(ls_travel_mapping) WITH KEY preliminary-travel_id = ls_create_buffer-travel_id.

      " If found we replace the number - otherwise we simply return the mapping a=a
      IF sy-subrc EQ 0.
        APPEND VALUE #( preliminary = VALUE #( travel_id  = ls_create_buffer-travel_id
                                               booking_id = ls_create_buffer-booking_id )
                        final       = VALUE #( travel_id  = ls_travel_mapping-final-travel_id
                                               booking_id = ls_create_buffer-booking_id ) ) TO rt_booking_mapping.

        ls_create_buffer-travel_id = ls_travel_mapping-final-travel_id.
      ELSE.
        APPEND VALUE #( preliminary = VALUE #( travel_id  = ls_create_buffer-travel_id
                                               booking_id = ls_create_buffer-booking_id )
                        final       = VALUE #( travel_id  = ls_create_buffer-travel_id
                                               booking_id = ls_create_buffer-booking_id ) ) TO rt_booking_mapping.
      ENDIF.
      INSERT ls_create_buffer INTO TABLE lt_create_buffer.
    ENDLOOP.
    mt_create_buffer = lt_create_buffer.
  ENDMETHOD.

  METHOD save.
    ASSERT mt_create_buffer_2 IS INITIAL.
    ASSERT mt_update_buffer_2 IS INITIAL.
    ASSERT mt_delete_buffer_2 IS INITIAL.

    " Make sure no preliminary travel_id is remaining (late numbering use-case)
    " If this dump happens, call adjust numbers before executing save
    IF mt_create_buffer IS NOT INITIAL.
      ASSERT mt_create_buffer[ lines( mt_create_buffer ) ]-travel_id < /dmo/if_flight_legacy=>late_numbering_boundary.
    ENDIF.

    INSERT /dmo/booking FROM TABLE @mt_create_buffer.
    UPDATE /dmo/booking FROM TABLE @mt_update_buffer.
    DELETE /dmo/booking FROM TABLE @( CORRESPONDING #( mt_delete_buffer ) ).
  ENDMETHOD.


  METHOD initialize.
    CLEAR: mt_create_buffer, mt_update_buffer, mt_delete_buffer.
  ENDMETHOD.


  METHOD check_booking_id." Here we can safely assume that the Travel ID has already been checked!
    rv_is_valid = abap_false.

    IF iv_booking_id IS INITIAL.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_no_key  travel_id = iv_travel_id ) TO ct_messages.
      RETURN.
    ENDIF.

    IF line_exists( mt_delete_buffer[ travel_id = iv_travel_id  booking_id = iv_booking_id ] ).
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_unknown  travel_id = iv_travel_id  booking_id = iv_booking_id ) TO ct_messages.
      RETURN.
    ENDIF.

    IF line_exists( mt_delete_buffer_2[ travel_id = iv_travel_id  booking_id = iv_booking_id ] ).
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_unknown  travel_id = iv_travel_id  booking_id = iv_booking_id ) TO ct_messages.
      RETURN.
    ENDIF.

    IF line_exists( mt_create_buffer[ travel_id = iv_travel_id  booking_id = iv_booking_id ] ).
      rv_is_valid = abap_true.
      RETURN.
    ENDIF.

    IF line_exists( mt_create_buffer_2[ travel_id = iv_travel_id  booking_id = iv_booking_id ] ).
      rv_is_valid = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE FROM /dmo/booking FIELDS @abap_true WHERE travel_id = @iv_travel_id AND booking_id = @iv_booking_id INTO @DATA(lv_db_exists).
    IF lv_db_exists = abap_true.
      rv_is_valid = abap_true.
      RETURN.
    ENDIF.

    APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_unknown  travel_id = iv_travel_id  booking_id = iv_booking_id ) TO ct_messages.
  ENDMETHOD.


  METHOD cud_prep.
    CLEAR et_booking.
    CLEAR et_messages.

    CHECK it_booking IS NOT INITIAL.

    DATA lt_booking_c  TYPE /dmo/t_booking.
    DATA lt_booking_u  TYPE /dmo/t_booking.
    DATA lt_booking_d  TYPE /dmo/t_booking.
    DATA lt_bookingx_u TYPE /dmo/t_bookingx.
    LOOP AT it_booking ASSIGNING FIELD-SYMBOL(<s_booking>).
      READ TABLE it_bookingx ASSIGNING FIELD-SYMBOL(<s_bookingx>) WITH TABLE KEY travel_id = <s_booking>-travel_id  booking_id = <s_booking>-booking_id.
      IF sy-subrc <> 0.
        APPEND NEW /dmo/cx_flight_legacy( textid     = /dmo/cx_flight_legacy=>booking_no_control
                                          travel_id  = <s_booking>-travel_id
                                          booking_id = <s_booking>-booking_id ) TO et_messages.
        RETURN.
      ENDIF.
      CASE CONV /dmo/if_flight_legacy=>action_code_enum( <s_bookingx>-action_code ).
        WHEN /dmo/if_flight_legacy=>action_code-create.
          INSERT <s_booking>  INTO TABLE lt_booking_c.
        WHEN /dmo/if_flight_legacy=>action_code-update.
          INSERT <s_booking>  INTO TABLE lt_booking_u.
          INSERT <s_bookingx> INTO TABLE lt_bookingx_u.
        WHEN /dmo/if_flight_legacy=>action_code-delete.
          INSERT <s_booking>  INTO TABLE lt_booking_d.
      ENDCASE.
    ENDLOOP.

    _create( EXPORTING it_booking  = lt_booking_c
             IMPORTING et_booking  = et_booking
                       et_messages = et_messages ).

    _update( EXPORTING it_booking  = lt_booking_u
                       it_bookingx = lt_bookingx_u
             IMPORTING et_booking  = DATA(lt_booking)
                       et_messages = DATA(lt_messages) ).
    INSERT LINES OF lt_booking INTO TABLE et_booking.
    APPEND LINES OF lt_messages TO et_messages.

    _delete( EXPORTING it_booking         = lt_booking_d
                       iv_no_delete_check = iv_no_delete_check
             IMPORTING et_messages        = lt_messages ).
    APPEND LINES OF lt_messages TO et_messages.
  ENDMETHOD.


  METHOD cud_copy.
    LOOP AT mt_create_buffer_2 ASSIGNING FIELD-SYMBOL(<s_create_buffer_2>).
      READ TABLE mt_create_buffer ASSIGNING FIELD-SYMBOL(<s_create_buffer>) WITH TABLE KEY travel_id = <s_create_buffer_2>-travel_id  booking_id = <s_create_buffer_2>-booking_id.
      IF sy-subrc <> 0.
        INSERT VALUE #( travel_id = <s_create_buffer_2>-travel_id  booking_id = <s_create_buffer_2>-booking_id ) INTO TABLE mt_create_buffer ASSIGNING <s_create_buffer>.
      ENDIF.
      <s_create_buffer>-gr_data = <s_create_buffer_2>-gr_data.
    ENDLOOP.
    LOOP AT mt_update_buffer_2 ASSIGNING FIELD-SYMBOL(<s_update_buffer_2>).
      READ TABLE mt_update_buffer ASSIGNING FIELD-SYMBOL(<s_update_buffer>) WITH TABLE KEY travel_id = <s_update_buffer_2>-travel_id  booking_id = <s_update_buffer_2>-booking_id.
      IF sy-subrc <> 0.
        INSERT VALUE #( travel_id = <s_update_buffer_2>-travel_id  booking_id = <s_update_buffer_2>-booking_id ) INTO TABLE mt_update_buffer ASSIGNING <s_update_buffer>.
      ENDIF.
      <s_update_buffer>-gr_data = <s_update_buffer_2>-gr_data.
    ENDLOOP.
    LOOP AT mt_delete_buffer_2 ASSIGNING FIELD-SYMBOL(<s_delete_buffer_2>).
      DELETE mt_create_buffer WHERE travel_id = <s_delete_buffer_2>-travel_id AND booking_id = <s_delete_buffer_2>-booking_id.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      DELETE mt_update_buffer WHERE travel_id = <s_delete_buffer_2>-travel_id AND booking_id = <s_delete_buffer_2>-booking_id.
      INSERT <s_delete_buffer_2> INTO TABLE mt_delete_buffer.
    ENDLOOP.
    CLEAR: mt_create_buffer_2, mt_update_buffer_2, mt_delete_buffer_2.
  ENDMETHOD.


  METHOD cud_disc.
    CLEAR: mt_create_buffer_2, mt_update_buffer_2, mt_delete_buffer_2.
  ENDMETHOD.


  METHOD get.
    CLEAR et_booking.

    CHECK it_booking IS NOT INITIAL.

    SELECT * FROM /dmo/booking FOR ALL ENTRIES IN @it_booking WHERE travel_id  = @it_booking-travel_id
      INTO TABLE @et_booking ##SELECT_FAE_WITH_LOB[DESCRIPTION]. "#EC CI_ALL_FIELDS_NEEDED "#EC CI_SEL_DEL

    IF iv_include_buffer = abap_true.
      LOOP AT it_booking ASSIGNING FIELD-SYMBOL(<s_booking>).
        LOOP AT mt_create_buffer ASSIGNING FIELD-SYMBOL(<s_create_buffer>) WHERE travel_id = <s_booking>-travel_id.
          INSERT <s_create_buffer> INTO TABLE et_booking.
        ENDLOOP.

        LOOP AT mt_update_buffer ASSIGNING FIELD-SYMBOL(<s_update_buffer>) WHERE travel_id = <s_booking>-travel_id.
          MODIFY TABLE et_booking FROM <s_update_buffer>.
        ENDLOOP.

        LOOP AT mt_delete_buffer ASSIGNING FIELD-SYMBOL(<s_delete_buffer>) WHERE travel_id = <s_booking>-travel_id.
          DELETE et_booking WHERE travel_id = <s_delete_buffer>-travel_id AND booking_id = <s_delete_buffer>-booking_id. "#EC CI_SEL_DEL
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    IF iv_include_temp_buffer = abap_true.
      LOOP AT it_booking ASSIGNING <s_booking>.
        LOOP AT mt_create_buffer_2 ASSIGNING <s_create_buffer> WHERE travel_id = <s_booking>-travel_id.
          DELETE et_booking WHERE travel_id = <s_create_buffer>-travel_id AND booking_id = <s_create_buffer>-booking_id. "#EC CI_SEL_DEL
          INSERT <s_create_buffer> INTO TABLE et_booking.
        ENDLOOP.

        LOOP AT mt_update_buffer_2 ASSIGNING <s_update_buffer> WHERE travel_id = <s_booking>-travel_id.
          MODIFY TABLE et_booking FROM <s_update_buffer>.
        ENDLOOP.

        LOOP AT mt_delete_buffer_2 ASSIGNING <s_delete_buffer> WHERE travel_id = <s_booking>-travel_id.
          DELETE et_booking WHERE travel_id = <s_delete_buffer>-travel_id AND booking_id = <s_delete_buffer>-booking_id. "#EC CI_SEL_DEL
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD _check.
    rv_is_valid = abap_true.

    IF NOT _check_booking_date( EXPORTING is_booking     = is_booking
                                          is_bookingx    = is_bookingx
                                          iv_change_mode = iv_change_mode
                                CHANGING  ct_messages    = ct_messages ).
      rv_is_valid = abap_false.
    ENDIF.

    IF NOT _check_customer( EXPORTING is_booking     = is_booking
                                      is_bookingx    = is_bookingx
                                      iv_change_mode = iv_change_mode
                            CHANGING  ct_messages    = ct_messages ).
      rv_is_valid = abap_false.
    ENDIF.

    IF NOT _check_flight( EXPORTING is_booking     = is_booking
                                    is_bookingx    = is_bookingx
                                    iv_change_mode = iv_change_mode
                          CHANGING  ct_messages    = ct_messages ).
      rv_is_valid = abap_false.
    ENDIF.

    IF NOT _check_currency_code( EXPORTING is_booking     = is_booking
                                           is_bookingx    = is_bookingx
                                           iv_change_mode = iv_change_mode
                                 CHANGING  ct_messages    = ct_messages ).
      rv_is_valid = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD _check_booking_date.
    rv_is_valid = abap_true.
    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_bookingx-booking_date = abap_true ).

      " A. Booking Date must not be initial
      " B. When the record is created it must not be in the past
      IF is_booking-booking_date IS INITIAL OR is_booking-booking_date = '' OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create AND is_booking-booking_date < cl_abap_context_info=>get_system_date( ) ).
        rv_is_valid = abap_false.
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_booking_date_invalid  travel_id = is_booking-travel_id  booking_id = is_booking-booking_id  booking_date = is_booking-booking_date ) TO ct_messages.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD _check_customer.
    rv_is_valid = abap_true.
    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_bookingx-customer_id = abap_true ).
      rv_is_valid = lcl_common_checks=>is_customer_id_valid( EXPORTING iv_customer_id = is_booking-customer_id CHANGING ct_messages = ct_messages ).
    ENDIF.
  ENDMETHOD.


  METHOD _check_flight.
    rv_is_valid = abap_true.
    IF     iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create
      OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update
        AND ( is_bookingx-carrier_id = abap_true OR is_bookingx-connection_id = abap_true OR is_bookingx-flight_date = abap_true ) ).
      IF mt_flight_key IS INITIAL.
        SELECT carrier_id, connection_id, flight_date FROM /dmo/flight INTO CORRESPONDING FIELDS OF TABLE @mt_flight_key. "#EC CI_NOWHERE
      ENDIF.
      READ TABLE mt_flight_key TRANSPORTING NO FIELDS WITH TABLE KEY carrier_id = is_booking-carrier_id  connection_id = is_booking-connection_id  flight_date = is_booking-flight_date.
      IF sy-subrc <> 0.
        rv_is_valid = abap_false.
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>flight_unknown  carrier_id = is_booking-carrier_id  connection_id = is_booking-connection_id  flight_date = is_booking-flight_date ) TO ct_messages.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD _check_currency_code.
    rv_is_valid = abap_true.
    IF   ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create AND is_booking-currency_code IS NOT INITIAL ) " Will be derived if initial
      OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_bookingx-currency_code = abap_true ).
      rv_is_valid = lcl_common_checks=>is_currency_code_valid( EXPORTING iv_currency_code = is_booking-currency_code CHANGING ct_messages = ct_messages ).
    ENDIF.
  ENDMETHOD.


  METHOD _determine.
    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create.
      " Derive price and currency code if one of the fields is initial
      IF cs_booking-flight_price IS INITIAL OR cs_booking-currency_code IS INITIAL.
        " Flight price might have changed, we need to use current flight price
        SELECT SINGLE price, currency_code FROM /dmo/flight WHERE carrier_id    = @cs_booking-carrier_id
                                                              AND connection_id = @cs_booking-connection_id
                                                              AND flight_date   = @cs_booking-flight_date INTO ( @cs_booking-flight_price, @cs_booking-currency_code ).
        ASSERT sy-subrc = 0. " Check has been done before
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_travel DEFINITION DEFERRED.
CLASS lcl_travel_buffer DEFINITION FINAL CREATE PRIVATE FRIENDS ltc_travel.
  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_travel_buffer.
    METHODS set_status_to_booked IMPORTING iv_travel_id TYPE /dmo/travel_id
                                 EXPORTING et_messages  TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS adjust_numbers RETURNING VALUE(rt_travel_mapping) TYPE /dmo/if_flight_legacy=>tt_ln_travel_mapping.
    METHODS save.
    METHODS initialize.
    METHODS check_travel_id IMPORTING iv_travel_id       TYPE /dmo/travel_id
                            CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                            RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    "! Prepare changes in a temporary buffer
    "! @parameter iv_no_delete_check | In some cases we do not need to check the existence of a record to be deleted, as this check has been done before.
    "!                               | E.g. delete all subnodes of a node to be deleted.  In this case we have read the subnodes to get their keys.
    METHODS cud_prep IMPORTING it_travel          TYPE /dmo/t_travel
                               it_travelx         TYPE /dmo/t_travelx
                               iv_no_delete_check TYPE abap_bool OPTIONAL
                               iv_numbering_mode  TYPE /dmo/if_flight_legacy=>t_numbering_mode DEFAULT /dmo/if_flight_legacy=>numbering_mode-early
                     EXPORTING et_travel          TYPE /dmo/t_travel
                               et_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    "! Add content of the temporary buffer to the real buffer and clear the temporary buffer
    METHODS cud_copy.
    "! Discard content of the temporary buffer
    METHODS cud_disc.
    METHODS get IMPORTING it_travel              TYPE /dmo/t_travel
                          iv_include_buffer      TYPE abap_boolean
                          iv_include_temp_buffer TYPE abap_boolean
                EXPORTING et_travel              TYPE /dmo/t_travel.

  PRIVATE SECTION.
    CLASS-DATA go_instance TYPE REF TO lcl_travel_buffer.

    DATA: gv_last_late_number TYPE /dmo/travel_id VALUE /dmo/if_flight_legacy=>late_numbering_boundary.

    " Main buffer
    DATA: mt_create_buffer TYPE /dmo/t_travel,
          mt_update_buffer TYPE /dmo/t_travel,
          mt_delete_buffer TYPE /dmo/t_travel_key.
    " Temporary buffer valid during create / update / delete Travel
    DATA: mt_create_buffer_2 TYPE /dmo/t_travel,
          mt_update_buffer_2 TYPE /dmo/t_travel,
          mt_delete_buffer_2 TYPE /dmo/t_travel_key.

    DATA mt_agency_id   TYPE SORTED TABLE OF /dmo/agency_id   WITH UNIQUE KEY table_line.

    METHODS _create IMPORTING it_travel         TYPE /dmo/t_travel
                              iv_numbering_mode TYPE /dmo/if_flight_legacy=>t_numbering_mode
                    EXPORTING et_travel         TYPE /dmo/t_travel
                              et_messages       TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS _update IMPORTING it_travel   TYPE /dmo/t_travel
                              it_travelx  TYPE /dmo/t_travelx
                    EXPORTING et_travel   TYPE /dmo/t_travel
                              et_messages TYPE /dmo/if_flight_legacy=>tt_if_t100_message.
    METHODS _delete IMPORTING it_travel          TYPE /dmo/t_travel
                              iv_no_delete_check TYPE abap_bool
                    EXPORTING et_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message.

    METHODS _check IMPORTING is_travel          TYPE /dmo/travel
                             is_travelx         TYPE /dmo/s_travelx OPTIONAL
                             iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
                   CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                   RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    METHODS _check_agency IMPORTING is_travel          TYPE /dmo/travel
                                    is_travelx         TYPE /dmo/s_travelx OPTIONAL
                                    iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
                          CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                          RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    METHODS _check_customer IMPORTING is_travel          TYPE /dmo/travel
                                      is_travelx         TYPE /dmo/s_travelx OPTIONAL
                                      iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
                            CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                            RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    METHODS _check_dates IMPORTING is_travel          TYPE /dmo/travel
                                   is_travelx         TYPE /dmo/s_travelx OPTIONAL
                                   iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
                         CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                         RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    METHODS _check_status IMPORTING is_travel          TYPE /dmo/travel
                                    is_travelx         TYPE /dmo/s_travelx OPTIONAL
                                    iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
                          CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                          RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    METHODS _check_currency_code IMPORTING is_travel          TYPE /dmo/travel
                                           is_travelx         TYPE /dmo/s_travelx OPTIONAL
                                           iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
                                 CHANGING  ct_messages        TYPE /dmo/if_flight_legacy=>tt_if_t100_message
                                 RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    METHODS _update_admin IMPORTING iv_new TYPE abap_bool CHANGING cs_travel_admin TYPE /dmo/travel_admin.
ENDCLASS.


CLASS lcl_travel_buffer IMPLEMENTATION.
  METHOD get_instance.
    go_instance = COND #( WHEN go_instance IS BOUND THEN go_instance ELSE NEW #( ) ).
    ro_instance = go_instance.
  ENDMETHOD.


  METHOD _create.
    CLEAR et_travel.
    CLEAR et_messages.

    CHECK it_travel IS NOT INITIAL.

    DATA lt_travel_to_process LIKE it_travel.
    DATA lv_travel_id_max     TYPE /dmo/travel_id.

    lt_travel_to_process = it_travel.

    " Checks
    LOOP AT it_travel INTO DATA(ls_travel_create) ##INTO_OK.
      IF _check( EXPORTING is_travel     = ls_travel_create
                           iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create
                 CHANGING  ct_messages   = et_messages ) = abap_false.
        DELETE lt_travel_to_process WHERE travel_id = ls_travel_create-travel_id.
      ENDIF.
    ENDLOOP.

    CHECK lt_travel_to_process IS NOT INITIAL.

    " Get Numbers
    IF iv_numbering_mode EQ /dmo/if_flight_legacy=>numbering_mode-late.

      LOOP AT lt_travel_to_process INTO ls_travel_create ##INTO_OK.
        " standard determinations
        ls_travel_create-createdby = sy-uname.
        GET TIME STAMP FIELD ls_travel_create-createdat.
        ls_travel_create-lastchangedby = ls_travel_create-createdby.
        ls_travel_create-lastchangedat = ls_travel_create-createdat.
        ls_travel_create-status = /dmo/if_flight_legacy=>travel_status-new.

        " Late Numbering - Assign a temporary Travel ID (starting with '9...').
        gv_last_late_number = gv_last_late_number + 1.
        ASSERT gv_last_late_number IS NOT INITIAL.
        ls_travel_create-travel_id = gv_last_late_number.

        INSERT ls_travel_create INTO TABLE mt_create_buffer_2.
      ENDLOOP.

    ELSE.
      TRY.
          cl_numberrange_runtime=>number_get(
            EXPORTING
              nr_range_nr       = '01'
              object            = '/DMO/TRAVL'
              quantity          = CONV #( lines( lt_travel_to_process ) )
            IMPORTING
              number            = DATA(lv_key)
              returncode        = DATA(lv_return_code)
              returned_quantity = DATA(lv_returned_quantity)
          ).
        CATCH cx_number_ranges INTO DATA(lx_number_ranges).
          APPEND lx_number_ranges TO et_messages.
          EXIT.
      ENDTRY.


      IF lv_return_code = '2' OR lv_return_code = '3'.
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_nr_last_number ) TO et_messages.
        EXIT.
      ENDIF.

      ASSERT lv_returned_quantity = lines( lt_travel_to_process ).

      lv_travel_id_max = CONV i( lv_key ) - lines( lt_travel_to_process ).

      LOOP AT lt_travel_to_process INTO ls_travel_create ##INTO_OK.

        " standard determinations
        ls_travel_create-createdby = sy-uname.
        GET TIME STAMP FIELD ls_travel_create-createdat.
        ls_travel_create-lastchangedby = ls_travel_create-createdby.
        ls_travel_create-lastchangedat = ls_travel_create-createdat.
        ls_travel_create-status = /dmo/if_flight_legacy=>travel_status-new.



        " **Internal** numbering: Override travel_id
        lv_travel_id_max = lv_travel_id_max + 1.
        ASSERT lv_travel_id_max IS NOT INITIAL.
        ls_travel_create-travel_id = lv_travel_id_max.

        INSERT ls_travel_create INTO TABLE mt_create_buffer_2.
      ENDLOOP.
    ENDIF.
    et_travel = mt_create_buffer_2.
  ENDMETHOD.


  METHOD _update.
    DATA lv_new TYPE abap_bool.

    CLEAR et_travel.
    CLEAR et_messages.

    CHECK it_travel IS NOT INITIAL.

    " Check for empty keys
    READ TABLE it_travel TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = '0'.
    IF sy-subrc = 0.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_key ) TO et_messages.
      RETURN.
    ENDIF.

    DATA lt_travel TYPE SORTED TABLE OF /dmo/travel WITH UNIQUE KEY travel_id.
    SELECT * FROM /dmo/travel FOR ALL ENTRIES IN @it_travel WHERE travel_id = @it_travel-travel_id INTO TABLE @lt_travel ##SELECT_FAE_WITH_LOB[DESCRIPTION].

    FIELD-SYMBOLS <s_buffer_travel> TYPE /dmo/travel.
    DATA ls_buffer_travel TYPE /dmo/travel.
    LOOP AT it_travel ASSIGNING FIELD-SYMBOL(<s_travel_update>).
      UNASSIGN <s_buffer_travel>.

      READ TABLE mt_delete_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = <s_travel_update>-travel_id.
      IF sy-subrc = 0." Error: Record to be updated marked for deletion
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = <s_travel_update>-travel_id ) TO et_messages.
        RETURN.
      ENDIF.

      IF <s_buffer_travel> IS NOT ASSIGNED." Special case: record already in temporary create buffer
        READ TABLE mt_create_buffer_2 ASSIGNING <s_buffer_travel> WITH TABLE KEY travel_id = <s_travel_update>-travel_id.
        IF sy-subrc = 0.
          lv_new = abap_true.
        ENDIF.
      ENDIF.

      IF <s_buffer_travel> IS NOT ASSIGNED." Special case: record already in create buffer
        lv_new = abap_false.
        READ TABLE mt_create_buffer INTO ls_buffer_travel WITH TABLE KEY travel_id = <s_travel_update>-travel_id.
        IF sy-subrc = 0.
          INSERT ls_buffer_travel INTO TABLE mt_create_buffer_2 ASSIGNING <s_buffer_travel>.
          lv_new = abap_true.
        ENDIF.
      ENDIF.

      IF <s_buffer_travel> IS NOT ASSIGNED." Special case: record already in temporary update buffer
        READ TABLE mt_update_buffer_2 ASSIGNING <s_buffer_travel> WITH TABLE KEY travel_id = <s_travel_update>-travel_id.
      ENDIF.

      IF <s_buffer_travel> IS NOT ASSIGNED." Special case: record already in update buffer
        READ TABLE mt_update_buffer INTO ls_buffer_travel WITH TABLE KEY travel_id = <s_travel_update>-travel_id.
        IF sy-subrc = 0.
          INSERT ls_buffer_travel INTO TABLE mt_update_buffer_2 ASSIGNING <s_buffer_travel>.
        ENDIF.
      ENDIF.

      IF <s_buffer_travel> IS NOT ASSIGNED." Usual case: record not already in update buffer
        READ TABLE lt_travel ASSIGNING FIELD-SYMBOL(<s_travel_old>) WITH TABLE KEY travel_id = <s_travel_update>-travel_id.
        IF sy-subrc = 0.
          INSERT <s_travel_old> INTO TABLE mt_update_buffer_2 ASSIGNING <s_buffer_travel>.
          ASSERT sy-subrc = 0.
        ENDIF.
      ENDIF.

      " Error
      IF <s_buffer_travel> IS NOT ASSIGNED.
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = <s_travel_update>-travel_id ) TO et_messages.
        RETURN.
      ENDIF.

      " Merge fields to be updated
      READ TABLE it_travelx ASSIGNING FIELD-SYMBOL(<s_travelx>) WITH KEY travel_id = <s_travel_update>-travel_id  action_code = /dmo/if_flight_legacy=>action_code-update.
      IF sy-subrc <> 0.
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_control  travel_id = <s_travel_update>-travel_id ) TO et_messages.
        RETURN.
      ENDIF.
      DATA lv_field TYPE i.
      lv_field = 3.
      DO.
        ASSIGN COMPONENT lv_field OF STRUCTURE <s_travelx> TO FIELD-SYMBOL(<v_flag>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        IF <v_flag> = abap_true.
          ASSIGN COMPONENT lv_field OF STRUCTURE <s_travel_update> TO FIELD-SYMBOL(<v_field_new>).
          ASSERT sy-subrc = 0.
          ASSIGN COMPONENT lv_field OF STRUCTURE <s_buffer_travel> TO FIELD-SYMBOL(<v_field_old>).
          ASSERT sy-subrc = 0.
          <v_field_old> = <v_field_new>.
        ENDIF.
        lv_field = lv_field + 1.
      ENDDO.

      " Checks
      IF _check( EXPORTING is_travel      = <s_buffer_travel>
                           is_travelx     = <s_travelx>
                           iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update
                 CHANGING  ct_messages    = et_messages ) = abap_false.
        RETURN.
      ENDIF.

      " Set administrative fields
      _update_admin( EXPORTING iv_new = lv_new CHANGING cs_travel_admin = <s_buffer_travel>-gr_admin ).

      " standard determinations

      INSERT <s_buffer_travel> INTO TABLE et_travel.
    ENDLOOP.
  ENDMETHOD.


  METHOD _delete.
    CLEAR et_messages.

    CHECK it_travel IS NOT INITIAL.

    " Check for empty keys
    READ TABLE it_travel TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = '0'.
    IF sy-subrc = 0.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_key ) TO et_messages.
      RETURN.
    ENDIF.

    DATA(lt_travel) = it_travel.

    " Special case: record already in create buffer
    LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<s_travel_delete>).
      READ TABLE mt_create_buffer TRANSPORTING NO FIELDS WITH KEY travel_id = <s_travel_delete>-travel_id.
      IF sy-subrc = 0.
        INSERT VALUE #( travel_id = <s_travel_delete>-travel_id ) INTO TABLE mt_delete_buffer_2.
        DELETE lt_travel.
      ENDIF.
    ENDLOOP.

    IF iv_no_delete_check = abap_false.
      DATA lt_travel_db TYPE SORTED TABLE OF /dmo/travel_id WITH UNIQUE KEY table_line.
      SELECT travel_id FROM /dmo/travel FOR ALL ENTRIES IN @lt_travel WHERE travel_id = @lt_travel-travel_id INTO TABLE @lt_travel_db.
    ENDIF.

    " Check existence and append to delete buffer
    LOOP AT lt_travel ASSIGNING <s_travel_delete>.
      IF iv_no_delete_check = abap_false.
        READ TABLE lt_travel_db ASSIGNING FIELD-SYMBOL(<s_travel_old>) WITH TABLE KEY table_line = <s_travel_delete>-travel_id.
        IF sy-subrc <> 0.
          APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = <s_travel_delete>-travel_id ) TO et_messages.
          RETURN.
        ENDIF.
      ENDIF.
      INSERT VALUE #( travel_id = <s_travel_delete>-travel_id ) INTO TABLE mt_delete_buffer_2.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_status_to_booked.
    DATA lv_new TYPE abap_bool.

    CLEAR et_messages.

    " Check for empty travel ID
    IF iv_travel_id IS INITIAL.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_key ) TO et_messages.
      RETURN.
    ENDIF.

    READ TABLE mt_delete_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = iv_travel_id.
    IF sy-subrc = 0." Error: Record of action marked for deletion
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = iv_travel_id ) TO et_messages.
      RETURN.
    ENDIF.

    " Special case: Record in CREATE buffer
    lv_new = abap_false.
    READ TABLE mt_create_buffer ASSIGNING FIELD-SYMBOL(<s_travel>) WITH TABLE KEY travel_id = iv_travel_id.
    IF sy-subrc = 0.
      lv_new = abap_true.
    ENDIF.

    " Special case: Record in UPDATE buffer
    IF <s_travel> IS NOT ASSIGNED.
      READ TABLE mt_update_buffer ASSIGNING <s_travel> WITH TABLE KEY travel_id = iv_travel_id.
    ENDIF.

    " Usual case: Read record from DB and put it into the UPDATE buffer
    IF <s_travel> IS NOT ASSIGNED.
      SELECT SINGLE * FROM /dmo/travel WHERE travel_id = @iv_travel_id INTO @DATA(ls_travel) .
      IF sy-subrc = 0.
        INSERT ls_travel INTO TABLE mt_update_buffer ASSIGNING <s_travel>.
      ENDIF.
    ENDIF.

    " Error
    IF <s_travel> IS NOT ASSIGNED.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = iv_travel_id ) TO et_messages.
      RETURN.
    ENDIF.

    <s_travel>-status = /dmo/if_flight_legacy=>travel_status-booked.
    _update_admin( EXPORTING iv_new = lv_new CHANGING cs_travel_admin = <s_travel>-gr_admin ).
  ENDMETHOD.


  METHOD adjust_numbers.
    DATA: lv_is_early_numbering_used TYPE abap_bool VALUE abap_false,
          lv_is_late_numbering_used  TYPE abap_bool VALUE abap_false,
          lt_create_buffer           TYPE /dmo/t_travel,
          lv_travel_id               TYPE /dmo/travel_id.


    ASSERT mt_create_buffer_2 IS INITIAL.
    ASSERT mt_update_buffer_2 IS INITIAL.
    ASSERT mt_delete_buffer_2 IS INITIAL.

    CHECK mt_create_buffer IS NOT INITIAL.  "Any Create to be processed?

    " As the table is sorted by the key travel_id the first entry will be the lowest travel_id and the last entry the highest travel_id
    IF mt_create_buffer[ 1 ]-travel_id > /dmo/if_flight_legacy=>late_numbering_boundary.
      lv_is_late_numbering_used = abap_true.
    ENDIF.
    IF mt_create_buffer[ lines( mt_create_buffer ) ]-travel_id < /dmo/if_flight_legacy=>late_numbering_boundary.
      lv_is_early_numbering_used = abap_true.
    ENDIF.

    " Mixed Numbering (late+early) is not supported
    ASSERT lv_is_late_numbering_used NE lv_is_early_numbering_used.

    " Are we in Late Numbering mode? -> Process Late Numbering
    IF lv_is_late_numbering_used EQ abap_true.
      TRY.
          cl_numberrange_runtime=>number_get(
            EXPORTING
              nr_range_nr       = '01'
              object            = '/DMO/TRAVL'
              quantity          = CONV #( lines( mt_create_buffer ) )
            IMPORTING
              number            = DATA(lv_key)
              returncode        = DATA(lv_return_code)
              returned_quantity = DATA(lv_returned_quantity)
          ).
        CATCH cx_number_ranges INTO DATA(lx_number_ranges).
          ASSERT 1 = 2.
      ENDTRY.
      ASSERT lv_returned_quantity = lines( mt_create_buffer ).

      lv_travel_id = CONV i( lv_key ) - lines( mt_create_buffer ).

      LOOP AT mt_create_buffer INTO DATA(ls_create_buffer).
        lv_travel_id = lv_travel_id + 1.
        ASSERT lv_travel_id IS NOT INITIAL.

        APPEND VALUE #( preliminary = VALUE #( travel_id = ls_create_buffer-travel_id )
                        final       = VALUE #( travel_id = lv_travel_id ) ) TO rt_travel_mapping.

        ls_create_buffer-travel_id = lv_travel_id.
        INSERT ls_create_buffer INTO TABLE lt_create_buffer.
      ENDLOOP.
      mt_create_buffer = lt_create_buffer.
    ENDIF.
  ENDMETHOD.

  METHOD save.
    ASSERT mt_create_buffer_2 IS INITIAL.
    ASSERT mt_update_buffer_2 IS INITIAL.
    ASSERT mt_delete_buffer_2 IS INITIAL.

    " Make sure no preliminary travel_id is remaining (late numbering use-case)
    " If this dump occurs, call adjust numbers before executing save
    IF mt_create_buffer IS NOT INITIAL.
      ASSERT mt_create_buffer[ lines( mt_create_buffer ) ]-travel_id < /dmo/if_flight_legacy=>late_numbering_boundary.
    ENDIF.

    INSERT /dmo/travel FROM TABLE @mt_create_buffer.
    UPDATE /dmo/travel FROM TABLE @mt_update_buffer.
    DELETE /dmo/travel FROM TABLE @( CORRESPONDING #( mt_delete_buffer ) ).
  ENDMETHOD.


  METHOD initialize.
    CLEAR: mt_create_buffer, mt_update_buffer, mt_delete_buffer.
  ENDMETHOD.


  METHOD check_travel_id.
    rv_is_valid = abap_false.

    IF iv_travel_id IS INITIAL.
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_key ) TO ct_messages.
      RETURN.
    ENDIF.

    IF line_exists( mt_delete_buffer[ travel_id = iv_travel_id ] ).
      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = iv_travel_id ) TO ct_messages.
      RETURN.
    ENDIF.

    IF line_exists( mt_create_buffer[ travel_id = iv_travel_id ] ).
      rv_is_valid = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE FROM /dmo/travel FIELDS @abap_true WHERE travel_id = @iv_travel_id INTO @DATA(lv_db_exists).
    IF lv_db_exists = abap_true.
      rv_is_valid = abap_true.
      RETURN.
    ENDIF.

    APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = iv_travel_id ) TO ct_messages.
  ENDMETHOD.


  METHOD cud_prep.
    CLEAR et_travel.
    CLEAR et_messages.

    CHECK it_travel IS NOT INITIAL.

    DATA lt_travel_c  TYPE /dmo/t_travel.
    DATA lt_travel_u  TYPE /dmo/t_travel.
    DATA lt_travel_d  TYPE /dmo/t_travel.
    DATA lt_travelx_u TYPE /dmo/t_travelx.
    LOOP AT it_travel ASSIGNING FIELD-SYMBOL(<s_travel>).
      READ TABLE it_travelx ASSIGNING FIELD-SYMBOL(<s_travelx>) WITH TABLE KEY travel_id = <s_travel>-travel_id.
      IF sy-subrc <> 0.
        APPEND NEW /dmo/cx_flight_legacy( textid     = /dmo/cx_flight_legacy=>travel_no_control
                                          travel_id  = <s_travel>-travel_id ) TO et_messages.
        RETURN.
      ENDIF.
      CASE CONV /dmo/if_flight_legacy=>action_code_enum( <s_travelx>-action_code ).
        WHEN /dmo/if_flight_legacy=>action_code-create.
          INSERT <s_travel>  INTO TABLE lt_travel_c.
        WHEN /dmo/if_flight_legacy=>action_code-update.
          INSERT <s_travel>  INTO TABLE lt_travel_u.
          INSERT <s_travelx> INTO TABLE lt_travelx_u.
        WHEN /dmo/if_flight_legacy=>action_code-delete.
          INSERT <s_travel>  INTO TABLE lt_travel_d.
      ENDCASE.
    ENDLOOP.

    _create( EXPORTING it_travel         = lt_travel_c
                       iv_numbering_mode = iv_numbering_mode
             IMPORTING et_travel         = et_travel
                       et_messages       = et_messages ).

    _update( EXPORTING it_travel   = lt_travel_u
                       it_travelx  = lt_travelx_u
             IMPORTING et_travel   = DATA(lt_travel)
                       et_messages = DATA(lt_messages) ).
    INSERT LINES OF lt_travel INTO TABLE et_travel.
    APPEND LINES OF lt_messages TO et_messages.

    _delete( EXPORTING it_travel          = lt_travel_d
                       iv_no_delete_check = iv_no_delete_check
             IMPORTING et_messages        = lt_messages ).
    APPEND LINES OF lt_messages TO et_messages.
  ENDMETHOD.


  METHOD cud_copy.
    LOOP AT mt_create_buffer_2 ASSIGNING FIELD-SYMBOL(<s_create_buffer_2>).
      READ TABLE mt_create_buffer ASSIGNING FIELD-SYMBOL(<s_create_buffer>) WITH TABLE KEY travel_id = <s_create_buffer_2>-travel_id.
      IF sy-subrc <> 0.
        INSERT VALUE #( travel_id = <s_create_buffer_2>-travel_id ) INTO TABLE mt_create_buffer ASSIGNING <s_create_buffer>.
      ENDIF.
      <s_create_buffer>-gr_data  = <s_create_buffer_2>-gr_data.
      <s_create_buffer>-gr_admin = <s_create_buffer_2>-gr_admin.
    ENDLOOP.
    LOOP AT mt_update_buffer_2 ASSIGNING FIELD-SYMBOL(<s_update_buffer_2>).
      READ TABLE mt_update_buffer ASSIGNING FIELD-SYMBOL(<s_update_buffer>) WITH TABLE KEY travel_id = <s_update_buffer_2>-travel_id.
      IF sy-subrc <> 0.
        INSERT VALUE #( travel_id = <s_update_buffer_2>-travel_id ) INTO TABLE mt_update_buffer ASSIGNING <s_update_buffer>.
      ENDIF.
      <s_update_buffer>-gr_data  = <s_update_buffer_2>-gr_data.
      <s_update_buffer>-gr_admin = <s_update_buffer_2>-gr_admin.
    ENDLOOP.
    LOOP AT mt_delete_buffer_2 ASSIGNING FIELD-SYMBOL(<s_delete_buffer_2>).
      DELETE mt_create_buffer WHERE travel_id = <s_delete_buffer_2>-travel_id.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      DELETE mt_update_buffer WHERE travel_id = <s_delete_buffer_2>-travel_id.
      INSERT <s_delete_buffer_2> INTO TABLE mt_delete_buffer.
    ENDLOOP.
    CLEAR: mt_create_buffer_2, mt_update_buffer_2, mt_delete_buffer_2.
  ENDMETHOD.


  METHOD cud_disc.
    CLEAR: mt_create_buffer_2, mt_update_buffer_2, mt_delete_buffer_2.
  ENDMETHOD.


  METHOD get.
    CLEAR et_travel.

    CHECK it_travel IS NOT INITIAL.

    SELECT * FROM /dmo/travel FOR ALL ENTRIES IN @it_travel WHERE travel_id = @it_travel-travel_id
      INTO TABLE @et_travel ##SELECT_FAE_WITH_LOB[DESCRIPTION]. "#EC CI_ALL_FIELDS_NEEDED "#EC CI_SEL_DEL

    IF iv_include_buffer = abap_true.
      LOOP AT it_travel ASSIGNING FIELD-SYMBOL(<s_travel>).
        READ TABLE mt_create_buffer ASSIGNING FIELD-SYMBOL(<s_create_buffer>) WITH TABLE KEY travel_id = <s_travel>-travel_id.
        IF sy-subrc = 0.
          INSERT <s_create_buffer> INTO TABLE et_travel.
        ENDIF.

        READ TABLE mt_update_buffer ASSIGNING FIELD-SYMBOL(<s_update_buffer>) WITH TABLE KEY travel_id = <s_travel>-travel_id.
        IF sy-subrc = 0.
          MODIFY TABLE et_travel FROM <s_update_buffer>.
        ENDIF.

        READ TABLE mt_delete_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = <s_travel>-travel_id.
        IF sy-subrc = 0.
          DELETE et_travel WHERE travel_id = <s_travel>-travel_id. "#EC CI_SEL_DEL
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF iv_include_temp_buffer = abap_true.
      LOOP AT it_travel ASSIGNING <s_travel>.
        READ TABLE mt_create_buffer_2 ASSIGNING <s_create_buffer> WITH TABLE KEY travel_id = <s_travel>-travel_id.
        IF sy-subrc = 0.
          DELETE et_travel WHERE travel_id = <s_travel>-travel_id. "#EC CI_SEL_DEL
          INSERT <s_create_buffer> INTO TABLE et_travel.
        ENDIF.

        READ TABLE mt_update_buffer_2 ASSIGNING <s_update_buffer> WITH TABLE KEY travel_id = <s_travel>-travel_id.
        IF sy-subrc = 0.
          MODIFY TABLE et_travel FROM <s_update_buffer>.
        ENDIF.

        READ TABLE mt_delete_buffer_2 TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = <s_travel>-travel_id.
        IF sy-subrc = 0.
          DELETE et_travel WHERE travel_id = <s_travel>-travel_id. "#EC CI_SEL_DEL
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD _check.
    rv_is_valid = abap_true.

    IF NOT _check_agency( EXPORTING is_travel        = is_travel
                                    is_travelx       = is_travelx
                                    iv_change_mode   = iv_change_mode
                          CHANGING  ct_messages      = ct_messages ).
      rv_is_valid = abap_false.
    ENDIF.

    IF NOT _check_customer( EXPORTING is_travel      = is_travel
                                      is_travelx     = is_travelx
                                      iv_change_mode = iv_change_mode
                            CHANGING  ct_messages    = ct_messages ).
      rv_is_valid = abap_false.
    ENDIF.

    IF NOT _check_dates( EXPORTING is_travel      = is_travel
                                   is_travelx     = is_travelx
                                   iv_change_mode = iv_change_mode
                         CHANGING  ct_messages    = ct_messages ).
      rv_is_valid = abap_false.
    ENDIF.

    IF NOT _check_status( EXPORTING is_travel      = is_travel
                                    is_travelx     = is_travelx
                                    iv_change_mode = iv_change_mode
                          CHANGING  ct_messages    = ct_messages ).
      rv_is_valid = abap_false.
    ENDIF.

    IF NOT _check_currency_code( EXPORTING is_travel      = is_travel
                                           is_travelx     = is_travelx
                                           iv_change_mode = iv_change_mode
                                 CHANGING  ct_messages    = ct_messages ).
      rv_is_valid = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD _check_agency.
    rv_is_valid = abap_true.
    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_travelx-agency_id = abap_true ).
      IF mt_agency_id IS INITIAL.
        SELECT DISTINCT agency_id FROM /dmo/agency INTO TABLE @mt_agency_id. "#EC CI_NOWHERE
      ENDIF.
      READ TABLE mt_agency_id TRANSPORTING NO FIELDS WITH TABLE KEY table_line = is_travel-agency_id.
      IF sy-subrc <> 0.
        rv_is_valid = abap_false.
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>agency_unkown  agency_id = is_travel-agency_id ) TO ct_messages.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD _check_customer.
    rv_is_valid = abap_true.
    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_travelx-customer_id = abap_true ).
      rv_is_valid = lcl_common_checks=>is_customer_id_valid( EXPORTING iv_customer_id = is_travel-customer_id CHANGING ct_messages = ct_messages ).
    ENDIF.
  ENDMETHOD.


  METHOD _check_dates.
    rv_is_valid = abap_true.

    " begin date
    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_travelx-begin_date = abap_true ).
      IF is_travel-begin_date IS INITIAL OR is_travel-begin_date = ''.
        rv_is_valid = abap_false.
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>no_begin_date  travel_id = is_travel-travel_id ) TO ct_messages.
      ENDIF.
    ENDIF.

    " end date
    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_travelx-end_date = abap_true ).
      IF is_travel-end_date IS INITIAL OR is_travel-end_date = ''.
        rv_is_valid = abap_false.
        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>no_end_date  travel_id = is_travel-travel_id ) TO ct_messages.
      ENDIF.
    ENDIF.

    " begin date < = end date
    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_travelx-begin_date = abap_true )
                                                                  OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_travelx-end_date   = abap_true ).
      IF is_travel-begin_date IS NOT INITIAL AND is_travel-end_date IS NOT INITIAL.
        IF is_travel-begin_date > is_travel-end_date.
          rv_is_valid = abap_false.
          APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>end_date_before_begin_date  begin_date = is_travel-begin_date  end_date = is_travel-end_date  travel_id = is_travel-travel_id ) TO ct_messages.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD _check_status.
    rv_is_valid = abap_true.
    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_travelx-status = abap_true.
      CASE is_travel-status.
        WHEN CONV /dmo/travel_status( /dmo/if_flight_legacy=>travel_status-booked ).   " OK
        WHEN CONV /dmo/travel_status( /dmo/if_flight_legacy=>travel_status-cancelled )." OK
        WHEN CONV /dmo/travel_status( /dmo/if_flight_legacy=>travel_status-new ).      " OK
        WHEN CONV /dmo/travel_status( /dmo/if_flight_legacy=>travel_status-planned ).  " OK
        WHEN OTHERS.
          rv_is_valid = abap_false.
          APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_status_invalid  status = is_travel-status ) TO ct_messages.
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD _check_currency_code.
    rv_is_valid = abap_true.
    IF is_travel-total_price IS INITIAL AND is_travel-booking_fee IS INITIAL AND is_travel-currency_code IS INITIAL.
      " When no prices have been entered yet, the currency code may be initial
      RETURN.
    ENDIF.
    IF   iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create
      OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_travelx-currency_code = abap_true ).
      rv_is_valid = lcl_common_checks=>is_currency_code_valid( EXPORTING iv_currency_code = is_travel-currency_code CHANGING ct_messages = ct_messages ).
    ENDIF.
  ENDMETHOD.


  METHOD _update_admin.
    cs_travel_admin-lastchangedby = sy-uname.
    GET TIME STAMP FIELD cs_travel_admin-lastchangedat.
    IF iv_new = abap_true.
      " For a BO to be created we want created* and lastchanged* to be the same
      cs_travel_admin-createdby = cs_travel_admin-lastchangedby.
      cs_travel_admin-createdat = cs_travel_admin-lastchangedat.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
