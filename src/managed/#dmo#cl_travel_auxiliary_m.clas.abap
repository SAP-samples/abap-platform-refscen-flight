CLASS /dmo/cl_travel_auxiliary_m DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.

*   Type definition for import parameters --------------------------
     TYPES tt_travel_id                 TYPE TABLE OF /dmo/travel_id.
    TYPES tt_travel_reported            TYPE TABLE FOR REPORTED /dmo/i_travel_m.
    TYPES tt_booking_reported           TYPE TABLE FOR REPORTED  /dmo/i_booking_m.
    TYPES tt_bookingsupplement_reported TYPE TABLE FOR REPORTED  /dmo/i_booksuppl_m.

*   Method for price calculation (used in determination calls) --------
    CLASS-METHODS calculate_price
                            IMPORTING it_travel_id          TYPE tt_travel_id.

PROTECTED SECTION.

PRIVATE SECTION.

    CLASS-METHODS new_message
                            IMPORTING id                    TYPE symsgid
                                      number                TYPE symsgno
                                      severity              TYPE if_abap_behv_message=>t_severity
                                      v1                    TYPE simple OPTIONAL
                                      v2                    TYPE simple OPTIONAL
                                      v3                    TYPE simple OPTIONAL
                                      v4                    TYPE simple OPTIONAL
                              RETURNING VALUE(obj)          TYPE REF TO if_abap_behv_message .

ENDCLASS.


CLASS /dmo/cl_travel_auxiliary_m IMPLEMENTATION.

  METHOD calculate_price.

    IF it_travel_id IS INITIAL.
      RETURN.
    ENDIF.

*   Read travel instance data
    READ ENTITIES OF /dmo/i_travel_m
         ENTITY travel
               FROM VALUE #( FOR lv_travel_id IN it_travel_id (
                                travel_id = lv_travel_id
                                %control-currency_code = if_abap_behv=>mk-on ) )
         RESULT   DATA(lt_read_travel).


    READ ENTITIES OF /dmo/i_travel_m
         ENTITY travel BY \_booking
               FROM VALUE #( FOR lv_travel_id IN it_travel_id (
                        travel_id = lv_travel_id
                        %control-flight_price  = if_abap_behv=>mk-on
                        %control-booking_id    = if_abap_behv=>mk-on
                        %control-currency_code = if_abap_behv=>mk-on ) )
         RESULT   DATA(lt_read_booking_by_travel).

    LOOP AT lt_read_booking_by_travel INTO DATA(ls_booking)
      GROUP BY ls_booking-travel_id INTO DATA(ls_travel_key).

      ASSIGN lt_read_travel[ KEY entity COMPONENTS travel_id = ls_travel_key ]
            TO FIELD-SYMBOL(<ls_travel>).
      CLEAR <ls_travel>-total_price.

      LOOP AT GROUP ls_travel_key INTO DATA(ls_booking_result)
        GROUP BY ls_booking_result-currency_code INTO DATA(lv_curr).

        DATA(total_book_price_by_trav_curr) = VALUE /dmo/total_price(  ).

        LOOP AT GROUP lv_curr INTO DATA(ls_booking_line).

          total_book_price_by_trav_curr   += ls_booking_line-flight_price.

        ENDLOOP.
        IF lv_curr  = <ls_travel>-currency_code.
          <ls_travel>-total_price += total_book_price_by_trav_curr.
        ELSE.
          /dmo/cl_flight_amdp=>convert_currency(
             EXPORTING
               iv_amount                   =  total_book_price_by_trav_curr
               iv_currency_code_source     =  lv_curr
               iv_currency_code_target     =  <ls_travel>-currency_code
               iv_exchange_rate_date       =  cl_abap_context_info=>get_system_date( )
             IMPORTING
               ev_amount                   = DATA(total_book_price_per_curr)
            ).
          <ls_travel>-total_price += total_book_price_per_curr.
        ENDIF.

      ENDLOOP.

    ENDLOOP.


    READ ENTITIES OF /dmo/i_travel_m
             ENTITY booking BY \_BookSupplement
                   FROM VALUE #( FOR ls_travel IN lt_read_booking_by_travel (
                          travel_id              = ls_travel-travel_id
                          booking_id             = ls_travel-booking_id
                          %control-price         = if_abap_behv=>mk-on
                          %control-currency_code = if_abap_behv=>mk-on ) )
          RESULT   DATA(lt_read_booksuppl).

    LOOP AT lt_read_booksuppl INTO DATA(ls_booking_suppl)
      GROUP BY ls_booking_suppl-travel_id INTO ls_travel_key.

      ASSIGN lt_read_travel[ KEY entity COMPONENTS travel_id = ls_travel_key ] TO <ls_travel>.

      LOOP AT GROUP ls_travel_key INTO DATA(ls_bookingsuppl_result)
        GROUP BY ls_bookingsuppl_result-currency_code INTO lv_curr.

        DATA(total_suppl_price_by_trav_curr) = VALUE /dmo/total_price(  ).

        LOOP AT GROUP lv_curr INTO DATA(ls_booking_suppl2).
          total_suppl_price_by_trav_curr    += ls_booking_suppl2-price.
        ENDLOOP.

        IF lv_curr  = <ls_travel>-currency_code.
          <ls_travel>-total_price    += total_suppl_price_by_trav_curr.
        ELSE.
          /dmo/cl_flight_amdp=>convert_currency(
             EXPORTING
               iv_amount                     =  total_suppl_price_by_trav_curr
               iv_currency_code_source       =  lv_curr
               iv_currency_code_target       =  <ls_travel>-currency_code
               iv_exchange_rate_date         =  cl_abap_context_info=>get_system_date( )
             IMPORTING
               ev_amount                     = DATA(total_suppl_price_per_curr)
            ).
          <ls_travel>-total_price     += total_suppl_price_per_curr.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    MODIFY ENTITIES OF /dmo/i_travel_m
          ENTITY travel
                 UPDATE FROM VALUE #( FOR travel IN lt_read_travel (
                   travel_id               = travel-travel_id
                   total_price             = travel-total_price
                   currency_code           = travel-currency_code
                   %control-total_price    = if_abap_behv=>mk-on ) ) .

  ENDMETHOD.


  METHOD new_message.
    obj = NEW lcl_abap_behv_msg(
      textid = VALUE #(
                 msgid = id
                 msgno = number
                 attr1 = COND #( WHEN v1 IS NOT INITIAL THEN 'IF_T100_DYN_MSG~MSGV1' )
                 attr2 = COND #( WHEN v2 IS NOT INITIAL THEN 'IF_T100_DYN_MSG~MSGV2' )
                 attr3 = COND #( WHEN v3 IS NOT INITIAL THEN 'IF_T100_DYN_MSG~MSGV3' )
                 attr4 = COND #( WHEN v4 IS NOT INITIAL THEN 'IF_T100_DYN_MSG~MSGV4' )
      )
      msgty = SWITCH #( severity
                WHEN if_abap_behv_message=>severity-error       THEN 'E'
                WHEN if_abap_behv_message=>severity-warning     THEN 'W'
                WHEN if_abap_behv_message=>severity-information THEN 'I'
                WHEN if_abap_behv_message=>severity-success     THEN 'S' )
      msgv1 = |{ v1 }|
      msgv2 = |{ v2 }|
      msgv3 = |{ v3 }|
      msgv4 = |{ v4 }|
    ).
    obj->m_severity = severity.
  ENDMETHOD.


ENDCLASS.
