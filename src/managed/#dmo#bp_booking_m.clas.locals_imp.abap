CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS calculate_total_flight_price FOR DETERMINATION booking~calculateTotalFlightPrice IMPORTING keys FOR booking.
    METHODS validate_booking_status      FOR VALIDATION booking~validateStatus   IMPORTING keys FOR booking.
    METHODS get_features                 FOR FEATURES IMPORTING keys REQUEST requested_features FOR booking RESULT result.
    METHODS create_booking_supplement    FOR MODIFY IMPORTING keys FOR ACTION booking~createBookingSupplement RESULT result.

*    METHODS check_authority_for_booking  FOR AUTHORIZATION IMPORTING it_booking_key REQUEST is_request FOR booking RESULT result.


ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

********************************************************************************
*
* Calculates total flight price
*
********************************************************************************
  METHOD calculate_total_flight_price.

    IF keys IS NOT INITIAL.
      /dmo/cl_travel_auxiliary_m=>calculate_price(
          it_travel_id = VALUE #(  FOR GROUPS <booking> OF booking_key IN keys
                                       GROUP BY booking_key-travel_id WITHOUT MEMBERS
                                             ( <booking> ) ) ).
    ENDIF.

  ENDMETHOD.


**********************************************************************
*
* Validates booking status when saving booking data
*
**********************************************************************
  METHOD validate_booking_status.

    READ ENTITY /DMO/I_Travel_M\\booking FROM VALUE #(
      FOR <root_key> IN keys ( %key     = <root_key>
                               %control = VALUE #( booking_status = if_abap_behv=>mk-on ) ) )
      RESULT DATA(lt_booking_result).

    LOOP AT lt_booking_result INTO DATA(ls_booking_result).
      CASE ls_booking_result-booking_status.
        WHEN 'N'.  " New
        WHEN 'X'.  " Canceled
        WHEN 'B'.  " Booked

        WHEN OTHERS.
          APPEND VALUE #( %key = ls_booking_result-%key ) TO failed.

          APPEND VALUE #( %key = ls_booking_result-%key
                          %msg = new_message( id       = /dmo/cx_flight_legacy=>status_is_not_valid-msgid
                                              number   = /dmo/cx_flight_legacy=>status_is_not_valid-msgno
                                              v1       = ls_booking_result-booking_status
                                              severity = if_abap_behv_message=>severity-error )
                          %element-booking_status = if_abap_behv=>mk-on ) TO reported.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


********************************************************************************
*
* Triggers feature control for booking data
*
********************************************************************************
  METHOD get_features.

    READ ENTITY /dmo/i_booking_m FROM VALUE #( FOR keyval IN keys
                                                      (  %key                  = keyval-%key
                                                         %control-booking_id   = if_abap_behv=>mk-on
                                                         %control-booking_date = if_abap_behv=>mk-on
                                                         %control-customer_id  = if_abap_behv=>mk-on
                                                      ) )
                                 RESULT    DATA(lt_booking_result).

    result = VALUE #( FOR ls_travel IN lt_booking_result
                       ( %key                = ls_travel-%key
                         %field-booking_id   = if_abap_behv=>fc-f-read_only
                         %field-booking_date = if_abap_behv=>fc-f-read_only
                         %field-customer_id  = if_abap_behv=>fc-f-read_only
                         "%features-%delete   = if_abap_behv=>fc-o-disabled  " Workaround for missing determinations OnDelete
                      ) ).

  ENDMETHOD.


********************************************************************************
*
* Workaround
*
********************************************************************************
  METHOD create_booking_supplement.

    DATA: lv_next_booksuppl_id TYPE /dmo/booking_supplement_id.

    LOOP AT keys INTO DATA(ls_cba).

      READ ENTITY /DMO/I_Booking_M BY \_BookSupplement
      FROM VALUE #( ( travel_id  = ls_cba-travel_id
                      booking_id = ls_cba-booking_id
                      %control  = VALUE #( travel_id  = if_abap_behv=>mk-on
                                           booking_id = if_abap_behv=>mk-on ) ) )
           RESULT   DATA(lt_read_result)
           FAILED   DATA(ls_read_failed)
           REPORTED DATA(ls_read_reported).

      IF lt_read_result IS INITIAL.
        lv_next_booksuppl_id = '01'.
      ELSE.
        SORT lt_read_result BY booking_supplement_id DESCENDING.
        lv_next_booksuppl_id = lt_read_result[ 1 ]-booking_supplement_id + 1.
      ENDIF.

      "MODIFY ENTITY /DMO/I_Booking_M
      MODIFY ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
        ENTITY booking
          CREATE BY \_BookSupplement FROM VALUE #( ( travel_id  = ls_cba-travel_id
                                                     booking_id = ls_cba-booking_id
                                                     %target    = VALUE #( ( travel_id                      = ls_cba-travel_id    "full key is required
                                                                             booking_id                     = ls_cba-booking_id   "full key is required
                                                                             booking_supplement_id          = lv_next_booksuppl_id
                                                                             supplement_id                  = 'ML-0012'
                                                                             price                          = '17.00'
                                                                             currency_code                  = 'EUR'
                                                                             %control-travel_id             = if_abap_behv=>mk-on
                                                                             %control-booking_id            = if_abap_behv=>mk-on
                                                                             %control-booking_supplement_id = if_abap_behv=>mk-on
                                                                             %control-supplement_id         = if_abap_behv=>mk-on
                                                                             %control-price                 = if_abap_behv=>mk-on
                                                                             %control-currency_code         = if_abap_behv=>mk-on ) ) ) )
          FAILED   DATA(ls_failed)
          MAPPED   DATA(ls_mapped)
          REPORTED DATA(ls_reported).

      APPEND LINES OF ls_failed-booksuppl   TO failed-booksuppl.
      APPEND LINES OF ls_reported-booksuppl TO reported-booksuppl.

      APPEND VALUE #( travel_id         = ls_cba-travel_id
                      booking_id        = ls_cba-booking_id
                      %param-travel_id  = ls_cba-travel_id
                      %param-booking_id = ls_cba-booking_id ) TO result.
    ENDLOOP.

  ENDMETHOD.




*  METHOD check_authority_for_booking.
*
*    LOOP AT it_booking_key INTO DATA(ls_booking_key).
*
*      result = VALUE #( ( travel_id           = ls_booking_key-travel_id
*                          booking_id          = ls_booking_key-booking_id
*                          %action-createBookingSupplement = if_abap_behv=>auth-unauthorized
*                      ) ).
*
*    ENDLOOP.
*
*  ENDMETHOD.

ENDCLASS.
