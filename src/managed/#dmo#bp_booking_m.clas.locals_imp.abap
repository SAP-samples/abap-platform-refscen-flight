CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS calculate_total_flight_price FOR DETERMINATION booking~calculateTotalFlightPrice IMPORTING keys FOR booking.
    METHODS validate_booking_status      FOR VALIDATION booking~validateStatus   IMPORTING keys FOR booking.
    METHODS get_features                 FOR FEATURES IMPORTING keys REQUEST requested_features FOR booking RESULT result.

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

    READ ENTITY /DMO/I_Travel_M\\booking
         FIELDS ( booking_status )
           WITH VALUE #( FOR <root_key> IN keys ( %key = <root_key> ) )
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

    READ ENTITY /dmo/i_booking_m
         FIELDS ( booking_id booking_date customer_id )
           WITH VALUE #( FOR keyval IN keys ( %key = keyval-%key ) )
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
* Implements what operations and actions are not allowed for booking instances
*
********************************************************************************

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
