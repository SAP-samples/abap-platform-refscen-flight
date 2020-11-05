CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS calculate_total_flight_price FOR DETERMINE ON MODIFY IMPORTING keys FOR booking~calculateTotalFlightPrice.
    METHODS validate_booking_status      FOR VALIDATE  ON SAVE   IMPORTING keys FOR booking~validateStatus.
    METHODS get_features                 FOR FEATURES            IMPORTING keys REQUEST requested_features FOR booking RESULT result.


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


    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY booking
        FIELDS ( booking_status )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_booking_result).

    LOOP AT lt_booking_result INTO DATA(ls_booking_result).
      CASE ls_booking_result-booking_status.
        WHEN 'N'.  " New
        WHEN 'X'.  " Canceled
        WHEN 'B'.  " Booked

        WHEN OTHERS.
          APPEND VALUE #( %key = ls_booking_result-%key ) TO failed-booking.

          APPEND VALUE #( %key = ls_booking_result-%key
                          %msg = new /dmo/cm_flight_messages(
                               textid = /dmo/cm_flight_messages=>TRAVEL_STATUS_INVALID
                               status = ls_booking_result-booking_status
                               severity = if_abap_behv_message=>severity-error )
                          %element-booking_status = if_abap_behv=>mk-on
                          %path = VALUE #( travel-travel_id    = ls_booking_result-travel_id ) ) TO reported-booking.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


********************************************************************************
*
* Triggers feature control for booking data
*
********************************************************************************
  METHOD get_features.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY booking
         FIELDS ( booking_id booking_status )
         WITH CORRESPONDING #( keys )
      RESULT    DATA(lt_booking_result)
      FAILED failed.

    result = VALUE #( FOR ls_travel IN lt_booking_result
                       ( %key                   = ls_travel-%key
                         %assoc-_BookSupplement = COND #( WHEN ls_travel-booking_status = 'B'
                                                          THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  ) ) ).

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
