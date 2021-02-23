CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS calculatetotalprice FOR DETERMINE ON MODIFY IMPORTING keys FOR booking~calculatetotalprice.
    METHODS validate_booking_status        FOR VALIDATE  ON SAVE   IMPORTING keys FOR booking~validatestatus.
    METHODS get_features                   FOR FEATURES            IMPORTING keys REQUEST requested_features FOR booking RESULT result.
    METHODS earlynumbering_cba_booksupplem FOR NUMBERING           IMPORTING entities FOR CREATE booking\_booksupplement.


*    METHODS check_authority_for_booking  FOR AUTHORIZATION IMPORTING it_booking_key REQUEST is_request FOR booking RESULT result.


ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

********************************************************************************
*
* Calculates total flight price
*
********************************************************************************
  METHOD calculatetotalprice.


    MODIFY ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
      ENTITY travel
        EXECUTE recalctotalprice
        FROM CORRESPONDING #( keys )
    REPORTED DATA(lt_reported).

    reported = CORRESPONDING #( DEEP lt_reported ).
  ENDMETHOD.

**********************************************************************
*
* Validates booking status when saving booking data
*
**********************************************************************
  METHOD validate_booking_status.


    READ ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
      ENTITY booking
        FIELDS ( booking_status )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_booking_result).

    LOOP AT lt_booking_result INTO DATA(ls_booking_result).
      CASE ls_booking_result-booking_status.
        WHEN 'N'.  " New
        WHEN 'X'.  " Canceled
        WHEN 'B'.  " Booked

          APPEND VALUE #(  %tky                 = ls_booking_result-%tky
                           %state_area          = 'VALIDATE_BOOKINGSTATUS' ) TO reported-booking.

        WHEN OTHERS.
          APPEND VALUE #( %key = ls_booking_result-%key ) TO failed-booking.

          APPEND VALUE #( %key = ls_booking_result-%key
                          %state_area         = 'VALIDATE_BOOKINGSTATUS'
                          %msg = NEW /dmo/cm_flight_messages(
                               textid = /dmo/cm_flight_messages=>status_invalid
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

    READ ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
      ENTITY booking
         FIELDS ( booking_id booking_status )
         WITH CORRESPONDING #( keys )
      RESULT    DATA(lt_booking_result)
      FAILED failed.

    result = VALUE #( FOR ls_travel IN lt_booking_result
                       ( %key                   = ls_travel-%key
                         %assoc-_booksupplement = COND #( WHEN ls_travel-booking_status = 'B'
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



  METHOD earlynumbering_cba_booksupplem.


    DATA: max_booking_suppl_id TYPE /dmo/booking_supplement_id .

    READ ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
      ENTITY booking BY \_booksupplement
        FIELDS ( booking_supplement_id )
          WITH CORRESPONDING #( entities )
          RESULT DATA(booking_supplements)
        FAILED failed.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<bookings>).

      " Get highest booking_id from bookings belonging to travel
      max_booking_suppl_id = REDUCE #( INIT max = CONV /dmo/booking_supplement_id( '0' )
                                       FOR  booksuppl IN booking_supplements USING KEY entity
                                                                             WHERE (     travel_id  = <bookings>-travel_id
                                                                                     AND booking_id = <bookings>-booking_id )
                                       NEXT max = COND /dmo/booking_supplement_id( WHEN   booksuppl-booking_supplement_id > max
                                                                          THEN booksuppl-booking_supplement_id
                                                                          ELSE max )
                                     ).


      " map booking_supplements which already have an id (required for draft)
      LOOP AT <bookings>-%target INTO DATA(booking_supplement_w_number) WHERE booking_supplement_id IS NOT INITIAL.
        APPEND CORRESPONDING #( booking_supplement_w_number ) TO mapped-booksuppl.
      ENDLOOP.

      "assign new booking_supplement-ids
      LOOP AT <bookings>-%target INTO DATA(booking_supplement_wo_number) WHERE booking_supplement_id IS INITIAL.
        APPEND CORRESPONDING #( booking_supplement_wo_number ) TO mapped-booksuppl ASSIGNING FIELD-SYMBOL(<mapped_booking_suppl>).
        max_booking_suppl_id += 5 .
        <mapped_booking_suppl>-booking_supplement_id = max_booking_suppl_id .
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
