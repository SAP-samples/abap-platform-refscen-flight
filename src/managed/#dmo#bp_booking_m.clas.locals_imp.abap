CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS calculatetotalprice FOR DETERMINE ON MODIFY
       IMPORTING keys FOR booking~calculatetotalprice.
    METHODS validateStatus FOR VALIDATE  ON SAVE
       IMPORTING keys FOR booking~validatestatus.
    METHODS get_features FOR INSTANCE FEATURES
       IMPORTING keys REQUEST requested_features FOR booking RESULT result.
    METHODS earlynumbering_cba_booksupplem FOR NUMBERING
       IMPORTING entities FOR CREATE booking\_booksupplement.


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
    REPORTED DATA(reported_modify).

    reported = CORRESPONDING #( DEEP reported_modify ).
  ENDMETHOD.

**********************************************************************
*
* Validates booking status when saving booking data
*
**********************************************************************
  METHOD validateStatus.


    READ ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
      ENTITY booking
        FIELDS ( booking_status )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).

    LOOP AT bookings INTO DATA(booking).
      CASE booking-booking_status.
        WHEN 'N'.  " New
        WHEN 'X'.  " Canceled
        WHEN 'B'.  " Booked

          APPEND VALUE #(  %tky                 = booking-%tky
                           %state_area          = 'VALIDATE_BOOKINGSTATUS'
                        ) TO reported-booking.

        WHEN OTHERS.
          APPEND VALUE #( %key = booking-%key ) TO failed-booking.

          APPEND VALUE #( %key = booking-%key
                          %state_area         = 'VALIDATE_BOOKINGSTATUS'
                          %msg = NEW /dmo/cm_flight_messages(
                               textid = /dmo/cm_flight_messages=>status_invalid
                               status = booking-booking_status
                               severity = if_abap_behv_message=>severity-error )
                          %element-booking_status = if_abap_behv=>mk-on
                          %path = VALUE #( travel-travel_id    = booking-travel_id )
                        ) TO reported-booking.
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
      RESULT    DATA(bookings)
      FAILED failed.

    result = VALUE #( FOR booking IN bookings
                       ( %key                   = booking-%key
                         %assoc-_booksupplement = COND #( WHEN booking-booking_status = 'B'
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
        FROM CORRESPONDING #( entities )
        LINK DATA(booking_supplements)
      FAILED failed.

    " Loop over all unique tky (TravelID + BookingID)
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<booking_group>) GROUP BY <booking_group>-%tky.

      " Get highest bookingsupplement_id from bookings belonging to booking
      max_booking_suppl_id = REDUCE #( INIT max = CONV /dmo/booking_supplement_id( '0' )
                                       FOR  booksuppl IN booking_supplements USING KEY entity
                                                                             WHERE (     source-travel_id  = <booking_group>-travel_id
                                                                                     AND source-booking_id = <booking_group>-booking_id )
                                       NEXT max = COND /dmo/booking_supplement_id( WHEN   booksuppl-target-booking_supplement_id > max
                                                                          THEN booksuppl-target-booking_supplement_id
                                                                          ELSE max )
                                     ).
      " Get highest assigned bookingsupplement_id from incoming entities
      max_booking_suppl_id = REDUCE #( INIT max = max_booking_suppl_id
                                       FOR  entity IN entities USING KEY entity
                                                               WHERE (     travel_id  = <booking_group>-travel_id
                                                                       AND booking_id = <booking_group>-booking_id )
                                       FOR  target IN entity-%target
                                       NEXT max = COND /dmo/booking_supplement_id( WHEN   target-booking_supplement_id > max
                                                                                     THEN target-booking_supplement_id
                                                                                     ELSE max )
                                     ).


      " Loop over all entries in entities with the same TravelID and BookingID
      LOOP AT entities ASSIGNING FIELD-SYMBOL(<booking>) USING KEY entity WHERE travel_id  = <booking_group>-travel_id
                                                                            AND booking_id = <booking_group>-booking_id.

        " Assign new booking_supplement-ids
        LOOP AT <booking>-%target ASSIGNING FIELD-SYMBOL(<booksuppl_wo_numbers>).
          APPEND CORRESPONDING #( <booksuppl_wo_numbers> ) TO mapped-booksuppl ASSIGNING FIELD-SYMBOL(<mapped_booksuppl>).
          IF <booksuppl_wo_numbers>-booking_supplement_id IS INITIAL.
            max_booking_suppl_id += 1 .
            <mapped_booksuppl>-booking_supplement_id = max_booking_suppl_id .
          ENDIF.
        ENDLOOP.

      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
