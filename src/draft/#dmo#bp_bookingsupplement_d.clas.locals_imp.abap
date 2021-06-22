CLASS lhc_bookingsupplement DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS setBookSupplNumber FOR DETERMINE ON SAVE
      IMPORTING keys FOR BookingSupplement~setBookSupplNumber.
    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR BookingSupplement~calculateTotalPrice.

    METHODS validateSupplement FOR VALIDATE ON SAVE
      IMPORTING keys FOR BookingSupplement~validateSupplement.

ENDCLASS.

CLASS lhc_bookingsupplement IMPLEMENTATION.

  METHOD setBookSupplNumber.
    DATA max_bookingsupplementid TYPE /dmo/booking_supplement_id.
    DATA bookingsupplements_update TYPE TABLE FOR UPDATE /DMO/I_Travel_D\\BookingSupplement.

    "Read all bookings for the requested booking supplements
    " If multiple booking supplements of the same booking are requested, the booking is returned only once.
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Booking
        FIELDS (  BookingUUID  )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).

    " Process all affected bookings. Read respective booking supplements for one booking
    LOOP AT bookings INTO DATA(ls_booking).
      READ ENTITIES OF /dmo/i_travel_d IN LOCAL MODE
        ENTITY Booking BY \_BookingSupplement
          FIELDS ( BookingSupplementID )
          WITH VALUE #( ( %tky = ls_booking-%tky ) )
        RESULT DATA(bookingsupplements).

      " find max used bookingID in all bookings of this travel
      max_bookingsupplementid = '00'.
      LOOP AT bookingsupplements INTO DATA(bookingsupplement).
        IF bookingsupplement-BookingSupplementID > max_bookingsupplementid.
          max_bookingsupplementid = bookingsupplement-BookingSupplementID.
        ENDIF.
      ENDLOOP.

      "Provide a booking supplement ID for all booking supplement of this booking that have none.
      LOOP AT bookingsupplements INTO bookingsupplement WHERE BookingSupplementID IS INITIAL.
        max_bookingsupplementid += 1.
        APPEND VALUE #( %tky                = bookingsupplement-%tky
                        bookingsupplementid = max_bookingsupplementid
                      ) TO bookingsupplements_update.

      ENDLOOP.
    ENDLOOP.

    " Provide a booking ID for all bookings that have none.
    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement
        UPDATE FIELDS ( BookingSupplementID ) WITH bookingsupplements_update
      REPORTED DATA(update_reported).

    reported = CORRESPONDING #( DEEP update_reported ).

  ENDMETHOD.

  METHOD calculateTotalPrice.
    " Read all parent UUIDs
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Travel
        FIELDS ( TravelUUID  )
        WITH CORRESPONDING #(  keys  )
      RESULT DATA(travels).

    " Trigger Re-Calculation on Root Node
    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        EXECUTE reCalcTotalPrice
          FROM CORRESPONDING  #( travels )
    REPORTED DATA(action_reported).

    reported = CORRESPONDING #( DEEP action_reported ).

  ENDMETHOD.

  METHOD validateSupplement.

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement
        FIELDS ( SupplementID )
        WITH CORRESPONDING #(  keys )
      RESULT DATA(bookingsupplements)
      FAILED DATA(read_failed).

    failed = CORRESPONDING #( DEEP read_failed ).

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Booking
        FROM CORRESPONDING #( bookingsupplements )
      LINK DATA(booksuppl_booking_links).

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Travel
        FROM CORRESPONDING #( bookingsupplements )
      LINK DATA(booksuppl_travel_links).


    DATA supplements TYPE SORTED TABLE OF /dmo/supplement WITH UNIQUE KEY supplement_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    supplements = CORRESPONDING #( bookingsupplements DISCARDING DUPLICATES MAPPING supplement_id = SupplementID EXCEPT * ).
    DELETE supplements WHERE supplement_id IS INITIAL.

    IF  supplements IS NOT INITIAL.
      " Check if customer ID exists
      SELECT FROM /dmo/supplement FIELDS supplement_id
                                  FOR ALL ENTRIES IN @supplements
                                  WHERE supplement_id = @supplements-supplement_id
      INTO TABLE @DATA(valid_supplements).
    ENDIF.

    LOOP AT bookingsupplements ASSIGNING FIELD-SYMBOL(<bookingsupplement>).

      APPEND VALUE #(  %tky        = <bookingsupplement>-%tky
                       %state_area = 'VALIDATE_SUPPLEMENT'
                    ) TO reported-bookingsupplement.

      IF <bookingsupplement>-SupplementID IS  INITIAL.
        APPEND VALUE #( %tky = <bookingsupplement>-%tky ) TO failed-bookingsupplement.

        APPEND VALUE #( %tky                  = <bookingsupplement>-%tky
                        %state_area           = 'VALIDATE_SUPPLEMENT'
                        %msg                  = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>ENTER_SUPPLEMENT_ID
                                                                severity = if_abap_behv_message=>severity-error )
                        %path                 = VALUE #( booking-%tky = booksuppl_booking_links[ source-%tky = <bookingsupplement>-%tky ]-target-%tky
                                                         travel-%tky  = booksuppl_travel_links[ source-%tky  = <bookingsupplement>-%tky ]-target-%tky )
                        %element-SupplementID = if_abap_behv=>mk-on
                       ) TO reported-bookingsupplement.


      ELSEIF <bookingsupplement>-SupplementID IS NOT INITIAL AND NOT line_exists( valid_supplements[ supplement_id = <bookingsupplement>-SupplementID ] ).
        APPEND VALUE #(  %tky = <bookingsupplement>-%tky ) TO failed-bookingsupplement.

        APPEND VALUE #( %tky                  = <bookingsupplement>-%tky
                        %state_area           = 'VALIDATE_SUPPLEMENT'
                        %msg                  = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>SUPPLEMENT_UNKNOWN
                                                                severity = if_abap_behv_message=>severity-error )
                        %path                 = VALUE #( booking-%tky = booksuppl_booking_links[ source-%tky = <bookingsupplement>-%tky ]-target-%tky
                                                        travel-%tky  = booksuppl_travel_links[ source-%tky  = <bookingsupplement>-%tky ]-target-%tky )
                        %element-SupplementID = if_abap_behv=>mk-on
                       ) TO reported-bookingsupplement.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
