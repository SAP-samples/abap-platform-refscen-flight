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
    DATA lt_bookingsupplement_update TYPE TABLE FOR UPDATE /DMO/I_Travel_D\\BookingSupplement.

    "Read all bookings for the requested booking supplements
    " If multiple booking supplements of the same booking are requested, the booking is returned only once.
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Booking
        FIELDS (  BookingUUID  )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_booking).

    " Process all affected bookings. Read respective booking supplements for one booking
    LOOP AT lt_booking INTO DATA(ls_booking).
      READ ENTITIES OF /dmo/i_travel_d IN LOCAL MODE
        ENTITY Booking BY \_BookingSupplement
          FIELDS ( BookingSupplementID )
          WITH VALUE #( ( %tky = ls_booking-%tky ) )
        RESULT DATA(lt_bookingsupplement).

      " find max used bookingID in all bookings of this travel
      max_bookingsupplementid = '00'.
      LOOP AT lt_bookingsupplement INTO DATA(ls_bookingsupplement).
        IF ls_bookingsupplement-BookingSupplementID > max_bookingsupplementid.
          max_bookingsupplementid = ls_bookingsupplement-BookingSupplementID.
        ENDIF.
      ENDLOOP.

      "Provide a booking supplement ID for all booking supplement of this booking that have none.
      LOOP AT lt_bookingsupplement INTO ls_bookingsupplement WHERE BookingSupplementID IS INITIAL.
        max_bookingsupplementid += 1.
        APPEND VALUE #( %tky                = ls_bookingsupplement-%tky
                        bookingsupplementid = max_bookingsupplementid ) TO lt_bookingsupplement_update.

      ENDLOOP.
    ENDLOOP.

    " Provide a booking ID for all bookings that have none.
    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement
        UPDATE FIELDS ( BookingSupplementID ) WITH lt_bookingsupplement_update
      REPORTED DATA(lt_reported).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

  METHOD calculateTotalPrice.
    " Read all parent UUIDs
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Travel
        FIELDS ( TravelUUID  )
        WITH CORRESPONDING #(  keys  )
      RESULT DATA(lt_travel).

    " Trigger Re-Calculation on Root Node
    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Travel
        EXECUTE reCalcTotalPrice
          FROM CORRESPONDING  #( lt_travel )
    REPORTED DATA(lt_reported).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

  METHOD validateSupplement.

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement
        FIELDS ( SupplementID )
        WITH CORRESPONDING #(  keys )
      RESULT DATA(lt_booksuppl)
      FAILED DATA(lt_failed).

    failed = CORRESPONDING #( DEEP lt_failed ).

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Booking
        FROM CORRESPONDING #( lt_booksuppl )
      LINK DATA(lt_link_booking).

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY BookingSupplement BY \_Travel
        FROM CORRESPONDING #( lt_booksuppl )
      LINK DATA(lt_link_travel).


    DATA lt_supplement TYPE SORTED TABLE OF /dmo/supplement WITH UNIQUE KEY supplement_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    lt_supplement = CORRESPONDING #( lt_booksuppl DISCARDING DUPLICATES MAPPING supplement_id = SupplementID EXCEPT * ).
    DELETE lt_supplement WHERE supplement_id IS INITIAL.

    IF  lt_supplement IS NOT INITIAL.
      " Check if customer ID exists
      SELECT FROM /dmo/supplement FIELDS supplement_id
                                  FOR ALL ENTRIES IN @lt_supplement
                                  WHERE supplement_id = @lt_supplement-supplement_id
      INTO TABLE @DATA(lt_supplement_db).
    ENDIF.

    LOOP AT lt_booksuppl ASSIGNING FIELD-SYMBOL(<fs_booksuppl>).

      APPEND VALUE #(  %tky        = <fs_booksuppl>-%tky
                       %state_area = 'VALIDATE_SUPPLEMENT' ) TO reported-bookingsupplement.

      IF <fs_booksuppl>-SupplementID IS  INITIAL.
        APPEND VALUE #( %tky = <fs_booksuppl>-%tky ) TO failed-bookingsupplement.

        APPEND VALUE #( %tky                  = <fs_booksuppl>-%tky
                        %state_area           = 'VALIDATE_SUPPLEMENT'
                        %msg                  = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>ENTER_SUPPLEMENT_ID
                                                                severity = if_abap_behv_message=>severity-error )
                        %path                 = VALUE #( booking-%tky = lt_link_booking[ source-%tky = <fs_booksuppl>-%tky ]-target-%tky
                                                         travel-%tky  = lt_link_travel[ source-%tky  = <fs_booksuppl>-%tky ]-target-%tky )
                        %element-SupplementID = if_abap_behv=>mk-on ) TO reported-bookingsupplement.


      ELSEIF <fs_booksuppl>-SupplementID IS NOT INITIAL AND NOT line_exists( lt_supplement_db[ supplement_id = <fs_booksuppl>-SupplementID ] ).
        APPEND VALUE #(  %tky = <fs_booksuppl>-%tky ) TO failed-bookingsupplement.

        APPEND VALUE #( %tky                  = <fs_booksuppl>-%tky
                        %state_area           = 'VALIDATE_SUPPLEMENT'
                        %msg                  = NEW /dmo/cm_flight_messages(
                                                                textid = /dmo/cm_flight_messages=>SUPPLEMENT_UNKNOWN
                                                                severity = if_abap_behv_message=>severity-error )
                        %path                 = VALUE #( booking-%tky = lt_link_booking[ source-%tky = <fs_booksuppl>-%tky ]-target-%tky
                                                        travel-%tky  = lt_link_travel[ source-%tky  = <fs_booksuppl>-%tky ]-target-%tky )
                        %element-SupplementID = if_abap_behv=>mk-on ) TO reported-bookingsupplement.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
