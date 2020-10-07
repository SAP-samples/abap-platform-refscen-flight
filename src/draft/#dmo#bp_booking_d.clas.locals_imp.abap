CLASS lhc_booking DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF booking_status,
        new    TYPE c LENGTH 1 VALUE 'N', "New
        booked TYPE c LENGTH 1 VALUE 'B', "Booked
      END OF booking_status.

    METHODS setBookingNumber FOR DETERMINE ON SAVE
      IMPORTING keys FOR Booking~setBookingNumber.
    METHODS setBookingDate FOR DETERMINE ON SAVE
      IMPORTING keys FOR Booking~setBookingDate.
    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Booking~calculateTotalPrice.

    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Booking~validateCustomer.
    METHODS validateConnection FOR VALIDATE ON SAVE
      IMPORTING keys FOR Booking~validateConnection.

ENDCLASS.

CLASS lhc_booking IMPLEMENTATION.

  METHOD setBookingNumber.
    DATA max_bookingid TYPE /dmo/booking_id.
    DATA lt_booking_update TYPE TABLE FOR UPDATE /DMO/I_Travel_D\\Booking.

    "Read all travels for the requested bookings
    " If multiple bookings of the same travel are requested, the travel is returned only once.
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Booking BY \_Travel
        FIELDS ( TravelUUID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travel).

    " Process all affected travels. Read respective bookings for one travel
    LOOP AT lt_travel INTO DATA(ls_travel).
      READ ENTITIES OF /dmo/i_travel_d IN LOCAL MODE
        ENTITY Travel BY \_Booking
          FIELDS ( BookingID )
          WITH VALUE #( ( %tky = ls_travel-%tky ) )
        RESULT DATA(lt_booking).

      " find max used bookingID in all bookings of this travel
      max_bookingid = '0000'.
      LOOP AT lt_booking INTO DATA(ls_booking).
        IF ls_booking-BookingID > max_bookingid.
          max_bookingid = ls_booking-BookingID.
        ENDIF.
      ENDLOOP.

      "Provide a booking ID for all bookings of this travel that have none.
      LOOP AT lt_booking INTO ls_booking WHERE BookingID IS INITIAL.
        max_bookingid += 1.
        APPEND VALUE #( %tky      = ls_booking-%tky
                        BookingID = max_bookingid ) TO lt_booking_update.

      ENDLOOP.
    ENDLOOP.

    " Provide a booking ID for all bookings that have none.
    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY booking
        UPDATE FIELDS ( BookingID ) WITH lt_booking_update
      REPORTED DATA(lt_reported).

    reported = CORRESPONDING #( DEEP lt_reported ).


  ENDMETHOD.

  METHOD setBookingDate.

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Booking
        FIELDS ( BookingDate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_booking).

    DELETE lt_booking WHERE BookingDate IS NOT INITIAL.
    CHECK lt_booking IS NOT INITIAL.

    LOOP AT lt_booking ASSIGNING FIELD-SYMBOL(<fs_booking>).
      <fs_booking>-BookingDate = cl_abap_context_info=>get_system_date( ).
    ENDLOOP.

    MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Booking
        UPDATE  FIELDS ( BookingDate )
        WITH CORRESPONDING #( lt_booking )
    REPORTED DATA(lt_reported).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

  METHOD calculateTotalPrice.

    " Read all parent UUIDs
    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Booking BY \_Travel
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


  METHOD validateCustomer.

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Booking
        FIELDS (  CustomerID )
        WITH CORRESPONDING #( keys )
    RESULT DATA(lt_booking)
    FAILED DATA(lt_failed).

    failed = CORRESPONDING #( DEEP lt_failed ).

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Booking BY \_Travel
        FROM CORRESPONDING #( lt_booking )
      LINK DATA(lt_link).

    DATA lt_customer TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    lt_customer = CORRESPONDING #( lt_booking DISCARDING DUPLICATES MAPPING customer_id = CustomerID EXCEPT * ).
    DELETE lt_customer WHERE customer_id IS INITIAL.

    IF  lt_customer IS NOT INITIAL.
      " Check if customer ID exists
      SELECT FROM /dmo/customer FIELDS customer_id
                                FOR ALL ENTRIES IN @lt_customer
                                WHERE customer_id = @lt_customer-customer_id
      INTO TABLE @DATA(lt_customer_db).
    ENDIF.

    " Raise message for non existing customer id
    LOOP AT lt_booking INTO DATA(ls_booking).
      APPEND VALUE #(  %tky               = ls_booking-%tky
                       %state_area        = 'VALIDATE_CUSTOMER' ) TO reported-booking.

      IF ls_booking-CustomerID IS  INITIAL.
        APPEND VALUE #( %tky = ls_booking-%tky ) TO failed-booking.

        APPEND VALUE #( %tky                = ls_booking-%tky
                        %state_area         = 'VALIDATE_CUSTOMER'
                        %msg                = new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                           number   = '056' " Customer is initial
                                                           v1       = ls_booking-BookingID
                                                           severity = if_abap_behv_message=>severity-error )
                        %path               = VALUE #( travel-%tky = lt_link[ source-%tky = ls_booking-%tky ]-target-%tky )
                        %element-CustomerID = if_abap_behv=>mk-on ) TO reported-booking.

      ELSEIF ls_booking-CustomerID IS NOT INITIAL AND NOT line_exists( lt_customer_db[ customer_id = ls_booking-CustomerID ] ).
        APPEND VALUE #(  %tky = ls_booking-%tky ) TO failed-booking.

        APPEND VALUE #( %tky                = ls_booking-%tky
                        %state_area         = 'VALIDATE_CUSTOMER'
                        %msg                = new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                           number   = '002' " Customer unknown
                                                           v1       = ls_booking-CustomerID
                                                           severity = if_abap_behv_message=>severity-error )
                        %path               = VALUE #( travel-%tky = lt_link[ source-%tky = ls_booking-%tky ]-target-%tky )
                        %element-CustomerID = if_abap_behv=>mk-on ) TO reported-booking.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD validateConnection.

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Booking
        FIELDS ( BookingID AirlineID ConnectionID FlightDate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_booking)
      FAILED DATA(lt_failed).

    failed = CORRESPONDING #( DEEP lt_failed ).

    READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
      ENTITY Booking BY \_Travel
        FROM CORRESPONDING #( lt_booking )
      LINK DATA(lt_link).

    LOOP AT lt_booking ASSIGNING FIELD-SYMBOL(<fs_booking>).
      "overwrite state area with empty message to avoid duplicate messages
      APPEND VALUE #(  %tky               = <fs_booking>-%tky
                       %state_area        = 'VALIDATE_CONNECTION' ) TO reported-booking.

      " Raise message for non existing airline ID
      IF <fs_booking>-AirlineID IS INITIAL.
        APPEND VALUE #( %tky = <fs_booking>-%tky ) TO failed-booking.

        APPEND VALUE #( %tky                = <fs_booking>-%tky
                        %state_area         = 'VALIDATE_CONNECTION'
                        %msg                = new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                           number   = '051' " AirlineID is initial
                                                           v1       = <fs_booking>-BookingID
                                                           severity = if_abap_behv_message=>severity-error )
                        %path              = VALUE #( travel-%tky = lt_link[ source-%tky = <fs_booking>-%tky ]-target-%tky )
                        %element-AirlineID = if_abap_behv=>mk-on ) TO reported-booking.
      ENDIF.
      " Raise message for non existing connection ID
      IF <fs_booking>-ConnectionID IS INITIAL.
        APPEND VALUE #( %tky = <fs_booking>-%tky ) TO failed-booking.

        APPEND VALUE #( %tky                = <fs_booking>-%tky
                        %state_area         = 'VALIDATE_CONNECTION'
                        %msg                = new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                           number   = '052' " Connection ID is initial
                                                           v1       = <fs_booking>-BookingID
                                                           severity = if_abap_behv_message=>severity-error )
                        %path               = VALUE #( travel-%tky = lt_link[ source-%tky = <fs_booking>-%tky ]-target-%tky )
                        %element-ConnectionID = if_abap_behv=>mk-on ) TO reported-booking.
      ENDIF.
      " Raise message for non existing flight date
      IF <fs_booking>-FlightDate IS INITIAL.
        APPEND VALUE #( %tky = <fs_booking>-%tky ) TO failed-booking.

        APPEND VALUE #( %tky                = <fs_booking>-%tky
                        %state_area         = 'VALIDATE_CONNECTION'
                        %msg                = new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                           number   = '053' " FLightDate is initial
                                                           v1       =  <fs_booking>-BookingID
                                                           severity = if_abap_behv_message=>severity-error )
                        %path               = VALUE #( travel-%tky = lt_link[ source-%tky = <fs_booking>-%tky ]-target-%tky )
                        %element-FlightDate = if_abap_behv=>mk-on ) TO reported-booking.
      ENDIF.
      " check if flight connection exists
      IF <fs_booking>-AirlineID IS NOT INITIAL AND
         <fs_booking>-ConnectionID IS NOT INITIAL AND
         <fs_booking>-FlightDate IS NOT INITIAL.
        SELECT SINGLE * FROM /dmo/flight  WHERE  carrier_id    = @<fs_booking>-AirlineID
                                            AND  connection_id = @<fs_booking>-ConnectionID
                                            AND  flight_date   = @<fs_booking>-FlightDate
                                           INTO  @DATA(ls_flight).

        IF sy-subrc <> 0.
          APPEND VALUE #( %tky = <fs_booking>-%tky ) TO failed-booking.

          APPEND VALUE #( %tky                 = <fs_booking>-%tky
                          %state_area          = 'VALIDATE_CONNECTION'
                          %msg                 = new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                              number   = '050' " Flight invalid
                                                              v1       = <fs_booking>-AirlineID
                                                              v2       = <fs_booking>-ConnectionID
                                                              v3       = <fs_booking>-FlightDate
                                                              severity = if_abap_behv_message=>severity-error )
                        %path                  = VALUE #( travel-%tky = lt_link[ source-%tky = <fs_booking>-%tky ]-target-%tky )
                        %element-FlightDate    = if_abap_behv=>mk-on
                        %element-AirlineID     = if_abap_behv=>mk-on
                        %element-ConnectionID  = if_abap_behv=>mk-on ) TO reported-booking.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
