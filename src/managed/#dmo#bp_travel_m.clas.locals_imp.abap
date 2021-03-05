CLASS test_readonly_methods DEFINITION DEFERRED FOR TESTING.
CLASS test_writing_methods  DEFINITION DEFERRED FOR TESTING.
CLASS test_using_entity_stub  DEFINITION DEFERRED FOR TESTING.
CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler
  FRIENDS test_readonly_methods test_writing_methods test_using_entity_stub.

  PRIVATE SECTION.

    TYPES tt_travel_update TYPE TABLE FOR UPDATE /DMO/I_Travel_M.

    METHODS validate_customer          FOR VALIDATE ON SAVE IMPORTING keys FOR travel~validatecustomer.
    METHODS validate_agency            FOR VALIDATE ON SAVE IMPORTING keys FOR travel~validateagency.
    METHODS validate_dates             FOR VALIDATE ON SAVE IMPORTING keys FOR travel~validatedates.
    METHODS validate_travel_status     FOR VALIDATE ON SAVE IMPORTING keys FOR travel~validatestatus.

    METHODS copyTravel                 FOR MODIFY IMPORTING   keys FOR ACTION travel~copyTravel                RESULT result.
    METHODS set_status_accepted        FOR MODIFY IMPORTING   keys FOR ACTION travel~accepttravel              RESULT result.
    METHODS set_status_rejected        FOR MODIFY IMPORTING   keys FOR ACTION travel~rejecttravel              RESULT result.
    METHODS get_features               FOR FEATURES IMPORTING keys REQUEST    requested_features FOR travel    RESULT result.
    METHODS recalctotalprice FOR MODIFY


      IMPORTING keys FOR ACTION travel~recalctotalprice.
    METHODS calculatetotalprice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR travel~calculatetotalprice.
    METHODS earlynumbering_cba_booking FOR NUMBERING
      IMPORTING entities FOR CREATE travel\_booking.
    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE travel.

*    METHODS check_authority_for_travel FOR AUTHORIZATION IMPORTING it_travel_key REQUEST is_request FOR travel RESULT result.


ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

**********************************************************************
*
* Validate customer data when saving travel data
*
**********************************************************************
  METHOD validate_customer.
    " Read relevant travel instance data
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
    ENTITY travel
     FIELDS ( customer_id )
     WITH CORRESPONDING #(  keys )
    RESULT DATA(lt_travel).

    DATA lt_customer TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    lt_customer = CORRESPONDING #( lt_travel DISCARDING DUPLICATES MAPPING customer_id = customer_id EXCEPT * ).
    DELETE lt_customer WHERE customer_id IS INITIAL.
    IF lt_customer IS NOT INITIAL.

      " Check if customer ID exists
      SELECT FROM /dmo/customer FIELDS customer_id
        FOR ALL ENTRIES IN @lt_customer
        WHERE customer_id = @lt_customer-customer_id
        INTO TABLE @DATA(lt_customer_db).
    ENDIF.
    " Raise msg for non existing and initial customer id
    LOOP AT lt_travel INTO DATA(ls_travel).
      IF ls_travel-customer_id IS INITIAL
         OR NOT line_exists( lt_customer_db[ customer_id = ls_travel-customer_id ] ).

        APPEND VALUE #(  travel_id = ls_travel-travel_id ) TO failed-travel.
        APPEND VALUE #(  travel_id = ls_travel-travel_id
                         %msg = NEW /dmo/cm_flight_messages(
                              customer_id = ls_travel-customer_id
                              textid = /dmo/cm_flight_messages=>customer_unkown
                              severity = if_abap_behv_message=>severity-error )
                         %element-customer_id = if_abap_behv=>mk-on )
          TO reported-travel.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

**********************************************************************
*
* Validate agency data when saving travel data
*
**********************************************************************

  METHOD validate_agency.
    " Read relevant travel instance data
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
    ENTITY travel
     FIELDS ( agency_id )
     WITH CORRESPONDING #(  keys )
    RESULT DATA(lt_travel).

    DATA lt_agency TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id.

    " Optimization of DB select: extract distinct non-initial agency IDs
    lt_agency = CORRESPONDING #(  lt_travel DISCARDING DUPLICATES MAPPING agency_id = agency_id EXCEPT * ).
    DELETE lt_agency WHERE agency_id IS INITIAL.
    IF  lt_agency IS NOT INITIAL.

      " check if agency ID exist
      SELECT FROM /dmo/agency FIELDS agency_id
        FOR ALL ENTRIES IN @lt_agency
        WHERE agency_id = @lt_agency-agency_id
        INTO TABLE @DATA(lt_agency_db).
    ENDIF.

    " Raise msg for non existing and initial agency id
    LOOP AT lt_travel INTO DATA(ls_travel).
      IF ls_travel-agency_id IS INITIAL
         OR NOT line_exists( lt_agency_db[ agency_id = ls_travel-agency_id ] ).
        APPEND VALUE #(  travel_id = ls_travel-travel_id ) TO failed-travel.
        APPEND VALUE #(  travel_id = ls_travel-travel_id
                         %msg = NEW /dmo/cm_flight_messages(
                          textid = /dmo/cm_flight_messages=>agency_unkown
                          agency_id = ls_travel-agency_id
                          severity = if_abap_behv_message=>severity-error )
                         %element-agency_id = if_abap_behv=>mk-on )
          TO reported-travel.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


**********************************************************************
* Check validity of date
*
**********************************************************************
  METHOD validate_dates.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
        FIELDS ( begin_date end_date )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travel_result).

    LOOP AT lt_travel_result INTO DATA(ls_travel_result).

      IF ls_travel_result-end_date < ls_travel_result-begin_date.  "end_date before begin_date

        APPEND VALUE #( %key        = ls_travel_result-%key
                        travel_id   = ls_travel_result-travel_id ) TO failed-travel.

        APPEND VALUE #( %key     = ls_travel_result-%key
                        %msg = NEW /dmo/cm_flight_messages(
                               textid = /dmo/cm_flight_messages=>begin_date_bef_end_date
                               severity = if_abap_behv_message=>severity-error
                               begin_date = ls_travel_result-begin_date
                               end_date = ls_travel_result-end_date
                               travel_id = ls_travel_result-travel_id
                          )
                        %element-begin_date = if_abap_behv=>mk-on
                        %element-end_date   = if_abap_behv=>mk-on ) TO reported-travel.

      ELSEIF ls_travel_result-begin_date < cl_abap_context_info=>get_system_date( ).  "begin_date must be in the future

        APPEND VALUE #( %key        = ls_travel_result-%key
                        travel_id   = ls_travel_result-travel_id ) TO failed-travel.

        APPEND VALUE #( %key = ls_travel_result-%key
                        %msg = NEW /dmo/cm_flight_messages(
                               textid = /dmo/cm_flight_messages=>begin_date_on_or_bef_sysdate
                               severity = if_abap_behv_message=>severity-error )

                        %element-begin_date = if_abap_behv=>mk-on
                        %element-end_date   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

**********************************************************************
*
* Validate travel status when saving travel data
*
**********************************************************************
  METHOD validate_travel_status.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
        ENTITY travel
          FIELDS ( overall_status )
          WITH CORRESPONDING #( keys )
        RESULT DATA(lt_travel_result).

    LOOP AT lt_travel_result INTO DATA(ls_travel_result).
      CASE ls_travel_result-overall_status.
        WHEN 'O'.  " Open
        WHEN 'X'.  " Cancelled
        WHEN 'A'.  " Accepted

        WHEN OTHERS.
          APPEND VALUE #( %key = ls_travel_result-%key ) TO failed-travel.

          APPEND VALUE #( %key = ls_travel_result-%key
                          %msg = NEW /dmo/cm_flight_messages(
                               textid = /dmo/cm_flight_messages=>status_invalid
                               severity = if_abap_behv_message=>severity-error
                               status = ls_travel_result-overall_status )
                          %element-overall_status = if_abap_behv=>mk-on ) TO reported-travel.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

**********************************************************************
*
* Create travel instances with initial values
*
**********************************************************************
  METHOD copyTravel.

    DATA:
      travels       TYPE TABLE FOR CREATE /DMO/I_Travel_M\\travel,
      bookings_cba  TYPE TABLE FOR CREATE /DMO/I_Travel_M\\travel\_booking,
      booksuppl_cba TYPE TABLE FOR CREATE /DMO/I_Travel_M\\booking\_booksupplement.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
       ALL FIELDS WITH CORRESPONDING #( keys )
                  RESULT DATA(travel_read_result)
     FAILED    failed
     REPORTED  reported.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel BY \_booking
        ALL FIELDS WITH CORRESPONDING #( travel_read_result )
                   RESULT DATA(book_read_result).

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY booking BY \_booksupplement
       ALL FIELDS WITH CORRESPONDING #( book_read_result )
                  RESULT DATA(booksuppl_read_result).


    LOOP AT travel_read_result ASSIGNING FIELD-SYMBOL(<travel>).
      APPEND VALUE #( %cid     = <travel>-travel_id   %data = CORRESPONDING #( <travel> EXCEPT travel_id ) ) TO travels ASSIGNING FIELD-SYMBOL(<new_travel>).
      APPEND VALUE #( %cid_ref = <travel>-travel_id ) TO bookings_cba ASSIGNING FIELD-SYMBOL(<bookings_cba>).

      <new_travel>-begin_date     = cl_abap_context_info=>get_system_date( ).
      <new_travel>-end_date       = cl_abap_context_info=>get_system_date( ) + 30.
      <new_travel>-overall_status = 'O'.  "Set to open to allow an editable instance

      LOOP AT book_read_result ASSIGNING FIELD-SYMBOL(<booking>) USING KEY entity WHERE travel_id EQ <travel>-travel_id.
        APPEND VALUE #( %cid     = <travel>-travel_id && <booking>-booking_id
                        %data    = CORRESPONDING #( <booking> EXCEPT travel_id ) ) TO <bookings_cba>-%target ASSIGNING FIELD-SYMBOL(<new_booking>).
        APPEND VALUE #( %cid_ref = <travel>-travel_id && <booking>-booking_id ) TO booksuppl_cba ASSIGNING FIELD-SYMBOL(<booksuppl_cba>).

        <new_booking>-booking_status = 'N'.

        LOOP AT booksuppl_read_result ASSIGNING FIELD-SYMBOL(<booksuppl>) USING KEY entity WHERE travel_id  EQ <travel>-travel_id
                                                                                           AND   booking_id EQ <booking>-booking_id.
          APPEND VALUE #( %cid  = <travel>-travel_id && <booking>-booking_id && <booksuppl>-booking_supplement_id
                          %data = CORRESPONDING #( <booksuppl> EXCEPT travel_id booking_id ) ) TO <booksuppl_cba>-%target.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    MODIFY ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
        CREATE SET FIELDS WITH travels
        CREATE BY \_Booking SET FIELDS WITH bookings_cba
      ENTITY booking
        CREATE BY \_BookSupplement SET FIELDS WITH booksuppl_cba
      MAPPED mapped
      FAILED DATA(failed_create)
      REPORTED DATA(reported_create).

    failed-travel   = CORRESPONDING #( BASE ( failed-travel )   failed_create-travel   MAPPING travel_id = %cid EXCEPT %cid ).
    reported-travel = CORRESPONDING #( BASE ( reported-travel ) reported_create-travel MAPPING travel_id = %cid EXCEPT %cid ).


    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
       ENTITY travel
         ALL FIELDS WITH CORRESPONDING #( mapped-travel )
         RESULT DATA(read_created_result).

    result = VALUE #( FOR new IN read_created_result ( %param = new  travel_id = new-travel_id ) ).
    result = CORRESPONDING #( result FROM mapped-travel USING KEY entity  travel_id = travel_id MAPPING travel_id = %cid     ).
    result = CORRESPONDING #( result FROM keys          USING KEY entity  travel_id = travel_id MAPPING %cid_ref  = %cid_ref ).


  ENDMETHOD.


********************************************************************************
*
* Implements travel action (in our case: for setting travel overall_status to completed)
*
********************************************************************************
  METHOD set_status_accepted.

    " Modify in local mode: BO-related updates that are not relevant for authorization checks
    MODIFY ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
           ENTITY travel
              UPDATE FIELDS ( overall_status )
                 WITH VALUE #( FOR key IN keys ( travel_id      = key-travel_id
                                                 overall_status = 'A' ) ) " Accepted
           FAILED   failed
           REPORTED reported.

    " Read changed data for action result
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
         ENTITY travel
           FIELDS ( agency_id
                    customer_id
                    begin_date
                    end_date
                    booking_fee
                    total_price
                    currency_code
                    overall_status
                    description
                    created_by
                    created_at
                    last_changed_at
                    last_changed_by )
             WITH VALUE #( FOR key IN keys ( travel_id = key-travel_id ) )
         RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels ( travel_id = travel-travel_id
                                                %param    = travel ) ).

  ENDMETHOD.

********************************************************************************
*
* Implements travel action(s) (in our case: for setting travel overall_status to cancelled)
*
********************************************************************************
  METHOD set_status_rejected.

    MODIFY ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
           ENTITY travel
              UPDATE FROM VALUE #( FOR key IN keys ( travel_id = key-travel_id
                                                     overall_status = 'X'   " Canceled
                                                     %control-overall_status = if_abap_behv=>mk-on ) )
           FAILED   failed
           REPORTED reported.

    " read changed data for result
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
     ENTITY travel
       FIELDS ( agency_id
                customer_id
                begin_date
                end_date
                booking_fee
                total_price
                currency_code
                overall_status
                description
                created_by
                created_at
                last_changed_at
                last_changed_by )
         WITH VALUE #( FOR key IN keys ( travel_id = key-travel_id ) )
     RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels ( travel_id = travel-travel_id
                                                %param    = travel
                                              ) ).

  ENDMETHOD.

********************************************************************************
*
* Implements the dynamic feature handling for travel instances
*
********************************************************************************
  METHOD get_features.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
         FIELDS (  travel_id overall_status )
         WITH CORRESPONDING #( keys )
       RESULT DATA(lt_travel_result)
       FAILED failed.


    result = VALUE #( FOR ls_travel IN lt_travel_result
                       ( %key                           = ls_travel-%key
                         %features-%action-rejecttravel = COND #( WHEN ls_travel-overall_status = 'X'
                                                                    THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                         %features-%action-accepttravel = COND #( WHEN ls_travel-overall_status = 'A'
                                                                    THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
                         %assoc-_booking                = COND #( WHEN ls_travel-overall_status = 'X'
                                                                    THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
                      ) ).

  ENDMETHOD.

********************************************************************************
*
* Implements what operations and actions are not allowed for travel instances
*
********************************************************************************
*  METHOD check_authority_for_travel.
*
*    DATA ls_result LIKE LINE OF result.
*    LOOP AT it_travel_key INTO DATA(ls_travel_key).
*      ls_result = VALUE #( travel_id            = ls_travel_key-travel_id
*                          %update              = if_abap_behv=>auth-allowed       "Default setting
*                          %delete              = if_abap_behv=>auth-unauthorized
*                          %action-rejectTravel = if_abap_behv=>auth-unauthorized
*                         ).
*      APPEND ls_result to result.
*    ENDLOOP.
*
*  ENDMETHOD.
*
  METHOD recalctotalprice.
    TYPES: BEGIN OF ty_amount_per_currencycode,
             amount        TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
           END OF ty_amount_per_currencycode.

    DATA: amount_per_currencycode TYPE STANDARD TABLE OF ty_amount_per_currencycode.

    " Read all relevant travel instances.
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
         ENTITY travel
            FIELDS ( booking_fee currency_code )
            WITH CORRESPONDING #( keys )
         RESULT DATA(lt_travel)
         FAILED failed.

    DELETE lt_travel WHERE currency_code IS INITIAL.

    LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<fs_travel>).
      " Set the start for the calculation by adding the booking fee.
      amount_per_currencycode = VALUE #( ( amount        = <fs_travel>-booking_fee
                                           currency_code = <fs_travel>-currency_code ) ).

      " Read all associated bookings and add them to the total price.
      READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
        ENTITY travel BY \_booking
          FIELDS ( flight_price currency_code )
        WITH VALUE #( ( %key = <fs_travel>-%key ) )
        RESULT DATA(lt_booking).

      LOOP AT lt_booking INTO DATA(booking) WHERE currency_code IS NOT INITIAL.
        COLLECT VALUE ty_amount_per_currencycode( amount        = booking-flight_price
                                                  currency_code = booking-currency_code ) INTO amount_per_currencycode.
      ENDLOOP.

      " Read all associated booking supplements and add them to the total price.
      READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
        ENTITY booking BY \_booksupplement
          FIELDS (  price currency_code )
        WITH VALUE #( FOR rba_booking IN lt_booking ( %tky = rba_booking-%tky ) )
        RESULT DATA(lt_bookingsupplement).

      LOOP AT lt_bookingsupplement INTO DATA(bookingsupplement) WHERE currency_code IS NOT INITIAL.
        COLLECT VALUE ty_amount_per_currencycode( amount        = bookingsupplement-price
                                                  currency_code = bookingsupplement-currency_code ) INTO amount_per_currencycode.
      ENDLOOP.

      CLEAR <fs_travel>-total_price.
      LOOP AT amount_per_currencycode INTO DATA(single_amount_per_currencycode).
        " If needed do a Currency Conversion
        IF single_amount_per_currencycode-currency_code = <fs_travel>-currency_code.
          <fs_travel>-total_price += single_amount_per_currencycode-amount.
        ELSE.
          /dmo/cl_flight_amdp=>convert_currency(
             EXPORTING
               iv_amount                   =  single_amount_per_currencycode-amount
               iv_currency_code_source     =  single_amount_per_currencycode-currency_code
               iv_currency_code_target     =  <fs_travel>-currency_code
               iv_exchange_rate_date       =  cl_abap_context_info=>get_system_date( )
             IMPORTING
               ev_amount                   = DATA(total_booking_price_per_curr)
            ).
          <fs_travel>-total_price += total_booking_price_per_curr.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " write back the modified total_price of travels
    MODIFY ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
        UPDATE FIELDS ( total_price )
        WITH CORRESPONDING #( lt_travel ).

  ENDMETHOD.

  METHOD calculatetotalprice.

    MODIFY ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
        EXECUTE recalctotalprice
        FROM CORRESPONDING #( keys )
    REPORTED DATA(lt_reported).

    reported = CORRESPONDING #( DEEP lt_reported ).
  ENDMETHOD.

  METHOD earlynumbering_create.

    DATA:
      entity        TYPE STRUCTURE FOR CREATE /DMO/I_Travel_M,
      travel_id_max TYPE /dmo/travel_id.

    " Ensure Travel ID is not set yet (idempotent)- must be checked when BO is draft-enabled
    LOOP AT entities INTO entity WHERE travel_id IS NOT INITIAL.
      APPEND CORRESPONDING #( entity ) TO mapped-travel.
    ENDLOOP.

    DATA(entities_wo_travelid) = entities.
    DELETE entities_wo_travelid WHERE travel_id IS NOT INITIAL.

    " Get Numbers
    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING
            nr_range_nr       = '01'
            object            = '/DMO/TRV_M'
            quantity          = CONV #( lines( entities_wo_travelid ) )
          IMPORTING
            number            = DATA(number_range_key)
            returncode        = DATA(number_range_return_code)
            returned_quantity = DATA(number_range_returned_quantity)
        ).
      CATCH cx_number_ranges INTO DATA(lx_number_ranges).
        LOOP AT entities_wo_travelid INTO entity.
          APPEND VALUE #(
                %cid = entity-%cid
                %key = entity-%key
                %msg = lx_number_ranges
            ) TO reported-travel.
          APPEND VALUE #(
                %cid        = entity-%cid
                %key        = entity-%key
            ) TO failed-travel.
        ENDLOOP.
        EXIT.
    ENDTRY.

    CASE number_range_return_code.
      WHEN '1'.
        " 1 - the returned number is in a critical range (specified under “percentage warning” in the object definition)
        LOOP AT entities_wo_travelid INTO entity.
          APPEND VALUE #(
                %cid      = entity-%cid
                %key      = entity-%key
                %msg      = NEW /dmo/cm_flight_messages(
                                textid = /dmo/cm_flight_messages=>number_range_depleted
                                severity = if_abap_behv_message=>severity-warning
                       )
            ) TO reported-travel.
        ENDLOOP.

      WHEN '2' OR '3'.
        " 2 - the last number of the interval was returned
        " 3 - if fewer numbers are available than requested,  the return code is 3
        LOOP AT entities_wo_travelid INTO entity.
          APPEND VALUE #(
                %cid      = entity-%cid
                %key      = entity-%key
                %msg      = NEW /dmo/cm_flight_messages(
                                textid = /dmo/cm_flight_messages=>not_sufficient_numbers
                                severity = if_abap_behv_message=>severity-error )
            ) TO reported-travel.
          APPEND VALUE #(
                %cid        = entity-%cid
                %key        = entity-%key
                %fail-cause = if_abap_behv=>cause-conflict
            ) TO failed-travel.
        ENDLOOP.
        EXIT.
    ENDCASE.

    " At this point ALL entities get a number!
    ASSERT number_range_returned_quantity = lines( entities_wo_travelid ).

    travel_id_max = number_range_key - number_range_returned_quantity.

    " Set Travel ID
    LOOP AT entities_wo_travelid INTO entity.
      travel_id_max += 1.
      entity-travel_id = travel_id_max .

      APPEND VALUE #(
          %cid      = entity-%cid
          %key      = entity-%key
        ) TO mapped-travel.
    ENDLOOP.

  ENDMETHOD.

  METHOD earlynumbering_cba_booking.

    DATA: max_booking_id TYPE /dmo/booking_id.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel BY \_booking
        FIELDS ( booking_id )
          WITH CORRESPONDING #( entities )
          RESULT DATA(bookings)
          FAILED failed.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<booking>).

      " Get highest booking_id from bookings belonging to travel
      max_booking_id = REDUCE #( INIT max = CONV /dmo/booking_id( '0' )
                                       FOR  booking IN bookings USING KEY entity
                                                                             WHERE (     travel_id  = <booking>-travel_id )
                                       NEXT max = COND /dmo/booking_id(      WHEN   booking-booking_id > max
                                                                             THEN booking-booking_id
                                                                             ELSE max )
                                     ).

      " map booking which already have an id (required for draft)

      LOOP AT <booking>-%target INTO DATA(booking_w_numbers) WHERE booking_id IS NOT INITIAL.
        APPEND CORRESPONDING #( booking_w_numbers ) TO mapped-booking.
      ENDLOOP.


      "assign new booking-ids

      LOOP AT <booking>-%target INTO DATA(booking_wo_numbers) WHERE booking_id IS INITIAL.
        APPEND CORRESPONDING #( booking_wo_numbers ) TO mapped-booking ASSIGNING FIELD-SYMBOL(<mapped_booking>).
        max_booking_id += 10 .
        <mapped_booking>-booking_id = max_booking_id .
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_save DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.
    METHODS save_modified REDEFINITION.

ENDCLASS.


CLASS lcl_save IMPLEMENTATION.

  METHOD save_modified.


********************************************************************************
*
* Implements additional save
*
********************************************************************************

    DATA lt_travel_log   TYPE STANDARD TABLE OF /dmo/log_travel.
    DATA lt_travel_log_c TYPE STANDARD TABLE OF /dmo/log_travel.
    DATA lt_travel_log_u TYPE STANDARD TABLE OF /dmo/log_travel.

    " (1) Get instance data of all instances that have been created
    IF create-travel IS NOT INITIAL.
      " Creates internal table with instance data
      lt_travel_log = CORRESPONDING #( create-travel ).

      LOOP AT lt_travel_log ASSIGNING FIELD-SYMBOL(<fs_travel_log_c>).
        <fs_travel_log_c>-changing_operation = 'CREATE'.

        " Generate time stamp
        GET TIME STAMP FIELD <fs_travel_log_c>-created_at.

        " Read travel instance data into ls_travel that includes %control structure
        READ TABLE create-travel WITH TABLE KEY entity COMPONENTS travel_id = <fs_travel_log_c>-travel_id INTO DATA(ls_travel).
        IF sy-subrc = 0.

          " If new value of the booking_fee field created
          IF ls_travel-%control-booking_fee = cl_abap_behv=>flag_changed.
            " Generate uuid as value of the change_id field
            TRY.
                <fs_travel_log_c>-change_id = cl_system_uuid=>create_uuid_x16_static( ) .
              CATCH cx_uuid_error.
                "handle exception
            ENDTRY.
            <fs_travel_log_c>-changed_field_name = 'booking_fee'.
            <fs_travel_log_c>-changed_value = ls_travel-booking_fee.
            APPEND <fs_travel_log_c> TO lt_travel_log_c.
          ENDIF.

          " If new value of the overall_status field created
          IF ls_travel-%control-overall_status = cl_abap_behv=>flag_changed.
            " Generate uuid as value of the change_id field
            TRY.
                <fs_travel_log_c>-change_id = cl_system_uuid=>create_uuid_x16_static( ) .
              CATCH cx_uuid_error.
                "handle exception
            ENDTRY.
            <fs_travel_log_c>-changed_field_name = 'overall_status'.
            <fs_travel_log_c>-changed_value = ls_travel-overall_status.
            APPEND <fs_travel_log_c> TO lt_travel_log_c.
          ENDIF.

          " IF  ls_travel-%control-...

        ENDIF.

      ENDLOOP.

      " Inserts rows specified in lt_travel_log into the DB table /dmo/log_travel
      INSERT /dmo/log_travel FROM TABLE @lt_travel_log_c.

    ENDIF.


    " (2) Get instance data of all instances that have been updated during the transaction
    IF update-travel IS NOT INITIAL.
      lt_travel_log = CORRESPONDING #( update-travel ).

      LOOP AT update-travel ASSIGNING FIELD-SYMBOL(<fs_travel_log_u>).

        ASSIGN lt_travel_log[ travel_id = <fs_travel_log_u>-travel_id ] TO FIELD-SYMBOL(<fs_travel_db>).

        <fs_travel_db>-changing_operation = 'UPDATE'.

        " Generate time stamp
        GET TIME STAMP FIELD <fs_travel_db>-created_at.


        IF <fs_travel_log_u>-%control-customer_id = if_abap_behv=>mk-on.
          <fs_travel_db>-changed_value = <fs_travel_log_u>-customer_id.
          " Generate uuid as value of the change_id field
          TRY.
              <fs_travel_db>-change_id = cl_system_uuid=>create_uuid_x16_static( ) .
            CATCH cx_uuid_error.
              "handle exception
          ENDTRY.

          <fs_travel_db>-changed_field_name = 'customer_id'.

          APPEND <fs_travel_db> TO lt_travel_log_u.

        ENDIF.

        IF <fs_travel_log_u>-%control-description = if_abap_behv=>mk-on.
          <fs_travel_db>-changed_value = <fs_travel_log_u>-description.

          " Generate uuid as value of the change_id field
          TRY.
              <fs_travel_db>-change_id = cl_system_uuid=>create_uuid_x16_static( ) .
            CATCH cx_uuid_error.
              "handle exception
          ENDTRY.

          <fs_travel_db>-changed_field_name = 'description'.

          APPEND <fs_travel_db> TO lt_travel_log_u.

        ENDIF.

        "IF <fs_travel_log_u>-%control-...

      ENDLOOP.


      " Inserts rows specified in lt_travel_log into the DB table /dmo/log_travel
      INSERT /dmo/log_travel FROM TABLE @lt_travel_log_u.

    ENDIF.

    " (3) Get keys of all travel instances that have been deleted during the transaction
    IF delete-travel IS NOT INITIAL.
      lt_travel_log = CORRESPONDING #( delete-travel ).
      LOOP AT lt_travel_log ASSIGNING FIELD-SYMBOL(<fs_travel_log_d>).
        <fs_travel_log_d>-changing_operation = 'DELETE'.
        " Generate time stamp
        GET TIME STAMP FIELD <fs_travel_log_d>-created_at.
        " Generate uuid as value of the change_id field
        TRY.
            <fs_travel_log_d>-change_id = cl_system_uuid=>create_uuid_x16_static( ) .
          CATCH cx_uuid_error.
            "handle exception
        ENDTRY.

      ENDLOOP.

      " Inserts rows specified in lt_travel_log into the DB table /dmo/log_travel
      INSERT /dmo/log_travel FROM TABLE @lt_travel_log.

    ENDIF.


********************************************************************************
*
* Implements unmanaged save
*
********************************************************************************
    DATA lt_booksuppl_db TYPE STANDARD TABLE OF /dmo/booksuppl_m.

    " (1) Get instance data of all instances that have been created
    IF create-booksuppl IS NOT INITIAL.
      lt_booksuppl_db = CORRESPONDING #( create-booksuppl ).

      CALL FUNCTION '/DMO/FLIGHT_BOOKSUPPL_C' EXPORTING values = lt_booksuppl_db .

    ENDIF.

    " (2) Get instance data of all instances that have been updated during the transaction
    lt_booksuppl_db = CORRESPONDING #( update-booksuppl ).
    IF lt_booksuppl_db IS NOT INITIAL.

      " Read all field values from database
      SELECT * FROM /dmo/booksuppl_m FOR ALL ENTRIES IN @lt_booksuppl_db
               WHERE booking_supplement_id = @lt_booksuppl_db-booking_supplement_id
               INTO TABLE @lt_booksuppl_db .

      " Take over field values that have been changed during the transaction
      LOOP AT update-booksuppl ASSIGNING FIELD-SYMBOL(<ls_unmanaged_booksuppl>).
        ASSIGN lt_booksuppl_db[ travel_id  = <ls_unmanaged_booksuppl>-travel_id
                                booking_id = <ls_unmanaged_booksuppl>-booking_id
                     booking_supplement_id = <ls_unmanaged_booksuppl>-booking_supplement_id
                       ] TO FIELD-SYMBOL(<ls_booksuppl_db>).

        IF <ls_unmanaged_booksuppl>-%control-supplement_id = if_abap_behv=>mk-on.
          <ls_booksuppl_db>-supplement_id = <ls_unmanaged_booksuppl>-supplement_id.
        ENDIF.

        IF <ls_unmanaged_booksuppl>-%control-price = if_abap_behv=>mk-on.
          <ls_booksuppl_db>-price = <ls_unmanaged_booksuppl>-price.
        ENDIF.

        IF <ls_unmanaged_booksuppl>-%control-currency_code = if_abap_behv=>mk-on.
          <ls_booksuppl_db>-currency_code = <ls_unmanaged_booksuppl>-currency_code.
        ENDIF.

      ENDLOOP.

      " Update the complete instance data
      CALL FUNCTION '/DMO/FLIGHT_BOOKSUPPL_U' EXPORTING values = lt_booksuppl_db .

    ENDIF.

    " (3) Get keys of all travel instances that have been deleted during the transaction
    IF delete-booksuppl IS NOT INITIAL.
      lt_booksuppl_db = CORRESPONDING #( delete-booksuppl ).

      CALL FUNCTION '/DMO/FLIGHT_BOOKSUPPL_D' EXPORTING values = lt_booksuppl_db .

    ENDIF.


  ENDMETHOD.

ENDCLASS.
