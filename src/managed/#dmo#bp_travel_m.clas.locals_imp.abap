CLASS ltc_managed DEFINITION DEFERRED FOR TESTING.
CLASS ltc_travl_not_in_documentation DEFINITION DEFERRED FOR TESTING.
CLASS test_using_entity_stub  DEFINITION DEFERRED FOR TESTING.
CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler
  FRIENDS ltc_managed test_using_entity_stub ltc_travl_not_in_documentation.

  PRIVATE SECTION.

    TYPES tt_travel_update TYPE TABLE FOR UPDATE /DMO/I_Travel_M.

    METHODS validate_customer FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validatecustomer.
    METHODS validate_agency FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validateagency.
    METHODS validate_dates  FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validatedates.
    METHODS validate_travel_status FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validatestatus.
    METHODS validate_currencycode FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validatecurrencycode.

    METHODS copyTravel FOR MODIFY
      IMPORTING keys FOR ACTION travel~copyTravel.
    METHODS set_status_accepted FOR MODIFY
      IMPORTING keys FOR ACTION travel~accepttravel RESULT result.
    METHODS set_status_rejected FOR MODIFY
      IMPORTING keys FOR ACTION travel~rejecttravel RESULT result.

    METHODS get_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR travel RESULT result.

    METHODS recalctotalprice FOR MODIFY
      IMPORTING keys FOR ACTION travel~recalctotalprice.
    METHODS calculatetotalprice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR travel~calculatetotalprice.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR travel RESULT result.

    METHODS earlynumbering_cba_booking FOR NUMBERING
      IMPORTING entities FOR CREATE travel\_booking.
    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE travel.


ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

  METHOD validate_customer.
    " Read relevant travel instance data
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
    ENTITY travel
     FIELDS ( customer_id )
     WITH CORRESPONDING #(  keys )
    RESULT DATA(travels).

    DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    customers = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING customer_id = customer_id EXCEPT * ).
    DELETE customers WHERE customer_id IS INITIAL.
    IF customers IS NOT INITIAL.

      " Check if customer ID exists
      SELECT FROM /dmo/customer FIELDS customer_id
        FOR ALL ENTRIES IN @customers
        WHERE customer_id = @customers-customer_id
        INTO TABLE @DATA(customers_db).
    ENDIF.
    " Raise msg for non existing and initial customer id
    LOOP AT travels INTO DATA(travel).
      IF travel-customer_id IS INITIAL
         OR NOT line_exists( customers_db[ customer_id = travel-customer_id ] ).

        APPEND VALUE #(  %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #(  %tky = travel-%tky
                         %msg      = NEW /dmo/cm_flight_messages(
                                         customer_id = travel-customer_id
                                         textid      = /dmo/cm_flight_messages=>customer_unkown
                                         severity    = if_abap_behv_message=>severity-error )
                         %element-customer_id = if_abap_behv=>mk-on
                      ) TO reported-travel.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.



  METHOD validate_agency.
    " Read relevant travel instance data
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
    ENTITY travel
     FIELDS ( agency_id )
     WITH CORRESPONDING #(  keys )
    RESULT DATA(travels).

    DATA agencies TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id.

    " Optimization of DB select: extract distinct non-initial agency IDs
    agencies = CORRESPONDING #(  travels DISCARDING DUPLICATES MAPPING agency_id = agency_id EXCEPT * ).
    DELETE agencies WHERE agency_id IS INITIAL.
    IF  agencies IS NOT INITIAL.

      " check if agency ID exist
      SELECT FROM /dmo/agency FIELDS agency_id
        FOR ALL ENTRIES IN @agencies
        WHERE agency_id = @agencies-agency_id
        INTO TABLE @DATA(agencies_db).
    ENDIF.

    " Raise msg for non existing and initial agency id
    LOOP AT travels INTO DATA(travel).
      IF travel-agency_id IS INITIAL
         OR NOT line_exists( agencies_db[ agency_id = travel-agency_id ] ).

        APPEND VALUE #(  %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #(  %tky = travel-%tky
                         %msg      = NEW /dmo/cm_flight_messages(
                                          textid    = /dmo/cm_flight_messages=>agency_unkown
                                          agency_id = travel-agency_id
                                          severity  = if_abap_behv_message=>severity-error )
                         %element-agency_id = if_abap_behv=>mk-on
                      ) TO reported-travel.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_dates.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
        FIELDS ( begin_date end_date )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    LOOP AT travels INTO DATA(travel).

      IF travel-end_date < travel-begin_date.  "end_date before begin_date

        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky = travel-%tky
                        %msg = NEW /dmo/cm_flight_messages(
                                   textid     = /dmo/cm_flight_messages=>begin_date_bef_end_date
                                   severity   = if_abap_behv_message=>severity-error
                                   begin_date = travel-begin_date
                                   end_date   = travel-end_date
                                   travel_id  = travel-travel_id )
                        %element-begin_date   = if_abap_behv=>mk-on
                        %element-end_date     = if_abap_behv=>mk-on
                     ) TO reported-travel.

      ELSEIF travel-begin_date < cl_abap_context_info=>get_system_date( ).  "begin_date must be in the future

        APPEND VALUE #( %tky        = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky = travel-%tky
                        %msg = NEW /dmo/cm_flight_messages(
                                    textid   = /dmo/cm_flight_messages=>begin_date_on_or_bef_sysdate
                                    severity = if_abap_behv_message=>severity-error )
                        %element-begin_date  = if_abap_behv=>mk-on
                        %element-end_date    = if_abap_behv=>mk-on
                      ) TO reported-travel.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD validate_travel_status.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
        ENTITY travel
          FIELDS ( overall_status )
          WITH CORRESPONDING #( keys )
        RESULT DATA(travels).

    LOOP AT travels INTO DATA(travel).
      CASE travel-overall_status.
        WHEN 'O'.  " Open
        WHEN 'X'.  " Cancelled
        WHEN 'A'.  " Accepted

        WHEN OTHERS.
          APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

          APPEND VALUE #( %tky = travel-%tky
                          %msg = NEW /dmo/cm_flight_messages(
                                     textid = /dmo/cm_flight_messages=>status_invalid
                                     severity = if_abap_behv_message=>severity-error
                                     status = travel-overall_status )
                          %element-overall_status = if_abap_behv=>mk-on
                        ) TO reported-travel.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD copyTravel.

    DATA:
      travels       TYPE TABLE FOR CREATE /DMO/I_Travel_M\\travel,
      bookings_cba  TYPE TABLE FOR CREATE /DMO/I_Travel_M\\travel\_booking,
      booksuppl_cba TYPE TABLE FOR CREATE /DMO/I_Travel_M\\booking\_booksupplement.

    READ TABLE keys WITH KEY %cid = '' INTO DATA(key_with_inital_cid).
    ASSERT key_with_inital_cid IS INITIAL.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
       ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(travel_read_result)
    FAILED failed.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel BY \_booking
       ALL FIELDS WITH CORRESPONDING #( travel_read_result )
     RESULT DATA(book_read_result).

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY booking BY \_booksupplement
       ALL FIELDS WITH CORRESPONDING #( book_read_result )
    RESULT DATA(booksuppl_read_result).

    LOOP AT travel_read_result ASSIGNING FIELD-SYMBOL(<travel>).
      "Fill travel container for creating new travel instance
      APPEND VALUE #( %cid     = keys[ KEY entity %tky = <travel>-%tky ]-%cid
                      %data    = CORRESPONDING #( <travel> EXCEPT travel_id ) )
        TO travels ASSIGNING FIELD-SYMBOL(<new_travel>).

      "Fill %cid_ref of travel as instance identifier for cba booking
      APPEND VALUE #( %cid_ref = keys[ KEY entity %tky = <travel>-%tky ]-%cid )
        TO bookings_cba ASSIGNING FIELD-SYMBOL(<bookings_cba>).

      <new_travel>-begin_date     = cl_abap_context_info=>get_system_date( ).
      <new_travel>-end_date       = cl_abap_context_info=>get_system_date( ) + 30.
      <new_travel>-overall_status = 'O'.  "Set to open to allow an editable instance

      LOOP AT book_read_result ASSIGNING FIELD-SYMBOL(<booking>) USING KEY entity WHERE travel_id EQ <travel>-travel_id.
        "Fill booking container for creating booking with cba
        APPEND VALUE #( %cid     = keys[ KEY entity %tky = <travel>-%tky ]-%cid && <booking>-booking_id
                        %data    = CORRESPONDING #(  book_read_result[ KEY entity %tky = <booking>-%tky ] EXCEPT travel_id ) )
          TO <bookings_cba>-%target ASSIGNING FIELD-SYMBOL(<new_booking>).

        "Fill %cid_ref of booking as instance identifier for cba booksuppl
        APPEND VALUE #( %cid_ref = keys[ KEY entity %tky = <travel>-%tky ]-%cid && <booking>-booking_id )
          TO booksuppl_cba ASSIGNING FIELD-SYMBOL(<booksuppl_cba>).

        <new_booking>-booking_status = 'N'.

        LOOP AT booksuppl_read_result ASSIGNING FIELD-SYMBOL(<booksuppl>) USING KEY entity WHERE travel_id  EQ <travel>-travel_id
                                                                                           AND   booking_id EQ <booking>-booking_id.
          "Fill booksuppl container for creating supplement with cba
          APPEND VALUE #( %cid  = keys[ KEY entity %tky = <travel>-%tky ]-%cid  && <booking>-booking_id && <booksuppl>-booking_supplement_id
                          %data = CORRESPONDING #( <booksuppl> EXCEPT travel_id booking_id ) )
            TO <booksuppl_cba>-%target.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    "create new BO instance
    MODIFY ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
        CREATE FIELDS ( agency_id customer_id begin_date end_date booking_fee total_price currency_code overall_status description )
          WITH travels
        CREATE BY \_Booking FIELDS ( booking_id booking_date customer_id carrier_id connection_id flight_date flight_price currency_code booking_status )
          WITH bookings_cba
      ENTITY booking
        CREATE BY \_BookSupplement FIELDS ( booking_supplement_id supplement_id price currency_code )
          WITH booksuppl_cba
      MAPPED DATA(mapped_create).

    mapped-travel   =  mapped_create-travel .

  ENDMETHOD.


  METHOD set_status_accepted.

    " Modify in local mode: BO-related updates that are not relevant for authorization checks
    MODIFY ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
           ENTITY travel
              UPDATE FIELDS ( overall_status )
                 WITH VALUE #( FOR key IN keys ( %tky      = key-%tky
                                                 overall_status = 'A' ) ). " Accepted

    " Read changed data for action result
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
         ALL FIELDS WITH
         CORRESPONDING #( keys )
       RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels ( %tky      = travel-%tky
                                              %param    = travel ) ).

  ENDMETHOD.

  METHOD set_status_rejected.

    MODIFY ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
           ENTITY travel
              UPDATE FIELDS ( overall_status )
                 WITH VALUE #( FOR key IN keys ( %tky      = key-%tky
                                                 overall_status = 'X' ) ). " Rejected


    " read changed data for result
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
         ALL FIELDS WITH
         CORRESPONDING #( keys )
       RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels ( %tky      = travel-%tky
                                              %param    = travel ) ).

  ENDMETHOD.

  METHOD get_features.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
         FIELDS (  travel_id overall_status )
         WITH CORRESPONDING #( keys )
       RESULT DATA(travels)
       FAILED failed.


    result = VALUE #( FOR travel IN travels
                       ( %tky                           = travel-%tky
                         %features-%action-rejecttravel = COND #( WHEN travel-overall_status = 'X'
                                                                  THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                         %features-%action-accepttravel = COND #( WHEN travel-overall_status = 'A'
                                                                  THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
                         %assoc-_booking                = COND #( WHEN travel-overall_status = 'X'
                                                                  THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
                      ) ).

  ENDMETHOD.


  METHOD recalctotalprice.
    TYPES: BEGIN OF ty_amount_per_currencycode,
             amount        TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
           END OF ty_amount_per_currencycode.

    DATA: amounts_per_currencycode TYPE STANDARD TABLE OF ty_amount_per_currencycode.

    " Read all relevant travel instances.
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
         ENTITY travel
            FIELDS ( booking_fee currency_code )
            WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    DELETE travels WHERE currency_code IS INITIAL.

    " Read all associated bookings and add them to the total price.
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel BY \_booking
        FIELDS ( flight_price currency_code )
      WITH CORRESPONDING #( travels )
      RESULT DATA(bookings).

    " Read all associated booking supplements and add them to the total price.
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY booking BY \_booksupplement
        FIELDS (  price currency_code )
      WITH CORRESPONDING #( bookings )
      RESULT DATA(bookingsupplements).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).
      " Set the start for the calculation by adding the booking fee.
      amounts_per_currencycode = VALUE #( ( amount        = <travel>-booking_fee
                                           currency_code = <travel>-currency_code ) ).


      LOOP AT bookings INTO DATA(booking) USING KEY id WHERE   travel_id = <travel>-travel_id
                                                       AND     currency_code IS NOT INITIAL.
        COLLECT VALUE ty_amount_per_currencycode( amount        = booking-flight_price
                                                  currency_code = booking-currency_code
                                                ) INTO amounts_per_currencycode.
      ENDLOOP.


      LOOP AT bookingsupplements INTO DATA(bookingsupplement) USING KEY id WHERE   travel_id = <travel>-travel_id
                                                                           AND     currency_code IS NOT INITIAL.
        COLLECT VALUE ty_amount_per_currencycode( amount        = bookingsupplement-price
                                                  currency_code = bookingsupplement-currency_code
                                                ) INTO amounts_per_currencycode.
      ENDLOOP.

      DELETE amounts_per_currencycode WHERE currency_code IS INITIAL.

      CLEAR <travel>-total_price.
      LOOP AT amounts_per_currencycode INTO DATA(amount_per_currencycode).
        " If needed do a Currency Conversion
        IF amount_per_currencycode-currency_code = <travel>-currency_code.
          <travel>-total_price += amount_per_currencycode-amount.
        ELSE.
          /dmo/cl_flight_amdp=>convert_currency(
             EXPORTING
               iv_amount                   =  amount_per_currencycode-amount
               iv_currency_code_source     =  amount_per_currencycode-currency_code
               iv_currency_code_target     =  <travel>-currency_code
               iv_exchange_rate_date       =  cl_abap_context_info=>get_system_date( )
             IMPORTING
               ev_amount                   = DATA(total_booking_price_per_curr)
            ).
          <travel>-total_price += total_booking_price_per_curr.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " write back the modified total_price of travels
    MODIFY ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
        UPDATE FIELDS ( total_price )
        WITH CORRESPONDING #( travels ).

  ENDMETHOD.

  METHOD calculatetotalprice.

    MODIFY ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
        EXECUTE recalctotalprice
        FROM CORRESPONDING #( keys ).

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
          APPEND VALUE #(  %cid = entity-%cid
                           %key = entity-%key
                           %msg = lx_number_ranges
                        ) TO reported-travel.
          APPEND VALUE #(  %cid = entity-%cid
                           %key = entity-%key
                        ) TO failed-travel.
        ENDLOOP.
        EXIT.
    ENDTRY.

    CASE number_range_return_code.
      WHEN '1'.
        " 1 - the returned number is in a critical range (specified under “percentage warning” in the object definition)
        LOOP AT entities_wo_travelid INTO entity.
          APPEND VALUE #( %cid = entity-%cid
                          %key = entity-%key
                          %msg = NEW /dmo/cm_flight_messages(
                                      textid = /dmo/cm_flight_messages=>number_range_depleted
                                      severity = if_abap_behv_message=>severity-warning )
                        ) TO reported-travel.
        ENDLOOP.

      WHEN '2' OR '3'.
        " 2 - the last number of the interval was returned
        " 3 - if fewer numbers are available than requested,  the return code is 3
        LOOP AT entities_wo_travelid INTO entity.
          APPEND VALUE #( %cid = entity-%cid
                          %key = entity-%key
                          %msg = NEW /dmo/cm_flight_messages(
                                      textid = /dmo/cm_flight_messages=>not_sufficient_numbers
                                      severity = if_abap_behv_message=>severity-warning )
                        ) TO reported-travel.
          APPEND VALUE #( %cid        = entity-%cid
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

      APPEND VALUE #( %cid  = entity-%cid
                      %key  = entity-%key
                    ) TO mapped-travel.
    ENDLOOP.

  ENDMETHOD.

  METHOD earlynumbering_cba_booking.
    DATA: max_booking_id TYPE /dmo/booking_id.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel BY \_booking
        FROM CORRESPONDING #( entities )
        LINK DATA(bookings).

    " Loop over all unique TravelIDs
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<travel_group>) GROUP BY <travel_group>-travel_id.

      " Get highest booking_id from bookings belonging to travel
      max_booking_id = REDUCE #( INIT max = CONV /dmo/booking_id( '0' )
                                 FOR  booking IN bookings USING KEY entity WHERE ( source-travel_id  = <travel_group>-travel_id )
                                 NEXT max = COND /dmo/booking_id( WHEN booking-target-booking_id > max
                                                                    THEN booking-target-booking_id
                                                                    ELSE max )
                               ).
      " Get highest assigned booking_id from incoming entities
      max_booking_id = REDUCE #( INIT max = max_booking_id
                                 FOR  entity IN entities USING KEY entity WHERE ( travel_id  = <travel_group>-travel_id )
                                 FOR  target IN entity-%target
                                 NEXT max = COND /dmo/booking_id( WHEN   target-booking_id > max
                                                                    THEN target-booking_id
                                                                    ELSE max )
                               ).

      " Loop over all entries in entities with the same TravelID
      LOOP AT entities ASSIGNING FIELD-SYMBOL(<travel>) USING KEY entity WHERE travel_id = <travel_group>-travel_id.

        " Assign new booking-ids if not already assigned
        LOOP AT <travel>-%target ASSIGNING FIELD-SYMBOL(<booking_wo_numbers>).
          APPEND CORRESPONDING #( <booking_wo_numbers> ) TO mapped-booking ASSIGNING FIELD-SYMBOL(<mapped_booking>).
          IF <booking_wo_numbers>-booking_id IS INITIAL.
            max_booking_id += 10 .
            <mapped_booking>-booking_id = max_booking_id .
          ENDIF.
        ENDLOOP.

      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD validate_currencycode.
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel
        FIELDS ( currency_code )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    DATA: currencies TYPE SORTED TABLE OF I_Currency WITH UNIQUE KEY currency.

    currencies = CORRESPONDING #(  travels DISCARDING DUPLICATES MAPPING currency = currency_code EXCEPT * ).
    DELETE currencies WHERE currency IS INITIAL.

    IF currencies IS NOT INITIAL.
      SELECT FROM I_Currency FIELDS currency
        FOR ALL ENTRIES IN @currencies
        WHERE currency = @currencies-currency
        INTO TABLE @DATA(currency_db).
    ENDIF.


    LOOP AT travels INTO DATA(travel).
      IF travel-currency_code IS INITIAL.
        " Raise message for empty Currency
        APPEND VALUE #( %tky                   = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky                   = travel-%tky
                        %msg                   = NEW /dmo/cm_flight_messages(
                                                        textid    = /dmo/cm_flight_messages=>currency_required
                                                        severity  = if_abap_behv_message=>severity-error )
                        %element-currency_code = if_abap_behv=>mk-on
                      ) TO reported-travel.
      ELSEIF NOT line_exists( currency_db[ currency = travel-currency_code ] ).
        " Raise message for not existing Currency
        APPEND VALUE #( %tky                   = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky                   = travel-%tky
                        %msg                   = NEW /dmo/cm_flight_messages(
                                                        textid        = /dmo/cm_flight_messages=>currency_not_existing
                                                        severity      = if_abap_behv_message=>severity-error
                                                        currency_code = travel-currency_code )
                        %element-currency_code = if_abap_behv=>mk-on
                      ) TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.



CLASS ltc_save_not_in_documentation DEFINITION DEFERRED FOR TESTING.
CLASS lcl_save DEFINITION INHERITING FROM cl_abap_behavior_saver
  FRIENDS ltc_save_not_in_documentation.

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

    DATA travel_log        TYPE STANDARD TABLE OF /dmo/log_travel.
    DATA travel_log_create TYPE STANDARD TABLE OF /dmo/log_travel.
    DATA travel_log_update TYPE STANDARD TABLE OF /dmo/log_travel.

    " (1) Get instance data of all instances that have been created
    IF create-travel IS NOT INITIAL.
      " Creates internal table with instance data
      travel_log = CORRESPONDING #( create-travel ).

      LOOP AT travel_log ASSIGNING FIELD-SYMBOL(<travel_log>).
        <travel_log>-changing_operation = 'CREATE'.

        " Generate time stamp
        GET TIME STAMP FIELD <travel_log>-created_at.

        " Read travel instance data into ls_travel that includes %control structure
        READ TABLE create-travel WITH TABLE KEY entity COMPONENTS travel_id = <travel_log>-travel_id INTO DATA(travel).
        IF sy-subrc = 0.

          " If new value of the booking_fee field created
          IF travel-%control-booking_fee = cl_abap_behv=>flag_changed.
            " Generate uuid as value of the change_id field
            TRY.
                <travel_log>-change_id = cl_system_uuid=>create_uuid_x16_static( ) .
              CATCH cx_uuid_error.
                "handle exception
            ENDTRY.
            <travel_log>-changed_field_name = 'booking_fee'.
            <travel_log>-changed_value = travel-booking_fee.
            APPEND <travel_log> TO travel_log_create.
          ENDIF.

          " If new value of the overall_status field created
          IF travel-%control-overall_status = cl_abap_behv=>flag_changed.
            " Generate uuid as value of the change_id field
            TRY.
                <travel_log>-change_id = cl_system_uuid=>create_uuid_x16_static( ) .
              CATCH cx_uuid_error.
                "handle exception
            ENDTRY.
            <travel_log>-changed_field_name = 'overall_status'.
            <travel_log>-changed_value = travel-overall_status.
            APPEND <travel_log> TO travel_log_create.
          ENDIF.

          " IF  ls_travel-%control-...

        ENDIF.

      ENDLOOP.

      " Inserts rows specified in lt_travel_log_c into the DB table /dmo/log_travel
      INSERT /dmo/log_travel FROM TABLE @travel_log_create.

    ENDIF.


    " (2) Get instance data of all instances that have been updated during the transaction
    IF update-travel IS NOT INITIAL.
      travel_log = CORRESPONDING #( update-travel ).

      LOOP AT update-travel ASSIGNING FIELD-SYMBOL(<travel_log_update>).

        ASSIGN travel_log[ travel_id = <travel_log_update>-travel_id ] TO FIELD-SYMBOL(<travel_log_db>).

        <travel_log_db>-changing_operation = 'UPDATE'.

        " Generate time stamp
        GET TIME STAMP FIELD <travel_log_db>-created_at.


        IF <travel_log_update>-%control-customer_id = if_abap_behv=>mk-on.
          <travel_log_db>-changed_value = <travel_log_update>-customer_id.
          " Generate uuid as value of the change_id field
          TRY.
              <travel_log_db>-change_id = cl_system_uuid=>create_uuid_x16_static( ) .
            CATCH cx_uuid_error.
              "handle exception
          ENDTRY.

          <travel_log_db>-changed_field_name = 'customer_id'.

          APPEND <travel_log_db> TO travel_log_update.

        ENDIF.

        IF <travel_log_update>-%control-description = if_abap_behv=>mk-on.
          <travel_log_db>-changed_value = <travel_log_update>-description.

          " Generate uuid as value of the change_id field
          TRY.
              <travel_log_db>-change_id = cl_system_uuid=>create_uuid_x16_static( ) .
            CATCH cx_uuid_error.
              "handle exception
          ENDTRY.

          <travel_log_db>-changed_field_name = 'description'.

          APPEND <travel_log_db> TO travel_log_update.

        ENDIF.

        "IF <fs_travel_log_u>-%control-...

      ENDLOOP.


      " Inserts rows specified in lt_travel_log_u into the DB table /dmo/log_travel
      INSERT /dmo/log_travel FROM TABLE @travel_log_update.

    ENDIF.

    " (3) Get keys of all travel instances that have been deleted during the transaction
    IF delete-travel IS NOT INITIAL.
      travel_log = CORRESPONDING #( delete-travel ).
      LOOP AT travel_log ASSIGNING FIELD-SYMBOL(<travel_log_delete>).
        <travel_log_delete>-changing_operation = 'DELETE'.
        " Generate time stamp
        GET TIME STAMP FIELD <travel_log_delete>-created_at.
        " Generate uuid as value of the change_id field
        TRY.
            <travel_log_delete>-change_id = cl_system_uuid=>create_uuid_x16_static( ) .
          CATCH cx_uuid_error.
            "handle exception
        ENDTRY.

      ENDLOOP.

      " Inserts rows specified in lt_travel_log into the DB table /dmo/log_travel
      INSERT /dmo/log_travel FROM TABLE @travel_log.

    ENDIF.


********************************************************************************
*
* Implements unmanaged save
*
********************************************************************************
    DATA booksuppls_db TYPE STANDARD TABLE OF /dmo/booksuppl_m.

    " (1) Get instance data of all instances that have been created
    IF create-booksuppl IS NOT INITIAL.
      booksuppls_db = CORRESPONDING #( create-booksuppl MAPPING FROM ENTITY ).

      CALL FUNCTION '/DMO/FLIGHT_BOOKSUPPL_C' EXPORTING values = booksuppls_db.

    ENDIF.

    " (2) Get instance data of all instances that have been updated during the transaction
    booksuppls_db = CORRESPONDING #( update-booksuppl MAPPING FROM ENTITY ).
    IF booksuppls_db IS NOT INITIAL.

      " Read all field values from database
      SELECT * FROM /dmo/booksuppl_m FOR ALL ENTRIES IN @booksuppls_db
               WHERE booking_supplement_id = @booksuppls_db-booking_supplement_id
               INTO TABLE @booksuppls_db .

      " Take over field values that have been changed during the transaction
      LOOP AT update-booksuppl ASSIGNING FIELD-SYMBOL(<unmanaged_booksuppl>).
        ASSIGN booksuppls_db[ travel_id  = <unmanaged_booksuppl>-travel_id
                              booking_id = <unmanaged_booksuppl>-booking_id
                   booking_supplement_id = <unmanaged_booksuppl>-booking_supplement_id
                            ] TO FIELD-SYMBOL(<booksuppl_db>).

        IF <unmanaged_booksuppl>-%control-supplement_id = if_abap_behv=>mk-on.
          <booksuppl_db>-supplement_id = <unmanaged_booksuppl>-supplement_id.
        ENDIF.

        IF <unmanaged_booksuppl>-%control-price = if_abap_behv=>mk-on.
          <booksuppl_db>-price = <unmanaged_booksuppl>-price.
        ENDIF.

        IF <unmanaged_booksuppl>-%control-currency_code = if_abap_behv=>mk-on.
          <booksuppl_db>-currency_code = <unmanaged_booksuppl>-currency_code.
        ENDIF.

      ENDLOOP.

      " Update the complete instance data
      CALL FUNCTION '/DMO/FLIGHT_BOOKSUPPL_U' EXPORTING values = booksuppls_db.

    ENDIF.

    " (3) Get keys of all travel instances that have been deleted during the transaction
    IF delete-booksuppl IS NOT INITIAL.
      booksuppls_db = CORRESPONDING #( delete-booksuppl MAPPING FROM ENTITY ).

      CALL FUNCTION '/DMO/FLIGHT_BOOKSUPPL_D' EXPORTING values = booksuppls_db.

    ENDIF.


  ENDMETHOD.

ENDCLASS.
