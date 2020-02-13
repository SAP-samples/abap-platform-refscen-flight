CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    TYPES tt_travel_update TYPE TABLE FOR UPDATE /DMO/I_Travel_M.

    METHODS validate_customer          FOR VALIDATION travel~validateCustomer IMPORTING keys FOR travel.
    METHODS validate_agency            FOR VALIDATION travel~validateAgency   IMPORTING keys FOR travel.
    METHODS validate_dates             FOR VALIDATION travel~validateDates    IMPORTING keys FOR travel.
    METHODS validate_travel_status     FOR VALIDATION travel~validateStatus   IMPORTING keys FOR travel.

    METHODS copy_travel                FOR MODIFY IMPORTING   keys FOR ACTION travel~createTravelByTemplate    RESULT result.
    METHODS set_status_completed       FOR MODIFY IMPORTING   keys FOR ACTION travel~acceptTravel              RESULT result.
    METHODS set_status_cancelled       FOR MODIFY IMPORTING   keys FOR ACTION travel~rejectTravel              RESULT result.
    METHODS get_features               FOR FEATURES IMPORTING keys REQUEST    requested_features FOR travel    RESULT result.

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

        APPEND VALUE #(  travel_id = ls_travel-travel_id ) TO failed.
        APPEND VALUE #(  travel_id = ls_travel-travel_id
                         %msg = new_message( id        = '/DMO/CM_FLIGHT_LEGAC'
                                             number    = '002'
                                             v1        = ls_travel-customer_id
                                             severity  = if_abap_behv_message=>severity-error )
                         %element-customer_id = if_abap_behv=>mk-on )
          TO reported.
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
        APPEND VALUE #(  travel_id = ls_travel-travel_id ) TO failed.
        APPEND VALUE #(  travel_id = ls_travel-travel_id
                         %msg = new_message( id        = '/DMO/CM_FLIGHT_LEGAC'
                                             number    = '002'
                                             v1        = ls_travel-agency_id
                                             severity  = if_abap_behv_message=>severity-error )
                         %element-agency_id = if_abap_behv=>mk-on )
          TO reported.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


**********************************************************************
* Check validity of date
*
**********************************************************************
  METHOD validate_dates.

    READ ENTITY /DMO/I_Travel_M\\travel FIELDS ( begin_date end_date )  WITH
        VALUE #( FOR <root_key> IN keys ( %key = <root_key> ) )
        RESULT DATA(lt_travel_result).

    LOOP AT lt_travel_result INTO DATA(ls_travel_result).

      IF ls_travel_result-end_date < ls_travel_result-begin_date.  "end_date before begin_date

        APPEND VALUE #( %key        = ls_travel_result-%key
                        travel_id   = ls_travel_result-travel_id ) TO failed.

        APPEND VALUE #( %key     = ls_travel_result-%key
                        %msg     = new_message( id       = /dmo/cx_flight_legacy=>end_date_before_begin_date-msgid
                                                number   = /dmo/cx_flight_legacy=>end_date_before_begin_date-msgno
                                                v1       = ls_travel_result-begin_date
                                                v2       = ls_travel_result-end_date
                                                v3       = ls_travel_result-travel_id
                                                severity = if_abap_behv_message=>severity-error )
                        %element-begin_date = if_abap_behv=>mk-on
                        %element-end_date   = if_abap_behv=>mk-on ) TO reported.

      ELSEIF ls_travel_result-begin_date < cl_abap_context_info=>get_system_date( ).  "begin_date must be in the future

        APPEND VALUE #( %key        = ls_travel_result-%key
                        travel_id   = ls_travel_result-travel_id ) TO failed.

        APPEND VALUE #( %key = ls_travel_result-%key
                        %msg = new_message( id       = /dmo/cx_flight_legacy=>begin_date_before_system_date-msgid
                                            number   = /dmo/cx_flight_legacy=>begin_date_before_system_date-msgno
                                            severity = if_abap_behv_message=>severity-error )
                        %element-begin_date = if_abap_behv=>mk-on
                        %element-end_date   = if_abap_behv=>mk-on ) TO reported.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

**********************************************************************
*
* Validate travel status when saving travel data
*
**********************************************************************
  METHOD validate_travel_status.

    READ ENTITY /DMO/I_Travel_M\\travel FIELDS ( overall_status ) WITH
        VALUE #( FOR <root_key> IN keys ( %key = <root_key> ) )
        RESULT DATA(lt_travel_result).

    LOOP AT lt_travel_result INTO DATA(ls_travel_result).
      CASE ls_travel_result-overall_status.
        WHEN 'O'.  " Open
        WHEN 'X'.  " Cancelled
        WHEN 'A'.  " Accepted

        WHEN OTHERS.
          APPEND VALUE #( %key = ls_travel_result-%key ) TO failed.

          APPEND VALUE #( %key = ls_travel_result-%key
                          %msg = new_message( id       = /dmo/cx_flight_legacy=>status_is_not_valid-msgid
                                              number   = /dmo/cx_flight_legacy=>status_is_not_valid-msgno
                                              v1       = ls_travel_result-overall_status
                                              severity = if_abap_behv_message=>severity-error )
                          %element-overall_status = if_abap_behv=>mk-on ) TO reported.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

**********************************************************************
*
* Create travel instances with initial values
*
**********************************************************************
  METHOD copy_travel.

    SELECT MAX( travel_id ) FROM /dmo/travel_m INTO @DATA(lv_travel_id).

    READ ENTITY /dmo/i_travel_m
         FIELDS ( travel_id
                  agency_id
                  customer_id
                  booking_fee
                  total_price
                  currency_code )
           WITH VALUE #( FOR travel IN keys (  %key = travel-%key ) )
         RESULT    DATA(lt_read_result)
         FAILED    failed
         REPORTED  reported.

    DATA(lv_today) = cl_abap_context_info=>get_system_date( ).

    DATA lt_create TYPE TABLE FOR CREATE /DMO/I_Travel_M\\travel.

    lt_create = VALUE #( FOR row IN  lt_read_result INDEX INTO idx
                             ( travel_id      = lv_travel_id + idx
                               agency_id      = row-agency_id
                               customer_id    = row-customer_id
                               begin_date     = lv_today
                               end_date       = lv_today + 30
                               booking_fee    = row-booking_fee
                               total_price    = row-total_price
                               currency_code  = row-currency_code
                               description    = 'Enter your comments here'
                               overall_status = 'O' ) ). " Open


    MODIFY ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
        ENTITY travel
           CREATE FIELDS (    travel_id
                              agency_id
                              customer_id
                              begin_date
                              end_date
                              booking_fee
                              total_price
                              currency_code
                              description
                              overall_status )
           WITH lt_create
         MAPPED   mapped
         FAILED   failed
         REPORTED reported.


    result = VALUE #( FOR create IN  lt_create INDEX INTO idx
                             ( %cid_ref = keys[ idx ]-%cid_ref
                               %key     = keys[ idx ]-travel_id
                               %param   = CORRESPONDING #(  create ) ) ) .





  ENDMETHOD.


********************************************************************************
*
* Implements travel action (in our case: for setting travel overall_status to completed)
*
********************************************************************************
  METHOD set_status_completed.

    " Modify in local mode: BO-related updates that are not relevant for authorization checks
    MODIFY ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
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
         RESULT DATA(lt_travel).

    result = VALUE #( FOR travel IN lt_travel ( travel_id = travel-travel_id
                                                %param    = travel ) ).

  ENDMETHOD.

********************************************************************************
*
* Implements travel action(s) (in our case: for setting travel overall_status to cancelled)
*
********************************************************************************
  METHOD set_status_cancelled.

    MODIFY ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
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
     RESULT DATA(lt_travel).

    result = VALUE #( FOR travel IN lt_travel ( travel_id = travel-travel_id
                                                %param    = travel
                                              ) ).

  ENDMETHOD.

********************************************************************************
*
* Implements the dynamic feature handling for travel instances
*
********************************************************************************
  METHOD get_features.

    READ ENTITY /dmo/i_travel_m
         FIELDS (  travel_id overall_status )
           WITH VALUE #( FOR keyval IN keys (  %key = keyval-%key ) )
         RESULT DATA(lt_travel_result).


    result = VALUE #( FOR ls_travel IN lt_travel_result
                       ( %key                           = ls_travel-%key
                         %field-travel_id               = if_abap_behv=>fc-f-read_only
                         %features-%action-rejectTravel = COND #( WHEN ls_travel-overall_status = 'X'
                                                                    THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                         %features-%action-acceptTravel = COND #( WHEN ls_travel-overall_status = 'A'
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

          " If new value of the overal_status field created
          IF ls_travel-%control-overall_status = cl_abap_behv=>flag_changed.
            " Generate uuid as value of the change_id field
            TRY.
                <fs_travel_log_c>-change_id = cl_system_uuid=>create_uuid_x16_static( ) .
              CATCH cx_uuid_error.
                "handle exception
            ENDTRY.
            <fs_travel_log_c>-changed_field_name = 'overal_status'.
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
    IF update-booksuppl IS NOT INITIAL.
      lt_booksuppl_db = CORRESPONDING #( update-booksuppl ).

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
