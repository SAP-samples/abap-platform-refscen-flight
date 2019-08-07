CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    TYPES tt_travel_update TYPE TABLE FOR UPDATE /DMO/I_Travel_M.

    METHODS validate_customer          FOR VALIDATION travel~validateCustomer IMPORTING keys FOR travel.
    METHODS validate_dates             FOR VALIDATION travel~validateDates    IMPORTING keys FOR travel.
    METHODS validate_travel_status     FOR VALIDATION travel~validateStatus   IMPORTING keys FOR travel.

    METHODS copy_travel                FOR MODIFY IMPORTING   keys FOR ACTION travel~createTravelByTemplate    RESULT result.
    METHODS set_status_completed       FOR MODIFY IMPORTING   keys FOR ACTION travel~acceptTravel              RESULT result.
    METHODS set_status_cancelled       FOR MODIFY IMPORTING   keys FOR ACTION travel~rejectTravel              RESULT result.
    METHODS get_features               FOR FEATURES IMPORTING keys REQUEST    requested_features FOR travel    RESULT result.

*    METHODS check_authority_for_travel FOR AUTHORIZATION IMPORTING it_travel_key REQUEST is_request FOR travel RESULT result.

    " Workaround
    METHODS create_booking             FOR MODIFY IMPORTING keys   FOR ACTION travel~createBooking             RESULT result.

ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

**********************************************************************
*
* Validate customer data when saving travel data
*
**********************************************************************
  METHOD validate_customer.

  READ ENTITY /DMO/I_Travel_M\\travel FROM VALUE #(
        FOR <root_key> IN keys ( %key     = <root_key>
                                 %control = VALUE #( customer_id = if_abap_behv=>mk-on ) ) )
        RESULT DATA(lt_travel).

    DATA lt_customer TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    lt_customer = CORRESPONDING #( lt_travel DISCARDING DUPLICATES MAPPING customer_id = customer_id EXCEPT * ).
    DELETE lt_customer WHERE customer_id IS INITIAL.
    CHECK lt_customer IS NOT INITIAL.

    " Check if customer ID exist
    SELECT FROM /dmo/customer FIELDS customer_id
      FOR ALL ENTRIES IN @lt_customer
      WHERE customer_id = @lt_customer-customer_id
      INTO TABLE @DATA(lt_customer_db).

    " Raise msg for non existing customer id
    LOOP AT lt_travel INTO DATA(ls_travel).
      IF ls_travel-customer_id IS NOT INITIAL AND NOT line_exists( lt_customer_db[ customer_id = ls_travel-customer_id ] ).
        APPEND VALUE #(  travel_id = ls_travel-travel_id ) TO failed.
        APPEND VALUE #(  travel_id = ls_travel-travel_id
                         %msg      = new_message( id       = '/DMO/CM_FLIGHT_LEGAC'
                                                  number   = '002'
                                                  v1       = ls_travel-customer_id
                                                  severity = if_abap_behv_message=>severity-error )
                         %element-customer_id = if_abap_behv=>mk-on ) TO reported.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


**********************************************************************
*
* Check validity of date
*
**********************************************************************
  METHOD validate_dates.

    READ ENTITY /DMO/I_Travel_M\\travel FROM VALUE #(
        FOR <root_key> IN keys ( %key     = <root_key>
                                 %control = VALUE #( begin_date = if_abap_behv=>mk-on
                                                     end_date   = if_abap_behv=>mk-on ) ) )
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

   READ ENTITY /DMO/I_Travel_M\\travel FROM VALUE #(
      FOR <root_key> IN keys ( %key     = <root_key>
                               %control = VALUE #( overall_status = if_abap_behv=>mk-on ) ) )
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

    READ ENTITY /dmo/i_travel_m FROM VALUE #( FOR travel IN keys
                                                    (  %key                                = travel-%key
                                                       %control = VALUE #( travel_id       = if_abap_behv=>mk-on
                                                                           agency_id       = if_abap_behv=>mk-on
                                                                           customer_id     = if_abap_behv=>mk-on
                                                                           booking_fee     = if_abap_behv=>mk-on
                                                                           total_price     = if_abap_behv=>mk-on
                                                                           currency_code   = if_abap_behv=>mk-on
                                                                         ) ) )
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
                               overall_status = 'O' " Open
                               %control       = VALUE #( travel_id      = if_abap_behv=>mk-on
                                                         agency_id      = if_abap_behv=>mk-on
                                                         customer_id    = if_abap_behv=>mk-on
                                                         begin_date     = if_abap_behv=>mk-on
                                                         end_date       = if_abap_behv=>mk-on
                                                         booking_fee    = if_abap_behv=>mk-on
                                                         total_price    = if_abap_behv=>mk-on
                                                         currency_code  = if_abap_behv=>mk-on
                                                         description    = if_abap_behv=>mk-on
                                                         overall_status = if_abap_behv=>mk-on ) ) ) .

*    MODIFY ENTITY /DMO/I_Travel_M\\travel
     MODIFY ENTITIES OF /DMO/I_TRAVEL_M IN LOCAL MODE
           ENTITY travel
        CREATE FROM lt_create
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
    MODIFY ENTITIES OF /DMO/I_TRAVEL_M IN LOCAL MODE
           ENTITY travel
              UPDATE FROM VALUE #( for key in keys ( travel_id = key-travel_id
                                                     overall_status = 'A' " Accepted
                                                     %control-overall_status = if_abap_behv=>mk-on ) )
           FAILED   failed
           REPORTED reported.

  ENDMETHOD.

********************************************************************************
*
* Implements travel action(s) (in our case: for setting travel overall_status to cancelled)
*
********************************************************************************
  METHOD set_status_cancelled.

    MODIFY ENTITIES OF /DMO/I_TRAVEL_M IN LOCAL MODE
           ENTITY travel
              UPDATE FROM VALUE #( for key in keys ( travel_id = key-travel_id
                                                     overall_status = 'X'   " Canceled
                                                     %control-overall_status = if_abap_behv=>mk-on ) )
           FAILED   failed
           REPORTED reported.

  ENDMETHOD.

********************************************************************************
*
* Implements the dynamic feature handling for travel instances
*
********************************************************************************
  METHOD get_features.

    READ ENTITY /dmo/i_travel_m FROM VALUE #( FOR keyval IN keys
                                                      (  %key                    = keyval-%key
                                                         %control-travel_id      = if_abap_behv=>mk-on
                                                         %control-overall_status = if_abap_behv=>mk-on ) )
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
* Workaround for missing "Fiori UI + button" when implementing "create by association"
*
********************************************************************************
  METHOD create_booking.

    DATA: lv_next_booking_id TYPE /dmo/booking_id.

    LOOP AT keys INTO DATA(ls_cba).

      READ ENTITY /dmo/i_travel_m BY \_booking
      FROM VALUE #( ( travel_id = ls_cba-travel_id
                      %control  = VALUE #( travel_id = if_abap_behv=>mk-on ) ) )
           RESULT   DATA(lt_read_result)
           FAILED   DATA(ls_read_failed)
           REPORTED DATA(ls_read_reported).

      IF lt_read_result IS INITIAL.
        lv_next_booking_id = '0001'.
      ELSE.
        SORT lt_read_result BY booking_id DESCENDING.
        lv_next_booking_id = lt_read_result[ 1 ]-booking_id + 1.
      ENDIF.

      MODIFY ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
      ENTITY  travel CREATE BY \_booking FROM VALUE #( ( travel_id = ls_cba-travel_id
                                                           %target = VALUE #( ( travel_id               = ls_cba-travel_id    "full key is required
                                                                                booking_id              = lv_next_booking_id  "full key is required
                                                                                booking_date            = cl_abap_context_info=>get_system_date( )
                                                                                customer_id             = '000007'
                                                                                carrier_id              = 'UA'
                                                                                connection_id           = '1537'
                                                                                flight_date             = cl_abap_context_info=>get_system_date( ) + 5
                                                                                flight_price            = '422.00'
                                                                                currency_code           = 'EUR'
                                                                                booking_status          = 'N'
                                                                                %control-travel_id      = if_abap_behv=>mk-on
                                                                                %control-booking_id     = if_abap_behv=>mk-on
                                                                                %control-booking_date   = if_abap_behv=>mk-on
                                                                                %control-customer_id    = if_abap_behv=>mk-on
                                                                                %control-carrier_id     = if_abap_behv=>mk-on
                                                                                %control-connection_id  = if_abap_behv=>mk-on
                                                                                %control-flight_date    = if_abap_behv=>mk-on
                                                                                %control-flight_price   = if_abap_behv=>mk-on
                                                                                %control-currency_code  = if_abap_behv=>mk-on
                                                                                %control-booking_status = if_abap_behv=>mk-on
                                                                                ) ) ) )
      FAILED   DATA(ls_failed)
      MAPPED   DATA(ls_mapped)
      REPORTED DATA(ls_reported).

      APPEND LINES OF ls_failed-booking   TO failed-booking.
      APPEND LINES OF ls_reported-booking TO reported-booking.

      APPEND VALUE #( travel_id        = ls_cba-travel_id
                      %param-travel_id = ls_cba-travel_id ) TO result.
    ENDLOOP.

  ENDMETHOD.


********************************************************************************
*
* Implements what opearations and actions are not allowed for travel instances
*
********************************************************************************
*  METHOD check_authority_for_travel.
*
*    LOOP AT it_travel_key INTO DATA(ls_travel_key).
*      result = VALUE #( ( travel_id            = ls_travel_key-travel_id
*                          %update              = if_abap_behv=>auth-allowed       "Default setting
*                          %delete              = if_abap_behv=>auth-unauthorized
*                          %action-rejectTravel = if_abap_behv=>auth-unauthorized
*                      ) ).
*    ENDLOOP.
*
*  ENDMETHOD.

ENDCLASS.
