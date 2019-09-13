**********************************************************************
*
* Handler class for managing travels
*
**********************************************************************
CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    TYPES:
         tt_travel_update TYPE TABLE FOR UPDATE /dmo/i_travel_u.

    METHODS:
      create_travel     FOR MODIFY
        IMPORTING it_travel_create FOR CREATE travel,
      update_travel     FOR MODIFY
        IMPORTING it_travel_update FOR UPDATE travel,
      delete_travel     FOR MODIFY
        IMPORTING it_travel_delete FOR DELETE travel,
      read_travel       FOR READ
        IMPORTING it_travel FOR READ travel
        RESULT    et_travel,
      set_travel_status FOR MODIFY
        IMPORTING it_travel_set_status_booked FOR ACTION travel~set_status_booked
        RESULT    et_travel_set_status_booked,
      cba_booking       FOR MODIFY
        IMPORTING it_booking_create_ba FOR CREATE travel\_booking,

      get_features      FOR FEATURES
        IMPORTING keys   REQUEST requested_features FOR travel
        RESULT    result,

      _fill_travel_inx
        IMPORTING is_travel_update     TYPE LINE OF tt_travel_update
        RETURNING VALUE(rs_travel_inx) TYPE /dmo/if_flight_legacy=>ts_travel_inx.
ENDCLASS.


CLASS lhc_travel IMPLEMENTATION.

**********************************************************************
*
* Create travel instances
*
**********************************************************************
  METHOD create_travel.

    DATA lt_messages   TYPE /dmo/if_flight_legacy=>tt_message.
    DATA ls_travel_in  TYPE /dmo/travel.
    DATA ls_travel_out TYPE /dmo/travel.

    LOOP AT it_travel_create ASSIGNING FIELD-SYMBOL(<fs_travel_create>).

      ls_travel_in = CORRESPONDING #( <fs_travel_create> MAPPING FROM ENTITY USING CONTROL ).


      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_CREATE'
        EXPORTING
          is_travel   = CORRESPONDING /dmo/if_flight_legacy=>ts_travel_in( ls_travel_in )
        IMPORTING
          es_travel   = ls_travel_out
          et_messages = lt_messages.



      IF lt_messages IS INITIAL.
        INSERT VALUE #( %cid = <fs_travel_create>-%cid  travelid = ls_travel_out-travel_id )
                       INTO TABLE mapped-travel.
      ELSE.


        /dmo/cl_travel_auxiliary=>handle_travel_messages(
          EXPORTING
            iv_cid       = <fs_travel_create>-%cid
            it_messages  = lt_messages
          CHANGING
            failed       = failed-travel
            reported     = reported-travel ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


**********************************************************************
*
* Update data of existing travel instances
*
**********************************************************************
  METHOD update_travel.

    DATA lt_messages    TYPE /dmo/if_flight_legacy=>tt_message.
    DATA ls_travel      TYPE /dmo/travel.
    DATA ls_travelx TYPE /dmo/if_flight_legacy=>ts_travel_inx. "refers to x structure (> BAPIs)

    LOOP AT it_travel_update ASSIGNING FIELD-SYMBOL(<fs_travel_update>).

      ls_travel = CORRESPONDING #( <fs_travel_update> MAPPING FROM ENTITY ).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = CORRESPONDING /dmo/if_flight_legacy=>ts_travel_in( ls_travel )
          is_travelx  = _fill_travel_inx( <fs_travel_update> )
        IMPORTING
          et_messages = lt_messages.

      /dmo/cl_travel_auxiliary=>handle_travel_messages(
        EXPORTING
          iv_cid       = <fs_travel_update>-%cid_ref
          iv_travel_id = <fs_travel_update>-travelid
          it_messages  = lt_messages
        CHANGING
          failed   = failed-travel
          reported = reported-travel ).

    ENDLOOP.

  ENDMETHOD.

**********************************************************************
* Helper method:
* Indicates the travel fields that have been changed by the client
*
**********************************************************************
  METHOD _fill_travel_inx.

    CLEAR rs_travel_inx.
    rs_travel_inx-travel_id = is_travel_update-TravelID.

    rs_travel_inx-agency_id     = xsdbool( is_travel_update-%control-agencyid     = if_abap_behv=>mk-on ).
    rs_travel_inx-customer_id   = xsdbool( is_travel_update-%control-customerid   = if_abap_behv=>mk-on ).
    rs_travel_inx-begin_date    = xsdbool( is_travel_update-%control-begindate    = if_abap_behv=>mk-on ).
    rs_travel_inx-end_date      = xsdbool( is_travel_update-%control-enddate      = if_abap_behv=>mk-on ).
    rs_travel_inx-booking_fee   = xsdbool( is_travel_update-%control-bookingfee   = if_abap_behv=>mk-on ).
    rs_travel_inx-total_price   = xsdbool( is_travel_update-%control-totalprice   = if_abap_behv=>mk-on ).
    rs_travel_inx-currency_code = xsdbool( is_travel_update-%control-currencycode = if_abap_behv=>mk-on ).
    rs_travel_inx-description   = xsdbool( is_travel_update-%control-memo         = if_abap_behv=>mk-on ).
    rs_travel_inx-status        = xsdbool( is_travel_update-%control-status       = if_abap_behv=>mk-on ).
  ENDMETHOD.


**********************************************************************
*
* Delete of travel instances
*
**********************************************************************
  METHOD delete_travel.

    DATA lt_messages TYPE /dmo/if_flight_legacy=>tt_message.

    LOOP AT it_travel_delete ASSIGNING FIELD-SYMBOL(<fs_travel_delete>).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_DELETE'
        EXPORTING
          iv_travel_id = <fs_travel_delete>-travelid
        IMPORTING
          et_messages  = lt_messages.


      /dmo/cl_travel_auxiliary=>handle_travel_messages(
        EXPORTING
          iv_cid       = <fs_travel_delete>-%cid_ref
          iv_travel_id = <fs_travel_delete>-travelid
          it_messages  = lt_messages
        CHANGING
          failed       = failed-travel
          reported     = reported-travel ).

    ENDLOOP.

  ENDMETHOD.



**********************************************************************
*
* Read travel data from buffer
*
**********************************************************************
  METHOD read_travel.
    DATA: ls_travel_out TYPE /dmo/travel.

    LOOP AT it_travel INTO DATA(ls_travel_to_read).
      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = ls_travel_to_read-travelid
        IMPORTING
          es_travel    = ls_travel_out.

      INSERT VALUE #( travelid      = ls_travel_to_read-travelid
                      agencyid      = ls_travel_out-agency_id
                      customerid    = ls_travel_out-customer_id
                      begindate     = ls_travel_out-begin_date
                      enddate       = ls_travel_out-end_date
                      bookingfee    = ls_travel_out-booking_fee
                      totalprice    = ls_travel_out-total_price
                      currencycode  = ls_travel_out-currency_code
                      memo          = ls_travel_out-description
                      status        = ls_travel_out-status
                      lastchangedat = ls_travel_out-lastchangedat ) INTO TABLE et_travel.
    ENDLOOP.

  ENDMETHOD.


**********************************************************************
*
* Implement travel action(s) (in our case: for setting travel status)
*
**********************************************************************

  METHOD set_travel_status.

    DATA lt_messages TYPE /dmo/if_flight_legacy=>tt_message.
    DATA ls_travel_out TYPE /dmo/travel.

    CLEAR et_travel_set_status_booked.

    LOOP AT it_travel_set_status_booked ASSIGNING FIELD-SYMBOL(<fs_travel_set_status_booked>).

      DATA(lv_travelid) = <fs_travel_set_status_booked>-travelid.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_SET_BOOKING'
        EXPORTING
          iv_travel_id = lv_travelid
        IMPORTING
          et_messages  = lt_messages.

      IF lt_messages IS INITIAL.
        APPEND VALUE #( travelid        = lv_travelid
                        %param-travelid = lv_travelid )
               TO et_travel_set_status_booked.
      ELSE.

        /dmo/cl_travel_auxiliary=>handle_travel_messages(
          EXPORTING
            iv_cid       = <fs_travel_set_status_booked>-%cid_ref
            iv_travel_id = lv_travelid
            it_messages  = lt_messages
          CHANGING
            failed       = failed-travel
            reported     = reported-travel ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


**********************************************************************
*
* Create associated booking instances
*
**********************************************************************
  METHOD cba_booking.
    DATA lt_messages        TYPE /dmo/if_flight_legacy=>tt_message.
    DATA lt_booking_old     TYPE /dmo/if_flight_legacy=>tt_booking.
    DATA ls_booking         TYPE /dmo/booking.
    DATA lv_last_booking_id TYPE /dmo/booking_id VALUE '0'.

    LOOP AT it_booking_create_ba ASSIGNING FIELD-SYMBOL(<fs_booking_create_ba>).

      DATA(lv_travelid) = <fs_booking_create_ba>-travelid.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = lv_travelid
        IMPORTING
          et_booking   = lt_booking_old
          et_messages  = lt_messages.

      IF lt_messages IS INITIAL.

        IF lt_booking_old IS NOT INITIAL.
          lv_last_booking_id = lt_booking_old[ lines( lt_booking_old ) ]-booking_id.
        ENDIF.

        LOOP AT <fs_booking_create_ba>-%target ASSIGNING FIELD-SYMBOL(<fs_booking_create>).
          ls_booking = CORRESPONDING #( <fs_booking_create> MAPPING FROM ENTITY USING CONTROL ) .

          lv_last_booking_id += 1.
          ls_booking-booking_id = lv_last_booking_id.

          CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
            EXPORTING
              is_travel   = VALUE /dmo/if_flight_legacy=>ts_travel_in( travel_id = lv_travelid )
              is_travelx  = VALUE /dmo/if_flight_legacy=>ts_travel_inx( travel_id = lv_travelid )
              it_booking  = VALUE /dmo/if_flight_legacy=>tt_booking_in( ( CORRESPONDING #( ls_booking ) ) )
              it_bookingx = VALUE /dmo/if_flight_legacy=>tt_booking_inx( ( booking_id  = ls_booking-booking_id
                                                                           action_code = /dmo/if_flight_legacy=>action_code-create ) )
            IMPORTING
              et_messages = lt_messages.

          IF lt_messages IS INITIAL.
            INSERT VALUE #( %cid = <fs_booking_create>-%cid  travelid = lv_travelid  bookingid = ls_booking-booking_id )
                INTO TABLE mapped-booking.
          ELSE.

            LOOP AT lt_messages INTO DATA(ls_message) WHERE msgty = 'E' OR msgty = 'A'.
              INSERT VALUE #( %cid = <fs_booking_create>-%cid ) INTO TABLE failed-booking.

              INSERT VALUE #(
                    %cid     = <fs_booking_create>-%cid
                    travelid = <fs_booking_create>-TravelID
                    %msg     = new_message(
                              id       = ls_message-msgid
                              number   = ls_message-msgno
                              severity = if_abap_behv_message=>severity-error
                              v1       = ls_message-msgv1
                              v2       = ls_message-msgv2
                              v3       = ls_message-msgv3
                              v4       = ls_message-msgv4
                        )
               )
              INTO TABLE reported-booking.

            ENDLOOP.

          ENDIF.

        ENDLOOP.

      ELSE.

        /dmo/cl_travel_auxiliary=>handle_travel_messages(
         EXPORTING
           iv_cid       = <fs_booking_create_ba>-%cid_ref
           iv_travel_id = lv_travelid
           it_messages  = lt_messages
         CHANGING
           failed       = failed-travel
           reported     = reported-travel ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


********************************************************************************
*
* Implements the dynamic action handling for travel instances
*
********************************************************************************
  METHOD get_features.

    READ ENTITY /dmo/i_travel_u FROM VALUE #( FOR keyval IN keys
                                                      (  %key                    = keyval-%key
                                                         %control-TravelID      = if_abap_behv=>mk-on
                                                         ) )
                                RESULT DATA(lt_travel_result).


    result = VALUE #( FOR ls_travel IN lt_travel_result
                       ( %key                        = ls_travel-%key
                         %features-%action-set_status_booked = COND #( WHEN ls_travel-Status = 'B'
                                                                          THEN if_abap_behv=>fc-o-disabled
                                                                          ELSE if_abap_behv=>fc-o-enabled  )
                      ) ).

  ENDMETHOD.



ENDCLASS.


**********************************************************************
*
* Saver class implements the save sequence for data persistence
*
**********************************************************************
CLASS lsc_saver DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.
    METHODS finalize          REDEFINITION.
    METHODS check_before_save REDEFINITION.
    METHODS save              REDEFINITION.
    METHODS cleanup           REDEFINITION.
ENDCLASS.

CLASS lsc_saver IMPLEMENTATION.
  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
    CALL FUNCTION '/DMO/FLIGHT_TRAVEL_SAVE'.
  ENDMETHOD.

  METHOD cleanup.
    CALL FUNCTION '/DMO/FLIGHT_TRAVEL_INITIALIZE'.
  ENDMETHOD.
ENDCLASS.
