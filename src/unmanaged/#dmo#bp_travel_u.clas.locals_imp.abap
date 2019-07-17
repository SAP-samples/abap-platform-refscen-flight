**********************************************************************
*
* Local class for handling travel messages
*
**********************************************************************
CLASS lcl_message_helper DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES tt_travel_failed      TYPE TABLE FOR FAILED   /dmo/i_travel_u.
    TYPES tt_travel_reported    TYPE TABLE FOR REPORTED /dmo/i_travel_u.

    CLASS-METHODS handle_travel_messages
      IMPORTING iv_cid       TYPE string OPTIONAL
                iv_travel_id TYPE /dmo/travel_id OPTIONAL
                it_messages  TYPE /dmo/if_flight_legacy=>tt_message
      CHANGING  failed       TYPE tt_travel_failed
                reported     TYPE tt_travel_reported.
ENDCLASS.

CLASS lcl_message_helper IMPLEMENTATION.

  METHOD handle_travel_messages.
    LOOP AT it_messages INTO DATA(ls_message) WHERE msgty = 'E' OR msgty = 'A'.
      INSERT VALUE #( %cid = iv_cid  travelid = iv_travel_id )
             INTO TABLE failed.
      INSERT /dmo/cl_travel_auxiliary=>map_travel_message(
                                          iv_travel_id = iv_travel_id
                                          is_message   = ls_message ) INTO TABLE reported.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

**********************************************************************
*
* Handler class for managing travels
*
**********************************************************************
CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS:
        create_travel     FOR MODIFY
                                IMPORTING it_travel_create               FOR CREATE travel,
        update_travel     FOR MODIFY
                                IMPORTING it_travel_update               FOR UPDATE travel,
        delete_travel     FOR MODIFY
                                IMPORTING it_travel_delete               FOR DELETE travel,
        read_travel       FOR READ
                                IMPORTING it_travel                      FOR READ travel
                                RESULT et_travel,
        set_travel_status FOR MODIFY
                                IMPORTING it_travel_set_status_booked    FOR ACTION travel~set_status_booked
                                RESULT et_travel_set_status_booked,
        cba_booking       FOR MODIFY
                                IMPORTING it_booking_create_ba           FOR CREATE travel\_booking.

ENDCLASS.


CLASS lhc_travel IMPLEMENTATION.
**********************************************************************
*
* Create travel instances
*
**********************************************************************
  METHOD create_travel.

    DATA lt_messages   TYPE /dmo/if_flight_legacy=>tt_message.
    DATA ls_travel_in  TYPE /dmo/if_flight_legacy=>ts_travel_in.
    DATA ls_travel_out TYPE /dmo/travel.

    LOOP AT it_travel_create ASSIGNING FIELD-SYMBOL(<fs_travel_create>).
      CLEAR ls_travel_in.
      ls_travel_in = CORRESPONDING #( /dmo/cl_travel_auxiliary=>map_travel_cds_to_db( CORRESPONDING #( <fs_travel_create> ) ) ).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_CREATE'
        EXPORTING
          is_travel   = ls_travel_in
        IMPORTING
          es_travel   = ls_travel_out
          et_messages = lt_messages.



      IF lt_messages IS INITIAL.
        INSERT VALUE #( %cid = <fs_travel_create>-%cid  travelid = ls_travel_out-travel_id )
                       INTO TABLE mapped-travel.
      ELSE.

        lcl_message_helper=>handle_travel_messages(
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
    DATA ls_travel      TYPE /dmo/if_flight_legacy=>ts_travel_in.
    DATA ls_travelx TYPE /dmo/if_flight_legacy=>ts_travel_inx. "refers to x structure (> BAPIs)

    LOOP AT it_travel_update ASSIGNING FIELD-SYMBOL(<fs_travel_update>).

      CLEAR ls_travel.
      ls_travel = CORRESPONDING #( /dmo/cl_travel_auxiliary=>map_travel_cds_to_db( CORRESPONDING #( <fs_travel_update> ) ) ).

      ls_travelx-travel_id     = ls_travel-travel_id.

      ls_travelx-agency_id     = xsdbool( <fs_travel_update>-%control-agencyid     = if_abap_behv=>mk-on ).
      ls_travelx-customer_id   = xsdbool( <fs_travel_update>-%control-customerid   = if_abap_behv=>mk-on ).
      ls_travelx-begin_date    = xsdbool( <fs_travel_update>-%control-begindate    = if_abap_behv=>mk-on ).
      ls_travelx-end_date      = xsdbool( <fs_travel_update>-%control-enddate      = if_abap_behv=>mk-on ).
      ls_travelx-booking_fee   = xsdbool( <fs_travel_update>-%control-bookingfee   = if_abap_behv=>mk-on ).
      ls_travelx-total_price   = xsdbool( <fs_travel_update>-%control-totalprice   = if_abap_behv=>mk-on ).
      ls_travelx-currency_code = xsdbool( <fs_travel_update>-%control-currencycode = if_abap_behv=>mk-on ).
      ls_travelx-description   = xsdbool( <fs_travel_update>-%control-memo         = if_abap_behv=>mk-on ).
      ls_travelx-status        = xsdbool( <fs_travel_update>-%control-status       = if_abap_behv=>mk-on ).


      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = ls_travel
          is_travelx  = ls_travelx
        IMPORTING
          et_messages = lt_messages.


      lcl_message_helper=>handle_travel_messages(
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

      lcl_message_helper=>handle_travel_messages(
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
        lcl_message_helper=>handle_travel_messages(
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
    DATA ls_booking         TYPE LINE OF /dmo/if_flight_legacy=>tt_booking_in.
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
          ls_booking = /dmo/cl_travel_auxiliary=>map_booking_cds_to_db( CORRESPONDING #( <fs_booking_create> ) ).

          lv_last_booking_id += 1.
          ls_booking-booking_id = lv_last_booking_id.

          CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
            EXPORTING
              is_travel   = VALUE /dmo/if_flight_legacy=>ts_travel_in( travel_id = lv_travelid )
              is_travelx  = VALUE /dmo/if_flight_legacy=>ts_travel_inx( travel_id = lv_travelid )
              it_booking  = VALUE /dmo/if_flight_legacy=>tt_booking_in( ( ls_booking ) )
              it_bookingx = VALUE /dmo/if_flight_legacy=>tt_booking_inx( ( booking_id  = ls_booking-booking_id
                                                                           action_code = /dmo/if_flight_legacy=>action_code-create ) )
            IMPORTING
              et_messages = lt_messages.

          IF lt_messages IS INITIAL.
            INSERT VALUE #( %cid = <fs_booking_create>-%cid  travelid = lv_travelid  bookingid = ls_booking-booking_id ) INTO TABLE mapped-booking.
          ELSE.

            LOOP AT lt_messages INTO DATA(ls_message) WHERE msgty = 'E' OR msgty = 'A'.
              INSERT VALUE #( %cid = <fs_booking_create>-%cid ) INTO TABLE failed-booking.
              INSERT /dmo/cl_travel_auxiliary=>map_booking_message(
                                                        iv_cid     = <fs_booking_create>-%cid
                                                        is_message = ls_message )
                                                    INTO TABLE reported-booking.
            ENDLOOP.

          ENDIF.

        ENDLOOP.

      ELSE.

        lcl_message_helper=>handle_travel_messages(
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
