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
      create_booking_ba FOR MODIFY
        IMPORTING it_booking_create_ba FOR CREATE travel\_booking,
      read_booking_ba   FOR READ

        IMPORTING it_travel  FOR READ travel\_Booking
                    FULL iv_full_requested
        RESULT    et_booking
                    LINK et_link_table,
      lock              FOR LOCK
        IMPORTING it_travel_lock FOR LOCK travel,

      set_travel_status FOR MODIFY
        IMPORTING it_travel_set_status_booked FOR ACTION travel~set_status_booked
        RESULT    et_travel_set_status_booked,

      get_features      FOR FEATURES
        IMPORTING keys   REQUEST requested_features FOR travel
        RESULT    result.

ENDCLASS.


CLASS lhc_travel IMPLEMENTATION.



**********************************************************************
*
* Create travel instances
*
**********************************************************************
  METHOD create_travel.

    DATA lt_messages   TYPE /dmo/t_message.
    DATA ls_travel_in  TYPE /dmo/travel.
    DATA ls_travel_out TYPE /dmo/travel.

    LOOP AT it_travel_create ASSIGNING FIELD-SYMBOL(<fs_travel_create>).

      ls_travel_in = CORRESPONDING #( <fs_travel_create> MAPPING FROM ENTITY USING CONTROL ).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_CREATE'
        EXPORTING
          is_travel   = CORRESPONDING /dmo/s_travel_in( ls_travel_in )
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
            reported     = reported-travel
        ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


**********************************************************************
*
* Update data of existing travel instances
*
**********************************************************************
  METHOD update_travel.

    DATA lt_messages TYPE /dmo/t_message.
    DATA ls_travel   TYPE /dmo/travel.
    DATA ls_travelx  TYPE /dmo/s_travel_inx. "refers to x structure (> BAPIs)

    LOOP AT it_travel_update ASSIGNING FIELD-SYMBOL(<fs_travel_update>).

      ls_travel = CORRESPONDING #( <fs_travel_update> MAPPING FROM ENTITY ).

      ls_travelx-travel_id = <fs_travel_update>-TravelID.
      ls_travelx-_intx     = CORRESPONDING #( <fs_travel_update> MAPPING FROM ENTITY ).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = CORRESPONDING /dmo/s_travel_in( ls_travel )
          is_travelx  = ls_travelx
        IMPORTING
          et_messages = lt_messages.

      /dmo/cl_travel_auxiliary=>handle_travel_messages(
        EXPORTING
          iv_cid       = <fs_travel_update>-%cid_ref
          iv_travel_id = <fs_travel_update>-travelid
          it_messages  = lt_messages
        CHANGING
          failed   = failed-travel
          reported = reported-travel
      ).

    ENDLOOP.

  ENDMETHOD.


**********************************************************************
*
* Delete of travel instances
*
**********************************************************************
  METHOD delete_travel.

    DATA lt_messages TYPE /dmo/t_message.

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
          reported     = reported-travel
      ).

    ENDLOOP.

  ENDMETHOD.



**********************************************************************
*
* Read travel data from buffer
*
**********************************************************************
  METHOD read_travel.
    DATA: ls_travel_out TYPE /dmo/travel,
          lt_messages   TYPE /dmo/t_message.

    LOOP AT it_travel INTO DATA(ls_travel_to_read) GROUP BY ls_travel_to_read-%key.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = ls_travel_to_read-travelid
        IMPORTING
          es_travel    = ls_travel_out
          et_messages  = lt_messages.

      IF lt_messages IS INITIAL.
        "fill result parameter with flagged fields

        INSERT CORRESPONDING #( ls_travel_out MAPPING TO ENTITY ) INTO TABLE et_travel.

      ELSE.
        "fill failed table in case of error

*        failed-travel = VALUE #(
*          BASE failed-travel
*          FOR msg IN lt_messages (
*            %key = ls_travel_to_read-%key
*            %fail-cause = COND #(
*              WHEN msg-msgty = 'E' AND ( msg-msgno = '016' OR msg-msgno = '009' )
*              THEN if_abap_behv=>cause-not_found
*              ELSE if_abap_behv=>cause-unspecific
*            )
*          )
*        ).

        /dmo/cl_travel_auxiliary=>handle_travel_messages(
          EXPORTING
            iv_travel_id = ls_travel_to_read-travelid
            it_messages  = lt_messages
          CHANGING
            failed       = failed-travel
            reported     = reported-travel
        ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


**********************************************************************
*
* Create associated booking instances
*
**********************************************************************
  METHOD create_booking_ba.
    DATA lt_messages        TYPE /dmo/t_message.
    DATA lt_booking_old     TYPE /dmo/t_booking.
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
              is_travel   = VALUE /dmo/s_travel_in( travel_id = lv_travelid )
              is_travelx  = VALUE /dmo/s_travel_inx( travel_id = lv_travelid )
              it_booking  = VALUE /dmo/t_booking_in( ( CORRESPONDING #( ls_booking ) ) )
              it_bookingx = VALUE /dmo/t_booking_inx(
                (
                  booking_id  = ls_booking-booking_id
                  action_code = /dmo/if_flight_legacy=>action_code-create
                )
              )
            IMPORTING
              et_messages = lt_messages.

          IF lt_messages IS INITIAL.

            INSERT
              VALUE #(
                %cid = <fs_booking_create>-%cid
                travelid = lv_travelid
                bookingid = ls_booking-booking_id
              )
              INTO TABLE mapped-booking.

          ELSE.

            LOOP AT lt_messages INTO DATA(ls_message) WHERE msgty = 'E' OR msgty = 'A'.

              INSERT VALUE #( %cid = <fs_booking_create>-%cid ) INTO TABLE failed-booking.

              INSERT
                VALUE #(
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
            reported     = reported-travel
        ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

**********************************************************************
*
* Read booking data by association from buffer
*
**********************************************************************
  METHOD read_booking_ba.
    DATA: ls_travel_out  TYPE /dmo/travel,
          lt_booking_out TYPE /dmo/t_booking,
          ls_booking     LIKE LINE OF et_booking,
          lt_messages    TYPE /dmo/t_message.


    LOOP AT it_travel ASSIGNING FIELD-SYMBOL(<fs_travel_rba>) GROUP BY <fs_travel_rba>-TravelID.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = <fs_travel_rba>-travelid
        IMPORTING
          es_travel    = ls_travel_out
          et_booking   = lt_booking_out
          et_messages  = lt_messages.

      IF lt_messages IS INITIAL.

        LOOP AT lt_booking_out ASSIGNING FIELD-SYMBOL(<fs_booking>).
          "fill link table with key fields

          INSERT
            VALUE #(
              source-%key = <fs_travel_rba>-%key
              target-%key = VALUE #(
                TravelID  = <fs_booking>-travel_id
                BookingID = <fs_booking>-booking_id
              )
            )
            INTO TABLE et_link_table.

          "fill result parameter with flagged fields
          IF iv_full_requested = abap_true.

            ls_booking = CORRESPONDING #( <fs_booking> MAPPING TO ENTITY ).
*            ls_booking-lastchangedat = ls_travel_out-lastchangedat.
            INSERT ls_booking INTO TABLE et_booking.

          ENDIF.

        ENDLOOP.

      ELSE.
        "fill failed table in case of error

*        failed-travel = VALUE #(
*          BASE failed-travel
*          FOR msg IN lt_messages (
*            %key = <fs_travel_rba>-TravelID
*            %fail-cause = COND #(
*              WHEN msg-msgty = 'E' AND ( msg-msgno = '016' OR msg-msgno = '009' )
*              THEN if_abap_behv=>cause-not_found
*              ELSE if_abap_behv=>cause-unspecific
*            )
*          )
*        ).

        /dmo/cl_travel_auxiliary=>handle_travel_messages(
          EXPORTING
            iv_travel_id = <fs_travel_rba>-travelid
            it_messages  = lt_messages
          CHANGING
            failed       = failed-travel
            reported     = reported-travel
        ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


**********************************************************************
*
* LOCK
*
**********************************************************************

  METHOD lock.

    TRY.
        "Instantiate lock object
        DATA(lock) = cl_abap_lock_object_factory=>get_instance( iv_name = '/DMO/ETRAVEL' ).
      CATCH cx_abap_lock_failure INTO DATA(lr_exp).
        RAISE SHORTDUMP lr_exp.
    ENDTRY.

    LOOP AT it_travel_lock   ASSIGNING FIELD-SYMBOL(<fs_travel>).
      TRY.
          "enqueue travel instance
          lock->enqueue(
              it_parameter  = VALUE #( (  name = 'TRAVEL_ID' value = REF #( <fs_travel>-travelid ) ) )
          ).
          "if foreign lock exists
        CATCH cx_abap_foreign_lock INTO DATA(lx_foreign_lock).
          /dmo/cl_travel_auxiliary=>handle_travel_messages(
           EXPORTING
                iv_travel_id = <fs_travel>-TravelID
                it_messages  =  VALUE #( (
                                              msgid = '/DMO/CM_FLIGHT_LEGAC'
                                              msgty = 'E'
                                              msgno = '032'
                                              msgv1 = <fs_travel>-travelid
                                              msgv2 = lx_foreign_lock->user_name )
                             )
              CHANGING
                failed       = failed-travel
                reported     = reported-travel
            ).

        CATCH cx_abap_lock_failure INTO lr_exp.
          RAISE SHORTDUMP lr_exp.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.



**********************************************************************
*
* Implement travel action(s) (in our case: for setting travel status)
*
**********************************************************************

  METHOD set_travel_status.

    DATA lt_messages TYPE /dmo/t_message.
    DATA ls_travel_out TYPE /dmo/travel.
    DATA ls_travel_set_status_booked LIKE LINE OF et_travel_set_status_booked.

    CLEAR et_travel_set_status_booked.

    LOOP AT it_travel_set_status_booked ASSIGNING FIELD-SYMBOL(<fs_travel_set_status_booked>).

      DATA(lv_travelid) = <fs_travel_set_status_booked>-travelid.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_SET_BOOKING'
        EXPORTING
          iv_travel_id = lv_travelid
        IMPORTING
          et_messages  = lt_messages.

      IF lt_messages IS INITIAL.

        CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
          EXPORTING
            iv_travel_id = lv_travelid
          IMPORTING
            es_travel    = ls_travel_out.

        ls_travel_set_status_booked-travelid        = lv_travelid.
        ls_travel_set_status_booked-%param          = CORRESPONDING #( ls_travel_out MAPPING TO ENTITY ).
        ls_travel_set_status_booked-%param-travelid = lv_travelid.
        APPEND ls_travel_set_status_booked TO et_travel_set_status_booked.

      ELSE.

        /dmo/cl_travel_auxiliary=>handle_travel_messages(
          EXPORTING
            iv_cid       = <fs_travel_set_status_booked>-%cid_ref
            iv_travel_id = lv_travelid
            it_messages  = lt_messages
          CHANGING
            failed       = failed-travel
            reported     = reported-travel
        ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
********************************************************************************
*
* Implements the dynamic action handling for travel instances
*
********************************************************************************
  METHOD get_features.

    READ ENTITIES OF /DMO/I_Travel_U IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelID Status )
        WITH CORRESPONDING #( keys )
    RESULT DATA(lt_travel_result)
    FAILED failed.

    result = VALUE #(
      FOR ls_travel IN lt_travel_result (
        %key                                = ls_travel-%key
        %features-%action-set_status_booked = COND #( WHEN ls_travel-Status = 'B'
                                                      THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled )
        %assoc-_Booking                     = COND #( WHEN ls_travel-Status = 'B'
                                                      THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled )

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
