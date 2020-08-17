**********************************************************************
*
* Handler class implements UPDATE and DELETE for booking supplements
*
**********************************************************************
CLASS lhc_supplement DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    TYPES tt_bookingsupplement_failed   TYPE TABLE FOR FAILED   /dmo/i_bookingsupplement_u.
    TYPES tt_bookingsupplement_reported TYPE TABLE FOR REPORTED /dmo/i_bookingsupplement_u.

    TYPES:
      tt_booking_update           TYPE TABLE FOR UPDATE    /dmo/i_booking_u,
      tt_bookingsupplement_update TYPE TABLE FOR UPDATE    /dmo/i_bookingsupplement_u.


    METHODS:
      update_bookingsupplement FOR MODIFY
        IMPORTING it_bookingsupplement_update FOR UPDATE bookingsupplement,
      delete_bookingsupplement FOR MODIFY
        IMPORTING it_bookingsupplement_delete FOR DELETE bookingsupplement,
      read_bookingsupplement FOR READ
        IMPORTING it_bookingsupplement_read FOR READ bookingsupplement
        RESULT    et_bookingsupplement,
      read_travel_ba FOR READ
        IMPORTING it_bookingsupplmement FOR READ bookingsupplement\_Travel FULL iv_full_requested
        RESULT    et_travel LINK et_link_table.


    METHODS _fill_bookingsupplement_inx
      IMPORTING is_bookingsupplement_update     TYPE LINE OF tt_bookingsupplement_update
      RETURNING VALUE(rs_bookingsupplement_inx) TYPE /dmo/s_booking_supplement_inx.
ENDCLASS.


CLASS lhc_supplement IMPLEMENTATION.



**********************************************************************
*
* Implements the UPDATE operation for a set of booking supplements
*
**********************************************************************
  METHOD update_bookingsupplement.

    DATA lt_messages TYPE /dmo/t_message.
    DATA lt_book_supplement TYPE /dmo/book_suppl.

    LOOP AT it_bookingsupplement_update ASSIGNING FIELD-SYMBOL(<fs_bookingsupplement_update>).
      lt_book_supplement = CORRESPONDING  #( <fs_bookingsupplement_update> MAPPING FROM ENTITY ).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel              = VALUE /dmo/s_travel_in( travel_id = <fs_bookingsupplement_update>-travelid )
          is_travelx             = VALUE /dmo/s_travel_inx( travel_id = <fs_bookingsupplement_update>-travelid )
          it_booking_supplement  = VALUE /dmo/t_booking_supplement_in( ( CORRESPONDING #( lt_book_supplement ) ) )
          it_booking_supplementx = VALUE /dmo/t_booking_supplement_inx( ( _fill_bookingsupplement_inx( <fs_bookingsupplement_update> ) ) )
        IMPORTING
          et_messages            = lt_messages.


      /dmo/cl_travel_auxiliary=>handle_booksupplement_messages(
        EXPORTING
          iv_cid                  = <fs_bookingsupplement_update>-%cid_ref
          iv_travel_id            = <fs_bookingsupplement_update>-travelid
          iv_booking_id           = <fs_bookingsupplement_update>-bookingid
          iv_bookingsupplement_id = <fs_bookingsupplement_update>-bookingsupplementid
          it_messages             = lt_messages
        CHANGING
            failed   = failed-bookingsupplement
            reported = reported-bookingsupplement ).

    ENDLOOP.

  ENDMETHOD.


**********************************************************************
* Helper method:
* Indicates the booking supplement fields that have been changed by the client
**********************************************************************

  METHOD _fill_bookingsupplement_inx.

    CLEAR rs_bookingsupplement_inx.
    rs_bookingsupplement_inx-booking_supplement_id = is_bookingsupplement_update-bookingsupplementid.
    rs_bookingsupplement_inx-action_code           = /dmo/if_flight_legacy=>action_code-update.
    rs_bookingsupplement_inx-booking_id            = is_bookingsupplement_update-bookingid.

    rs_bookingsupplement_inx-supplement_id         = xsdbool( is_bookingsupplement_update-%control-supplementid = if_abap_behv=>mk-on ).
    rs_bookingsupplement_inx-price                 = xsdbool( is_bookingsupplement_update-%control-price        = if_abap_behv=>mk-on ).
    rs_bookingsupplement_inx-currency_code         = xsdbool( is_bookingsupplement_update-%control-currencycode = if_abap_behv=>mk-on ).
  ENDMETHOD.


**********************************************************************
*
* Implements the DELETE operation for a set of booking supplements
*
**********************************************************************
  METHOD delete_bookingsupplement.

    DATA lt_messages TYPE /dmo/t_message.

    LOOP AT it_bookingsupplement_delete INTO DATA(ls_bookingsupplement_delete).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel              = VALUE /dmo/s_travel_in( travel_id = ls_bookingsupplement_delete-travelid )
          is_travelx             = VALUE /dmo/s_travel_inx( travel_id = ls_bookingsupplement_delete-travelid )
          it_booking             = VALUE /dmo/t_booking_in( ( booking_id = ls_bookingsupplement_delete-bookingid ) )
          it_bookingx            = VALUE /dmo/t_booking_inx( ( booking_id = ls_bookingsupplement_delete-bookingid ) )
          it_booking_supplement  = VALUE /dmo/t_booking_supplement_in( (  booking_supplement_id = ls_bookingsupplement_delete-bookingSupplementid
                                                                          booking_id            = ls_bookingsupplement_delete-BookingID ) )
          it_booking_supplementx = VALUE /dmo/t_booking_supplement_inx( ( booking_supplement_id = ls_bookingsupplement_delete-bookingsupplementid
                                                                          booking_id            = ls_bookingsupplement_delete-bookingid
                                                                          action_code           = /dmo/if_flight_legacy=>action_code-delete ) )
        IMPORTING
          et_messages            = lt_messages.

      IF lt_messages IS NOT INITIAL.

        /dmo/cl_travel_auxiliary=>handle_booksupplement_messages(
         EXPORTING
           iv_cid                  = ls_bookingsupplement_delete-%cid_ref
           iv_travel_id            = ls_bookingsupplement_delete-travelid
           iv_booking_id           = ls_bookingsupplement_delete-bookingid
           iv_bookingsupplement_id = ls_bookingsupplement_delete-bookingsupplementid
           it_messages = lt_messages
         CHANGING
           failed   = failed-bookingsupplement
           reported = reported-bookingsupplement ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

**********************************************************************
*
* Implements the READ operation for a set of booking supplements
*
**********************************************************************
  METHOD read_bookingsupplement.

    DATA: ls_travel_out    TYPE /dmo/travel,
          lt_booksuppl_out TYPE /dmo/t_booking_supplement,
          lt_message       TYPE /dmo/t_message.

    LOOP AT it_bookingsupplement_read ASSIGNING FIELD-SYMBOL(<fs_travel_read>)
                                      GROUP BY <fs_travel_read>-TravelID.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id          = <fs_travel_read>-travelid
        IMPORTING
          es_travel             = ls_travel_out
          et_booking_supplement = lt_booksuppl_out
          et_messages           = lt_message.

      IF lt_message IS INITIAL.
        LOOP AT GROUP <fs_travel_read> ASSIGNING FIELD-SYMBOL(<fs_bookingsuppl_read>)
                                       GROUP BY <fs_bookingsuppl_read>-%key.

          READ TABLE lt_booksuppl_out INTO DATA(ls_bookingsuppl) WITH KEY  travel_id             = <fs_bookingsuppl_read>-%key-TravelID
                                                                           booking_id            = <fs_bookingsuppl_read>-%key-BookingID
                                                                           booking_supplement_id = <fs_bookingsuppl_read>-%key-BookingSupplementID.
          IF sy-subrc = 0 .
            "fill result parameter with flagged fields
            INSERT
               VALUE #( travelid            = ls_bookingsuppl-travel_id
                        bookingid           = ls_bookingsuppl-booking_id
                        bookingsupplementid = ls_bookingsuppl-booking_supplement_id
                        supplementid        = COND #( WHEN <fs_bookingsuppl_read>-%control-SupplementID        = cl_abap_behv=>flag_changed THEN ls_bookingsuppl-supplement_id  )
                        price               = COND #( WHEN <fs_bookingsuppl_read>-%control-Price               = cl_abap_behv=>flag_changed THEN ls_bookingsuppl-price  )
                        currencycode        = COND #( WHEN <fs_bookingsuppl_read>-%control-CurrencyCode        = cl_abap_behv=>flag_changed THEN ls_bookingsuppl-currency_code  )
*                        lastchangedat       = COND #( WHEN <fs_bookingsuppl_read>-%control-LastChangedAt       = cl_abap_behv=>flag_changed THEN ls_travel_out-lastchangedat   )
                      ) INTO TABLE et_bookingsupplement.



          ELSE.
            "BookingSupplementID not found
            INSERT
              VALUE #( travelid           = <fs_bookingsuppl_read>-TravelID
                       bookingid          = <fs_bookingsuppl_read>-BookingID
                       bookingsupplementid = <fs_bookingsuppl_read>-BookingSupplementID
                       %fail-cause = if_abap_behv=>cause-not_found )
              INTO TABLE failed-bookingsupplement.
          ENDIF.
        ENDLOOP.
      ELSE.

        "TravelID not found or other fail cause
        LOOP AT GROUP <fs_travel_read> ASSIGNING <fs_bookingsuppl_read>.
          failed-bookingsupplement = VALUE #(  BASE failed-bookingsupplement
                                     FOR msg IN lt_message ( %key-TravelID            = <fs_bookingsuppl_read>-TravelID
                                                             %key-BookingID           = <fs_bookingsuppl_read>-BookingID
                                                             %key-bookingsupplementid = <fs_bookingsuppl_read>-BookingSupplementID
                                                             %fail-cause              = COND #( WHEN msg-msgty = 'E' AND ( msg-msgno = '016' OR msg-msgno = '009' )
                                                                                                THEN if_abap_behv=>cause-not_found
                                                                                                ELSE if_abap_behv=>cause-unspecific ) ) ).
        ENDLOOP.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.
**********************************************************************
*
* Read travel  by association
*
**********************************************************************
  METHOD read_travel_ba.
    DATA: ls_travel_out TYPE /dmo/travel,
          ls_travel     LIKE LINE OF et_travel,
          lt_message    TYPE /dmo/t_message.

    LOOP AT it_bookingsupplmement ASSIGNING FIELD-SYMBOL(<fs_travel>)
                                            GROUP BY <fs_travel>-TravelID.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = <fs_travel>-%key-TravelID
        IMPORTING
          es_travel    = ls_travel_out
          et_messages  = lt_message.

      IF lt_message IS INITIAL.
        LOOP AT GROUP <fs_travel> ASSIGNING FIELD-SYMBOL(<fs_bookingsupplement>).
          INSERT VALUE #( source-%key = <fs_bookingsupplement>-%key
                          target-%key = ls_travel_out-travel_id )
          INTO TABLE et_link_table.

          IF iv_full_requested = abap_true.
            ls_travel = CORRESPONDING #(  ls_travel_out MAPPING TO ENTITY ).
            INSERT ls_travel INTO TABLE et_travel.
          ENDIF.

        ENDLOOP.

      ELSE. "fill failed table in case of error
        failed-bookingsupplement = VALUE #(  BASE failed-bookingsupplement
                              FOR msg IN lt_message ( %key-TravelID            = <fs_travel>-%key-TravelID
                                                      %key-BookingID           = <fs_travel>-%key-BookingID
                                                      %key-BookingSupplementID = <fs_travel>-%key-BookingSupplementID
                                                      %fail-cause              = COND #( WHEN msg-msgty = 'E' AND ( msg-msgno = '016' OR msg-msgno = '009' )
                                                                                         THEN if_abap_behv=>cause-not_found
                                                                                         ELSE if_abap_behv=>cause-unspecific ) ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
