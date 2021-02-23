**********************************************************************
*
* Handler class implements UPDATE and DELETE for booking supplements
*
**********************************************************************
CLASS lhc_supplement DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    TYPES:
      tt_bookingsupplement_update TYPE TABLE FOR UPDATE    /dmo/i_bookingsupplement_u.


    METHODS:
      update FOR MODIFY
        IMPORTING entities FOR UPDATE bookingsupplement,
      delete FOR MODIFY
        IMPORTING entities FOR DELETE bookingsupplement,
      read FOR READ
        IMPORTING keys FOR READ bookingsupplement
        RESULT    result,
      rba_travel FOR READ
        IMPORTING keys_rba FOR READ bookingsupplement\_travel FULL result_requested
        RESULT result LINK association_links.



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
  METHOD update.

    DATA messages TYPE /dmo/t_message.
    DATA book_supplements TYPE /dmo/book_suppl.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<bookingsupplement>).
      book_supplements = CORRESPONDING   #( <bookingsupplement> MAPPING FROM ENTITY ).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel              = VALUE /dmo/s_travel_in( travel_id = <bookingsupplement>-travelid )
          is_travelx             = VALUE /dmo/s_travel_inx( travel_id = <bookingsupplement>-travelid )
          it_booking_supplement  = VALUE /dmo/t_booking_supplement_in( ( CORRESPONDING #( book_supplements ) ) )
          it_booking_supplementx = VALUE /dmo/t_booking_supplement_inx( ( _fill_bookingsupplement_inx( <bookingsupplement> ) ) )
        IMPORTING
          et_messages            = messages.


      /dmo/cl_travel_auxiliary=>handle_booksupplement_messages(
        EXPORTING
          iv_cid                  = <bookingsupplement>-%cid_ref
          iv_travel_id            = <bookingsupplement>-travelid
          iv_booking_id           = <bookingsupplement>-bookingid
          iv_bookingsupplement_id = <bookingsupplement>-bookingsupplementid
          it_messages             = messages
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
  METHOD delete.

    DATA messages TYPE /dmo/t_message.

    LOOP AT entities INTO DATA(bookingsupplement).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel              = VALUE /dmo/s_travel_in( travel_id = bookingsupplement-travelid )
          is_travelx             = VALUE /dmo/s_travel_inx( travel_id = bookingsupplement-travelid )
          it_booking             = VALUE /dmo/t_booking_in( ( booking_id = bookingsupplement-bookingid ) )
          it_bookingx            = VALUE /dmo/t_booking_inx( ( booking_id = bookingsupplement-bookingid ) )
          it_booking_supplement  = VALUE /dmo/t_booking_supplement_in( (  booking_supplement_id = bookingsupplement-bookingSupplementid
                                                                          booking_id            = bookingsupplement-BookingID ) )
          it_booking_supplementx = VALUE /dmo/t_booking_supplement_inx( ( booking_supplement_id = bookingsupplement-bookingsupplementid
                                                                          booking_id            = bookingsupplement-bookingid
                                                                          action_code           = /dmo/if_flight_legacy=>action_code-delete ) )
        IMPORTING
          et_messages            = messages.

      IF messages IS NOT INITIAL.

        /dmo/cl_travel_auxiliary=>handle_booksupplement_messages(
         EXPORTING
           iv_cid                  = bookingsupplement-%cid_ref
           iv_travel_id            = bookingsupplement-travelid
           iv_booking_id           = bookingsupplement-bookingid
           iv_bookingsupplement_id = bookingsupplement-bookingsupplementid
           it_messages = messages
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
  METHOD read.

    DATA: travels_out    TYPE /dmo/travel,
          booksuppls_out TYPE /dmo/t_booking_supplement,
          messages       TYPE /dmo/t_message.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<booksuppl_by_travel>)
                                      GROUP BY <booksuppl_by_travel>-TravelID.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id          = <booksuppl_by_travel>-travelid
        IMPORTING
          es_travel             = travels_out
          et_booking_supplement = booksuppls_out
          et_messages           = messages.

      IF messages IS INITIAL.
        LOOP AT GROUP <booksuppl_by_travel> ASSIGNING FIELD-SYMBOL(<bookingsupplement>)
                                       GROUP BY <bookingsupplement>-%key.

          READ TABLE booksuppls_out INTO DATA(bookingsuppl) WITH KEY  travel_id             = <bookingsupplement>-%key-TravelID
                                                                         booking_id            = <bookingsupplement>-%key-BookingID
                                                                         booking_supplement_id = <bookingsupplement>-%key-BookingSupplementID.
          IF sy-subrc = 0 .
            "fill result parameter with flagged fields
            INSERT
               VALUE #( travelid            = bookingsuppl-travel_id
                        bookingid           = bookingsuppl-booking_id
                        bookingsupplementid = bookingsuppl-booking_supplement_id
                        supplementid        = COND #( WHEN <bookingsupplement>-%control-SupplementID        = cl_abap_behv=>flag_changed THEN bookingsuppl-supplement_id  )
                        price               = COND #( WHEN <bookingsupplement>-%control-Price               = cl_abap_behv=>flag_changed THEN bookingsuppl-price  )
                        currencycode        = COND #( WHEN <bookingsupplement>-%control-CurrencyCode        = cl_abap_behv=>flag_changed THEN bookingsuppl-currency_code  )
*                        lastchangedat       = COND #( WHEN <bookingsupplement>-%control-LastChangedAt       = cl_abap_behv=>flag_changed THEN ls_travel_out-lastchangedat   )
                      ) INTO TABLE result.



          ELSE.
            "BookingSupplementID not found
            INSERT
              VALUE #( travelid           = <bookingsupplement>-TravelID
                       bookingid          = <bookingsupplement>-BookingID
                       bookingsupplementid = <bookingsupplement>-BookingSupplementID
                       %fail-cause = if_abap_behv=>cause-not_found )
              INTO TABLE failed-bookingsupplement.
          ENDIF.
        ENDLOOP.
      ELSE.

        "TravelID not found or other fail cause
        LOOP AT GROUP <booksuppl_by_travel> ASSIGNING <bookingsupplement>.
          failed-bookingsupplement = VALUE #(  BASE failed-bookingsupplement
                                     FOR msg IN messages ( %key-TravelID            = <bookingsupplement>-TravelID
                                                           %key-BookingID           = <bookingsupplement>-BookingID
                                                           %key-bookingsupplementid = <bookingsupplement>-BookingSupplementID
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
  METHOD rba_travel.
    DATA: travel_out TYPE /dmo/travel,
          travel     LIKE LINE OF result,
          messages    TYPE /dmo/t_message.

    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<booksuppl_by_travel>)
                                            GROUP BY <booksuppl_by_travel>-TravelID.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = <booksuppl_by_travel>-%key-TravelID
        IMPORTING
          es_travel    = travel_out
          et_messages  = messages.

      IF messages IS INITIAL.
        LOOP AT GROUP <booksuppl_by_travel> ASSIGNING FIELD-SYMBOL(<bookingsupplement>).
          INSERT VALUE #( source-%key = <bookingsupplement>-%key
                          target-%key = travel_out-travel_id )
          INTO TABLE association_links.

          IF result_requested = abap_true.
            travel = CORRESPONDING #(  travel_out MAPPING TO ENTITY ).
            INSERT travel INTO TABLE result.
          ENDIF.

        ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM association_links COMPARING ALL FIELDS.
      DELETE ADJACENT DUPLICATES FROM result COMPARING ALL FIELDS.

      ELSE. "fill failed table in case of error
        failed-bookingsupplement = VALUE #(  BASE failed-bookingsupplement
                              FOR msg IN messages ( %key-TravelID            = <booksuppl_by_travel>-%key-TravelID
                                                    %key-BookingID           = <booksuppl_by_travel>-%key-BookingID
                                                    %key-BookingSupplementID = <booksuppl_by_travel>-%key-BookingSupplementID
                                                    %fail-cause              = COND #( WHEN msg-msgty = 'E' AND ( msg-msgno = '016' OR msg-msgno = '009' )
                                                                                         THEN if_abap_behv=>cause-not_found
                                                                                         ELSE if_abap_behv=>cause-unspecific ) ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.



ENDCLASS.
