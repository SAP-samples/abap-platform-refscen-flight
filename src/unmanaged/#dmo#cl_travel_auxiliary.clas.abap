CLASS /dmo/cl_travel_auxiliary DEFINITION
  INHERITING FROM cl_abap_behv
  PUBLIC
  FINAL
  CREATE PUBLIC .


  PUBLIC SECTION.


    "Type definition for import parameters --------------------------
    TYPES tt_travel_create              TYPE TABLE FOR CREATE   /dmo/i_travel_u.
    TYPES tt_travel_update              TYPE TABLE FOR UPDATE   /dmo/i_travel_u.
    TYPES tt_travel_delete              TYPE TABLE FOR DELETE   /dmo/i_travel_u.

    TYPES tt_travel_failed              TYPE TABLE FOR FAILED   /dmo/i_travel_u.
    TYPES tt_travel_mapped              TYPE TABLE FOR MAPPED   /dmo/i_travel_u.
    TYPES tt_travel_reported            TYPE TABLE FOR REPORTED /dmo/i_travel_u.

    "TYPES tt_booking_create             TYPE TABLE FOR CREATE    /dmo/i_booking_u.
    TYPES tt_booking_update             TYPE TABLE FOR UPDATE    /dmo/i_booking_u.
    TYPES tt_booking_delete             TYPE TABLE FOR DELETE    /dmo/i_booking_u.

    TYPES tt_booking_failed             TYPE TABLE FOR FAILED    /dmo/i_booking_u.
    TYPES tt_booking_mapped             TYPE TABLE FOR MAPPED    /dmo/i_booking_u.
    TYPES tt_booking_reported           TYPE TABLE FOR REPORTED  /dmo/i_booking_u.

    TYPES tt_bookingsupplement_failed   TYPE TABLE FOR FAILED    /dmo/i_bookingsupplement_u.
    TYPES tt_bookingsupplement_mapped   TYPE TABLE FOR MAPPED    /dmo/i_bookingsupplement_u.
    TYPES tt_bookingsupplement_reported TYPE TABLE FOR REPORTED  /dmo/i_bookingsupplement_u.




    CLASS-METHODS handle_travel_messages
      IMPORTING
        iv_cid       TYPE string   OPTIONAL
        iv_travel_id TYPE /dmo/travel_id OPTIONAL
        it_messages  TYPE /dmo/t_message
      CHANGING
        failed       TYPE tt_travel_failed
        reported     TYPE tt_travel_reported.


    CLASS-METHODS handle_booking_messages
      IMPORTING
        cid        TYPE string OPTIONAL
        travel_id  TYPE /dmo/travel_id OPTIONAL
        booking_id TYPE /dmo/booking_id OPTIONAL
        messages   TYPE /dmo/t_message
      CHANGING
        failed        TYPE tt_booking_failed
        reported      TYPE tt_booking_reported.


    CLASS-METHODS handle_booksupplement_messages
      IMPORTING
        iv_cid                  TYPE string OPTIONAL
        iv_travel_id            TYPE /dmo/travel_id OPTIONAL
        iv_booking_id           TYPE /dmo/booking_id OPTIONAL
        iv_bookingsupplement_id TYPE /dmo/booking_supplement_id OPTIONAL
        it_messages             TYPE /dmo/t_message
      CHANGING
        failed                  TYPE tt_bookingsupplement_failed
        reported                TYPE tt_bookingsupplement_reported.



  PRIVATE SECTION.

    CLASS-DATA obj TYPE REF TO /dmo/cl_travel_auxiliary.

    CLASS-METHODS get_message_object
      RETURNING VALUE(r_result) TYPE REF TO /dmo/cl_travel_auxiliary.

    CLASS-METHODS get_cause_from_message
      IMPORTING
                iv_msgid            TYPE symsgid
                iv_msgno            TYPE symsgno
      RETURNING VALUE(r_fail_cause) TYPE if_abap_behv=>t_fail_cause.

ENDCLASS.



CLASS /dmo/cl_travel_auxiliary IMPLEMENTATION.


  METHOD get_message_object.

    IF obj IS INITIAL.
      CREATE OBJECT obj.
    ENDIF.
    r_result = obj.

  ENDMETHOD.


  METHOD handle_booking_messages.

    LOOP AT messages INTO DATA(message) WHERE msgty = 'E' OR msgty = 'A'.
      APPEND VALUE #( %cid        = cid
                      travelid    = travel_id
                      bookingid   = booking_id
                      %fail-cause = get_cause_from_message(
                                      iv_msgid = message-msgid
                                      iv_msgno = message-msgno
                                    ) ) TO failed.

      APPEND VALUE #( %msg = get_message_object( )->new_message(
                                          id       = message-msgid
                                          number   = message-msgno
                                          severity = if_abap_behv_message=>severity-error
                                          v1       = message-msgv1
                                          v2       = message-msgv2
                                          v3       = message-msgv3
                                          v4       = message-msgv4 )
                      %key-TravelID = travel_id
                      %cid          = cid
                      TravelID      = travel_id
                      BookingID     = booking_id ) TO reported.

    ENDLOOP.

  ENDMETHOD.


  METHOD handle_booksupplement_messages.

    LOOP AT it_messages INTO DATA(ls_message) WHERE msgty = 'E' OR msgty = 'A'.
      APPEND VALUE #( %cid                = iv_cid
                      travelid            = iv_travel_id
                      bookingid           = iv_booking_id
                      bookingsupplementid = iv_bookingsupplement_id
                      %fail-cause         = get_cause_from_message(
                                              iv_msgid = ls_message-msgid
                                              iv_msgno = ls_message-msgno
                                            ) )
                     TO failed.

      APPEND VALUE #( %key-TravelID = iv_travel_id
                      %cid      = iv_cid
                      TravelID  = iv_travel_id
                      BookingID = iv_booking_id
                      %msg      = get_message_object( )->new_message(
                                               id       = ls_message-msgid
                                               number   = ls_message-msgno
                                               severity = if_abap_behv_message=>severity-error
                                               v1       = ls_message-msgv1
                                               v2       = ls_message-msgv2
                                               v3       = ls_message-msgv3
                                               v4       = ls_message-msgv4 )
                    ) TO reported.
    ENDLOOP.

  ENDMETHOD.


  METHOD handle_travel_messages.

    LOOP AT it_messages INTO DATA(ls_message) WHERE msgty = 'E' OR msgty = 'A'.

      APPEND VALUE #( %cid        = iv_cid
                      travelid    = iv_travel_id
                      %fail-cause = get_cause_from_message(
                                      iv_msgid = ls_message-msgid
                                      iv_msgno = ls_message-msgno
                                    ) )
             TO failed.

      APPEND VALUE #( %msg          = get_message_object( )->new_message(
                                               id       = ls_message-msgid
                                               number   = ls_message-msgno
                                               severity = if_abap_behv_message=>severity-error
                                               v1       = ls_message-msgv1
                                               v2       = ls_message-msgv2
                                               v3       = ls_message-msgv3
                                               v4       = ls_message-msgv4 )
                      %key-TravelID = iv_travel_id
                      %cid          = iv_cid
                      TravelID      = iv_travel_id )
             TO reported.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_cause_from_message.

    r_fail_cause = if_abap_behv=>cause-unspecific.

    IF iv_msgid = '/DMO/CM_FLIGHT_LEGAC'.
      CASE iv_msgno.
        WHEN '009' OR '016' OR '017' OR '021'.
          r_fail_cause = if_abap_behv=>cause-not_found.
        WHEN '032'.
          r_fail_cause = if_abap_behv=>cause-locked.
        WHEN '046'.
          r_fail_cause = if_abap_behv=>cause-unauthorized.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
