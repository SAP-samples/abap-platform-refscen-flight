CLASS /dmo/cl_travel_auxiliary_m DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

*   Type definition for import parameters --------------------------
    TYPES tt_travel_id                 TYPE TABLE OF /dmo/travel_id.
    TYPES tt_travel_reported            TYPE TABLE FOR REPORTED /dmo/i_travel_m.
    TYPES tt_booking_reported           TYPE TABLE FOR REPORTED  /dmo/i_booking_m.
    TYPES tt_bookingsupplement_reported TYPE TABLE FOR REPORTED  /dmo/i_booksuppl_m.


  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-METHODS new_message
      IMPORTING id         TYPE symsgid
                number     TYPE symsgno
                severity   TYPE if_abap_behv_message=>t_severity
                v1         TYPE simple OPTIONAL
                v2         TYPE simple OPTIONAL
                v3         TYPE simple OPTIONAL
                v4         TYPE simple OPTIONAL
      RETURNING VALUE(obj) TYPE REF TO if_abap_behv_message .

ENDCLASS.


CLASS /dmo/cl_travel_auxiliary_m IMPLEMENTATION.



  METHOD new_message.
    obj = NEW lcl_abap_behv_msg(
      textid = VALUE #(
                 msgid = id
                 msgno = number
                 attr1 = COND #( WHEN v1 IS NOT INITIAL THEN 'IF_T100_DYN_MSG~MSGV1' )
                 attr2 = COND #( WHEN v2 IS NOT INITIAL THEN 'IF_T100_DYN_MSG~MSGV2' )
                 attr3 = COND #( WHEN v3 IS NOT INITIAL THEN 'IF_T100_DYN_MSG~MSGV3' )
                 attr4 = COND #( WHEN v4 IS NOT INITIAL THEN 'IF_T100_DYN_MSG~MSGV4' )
      )
      msgty = SWITCH #( severity
                WHEN if_abap_behv_message=>severity-error       THEN 'E'
                WHEN if_abap_behv_message=>severity-warning     THEN 'W'
                WHEN if_abap_behv_message=>severity-information THEN 'I'
                WHEN if_abap_behv_message=>severity-success     THEN 'S' )
      msgv1 = |{ v1 }|
      msgv2 = |{ v2 }|
      msgv3 = |{ v3 }|
      msgv4 = |{ v4 }|
    ).
    obj->m_severity = severity.
  ENDMETHOD.


ENDCLASS.
