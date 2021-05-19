CLASS /dmo/cl_travel_auxiliary DEFINITION
  INHERITING FROM cl_abap_behv
  PUBLIC
  FINAL
  CREATE PUBLIC .


  PUBLIC SECTION.

    CLASS-METHODS get_cause_from_message
      IMPORTING
        msgid             TYPE symsgid
        msgno             TYPE symsgno
        is_dependend      TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(fail_cause) TYPE if_abap_behv=>t_fail_cause.

ENDCLASS.



CLASS /dmo/cl_travel_auxiliary IMPLEMENTATION.

  METHOD get_cause_from_message.
    fail_cause = if_abap_behv=>cause-unspecific.

    IF msgid = '/DMO/CM_FLIGHT_LEGAC'.
      CASE msgno.
        WHEN '009'  "Travel Key initial
          OR '016'  "Travel does not exist
          OR '017'  "Booking does not exist
          OR '021'. "BookingSupplement does not exist
          IF is_dependend = abap_true.
            fail_cause = if_abap_behv=>cause-unspecific.
          ELSE.
            fail_cause = if_abap_behv=>cause-not_found.
          ENDIF.
        WHEN '032'. "Travel is locked by
          fail_cause = if_abap_behv=>cause-locked.
        WHEN '046'. "You are not authorized
          fail_cause = if_abap_behv=>cause-unauthorized.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
