CLASS /dmo/cl_travel_auxiliary DEFINITION
  INHERITING FROM cl_abap_behv
  PUBLIC
  FINAL
  CREATE PUBLIC .


  PUBLIC SECTION.

    "! Calculates the <em>fail_cause</em> according to the message class and ID.  If the the message is raised in a dependent
    "! like <em>Create by Association</em> this is also taken in account.
    "!
    "! @parameter msgid | <p class="shorttext synchronized" lang="en">Message class</p>
    "! @parameter msgno | <p class="shorttext synchronized" lang="en">Message ID</p>
    "! @parameter is_dependend | <p class="shorttext synchronized" lang="en">Is this message raised in a dependent situation</p>
    "! @parameter fail_cause | <p class="shorttext synchronized" lang="en"></p>
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
