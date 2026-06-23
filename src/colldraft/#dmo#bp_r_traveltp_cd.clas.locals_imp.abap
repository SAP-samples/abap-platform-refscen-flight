CLASS lhc_Travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Travel RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Travel RESULT result.

    METHODS is_create_granted
      RETURNING VALUE(create_granted) TYPE abap_bool.

    METHODS is_update_granted
      IMPORTING iv_overall_status     TYPE /dmo/overall_status OPTIONAL
      RETURNING VALUE(update_granted) TYPE abap_bool.

    METHODS is_delete_granted
      IMPORTING iv_overall_status     TYPE /dmo/overall_status OPTIONAL
      RETURNING VALUE(delete_granted) TYPE abap_bool.

ENDCLASS.

CLASS lhc_Travel IMPLEMENTATION.
  METHOD get_global_authorizations.
    IF requested_authorizations-%create = if_abap_behv=>mk-on.
      IF is_create_granted( ) = abap_true.
        result-%create = if_abap_behv=>auth-allowed.
      ELSE.
        result-%create = if_abap_behv=>auth-unauthorized.
        APPEND VALUE #( %msg    = NEW /dmo/cm_flight_messages( textid   = /dmo/cm_flight_messages=>not_authorized
                                                               severity = if_abap_behv_message=>severity-error )
                        %global = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
    ENDIF.

    " Edit is treated like update
    IF    requested_authorizations-%update      = if_abap_behv=>mk-on
       OR requested_authorizations-%action-Edit = if_abap_behv=>mk-on.
      IF is_update_granted( ) = abap_true.
        result-%update      = if_abap_behv=>auth-allowed.
        result-%action-Edit = if_abap_behv=>auth-allowed.
      ELSE.
        result-%update      = if_abap_behv=>auth-unauthorized.
        result-%action-Edit = if_abap_behv=>auth-unauthorized.
        APPEND VALUE #( %msg    = NEW /dmo/cm_flight_messages( textid   = /dmo/cm_flight_messages=>not_authorized
                                                               severity = if_abap_behv_message=>severity-error )
                        %global = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
    ENDIF.

    IF requested_authorizations-%delete = if_abap_behv=>mk-on.
      IF is_delete_granted( ) = abap_true.
        result-%delete = if_abap_behv=>auth-allowed.
      ELSE.
        result-%delete = if_abap_behv=>auth-unauthorized.
        APPEND VALUE #( %msg    = NEW /dmo/cm_flight_messages( textid   = /dmo/cm_flight_messages=>not_authorized
                                                               severity = if_abap_behv_message=>severity-error )
                        %global = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_instance_authorizations.
    DATA update_requested TYPE abap_bool.
    DATA delete_requested TYPE abap_bool.
    DATA update_granted   TYPE abap_bool.
    DATA delete_granted   TYPE abap_bool.

    READ ENTITIES OF /DMO/R_TravelTP_CD IN LOCAL MODE
         ENTITY Travel
         FIELDS ( OverallStatus )
         WITH CORRESPONDING #( keys )
         RESULT DATA(travels)
         FAILED failed.

    IF travels IS INITIAL.
      RETURN.
    ENDIF.

    update_requested = COND #( WHEN requested_authorizations-%update      = if_abap_behv=>mk-on
                                 OR requested_authorizations-%action-Edit = if_abap_behv=>mk-on
                               THEN abap_true
                               ELSE abap_false ).

    delete_requested = COND #( WHEN requested_authorizations-%delete = if_abap_behv=>mk-on
                               THEN abap_true
                               ELSE abap_false ).

    LOOP AT travels INTO DATA(travel).

      IF update_requested = abap_true.
        update_granted = is_update_granted( travel-OverallStatus ).
        IF update_granted = abap_false.
          APPEND VALUE #( %tky = travel-%tky
                          %msg = NEW /dmo/cm_flight_messages( textid   = /dmo/cm_flight_messages=>not_authorized
                                                              severity = if_abap_behv_message=>severity-error ) )
                 TO reported-travel.
        ENDIF.
      ENDIF.

      IF delete_requested = abap_true.
        delete_granted = is_delete_granted( travel-OverallStatus ).
        IF delete_granted = abap_false.
          APPEND VALUE #( %tky = travel-%tky
                          %msg = NEW /dmo/cm_flight_messages( textid   = /dmo/cm_flight_messages=>not_authorized
                                                              severity = if_abap_behv_message=>severity-error ) )
                 TO reported-travel.
        ENDIF.
      ENDIF.

      DATA(upd_auth) = COND #( WHEN update_granted = abap_true
                               THEN if_abap_behv=>auth-allowed
                               ELSE if_abap_behv=>auth-unauthorized ).
      DATA(del_auth) = COND #( WHEN delete_granted = abap_true
                               THEN if_abap_behv=>auth-allowed
                               ELSE if_abap_behv=>auth-unauthorized ).
      APPEND VALUE #( %tky         = travel-%tky
                      %update      = upd_auth
                      %action-Edit = upd_auth
                      %delete      = del_auth ) TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_create_granted.
    AUTHORITY-CHECK OBJECT '/DMO/TR_CD'
                    ID '/DMO/OAST' DUMMY
                    ID 'ACTVT'     FIELD '01'.
    create_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    " Simulation for full authorization
    "(not to be used in productive code)
    create_granted = abap_true.
  ENDMETHOD.

  METHOD is_update_granted.
    " For instance authorization
    IF iv_overall_status IS SUPPLIED.
      AUTHORITY-CHECK OBJECT '/DMO/TR_CD'
                      ID '/DMO/OAST' FIELD iv_overall_status
                      ID 'ACTVT'     FIELD '02'.
      update_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      " Simulation for full authorization
      "(not to be used in productive code)
      update_granted = abap_true.

    " For global authorization
    ELSE.
      AUTHORITY-CHECK OBJECT '/DMO/TR_CD'
                      ID '/DMO/OAST' DUMMY
                      ID 'ACTVT'     FIELD '02'.
      update_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      " Simulation for full authorization
      "(not to be used in productive code)
      update_granted = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD is_delete_granted.
    " For instance authorization
    IF iv_overall_status IS SUPPLIED.
      AUTHORITY-CHECK OBJECT '/DMO/TR_CD'
                      ID '/DMO/OAST' FIELD iv_overall_status
                      ID 'ACTVT'     FIELD '06'.
      delete_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      " Simulation for full authorization
      "(not to be used in productive code)
      delete_granted = abap_true.

    " For global authorization
    ELSE.
      AUTHORITY-CHECK OBJECT '/DMO/TR_CD'
                      ID '/DMO/OAST' DUMMY
                      ID 'ACTVT'     FIELD '06'.
      delete_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      " Simulation for full authorization
      "(not to be used in productive code)
      delete_granted = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
