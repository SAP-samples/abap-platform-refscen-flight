CLASS ltc_general DEFINITION DEFERRED FOR TESTING.

"! Implentation of general functionality such as basic validations and determinations.
CLASS lhc_general DEFINITION INHERITING FROM cl_abap_behavior_handler FRIENDS ltc_general.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF state_areas,
        validation_currency_exists TYPE string VALUE 'VALIDATIONCURRENCYEXISTS' ##NO_TEXT,
        validation_name_given      TYPE string VALUE 'VALIDATIONNAMEGIVEN' ##NO_TEXT,
        validation_salary_positive TYPE string VALUE 'VALIDATIONSALARYPOSITIVE' ##NO_TEXT,
      END OF state_areas.

    METHODS validationcurrencyexists FOR VALIDATE ON SAVE
      IMPORTING keys FOR employee~validationcurrencyexists.

    METHODS validationnamegiven FOR VALIDATE ON SAVE
      IMPORTING keys FOR employee~validationnamegiven.

    METHODS validationsalarypositive FOR VALIDATE ON SAVE
      IMPORTING keys FOR employee~validationsalarypositive.

ENDCLASS.

CLASS lhc_general IMPLEMENTATION.

  METHOD validationcurrencyexists.
    READ ENTITIES OF /dmo/r_agencytp_hd IN LOCAL MODE
      ENTITY employee
          FIELDS ( salarycurrency )
          WITH CORRESPONDING #( keys )
        RESULT DATA(employees)
      ENTITY employee BY \_agency
          FROM CORRESPONDING #( keys )
        LINK DATA(employee_agency_links).

    DATA: currencies TYPE SORTED TABLE OF i_currency WITH UNIQUE KEY currency.

    currencies = CORRESPONDING #(  employees DISCARDING DUPLICATES MAPPING currency = salarycurrency EXCEPT * ).
    DELETE currencies WHERE currency IS INITIAL.

    IF currencies IS NOT INITIAL.
      SELECT FROM i_currency FIELDS currency
        FOR ALL ENTRIES IN @currencies
        WHERE currency = @currencies-currency
        INTO TABLE @DATA(currency_db).
    ENDIF.


    LOOP AT employees INTO DATA(employee).
      APPEND VALUE #(
          %tky               = employee-%tky
          %state_area        = state_areas-validation_currency_exists
        ) TO reported-employee.
      IF employee-salarycurrency IS INITIAL.
        APPEND VALUE #( %tky = employee-%tky ) TO failed-employee.
        APPEND VALUE #(
            %tky                    = employee-%tky
            %state_area             = state_areas-validation_currency_exists
            %element-salarycurrency = if_abap_behv=>mk-on
            %path                   = VALUE #(
                agency-%tky = employee_agency_links[
                    KEY id
                    source-%tky = employee-%tky
                  ]-target-%tky
              )
            %msg                    = NEW /dmo/cm_flight_messages(
                textid    = /dmo/cm_flight_messages=>currency_required
                severity  = if_abap_behv_message=>severity-error
              )
          ) TO reported-employee.
      ELSEIF NOT line_exists( currency_db[ currency = employee-salarycurrency ] ).
        APPEND VALUE #( %tky = employee-%tky ) TO failed-employee.
        APPEND VALUE #(
            %tky                    = employee-%tky
            %state_area             = state_areas-validation_currency_exists
            %element-salarycurrency = if_abap_behv=>mk-on
            %path                   = VALUE #(
                agency-%tky = employee_agency_links[
                    KEY id
                    source-%tky = employee-%tky
                  ]-target-%tky
              )
            %msg                    = NEW /dmo/cm_flight_messages(
                textid        = /dmo/cm_flight_messages=>currency_not_existing
                severity      = if_abap_behv_message=>severity-error
                currency_code = employee-salarycurrency
              )
          ) TO reported-employee.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validationnamegiven.
    READ ENTITIES OF /dmo/r_agencytp_hd IN LOCAL MODE
      ENTITY employee
          FIELDS ( firstname lastname )
          WITH CORRESPONDING #( keys )
        RESULT DATA(employees)
      ENTITY employee BY \_agency
          FROM CORRESPONDING #( keys )
        LINK DATA(employee_agency_links).

    LOOP AT employees INTO DATA(employee).
      APPEND VALUE #(
          %tky               = employee-%tky
          %state_area        = state_areas-validation_name_given
        ) TO reported-employee.
      IF employee-firstname IS INITIAL AND employee-lastname IS INITIAL.
        APPEND VALUE #( %tky = employee-%tky ) TO failed-employee.
        APPEND VALUE #(
            %tky               = employee-%tky
            %state_area        = state_areas-validation_name_given
            %element-firstname = if_abap_behv=>mk-on
            %element-lastname  = if_abap_behv=>mk-on
            %path              = VALUE #(
                agency-%tky = employee_agency_links[
                    KEY id
                    source-%tky = employee-%tky
                  ]-target-%tky
              )
            %msg               = NEW /dmo/cx_hierarchy(
                textid    = /dmo/cx_hierarchy=>names_given
                severity  = if_abap_behv_message=>severity-error
              )
          ) TO reported-employee.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validationsalarypositive.
    READ ENTITIES OF /dmo/r_agencytp_hd IN LOCAL MODE
      ENTITY employee
          FIELDS ( salary )
          WITH CORRESPONDING #( keys )
        RESULT DATA(employees)
      ENTITY employee BY \_agency
          FROM CORRESPONDING #( keys )
        LINK DATA(employee_agency_links).

    LOOP AT employees INTO DATA(employee).
      APPEND VALUE #(
          %tky               = employee-%tky
          %state_area        = state_areas-validation_salary_positive
        ) TO reported-employee.
      IF employee-salary <= '0'.
        APPEND VALUE #( %tky = employee-%tky ) TO failed-employee.
        APPEND VALUE #(
            %tky            = employee-%tky
            %state_area     = state_areas-validation_salary_positive
            %element-salary = if_abap_behv=>mk-on
            %path           = VALUE #(
                agency-%tky = employee_agency_links[
                    KEY id
                    source-%tky = employee-%tky
                  ]-target-%tky
              )
            %msg            = NEW /dmo/cx_hierarchy(
                textid    = /dmo/cx_hierarchy=>invalid_salary
                severity  = if_abap_behv_message=>severity-error
              )
          ) TO reported-employee.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
