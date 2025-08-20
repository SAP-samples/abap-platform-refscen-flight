INTERFACE lif_numbering.
  TYPES:
    tt_agency_create TYPE TABLE FOR CREATE /dmo/r_agencytp_hd,
    tt_reported      TYPE RESPONSE FOR REPORTED EARLY /dmo/r_agencytp_hd,
    tt_failed        TYPE RESPONSE FOR FAILED EARLY /dmo/r_agencytp_hd.

  METHODS:
    reserve_numbers
      IMPORTING
        quantity TYPE i
      EXPORTING
        message  TYPE REF TO if_abap_behv_message
        success  TYPE abap_boolean,

    get_lowest_reserved_number
      RETURNING
        VALUE(result) TYPE /dmo/agency_id.
ENDINTERFACE.

CLASS lcl_numbering_by_numberrange DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_numbering.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      number_range_key               TYPE cl_numberrange_runtime=>nr_number,
      number_range_returned_quantity TYPE cl_numberrange_runtime=>nr_returned_quantity.

ENDCLASS.

CLASS lcl_numbering_by_numberrange IMPLEMENTATION.

  METHOD lif_numbering~get_lowest_reserved_number.
    result = number_range_key - number_range_returned_quantity + 1.
  ENDMETHOD.

  METHOD lif_numbering~reserve_numbers.
    success = abap_false.
    CLEAR message.

    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING
            nr_range_nr       = '01'
            object            = '/DMO/AGNHD'
            quantity          = CONV #( quantity )
          IMPORTING
            number            = number_range_key
            returncode        = DATA(number_range_return_code)
            returned_quantity = number_range_returned_quantity
        ).
      CATCH cx_number_ranges INTO DATA(lx_number_ranges).
        message = lx_number_ranges.
        success = abap_false.
        EXIT.
    ENDTRY.

    CASE number_range_return_code.
      WHEN '1'.
        " 1 - the returned number is in a critical range (specified under “percentage warning” in the object definition)
        message = NEW /dmo/cm_flight_messages(
                                      textid = /dmo/cm_flight_messages=>number_range_depleted
                                      severity = if_abap_behv_message=>severity-warning ).
        success = abap_true.

      WHEN '2' OR '3'.
        " 2 - the last number of the interval was returned
        " 3 - if fewer numbers are available than requested,  the return code is 3
        message = NEW /dmo/cm_flight_messages(
                                    textid = /dmo/cm_flight_messages=>not_sufficient_numbers
                                    severity = if_abap_behv_message=>severity-warning ).
        success = abap_false.
      WHEN OTHERS.
        success = abap_true.
    ENDCASE.

    IF success = abap_true.
      ASSERT quantity = number_range_returned_quantity.
    ENDIF.
  ENDMETHOD.

ENDCLASS.



CLASS ltc_agency DEFINITION DEFERRED FOR TESTING.

CLASS lhc_agency DEFINITION INHERITING FROM cl_abap_behavior_handler
  FRIENDS ltc_agency.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF state_areas,
        validate_name    TYPE string VALUE 'VALIDATE_NAME'       ##NO_TEXT,
        validate_email   TYPE string VALUE 'VALIDATE_EMAIL'      ##NO_TEXT,
        validate_country TYPE string VALUE 'VALIDATE_COUNTRY'    ##NO_TEXT,
      END OF state_areas.

    METHODS:
*      get_global_authorizations FOR GLOBAL AUTHORIZATION
*        REQUEST requested_authorizations FOR agency
*        RESULT result,

      earlynumbering_create FOR NUMBERING
        IMPORTING new_agencies FOR CREATE agency,

      earlynumbering_cba_employee FOR NUMBERING
        IMPORTING new_employees FOR CREATE agency\_employee,

      validatecountrycode FOR VALIDATE ON SAVE
        IMPORTING keys FOR agency~validatecountrycode,

      validateemailaddress FOR VALIDATE ON SAVE
        IMPORTING keys FOR agency~validateemailaddress,

      validatename FOR VALIDATE ON SAVE
        IMPORTING keys FOR agency~validatename.



    TYPES:
      tt_agency_create  TYPE TABLE FOR CREATE /dmo/r_agencytp_hd,
      tt_reported       TYPE RESPONSE FOR REPORTED EARLY /dmo/r_agencytp_hd,
      tt_failed         TYPE RESPONSE FOR FAILED EARLY /dmo/r_agencytp_hd,
      tt_employee_id    TYPE STANDARD TABLE OF /dmo/employee_id WITH DEFAULT KEY,
      tt_employee_cba   TYPE TABLE FOR CREATE /dmo/r_agencytp_hd\\agency\_employee,
      tt_employee_links TYPE TABLE FOR READ LINK /dmo/r_agencytp_hd\_employee.

    DATA:
      numbering TYPE REF TO lif_numbering.

    METHODS:
      _get_max_agency_id
        IMPORTING
          agencies_wo_id TYPE tt_agency_create
        EXPORTING
          result         TYPE /dmo/agency_id
        CHANGING
          failed         TYPE tt_failed
          reported       TYPE tt_reported,

      _handle_numbering_messages
        IMPORTING
          agencies_wo_id TYPE tt_agency_create
          message        TYPE REF TO if_abap_behv_message
          success        TYPE abap_boolean
        CHANGING
          failed         TYPE tt_failed
          reported       TYPE tt_reported,

      _get_max_employee_by_agency
        IMPORTING
          new_employees   TYPE tt_employee_cba
          exist_employees TYPE tt_employee_links
          agency          TYPE /dmo/agency_id
        RETURNING
          VALUE(result)   TYPE /dmo/employee_id,

      _get_max_exist_employ_by_agncy
        IMPORTING
          exist_employees TYPE tt_employee_links
          agency          TYPE /dmo/agency_id
        RETURNING
          VALUE(result)   TYPE /dmo/employee_id,

      _get_max_incom_employ_by_agncy
        IMPORTING
          new_employees TYPE tt_employee_cba
          agency        TYPE /dmo/agency_id
        RETURNING
          VALUE(result) TYPE /dmo/employee_id,

      _get_max_employee_id
        IMPORTING employees     TYPE tt_employee_id
        RETURNING VALUE(result) TYPE /dmo/employee_id.

ENDCLASS.

CLASS lhc_agency IMPLEMENTATION.

*  METHOD get_global_authorizations.
*  ENDMETHOD.

  METHOD earlynumbering_create.

    LOOP AT new_agencies INTO DATA(agency) WHERE agency IS NOT INITIAL.
      APPEND CORRESPONDING #( agency ) TO mapped-agency.
    ENDLOOP.

    DATA(agencies_wo_agencyid) = new_agencies.
    DELETE agencies_wo_agencyid WHERE agency IS NOT INITIAL.

    IF agencies_wo_agencyid IS INITIAL.
      EXIT.
    ENDIF.

    _get_max_agency_id(
        EXPORTING
          agencies_wo_id = agencies_wo_agencyid
        IMPORTING
          result         = DATA(agency_id_max)
        CHANGING
          failed         = failed
          reported       = reported
      ).

    IF agency_id_max = 0.
      EXIT.
    ENDIF.

    LOOP AT agencies_wo_agencyid INTO DATA(agency_wo_id).
      agency_wo_id-agency = agency_id_max .

      APPEND VALUE #(
          %cid      = agency_wo_id-%cid
          %is_draft = agency_wo_id-%is_draft
          %key      = agency_wo_id-%key
        ) TO mapped-agency.

      agency_id_max += 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD _get_max_agency_id.
    IF numbering IS NOT BOUND.
      numbering = NEW lcl_numbering_by_numberrange( ).
    ENDIF.

    numbering->reserve_numbers(
      EXPORTING
        quantity = lines( agencies_wo_id )
      IMPORTING
        message  = DATA(message)
        success  = DATA(success)
    ).

    _handle_numbering_messages(
        EXPORTING
          agencies_wo_id = agencies_wo_id
          message        = message
          success        = success
        CHANGING
          failed         = failed
          reported       = reported
      ).

    IF success = abap_false.
      result = 0.
      EXIT.
    ENDIF.


    result = numbering->get_lowest_reserved_number( ).
  ENDMETHOD.

  METHOD _handle_numbering_messages.
    IF message IS NOT BOUND.
      " No message, no problem.
      EXIT.
    ENDIF.

    LOOP AT agencies_wo_id INTO DATA(agency_wo_id).
      APPEND VALUE #( %cid = agency_wo_id-%cid
                      %key = agency_wo_id-%key
                      %msg = message
                    ) TO reported-agency.
      IF success = abap_false.
        APPEND VALUE #( %cid        = agency_wo_id-%cid
                        %key        = agency_wo_id-%key
                        %fail-cause = if_abap_behv=>cause-unspecific
                      ) TO failed-agency.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.







  METHOD earlynumbering_cba_employee.
    READ ENTITIES OF /dmo/r_agencytp_hd IN LOCAL MODE
      ENTITY agency BY \_employee
        FROM CORRESPONDING #( new_employees )
        LINK DATA(exist_employees).

    LOOP AT new_employees ASSIGNING FIELD-SYMBOL(<agency>).

      DATA(max_employee_id_by_agency) = _get_max_employee_by_agency(
          new_employees   = new_employees
          exist_employees = exist_employees
          agency          = <agency>-agency
        ).

      LOOP AT <agency>-%target ASSIGNING FIELD-SYMBOL(<employee_wo_numbers>).
        APPEND CORRESPONDING #( <employee_wo_numbers> ) TO mapped-employee ASSIGNING FIELD-SYMBOL(<mapped_employee>).
        IF <employee_wo_numbers>-employee IS INITIAL.
          max_employee_id_by_agency += 1 .
          <mapped_employee>-employee = max_employee_id_by_agency.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.


  METHOD _get_max_employee_by_agency.

    DATA(max_exist_employee_by_agncy) = _get_max_exist_employ_by_agncy( exist_employees = exist_employees  agency = agency ).

    DATA(max_new_employee_by_agncy) = _get_max_incom_employ_by_agncy( new_employees = new_employees agency = agency ).

    result = COND /dmo/employee_id(
        WHEN max_exist_employee_by_agncy > max_new_employee_by_agncy
          THEN max_exist_employee_by_agncy
          ELSE max_new_employee_by_agncy
      ).

  ENDMETHOD.

  METHOD _get_max_incom_employ_by_agncy.
    DATA(filtered_incom_employ_by_agncy) = VALUE tt_employee_id(
        FOR  entity IN new_employees USING KEY entity WHERE ( agency  = agency )
        FOR  target IN entity-%target
        ( target-employee )
      ).

    result = _get_max_employee_id( filtered_incom_employ_by_agncy ).
  ENDMETHOD.

  METHOD _get_max_exist_employ_by_agncy.
    DATA(filtered_exist_employ_by_agncy) = VALUE tt_employee_id(
        FOR employee IN exist_employees USING KEY entity WHERE ( source-agency = agency )
        ( employee-target-employee )
      ).

    result = _get_max_employee_id( filtered_exist_employ_by_agncy ).
  ENDMETHOD.

  METHOD _get_max_employee_id.
    result = REDUCE #( INIT max = CONV /dmo/employee_id( '0' )
                       FOR  employee IN employees
                       NEXT max = COND /dmo/employee_id( WHEN employee > max
                                                           THEN employee
                                                           ELSE max )
                    ).
  ENDMETHOD.


  METHOD validatecountrycode.
    DATA:
      countries TYPE SORTED TABLE OF i_country WITH UNIQUE KEY country.

    READ ENTITIES OF /dmo/r_agencytp_hd IN LOCAL MODE
      ENTITY agency
        FIELDS ( countrycode )
        WITH CORRESPONDING #(  keys )
      RESULT DATA(agencies).


    countries = CORRESPONDING #( agencies DISCARDING DUPLICATES MAPPING country = countrycode EXCEPT * ).
    DELETE countries WHERE country IS INITIAL.

    IF countries IS NOT INITIAL.
      SELECT FROM i_country FIELDS country
        FOR ALL ENTRIES IN @countries
        WHERE country = @countries-country
        INTO TABLE @DATA(countries_db).
    ENDIF.

    LOOP AT agencies INTO DATA(agency).
      APPEND VALUE #(
          %tky        = agency-%tky
          %state_area = state_areas-validate_country
        ) TO reported-agency.

      IF        agency-countrycode IS INITIAL
         OR NOT line_exists( countries_db[ country = agency-countrycode ] ).

        APPEND VALUE #( %tky = agency-%tky ) TO failed-agency.
        APPEND VALUE #(
            %tky                 = agency-%tky
            %state_area          = state_areas-validate_country
            %msg                 = NEW /dmo/cx_agency(
                textid      = /dmo/cx_agency=>country_code_invalid
                countrycode = agency-countrycode
              )
            %element-countrycode = if_abap_behv=>mk-on
          ) TO reported-agency.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD validateemailaddress.
    READ ENTITIES OF /dmo/r_agencytp_hd IN LOCAL MODE
      ENTITY agency
        FIELDS ( emailaddress )
        WITH CORRESPONDING #( keys )
      RESULT DATA(agencies).

    LOOP AT agencies INTO DATA(agency).

      APPEND VALUE #( %tky        = agency-%tky
                      %state_area = state_areas-validate_email )
             TO reported-agency.

      " Conversion to string to truncate trailing spaces, so + doesn't match space.
      IF CONV string( agency-emailaddress ) NP '+*@+*.+*' ##OPERATOR[STRING].

        APPEND VALUE #( %tky = agency-%tky ) TO failed-agency.

        APPEND VALUE #(
            %tky                  = agency-%tky
            %state_area           = state_areas-validate_email
            %msg                  = NEW /dmo/cx_agency( /dmo/cx_agency=>email_invalid_format )
            %element-emailaddress = if_abap_behv=>mk-on
          ) TO reported-agency.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD validatename.
    READ ENTITIES OF /dmo/r_agencytp_hd IN LOCAL MODE
      ENTITY agency
        FIELDS ( name )
        WITH CORRESPONDING #( keys )
      RESULT DATA(agencies).

    LOOP AT agencies INTO DATA(agency).
      APPEND VALUE #(
          %tky        = agency-%tky
          %state_area = state_areas-validate_name
        ) TO reported-agency.

      IF agency-name IS INITIAL.
        APPEND VALUE #( %tky = agency-%tky ) TO failed-agency.

        APPEND VALUE #(
            %tky          = agency-%tky
            %state_area   = state_areas-validate_name
            %msg          = NEW /dmo/cx_agency( /dmo/cx_agency=>name_required )
            %element-name = if_abap_behv=>mk-on
          ) TO reported-agency.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
