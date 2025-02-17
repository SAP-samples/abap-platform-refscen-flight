
CLASS lcl_text_output DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        IMPORTING
          out           TYPE REF TO if_oo_adt_classrun_out
        RETURNING
          VALUE(result) TYPE REF TO lcl_text_output.

    METHODS:
      print_title
        IMPORTING
          title TYPE string,
      print_delete,
      print_build,
      print_insert,
      print_done.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      instance TYPE REF TO lcl_text_output.

    DATA:
      out TYPE REF TO if_oo_adt_classrun_out.

    METHODS:
      constructor
        IMPORTING
          out TYPE REF TO if_oo_adt_classrun_out.

ENDCLASS.

CLASS lcl_text_output IMPLEMENTATION.

  METHOD get_instance.
    IF instance IS NOT BOUND.
      instance = NEW lcl_text_output( out ).
    ENDIF.
    result = instance.
  ENDMETHOD.

  METHOD constructor.
    me->out = out.
  ENDMETHOD.

  METHOD print_build.
    IF out IS BOUND.
      out->write( '--> Build Content.' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD print_delete.
    IF out IS BOUND.
      out->write( '--> Delete Content.' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD print_done.
    IF out IS BOUND.
      out->write( |--> Done.\r\n| ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD print_insert.
    IF out IS BOUND.
      out->write( '--> Insert Content.' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD print_title.
    IF out IS BOUND.
      out->write( |Generating Data: { title }.| ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

ENDCLASS.






CLASS lcl_name_generator DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_name,
        first TYPE /dmo/employ_a_hd-first_name,
        last  TYPE /dmo/employ_a_hd-last_name,
      END OF ts_name,

      BEGIN OF ts_first_name,
        first_name TYPE /dmo/employ_a_hd-first_name,
        gender     TYPE c LENGTH 1,
      END OF ts_first_name,
      tt_first_name TYPE STANDARD TABLE OF ts_first_name WITH DEFAULT KEY,
      BEGIN OF ts_last_name,
        last_name TYPE /dmo/employ_a_hd-last_name,
      END OF ts_last_name,
      tt_last_name TYPE STANDARD TABLE OF ts_last_name WITH DEFAULT KEY.

    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(ro_name_generator) TYPE REF TO lcl_name_generator.

    METHODS:
      generate
        RETURNING
          VALUE(rs_name) TYPE ts_name,
      get_first_names
        RETURNING VALUE(result) TYPE tt_first_name,
      get_last_names
        RETURNING VALUE(result) TYPE tt_last_name.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      go_name_generator TYPE REF TO lcl_name_generator.

    DATA:
      mt_first_name TYPE tt_first_name,
      mt_last_name  TYPE tt_last_name,
      mo_random     TYPE REF TO cl_abap_random_float.

    METHODS:
      build_first_names,
      build_last_names,
      init.

ENDCLASS.

CLASS lcl_name_generator IMPLEMENTATION.

  METHOD get_instance.
    IF go_name_generator IS NOT BOUND.
      go_name_generator = NEW lcl_name_generator( ).
      go_name_generator->init( ).
    ENDIF.
    ro_name_generator = go_name_generator.
  ENDMETHOD.

  METHOD generate.
    rs_name = VALUE ts_name(
        first = mt_first_name[ floor( mo_random->get_next( ) * lines( mt_first_name ) ) + 1 ]
        last  = mt_last_name[  floor( mo_random->get_next( ) * lines( mt_last_name  ) ) + 1 ]
      ).
  ENDMETHOD.


  METHOD build_first_names.
    mt_first_name = VALUE tt_first_name( ##NO_TEXT
                ( first_name = 'Simon'  gender = 'M' )
                ( first_name = 'Harish'  gender = 'M' )
                ( first_name = 'Volker'  gender = 'M' )
                ( first_name = 'Jasmin'  gender = 'F' )
                ( first_name = 'Felix'  gender = 'M' )
                ( first_name = 'Kristina'  gender = 'F' )
                ( first_name = 'Thilo'  gender = 'M' )
                ( first_name = 'Andrej'  gender = 'M' )
                ( first_name = 'Anna'  gender = 'F' )
                ( first_name = 'Johannes' gender = 'M' )
                ( first_name = 'Johann'  gender = 'M' )
                ( first_name = 'Christoph' gender = 'M' )
                ( first_name = 'Andreas' gender = 'M' )
                ( first_name = 'Stephen' gender = 'M' )
                ( first_name = 'Mathilde' gender = 'F' )
                ( first_name = 'August'  gender = 'M' )
                ( first_name = 'Illya'  gender = 'M' )
                ( first_name = 'Georg'  gender = 'M' )
                ( first_name = 'Gisela'  gender = 'F' )
                ( first_name = 'Christa' gender = 'F' )
                ( first_name = 'Holm'  gender = 'M' )
                ( first_name = 'Irmtraut' gender = 'F' )
                ( first_name = 'Ludwig'  gender = 'M' )
                ( first_name = 'Laura'  gender = 'F' )
                ( first_name = 'Kurt'  gender = 'M' )
                ( first_name = 'Guenther' gender = 'M' )
                ( first_name = 'Horst'  gender = 'M' )
                ( first_name = 'Matthias' gender = 'M' )
                ( first_name = 'Amelie'  gender = 'F' )
                ( first_name = 'Walter'  gender = 'M' )
                ( first_name = 'Sophie'  gender = 'F' )
                ( first_name = 'Claire'  gender = 'F' )
                ( first_name = 'Chantal' gender = 'F' )
                ( first_name = 'Jean'  gender = 'M' )
                ( first_name = 'Cindy'  gender = 'F' )
                ( first_name = 'Pierre'  gender = 'M' )
                ( first_name = 'Irene'  gender = 'F' )
                ( first_name = 'Adam'  gender = 'M' )
                ( first_name = 'Fabio'  gender = 'M' )
                ( first_name = 'Lothar'  gender = 'M' )
                ( first_name = 'Annemarie' gender = 'F' )
                ( first_name = 'Ida'  gender = 'F' )
                ( first_name = 'Roland'  gender = 'M' )
                ( first_name = 'Achim'  gender = 'M' )
                ( first_name = 'Allen'  gender = 'M' )
                ( first_name = 'Lee'  gender = 'M' )
                ( first_name = 'Guillermo' gender = 'M' )
                ( first_name = 'Florian' gender = 'M' )
                ( first_name = 'Ulla'  gender = 'F' )
                ( first_name = 'Juan'  gender = 'M' )
                ( first_name = 'Marta'  gender = 'F' )
                ( first_name = 'Salvador' gender = 'M' )
                ( first_name = 'Christine' gender = 'F' )
                ( first_name = 'Dominik' gender = 'M' )
                ( first_name = 'Astrid'  gender = 'F' )
                ( first_name = 'Ruth'  gender = 'F' )
                ( first_name = 'Theresia' gender = 'F' )
                ( first_name = 'Thomas'  gender = 'M' )
                ( first_name = 'Friedrich' gender = 'M' )
                ( first_name = 'Anneliese' gender = 'F' )
                ( first_name = 'Peter'  gender = 'M' )
                ( first_name = 'Anne-Marie' gender = 'F' )
                ( first_name = 'James'  gender = 'M' )
                ( first_name = 'Jean-Luc' gender = 'M' )
                ( first_name = 'Benjamin' gender = 'M' )
                ( first_name = 'Hendrik' gender = 'M' )
                ( first_name = 'Uli'  gender = 'F' )
                ( first_name = 'Siegfried' gender = 'M' )
                ( first_name = 'Max' gender = 'M' )
              ).

  ENDMETHOD.

  METHOD build_last_names.
    mt_last_name = VALUE tt_last_name( ##NO_TEXT ##STRING_OK
          ( last_name = 'Buchholm' )
          ( last_name = 'Vrsic' )
          ( last_name = 'Jeremias' )
          ( last_name = 'Gutenberg' )
          ( last_name = 'Fischmann' )
          ( last_name = 'Columbo' )
          ( last_name = 'Neubasler' )
          ( last_name = 'Martin' )
          ( last_name = 'Detemple' )
          ( last_name = 'Barth' )
          ( last_name = 'Benz' )
          ( last_name = 'Hansmann' )
          ( last_name = 'Koslowski' )
          ( last_name = 'Wohl' )
          ( last_name = 'Koller' )
          ( last_name = 'Hoffen' )
          ( last_name = 'Dumbach' )
          ( last_name = 'Goelke' )
          ( last_name = 'Waldmann' )
          ( last_name = 'Mechler' )
          ( last_name = 'Buehler' )
          ( last_name = 'Heller' )
          ( last_name = 'Simonen' )
          ( last_name = 'Henry' )
          ( last_name = 'Marshall' )
          ( last_name = 'Legrand' )
          ( last_name = 'Jacqmain' )
          ( last_name = 'D´Oultrement' )
          ( last_name = 'Hunter' )
          ( last_name = 'Delon' )
          ( last_name = 'Kreiss' )
          ( last_name = 'Trensch' )
          ( last_name = 'Cesari' )
          ( last_name = 'Matthaeus' )
          ( last_name = 'Babilon' )
          ( last_name = 'Zimmermann' )
          ( last_name = 'Kramer' )
          ( last_name = 'Illner' )
          ( last_name = 'Pratt' )
          ( last_name = 'Gahl' )
          ( last_name = 'Benjamin' )
          ( last_name = 'Miguel' )
          ( last_name = 'Weiss' )
          ( last_name = 'Sessler' )
          ( last_name = 'Montero' )
          ( last_name = 'Domenech' )
          ( last_name = 'Moyano' )
          ( last_name = 'Sommer' )
          ( last_name = 'Schneider' )
          ( last_name = 'Eichbaum' )
          ( last_name = 'Gueldenpfennig' )
          ( last_name = 'Sudhoff' )
          ( last_name = 'Lautenbach' )
          ( last_name = 'Ryan' )
          ( last_name = 'Prinz' )
          ( last_name = 'Deichgraeber' )
          ( last_name = 'Pan' )
          ( last_name = 'Lindwurm' )
          ( last_name = 'Kirk' )
          ( last_name = 'Picard' )
          ( last_name = 'Sisko' )
          ( last_name = 'Madeira' )
          ( last_name = 'Meier' )
          ( last_name = 'Rahn' )
          ( last_name = 'Leisert' )
          ( last_name = 'Müller' )
          ( last_name = 'Mustermann' )
          ( last_name = 'Becker' )
          ( last_name = 'Fischer' )
      ).
  ENDMETHOD.


  METHOD init.
    build_first_names( ).
    build_last_names( ).
    mo_random = cl_abap_random_float=>create( ).
  ENDMETHOD.

  METHOD get_first_names.
    result = mt_first_name.
  ENDMETHOD.

  METHOD get_last_names.
    result = mt_last_name.
  ENDMETHOD.

ENDCLASS.





CLASS lcl_agency_generator DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      tt_agency TYPE STANDARD TABLE OF /dmo/agency_a_hd WITH KEY agency.

    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO lcl_agency_generator.

    METHODS:
      create
        IMPORTING out TYPE REF TO if_oo_adt_classrun_out OPTIONAL,

      get_agencies
        RETURNING
          VALUE(result) TYPE tt_agency.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      cv_numberrange_interval TYPE cl_numberrange_runtime=>nr_interval VALUE '01',
      cv_numberrange_object   TYPE cl_numberrange_runtime=>nr_object   VALUE '/DMO/AGNHD' ##NO_TEXT.

    CONSTANTS:
      cv_fromnumber TYPE cl_numberrange_intervals=>nr_nriv_line-fromnumber VALUE '004001',
      cv_tonumber   TYPE cl_numberrange_intervals=>nr_nriv_line-tonumber   VALUE '999999'.

    CLASS-DATA:
      go_instance TYPE REF TO lcl_agency_generator.
    DATA:
      mt_agencies TYPE tt_agency.
    METHODS initiate_numberrange.
ENDCLASS.

CLASS lcl_agency_generator IMPLEMENTATION.

  METHOD create.
    DATA(lo_text) = lcl_text_output=>get_instance( out ).
    lo_text->print_title( 'Agency' ).

    lo_text->print_delete( ).
    DELETE FROM /dmo/agency_a_hd.

    lo_text->print_build( ).
    initiate_numberrange( ).
    get_agencies( ).

    lo_text->print_insert( ).
    INSERT /dmo/agency_a_hd FROM TABLE @mt_agencies.

    lo_text->print_done( ).
  ENDMETHOD.

  METHOD get_agencies.
    DATA:
      lv_agency_id_max TYPE /dmo/agency_id.

    IF mt_agencies IS NOT INITIAL.
      result = mt_agencies.
      EXIT.
    ENDIF.


    SELECT
      FROM /dmo/agency
      FIELDS
        name                  AS  name                 ,
        street                AS  street               ,
        postal_code           AS  postal_code          ,
        city                  AS  city                 ,
        country_code          AS  country_code         ,
        phone_number          AS  phone_number         ,
        email_address         AS  email_address        ,
        web_address           AS  web_address          ,
        local_created_by      AS  local_created_by     ,
        local_created_at      AS  local_created_at     ,
        local_last_changed_by AS  local_last_changed_by,
        local_last_changed_at AS  local_last_changed_at,
        last_changed_at       AS  last_changed_at
      INTO CORRESPONDING FIELDS OF TABLE @mt_agencies.

    TRY.
        cl_numberrange_runtime=>number_get(
            EXPORTING
              nr_range_nr       = cv_numberrange_interval
              object            = cv_numberrange_object
              quantity          = CONV #( lines( mt_agencies ) )
            IMPORTING
              number            = DATA(lv_number)
              returncode        = DATA(lv_returncode)
              returned_quantity = DATA(lv_returned_quantity)
          ).
      CATCH cx_nr_object_not_found cx_number_ranges INTO DATA(lx).
    ENDTRY.

    ASSERT lx IS INITIAL.
    ASSERT lv_returncode IS INITIAL.
    ASSERT lv_returned_quantity = lines( mt_agencies ).

    lv_agency_id_max = lv_number - lv_returned_quantity.

    LOOP AT mt_agencies ASSIGNING FIELD-SYMBOL(<agency>).
      lv_agency_id_max += 1.
      <agency>-agency = lv_agency_id_max .
    ENDLOOP.

    result = mt_agencies.
  ENDMETHOD.

  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      go_instance = NEW #( ).
    ENDIF.
    result = go_instance.
  ENDMETHOD.


  METHOD initiate_numberrange.
    /dmo/cl_flight_data_generator=>reset_numberrange_interval(
      EXPORTING
        numberrange_object   = cv_numberrange_object
        numberrange_interval = cv_numberrange_interval
        fromnumber           = cv_fromnumber
        tonumber             = cv_tonumber ).
  ENDMETHOD.

ENDCLASS.






CLASS lcl_employee DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      tt_employee TYPE STANDARD TABLE OF /dmo/employ_a_hd WITH KEY employee,

      BEGIN OF ts_parameter,
        agency               TYPE /dmo/employ_a_hd-agency,
        max_deepth           TYPE i,
        sibling_order_number TYPE /dmo/sibling_order_number,
        salary_currency      TYPE /dmo/employ_a_hd-salary_currency,
      END OF ts_parameter.


    DATA:
      gt_employee TYPE lcl_employee=>tt_employee.

    METHODS:
      constructor
        IMPORTING
          is_parameter TYPE ts_parameter.

    METHODS:
      generate
        RETURNING
          VALUE(rt_employee) TYPE tt_employee.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      cv_max_children             TYPE i VALUE 20,
      cv_min_salary               TYPE /dmo/employ_a_hd-salary VALUE '50000',
      cv_salary_add_per_layer_fix TYPE /dmo/employ_a_hd-salary VALUE '10000',
      cv_salary_add_per_layer_var TYPE /dmo/employ_a_hd-salary VALUE '10000'.

    DATA:
      ms_parameter TYPE ts_parameter,
      mo_random    TYPE REF TO cl_abap_random_float,
      mv_id        TYPE /dmo/employ_a_hd-employee.

    CLASS-DATA:
      go_employee TYPE REF TO lcl_employee.

    METHODS:
      get_employees
        IMPORTING
          is_parameter       TYPE ts_parameter
          iv_manager         TYPE /dmo/employ_a_hd-manager OPTIONAL
        RETURNING
          VALUE(rt_employee) TYPE tt_employee,
      _calculate_salary
        IMPORTING
          iv_level         TYPE i
        RETURNING
          VALUE(rv_salary) TYPE /dmo/employ_a_hd-salary.

ENDCLASS.

CLASS lcl_employee IMPLEMENTATION.

  METHOD constructor.
    ms_parameter = is_parameter.
    mo_random = cl_abap_random_float=>create( ).
    mv_id = 0.
  ENDMETHOD.

  METHOD get_employees.
    DATA:
      lv_sibling_order_number TYPE /dmo/sibling_order_number.

    mv_id += 1.

    DATA(ls_name) = lcl_name_generator=>get_instance( )->generate( ).
    DATA(lv_manager) = mv_id.
    APPEND VALUE /dmo/employ_a_hd(
          employee             = mv_id
          first_name           = ls_name-first
          last_name            = ls_name-last
          salary               = _calculate_salary( is_parameter-max_deepth )
          salary_currency      = is_parameter-salary_currency
          manager              = iv_manager
          sibling_order_number = is_parameter-sibling_order_number
          agency               = is_parameter-agency
      ) TO rt_employee.

    lv_sibling_order_number = '0'.

    IF is_parameter-max_deepth > 0.
      DO floor( mo_random->get_next( ) * cv_max_children + 1 ) TIMES.
        lv_sibling_order_number += 1.
        APPEND LINES OF get_employees(
                 is_parameter = VALUE ts_parameter(
                                    max_deepth           = floor( mo_random->get_next( ) * is_parameter-max_deepth )
                                    agency               = is_parameter-agency
                                    salary_currency      = is_parameter-salary_currency
                                    sibling_order_number = lv_sibling_order_number
                                  )
                 iv_manager   = lv_manager
               ) TO rt_employee.
      ENDDO.
    ENDIF.
  ENDMETHOD.

  METHOD generate.
    rt_employee = get_employees( ms_parameter ).
  ENDMETHOD.


  METHOD _calculate_salary.
    rv_salary =
        cv_min_salary
      + iv_level * cv_salary_add_per_layer_fix
      + mo_random->get_next( ) * cv_salary_add_per_layer_var.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_employee_generator DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      create
        IMPORTING out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

    DATA:
      gt_employee TYPE lcl_employee=>tt_employee.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      cv_max_deepth TYPE i VALUE 5.

    DATA:
      mt_employees TYPE STANDARD TABLE OF /dmo/employ_a_hd WITH KEY agency employee,
      mt_agencies  TYPE STANDARD TABLE OF /dmo/agency_a_hd WITH KEY agency.

    METHODS:
      _iterate_over_agencies,
      _get_agencies.

ENDCLASS.

CLASS lcl_employee_generator IMPLEMENTATION.

  METHOD create.
    DATA(lo_text) = lcl_text_output=>get_instance( out ).
    lo_text->print_title( 'Employee' ).

    lo_text->print_delete( ).
    DELETE FROM /dmo/employ_a_hd.

    lo_text->print_build( ).
    _get_agencies( ).
    _iterate_over_agencies( ).

    lo_text->print_insert( ).
    INSERT /dmo/employ_a_hd FROM TABLE @mt_employees.

    lo_text->print_done( ).
  ENDMETHOD.

  METHOD _iterate_over_agencies.

    _get_agencies( ).
    DATA(lo_random) = cl_abap_random_float=>create( ).

    LOOP AT mt_agencies INTO DATA(ls_agency).
      DATA(lv_max_deepth) = CONV i( floor( lo_random->get_next( ) * cv_max_deepth ) + 1 ).
      DATA(lt_employees_of_agency) = NEW lcl_employee(
                                              is_parameter = VALUE lcl_employee=>ts_parameter(
                                                                max_deepth      = lv_max_deepth
                                                                agency          = ls_agency-agency
                                                                salary_currency = 'EUR'
                                            )
                            )->generate( ).

      APPEND LINES OF lt_employees_of_agency TO mt_employees.

    ENDLOOP.

  ENDMETHOD.


  METHOD _get_agencies.
    mt_agencies = lcl_agency_generator=>get_instance( )->get_agencies( ).
  ENDMETHOD.

ENDCLASS.
