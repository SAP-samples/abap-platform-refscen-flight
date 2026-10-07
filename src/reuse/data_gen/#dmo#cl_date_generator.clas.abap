CLASS /dmo/cl_date_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES /dmo/if_date_generator.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS seed                   TYPE i VALUE 42.
    CONSTANTS start_at_0             TYPE i VALUE 0.
    CONSTANTS start_at_1             TYPE i VALUE 1.
    CONSTANTS year_char_len          TYPE i VALUE 4.
    CONSTANTS month_min              TYPE i VALUE 12.
    CONSTANTS month_max              TYPE i VALUE 27.
    CONSTANTS second_max             TYPE i VALUE 59.
    CONSTANTS hour_max               TYPE i VALUE 23.
    CONSTANTS date_offset_min        TYPE i VALUE 2.
    CONSTANTS date_offset_max        TYPE i VALUE 100.
    CONSTANTS start_date_year_offset TYPE i VALUE 20.
    CONSTANTS days_before            TYPE i VALUE 15.
    CONSTANTS date_min               TYPE i VALUE 0.
    CONSTANTS date_max               TYPE i VALUE 20.

    CLASS-DATA date_generator_instance TYPE REF TO /dmo/cl_date_generator.

    DATA date_today             TYPE d.
    DATA mo_random              TYPE REF TO cl_abap_random_float.
    DATA ran_start_date_year    TYPE REF TO cl_abap_random_int.
    DATA ran_start_date_month   TYPE REF TO cl_abap_random_int.
    DATA ran_start_date_day     TYPE REF TO cl_abap_random_int.
    DATA ran_hour               TYPE REF TO cl_abap_random_int.
    DATA ran_min_sec            TYPE REF TO cl_abap_random_int.
    DATA ran_end_date_offset    TYPE REF TO cl_abap_random_int.
    DATA ran_travel_change_date TYPE REF TO cl_abap_random_int.

    METHODS init.
    METHODS set_today.

    METHODS generate_date_with_offset
      IMPORTING reference_date               TYPE d OPTIONAL
      RETURNING VALUE(date_with_offset_tmsp) TYPE timestampl.
ENDCLASS.



CLASS /dmo/cl_date_generator IMPLEMENTATION.

  METHOD /dmo/if_date_generator~get_instance.
    IF date_generator_instance IS NOT BOUND.
      date_generator_instance = NEW /dmo/cl_date_generator( ).
      date_generator_instance->init( ).
    ENDIF.

    RETURN date_generator_instance.
  ENDMETHOD.

  METHOD init.
    set_today( ).

    DATA(year) = date_today(year_char_len).

    ran_start_date_year = cl_abap_random_int=>create( min = year + start_at_1
                                                      max = year + start_date_year_offset ).
    ran_start_date_month = cl_abap_random_int=>create( min = start_at_1
                                                       max = month_min ).
    ran_start_date_day = cl_abap_random_int=>create( min = start_at_1
                                                     max = month_max ).
    ran_hour = cl_abap_random_int=>create( min = start_at_0
                                           max = hour_max ).
    ran_min_sec = cl_abap_random_int=>create( min = start_at_0
                                              max = second_max ).
    ran_end_date_offset = cl_abap_random_int=>create( seed = seed
                                                      min  = date_offset_min
                                                      max  = date_offset_max ).
    ran_travel_change_date = cl_abap_random_int=>create( min = date_min
                                                         max = date_max ).
    mo_random = cl_abap_random_float=>create( seed = seed ).
  ENDMETHOD.

  METHOD set_today.
    CONSTANTS substract_by_one TYPE i VALUE 1.

    GET TIME STAMP FIELD DATA(current_timestamp).
    CONVERT TIME STAMP current_timestamp TIME ZONE space INTO DATE date_today.
    date_today = date_today - substract_by_one.
  ENDMETHOD.

  METHOD /dmo/if_date_generator~date_to_timestamp.
    CONSTANTS _1000000 TYPE i VALUE 1000000.
    DATA date_generator TYPE REF TO /dmo/if_date_generator.

    date_generator = /dmo/if_date_generator~get_instance( ).

    timestamp = CONV string( date ) * _1000000.
    timestamp += date_generator->generate_random_time( ).
  ENDMETHOD.

  METHOD /dmo/if_date_generator~calc_days_before_ref_or_today.
    CONSTANTS one_day_in_sec TYPE i VALUE -86400.
    CONSTANTS _0             TYPE i VALUE 0.

    cl_abap_tstmp=>td_add( EXPORTING date     = COND d( WHEN date_today < reference_date or reference_date IS INITIAL
                                                        THEN date_today
                                                        ELSE reference_date )
                                     time     = CONV t( _0 )
                                     secs     = one_day_in_sec * days_before
                           IMPORTING res_date = result  ).
  ENDMETHOD.

  METHOD generate_date_with_offset.
    DATA date_with_offset TYPE d.

    date_with_offset = reference_date + ran_travel_change_date->get_next( ).
    RETURN /dmo/if_date_generator~date_to_timestamp( date_with_offset ).
  ENDMETHOD.

  METHOD /dmo/if_date_generator~generate_random_time.
    CONSTANTS _10000 TYPE i VALUE 10000.
    CONSTANTS _100 TYPE i VALUE 100.

    result = ran_hour->get_next( ) * _10000 + ran_min_sec->get_next( ) * _100 + ran_min_sec->get_next( ).
  ENDMETHOD.

  METHOD /dmo/if_date_generator~generate_date.
    DATA year_of_date  TYPE n LENGTH 4.
    DATA month_of_date TYPE n LENGTH 2.
    DATA day_of_date   TYPE n LENGTH 2.

    year_of_date = ran_start_date_year->get_next( ).
    month_of_date = ran_start_date_month->get_next( ).
    day_of_date = ran_start_date_day->get_next( ).

    result+0(4) = year_of_date.
    result+4(2) = month_of_date.
    result+6(2) = day_of_date.
  ENDMETHOD.

  METHOD /dmo/if_date_generator~generate_new_date_with_offset.
    RETURN reference_date + ran_end_date_offset->get_next( ).
  ENDMETHOD.

  METHOD /dmo/if_date_generator~get_date_today.
    RETURN date_today.
  ENDMETHOD.

  METHOD /dmo/if_date_generator~generate_date_in_range.
    DATA(span) = to_date - from_date.
    IF span <= 0.
      RETURN from_date.
    ENDIF.
    RETURN from_date + CONV i( floor( mo_random->get_next( ) * ( span + 1 ) ) ).
  ENDMETHOD.

ENDCLASS.
