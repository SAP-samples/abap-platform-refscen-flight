CLASS lcl_travel_gen_ana DEFINITION INHERITING FROM /dmo/cl_abstract_data_gen CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance RETURNING VALUE(instance) TYPE REF TO lcl_travel_gen_ana.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS build_additional_fields REDEFINITION.
    METHODS setup_for_building REDEFINITION.
  PRIVATE SECTION.
    TYPES: BEGIN OF status_structure,
             overall_status TYPE c LENGTH 1,
           END OF status_structure.

    TYPES table_of_travels   TYPE STANDARD TABLE OF /dmo/a_trvl_ana WITH DEFAULT KEY.
    TYPES currency_code_type TYPE STANDARD TABLE OF I_CurrencyStdVH WITH KEY Currency.
    TYPES country_type       TYPE STANDARD TABLE OF I_CountryText WITH KEY Country.
    TYPES status_type        TYPE STANDARD TABLE OF status_structure WITH KEY overall_status.

    CLASS-DATA travel_generator_instance TYPE REF TO lcl_travel_gen_ana.

    CLASS-METHODS prepare_skeleton
      RETURNING VALUE(travels) TYPE table_of_travels.

    DATA ran_travel_description TYPE REF TO cl_abap_random_int.
    DATA ran_currency_code      TYPE REF TO cl_abap_random_int.
    DATA ran_total_price_float  TYPE REF TO cl_abap_random_decfloat16.
    DATA ran_total_price_int    TYPE REF TO cl_abap_random_int.
    DATA ran_countryname        TYPE REF TO cl_abap_random_int.
    DATA ran_booking_fee_int    TYPE REF TO cl_abap_random_int.
    DATA overall_status         TYPE status_type.
    DATA ran_overall_status     TYPE REF TO cl_abap_random_int.
    DATA currency_codes         TYPE currency_code_type.
    DATA countrynames           TYPE country_type.

    METHODS generate_description
      RETURNING VALUE(result) TYPE /dmo/a_trvl_ana-description.

    METHODS build_currency_code
      RETURNING VALUE(result) TYPE currency_code_type.

    METHODS build_country
      RETURNING VALUE(result) TYPE country_type.

    METHODS build_overall_status
      RETURNING VALUE(result) TYPE status_type.


ENDCLASS.

CLASS lcl_travel_gen_ana IMPLEMENTATION.

  METHOD constructor.

    DATA(travel_skeleton) = prepare_skeleton( ).

    DATA(travel_features) = VALUE feature_structure( with_admin_fields = abap_true
                                                     with_semantic_id  = abap_true
                                                     with_uuid         = abap_true ).

    DATA(field_mapping) = VALUE field_structure( last_changed_at       = 'last_changed_at'
                                                 local_created_at      = 'local_created_at'
                                                 local_created_by      = 'local_created_by'
                                                 local_last_changed_at = 'local_last_changed_at'
                                                 local_last_changed_by = 'local_last_changed_by'
                                                 semantic_id           = 'travel_id'
                                                 uuid                  = 'travel_uuid' ).

    super->constructor( skeleton_data        = REF #( travel_skeleton )
                        scenario_name        = 'Travel ANA' ##NO_TEXT
                        package_name         = '/DMO/FLIGHT_ANA'
                        table_name_active    = '/dmo/a_trvl_ana'
                        nr_minimum           = '000001'
                        nr_maximum           = '899999'
                        numberrange_object   = '/DMO/TRAVL'
                        numberrange_interval = '01'
                        features             = travel_features
                        fields               = field_mapping ).

  ENDMETHOD.

  METHOD get_instance.

    IF travel_generator_instance IS NOT BOUND.
      travel_generator_instance = NEW lcl_travel_gen_ana( ).
    ENDIF.

    RETURN travel_generator_instance.

  ENDMETHOD.

  METHOD prepare_skeleton.

    CONSTANTS travel_amount TYPE i VALUE 100.

    DO travel_amount TIMES.
      APPEND VALUE /dmo/a_trvl_ana( ) TO travels.
    ENDDO.

    RETURN travels.

  ENDMETHOD.

  METHOD setup_for_building.

    CONSTANTS start_at_1      TYPE i VALUE 1.
    CONSTANTS seed            TYPE i VALUE 42.
    CONSTANTS description_max TYPE i VALUE 9.
    CONSTANTS price_int_min   TYPE i VALUE 1000.
    CONSTANTS price_int_max   TYPE i VALUE 99999.
    CONSTANTS fee_int_max     TYPE i VALUE 150.
    CONSTANTS fee_int_min     TYPE i VALUE 1.



    currency_codes = build_currency_code( ).
    ran_currency_code = cl_abap_random_int=>create( min = start_at_1
                                                       max = lines( currency_codes ) ).

    countrynames = build_country( ).
    ran_countryname = cl_abap_random_int=>create( min = start_at_1
                                                     max = lines( countrynames ) ).

    ran_travel_description = cl_abap_random_int=>create( min = start_at_1
                                                            max = description_max ).

    ran_total_price_float = cl_abap_random_decfloat16=>create( seed = seed ).
    ran_total_price_int = cl_abap_random_int=>create( min = price_int_min
                                                         max = price_int_max ).
    ran_booking_fee_int = cl_abap_random_int=>create( min = fee_int_min
                                                         max = fee_int_max ).
    overall_status = build_overall_status(  ).
    ran_overall_status = cl_abap_random_int=>create( min = start_at_1
                                                       max = lines( overall_status ) ).

  ENDMETHOD.

  METHOD build_currency_code.
    SELECT Currency FROM I_CurrencyStdVH INTO TABLE @result. "#EC CI_NOWHERE
  ENDMETHOD.

  METHOD build_country.
    SELECT FROM I_CountryText FIELDS country, countryname WHERE language = 'E' INTO TABLE @result.
  ENDMETHOD.

  METHOD build_overall_status.
    result = VALUE #( ( overall_status = 'A' )
                      ( overall_status = 'X' )
                      ( overall_status = 'O' ) ).
  ENDMETHOD.

  METHOD build_additional_fields.
    FIELD-SYMBOLS <travel> TYPE /dmo/a_trvl_ana.
    ASSIGN entry->* TO <travel>.

    DATA(date_generator) = /dmo/cl_data_gen_util_factory=>/dmo/if_data_gen_util_factory~create_date_gen_instance( ).

    <travel>-total_price   = ran_total_price_float->get_next( ) + ran_total_price_int->get_next( ).
    <travel>-booking_fee   = ran_total_price_float->get_next( ) + ran_booking_fee_int->get_next( ).
    <travel>-currency_code = currency_codes[ ran_currency_code->get_next( ) ]-Currency. "#EC CI_NOORDER
    <travel>-begin_date = date_generator->generate_date( ).
    <travel>-end_date   = date_generator->generate_new_date_with_offset( <travel>-begin_date ).
    <travel>-description = generate_description( ).
    <travel>-overall_status = overall_status[ ran_overall_status->get_next(  ) ]-Overall_Status.
  ENDMETHOD.

  METHOD generate_description.
    DATA(person) = /dmo/cl_data_gen_util_factory=>/dmo/if_data_gen_util_factory~create_name_gen_instance( )->generate( ).
    DATA(country) = countrynames[ ran_countryname->get_next( ) ]-CountryName. "#EC CI_NOORDER

    result = SWITCH /dmo/a_trvl_ana-description(
                               ran_travel_description->get_next( )
                               WHEN 1 THEN |Business Trip for { person-first-salutation } { person-last }|
                               WHEN 2 THEN |Vacation for { person-first-salutation } { person-last }|
                               WHEN 3 THEN |Business Trip to { country }|
                               WHEN 4 THEN |Vacation to { country }|
                               WHEN 5 THEN |Visiting { person-first-salutation } { person-last }|
                               WHEN 6 THEN |Business Trip|
                               ELSE        |Vacation| )
                        ##NO_TEXT.
  ENDMETHOD.

ENDCLASS.
