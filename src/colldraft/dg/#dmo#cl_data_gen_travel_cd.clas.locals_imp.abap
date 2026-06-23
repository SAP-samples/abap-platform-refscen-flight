*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_travel_gen_cd DEFINITION INHERITING FROM /dmo/cl_abstract_data_gen CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance RETURNING VALUE(instance) TYPE REF TO lcl_travel_gen_cd.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS build_additional_fields REDEFINITION.
    METHODS setup_for_building REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF status_structure,
             overall_status TYPE c LENGTH 1,
           END OF status_structure.

    TYPES table_of_travels   TYPE STANDARD TABLE OF /dmo/a_travel_cd WITH DEFAULT KEY.
    TYPES currency_code_type TYPE STANDARD TABLE OF I_CurrencyStdVH WITH KEY Currency.
    TYPES status_type        TYPE STANDARD TABLE OF status_structure WITH KEY overall_status.

    CONSTANTS: BEGIN OF travel_status,
                 accepted TYPE c LENGTH 1 VALUE 'A',
                 rejected TYPE c LENGTH 1 VALUE 'X',
                 open     TYPE c LENGTH 1 VALUE 'O',
               END OF travel_status.

    CONSTANTS: BEGIN OF description_type,
                 business_trip  TYPE i VALUE 1,
                 vacation       TYPE i VALUE 2,
                 family_visit   TYPE i VALUE 3,
                 conference     TYPE i VALUE 4,
                 training       TYPE i VALUE 5,
               END OF description_type.

    CLASS-DATA travel_generator_instance TYPE REF TO lcl_travel_gen_cd.

    CLASS-METHODS prepare_skeleton
      RETURNING VALUE(travels) TYPE table_of_travels.

    DATA ran_travel_description TYPE REF TO cl_abap_random_int.
    DATA ran_currency_code      TYPE REF TO cl_abap_random_int.
    DATA ran_booking_fee_float  TYPE REF TO cl_abap_random_decfloat16.
    DATA ran_booking_fee_int    TYPE REF TO cl_abap_random_int.
    DATA overall_status         TYPE status_type.
    DATA ran_overall_status     TYPE REF TO cl_abap_random_int.
    DATA currency_codes         TYPE currency_code_type.
    DATA date_generator         TYPE REF TO /dmo/if_date_generator.

    METHODS generate_description
      RETURNING VALUE(result) TYPE /dmo/a_travel_cd-description.

    METHODS build_currency_code
      RETURNING VALUE(result) TYPE currency_code_type.

    METHODS build_overall_status
      RETURNING VALUE(result) TYPE status_type.

ENDCLASS.


CLASS lcl_travel_gen_cd IMPLEMENTATION.
  METHOD constructor.
    DATA(travel_skeleton) = prepare_skeleton( ).

    DATA(travel_features) = VALUE feature_structure( with_db           = abap_true
                                                     with_admin_fields = abap_true
                                                     with_semantic_id  = abap_true
                                                     with_uuid         = abap_true ).

    DATA(semantic_id_config) = VALUE semantic_id_components( numberrange_lenght   = 6
                                                             numberrange_max      = '899999'
                                                             numberrange_min      = '1'
                                                             numberrange_interval = '01'
                                                             numberrange_object   = '/DMO/TRAVL' ).

    DATA(field_mapping) = VALUE field_structure( last_changed_at       = 'last_changed_at'
                                                 local_created_at      = 'local_created_at'
                                                 local_created_by      = 'local_created_by'
                                                 local_last_changed_at = 'local_last_changed_at'
                                                 local_last_changed_by = 'local_last_changed_by'
                                                 semantic_id           = 'travel_id'
                                                 uuid                  = 'travel_uuid' ).

    super->constructor( skeleton_data      = REF #( travel_skeleton )
                        scenario_name      = 'Travel Coll. Draft' ##NO_TEXT
                        package_name       = '/DMO/FLIGHT_COLLDRAFT'
                        table_name_active  = '/dmo/a_travel_cd'
                        table_name_draft   = '/dmo/d_travel_cd'
                        semantic_id_config = semantic_id_config
                        features           = travel_features
                        fields             = field_mapping ).
  ENDMETHOD.

  METHOD get_instance.
    IF travel_generator_instance IS NOT BOUND.
      travel_generator_instance = NEW lcl_travel_gen_cd( ).
    ENDIF.

    RETURN travel_generator_instance.
  ENDMETHOD.

  METHOD prepare_skeleton.
    CONSTANTS travel_amount TYPE i VALUE 100.

    DO travel_amount TIMES.
      APPEND VALUE /dmo/a_travel_cd( ) TO travels.
    ENDDO.

    RETURN travels.
  ENDMETHOD.

  METHOD setup_for_building.
    CONSTANTS start_at_1      TYPE i VALUE 1.
    CONSTANTS seed            TYPE i VALUE 42.
    CONSTANTS description_max TYPE i VALUE 5.
    CONSTANTS fee_int_max     TYPE i VALUE 150.
    CONSTANTS fee_int_min     TYPE i VALUE 1.

    currency_codes = build_currency_code( ).
    ran_currency_code = cl_abap_random_int=>create( min = start_at_1
                                                    max = lines( currency_codes ) ).

    ran_travel_description = cl_abap_random_int=>create( min = start_at_1
                                                         max = description_max ).

    ran_booking_fee_float = cl_abap_random_decfloat16=>create( seed = seed ).
    ran_booking_fee_int = cl_abap_random_int=>create( min = fee_int_min
                                                      max = fee_int_max ).

    date_generator = /dmo/cl_data_gen_util_factory=>/dmo/if_data_gen_util_factory~create_date_gen_instance( ).

    overall_status = build_overall_status( ).
    ran_overall_status = cl_abap_random_int=>create( min = start_at_1
                                                     max = lines( overall_status ) ).
  ENDMETHOD.

  METHOD build_currency_code.
    result = VALUE #( ( Currency = 'EUR' )
                      ( Currency = 'USD' ) ).
  ENDMETHOD.

  METHOD build_overall_status.
    result = VALUE #( ( overall_status = travel_status-accepted )
                      ( overall_status = travel_status-rejected )
                      ( overall_status = travel_status-open ) ).
  ENDMETHOD.

  METHOD build_additional_fields.
    FIELD-SYMBOLS <travel> TYPE /dmo/a_travel_cd.

    ASSIGN entry->* TO <travel>.

    <travel>-booking_fee    = ran_booking_fee_float->get_next( ) + ran_booking_fee_int->get_next( ).
    <travel>-currency_code  = currency_codes[ ran_currency_code->get_next( ) ]-Currency. "#EC CI_NOORDER
    <travel>-begin_date     = date_generator->generate_date( ).
    <travel>-end_date       = date_generator->generate_new_date_with_offset( <travel>-begin_date ).
    <travel>-description    = generate_description( ).
    <travel>-overall_status = overall_status[ ran_overall_status->get_next( ) ]-Overall_Status.
  ENDMETHOD.

  METHOD generate_description.
    result = SWITCH /dmo/a_travel_cd-description(
                               ran_travel_description->get_next( )
                               WHEN description_type-business_trip THEN |Business Trip|
                               WHEN description_type-vacation      THEN |Vacation|
                               WHEN description_type-family_visit  THEN |Family Visit|
                               WHEN description_type-conference    THEN |Conference|
                               WHEN description_type-training      THEN |Training|
                               ELSE                                     |Vacation| )
                        ##NO_TEXT.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_booking_gen_cd DEFINITION INHERITING FROM /dmo/cl_abstract_data_gen CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance RETURNING VALUE(instance) TYPE REF TO lcl_booking_gen_cd.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS build_additional_fields REDEFINITION.
    METHODS setup_for_building      REDEFINITION.

  PRIVATE SECTION.
    TYPES table_of_bookings TYPE STANDARD TABLE OF /dmo/a_book_cd WITH DEFAULT KEY.
    TYPES travel_type       TYPE STANDARD TABLE OF /dmo/a_travel_cd WITH KEY travel_uuid.

    TYPES: BEGIN OF booking_counter_type,
             travel_uuid TYPE sysuuid_x16,
             count       TYPE i,
           END OF booking_counter_type.

    CLASS-DATA booking_generator_instance TYPE REF TO lcl_booking_gen_cd.

    CLASS-METHODS prepare_skeleton
      RETURNING VALUE(bookings) TYPE table_of_bookings.

    DATA travels          TYPE travel_type.
    DATA ran_travel       TYPE REF TO cl_abap_random_int.
    DATA ran_price_int    TYPE REF TO cl_abap_random_int.
    DATA ran_price_float  TYPE REF TO cl_abap_random_decfloat16.
    DATA booking_counters TYPE HASHED TABLE OF booking_counter_type WITH UNIQUE KEY travel_uuid.
    DATA date_generator   TYPE REF TO /dmo/if_date_generator.

    METHODS get_next_booking_id
      IMPORTING travel_uuid   TYPE sysuuid_x16
      RETURNING VALUE(result) TYPE /dmo/booking_id.

ENDCLASS.


CLASS lcl_booking_gen_cd IMPLEMENTATION.
  METHOD constructor.
    DATA(booking_skeleton) = prepare_skeleton( ).

    DATA(booking_features) = VALUE feature_structure( with_db           = abap_true
                                                      with_admin_fields = abap_false
                                                      with_semantic_id  = abap_false
                                                      with_uuid         = abap_true ).

    DATA(field_mapping) = VALUE field_structure( uuid = 'booking_uuid' ).

    super->constructor( skeleton_data     = REF #( booking_skeleton )
                        scenario_name     = 'Booking Coll. Draft' ##NO_TEXT
                        package_name      = '/DMO/FLIGHT_COLLDRAFT'
                        table_name_active = '/dmo/a_book_cd'
                        table_name_draft  = '/dmo/d_book_cd'
                        features          = booking_features
                        fields            = field_mapping ).
  ENDMETHOD.

  METHOD get_instance.
    IF booking_generator_instance IS NOT BOUND.
      booking_generator_instance = NEW lcl_booking_gen_cd( ).
    ENDIF.

    RETURN booking_generator_instance.
  ENDMETHOD.

  METHOD prepare_skeleton.
    CONSTANTS booking_amount TYPE i VALUE 200.

    DO booking_amount TIMES.
      APPEND VALUE /dmo/a_book_cd( ) TO bookings.
    ENDDO.

    RETURN bookings.
  ENDMETHOD.

  METHOD setup_for_building.
    CONSTANTS start_at_1    TYPE i VALUE 1.
    CONSTANTS seed          TYPE i VALUE 42.
    CONSTANTS price_int_min TYPE i VALUE 100.
    CONSTANTS price_int_max TYPE i VALUE 9999.

    travels = lcl_travel_gen_cd=>get_instance( )->get_data( )->*.
    ran_travel = cl_abap_random_int=>create( seed = seed
                                             min  = start_at_1
                                             max  = lines( travels ) ).

    ran_price_int   = cl_abap_random_int=>create( min = price_int_min
                                                  max = price_int_max ).
    ran_price_float = cl_abap_random_decfloat16=>create( seed = seed ).

    date_generator = /dmo/cl_data_gen_util_factory=>/dmo/if_data_gen_util_factory~create_date_gen_instance( ).
  ENDMETHOD.

  METHOD build_additional_fields.
    FIELD-SYMBOLS <booking> TYPE /dmo/a_book_cd.

    ASSIGN entry->* TO <booking>.

    DATA(travel) = travels[ ran_travel->get_next( ) ].

    <booking>-parent_uuid   = travel-travel_uuid.
    <booking>-booking_id    = get_next_booking_id( travel-travel_uuid ).
    <booking>-flight_date   = date_generator->generate_date_in_range( from_date = travel-begin_date
                                                                      to_date   = travel-end_date ).
    <booking>-booking_date  = date_generator->generate_date_in_range( from_date = travel-begin_date
                                                                      to_date   = <booking>-flight_date ).
    <booking>-flight_price  = ran_price_float->get_next( ) + ran_price_int->get_next( ).
    <booking>-currency_code = travel-currency_code.
  ENDMETHOD.

  METHOD get_next_booking_id.
    ASSIGN booking_counters[ travel_uuid = travel_uuid ] TO FIELD-SYMBOL(<counter>).
    IF sy-subrc <> 0.
      INSERT VALUE #( travel_uuid = travel_uuid
                      count       = 0 ) INTO TABLE booking_counters ASSIGNING <counter>.
    ENDIF.
    <counter>-count += 1.
    result = <counter>-count.
  ENDMETHOD.

ENDCLASS.

