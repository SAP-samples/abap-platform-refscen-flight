class /dmo/cl_abstract_data_gen definition PUBLIC ABSTRACT create PROTECTED.

  PUBLIC SECTION.
    TYPES: BEGIN OF field_structure,
             uuid                  TYPE string,
             semantic_id           TYPE string,
             local_created_at      TYPE string,
             last_changed_at       TYPE string,
             local_last_changed_at TYPE string,
             local_created_by      TYPE string,
             local_last_changed_by TYPE string,
           END OF field_structure.
    TYPES: BEGIN OF feature_structure,
             with_uuid         TYPE abap_bool,
             with_semantic_id  TYPE abap_bool,
             with_admin_fields TYPE abap_bool,
           END OF feature_structure.

    METHODS get_data RETURNING VALUE(result) TYPE REF TO data.

    METHODS create
      IMPORTING !out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

  PROTECTED SECTION.
    DATA fields   TYPE field_structure.
    DATA features TYPE feature_structure.

    METHODS constructor
      IMPORTING skeleton_data        TYPE REF TO data                         OPTIONAL
                scenario_name        TYPE string
                package_name         TYPE string                              OPTIONAL
                table_name_active    TYPE string
                table_name_draft     TYPE string                              OPTIONAL
                nr_minimum           TYPE n                                   DEFAULT '070001'
                nr_maximum           TYPE n                                   DEFAULT '079999'
                numberrange_object   TYPE cl_numberrange_runtime=>nr_object   DEFAULT '/DMO/TRAVL'
                numberrange_interval TYPE cl_numberrange_runtime=>nr_interval DEFAULT '01'
                !features            TYPE feature_structure
                !fields              TYPE field_structure.

    METHODS build_additional_fields
      CHANGING !entry TYPE REF TO data.

    METHODS setup_for_building.

  PRIVATE SECTION.
    DATA gen_data             TYPE REF TO data.
    DATA skeleton_data        TYPE REF TO data.
    DATA scenario_name        TYPE string.
    DATA package_name         TYPE string.
    DATA table_name_active    TYPE string.
    DATA table_name_draft     TYPE string.
    DATA nr_minimum           TYPE n LENGTH 6.
    DATA nr_maximum           TYPE n LENGTH 6.
    DATA numberrange_interval TYPE cl_numberrange_runtime=>nr_interval.
    DATA numberrange_object   TYPE cl_numberrange_runtime=>nr_object.

    METHODS check_database_table_name
      IMPORTING table_name                TYPE string
      RETURNING VALUE(table_name_checked) TYPE string.

    METHODS delete_content.
    METHODS insert_content.
    METHODS set_numberrange.
    METHODS build_content.
    METHODS build_uuid.
    METHODS build_semantic_key.
    METHODS build_admin_fields.
    METHODS build_skeleton.
    METHODS build_additional_fields_loop.


endclass.

class /dmo/cl_abstract_data_gen implementation.
  METHOD constructor.
    me->table_name_active = table_name_active.

    CREATE DATA gen_data TYPE STANDARD TABLE OF (table_name_active) WITH EMPTY KEY.
    CREATE DATA me->skeleton_data TYPE STANDARD TABLE OF (table_name_active) WITH EMPTY KEY.

    IF skeleton_data IS BOUND AND skeleton_data->* IS NOT INITIAL.
      me->skeleton_data->* = skeleton_data->*.
    ENDIF.

    me->scenario_name        = scenario_name.
    me->package_name         = package_name.
    me->table_name_draft     = table_name_draft.
    me->nr_minimum           = nr_minimum.
    me->nr_maximum           = nr_maximum.
    me->numberrange_interval = numberrange_interval.
    me->numberrange_object   = numberrange_object.
    me->fields               = fields.
    me->features             = features.
  ENDMETHOD.

  METHOD check_database_table_name.
    DATA error TYPE REF TO cx_static_check.
    DATA(table_name_upper) = to_upper( table_name ).

    TRY.
        cl_abap_dyn_prg=>check_table_name_str( val      = table_name_upper
                                               packages = package_name ).
      CATCH cx_abap_not_a_table
            cx_abap_not_in_package INTO error.
        RAISE SHORTDUMP error.
    ENDTRY.

    RETURN table_name_upper.
  ENDMETHOD.

  METHOD delete_content.
    IF table_name_draft IS NOT INITIAL.
      DATA(table_name_draft_checked) = check_database_table_name( table_name_draft ).
      DELETE FROM (table_name_draft_checked). "#EC CI_NOWHERE
    ENDIF.

    DATA(table_name_active_checked) = check_database_table_name( table_name_active ).
    DELETE FROM (table_name_active_checked). "#EC CI_NOWHERE
  ENDMETHOD.

  METHOD insert_content.
    DATA(table_name_active_checked) = check_database_table_name( table_name_active ).
    INSERT (table_name_active_checked) FROM TABLE @gen_data->*.
  ENDMETHOD.

  METHOD set_numberrange.
    /dmo/cl_flight_data_generator=>reset_numberrange_interval(
        numberrange_object   = numberrange_object
        numberrange_interval = numberrange_interval
        fromnumber           = CONV cl_numberrange_intervals=>nr_nriv_line-fromnumber( nr_minimum )
        tonumber             = CONV cl_numberrange_intervals=>nr_nriv_line-tonumber(   nr_maximum ) ).
  ENDMETHOD.

  METHOD create.
    DATA(text_output) = /dmo/cl_data_gen_util_factory=>/dmo/if_data_gen_util_factory~create_text_output_instance( out ).
    ##NO_TEXT
    text_output->print_title( scenario_name ).

    text_output->print_delete( ).
    delete_content( ).

    text_output->print_build( ).
    IF features-with_semantic_id = abap_true.
      set_numberrange( ).
    ENDIF.
    get_data( ).

    text_output->print_insert( ).
    insert_content( ).

    text_output->print_done( ).
  ENDMETHOD.

  METHOD get_data.
    IF gen_data IS BOUND AND gen_data->* IS INITIAL.
      build_content( ).
    ENDIF.
    RETURN gen_data.
  ENDMETHOD.

  METHOD build_content.
    setup_for_building( ).

    IF gen_data IS BOUND AND gen_data->* IS INITIAL.
      build_skeleton( ).
    ENDIF.
    IF features-with_uuid = abap_true.
      build_uuid( ).
    ENDIF.
    IF features-with_semantic_id = abap_true.
      build_semantic_key( ).
    ENDIF.
    IF features-with_admin_fields = abap_true.
      build_admin_fields( ).
    ENDIF.
    build_additional_fields_loop( ).
  ENDMETHOD.

  METHOD build_skeleton.
    IF skeleton_data IS BOUND AND skeleton_data->* IS NOT INITIAL.
      gen_data->* = skeleton_data->*.
    ENDIF.
  ENDMETHOD.

  METHOD build_uuid.
    DATA uuid  TYPE sysuuid_x16.
    DATA error TYPE REF TO cx_uuid_error.

    LOOP AT gen_data->* ASSIGNING FIELD-SYMBOL(<entry>).
      TRY.
          uuid = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error INTO error.
          RAISE SHORTDUMP error.
      ENDTRY.

      <entry>-(fields-uuid) = uuid.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_semantic_key.
    DATA data_lines      TYPE i.
    DATA numberrange_key TYPE cl_numberrange_runtime=>nr_number.
    DATA error           TYPE REF TO cx_number_ranges.
    DATA maximum         TYPE i.
    DATA current         TYPE i.

    data_lines = lines( gen_data->* ).

    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING nr_range_nr = numberrange_interval
                    object      = numberrange_object
                    quantity    = CONV cl_numberrange_runtime=>nr_quantity( data_lines )
          IMPORTING number      = numberrange_key ).
      CATCH cx_number_ranges INTO error.
        RAISE SHORTDUMP error.
    ENDTRY.

    maximum = CONV i( numberrange_key+2 ).
    current = maximum - data_lines.

    LOOP AT gen_data->* ASSIGNING FIELD-SYMBOL(<entry>).
      current += 1.
      <entry>-(fields-semantic_id) = CONV #( current ).
    ENDLOOP.
  ENDMETHOD.

  METHOD build_admin_fields.
    DATA admin_field_generator TYPE REF TO /dmo/if_admin_field_generator.

    LOOP AT gen_data->* ASSIGNING FIELD-SYMBOL(<entry>).
      admin_field_generator = /dmo/cl_data_gen_util_factory=>/dmo/if_data_gen_util_factory~create_adm_field_gen_instance( ).

      <entry>-(fields-local_created_at)      = admin_field_generator->generate_local_created_at( ).
      <entry>-(fields-last_changed_at)       = admin_field_generator->generate_last_changed_at( ).
      <entry>-(fields-local_last_changed_at) = admin_field_generator->generate_local_last_changed_at( ).
      <entry>-(fields-local_created_by)      = admin_field_generator->generate_local_created_by( ).
      <entry>-(fields-local_last_changed_by) = admin_field_generator->generate_local_last_changed_by( ).
    ENDLOOP.
  ENDMETHOD.

  METHOD build_additional_fields.
  ENDMETHOD.

  METHOD build_additional_fields_loop.
    DATA entry_ref TYPE REF TO data.

    LOOP AT gen_data->* ASSIGNING FIELD-SYMBOL(<entry>).
      entry_ref = REF #( <entry> ).
      build_additional_fields( CHANGING entry = entry_ref ).
    ENDLOOP.
  ENDMETHOD.

  METHOD setup_for_building.
  ENDMETHOD.

endclass.
