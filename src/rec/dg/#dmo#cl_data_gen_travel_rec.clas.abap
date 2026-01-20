CLASS /dmo/cl_data_gen_travel_rec DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_badi_interface.
    INTERFACES /dmo/if_data_generation_badi.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/cl_data_gen_travel_rec IMPLEMENTATION.
  METHOD /dmo/if_data_generation_badi~data_generation.
    acc_data_generator=>get_instance( )->create( out ).
    travel_data_generator=>get_instance( )->create( out ).
  ENDMETHOD.

ENDCLASS.
