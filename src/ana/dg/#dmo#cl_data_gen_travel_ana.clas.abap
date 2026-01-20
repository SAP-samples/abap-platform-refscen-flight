CLASS /dmo/cl_data_gen_travel_ana DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES /dmo/if_data_generation_badi .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/cl_data_gen_travel_ana IMPLEMENTATION.


  METHOD /dmo/if_data_generation_badi~data_generation.
    lcl_travel_gen_ana=>get_instance( )->create( out ).
  ENDMETHOD.
ENDCLASS.
