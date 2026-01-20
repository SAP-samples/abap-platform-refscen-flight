CLASS /dmo/cl_data_gen_agency_xbo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES /dmo/if_data_generation_badi .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/cl_data_gen_agency_xbo IMPLEMENTATION.


  METHOD /dmo/if_data_generation_badi~data_generation.
    lcl_agency_gen_xbo=>get_instance( )->create( out ).
    lcl_travel_gen_xbo=>get_instance( )->create( out ).
  ENDMETHOD.
ENDCLASS.
