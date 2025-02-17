CLASS /dmo/cl_data_gen_hierarchydrft DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES /dmo/if_data_generation_badi .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/cl_data_gen_hierarchydrft IMPLEMENTATION.


  METHOD /dmo/if_data_generation_badi~data_generation.
    lcl_agency_generator=>get_instance( )->create( out ).
    NEW lcl_employee_generator( )->create( out ).
  ENDMETHOD.
ENDCLASS.
