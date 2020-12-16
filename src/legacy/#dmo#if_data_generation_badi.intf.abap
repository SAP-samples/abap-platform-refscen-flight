INTERFACE /dmo/if_data_generation_badi
  PUBLIC .

  INTERFACES if_badi_interface .

  METHODS data_generation
    IMPORTING out TYPE REF TO if_oo_adt_classrun_out  .
ENDINTERFACE.
