*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_abap_behv_msg DEFINITION CREATE PUBLIC INHERITING FROM cx_no_check.
  PUBLIC SECTION.

    INTERFACES if_abap_behv_message .

    ALIASES msgty
      FOR if_t100_dyn_msg~msgty .
    ALIASES msgv1
      FOR if_t100_dyn_msg~msgv1 .
    ALIASES msgv2
      FOR if_t100_dyn_msg~msgv2 .
    ALIASES msgv3
      FOR if_t100_dyn_msg~msgv3 .
    ALIASES msgv4
      FOR if_t100_dyn_msg~msgv4 .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgty    TYPE symsgty OPTIONAL
        !msgv1    TYPE simple OPTIONAL
        !msgv2    TYPE simple OPTIONAL
        !msgv3    TYPE simple OPTIONAL
        !msgv4    TYPE simple OPTIONAL .

ENDCLASS.
