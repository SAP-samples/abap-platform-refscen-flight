CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY IMPORTING keys FOR booksuppl~calculateTotalPrice.

ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

********************************************************************************
*
* Calculates total supplement price
*
********************************************************************************
  METHOD calculateTotalPrice.



    MODIFY ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY Travel
        EXECUTE ReCalcTotalPrice
        FROM CORRESPONDING #( keys )
    REPORTED DATA(reported_modify).

    reported = CORRESPONDING #( DEEP reported_modify ).
  ENDMETHOD.



ENDCLASS.
