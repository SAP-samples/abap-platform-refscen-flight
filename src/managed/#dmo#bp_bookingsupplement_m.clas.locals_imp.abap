CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS calculate_total_supplm_price FOR DETERMINE ON MODIFY IMPORTING keys FOR booksuppl~calculateTotalSupplmPrice.

ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

********************************************************************************
*
* Calculates total supplement price
*
********************************************************************************
  METHOD calculate_total_supplm_price.

    IF keys IS NOT INITIAL.
      /dmo/cl_travel_auxiliary_m=>calculate_price(
          it_travel_id = VALUE #(  FOR GROUPS <booking_suppl> OF booksuppl_key IN keys
                                       GROUP BY booksuppl_key-travel_id WITHOUT MEMBERS
                                             ( <booking_suppl> ) ) ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
