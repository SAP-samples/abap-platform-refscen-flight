CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS calculate_total_supplm_price FOR DETERMINATION booksuppl~calculateTotalSupplmPrice IMPORTING keys FOR booksuppl.
    METHODS get_features                 FOR FEATURES IMPORTING keys REQUEST requested_features FOR booksuppl RESULT result.

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

********************************************************************************
*
* Implements the dynamic feature handling for booking supplement instances
*
********************************************************************************
  METHOD get_features.

      READ ENTITY /dmo/i_booksuppl_m
           FIELDS ( booking_supplement_id )
             WITH VALUE #( FOR keyval IN keys ( %key = keyval-%key ) )
           RESULT  DATA(lt_booksupppl_result).


    result = VALUE #( FOR ls_travel IN lt_booksupppl_result
                       ( %key                         = ls_travel-%key
                         %field-booking_supplement_id = if_abap_behv=>fc-f-read_only
                         "%features-%delete = if_abap_behv=>fc-o-disabled  " Workaround for missing determinations on delete
                     ) ).

  ENDMETHOD.



ENDCLASS.
