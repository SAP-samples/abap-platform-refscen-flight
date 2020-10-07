CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS calculate_total_supplm_price FOR DETERMINE ON MODIFY IMPORTING keys FOR booksuppl~calculateTotalSupplmPrice.
    METHODS get_features                 FOR FEATURES            IMPORTING keys REQUEST requested_features FOR booksuppl RESULT result.

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

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY booksuppl
         FIELDS ( booking_supplement_id )
           WITH VALUE #( FOR keyval IN keys ( %tky = keyval-%tky ) )
         RESULT  DATA(lt_booksupppl_result).


    result = VALUE #( FOR ls_travel IN lt_booksupppl_result
                       ( %tky                         = ls_travel-%tky
                         %field-booking_supplement_id = if_abap_behv=>fc-f-read_only
                        ) ).

  ENDMETHOD.



ENDCLASS.
