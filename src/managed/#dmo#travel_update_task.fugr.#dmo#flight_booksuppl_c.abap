FUNCTION /DMO/FLIGHT_BOOKSUPPL_C.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(VALUES) TYPE  /DMO/TT_BOOKSUPPL_M
*"----------------------------------------------------------------------

  INSERT /dmo/booksuppl_m FROM TABLE @values.

ENDFUNCTION.
