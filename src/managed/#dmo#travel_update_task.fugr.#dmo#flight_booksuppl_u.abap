FUNCTION /DMO/FLIGHT_BOOKSUPPL_U.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(VALUES) TYPE  /DMO/TT_BOOKSUPPL_M
*"----------------------------------------------------------------------

  UPDATE /dmo/booksuppl_m FROM TABLE @values.

ENDFUNCTION.
