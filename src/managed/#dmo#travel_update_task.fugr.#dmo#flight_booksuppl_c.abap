FUNCTION /dmo/flight_booksuppl_c.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(VALUES) TYPE  /DMO/TT_BOOKSUPPL_M
*"----------------------------------------------------------------------
  INSERT /dmo/booksuppl_m FROM TABLE @values.

ENDFUNCTION. "#EC CI_VALPAR
