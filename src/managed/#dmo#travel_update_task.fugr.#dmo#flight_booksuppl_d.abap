FUNCTION /dmo/flight_booksuppl_d.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(VALUES) TYPE  /DMO/TT_BOOKSUPPL_M
*"----------------------------------------------------------------------
  DELETE /dmo/booksuppl_m FROM TABLE @values.

ENDFUNCTION.  "#EC CI_VALPAR
