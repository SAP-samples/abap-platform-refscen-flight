FUNCTION /dmo/flight_booksuppl_u.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(VALUES) TYPE  /DMO/TT_BOOKSUPPL_M
*"----------------------------------------------------------------------
  UPDATE /dmo/booksuppl_m FROM TABLE @values.

ENDFUNCTION.  "#EC CI_VALPAR
