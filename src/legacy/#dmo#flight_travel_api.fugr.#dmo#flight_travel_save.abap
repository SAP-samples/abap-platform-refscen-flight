"! API for Saving the Transactional Buffer of the Travel API
"!
FUNCTION /DMO/FLIGHT_TRAVEL_SAVE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
  /dmo/cl_flight_legacy=>get_instance( )->save( ).
ENDFUNCTION.
