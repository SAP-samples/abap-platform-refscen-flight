"! API for Initializing the Transactional Buffer of the Travel API
"!
FUNCTION /DMO/FLIGHT_TRAVEL_INITIALIZE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
  /dmo/cl_flight_legacy=>get_instance( )->initialize( ).
ENDFUNCTION.
