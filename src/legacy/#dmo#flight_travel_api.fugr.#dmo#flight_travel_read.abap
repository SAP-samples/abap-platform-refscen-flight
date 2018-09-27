FUNCTION /DMO/FLIGHT_TRAVEL_READ.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_TRAVEL_ID) TYPE  /DMO/TRAVEL_ID
*"     REFERENCE(IV_INCLUDE_BUFFER) TYPE  ABAP_BOOLEAN DEFAULT
*"       ABAP_TRUE
*"  EXPORTING
*"     REFERENCE(ES_TRAVEL) TYPE  /DMO/TRAVEL
*"     REFERENCE(ET_BOOKING) TYPE  /DMO/IF_FLIGHT_LEGACY=>TT_BOOKING
*"     REFERENCE(ET_BOOKING_SUPPLEMENT) TYPE
*"        /DMO/IF_FLIGHT_LEGACY=>TT_BOOKING_SUPPLEMENT
*"     REFERENCE(ET_MESSAGES) TYPE  /DMO/IF_FLIGHT_LEGACY=>TT_MESSAGE
*"----------------------------------------------------------------------
  CLEAR es_travel.
  CLEAR et_booking.
  CLEAR et_booking_supplement.
  CLEAR et_messages.

  /dmo/cl_flight_legacy=>get_instance( )->get_travel( EXPORTING iv_travel_id          = iv_travel_id
                                                                iv_include_buffer     = iv_include_buffer
                                                      IMPORTING es_travel             = es_travel
                                                                et_booking            = et_booking
                                                                et_booking_supplement = et_booking_supplement
                                                                et_messages           = DATA(lt_messages) ).

  /dmo/cl_flight_legacy=>get_instance( )->convert_messages( EXPORTING it_messages = lt_messages
                                                            IMPORTING et_messages = et_messages ).
ENDFUNCTION.
