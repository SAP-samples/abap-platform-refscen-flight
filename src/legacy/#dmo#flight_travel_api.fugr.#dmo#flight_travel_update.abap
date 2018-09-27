FUNCTION /DMO/FLIGHT_TRAVEL_UPDATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_TRAVEL) TYPE  /DMO/IF_FLIGHT_LEGACY=>TS_TRAVEL_IN
*"     REFERENCE(IS_TRAVELX) TYPE  /DMO/IF_FLIGHT_LEGACY=>TS_TRAVEL_INX
*"     REFERENCE(IT_BOOKING) TYPE  /DMO/IF_FLIGHT_LEGACY=>TT_BOOKING_IN
*"       OPTIONAL
*"     REFERENCE(IT_BOOKINGX) TYPE
*"        /DMO/IF_FLIGHT_LEGACY=>TT_BOOKING_INX OPTIONAL
*"     REFERENCE(IT_BOOKING_SUPPLEMENT) TYPE
*"        /DMO/IF_FLIGHT_LEGACY=>TT_BOOKING_SUPPLEMENT_IN OPTIONAL
*"     REFERENCE(IT_BOOKING_SUPPLEMENTX) TYPE
*"        /DMO/IF_FLIGHT_LEGACY=>TT_BOOKING_SUPPLEMENT_INX OPTIONAL
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

  /dmo/cl_flight_legacy=>get_instance( )->update_travel( EXPORTING is_travel              = is_travel
                                                                   it_booking             = it_booking
                                                                   it_booking_supplement  = it_booking_supplement
                                                                   is_travelx             = is_travelx
                                                                   it_bookingx            = it_bookingx
                                                                   it_booking_supplementx = it_booking_supplementx
                                                         IMPORTING es_travel              = es_travel
                                                                   et_booking             = et_booking
                                                                   et_booking_supplement  = et_booking_supplement
                                                                   et_messages            = DATA(lt_messages) ).

  /dmo/cl_flight_legacy=>get_instance( )->convert_messages( EXPORTING it_messages = lt_messages
                                                            IMPORTING et_messages = et_messages ).
ENDFUNCTION.
