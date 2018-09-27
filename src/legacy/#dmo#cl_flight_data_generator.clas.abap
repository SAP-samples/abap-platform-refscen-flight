CLASS /dmo/cl_flight_data_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      "! Calculation of Price <br/>
      "!  <br/>
      "! Propagation to /dmo/cl_flight_legacy=>calculate_flight_price.<br/>
      "! @parameter iv_seats_occupied_percent | occupied seats
      "! @parameter iv_flight_distance | flight distance in kilometer
      "! @parameter rv_price | calculated flight price
      calculate_flight_price
        IMPORTING
          iv_seats_occupied_percent TYPE /dmo/plane_seats_occupied
          iv_flight_distance        TYPE i
        RETURNING
          VALUE(rv_price)           TYPE /dmo/flight_price.
ENDCLASS.



CLASS /dmo/cl_flight_data_generator IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    out->write( 'Starting Data Generation' ).

    out->write( 'Generate Data: Airport      /DMO/AIRPORT' ).
    lcl_airport_data_generator=>lif_data_generator~create( out ).

    out->write( 'Generate Data: Carrier      /DMO/CARRIER' ).
    lcl_carrier_data_generator=>lif_data_generator~create( out ).
    out->write( 'Generate Data: Connection   /DMO/CONNECTION' ).
    lcl_connection_data_generator=>lif_data_generator~create( out ).
    out->write( 'Generate Data: Flight       /DMO/FLIGHT' ).
    lcl_flight_data_generator=>lif_data_generator~create( out ).


    out->write( 'Generate Data: Agency       /DMO/AGENCY' ).
    lcl_agency_data_generator=>lif_data_generator~create( out ).

    out->write( 'Generate Data: Customer      /DMO/CUSTOMER' ).
    lcl_customer_data_generator=>lif_data_generator~create( out ).

    out->write( 'Generate Data: Supplement      /DMO/SUPPLEMENT' ).
    lcl_supplement_data_generator=>lif_data_generator~create( out ).

    out->write( 'Generate Data: Travel      /DMO/TRAVEL' ).
    out->write( 'Generate Data: Booking      /DMO/BOOKING' ).
    out->write( 'Generate Data: Booking Supplement      /DMO/BOOK_SUPPL' ).
    lcl_travel_data_generator=>lif_data_generator~create( out ).

    out->write( 'Finished Data Generation' ).
  ENDMETHOD.
  METHOD calculate_flight_price.
    rv_price = /dmo/cl_flight_legacy=>calculate_flight_price(
                 iv_seats_occupied_percent = iv_seats_occupied_percent
                 iv_flight_distance        = iv_flight_distance
               ).
  ENDMETHOD.

ENDCLASS.
