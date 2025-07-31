@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Connection View - CDS Data Model'

define view entity /DMO/I_Connection_R
  as select from /dmo/connection as Connection

  association [1..*] to /DMO/I_Flight_R as _Flight      on  $projection.AirlineID    = _Flight.AirlineID
                                                        and $projection.ConnectionID = _Flight.ConnectionID

  association [1]    to /DMO/I_Carrier  as _Airline     on  $projection.AirlineID = _Airline.AirlineID
  association [1]    to /DMO/I_Airport  as _AirportFrom on  $projection.DepartureAirport = _AirportFrom.AirportID
  association [1]    to /DMO/I_Airport  as _AirportTo   on  $projection.DestinationAirport = _AirportTo.AirportID

{
        @ObjectModel.text.association: '_Airline'
  key   Connection.carrier_id      as AirlineID,
  key   Connection.connection_id   as ConnectionID,

        @ObjectModel.text.association: '_AirportFrom'
        Connection.airport_from_id as DepartureAirport,

        @ObjectModel.text.association: '_AirportTo'
        Connection.airport_to_id   as DestinationAirport,

        Connection.departure_time  as DepartureTime,
        Connection.arrival_time    as ArrivalTime,

//      @Semantics.quantity.unitOfMeasure: 'DistanceUnit'
        Connection.distance        as Distance,
        Connection.distance_unit   as DistanceUnit,

        /* Associations */
        _Flight,
        _Airline,
        _AirportFrom,
        _AirportTo
}
