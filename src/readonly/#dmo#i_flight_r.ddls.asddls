@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Carrier View - CDS Data Model'


@Search.searchable: true


define view entity /DMO/I_Flight_R
  as select from /dmo/flight as Flight

  association [1] to /DMO/I_Carrier as _Airline on $projection.AirlineID = _Airline.AirlineID


{
      @UI.lineItem: [ { position: 10, label: 'Airline'} ]
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.7
      @ObjectModel.text.association: '_Airline'
  key Flight.carrier_id     as AirlineID,

      @UI.lineItem: [ { position: 20, label: 'Connection Number' } ]
  key Flight.connection_id  as ConnectionID,

      @UI.lineItem: [ { position: 30, label: 'Flight Date' } ]
  key Flight.flight_date    as FlightDate,

      @UI.lineItem: [ { position: 40, label: 'Price' } ]
      @Semantics.amount.currencyCode: 'CurrencyCode'
      Flight.price          as Price,

      Flight.currency_code  as CurrencyCode,

      @UI.lineItem: [ { position: 50, label: 'Plane Type' } ]
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.7
      Flight.plane_type_id  as PlaneType,

      @UI.lineItem: [ { position: 60, label: 'Maximum Seats' } ]
      Flight.seats_max      as MaximumSeats,

      @UI.lineItem: [ { position: 70, label: 'Occupied Seats' } ]
      Flight.seats_occupied as OccupiedSeats,

      /* Associations */
      _Airline
}
