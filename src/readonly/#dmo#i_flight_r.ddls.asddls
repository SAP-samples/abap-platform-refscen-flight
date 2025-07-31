@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Flight View - CDS Data Model'

define view entity /DMO/I_Flight_R
  as select from /dmo/flight as Flight

  association [1] to /DMO/I_Carrier as _Airline on $projection.AirlineID = _Airline.AirlineID

{
      @ObjectModel.text.association: '_Airline'
  key Flight.carrier_id     as AirlineID,
  key Flight.connection_id  as ConnectionID,
  key Flight.flight_date    as FlightDate,

      @Semantics.amount.currencyCode: 'CurrencyCode'
      Flight.price          as Price,
      Flight.currency_code  as CurrencyCode,
      Flight.plane_type_id  as PlaneType,
      Flight.seats_max      as MaximumSeats,
      Flight.seats_occupied as OccupiedSeats,

      /* Associations */
      _Airline
}
