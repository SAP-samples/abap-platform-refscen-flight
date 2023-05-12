@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Flight View - CDS Data Model'

@Search.searchable: true

define view entity /DMO/I_Flight
  as select from /dmo/flight as Flight

  association [1] to /DMO/I_Carrier as _Airline         on $projection.AirlineID = _Airline.AirlineID
  association [1] to /DMO/I_Connection as _Connection   on $projection.ConnectionID = _Connection.ConnectionID 
                                                       and $projection.AirlineID = _Connection.AirlineID
  association [0..1] to I_Currency     as _Currency     on $projection.CurrencyCode = _Currency.Currency

{
      @Search.defaultSearchElement: true
      @ObjectModel.text.association: '_Airline'
      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Carrier_StdVH', element: 'AirlineID' }, useForValidation: true}]
  key Flight.carrier_id     as AirlineID,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Connection_StdVH', element: 'ConnectionID'}, 
                     additionalBinding: [{ element: 'AirlineID', localElement: 'AirlineID' }] }]
  key Flight.connection_id  as ConnectionID,

      @Search.defaultSearchElement: true
  key Flight.flight_date    as FlightDate,

      @Semantics.amount.currencyCode: 'CurrencyCode'
      Flight.price          as Price,

      Flight.currency_code  as CurrencyCode,

      Flight.plane_type_id  as PlaneType,

      Flight.seats_max      as MaximumSeats,

      Flight.seats_occupied as OccupiedSeats,

      /* Associations */
      _Airline,
      _Connection,
      _Currency
}
