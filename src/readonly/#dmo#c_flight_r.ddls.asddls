@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Flight Consumption View'
@Metadata.allowExtensions: true

@Search.searchable: true

define view entity /DMO/C_Flight_R
  as select from /DMO/I_Flight_R
{
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.7
      @ObjectModel.text.element: ['AirlineName']
  key AirlineID,
  key ConnectionID,
  key FlightDate,

      _Airline.Name as AirlineName,

      Price,
      CurrencyCode,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.7
      PlaneType,
      MaximumSeats,
      OccupiedSeats,
      OccupiedSeats as OccupiedSeatsForChart,

      /* Associations */
      _Airline
}
