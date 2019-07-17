@AbapCatalog.sqlViewName: '/DMO/ICONNECT_RE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Connection View - CDS Data Model'

@Search.searchable: true

define view /DMO/I_Connection
  as select from /dmo/connection as Connection
  
  association [1..1] to /DMO/I_Carrier as _Airline  on $projection.AirlineID = _Airline.AirlineID

{
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @ObjectModel.text.association: '_Airline'
      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Carrier', element: 'CarrierID'} }]
  key Connection.carrier_id      as AirlineID,

  key Connection.connection_id   as ConnectionID,

      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Airport', element: 'Airport_ID' } }]
      Connection.airport_from_id as DepartureAirport,

      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Airport', element: 'Airport_ID' } }]
      Connection.airport_to_id   as DestinationAirport,

      Connection.departure_time  as DepartureTime,

      Connection.arrival_time    as ArrivalTime,

      @Semantics.quantity.unitOfMeasure: 'DistanceUnit'
      Connection.distance        as Distance,

      @Semantics.unitOfMeasure: true
      Connection.distance_unit   as DistanceUnit, 
      
      /* Associations */
      _Airline
}
