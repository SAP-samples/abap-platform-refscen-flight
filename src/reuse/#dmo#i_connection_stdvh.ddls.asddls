@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Connection ValueHelp'
@Search.searchable: true

define view entity /DMO/I_Connection_StdVH
  as select from /DMO/I_Connection
{

      @Search.defaultSearchElement: true
      @ObjectModel.text.element: ['AirlineName']
      @UI.textArrangement: #TEXT_FIRST
      @UI.lineItem: [{ position: 10, importance: #HIGH }]
      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Carrier_StdVH', element: 'AirlineID' }, useForValidation: true}]
  key AirlineID,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @UI.lineItem: [{ position: 20, importance: #HIGH }]
  key ConnectionID,

      @UI.hidden: true
      @Semantics.text: true
      @Search.fuzzinessThreshold: 0.8
      _Airline.Name            as AirlineName,


      @Search.defaultSearchElement: true
      @ObjectModel.text.element: ['DepartureAirportName']
      @UI.textArrangement: #TEXT_FIRST
      @UI.lineItem: [{ position: 30, importance: #HIGH }]
      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Airport_StdVH', element: 'AirportID' }, useForValidation: true}]
      DepartureAirport,

      @UI.hidden: true
      @Semantics.text: true
      @Search.fuzzinessThreshold: 0.8
      _DepartureAirport.Name   as DepartureAirportName,

      @Search.defaultSearchElement: true
      @ObjectModel.text.element: ['DestinationAirportName']
      @UI.textArrangement: #TEXT_FIRST
      @UI.lineItem: [{ position: 30, importance: #HIGH }]
      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Airport_StdVH', element: 'AirportID' }, useForValidation: true}]
      DestinationAirport,

      @UI.hidden: true
      @Semantics.text: true
      @Search.fuzzinessThreshold: 0.8
      _DestinationAirport.Name as DestinationAirportName,

      @UI.lineItem: [{ position: 40, importance: #MEDIUM }]
      DepartureTime,

      @UI.lineItem: [{ position: 50, importance: #MEDIUM }]
      ArrivalTime,

      @UI.lineItem: [{ position: 50, importance: #LOW }]
      Distance,

      @UI.hidden: true
      DistanceUnit
}
