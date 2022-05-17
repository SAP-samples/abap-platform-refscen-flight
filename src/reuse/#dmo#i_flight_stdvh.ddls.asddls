@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Flight ValueHelp'
@Search.searchable: true

define view entity /DMO/I_Flight_StdVH
  as select from /DMO/I_Flight
{
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: ['AirlineName']
      @UI.textArrangement: #TEXT_FIRST
      @UI.lineItem: [{ position: 10, importance: #HIGH }]
      @UI.selectionField: [{ position: 10 }]
      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Carrier_StdVH', element: 'AirlineID' }, useForValidation: true}]
  key AirlineID,

      @Search.defaultSearchElement: true
      @UI.lineItem: [{ position: 20, importance: #HIGH }]
      @UI.selectionField: [{ position: 20 }]
      @Consumption.valueHelpDefinition: [
        {  entity: {name: '/DMO/I_Connection_StdVH', element: 'ConnectionID' }, 
           additionalBinding: [{ element: 'AirlineID', localElement: 'AirlineID', usage: #FILTER_AND_RESULT }],
           useForValidation: true } ]
  key ConnectionID,

      @Search.defaultSearchElement: true
      @UI.lineItem: [{ position: 30, importance: #HIGH }]
      @UI.selectionField: [{ position: 30 }]
  key FlightDate,

      @Semantics.amount.currencyCode: 'CurrencyCode'
      @UI.lineItem: [{ position: 40, importance: #HIGH }]
      Price,

      @UI.hidden: true
      CurrencyCode,

      @UI.lineItem: [{ position: 50, importance: #LOW }]
      PlaneType,
      @UI.lineItem: [{ position: 60, importance: #LOW }]
      MaximumSeats,
      @UI.lineItem: [{ position: 70, importance: #LOW }]
      OccupiedSeats,

      @UI.hidden: true
      _Airline.Name as AirlineName
}
