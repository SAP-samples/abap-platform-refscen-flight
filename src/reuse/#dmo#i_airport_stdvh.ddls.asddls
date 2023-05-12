@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Airport ValueHelp'

@Search.searchable: true

define view entity /DMO/I_Airport_StdVH
  as select from /DMO/I_Airport as Airport
{
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: ['Name']
      @UI.textArrangement: #TEXT_SEPARATE
      @UI.lineItem: [{ position: 10, importance: #HIGH }]
  key AirportID,

      @Semantics.text: true
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @UI.lineItem: [{ position: 20, importance: #HIGH }]
      Name,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @UI.lineItem: [{ position: 30, importance: #MEDIUM }]
      City,

      @Consumption.valueHelpDefinition: [{entity: { name: 'I_CountryVH', element: 'Country' }, useForValidation: true }]
      @ObjectModel.text.element: ['CountryCodeText']
      @UI.textArrangement: #TEXT_ONLY
      @UI.lineItem: [{ position: 40, importance: #MEDIUM }]
      CountryCode,

      @UI.hidden: true
      _Country._Text[1:Language = $session.system_language].CountryName as CountryCodeText
}
