@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agency ValueHelp'
@Search.searchable: true

define view entity /DMO/I_Agency_StdVH
  as select from /DMO/I_Agency as Agency
{
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: ['Name']
      @UI.textArrangement: #TEXT_SEPARATE
      @UI.lineItem: [{ position: 10, importance: #HIGH }]
  key AgencyID,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Semantics.text: true
      @UI.lineItem: [{ position: 20, importance: #HIGH }]
      Name,

      @UI.lineItem: [{ position: 30, importance: #LOW }]
      Street,

      @UI.lineItem: [{ position: 40, importance: #MEDIUM }]
      PostalCode,

      @Search.defaultSearchElement: true
      @UI.lineItem: [{ position: 50, importance: #MEDIUM }]
      City,

      @Consumption.valueHelpDefinition: [{entity: { name: 'I_CountryVH', element: 'Country' }, useForValidation: true }]
      @ObjectModel.text.element: ['CountryCodeText']
      @UI.textArrangement: #TEXT_ONLY
      @UI.lineItem: [{ position: 60, importance: #MEDIUM }]
      CountryCode,

      @UI.hidden: true
      _Country._Text[1:Language = $session.system_language].CountryName as CountryCodeText,

      @UI.lineItem: [{ position: 70, importance: #LOW }]
      PhoneNumber,

      @UI.lineItem: [{ position: 80, importance: #LOW }]
      EMailAddress,

      @UI.lineItem: [{ position: 90, importance: #LOW }]
      WebAddress
}
