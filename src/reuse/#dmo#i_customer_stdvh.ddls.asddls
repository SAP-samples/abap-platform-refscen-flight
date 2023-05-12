@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer ValueHelp'
@Search.searchable: true

define view entity /DMO/I_Customer_StdVH
  as select from /DMO/I_Customer
{

      @UI.lineItem: [{ position: 10, importance: #HIGH }]
  key CustomerID,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @UI.lineItem: [{ position: 20, importance: #HIGH }]
      FirstName,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @UI.lineItem: [{ position: 30, importance: #HIGH }]
      LastName,

      @UI.lineItem: [{ position: 40, importance: #HIGH }]
      Title,

      @UI.lineItem: [{ position: 50, importance: #MEDIUM }]
      Street,

      @UI.lineItem: [{ position: 60, importance: #MEDIUM }]
      PostalCode,

      @UI.lineItem: [{ position: 70, importance: #MEDIUM }]
      City,

      @Consumption.valueHelpDefinition: [{entity: { name: 'I_CountryVH', element: 'Country' }, useForValidation: true }]
      @ObjectModel.text.element: ['CountryCodeText']
      @UI.textArrangement: #TEXT_ONLY
      @UI.lineItem: [{ position: 80, importance: #MEDIUM }]
      CountryCode,

      @UI.hidden: true
      _Country._Text[1:Language = $session.system_language].CountryName as CountryCodeText,

      @UI.lineItem: [{ position: 90, importance: #LOW }]
      PhoneNumber,

      @UI.lineItem: [{ position: 100, importance: #LOW }]
      EMailAddress
}
