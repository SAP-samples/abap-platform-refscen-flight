@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Airport View - CDS Data Model'

@Search.searchable: true

define view entity /DMO/I_Airport
  as select from /dmo/airport as Airport

  association [0..1] to I_Country as _Country on $projection.CountryCode = _Country.Country

{
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: ['Name']
  key Airport.airport_id as AirportID,

      @Semantics.text: true
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      Airport.name       as Name,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      Airport.city       as City,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Consumption.valueHelpDefinition: [{entity: { name: 'I_CountryVH', element: 'Country' } }]
      Airport.country    as CountryCode,

      /* Associations */
      _Country
}
