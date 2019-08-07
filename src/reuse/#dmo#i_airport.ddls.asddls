@AbapCatalog.sqlViewName: '/DMO/IAIRPORT_RE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Airport View - CDS Data Model'

@Search.searchable: true

define view /DMO/I_Airport
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
      Airport.city       as City,

      @Consumption.valueHelpDefinition: [{entity: { name: 'I_Country', element: 'country' } }]
      Airport.country    as CountryCode,

      /* Associations */
      _Country
}
