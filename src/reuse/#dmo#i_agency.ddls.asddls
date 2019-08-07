@AbapCatalog.sqlViewName: '/DMO/IAGENCY_RE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agency View - CDS Data Model'

@Search.searchable: true

define view /DMO/I_Agency
  as select from /dmo/agency as Agency

  association [0..1] to I_Country as _Country on $projection.CountryCode = _Country.Country

{
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: ['Name']
  key Agency.agency_id     as AgencyID,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Semantics.text: true
      Agency.name          as Name,

      Agency.street        as Street,

      Agency.postal_code   as PostalCode,

      Agency.city          as City,

      @Consumption.valueHelpDefinition: [{entity: { name: 'I_Country', element: 'country' } }]
      Agency.country_code  as CountryCode,

      Agency.phone_number  as PhoneNumber,

      Agency.email_address as EMailAddress,

      Agency.web_address   as WebAddress,

      /* Associations */
      _Country
}
