@AbapCatalog.sqlViewName: '/DMO/ICUSTOM_RE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer View - CDS Data Model'

@Search.searchable: true

define view /DMO/I_Customer
  as select from /dmo/customer as Customer

  association [0..1] to I_Country as _Country on $projection.CountryCode = _Country.Country

{
      @ObjectModel.text.element: ['LastName']
  key Customer.customer_id   as CustomerID,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      Customer.first_name    as FirstName,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Semantics.text: true
      Customer.last_name     as LastName,

      Customer.title         as Title,

      Customer.street        as Street,

      Customer.postal_code   as PostalCode,

      Customer.city          as City,

      @Consumption.valueHelpDefinition: [{entity: { name: 'I_Country', element: 'country' } }]
      Customer.country_code  as CountryCode,

      Customer.phone_number  as PhoneNumber,

      Customer.email_address as EMailAddress,

      /* Associations */
      _Country

}
