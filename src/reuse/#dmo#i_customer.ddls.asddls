@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer View - CDS Data Model'

@Search.searchable: true

define view entity /DMO/I_Customer
  as select from /dmo/customer as Customer

  association [0..1] to I_Country as _Country on $projection.CountryCode = _Country.Country

{
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: ['LastName']
  key Customer.customer_id   as CustomerID,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Semantics.name.givenName: true
      Customer.first_name    as FirstName,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Semantics.name.familyName: true
      @Semantics.text: true
      Customer.last_name     as LastName,

      @Semantics.name.prefix: true
      Customer.title         as Title,

      @Semantics.address.street: true
      Customer.street        as Street,

      @Semantics.address.zipCode: true
      Customer.postal_code   as PostalCode,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Semantics.address.city: true
      Customer.city          as City,

      @Consumption.valueHelpDefinition: [{entity: { name: 'I_CountryVH', element: 'Country' }, useForValidation: true }]
      @Semantics.address.country: true
      Customer.country_code  as CountryCode,

      @Semantics.telephone.type: [#HOME]
      Customer.phone_number  as PhoneNumber,

      @Semantics.eMail.address: true
      Customer.email_address as EMailAddress,

      /* Associations */
      _Country

}
