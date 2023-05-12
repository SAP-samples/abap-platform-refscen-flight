@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agency View - CDS Data Model'
@Search.searchable: true

define view entity /DMO/I_Agency
  as select from /dmo/agency as Agency

  association [0..1] to I_Country as _Country on $projection.CountryCode = _Country.Country

{
      @ObjectModel.text.element: ['Name']
  key Agency.agency_id             as AgencyID,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Semantics.text: true
      @Semantics.organization.name: true
      Agency.name                  as Name,



      @Semantics.address.street: true
      Agency.street                as Street,

      @Semantics.address.zipCode: true
      Agency.postal_code           as PostalCode,

      @Search.defaultSearchElement: true
      @Semantics.address.city: true
      Agency.city                  as City,

      @Consumption.valueHelpDefinition: [{entity: { name: 'I_CountryVH', element: 'Country' } }]
      @Semantics.address.country: true
      Agency.country_code          as CountryCode,



      @Semantics.telephone.type: [#WORK]
      Agency.phone_number          as PhoneNumber,

      @Semantics.eMail.address: true
      Agency.email_address         as EMailAddress,

      Agency.web_address           as WebAddress,



      @Semantics.largeObject: { mimeType: 'MimeType',
                                fileName: 'Filename',
                                contentDispositionPreference: #INLINE }
      Agency.attachment            as Attachment,

      @Semantics.mimeType: true
      Agency.mime_type             as MimeType,

      Agency.filename              as Filename,



      @Semantics.user.createdBy: true
      Agency.local_created_by      as LocalCreatedBy,

      @Semantics.systemDateTime.createdAt: true
      Agency.local_created_at      as LocalCreatedAt,

      @Semantics.user.lastChangedBy: true
      Agency.local_last_changed_by as LocalLastChangedBy,

      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      Agency.local_last_changed_at as LocalLastChangedAt,

      @Semantics.systemDateTime.lastChangedAt: true
      Agency.last_changed_at       as LastChangedAt,

      /* Associations */
      _Country
}
