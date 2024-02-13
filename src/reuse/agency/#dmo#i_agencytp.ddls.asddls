@Metadata.ignorePropagatedAnnotations: true
@EndUserText.label: 'Agency'
@Search.searchable: true
@AccessControl.authorizationCheck: #NOT_REQUIRED

@AbapCatalog.extensibility: {
  extensible: true,
  dataSources: ['AGENCY'],
  elementSuffix: 'ZAG',
  quota: {
    maximumFields: 500,
    maximumBytes: 50000
  }, allowNewCompositions: true
}

define root view entity /DMO/I_AgencyTP
  provider contract transactional_interface
  as projection on /DMO/R_AgencyTP as Agency
{
      @ObjectModel.text.element: ['Name']
  key AgencyID,
  
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Semantics.text: true
      @Semantics.organization.name: true
      Name,
      
      @Semantics.address.street: true
      Street,
      
      @Semantics.address.zipCode: true
      PostalCode,
      
      @Search.defaultSearchElement: true
      @Semantics.address.city: true
      City,
      
      @Consumption.valueHelpDefinition: [{entity: { name: 'I_CountryVH', element: 'Country' } }]
      @Semantics.address.country: true
      CountryCode,
      
      @Semantics.telephone.type: [#WORK]
      PhoneNumber,
      
      @Semantics.eMail.address: true
      EMailAddress,
      WebAddress,
      
      @Semantics.largeObject: { mimeType: 'MimeType',
                                fileName: 'Filename',
                                contentDispositionPreference: #INLINE }
      Attachment,
      
      @Semantics.mimeType: true
      MimeType,
      Filename,
      
      @Semantics.user.createdBy: true
      LocalCreatedBy,
      
      @Semantics.systemDateTime.createdAt: true
      LocalCreatedAt,
      
      @Semantics.user.lastChangedBy: true
      LocalLastChangedBy,
      
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      LocalLastChangedAt,
      
      @Semantics.systemDateTime.lastChangedAt: true
      LastChangedAt,
      /* Associations */
      _Country
}
