@EndUserText.label: 'Agency'
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
  key AgencyID,
      Name,
      Street,
      PostalCode,
      City,
      CountryCode,
      PhoneNumber,
      EMailAddress,
      WebAddress,
      Attachment,
      MimeType,
      Filename,
      LocalCreatedBy,
      LocalCreatedAt,
      LocalLastChangedBy,
      LocalLastChangedAt,
      LastChangedAt,
      /* Associations */
      _Country
}
