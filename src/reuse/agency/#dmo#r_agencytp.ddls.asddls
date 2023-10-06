@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agency'
@Search.searchable: true

@AbapCatalog.extensibility: {
  extensible: true,
  elementSuffix: 'ZAG',
  allowNewDatasources: false,
  dataSources: ['_Extension'],
  quota: {
    maximumFields: 500,
    maximumBytes: 50000
  },
  allowNewCompositions: true
}

define root view entity /DMO/R_AgencyTP
  as select from /DMO/I_Agency as Agency
  association [0..1] to I_Country     as _Country   on $projection.CountryCode = _Country.Country
  association [1]    to /DMO/E_Agency as _Extension on $projection.AgencyID = _Extension.AgencyId
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

      _Country
}
