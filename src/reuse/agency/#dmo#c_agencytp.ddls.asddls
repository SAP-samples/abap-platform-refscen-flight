@EndUserText.label: 'Agency'
@AccessControl.authorizationCheck: #NOT_REQUIRED

@Search.searchable: true
@Metadata.allowExtensions: true
@ObjectModel.semanticKey: ['AgencyID']

@AbapCatalog.extensibility: {
  extensible: true,
  dataSources: ['Agency'],
  elementSuffix: 'ZAG',
  quota: {
    maximumFields: 500,
    maximumBytes: 50000
  }, allowNewCompositions: true  
}

define root view entity /DMO/C_AgencyTP
  provider contract transactional_query
  as projection on /DMO/R_AgencyTP as Agency
{
  key     AgencyID,

          Name,

          Street,

          PostalCode,

          City,

          @ObjectModel.text.element: ['CountryName']
          CountryCode,

          _Country._Text.CountryName as CountryName : localized,

          PhoneNumber,

          EMailAddress,

          WebAddress,

          Attachment,

          MimeType,

          Filename,

          LocalLastChangedAt,

          /* Associations */
          _Country
}
