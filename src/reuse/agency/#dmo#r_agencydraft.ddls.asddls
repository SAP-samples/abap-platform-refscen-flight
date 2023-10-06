@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Draft Query View: Agency'

@AbapCatalog.extensibility: {
  extensible: true,
  dataSources: ['Agency'],
  elementSuffix: 'ZAG',
  quota: {
    maximumFields: 500,
    maximumBytes: 50000
  }
}

define view entity /DMO/R_AgencyDraft
  as select from /dmo/agency_d as Agency
{
  key agencyid                      as Agencyid,
  key draftuuid                     as Draftuuid,
      name                          as Name,
      street                        as Street,
      postalcode                    as Postalcode,
      city                          as City,
      countrycode                   as Countrycode,
      phonenumber                   as Phonenumber,
      emailaddress                  as Emailaddress,
      webaddress                    as Webaddress,
      attachment                    as Attachment,
      mimetype                      as Mimetype,
      filename                      as Filename,
      localcreatedby                as Localcreatedby,
      localcreatedat                as Localcreatedat,
      locallastchangedby            as Locallastchangedby,
      locallastchangedat            as Locallastchangedat,
      lastchangedat                 as Lastchangedat,
      draftentitycreationdatetime   as Draftentitycreationdatetime,
      draftentitylastchangedatetime as Draftentitylastchangedatetime,
      draftadministrativedatauuid   as Draftadministrativedatauuid,
      draftentityoperationcode      as Draftentityoperationcode,
      hasactiveentity               as Hasactiveentity,
      draftfieldchanges             as Draftfieldchanges
}
