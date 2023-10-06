@AbapCatalog.viewEnhancementCategory: [#PROJECTION_LIST]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agency - Extension Include View'

@AbapCatalog.extensibility: {
  extensible: true,
  elementSuffix: 'ZAG',
  allowNewDatasources: false,
  dataSources: ['Agency'],
  quota: {
    maximumFields: 500,
    maximumBytes: 50000
  }
}



define view entity /DMO/E_Agency
  as select from /dmo/agency as Agency
{
  key agency_id as AgencyId

}
