@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@EndUserText.label: 'Draft Query View /DMO/D_TRAVEL_CD'
define view entity /DMO/R_TRAVELTPDRAFT_CD
  as select from /dmo/d_travel_cd
{
  key traveluuid as TravelUuid,
  travelid as TravelId,
  begindate as BeginDate,
  enddate as EndDate,
  bookingfee as BookingFee,
  currencycode as CurrencyCode,
  description as Description,
  localcreatedby as LocalCreatedBy,
  localcreatedat as LocalCreatedAt,
  locallastchangedby as LocalLastChangedBy,
  locallastchangedat as LocalLastChangedAt,
  lastchangedat as LastChangedAt,
  draftentitycreationdatetime as draftentitycreationdatetime,
  draftentitylastchangedatetime as draftentitylastchangedatetime,
  draftadministrativedatauuid as draftadministrativedatauuid,
  draftentityoperationcode as draftentityoperationcode,
  hasactiveentity as hasactiveentity,
  draftfieldchanges as draftfieldchanges
}
