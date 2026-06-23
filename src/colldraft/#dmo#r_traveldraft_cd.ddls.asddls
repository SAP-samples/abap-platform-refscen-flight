@AccessControl.authorizationCheck: #MANDATORY

@EndUserText.label: 'Draft Query View for /DMO/D_TRAVEL_CD'

@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true

define root view entity /DMO/R_TravelDraft_CD
  as select from /dmo/d_travel_cd

{
  key traveluuid                    as TravelUuid,

      travelid                      as TravelId,
      begindate                     as BeginDate,
      enddate                       as EndDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      bookingfee                    as BookingFee,
      currencycode                  as CurrencyCode,
      description                   as Description,
      overallstatus                 as OverallStatus,
      localcreatedby                as LocalCreatedBy,
      localcreatedat                as LocalCreatedAt,
      locallastchangedby            as LocalLastChangedBy,
      locallastchangedat            as LocalLastChangedAt,
      lastchangedat                 as LastChangedAt,
      draftentitycreationdatetime   as draftentitycreationdatetime,
      draftentitylastchangedatetime as draftentitylastchangedatetime,
      draftadministrativedatauuid   as draftadministrativedatauuid,
      draftentityoperationcode      as draftentityoperationcode,
      hasactiveentity               as hasactiveentity,
      draftfieldchanges             as draftfieldchanges
}
