@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Travel View Entity for Recommendations'
@Metadata.ignorePropagatedAnnotations: true
@Consumption.valueHelpDefault.fetchValues: #ON_EXPLICIT_REQUEST
define root view entity /dmo/r_traveltp_rec
  as select from /dmo/a_trvl_rec
{
  key travel_uuid           as TravelUuid,
      travel_id             as TravelId,
      description           as Description,
      destination           as Destination,
      accommodation         as Accommodation,
      @Semantics.user.createdBy: true
      local_created_by      as LocalCreatedBy,
      @Semantics.systemDateTime.createdAt: true
      local_created_at      as LocalCreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      local_last_changed_by as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt
}
