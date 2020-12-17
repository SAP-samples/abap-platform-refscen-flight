@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Overall Status Value Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
 serviceQuality: #A,
 sizeCategory: #S,
 dataClass: #MASTER
 }
@ObjectModel.resultSet.sizeCategory: #XS
define view entity /DMO/I_Overall_Status_VH_Text
  as select from /dmo/oall_stat_t

  association [1..1] to /DMO/I_Overall_Status_VH as _OverallStatus on $projection.OverallStatus = _OverallStatus.OverallStatus

{
      @ObjectModel.text.element: ['Text']
  key overall_status as OverallStatus,

      @Semantics.language: true
  key language       as Language,

      @Semantics.text: true
      text           as Text,

      _OverallStatus
}
