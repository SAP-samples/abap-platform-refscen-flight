@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Status Value Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
 serviceQuality: #A,
 sizeCategory: #S,
 dataClass: #MASTER
 }
@ObjectModel.resultSet.sizeCategory: #XS
define view entity /DMO/I_Booking_Status_VH_Text
  as select from /dmo/book_stat_t

  association [1..1] to /DMO/I_Booking_Status_VH as _BookingStatus on $projection.BookingStatus = _BookingStatus.BookingStatus

{
      @ObjectModel.text.element: ['Text']
  key booking_status as BookingStatus,

      @Semantics.language: true
  key language       as Language,

      @Semantics.text: true
      text           as Text,

      _BookingStatus
}
