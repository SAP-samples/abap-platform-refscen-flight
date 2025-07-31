@AccessControl.authorizationCheck: #MANDATORY

@EndUserText.label: 'Booking View Entity Coll. Draft RefScen'

@Metadata.ignorePropagatedAnnotations: true

define view entity /DMO/R_BookingTP_CD
  as select from /dmo/a_book_cd

  association to parent /DMO/R_TravelTP_CD as _Travel on $projection.TravelUuid = _Travel.TravelUuid

{
  key booking_uuid  as BookingUuid,

      parent_uuid   as TravelUuid,
      booking_id    as BookingId,
      booking_date  as BookingDate,
      flight_date   as FlightDate,

      @Semantics.amount.currencyCode: 'CurrencyCode'
      flight_price  as FlightPrice,

      currency_code as CurrencyCode,

      /* Associations */
      _Travel
}
