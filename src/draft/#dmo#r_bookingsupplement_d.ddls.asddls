@EndUserText.label: 'BookSuppl View Entity fro Draft RefScen'
@AccessControl.authorizationCheck: #NOT_REQUIRED

define view entity /DMO/R_BookingSupplement_D
  as select from /dmo/a_bksuppl_d

  association        to parent /DMO/R_Booking_D as _Booking        on $projection.BookingUUID  = _Booking.BookingUUID
  association [1..1] to /DMO/R_Travel_D         as _Travel         on $projection.TravelUUID   = _Travel.TravelUUID

  association [1..1] to /DMO/I_Supplement       as _Product        on $projection.SupplementID = _Product.SupplementID
  association [1..*] to /DMO/I_SupplementText   as _SupplementText on $projection.SupplementID = _SupplementText.SupplementID

{ ///dmo/a_bksuppl_d
  key booksuppl_uuid        as BookSupplUUID,
      root_uuid             as TravelUUID,
      parent_uuid           as BookingUUID,

      booking_supplement_id as BookingSupplementID,
      supplement_id         as SupplementID,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      price                 as BookSupplPrice,
      currency_code         as CurrencyCode,

      //local ETag field --> OData ETag
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,

      //Associations
      _Travel,
      _Booking,

      _Product,
      _SupplementText
}
