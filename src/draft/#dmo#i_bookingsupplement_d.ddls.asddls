@EndUserText.label: 'BookSupplement Interface Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity /DMO/I_BookingSupplement_D
  as projection on /DMO/R_BookingSupplement_D
{
  key BookSupplUUID,
      TravelUUID,
      BookingUUID,
      BookingSupplementID,
      SupplementID,
      BookSupplPrice,
      CurrencyCode,
      LocalLastChangedAt,
      /* Associations */
      _Booking : redirected to parent /DMO/I_Booking_D,
      _Product,
      _SupplementText,
      _Travel  : redirected to /DMO/I_Travel_D
}
