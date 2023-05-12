@EndUserText.label: 'Booking Interface Projection View '
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity /DMO/I_Booking_D
  as projection on /DMO/R_Booking_D
{
  key BookingUUID,
      TravelUUID,
      BookingID,
      BookingDate,
      CustomerID,
      AirlineID,
      ConnectionID,
      FlightDate,
      FlightPrice,
      CurrencyCode,
      BookingStatus,
      LocalLastChangedAt,
      /* Associations */
      _BookingStatus,
      _BookingSupplement : redirected to composition child /DMO/I_BookingSupplement_D,
      _Carrier,
      _Connection,
      _Customer,
      _Travel            : redirected to parent /DMO/I_Travel_D
}
