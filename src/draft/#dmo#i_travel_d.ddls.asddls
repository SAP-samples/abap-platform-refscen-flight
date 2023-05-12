@EndUserText.label: 'Travel Interface Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity /DMO/I_Travel_D
  provider contract transactional_interface
  as projection on /DMO/R_Travel_D
{
  key TravelUUID,
      TravelID,
      AgencyID,
      CustomerID,
      BeginDate,
      EndDate,
      BookingFee,
      TotalPrice,
      CurrencyCode,
      Description,
      OverallStatus,
      LocalLastChangedAt,
      /* Associations */
      _Agency,
      _Booking : redirected to composition child /DMO/I_Booking_D,
      _Currency,
      _Customer,
      _OverallStatus
}
