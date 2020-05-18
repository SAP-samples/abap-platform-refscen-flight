@AbapCatalog.sqlViewName: '/DMO/IBOOKING_M'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED

//@Metadata.ignorePropagatedAnnotations:true

@EndUserText.label: 'Booking view'

define view /DMO/I_Booking_M
  as select from /dmo/booking_m as Booking

  association        to parent /DMO/I_Travel_M as _Travel     on  $projection.travel_id = _Travel.travel_id
  composition [0..*] of /DMO/I_BookSuppl_M     as _BookSupplement

  association [1..1] to /DMO/I_Customer        as _Customer   on  $projection.customer_id = _Customer.CustomerID
  association [1..1] to /DMO/I_Carrier         as _Carrier    on  $projection.carrier_id = _Carrier.AirlineID
  association [1..1] to /DMO/I_Connection      as _Connection on  $projection.carrier_id    = _Connection.AirlineID
                                                              and $projection.connection_id = _Connection.ConnectionID

{
  key travel_id,
  key booking_id,

      booking_date,
      customer_id,
      carrier_id,
      connection_id,
      flight_date,
      @Semantics.amount.currencyCode: 'currency_code'
      flight_price,
      @Semantics.currencyCode: true
      currency_code,
      booking_status,
      
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at,                -- used as etag field

      /* Associations */
      _Travel,
      _BookSupplement,
      _Customer,
      _Carrier,
      _Connection

}
