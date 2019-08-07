@AbapCatalog.sqlViewName: '/DMO/IBOOKING_U'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Booking view'

@UI: {
  headerInfo: { typeName: 'Booking',
                typeNamePlural: 'Bookings',
                title: { type: #STANDARD, value: 'BookingID' }
  }
}

@Search.searchable: true

define view /DMO/I_Booking_U
  as select from /dmo/booking as Booking

  association        to parent /DMO/I_Travel_U     as _Travel     on  $projection.TravelID = _Travel.TravelID
  composition [0..*] of /DMO/I_BookingSupplement_U as _BookSupplement

  association [1..1] to /DMO/I_Customer            as _Customer   on  $projection.CustomerID = _Customer.CustomerID
  association [1..1] to /DMO/I_Carrier             as _Carrier    on  $projection.AirlineID = _Carrier.AirlineID
  association [1..1] to /DMO/I_Connection          as _Connection on  $projection.AirlineID    = _Connection.AirlineID
                                                                  and $projection.ConnectionID = _Connection.ConnectionID
{


      @UI.facet: [ { id:            'Booking',
                     purpose:       #STANDARD,
                     type:          #IDENTIFICATION_REFERENCE,
                     label:         'Booking',
                     position:      10 },
                   { id:            'BookingSupplement',
                     purpose:       #STANDARD,
                     type:          #LINEITEM_REFERENCE,
                     label:         'Booking Supplement',
                     position:      20,
                     targetElement: '_BookSupplement'} ]

      @Search.defaultSearchElement: true
  key Booking.travel_id             as TravelID,

      @UI: { lineItem:  [ { position: 20, importance: #HIGH } ], identification: [{position: 20 }] }
      @Search.defaultSearchElement: true
  key Booking.booking_id            as BookingID,

      @UI: { lineItem:  [ { position: 30, importance: #HIGH } ],
             identification: [ { position: 30 } ] }
      Booking.booking_date          as BookingDate,

      @UI: { lineItem:  [ { position: 40, importance: #HIGH } ],
             identification: [ { position: 40 } ] }
      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Customer', element: 'CustomerID' }}]
      @Search.defaultSearchElement: true
      Booking.customer_id           as CustomerID,

      @UI: { lineItem:       [ { position: 50, importance: #HIGH } ],
             identification: [ { position: 50 } ] }
      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Carrier', element: 'AirlineID' }}]
      @ObjectModel.text.association: '_Carrier'
      Booking.carrier_id            as AirlineID,

      @UI: { lineItem:       [ { position: 60, importance: #HIGH } ],
             identification: [ { position: 60 } ] }
      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Flight', element: 'ConnectionID' },
                                          additionalBinding: [{ localElement: 'FlightDate',   element: 'FlightDate'},
                                                              { localElement: 'AirlineID',    element: 'AirlineID'},
                                                              { localElement: 'FlightPrice',  element: 'Price' },
                                                              { localElement: 'CurrencyCode', element: 'CurrencyCode' }]}]
      @ObjectModel.text.association: '_Connection'
      Booking.connection_id         as ConnectionID,


      @UI: { lineItem:       [ { position: 70, importance: #HIGH } ],
             identification: [ { position: 70 } ] }
      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Flight', element: 'FlightDate' },
                                          additionalBinding: [{ localElement: 'ConnectionID', element: 'ConnectionID'},
                                                              { localElement: 'AirlineID',    element: 'AirlineID'},
                                                              { localElement: 'FlightPrice',  element: 'Price' },
                                                              { localElement: 'CurrencyCode', element: 'CurrencyCode' }]}]
      Booking.flight_date           as FlightDate,

      @UI: { lineItem:       [ { position: 80, importance: #HIGH } ], identification: [ { position: 80 } ] }
      @Semantics.amount.currencyCode: 'CurrencyCode'
      Booking.flight_price          as FlightPrice,


      @Semantics.currencyCode: true
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_Currency', element: 'Currency' }}]
      Booking.currency_code         as CurrencyCode,

      @UI.hidden: true
      _Travel.LastChangedAt         as LastChangedAt, -- Take over ETag from parent

      /* Associations */
      _Travel,
      _BookSupplement,
      _Customer,
      _Carrier,
      _Connection

}
