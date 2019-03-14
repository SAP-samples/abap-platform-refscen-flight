@AbapCatalog.sqlViewName: '/DMO/IBOOKSUPP_U'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED


@UI: { headerInfo: { typeName: 'Booking Supplement',
                     typeNamePlural: 'Booking Supplements',
                     title: { type: #STANDARD, label: 'Booking Supplement', value: 'BookingSupplementID' } } }
@EndUserText.label: 'Booking Supplement view - CDS data model'

@Search.searchable: true

define view /DMO/I_BookingSupplement_U
  as select from /dmo/book_suppl as BookingSupplement

  association        to parent /DMO/I_Booking_U  as _Booking         on  $projection.TravelID  = _Booking.TravelID
                                                                     and $projection.BookingID = _Booking.BookingID

  association [1..1] to /DMO/I_Supplement     as _Product  on $projection.SupplementID = _Product.SupplementID
  association [1..*] to /DMO/I_SupplementText as _SupplementText on  $projection.SupplementID = _SupplementText.SupplementID
{
      @UI.facet: [ { id:            'BookingSupplement',
                     purpose:       #STANDARD,
                     type:          #IDENTIFICATION_REFERENCE,
                     label:         'Booking Supplement',
                     position:      10 } ,
                    { id:              'PriceHeader',
                      type:            #DATAPOINT_REFERENCE,
                      purpose:         #HEADER,
                      targetQualifier: 'Price',
                      label:           'Price',
                      position:        20 } ]

  @Search.defaultSearchElement: true
  key BookingSupplement.travel_id              as TravelID,
  
  @Search.defaultSearchElement: true
  key BookingSupplement.booking_id             as BookingID,

      @UI: { lineItem:       [ { position: 10, importance: #HIGH } ],
             identification: [ { position: 10 } ] }
  key BookingSupplement.booking_supplement_id  as BookingSupplementID,

      @UI: { lineItem:       [ { position: 20, importance: #HIGH } ],
             identification: [ { position: 20 } ] }
      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_SUPPLEMENT', element: 'SupplementID' },
                                          additionalBinding: [{ localElement: 'Price',        element: 'Price'},
                                                              { localElement: 'CurrencyCode', element: 'CurrencyCode'}]}]
      @ObjectModel.text.association: '_SupplementText'
      BookingSupplement.supplement_id          as SupplementID,

      @UI: { lineItem:       [ { position: 30, importance: #HIGH } ],
             identification: [ { position: 30 } ],
             dataPoint:      { title: 'Price' }
             }
      @Semantics.amount.currencyCode: 'CurrencyCode'
      BookingSupplement.price                  as Price,

      @Semantics.currencyCode: true
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_Currency', element: 'Currency' }}]
      BookingSupplement.currency_code          as CurrencyCode,

      @UI.hidden
      _Booking.LastChangedAt as LastChangedAt,

      /* Associations */
      _Booking,
      _Product,
      _SupplementText
}
