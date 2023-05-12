@EndUserText.label: 'Booking Supplement Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED

@Metadata.allowExtensions: true

@Search.searchable: true
define view entity /DMO/C_BookingSupplement_U
  as projection on /DMO/I_BookingSupplement_U

{     ///DMO/I_BookingSupplement_U
      @Search.defaultSearchElement: true
  key TravelID,

      @Search.defaultSearchElement: true
  key BookingID,

  key BookingSupplementID,


      @Consumption.valueHelpDefinition: [ 
          {  entity: {name: '/DMO/I_Supplement_StdVH', element: 'SupplementID' },
             additionalBinding: [ { localElement: 'Price',        element: 'Price',        usage: #RESULT },
                                  { localElement: 'CurrencyCode', element: 'CurrencyCode', usage: #RESULT }], 
             useForValidation: true }
        ]
      @ObjectModel.text.element: ['SupplementText']
      SupplementID,
      _SupplementText.Description as SupplementText : localized,

      Price,

      @Consumption.valueHelpDefinition: [{entity: {name: 'I_CurrencyStdVH', element: 'Currency' }, useForValidation: true }]
      CurrencyCode,

//      LastChangedAt,

      /* Associations */
      ///DMO/I_BookingSupplement_U
      _Booking : redirected to parent /DMO/C_Booking_U,
      _Travel  : redirected to /DMO/C_Travel_U ,         
      _Product,
      _SupplementText

}
