@EndUserText.label: 'Booking Proj View for Draft RefScen'
@AccessControl.authorizationCheck: #NOT_REQUIRED

@Metadata.allowExtensions: true
@Search.searchable: true
define view entity /DMO/C_Booking_A_D
  as projection on /DMO/R_Booking_D
{
  key BookingUUID,

      TravelUUID,

      @Search.defaultSearchElement: true
      BookingID,

      BookingDate,

      @ObjectModel.text.element: ['CustomerName']
      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Customer_StdVH', element: 'CustomerID' }, useForValidation: true}]
      CustomerID,
      _Customer.LastName as CustomerName,

      @ObjectModel.text.element: ['CarrierName']
      @Consumption.valueHelpDefinition: [ 
          { entity: {name: '/DMO/I_Flight_StdVH', element: 'AirlineID'},
            additionalBinding: [ { localElement: 'FlightDate',   element: 'FlightDate',   usage: #RESULT},
                                 { localElement: 'ConnectionID', element: 'ConnectionID', usage: #RESULT},
                                 { localElement: 'FlightPrice',  element: 'Price',        usage: #RESULT},
                                 { localElement: 'CurrencyCode', element: 'CurrencyCode', usage: #RESULT } ], 
            useForValidation: true }
        ]
      AirlineID,
      _Carrier.Name      as CarrierName,

      @Consumption.valueHelpDefinition: [ 
          { entity: {name: '/DMO/I_Flight_StdVH', element: 'ConnectionID'},
            additionalBinding: [ { localElement: 'FlightDate',   element: 'FlightDate',   usage: #RESULT},
                                 { localElement: 'AirlineID',    element: 'AirlineID',    usage: #FILTER_AND_RESULT},
                                 { localElement: 'FlightPrice',  element: 'Price',        usage: #RESULT},
                                 { localElement: 'CurrencyCode', element: 'CurrencyCode', usage: #RESULT } ], 
            useForValidation: true }
        ]
      ConnectionID,


      @Consumption.valueHelpDefinition: [ 
          { entity: {name: '/DMO/I_Flight_StdVH', element: 'FlightDate'},
            additionalBinding: [ { localElement: 'AirlineID',    element: 'AirlineID',    usage: #FILTER_AND_RESULT},
                                 { localElement: 'ConnectionID', element: 'ConnectionID', usage: #FILTER_AND_RESULT},
                                 { localElement: 'FlightPrice',  element: 'Price',        usage: #RESULT},
                                 { localElement: 'CurrencyCode', element: 'CurrencyCode', usage: #RESULT } ], 
            useForValidation: true }
        ]
      FlightDate,

      @Consumption.valueHelpDefinition: [ 
          { entity: {name: '/DMO/I_Flight_StdVH', element: 'Price'},
            additionalBinding: [ { localElement: 'FlightDate',   element: 'FlightDate',   usage: #FILTER_AND_RESULT},
                                 { localElement: 'AirlineID',    element: 'AirlineID',    usage: #FILTER_AND_RESULT},
                                 { localElement: 'ConnectionID', element: 'ConnectionID', usage: #FILTER_AND_RESULT},
                                 { localElement: 'CurrencyCode', element: 'CurrencyCode', usage: #RESULT } ], 
            useForValidation: true }
        ]
      FlightPrice,

      @Consumption.valueHelpDefinition: [{entity: {name: 'I_CurrencyStdVH', element: 'Currency' }, useForValidation: true }]
      CurrencyCode,

      @ObjectModel.text.element: ['BookingStatusText']
      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Booking_Status_VH', element: 'BookingStatus' }}]
      BookingStatus,
      _BookingStatus._Text.Text as BookingStatusText: localized, 

      LocalLastChangedAt,

      /* Associations */
      _BookingSupplement: redirected to composition child /DMO/C_BookingSupplement_A_D,
      _BookingStatus, 
      _Carrier,
      _Connection,
      _Customer,
      _Travel: redirected to parent /DMO/C_Travel_A_D
}
