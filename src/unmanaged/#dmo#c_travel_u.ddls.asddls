@EndUserText.label: 'Travel Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED

@Metadata.allowExtensions: true

@Search.searchable: true
define root view entity /DMO/C_Travel_U
  provider contract transactional_query
  as projection on /DMO/I_Travel_U

{     ///DMO/I_Travel_U

  key TravelID,

      @Consumption.valueHelpDefinition: [{ entity : {name: '/DMO/I_Agency_StdVH', element: 'AgencyID'  }, useForValidation: true }]
      @ObjectModel.text.element: ['AgencyName']
      @Search.defaultSearchElement: true
      AgencyID,
      _Agency.Name       as AgencyName,

      @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Customer_StdVH', element: 'CustomerID' }, useForValidation: true}]
      @ObjectModel.text.element: ['CustomerName']
      @Search.defaultSearchElement: true
      CustomerID,
      _Customer.LastName as CustomerName,

      BeginDate,

      EndDate,

      BookingFee,

      TotalPrice,

      @Consumption.valueHelpDefinition: [{entity: {name: 'I_CurrencyStdVH', element: 'Currency' }, useForValidation: true }]
      CurrencyCode,

      Memo,

      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Travel_Status_VH', element: 'TravelStatus' }}]
      @ObjectModel.text.element: ['StatusText']  
      Status,
      
      _TravelStatus._Text.Text as StatusText : localized,

      LastChangedAt,
      /* Associations */
      ///DMO/I_Travel_U
      _Booking : redirected to composition child /DMO/C_Booking_U,
      _Agency,
      _Currency,
      _Customer,
      _TravelStatus
      
     
}

