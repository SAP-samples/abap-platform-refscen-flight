@Metadata.allowExtensions: true
@AccessControl.authorizationCheck:#NOT_REQUIRED
@EndUserText.label: 'Projection View Travel Analytical Table'
@OData.applySupportedForAggregation: #FULL

define root view entity /DMO/C_Travel_ANA
  provider contract transactional_query
  as projection on /DMO/I_Travel_ANA
{
  key TravelUuid,
      TravelId,
      AgencyId,
      CustomerId,
      BeginDate,
      EndDate,
      @Aggregation.default: #AVG
      @EndUserText.label: 'Booking Fee (#AVG)'
      @Semantics.amount.currencyCode: 'CurrencyCode'
      BookingFee,
      @Aggregation.default: #MIN
      @EndUserText.label: 'Min Booking Fee (#MIN)'
      @Semantics.amount.currencyCode: 'CurrencyCode'
      MinBookingFee,
      @Aggregation.default: #MAX
      @EndUserText.label: 'Max Booking Fee (#MAX)'
      @Semantics.amount.currencyCode: 'CurrencyCode'
      MaxBookingFee,
      @Aggregation.default: #SUM
      @EndUserText.label: 'Total Price (#SUM)'
      TotalPrice,
      @Aggregation.default: #COUNT_DISTINCT
      @EndUserText.label: 'Different Currencies (#COUNT_DISTINCT)'
      @Aggregation.referenceElement: ['CurrencyCode']
      differentCurrencies,
      @Consumption: {
      valueHelpDefinition: [ {
      entity.element: 'Currency',
      entity.name: 'I_CurrencyStdVH',
      useForValidation: true
      } ]
      }
      CurrencyCode,
      Description,
      OverallStatus
}
