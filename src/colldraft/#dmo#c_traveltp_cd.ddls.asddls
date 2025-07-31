@AccessControl.authorizationCheck: #MANDATORY

@EndUserText.label: 'Travel Projection View with CollDraft'

@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true

@ObjectModel.semanticKey: [ 'TravelId' ]

@Search.searchable: true

define root view entity /DMO/C_TravelTP_CD
  provider contract transactional_query
  as projection on /DMO/R_TravelTP_CD

{
  key TravelUuid,

      @Search.defaultSearchElement: true
      TravelId,

      BeginDate,
      EndDate,

      @Semantics.amount.currencyCode: 'CurrencyCode'
      BookingFee,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CurrencyStdVH', element: 'Currency' },
                                            useForValidation: true } ]
      CurrencyCode,

      Description,

      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      LocalLastChangedAt,

      /* Associations */
      _Booking : redirected to composition child /DMO/C_BookingTP_CD
}
