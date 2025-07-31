@AccessControl.authorizationCheck: #MANDATORY

@EndUserText.label: 'Booking Projection View with CollDraft'

@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true

@ObjectModel.semanticKey: [ 'BookingId' ]

@Search.searchable: true

define view entity /DMO/C_BookingTP_CD
  as projection on /DMO/R_BookingTP_CD

{
  key BookingUuid,

      TravelUuid,

      @Search.defaultSearchElement: true
      BookingId,

      BookingDate,
      FlightDate,

      @Semantics.amount.currencyCode: 'CurrencyCode'
      FlightPrice,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CurrencyStdVH', element: 'Currency' },
                                            useForValidation: true } ]
      CurrencyCode,

      /* Associations */
      _Travel : redirected to parent /DMO/C_TravelTP_CD
}
