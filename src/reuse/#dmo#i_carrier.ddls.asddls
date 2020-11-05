@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Carrier View - CDS Data Model'

@Search.searchable: true

define view entity /DMO/I_Carrier
  as select from /dmo/carrier as Airline

  association [0..1] to I_Currency as _Currency on $projection.CurrencyCode = _Currency.Currency

{
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: ['Name']
  key Airline.carrier_id    as AirlineID,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.7
      @Semantics.text: true
      Airline.name          as Name,

      Airline.currency_code as CurrencyCode,

      /* Associations */
      _Currency
}
