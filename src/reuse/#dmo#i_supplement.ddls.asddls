@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Supplement View - CDS Data Model'

@Search.searchable: true

define view entity /DMO/I_Supplement
  as select from /dmo/supplement as Supplement

  association [0..*] to /DMO/I_SupplementText as _SupplText on $projection.SupplementID = _SupplText.SupplementID
  association [0..1] to I_Currency            as _Currency  on $projection.CurrencyCode = _Currency.Currency

{
      @Search.defaultSearchElement: true
      @ObjectModel.text.association: '_SupplText'
  key Supplement.supplement_id as SupplementID,

      @Search.defaultSearchElement: true
      @Semantics.amount.currencyCode: 'CurrencyCode'
      Supplement.price         as Price,

      Supplement.currency_code as CurrencyCode,

      /* Associations */
      @Search.defaultSearchElement: true
      _SupplText,
      _Currency
}
