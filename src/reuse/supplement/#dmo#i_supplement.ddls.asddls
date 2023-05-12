@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'supplement view - cds data model'

@Search.searchable: true

define root view entity /DMO/I_Supplement
  as select from /dmo/supplement as supplement

  composition [0..*] of /DMO/I_SupplementText        as _SupplementText
  association [0..1] to I_Currency                   as _Currency           on $projection.CurrencyCode = _Currency.Currency
  association [0..1] to /DMO/I_SupplementCategory_VH as _SupplementCategory on $projection.SupplementCategory = _SupplementCategory.SupplementCategory

{
      @Search.defaultSearchElement: true
  key supplement.supplement_id         as SupplementID,

      supplement.supplement_category   as SupplementCategory,

      @Search.defaultSearchElement: true
      @Semantics.amount.currencyCode: 'currencycode'
      supplement.price                 as Price,

      supplement.currency_code         as CurrencyCode,

      @Semantics.user.createdBy: true
      supplement.local_created_by      as LocalCreatedBy,
      @Semantics.systemDateTime.createdAt: true
      supplement.local_created_at      as LocalCreatedAt,
      @Semantics.user.lastChangedBy: true
      supplement.local_last_changed_by as LocalLastChangedBy,
      //local etag field --> odata etag
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      supplement.local_last_changed_at as LocalLastChangedAt,

      //total etag field
      @Semantics.systemDateTime.lastChangedAt: true
      supplement.last_changed_at       as LastChangedAt,

      /* associations */
      @Search.defaultSearchElement: true
      _SupplementText,
      _SupplementCategory,
      _Currency
}
