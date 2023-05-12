@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Supplement Text View - CDS Data Model'

@Search.searchable: true

define view entity /DMO/I_SupplementText
  as select from /dmo/suppl_text as SupplementText

  association        to parent /DMO/I_Supplement as _Supplement on $projection.SupplementID = _Supplement.SupplementID
  association [0..1] to I_Language               as _Language   on $projection.LanguageCode = _Language.Language

{
      @ObjectModel.text.element: ['Description']
  key SupplementText.supplement_id as SupplementID,

      @Semantics.language: true
  key SupplementText.language_code as LanguageCode,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Semantics.text: true
      SupplementText.description   as Description,

      //local ETag field --> OData ETag
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      SupplementText.local_last_changed_at as LocalLastChangedAt,

      _Supplement,
      _Language
}
