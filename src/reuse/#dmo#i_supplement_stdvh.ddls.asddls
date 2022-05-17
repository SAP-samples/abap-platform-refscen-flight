@EndUserText.label: 'Supplement ValueHelp'
@AccessControl.authorizationCheck: #NOT_REQUIRED

@Metadata.ignorePropagatedAnnotations: true
@Search.searchable: true
@ObjectModel.semanticKey: ['SupplementID']
@ObjectModel.dataCategory:#VALUE_HELP

define view entity /DMO/I_Supplement_StdVH
  as select from /DMO/I_Supplement as Supplement

{
      @ObjectModel.text.element: ['SupplementText']
      @Search.defaultSearchElement: true
      @UI.lineItem: [{ position: 10 }]
      @UI.textArrangement: #TEXT_SEPARATE
  key Supplement.SupplementID,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.87
      @UI.lineItem: [{ position: 20 }]
      _SupplementText[ 1: LanguageCode = $session.system_language ].Description         as SupplementText,

      @Consumption.valueHelpDefinition: [{entity: {
                                            name: '/DMO/I_SupplementCategory_VH',
                                            element: 'SupplementCategory'
                                          }}]
      @ObjectModel.text.element: ['SupplementCategoryText']
      @UI.lineItem: [{ position: 30 }]
      @UI.textArrangement: #TEXT_FIRST
      Supplement.SupplementCategory,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.87
      @UI.hidden: true
      _SupplementCategory._Text[ 1: LanguageCode = $session.system_language ].Description as SupplementCategoryText,

      @Semantics.amount.currencyCode: 'CurrencyCode'
      @UI.lineItem: [{ position: 40 }]
      Supplement.Price,

      @UI.hidden: true
      Supplement.CurrencyCode
}
