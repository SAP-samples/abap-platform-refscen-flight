@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Destination View Entity for Recs'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable: true
define view entity /dmo/c_dest_rec_h
  as select from I_CountryText
{
  key Country,
  key Language,
      @Search.defaultSearchElement: true
      CountryName
}

where
  Language = 'E'
