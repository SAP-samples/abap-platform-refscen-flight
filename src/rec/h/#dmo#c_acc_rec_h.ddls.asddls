@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Accommodation View Entity for Recs'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable: true
@Metadata.allowExtensions: true
define view entity /dmo/c_acc_rec_h
  as select from /dmo/acc_rec_h
{
  key acc_id          as AccId,
      @Search.defaultSearchElement: true
      acc_name        as AccName,
      acc_type        as AccType,
      acc_address     as AccAddress,
      acc_postal_code as AccPostalCode,
      acc_city        as AccCity,
      acc_country     as AccCountry,
      acc_phone       as AccPhone,
      acc_email       as AccEmail,
      acc_website     as AccWebsite
}
