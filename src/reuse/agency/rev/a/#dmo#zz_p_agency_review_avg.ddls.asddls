@EndUserText.label: 'Extension View: Review Average'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity /DMO/ZZ_P_Agency_Review_Avg 
  as select distinct from /DMO/ZZ_I_Agency_Review
{
  key AgencyId,
      cast( avg( all Rating as abap.fltp ) as abap.dec(2,1) ) as AverageRating
}
group by
  AgencyId
