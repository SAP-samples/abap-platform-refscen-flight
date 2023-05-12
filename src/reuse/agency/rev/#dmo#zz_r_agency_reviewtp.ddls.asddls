@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agency Review'
define view entity /DMO/ZZ_R_Agency_ReviewTP
  as select from /DMO/ZZ_I_Agency_Review
  association to parent /DMO/R_AgencyTP as _Agency on $projection.AgencyId = _Agency.AgencyID
{
  key AgencyId,
  key ReviewId,
      Rating,
      FreeTextComment,
      HelpfulCount,
      HelpfulTotal,
      Reviewer,
      LocalCreatedAt,
      LocalLastChangedAt,
      _Agency
}
