@Metadata.ignorePropagatedAnnotations: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agency Review'
define view entity /DMO/ZZ_I_Agency_ReviewTP
  as projection on /DMO/ZZ_R_Agency_ReviewTP
{
  key AgencyId,
  key ReviewId,
      Rating,
      FreeTextComment,
      HelpfulCount,
      HelpfulTotal,
      @Semantics.user.createdBy: true
      Reviewer,
      @Semantics.systemDateTime.createdAt: true
      LocalCreatedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      LocalLastChangedAt,
      /* Associations */
      _Agency : redirected to parent /DMO/I_AgencyTP
}
