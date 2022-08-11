@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agency Review'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity /DMO/ZZ_I_Agency_Review
  as select from /dmo/zz_agn_reva as Review
{
  key Review.agency_id             as AgencyId,
  key Review.review_id             as ReviewId,
      Review.rating                as Rating,
      Review.free_text_comment     as FreeTextComment,
      Review.helpful_count         as HelpfulCount,
      Review.helpful_total         as HelpfulTotal,
      @Semantics.user.createdBy: true
      Review.reviewer              as Reviewer,
      @Semantics.systemDateTime.createdAt: true
      Review.local_created_at      as LocalCreatedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      Review.local_last_changed_at as LocalLastChangedAt
}
