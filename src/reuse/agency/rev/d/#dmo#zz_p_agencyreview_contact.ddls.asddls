@EndUserText.label: 'Agency Extension: Derived Event'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity /DMO/ZZ_P_AgencyReview_Contact
  as select from /DMO/I_Agency
{
  key AgencyID,
      Name,
      PhoneNumber
}
