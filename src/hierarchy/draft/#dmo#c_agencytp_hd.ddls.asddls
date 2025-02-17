@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Hierarchy: Draft: Agency'

@Metadata.allowExtensions: true

define root view entity /DMO/C_AgencyTP_HD
  provider contract transactional_query
  as projection on /DMO/R_AgencyTP_HD

{

  key Agency,
      Name,
      Street,
      PostalCode,
      City,
      CountryCode,
      PhoneNumber,
      EMailAddress,
      WebAddress,
      LocalLastChangedAt,
      /* Associations */
      _Country,
      _Employee : redirected to composition child /DMO/C_EmployeeTP_HD
}
