@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Hierarchy: Draft: Employee Standard VH'

@Search.searchable: true
define view entity /DMO/I_Employee_StdVH_HD
  as select from /DMO/I_Employee_HD
{
  key Agency,
  key Employee,
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.87
      FirstName,
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.87
      LastName,
      Manager,
      _Manager,
      _Agency

}
