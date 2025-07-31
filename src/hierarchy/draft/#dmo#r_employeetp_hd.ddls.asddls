@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Hierarchy: Draft: Employee'

define view entity /DMO/R_EmployeeTP_HD
  as select from /DMO/I_Employee_HD

  association         to parent /DMO/R_AgencyTP_HD as _Agency   on  $projection.Agency = _Agency.Agency
  association of many to one /DMO/R_EmployeeTP_HD  as _Manager  on  $projection.Agency  = _Manager.Agency
                                                                and $projection.Manager = _Manager.Employee
  association of one  to many /DMO/R_EmployeeTP_HD as _Employee on  $projection.Agency   = _Employee.Agency
                                                                and $projection.Employee = _Employee.Manager
  association of one  to one I_Currency            as _Currency on  $projection.SalaryCurrency = _Currency.Currency

{

  key Agency,
  key Employee,
      FirstName,
      LastName,
      @Semantics.amount.currencyCode: 'SalaryCurrency'
      Salary, 
      SalaryCurrency,
      Manager,
      SiblingOrderNumber,
      LocalCreatedBy,
      LocalCreatedAt,
      LocalLastChangedBy,
      LocalLastChangedAt,
      /* Associations */
      _Agency,
      _Manager,
      _Employee,
      _Currency

}
