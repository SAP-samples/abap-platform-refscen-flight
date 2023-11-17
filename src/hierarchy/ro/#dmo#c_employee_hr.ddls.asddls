@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Hierarchy: Read Only: Employee'

@Metadata.allowExtensions: true
@Search.searchable: true

@OData.hierarchy.recursiveHierarchy:[{ entity.name: '/DMO/I_EmployeeHN_HR' }]

define view entity /DMO/C_Employee_HR
  as select from /DMO/I_Employee_HR

  association of many to one /DMO/C_Employee_HR as _Manager on $projection.Manager = _Manager.Employee
{
  key Employee,

      @Search: {
          defaultSearchElement: true,
          fuzzinessThreshold:  0.87
        }
      FirstName,

      @Search: {
          defaultSearchElement: true,
          fuzzinessThreshold:  0.87
        }
      LastName,

      Salary,
      SalaryCurrency,
      Manager,
      /* Associations */
      _Manager
}
