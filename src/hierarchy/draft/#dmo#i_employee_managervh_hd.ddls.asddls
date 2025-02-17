@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Hierarchy: Draft: Employee: Manager VH'

@Search.searchable: true

define view entity /DMO/I_Employee_ManagerVH_HD
  as select from /DMO/I_Employee_StdVH_HD
{
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.87
      @ObjectModel.text.element: ['AgencyName']
      @EndUserText.label: 'Agency'
      @Consumption.valueHelpDefinition: [ { entity: { name: '/DMO/I_AGENCY_STDVH_HD', element: 'Agency' } } ]
  key Agency,

  key Employee,


      FirstName,
      LastName,

      @UI.hidden: true
      _Agency.Name       as AgencyName,

      Manager,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.87
      @EndUserText.label: 'Managers First Name'
      _Manager.FirstName as ManagerFirstName,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.87
      @EndUserText.label: 'Managers Last Name'
      _Manager.LastName  as ManagerLastName
}
