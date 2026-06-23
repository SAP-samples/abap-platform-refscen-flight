extend view entity /DMO/C_AgencyTP with
{
  @UI: {
    lineItem:   [ { position: 25, importance: #HIGH } ],
    fieldGroup: [ { position: 25, qualifier: 'General_FG' } ],
    identification: [ { position: 20, type: #FOR_ACTION, dataAction: '/DMO/zzChangeAddress', label: 'Change Address' } ] 
  }
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.8
  Agency./DMO/ZZSloganZAG
}
