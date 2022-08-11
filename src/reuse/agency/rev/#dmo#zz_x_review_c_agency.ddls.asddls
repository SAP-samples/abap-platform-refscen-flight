extend view entity /DMO/C_AgencyTP with
{
  @UI.facet: [
      {
        id:            'Review',
        purpose:       #STANDARD,
        type:          #LINEITEM_REFERENCE,
        label:         'Review',
        position:      20,
        targetElement: '/DMO/ZZ_ReviewZAG'
      }
    ]
  Agency./DMO/ZZ_ReviewZAG : redirected to composition child /DMO/ZZ_C_Agency_ReviewTP
}
