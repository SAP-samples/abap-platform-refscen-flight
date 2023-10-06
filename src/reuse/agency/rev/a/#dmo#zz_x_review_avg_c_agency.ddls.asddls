extend view entity /DMO/C_AgencyTP with
{
  @UI.facet: [
      {
        id:              'RatingID',
        purpose:         #HEADER,
        type:            #DATAPOINT_REFERENCE,
        label:           'Rating',
        position:        10,
        targetQualifier: 'xDMOxZZAverageRatingZAG'
      }
    ]
  @UI:{
        dataPoint: {
          title: 'Rating',
          visualization: #RATING,
          targetValue: 5
        }
        ,lineItem: [
            {
              position: 25,
              label: 'Rating',
              importance: #HIGH,
              type: #AS_DATAPOINT,
              valueQualifier: 'AVGRating'
            }
          ]
    }
  Agency./DMO/ZZ_ReviewAvgZAG.AverageRating as /DMO/ZZAverageRatingZAG
}
