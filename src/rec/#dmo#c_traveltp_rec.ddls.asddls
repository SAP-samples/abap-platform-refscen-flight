@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection for Recommendations'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@Consumption.valueHelpDefault.fetchValues: #ON_EXPLICIT_REQUEST
define root view entity /dmo/c_traveltp_rec
  provider contract transactional_query
  as projection on /dmo/r_traveltp_rec

{
  key TravelUuid,
      TravelId,
      Description,
      @Consumption.valueHelpDefinition: [{ entity: { name: '/dmo/c_dest_rec_h',
                                                     element: 'CountryName'
                                                   }
                                        }]
      Destination,
      @Consumption.valueHelpDefinition: [{ entity: { name: '/dmo/c_acc_rec_h',
                                                     element: 'AccName'
                                                   }
                                        }]
      Accommodation,
      @Semantics.user.createdBy: true
      LocalCreatedBy,
      @Semantics.systemDateTime.createdAt: true
      LocalCreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      LastChangedAt
}
