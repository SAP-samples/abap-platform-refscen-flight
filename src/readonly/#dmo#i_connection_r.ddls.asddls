@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Connection View - CDS Data Model'

@UI.headerInfo: { typeName: 'Connection',
                  typeNamePlural: 'Connections' }

@Search.searchable: true

define view entity /DMO/I_Connection_R
  as select from /dmo/connection as Connection

  association [1..*] to /DMO/I_Flight_R as _Flight  on  $projection.AirlineID    = _Flight.AirlineID
                                                    and $projection.ConnectionID = _Flight.ConnectionID
  association [1]    to /DMO/I_Carrier  as _Airline on  $projection.AirlineID = _Airline.AirlineID

{
        @UI.facet: [
        { id:     'Connection',
        purpose:  #STANDARD,
        type:     #IDENTIFICATION_REFERENCE,
        label:    'Connection',
        position: 10 } ,
        { id:     'Flight',
        purpose:  #STANDARD,
        type:     #LINEITEM_REFERENCE,
        label:    'Flight',
        position: 20,
        targetElement: '_Flight' }
        ]

        @UI.lineItem: [ { position: 10, label: 'Airline'} ]
        @UI: { identification:[ { position: 10, label: 'Airline' } ]}
        @EndUserText.quickInfo: 'Airline that operates the flight.'
        @ObjectModel.text.association: '_Airline'
        @Search.defaultSearchElement: true
        @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Carrier_StdVH', element: 'AirlineID' }, useForValidation: true}]
  key   Connection.carrier_id       as AirlineID,

        @UI.lineItem: [ { position: 20, label:'Connection Number' } ]
        @UI: { identification:[ { position: 20, label: 'Connection Number' } ] }
  key   Connection.connection_id    as ConnectionID,

        @UI: {
        lineItem: [ { position: 30, label: 'Departure Airport Code' } ],
        selectionField: [ { position: 10 }  ],
        identification:[ { position: 30, label: 'Departure Airport Code' } ] }
        @EndUserText.label: 'Departure Airport Code'
        @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Airport_StdVH', element: 'AirportID' }, useForValidation: true }]
        @Search.defaultSearchElement: true
        @Search.fuzzinessThreshold: 0.7
        Connection.airport_from_id  as DepartureAirport,

        @UI: {
        lineItem: [ { position: 40, label: 'Destination Airport Code'} ],
        selectionField: [ { position: 20 }  ],
        identification:[ { position: 40, label: 'Destination Airport Code' } ] }
        @EndUserText.label: 'Destination Airport Code'
        @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Airport_StdVH', element: 'AirportID' }, useForValidation: true }]
        @Search.defaultSearchElement: true
        @Search.fuzzinessThreshold: 0.7
        Connection.airport_to_id    as DestinationAirport,

        @UI.lineItem: [ { position: 50 , label: 'Departure Time'} ]
        @UI: { identification:[ { position: 50, label: 'Departure Time' } ] }
        Connection.departure_time   as DepartureTime,

        @UI.lineItem: [ { position: 60 ,  label: 'Arrival Time' } ]
        @UI: { identification:[ { position: 60, label: 'Arrival Time'  } ] }
        Connection.arrival_time     as ArrivalTime,

//        @Semantics.quantity.unitOfMeasure: 'DistanceUnit'
        @UI: { identification:[ { position: 70, label: 'Distance' } ] }
        Connection.distance         as Distance,

        Connection.distance_unit    as DistanceUnit,

        /* Associations */
        @Search.defaultSearchElement: true
        _Flight,
        _Airline

}
