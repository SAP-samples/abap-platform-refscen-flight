"! <strong>Interface for Flight Legacy Coding</strong><br/>
"! Every used structure or table type needed in the API Function Modules
"! will be defined here.
INTERFACE /dmo/if_flight_legacy
  PUBLIC.

***********************
* Version information *
***********************
  CONSTANTS co_version_major TYPE int2 VALUE 3.
  CONSTANTS co_version_minor TYPE int2 VALUE 0.

  " Please do NOT delete old comments
  " Version x.x  Date xx.xx.xxxx  Description ...
  "         0.9       25.07.2018  More or less ready!
  "         0.91      02.08.2018  Derivations and checks for price / currency
  "                               Documentation
  "         0.92      03.08.2018  Commented out locking
  "         0.93      09.08.2018  Data Generator Bug Fix; Description searchable short string
  "         0.94      24.08.2018  Minor corrections
  "         0.95      07.09.2018  Derivation of Total Price, minor corrections
  "         0.96      17.09.2018  Performance DELETE
  "         0.961     18.09.2018  Removed +=
  "         0.962     18.09.2018  Removed ABAPDoc from FuBa, Switched Function Group to Unicode
  "         1.00      27.09.2018  No real change, only release
  "         1.01      28.09.2018  Minor text adjustments
  "         1.02      22.10.2018  Data generator adjustment
  "                               Unit Test moved from function group into separate ABAP class
  "         2.00      05.03.2019  Added ReadOnly and Unmanaged Content
  "                               Added AMDP class with Currency Conversion
  "                               (Re-)Added ABAPDoc to FuBa
  "         3.00      06.08.2019  Cloud Platform 1908
  "                               Added Managed sub package

******************************
* Database table table types *
******************************

  "! Table type of the table /DMO/TRAVEL
  TYPES tt_travel             TYPE SORTED TABLE OF /dmo/travel     WITH UNIQUE KEY travel_id.
  "! Table type of the table /DMO/BOOKING
  TYPES tt_booking            TYPE SORTED TABLE OF /dmo/booking    WITH UNIQUE KEY travel_id  booking_id.
  "! Table type of the table /DMO/BOOK_SUPPL
  TYPES tt_booking_supplement TYPE SORTED TABLE OF /dmo/book_suppl WITH UNIQUE KEY travel_id  booking_id  booking_supplement_id.
  "! Table type of the table /DMO/FLIGHT
  TYPES tt_flight TYPE STANDARD TABLE OF /dmo/flight WITH KEY client carrier_id connection_id flight_date.



******************
* Key structures *
******************

  "! Key structure of Travel
  TYPES BEGIN OF ts_travel_key.
  TYPES travel_id TYPE /dmo/travel_id.
  TYPES END OF ts_travel_key.
  "! Table type that contains only the keys of Travel
  TYPES tt_travel_key TYPE SORTED TABLE OF ts_travel_key WITH UNIQUE KEY travel_id.

  "! Key structure of Booking
  TYPES BEGIN OF ts_booking_key.
  INCLUDE TYPE ts_travel_key.
  TYPES booking_id TYPE /dmo/booking_id.
  TYPES END OF ts_booking_key.
  "! Table type that contains only the keys of Booking
  TYPES tt_booking_key TYPE SORTED TABLE OF ts_booking_key WITH UNIQUE KEY travel_id  booking_id.

  "! Key structure of Booking Supplements
  TYPES BEGIN OF ts_booking_supplement_key.
  INCLUDE TYPE ts_booking_key.
  TYPES booking_supplement_id TYPE /dmo/booking_supplement_id.
  TYPES END OF ts_booking_supplement_key.
  "! Table type that contains only the keys of Booking Supplements
  TYPES tt_booking_supplement_key TYPE SORTED TABLE OF ts_booking_supplement_key WITH UNIQUE KEY travel_id  booking_id  booking_supplement_id.



***********************************************************************************************************************************
* Flag structures for data components                                                                                             *
* IMPORTANT: When you add or remove fields from /DMO/TRAVEL, /DMO/BOOKING, /DMO/BOOK_SUPPL you need to change the following types *
***********************************************************************************************************************************

  TYPES:
    "! <strong>Flag structure for Travel data. </strong><br/>
    "! Each component identifies if the corresponding data has been changed.
    "! Where <em>abap_true</em> represents a change.
    BEGIN OF ts_travel_intx,
      agency_id     TYPE abap_bool,
      customer_id   TYPE abap_bool,
      begin_date    TYPE abap_bool,
      end_date      TYPE abap_bool,
      booking_fee   TYPE abap_bool,
      total_price   TYPE abap_bool,
      currency_code TYPE abap_bool,
      description   TYPE abap_bool,
      status        TYPE abap_bool,
    END OF ts_travel_intx.
  TYPES:
    "! <strong>Flag structure for Booking data. </strong><br/>
    "! Each component identifies if the corresponding data has been changed.
    "! Where <em>abap_true</em> represents a change.
    BEGIN OF ts_booking_intx,
      booking_date  TYPE abap_bool,
      customer_id   TYPE abap_bool,
      carrier_id    TYPE abap_bool,
      connection_id TYPE abap_bool,
      flight_date   TYPE abap_bool,
      flight_price  TYPE abap_bool,
      currency_code TYPE abap_bool,
    END OF ts_booking_intx.
  TYPES:
    "! <strong>Flag structure for Booking Supplement data. </strong><br/>
    "! Each component identifies if the corresponding data has been changed.
    "! Where <em>abap_true</em> represents a change.
    BEGIN OF ts_booking_supplement_intx,
      supplement_id TYPE abap_bool,
      price         TYPE abap_bool,
      currency_code TYPE abap_bool,
    END OF ts_booking_supplement_intx.



**********************************************************************
* Internal
**********************************************************************

  " Internally we use the full X-structures: With complete key and action code
  TYPES BEGIN OF ts_travelx.
  INCLUDE TYPE ts_travel_key.
  TYPES action_code TYPE /dmo/action_code.
  INCLUDE TYPE ts_travel_intx.
  TYPES END OF ts_travelx.
  TYPES: tt_travelx TYPE SORTED TABLE OF ts_travelx WITH UNIQUE KEY travel_id.

  TYPES BEGIN OF ts_bookingx.
  INCLUDE TYPE ts_booking_key.
  TYPES action_code TYPE /dmo/action_code.
  INCLUDE TYPE ts_booking_intx.
  TYPES END OF ts_bookingx.
  TYPES: tt_bookingx TYPE SORTED TABLE OF ts_bookingx WITH UNIQUE KEY travel_id  booking_id.

  TYPES BEGIN OF ts_booking_supplementx.
  INCLUDE TYPE ts_booking_supplement_key.
  TYPES action_code TYPE /dmo/action_code.
  INCLUDE TYPE ts_booking_supplement_intx.
  TYPES END OF ts_booking_supplementx.
  TYPES: tt_booking_supplementx TYPE SORTED TABLE OF ts_booking_supplementx WITH UNIQUE KEY travel_id  booking_id  booking_supplement_id.



*********
* ENUMs *
*********

  TYPES:
    "! Action codes for CUD Operations
    "! <ul>
    "! <li><em>create</em> = create a node</li>
    "! <li><em>update</em> = update a node</li>
    "! <li><em>delete</em> = delete a node</li>
    "! </ul>
    BEGIN OF ENUM action_code_enum STRUCTURE action_code BASE TYPE /dmo/action_code,
      initial VALUE IS INITIAL,
      create  VALUE 'C',
      update  VALUE 'U',
      delete  VALUE 'D',
    END OF ENUM action_code_enum STRUCTURE action_code.

  TYPES:
    "! Travel Stati
    "! <ul>
    "! <li><em>New</em> = New Travel</li>
    "! <li><em>Planned</em> = Planned Travel</li>
    "! <li><em>Booked</em> = Booked Travel</li>
    "! <li><em>Cancelled</em> = Cancelled Travel</li>
    "! </ul>
    BEGIN OF ENUM travel_status_enum STRUCTURE travel_status BASE TYPE /dmo/travel_status,
      initial   VALUE IS INITIAL,
      new       VALUE 'N',
      planned   VALUE 'P',
      booked    VALUE 'B',
      cancelled VALUE 'X',
    END OF ENUM travel_status_enum STRUCTURE travel_status.



************************
* Importing structures *
************************

  "! INcoming structure of the node Travel.  It contains key and data fields.<br/>
  "! The caller of the BAPI like function modules shall not provide the administrative fields.
  TYPES BEGIN OF ts_travel_in.
  INCLUDE TYPE ts_travel_key.
  INCLUDE TYPE /dmo/travel_data.
  TYPES END OF ts_travel_in.

  "! INcoming structure of the node Booking.  It contains the booking key and data fields.<br/>
  "! The BAPI like function modules always refer to a single travel.
  "! Therefore the Travel ID is not required in the subnode tables.
  TYPES BEGIN OF ts_booking_in.
  TYPES booking_id TYPE /dmo/booking_id.
  INCLUDE TYPE /dmo/booking_data.
  TYPES END OF ts_booking_in.
  "! INcoming table type of the node Booking.  It contains the booking key and data fields.
  TYPES tt_booking_in TYPE SORTED TABLE OF ts_booking_in WITH UNIQUE KEY booking_id.

  "! INcoming structure of the node Booking Supplement.  It contains the booking key, booking supplement key and data fields.<br/>
  "! The BAPI like function modules always refer to a single travel.
  "! Therefore the Travel ID is not required in the subnode tables but the booking key is required as it refers to it corresponding super node.
  TYPES BEGIN OF ts_booking_supplement_in.
  TYPES booking_id TYPE /dmo/booking_id.
  TYPES booking_supplement_id TYPE /dmo/booking_supplement_id.
  INCLUDE TYPE /dmo/book_suppl_data.
  TYPES END OF ts_booking_supplement_in.
  "! INcoming table type of the node Booking Supplement.  It contains the booking key, booking supplement key and data fields.
  TYPES tt_booking_supplement_in TYPE SORTED TABLE OF ts_booking_supplement_in WITH UNIQUE KEY booking_id  booking_supplement_id.

  "! INcoming flag structure of the node Travel.  It contains key and the bit flag to the corresponding fields.<br/>
  "! The caller of the BAPI like function modules shall not provide the administrative fields.
  "! Furthermore the action Code is not required for the root (because it is already determined by the function module name).
  TYPES BEGIN OF ts_travel_inx.
  INCLUDE TYPE ts_travel_key.
  INCLUDE TYPE ts_travel_intx.
  TYPES END OF ts_travel_inx.

  "! INcoming flag structure of the node Booking.  It contains key and the bit flag to the corresponding fields.<br/>
  "! The BAPI like function modules always refer to a single travel.
  "! Therefore the Travel ID is not required in the subnode tables.
  TYPES BEGIN OF ts_booking_inx.
  TYPES booking_id TYPE /dmo/booking_id.
  TYPES action_code TYPE /dmo/action_code.
  INCLUDE TYPE ts_booking_intx.
  TYPES END OF ts_booking_inx.
  "! INcoming flag table type of the node Booking.  It contains key and the bit flag to the corresponding fields.
  TYPES tt_booking_inx TYPE SORTED TABLE OF ts_booking_inx WITH UNIQUE KEY booking_id.

  "! INcoming flag structure of the node Booking Supplement.  It contains key and the bit flag to the corresponding fields.<br/>
  "! The BAPI like function modules always refer to a single travel.
  "! Therefore the Travel ID is not required in the subnode tables.
  TYPES BEGIN OF ts_booking_supplement_inx.
  TYPES           booking_id            TYPE /dmo/booking_id.
  TYPES           booking_supplement_id TYPE /dmo/booking_supplement_id.
  TYPES           action_code           TYPE /dmo/action_code.
  INCLUDE TYPE ts_booking_supplement_intx.
  TYPES END OF ts_booking_supplement_inx.
  "! INcoming flag table type of the node Booking Supplement.  It contains key and the bit flag to the corresponding fields.
  TYPES tt_booking_supplement_inx TYPE SORTED TABLE OF ts_booking_supplement_inx WITH UNIQUE KEY booking_id  booking_supplement_id.



*****************
* Message table *
*****************

  "! Table of messages
  TYPES tt_message TYPE STANDARD TABLE OF symsg.

  "! Table of messages like T100. <br/>
  "! We have only error messages.
  "! Currently we do not communicate Warnings or Success Messages.
  "! Internally we use a table of exceptions.
  TYPES tt_if_t100_message TYPE STANDARD TABLE OF REF TO if_t100_message WITH EMPTY KEY.
ENDINTERFACE.
