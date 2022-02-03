"! @testing BDEF:/DMO/R_Booking_D
CLASS ltc_booking DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_booking,
      cds_test_environment TYPE REF TO if_cds_test_environment,
      sql_test_environment TYPE REF TO if_osql_test_environment.

    CONSTANTS:
      travel_uuid1 TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A1',
      travel_uuid2 TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A2',
      uuid1        TYPE sysuuid_x16 VALUE 'BBBBBBBBB8E4645C17002DF03754A1',
      uuid2        TYPE sysuuid_x16 VALUE 'BBBBBBBBB8E4645C17002DF03754A2',
      uuid3        TYPE sysuuid_x16 VALUE 'BBBBBBBBB8E4645C17002DF03754A3',
      uuid4        TYPE sysuuid_x16 VALUE 'BBBBBBBBB8E4645C17002DF03754A4',
      uuid5        TYPE sysuuid_x16 VALUE 'BBBBBBBBB8E4645C17002DF03754A5',
      uuid6        TYPE sysuuid_x16 VALUE 'BBBBBBBBB8E4645C17002DF03754A6',
      c_currency   TYPE /dmo/currency_code VALUE 'EUR'.

    CLASS-METHODS:
      "! Instantiate class under test and set up test double framework
      class_setup,

      "! Destroy test environment and test double
      class_teardown.


    METHODS:
      "! Reset test double
      setup,

      "! Reset transactional buffer
      teardown.

    METHODS:
      "! Checks if { @link ..lhc_booking.METH:setbookingNumber } draws the correct numbers
      "! when applying without an ID.
      setbookingNumber_idempotence   FOR TESTING,

      "! Checks if { @link ..lhc_booking.METH:setbookingNumber } doesn't draw a number
      "! when applying bookings with an ID.
      setbookingnumber_newbookingids  FOR TESTING,

      "! Checks if { @link ..lhc_booking.METH:setbookingNumber } draws the correct numbers
      "! when applying bookings with and without an ID.
      setbookingnumber_mixed         FOR TESTING,

      "! Checks if { @link ..lhc_booking.METH:setBookingDate } draws the current date
      "! and doesn't overwrite existing dates.
      setBookingDate         FOR TESTING,

      "! Calls { @link ..lhc_booking.METH:calculateTotalPrice } and expects
      "! that afterwards the totalprice is adjusted.
      calculateTotalPrice         FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateCustomer }
      "! and checks if an existing customer is set.
      validateCustomer_success      FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateCustomer }
      "! and checks for a message for an initial customer.
      validateCustomer_initial      FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateCustomer }
      "! and checks for a message for a non-existing customer.
      validateCustomer_not_exist    FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateConnection }
      "! and checks if an existing Connection is set.
      validateConnection_success      FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateConnection }
      "! and checks for a message for an initial Connection.
      validateConnection_initial      FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateConnection }
      "! and checks for a message for a non-existing Connection.
      validateConnection_not_exist    FOR TESTING.


ENDCLASS.


CLASS ltc_booking IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds(
                               VALUE #(
                                   ( i_for_entity = '/DMO/R_Travel_D'            )
                                   ( i_for_entity = '/DMO/R_Booking_D'           )
                                   ( i_for_entity = '/DMO/R_BookingSupplement_D' )
                                 )
                             ).
    cds_test_environment->enable_double_redirection(  ).
    sql_test_environment = cl_osql_test_environment=>create(
                               VALUE #(
                                   ( '/DMO/CUSTOMER' )
                                   ( '/DMO/FLIGHT' )
                                 )
                               ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).

    DATA:
      travel_mock_data TYPE STANDARD TABLE OF /dmo/a_travel_d.

    travel_mock_data = VALUE #(
        ( travel_uuid = travel_uuid1  currency_code = c_currency )
        ( travel_uuid = travel_uuid2  currency_code = c_currency )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
    cds_test_environment->destroy( ).
    sql_test_environment->destroy( ).
  ENDMETHOD.


  METHOD setbookingnumber_idempotence.
    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test  TYPE STANDARD TABLE OF /DMO/R_Booking_D WITH KEY bookinguuid,
      exp_bookings      TYPE TABLE FOR READ RESULT /DMO/R_Travel_D\\booking,
      reported          TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    booking_mock_data = VALUE #( ( booking_uuid = uuid1  parent_uuid = travel_uuid1  booking_id = '1' ) ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING TO ENTITY ).
    exp_bookings     = CORRESPONDING #( booking_mock_data MAPPING TO ENTITY ).

    class_under_test->setbookingnumber(
         EXPORTING
           keys     = CORRESPONDING #( bookings_to_test )
         CHANGING
           reported = reported
       ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Booking
        FIELDS ( bookingid TravelUUID ) WITH CORRESPONDING #( bookings_to_test )
        RESULT DATA(read_result).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_bookings
        act = read_result
      ).
  ENDMETHOD.



  METHOD setbookingnumber_newbookingids.
    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test  TYPE STANDARD TABLE OF /DMO/R_Booking_D WITH KEY bookinguuid,
      exp_bookings      TYPE TABLE FOR READ RESULT /DMO/R_Travel_D\\booking,
      reported          TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    booking_mock_data = VALUE #(
         ( booking_uuid = uuid1  parent_uuid = travel_uuid1 )
         ( booking_uuid = uuid2  parent_uuid = travel_uuid1 )
         ( booking_uuid = uuid3  parent_uuid = travel_uuid2 )
         ( booking_uuid = uuid4  parent_uuid = travel_uuid2 )
       ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING TO ENTITY ).

    class_under_test->setbookingnumber(
       EXPORTING
         keys     = CORRESPONDING #( bookings_to_test )
       CHANGING
         reported = reported
     ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Booking
        FIELDS ( bookingid TravelUUID ) WITH CORRESPONDING #( bookings_to_test )
        RESULT DATA(read_result).

    SORT read_result BY TravelUUID ASCENDING  bookingid ASCENDING.

    exp_bookings = VALUE #(
        %is_draft = if_abap_behv=>mk-off
          ( bookinguuid = uuid1  traveluuid = travel_uuid1  bookingid = '1' )
          ( bookinguuid = uuid2  traveluuid = travel_uuid1  bookingid = '2' )
          ( bookinguuid = uuid3  traveluuid = travel_uuid2  bookingid = '1' )
          ( bookinguuid = uuid4  traveluuid = travel_uuid2  bookingid = '2' )
        ).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_bookings
        act = read_result
      ).
  ENDMETHOD.



  METHOD setbookingnumber_mixed.
    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test  TYPE STANDARD TABLE OF /DMO/R_Booking_D WITH KEY bookinguuid,
      exp_bookings      TYPE TABLE FOR READ RESULT /DMO/R_Travel_D\\booking,
      reported          TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    booking_mock_data = VALUE #(
        ( booking_uuid = uuid1  parent_uuid = travel_uuid1 )
        ( booking_uuid = uuid2  parent_uuid = travel_uuid1  booking_id = '1' )
        ( booking_uuid = uuid3  parent_uuid = travel_uuid1 )
        ( booking_uuid = uuid4  parent_uuid = travel_uuid2 )
        ( booking_uuid = uuid5  parent_uuid = travel_uuid2  booking_id = '4' )
        ( booking_uuid = uuid6  parent_uuid = travel_uuid2 )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING TO ENTITY ).

    exp_bookings = VALUE #(
        %is_draft = if_abap_behv=>mk-off
          ( bookinguuid = uuid1  traveluuid = travel_uuid1  bookingid = '2' )
          ( bookinguuid = uuid2  traveluuid = travel_uuid1  bookingid = '1' )
          ( bookinguuid = uuid3  traveluuid = travel_uuid1  bookingid = '3' )
          ( bookinguuid = uuid4  traveluuid = travel_uuid2  bookingid = '5' )
          ( bookinguuid = uuid5  traveluuid = travel_uuid2  bookingid = '4' )
          ( bookinguuid = uuid6  traveluuid = travel_uuid2  bookingid = '6' )
        ).

    class_under_test->setbookingnumber(
       EXPORTING
         keys     = CORRESPONDING #( bookings_to_test )
       CHANGING
         reported = reported
     ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Booking
        FIELDS ( bookingid TravelUUID ) WITH CORRESPONDING #( bookings_to_test )
        RESULT DATA(read_result).

    SORT read_result  BY TravelUUID ASCENDING  bookingid ASCENDING.
    SORT exp_bookings BY TravelUUID ASCENDING  bookingid ASCENDING.

    cl_abap_unit_assert=>assert_equals(
        exp = exp_bookings
        act = read_result
      ).
  ENDMETHOD.


  METHOD setbookingdate.
    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test  TYPE STANDARD TABLE OF /DMO/R_Booking_D WITH KEY bookinguuid,
      exp_bookings      TYPE TABLE FOR READ RESULT /DMO/R_Travel_D\\booking,
      reported          TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D,
      today             TYPE cl_abap_context_info=>ty_system_date,
      a_week_ago        TYPE cl_abap_context_info=>ty_system_date.

    today      = cl_abap_context_info=>get_system_date( ).
    a_week_ago = cl_abap_context_info=>get_system_date( ) - 7.

    booking_mock_data = VALUE #(
        ( booking_uuid = uuid1  parent_uuid = travel_uuid1 )
        ( booking_uuid = uuid2  parent_uuid = travel_uuid1  booking_date = a_week_ago )
        ( booking_uuid = uuid3  parent_uuid = travel_uuid1 )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING TO ENTITY ).

    exp_bookings = VALUE #(
        %is_draft  = if_abap_behv=>mk-off
        TravelUUID = travel_uuid1
          ( bookinguuid = uuid1  bookingdate = today      )
          ( bookinguuid = uuid2  bookingdate = a_week_ago )
          ( bookinguuid = uuid3  bookingdate = today      )
        ).


    class_under_test->setBookingDate(
       EXPORTING
         keys     = CORRESPONDING #( bookings_to_test )
       CHANGING
         reported = reported
     ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Booking
        FIELDS ( BookingDate TravelUUID ) WITH CORRESPONDING #( bookings_to_test )
        RESULT DATA(read_result).

    SORT read_result  BY BookingUUID ASCENDING.

    cl_abap_unit_assert=>assert_equals(
        exp = exp_bookings
        act = read_result
      ).
  ENDMETHOD.

  METHOD calculatetotalprice.
    CONSTANTS:
      c_flight_price TYPE /dmo/flight_price VALUE '10'.

    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test  TYPE STANDARD TABLE OF /DMO/R_Booking_D WITH KEY bookinguuid,
      reported          TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    booking_mock_data = VALUE #(
        flight_price  = c_flight_price
        currency_code = c_currency
        ( booking_uuid = uuid1  parent_uuid = travel_uuid1 )
        ( booking_uuid = uuid2  parent_uuid = travel_uuid1 )
        ( booking_uuid = uuid3  parent_uuid = travel_uuid1 )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING TO ENTITY ).

    class_under_test->calculatetotalprice(
       EXPORTING
         keys     = CORRESPONDING #( bookings_to_test )
       CHANGING
         reported = reported
     ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY Travel
        FIELDS ( TravelUUID TotalPrice ) WITH VALUE #( ( %is_draft = if_abap_behv=>mk-off  TravelUUID = travel_uuid1 ) )
        RESULT DATA(read_result).

    cl_abap_unit_assert=>assert_not_initial( read_result ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/total_price( c_flight_price * lines( booking_mock_data ) )
        act = read_result[ 1 ]-TotalPrice
      ).

  ENDMETHOD.

  METHOD validatecustomer_success.
    CONSTANTS:
      c_customer_id TYPE /dmo/customer_id VALUE '123'.

    DATA:
      customer_mock_data TYPE STANDARD TABLE OF /dmo/customer,
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test   TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\booking~validateCustomer,
      failed             TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported           TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    booking_mock_data = VALUE #(
        ( booking_uuid = uuid1  parent_uuid = travel_uuid1  customer_id = c_customer_id )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookingUUID = booking_uuid ).

    class_under_test->validateCustomer(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     failed   ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( reported-booking )
      ).
  ENDMETHOD.

  METHOD validatecustomer_initial.
    CONSTANTS:
      c_customer_id            TYPE /dmo/customer_id VALUE '123',
      c_customer_id_of_booking TYPE /dmo/customer_id VALUE '111'.

    DATA:
      customer_mock_data TYPE STANDARD TABLE OF /dmo/customer,
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test   TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\Booking~validateCustomer,
      failed             TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported           TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    booking_mock_data = VALUE #(
        ( booking_uuid = uuid1  parent_uuid = travel_uuid1  customer_id = c_customer_id_of_booking )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookingUUID = booking_uuid ).

    class_under_test->validateCustomer(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-booking )
      ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lines( reported-booking )
      ).
  ENDMETHOD.

  METHOD validatecustomer_not_exist.
    CONSTANTS:
      c_customer_id            TYPE /dmo/customer_id VALUE '123',
      c_customer_id_of_booking TYPE /dmo/customer_id VALUE IS INITIAL.

    DATA:
      customer_mock_data TYPE STANDARD TABLE OF /dmo/customer,
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test   TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\Booking~validateCustomer,
      failed             TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported           TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    booking_mock_data = VALUE #(
        ( booking_uuid = uuid1  parent_uuid = travel_uuid1  customer_id = c_customer_id_of_booking )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookingUUID = booking_uuid ).

    class_under_test->validateCustomer(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-booking )
      ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lines( reported-booking )
      ).
  ENDMETHOD.


  METHOD validateConnection_success.
    CONSTANTS:
      c_airline TYPE /dmo/carrier_id VALUE 'TS',
      c_connection TYPE /dmo/connection_id VALUE '123',
      c_flight_date TYPE /dmo/flight_date VALUE '20200202'.

    DATA:
      flight_mock_data TYPE STANDARD TABLE OF /dmo/flight,
      booking_mock_data    TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test     TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\booking~validateConnection,
      failed               TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported             TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    flight_mock_data = VALUE #( ( carrier_id = c_airline  Connection_id = c_connection  flight_date = c_flight_date ) ).
    sql_test_environment->insert_test_data( flight_mock_data ).

    booking_mock_data = VALUE #(
        (
          booking_uuid  = uuid1
          parent_uuid   = travel_uuid1
          carrier_id    = c_airline
          Connection_id = c_connection
          flight_date   = c_flight_date
        )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookingUUID = booking_uuid ).

    class_under_test->validateConnection(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     failed   ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( reported-booking )
      ).
  ENDMETHOD.

  METHOD validateConnection_initial.
    CONSTANTS:
      c_airline TYPE /dmo/carrier_id VALUE 'TS',
      c_connection TYPE /dmo/connection_id VALUE '123',
      c_flight_date TYPE /dmo/flight_date VALUE '20200202'.

    DATA:
      flight_mock_data TYPE STANDARD TABLE OF /dmo/flight,
      booking_mock_data    TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test     TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\Booking~validateConnection,
      failed               TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported             TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    flight_mock_data = VALUE #( ( carrier_id = c_airline  Connection_id = c_connection  flight_date = c_flight_date ) ).
    sql_test_environment->insert_test_data( flight_mock_data ).

    booking_mock_data = VALUE #(
        (
          booking_uuid  = uuid1
          parent_uuid   = travel_uuid1
        )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookingUUID = booking_uuid ).

    class_under_test->validateConnection(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 3 " AirlineID + ConnectionID + FlightDate
        act = lines( failed-booking )
      ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 4 " Initiate + AirlineID + ConnectionID + FlightDate
        act = lines( reported-booking )
      ).
  ENDMETHOD.

  METHOD validateConnection_not_exist.
    CONSTANTS:
      c_airline TYPE /dmo/carrier_id VALUE 'TS',
      c_connection TYPE /dmo/connection_id VALUE '123',
      c_connection_not_exist TYPE /dmo/connection_id VALUE '321',
      c_flight_date TYPE /dmo/flight_date VALUE '20200202'.

    DATA:
      flight_mock_data TYPE STANDARD TABLE OF /dmo/flight,
      booking_mock_data    TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test     TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\Booking~validateConnection,
      failed               TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported             TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    flight_mock_data = VALUE #( ( carrier_id = c_airline  Connection_id = c_connection  flight_date = c_flight_date ) ).
    sql_test_environment->insert_test_data( flight_mock_data ).

    booking_mock_data = VALUE #(
        (
          booking_uuid  = uuid1
          parent_uuid   = travel_uuid1
          carrier_id    = c_airline
          Connection_id = c_connection_not_exist
          flight_date   = c_flight_date
        )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookingUUID = booking_uuid ).

    class_under_test->validateConnection(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-booking )
      ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lines( reported-booking )
      ).
  ENDMETHOD.

ENDCLASS.
