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
      setbookingnumber_idempotence   FOR TESTING,

      "! Checks if { @link ..lhc_booking.METH:setbookingNumber } doesn't draw a number
      "! when applying bookings with an ID.
      setbookingnumber_newbookingids  FOR TESTING,

      "! Checks if { @link ..lhc_booking.METH:setbookingNumber } draws the correct numbers
      "! when applying bookings with and without an ID.
      setbookingnumber_mixed         FOR TESTING,

      "! Checks if { @link ..lhc_booking.METH:setBookingDate } draws the current date
      "! and doesn't overwrite existing dates.
      setbookingdate         FOR TESTING,

      "! Calls { @link ..lhc_booking.METH:calculateTotalPrice } and expects
      "! that afterwards the totalprice is adjusted.
      calculatetotalprice         FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateCustomer }
      "! and checks if an existing customer is set.
      validatecustomer_success      FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateCustomer }
      "! and checks for a message for an initial customer.
      validatecustomer_initial      FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateCustomer }
      "! and checks for a message for a non-existing customer.
      validatecustomer_not_exist    FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateConnection }
      "! and checks if an existing Connection is set.
      validateconnection_success      FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateConnection }
      "! and checks for a message for an initial Connection.
      validateconnection_initial      FOR TESTING,

      "! Calls { @link ..lhc_Booking.METH:validateConnection }
      "! and checks for a message for a non-existing Connection.
      validateconnection_not_exist    FOR TESTING,

      "! Calls { @link ..lhc_booking.METH:validatecurrencycode }
      "! and checks if a pair of status is valid.
      validate_currency_success        FOR TESTING,

      "! Calls { @link ..lhc_booking.METH:validatecurrencycode }
      "! and checks if invalid permutations of sets of status
      "! returns messages.
      validate_currency_not_valid      FOR TESTING.


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
                                   ( 'I_CURRENCY'    )
                                   ( '/DMO/CUSTOMER' )
                                   ( '/DMO/FLIGHT'   )
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

    DATA: currencies TYPE STANDARD TABLE OF i_currency.
    currencies = VALUE #(
        ( currency = 'EUR' )
        ( currency = 'USD' )
      ).
    sql_test_environment->insert_test_data( currencies ).
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
      bookings_to_test  TYPE STANDARD TABLE OF /dmo/r_booking_d WITH KEY bookinguuid,
      exp_bookings      TYPE TABLE FOR READ RESULT /dmo/r_travel_d\\booking,
      reported          TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

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


    READ ENTITIES OF /dmo/r_travel_d
      ENTITY booking
        FIELDS ( bookingid traveluuid ) WITH CORRESPONDING #( bookings_to_test )
        RESULT DATA(read_result).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_bookings
        act = read_result
      ).
  ENDMETHOD.



  METHOD setbookingnumber_newbookingids.
    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test  TYPE STANDARD TABLE OF /dmo/r_booking_d WITH KEY bookinguuid,
      exp_bookings      TYPE TABLE FOR READ RESULT /dmo/r_travel_d\\booking,
      reported          TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

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


    READ ENTITIES OF /dmo/r_travel_d
      ENTITY booking
        FIELDS ( bookingid traveluuid ) WITH CORRESPONDING #( bookings_to_test )
        RESULT DATA(read_results).

    SORT read_results BY traveluuid ASCENDING  bookingid ASCENDING.

    exp_bookings = VALUE #(
        %is_draft = if_abap_behv=>mk-off
          ( bookinguuid = uuid1  traveluuid = travel_uuid1  bookingid = '1' )
          ( bookinguuid = uuid2  traveluuid = travel_uuid1  bookingid = '2' )
          ( bookinguuid = uuid3  traveluuid = travel_uuid2  bookingid = '1' )
          ( bookinguuid = uuid4  traveluuid = travel_uuid2  bookingid = '2' )
        ).

    " Delete BookingUUID as the order of it should not relate to given (new) BookingIDs.
    DATA: empty_booking LIKE LINE OF read_results.
    MODIFY exp_bookings FROM empty_booking TRANSPORTING bookinguuid WHERE bookingid <> empty_booking-bookingid.
    MODIFY read_results FROM empty_booking TRANSPORTING bookinguuid WHERE bookingid <> empty_booking-bookingid.

    SORT read_results BY traveluuid ASCENDING  bookingid ASCENDING.
    SORT exp_bookings BY traveluuid ASCENDING  bookingid ASCENDING.

    cl_abap_unit_assert=>assert_equals(
        exp = exp_bookings
        act = read_results
      ).
  ENDMETHOD.



  METHOD setbookingnumber_mixed.
    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test  TYPE STANDARD TABLE OF /dmo/r_booking_d WITH KEY bookinguuid,
      exp_bookings      TYPE TABLE FOR READ RESULT /dmo/r_travel_d\\booking,
      reported          TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

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


    READ ENTITIES OF /dmo/r_travel_d
      ENTITY booking
        FIELDS ( bookingid traveluuid ) WITH CORRESPONDING #( bookings_to_test )
        RESULT DATA(read_results).

    " Ensure the given (existing) BookingIDs are still in place for the related BookingUUID before we delete the BookingUUID.
    LOOP AT booking_mock_data INTO DATA(booking_mock) WHERE booking_id IS NOT INITIAL.
      DATA(read_result) = VALUE #( read_results[ KEY id  %is_draft = if_abap_behv=>mk-off  bookinguuid = booking_mock-booking_uuid ] OPTIONAL ).
      cl_abap_unit_assert=>assert_not_initial( read_result ).
      cl_abap_unit_assert=>assert_equals(
          exp = booking_mock-booking_id
          act = read_result-bookingid
        ).
    ENDLOOP.

    " Delete BookingUUID as the order of it should not relate to given (new) BookingIDs.
    DATA: empty_booking LIKE LINE OF read_results.
    MODIFY exp_bookings FROM empty_booking TRANSPORTING bookinguuid WHERE bookingid <> empty_booking-bookingid.
    MODIFY read_results FROM empty_booking TRANSPORTING bookinguuid WHERE bookingid <> empty_booking-bookingid.

    SORT read_results BY traveluuid ASCENDING  bookingid ASCENDING.
    SORT exp_bookings BY traveluuid ASCENDING  bookingid ASCENDING.

    cl_abap_unit_assert=>assert_equals(
        exp = exp_bookings
        act = read_results
      ).
  ENDMETHOD.


  METHOD setbookingdate.
    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test  TYPE STANDARD TABLE OF /dmo/r_booking_d WITH KEY bookinguuid,
      exp_bookings      TYPE TABLE FOR READ RESULT /dmo/r_travel_d\\booking,
      reported          TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d,
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
        traveluuid = travel_uuid1
          ( bookinguuid = uuid1  bookingdate = today      )
          ( bookinguuid = uuid2  bookingdate = a_week_ago )
          ( bookinguuid = uuid3  bookingdate = today      )
        ).


    class_under_test->setbookingdate(
       EXPORTING
         keys     = CORRESPONDING #( bookings_to_test )
       CHANGING
         reported = reported
     ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /dmo/r_travel_d
      ENTITY booking
        FIELDS ( bookingdate traveluuid ) WITH CORRESPONDING #( bookings_to_test )
        RESULT DATA(read_result).

    SORT read_result  BY bookinguuid ASCENDING.

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
      bookings_to_test  TYPE STANDARD TABLE OF /dmo/r_booking_d WITH KEY bookinguuid,
      reported          TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

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


    READ ENTITIES OF /dmo/r_travel_d
      ENTITY travel
        FIELDS ( traveluuid totalprice ) WITH VALUE #( ( %is_draft = if_abap_behv=>mk-off  traveluuid = travel_uuid1 ) )
        RESULT DATA(read_result).

    cl_abap_unit_assert=>assert_not_initial( read_result ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/total_price( c_flight_price * lines( booking_mock_data ) )
        act = read_result[ 1 ]-totalprice
      ).

  ENDMETHOD.

  METHOD validatecustomer_success.
    CONSTANTS:
      c_customer_id TYPE /dmo/customer_id VALUE '123'.

    DATA:
      customer_mock_data TYPE STANDARD TABLE OF /dmo/customer,
      booking_mock_data  TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test   TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\booking~validatecustomer,
      failed             TYPE RESPONSE FOR FAILED LATE  /dmo/r_travel_d,
      reported           TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    booking_mock_data = VALUE #(
        ( booking_uuid = uuid1  parent_uuid = travel_uuid1  customer_id = c_customer_id )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookinguuid = booking_uuid ).

    class_under_test->validatecustomer(
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
      bookings_to_test   TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\booking~validatecustomer,
      failed             TYPE RESPONSE FOR FAILED LATE  /dmo/r_travel_d,
      reported           TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    booking_mock_data = VALUE #(
        ( booking_uuid = uuid1  parent_uuid = travel_uuid1  customer_id = c_customer_id_of_booking )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookinguuid = booking_uuid ).

    class_under_test->validatecustomer(
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
      bookings_to_test   TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\booking~validatecustomer,
      failed             TYPE RESPONSE FOR FAILED LATE  /dmo/r_travel_d,
      reported           TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

    customer_mock_data = VALUE #( ( customer_id = c_customer_id ) ).
    sql_test_environment->insert_test_data( customer_mock_data ).

    booking_mock_data = VALUE #(
        ( booking_uuid = uuid1  parent_uuid = travel_uuid1  customer_id = c_customer_id_of_booking )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookinguuid = booking_uuid ).

    class_under_test->validatecustomer(
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


  METHOD validateconnection_success.
    CONSTANTS:
      c_airline     TYPE /dmo/carrier_id VALUE 'TS',
      c_connection  TYPE /dmo/connection_id VALUE '123',
      c_flight_date TYPE /dmo/flight_date VALUE '20200202'.

    DATA:
      flight_mock_data  TYPE STANDARD TABLE OF /dmo/flight,
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test  TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\booking~validateconnection,
      failed            TYPE RESPONSE FOR FAILED LATE  /dmo/r_travel_d,
      reported          TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

    flight_mock_data = VALUE #( ( carrier_id = c_airline  connection_id = c_connection  flight_date = c_flight_date ) ).
    sql_test_environment->insert_test_data( flight_mock_data ).

    booking_mock_data = VALUE #(
        (
          booking_uuid  = uuid1
          parent_uuid   = travel_uuid1
          carrier_id    = c_airline
          connection_id = c_connection
          flight_date   = c_flight_date
        )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookinguuid = booking_uuid ).

    class_under_test->validateconnection(
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

  METHOD validateconnection_initial.
    CONSTANTS:
      c_airline     TYPE /dmo/carrier_id VALUE 'TS',
      c_connection  TYPE /dmo/connection_id VALUE '123',
      c_flight_date TYPE /dmo/flight_date VALUE '20200202'.

    DATA:
      flight_mock_data  TYPE STANDARD TABLE OF /dmo/flight,
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test  TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\booking~validateconnection,
      failed            TYPE RESPONSE FOR FAILED LATE  /dmo/r_travel_d,
      reported          TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

    flight_mock_data = VALUE #( ( carrier_id = c_airline  connection_id = c_connection  flight_date = c_flight_date ) ).
    sql_test_environment->insert_test_data( flight_mock_data ).

    booking_mock_data = VALUE #(
        (
          booking_uuid  = uuid1
          parent_uuid   = travel_uuid1
        )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookinguuid = booking_uuid ).

    class_under_test->validateconnection(
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

  METHOD validateconnection_not_exist.
    CONSTANTS:
      c_airline              TYPE /dmo/carrier_id VALUE 'TS',
      c_connection           TYPE /dmo/connection_id VALUE '123',
      c_connection_not_exist TYPE /dmo/connection_id VALUE '321',
      c_flight_date          TYPE /dmo/flight_date VALUE '20200202'.

    DATA:
      flight_mock_data  TYPE STANDARD TABLE OF /dmo/flight,
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test  TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\booking~validateconnection,
      failed            TYPE RESPONSE FOR FAILED LATE  /dmo/r_travel_d,
      reported          TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

    flight_mock_data = VALUE #( ( carrier_id = c_airline  connection_id = c_connection  flight_date = c_flight_date ) ).
    sql_test_environment->insert_test_data( flight_mock_data ).

    booking_mock_data = VALUE #(
        (
          booking_uuid  = uuid1
          parent_uuid   = travel_uuid1
          carrier_id    = c_airline
          connection_id = c_connection_not_exist
          flight_date   = c_flight_date
        )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookinguuid = booking_uuid ).

    class_under_test->validateconnection(
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

  METHOD validate_currency_success.
    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_test  TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\booking~validatecurrencycode,
      failed            TYPE RESPONSE FOR FAILED   LATE /dmo/r_travel_d,
      reported          TYPE RESPONSE FOR REPORTED LATE /dmo/r_travel_d.

    booking_mock_data = VALUE #(
        ( booking_uuid = uuid1  currency_code = 'EUR' )
        ( booking_uuid = uuid2  currency_code = 'USD' )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookinguuid = booking_uuid ).

    class_under_test->validatecurrencycode(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = lines( bookings_to_test )
        act = lines( reported-booking )
      ).
  ENDMETHOD.

  METHOD validate_currency_not_valid.
    TYPES: BEGIN OF t_check.
             INCLUDE TYPE /dmo/a_booking_d.
    TYPES:   exp_amount_reported_entries TYPE i,
             exp_amount_failed_entries   TYPE i,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY booking_uuid.

    DATA:
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d,
      bookings_to_check TYPE t_check_table,
      booking_to_check  TYPE t_check,
      bookings_to_test  TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\booking~validatecurrencycode,
      failed            TYPE RESPONSE FOR FAILED   LATE /dmo/r_travel_d,
      reported          TYPE RESPONSE FOR REPORTED LATE /dmo/r_travel_d.

    bookings_to_check = VALUE #(
        exp_amount_reported_entries = '2'
        exp_amount_failed_entries   = '1'
        ( booking_uuid = uuid1  currency_code = ''    )
        ( booking_uuid = uuid2  currency_code = 'XXX' )
      ).
    booking_mock_data = CORRESPONDING #( bookings_to_check ).
    cds_test_environment->insert_test_data( booking_mock_data ).

    bookings_to_test = CORRESPONDING #( booking_mock_data MAPPING bookinguuid = booking_uuid ).

    class_under_test->validatecurrencycode(
        EXPORTING
          keys     = bookings_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR booking IN bookings_to_check
                            NEXT sum += booking-exp_amount_failed_entries )
           act  = lines( failed-booking )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT bookings_to_check INTO booking_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = booking_to_check-exp_amount_failed_entries
            act  = lines( FILTER #( failed-booking USING KEY entity WHERE bookinguuid = booking_to_check-booking_uuid ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ booking_to_check-exp_amount_failed_entries } line(s) in failed expected for '{ booking_to_check-booking_uuid }'|
          ).
      ENDLOOP.
    ENDIF.

    cl_abap_unit_assert=>assert_not_initial( reported ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR booking IN bookings_to_check
                            NEXT sum += booking-exp_amount_reported_entries )
           act  = lines( reported-booking )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT bookings_to_check INTO booking_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = booking_to_check-exp_amount_reported_entries
            act  = lines( FILTER #( reported-booking USING KEY entity WHERE bookinguuid = booking_to_check-booking_uuid ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ booking_to_check-exp_amount_reported_entries } line(s) in reported expected for '{ booking_to_check-booking_id }'|
          ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
