"! @testing BDEF:/DMO/R_BookingSupplement_D
CLASS ltc_BookingSupplement DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_BookingSupplement,
      cds_test_environment TYPE REF TO if_cds_test_environment,
      sql_test_environment TYPE REF TO if_osql_test_environment.

    CONSTANTS:
      travel_uuid1  TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A1',
      travel_uuid2  TYPE sysuuid_x16 VALUE '66657221A8E4645C17002DF03754A2',
      booking_uuid1 TYPE sysuuid_x16 VALUE 'CCC7221A8E4645C17002DF03754A1',
      booking_uuid2 TYPE sysuuid_x16 VALUE 'CCC7221A8E4645C17002DF03754A2',
      uuid1         TYPE sysuuid_x16 VALUE 'BBBBBBBBB8E4645C17002DF03754A1',
      uuid2         TYPE sysuuid_x16 VALUE 'BBBBBBBBB8E4645C17002DF03754A2',
      uuid3         TYPE sysuuid_x16 VALUE 'BBBBBBBBB8E4645C17002DF03754A3',
      uuid4         TYPE sysuuid_x16 VALUE 'BBBBBBBBB8E4645C17002DF03754A4',
      uuid5         TYPE sysuuid_x16 VALUE 'BBBBBBBBB8E4645C17002DF03754A5',
      uuid6         TYPE sysuuid_x16 VALUE 'BBBBBBBBB8E4645C17002DF03754A6',
      c_currency    TYPE /dmo/currency_code VALUE 'EUR'.

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
      "! Checks if { @link ..lhc_BookingSupplement.METH:setBookSupplNumber } draws the correct numbers
      "! when applying without an ID.
      setBookSupplNumber_idempotence   FOR TESTING,

      "! Checks if { @link ..lhc_BookingSupplement.METH:setBookSupplNumber } doesn't draw a number
      "! when applying BookingSupplements with an ID.
      setBookSupplNumber_newBSIDs  FOR TESTING,

      "! Checks if { @link ..lhc_BookingSupplement.METH:setBookSupplNumber } draws the correct numbers
      "! when applying BookingSupplements with and without an ID.
      setBookSupplNumber_mixed         FOR TESTING,

      "! Calls { @link ..lhc_BookingSupplement.METH:calculateTotalPrice } and expects
      "! that afterwards the totalprice is adjusted.
      calculateTotalPrice         FOR TESTING,

      "! Calls { @link ..lhc_BookingSupplement.METH:validateSupplement }
      "! and checks if an existing supplement is set.
      validateSupplement_success      FOR TESTING,

      "! Calls { @link ..lhc_BookingSupplement.METH:validateSupplement }
      "! and checks for a message for an initial supplement.
      validateSupplement_initial      FOR TESTING,

      "! Calls { @link ..lhc_BookingSupplement.METH:validateSupplement }
      "! and checks for a message for a non-existing supplement.
      validateSupplement_not_exist    FOR TESTING.


ENDCLASS.

CLASS ltc_BookingSupplement IMPLEMENTATION.

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
                                   ( '/DMO/SUPPLEMENT' )
                                 )
                               ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).

    DATA:
      travel_mock_data  TYPE STANDARD TABLE OF /dmo/a_travel_d,
      booking_mock_data TYPE STANDARD TABLE OF /dmo/a_booking_d.

    travel_mock_data = VALUE #(
        ( travel_uuid = travel_uuid1  currency_code = c_currency )
        ( travel_uuid = travel_uuid2  currency_code = c_currency )
      ).
    cds_test_environment->insert_test_data( travel_mock_data ).

    booking_mock_data = VALUE #(
        ( booking_uuid = booking_uuid1  parent_uuid = travel_uuid1 )
        ( booking_uuid = booking_uuid2  parent_uuid = travel_uuid1 )
      ).
    cds_test_environment->insert_test_data( booking_mock_data ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.                                 "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD class_teardown.
    cds_test_environment->destroy( ).
    sql_test_environment->destroy( ).
  ENDMETHOD.


  METHOD setBookSupplNumber_idempotence.
    DATA:
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test  TYPE STANDARD TABLE OF /DMO/R_BookingSupplement_D WITH KEY bookinguuid,
      exp_bookingsupplements      TYPE TABLE FOR READ RESULT /DMO/R_Travel_D\\BookingSupplement,
      reported                    TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    bookingsupplement_mock_data = VALUE #( ( booksuppl_uuid = uuid1  parent_uuid = booking_uuid1  booking_supplement_id = '1' ) ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).

    bookingsupplements_to_test = CORRESPONDING #( bookingsupplement_mock_data MAPPING TO ENTITY ).
    exp_bookingsupplements     = CORRESPONDING #( bookingsupplement_mock_data MAPPING TO ENTITY ).

    class_under_test->setBookSupplNumber(
         EXPORTING
           keys     = CORRESPONDING #( bookingsupplements_to_test )
         CHANGING
           reported = reported
       ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY BookingSupplement
        FIELDS ( BookingSupplementID BookingUUID ) WITH CORRESPONDING #( bookingsupplements_to_test )
        RESULT DATA(read_result).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_bookingsupplements
        act = read_result
      ).
  ENDMETHOD.



  METHOD setbooksupplnumber_newbsids.
    DATA:
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test  TYPE STANDARD TABLE OF /DMO/R_BookingSupplement_D WITH KEY bookinguuid,
      exp_bookingsupplements      TYPE TABLE FOR READ RESULT /DMO/R_Travel_D\\BookingSupplement,
      reported                    TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    bookingsupplement_mock_data = VALUE #(
         ( booksuppl_uuid = uuid1  parent_uuid = booking_uuid1 )
         ( booksuppl_uuid = uuid2  parent_uuid = booking_uuid1 )
         ( booksuppl_uuid = uuid3  parent_uuid = booking_uuid2 )
         ( booksuppl_uuid = uuid4  parent_uuid = booking_uuid2 )
       ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).

    bookingsupplements_to_test = CORRESPONDING #( bookingsupplement_mock_data MAPPING TO ENTITY ).

    class_under_test->setBookSupplNumber(
       EXPORTING
         keys     = CORRESPONDING #( bookingsupplements_to_test )
       CHANGING
         reported = reported
     ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY BookingSupplement
        FIELDS ( BookingSupplementID BookingUUID ) WITH CORRESPONDING #( bookingsupplements_to_test )
        RESULT DATA(read_result).

    SORT read_result BY BookingUUID ASCENDING  BookingSupplementID ASCENDING.

    exp_bookingsupplements = VALUE #(
        %is_draft = if_abap_behv=>mk-off
          ( booksuppluuid = uuid1  bookinguuid = booking_uuid1  bookingsupplementid = '1' )
          ( booksuppluuid = uuid2  bookinguuid = booking_uuid1  bookingsupplementid = '2' )
          ( booksuppluuid = uuid3  bookinguuid = booking_uuid2  bookingsupplementid = '1' )
          ( booksuppluuid = uuid4  bookinguuid = booking_uuid2  bookingsupplementid = '2' )
        ).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_bookingsupplements
        act = read_result
      ).
  ENDMETHOD.



  METHOD setBookSupplNumber_mixed.
    DATA:
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test  TYPE STANDARD TABLE OF /DMO/R_BookingSupplement_D WITH KEY bookinguuid,
      exp_bookingsupplements      TYPE TABLE FOR READ RESULT /DMO/R_Travel_D\\BookingSupplement,
      reported                    TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    bookingsupplement_mock_data = VALUE #(
        ( booksuppl_uuid = uuid1  parent_uuid = booking_uuid1 )
        ( booksuppl_uuid = uuid2  parent_uuid = booking_uuid1  booking_supplement_id = '1' )
        ( booksuppl_uuid = uuid3  parent_uuid = booking_uuid1 )
        ( booksuppl_uuid = uuid4  parent_uuid = booking_uuid2 )
        ( booksuppl_uuid = uuid5  parent_uuid = booking_uuid2  booking_supplement_id = '4' )
        ( booksuppl_uuid = uuid6  parent_uuid = booking_uuid2 )
      ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).

    bookingsupplements_to_test = CORRESPONDING #( bookingsupplement_mock_data MAPPING TO ENTITY ).

    exp_bookingsupplements = VALUE #(
        %is_draft = if_abap_behv=>mk-off
          ( booksuppluuid = uuid1  bookinguuid = booking_uuid1  bookingsupplementid = '2' )
          ( booksuppluuid = uuid2  bookinguuid = booking_uuid1  bookingsupplementid = '1' )
          ( booksuppluuid = uuid3  bookinguuid = booking_uuid1  bookingsupplementid = '3' )
          ( booksuppluuid = uuid4  bookinguuid = booking_uuid2  bookingsupplementid = '5' )
          ( booksuppluuid = uuid5  bookinguuid = booking_uuid2  bookingsupplementid = '4' )
          ( booksuppluuid = uuid6  bookinguuid = booking_uuid2  bookingsupplementid = '6' )
        ).

    class_under_test->setBookSupplNumber(
       EXPORTING
         keys     = CORRESPONDING #( bookingsupplements_to_test )
       CHANGING
         reported = reported
     ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /DMO/R_Travel_D
      ENTITY BookingSupplement
        FIELDS ( BookingSupplementID BookingUUID ) WITH CORRESPONDING #( bookingsupplements_to_test )
        RESULT DATA(read_result).

    SORT read_result            BY BookingUUID ASCENDING  BookingSupplementID ASCENDING.
    SORT exp_bookingsupplements BY TravelUUID  ASCENDING  BookingSupplementID ASCENDING.

    cl_abap_unit_assert=>assert_equals(
        exp = exp_bookingsupplements
        act = read_result
      ).
  ENDMETHOD.


  METHOD calculatetotalprice.
    CONSTANTS:
      c_supplement_price TYPE /dmo/supplement_price VALUE '10'.

    DATA:
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test  TYPE STANDARD TABLE OF /DMO/R_BookingSupplement_D WITH KEY bookinguuid,
      reported                    TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    bookingsupplement_mock_data = VALUE #(
        price  = c_supplement_price
        currency_code = c_currency
        ( booksuppl_uuid = uuid1  parent_uuid = booking_uuid1  root_uuid = travel_uuid1 )
        ( booksuppl_uuid = uuid2  parent_uuid = booking_uuid1  root_uuid = travel_uuid1 )
        ( booksuppl_uuid = uuid3  parent_uuid = booking_uuid1  root_uuid = travel_uuid1 )
      ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).

    bookingsupplements_to_test = CORRESPONDING #( bookingsupplement_mock_data MAPPING TO ENTITY ).

    class_under_test->calculatetotalprice(
       EXPORTING
         keys     = CORRESPONDING #( bookingsupplements_to_test )
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
        exp = CONV /dmo/total_price( c_supplement_price * lines( bookingsupplement_mock_data ) )
        act = read_result[ 1 ]-TotalPrice
      ).

  ENDMETHOD.


  METHOD validateSupplement_success.
    CONSTANTS:
      c_supplement_id TYPE /dmo/supplement_id VALUE '123'.

    DATA:
      supplement_mock_data TYPE STANDARD TABLE OF /dmo/supplement,
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test   TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\BookingSupplement~validateSupplement,
      failed             TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported           TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    supplement_mock_data = VALUE #( ( supplement_id = c_supplement_id ) ).
    sql_test_environment->insert_test_data( supplement_mock_data ).

    bookingsupplement_mock_data = VALUE #(
        (
          booksuppl_uuid = uuid1
          parent_uuid    = booking_uuid1
          root_uuid      = travel_uuid1
          supplement_id  = c_supplement_id
        )
      ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).

    bookingsupplements_to_test = CORRESPONDING #( bookingsupplement_mock_data MAPPING BookSupplUUID = booksuppl_uuid ).

    class_under_test->validateSupplement(
        EXPORTING
          keys     = bookingsupplements_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial(     failed   ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( reported-bookingsupplement )
      ).
  ENDMETHOD.

  METHOD validateSupplement_initial.
    CONSTANTS:
      c_supplement_id            TYPE /dmo/supplement_id VALUE '123',
      c_supplement_id_of_booking TYPE /dmo/supplement_id VALUE '111'.

    DATA:
      supplement_mock_data TYPE STANDARD TABLE OF /dmo/supplement,
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test   TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\BookingSupplement~validateSupplement,
      failed             TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported           TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    supplement_mock_data = VALUE #( ( supplement_id = c_supplement_id ) ).
    sql_test_environment->insert_test_data( supplement_mock_data ).

    bookingsupplement_mock_data = VALUE #(
        (
          booksuppl_uuid = uuid1
          parent_uuid    = booking_uuid1
          root_uuid      = travel_uuid1
          supplement_id  = c_supplement_id_of_booking
        )
      ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).

    bookingsupplements_to_test = CORRESPONDING #( bookingsupplement_mock_data MAPPING BookSupplUUID = booksuppl_uuid ).


    class_under_test->validateSupplement(
        EXPORTING
          keys     = bookingsupplements_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-bookingsupplement )
      ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lines( reported-bookingsupplement )
      ).
  ENDMETHOD.

  METHOD validateSupplement_not_exist.
    CONSTANTS:
      c_supplement_id            TYPE /dmo/supplement_id VALUE '123',
      c_supplement_id_of_booking TYPE /dmo/supplement_id VALUE IS INITIAL.

    DATA:
      supplement_mock_data TYPE STANDARD TABLE OF /dmo/supplement,
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test   TYPE TABLE FOR VALIDATION /DMO/R_Travel_D\\BookingSupplement~validateSupplement,
      failed             TYPE RESPONSE FOR FAILED LATE  /DMO/R_Travel_D,
      reported           TYPE RESPONSE FOR REPORTED LATE  /DMO/R_Travel_D.

    supplement_mock_data = VALUE #( ( supplement_id = c_supplement_id ) ).
    sql_test_environment->insert_test_data( supplement_mock_data ).

    bookingsupplement_mock_data = VALUE #(
        (
          booksuppl_uuid = uuid1
          parent_uuid    = booking_uuid1
          root_uuid      = travel_uuid1
          supplement_id  = c_supplement_id_of_booking
        )
      ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).

    bookingsupplements_to_test = CORRESPONDING #( bookingsupplement_mock_data MAPPING BookSupplUUID = booksuppl_uuid ).


    class_under_test->validateSupplement(
        EXPORTING
          keys     = bookingsupplements_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = lines( failed-bookingsupplement )
      ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = lines( reported-bookingsupplement )
      ).
  ENDMETHOD.



ENDCLASS.
