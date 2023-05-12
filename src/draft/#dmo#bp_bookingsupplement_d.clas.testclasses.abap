"! @testing BDEF:/DMO/R_BookingSupplement_D
CLASS ltc_bookingsupplement DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_bookingsupplement,
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
      setbooksupplnumber_idempotence   FOR TESTING,

      "! Checks if { @link ..lhc_BookingSupplement.METH:setBookSupplNumber } doesn't draw a number
      "! when applying BookingSupplements with an ID.
      setbooksupplnumber_newbsids  FOR TESTING,

      "! Checks if { @link ..lhc_BookingSupplement.METH:setBookSupplNumber } draws the correct numbers
      "! when applying BookingSupplements with and without an ID.
      setbooksupplnumber_mixed         FOR TESTING,

      "! Calls { @link ..lhc_BookingSupplement.METH:calculateTotalPrice } and expects
      "! that afterwards the totalprice is adjusted.
      calculatetotalprice         FOR TESTING,

      "! Calls { @link ..lhc_BookingSupplement.METH:validateSupplement }
      "! and checks if an existing supplement is set.
      validatesupplement_success      FOR TESTING,

      "! Calls { @link ..lhc_BookingSupplement.METH:validateSupplement }
      "! and checks for a message for an initial supplement.
      validatesupplement_initial      FOR TESTING,

      "! Calls { @link ..lhc_BookingSupplement.METH:validateSupplement }
      "! and checks for a message for a non-existing supplement.
      validatesupplement_not_exist    FOR TESTING,

      "! Calls { @link ..lhc_BookingSupplement.METH:validatecurrencycode }
      "! and checks if a pair of status is valid.
      validate_currency_success        FOR TESTING,

      "! Calls { @link ..lhc_BookingSupplement.METH:validatecurrencycode }
      "! and checks if invalid permutations of sets of status
      "! returns messages.
      validate_currency_not_valid      FOR TESTING.


ENDCLASS.

CLASS ltc_bookingsupplement IMPLEMENTATION.

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
                                   ( 'I_CURRENCY'      )
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


  METHOD setbooksupplnumber_idempotence.
    DATA:
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test  TYPE STANDARD TABLE OF /dmo/r_bookingsupplement_d WITH KEY bookinguuid,
      exp_bookingsupplements      TYPE TABLE FOR READ RESULT /dmo/r_travel_d\\bookingsupplement,
      reported                    TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

    bookingsupplement_mock_data = VALUE #( ( booksuppl_uuid = uuid1  parent_uuid = booking_uuid1  booking_supplement_id = '1' ) ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).

    bookingsupplements_to_test = CORRESPONDING #( bookingsupplement_mock_data MAPPING TO ENTITY ).
    exp_bookingsupplements     = CORRESPONDING #( bookingsupplement_mock_data MAPPING TO ENTITY ).

    class_under_test->setbooksupplnumber(
         EXPORTING
           keys     = CORRESPONDING #( bookingsupplements_to_test )
         CHANGING
           reported = reported
       ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /dmo/r_travel_d
      ENTITY bookingsupplement
        FIELDS ( bookingsupplementid bookinguuid ) WITH CORRESPONDING #( bookingsupplements_to_test )
        RESULT DATA(read_result).

    cl_abap_unit_assert=>assert_equals(
        exp = exp_bookingsupplements
        act = read_result
      ).
  ENDMETHOD.



  METHOD setbooksupplnumber_newbsids.
    DATA:
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test  TYPE STANDARD TABLE OF /dmo/r_bookingsupplement_d WITH KEY bookinguuid,
      exp_bookingsupplements      TYPE TABLE FOR READ RESULT /dmo/r_travel_d\\bookingsupplement,
      reported                    TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

    bookingsupplement_mock_data = VALUE #(
         ( booksuppl_uuid = uuid1  parent_uuid = booking_uuid1 )
         ( booksuppl_uuid = uuid2  parent_uuid = booking_uuid1 )
         ( booksuppl_uuid = uuid3  parent_uuid = booking_uuid2 )
         ( booksuppl_uuid = uuid4  parent_uuid = booking_uuid2 )
       ).
    cds_test_environment->insert_test_data( bookingsupplement_mock_data ).

    bookingsupplements_to_test = CORRESPONDING #( bookingsupplement_mock_data MAPPING TO ENTITY ).

    class_under_test->setbooksupplnumber(
       EXPORTING
         keys     = CORRESPONDING #( bookingsupplements_to_test )
       CHANGING
         reported = reported
     ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /dmo/r_travel_d
      ENTITY bookingsupplement
        FIELDS ( bookingsupplementid bookinguuid ) WITH CORRESPONDING #( bookingsupplements_to_test )
        RESULT DATA(read_results).

    SORT read_results BY bookinguuid ASCENDING  bookingsupplementid ASCENDING.

    exp_bookingsupplements = VALUE #(
        %is_draft = if_abap_behv=>mk-off
          ( booksuppluuid = uuid1  bookinguuid = booking_uuid1  bookingsupplementid = '1' )
          ( booksuppluuid = uuid2  bookinguuid = booking_uuid1  bookingsupplementid = '2' )
          ( booksuppluuid = uuid3  bookinguuid = booking_uuid2  bookingsupplementid = '1' )
          ( booksuppluuid = uuid4  bookinguuid = booking_uuid2  bookingsupplementid = '2' )
        ).

    " Delete BookSupplUUID as the order of it should not relate to given (new) BookingSupplemntIDs.
    DATA: empty_booking_supplement LIKE LINE OF read_results.
    MODIFY exp_bookingsupplements FROM empty_booking_supplement TRANSPORTING booksuppluuid WHERE bookingsupplementid <> empty_booking_supplement-bookingsupplementid.
    MODIFY read_results           FROM empty_booking_supplement TRANSPORTING booksuppluuid WHERE bookingsupplementid <> empty_booking_supplement-bookingsupplementid.

    cl_abap_unit_assert=>assert_equals(
        exp = exp_bookingsupplements
        act = read_results
      ).
  ENDMETHOD.



  METHOD setbooksupplnumber_mixed.
    DATA:
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test  TYPE STANDARD TABLE OF /dmo/r_bookingsupplement_d WITH KEY bookinguuid,
      exp_bookingsupplements      TYPE TABLE FOR READ RESULT /dmo/r_travel_d\\bookingsupplement,
      reported                    TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

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

    class_under_test->setbooksupplnumber(
       EXPORTING
         keys     = CORRESPONDING #( bookingsupplements_to_test )
       CHANGING
         reported = reported
     ).

    cl_abap_unit_assert=>assert_initial( reported ).


    READ ENTITIES OF /dmo/r_travel_d
      ENTITY bookingsupplement
        FIELDS ( bookingsupplementid bookinguuid ) WITH CORRESPONDING #( bookingsupplements_to_test )
        RESULT DATA(read_results).

    " Ensure the given (existing) BookingSupplementIDs are still in place for the related BookSupplUUID before we delete the BookSupplUUID.
    LOOP AT bookingsupplement_mock_data INTO DATA(booking_supplement_mock) WHERE booking_supplement_id IS NOT INITIAL.
      DATA(read_result) = VALUE #( read_results[ KEY id  %is_draft = if_abap_behv=>mk-off  booksuppluuid = booking_supplement_mock-booksuppl_uuid ] OPTIONAL ).
      cl_abap_unit_assert=>assert_not_initial( read_result ).
      cl_abap_unit_assert=>assert_equals(
          exp = booking_supplement_mock-booking_supplement_id
          act = read_result-bookingsupplementid
        ).
    ENDLOOP.

    " Delete BookSupplUUID as the order of it should not relate to given (new) BookingSupplemntIDs.
    DATA: empty_booking_supplement LIKE LINE OF read_results.
    MODIFY exp_bookingsupplements FROM empty_booking_supplement TRANSPORTING booksuppluuid WHERE bookingsupplementid <> empty_booking_supplement-bookingsupplementid.
    MODIFY read_results           FROM empty_booking_supplement TRANSPORTING booksuppluuid WHERE bookingsupplementid <> empty_booking_supplement-bookingsupplementid.

    SORT read_results           BY bookinguuid ASCENDING  bookingsupplementid ASCENDING.
    SORT exp_bookingsupplements BY traveluuid  ASCENDING  bookingsupplementid ASCENDING.

    cl_abap_unit_assert=>assert_equals(
        exp = exp_bookingsupplements
        act = read_results
      ).
  ENDMETHOD.


  METHOD calculatetotalprice.
    CONSTANTS:
      c_supplement_price TYPE /dmo/supplement_price VALUE '10'.

    DATA:
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test  TYPE STANDARD TABLE OF /dmo/r_bookingsupplement_d WITH KEY bookinguuid,
      reported                    TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

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


    READ ENTITIES OF /dmo/r_travel_d
      ENTITY travel
        FIELDS ( traveluuid totalprice ) WITH VALUE #( ( %is_draft = if_abap_behv=>mk-off  traveluuid = travel_uuid1 ) )
        RESULT DATA(read_result).

    cl_abap_unit_assert=>assert_not_initial( read_result ).

    cl_abap_unit_assert=>assert_equals(
        exp = CONV /dmo/total_price( c_supplement_price * lines( bookingsupplement_mock_data ) )
        act = read_result[ 1 ]-totalprice
      ).

  ENDMETHOD.


  METHOD validatesupplement_success.
    CONSTANTS:
      c_supplement_id TYPE /dmo/supplement_id VALUE '123'.

    DATA:
      supplement_mock_data        TYPE STANDARD TABLE OF /dmo/supplement,
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test  TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\bookingsupplement~validatesupplement,
      failed                      TYPE RESPONSE FOR FAILED LATE  /dmo/r_travel_d,
      reported                    TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

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

    bookingsupplements_to_test = CORRESPONDING #( bookingsupplement_mock_data MAPPING booksuppluuid = booksuppl_uuid ).

    class_under_test->validatesupplement(
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

  METHOD validatesupplement_initial.
    CONSTANTS:
      c_supplement_id            TYPE /dmo/supplement_id VALUE '123',
      c_supplement_id_of_booking TYPE /dmo/supplement_id VALUE '111'.

    DATA:
      supplement_mock_data        TYPE STANDARD TABLE OF /dmo/supplement,
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test  TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\bookingsupplement~validatesupplement,
      failed                      TYPE RESPONSE FOR FAILED LATE  /dmo/r_travel_d,
      reported                    TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

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

    bookingsupplements_to_test = CORRESPONDING #( bookingsupplement_mock_data MAPPING booksuppluuid = booksuppl_uuid ).


    class_under_test->validatesupplement(
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

  METHOD validatesupplement_not_exist.
    CONSTANTS:
      c_supplement_id            TYPE /dmo/supplement_id VALUE '123',
      c_supplement_id_of_booking TYPE /dmo/supplement_id VALUE IS INITIAL.

    DATA:
      supplement_mock_data        TYPE STANDARD TABLE OF /dmo/supplement,
      bookingsupplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      bookingsupplements_to_test  TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\bookingsupplement~validatesupplement,
      failed                      TYPE RESPONSE FOR FAILED LATE  /dmo/r_travel_d,
      reported                    TYPE RESPONSE FOR REPORTED LATE  /dmo/r_travel_d.

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

    bookingsupplements_to_test = CORRESPONDING #( bookingsupplement_mock_data MAPPING booksuppluuid = booksuppl_uuid ).


    class_under_test->validatesupplement(
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

  METHOD validate_currency_success.
    DATA:
      booking_supplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      booking_supplements_to_test  TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\bookingsupplement~validatecurrencycode,
      failed                       TYPE RESPONSE FOR FAILED   LATE /dmo/r_travel_d,
      reported                     TYPE RESPONSE FOR REPORTED LATE /dmo/r_travel_d.

    booking_supplement_mock_data = VALUE #(
        ( booksuppl_uuid = uuid1  currency_code = 'EUR' )
        ( booksuppl_uuid = uuid2  currency_code = 'USD' )
      ).
    cds_test_environment->insert_test_data( booking_supplement_mock_data ).

    booking_supplements_to_test = CORRESPONDING #( booking_supplement_mock_data MAPPING booksuppluuid = booksuppl_uuid ).

    class_under_test->validatecurrencycode(
        EXPORTING
          keys     = booking_supplements_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_initial( failed ).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
        exp = lines( booking_supplements_to_test )
        act = lines( reported-bookingsupplement )
      ).
  ENDMETHOD.

  METHOD validate_currency_not_valid.
    TYPES: BEGIN OF t_check.
             INCLUDE TYPE /dmo/a_bksuppl_d.
    TYPES:   exp_amount_reported_entries TYPE i,
             exp_amount_failed_entries   TYPE i,
           END OF t_check,
           t_check_table TYPE STANDARD TABLE OF t_check WITH KEY booksuppl_uuid.

    DATA:
      booking_supplement_mock_data TYPE STANDARD TABLE OF /dmo/a_bksuppl_d,
      booking_supplements_to_check TYPE t_check_table,
      booking_supplement_to_check  TYPE t_check,
      booking_supplements_to_test  TYPE TABLE FOR VALIDATION /dmo/r_travel_d\\bookingsupplement~validatecurrencycode,
      failed                       TYPE RESPONSE FOR FAILED   LATE /dmo/r_travel_d,
      reported                     TYPE RESPONSE FOR REPORTED LATE /dmo/r_travel_d.

    booking_supplements_to_check = VALUE #(
        exp_amount_reported_entries = '2'
        exp_amount_failed_entries   = '1'
        ( booksuppl_uuid = uuid1  currency_code = ''    )
        ( booksuppl_uuid = uuid2  currency_code = 'XXX' )
      ).
    booking_supplement_mock_data = CORRESPONDING #( booking_supplements_to_check ).
    cds_test_environment->insert_test_data( booking_supplement_mock_data ).

    booking_supplements_to_test = CORRESPONDING #( booking_supplement_mock_data MAPPING booksuppluuid = booksuppl_uuid ).

    class_under_test->validatecurrencycode(
        EXPORTING
          keys     = booking_supplements_to_test
        CHANGING
          failed   = failed
          reported = reported
      ).

    cl_abap_unit_assert=>assert_not_initial( failed ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR booking_supplement IN booking_supplements_to_check
                            NEXT sum += booking_supplement-exp_amount_failed_entries )
           act  = lines( failed-bookingsupplement )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT booking_supplements_to_check INTO booking_supplement_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = booking_supplement_to_check-exp_amount_failed_entries
            act  = lines( FILTER #( failed-bookingsupplement USING KEY entity WHERE booksuppluuid = booking_supplement_to_check-booksuppl_uuid ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ booking_supplement_to_check-exp_amount_failed_entries } line(s) in failed expected for '{ booking_supplement_to_check-booksuppl_uuid }'|
          ).
      ENDLOOP.
    ENDIF.

    cl_abap_unit_assert=>assert_not_initial( reported ).
    IF cl_abap_unit_assert=>assert_equals(
           exp  = REDUCE i( INIT sum = 0
                            FOR booking_supplement IN booking_supplements_to_check
                            NEXT sum += booking_supplement-exp_amount_reported_entries )
           act  = lines( reported-bookingsupplement )
           quit = if_abap_unit_constant=>quit-no
         ).
      LOOP AT booking_supplements_to_check INTO booking_supplement_to_check.
        cl_abap_unit_assert=>assert_equals(
            exp  = booking_supplement_to_check-exp_amount_reported_entries
            act  = lines( FILTER #( reported-bookingsupplement USING KEY entity WHERE booksuppluuid = booking_supplement_to_check-booksuppl_uuid ) )
            quit = if_abap_unit_constant=>quit-no
            msg  = |{ booking_supplement_to_check-exp_amount_reported_entries } line(s) in reported expected for '{ booking_supplement_to_check-booksuppl_uuid }'|
          ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.



ENDCLASS.
