"! <p class="shorttext synchronized" lang="en">/DMO/I_TRAVEL_M consumer as code under test for RAP BO TDF</p>
"! A consumer of /DMO/I_TRAVEL_M BO for demonstration of RAP BO Test double framework which is used as code under test.<br/>
"! The consumer methods has EML statements as dependency on /DMO/I_TRAVEL_M BO, which can be isolated using RAP BO test double framework.<br/><br/>
"! Check test classes from {@link /DMO/TC_BOTD_TRAVEL_M_DEMOS} for demos.
CLASS /dmo/tc_travel_m_bo_consumer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  types:
    failed_travel_type type response for failed early /DMO/I_TRAVEL_M .
  types:
    reported_travel_type type response for reported early /DMO/I_TRAVEL_M .
  types:
    mapped_travel_type type response for mapped early /DMO/I_TRAVEL_M .
  types:
  ""MODIFY
    create_travel_input_type type table for create /DMO/I_TRAVEL_M .
  types:
    create_booking_by_assoc_in_typ type table for create /DMO/I_TRAVEL_M\_Booking .
  types:
    update_travel_input_type type table for update /DMO/I_TRAVEL_M .
  types:
    delete_travel_input_type type table for delete /DMO/I_TRAVEL_M .
  types:
    update_booking_input_type type table for update /DMO/I_TRAVEL_M\\BOOKING .
  types:
    accept_travel_act_in_type type table for action import  /DMO/I_TRAVEL_M~acceptTravel .
  types:
    accept_travel_act_res_type type table for action result  /DMO/I_TRAVEL_M~acceptTravel .
  types:
  ""READ
    read_travel_input_type type table for read import /DMO/I_TRAVEL_M .
  types:
    read_booking_by_assoc_in_typ type table for read import /DMO/I_TRAVEL_M\_Booking .
  types:
    read_travel_result_type type table for read result /DMO/I_TRAVEL_M .
  types:
    read_booking_by_assoc_res_typ type table for read result /DMO/I_TRAVEL_M\_Booking .
  types:
    read_booking_by_assoc_link_typ type table for read link /DMO/I_TRAVEL_M\_Booking .
  types:
    get_perm_input_instances_type type table for permissions key /DMO/I_TRAVEL_M .
  types:
    get_perm_input_request_type type structure for permissions request /DMO/I_TRAVEL_M .
  types:
    get_permissions_response_type type structure for permissions result /DMO/I_TRAVEL_M .
  types:
    set_locks_input_type type table for lock /DMO/I_TRAVEL_M .

  methods CREATE_TRAVEL
    importing
      !TRAVEL type CREATE_TRAVEL_INPUT_TYPE
    exporting
      !MAPPED type MAPPED_TRAVEL_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE .
   methods CREATE_TRAVEL_AUTO_FILL_CID
    importing
      !TRAVEL type CREATE_TRAVEL_INPUT_TYPE
    exporting
      !MAPPED type MAPPED_TRAVEL_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE .
  methods CREATE_BOOKING_BY_ASSOC
    importing
      !BOOKING_BY_TRAVEL type CREATE_BOOKING_BY_ASSOC_IN_TYP
    exporting
      !MAPPED type MAPPED_TRAVEL_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE .
  methods CREATE_BA_BOOKING_FOR_TRAVEL_1
    exporting
      !MAPPED type MAPPED_TRAVEL_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE .
  methods UPDATE_TRAVEL
    importing
      !TRAVEL type UPDATE_TRAVEL_INPUT_TYPE
    exporting
      !MAPPED type MAPPED_TRAVEL_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE .
  methods DELETE_TRAVEL
    importing
      !TRAVEL type DELETE_TRAVEL_INPUT_TYPE
    exporting
      !MAPPED type MAPPED_TRAVEL_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE .
  methods DEEP_CREATE_TRAVEL_BO
    exporting
      !MAPPED type MAPPED_TRAVEL_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE .
  methods CREATE_ND_ACTION_IN_SAME_EML
    importing
      !TRAVEL type CREATE_TRAVEL_INPUT_TYPE
    exporting
      !MAPPED type MAPPED_TRAVEL_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE .
  methods CREATE_ND_ACTION_IN_SPLIT_EMLS
    importing
      !TRAVEL type CREATE_TRAVEL_INPUT_TYPE
      !ACCEPT_TRAVEL_INPUT type ACCEPT_TRAVEL_ACT_IN_TYPE
    exporting
      !MAPPED type MAPPED_TRAVEL_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE .
  methods READ_TRAVEL
    importing
      !TRAVEL type READ_TRAVEL_INPUT_TYPE
    exporting
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE
      !RESULT type READ_TRAVEL_RESULT_TYPE .
  methods READ_TRAVEL_1
    exporting
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE
      !RESULT type READ_TRAVEL_RESULT_TYPE .
  methods READ_BOOKING_BY_ASSOC
    importing
      !TRAVEL type READ_BOOKING_BY_ASSOC_IN_TYP
    exporting
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE
      !RESULT type READ_BOOKING_BY_ASSOC_RES_TYP
      !LINKS  type READ_BOOKING_BY_ASSOC_LINK_TYP.
  methods READ_BOOKING_BY_ASSOC_LINK
    importing
      !BOOKING type READ_BOOKING_BY_ASSOC_IN_TYP
    exporting
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE
      !RESULT type READ_BOOKING_BY_ASSOC_RES_TYP
      !LINKS  type READ_BOOKING_BY_ASSOC_LINK_TYP.

  methods ACCEPT_TRAVEL_ACTION
    importing
      !ACCEPT_TRAVEL_INPUT type ACCEPT_TRAVEL_ACT_IN_TYPE
    exporting
      !MAPPED type MAPPED_TRAVEL_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE
      !RESULT type ACCEPT_TRAVEL_ACT_RES_TYPE.

  methods MOD_EML_W_MULT_ENTS_ND_OPS
    importing
      !CREATE_TRAVEL type CREATE_TRAVEL_INPUT_TYPE OPTIONAL
      !CREATE_BA_BOOKING type CREATE_BOOKING_BY_ASSOC_IN_TYP OPTIONAL
      !UPDATE_BOOKING type UPDATE_BOOKING_INPUT_TYPE OPTIONAL
    exporting
      !MAPPED type MAPPED_TRAVEL_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE .

  methods GET_PERMISSIONS
    importing
      !GET_PERMISSIONS_INSTANCES_INP type GET_PERM_INPUT_INSTANCES_TYPE
      !GET_PERMISSIONS_REQUEST_INP type GET_PERM_INPUT_REQUEST_TYPE
    exporting
      !RESULT type GET_PERMISSIONS_RESPONSE_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE.

  methods SET_LOCKS
    importing
      !SET_LOCKS_INPUT type SET_LOCKS_INPUT_TYPE
    exporting
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/tc_travel_m_bo_consumer IMPLEMENTATION.

  method create_travel.

  "This is a demo code under test and only includes the EML operation to be isolated.
  "In actual scenario there will be additional business logic that forms the code to be tested.

    modify entities of /dmo/i_travel_m
     entity travel
       create from travel
     reported reported
     failed failed
     mapped mapped.

  "This is a demo code under test and only includes the EML operation to be isolated.
  "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.

   method create_travel_auto_fill_cid.

  "This is a demo code under test and only includes the EML operation to be isolated.
  "In actual scenario there will be additional business logic that forms the code to be tested.

    modify entities of /dmo/i_travel_m
     entity travel
       create auto fill cid with value #( ( Travel_id = '000001' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_987' )
                                          ( Travel_id = '000002' Agency_ID = '000111' Customer_ID = '000006' description = 'Travel_988' ) )
     reported reported
     failed failed
     mapped mapped.

  "This is a demo code under test and only includes the EML operation to be isolated.
  "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.

  method mod_eml_w_mult_ents_nd_ops.

  "This is a demo code under test and only includes the EML operation to be isolated.
  "In actual scenario there will be additional business logic that forms the code to be tested.

    modify entities of /dmo/i_travel_m
     entity travel
       create from create_travel
       create by \_Booking from create_ba_booking
     entity booking
       update from update_booking
     reported reported
     failed failed
     mapped mapped.

  "This is a demo code under test and only includes the EML operation to be isolated.
  "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.


  method update_travel.
  "This is a demo code under test and only includes the EML operation to be isolated.
  "In actual scenario there will be additional business logic that forms the code to be tested.

    modify entities of /dmo/i_travel_m
     entity travel
       update from travel
     reported reported
     failed failed
     mapped mapped.

  "This is a demo code under test and only includes the EML operation to be isolated.
  "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.


  method create_booking_by_assoc.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      modify entities of /dmo/i_travel_m
       entity travel
         create by \_Booking
          from booking_by_travel
       reported reported
       failed failed
       mapped mapped.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.

  method create_ba_booking_for_travel_1.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      modify entities of /dmo/i_travel_m
       entity travel
         create by \_Booking
             from value #( ( %key-Travel_ID = 1
                            %target = VALUE #( (  %cid = 'Travel_1_Booking_1'
                                                  carrier_id = '111' %control-carrier_id = if_abap_behv=>mk-on
                                                  connection_id = '1234' %control-connection_id = if_abap_behv=>mk-on
                                                  customer_id = '000006' %control-Customer_ID = if_abap_behv=>mk-on
                                                  flight_date = cl_abap_context_info=>get_system_date( ) + 10 %control-Flight_Date = if_abap_behv=>mk-on
                                                  booking_date = cl_abap_context_info=>get_system_date( ) %control-booking_date = if_abap_behv=>mk-on
                                                  booking_status = 'B' %control-booking_status = if_abap_behv=>mk-on ) ) ) )
       reported reported
       failed failed
       mapped mapped.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.


  method create_nd_action_in_same_eml.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      modify entities of /dmo/i_travel_m
       entity travel
         create from travel
         execute acceptTravel from value #( ( %cid_ref = travel[ 1 ]-%cid ) )
       reported reported
       failed failed
       mapped mapped.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

  endmethod.


  method create_nd_action_in_split_emls.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      modify entities of /dmo/i_travel_m
       entity travel
         create from travel
       reported reported
       failed failed
       mapped mapped.

      if mapped is not initial.
        modify entities of /dmo/i_travel_m
         entity travel
           execute acceptTravel from value #( ( %key-travel_id = mapped-travel[ 1 ]-travel_id ) )
         reported reported
         failed failed
         mapped mapped.
      endif.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.


  method deep_create_travel_bo.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

    " Create a BO hierarchy using Deep Create
    modify entities of /dmo/i_travel_m
     entity travel
             create fields ( customer_id begin_date end_date overall_status ) with
                         value #( ( %cid        = 'Travel_987'    " Preliminary ID for new travel instance
                                    customer_id  = '000006'
                                    description = 'Travel_987'
                                    overall_status = 'O' ) ) "Travel status open
             create by \_booking fields ( carrier_id connection_id customer_id Booking_Date booking_status Flight_Date ) with
                         value #( ( %cid_ref  = 'Travel_987'      "refers to the root (travel instance)
                                    %target   = value #( (
                                                   %cid = 'Travel_987_Booking_001' " Preliminary ID for new booking instance
                                                          carrier_id = '000111'
                                                          connection_id = 'C1'
                                                          Customer_ID = '000006'
                                                          Flight_Date = cl_abap_context_info=>get_system_date( ) + 10
                                                          booking_date = cl_abap_context_info=>get_system_date( )
                                                          booking_status = 'B' ) ) ) )
       entity booking
             create by \_BookSupplement fields ( supplement_id price  ) with
                       value #( ( %cid_ref = 'Travel_987_Booking_001'
                                  %target  = value #( (
                                                 %cid          = 'Travel_987_Booking_001_BookingSupp_01'
                                                 supplement_id  = '007'
                                                 price         = 100
                                                  ) ) ) )

       MAPPED   mapped
       FAILED   failed
       REPORTED reported ##NO_TEXT.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.


  method read_booking_by_assoc.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      read entities of /dmo/i_travel_m
       entity travel
         by \_Booking
          from travel
       link links
       result result
       reported reported
       failed failed.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.

   method read_booking_by_assoc_link.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

       """""""""""""""""""""""""""CODE UNDER TEST"""""""""""""""""""""""""""""""""""""""
    read entities of /dmo/i_travel_m
     entity travel
       by \_Booking from booking
         result result
         link links
     reported reported
     failed failed.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.


  method read_travel.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      read entities of /dmo/i_travel_m
       entity travel
         from travel
       result result
       reported reported
       failed failed.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.

  method read_travel_1.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

       READ ENTITIES OF /dmo/i_travel_m
       ENTITY travel
         FROM value #( ( travel_id =  1
                         %control-description = if_abap_behv=>mk-on ) )
       RESULT result
       REPORTED reported
       FAILED failed.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.

  method delete_travel.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      modify entities of /dmo/i_travel_m
       entity travel
         delete from travel
       reported reported
       failed failed
       mapped mapped.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.

  METHOD ACCEPT_TRAVEL_ACTION.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

     modify entities of /dmo/i_travel_m
      entity travel
       execute acceptTravel from accept_travel_input
        result result
        mapped mapped
        failed failed
        reported reported.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

  ENDMETHOD.

  METHOD get_permissions.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      get permissions only instance of /dmo/i_travel_m
      entity travel
      from get_permissions_instances_inp
      request get_permissions_request_inp
      result result
      failed failed
      reported reported.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

  ENDMETHOD.

  METHOD set_locks.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      set locks of /dmo/i_travel_m
      entity travel
      from value #( ( travel_id = 1 ) )
      entity Booking
      from value #( ( travel_id = 2  booking_id = 201 ) )
      failed   failed
      reported reported.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

  ENDMETHOD.

ENDCLASS.
