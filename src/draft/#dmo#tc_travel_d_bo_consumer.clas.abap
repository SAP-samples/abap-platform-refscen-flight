"! <p class="shorttext synchronized" lang="en">/DMO/R_TRAVEL_D consumer as code under test for RAP BO TDF</p>
"! A consumer of /DMO/R_TRAVEL_D BO for demonstration of RAP BO Test double framework which is used as code under test.<br/>
"! The consumer methods has EML statements with draft instances, as dependency on /DMO/R_TRAVEL_D BO, which can be isolated using RAP BO test double framework.<br/><br/>
"! Check test classes from {@link /DMO/TC_BOTD_TRAVEL_D_DEMOS} for demos.
CLASS /dmo/tc_travel_d_bo_consumer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  types:
    failed_travel_type type response for failed early /DMO/R_TRAVEL_D .
  types:
    reported_travel_type type response for reported early /DMO/R_TRAVEL_D .
  types:
    mapped_travel_type type response for mapped early /DMO/R_TRAVEL_D .
  types:
  ""MODIFY
    create_travel_input_type type table for create /DMO/R_TRAVEL_D .
  types:
    create_booking_by_assoc_in_typ type table for create /DMO/R_TRAVEL_D\_Booking .
  types:
    update_travel_input_type type table for update /DMO/R_TRAVEL_D .
  types:
    update_booking_input_type type table for update /DMO/R_TRAVEL_D\\BOOKING .
  types:
    delete_travel_input_type type table for delete /DMO/R_TRAVEL_D .
  types:
    accept_travel_act_in_type type table for action import  /DMO/R_TRAVEL_D~acceptTravel .
  types:
    accept_travel_act_res_type type table for action result  /DMO/R_TRAVEL_D~acceptTravel .
  types:
    deduct_discount_act_in_type type table for action import  /DMO/R_TRAVEL_D~deductDiscount .
  types:
    deduct_discount_act_res_type type table for action result  /DMO/R_TRAVEL_D~deductDiscount .

  types:
  ""READ
    read_travel_input_type type table for read import /DMO/R_TRAVEL_D .
  types:
    read_booking_by_assoc_in_typ type table for read import /DMO/R_TRAVEL_D\_Booking .
  types:
    read_travel_result_type type table for read result /DMO/R_TRAVEL_D .
  types:
    read_booking_by_assoc_res_typ type table for read result /DMO/R_TRAVEL_D\_Booking .
  types:
    read_booking_by_assoc_link_typ type table for read link /DMO/R_TRAVEL_D\_Booking .

  methods CREATE_TRAVEL
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
  methods READ_BOOKING_BY_ASSOC
    importing
      !TRAVEL type READ_BOOKING_BY_ASSOC_IN_TYP
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

  methods DEDUCT_DISCOUNT_ACTION
    importing
      !DEDUCT_DISCOUNT_INPUT type DEDUCT_DISCOUNT_ACT_IN_TYPE
    exporting
      !MAPPED type MAPPED_TRAVEL_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE
      !RESULT type DEDUCT_DISCOUNT_ACT_RES_TYPE.

  methods MOD_EML_W_MULT_ENTS_ND_OPS
    importing
      !CREATE_TRAVEL type CREATE_TRAVEL_INPUT_TYPE OPTIONAL
      !CREATE_BA_BOOKING type CREATE_BOOKING_BY_ASSOC_IN_TYP OPTIONAL
      !UPDATE_BOOKING type UPDATE_BOOKING_INPUT_TYPE OPTIONAL
    exporting
      !MAPPED type MAPPED_TRAVEL_TYPE
      !REPORTED type REPORTED_TRAVEL_TYPE
      !FAILED type FAILED_TRAVEL_TYPE .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/tc_travel_d_bo_consumer IMPLEMENTATION.

  method create_travel.

  "This is a demo code under test and only includes the EML operation to be isolated.
  "In actual scenario there will be additional business logic that forms the code to be tested.

    modify entities of /DMO/R_TRAVEL_D
     entity travel
       create from travel
     reported reported
     failed failed
     mapped mapped.

  "This is a demo code under test and only includes the EML operation to be isolated.
  "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.


  method update_travel.
  "This is a demo code under test and only includes the EML operation to be isolated.
  "In actual scenario there will be additional business logic that forms the code to be tested.

    modify entities of /DMO/R_TRAVEL_D
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

      modify entities of /DMO/R_TRAVEL_D
       entity travel
         create by \_Booking
          from booking_by_travel
       reported reported
       failed failed
       mapped mapped.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.


  method create_nd_action_in_same_eml.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      modify entities of /DMO/R_TRAVEL_D
       entity travel
         create from travel
         execute acceptTravel from value #( ( %cid_ref = travel[ 1 ]-%cid ) )
       reported reported
       failed failed
       mapped mapped.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

  endmethod.

  method mod_eml_w_mult_ents_nd_ops.

  "This is a demo code under test and only includes the EML operation to be isolated.
  "In actual scenario there will be additional business logic that forms the code to be tested.

    modify entities of /dmo/r_travel_d
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

  method create_nd_action_in_split_emls.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      modify entities of /DMO/R_TRAVEL_D
       entity travel
         create from travel
       reported reported
       failed failed
       mapped mapped.

      if mapped is not initial.
        modify entities of /DMO/R_TRAVEL_D
         entity travel
           execute acceptTravel from value #( ( %key-TravelUUID = mapped-travel[ 1 ]-TravelUUID ) )
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
    modify entities of /DMO/R_TRAVEL_D
     entity travel
             create fields ( customerid begindate enddate ) with
                         value #( ( %cid        = 'CID_100'    " Preliminary ID for new travel instance
                                    %is_draft = if_abap_behv=>mk-on
                                    customerid  = '9876'
                                    AgencyID    = '70006'
                                    begindate   = '20180101'
                                    enddate     = '20180101' ) )
             " Create a new booking by association
             create by \_booking fields ( customerid FlightPrice ) with
                         value #( ( %cid_ref  = 'CID_100'      "refers to the root (travel instance)
                                    %tky-%is_draft = if_abap_behv=>mk-on
                                    %target   = value #( (
                                                  %cid           = 'CID_200' " Preliminary ID for new booking instance
                                                  %is_draft = if_abap_behv=>mk-on
                                                  %data-customerid     = '9876'
                                                  %data-flightprice    = 100 ) ) ) )
       " Create a booking supplement by association
       entity booking
             create by \_BookingSupplement fields ( supplementid BookSupplPrice  ) with
                       value #( ( %cid_ref = 'CID_200'
                                  %is_draft = if_abap_behv=>mk-on
                                  %target  = value #( (
                                                 %cid          = 'CID_300'
                                                 %is_draft = if_abap_behv=>mk-on
                                                 supplementid  = '007'
                                                 BookSupplPrice  = 100
                                                  ) ) ) )

       MAPPED   mapped
       FAILED   failed
       REPORTED reported.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.


  method read_booking_by_assoc.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      read entities of /DMO/R_TRAVEL_D
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


  method read_travel.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      read entities of /DMO/R_TRAVEL_D
       entity travel
         from travel
       result result
       reported reported
       failed failed.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.
  endmethod.


  method delete_travel.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

      modify entities of /DMO/R_TRAVEL_D
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

     modify entities of /DMO/R_TRAVEL_D
      entity travel
       execute acceptTravel from accept_travel_input
        result result
        mapped mapped
        failed failed
        reported reported.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.
  ENDMETHOD.

  METHOD DEDUCT_DISCOUNT_ACTION.
    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

     modify entities of /DMO/R_TRAVEL_D
      entity travel
       execute deductDiscount from deduct_discount_input
        result result
        mapped mapped
        failed failed
        reported reported.

    "This is a demo code under test and only includes the EML operation to be isolated.
    "In actual scenario there will be additional business logic that forms the code to be tested.

  ENDMETHOD.

ENDCLASS.
