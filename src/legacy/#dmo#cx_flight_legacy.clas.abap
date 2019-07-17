CLASS /dmo/cx_flight_legacy DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_t100_message .

    CONSTANTS:
      gc_msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',

      BEGIN OF agency_unkown,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_AGENCY_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF agency_unkown,

      BEGIN OF customer_unkown,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MV_CUSTOMER_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF customer_unkown,

      BEGIN OF connection_unknown,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_CARRIER_ID',
        attr2 TYPE scx_attrname VALUE 'MV_CONNECTION_ID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF connection_unknown      ,

      BEGIN OF carrier_unknown,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MV_CARRIER_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF carrier_unknown            ,

      BEGIN OF supplement_unknown,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'MV_SUPPLEMENT_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF supplement_unknown,

      BEGIN OF travel_status_invalid,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'MV_STATUS',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF travel_status_invalid,

      BEGIN OF currency_unknown,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MV_CURRENCY_CODE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF currency_unknown,

      BEGIN OF travel_no_key,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF travel_no_key,

      BEGIN OF no_begin_date,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_begin_date,

      BEGIN OF no_end_date,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_end_date,

      BEGIN OF end_date_before_begin_date,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE 'MV_BEGIN_DATE',
        attr2 TYPE scx_attrname VALUE 'MV_END_DATE',
        attr3 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr4 TYPE scx_attrname VALUE '',
      END OF end_date_before_begin_date,

      BEGIN OF travel_unknown,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF travel_unknown,

      BEGIN OF booking_unknown,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_unknown,

      BEGIN OF booking_no_key,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_no_key,

      BEGIN OF booking_booking_date_invalid,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE 'MV_BOOKING_DATE',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_booking_date_invalid,

      BEGIN OF flight_unknown,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE 'MV_CARRIER_ID',
        attr2 TYPE scx_attrname VALUE 'MV_CONNECTION_ID',
        attr3 TYPE scx_attrname VALUE 'MV_FLIGHT_DATE',
        attr4 TYPE scx_attrname VALUE '',
      END OF flight_unknown,

      BEGIN OF booking_supplement_unknown,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '021',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE 'MV_BOOKING_SUPPLEMENT_ID',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_supplement_unknown,

      BEGIN OF booking_supplement_no_key,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '022',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_supplement_no_key,

      BEGIN OF booking_exists,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '023',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_exists,

      BEGIN OF booking_supplement_exists,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '024',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE 'MV_BOOKING_SUPPLEMENT_ID',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_supplement_exists,

      BEGIN OF travel_no_control,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '025',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF travel_no_control,

      BEGIN OF booking_no_control,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '026',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_no_control,

      BEGIN OF booking_supplement_no_control,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '027',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE 'MV_BOOKING_SUPPLEMENT_ID',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_supplement_no_control,

      BEGIN OF booking_supplement_suppl_id_u,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '028',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE 'MV_BOOKING_SUPPLEMENT_ID',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_supplement_suppl_id_u,

      BEGIN OF booking_supplement_pri_curr_u,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '029',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE 'MV_BOOKING_SUPPLEMENT_ID',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_supplement_pri_curr_u,

      BEGIN OF booking_flight_u,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '030',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_flight_u,

      BEGIN OF booking_price_currency_u,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '031',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_price_currency_u,

      BEGIN OF travel_lock,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '032',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_UNAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF travel_lock,

      BEGIN OF travel_already_exists,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '033',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF travel_already_exists,

      BEGIN OF status_is_not_valid,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '037',
        attr1 TYPE scx_attrname VALUE 'MV_STATUS',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF status_is_not_valid,

      BEGIN OF begin_date_before_system_date,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '038',
        attr1 TYPE scx_attrname VALUE 'MV_BEGIN_DATE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF begin_date_before_system_date,

      BEGIN OF end_date_before_system_date,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '039',
        attr1 TYPE scx_attrname VALUE 'MV_BEGIN_DATE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF end_date_before_system_date,

      BEGIN OF booking_enter_booking_date,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '040',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF booking_enter_booking_date,

      BEGIN OF currency_code_is_initial,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '041',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF currency_code_is_initial,

      BEGIN OF flight_date_befor_booking_date,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '042',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF flight_date_befor_booking_date,

      BEGIN OF BOOKING_date_befor_system_date,
        msgid TYPE symsgid VALUE '/DMO/CM_FLIGHT_LEGAC',
        msgno TYPE symsgno VALUE '043',
        attr1 TYPE scx_attrname VALUE 'MV_TRAVEL_ID',
        attr2 TYPE scx_attrname VALUE 'MV_BOOKING_ID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF BOOKING_date_befor_system_date.


    METHODS constructor
      IMPORTING
        textid                LIKE if_t100_message=>t100key OPTIONAL
        previous              LIKE previous OPTIONAL
        travel_id             TYPE /dmo/travel_id OPTIONAL
        booking_id            TYPE /dmo/booking_id OPTIONAL
        booking_supplement_id TYPE /dmo/booking_supplement_id OPTIONAL
        agency_id             TYPE /dmo/agency_id OPTIONAL
        customer_id           TYPE /dmo/customer_id OPTIONAL
        carrier_id            TYPE /dmo/carrier-carrier_id OPTIONAL
        connection_id         TYPE /dmo/connection-connection_id OPTIONAL
        supplement_id         TYPE /dmo/supplement-supplement_id OPTIONAL
        begin_date            TYPE /dmo/begin_date OPTIONAL
        end_date              TYPE /dmo/end_date OPTIONAL
        booking_date          TYPE /dmo/booking_date OPTIONAL
        flight_date           TYPE /dmo/flight_date OPTIONAL
        status                TYPE /dmo/travel_status OPTIONAL
        currency_code         TYPE /dmo/currency_code OPTIONAL
        uname                 TYPE syuname OPTIONAL.


    DATA: mv_travel_id             TYPE /dmo/travel_id,
          mv_booking_id            TYPE /dmo/booking_id,
          mv_booking_supplement_id TYPE /dmo/booking_supplement_id,
          mv_agency_id             TYPE /dmo/agency_id,
          mv_customer_id           TYPE /dmo/customer_id,
          mv_carrier_id            TYPE /dmo/carrier-carrier_id,
          mv_connection_id         TYPE /dmo/connection-connection_id,
          mv_supplement_id         TYPE /dmo/supplement-supplement_id,
          mv_begin_date            TYPE /dmo/begin_date,
          mv_end_date              TYPE /dmo/end_date,
          mv_booking_date          TYPE /dmo/booking_date,
          mv_flight_date           TYPE /dmo/flight_date,
          mv_status                TYPE /dmo/travel_status,
          mv_currency_code         TYPE /dmo/currency_code,
          mv_uname                 TYPE syuname.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/cx_flight_legacy IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    me->mv_travel_id             = travel_id.
    me->mv_booking_id            = booking_id.
    me->mv_booking_supplement_id = booking_supplement_id.
    me->mv_agency_id             = agency_id.
    me->mv_customer_id           = customer_id.
    me->mv_carrier_id            = carrier_id.
    me->mv_connection_id         = connection_id.
    me->mv_supplement_id         = supplement_id.
    me->mv_begin_date            = begin_date.
    me->mv_end_date              = end_date.
    me->mv_booking_date          = booking_date.
    me->mv_flight_date           = flight_date.
    me->mv_status                = status.
    me->mv_currency_code         = currency_code.
    me->mv_uname                 = uname.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

