CLASS /dmo/zz_cx_agency_country DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      if_t100_message,
      if_abap_behv_message.

    CONSTANTS:
      message_class TYPE symsgid VALUE '/DMO/ZZ_AGENCY_CNTRY',
      BEGIN OF number_invalid,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_PHONE_NUMBER',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF number_invalid,

      BEGIN OF combination_invalid,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MV_PHONE_NUMBER',
        attr2 TYPE scx_attrname VALUE 'MV_COUNTRY_CODE',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF combination_invalid.


    DATA:
      mv_phone_number TYPE /DMO/I_Agency-PhoneNumber,
      mv_COUNTRY_CODE TYPE /DMO/I_Agency-CountryCode.


    METHODS:
      constructor
        IMPORTING
          textid      LIKE if_t100_message=>t100key         OPTIONAL
          previous    LIKE previous                         OPTIONAL
          severity    TYPE if_abap_behv_message=>t_severity DEFAULT  if_abap_behv_message=>severity-error
          phonenumber TYPE /dmo/i_agency-PhoneNumber        OPTIONAL
          countrycode TYPE /dmo/i_agency-CountryCode        OPTIONAL
            PREFERRED PARAMETER textid
        .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/zz_cx_agency_country IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    me->if_abap_behv_message~m_severity = severity.

    me->mv_phone_number        = phonenumber.
    me->mv_country_code        = countrycode.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
