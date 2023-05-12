CLASS /dmo/cx_agency DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      if_t100_message,
      if_abap_behv_message.

    CONSTANTS:
      message_class TYPE symsgid VALUE '/DMO/CM_AGENCY',
      BEGIN OF name_required,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF name_required,

      BEGIN OF email_invalid_format,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF email_invalid_format,

      BEGIN OF country_code_invalid,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MV_COUNTRY_CODE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF country_code_invalid,

      BEGIN OF attachment_properties_invalid,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF attachment_properties_invalid,

      BEGIN OF not_sufficient_numbers,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF not_sufficient_numbers,

      BEGIN OF number_range_depleted,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF number_range_depleted,

      BEGIN OF mimetype_missing,
        msgid       TYPE symsgid      VALUE message_class,
        msgno       TYPE symsgno      VALUE '007',
        attr1       TYPE scx_attrname VALUE 'MV_ATTACHMENT',
        attr2       TYPE scx_attrname VALUE '',
        attr3       TYPE scx_attrname VALUE '',
        attr4       TYPE scx_attrname VALUE '',
      END OF mimetype_missing,

      BEGIN OF attachment_empty_missing,
        msgid       TYPE symsgid      VALUE message_class,
        msgno       TYPE symsgno      VALUE '008',
        attr1       TYPE scx_attrname VALUE 'MV_MIMETYPE',
        attr2       TYPE scx_attrname VALUE '',
        attr3       TYPE scx_attrname VALUE '',
        attr4       TYPE scx_attrname VALUE '',
      END OF attachment_empty_missing,

      BEGIN OF mimetype_not_supported,
        msgid       TYPE symsgid      VALUE message_class,
        msgno       TYPE symsgno      VALUE '009',
        attr1       TYPE scx_attrname VALUE 'MV_MIMETYPE',
        attr2       TYPE scx_attrname VALUE '',
        attr3       TYPE scx_attrname VALUE '',
        attr4       TYPE scx_attrname VALUE '',
      END OF mimetype_not_supported,

      BEGIN OF extension_mimetype_mismatch,
        msgid       TYPE symsgid      VALUE message_class,
        msgno       TYPE symsgno      VALUE '010',
        attr1       TYPE scx_attrname VALUE 'MV_MIMETYPE',
        attr2       TYPE scx_attrname VALUE '',
        attr3       TYPE scx_attrname VALUE '',
        attr4       TYPE scx_attrname VALUE '',
      END OF extension_mimetype_mismatch,

      BEGIN OF only_filename,
        msgid       TYPE symsgid      VALUE message_class,
        msgno       TYPE symsgno      VALUE '011',
        attr1       TYPE scx_attrname VALUE 'MV_FILENAME',
        attr2       TYPE scx_attrname VALUE '',
        attr3       TYPE scx_attrname VALUE '',
        attr4       TYPE scx_attrname VALUE '',
      END OF only_filename.


    DATA:
      mv_country_code TYPE /DMO/R_AgencyTP-CountryCode,
      mv_numbers_left TYPE i,
      mv_attachment     TYPE /DMO/R_AgencyTP-attachment,
      mv_mimetype       TYPE /DMO/R_AgencyTP-mimetype,
      mv_filename       TYPE /DMO/R_AgencyTP-Filename.


    METHODS:
      constructor
        IMPORTING
          textid       LIKE if_t100_message=>t100key         OPTIONAL
          previous     LIKE previous                         OPTIONAL
          severity     TYPE if_abap_behv_message=>t_severity DEFAULT  if_abap_behv_message=>severity-error
          countrycode  TYPE /DMO/R_AgencyTP-CountryCode      OPTIONAL
          numbers_left TYPE i                                OPTIONAL
          attachment   TYPE /DMO/R_AgencyTP-attachment      OPTIONAL
          mimetype     TYPE /DMO/R_AgencyTP-mimetype        OPTIONAL
          filename     TYPE /DMO/R_AgencyTP-Filename        OPTIONAL
            PREFERRED PARAMETER textid
        .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/cx_agency IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    me->if_abap_behv_message~m_severity = severity.

    me->mv_country_code   = countrycode.
    me->mv_numbers_left   = numbers_left.
    me->mv_attachment     = attachment.
    me->mv_mimetype       = mimetype.
    me->mv_filename       = filename.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
