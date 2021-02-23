CLASS /dmo/cx_supplement DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      if_t100_message,
      if_abap_behv_message.

    CONSTANTS:
      BEGIN OF price_required,
        msgid TYPE symsgid VALUE '/DMO/CM_SUPPLEMENT',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF price_required,

      BEGIN OF currency_required,
        msgid TYPE symsgid VALUE '/DMO/CM_SUPPLEMENT',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF currency_required,

      BEGIN OF description_required,
        msgid TYPE symsgid VALUE '/DMO/CM_SUPPLEMENT',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF description_required,

      BEGIN OF numbers_left,
        msgid TYPE symsgid VALUE '/DMO/CM_SUPPLEMENT',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_NUMBERS_LEFT',
        attr2 TYPE scx_attrname VALUE 'MV_SUPPLEMENT_CATEGORY',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF numbers_left,

      BEGIN OF numbers_last,
        msgid TYPE symsgid VALUE '/DMO/CM_SUPPLEMENT',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MV_SUPPLEMENT_CATEGORY',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF numbers_last.


    DATA:
      mv_supplement_category TYPE /dmo/supplement_category,
      mv_numbers_left        TYPE i.


    METHODS:
      constructor
        IMPORTING
          textid              LIKE if_t100_message=>t100key         OPTIONAL
          previous            LIKE previous                         OPTIONAL
          severity            TYPE if_abap_behv_message=>t_severity DEFAULT  if_abap_behv_message=>severity-error
          supplement_category TYPE /dmo/supplement_category         OPTIONAL
          numbers_left        TYPE i                                OPTIONAL
        .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/cx_supplement IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    me->if_abap_behv_message~m_severity = severity.

    me->mv_supplement_category = supplement_category.
    me->mv_numbers_left        = numbers_left.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
