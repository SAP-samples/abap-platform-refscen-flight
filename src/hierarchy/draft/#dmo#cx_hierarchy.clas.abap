CLASS /dmo/cx_hierarchy DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      if_t100_message,
      if_abap_behv_message.

    CONSTANTS:
      message_class TYPE symsgid VALUE '/DMO/CM_HIERARCHY',

      " 100      Reorder Action received invalid values/combinations.
      BEGIN OF invalid_combination,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '100',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_combination,

      " 110      Source key "&1" does not exist.
      BEGIN OF source_does_not_exist,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '110',
        attr1 TYPE scx_attrname VALUE 'EMPLOYEE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF source_does_not_exist,

      " 111      Source key "&1" is used multiple times.
      BEGIN OF source_used_multiple_times,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '111',
        attr1 TYPE scx_attrname VALUE 'EMPLOYEE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF source_used_multiple_times,

      " 120      Target key "&1" does not exist.
      BEGIN OF target_does_not_exist,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '120',
        attr1 TYPE scx_attrname VALUE 'EMPLOYEE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF target_does_not_exist,

      " 121      Target key "&1" is used multiple times.
      BEGIN OF target_used_multiple_times,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '121',
        attr1 TYPE scx_attrname VALUE 'EMPLOYEE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF target_used_multiple_times,

      " 122      Either target or last position shall be flagged.
      BEGIN OF target_key_and_last,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '122',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF target_key_and_last,

      " 130      Cannot move multiple Employees to last postion under same Manager.
      BEGIN OF multiple_last,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '130',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF multiple_last,

      " 131      Key "&1" is used as source and target.
      BEGIN OF source_and_target,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '131',
        attr1 TYPE scx_attrname VALUE 'EMPLOYEE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF source_and_target,

      " 132      Cannot reorder Employee "&1" while assinging different manager.
      BEGIN OF different_manager,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '132',
        attr1 TYPE scx_attrname VALUE 'EMPLOYEE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF different_manager,

      " 200      At least one name needs to be given.
      BEGIN OF names_given,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '200',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF names_given,

      " 201      Invalid Salary: Enter a positive numerical value.
      BEGIN OF invalid_salary,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '201',
        attr1 TYPE scx_attrname VALUE 'EMPLOYEE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_salary.

    DATA:
      employee TYPE /dmo/employee_id.

    METHODS:
      constructor
        IMPORTING
          textid   LIKE if_t100_message=>t100key         OPTIONAL
          previous LIKE previous                         OPTIONAL
          severity TYPE if_abap_behv_message=>t_severity DEFAULT  if_abap_behv_message=>severity-error
          employee TYPE /dmo/employee_id      OPTIONAL
            PREFERRED PARAMETER textid
        .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/cx_hierarchy IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    me->if_abap_behv_message~m_severity = severity.

    me->employee = employee.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
