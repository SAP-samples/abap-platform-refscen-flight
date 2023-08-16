*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lc_test_data_config DEFINITION FINAL CREATE PUBLIC.

PUBLIC SECTION.
  METHODS constructor.
  METHODS insert_test_data_in_doubles
            CHANGING co_environment TYPE REF TO if_cds_test_environment.
  METHODS get_agency_data
            RETURNING VALUE(rs_agency) TYPE /dmo/agency.
  METHODS get_customer_data
            RETURNING VALUE(rs_customer) TYPE /dmo/customer.
  METHODS get_carrier_data
            RETURNING VALUE(rs_carrier) TYPE /dmo/carrier.

PRIVATE SECTION.
   class-data gt_agency TYPE STANDARD TABLE OF /dmo/agency.
   class-data gt_customer TYPE STANDARD TABLE OF /dmo/customer.
   class-data gt_carrier TYPE STANDARD TABLE OF /dmo/carrier.

ENDCLASS.

CLASS lc_test_data_config IMPLEMENTATION.
  METHOD constructor.
    gt_agency = VALUE #( ( agency_id = '70001' ##NO_TEXT
                           name = 'SNS travels'
                           street = 'wall street'
                           postal_code = '560056'
                           city = 'berlin'
                           country_code = 'DE'
                             ) ).

    gt_customer =  VALUE #( ( customer_id = '00010' ##NO_TEXT
                              first_name = 'John'
                              last_name = 'doe'
                              title = 'Mr.'
                              street = 'wall street'
                              postal_code = '560056'
                              city = 'berlin'
                              country_code = 'DE'
                               ) ).

    gt_carrier = VALUE #( ( carrier_id = 'AA' ##NO_TEXT
                            name = 'American Airlines'
                            currency_code = 'USD'
                             ) ).
  ENDMETHOD.

  METHOD insert_test_data_in_doubles.
     co_environment->insert_test_data( gt_agency ).
     co_environment->insert_test_data( gt_customer ).
  ENDMETHOD.

  METHOD get_agency_data.
    rs_agency = gt_agency[ 1 ].
  ENDMETHOD.

  METHOD get_customer_data.
    rs_customer = gt_customer[ 1 ].
  ENDMETHOD.

  METHOD get_carrier_data.
    rs_carrier = gt_carrier[ 1 ].
  ENDMETHOD.


ENDCLASS.
