CLASS /dmo/cl_data_gen_cross_bo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES /dmo/if_data_generation_badi .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS _travel.
    METHODS _customer.
    METHODS _agency.
ENDCLASS.



CLASS /dmo/cl_data_gen_cross_bo IMPLEMENTATION.


  METHOD /dmo/if_data_generation_badi~data_generation.
    out->write( 'Cross-BO Data Generator' ).
    out->write( '-> Travel' ).
    _travel( ).
    out->write( '-> Customer' ).
    _customer( ).
    out->write( '-> Agency' ).
    _agency( ).
  ENDMETHOD.

  METHOD _travel.
    DELETE FROM /dmo/travl_a_xbo.

    INSERT /dmo/travl_a_xbo
      FROM (
        SELECT
          FROM /dmo/travel_m
          FIELDS
            travel_id       AS travel_id            ,
            agency_id       AS agency_id            ,
            customer_id     AS customer_id          ,
            begin_date      AS begin_date           ,
            end_date        AS end_date             ,
            booking_fee     AS booking_fee          ,
            total_price     AS total_price          ,
            currency_code   AS currency_code        ,
            description     AS description          ,
            overall_status  AS overall_status       ,
            created_by      AS local_created_by     ,
            created_at      AS local_created_at     ,
            last_changed_by AS local_last_changed_by,
            last_changed_at AS local_last_changed_at,
            last_changed_at AS last_changed_at
      ).

    SELECT
      FROM /dmo/travl_a_xbo
      FIELDS MAX( travel_id )
      INTO @DATA(max).

    max += 1.

    /dmo/cl_flight_data_generator=>reset_numberrange_interval(
        numberrange_object   = '/DMO/TRVXB'
        numberrange_interval = '01'
        fromnumber           = CONV #( max )
        tonumber             = '99999999'
      ).
  ENDMETHOD.

  METHOD _customer.
    DELETE FROM /dmo/custm_a_xbo.

    INSERT /dmo/custm_a_xbo
      FROM (
        SELECT
          FROM /dmo/customer
          FIELDS
            customer_id           AS   customer_id          ,
            first_name            AS   first_name           ,
            last_name             AS   last_name            ,
            title                 AS   title                ,
            street                AS   street               ,
            postal_code           AS   postal_code          ,
            city                  AS   city                 ,
            country_code          AS   country_code         ,
            phone_number          AS   phone_number         ,
            email_address         AS   email_address        ##NULL_VALUES,
            local_created_by      AS   local_created_by     ,
            local_created_at      AS   local_created_at     ,
            local_last_changed_by AS   local_last_changed_by,
            local_last_changed_at AS   local_last_changed_at,
            last_changed_at       AS   last_changed_at
      ).

    SELECT
      FROM /dmo/custm_a_xbo
      FIELDS MAX( customer_id )
      INTO @DATA(max).

    max += 1.

    /dmo/cl_flight_data_generator=>reset_numberrange_interval(
        numberrange_object   = '/DMO/CSTXB'
        numberrange_interval = '01'
        fromnumber           = CONV #( max )
        tonumber             = '999999'
      ).
  ENDMETHOD.

  METHOD _agency.
    DELETE FROM /dmo/agncy_a_xbo.

    INSERT /dmo/agncy_a_xbo
      FROM (
        SELECT
          FROM /dmo/agency
          FIELDS
            agency_id             AS agency_id            ,
            name                  AS name                 ,
            street                AS street               ,
            postal_code           AS postal_code          ,
            city                  AS city                 ,
            country_code          AS country_code         ,
            phone_number          AS phone_number         ,
            email_address         AS email_address        ##NULL_VALUES,
            web_address           AS web_address          ##NULL_VALUES,
            local_created_by      AS local_created_by     ,
            local_created_at      AS local_created_at     ,
            local_last_changed_by AS local_last_changed_by,
            local_last_changed_at AS local_last_changed_at,
            last_changed_at       AS last_changed_at
      ).

    SELECT
      FROM /dmo/agncy_a_xbo
      FIELDS MAX( agency_id )
      INTO @DATA(max).

    max += 1.

    /dmo/cl_flight_data_generator=>reset_numberrange_interval(
        numberrange_object   = '/DMO/AGCXB'
        numberrange_interval = '01'
        fromnumber           = CONV #( max )
        tonumber             = '999999'
      ).
  ENDMETHOD.

ENDCLASS.
