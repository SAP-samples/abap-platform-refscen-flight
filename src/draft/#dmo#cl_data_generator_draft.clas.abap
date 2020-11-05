CLASS /dmo/cl_data_generator_draft DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /dmo/if_data_generation_badi .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS /dmo/cl_data_generator_draft IMPLEMENTATION.


  METHOD /dmo/if_data_generation_badi~data_generation.
    " Travels
    out->write( ' --> /DMO/A_TRAVEL_D' ).

    DELETE FROM /dmo/d_travel_d.                        "#EC CI_NOWHERE
    DELETE FROM /dmo/a_travel_d.                        "#EC CI_NOWHERE

    INSERT /dmo/a_travel_d FROM (
      SELECT FROM /dmo/travel FIELDS
        " client
        uuid( ) AS travel_uuid,
        travel_id,
        agency_id,
        customer_id,
        begin_date,
        end_date,
        booking_fee,
        total_price,
        currency_code,
        description,
        CASE status WHEN 'B' THEN 'A'
                    WHEN 'P' THEN 'O'
                    WHEN 'N' THEN 'O'
                    ELSE 'X' END AS overall_status,
        createdby AS local_created_by,
        createdat AS local_created_at,
        lastchangedby AS local_last_changed_by,
        lastchangedat AS local_last_changed_at,
        lastchangedat AS last_changed_at
    ).


    " bookings
    out->write( ' --> /DMO/A_BOOKING_D' ).

    DELETE FROM /dmo/d_booking_d.                       "#EC CI_NOWHERE
    DELETE FROM /dmo/a_booking_d.                       "#EC CI_NOWHERE

    INSERT /dmo/a_booking_d FROM (
        SELECT
          FROM /dmo/booking
            JOIN /dmo/a_travel_d ON /dmo/booking~travel_id = /dmo/a_travel_d~travel_id
            JOIN /dmo/travel ON /dmo/travel~travel_id = /dmo/booking~travel_id
          FIELDS  "client,
                  uuid( ) AS booking_uuid,
                  /dmo/a_travel_d~travel_uuid AS parent_uuid,
                  /dmo/booking~booking_id,
                  /dmo/booking~booking_date,
                  /dmo/booking~customer_id,
                  /dmo/booking~carrier_id,
                  /dmo/booking~connection_id,
                  /dmo/booking~flight_date,
                  /dmo/booking~flight_price,
                  /dmo/booking~currency_code,
                  CASE /dmo/travel~status WHEN 'P' THEN 'N'
                                                   ELSE /dmo/travel~status END AS booking_status,
                  /dmo/a_travel_d~last_changed_at AS local_last_changed_at
    ).



    " Booking supplements
    out->write( ' --> /DMO/A_BKSUPPL_D' ).

    DELETE FROM /dmo/d_bksuppl_d.                       "#EC CI_NOWHERE
    DELETE FROM /dmo/a_bksuppl_d.                       "#EC CI_NOWHERE

    INSERT /dmo/a_bksuppl_d FROM (
      SELECT FROM /dmo/book_suppl    AS supp
               JOIN /dmo/a_travel_d  AS trvl ON trvl~travel_id = supp~travel_id
               JOIN /dmo/a_booking_d AS book ON book~parent_uuid = trvl~travel_uuid
                                            AND book~booking_id = supp~booking_id

        FIELDS
          " client
          uuid( )                 AS booksuppl_uuid,
          trvl~travel_uuid        AS root_uuid,
          book~booking_uuid       AS parent_uuid,
          supp~booking_supplement_id,
          supp~supplement_id,
          supp~price,
          supp~currency_code,
          trvl~last_changed_at    AS local_last_changed_at
    ).

  ENDMETHOD.


ENDCLASS.
