CLASS /dmo/cl_data_generator_managed DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES /dmo/if_data_generation_badi .
    INTERFACES if_badi_interface .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/cl_data_generator_managed IMPLEMENTATION.

  METHOD /dmo/if_data_generation_badi~data_generation.
    " Travels
    DATA lt_travel TYPE SORTED TABLE OF /dmo/travel WITH UNIQUE KEY travel_id.
    SELECT * FROM /dmo/travel INTO TABLE @lt_travel. "#EC CI_NOWHERE

    DATA lt_travel_m TYPE STANDARD TABLE OF /dmo/travel_m.
    lt_travel_m = CORRESPONDING #( lt_travel MAPPING overall_status = status
                                                     created_by = createdby
                                                     created_at = createdat
                                                     last_changed_by = lastchangedby
                                                     last_changed_at = lastchangedat ).
    " fill in some overall status.
    LOOP AT lt_travel_m ASSIGNING FIELD-SYMBOL(<travel>).
      CASE <travel>-overall_status.
        WHEN 'B'.
          " Booked -> Accepted
          <travel>-overall_status = 'A'.
        WHEN 'P' OR 'N'.
          " Planned or New -> Open
          <travel>-overall_status = 'O'.

        WHEN OTHERS.
          " Canceled
          <travel>-overall_status = 'X'.
      ENDCASE.
    ENDLOOP.

    out->write( ' --> /DMO/TRAVEL_M' ).
    DELETE FROM /dmo/travel_m.                          "#EC CI_NOWHERE
    INSERT /dmo/travel_m FROM TABLE @lt_travel_m.


    " bookings
    SELECT * FROM /dmo/booking INTO TABLE @DATA(lt_booking). "#EC CI_NOWHERE
    DATA lt_booking_m TYPE STANDARD TABLE OF /dmo/booking_m.
    lt_booking_m = CORRESPONDING #( lt_booking ).
    " copy status and last_changed_at from travels
    lt_booking_m = CORRESPONDING #( lt_booking_m FROM lt_travel USING travel_id = travel_id
                                                  MAPPING booking_status = status
                                                          last_changed_at = lastchangedat
                                                          EXCEPT * ).

    LOOP AT lt_booking_m ASSIGNING FIELD-SYMBOL(<booking>).
      IF <booking>-booking_status = 'P'.
        <booking>-booking_status = 'N'.
      ENDIF.
    ENDLOOP.

    out->write( ' --> /DMO/BOOKING_M' ).
    DELETE FROM /dmo/booking_m.                         "#EC CI_NOWHERE
    INSERT /dmo/booking_m FROM TABLE @lt_booking_m.



    " Booking supplements
    DATA lt_booksuppl_m TYPE STANDARD TABLE OF /dmo/booksuppl_m.
    SELECT * FROM /dmo/book_suppl "#EC CI_ALL_FIELDS_NEEDED
                INTO CORRESPONDING FIELDS OF TABLE @lt_booksuppl_m. "#EC CI_NOWHERE
    " copy last_changed_at from travels
    lt_booksuppl_m = CORRESPONDING #( lt_booksuppl_m FROM lt_travel USING travel_id = travel_id
                                                  MAPPING last_changed_at = lastchangedat
                                                          EXCEPT * ).

    out->write( ' --> /DMO/BOOKSUPPL_M' ).
    DELETE FROM /dmo/booksuppl_m.                       "#EC CI_NOWHERE
    INSERT /dmo/booksuppl_m FROM TABLE @lt_booksuppl_m.

  ENDMETHOD.


ENDCLASS.
