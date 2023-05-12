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

    DATA max_travel_id TYPE /dmo/travel_id .

    " Travels
    DATA lt_travel TYPE SORTED TABLE OF /dmo/travel WITH UNIQUE KEY travel_id.
    SELECT * FROM /dmo/travel   "#EC CI_ALL_FIELDS_NEEDED
      INTO TABLE @lt_travel.    "#EC CI_NOWHERE

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

      IF <travel>-travel_id > max_travel_id.  max_travel_id = <travel>-travel_id.  ENDIF.
    ENDLOOP.

    out->write( ' --> /DMO/TRAVEL_M' ) ##NO_TEXT.
    DELETE FROM /dmo/travel_m.                          "#EC CI_NOWHERE
    INSERT /dmo/travel_m FROM TABLE @lt_travel_m.

    out->write( ' --> Set up Number Range Interval' ) ##NO_TEXT.
    CONSTANTS:
      cv_numberrange_interval TYPE cl_numberrange_runtime=>nr_interval VALUE '01',
      cv_numberrange_object   TYPE cl_numberrange_runtime=>nr_object   VALUE '/DMO/TRV_M' ##NO_TEXT,
      cv_fromnumber           TYPE cl_numberrange_intervals=>nr_nriv_line-fromnumber VALUE '00000001',
      cv_tonumber             TYPE cl_numberrange_intervals=>nr_nriv_line-tonumber   VALUE '99999999'.

    /dmo/cl_flight_data_generator=>reset_numberrange_interval(
      EXPORTING
        numberrange_object   = cv_numberrange_object
        numberrange_interval = cv_numberrange_interval
        fromnumber           = cv_fromnumber
        tonumber             = cv_tonumber
        nrlevel              = conv #( max_travel_id ) ).


    " bookings
    SELECT * FROM /dmo/booking      "#EC CI_ALL_FIELDS_NEEDED
      INTO TABLE @DATA(lt_booking). "#EC CI_NOWHERE
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

    out->write( ' --> /DMO/BOOKING_M' ) ##NO_TEXT.
    DELETE FROM /dmo/booking_m.                         "#EC CI_NOWHERE
    INSERT /dmo/booking_m FROM TABLE @lt_booking_m.



    " Booking supplements
    DATA lt_booksuppl_m TYPE STANDARD TABLE OF /dmo/booksuppl_m.
    SELECT * FROM /dmo/book_suppl             "#EC CI_ALL_FIELDS_NEEDED
                INTO CORRESPONDING FIELDS OF TABLE @lt_booksuppl_m. "#EC CI_NOWHERE
    " copy last_changed_at from travels
    lt_booksuppl_m = CORRESPONDING #( lt_booksuppl_m FROM lt_travel USING travel_id = travel_id
                                                  MAPPING last_changed_at = lastchangedat
                                                          EXCEPT * ).

    out->write( ' --> /DMO/BOOKSUPPL_M' ) ##NO_TEXT.
    DELETE FROM /dmo/booksuppl_m.                       "#EC CI_NOWHERE
    INSERT /dmo/booksuppl_m FROM TABLE @lt_booksuppl_m.

  ENDMETHOD.

ENDCLASS.
