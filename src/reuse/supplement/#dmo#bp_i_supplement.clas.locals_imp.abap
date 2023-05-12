CLASS ltc_supplement DEFINITION DEFERRED FOR TESTING.
CLASS lhc_supplement DEFINITION
  INHERITING FROM cl_abap_behavior_handler
  FRIENDS ltc_supplement
  .

  PRIVATE SECTION.

    METHODS validateprice  FOR VALIDATE ON SAVE
      IMPORTING keys FOR supplement~validateprice.

    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE supplement.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR supplement RESULT result.



ENDCLASS.

CLASS lhc_supplement IMPLEMENTATION.

  METHOD validateprice.
    READ ENTITIES OF /dmo/i_supplement IN LOCAL MODE
      ENTITY supplement
        FIELDS ( price currencycode )
        WITH CORRESPONDING #( keys )
        RESULT DATA(supplements).


    DATA: currencies TYPE SORTED TABLE OF i_currency WITH UNIQUE KEY currency.

    currencies = CORRESPONDING #(  supplements DISCARDING DUPLICATES MAPPING currency = currencycode EXCEPT * ).
    DELETE currencies WHERE currency IS INITIAL.

    IF currencies IS NOT INITIAL.
      SELECT FROM i_currency FIELDS currency
        FOR ALL ENTRIES IN @currencies
        WHERE currency = @currencies-currency
        INTO TABLE @DATA(currency_db).
    ENDIF.


    LOOP AT supplements INTO DATA(supplement).

      APPEND VALUE #(  %tky          = supplement-%tky
                       %state_area   = 'VALIDATE_PRICE' )  TO reported-supplement.

      IF supplement-price IS INITIAL.
        " Raise message for empty Price
        APPEND VALUE #( %tky           = supplement-%tky ) TO failed-supplement.
        APPEND VALUE #( %tky           = supplement-%tky
                        %state_area    = 'VALIDATE_PRICE'
                        %msg           = NEW /dmo/cx_supplement(
                                             textid    = /dmo/cx_supplement=>price_required
                                             severity  = if_abap_behv_message=>severity-error )
                        %element-price = if_abap_behv=>mk-on
                      ) TO reported-supplement.
      ENDIF.

      IF supplement-currencycode IS INITIAL.
        " Raise message for empty Currency
        APPEND VALUE #( %tky                 = supplement-%tky ) TO failed-supplement.
        APPEND VALUE #( %tky                 = supplement-%tky
                        %state_area          = 'VALIDATE_PRICE'
                        %msg                 = NEW /dmo/cx_supplement(
                                                      textid    = /dmo/cx_supplement=>currency_required
                                                      severity  = if_abap_behv_message=>severity-error )
                        %element-currencycode = if_abap_behv=>mk-on
                      ) TO reported-supplement.
      ELSEIF NOT line_exists( currency_db[ currency = supplement-currencycode ] ).
        " Raise message for not existing Currency
        APPEND VALUE #( %tky                 = supplement-%tky ) TO failed-supplement.
        APPEND VALUE #( %tky                 = supplement-%tky
                        %state_area          = 'VALIDATE_PRICE'
                        %msg                 = NEW /dmo/cx_supplement(
                                                      textid    = /dmo/cx_supplement=>currency_not_existing
                                                      severity  = if_abap_behv_message=>severity-error )
                        %element-currencycode = if_abap_behv=>mk-on
                      ) TO reported-supplement.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD earlynumbering_create.
    DATA: entities_wo_supplementid     TYPE TABLE FOR CREATE /dmo/i_supplement,
          entities_wo_supplid_filtered TYPE TABLE FOR CREATE /dmo/i_supplement,
          entity                       TYPE STRUCTURE FOR CREATE /dmo/i_supplement.

    " Ensure Supplement ID is not set yet
    LOOP AT entities INTO entity WHERE supplementid IS NOT INITIAL.
      APPEND CORRESPONDING #( entity ) TO mapped-supplement.
    ENDLOOP.

    entities_wo_supplementid = entities.
    DELETE entities_wo_supplementid WHERE supplementid IS NOT INITIAL.
    IF entities_wo_supplementid IS INITIAL.
      EXIT.
    ENDIF.


    LOOP AT entities_wo_supplementid INTO DATA(supplement) GROUP BY supplement-supplementcategory INTO DATA(supplementcategory).
      entities_wo_supplid_filtered = entities_wo_supplementid.
      DELETE entities_wo_supplid_filtered WHERE supplementcategory <> supplementcategory.

      " Get Numbers
      TRY.
          cl_numberrange_runtime=>number_get(
            EXPORTING
              nr_range_nr       = '01'
              subobject         = CONV #( supplementcategory )
              object            = '/DMO/SUPPL'
              quantity          = CONV #( lines( entities_wo_supplid_filtered ) )
            IMPORTING
              number            = DATA(number_range_key)
              returncode        = DATA(number_range_return_code)
              returned_quantity = DATA(number_range_returned_quantity)
            ).
        CATCH cx_number_ranges INTO DATA(number_range_exception).
          LOOP AT entities_wo_supplid_filtered INTO entity.
            APPEND VALUE #(
                  %cid = entity-%cid
                  %key = entity-%key
                  %msg = number_range_exception
              ) TO reported-supplement.
            APPEND VALUE #(
                  %cid        = entity-%cid
                  %key        = entity-%key
              ) TO failed-supplement.
          ENDLOOP.
          EXIT.
      ENDTRY.

      CASE number_range_return_code.
        WHEN '1'.
          " 1 - the returned number is in a critical range (specified under “percentage warning” in the object definition)
          APPEND NEW /dmo/cx_supplement(
                              textid          = /dmo/cx_supplement=>numbers_left
                              severity        = if_abap_behv_message=>severity-warning
                              supplement_category = supplementcategory
                              numbers_left    = CONV i( '9999' - CONV i( number_range_key+2 ) )
             ) TO reported-%other.

        WHEN '2' OR '3'.
          " 2 - the last number of the interval was returned
          " 3 - if fewer numbers are available than requested,  the return code is 3
          LOOP AT entities_wo_supplid_filtered INTO entity.
            APPEND VALUE #(
                  %cid      = entity-%cid
                  %is_draft = entity-%is_draft
                  %key      = entity-%key
                  %msg      = NEW /dmo/cx_supplement(
                                    textid              = /dmo/cx_supplement=>numbers_last
                                    severity            = if_abap_behv_message=>severity-error
                                    supplement_category = supplementcategory )
              ) TO reported-supplement.
            APPEND VALUE #(
                  %cid        = entity-%cid
                  %is_draft   = entity-%is_draft
                  %key        = entity-%key
              ) TO failed-supplement.
          ENDLOOP.
          EXIT.
      ENDCASE.

      " At this point ALL entities get a number!
      ASSERT number_range_returned_quantity = lines( entities_wo_supplid_filtered ).

      " Draw lowest number
      DATA(supplement_id_max) = CONV i( number_range_key+2 ) - CONV i( number_range_returned_quantity ).

      " Draw numbers and map them
      LOOP AT entities_wo_supplid_filtered INTO entity.
        supplement_id_max += 1.

        APPEND VALUE #(
           %cid          = entity-%cid
           %is_draft     = entity-%is_draft
           supplementid  = |{ supplementcategory }-{ supplement_id_max  ALIGN = RIGHT  PAD = `0`  WIDTH = 4 }|
           ) TO mapped-supplement.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.



  METHOD get_global_authorizations.
  ENDMETHOD.

ENDCLASS.
