CLASS lhc_Supplement DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS:
      validateprice         FOR VALIDATE ON SAVE IMPORTING keys     FOR supplement~validateprice,
      earlynumbering_create FOR NUMBERING        IMPORTING entities FOR CREATE supplement.



ENDCLASS.

CLASS lhc_Supplement IMPLEMENTATION.

  METHOD validatePrice.
    DATA: supplement TYPE STRUCTURE FOR READ RESULT /dmo/i_supplement.

    READ ENTITIES OF /dmo/i_supplement IN LOCAL MODE
      ENTITY Supplement
        FIELDS ( Price CurrencyCode )
        WITH CORRESPONDING #( keys )
        RESULT DATA(result).

    " Raise message for empty Price
    LOOP AT result INTO supplement WHERE Price IS INITIAL.
      APPEND VALUE #( %tky = supplement-%tky ) TO failed-supplement.
      APPEND VALUE #( %tky = supplement-%tky
                      %msg = NEW /dmo/cx_supplement(
                                    textid    = /dmo/cx_supplement=>price_required
                                    severity  = if_abap_behv_message=>severity-error )
                      %element-Price        = if_abap_behv=>mk-on
                    ) TO reported-supplement.
    ENDLOOP.

    " Raise message for empty Currency
    LOOP AT result INTO supplement WHERE CurrencyCode IS INITIAL.
      APPEND VALUE #( %tky = supplement-%tky ) TO failed-supplement.
      APPEND VALUE #( %tky = supplement-%tky
                      %msg = NEW /dmo/cx_supplement(
                                    textid    = /dmo/cx_supplement=>currency_required
                                    severity  = if_abap_behv_message=>severity-error )
                      %element-CurrencyCode = if_abap_behv=>mk-on
                    ) TO reported-supplement.
    ENDLOOP.
  ENDMETHOD.

  METHOD earlynumbering_create.
    DATA: entities_wo_supplementid     TYPE TABLE FOR CREATE /dmo/i_supplement,
          entities_wo_supplid_filtered TYPE TABLE FOR CREATE /dmo/i_supplement,
          entity                       TYPE STRUCTURE FOR CREATE /dmo/i_supplement.

    " Ensure Supplement ID is not set yet
    LOOP AT entities INTO entity WHERE SupplementID IS NOT INITIAL.
      APPEND CORRESPONDING #( entity ) TO mapped-supplement.
    ENDLOOP.

    entities_wo_supplementid = entities.
    DELETE entities_wo_supplementid WHERE SupplementID IS NOT INITIAL.
    CHECK entities_wo_supplementid IS NOT INITIAL.


    LOOP AT entities_wo_supplementid INTO DATA(supplement) GROUP BY supplement-SupplementCategory INTO DATA(SupplementCategory).
      entities_wo_supplid_filtered = entities_wo_supplementid.
      DELETE entities_wo_supplid_filtered WHERE SupplementCategory <> SupplementCategory.

      " Get Numbers
      TRY.
          cl_numberrange_runtime=>number_get(
            EXPORTING
              nr_range_nr       = '01'
              subobject         = CONV #( SupplementCategory )
              object            = '/DMO/SUPPL'
              quantity          = CONV #( lines( entities_wo_supplid_filtered ) )
            IMPORTING
              number            = DATA(number_range_key)
              returncode        = DATA(number_range_return_code)
              returned_quantity = DATA(number_range_returned_quantity)
            ).
        CATCH cx_number_ranges INTO DATA(lx_number_ranges).
          LOOP AT entities_wo_supplid_filtered INTO entity.
            APPEND VALUE #(
                  %cid = entity-%cid
                  %key = entity-%key
                  %msg = lx_number_ranges
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
                              supplement_category = SupplementCategory
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
                                    supplement_category = SupplementCategory )
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
           %key          = entity-%key
           %is_draft     = entity-%is_draft
           SupplementID  = |{ SupplementCategory }-{ supplement_id_max  ALIGN = RIGHT  PAD = `0`  WIDTH = 4 }|
           ) TO mapped-supplement.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
