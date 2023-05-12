CLASS /dmo/zz_cl_data_gen_agency_ext DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      if_badi_interface,
      /dmo/if_data_generation_badi.
    METHODS:
      generate_slogan,
      generate_reviews.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      out TYPE REF TO if_oo_adt_classrun_out.

ENDCLASS.



CLASS /dmo/zz_cl_data_gen_agency_ext IMPLEMENTATION.


  METHOD /dmo/if_data_generation_badi~data_generation.
    me->out = out.
    out->write( ' --> Agency Extension' ) ##NO_TEXT.
    out->write( ' --> --> Agency Slogan' ) ##NO_TEXT.
    generate_slogan( ).
    out->write( ' --> --> Agency Reviews' ) ##NO_TEXT.
    generate_reviews( ).
  ENDMETHOD.

  METHOD generate_slogan.
    lcl_slogan=>get_instance( out )->generate_slogan(
      text = VALUE #( ##NO_TEXT
          root = abap_true
          followed_by = VALUE #( ( 2 ) ( 3 ) )
          posible_end = abap_false
          ( id = 1  text = 'We are' )
          ( id = 7  text = |{ lcl_slogan=>agencys_name } is| )
          followed_by = VALUE #( ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
          ( id = 6  text = 'Fly with' )
          followed_by = VALUE #( ( 3 ) ( 4 ) ( 5 ) )
          ( id = 11  text = 'Better with' )

          root = abap_false
          posible_end = abap_true
          followed_by = VALUE #(  )
          ( id = 2  text = 'the best!' )
          ( id = 3  text = 'the leader in business!' )
          followed_by = VALUE #( ( 1 ) )
          ( id = 5  text = 'us!' )
          ( id = 4  text = |{ lcl_slogan=>agencys_name }!| )



          root = abap_true
          followed_by = VALUE #( ( 10 ) )
          posible_end = abap_false
          ( id = 8  text = 'Cool' )
          ( id = 9  text = 'Fast' )

          root = abap_false
          posible_end = abap_false
          followed_by = VALUE #( ( 12 ) ( 13 ) )
          ( id = 10  text = 'and' )

          root = abap_false
          posible_end = abap_true
          followed_by = VALUE #( ( 1 ) ( 7 ) ( 6 ) )
          ( id = 12  text = 'smooth!' )
          ( id = 13  text = 'save!' )
        )
      ).
  ENDMETHOD.

  METHOD generate_reviews.
    lcl_review_text=>set_text_graph(
        VALUE #( ##NO_TEXT
            root = abap_true
            posible_end = abap_false
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
            followed_by = VALUE #( ( 3 ) ( 4 ) ( 5 ) )
            ( id = 1  text = 'The service' )
            ( id = 2  text = 'The consulting' )

            root = abap_false
            posible_end = abap_false
            followed_by = VALUE #( ( 4 ) ( 5 ) )
            ( id = 3  text = |of { lcl_review_generator=>agencys_name }| )
            rating = VALUE #( ( 1 ) ( 5 ) )
            followed_by = VALUE #( ( 6 ) ( 7 ) ( 9 ) ( 10 ) ( 11 ) )
            ( id = 4  text = 'was really' )
            rating = VALUE #( ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
            followed_by = VALUE #( ( 6 ) ( 7 ) ( 8 ) ( 9 ) ( 10 ) ( 11 ) )
            ( id = 5  text = 'was' )

            root = abap_false
            posible_end = abap_true
            followed_by = VALUE #(  )
            rating = VALUE #( ( 1 ) ( 2 ) )
            ( id = 6  text = 'bad' )
            ( id = 7  text = 'poor' )
            rating = VALUE #( ( 3 ) )
            ( id = 8  text = 'ok' )
            rating = VALUE #( ( 4 ) ( 5 ) )
            ( id = 9   text = 'good' )
            rating = VALUE #( ( 5 ) )
            ( id = 10  text = 'awesome' )
            ( id = 11  text = 'a delight' )



            root = abap_true
            posible_end = abap_false
            followed_by = VALUE #( ( 14 ) ( 15 ) ( 20 ) ( 21 ) )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
            ( id = 12  text = 'I' )
            ( id = 13  text = 'We' )

            root = abap_false
            posible_end = abap_false
            followed_by = VALUE #( ( 16 ) ( 17 ) )
            rating = VALUE #( ( 4 ) ( 5 ) )
            ( id = 14  text = 'would recommend' )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) )
            ( id = 15  text = `wouldn't recommend` )

            root = abap_false
            posible_end = abap_true
            followed_by = VALUE #( ( 18 ) ( 19 ) )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
            ( id = 16  text = 'them' )
            ( id = 17  text = |{ lcl_review_generator=>agencys_name }| )

            root = abap_false
            posible_end = abap_true
            followed_by = VALUE #( )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
            ( id = 18  text = 'to my friends' )
            ( id = 19  text = 'to my family' )

            root = abap_false
            posible_end = abap_true
            followed_by = VALUE #( ( 22 ) ( 23 ) )
            rating = VALUE #( ( 3 ) ( 4 ) ( 5 ) )
            ( id = 20  text = 'felt welcome' )
            rating = VALUE #( ( 1 ) ( 2 ) )
            ( id = 21  text = 'felt not welcome' )

            root = abap_false
            posible_end = abap_true
            followed_by = VALUE #( ( 24 ) )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
            ( id = 22  text = 'at their office' )
            ( id = 23  text = |at { lcl_review_generator=>agencys_name }| )

            root = abap_false
            posible_end = abap_true
            followed_by = VALUE #( )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
            ( id = 24  text = |in { lcl_review_generator=>agencys_city }| )





            root = abap_true
            posible_end = abap_false
            followed_by = VALUE #( ( 27 ) ( 28 ) )
            rating = VALUE #( ( 4 ) ( 5 ) )
            ( id = 25  text = 'Easy to find' )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) )
            ( id = 26  text = 'I had issues finding' )

            root = abap_false
            posible_end = abap_false
            followed_by = VALUE #( ( 29 ) ( 30 ) ( 31 ) ( 32 ) ( 33 ) )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
            ( id = 27  text = 'their' )
            ( id = 28  text = |{ lcl_review_generator=>agencys_name }'s| )

            root = abap_false
            posible_end = abap_true
            followed_by = VALUE #( ( 34 ) )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
            ( id = 29  text = 'bureau' )
            ( id = 30  text = `office` )
            ( id = 31  text = `shop` )
            ( id = 32  text = `store` )
            ( id = 33  text = `location` )

            root = abap_false
            posible_end = abap_true
            followed_by = VALUE #( )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
            ( id = 34  text = |in { lcl_review_generator=>agencys_city }| )



            root = abap_true
            posible_end = abap_false
            followed_by = VALUE #( ( 37 ) ( 38 ) ( 39 ) ( 40 ) ( 41 ) )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
            ( id = 35  text = 'Their' )
            ( id = 36  text = |{ lcl_review_generator=>agencys_name }'s| )

            root = abap_false
            posible_end = abap_false
            followed_by = VALUE #( ( 42 ) ( 43 ) ( 44 ) ( 45 ) ( 46 ) ( 47 ) ( 48 ) ( 49 ) )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
            ( id = 37  text = 'bureau' )
            ( id = 38  text = `office` )
            ( id = 39  text = `shop` )
            ( id = 40  text = `store` )
            ( id = 41  text = `location` )

            root = abap_false
            posible_end = abap_false
            followed_by = VALUE #( ( 43 ) ( 44 ) ( 45 ) ( 46 ) ( 47 ) ( 48 ) ( 49 ) )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) )
            ( id = 42  text = |in { lcl_review_generator=>agencys_city }| )

            root = abap_false
            posible_end = abap_true
            followed_by = VALUE #( )
            rating = VALUE #( ( 1 ) ( 2 ) )
            ( id = 43  text = 'was rusty' )
            ( id = 44  text = `was old and ugly` )
            rating = VALUE #( ( 1 ) )
            ( id = 45  text = `was sticky` )
            rating = VALUE #( ( 1 ) ( 2 ) ( 3 ) )
            ( id = 46  text = `was not clean` )
            rating = VALUE #( ( 5 ) )
            ( id = 47  text = `was beautiful` )
            ( id = 48  text = `was state of the art` )
            rating = VALUE #( ( 3 ) ( 4 ) ( 5 ) )
            ( id = 49  text = `was clean` )
          )
      ).

    lcl_review_generator=>get_instance( out )->generate_reviews( ).
  ENDMETHOD.

ENDCLASS.
