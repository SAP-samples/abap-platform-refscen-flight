* Hierarchy:
* Abstract/Agency-Properties
* -> Slogan
* -> Review Generator (based Average)
* Review
* Text

* Dependency:
* Review Generator
* uses Review
*      uses Text


CLASS lcl_abstract DEFINITION ABSTRACT CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      agencys_name TYPE string VALUE '$self',
      agencys_city TYPE string VALUE '$city'.

  PROTECTED SECTION.
    TYPES:
      ts_agency_with_indicators TYPE /dmo/agency WITH INDICATORS control.

    CLASS-DATA:
      out       TYPE REF TO if_oo_adt_classrun_out.

    CLASS-METHODS:
      _initialize.

    CLASS-DATA:
      gt_agency TYPE STANDARD TABLE OF ts_agency_with_indicators WITH KEY agency_id,
      go_rnd    TYPE REF TO cl_abap_random_float.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_abstract IMPLEMENTATION.

  METHOD _initialize.
    SELECT
      FROM /dmo/agency
      FIELDS agency_id, name, city
      INTO CORRESPONDING FIELDS OF TABLE @gt_agency.    "#EC CI_NOWHERE

    go_rnd = cl_abap_random_float=>create( ).
  ENDMETHOD.

ENDCLASS.





CLASS lcl_slogan DEFINITION
  CREATE PUBLIC
  INHERITING FROM lcl_abstract.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_slogan,
        id          TYPE i,
        text        TYPE /dmo/zz_free_text_comment,
        followed_by TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
        root        TYPE abap_bool,
        posible_end TYPE abap_bool,
      END OF t_slogan,
      tt_slogan TYPE SORTED TABLE OF t_slogan
                       WITH UNIQUE KEY primary_key COMPONENTS id
                       WITH NON-UNIQUE SORTED KEY filter COMPONENTS root.

    CLASS-METHODS:
      get_instance
        IMPORTING
          io_adt_classrun_out TYPE REF TO if_oo_adt_classrun_out
        RETURNING
          VALUE(ro_slogan)    TYPE REF TO lcl_slogan.

    METHODS:
      generate_slogan
        IMPORTING
          text TYPE tt_slogan.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      go_slogan TYPE REF TO lcl_slogan.

    DATA:
      mt_text TYPE tt_slogan.

    METHODS:
      _slogan_iterate.

ENDCLASS.

CLASS lcl_slogan IMPLEMENTATION.

  METHOD get_instance.
    IF go_slogan IS NOT BOUND.
      go_slogan = NEW #( ).
    ENDIF.
    out = io_adt_classrun_out.

    ro_slogan = go_slogan.
  ENDMETHOD.

  METHOD generate_slogan.
    IF gt_agency IS INITIAL.
      _initialize( ).
    ENDIF.

    mt_text = text.

    _slogan_iterate( ).

    UPDATE /dmo/agency FROM TABLE @gt_agency INDICATORS SET STRUCTURE control.
  ENDMETHOD.

  METHOD _slogan_iterate.
    DATA:
      id        TYPE i.

    DATA(roots) = FILTER tt_slogan( mt_text USING KEY filter WHERE root = abap_true ).
    LOOP AT gt_agency ASSIGNING FIELD-SYMBOL(<agency>).
      <agency>-control-/dmo/zzsloganzag = if_abap_behv=>mk-on.
      DATA(next) = roots[ floor( go_rnd->get_next( ) * lines( roots ) ) + 1 ].
      <agency>-/dmo/zzsloganzag = next-text.
      WHILE next-followed_by IS NOT INITIAL.
        IF next-posible_end = abap_true AND go_rnd->get_next( ) > '0.6'.
          EXIT.
        ENDIF.
        id = floor( go_rnd->get_next( ) * lines( next-followed_by ) ) + 1.
        next = mt_text[ id = next-followed_by[ id ] ].
        <agency>-/dmo/zzsloganzag &&= ` ` && next-text.
      ENDWHILE.
      REPLACE ALL OCCURRENCES OF agencys_name IN <agency>-/dmo/zzsloganzag WITH <agency>-name.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.





CLASS lcl_review_generator DEFINITION
  CREATE PUBLIC
  INHERITING FROM lcl_abstract.

  PUBLIC SECTION.

    TYPES:
      tt_review                 TYPE STANDARD TABLE OF /dmo/zz_agn_reva WITH KEY agency_id review_id.




    CLASS-METHODS:
      get_instance
        IMPORTING
          io_adt_classrun_out TYPE REF TO if_oo_adt_classrun_out
        RETURNING
          VALUE(ro_reviews)   TYPE REF TO lcl_review_generator.

    METHODS:
      generate_reviews.

  PROTECTED SECTION.

    CONSTANTS:
      cv_reviews_min           TYPE i VALUE 50,
      cv_reviews_max           TYPE i VALUE 500,

      "No agencies with 1 or 2 star rating
      "Percentage of 5 star is 1 - %3Star - %4Star.
      cv_reviews_avg_3_percent TYPE f VALUE '0.2',
      cv_reviews_avg_4_percent TYPE f VALUE '0.35'.


    DATA:
      mt_reviews_to_create TYPE tt_review.

  PRIVATE SECTION.


    CLASS-DATA:
      go_reviews TYPE REF TO lcl_review_generator.


    METHODS:
      _review_iterate,
      _get_review_total_per_agency
        RETURNING
          VALUE(rv_review_amount) TYPE i,
      _get_review_rating_avg
        RETURNING
          VALUE(rv_review_rating_average) TYPE /dmo/zz_rating.


ENDCLASS.



CLASS lcl_review DEFINITION
CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.

    METHODS:
      constructor
        IMPORTING
          is_agency         TYPE /dmo/agency
          iv_rating_average TYPE /dmo/zz_rating,
      generate
        RETURNING
          VALUE(rs_review) TYPE /dmo/zz_agn_reva.

  PROTECTED SECTION.
    DATA:
      mv_rating_average TYPE /dmo/zz_rating.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_id_map,
        agency_id TYPE /dmo/agency_id,
        review_id TYPE /dmo/zz_review_id,
      END OF ts_id_map,
      tt_id_map TYPE STANDARD TABLE OF ts_id_map WITH KEY agency_id.

    CONSTANTS:
      cv_helpful_min         TYPE i VALUE 100,
      cv_helpful_max         TYPE i VALUE 800,
      cv_helpful_percent_min TYPE f VALUE '0.7',
      cv_helpful_percent_max TYPE f VALUE '1.0'.

    METHODS:
      _get_review_rating
        IMPORTING
          iv_target_rating        TYPE /dmo/zz_rating
        RETURNING
          VALUE(rv_review_rating) TYPE /dmo/zz_rating,
      _calculate_helpfulness.

    CLASS-DATA:
      gt_id_map TYPE tt_id_map,
      go_rnd    TYPE REF TO cl_abap_random_float.

    DATA:
      ms_agency TYPE /dmo/agency,
      ms_review TYPE /dmo/zz_agn_reva.
ENDCLASS.




CLASS lcl_review_text DEFINITION
CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_review_text,
        id          TYPE i,
        rating      TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line,
        text        TYPE /dmo/zz_free_text_comment,
        followed_by TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
        root        TYPE abap_bool,
        posible_end TYPE abap_bool,
      END OF t_review_text,
      tt_review_text TYPE SORTED TABLE OF t_review_text WITH UNIQUE KEY primary_key COMPONENTS id
                                          WITH NON-UNIQUE SORTED KEY root COMPONENTS root,
      BEGIN OF t_review_text_edges,
        id                 TYPE i,
        followed_by_rating TYPE /dmo/zz_rating,
        followed_by        TYPE i,
      END OF t_review_text_edges,
      tt_review_text_edges TYPE SORTED TABLE OF t_review_text_edges WITH NON-UNIQUE KEY primary_key COMPONENTS id followed_by_rating.

    CLASS-METHODS:
      set_text_graph
        IMPORTING
          it_text TYPE tt_review_text,

      get_instance_for_rating
        IMPORTING
          iv_rating             TYPE /dmo/zz_rating
        RETURNING
          VALUE(ro_review_text) TYPE REF TO lcl_review_text.

    METHODS:
      generate_text_for_agency
        IMPORTING
          is_agency      TYPE /dmo/agency
        RETURNING
          VALUE(rv_text) TYPE /dmo/zz_free_text_comment.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      cv_propable_stop_when_possible TYPE f VALUE '0.3',
      cv_propable_multiple_roots     TYPE f VALUE '0.6'.

    TYPES:
      BEGIN OF ts_roots_by_rating,
        rating TYPE /dmo/zz_rating,
        roots  TYPE tt_review_text,
      END OF ts_roots_by_rating,
      tt_roots_by_rating TYPE STANDARD TABLE OF ts_roots_by_rating WITH KEY rating.

    CLASS-DATA:
      go_rnd             TYPE REF TO cl_abap_random_float,
      mt_text            TYPE tt_review_text,
      mt_edges           TYPE tt_review_text_edges,
      mt_roots_by_rating TYPE tt_roots_by_rating.


    CLASS-METHODS:
      _calculate_possible_edges,
      _calculate_roots_by_rating.

    DATA:
      mv_rating       TYPE /dmo/zz_rating,
      ms_agency       TYPE /dmo/agency,
      mv_text         TYPE /dmo/zz_free_text_comment,
      mt_root_visited TYPE STANDARD TABLE OF i.

    METHODS:
      constructor
        IMPORTING
          iv_rating TYPE /dmo/zz_rating,
      _replace_wildcards,
      _get_next
        IMPORTING
          current     TYPE lcl_review_text=>t_review_text
        RETURNING
          VALUE(next) TYPE lcl_review_text=>t_review_text,
      _get_new_root
        RETURNING
          VALUE(next) TYPE lcl_review_text=>t_review_text,
      _get_followed
        IMPORTING
          current     TYPE lcl_review_text=>t_review_text
        RETURNING
          VALUE(next) TYPE lcl_review_text=>t_review_text,
      _iterate_textblocks.
ENDCLASS.








CLASS lcl_review_generator IMPLEMENTATION.

  METHOD get_instance.
    IF go_reviews IS NOT BOUND.
      go_reviews = NEW #( ).
    ENDIF.
    out = io_adt_classrun_out.

    ro_reviews = go_reviews.
  ENDMETHOD.

  METHOD generate_reviews.
    IF gt_agency IS INITIAL.
      _initialize( ).
    ENDIF.

    _review_iterate( ).

    DELETE FROM /dmo/zz_agn_reva.                       "#EC CI_NOWHERE

    INSERT /dmo/zz_agn_reva FROM TABLE @mt_reviews_to_create.

  ENDMETHOD.

  METHOD _review_iterate.
    LOOP AT gt_agency ASSIGNING FIELD-SYMBOL(<agency>).
      DATA(lv_rating_average) = _get_review_rating_avg( ).

      DO _get_review_total_per_agency( ) TIMES.
        DATA(review) = NEW lcl_review(
            is_agency         = CORRESPONDING #( <agency> )
            iv_rating_average = lv_rating_average
          ).

        INSERT review->generate( ) INTO TABLE mt_reviews_to_create.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

  METHOD _get_review_total_per_agency.
    rv_review_amount = floor(
        go_rnd->get_next( ) * ( cv_reviews_max - cv_reviews_min ) + cv_reviews_min
      ).
  ENDMETHOD.

  METHOD _get_review_rating_avg.
    rv_review_rating_average = COND i(
        LET rnd = go_rnd->get_next( ) IN
        WHEN rnd < cv_reviews_avg_3_percent                            THEN 3
        WHEN rnd < cv_reviews_avg_3_percent + cv_reviews_avg_4_percent THEN 4
        ELSE 5
      ).
  ENDMETHOD.


ENDCLASS.







CLASS lcl_review IMPLEMENTATION.

  METHOD class_constructor.
    go_rnd = cl_abap_random_float=>create( ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    mv_rating_average = iv_rating_average.
    ms_agency = is_agency.

    IF line_exists( gt_id_map[ agency_id = ms_agency-agency_id ] ).
      ASSIGN gt_id_map[ agency_id = ms_agency-agency_id ]-review_id TO FIELD-SYMBOL(<review_id>).
      <review_id> += 1.
    ELSE.
      APPEND VALUE ts_id_map(
          agency_id = ms_agency-agency_id
          review_id = 1
        ) TO gt_id_map.
    ENDIF.
  ENDMETHOD.


  METHOD generate.
    GET TIME STAMP FIELD DATA(current_timestamp).

    ms_review = VALUE #(
        agency_id             = ms_agency-agency_id
        review_id             = gt_id_map[ agency_id = ms_agency-agency_id ]-review_id
        reviewer              = 'GENERATOR'
        local_created_at      = current_timestamp
        local_last_changed_at = current_timestamp
      ).

    ms_review-rating = _get_review_rating( mv_rating_average ).

    ms_review-free_text_comment = lcl_review_text=>get_instance_for_rating( ms_review-rating
                                    )->generate_text_for_agency( ms_agency ).

    _calculate_helpfulness( ).

    rs_review = ms_review.
  ENDMETHOD.

  METHOD _get_review_rating.
    " Target | % of 1 | % of 2 | % of 3 | % of 4 | % of 5 | AVG
    "--------+--------+--------+--------+--------+--------+-----
    " 3      |  0.1   |  0.15  |  0.4   |  0.25  |  0.1   |  3.1
    " 4      |  0.05  |  0.05  |  0.1   |  0.45  |  0.35  |  4
    " 5      |  0     |  0.01  |  0.05  |  0.1   |  0.84  |  4.77

    DATA:
      lv_percent_of_1 TYPE f,
      lv_percent_of_2 TYPE f,
      lv_percent_of_3 TYPE f,
      lv_percent_of_4 TYPE f.

    IF iv_target_rating = 3.
      lv_percent_of_1 = '0.1'.
      lv_percent_of_2 = '0.15'.
      lv_percent_of_3 = '0.4'.
      lv_percent_of_4 = '0.25'.
    ELSEIF iv_target_rating = 4.
      lv_percent_of_1 = '0.05'.
      lv_percent_of_2 = '0.05'.
      lv_percent_of_3 = '0.1'.
      lv_percent_of_4 = '0.45'.
    ELSEIF iv_target_rating = 5.
      lv_percent_of_1 = '0'.
      lv_percent_of_2 = '0.01'.
      lv_percent_of_3 = '0.05'.
      lv_percent_of_4 = '0.1'..
    ELSE.
      ASSERT 1 = 0.
    ENDIF.

    rv_review_rating = COND i(
        LET rnd = go_rnd->get_next( ) IN
        WHEN rnd < lv_percent_of_1
          THEN 1
        WHEN rnd < lv_percent_of_1 + lv_percent_of_2
          THEN 2
        WHEN rnd < lv_percent_of_1 + lv_percent_of_2
          THEN 2
        WHEN rnd < lv_percent_of_1 + lv_percent_of_2 + lv_percent_of_3
          THEN 3
        WHEN rnd < lv_percent_of_1 + lv_percent_of_2 + lv_percent_of_3 + lv_percent_of_4
          THEN 4
        ELSE 5
      ).
  ENDMETHOD.



  METHOD _calculate_helpfulness.
    ms_review-helpful_total = floor( go_rnd->get_next( ) * ( cv_helpful_max - cv_helpful_min ) + cv_helpful_min ).
    DATA(lv_helpful_count_min) = CONV i( ms_review-helpful_total * ( 1 - ( abs( mv_rating_average - ms_review-rating ) + 1 ) * '0.2' ) ).
    DATA(lv_helpful_count_max) = CONV i( ms_review-helpful_total * ( 1 - ( abs( mv_rating_average - ms_review-rating )     ) * '0.2' ) ).
    ms_review-helpful_count = floor( go_rnd->get_next( ) * ( lv_helpful_count_max - lv_helpful_count_min ) + lv_helpful_count_min ).
  ENDMETHOD.

ENDCLASS.



CLASS lcl_review_text IMPLEMENTATION.

  METHOD set_text_graph.
    mt_text = it_text.

    _calculate_possible_edges( ).

    _calculate_roots_by_rating( ).
  ENDMETHOD.

  METHOD _calculate_possible_edges.
    mt_edges = VALUE tt_review_text_edges(
        FOR txt IN mt_text
        FOR f IN txt-followed_by
        FOR r IN mt_text[ id = f ]-rating
        (
          id          = txt-id
          followed_by = f
          followed_by_rating = r
        )
      ).
  ENDMETHOD.


  METHOD _calculate_roots_by_rating.
    mt_roots_by_rating = VALUE tt_roots_by_rating(
        ( rating = 1 )
        ( rating = 2 )
        ( rating = 3 )
        ( rating = 4 )
        ( rating = 5 )
      ).

    DATA(lt_roots) = FILTER tt_review_text( mt_text USING KEY root WHERE root = abap_true ).
    LOOP AT lt_roots INTO DATA(ls_root).
      LOOP AT ls_root-rating INTO DATA(lv_root_rating).
        ASSIGN mt_roots_by_rating[ rating = lv_root_rating ]-roots TO FIELD-SYMBOL(<roots_by_rating>).
        INSERT ls_root INTO TABLE <roots_by_rating>.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD generate_text_for_agency.
    DATA: id TYPE i.

    ms_agency = is_agency.

    _iterate_textblocks( ).

    _replace_wildcards( ).

    rv_text = mv_text.
  ENDMETHOD.

  METHOD get_instance_for_rating.
    IF go_rnd IS NOT BOUND.
      go_rnd = cl_abap_random_float=>create( ).
    ENDIF.

    ro_review_text = NEW lcl_review_text( iv_rating ).
  ENDMETHOD.

  METHOD constructor.
    mv_rating = iv_rating.
  ENDMETHOD.


  METHOD _replace_wildcards.
    REPLACE ALL OCCURRENCES OF lcl_review_generator=>agencys_name IN mv_text WITH ms_agency-name.
    REPLACE ALL OCCURRENCES OF lcl_review_generator=>agencys_city IN mv_text WITH ms_agency-city.
  ENDMETHOD.


  METHOD _get_next.
    IF current-followed_by IS INITIAL.
      IF go_rnd->get_next( ) < cv_propable_multiple_roots.
        next = _get_new_root( ).
        mv_text &&= `. `.
      ENDIF.
    ELSE.
      next = _get_followed( current ).
    ENDIF.
  ENDMETHOD.


  METHOD _get_new_root.
    DATA(lt_posible_next_root) = mt_roots_by_rating[ rating = mv_rating ]-roots.

    LOOP AT mt_root_visited INTO DATA(lv_root_visited).
      DELETE lt_posible_next_root WHERE id = lv_root_visited.
    ENDLOOP.

    IF lt_posible_next_root IS NOT INITIAL.
      next = lt_posible_next_root[ floor( go_rnd->get_next( ) * lines( lt_posible_next_root ) ) + 1 ].
      APPEND next-id TO mt_root_visited.
    ENDIF.
  ENDMETHOD.


  METHOD _get_followed.
    DATA:
      id TYPE i.

    DATA(lt_posible_next) = FILTER tt_review_text_edges( mt_edges WHERE id = current-id AND followed_by_rating = mv_rating ).
    id = floor( go_rnd->get_next( ) * lines( lt_posible_next ) ) + 1.
    next = mt_text[ id = lt_posible_next[ id ]-followed_by ].
  ENDMETHOD.


  METHOD _iterate_textblocks.
    DATA(next) = _get_new_root( ).
    mv_text = next-text.

    DO.
      IF next-posible_end = abap_true AND go_rnd->get_next( ) < cv_propable_stop_when_possible.
        EXIT.
      ENDIF.

      next = _get_next( next ).
      IF next IS INITIAL.
        EXIT.
      ENDIF.

      mv_text &&= ` ` && next-text.
    ENDDO.

    mv_text &&= `.`.
  ENDMETHOD.

ENDCLASS.
