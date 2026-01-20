CLASS lhc_r_traveltp_rec DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    TYPES: BEGIN OF rec_struc,
             value     TYPE string,
             score     TYPE int1,
             suggested TYPE abap_bool,
           END OF rec_struc.
    TYPES rec_tab TYPE TABLE OF rec_struc WITH DEFAULT KEY.
    TYPES country_tab TYPE TABLE OF /dmo/c_dest_rec_h WITH DEFAULT KEY.
    TYPES business_user_travel TYPE TABLE FOR READ RESULT /dmo/r_traveltp_rec\\travel.
    METHODS GetRecommendedValuesFunction FOR READ
      IMPORTING keys FOR FUNCTION Travel~GetRecommendedValuesFunction RESULT result.
    METHODS get_accommodation_names
      IMPORTING business_user_travel TYPE business_user_travel
      RETURNING VALUE(result)        TYPE string_table.
    METHODS get_country_names
      RETURNING VALUE(result) TYPE country_tab.
    METHODS get_rec_values
      IMPORTING rec_values    TYPE string_table
      RETURNING VALUE(result) TYPE rec_tab.
ENDCLASS.

CLASS lhc_r_traveltp_rec IMPLEMENTATION.

  METHOD GetRecommendedValuesFunction.
    READ ENTITIES OF /dmo/r_traveltp_rec IN LOCAL MODE
       ENTITY Travel
       FIELDS ( Description Destination ) WITH CORRESPONDING #( keys )
       RESULT DATA(business_user_travel)
       FAILED failed.

    "Selects all available country names from the value help
    DATA(country_names) = get_country_names( ).

    "Selects all accommodations that match the business user's input for the destination for the deterministic approach
    DATA(accommodation_names) = get_accommodation_names( business_user_travel ).

    "Prepare recommendation values for accommodations
    DATA(accommodation_rec_values) = get_rec_values( accommodation_names ).

    LOOP AT business_user_travel ASSIGNING FIELD-SYMBOL(<travel>).

      "Based on the business user's input for the description, the AI should suggest destinations
      DATA(destinations) = /dmo/cl_ai_util_rec=>get_destination_rec_values( description = <travel>-Description ).

      "Deletes destinations the AI suggested, but that are not actual countries
      LOOP AT destinations ASSIGNING FIELD-SYMBOL(<dest>).
        IF NOT line_exists( country_names[ CountryName = <dest> ] ).
          DELETE destinations FROM sy-tabix.
        ENDIF.
      ENDLOOP.

      "Prepare recommendation values for destinations
      DATA(destination_rec_values) = get_rec_values( destinations ).

      "Return recommended values for destination and accommodation
      INSERT VALUE #(  %tky = <travel>-%tky
                       %param-travel = VALUE #( destination = CORRESPONDING #( destination_rec_values )
                                                accommodation = CORRESPONDING #( accommodation_rec_values ) ) ) INTO TABLE result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_country_names.
    SELECT FROM /dmo/c_dest_rec_h
      FIELDS CountryName
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.

  METHOD get_accommodation_names.
    IF lines( business_user_travel ) IS INITIAL.
      RETURN.
    ENDIF.

    SELECT DISTINCT acc_name
          FROM /dmo/acc_rec_h AS accommodation
          INNER JOIN @business_user_travel AS destination
          ON accommodation~acc_country = destination~Destination
        INTO TABLE @result.
  ENDMETHOD.

  METHOD get_rec_values.
    IF rec_values IS INITIAL.
      RETURN.
    ENDIF.

    DATA(counter) = 0.

    LOOP AT rec_values ASSIGNING FIELD-SYMBOL(<rec_values>) TO 3. "#EC CI_NOORDER
      counter += 1.

      INSERT VALUE #( value = <rec_values>
                      score = 100 - counter
                      suggested = COND #( WHEN counter = 1
                                          THEN abap_true
                                          ELSE abap_false ) ) INTO TABLE result.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
