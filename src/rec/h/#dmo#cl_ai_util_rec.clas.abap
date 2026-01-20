CLASS /dmo/cl_ai_util_rec DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS get_destination_rec_values
      IMPORTING description   TYPE string
      RETURNING VALUE(result) TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS get_destinations
      RETURNING VALUE(result) TYPE string.
    CLASS-METHODS get_prompt_settings
      IMPORTING description   TYPE string
                destinations  TYPE string
      RETURNING VALUE(result) TYPE string.
    CLASS-METHODS get_completion_ai
      IMPORTING prompt_setting TYPE string
      RETURNING VALUE(result)  TYPE string.
    CLASS-METHODS get_completion_api_instance
      RETURNING VALUE(result) TYPE REF TO if_aic_completion_api.
    CLASS-METHODS get_destinations_tab
      IMPORTING destination_string TYPE string
      RETURNING VALUE(result)      TYPE string_table.
ENDCLASS.



CLASS /dmo/cl_ai_util_rec IMPLEMENTATION.

  METHOD get_destination_rec_values.
    DATA(destinations) = get_destinations( ).
    "DATA(prompt_setting) = get_prompt_settings( description = description destinations = destinations ).
    "DATA(destination_string) = get_completion_ai( prompt_setting ).

    "result = get_destinations_tab( destination_string ).
    result = VALUE string_table( ( ) ).
  ENDMETHOD.

  METHOD get_destinations.
    "Using this explicit list of countries, the AI result is improved
    SELECT FROM /dmo/c_dest_rec_h
      FIELDS CountryName
      INTO TABLE @DATA(country_names).

    LOOP AT country_names ASSIGNING FIELD-SYMBOL(<country_name>).
      result &&= <country_name>-CountryName && ', '.    "#EC CI_NOORDER
    ENDLOOP.
  ENDMETHOD.

  METHOD get_prompt_settings.
    IF description IS NOT INITIAL.
      "For security reasons you should extract this to, for example, your ISLM scenario
      result = ##NO_TEXT
               | \{ "model": "gpt-4o",| &&
               | "messages": [| &&
               | \{ "role": "system", "content": "You are a travel assistant returning destinations based on a description." \},| &&
               | \{ "role": "user", "content": "Example 1: Analyze: `Vacation in Barcelona`. Expected: Spain" \},| &&
               | \{ "role": "assistant", "content": "1" \},| &&
               | \{ "role": "user", "content": "Example 2: Analyze: `Trip to Munich`. Expected: Germany" \},| &&
               | \{ "role": "assistant", "content": "2" \},| &&
               | \{ "role": "user", "content": "Example 3: Analyze: `Business in Paris`. Expected: France" \},| &&
               | \{ "role": "assistant", "content": "3" \},| &&
               | \{ "role": "user", "content": "Example 4: Analyze: `Sports in Sydney`. Expected: Australia" \},| &&
               | \{ "role": "assistant", "content": "4" \},| &&
               | \{ "role": "user", "content": "Example 5: Analyze: `Visit to Hanover`. Expected: Germany, USA" \},| &&
               | \{ "role": "assistant", "content": "5" \},| &&
               | \{ "role": "user", "content": "Analyze the type of the following description. Respond only with the country names of the most suitable locations from the provided list. | &&
               | Do not suggest, infer, or create any location that is not explicitly present in the list.| &&
               | Your response must contain only the country names, without any explanations or additional text.| &&
               | The list is: { destinations }. Description: `{ description }`.| &&
               | The result should not contain any decoration or quotation marks. Only up to three words should be displayed." \}| &&
               | If there is more than one destination, separate them by commas." \}| &&
               | ],| &&
               | "max_tokens": 3,| &&
               | "temperature": 0.0 \}|.
    ENDIF.
  ENDMETHOD.

  METHOD get_completion_ai.
    DATA(completion_api_instance) = get_completion_api_instance( ).

    IF completion_api_instance IS BOUND.
      TRY.
          DATA(completion_api_result) = completion_api_instance->execute_for_string( prompt_setting ).
        CATCH cx_aic_completion_api.
          RETURN result.
      ENDTRY.

      result = completion_api_result->get_completion( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_completion_api_instance.
    TRY.
        result = cl_aic_islm_compl_api_factory=>get( )->create_instance( 'MyIntelligentScen' ).
      CATCH cx_aic_api_factory.
        RETURN result.
    ENDTRY.
  ENDMETHOD.

  METHOD get_destinations_tab.
    SPLIT destination_string AT `, ` INTO TABLE result.
  ENDMETHOD.

ENDCLASS.
