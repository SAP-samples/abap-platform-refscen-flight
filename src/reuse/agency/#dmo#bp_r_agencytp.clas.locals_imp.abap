CLASS ltc_agency_handler DEFINITION DEFERRED FOR TESTING.
CLASS lhc_agency DEFINITION
  INHERITING FROM cl_abap_behavior_handler
  FRIENDS ltc_agency_handler.

  PUBLIC SECTION.
    CONSTANTS state_area_validate_attachment TYPE string VALUE 'VALIDATE_ATTACHMENT' ##NO_TEXT.
    CONSTANTS state_area_validate_name       TYPE string VALUE 'VALIDATE_NAME'       ##NO_TEXT.
    CONSTANTS state_area_validate_email      TYPE string VALUE 'VALIDATE_EMAIL'      ##NO_TEXT.
    CONSTANTS state_area_validate_country    TYPE string VALUE 'VALIDATE_COUNTRY'    ##NO_TEXT.
    CONSTANTS state_area_validate_lob        TYPE string VALUE 'VALIDATE_LOB'     ##NO_TEXT.

    TYPES: BEGIN OF t_mimetypes,
             file_extension TYPE string,
             mimetype       TYPE string,
           END OF t_mimetypes.

    CLASS-DATA allowed_mimetypes TYPE STANDARD TABLE OF t_mimetypes WITH KEY mimetype.

    CLASS-METHODS class_constructor.

  PRIVATE SECTION.
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION IMPORTING REQUEST requested_authorizations FOR /dmo/agency RESULT result.

    METHODS validatecountrycode FOR VALIDATE ON SAVE IMPORTING keys FOR /dmo/agency~/dmo/validatecountrycode.

    METHODS validateemailaddress FOR VALIDATE ON SAVE IMPORTING keys FOR /dmo/agency~/dmo/validateemailaddress.

    METHODS validatename FOR VALIDATE ON SAVE IMPORTING keys FOR /dmo/agency~/dmo/validatename.

    METHODS validatelargeobject FOR VALIDATE ON SAVE IMPORTING keys FOR /dmo/agency~/dmo/validatelargeobject.

ENDCLASS.



CLASS lhc_agency IMPLEMENTATION.

  METHOD class_constructor.
    allowed_mimetypes = VALUE #( ( file_extension = '.txt' mimetype = 'text/plain' )
                                 ( file_extension = '.jpg' mimetype = 'image/jpeg' )
                                 ( file_extension = '.png' mimetype = 'image/png' )
                                 ( file_extension = '.bmp' mimetype = 'image/bmp' ) ).

  ENDMETHOD.


  METHOD get_global_authorizations.
  ENDMETHOD.


  METHOD validatecountrycode.
    DATA countries TYPE SORTED TABLE OF i_country WITH UNIQUE KEY country.

    READ ENTITIES OF /dmo/r_agencytp IN LOCAL MODE
         ENTITY /dmo/agency
         FIELDS ( countrycode )
         WITH CORRESPONDING #(  keys )
         RESULT DATA(agencies).


    countries = CORRESPONDING #( agencies DISCARDING DUPLICATES MAPPING country = countrycode EXCEPT * ).
    DELETE countries WHERE country IS INITIAL.

    IF countries IS NOT INITIAL.
      SELECT FROM i_country FIELDS country
        FOR ALL ENTRIES IN @countries
        WHERE country = @countries-country
        INTO TABLE @DATA(countries_db).
    ENDIF.

    LOOP AT agencies INTO DATA(agency).
      APPEND VALUE #( %tky        = agency-%tky
                      %state_area = state_area_validate_country )
             TO reported-/dmo/agency.

      IF        agency-countrycode IS INITIAL
         OR NOT line_exists( countries_db[ country = agency-countrycode ] ).

        APPEND VALUE #(  %tky = agency-%tky ) TO failed-/dmo/agency.
        APPEND VALUE #( %tky                 = agency-%tky
                        %state_area          = state_area_validate_country
                        %msg                 = NEW /dmo/cx_agency( textid      = /dmo/cx_agency=>country_code_invalid
                                                                   countrycode = agency-countrycode )
                        %element-countrycode = if_abap_behv=>mk-on )
               TO reported-/dmo/agency.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD validateemailaddress.
    READ ENTITIES OF /dmo/r_agencytp IN LOCAL MODE
         ENTITY /dmo/agency
         FIELDS ( emailaddress )
         WITH CORRESPONDING #( keys )
         RESULT DATA(agencies).

    LOOP AT agencies INTO DATA(agency).

      APPEND VALUE #( %tky        = agency-%tky
                      %state_area = state_area_validate_email )
             TO reported-/dmo/agency.

      " Conversion to string to truncate trailing spaces, so + doesn't match space.
      IF CONV string( agency-emailaddress ) NP '+*@+*.+*'.

        APPEND VALUE #( %tky = agency-%tky ) TO failed-/dmo/agency.

        APPEND VALUE #( %tky                  = agency-%tky
                        %state_area           = state_area_validate_email
                        %msg                  = NEW /dmo/cx_agency( /dmo/cx_agency=>email_invalid_format )
                        %element-emailaddress = if_abap_behv=>mk-on )
               TO reported-/dmo/agency.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD validatename.
    READ ENTITIES OF /dmo/r_agencytp IN LOCAL MODE
         ENTITY /dmo/agency
         FIELDS ( name )
         WITH CORRESPONDING #( keys )
         RESULT DATA(agencies).

    LOOP AT agencies INTO DATA(agency).
      APPEND VALUE #( %tky        = agency-%tky
                      %state_area = state_area_validate_name )
             TO reported-/dmo/agency.

      IF agency-name IS INITIAL.
        APPEND VALUE #( %tky = agency-%tky ) TO failed-/dmo/agency.

        APPEND VALUE #( %tky          = agency-%tky
                        %state_area   = state_area_validate_name
                        %msg          = NEW /dmo/cx_agency( /dmo/cx_agency=>name_required )
                        %element-name = if_abap_behv=>mk-on )
               TO reported-/dmo/agency.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD validatelargeobject.

    READ ENTITIES OF /dmo/r_agencytp IN LOCAL MODE
         ENTITY /dmo/agency
         FIELDS ( attachment mimetype filename )
         WITH CORRESPONDING #( keys )
         RESULT DATA(agencies).

    LOOP AT agencies INTO DATA(agency).

      APPEND VALUE #( %tky        = agency-%tky
                      %state_area = state_area_validate_lob ) TO reported-/dmo/agency.

      " No mimetype for attachment
      IF agency-attachment IS NOT INITIAL AND agency-mimetype IS INITIAL.
        APPEND VALUE #( %tky = agency-%tky ) TO failed-/dmo/agency.
        APPEND VALUE #( %tky              = agency-%tky
                        %state_area       = state_area_validate_lob
                        %msg              = NEW /dmo/cx_agency( textid     = /dmo/cx_agency=>mimetype_missing
                                                                attachment = agency-attachment )
                        %element-mimetype = if_abap_behv=>mk-on )
               TO reported-/dmo/agency.

      ENDIF.

      IF agency-mimetype IS NOT INITIAL.

        " No (empty) attachment for mimetype
        IF agency-attachment IS INITIAL.
          APPEND VALUE #( %tky = agency-%tky ) TO failed-/dmo/agency.
          APPEND VALUE #(
              %tky                = agency-%tky
              %state_area         = state_area_validate_lob
              %msg                = NEW /dmo/cx_agency( textid     = /dmo/cx_agency=>attachment_empty_missing
                                                        attachment = agency-attachment )
              %element-attachment = if_abap_behv=>mk-on )
                 TO reported-/dmo/agency.
        ENDIF.

        " Mimetype is not supported
        IF NOT line_exists( allowed_mimetypes[ mimetype = agency-mimetype ] ).
          APPEND VALUE #( %tky = agency-%tky ) TO failed-/dmo/agency.
          APPEND VALUE #( %tky              = agency-%tky
                          %state_area       = state_area_validate_lob
                          %msg              = NEW /dmo/cx_agency( textid   = /dmo/cx_agency=>mimetype_not_supported
                                                                  mimetype = agency-mimetype )
                          %element-mimetype = if_abap_behv=>mk-on )
                 TO reported-/dmo/agency.
        ELSE.

          IF agency-filename IS NOT INITIAL.
            DATA(file_extension) = substring_from( val = agency-filename pcre = '(.[^.]*)$' ).

            " File extension does not match mimetype
            IF file_extension <> allowed_mimetypes[ mimetype = agency-mimetype ]-file_extension.
              APPEND VALUE #( %tky = agency-%tky ) TO failed-/dmo/agency.
              APPEND VALUE #(
                  %tky              = agency-%tky
                  %state_area       = state_area_validate_lob
                  %msg              = NEW /dmo/cx_agency( textid   = /dmo/cx_agency=>extension_mimetype_mismatch
                                                          mimetype = agency-mimetype )
                  %element-mimetype = if_abap_behv=>mk-on
                  %element-filename = if_abap_behv=>mk-on )
                     TO reported-/dmo/agency.
            ENDIF.

          ENDIF.

        ENDIF.

      ELSEIF agency-attachment IS INITIAL AND agency-filename IS NOT INITIAL.
        APPEND VALUE #( %tky = agency-%tky ) TO failed-/dmo/agency.
        APPEND VALUE #( %tky                = agency-%tky
                        %state_area         = state_area_validate_lob
                        %msg                = NEW /dmo/cx_agency( textid   = /dmo/cx_agency=>only_filename
                                                                  filename = agency-filename )
                        %element-attachment = if_abap_behv=>mk-on
                        %element-mimetype   = if_abap_behv=>mk-on )
               TO reported-/dmo/agency.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS ltc_agency_saver DEFINITION DEFERRED FOR TESTING.
CLASS lsc_agency DEFINITION
  INHERITING FROM cl_abap_behavior_saver
  FRIENDS ltc_agency_saver.

  PROTECTED SECTION.
    METHODS adjust_numbers REDEFINITION.
ENDCLASS.



CLASS lsc_agency IMPLEMENTATION.

  METHOD adjust_numbers.
    DATA agency_id_max TYPE /dmo/agency_id.
    DATA entity        TYPE STRUCTURE FOR MAPPED LATE /dmo/r_agencytp.

    DATA(entities_wo_agencyid) = mapped-/dmo/agency.
    DELETE entities_wo_agencyid WHERE agencyid IS NOT INITIAL.

    IF entities_wo_agencyid IS INITIAL.
      EXIT.
    ENDIF.

    " Get Numbers
    TRY.
        cl_numberrange_runtime=>number_get( EXPORTING nr_range_nr       = '01'
                                                      object            = '/DMO/AGNCY'
                                                      quantity          = CONV #( lines( entities_wo_agencyid  ) )
                                            IMPORTING number            = DATA(number_range_key)
                                                      returncode        = DATA(number_range_return_code)
                                                      returned_quantity = DATA(number_range_returned_quantity) ).
      CATCH cx_number_ranges INTO DATA(lx_number_ranges).
        RAISE SHORTDUMP lx_number_ranges.

    ENDTRY.

    CASE number_range_return_code.
      WHEN '1'.
        " 1 - the returned number is in a critical range (specified under “percentage warning” in the object definition)
        LOOP AT entities_wo_agencyid INTO entity.
          APPEND VALUE #( %pid = entity-%pid
                          %key = entity-%key
                          %msg = NEW /dmo/cx_agency( textid   = /dmo/cx_agency=>number_range_depleted
                                                     severity = if_abap_behv_message=>severity-warning ) )
                 TO reported-/dmo/agency.
        ENDLOOP.

      WHEN '2' OR '3'.
        " 2 - the last number of the interval was returned
        " 3 - if fewer numbers are available than requested,  the return code is 3
        RAISE SHORTDUMP NEW /dmo/cx_agency( textid   = /dmo/cx_agency=>not_sufficient_numbers
                                            severity = if_abap_behv_message=>severity-warning ).
    ENDCASE.

    " At this point ALL entities get a number!
    ASSERT number_range_returned_quantity = lines( entities_wo_agencyid ).

    agency_id_max = number_range_key - number_range_returned_quantity.

    " Set Agency ID
    LOOP AT mapped-/dmo/agency ASSIGNING FIELD-SYMBOL(<agency>). " USING KEY entity" WHERE agencyid IS INITIAL.
      IF <agency>-agencyid IS INITIAL. " If condition necessary?
        agency_id_max += 1.
        <agency>-agencyid = agency_id_max.

        " Read table mapped assign
      ENDIF.
    ENDLOOP.

    ASSERT agency_id_max = number_range_key.

  ENDMETHOD.

ENDCLASS.
