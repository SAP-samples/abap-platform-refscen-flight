CLASS ltc_agency_handler DEFINITION DEFERRED FOR TESTING.
CLASS lhc_Agency DEFINITION
  INHERITING FROM cl_abap_behavior_handler
  FRIENDS ltc_agency_handler
  .

  PUBLIC SECTION.

    CONSTANTS:
      state_area_validate_attachment TYPE string VALUE 'VALIDATE_ATTACHMENT' ##NO_TEXT,
      state_area_validate_name       TYPE string VALUE 'VALIDATE_NAME'       ##NO_TEXT,
      state_area_validate_email      TYPE string VALUE 'VALIDATE_EMAIL'      ##NO_TEXT,
      state_area_validate_country    TYPE string VALUE 'VALIDATE_COUNTRY'    ##NO_TEXT.


  PRIVATE SECTION.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR /DMO/Agency RESULT result.

    METHODS validateCountryCode FOR VALIDATE ON SAVE
      IMPORTING keys FOR /DMO/Agency~/DMO/validateCountryCode.

    METHODS validateEMailAddress FOR VALIDATE ON SAVE
      IMPORTING keys FOR /DMO/Agency~/DMO/validateEMailAddress.

    METHODS validateName FOR VALIDATE ON SAVE
      IMPORTING keys FOR /DMO/Agency~/DMO/validateName.


ENDCLASS.

CLASS lhc_Agency IMPLEMENTATION.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD validateCountryCode.
    DATA: countries TYPE SORTED TABLE OF I_Country WITH UNIQUE KEY Country.

    READ ENTITIES OF /DMO/R_AgencyTP IN LOCAL MODE
      ENTITY /DMO/Agency
        FIELDS ( CountryCode )
        WITH CORRESPONDING #(  keys )
        RESULT DATA(agencies).


    countries = CORRESPONDING #( agencies DISCARDING DUPLICATES MAPPING Country = CountryCode EXCEPT * ).
    DELETE countries WHERE Country IS INITIAL.

    IF countries IS NOT INITIAL.
      SELECT FROM I_Country FIELDS Country
        FOR ALL ENTRIES IN @countries
        WHERE Country = @countries-Country
        INTO TABLE @DATA(countries_db).
    ENDIF.

    LOOP AT agencies INTO DATA(agency).
      APPEND VALUE #(
          %tky        = agency-%tky
          %state_area = state_area_validate_country
        ) TO reported-/dmo/agency.

      IF   agency-CountryCode IS INITIAL
        OR NOT line_exists( countries_db[ Country = agency-CountryCode ] ).

        APPEND VALUE #(  %tky = agency-%tky ) TO failed-/dmo/agency.
        APPEND VALUE #(
            %tky                 = agency-%tky
            %state_area          = state_area_validate_country
            %msg                 = NEW /dmo/cx_agency(
                                       textid      = /dmo/cx_agency=>country_code_invalid
                                       countrycode = agency-CountryCode
                                     )
            %element-CountryCode = if_abap_behv=>mk-on
          ) TO reported-/dmo/agency.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateEMailAddress.
    READ ENTITIES OF /DMO/R_AgencyTP IN LOCAL MODE
      ENTITY /DMO/Agency
        FIELDS ( EMailAddress )
        WITH CORRESPONDING #( keys )
        RESULT DATA(agencies).

    LOOP AT agencies INTO DATA(agency).

      APPEND VALUE #(
          %tky        = Agency-%tky
          %state_area = state_area_validate_email
        ) TO reported-/DMO/Agency.

      " Conversion to string to truncate trailing spaces, so + doesn't match space.
      IF CONV string( agency-emailaddress ) NP '+*@+*.+*'.

        APPEND VALUE #( %tky = agency-%tky ) TO failed-/DMO/Agency.

        APPEND VALUE #(
            %tky                  = agency-%tky
            %state_area           = state_area_validate_email
            %msg                  = NEW /dmo/cx_agency( /dmo/cx_agency=>email_invalid_format )
            %element-EMailaddress = if_abap_behv=>mk-on
          ) TO reported-/DMO/Agency.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateName.
    READ ENTITIES OF /DMO/R_AgencyTP IN LOCAL MODE
      ENTITY /DMO/Agency
        FIELDS ( Name )
        WITH CORRESPONDING #( keys )
        RESULT DATA(Agencies).

    LOOP AT Agencies INTO DATA(Agency).
      APPEND VALUE #(
          %tky        = Agency-%tky
          %state_area = state_area_validate_name
        ) TO reported-/DMO/Agency.

      IF Agency-Name IS INITIAL.
        APPEND VALUE #( %tky = Agency-%tky ) TO failed-/DMO/Agency.

        APPEND VALUE #(
            %tky          = Agency-%tky
            %state_area   = state_area_validate_name
            %msg          = NEW /dmo/cx_agency( /dmo/cx_agency=>name_required )
            %element-Name = if_abap_behv=>mk-on
          ) TO reported-/DMO/Agency.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_agency_saver DEFINITION DEFERRED FOR TESTING.
CLASS lsc_Agency DEFINITION
  INHERITING FROM cl_abap_behavior_saver
  FRIENDS ltc_agency_saver.
  PROTECTED SECTION.
    METHODS adjust_numbers REDEFINITION.
ENDCLASS.

CLASS lsc_agency IMPLEMENTATION.

  METHOD adjust_numbers.
    DATA:
      agency_id_max TYPE /dmo/agency_id,
      entity        TYPE STRUCTURE FOR MAPPED LATE /DMO/R_AgencyTP.

    DATA(entities_wo_agencyid) = mapped-/DMO/Agency.
    DELETE entities_wo_agencyid WHERE agencyid IS NOT INITIAL.

    IF entities_wo_agencyid IS INITIAL.
      EXIT.
    ENDIF.

    " Get Numbers
    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING
            nr_range_nr       = '01'
            object            = '/DMO/AGNCY'
            quantity          = CONV #( lines( entities_wo_agencyid  ) )
          IMPORTING
            number            = DATA(number_range_key)
            returncode        = DATA(number_range_return_code)
            returned_quantity = DATA(number_range_returned_quantity)
        ).
      CATCH cx_number_ranges INTO DATA(lx_number_ranges).
        RAISE SHORTDUMP lx_number_ranges.

    ENDTRY.

    CASE number_range_return_code.
      WHEN '1'.
        " 1 - the returned number is in a critical range (specified under “percentage warning” in the object definition)
        LOOP AT entities_wo_agencyid INTO entity.
          APPEND VALUE #(
              %pid = entity-%pid
              %key = entity-%key
              %msg = NEW /dmo/cx_agency(
                          textid   = /dmo/cx_agency=>number_range_depleted
                          severity = if_abap_behv_message=>severity-warning )
            ) TO reported-/DMO/Agency.
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
    LOOP AT mapped-/DMO/Agency ASSIGNING FIELD-SYMBOL(<agency>) ."USING KEY entity" WHERE agencyid IS INITIAL.
      IF <agency>-agencyid IS INITIAL. "If condition necessary?
        agency_id_max += 1.
        <agency>-agencyid = agency_id_max .

        " Read table mapped assign
      ENDIF.
    ENDLOOP.

    ASSERT agency_id_max = number_range_key.

  ENDMETHOD.

ENDCLASS.
