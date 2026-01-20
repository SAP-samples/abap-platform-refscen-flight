CLASS acc_data_generator DEFINITION INHERITING FROM /dmo/cl_abstract_data_gen CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES accommodation_type TYPE STANDARD TABLE OF /dmo/acc_rec_h WITH KEY acc_id.

    CLASS-METHODS get_instance
      RETURNING VALUE(instance) TYPE REF TO acc_data_generator.
    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA acc_generator_instance TYPE REF TO acc_data_generator.

    CLASS-METHODS prepare_skeleton
      RETURNING VALUE(accommodations) TYPE accommodation_type.
ENDCLASS.

CLASS acc_data_generator IMPLEMENTATION.
  METHOD constructor.
    DATA(acc_skeleton) = prepare_skeleton( ).

    DATA(features_acc) = VALUE feature_structure( with_uuid        = abap_true ).

    DATA(field_mapping) = VALUE field_structure( uuid = 'acc_id' ).

    super->constructor( skeleton_data        = REF #( acc_skeleton )
                        scenario_name        = 'Accommodations for RAP Recommendations' ##NO_TEXT
                        package_name         = '/DMO/FLIGHT_REC_H'
                        table_name_active    = '/DMO/ACC_REC_H'
                        features             = features_acc
                        fields               = field_mapping
                      ).
  ENDMETHOD.

  METHOD get_instance.
    IF acc_generator_instance IS NOT BOUND.
      acc_generator_instance = NEW acc_data_generator( ).
    ENDIF.

    RETURN acc_generator_instance.
  ENDMETHOD.

  METHOD prepare_skeleton.
    accommodations = VALUE #( ##NO_TEXT
                              ( acc_name = 'Sunshine Hotel'
                                acc_type = 'Hotel'
                                acc_address = '123 Sunny Lane'
                                acc_postal_code = '12345'
                                acc_city = 'Miami'
                                acc_country = 'USA'
                                acc_phone = '+1 305 1234567'
                                acc_email = 'info@sunshinehotel.com'
                                acc_website = 'www.sunshinehotel.com' )
                              ( acc_name = 'Paradise Resort'
                                acc_type = 'Resort'
                                acc_address = '789 Beach Avenue'
                                acc_postal_code = '24680'
                                acc_city = 'Hawaii'
                                acc_country = 'USA'
                                acc_phone = '+1 808 2468012'
                                acc_email = 'info@paradiseresort.com'
                                acc_website = 'www.paradiseresort.com' )
                              ( acc_name = 'Eiffel View Hotel'
                                acc_type = 'Hotel'
                                acc_address = '123 Rue de la Paix'
                                acc_postal_code = '75001'
                                acc_city = 'Paris'
                                acc_country = 'France'
                                acc_phone = '+33 1 1234567'
                                acc_email = 'info@eiffelviewhotel.fr'
                                acc_website = 'www.eiffelviewhotel.fr' )
                              ( acc_name = 'Tuscany Villa'
                                acc_type = 'Villa'
                                acc_address = '456 Via dei Fiori'
                                acc_postal_code = '50123'
                                acc_city = 'Florence'
                                acc_country = 'Italy'
                                acc_phone = '+39 055 123456'
                                acc_email = 'contact@tuscanyvilla.it'
                                acc_website = 'www.tuscanyvilla.it' )
                              ( acc_name = 'Sydney Harbor View'
                                acc_type = 'Hotel'
                                acc_address = '789 Harbour Street'
                                acc_postal_code = '2000'
                                acc_city = 'Sydney'
                                acc_country = 'Australia'
                                acc_phone = '+61 2 1234567'
                                acc_email = 'info@sydneyharborview.com.au'
                                acc_website = 'www.sydneyharborview.com.au' )
                              ( acc_name = 'Kyoto Traditional Inn'
                                acc_type = 'Inn'
                                acc_address = '123 Higashiyama Ward'
                                acc_postal_code = '605-0805'
                                acc_city = 'Kyoto'
                                acc_country = 'Japan'
                                acc_phone = '+81 75 1234567'
                                acc_email = 'info@kyototraditionalinn.jp'
                                acc_website = 'www.kyototraditionalinn.jp' )
                              ( acc_name = 'Berlin Central Hotel'
                                acc_type = 'Hotel'
                                acc_address = '456 Alexanderplatz'
                                acc_postal_code = '10178'
                                acc_city = 'Berlin'
                                acc_country = 'Germany'
                                acc_phone = '+49 30 1234567'
                                acc_email = 'contact@berlincentralhotel.de'
                                acc_website = 'www.berlincentralhotel.de' )
                              ( acc_name = 'Amsterdam Canal View'
                                acc_type = 'Hotel'
                                acc_address = '789 Prinsengracht'
                                acc_postal_code = '1016HH'
                                acc_city = 'Amsterdam'
                                acc_country = 'Netherlands'
                                acc_phone = '+31 20 1234567'
                                acc_email = 'info@amsterdamcanalview.nl'
                                acc_website = 'www.amsterdamcanalview.nl' )
                              ( acc_name = 'Singapore Marina Bay'
                                acc_type = 'Hotel'
                                acc_address = '123 Marina Boulevard'
                                acc_postal_code = '018951'
                                acc_city = 'Singapore'
                                acc_country = 'Singapore'
                                acc_phone = '+65 61234567'
                                acc_email = 'contact@singaporemarinabay.com'
                                acc_website = 'www.singaporemarinabay.com' )
                              ( acc_name = 'Barcelona Beachfront'
                                acc_type = 'Hotel'
                                acc_address = '456 Passeig Marítim' ##STRING_OK
                                acc_postal_code = '08003'
                                acc_city = 'Barcelona'
                                acc_country = 'Spain'
                                acc_phone = '+34 93 1234567'
                                acc_email = 'info@barcelonabeachfront.es'
                                acc_website = 'www.barcelonabeachfront.es' )
                              ( acc_name = 'Cape Town Luxury Lodge'
                                acc_type = 'Lodge'
                                acc_address = '789 Victoria Road'
                                acc_postal_code = '8005'
                                acc_city = 'Cape Town'
                                acc_country = 'South Africa'
                                acc_phone = '+27 21 1234567'
                                acc_email = 'contact@capetownluxurylodge.co.za'
                                acc_website = 'www.capetownluxurylodge.co.za' )
                              ( acc_name = 'Bangkok Grand Palace'
                                acc_type = 'Hotel'
                                acc_address = '123 Ratchadamnoen Avenue'
                                acc_postal_code = '10200'
                                acc_city = 'Bangkok'
                                acc_country = 'Thailand'
                                acc_phone = '+66 2 1234567'
                                acc_email = 'info@bangkokgrandpalace.com'
                                acc_website = 'www.bangkokgrandpalace.com' )
                              ( acc_name = 'Rio de Janeiro Beach Resort'
                                acc_type = 'Resort'
                                acc_address = '456 Avenida Atlântica' ##STRING_OK
                                acc_postal_code = '22010-000'
                                acc_city = 'Rio de Janeiro'
                                acc_country = 'Brazil'
                                acc_phone = '+55 21 1234567'
                                acc_email = 'contact@riodejaneirobeachresort.com.br'
                                acc_website = 'www.riodejaneirobeachresort.com.br' )
                              ( acc_name = 'Marrakech Royal Palace'
                                acc_type = 'Hotel'
                                acc_address = '789 Avenue Mohammed V'
                                acc_postal_code = '40000'
                                acc_city = 'Marrakech'
                                acc_country = 'Morocco'
                                acc_phone = '+212 524 123456'
                                acc_email = 'info@marrakechroyalpalace.com'
                                acc_website = 'www.marrakechroyalpalace.com' )
                              ( acc_name = 'Dubai Luxury Tower'
                                acc_type = 'Hotel'
                                acc_address = '123 Sheikh Zayed Road'
                                acc_postal_code = '123456'
                                acc_city = 'Dubai'
                                acc_country = 'UAE'
                                acc_phone = '+971 4 1234567'
                                acc_email = 'contact@dubailuxurytower.ae'
                                acc_website = 'www.dubailuxurytower.ae' )
                              ( acc_name = 'Istanbul Historic Hotel'
                                acc_type = 'Hotel'
                                acc_address = '456 Sultanahmet Square'
                                acc_postal_code = '34122'
                                acc_city = 'Istanbul'
                                acc_country = 'Turkey'
                                acc_phone = '+90 212 1234567'
                                acc_email = 'info@istanbulhistorichotel.com'
                                acc_website = 'www.istanbulhistorichotel.com' )
                              ( acc_name = 'Prague Castle View'
                                acc_type = 'Hotel'
                                acc_address = '789 Nerudova Street'
                                acc_postal_code = '11800'
                                acc_city = 'Prague'
                                acc_country = 'Czech Republic'
                                acc_phone = '+420 2 1234567'
                                acc_email = 'contact@praguecastleview.cz'
                                acc_website = 'www.praguecastleview.cz' )
                              ( acc_name = 'Vienna Imperial Hotel'
                                acc_type = 'Hotel'
                                acc_address = '123 Kaerntner Strasse'
                                acc_postal_code = '1010'
                                acc_city = 'Vienna'
                                acc_country = 'Austria'
                                acc_phone = '+43 1 1234567'
                                acc_email = 'info@viennaimperialhotel.at'
                                acc_website = 'www.viennaimperialhotel.at' )
                              ( acc_name = 'Cairo Pyramids View'
                                acc_type = 'Hotel'
                                acc_address = '456 Pyramids Road'
                                acc_postal_code = '12556'
                                acc_city = 'Cairo'
                                acc_country = 'Egypt'
                                acc_phone = '+20 2 1234567'
                                acc_email = 'contact@cairopyramidsview.com'
                                acc_website = 'www.cairopyramidsview.com' )
                              ( acc_name = 'Toronto Skyline Hotel'
                                acc_type = 'Hotel'
                                acc_address = '789 Yonge Street'
                                acc_postal_code = 'M5S 1Y7'
                                acc_city = 'Toronto'
                                acc_country = 'Canada'
                                acc_phone = '+1 416 1234567'
                                acc_email = 'info@torontoskylinehotel.ca'
                                acc_website = 'www.torontoskylinehotel.ca' )
                              ( acc_name = 'Mexico City Historic Inn'
                                acc_type = 'Inn'
                                acc_address = '123 Calle Madero'
                                acc_postal_code = '06000'
                                acc_city = 'Mexico City'
                                acc_country = 'Mexico'
                                acc_phone = '+52 55 1234567'
                                acc_email = 'contact@mexicocityhistoricinn.mx'
                                acc_website = 'www.mexicocityhistoricinn.mx' )
                              ( acc_name = 'Moscow Red Square Hotel'
                                acc_type = 'Hotel'
                                acc_address = '456 Tverskaya Street'
                                acc_postal_code = '125009'
                                acc_city = 'Moscow'
                                acc_country = 'Russia'
                                acc_phone = '+7 495 1234567'
                                acc_email = 'info@moscowredsquarehotel.ru'
                                acc_website = 'www.moscowredsquarehotel.ru' )
                              ( acc_name = 'Helsinki Bay View'
                                acc_type = 'Hotel'
                                acc_address = '789 Esplanadi'
                                acc_postal_code = '00100'
                                acc_city = 'Helsinki'
                                acc_country = 'Finland'
                                acc_phone = '+358 9 1234567'
                                acc_email = 'contact@helsinkibayview.fi'
                                acc_website = 'www.helsinkibayview.fi' )
                              ( acc_name = 'Oslo Fjord View'
                                acc_type = 'Hotel'
                                acc_address = '123 Karl Johans Gate'
                                acc_postal_code = '0154'
                                acc_city = 'Oslo'
                                acc_country = 'Norway'
                                acc_phone = '+47 22 123456'
                                acc_email = 'info@oslofjordview.no'
                                acc_website = 'www.oslofjordview.no' )
                              ( acc_name = 'Copenhagen Harbor View'
                                acc_type = 'Hotel'
                                acc_address = '456 Ved Stranden'
                                acc_postal_code = '1061'
                                acc_city = 'Copenhagen'
                                acc_country = 'Denmark'
                                acc_phone = '+45 33 123456'
                                acc_email = 'contact@copenhagenharborview.dk'
                                acc_website = 'www.copenhagenharborview.dk' )
                              ( acc_name = 'Stockholm Old Town'
                                acc_type = 'Hotel'
                                acc_address = '789 Västerlånggatan' ##STRING_OK
                                acc_postal_code = '11129'
                                acc_city = 'Stockholm'
                                acc_country = 'Sweden'
                                acc_phone = '+46 8 1234567'
                                acc_email = 'info@stockholmoldtown.se'
                                acc_website = 'www.stockholmoldtown.se' )
                              ( acc_name = 'Zurich Lake View'
                                acc_type = 'Hotel'
                                acc_address = '123 Bahnhofstrasse'
                                acc_postal_code = '8001'
                                acc_city = 'Zurich'
                                acc_country = 'Switzerland'
                                acc_phone = '+41 44 1234567'
                                acc_email = 'contact@zurichlakeview.ch'
                                acc_website = 'www.zurichlakeview.ch' )
                              ( acc_name = 'Lisbon Historic Inn'
                                acc_type = 'Inn'
                                acc_address = '456 Rua Augusta'
                                acc_postal_code = '1100-053'
                                acc_city = 'Lisbon'
                                acc_country = 'Portugal'
                                acc_phone = '+351 21 1234567'
                                acc_email = 'info@lisbonhistoricinn.pt'
                                acc_website = 'www.lisbonhistoricinn.pt' )
                              ( acc_name = 'Athens Acropolis View'
                                acc_type = 'Hotel'
                                acc_address = '789 Dionysiou Areopagitou'
                                acc_postal_code = '10558'
                                acc_city = 'Athens'
                                acc_country = 'Greece'
                                acc_phone = '+30 210 1234567'
                                acc_email = 'contact@athensacropolisview.gr'
                                acc_website = 'www.athensacropolisview.gr' )
                              ( acc_name = 'Dublin Castle View'
                                acc_type = 'Hotel'
                                acc_address = '123 Dame Street'
                                acc_postal_code = 'D02 CV65'
                                acc_city = 'Dublin'
                                acc_country = 'Ireland'
                                acc_phone = '+353 1 1234567'
                                acc_email = 'info@dublincastleview.ie'
                                acc_website = 'www.dublincastleview.ie' )
                              ( acc_name = 'Reykjavik Northern Lights'
                                acc_type = 'Hotel'
                                acc_address = '456 Laugavegur'
                                acc_postal_code = '101'
                                acc_city = 'Reykjavik'
                                acc_country = 'Iceland'
                                acc_phone = '+354 5 123456'
                                acc_email = 'contact@reykjaviknorthernlights.is'
                                acc_website = 'www.reykjaviknorthernlights.is' )
                              ( acc_name = 'Budapest Danube View'
                                acc_type = 'Hotel'
                                acc_address = '789 Váci Street' ##STRING_OK
                                acc_postal_code = '1052'
                                acc_city = 'Budapest'
                                acc_country = 'Hungary'
                                acc_phone = '+36 1 1234567'
                                acc_email = 'info@budapestdanubeview.hu'
                                acc_website = 'www.budapestdanubeview.hu' )
                              ( acc_name = 'Warsaw Old Town'
                                acc_type = 'Hotel'
                                acc_address = '123 Krakowskie Przedmieście' ##STRING_OK
                                acc_postal_code = '00-071'
                                acc_city = 'Warsaw'
                                acc_country = 'Poland'
                                acc_phone = '+48 22 1234567'
                                acc_email = 'contact@warsawoldtown.pl'
                                acc_website = 'www.warsawoldtown.pl' )
                              ( acc_name = 'Bucharest Old Town'
                                acc_type = 'Hotel'
                                acc_address = '456 Calea Victoriei'
                                acc_postal_code = '010083'
                                acc_city = 'Bucharest'
                                acc_country = 'Romania'
                                acc_phone = '+40 21 1234567'
                                acc_email = 'info@bucharestoldtown.ro'
                                acc_website = 'www.bucharestoldtown.ro' )
                              ( acc_name = 'Sofia Central Hotel'
                                acc_type = 'Hotel'
                                acc_address = '789 Vitosha Boulevard'
                                acc_postal_code = '1000'
                                acc_city = 'Sofia'
                                acc_country = 'Bulgaria'
                                acc_phone = '+359 2 1234567'
                                acc_email = 'contact@sofiacentralhotel.bg'
                                acc_website = 'www.sofiacentralhotel.bg' )
                              ( acc_name = 'Kuala Lumpur City View'
                                acc_type = 'Hotel'
                                acc_address = '123 Jalan Ampang'
                                acc_postal_code = '50450'
                                acc_city = 'Kuala Lumpur'
                                acc_country = 'Malaysia'
                                acc_phone = '+60 3 1234567'
                                acc_email = 'info@kualalumpurcityview.com.my'
                                acc_website = 'www.kualalumpurcityview.com.my' )
                              ( acc_name = 'Delhi Luxury Hotel'
                                acc_type = 'Hotel'
                                acc_address = '456 Connaught Place'
                                acc_postal_code = '110001'
                                acc_city = 'New Delhi'
                                acc_country = 'India'
                                acc_phone = '+91 11 1234567'
                                acc_email = 'contact@delhiluxuryhotel.in'
                                acc_website = 'www.delhiluxuryhotel.in' )
                              ( acc_name = 'Manila Bay View'
                                acc_type = 'Hotel'
                                acc_address = '789 Roxas Boulevard'
                                acc_postal_code = '1000'
                                acc_city = 'Manila'
                                acc_country = 'Philippines'
                                acc_phone = '+63 2 1234567'
                                acc_email = 'info@manilabayview.com'
                                acc_website = 'www.manilabayview.com' )
                              ( acc_name = 'Bali Beach Resort'
                                acc_type = 'Resort'
                                acc_address = '123 Jalan Pantai Kuta'
                                acc_postal_code = '80361'
                                acc_city = 'Kuta'
                                acc_country = 'Indonesia'
                                acc_phone = '+62 361 1234567'
                                acc_email = 'info@balibeachresort.com'
                                acc_website = 'www.balibeachresort.com' )
                              ( acc_name = 'Sofia City Hotel'
                                acc_type = 'Hotel'
                                acc_address = '10 Vitosha Boulevard'
                                acc_postal_code = '1000'
                                acc_city = 'Sofia'
                                acc_country = 'Bulgaria'
                                acc_phone = '+359 2 987 6543'
                                acc_email = 'contact@sofiacityhotel.bg'
                                acc_website = 'www.sofiacityhotel.bg' )
                              ( acc_name = 'Sunny Beach Resort'
                                acc_type = 'Resort'
                                acc_address = '7 Beach Road'
                                acc_postal_code = '8240'
                                acc_city = 'Sunny Beach'
                                acc_country = 'Bulgaria'
                                acc_phone = '+359 554 555 666'
                                acc_email = 'info@sunnybeachresort.bg'
                                acc_website = 'www.sunnybeachresort.bg' ) ).
  ENDMETHOD.

ENDCLASS.


CLASS travel_data_generator DEFINITION INHERITING FROM /dmo/cl_abstract_data_gen CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING VALUE(instance) TYPE REF TO travel_data_generator.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS build_additional_fields REDEFINITION.
    METHODS setup_for_building REDEFINITION.
  PRIVATE SECTION.
    TYPES table_of_travels TYPE STANDARD TABLE OF /dmo/a_trvl_rec WITH KEY travel_id.
    TYPES destination_type TYPE STANDARD TABLE OF I_CountryText WITH KEY Country.
    TYPES accommodation_type TYPE STANDARD TABLE OF /dmo/acc_rec_h WITH KEY acc_id.
    TYPES country TYPE c LENGTH 50.

    CLASS-METHODS prepare_skeleton
      RETURNING VALUE(travels) TYPE table_of_travels.
    METHODS generate_description
      IMPORTING dest          TYPE country
      RETURNING VALUE(result) TYPE /dmo/a_trvl_rec-description.
    METHODS build_destination
      RETURNING VALUE(result) TYPE destination_type.
    METHODS build_accommodations
      RETURNING VALUE(result) TYPE accommodation_type.
    METHODS build_accommodation_values
      IMPORTING dest          TYPE country
      RETURNING VALUE(result) TYPE accommodation_type.
    METHODS generate_accommodation
      IMPORTING dest          TYPE country
      RETURNING VALUE(result) TYPE country.
    METHODS build_rndm_acc_values
      IMPORTING dest          TYPE country
      RETURNING VALUE(result) TYPE /dmo/a_trvl_rec-accommodation.

    CLASS-DATA travel_generator_instance TYPE REF TO travel_data_generator.

    DATA destination TYPE destination_type.
    DATA accommodation TYPE accommodation_type.
    DATA rndm_destination TYPE REF TO cl_abap_random_int.
    DATA rndm_description TYPE REF TO cl_abap_random_int.
    DATA rndm_accommodation TYPE REF TO cl_abap_random_int.
ENDCLASS.

CLASS travel_data_generator IMPLEMENTATION.
  METHOD constructor.
    DATA(travel_skeleton) = prepare_skeleton( ).

    DATA(travel_features) = VALUE feature_structure( with_admin_fields = abap_true
                                                     with_semantic_id  = abap_true
                                                     with_uuid         = abap_true ).

    DATA(field_mapping) = VALUE field_structure( last_changed_at       = 'last_changed_at'
                                                 local_created_at      = 'local_created_at'
                                                 local_created_by      = 'local_created_by'
                                                 local_last_changed_at = 'local_last_changed_at'
                                                 local_last_changed_by = 'local_last_changed_by'
                                                 semantic_id           = 'travel_id'
                                                 uuid                  = 'travel_uuid' ).

    super->constructor( skeleton_data        = REF #( travel_skeleton )
                        scenario_name        = 'Travel for RAP Recommendations' ##NO_TEXT
                        package_name         = '/DMO/FLIGHT_REC'
                        table_name_active    = '/dmo/a_trvl_rec'
                        table_name_draft     = '/dmo/d_trvl_rec'
                        nr_minimum           = '000001'
                        nr_maximum           = '899999'
                        numberrange_object   = '/DMO/TRAVL'
                        numberrange_interval = '01'
                        features             = travel_features
                        fields               = field_mapping ).
  ENDMETHOD.

  METHOD get_instance.
    IF travel_generator_instance IS NOT BOUND.
      travel_generator_instance = NEW travel_data_generator( ).
    ENDIF.

    RETURN travel_generator_instance.
  ENDMETHOD.

  METHOD prepare_skeleton.
    DO 100 TIMES.
      APPEND VALUE /dmo/a_trvl_rec( ) TO travels.
    ENDDO.
  ENDMETHOD.

  METHOD setup_for_building.
    destination = build_destination( ).
    rndm_destination = cl_abap_random_int=>create( min = 1
                                                   max = lines( destination ) ).

    rndm_description  = cl_abap_random_int=>create( min = 1
                                                    max = 5 ).
  ENDMETHOD.

  METHOD build_destination.
    SELECT FROM I_CountryText
      FIELDS Country, Language, CountryName, NationalityName, NationalityLongName, CountryShortName
      WHERE language = 'E'
      INTO TABLE @result.
  ENDMETHOD.

  METHOD build_accommodations.
    SELECT FROM /dmo/acc_rec_h
      FIELDS client, acc_id, acc_name, acc_type, acc_address, acc_postal_code, acc_city, acc_country, acc_phone, acc_email, acc_website
      INTO TABLE @result.                               "#EC CI_NOWHERE
  ENDMETHOD.

  METHOD build_additional_fields.
    FIELD-SYMBOLS <travel> TYPE /dmo/a_trvl_rec.
    ASSIGN entry->* TO <travel>.

    DATA(dest) = destination[ rndm_destination->get_next( ) ]-CountryName.
    <travel>-description = generate_description( dest ).
    <travel>-destination = dest.
    <travel>-accommodation = generate_accommodation( dest ).
  ENDMETHOD.

  METHOD generate_accommodation.
    accommodation = build_accommodation_values( dest ).
    result = build_rndm_acc_values( dest ).
  ENDMETHOD.

  METHOD build_accommodation_values.
    SELECT FROM /dmo/acc_rec_h
      FIELDS client, acc_id, acc_name, acc_type, acc_address, acc_postal_code, acc_city, acc_country, acc_phone, acc_email, acc_website
      WHERE acc_country = @dest
      INTO TABLE @result.
  ENDMETHOD.

  METHOD build_rndm_acc_values.
    IF accommodation IS INITIAL.
      result = |Accommodation in { dest }| ##NO_TEXT.
    ELSE.
      rndm_accommodation = cl_abap_random_int=>create( min = 1
                                                       max = lines( accommodation ) ).

      result = accommodation[ rndm_accommodation->get_next( ) ]-acc_name. "#EC CI_NOORDER
    ENDIF.
  ENDMETHOD.

  METHOD generate_description.
    result = SWITCH /dmo/a_trvl_rec-description(
                      rndm_description->get_next( )
                      WHEN 1 THEN |Business Trip to { dest }| ##NO_TEXT
                      WHEN 2 THEN |Vacation to { dest }| ##NO_TEXT
                      WHEN 3 THEN |Business Trip| ##NO_TEXT
                      ELSE |Vacation| ##NO_TEXT
                    ).
  ENDMETHOD.

ENDCLASS.
