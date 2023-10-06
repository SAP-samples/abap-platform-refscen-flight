INTERFACE lif_data_generator.
  CLASS-METHODS:
    create
      IMPORTING out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.
ENDINTERFACE.

CLASS lcl_agency_data_generator DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES: lif_data_generator.
    TYPES: tt_agency TYPE STANDARD TABLE OF /dmo/agency WITH KEY agency_id.
    CLASS-METHODS: get_data
      RETURNING VALUE(rt_data) TYPE tt_agency.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      cv_numberrange_interval TYPE cl_numberrange_runtime=>nr_interval VALUE '01',
      cv_numberrange_object   TYPE cl_numberrange_runtime=>nr_object   VALUE '/DMO/AGNCY' ##NO_TEXT,
      cv_agency_minimum       TYPE /dmo/agency_id VALUE '070001',
      cv_agency_maximum       TYPE /dmo/agency_id VALUE '079999'.
    CLASS-DATA gt_data TYPE lcl_agency_data_generator=>tt_agency.
    CLASS-METHODS:
      set_numberrange,
      build_content.
ENDCLASS.

CLASS lcl_agency_data_generator IMPLEMENTATION.

  METHOD lif_data_generator~create.
    IF out IS BOUND.  out->write( '--> Delete Content.' ) ##NO_TEXT.
    ENDIF.
    DELETE FROM /dmo/agency.                            "#EC CI_NOWHERE

    IF out IS BOUND.  out->write( '--> Set Numberranges.' ) ##NO_TEXT.
    ENDIF.
    set_numberrange( ).

    IF out IS BOUND.  out->write( '--> Build Content.' ) ##NO_TEXT.
    ENDIF.
    DATA(lt_data) = get_data( ).

    IF out IS BOUND.  out->write( '--> Insert Content.' ) ##NO_TEXT.
    ENDIF.
    INSERT /dmo/agency FROM TABLE @lt_data.

    IF out IS BOUND.  out->write( '--> Done.' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD set_numberrange.

    /dmo/cl_flight_data_generator=>reset_numberrange_interval(
        numberrange_object   = cv_numberrange_object
        numberrange_interval = cv_numberrange_interval
        fromnumber           = CONV cl_numberrange_intervals=>nr_nriv_line-fromnumber( cv_agency_minimum )
        tonumber             = CONV cl_numberrange_intervals=>nr_nriv_line-tonumber(   cv_agency_maximum )
      ).

  ENDMETHOD.

  METHOD get_data.
    IF gt_data IS INITIAL.
      build_content( ).
    ENDIF.
    rt_data = gt_data.
  ENDMETHOD.


  METHOD build_content.
    gt_data = VALUE tt_agency(  ##NO_TEXT
          ( name      = 'Sunshine Travel'
            street    = '134 West Street          '
            postal_code  = '54323                    '
            city      = 'Rochester                '
            country_code   = 'US '
            phone_number = '+1 901-632-5620             '
            web_address       = 'http://www.sunshine-travel.sap               '
            email_address = 'info@sunshine-travel.sap               '
            )
          ( name      = 'Fly High'
            street    = 'Berliner Allee 11        '
            postal_code  = '40880                    '
            city      = 'Duesseldorf               '
            country_code   = 'DE '
            phone_number = '+49 2102 69555              '
            web_address       = 'http://www.flyhigh.sap                       '
            email_address = 'info@flyhigh.sap                       '
            )
          ( name      = 'Happy Hopping'
            street    = 'Calvinstr. 36            '
            postal_code  = '13467                    '
            city      = 'Berlin                   '
            country_code   = 'DE '
            phone_number = '+49 30-8853-0               '
            web_address       = 'http://www.haphop.sap                        '
            email_address = 'info@haphop.sap                        '
            )
          ( name      = 'Pink Panther'
            street    = 'Auf der Schanz 54        '
            postal_code  = '65936                    '
            city      = 'Frankfurt                '
            country_code   = 'DE '
            phone_number = '+49 69-467653-0             '
            web_address       = 'http://www.pinkpanther.sap'
            email_address = 'info@pinkpanther.sap                    '
            )
          ( name      = 'Your Choice'
            street    = 'Gustav-Jung-Str. 425     '
            postal_code  = '90455'
            city      = 'Nuernberg'
            country_code   = 'DE'
            phone_number = '+49 9256-4548-0'
            web_address       = 'http://www.yc.sap'
            email_address = 'info@yc.sap'
            )
          ( name      = 'Bella Italia'
            street    = 'Via Marconi 123'
            postal_code  = '00139'
            city      = 'Roma'
            country_code   = 'IT'
            phone_number = '+39 6 893546721'
            web_address       = 'http://www.tours.it/Adventure/'
            email_address = 'info@tours.it/Adventure/'
            )
          ( name      = 'Hot Socks Travel'
            street    = '224 Balnagask Rd          '
            postal_code  = '8053                    '
            city      = 'Sydney'
            country_code   = 'AU '
            phone_number = '+61 2 2004 5000             '
            web_address       = 'http://www.hst.co.au'
            email_address = 'info@hst.co.au'
            )
          ( name      = 'Burns Nuclear'
            street    = '14 Science Park Drive'
            postal_code  = '118228'
            city      = 'Singapore'
            country_code   = 'SG'
            phone_number = '+65 777-5566'
            web_address       = 'http://www.burns-burns-burns.sg'
            email_address = 'info@burns-burns-burns.sg'
            )
          ( name      = 'Honauer Reisen GmbH'
            street    = 'Baumgarten 8'
            postal_code  = '4212'
            city      = 'Neumarkt'
            country_code   = 'AT'
            phone_number = '+43 7941 8903'
            web_address       = 'http://www.honauer.at'
            email_address = 'info@honauer.at'
            )
          ( name      = 'Travel from Walldorf'
            street    = 'Altonaer Str. 24         '
            postal_code  = '10557                    '
            city      = 'Berlin                   '
            country_code   = 'DE '
            phone_number = '+49 30-622860               '
            web_address       = 'http://www.travel-from-walldorf'
            email_address = 'info@travel-from-walldorf'
            )
          ( name      = 'Voyager Enterprises'
            street    = 'Gustavslundsvaegen 151'
            postal_code  = '70563                    '
            city      = 'Stockholm                '
            country_code   = 'SE '
            phone_number = '+46 8/ 587 70000'
            web_address       = 'http://www.starfleet.ufp'
            email_address = 'info@starfleet.ufp'
            )
          ( name      = 'Ben McCloskey Ltd.'
            street    = '74 Court Oak Rd'
            postal_code  = 'B17 9TN'
            city      = 'Birmingham'
            country_code   = 'GB'
            phone_number = '+44 121 365-2251              '
            web_address       = 'http://www.ben-mcCloskey.co.uk'
            email_address = 'info@ben-mcCloskey.co.uk'
            )
          ( name      = 'Pillepalle Trips'
            street    = 'Gorki Park 4             '
            postal_code  = '8008                   '
            city      = 'Zuerich                   '
            country_code   = 'CH '
            phone_number = '+41 1 345-5321            '
            web_address       = 'http://www.pi-pa-tri.sap'
            email_address = 'info@pi-pa-tri.sap'
            )
          ( name      = 'Kangeroos'
            street    = 'Lancaster drive 435      '
            postal_code  = '20001                    '
            city      = 'London                   '
            country_code   = 'GB '
            phone_number = '+44 171-2937638           '
            web_address       = 'http://www.hopp.sap                          '
            email_address = 'info@hopp.sap                          '
            )
          ( name      = 'Bavarian Castle'
            street    = 'Pilnizerstr. 241         '
            postal_code  = '01069                    '
            city      = 'Dresden                  '
            country_code   = 'DE '
            phone_number = '+49 98-32832732          '
            web_address       = 'http://www.neu.schwanstein.sap               '
            email_address = 'info@neu.schwanstein.sap               '
            )
          ( name      = 'Ali''s Bazar'
            street    = '45, Mac Arthur Boulevard '
            postal_code  = '19113                    '
            city      = 'Boston                   '
            country_code   = 'US '
            phone_number = '+1 508-692-5200             '
            web_address       = 'http://www.ali.sap                           '
            email_address = 'info@ali.sap                           '
            )
          ( name      = 'Super Agency'
            street    = '50 Cranworth St'
            postal_code  = 'G12 8AG'
            city      = 'Glasgow'
            country_code   = 'GB'
            phone_number = '+44 141 711-5643'
            web_address       = 'http://www.super.sap'
            email_address = 'info@super.sap'
            )
          ( name      = 'Wang Chong'
            street    = 'Gagarine Park            '
            postal_code  = '150021                   '
            city      = 'Moscow                   '
            country_code   = 'RU '
            phone_number = '+7 3287-213321    '
            web_address       = 'http://www.wang.chong.sap'
            email_address = 'info@wang.chong.sap'
            )
          ( name      = 'Around the World'
            street    = 'An der Breiten Wiese 122 '
            postal_code  = '30625                    '
            city      = 'Hannover                 '
            country_code   = 'DE '
            phone_number = '+49 511-347589-0            '
            web_address       = 'http://www.atw.sap'
            email_address = 'info@atw.sap'
            )
          ( name      = 'No Return'
            street    = 'Wahnheider Str. 57       '
            postal_code  = '51105                    '
            city      = 'Koeln                     '
            country_code   = 'DE '
            phone_number = '+49 221-5689-100            '
            web_address       = 'http://www.bye-bye.sap                       '
            email_address = 'info@bye-bye.sap                       '
            )
          ( name      = 'Special Agency Peru'
            street    = 'Triberger Str. 42        '
            postal_code  = '70569                    '
            city      = 'Stuttgart                '
            country_code   = 'DE '
            phone_number = '+49 711-7100                '
            web_address       = 'http://www.sap.com                           '
            email_address = 'info@sap.com                           '
            )
          ( name      = 'Caribian Dreams'
            street    = 'Deichstrasse 45           '
            postal_code  = '26721                    '
            city      = 'Emden                    '
            country_code   = 'DE '
            phone_number = '+49 2670-8560-0             '
            web_address       = 'http://www.cuba-libre.sap                   '
            email_address = 'info@cuba-libre.sap                   '
            )
          ( name      = 'Asia By Plane'
            street    = '6-9 Iidabashi 7-chome'
            postal_code  = '102-0072'
            city      = 'Tokyo                  '
            country_code   = 'JP'
            phone_number = '+81 3-3239-3501 '
            web_address       = 'http://www.asia-by-plane.co.jp'
            email_address = 'info@asia-by-plane.co.jp'
            )
          ( name      = 'Everywhere'
            street    = 'Regensburger Platz 23    '
            postal_code  = '81679                    '
            city      = 'Muenchen                  '
            country_code   = 'DE '
            phone_number = '+49 89-2499239              '
            web_address       = 'http://www.everywhere.sap'
            email_address = 'info@everywhere.sap'
            )
          ( name      = 'Happy Holiday'
            street    = 'Rastenburger Str. 12'
            postal_code  = '28779                    '
            city      = 'Bremen                   '
            country_code   = 'DE '
            phone_number = '+49 3266-288817             '
            web_address       = 'http://www.haphol.sap'
            email_address = 'info@haphol.sap'
            )
          ( name      = 'No Name'
            street    = 'Schwalbenweg 43          '
            postal_code  = '52078                    '
            city      = 'Aachen                   '
            country_code   = 'DE '
            phone_number = '+49 241-77729               '
            web_address       = 'http://www.nn.sap'
            email_address = 'info@nn.sap'
            )
          ( name      = 'Fly Low'
            street    = 'Chemnitzer Str. 42       '
            postal_code  = '01187                    '
            city      = 'Dresden                  '
            country_code   = 'DE '
            phone_number = '+49 351-5423-00             '
            web_address       = 'http://www.fly-low.sap'
            email_address = 'info@fly-low.sap'
            )
          ( name      = 'Aussie Travel'
            street    = 'Queens Road              '
            postal_code  = 'M8 7RYP                  '
            city      = 'Manchester               '
            country_code   = 'GB '
            phone_number = '+44 161 2052000           '
            web_address       = 'http://www.down-under.sap'
            email_address = 'info@down-under.sap'
            )
          ( name      = 'Up ''n'' Away'
            street    = 'Nackenbergerstr. 92      '
            postal_code  = '30625                    '
            city      = 'Hannover                 '
            country_code   = 'DE '
            phone_number = '+49 511 403266-0            '
            web_address       = 'http://www.una.sap                           '
            email_address = 'info@una.sap                           '
            )
          ( name      = 'Trans World Travel'
            street    = '100 Industrial Drive     '
            postal_code  = '60804                    '
            city      = 'Chicago                  '
            country_code   = 'US '
            phone_number = '+1 708-454-8723             '
            web_address       = 'http://www.twt.sap                           '
            email_address = 'info@twt.sap                           '
            )
          ( name      = 'Bright Side of Life'
            street    = '340 State Street         '
            postal_code  = '30432                    '
            city      = 'San Francisco            '
            country_code   = 'US '
            phone_number = '+1 415-454-9877             '
            web_address       = 'http://www.ruebennase.sap                    '
            email_address = 'info@ruebennase.sap                    '
            )
          ( name      = 'Sunny, Sunny, Sunny'
            street    = '1300 State Street        '
            postal_code  = '19003                    '
            city      = 'Philadelphia             '
            country_code   = 'US '
            phone_number = '+1 215-090-7659             '
            web_address       = 'http://www.s3.sap                           '
            email_address = 'info@s3.sap                           '
            )
          ( name      = 'Fly & Smile'
            street    = 'Zeppelinstr. 17          '
            postal_code  = '60318                    '
            city      = 'Frankfurt                '
            country_code   = 'DE '
            phone_number = '+49 69-99-0                 '
            web_address       = 'http://www.fly-and-smile.sap            '
            email_address = 'info@fly-and-smile.sap            '
            )
          ( name      = 'Supercheap'
            street    = '1400, Washington Circle  '
            postal_code  = '30439                    '
            city      = 'Los Angeles              '
            country_code   = 'US '
            phone_number = '+1 251-369-2510             '
            web_address       = 'http://www.supercheap.sap                    '
            email_address = 'info@supercheap.sap                    '
            )
          ( name      = 'Hitchhiker'
            street    = '21 Rue de Moselle        '
            postal_code  = '92132                    '
            city      = 'Issy-les-Moulineaux      '
            country_code   = 'FR '
            phone_number = '+33 1-405-555-888         '
            web_address       = 'http://www.42.sap                            '
            email_address = 'info@42.sap                            '
            )
          ( name      = 'Fly Now, Pay Later'
            street    = '100 Madison              '
            postal_code  = '11012                    '
            city      = 'New York                 '
            country_code   = 'US '
            phone_number = '+1 512 343-8543             '
            web_address       = 'http://www.fn-pl.sap                         '
            email_address = 'info@fn-pl.sap                         '
            )
          ( name      = 'Real Weird Vacation'
            street    = '949 5th Street           '
            postal_code  = 'V6T 1Z4'
            city      = 'Vancouver'
            country_code   = 'CA '
            phone_number = '+1 604 827-8024'
            web_address       = 'http://www.reweva.sap                        '
            email_address = 'info@reweva.sap                        '
            )
          ( name      = 'Cap Travels Ltd.'
            street    = '10 Mandela St'
            postal_code  = '2128'
            city      = 'Johannesburg'
            country_code   = 'ZA'
            phone_number = '+27 11 886-8981'
            web_address       = 'http://www.cap-travels.co.za'
            email_address = 'info@cap-travels.co.za'
            )
          ( name      = 'Rainy, Stormy, Cloudy'
            street    = 'Lindenstr. 462           '
            postal_code  = '70563                    '
            city      = 'Stuttgart                '
            country_code   = 'DE '
            phone_number = '+49 711-7992-00             '
            web_address       = 'http://www.windy.sap/rsc/                    '
            email_address = 'info@windy.sap/rsc/                    '
            )
          ( name      = 'Women only'
            street    = 'Kirchstr. 53             '
            postal_code  = '55124                    '
            city      = 'Mainz                    '
            country_code   = 'DE '
            phone_number = '+49 6131-543-00             '
            web_address       = 'http://www.women-only.sap                    '
            email_address = 'info@women-only.sap                    '
            )
          ( name      = 'Maxitrip'
            street    = 'Flugfeld 17'
            postal_code  = '65128'
            city      = 'Wiesbaden'
            country_code   = 'DE'
            phone_number = '+49 611-55 66 77'
            web_address       = 'http://www.maxitrip.sap'
            email_address = 'info@maxitrip.sap'
            )
          ( name      = 'The Ultimate Answer'
            street    = 'Manchester Rd 20         '
            postal_code  = 'AB1 1SA                  '
            city      = 'Avon                     '
            country_code   = 'GB '
            phone_number = '+44 934-66799          '
            web_address       = 'http://www.thulan.sap                        '
            email_address = 'info@thulan.sap                        '
            )
          ( name      = 'Intertravel'
            street    = 'Michigan Ave             '
            postal_code  = '60154                    '
            city      = 'Chicago                  '
            country_code   = 'US '
            phone_number = '+1 788 798-6555            '
            web_address       = 'http://www.intertravel.sap                   '
            email_address = 'info@intertravel.sap                   '
            )
          ( name      = 'Ultimate Goal'
            street    = '300 Peach tree street Sou'
            postal_code  = '01069                    '
            city      = 'Atlanta                  '
            country_code   = 'US '
            phone_number = '+1 874-654-6686'
            web_address       = 'http://www.ultimate-goal.sap                 '
            email_address = 'info@ultimate-goal.sap                 '
            )
          ( street    = '20890 East Central Ave   '
            postal_code  = '30987                    '
            city      = 'Palo Alto                '
            country_code   = 'US '
            phone_number = '+1 652 645-5236               '
            web_address       = 'http://www.sar.sap                           '
            email_address = 'info@sar.sap                           '
            )
          ( name      = 'Hendrik''s'
            street    = '1200 Industrial Drive    '
            postal_code  = '60153                    '
            city      = 'Chicago                  '
            country_code   = 'US '
            phone_number = '+1 08-924-9884             '
            web_address       = 'http://www.essen.sap/150596                  '
            email_address = 'info@essen.sap/150596                  '
            )
          ( name      = 'All British Air Planes'
            street    = '224 Tomato Lane          '
            postal_code  = '08965                    '
            city      = 'Vineland                 '
            country_code   = 'US '
            phone_number = '+44 609-896-Moore            '
            web_address       = 'http://www.abap.sap                           '
            email_address = 'info@abap.sap                           '
            )
          ( name      = 'Rocky Horror Tours'
            street    = '789 Santa Monica Blvd.   '
            postal_code  = '08934                    '
            city      = 'Santa Monica             '
            country_code   = 'US '
            phone_number = '+1 64351-6455-654          '
            web_address       = 'http://www.frank.furter.sap                  '
            email_address = 'info@frank.furter.sap                  '
            )
          ( name      = 'Flights and More'
            street    = '213 Park Blvd.      '
            postal_code  = '35515                    '
            city      = 'Los Walldos'
            country_code   = 'US '
            phone_number = '+1 646 555-6876           '
            web_address       = 'http://www.fam.sap'
            email_address = 'info@fam.sap'
            )
          ( name      = 'Not Only By Bike'
            street    = 'Saalburgstr. 765         '
            postal_code  = '60385                    '
            city      = 'Frankfurt                '
            country_code   = 'DE '
            phone_number = '+49 69 465789-0'
            web_address       = 'http://www.nobb.sap'
            email_address = 'info@nobb.sap'
            )
   ).

    DATA(lv_lines) = lines( gt_data ).
    CHECK lv_lines > 0.
    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING
            nr_range_nr = cv_numberrange_interval
            object      = cv_numberrange_object
            quantity    = CONV cl_numberrange_runtime=>nr_quantity( lv_lines )
          IMPORTING
            number      = DATA(lv_key)
        ).
        DATA(lv_maximum) = CONV i( lv_key+2 ).
        DATA(lv_current) = lv_maximum - lv_lines.

        LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<agency>).
          lv_current += 1.
          <agency>-agency_id = CONV #( lv_current ).
        ENDLOOP.
      CATCH cx_number_ranges INTO DATA(lx).
        " Should not happen.  If so, something is wrong
        RAISE SHORTDUMP lx.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_airport_data_generator DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES: lif_data_generator.
    TYPES: tt_airport TYPE STANDARD TABLE OF /dmo/airport WITH KEY airport_id.
    CLASS-METHODS: get_data
      RETURNING VALUE(rt_data) TYPE lcl_airport_data_generator=>tt_airport.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_airport_data_generator IMPLEMENTATION.

  METHOD lif_data_generator~create.
    IF out IS BOUND.  out->write( '--> Delete Content.' ) ##NO_TEXT.
    ENDIF.
    DELETE FROM /dmo/airport.                           "#EC CI_NOWHERE

    IF out IS BOUND.  out->write( '--> Build Content.' ) ##NO_TEXT.
    ENDIF.
    DATA(lt_data) = get_data(  ).

    IF out IS BOUND.  out->write( '--> Insert Content.' ) ##NO_TEXT.
    ENDIF.
    INSERT /dmo/airport FROM TABLE @lt_data.

    IF out IS BOUND.  out->write( '--> Done.' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_data.
    rt_data = VALUE tt_airport( ##NO_TEXT ##STRING_OK
          " Europe
          ( airport_id = 'FRA'    name = 'Frankfurt Airport'                      city = 'Frankfurt/Main'                    country = 'DE' )
          ( airport_id = 'HAM'    name = 'Hamburg Airport'                        city = 'Hamburg'                           country = 'DE' )
          ( airport_id = 'MUC'    name = 'Munich Airport'                         city = 'Munich'                            country = 'DE' )
          ( airport_id = 'SXF'    name = 'Berlin Schönefeld Airport'              city = 'Berlin'                            country = 'DE' )
          ( airport_id = 'THF'    name = 'Berlin Tempelhof Airport'               city = 'Berlin'                            country = 'DE' )
          ( airport_id = 'TXL'    name = 'Berlin Tegel Airport'                   city = 'Berlin'                            country = 'DE' )
          ( airport_id = 'CDG'    name = 'Charles de Gaulle Airport'              city = 'Paris'                             country = 'FR' )
          ( airport_id = 'ORY'    name = 'Orly Airport'                           city = 'Paris'                             country = 'FR' )
          ( airport_id = 'VIE'    name = 'Vienna International Airport'           city = 'Vienna'                            country = 'AT' )
          ( airport_id = 'ZRH'    name = 'Zürich Airport'                         city = 'Zurich'                            country = 'CH' )
          ( airport_id = 'RTM'    name = 'Rotterdam The Hague Airport'            city = 'Rotterdam'                         country = 'NL' )
          ( airport_id = 'FCO'    name = 'Leonardo da Vinci–Fiumicino Airport'    city = 'Rome'                              country = 'IT' )
          ( airport_id = 'VCE'    name = 'Venice Marco Polo Airport'              city = 'Venice'                            country = 'IT' )
          ( airport_id = 'LCY'    name = 'London City Airport'                    city = 'London'                            country = 'UK' )
          ( airport_id = 'LGW'    name = 'Gatwick Airport'                        city = 'London'                            country = 'UK' )
          ( airport_id = 'LHR'    name = 'Heathrow Airport'                       city = 'London'                            country = 'UK' )
          ( airport_id = 'MAD'    name = 'Adolfo Suárez Madrid–Barajas Airport'   city = 'Madrid'                            country = 'ES' )
          ( airport_id = 'VKO'    name = 'Vnukovo International Airport'          city = 'Moscow'                            country = 'RU' )
          ( airport_id = 'SVO'    name = 'Sheremetyevo International Airport'     city = 'Moscow'                            country = 'RU' )

          " America
          ( airport_id = 'JFK'    name = 'John F. Kennedy International Airport'  city = 'New York City, New York'           country = 'US' )
          ( airport_id = 'BNA'    name = 'Nashville International Airport'        city = 'Nashville, Tennessee'              country = 'US' )
          ( airport_id = 'BOS'    name = 'Logan International Airport'            city = 'Boston, Massachusetts'             country = 'US' )
          ( airport_id = 'ELP'    name = 'El Paso International Airport'          city = 'El Paso, Texas'                    country = 'US' )
          ( airport_id = 'DEN'    name = 'Denver International Airport'           city = 'Denver, Colorado'                  country = 'US' )
          ( airport_id = 'HOU'    name = 'William P. Hobby Airport'               city = 'Houston, Texas'                    country = 'US' )
          ( airport_id = 'LAS'    name = 'McCarran International Airport'         city = 'Las Vegas, Nevada'                 country = 'US' )
          ( airport_id = 'LAX'    name = 'Los Angeles International Airport'      city = 'Los Angeles, California'           country = 'US' )
          ( airport_id = 'MCI'    name = 'Kansas City International Airport'      city = 'Kansas City, Missouri'             country = 'US' )
          ( airport_id = 'MIA'    name = 'Miami International Airport'            city = 'Miami, Florida'                    country = 'US' )
          ( airport_id = 'SFO'    name = 'San Francisco International Airport'    city = 'San Francisco, California'         country = 'US' )
          ( airport_id = 'EWR'    name = 'Newark Liberty International Airport'   city = 'Newark, New Jersey'                country = 'US' )
          ( airport_id = 'YOW'    name = 'Ottawa Macdonald–Cartier Int. Airport'  city = 'Ottawa, Ontario'                   country = 'CA' )
          ( airport_id = 'ACA'    name = 'General Juan N. Álvarez Int. Airport'   city = 'Acapulco, Guerrero'                country = 'MX' )
          ( airport_id = 'GIG'    name = 'Rio de Janeiro–Galeão Int. Airport'     city = 'Rio de Janeiro'                    country = 'BR' )
          ( airport_id = 'HAV'    name = 'José Martí International Airport'       city = 'Havana'                            country = 'CU' )

          " Australia
          ( airport_id = 'ASP'    name = 'Alice Springs Airport'                  city = 'Alice Springs, Northern Territory' country = 'AU' )

          " Africa
          ( airport_id = 'ACE'    name = 'Lanzarote Airport'                      city = 'Lanzarote, Canary Islands'         country = 'ES' )
          ( airport_id = 'HRE'    name = 'Harare International Airport'           city = 'Harare'                            country = 'ZW' )
          ( airport_id = 'GCJ'    name = 'Grand Central Airport'                  city = 'Johannesburg'                      country = 'SA' )

          " Asia
          ( airport_id = 'NRT'    name = 'Narita International Airport'           city = 'Tokyo, Honshu'                     country = 'JP' )
          ( airport_id = 'ITM'    name = 'Osaka International Airport'            city = 'Osaka, Honshu'                     country = 'JP' )
          ( airport_id = 'KIX'    name = 'Kansai International Airport'           city = 'Osaka, Honshu'                     country = 'JP' )
          ( airport_id = 'HIJ'    name = 'Hiroshima Airport'                      city = 'Hiroshima, Honshu'                 country = 'JP' )
          ( airport_id = 'SIN'    name = 'Singapore Changi Airport'               city = 'Singapore'                         country = 'SG' )
          ( airport_id = 'KUL'    name = 'Kuala Lumpur International Airport'     city = 'Kuala Lumpur'                      country = 'MY' )
          ( airport_id = 'HKG'    name = 'Hong Kong International Airport'        city = 'Hongkong'                          country = 'CN' )
          ( airport_id = 'BKK'    name = 'Suvarnabhumi Airport'                   city = 'Bangkok'                           country = 'TH' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_carrier_data_generator DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES: lif_data_generator.
    TYPES: tt_carrier TYPE STANDARD TABLE OF /dmo/carrier WITH KEY carrier_id.
    CLASS-METHODS: get_data
      RETURNING
        VALUE(rt_data) TYPE tt_carrier.
  PROTECTED SECTION.
  PRIVATE SECTION.



ENDCLASS.

CLASS lcl_carrier_data_generator IMPLEMENTATION.

  METHOD lif_data_generator~create.
    IF out IS BOUND.  out->write( '--> Delete Content.' ) ##NO_TEXT.
    ENDIF.
    DELETE FROM /dmo/carrier.                           "#EC CI_NOWHERE

    IF out IS BOUND.  out->write( '--> Build Content.' ) ##NO_TEXT.
    ENDIF.
    DATA(lt_data) = get_data( ).

    IF out IS BOUND.  out->write( '--> Insert Content.' ) ##NO_TEXT.
    ENDIF.
    INSERT /dmo/carrier FROM TABLE @lt_data.

    IF out IS BOUND.  out->write( '--> Done.' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_data.
    GET TIME STAMP FIELD DATA(current_timestamp).
    rt_data = VALUE tt_carrier( ##NO_TEXT
        local_created_by      = 'GENERATOR'
        local_last_changed_by = 'GENERATOR'
        local_created_at      = current_timestamp
        local_last_changed_at = current_timestamp
        last_changed_at       = current_timestamp
        (   carrier_id = 'AA'  name = 'American Airlines Inc.'                  currency_code = 'USD'  )
        (   carrier_id = 'AC'  name = 'Air Canada'                              currency_code = 'CAD'  )
        (   carrier_id = 'AF'  name = 'Air France'                              currency_code = 'EUR'  )
        (   carrier_id = 'AZ'  name = 'Alitalia Societa Aerea Italiana S.p.A.'  currency_code = 'EUR'  )
        (   carrier_id = 'BA'  name = 'British Airways p.l.c.'                  currency_code = 'GBP'  )
        (   carrier_id = 'FJ'  name = 'Air Pacific Limited t/a Fiji Airway'     currency_code = 'USD'  )
        (   carrier_id = 'CO'  name = 'Cobaltair Ltd. dba Cobalt'               currency_code = 'USD'  )
        (   carrier_id = 'DL'  name = 'Delta Air Lines, Inc.'                   currency_code = 'USD'  )
        (   carrier_id = 'LH'  name = 'Deutsche Lufthansa AG'                   currency_code = 'EUR'  )
        (   carrier_id = 'NG'  name = 'AL-Naser Wings'                          currency_code = 'EUR'  )
        (   carrier_id = 'JL'  name = 'Japan Airlines Co., Ltd.'                currency_code = 'JPY'  )
        (   carrier_id = 'QF'  name = 'Qantas Airways Ltd.'                     currency_code = 'AUD'  )
        (   carrier_id = 'SA'  name = 'South African Airways'                   currency_code = 'ZAR'  )
        (   carrier_id = 'SQ'  name = 'Singapore Airlines Limited'              currency_code = 'SGD'  )
        (   carrier_id = 'SR'  name = 'Sundair GmbH'                            currency_code = 'CHF'  )
        (   carrier_id = 'UA'  name = 'United Airlines, Inc.'                   currency_code = 'USD'  )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS lcl_connection_data_generator DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES: lif_data_generator.
    TYPES: tt_connection TYPE STANDARD TABLE OF /dmo/connection WITH KEY carrier_id connection_id.

    TYPES:
      "! Structure for additional information for generation. <br/>
      "! <em>weekday</em> '1' means Monday, '7' is Sunday
      BEGIN OF ty_connection_additional_info.
        INCLUDE TYPE /dmo/connection.
    TYPES: weekday TYPE i,
      END OF ty_connection_additional_info.

    TYPES: tt_connection_additional_info TYPE STANDARD TABLE OF ty_connection_additional_info WITH KEY connection_id.

    CLASS-METHODS: get_data "provide data public
      RETURNING VALUE(rt_data) TYPE tt_connection_additional_info.
  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_connection_data_generator IMPLEMENTATION.

  METHOD lif_data_generator~create.
    IF out IS BOUND.  out->write( '--> Delete Content.' ) ##NO_TEXT.
    ENDIF.
    DELETE FROM /dmo/connection.                        "#EC CI_NOWHERE

    IF out IS BOUND.  out->write( '--> Build Content.' ) ##NO_TEXT.
    ENDIF.
    DATA(lt_data) = get_data(  ).
    DATA(lt_data_db) = CORRESPONDING tt_connection( lt_data ).

    IF out IS BOUND.  out->write( '--> Insert Content.' ) ##NO_TEXT.
    ENDIF.
    INSERT /dmo/connection FROM TABLE @lt_data.

    IF out IS BOUND.  out->write( '--> Done.' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_data.
    rt_data = VALUE tt_connection_additional_info( ##NO_TEXT
          ( carrier_id = 'SQ'  connection_id = '0001'  airport_from_id = 'SFO'  airport_to_id = 'SIN'  departure_time = '011500'  arrival_time = '115000'  distance = 13523  distance_unit = 'KM'  weekday = 3 ) "1-7
          ( carrier_id = 'SQ'  connection_id = '0002'  airport_from_id = 'SIN'  airport_to_id = 'SFO'  departure_time = '063000'  arrival_time = '091500'  distance = 13523  distance_unit = 'KM'  weekday = 4 ) "1-7
          ( carrier_id = 'SQ'  connection_id = '0011'  airport_from_id = 'NRT'  airport_to_id = 'SIN'  departure_time = '145500'  arrival_time = '205000'  distance =  5363  distance_unit = 'KM'  weekday = 4 ) "1-7
          ( carrier_id = 'SQ'  connection_id = '0012'  airport_from_id = 'SIN'  airport_to_id = 'NRT'  departure_time = '095300'  arrival_time = '175400'  distance =  5363  distance_unit = 'KM'  weekday = 6 ) "1-7
          ( carrier_id = 'UA'  connection_id = '0058'  airport_from_id = 'SFO'  airport_to_id = 'FRA'  departure_time = '134500'  arrival_time = '095500'  distance =  9608  distance_unit = 'KM'  weekday = 1 ) "1-7
          ( carrier_id = 'UA'  connection_id = '0059'  airport_from_id = 'FRA'  airport_to_id = 'SFO'  departure_time = '135500'  arrival_time = '163000'  distance =  9608  distance_unit = 'KM'  weekday = 2 ) "1-7
          ( carrier_id = 'UA'  connection_id = '1537'  airport_from_id = 'EWR'  airport_to_id = 'MIA'  departure_time = '215600'  arrival_time = '004700'  distance =  1752  distance_unit = 'KM'  weekday = 5 ) "1-7
          ( carrier_id = 'AA'  connection_id = '0322'  airport_from_id = 'MIA'  airport_to_id = 'EWR'  departure_time = '201700'  arrival_time = '231900'  distance =  1752  distance_unit = 'KM'  weekday = 7 ) "1-7
          ( carrier_id = 'AA'  connection_id = '0017'  airport_from_id = 'MIA'  airport_to_id = 'HAV'  departure_time = '071900'  arrival_time = '080300'  distance =   520  distance_unit = 'KM'  weekday = 3 ) "1-7
          ( carrier_id = 'AA'  connection_id = '2678'  airport_from_id = 'HAV'  airport_to_id = 'MIA'  departure_time = '061500'  arrival_time = '103000'  distance =   520  distance_unit = 'KM'  weekday = 6 ) "1-7
          ( carrier_id = 'AA'  connection_id = '0015'  airport_from_id = 'JFK'  airport_to_id = 'SFO'  departure_time = '071300'  arrival_time = '100400'  distance =  4156  distance_unit = 'KM'  weekday = 5 ) "1-7
          ( carrier_id = 'AA'  connection_id = '0018'  airport_from_id = 'SFO'  airport_to_id = 'JFK'  departure_time = '064000'  arrival_time = '150600'  distance =  4156  distance_unit = 'KM'  weekday = 4 ) "1-7
          ( carrier_id = 'LH'  connection_id = '0400'  airport_from_id = 'FRA'  airport_to_id = 'JFK'  departure_time = '101000'  arrival_time = '113400'  distance =  6162  distance_unit = 'KM'  weekday = 6 ) "1-7
          ( carrier_id = 'LH'  connection_id = '0401'  airport_from_id = 'JFK'  airport_to_id = 'FRA'  departure_time = '183000'  arrival_time = '074500'  distance =  6162  distance_unit = 'KM'  weekday = 5 ) "1-7
          ( carrier_id = 'LH'  connection_id = '0402'  airport_from_id = 'FRA'  airport_to_id = 'EWR'  departure_time = '133000'  arrival_time = '153500'  distance =  6217  distance_unit = 'KM'  weekday = 1 ) "1-7
          ( carrier_id = 'LH'  connection_id = '0403'  airport_from_id = 'EWR'  airport_to_id = 'FRA'  departure_time = '180900'  arrival_time = '073000'  distance =  6217  distance_unit = 'KM'  weekday = 1 ) "1-7
          ( carrier_id = 'JL'  connection_id = '0407'  airport_from_id = 'NRT'  airport_to_id = 'FRA'  departure_time = '132300'  arrival_time = '155600'  distance =  9379  distance_unit = 'KM'  weekday = 5 ) "1-7
          ( carrier_id = 'JL'  connection_id = '0408'  airport_from_id = 'FRA'  airport_to_id = 'NRT'  departure_time = '202500'  arrival_time = '154000'  distance =  9379  distance_unit = 'KM'  weekday = 6 ) "1-7
          ( carrier_id = 'AZ'  connection_id = '0788'  airport_from_id = 'VCE'  airport_to_id = 'NRT'  departure_time = '132500'  arrival_time = '101300'  distance =  9595  distance_unit = 'KM'  weekday = 6 )
          ( carrier_id = 'AZ'  connection_id = '0789'  airport_from_id = 'NRT'  airport_to_id = 'VCE'  departure_time = '142600'  arrival_time = '213100'  distance =  9595  distance_unit = 'KM'  weekday = 5 )
      ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_flight_data_generator DEFINITION DEFERRED.
CLASS /dmo/cl_flight_data_generator DEFINITION LOCAL FRIENDS lcl_flight_data_generator.
CLASS lcl_flight_data_generator DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES: lif_data_generator.
    TYPES: tt_flights TYPE STANDARD TABLE OF /dmo/flight
                            WITH KEY carrier_id connection_id flight_date
                            WITH NON-UNIQUE SORTED KEY key_sorted_seats COMPONENTS seats_occupied
                            WITH NON-UNIQUE SORTED KEY key_sorted_date COMPONENTS connection_id flight_date.
    CLASS-METHODS: get_data
      RETURNING VALUE(rt_data) TYPE tt_flights.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_plane_type,
             id            TYPE /dmo/plane_type_id,
             seats_max     TYPE /dmo/plane_seats_max,
             long_distance TYPE abap_bool,
             index         TYPE int1,
           END OF ty_plane_type,
           BEGIN OF ty_flight_info,
             id             TYPE /dmo/plane_type_id,
             long_distance  TYPE abap_bool,
             seats_max      TYPE /dmo/plane_seats_max,
             seats_occupied TYPE /dmo/plane_seats_occupied,
             price          TYPE /dmo/flight_price,
           END OF ty_flight_info,
           BEGIN OF ty_connection_recurrency,
             id         TYPE /dmo/connection_id,
             recurrency TYPE STANDARD TABLE OF /dmo/flight_date WITH EMPTY KEY,
           END OF ty_connection_recurrency.

    TYPES: tt_plane_type            TYPE STANDARD TABLE OF ty_plane_type               WITH KEY id,
           tt_connection_recurrency TYPE STANDARD TABLE OF ty_connection_recurrency    WITH KEY id.
    CONSTANTS: cv_random_offset  TYPE i VALUE 25,
               cv_random_percent TYPE i VALUE 70.

    CLASS-DATA: gt_connections               TYPE lcl_connection_data_generator=>tt_connection,
                gt_carrier                   TYPE lcl_carrier_data_generator=>tt_carrier,
                gt_plane_types               TYPE tt_plane_type,
                go_random_int_distance_long  TYPE REF TO cl_abap_random_int,
                go_random_int_distance_short TYPE REF TO cl_abap_random_int,
                gv_plane_distance_long       TYPE i,
                gv_plane_distance_short      TYPE i,
                gt_connection_recurrency     TYPE lcl_flight_data_generator=>tt_connection_recurrency,
                gt_flights                   TYPE lcl_flight_data_generator=>tt_flights,
                go_random_seats              TYPE REF TO cl_abap_random_int.

    CLASS-METHODS: build_dependent_content,
      get_flight_info
        IMPORTING
          iv_connection_id     TYPE /dmo/connection_id
        RETURNING
          VALUE(rs_plane_info) TYPE ty_flight_info,
      build_plane_types
        RETURNING
          VALUE(rt_data) TYPE lcl_flight_data_generator=>tt_plane_type,

      build_connection_recurrency
        RETURNING
          VALUE(rt_data) TYPE lcl_flight_data_generator=>tt_connection_recurrency,
      calc_next_monday
        IMPORTING
          iv_date        TYPE d
        RETURNING
          VALUE(rv_date) TYPE d.

ENDCLASS.

CLASS lcl_flight_data_generator IMPLEMENTATION.

  METHOD lif_data_generator~create.
    IF out IS BOUND.  out->write( '--> Delete Content.' ) ##NO_TEXT.
    ENDIF.
    DELETE FROM /dmo/flight.                            "#EC CI_NOWHERE

    IF out IS BOUND.  out->write( '--> Build Dependent Content.' ) ##NO_TEXT.
    ENDIF.
    build_dependent_content( ).

    IF out IS BOUND.  out->write( '--> Build Content.' ) ##NO_TEXT.
    ENDIF.
    get_data(  ).

    IF out IS BOUND.  out->write( '--> Insert Content.' ) ##NO_TEXT.
    ENDIF.
    INSERT /dmo/flight FROM TABLE @gt_flights.

    IF out IS BOUND.  out->write( '--> Done.' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_data.
    DATA: lt_flights TYPE tt_flights,
          ls_flight  TYPE lcl_flight_data_generator=>ty_flight_info.

    IF gt_flights IS NOT INITIAL.
      rt_data = gt_flights.
      EXIT.
    ENDIF.

    IF gt_connections             IS INITIAL
      OR gt_carrier               IS INITIAL
      OR gt_plane_types           IS INITIAL
      OR gt_connection_recurrency IS INITIAL.
      build_dependent_content( ).
    ENDIF.

    LOOP AT gt_connections INTO DATA(ls_connection).
      LOOP AT gt_connection_recurrency[ id = ls_connection-connection_id ]-recurrency INTO DATA(lv_flight_date).
        ls_flight = get_flight_info( ls_connection-connection_id ).
        APPEND VALUE /dmo/flight(
                  carrier_id     = ls_connection-carrier_id
                  connection_id  = ls_connection-connection_id
                  flight_date    = lv_flight_date
                  price          = ls_flight-price
                  currency_code  = VALUE /dmo/flight-currency_code(
                                                 gt_carrier[ carrier_id = ls_connection-carrier_id ]-currency_code
                                                 DEFAULT 'EUR' )
                  plane_type_id  =  ls_flight-id
                  seats_max      =  ls_flight-seats_max
                  seats_occupied =  ls_flight-seats_occupied
        ) TO gt_flights.
      ENDLOOP.
      rt_data = gt_flights.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_dependent_content.
    gt_connections = CORRESPONDING #( lcl_connection_data_generator=>get_data( ) ).

    gt_carrier = lcl_carrier_data_generator=>get_data( ).

    gt_plane_types = build_plane_types( ).

    go_random_seats = cl_abap_random_int=>create(
                                              min  = cv_random_percent - cv_random_offset
                                              max  = cv_random_percent + cv_random_offset
                                         ).

    gt_connection_recurrency = build_connection_recurrency( ).
  ENDMETHOD.


  METHOD get_flight_info.
    DATA: lv_count  TYPE i,
          lo_random TYPE REF TO cl_abap_random_int.
    DATA(lt_connections) = lcl_connection_data_generator=>get_data( ).
    DATA(lv_is_long_distance) = COND abap_bool( WHEN lt_connections[ connection_id = iv_connection_id ]-distance > 3000
                                                 THEN abap_true
                                                 ELSE abap_false ).

    IF lv_is_long_distance = abap_true.
      IF gv_plane_distance_long IS INITIAL.
        SELECT COUNT(*) FROM @gt_plane_types AS planes WHERE long_distance = @lv_is_long_distance INTO @gv_plane_distance_long.
      ENDIF.
      lv_count = gv_plane_distance_long.

      IF go_random_int_distance_long IS NOT BOUND.
        go_random_int_distance_long = cl_abap_random_int=>create( seed = 1337  min = 1  max = gv_plane_distance_long ).
      ENDIF.
      lo_random = go_random_int_distance_long.

    ELSE.
      IF gv_plane_distance_short IS INITIAL.
        SELECT COUNT(*) FROM @gt_plane_types AS planes WHERE long_distance = @lv_is_long_distance INTO @gv_plane_distance_short.
      ENDIF.
      lv_count = gv_plane_distance_short.

      IF go_random_int_distance_short IS NOT BOUND.
        go_random_int_distance_short = cl_abap_random_int=>create( seed = 1337  min = 1  max = gv_plane_distance_short ).
      ENDIF.
      lo_random = go_random_int_distance_short.
    ENDIF.

    DATA(ls_plane_type) = gt_plane_types[
               long_distance = lv_is_long_distance
               index = lo_random->get_next( )
            ].

    DATA(lv_seats_occupied_percent) = go_random_seats->get_next( ).
    rs_plane_info = VALUE ty_flight_info(
        id             = ls_plane_type-id
        long_distance  = ls_plane_type-long_distance
        seats_max      = ls_plane_type-seats_max
        seats_occupied = ls_plane_type-seats_max * lv_seats_occupied_percent DIV 100
        price          = /dmo/cl_flight_data_generator=>calculate_flight_price(
                            iv_seats_occupied_percent = lv_seats_occupied_percent
                            iv_flight_distance        = lt_connections[ connection_id = iv_connection_id ]-distance
                          )
    ).

  ENDMETHOD.


  METHOD build_plane_types.
    rt_data = VALUE tt_plane_type( ##NO_TEXT
                ( id = 'A320-200' seats_max = 130 long_distance = ' ' index = 1 )
                ( id = 'A321-200' seats_max = 150 long_distance = ' ' index = 2 )
                ( id = '737-800'  seats_max = 140 long_distance = ' ' index = 3 )
                ( id = 'A319-100' seats_max = 120 long_distance = ' ' index = 4 )
                ( id = '747-400'  seats_max = 385 long_distance = 'X' index = 1 )
                ( id = '767-200'  seats_max = 260 long_distance = 'X' index = 2 )
                ( id = 'A340-600' seats_max = 330 long_distance = 'X' index = 3 )
                ( id = 'A380-800' seats_max = 475 long_distance = 'X' index = 4 )
    ).
  ENDMETHOD.


  METHOD build_connection_recurrency.
    CONSTANTS:
      cv_days_between_test   TYPE i VALUE 300,
      cv_days_between_8weeks TYPE i VALUE 56,
      cv_days_between_4weeks TYPE i VALUE 28,
      cv_days_between_2weeks TYPE i VALUE 14,
      cv_days_between_1weeks TYPE i VALUE  7.

    DATA:
      flight_date_max TYPE d,
      flight_date_min TYPE d.

    DATA(lv_days_between) = cv_days_between_test.

    GET TIME STAMP FIELD DATA(current_timestamp).
    DATA(tmp) = CONV string( current_timestamp ).
    DATA lv_datum TYPE d.
    lv_datum = tmp(8).

    " flight_date_max is a Monday 8 months in the future
    flight_date_max = calc_next_monday( CONV /dmo/flight_date( lv_datum + 217 ) ).

    " flight_date_min is a Monday 5 months in the past
    flight_date_min = calc_next_monday( CONV /dmo/flight_date( lv_datum - 148 ) ).

    LOOP AT lcl_connection_data_generator=>get_data( ) INTO DATA(ls_connection).
      APPEND VALUE ty_connection_recurrency(
          id         = ls_connection-connection_id
          recurrency = VALUE ty_connection_recurrency-recurrency(
                                    FOR   flightdate = flight_date_max + ls_connection-weekday - 1
                                    THEN  flightdate - lv_days_between
                                    UNTIL flightdate < flight_date_min + ls_connection-weekday - 1
                                       ( CONV /dmo/flight_date( flightdate ) )
                               )
      ) TO rt_data.
    ENDLOOP.
  ENDMETHOD.

  METHOD calc_next_monday.
* 01.01.1900 was a Saturday.
    DATA(lv_weekday)  = iv_date MOD 7.
* Therefore 0 is a Saturday, 1 for Sunday, etc.. and will be mapped to 1 for Monday, 2 for Tuesday, etc..
    IF lv_weekday > 1.
      lv_weekday = lv_weekday - 1.
    ELSE.
      lv_weekday = lv_weekday + 6.
    ENDIF.

    rv_date = iv_date - lv_weekday + 8.
  ENDMETHOD.



ENDCLASS.


CLASS lcl_customer_data_generator DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES: lif_data_generator.
    TYPES: BEGIN OF ty_last_name,
             last_name TYPE /dmo/last_name,
           END OF ty_last_name.
    TYPES: tt_customer  TYPE STANDARD TABLE OF /dmo/customer WITH KEY customer_id,
           tt_last_name TYPE STANDARD TABLE OF ty_last_name  WITH KEY last_name.
    CLASS-METHODS: get_data
      RETURNING VALUE(rt_data) TYPE tt_customer,
      build_last_names
        RETURNING
          VALUE(rt_data) TYPE tt_last_name.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      " Names
      BEGIN OF ty_first_name,
        first_name TYPE /dmo/first_name,
        gender     TYPE c LENGTH 1,
      END OF ty_first_name,
      BEGIN OF ty_name,
        first_name TYPE /dmo/first_name,
        last_name  TYPE /dmo/last_name,
        title      TYPE /dmo/title,
      END OF ty_name,
      " Addresses
      BEGIN OF ty_city,
        country     TYPE land1,
        postal_code TYPE /dmo/postal_code,
        city        TYPE /dmo/city,
      END OF ty_city,
      tt_street_per_country TYPE STANDARD TABLE OF /dmo/street WITH EMPTY KEY,
      BEGIN OF ty_street,
        country TYPE land1,
        streets TYPE tt_street_per_country,
      END OF ty_street,
      BEGIN OF ty_address,
        country       TYPE land1,
        postal_code   TYPE /dmo/postal_code,
        city          TYPE /dmo/city,
        street        TYPE /dmo/street,
        phone_number  TYPE  /dmo/phone_number,
        email_address TYPE /dmo/email_address,
      END OF ty_address.


    TYPES:
      " Names
      tt_first_name TYPE STANDARD TABLE OF ty_first_name WITH KEY first_name,
      tt_name       TYPE STANDARD TABLE OF ty_name       WITH KEY first_name last_name,
      " Addresses
      tt_city       TYPE STANDARD TABLE OF ty_city WITH KEY country city,
      tt_street     TYPE STANDARD TABLE OF ty_street WITH KEY country,
      tt_address    TYPE STANDARD TABLE OF ty_address WITH KEY country city street.
    CONSTANTS cv_email_host TYPE string VALUE `flight.example` ##NO_TEXT.
    CONSTANTS cv_phone_number_seperator TYPE string VALUE `-` ##NO_TEXT.
    CONSTANTS cv_phone_number_min TYPE int8 VALUE 1234567890.
    CONSTANTS cv_phone_number_max TYPE int8 VALUE 9999999999.
    CLASS-DATA gt_data TYPE lcl_customer_data_generator=>tt_customer.

    CLASS-METHODS:
      " Names
      generate_names
        RETURNING
          VALUE(rt_data) TYPE tt_name,
      build_first_names
        RETURNING
          VALUE(rt_data) TYPE tt_first_name,
      " Adresses
      generate_addresses
        RETURNING
          VALUE(rt_data) TYPE tt_address,
      build_city
        RETURNING
          VALUE(rt_data) TYPE tt_city,
      build_street
        RETURNING
          VALUE(rt_data) TYPE tt_street.


ENDCLASS.

CLASS lcl_customer_data_generator IMPLEMENTATION.

  METHOD lif_data_generator~create.
    IF out IS BOUND.  out->write( '--> Delete Content.' ) ##NO_TEXT.
    ENDIF.
    DELETE FROM /dmo/customer.                          "#EC CI_NOWHERE

    IF out IS BOUND.  out->write( '--> Build Content.' ) ##NO_TEXT.
    ENDIF.
    DATA(lt_data) = get_data( ).

    IF out IS BOUND.  out->write( '--> Insert Content.' ) ##NO_TEXT.
    ENDIF.
    INSERT /dmo/customer FROM TABLE @lt_data.

    IF out IS BOUND.  out->write( '--> Done.' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD build_first_names.
    rt_data = VALUE tt_first_name( ##NO_TEXT
                ( first_name = 'Simon'  gender = 'M' )
                ( first_name = 'Harish'  gender = 'M' )
                ( first_name = 'Volker'  gender = 'M' )
                ( first_name = 'Jasmin'  gender = 'F' )
                ( first_name = 'Felix'  gender = 'M' )
                ( first_name = 'Kristina'  gender = 'F' )
                ( first_name = 'Thilo'  gender = 'M' )
                ( first_name = 'Andrej'  gender = 'M' )
                ( first_name = 'Anna'  gender = 'F' )
                ( first_name = 'Johannes' gender = 'M' )
                ( first_name = 'Johann'  gender = 'M' )
                ( first_name = 'Christoph' gender = 'M' )
                ( first_name = 'Andreas' gender = 'M' )
                ( first_name = 'Stephen' gender = 'M' )
                ( first_name = 'Mathilde' gender = 'F' )
                ( first_name = 'August'  gender = 'M' )
                ( first_name = 'Illya'  gender = 'M' )
                ( first_name = 'Georg'  gender = 'M' )
                ( first_name = 'Gisela'  gender = 'F' )
                ( first_name = 'Christa' gender = 'F' )
                ( first_name = 'Holm'  gender = 'M' )
                ( first_name = 'Irmtraut' gender = 'F' )
                ( first_name = 'Ludwig'  gender = 'M' )
                ( first_name = 'Laura'  gender = 'F' )
                ( first_name = 'Kurt'  gender = 'M' )
                ( first_name = 'Guenther' gender = 'M' )
                ( first_name = 'Horst'  gender = 'M' )
                ( first_name = 'Matthias' gender = 'M' )
                ( first_name = 'Amelie'  gender = 'F' )
                ( first_name = 'Walter'  gender = 'M' )
                ( first_name = 'Sophie'  gender = 'F' )
                ( first_name = 'Claire'  gender = 'F' )
                ( first_name = 'Chantal' gender = 'F' )
                ( first_name = 'Jean'  gender = 'M' )
                ( first_name = 'Cindy'  gender = 'F' )
                ( first_name = 'Pierre'  gender = 'M' )
                ( first_name = 'Irene'  gender = 'F' )
                ( first_name = 'Adam'  gender = 'M' )
                ( first_name = 'Fabio'  gender = 'M' )
                ( first_name = 'Lothar'  gender = 'M' )
                ( first_name = 'Annemarie' gender = 'F' )
                ( first_name = 'Ida'  gender = 'F' )
                ( first_name = 'Roland'  gender = 'M' )
                ( first_name = 'Achim'  gender = 'M' )
                ( first_name = 'Allen'  gender = 'M' )
                ( first_name = 'Lee'  gender = 'M' )
                ( first_name = 'Guillermo' gender = 'M' )
                ( first_name = 'Florian' gender = 'M' )
                ( first_name = 'Ulla'  gender = 'F' )
                ( first_name = 'Juan'  gender = 'M' )
                ( first_name = 'Marta'  gender = 'F' )
                ( first_name = 'Salvador' gender = 'M' )
                ( first_name = 'Christine' gender = 'F' )
                ( first_name = 'Dominik' gender = 'M' )
                ( first_name = 'Astrid'  gender = 'F' )
                ( first_name = 'Ruth'  gender = 'F' )
                ( first_name = 'Theresia' gender = 'F' )
                ( first_name = 'Thomas'  gender = 'M' )
                ( first_name = 'Friedrich' gender = 'M' )
                ( first_name = 'Anneliese' gender = 'F' )
                ( first_name = 'Peter'  gender = 'M' )
                ( first_name = 'Anne-Marie' gender = 'F' )
                ( first_name = 'James'  gender = 'M' )
                ( first_name = 'Jean-Luc' gender = 'M' )
                ( first_name = 'Benjamin' gender = 'M' )
                ( first_name = 'Hendrik' gender = 'M' )
                ( first_name = 'Uli'  gender = 'F' )
                ( first_name = 'Siegfried' gender = 'M' )
                ( first_name = 'Max' gender = 'M' )
              ).

  ENDMETHOD.

  METHOD build_last_names.
    rt_data = VALUE tt_last_name( ##NO_TEXT ##STRING_OK
          ( last_name = 'Buchholm' )
          ( last_name = 'Vrsic' )
          ( last_name = 'Jeremias' )
          ( last_name = 'Gutenberg' )
          ( last_name = 'Fischmann' )
          ( last_name = 'Columbo' )
          ( last_name = 'Neubasler' )
          ( last_name = 'Martin' )
          ( last_name = 'Detemple' )
          ( last_name = 'Barth' )
          ( last_name = 'Benz' )
          ( last_name = 'Hansmann' )
          ( last_name = 'Koslowski' )
          ( last_name = 'Wohl' )
          ( last_name = 'Koller' )
          ( last_name = 'Hoffen' )
          ( last_name = 'Dumbach' )
          ( last_name = 'Goelke' )
          ( last_name = 'Waldmann' )
          ( last_name = 'Mechler' )
          ( last_name = 'Buehler' )
          ( last_name = 'Heller' )
          ( last_name = 'Simonen' )
          ( last_name = 'Henry' )
          ( last_name = 'Marshall' )
          ( last_name = 'Legrand' )
          ( last_name = 'Jacqmain' )
          ( last_name = 'D´Oultrement' )
          ( last_name = 'Hunter' )
          ( last_name = 'Delon' )
          ( last_name = 'Kreiss' )
          ( last_name = 'Trensch' )
          ( last_name = 'Cesari' )
          ( last_name = 'Matthaeus' )
          ( last_name = 'Babilon' )
          ( last_name = 'Zimmermann' )
          ( last_name = 'Kramer' )
          ( last_name = 'Illner' )
          ( last_name = 'Pratt' )
          ( last_name = 'Gahl' )
          ( last_name = 'Benjamin' )
          ( last_name = 'Miguel' )
          ( last_name = 'Weiss' )
          ( last_name = 'Sessler' )
          ( last_name = 'Montero' )
          ( last_name = 'Domenech' )
          ( last_name = 'Moyano' )
          ( last_name = 'Sommer' )
          ( last_name = 'Schneider' )
          ( last_name = 'Eichbaum' )
          ( last_name = 'Gueldenpfennig' )
          ( last_name = 'Sudhoff' )
          ( last_name = 'Lautenbach' )
          ( last_name = 'Ryan' )
          ( last_name = 'Prinz' )
          ( last_name = 'Deichgraeber' )
          ( last_name = 'Pan' )
          ( last_name = 'Lindwurm' )
          ( last_name = 'Kirk' )
          ( last_name = 'Picard' )
          ( last_name = 'Sisko' )
          ( last_name = 'Madeira' )
          ( last_name = 'Meier' )
          ( last_name = 'Rahn' )
          ( last_name = 'Leisert' )
          ( last_name = 'Müller' )
          ( last_name = 'Mustermann' )
          ( last_name = 'Becker' )
          ( last_name = 'Fischer' )
      ).
  ENDMETHOD.


  METHOD get_data.
    IF gt_data IS NOT INITIAL.
      rt_data = gt_data.
      EXIT.
    ENDIF.

    DATA(lt_names) = generate_names( ).
    DATA(lt_addresses) = generate_addresses( ).

    DATA(lo_random_phone_number) = cl_abap_random_int8=>create( min = cv_phone_number_min
                                                                max = cv_phone_number_max ).
    DATA(lo_random_city) = cl_abap_random_int=>create( min = 1  max = lines( lt_addresses ) ).
    DATA(lo_random_street_number) = cl_abap_random_int=>create( min = 1  max = 100 ).

    gt_data = VALUE tt_customer(
          FOR i = 1 THEN i + 1 WHILE i <= lines( lt_names ) LET j = lo_random_city->get_next( ) IN
               (
                  customer_id   = i
                  first_name    = lt_names[ i ]-first_name
                  last_name     = lt_names[ i ]-last_name
                  title         = lt_names[ i ]-title
                  street        = lt_addresses[ j ]-street && ` ` && lo_random_street_number->get_next( )
                  postal_code   = lt_addresses[ j ]-postal_code
                  city          = lt_addresses[ j ]-city
                  country_code  = lt_addresses[ j ]-country
                  phone_number  = '+' && COND string(
                                              WHEN lt_addresses[ j ]-country = 'AT' THEN '43'
                                              WHEN lt_addresses[ j ]-country = 'BE' THEN '32'
                                              WHEN lt_addresses[ j ]-country = 'CH' THEN '41'
                                              WHEN lt_addresses[ j ]-country = 'DE' THEN '49'
                                              WHEN lt_addresses[ j ]-country = 'ES' THEN '34'
                                              WHEN lt_addresses[ j ]-country = 'FR' THEN '33'
                                              WHEN lt_addresses[ j ]-country = 'IT' THEN '39'
                                              WHEN lt_addresses[ j ]-country = 'SI' THEN '386'
                                              WHEN lt_addresses[ j ]-country = 'US' THEN '1'
                                              ELSE '89'
                                            )
                                     && cv_phone_number_seperator
                                     && |{ replace( val = lo_random_phone_number->get_next( )  off = 3  len = 1  with = cv_phone_number_seperator ) }|
                  email_address = to_lower( lt_names[ i ]-first_name && `.` && lt_names[ i ]-last_name && `@` && cv_email_host && `.` && lt_addresses[ j ]-country )
               )
           ).
    rt_data = gt_data.
  ENDMETHOD.


  METHOD generate_names.
    DATA: lo_random_counter    TYPE REF TO cl_abap_random_int,
          lo_random_first_name TYPE REF TO cl_abap_random_int,
          lo_random_title      TYPE REF TO cl_abap_random_int,
          ls_first_name        TYPE lcl_customer_data_generator=>ty_first_name.

    DATA(lt_first_names) = build_first_names( ).

    lo_random_counter = cl_abap_random_int=>create( min = 5  max = 15 ).
    lo_random_first_name = cl_abap_random_int=>create( min = 1  max = lines( lt_first_names ) ).

    LOOP AT build_last_names( ) INTO DATA(ls_last_name).
      DO lo_random_counter->get_next( ) TIMES.
        ls_first_name = lt_first_names[ lo_random_first_name->get_next( ) ].
        APPEND VALUE ty_name(
            first_name = ls_first_name-first_name
            last_name  = ls_last_name-last_name
            title      = COND /dmo/title( WHEN ls_first_name-gender = 'M' THEN 'Mr.'
                                          WHEN ls_first_name-gender = 'F' THEN 'Mrs.'
                                          ELSE 'Martian' ) ##NO_TEXT
        ) TO rt_data.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_city.
    rt_data = VALUE tt_city( ##NO_TEXT ##STRING_OK
              ( country = 'DE' postal_code =    '23496' city = 'Dielheim' )
              ( country = 'SI' postal_code =     '1000' city = 'Ljubljana' )
              ( country = 'DE' postal_code =    '86343' city = 'Koenigsbrunn' )
              ( country = 'DE' postal_code =    '55128' city = 'Mainz' )
              ( country = 'DE' postal_code =    '11111' city = 'Berlin' )
              ( country = 'US' postal_code =    '17844' city = 'Washington' )
              ( country = 'AT' postal_code =     '4020' city = 'Linz' )
              ( country = 'DE' postal_code =    '68723' city = 'Schwetzingen' )
              ( country = 'DE' postal_code =    '68789' city = 'St. Leon-Rot' )
              ( country = 'DE' postal_code =    '66464' city = 'Homburg' )
              ( country = 'DE' postal_code =    '69231' city = 'Rauenberg' )
              ( country = 'DE' postal_code =    '69190' city = 'Walldorf' )
              ( country = 'DE' postal_code =    '58332' city = 'Schwelm' )
              ( country = 'DE' postal_code =    '64342' city = 'Seeheim-Jugenheim' )
              ( country = 'DE' postal_code =    '69121' city = 'Heidelberg' )
              ( country = 'DE' postal_code =    '63150' city = 'Heusenstamm' )
              ( country = 'DE' postal_code =    '64283' city = 'Darmstadt' )
              ( country = 'DE' postal_code =    '69207' city = 'Kurt' )
              ( country = 'DE' postal_code =    '79104' city = 'Freiburg' )
              ( country = 'DE' postal_code =    '79312' city = 'Emmendingen' )
              ( country = 'DE' postal_code =    '68753' city = 'Amelie' )
              ( country = 'DE' postal_code =    '68163' city = 'Mannheim' )
              ( country = 'DE' postal_code =    '67105' city = 'Schifferstadt' )
              ( country = 'DE' postal_code =    '68163' city = 'Mannheim-Lindenhof' )
              ( country = 'FR' postal_code =    '78140' city = 'Vélizy' )
              ( country = 'CH' postal_code =     '1211' city = 'Genève' )
              ( country = 'BE' postal_code = 'B - 1030' city = 'Bruxelles' )
              ( country = 'US' postal_code =    '76018' city = 'Arlington' )
              ( country = 'FR' postal_code =    '06130' city = 'Grasse' )
              ( country = 'DE' postal_code =    '27299' city = 'Langwedel' )
              ( country = 'DE' postal_code =    '69483' city = 'Wald-Michelbach' )
              ( country = 'IT' postal_code =    '00195' city = 'Roma' )
              ( country = 'DE' postal_code =    '81375' city = 'Muenchen' )
              ( country = 'DE' postal_code =    '67663' city = 'Kaiserslautern' )
              ( country = 'DE' postal_code =    '66386' city = 'St. Ingbert' )
              ( country = 'DE' postal_code =    '79761' city = 'Waldshut' )
              ( country = 'DE' postal_code =    '76137' city = 'Karlsruhe' )
              ( country = 'US' postal_code =    '07666' city = 'Teaneck' )
              ( country = 'US' postal_code =    '17758' city = 'N. Massapequa' )
              ( country = 'US' postal_code =    '09765' city = 'Boulder' )
              ( country = 'ES' postal_code =    '28020' city = 'Madrid' )
              ( country = 'DE' postal_code =    '69180' city = 'Wiesloch' )
              ( country = 'ES' postal_code =    '08014' city = 'Barcelona' )
              ( country = 'ES' postal_code =    '41006' city = 'Sevilla' )
              ( country = 'DE' postal_code =    '75305' city = 'Neuenburg' )
              ( country = 'DE' postal_code =    '41466' city = 'Neuss' )
              ( country = 'DE' postal_code =    '71116' city = 'Gaertringen' )
              ( country = 'US' postal_code =    '60657' city = 'Chicago' )
              ( country = 'DE' postal_code =    '63263' city = 'Neu-Isenburg' )
              ( country = 'DE' postal_code =    '23056' city = 'Buxtehude' )
              ( country = 'DE' postal_code =    '16233' city = 'Potsdam' )
              ( country = 'DE' postal_code =    '90419' city = 'Nuernberg' )
              ( country = 'US' postal_code =    '22334' city = 'San Francisco' )
              ( country = 'FR' postal_code =    '75839' city = 'Paris' )
              ( country = 'US' postal_code =    '63728' city = 'New Orleans' )
              ( country = 'DE' postal_code =    '75757' city = 'Elsenz' )
              ( country = 'DE' postal_code =    '70111' city = 'Reutlingen' )
              ( country = 'DE' postal_code =    '15344' city = 'Strausberg' )
          ).
  ENDMETHOD.


  METHOD generate_addresses.
    TYPES: BEGIN OF ty_random_street,
             country TYPE land1,
             random  TYPE REF TO cl_abap_random_int,
           END OF ty_random_street,
           tt_random_street TYPE STANDARD TABLE OF ty_random_street WITH KEY country.

    DATA: lo_random_counter TYPE REF TO cl_abap_random_int,
          lo_random_city    TYPE REF TO cl_abap_random_int,
          lt_random_street  TYPE tt_random_street,
          lo_phone_number   TYPE REF TO cl_abap_random_int.
    DATA(lt_street) = build_street( ).

    lo_random_counter = cl_abap_random_int=>create( min = 5  max = 15 ).
    lo_phone_number         = cl_abap_random_int=>create( min = 1  max = 100 ).
    LOOP AT lt_street INTO DATA(ls_street).
      APPEND VALUE ty_random_street(
          country = ls_street-country
          random  = cl_abap_random_int=>create( min = 1  max = lines( ls_street-streets ) )
      ) TO lt_random_street.
    ENDLOOP.

    LOOP AT build_city( ) INTO DATA(ls_city).
      DO lo_random_counter->get_next( ) TIMES.
        APPEND VALUE ty_address(
            country     = ls_city-country
            postal_code = ls_city-postal_code
            city        = ls_city-city
            street      = |{ lt_street[
                                   country = ls_city-country
                              ]-streets[
                                   lt_random_street[ country = ls_city-country ]-random->get_next( )
                              ] }|

        ) TO rt_data.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

  METHOD build_street.
    rt_data = VALUE tt_street( ##NO_TEXT ##STRING_OK
        ( country = 'AT' streets = VALUE tt_street_per_country( ( 'Hasnerstrasse' ) ) )
        ( country = 'BE' streets = VALUE tt_street_per_country( ( 'rue Voltaire' ) ) )
        ( country = 'CH' streets = VALUE tt_street_per_country( ( 'rue de Moillebeau' ) ) )
        ( country = 'DE' streets = VALUE tt_street_per_country( ( 'Akazienweg' )
              ( 'Albert-Schweitzer-Str.' )
              ( 'Alte Reichsstr.' )
              ( 'Am Deich' )
              ( 'Arionweg' )
              ( 'Arndtstrasse' )
              ( 'Auf dem Huegel' )
              ( 'Ausfallstr.' )
              ( 'Ausserhalb' )
              ( 'Carl-Metz Strasse' )
              ( 'Caspar-David-Friedrich-Str.' )
              ( 'Dudweilerstr.' )
              ( 'Elzstrasse' )
              ( 'Emil-Heckel-Str.' )
              ( 'Erlengrund' )
              ( 'Franz-Marc-Str.' )
              ( 'Friedensallee' )
              ( 'Froschstr.' )
              ( 'Gartenstr.' )
              ( 'Gemeindestr.' )
              ( 'Goeckinghofstr.' )
              ( 'Gruenlingweg' )
              ( 'Hauptstr.' )
              ( 'Heidelberger Str.' )
              ( 'Im Warmet' )
              ( 'Jacobistrasse' )
              ( 'Karl-Marx-Allee' )
              ( 'Karl-Schwaner-Str.' )
              ( 'Leichhof' )
              ( 'Lerchenstr.' )
              ( 'Marktplatz' )
              ( 'Max-Planck-Str.' )
              ( 'Meerfeldstr.' )
              ( 'Melissenstr.' )
              ( 'Muehltalstr.' )
              ( 'Mutterstadter Str.' )
              ( 'N7,' )
              ( 'Rankestr.' )
              ( 'Raupelsweg' )
              ( 'Schillerstr.' )
              ( 'Stauboernchenstrasse' )
              ( 'Stiftsbogen' )
              ( 'Waldmann' )
              ( 'Wilhelminentr.' )
              ( 'Zwischergasse' ) ) )
        ( country = 'ES' streets = VALUE tt_street_per_country( ( 'Camelias' )
              ( 'Fuenlabrada' )
              ( 'Piedad' )
              ( 'Pza. Pablo Ruiz Picasso' ) ) )
        ( country = 'FR' streets = VALUE tt_street_per_country( ( 'Rue Balzac' )
              ( 'route de Pégomas' )
              ( 'rue Nieuport' ) ) )
        ( country = 'IT' streets = VALUE tt_street_per_country( ( 'Via Giulio Cesare' ) ) )
        ( country = 'SI' streets = VALUE tt_street_per_country( ( 'Poklukarjeva' ) ) )
        ( country = 'US' streets = VALUE tt_street_per_country( ( '17th St.' )
              ( 'Federal Avenue' )
              ( 'Golden Gate Drive' )
              ( 'Lake Shore Drive' )
              ( 'Oak Street' )
              ( 'Sagamore St.' )
              ( 'Voodoo Avenue' )
              ( 'Windstone Drive' ) ) )
          ).
  ENDMETHOD.


ENDCLASS.

CLASS lcl_supplement_data_generator DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES: lif_data_generator.
    TYPES:
      tt_supplement               TYPE STANDARD TABLE OF /dmo/supplement  WITH KEY supplement_id,
      tt_supplement_text          TYPE STANDARD TABLE OF /dmo/suppl_text  WITH KEY supplement_id language_code,
      tt_supplement_category      TYPE STANDARD TABLE OF /dmo/supplcat    WITH KEY supplement_category,
      tt_supplement_category_text TYPE STANDARD TABLE OF /dmo/supplcat_t  WITH KEY supplement_category language_code.

    " Merged types
    TYPES BEGIN OF ty_supplement_complete.
    INCLUDE TYPE /dmo/supplement.
    TYPES language_code TYPE spras.
    TYPES description   TYPE /dmo/description.
    TYPES END OF ty_supplement_complete.

    TYPES tt_supplement_category_compl TYPE STANDARD TABLE OF /dmo/supplcat_t WITH KEY supplement_category language_code.

    TYPES tt_supplement_complete TYPE STANDARD TABLE OF ty_supplement_complete WITH KEY supplement_id WITH NON-UNIQUE SORTED KEY category COMPONENTS supplement_category.

    CLASS-METHODS:
      get_data
        RETURNING
          VALUE(rt_data) TYPE tt_supplement_complete.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF cs_supplement_category,
        beverage TYPE /dmo/supplement_category VALUE 'BV',
        meal     TYPE /dmo/supplement_category VALUE 'ML',
        luggage  TYPE /dmo/supplement_category VALUE 'LU',
        extra    TYPE /dmo/supplement_category VALUE 'EX',
      END OF cs_supplement_category.

    CONSTANTS:
      cv_numberrange_interval TYPE cl_numberrange_runtime=>nr_interval VALUE '01',
      cv_numberrange_object   TYPE cl_numberrange_runtime=>nr_object   VALUE '/DMO/SUPPL' ##NO_TEXT.

    CLASS-DATA:
      gt_supplement_category TYPE tt_supplement_category_compl,
      gt_data                TYPE lcl_supplement_data_generator=>tt_supplement_complete.

    CLASS-METHODS:
      set_numberrange_intervals,

      get_supplement_category
        RETURNING
          VALUE(rt_supplement_category) TYPE tt_supplement_category_compl.

ENDCLASS.

CLASS lcl_supplement_data_generator IMPLEMENTATION.

  METHOD lif_data_generator~create.
    IF out IS BOUND.  out->write( '--> Delete Content.' ) ##NO_TEXT.
    ENDIF.
    DELETE FROM:
      /dmo/supplement,                                  "#EC CI_NOWHERE
      /dmo/suppl_text,                                  "#EC CI_NOWHERE
      /dmo/supplcat,                                    "#EC CI_NOWHERE
      /dmo/supplcat_t.                                  "#EC CI_NOWHERE

    IF out IS BOUND.  out->write( '--> Defining Supplement Types.' ) ##NO_TEXT.
    ENDIF.
    gt_supplement_category = get_supplement_category( ).

    DATA(lt_type) = get_supplement_category( ).
    INSERT /dmo/supplcat    FROM TABLE @( CORRESPONDING tt_supplement_category(      lt_type ) ).
    INSERT /dmo/supplcat_t  FROM TABLE @( CORRESPONDING tt_supplement_category_text( lt_type ) ).

*   The number range has to be set after inserting values into /dmo/supplcat as it reads the data from the table to validate the sub-objects ...
    IF out IS BOUND.  out->write( '--> Set up Number Range Intervals.' ) ##NO_TEXT.
    ENDIF.
    set_numberrange_intervals( ).

    IF out IS BOUND.  out->write( '--> Build Content.' ) ##NO_TEXT.
    ENDIF.
    DATA(lt_data) = get_data( ).

    IF out IS BOUND.  out->write( '--> Insert Content.' ) ##NO_TEXT.
    ENDIF.
    INSERT /dmo/supplement  FROM TABLE @( CORRESPONDING tt_supplement(               lt_data ) ).
    INSERT /dmo/suppl_text  FROM TABLE @( CORRESPONDING tt_supplement_text(          lt_data ) ).
    IF out IS BOUND.  out->write( '--> Done.' ) ##NO_TEXT.
    ENDIF.
    IF out IS BOUND.  out->write( '--> Done.' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_data.
    IF gt_data IS INITIAL.
      GET TIME STAMP FIELD DATA(current_timestamp).
      gt_data = VALUE tt_supplement_complete(  ##NO_TEXT

        currency_code         = 'EUR'
        language_code         = 'E'
        local_created_by      = 'GENERATOR'
        local_last_changed_by = 'GENERATOR'
        local_created_at      = current_timestamp
        local_last_changed_at = current_timestamp
        last_changed_at       = current_timestamp

        " Beverages
        supplement_category = cs_supplement_category-beverage
        ( price =  '2.30'  description = 'Hot Chocolate' )
        ( price =  '7.50'  description = 'Alcohol free Champagne' )
        ( price =  '3.50'  description = 'Coke' )
        ( price =  '3.50'  description = 'Orange Lemonade' )
        ( price =  '3.50'  description = 'Apple Juice' )
        ( price =  '3.50'  description = 'Pear Juice' )
        ( price =  '3.50'  description = 'Mango Juice' )
        ( price =  '3.50'  description = 'Lemon Lemonade' )
        ( price =  '4.50'  description = 'Tomato Juice' )

        " Meals
        supplement_category = cs_supplement_category-meal
        ( price =  '3.00'  description = 'Black Forest Cake' )
        ( price =  '2.00'  description = 'Chocolate Cake' )
        ( price =  '1.50'  description = 'Apple Pie' )
        ( price =  '1.50'  description = 'Pear Pie' )
        ( price =  '8.00'  description = 'Nice Salad' )
        ( price =  '9.00'  description = 'Paris Salad' )
        ( price = '12.00'  description = 'Hamburg Salad with Eggs' )
        ( price = '25.00'  description = 'Quail with French Salad and Black Forest Cake' )
        ( price = '13.00'  description = 'Duck on Lettuce' )
        ( price =  '5.00'  description = 'Carpaccio' )
        ( price =  '7.00'  description = 'Seasonal Salad' )
        ( price = '16.00'  description = 'Hamburg Salad with Fresh Shrimps' )
        ( price = '17.00'  description = 'Quail' )
        ( price = '14.00'  description = 'Wiener Schnitzel' )
        ( price = '13.00'  description = 'Pork Schnitzel' )
        ( price = '14.00'  description = 'Schnitzel with Pepper Sauce' )
        ( price = '11.00'  description = 'Chicken and French Fries' )
        ( price = '12.00'  description = 'Turkey Steak' )
        ( price = '15.00'  description = 'Bavarian Duck' )
        ( price = '14.00'  description = 'Knuckle of Pork' )
        ( price = '22.00'  description = 'Fillet of Beef' )
        ( price = '21.00'  description = 'Trout Au Bleu' )
        ( price = '20.00'  description = 'Trout Meuniere' )
        ( price = '17.00'  description = 'Monkfish' )
        ( price = '12.00'  description = 'Sole' )
        ( price =  '6.00'  description = 'Mini Fried Sole' )
        ( price = '14.00'  description = 'Salmon in a Bearnaise Sauce' )
        ( price = '15.00'  description = 'Salmon Lasagne' )
        ( price =  '3.00'  description = 'Chocolate Ice Cream' )
        ( price =  '2.50'  description = 'Vanilla Ice Cream' )
        ( price =  '4.50'  description = 'Vanilla Ice Cream with Hot Cherries' )
        ( price =  '4.50'  description = 'Vanilla Ice Cream with Hot Raspberries' )
        ( price =  '4.00'  description = 'Apple Strudel' )
        ( price =  '4.00'  description = 'Raspberry Sorbet' )
        ( price =  '4.00'  description = 'Strawberry Sorbet' )
        ( price = '40.00'  description = 'Extra baggage 5 kgs' )

        "Luggage
        supplement_category = cs_supplement_category-luggage
        ( price = '15.00'  description = 'Luggage transfer from airport to hotel' )
        ( price = '75.00'  description = 'Luggage pickup from home and return ' )
        ( price = '80.00'  description = 'Bulky goods like sports equipment' )
      )  .


      LOOP AT gt_supplement_category INTO DATA(ls_supplement_category) WHERE language_code = 'E'.
        DATA(lv_lines) = lines( FILTER #( gt_data USING KEY category WHERE supplement_category = ls_supplement_category-supplement_category ) ).
        CHECK lv_lines > 0.
        TRY.
            cl_numberrange_runtime=>number_get(
              EXPORTING
                nr_range_nr = cv_numberrange_interval
                subobject   = CONV #( ls_supplement_category-supplement_category )
                object      = cv_numberrange_object
                quantity    = CONV cl_numberrange_runtime=>nr_quantity( lv_lines )
              IMPORTING
                number      = DATA(lv_key)
            ).
            DATA(lv_maximum) = CONV i( lv_key+2 ).
            DATA(lv_current) = lv_maximum - lv_lines.
            LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<data>) USING KEY category WHERE supplement_category = ls_supplement_category-supplement_category.
              lv_current += 1.
              <data>-supplement_id = |{ ls_supplement_category-supplement_category }-{ lv_current  ALIGN = RIGHT  PAD = `0`  WIDTH = 4 }|.
            ENDLOOP.
          CATCH cx_number_ranges INTO DATA(lx).
            " Should not happen.  If so, something is wrong
            RAISE SHORTDUMP lx.
        ENDTRY.
      ENDLOOP.

    ENDIF.

    rt_data = gt_data.
  ENDMETHOD.

  METHOD set_numberrange_intervals.

    CONSTANTS:
      cv_fromnumber TYPE cl_numberrange_intervals=>nr_nriv_line-fromnumber VALUE '0001',
      cv_tonumber   TYPE cl_numberrange_intervals=>nr_nriv_line-tonumber   VALUE '9999'.

    LOOP AT gt_supplement_category INTO DATA(ls_supplement_category).
      /dmo/cl_flight_data_generator=>reset_numberrange_interval(
        EXPORTING
          numberrange_object   = cv_numberrange_object
          numberrange_interval = cv_numberrange_interval
          subobject            = CONV #( ls_supplement_category-supplement_category )
          fromnumber           = cv_fromnumber
          tonumber             = cv_tonumber ).
    ENDLOOP.

  ENDMETHOD.

  METHOD get_supplement_category.
    rt_supplement_category = VALUE tt_supplement_category_compl( ##NO_TEXT
        language_code = 'E'
        ( supplement_category = cs_supplement_category-beverage  description = 'Beverage' )
        ( supplement_category = cs_supplement_category-meal      description = 'Meal'     )
        ( supplement_category = cs_supplement_category-luggage   description = 'Luggage'  )
        ( supplement_category = cs_supplement_category-extra     description = 'Extra'    )
      ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_status_vh_data_generator DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES: lif_data_generator.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      type_travel_status  TYPE STANDARD TABLE OF /dmo/trvl_stat_t WITH KEY travel_status  language,
      type_overall_status TYPE STANDARD TABLE OF /dmo/oall_stat_t WITH KEY overall_status language,
      type_booking_status TYPE STANDARD TABLE OF /dmo/book_stat_t WITH KEY booking_status language.

    CLASS-METHODS:
      _travel_status
        RETURNING VALUE(travel_status) TYPE type_travel_status,
      _overall_status
        RETURNING VALUE(overall_status) TYPE type_overall_status,
      _booking_status
        RETURNING VALUE(booking_status) TYPE type_booking_status.

ENDCLASS.

CLASS lcl_status_vh_data_generator IMPLEMENTATION.

  METHOD lif_data_generator~create.
    IF out IS BOUND.  out->write( '--> Delete Content.' ) ##NO_TEXT.
    ENDIF.
    DELETE FROM:
       /dmo/trvl_stat,                                  "#EC CI_NOWHERE
       /dmo/oall_stat,                                  "#EC CI_NOWHERE
       /dmo/book_stat,                                  "#EC CI_NOWHERE
       /dmo/trvl_stat_t,                                "#EC CI_NOWHERE
       /dmo/oall_stat_t,                                "#EC CI_NOWHERE
       /dmo/book_stat_t.                                "#EC CI_NOWHERE


    IF out IS BOUND.  out->write( '--> Travel Status' ) ##no_text.
    ENDIF.
    DATA(travel_status) = _travel_status( ).
    INSERT /dmo/trvl_stat   FROM TABLE @( CORRESPONDING #( travel_status ) ).
    INSERT /dmo/trvl_stat_t FROM TABLE @travel_status.

    IF out IS BOUND.  out->write( '--> Overall Status' ) ##no_text.
    ENDIF.
    DATA(overall_status) = _overall_status( ).
    INSERT /dmo/oall_stat   FROM TABLE @( CORRESPONDING #( overall_status ) ).
    INSERT /dmo/oall_stat_t FROM TABLE @overall_status.

    IF out IS BOUND.  out->write( '--> Booking Status' ) ##no_text.
    ENDIF.
    DATA(booking_status) = _booking_status( ).
    INSERT /dmo/book_stat   FROM TABLE @( CORRESPONDING #( booking_status ) ).
    INSERT /dmo/book_stat_t FROM TABLE @booking_status.

    IF out IS BOUND.  out->write( '--> Done.' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD _travel_status.
    travel_status = VALUE type_travel_status( ##NO_TEXT
        language = 'E'
          ( travel_status = 'N'  text = 'New'       )
          ( travel_status = 'B'  text = 'Booked'    )
          ( travel_status = 'P'  text = 'Planned'   )
          ( travel_status = 'X'  text = 'Canceled'  )
      ).
  ENDMETHOD.

  METHOD _overall_status.
    overall_status = VALUE type_overall_status( ##NO_TEXT
        language = 'E'
          ( overall_status = 'O'  text = 'Open'      )
          ( overall_status = 'A'  text = 'Accepted'  )
          ( overall_status = 'X'  text = 'Rejected'  )
      ).
  ENDMETHOD.

  METHOD _booking_status.
    booking_status = VALUE type_booking_status( ##NO_TEXT
        language = 'E'
          ( booking_status = 'N'  text = 'New'       )
          ( booking_status = 'B'  text = 'Booked'    )
          ( booking_status = 'X'  text = 'Canceled'  )
      ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_travel_data_generator DEFINITION DEFERRED.
CLASS /dmo/cl_flight_data_generator DEFINITION LOCAL FRIENDS lcl_travel_data_generator.
CLASS lcl_travel_data_generator DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES: lif_data_generator.

    TYPES: tt_travel              TYPE STANDARD TABLE OF /dmo/travel     WITH KEY travel_id,
           tt_bookings            TYPE STANDARD TABLE OF /dmo/booking    WITH KEY travel_id booking_id,
           tt_booking_supplements TYPE STANDARD TABLE OF /dmo/book_suppl WITH KEY travel_id booking_id booking_supplement_id.

    " Build nested tables
    TYPES BEGIN OF ty_booking_complete.
    INCLUDE TYPE /dmo/booking.
    TYPES booking_supplements TYPE tt_booking_supplements.
    TYPES END OF ty_booking_complete.

    TYPES tt_booking_complete TYPE STANDARD TABLE OF ty_booking_complete WITH KEY travel_id booking_id.

    TYPES BEGIN OF ty_travel_complete.
    INCLUDE TYPE /dmo/travel.
    TYPES bookings TYPE tt_booking_complete.
    TYPES END OF ty_travel_complete.

    "! <em>Travel</em> structure <br/>
    "! <em>Bookings</em> table <br/>
    "! --> <em>Booking</em> structure <br/>
    "! --> <em>Booking Supplement</em> table <br/>
    "! -----> <em>Booking Supplement</em> structure
    TYPES tt_travel_complete TYPE STANDARD TABLE OF ty_travel_complete WITH KEY travel_id.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_countryname,
             country     TYPE i_countrytext-country,
             countryname TYPE i_countrytext-countryname,
           END OF ty_countryname.

    CONSTANTS:
      cv_travel_group_amount_max     TYPE i VALUE 3,
      cv_trip_length_center          TYPE i VALUE 3,
      cv_trip_length_offset          TYPE i VALUE 2,
      cv_booking_date_min            TYPE i VALUE 0,
      cv_booking_date_max            TYPE i VALUE 20,
      cv_travel_create_dat_befor_min TYPE i VALUE 0,
      cv_travel_create_dat_befor_max TYPE i VALUE 40,
      cv_travel_change_date_min      TYPE i VALUE 0,
      cv_travel_change_date_max      TYPE i VALUE 10,
      cv_booking_supplement_amount   TYPE i VALUE 5,
      cv_booking_days_before         TYPE i VALUE 15,
      cv_numberrange_interval        TYPE cl_numberrange_runtime=>nr_interval VALUE '01',
      cv_numberrange_object          TYPE cl_numberrange_runtime=>nr_object   VALUE '/DMO/TRAVL' ##NO_TEXT.

    CLASS-DATA:
      go_random_seats                TYPE REF TO cl_abap_random_int,
      gt_flights_seats_decrease      TYPE lcl_flight_data_generator=>tt_flights,
      go_ran_travel_group_amount     TYPE REF TO cl_abap_random_int,
      go_ran_trip_length             TYPE REF TO cl_abap_random_int,
      go_ran_customer                TYPE REF TO cl_abap_random_int,
      gt_agency                      TYPE lcl_agency_data_generator=>tt_agency,
      gt_customer                    TYPE lcl_customer_data_generator=>tt_customer,
      gt_flights_final               TYPE lcl_flight_data_generator=>tt_flights,
      go_ran_booking_date            TYPE REF TO cl_abap_random_int,
      go_ran_agency                  TYPE REF TO cl_abap_random_int,
      gt_connections                 TYPE lcl_connection_data_generator=>tt_connection_additional_info,
      go_ran_booking_supplement_id   TYPE REF TO cl_abap_random_int,
      go_ran_booking_supplement_amnt TYPE REF TO cl_abap_random_int,
      gt_supplements                 TYPE lcl_supplement_data_generator=>tt_supplement_complete,
      go_ran_travel_description      TYPE REF TO cl_abap_random_int,
      go_ran_travel_create_dat_befor TYPE REF TO cl_abap_random_int,
      go_ran_travel_change_date      TYPE REF TO cl_abap_random_int,
      go_ran_hour                    TYPE REF TO cl_abap_random_int,
      go_ran_min_sec                 TYPE REF TO cl_abap_random_int,
      gt_airports                    TYPE lcl_airport_data_generator=>tt_airport,
      gt_countryname                 TYPE STANDARD TABLE OF ty_countryname WITH KEY country,
      go_ran_status_description      TYPE REF TO cl_abap_random_int,
      gt_last_names                  TYPE lcl_customer_data_generator=>tt_last_name,
      go_ran_last_name               TYPE REF TO cl_abap_random_int,
      go_ran_customer_travel         TYPE REF TO cl_abap_random_int,
      mv_datum                       TYPE d.

    CLASS-METHODS:
      get_data
        RETURNING
          VALUE(rt_data) TYPE tt_travel_complete,
      set_numberrange,
      build_booking
        RETURNING
          VALUE(rt_bookings) TYPE tt_booking_complete,
      build_dependend_content,
      find_next_fitting_flight
        IMPORTING
          iv_seats_required  TYPE i
          is_flight_previous TYPE /dmo/flight OPTIONAL
        RETURNING
          VALUE(rs_flight)   TYPE /dmo/flight,
      generate_booking_supplements
        IMPORTING
          iv_booking_id  TYPE /dmo/booking-booking_id
        RETURNING
          VALUE(rt_data) TYPE tt_booking_supplements,
      generate_description
        IMPORTING
          it_bookings           TYPE lcl_travel_data_generator=>tt_booking_complete
        RETURNING
          VALUE(rv_description) TYPE /dmo/travel-description,
      generate_random_time
        RETURNING
          VALUE(r_result) TYPE i,
      calculate_booking_fee
        IMPORTING
          it_bookings           TYPE lcl_travel_data_generator=>tt_booking_complete
        RETURNING
          VALUE(rv_booking_fee) TYPE /dmo/travel-booking_fee,
      generate_travel_customer_id
        IMPORTING
          it_bookings           TYPE lcl_travel_data_generator=>tt_booking_complete
        RETURNING
          VALUE(rv_customer_id) TYPE /dmo/travel-customer_id,
      set_today,
      calc_days_before_book_or_today
        IMPORTING
          iv_booking_date                   TYPE lcl_travel_data_generator=>ty_booking_complete-booking_date
        RETURNING
          VALUE(rv_travel_create_date_dats) TYPE d
        RAISING
          cx_parameter_invalid_range
          cx_parameter_invalid_type .

ENDCLASS.

CLASS lcl_travel_data_generator IMPLEMENTATION.

  METHOD lif_data_generator~create.
    DATA: lt_travels             TYPE tt_travel,
          lt_bookings            TYPE tt_bookings,
          lt_booking_supplements TYPE tt_booking_supplements.

    IF out IS BOUND.  out->write( '--> Delete Travel Content.' ) ##NO_TEXT.
    ENDIF.
    DELETE FROM /dmo/travel.                            "#EC CI_NOWHERE
    IF out IS BOUND.  out->write( '--> Delete Booking Content.' ) ##NO_TEXT.
    ENDIF.
    DELETE FROM /dmo/booking.                           "#EC CI_NOWHERE
    IF out IS BOUND.  out->write( '--> Delete Booking Supplement Content.' ) ##NO_TEXT.
    ENDIF.
    DELETE FROM /dmo/book_suppl.                        "#EC CI_NOWHERE

    IF out IS BOUND.  out->write( '--> Set Numberranges.' ) ##NO_TEXT.
    ENDIF.
    set_numberrange( ).

    IF out IS BOUND.  out->write( '--> Build Content.' ) ##NO_TEXT.
    ENDIF.
    DATA(lt_data) = get_data(  ).

    IF out IS BOUND.  out->write( '--> Convert Content to Table Format' ) ##NO_TEXT.
    ENDIF.
    LOOP AT lt_data INTO DATA(ls_travel).
      APPEND CORRESPONDING /dmo/travel( ls_travel ) TO lt_travels.
      LOOP AT ls_travel-bookings INTO DATA(ls_booking).
        APPEND CORRESPONDING /dmo/booking( ls_booking ) TO lt_bookings.
        APPEND LINES OF ls_booking-booking_supplements TO lt_booking_supplements.
      ENDLOOP.
    ENDLOOP.

    IF out IS BOUND.  out->write( '--> Insert Travel Content' ) ##NO_TEXT.
    ENDIF.
    INSERT /dmo/travel FROM TABLE @lt_travels.
    IF out IS BOUND.  out->write( '--> Insert Booking Content' ) ##NO_TEXT.
    ENDIF.
    INSERT /dmo/booking FROM TABLE @lt_bookings.
    IF out IS BOUND.  out->write( '--> Insert Booking Supplement Content' ) ##NO_TEXT.
    ENDIF.
    INSERT /dmo/book_suppl FROM TABLE @lt_booking_supplements.

    IF out IS BOUND.  out->write( '--> Done.' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.


  METHOD get_data.
    DATA: lv_travel_id TYPE /dmo/booking-travel_id.

    build_dependend_content( ).
    set_today( ).

    DATA(lt_bookings) = build_booking( ).

    WHILE lt_bookings IS NOT INITIAL.
      DATA(lv_travel_create_date_dats) = calc_days_before_book_or_today( lt_bookings[ 1 ]-booking_date ).

      DATA(lv_booking_fee) = calculate_booking_fee( lt_bookings ).

      DATA(lastchanged_date) = lv_travel_create_date_dats.
      lastchanged_date = lastchanged_date + go_ran_travel_change_date->get_next( ).
      DATA(lastchangedat_stamp) = CONV timestampl( ( CONV string( lastchanged_date ) * 1000000 ) ).
      lastchangedat_stamp = lastchangedat_stamp + generate_random_time( ).

      APPEND VALUE ty_travel_complete(
          agency_id     = gt_agency[ go_ran_agency->get_next( ) ]-agency_id
          customer_id   = generate_travel_customer_id( lt_bookings )
          begin_date    = lt_bookings[ 1 ]-flight_date
          end_date      = lt_bookings[ lines( lt_bookings ) ]-flight_date
          booking_fee   = lv_booking_fee
          total_price   = lv_booking_fee + REDUCE /dmo/travel-total_price( INIT sum = 0
                                                          FOR booking IN lt_bookings
                                                          NEXT
                                                           sum = sum
                                                                 + booking-flight_price
                                                                 + REDUCE /dmo/flight_price( INIT sum_supplement = 0
                                                                                             FOR booking_supplement IN booking-booking_supplements
                                                                                             NEXT sum_supplement = sum_supplement + booking_supplement-price )
                                                        )
          currency_code = lt_bookings[ 1 ]-currency_code
          description   = generate_description( lt_bookings )
          status        = SWITCH /dmo/travel-status( go_ran_status_description->get_next( )
                                          WHEN 1 OR 2 THEN /dmo/if_flight_legacy=>travel_status-new
                                          WHEN 3      THEN /dmo/if_flight_legacy=>travel_status-booked
                                          WHEN 4      THEN /dmo/if_flight_legacy=>travel_status-planned  )
          createdby     = gt_last_names[ go_ran_last_name->get_next( ) ]-last_name
          createdat     = CONV timestampl( CONV string( lv_travel_create_date_dats ) * 1000000 + generate_random_time( ) )
          lastchangedby = gt_last_names[ go_ran_last_name->get_next( ) ]-last_name
          lastchangedat = lastchangedat_stamp
          bookings      = lt_bookings
      ) TO rt_data.

      lt_bookings = build_booking( ).
    ENDWHILE.

    DATA(lv_lines) = lines( rt_data ).
    CHECK lv_lines > 0.
    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING
            nr_range_nr = cv_numberrange_interval
            object      = cv_numberrange_object
            quantity    = CONV cl_numberrange_runtime=>nr_quantity( lv_lines )
          IMPORTING
            number      = DATA(lv_key)
        ).
        DATA(lv_maximum) = CONV i( lv_key+2 ).
        DATA(lv_current) = lv_maximum - lv_lines.

        LOOP AT rt_data ASSIGNING FIELD-SYMBOL(<travel>).
          lv_current += 1.
          <travel>-travel_id = CONV #( lv_current ).
          LOOP AT <travel>-bookings ASSIGNING FIELD-SYMBOL(<booking>).
            <booking>-travel_id = CONV #( lv_current ).
            LOOP AT <booking>-booking_supplements ASSIGNING FIELD-SYMBOL(<booking_supplement>).
              <booking_supplement>-travel_id = CONV #( lv_current ).
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.
      CATCH cx_number_ranges INTO DATA(lx).
        " Should not happen.  If so, something is wrong
        RAISE SHORTDUMP lx.
    ENDTRY.
  ENDMETHOD.

  METHOD set_numberrange.

    /dmo/cl_flight_data_generator=>reset_numberrange_interval(
      EXPORTING
        numberrange_object   = cv_numberrange_object
        numberrange_interval = cv_numberrange_interval
        fromnumber           = CONV cl_numberrange_intervals=>nr_nriv_line-fromnumber( '00000001' )
        tonumber             = CONV cl_numberrange_intervals=>nr_nriv_line-tonumber( CONV /dmo/travel_id( /dmo/if_flight_legacy=>late_numbering_boundary - 1 ) ) ).

  ENDMETHOD.

  METHOD calc_days_before_book_or_today.
    cl_abap_tstmp=>td_add(
      EXPORTING
          date     = COND d( WHEN mv_datum > iv_booking_date
                               THEN iv_booking_date
                               ELSE mv_datum  )
          time     = CONV t( 0 )
          secs     = -86400 * cv_booking_days_before
      IMPORTING
          res_date = rv_travel_create_date_dats  ).
  ENDMETHOD.



  METHOD set_today.
    GET TIME STAMP FIELD DATA(current_timestamp).
    DATA(tmp) = CONV string( current_timestamp ).
    mv_datum = tmp(8).
    mv_datum = mv_datum - 1.
  ENDMETHOD.



  METHOD generate_random_time.
    r_result = go_ran_hour->get_next( ) * 10000 + go_ran_min_sec->get_next( ) * 100 + go_ran_min_sec->get_next( ).
  ENDMETHOD.


  METHOD build_dependend_content.
    go_ran_hour = cl_abap_random_int=>create( min = 0   max = 23 ).
    go_ran_min_sec = cl_abap_random_int=>create( min = 0   max = 59 ).

    gt_agency = lcl_agency_data_generator=>get_data( ).
    go_ran_agency = cl_abap_random_int=>create( min = 1  max = lines( gt_agency ) ).

    gt_customer = lcl_customer_data_generator=>get_data( ).
    go_ran_customer = cl_abap_random_int=>create( min = 1  max = lines( gt_customer ) ).
    gt_last_names = lcl_customer_data_generator=>build_last_names( ).
    go_ran_last_name = cl_abap_random_int=>create( min = 1  max = lines( gt_last_names ) ).
    go_ran_customer_travel = cl_abap_random_int=>create( min = 1  max = 4 ).

    gt_connections = lcl_connection_data_generator=>get_data( ).

    gt_airports = lcl_airport_data_generator=>get_data( ).

    SELECT FROM i_countrytext FIELDS country, countryname WHERE language = 'E' INTO TABLE @gt_countryname.

    gt_flights_final = lcl_flight_data_generator=>get_data( ).
    SORT gt_flights_final BY flight_date ASCENDING.
    gt_flights_seats_decrease = lcl_flight_data_generator=>get_data( ).
    SORT gt_flights_seats_decrease BY flight_date ASCENDING.

    go_ran_travel_group_amount = cl_abap_random_int=>create( min = 1  max = cv_travel_group_amount_max ).
    go_ran_trip_length = cl_abap_random_int=>create( min = cv_trip_length_center - cv_trip_length_offset  max = cv_trip_length_center + cv_trip_length_offset ).
    go_ran_booking_date = cl_abap_random_int=>create( min = cv_booking_date_min   max = cv_booking_date_max ).
    go_ran_travel_create_dat_befor = cl_abap_random_int=>create( min = cv_booking_date_min   max = cv_booking_date_max ).
    go_ran_travel_change_date = cl_abap_random_int=>create( min = cv_booking_date_min   max = cv_booking_date_max ).

    gt_supplements = lcl_supplement_data_generator=>get_data( ).
    go_ran_booking_supplement_id = cl_abap_random_int=>create( min = 1  max = lines( lcl_supplement_data_generator=>get_data( ) ) ).
    go_ran_booking_supplement_amnt = cl_abap_random_int=>create( min = 0  max = cv_booking_supplement_amount ).

    go_ran_status_description = cl_abap_random_int=>create( min = 1  max = 4 ).
    go_ran_travel_description = cl_abap_random_int=>create( min = 0  max = 9 ).
  ENDMETHOD.


  METHOD build_booking.
    TYPES: tt_customer_id TYPE TABLE OF /dmo/customer-customer_id WITH EMPTY KEY.
    DATA: lv_booking_id      TYPE /dmo/booking-booking_id,
          lt_customer_id     TYPE tt_customer_id,
          lv_customer_amount TYPE i.

    DATA(lv_trip_length) = go_ran_trip_length->get_next( ).


    lt_customer_id = VALUE tt_customer_id(
                                  FOR i = 1  THEN i + 1  WHILE i <= go_ran_travel_group_amount->get_next( )
                                  LET j = go_ran_customer->get_next( ) IN
                                   ( gt_customer[ j ]-customer_id )
                              ).
    lt_customer_id = CORRESPONDING tt_customer_id( lt_customer_id DISCARDING DUPLICATES ).
    lv_customer_amount = lines( lt_customer_id ).

    lv_booking_id =  0.

    DATA(ls_flight) = find_next_fitting_flight( iv_seats_required = lv_customer_amount ).
    CHECK ls_flight IS NOT INITIAL.


    DO lv_trip_length TIMES.

      READ TABLE gt_flights_seats_decrease
        WITH KEY key_sorted_date
        COMPONENTS
          carrier_id    = ls_flight-carrier_id
          connection_id = ls_flight-connection_id
          flight_date   = ls_flight-flight_date
        ASSIGNING FIELD-SYMBOL(<flight>).

      DATA(lv_booking_date) = CONV /dmo/booking-booking_date( <flight>-flight_date - go_ran_booking_date->get_next( ) ).

      DATA(lv_price) = /dmo/cl_flight_data_generator=>calculate_flight_price(
                                      iv_flight_distance = gt_connections[ carrier_id = <flight>-carrier_id connection_id = <flight>-connection_id ]-distance
                                      iv_seats_occupied_percent = ( gt_flights_final[ KEY primary_key ##PRIMKEY[KEY_SORTED_DATE]
                                                                                      carrier_id    = <flight>-carrier_id
                                                                                      connection_id = <flight>-connection_id
                                                                                      flight_date   = <flight>-flight_date
                                                                                    ]-seats_occupied
                                                                     - <flight>-seats_occupied
                                                                   ) * 100 DIV <flight>-seats_max
                                   ).

      <flight>-seats_occupied = <flight>-seats_occupied - lv_customer_amount.

      APPEND LINES OF VALUE tt_booking_complete(
      FOR i = 1  THEN i + 1  WHILE i <= lines( lt_customer_id )
         (
            booking_id          = lv_booking_id + i
            booking_date        = lv_booking_date
            customer_id         = lt_customer_id[ i ]
            carrier_id          = <flight>-carrier_id
            connection_id       = <flight>-connection_id
            flight_date         = <flight>-flight_date
            flight_price        = lv_price
            currency_code       = <flight>-currency_code
            booking_supplements = generate_booking_supplements( CONV /dmo/booking-booking_id( lv_booking_id + i ) )
          )
      ) TO rt_bookings.

      lv_booking_id = lv_booking_id + lines( lt_customer_id ).

      ls_flight = find_next_fitting_flight(
                iv_seats_required = lv_customer_amount
                is_flight_previous = <flight>
           ).
      IF ls_flight IS INITIAL.
        EXIT.
      ENDIF.

    ENDDO.
  ENDMETHOD.


  METHOD find_next_fitting_flight.
    DATA(lt_flights_filtered) = FILTER lcl_flight_data_generator=>tt_flights(
                                          gt_flights_seats_decrease
                                             USING KEY key_sorted_seats
                                             WHERE seats_occupied >= iv_seats_required
                                       ).

    CHECK lt_flights_filtered IS NOT INITIAL.

    IF is_flight_previous IS SUPPLIED.
      DATA(lv_connection_id_new) = VALUE /dmo/connection-connection_id( gt_connections[
                                                        airport_from_id = gt_connections[
                                                                                 connection_id = is_flight_previous-connection_id
                                                                             ]-airport_to_id
                                                    ]-connection_id
                                          OPTIONAL ).
      CHECK lv_connection_id_new IS NOT INITIAL.
      DATA(lt_flights_filtered2) = FILTER lcl_flight_data_generator=>tt_flights(
                                                   lt_flights_filtered
                                                   USING KEY key_sorted_date
                                                   WHERE connection_id = lv_connection_id_new
                                                     AND flight_date >= is_flight_previous-flight_date
                                              ).

      CHECK lt_flights_filtered2 IS NOT INITIAL.
      rs_flight = lt_flights_filtered2[ 1 ].
    ELSE.
      rs_flight = lt_flights_filtered[ 1 ].
    ENDIF.
  ENDMETHOD.


  METHOD generate_booking_supplements.
    rt_data = VALUE tt_booking_supplements(
        FOR     i = 1
          THEN  i + 1
          WHILE i <= go_ran_booking_supplement_amnt->get_next( )
          LET   j =  go_ran_booking_supplement_id->get_next( ) IN
          (
            booking_id            = iv_booking_id
            booking_supplement_id = i
            supplement_id         = gt_supplements[ j ]-supplement_id
            price                 = gt_supplements[ j ]-price
            currency_code         = gt_supplements[ j ]-currency_code
          )
    ).
  ENDMETHOD.


  METHOD generate_description.
    TYPES: tt_customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.
    rv_description = SWITCH /dmo/travel-description(
                               go_ran_travel_description->get_next( )
                                  WHEN 1 THEN `Business Trip for ` &&
                                                REDUCE /dmo/travel-description(
                                                               LET travelers = CORRESPONDING tt_customers( it_bookings DISCARDING DUPLICATES MAPPING customer_id = customer_id ) IN
                                                               INIT s = `` i = 1
                                                               FOR traveler IN travelers
                                                               NEXT s =  s && gt_customer[ customer_id = traveler-customer_id ]-first_name
                                                                           && COND /dmo/travel-description( WHEN i < lines( it_bookings ) THEN `, ` )
                                                                    i = i + 1 )
                                  WHEN 2 THEN `Vacation for ` &&
                                                REDUCE /dmo/travel-description(
                                                               LET travelers2 = CORRESPONDING tt_customers( it_bookings DISCARDING DUPLICATES MAPPING customer_id = customer_id ) IN
                                                               INIT s = `` i = 1
                                                               FOR traveler IN travelers2
                                                               NEXT s =  s && gt_customer[ customer_id = traveler-customer_id ]-first_name
                                                                           && COND /dmo/travel-description( WHEN i < lines( it_bookings ) THEN `, ` )
                                                                    i = i + 1 )
                                  WHEN 3 THEN `Business Trip to ` &&
                                               gt_countryname[
                                                  country = gt_airports[
                                                                  airport_id = gt_connections[
                                                                                     carrier_id = it_bookings[ 1 ]-carrier_id  connection_id = it_bookings[ 1 ]-connection_id
                                                                                  ]-airport_to_id
                                                              ]-country
                                               ]-countryname
                                  WHEN 4 THEN `Vacation to ` &&
                                               gt_countryname[
                                                  country = gt_airports[
                                                                  airport_id = gt_connections[
                                                                                     carrier_id = it_bookings[ 1 ]-carrier_id  connection_id = it_bookings[ 1 ]-connection_id
                                                                                  ]-airport_to_id
                                                              ]-country
                                               ]-countryname

                                  WHEN 5 THEN `Sightseeing in ` && gt_airports[
                                                                      airport_id = gt_connections[
                                                                                         carrier_id = it_bookings[ 1 ]-carrier_id  connection_id = it_bookings[ 1 ]-connection_id
                                                                                      ]-airport_to_id
                                                                     ]-city
                                  WHEN 6 THEN `Visiting ` && gt_customer[ go_ran_customer->get_next( ) ]-first_name
                                  WHEN 7 THEN `Business Trip`
                                  ELSE `Vacation`
                        )  ##NO_TEXT.
  ENDMETHOD.


  METHOD calculate_booking_fee.
    rv_booking_fee = 10 * lines( it_bookings ).
  ENDMETHOD.


  METHOD generate_travel_customer_id.
    rv_customer_id = COND /dmo/travel-customer_id(
                        WHEN go_ran_customer_travel->get_next( ) = 1
                          THEN gt_customer[ go_ran_customer->get_next( ) ]-customer_id
                          ELSE it_bookings[ 1 ]-customer_id
      ).
  ENDMETHOD.

ENDCLASS.
