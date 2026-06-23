class /dmo/cl_skeleton_provider definition PUBLIC create private.

  PUBLIC SECTION.
    TYPES agencies                    TYPE STANDARD TABLE OF /dmo/agency WITH KEY agency_id.
    TYPES supplement_categories_compl TYPE STANDARD TABLE OF /dmo/supplcat_t WITH KEY supplement_category language_code
      WITH NON-UNIQUE SORTED KEY language COMPONENTS language_code.
    TYPES BEGIN OF supplement_complete.
            INCLUDE TYPE /dmo/supplement.
    TYPES   product_id    TYPE string.
    TYPES END OF supplement_complete.
    TYPES BEGIN OF suppl_desc_complete.
            INCLUDE TYPE /dmo/suppl_text.
    TYPES   product_id TYPE string.
    TYPES END OF suppl_desc_complete.
    TYPES supplements_complete TYPE STANDARD TABLE OF supplement_complete
      WITH KEY supplement_id
      WITH NON-UNIQUE SORTED KEY category COMPONENTS supplement_category
      WITH NON-UNIQUE SORTED KEY p_id COMPONENTS product_id.
    TYPES suppls_desc_complete TYPE STANDARD TABLE OF suppl_desc_complete
      WITH KEY supplement_id
      WITH NON-UNIQUE SORTED KEY language COMPONENTS language_code.

    CONSTANTS:
      BEGIN OF supplement_category,
        beverage TYPE /dmo/supplement_category VALUE 'BV',
        meal     TYPE /dmo/supplement_category VALUE 'ML',
        luggage  TYPE /dmo/supplement_category VALUE 'LU',
        extra    TYPE /dmo/supplement_category VALUE 'EX',
      END OF supplement_category.

    CONSTANTS: BEGIN OF language_enum,
                 e TYPE spras VALUE 'E',
                 d TYPE spras VALUE 'D',
               END OF language_enum.

    CLASS-METHODS get_agencies              RETURNING VALUE(result) TYPE agencies.
    CLASS-METHODS get_supplcats             RETURNING VALUE(result) TYPE supplement_categories_compl.
    CLASS-METHODS get_supplements_localized RETURNING VALUE(result) TYPE supplements_complete.
    CLASS-METHODS get_suppl_desc_localized  RETURNING VALUE(result) TYPE suppls_desc_complete.

  protected section.
  private section.

ENDCLASS.



CLASS /DMO/CL_SKELETON_PROVIDER IMPLEMENTATION.


  method get_agencies.
    RETURN VALUE agencies(  ##NO_TEXT
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
  endmethod.

  METHOD get_supplcats.

    RETURN VALUE supplement_categories_compl( ##NO_TEXT
        language_code = language_enum-e
        ( supplement_category = supplement_category-beverage  description = 'Beverage' )
        ( supplement_category = supplement_category-meal      description = 'Meal'     )
        ( supplement_category = supplement_category-luggage   description = 'Luggage'  )
        ( supplement_category = supplement_category-extra     description = 'Extra'    )
        language_code = language_enum-d
        ( supplement_category = supplement_category-beverage  description = 'Getränk'      )
        ( supplement_category = supplement_category-meal      description = 'Mahlzeit'     )
        ( supplement_category = supplement_category-luggage   description = 'Gepäck'       )
        ( supplement_category = supplement_category-extra     description = 'Zusätzliches' )
      ).

  ENDMETHOD.
  METHOD GET_SUPPLEMENTS_LOCALIZED.

    RETURN VALUE supplements_complete(  ##NO_TEXT
      currency_code         = 'EUR'

      " Beverages
      supplement_category = supplement_category-beverage
      ( price =  '2.30' product_id = 'hot_chocolate' )
      ( price =  '7.50' product_id = 'alcohol_free_champagne' )
      ( price =  '3.50' product_id = 'coke' )
      ( price =  '3.50' product_id = 'orange_lemonade' )
      ( price =  '3.50' product_id = 'apple_juice' )
      ( price =  '3.50' product_id = 'pear_juice' )
      ( price =  '3.50' product_id = 'mango_juice' )
      ( price =  '3.50' product_id = 'lemon_lemonade' )
      ( price =  '4.50' product_id = 'tomato_juice' )

      " Meals
      supplement_category = supplement_category-meal
      ( price =  '3.00' product_id = 'black_forest_cake' )
      ( price =  '2.00' product_id = 'chocolate_cake' )
      ( price =  '1.50' product_id = 'apple_pie' )
      ( price =  '1.50' product_id = 'pear_pie' )
      ( price =  '8.00' product_id = 'nice_salad' )
      ( price =  '9.00' product_id = 'paris_salad' )
      ( price = '12.00' product_id = 'hamburg_salad_with_eggs' )
      ( price = '25.00' product_id = 'quail_with_french_salad_and_black_forest_cake' )
      ( price = '13.00' product_id = 'duck_on_lettuce' )
      ( price =  '5.00' product_id = 'carpaccio' )
      ( price =  '7.00' product_id = 'seasonal_salad' )
      ( price = '16.00' product_id = 'hamburg_salad_with_fresh_shrimps' )
      ( price = '17.00' product_id = 'quail' )
      ( price = '14.00' product_id = 'wiener_schnitzel' )
      ( price = '13.00' product_id = 'pork_schnitzel' )
      ( price = '14.00' product_id = 'schnitzel_with_pepper_sauce' )
      ( price = '11.00' product_id = 'chicken_and_french_fries' )
      ( price = '12.00' product_id = 'turkey_steak' )
      ( price = '15.00' product_id = 'bavarian_duck' )
      ( price = '14.00' product_id = 'knuckle_of_pork' )
      ( price = '22.00' product_id = 'fillet_of_beef' )
      ( price = '21.00' product_id = 'trout_au_bleu' )
      ( price = '20.00' product_id = 'trout_meuniere' )
      ( price = '17.00' product_id = 'monkfish' )
      ( price = '12.00' product_id = 'sole' )
      ( price =  '6.00' product_id = 'mini_fried_sole' )
      ( price = '14.00' product_id = 'salmon_in_a_bearnaise_sauce' )
      ( price = '15.00' product_id = 'salmon_lasagne' )
      ( price =  '3.00' product_id = 'chocolate_ice_cream' )
      ( price =  '2.50' product_id = 'vanilla_ice_cream' )
      ( price =  '4.50' product_id = 'vanilla_ice_cream_with_hot_cherries' )
      ( price =  '4.50' product_id = 'vanilla_ice_cream_with_hot_raspberries' )
      ( price =  '4.00' product_id = 'apple_strudel' )
      ( price =  '4.00' product_id = 'raspberry_sorbet' )
      ( price =  '4.00' product_id = 'strawberry_sorbet' )
      ( price = '40.00' product_id = 'extra_baggage_5_kgs' )

      "Luggage
      supplement_category = supplement_category-luggage
      ( price = '15.00' product_id = 'luggage_transfer_from_airport_to_hotel' )
      ( price = '75.00' product_id = 'luggage_pickup_from_home_and_return' )
      ( price = '80.00' product_id = 'bulky_goods_like_sports_equipment' )
    )  .

  ENDMETHOD.

  METHOD get_suppl_desc_localized.
    RETURN VALUE suppls_desc_complete(
        ##NO_TEXT
        language_code = language_enum-e
        ( product_id = 'hot_chocolate'                                 description = 'Hot Chocolate' )
        ( product_id = 'alcohol_free_champagne'                        description = 'Alcohol free Champagne' )
        ( product_id = 'coke'                                          description = 'Coke' )
        ( product_id = 'orange_lemonade'                               description = 'Orange Lemonade' )
        ( product_id = 'apple_juice'                                   description = 'Apple Juice' )
        ( product_id = 'pear_juice'                                    description = 'Pear Juice' )
        ( product_id = 'mango_juice'                                   description = 'Mango Juice' )
        ( product_id = 'lemon_lemonade'                                description = 'Lemon Lemonade' )
        ( product_id = 'tomato_juice'                                  description = 'Tomato Juice' )
        ( product_id = 'black_forest_cake'                             description = 'Black Forest Cake' )
        ( product_id = 'chocolate_cake'                                description = 'Chocolate Cake' )
        ( product_id = 'apple_pie'                                     description = 'Apple Pie' )
        ( product_id = 'pear_pie'                                      description = 'Pear Pie' )
        ( product_id = 'nice_salad'                                    description = 'Nice Salad' )
        ( product_id = 'paris_salad'                                   description = 'Paris Salad' )
        ( product_id = 'hamburg_salad_with_eggs'                       description = 'Hamburg Salad with Eggs' )
        ( product_id = 'quail_with_french_salad_and_black_forest_cake' description = 'Quail with French Salad and Black Forest Cake' )
        ( product_id = 'duck_on_lettuce'                               description = 'Duck on Lettuce' )
        ( product_id = 'carpaccio'                                     description = 'Carpaccio' )
        ( product_id = 'seasonal_salad'                                description = 'Seasonal Salad' )
        ( product_id = 'hamburg_salad_with_fresh_shrimps'              description = 'Hamburg Salad with Fresh Shrimps' )
        ( product_id = 'quail'                                         description = 'Quail' )
        ( product_id = 'wiener_schnitzel'                              description = 'Wiener Schnitzel' )
        ( product_id = 'pork_schnitzel'                                description = 'Pork Schnitzel' )
        ( product_id = 'schnitzel_with_pepper_sauce'                   description = 'Schnitzel with Pepper Sauce' )
        ( product_id = 'chicken_and_french_fries'                      description = 'Chicken and French Fries' )
        ( product_id = 'turkey_steak'                                  description = 'Turkey Steak' )
        ( product_id = 'bavarian_duck'                                 description = 'Bavarian Duck' )
        ( product_id = 'knuckle_of_pork'                               description = 'Knuckle of Pork' )
        ( product_id = 'fillet_of_beef'                                description = 'Fillet of Beef' )
        ( product_id = 'trout_au_bleu'                                 description = 'Trout Au Bleu' )
        ( product_id = 'trout_meuniere'                                description = 'Trout Meuniere' )
        ( product_id = 'monkfish'                                      description = 'Monkfish' )
        ( product_id = 'sole'                                          description = 'Sole' )
        ( product_id = 'mini_fried_sole'                               description = 'Mini Fried Sole' )
        ( product_id = 'salmon_in_a_bearnaise_sauce'                   description = 'Salmon in a Bearnaise Sauce' )
        ( product_id = 'salmon_lasagne'                                description = 'Salmon Lasagne' )
        ( product_id = 'chocolate_ice_cream'                           description = 'Chocolate Ice Cream' )
        ( product_id = 'vanilla_ice_cream'                             description = 'Vanilla Ice Cream' )
        ( product_id = 'vanilla_ice_cream_with_hot_cherries'           description = 'Vanilla Ice Cream with Hot Cherries' )
        ( product_id = 'vanilla_ice_cream_with_hot_raspberries'        description = 'Vanilla Ice Cream with Hot Raspberries' )
        ( product_id = 'apple_strudel'                                 description = 'Apple Strudel' )
        ( product_id = 'raspberry_sorbet'                              description = 'Raspberry Sorbet' )
        ( product_id = 'strawberry_sorbet'                             description = 'Strawberry Sorbet' )
        ( product_id = 'extra_baggage_5_kgs'                           description = 'Extra baggage 5 kgs' )
        ( product_id = 'luggage_transfer_from_airport_to_hotel'        description = 'Luggage transfer from airport to hotel' )
        ( product_id = 'luggage_pickup_from_home_and_return'           description = 'Luggage pickup from home and return ' )
        ( product_id = 'bulky_goods_like_sports_equipment'             description = 'Bulky goods like sports equipment' )

        language_code = language_enum-d
        ( product_id = 'hot_chocolate'                                 description = 'Heiße Schokolade' )
        ( product_id = 'alcohol_free_champagne'                        description = 'Alkoholfreier Champagner' )
        ( product_id = 'coke'                                          description = 'Cola' )
        ( product_id = 'orange_lemonade'                               description = 'Orangenlimonade' )
        ( product_id = 'apple_juice'                                   description = 'Apfelsaft' )
        ( product_id = 'pear_juice'                                    description = 'Birnensaft' )
        ( product_id = 'mango_juice'                                   description = 'Mangosaft' )
        ( product_id = 'lemon_lemonade'                                description = 'Zitronenlimonade' )
        ( product_id = 'tomato_juice'                                  description = 'Tomatensaft' )
        ( product_id = 'black_forest_cake'                             description = 'Schwarzwälder Kirschtorte' )
        ( product_id = 'chocolate_cake'                                description = 'Schokoladenkuchen' )
        ( product_id = 'apple_pie'                                     description = 'Apfelkuchen' )
        ( product_id = 'pear_pie'                                      description = 'Birnenkuchen' )
        ( product_id = 'nice_salad'                                    description = 'Nizza Salat' )
        ( product_id = 'paris_salad'                                   description = 'Pariser Salat' )
        ( product_id = 'hamburg_salad_with_eggs'                       description = 'Hamburger Salat mit Eiern' )
        ( product_id = 'quail_with_french_salad_and_black_forest_cake' description = 'Wachtel mit französischem Salat und Schwarzwälder Kirschtorte' )
        ( product_id = 'duck_on_lettuce'                               description = 'Ente auf Blattsalat' )
        ( product_id = 'carpaccio'                                     description = 'Carpaccio' )
        ( product_id = 'seasonal_salad'                                description = 'Saisonaler Salat' )
        ( product_id = 'hamburg_salad_with_fresh_shrimps'              description = 'Hamburger Salat mit frischen Krabben' )
        ( product_id = 'quail'                                         description = 'Wachtel' )
        ( product_id = 'wiener_schnitzel'                              description = 'Wiener Schnitzel' )
        ( product_id = 'pork_schnitzel'                                description = 'Schweineschnitzel' )
        ( product_id = 'schnitzel_with_pepper_sauce'                   description = 'Schnitzel mit Pfeffersoße' )
        ( product_id = 'chicken_and_french_fries'                      description = 'Hähnchen mit Pommes frites' )
        ( product_id = 'turkey_steak'                                  description = 'Putensteak' )
        ( product_id = 'bavarian_duck'                                 description = 'Bayerische Ente' )
        ( product_id = 'knuckle_of_pork'                               description = 'Schweinshaxe' )
        ( product_id = 'fillet_of_beef'                                description = 'Rinderfilet' )
        ( product_id = 'trout_au_bleu'                                 description = 'Forelle blau' )
        ( product_id = 'trout_meuniere'                                description = 'Forelle Müllerin Art' )
        ( product_id = 'monkfish'                                      description = 'Seeteufel' )
        ( product_id = 'sole'                                          description = 'Seezunge' )
        ( product_id = 'mini_fried_sole'                               description = 'Mini-Seezunge gebraten' )
        ( product_id = 'salmon_in_a_bearnaise_sauce'                   description = 'Lachs in Sauce Béarnaise' )
        ( product_id = 'salmon_lasagne'                                description = 'Lachs-Lasagne' )
        ( product_id = 'chocolate_ice_cream'                           description = 'Schokoladeneis' )
        ( product_id = 'vanilla_ice_cream'                             description = 'Vanilleeis' )
        ( product_id = 'vanilla_ice_cream_with_hot_cherries'           description = 'Vanilleeis mit heißen Kirschen' )
        ( product_id = 'vanilla_ice_cream_with_hot_raspberries'        description = 'Vanilleeis mit heißen Himbeeren' )
        ( product_id = 'apple_strudel'                                 description = 'Apfelstrudel' )
        ( product_id = 'raspberry_sorbet'                              description = 'Himbeersorbet' )
        ( product_id = 'strawberry_sorbet'                             description = 'Erdbeersorbet' )
        ( product_id = 'extra_baggage_5_kgs'                           description = 'Zusätzliches Gepäck 5 kg' )
        ( product_id = 'luggage_transfer_from_airport_to_hotel'        description = 'Gepäcktransfer vom Flughafen zum Hotel' )
        ( product_id = 'luggage_pickup_from_home_and_return'           description = 'Gepäckabholung von zu Hause und Rücktransport' )
        ( product_id = 'bulky_goods_like_sports_equipment'             description = 'Sperrgut wie Sportausrüstung' ) ).
  ENDMETHOD.

ENDCLASS.
