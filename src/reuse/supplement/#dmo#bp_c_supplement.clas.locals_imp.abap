CLASS lhc_supplement DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS augment FOR MODIFY
      IMPORTING entities_create FOR CREATE Supplement
                entities_update FOR UPDATE Supplement.

ENDCLASS.

CLASS lhc_Supplement IMPLEMENTATION.

  METHOD augment.

    DATA: suppltext_for_new_suppl      TYPE TABLE FOR CREATE /DMO/I_Supplement\_SupplementText,
          suppltext_for_existing_suppl TYPE TABLE FOR CREATE /DMO/I_Supplement\_SupplementText,
          supplementtext_update        TYPE TABLE FOR UPDATE /DMO/I_SupplementText.
    DATA: relates_create TYPE abp_behv_relating_tab,
          relates_update TYPE abp_behv_relating_tab,
          relates_cba    TYPE abp_behv_relating_tab.
    DATA: suppl_text_tky_link TYPE STRUCTURE FOR READ LINK /DMO/I_Supplement\\Supplement\_SupplementText,
          suppl_text_tky      LIKE suppl_text_tky_link-target-%tky.

    "Handle create requests including SupplementDescription
    LOOP AT entities_create INTO DATA(supplement_create).
      "Count Table index for uniquely identifiably %cid on creating supplementtext
      APPEND sy-tabix TO relates_create.

      "Direct the Supplement Create to the corresponding SupplementText Create-By-Association using the current language
      APPEND VALUE #( %cid_ref           = supplement_create-%cid
                      %is_draft          = supplement_create-%is_draft
                      %key-supplementid  = supplement_create-%key-SupplementID
                      %target            = VALUE #( ( %cid              = |CREATETEXTCID{ sy-tabix }|
                                                      %is_draft         = supplement_create-%is_draft
                                                      languagecode      = sy-langu
                                                      description       = supplement_create-SupplementDescription
                                                      %control          = VALUE #( languagecode = if_abap_behv=>mk-on
                                                                                   description  = supplement_create-%control-SupplementDescription )
                                                   ) )
                     ) TO suppltext_for_new_suppl.

    ENDLOOP.

    MODIFY AUGMENTING ENTITIES OF /DMO/I_Supplement
      ENTITY Supplement
        CREATE BY \_SupplementText
        FROM suppltext_for_new_suppl
        RELATING TO entities_create BY relates_create.

    IF entities_update IS NOT INITIAL.

      READ ENTITIES OF /DMO/I_Supplement
        ENTITY Supplement BY \_SupplementText
          FROM CORRESPONDING #( entities_update )
          LINK DATA(link)
        FAILED DATA(link_failed).

      "To prevent empty updates, we check if something has changed for the text node.
      LOOP AT entities_update INTO DATA(supplement_update) WHERE %control-SupplementDescription = if_abap_behv=>mk-on.

        "To prevent an update on a text node for a non-existing supplement, we check against the failed-supplement table
        "but we also need to ensure that this instance could have been created in this request as well.
        CHECK NOT line_exists( link_failed-supplement[ KEY draft  %tky = CORRESPONDING #( supplement_update-%tky ) ] )
          OR line_exists( suppltext_for_new_suppl[ KEY cid  %cid_ref = supplement_update-%cid_ref  %is_draft = supplement_update-%is_draft ] ).

        DATA(tabix) = sy-tabix.

        "Create variable for %tky for target entity instances
        suppl_text_tky = CORRESPONDING #( supplement_update-%tky )  .
        suppl_text_tky-LanguageCode = sy-langu.

        "If a suppl_text with sy-langu already exists, perform an update. Else perform a create-by-association.
        IF line_exists( link[ KEY draft source-%tky  = CORRESPONDING #( supplement_update-%tky )
                                        target-%tky  = CORRESPONDING #( suppl_text_tky ) ] ).

          APPEND tabix TO relates_update.

          APPEND VALUE #( %tky             = suppl_text_tky
                          %cid_ref         = supplement_update-%cid_ref
                          description      = supplement_update-SupplementDescription
                          %control         = VALUE #( description = supplement_update-%control-SupplementDescription )
                        ) TO supplementtext_update.

          "If suppl_text was created in the current modify-statement, perform an update based on %cid
        ELSEIF line_exists(  suppltext_for_new_suppl[ KEY cid %is_draft = supplement_update-%is_draft
                                                              %cid_ref  = supplement_update-%cid_ref ] ).
          APPEND tabix TO relates_update.

          APPEND VALUE #( %tky             = suppl_text_tky
                          %cid_ref         = suppltext_for_new_suppl[ %is_draft = supplement_update-%is_draft
                                                                      %cid_ref  = supplement_update-%cid_ref ]-%target[ 1 ]-%cid
                          description      = supplement_update-SupplementDescription
                          %control         = VALUE #( description = supplement_update-%control-SupplementDescription )
                        ) TO supplementtext_update.

          "If suppl_text with sy-langu does not exist yet for corresponding supplement
        ELSE.
          APPEND tabix TO relates_cba.

          "Direct the Supplement Update to the corresponding SupplementText Create-By-Association using the current language
          APPEND VALUE #( %tky     = CORRESPONDING #( supplement_update-%tky )
                          %cid_ref = supplement_update-%cid_ref
                          %target  = VALUE #( ( %cid          = |UPDATETEXTCID{ tabix }|
                                                %is_draft     = suppl_text_tky-%is_draft
                                                languagecode  = suppl_text_tky-LanguageCode
                                                description   = supplement_update-SupplementDescription
                                                %control      = VALUE #( languagecode = if_abap_behv=>mk-on
                                                                         description  = supplement_update-%control-SupplementDescription )
                                             ) )
                         ) TO suppltext_for_existing_suppl.
        ENDIF.

      ENDLOOP.
    ENDIF.

    MODIFY AUGMENTING ENTITIES OF /DMO/I_Supplement
      ENTITY SupplementText
        UPDATE FROM supplementtext_update
        RELATING TO entities_update BY relates_update
      ENTITY Supplement
        CREATE BY \_SupplementText
        FROM suppltext_for_existing_suppl
        RELATING TO entities_update BY relates_cba.


  ENDMETHOD.


ENDCLASS.
