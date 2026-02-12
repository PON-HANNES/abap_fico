FUNCTION zfi_process_00002040.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_REGUH) LIKE  REGUH STRUCTURE  REGUH
*"  TABLES
*"      T_FIMSG STRUCTURE  FIMSG
*"  CHANGING
*"     VALUE(C_FINAA) LIKE  FINAA STRUCTURE  FINAA
*"----------------------------------------------------------------------
FIELD-SYMBOLS: <ls_fimsg> LIKE LINE OF t_fimsg.

  DATA:
    ls_kna1             TYPE kna1,
    ls_knb1             TYPE knb1,
    ls_lfa1             TYPE lfa1,
    ls_lfb1             TYPE lfb1.

  DATA:
    lv_koart LIKE bkorm-koart,     "Hilfsfeld Kontoart
    lv_parnr LIKE knb1-kunnr,      "Hilfsfeld Partnernummer
    lv_land1 LIKE kna1-land1,      "Hilfsfeld Faxanschluß
    lv_telfx LIKE kna1-telfx,      "Hilfsfeld Faxanschluß
    lv_zsabe LIKE knb1-zsabe,      "Hilfsfeld Faxanschluß
    lv_intad LIKE knb1-intad.      "Hilfsfeld Internet

  CLEAR: lv_land1,
         lv_telfx,
         lv_zsabe.

  IF     NOT i_reguh-lifnr IS INITIAL.
    lv_koart = 'K'.
    lv_parnr = i_reguh-lifnr.
  ELSEIF NOT i_reguh-kunnr IS INITIAL.
    lv_koart = 'D'.
    lv_parnr = i_reguh-kunnr.
  ENDIF.

  CASE lv_koart.
    WHEN 'D'.
**      SELECT SINGLE /sbk/fi_mavis
**                    INTO lv_mavis
**                    FROM knb1
**                   WHERE kunnr = lv_parnr
**                     AND bukrs = i_reguh-absbu.
    WHEN 'K'.
**      SELECT SINGLE /sbk/fi_mavis
**                    INTO lv_mavis
**                    FROM lfb1
**                   WHERE lifnr = lv_parnr
**                     AND bukrs = i_reguh-absbu.
    WHEN OTHERS.
*     Weitere Kontoarten sind nicht vorgesehen
**      CLEAR lv_mavis.
  ENDCASE.

**  IF lv_mavis = 'X'.

*   Stammsätze nur lesen, wenn Alternativmedium hinterlegt
    IF lv_koart = 'D'.
      SELECT SINGLE *
                    FROM kna1
                    INTO ls_kna1
                   WHERE kunnr = i_reguh-kunnr.
      IF sy-subrc EQ 0.
        lv_land1 = ls_kna1-land1.
        SELECT SINGLE *
                      FROM knb1
                      INTO ls_knb1
                     WHERE kunnr = i_reguh-kunnr
                       AND bukrs = i_reguh-absbu.
        IF sy-subrc EQ 0.
          lv_telfx = ls_knb1-tlfxs.
          lv_zsabe = ls_knb1-zsabe.
          lv_intad = ls_knb1-intad.
        ENDIF.
      ENDIF.
    ELSE.
      SELECT SINGLE *
                    FROM lfa1 INTO ls_lfa1
                   WHERE lifnr = i_reguh-lifnr.
      IF sy-subrc EQ 0.
        lv_land1 = ls_lfa1-land1.
        IF sy-subrc EQ 0.
          SELECT SINGLE *
                        FROM lfb1
                        INTO ls_lfb1
                       WHERE lifnr = i_reguh-lifnr
                         AND bukrs = i_reguh-absbu.
          lv_telfx = ls_lfb1-tlfxs.
          lv_zsabe = ls_lfb1-zsabe.
          lv_intad = ls_lfb1-intad.
        ENDIF.
      ENDIF.
    ENDIF.
*   FI-NachrichtenArt füllen oder MSG falls keine eMail-bzw. FaxNr
    c_finaa-nacha  = 'I'.
    IF c_finaa-nacha = 'I'.                 "Ausgabe über Internet
      IF lv_intad NE space.
        IF i_reguh-xvorl IS INITIAL.
          c_finaa-intad    = lv_intad.
**          IF lv_mail_outbox_link EQ abap_true.
***           eMail im Business Workplace des Absenders ablegen
**            c_finaa-mail_outbox_link = lv_mail_outbox_link.
**          ENDIF.
*         finaa-textf      = 'PDF'.
*         Es wird eine Mail-Nachricht an & mit der Adresse & erstellt.
          MESSAGE ID '/SBK/FI_BTE' TYPE 'S' NUMBER 302
                  WITH lv_parnr lv_intad.
* --> Vorschlagslauf: Es wurde keine Mail-Nachricht an & erstellt.
        ELSE.
          IF 1 = 2.
            MESSAGE w300(/sbk/fi_bte) WITH lv_intad.
          ENDIF.
          APPEND INITIAL LINE TO t_fimsg ASSIGNING <ls_fimsg>.
          <ls_fimsg>-msgid = '/SBK/FI_BTE'.
          <ls_fimsg>-msgty = 'W'.
          <ls_fimsg>-msgno = 300.
          <ls_fimsg>-msgv1 = lv_intad.
          c_finaa-nacha    = '1'.
        ENDIF.
      ELSE.
* --> E-Mail Adresse des Geschäftspartners &1 nicht vorhanden
        IF 1 = 2.
          MESSAGE w262(ftr0) WITH lv_parnr.
        ENDIF.
        APPEND INITIAL LINE TO t_fimsg ASSIGNING <ls_fimsg>.
        <ls_fimsg>-msgid = 'FTR0'.
        <ls_fimsg>-msgty = 'W'.
        <ls_fimsg>-msgno = '262'.
        <ls_fimsg>-msgv1 = lv_parnr.
        c_finaa-nacha = '1'.
      ENDIF.
    ELSEIF c_finaa-nacha = '2'.                   "Ausgabe auf Fax
      IF lv_telfx NE space.
        IF i_reguh-xvorl IS INITIAL.
          c_finaa-tdschedule = 'IMM'.
          c_finaa-tdteleland = lv_land1.
          c_finaa-tdtelenum  = lv_telfx.
          c_finaa-tdfaxuser  = space.
          c_finaa-namep      = lv_zsabe.
          c_finaa-formc      = 'FI_FAX_COVER_A4'.
          c_finaa-fornr      = space.
* ---> Es wird eine Fax-Nachricht an & mit der Faxnummer & erstellt.
          MESSAGE ID '/SBK/FI_BTE' TYPE 'S' NUMBER 303
                  WITH lv_parnr lv_telfx.
        ELSE.
* ---> Vorschlagslauf: Es wurde keine Fax-Nachricht an & erstellt.
          IF 1 = 2.
            MESSAGE w301(/sbk/fi_bte) WITH lv_telfx.
          ENDIF.
          APPEND INITIAL LINE TO t_fimsg ASSIGNING <ls_fimsg>.
          <ls_fimsg>-msgid = '/SBK/FI_BTE'.
          <ls_fimsg>-msgty = 'W'.
          <ls_fimsg>-msgno = 301.
          <ls_fimsg>-msgv1 = lv_telfx.
          c_finaa-nacha = '1'.
        ENDIF.
      ELSE.
* ---> Zum Partner & konnte keine Faxnummer ermittelt werden
        IF 1 = 2.
          MESSAGE w570(ve) WITH lv_parnr.
        ENDIF.
        APPEND INITIAL LINE TO t_fimsg ASSIGNING <ls_fimsg>.
        <ls_fimsg>-msgid = 'VE'.
        <ls_fimsg>-msgty = 'W'.
        <ls_fimsg>-msgno = 570.
        <ls_fimsg>-msgv1 = lv_parnr.
        c_finaa-nacha = '1'.
      ENDIF.
    ENDIF.
**  ELSE.
**    c_finaa-nacha = '1'.                   "Drucken
**  ENDIF.

ENDFUNCTION.
