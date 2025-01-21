*&---------------------------------------------------------------------*
*& Report ZFICO_BTC_CSV_GL                                             *
*&---------------------------------------------------------------------*
*& Date:   31.10.2024                                                  *
*& Author: Hannes Maisch (HANNESM)                                     *
*& Company:                                                            *
*& Requested from:                                                     *
*& Description: Upload per CSV-Datei mit anschließender Erstellung     *
*&              einer Batch-Input Mappe für Sachkontenbuchungen.       *
*&---------------------------------------------------------------------*
*& Change History                                                      *
*& Date        | Author   | CR &  Description                          *
*&---------------------------------------------------------------------*
REPORT zfico_btc_csv_gl MESSAGE-ID zfico_btc.

INCLUDE zfico_btc_csv_top.
INCLUDE zfico_btc_csv_scr.

INITIALIZATION.
  CLEAR so_saknr[].
  APPEND VALUE #( sign   = 'I'
                  option = 'BT'
                  low    = '0000156000'
                  high   = '0000157900' ) TO so_saknr.
  APPEND VALUE #( sign   = 'I'
                  option = 'BT'
                  low    = '0000176000'
                  high   = '0000177999' ) TO so_saknr.
  APPEND VALUE #( sign   = 'I'
                  option = 'BT'
                  low    = '0000177801'
                  high   = '0000178899'  ) TO so_saknr.

  GET PARAMETER ID 'BUK' FIELD p_bukrs.

START-OF-SELECTION.
  PERFORM upload_file.

  PERFORM split_fielddata.

  PERFORM matching_fields.

  PERFORM upd_protocoll.

  IF p_test IS INITIAL.
    PERFORM bdc_open.
    PERFORM btci_mappe.
    PERFORM bdc_close.

    IF gv_fehler IS INITIAL.
      CASE p_funct.
        WHEN 'B'.
          gv_text = 'BTCI-Mappe erstellt'(p01).
        WHEN 'C'.
          gv_text = 'Buchung erfolgreich ausgeführt'(p02).
        WHEN OTHERS.
          gv_text = 'direct input ist nicht vorgesehen'(p03).
      ENDCASE.

      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING titel = 'Status: Buchung' ##NO_TEXT
                  txt1  = gv_text
                  txt2  = ''.

    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  upload_file
*&---------------------------------------------------------------------*
FORM upload_file.
  " Frontend Services - GUI Upload File
  cl_gui_frontend_services=>gui_upload( EXPORTING  filename                = CONV #( p_file ) " Name der Datei
                                        CHANGING   data_tab                = gt_intab         " Übergabetabelle für Datei-Inhalt
                                        EXCEPTIONS file_open_error         = 1                " Datei nicht vorhanden, kann nicht geöffnet werde
                                                   file_read_error         = 2                " Fehler beim Lesen der Datei
                                                   no_batch                = 3                " Frontend-Funktion im Batch nicht ausführbar.
                                                   gui_refuse_filetransfer = 4                " Falsches Frontend oder Fehler im Frontend
                                                   invalid_type            = 5                " Falscher Parameter FILETYPE
                                                   no_authority            = 6                " Keine Berechtigung für Upload
                                                   unknown_error           = 7                " Unbekannter Fehler
                                                   bad_data_format         = 8                " Daten in der Datei können nicht interpretiert werden.
                                                   header_not_allowed      = 9                " Header ist nicht zulässig.
                                                   separator_not_allowed   = 10               " Separator ist nicht zulässig.
                                                   header_too_long         = 11               " Die Headerinformation ist zur Zeit auf maximal 1023 Bytes be
                                                   unknown_dp_error        = 12               " Fehler beim Aufruf des Dataprovider
                                                   access_denied           = 13               " Zugriff auf Datei nicht erlaubt.
                                                   dp_out_of_memory        = 14               " Nicht genug Speicher im Dataprovider
                                                   disk_full               = 15               " Speichermedium ist voll.
                                                   dp_timeout              = 16               " Timeout des Dataproviders
                                                   not_supported_by_gui    = 17               " Nicht unterstützt von GUI
                                                   error_no_gui            = 18               " GUI nicht verfügbar
                                                   OTHERS                  = 19 ).
  IF sy-subrc <> 0.
    MESSAGE e001 WITH sy-subrc.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  split_fielddata
*&---------------------------------------------------------------------*
FORM split_fielddata.
  DELETE gt_intab INDEX 1.
  DELETE gt_intab INDEX 1.

  " Aufsplittung der importierten Daten in fix definerte Felder (CHAR50)
  LOOP AT gt_intab ASSIGNING FIELD-SYMBOL(<intab>).
    CLEAR gs_felder.
    REPLACE ALL OCCURRENCES OF '"' IN <intab> WITH ''.
    SPLIT <intab> AT ';' INTO
      gs_felder-feld01     " Betrag incl 2 Nachkommastellen, Blank
      gs_felder-feld02     " Soll/Haben Kennzeichen
      gs_felder-feld03     " leer
      gs_felder-feld04     " leer
      gs_felder-feld05     " leer
      gs_felder-feld06     " leer
      gs_felder-feld07     " 1.Sachkonto,                    --> bestimmt ob FB01 oder FB41 Buchung erfolgen muss
      gs_felder-feld08     " 2.Sachkonto, für Gegenbuchung   --> bestimmt ob FB01 oder FB41 Buchung erfolgen muss
      gs_felder-feld09     " leer
      gs_felder-feld10     " leer
      gs_felder-feld11     " leer
      gs_felder-feld12     " Entscheidung, ob Buchung übernommen werden soll nach SAP oder nicht "SAP/Y" muss hier enthalten sein
      gs_felder-feld13     " leer
      gs_felder-feld14     " Text für SGTXT für beide Sachkonten
      gs_felder-feld15.    " leer

    IF gs_felder-feld12 CP '*SAP/Y*'.
      APPEND gs_felder TO gt_felder.
    ENDIF.
  ENDLOOP.

  IF gt_felder[] IS INITIAL.
    MESSAGE e002 WITH p_file.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  matching_fields
*&---------------------------------------------------------------------*
FORM matching_fields.
  LOOP AT gt_felder INTO gs_felder.
    CLEAR gs_upddat.
    REPLACE ALL OCCURRENCES OF ',' IN gs_felder-feld01 WITH '.'.
    gs_upddat-dmbtr = gs_felder-feld01. " Betrag
    gs_upddat-shkzg = gs_felder-feld02. " Soll/Haben Indikator
    gs_upddat-waers = p_waers.
    gs_upddat-gkont = |{ gs_felder-feld08 ALPHA = IN }|.
    SELECT COUNT( * ) FROM skb1 WHERE saknr = gs_upddat-gkont AND bukrs = p_bukrs.
    IF sy-subrc <> 0.
      gs_upddat-message = |Konto:{ gs_felder-feld08 } existiert nicht im Buchungskreis { p_bukrs } | ##NO_TEXT.
    ENDIF.
    gs_upddat-konto = |{ gs_felder-feld07 ALPHA = IN }|.
    SELECT COUNT( * ) FROM skb1 WHERE saknr = gs_upddat-konto AND bukrs = p_bukrs.
    IF sy-subrc <> 0.
      gs_upddat-message = |Konto:{ gs_felder-feld07 } existiert nicht im Buchungskreis { p_bukrs } | ##NO_TEXT.
    ENDIF.
    gs_upddat-sgtxt = gs_felder-feld14.
    APPEND gs_upddat TO gt_upddat.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  protokoll_upd
*&---------------------------------------------------------------------*
FORM upd_protocoll.
  LOOP AT gt_upddat INTO gs_upddat.
    IF sy-tabix = 1.
      FORMAT COLOR 3 ON.
      WRITE: / 'Währung' ##NO_TEXT,
           AT 10 'Betrag' ##NO_TEXT,
           AT 45 'Soll/Haben' ##NO_TEXT,
           AT 60 'Konto' ##NO_TEXT,
           AT 80 'Gegenkonto' ##NO_TEXT,
           AT 100 'Buchungstext' ##NO_TEXT,
           AT 175 'Meldungen' ##NO_TEXT.
      FORMAT COLOR 3 OFF.
    ENDIF.
    WRITE: / gs_upddat-waers,
         AT 10 gs_upddat-dmbtr CURRENCY 'EUR',
         AT 45 gs_upddat-shkzg,
         AT 60 gs_upddat-konto USING EDIT MASK '==ALPHA',
         AT 80 gs_upddat-gkont USING EDIT MASK '==ALPHA',
         AT 100 gs_upddat-sgtxt,
         AT 175 gs_upddat-message.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BDC_OPEN
*&---------------------------------------------------------------------*
FORM bdc_open.                                            " BTCI-STANDARD
  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING  i_function         = p_funct
               i_group            = p_mappe     " DATEV
               i_keep             = 'X'
               i_mode             = p_mode
               i_update           = p_update
               i_user             = sy-uname
    EXCEPTIONS client_incorrect   = 1
               function_invalid   = 2
               group_name_missing = 3
               mode_invalid       = 4
               update_invalid     = 5
               OTHERS             = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " BDC_OPEN                    "BTCI-STANDARD

*&---------------------------------------------------------------------*
*&      Form  btci_mappe
*&---------------------------------------------------------------------*
FORM btci_mappe.
  DATA msgid            TYPE sy-msgid.
  DATA msgno            TYPE sy-msgno.
  DATA msgty            TYPE sy-msgty.
  DATA msgv1            TYPE sy-msgv1.
  DATA msgv2            TYPE sy-msgv2.
  DATA msgv3            TYPE sy-msgv3.
  DATA msgv4            TYPE sy-msgv4.
  DATA subrc            TYPE sy-subrc.
  DATA lv_tcode         TYPE sy-tcode.
  DATA message          TYPE c LENGTH 200.
  DATA only_balance_acc TYPE boole_d.

  DATA lt_blntab        TYPE STANDARD TABLE OF blntab.
  DATA ls_blntab        TYPE blntab.
  DATA lt_ftpost        TYPE STANDARD TABLE OF ftpost.
  DATA ls_ftpost        TYPE ftpost.
  DATA lt_fttax         TYPE STANDARD TABLE OF fttax.
  DATA ls_fttax         TYPE fttax.

  DATA count            TYPE i.
  DATA bktxt            TYPE bkpf-bktxt.

  DEFINE pos.
    ls_ftpost-stype = 'P'.
    ls_ftpost-count = count.
    ls_Ftpost-fnam = &1.
    WRITE &2 TO ls_Ftpost-fval.
    SHIFT ls_ftpost-fval LEFT DELETING LEADING space.
    APPEND ls_ftpost TO lt_ftpost.
  END-OF-DEFINITION.

  DEFINE kopf.
    ls_ftpost-stype = 'K'.
    ls_ftpost-count = 1.
    ls_Ftpost-fnam = &1.
    WRITE &2 TO ls_ftpost-fval.
    SHIFT ls_ftpost-fval LEFT DELETING LEADING space.
    APPEND ls_ftpost TO lt_ftpost.
  END-OF-DEFINITION.

  LOOP AT gt_upddat INTO gs_upddat.

    CLEAR: lt_blntab,
           lt_ftpost,
           lt_fttax,
           lv_tcode,
           only_balance_acc.

    kopf 'BKPF-BLART' 'ZJ'.
    kopf 'BKPF-BUKRS' p_bukrs.
    kopf 'BKPF-BUDAT' p_budat.
    kopf 'BKPF-BLDAT' p_budat.
    kopf 'BKPF-WAERS' p_waers.
    kopf 'BKPF-MONAT' p_mon.

    CONCATENATE p_budat+6(2) p_budat+4(2) p_budat(4) INTO bktxt SEPARATED BY '.'.
    CONCATENATE 'JA' bktxt INTO bktxt SEPARATED BY space.

    kopf 'BKPF-BKTXT' bktxt.
    kopf 'BKPF-XBLNR' bktxt.

    " 1. Position
    SELECT COUNT( * ) FROM skb1
      WHERE saknr = gs_upddat-konto
        AND bukrs = p_bukrs.
    IF sy-subrc <> 0.

    ENDIF.

    count += 1.
    IF gs_upddat-shkzg = 'S'.
      pos 'RF05A-NEWBS' '40'.
    ELSE.
      pos 'RF05A-NEWBS' '50'.
    ENDIF.

    pos 'RF05A-NEWKO' gs_upddat-konto.
    pos 'BSEG-ZUONR'  gs_upddat-gkont.                      " JB20150422
    pos 'BSEG-WRBTR'  gs_upddat-dmbtr ##UOM_IN_MES.
    pos 'BSEG-SGTXT'  gs_upddat-sgtxt.

    gv_saknr = gs_upddat-konto.
    SELECT COUNT( * ) FROM ska1 "#EC CI_GENBUFF
      WHERE saknr = gv_saknr
        AND xbilk = ' '.
    IF sy-subrc = 0.
      IF gs_upddat-kostl IS INITIAL.
        gs_upddat-kostl = p_kostl.
      ENDIF.
      pos 'COBL-KOSTL' gs_upddat-kostl.
    ENDIF.

    " 2. Position
    SELECT COUNT( * ) FROM skb1
      WHERE saknr = gs_upddat-konto
        AND bukrs = p_bukrs.
    IF sy-subrc <> 0.

    ENDIF.

    count += 1.
    IF gs_upddat-shkzg = 'S'.
      pos 'RF05A-NEWBS' '50'.
    ELSE.
      pos 'RF05A-NEWBS' '40'.
    ENDIF.

    pos 'RF05A-NEWKO' gs_upddat-gkont.
    pos 'BSEG-ZUONR'  gs_upddat-konto.                      " JB20150422
    pos 'BSEG-WRBTR'  gs_upddat-dmbtr ##UOM_IN_MES.
    pos 'BSEG-SGTXT'  gs_upddat-sgtxt.

    gv_saknr = gs_upddat-gkont.
    SELECT COUNT( * ) FROM ska1 "#EC CI_GENBUFF
      WHERE saknr = gv_saknr
        AND xbilk = ' '.
    IF sy-subrc = 0.
      IF gs_upddat-kostl IS INITIAL.
        gs_upddat-kostl = p_kostl.
      ENDIF.
      pos 'COBL-KOSTL' gs_upddat-kostl.
    ENDIF.

    gv_saknr = gs_upddat-gkont.
    SELECT SINGLE * FROM ska1 "#EC CI_GENBUFF
      WHERE saknr = @gv_saknr
        AND xbilk = ' '
      INTO @DATA(ska1).
    IF sy-subrc <> 0. " GuV-Konto -> Belegsplit kein Problem
      gv_saknr = gs_upddat-konto.
      SELECT SINGLE * FROM ska1 "#EC CI_GENBUFF
        WHERE saknr = @gv_saknr
          AND xbilk = ' '
        INTO @ska1.
      IF sy-subrc <> 0.
        only_balance_acc = abap_true.
      ENDIF.
    ENDIF.

    IF only_balance_acc = abap_true.
      pos 'COBL-PRCTR' p_prctr.
    ENDIF.

    IF    ( gs_upddat-gkont IN so_saknr )
       OR ( gs_upddat-konto IN so_saknr ).
      lv_tcode = 'FB41'.
    ELSE.
      lv_tcode = 'FB01'.
    ENDIF.

    CALL FUNCTION 'POSTING_INTERFACE_DOCUMENT'
      EXPORTING  i_tcode                  = lv_tcode
                 i_sgfunct                = p_funct
      IMPORTING  e_msgid                  = msgid
                 e_msgno                  = msgno
                 e_msgty                  = msgty
                 e_msgv1                  = msgv1
                 e_msgv2                  = msgv2
                 e_msgv3                  = msgv3
                 e_msgv4                  = msgv4
                 e_subrc                  = subrc
      TABLES     t_blntab                 = lt_blntab
                 t_ftpost                 = lt_ftpost
                 t_fttax                  = lt_fttax
      EXCEPTIONS account_missing          = 1
                 company_code_missing     = 2
                 posting_key_invalid      = 3
                 posting_key_missing      = 4
                 record_type_invalid      = 5
                 transaction_code_invalid = 6
                 amount_format_error      = 7
                 too_many_line_items      = 8
                 company_code_invalid     = 9
                 screen_not_found         = 10
                 no_authorization         = 11
                 OTHERS                   = 12.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO message.

      WRITE / message.

      gv_fehler = 'X'.

    ENDIF.

  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BDC_CLOSE
*&---------------------------------------------------------------------*
FORM bdc_close.                                           " BTCI-STANDARD
  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXCEPTIONS session_not_processable = 1
               OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " BDC_CLOSE                   "BTCI-STANDARD
