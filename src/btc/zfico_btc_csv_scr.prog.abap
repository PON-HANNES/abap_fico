*&---------------------------------------------------------------------*
*& Include          ZFICO_BTC_CSV_SCR
*&---------------------------------------------------------------------*
"---Upload
SELECTION-SCREEN BEGIN OF BLOCK upld WITH FRAME TITLE TEXT-s01.
  PARAMETERS p_file TYPE localfile OBLIGATORY.
SELECTION-SCREEN END OF BLOCK upld.

"---Verbuchung
SELECTION-SCREEN BEGIN OF BLOCK verb WITH FRAME TITLE TEXT-s02.
  PARAMETERS: p_blart TYPE bkpf-blart DEFAULT 'ZJ' MODIF ID zj,
              p_bukrs TYPE bukrs OBLIGATORY,
              p_budat TYPE budat OBLIGATORY,
              p_mon TYPE bkpf-monat DEFAULT '13',
              p_waers TYPE waers DEFAULT 'EUR'.

  SELECTION-SCREEN SKIP 1.
  PARAMETERS: p_kredi TYPE bseg-koart DEFAULT '9' NO-DISPLAY,
              p_debi  TYPE bseg-koart DEFAULT '2' NO-DISPLAY.
*  SELECTION-SCREEN SKIP 1.
  PARAMETERS: p_kostl TYPE bseg-kostl OBLIGATORY MATCHCODE OBJECT kost,
              p_prctr TYPE bseg-prctr OBLIGATORY DEFAULT 'VERWALTUNG'.
  SELECTION-SCREEN SKIP 1.
  SELECT-OPTIONS so_saknr FOR gs_hkont.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS p_test TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK verb.

"---Optionen: Batch-Input Mappenerstellung
SELECTION-SCREEN BEGIN OF BLOCK btci WITH FRAME TITLE TEXT-s03.
  PARAMETERS: p_mappe(12) TYPE c DEFAULT 'JA_',
              p_funct     TYPE rfpdo-rfbifunct AS LISTBOX VISIBLE LENGTH 35 DEFAULT 'B' OBLIGATORY,
              p_mode      TYPE ctu_mode   DEFAULT 'N' NO-DISPLAY,
              p_update    TYPE ctu_update DEFAULT 'S' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK btci.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING static    = 'X'
    CHANGING  file_name = p_file.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    IF screen-group1 = 'ZJ'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
