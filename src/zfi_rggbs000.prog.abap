PROGRAM zfi_rggbs000.
" ----------------------------------------------------------------------
" Corrections/ repair
" wms092357 070703 Note 638886: template routines to be used for
"                  workaround to substitute bseg-bewar from bseg-xref1/2
" ----------------------------------------------------------------------
"                                                                     -
"   Substitutions: EXIT-Formpool for Uxxx-Exits                       -
"                                                                     -
"   This formpool is used by SAP for testing purposes only.           -
"                                                                     -
"   Note: If you define a new user exit, you have to enter your       -
"         user exit in the form routine GET_EXIT_TITLES.              -
"                                                                     -
" ----------------------------------------------------------------------
INCLUDE fgbbgd00.              " Standard data types


" !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-
"     PLEASE INCLUDE THE FOLLOWING "TYPE-POOL"  AND "TABLES" COMMANDS  -
"         IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM         -
TYPE-POOLS gb002. " TO BE INCLUDED IN                       "wms092357
TABLES: bkpf,      " ANY SYSTEM THAT                         "wms092357
        bseg,      " HAS 'FI' INSTALLED                      "wms092357
        cobl,                                               " wms092357
        csks,                                               " wms092357
        anlz,                                               " wms092357
        glu1.                                               " wms092357
" !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-


" -----------------------------------------------------------------------
"      FORM GET_EXIT_TITLES                                           -
" -----------------------------------------------------------------------
"      returns name and title of all available standard-exits         -
"      every exit in this formpool has to be added to this form.      -
"      You have to specify a parameter type in order to enable the    -
"      code generation program to determine correctly how to          -
"      generate the user exit call, i.e. how many and what kind of    -
"      parameter(s) are used in the user exit.                        -
"      The following parameter types exist:                           -
"                                                                     -
"      TYPE                Description              Usage             -
"   ------------------------------------------------------------      -
"      C_EXIT_PARAM_NONE   Use no parameter         Subst. and Valid. -
"                          except B_RESULT                            -
"      C_EXIT_PARAM_FIELD  Use one field as param.  Only Substitution -
"      C_EXIT_PARAM_CLASS  Use a type as parameter  Subst. and Valid  -
"                                                                     -
" -----------------------------------------------------------------------
" -->  EXIT_TAB  table with exit-name and exit-titles                 -
"                structure: NAME(5), PARAM(1), TITEL(60)
" -----------------------------------------------------------------------
FORM get_exit_titles TABLES etab.
  DATA: BEGIN OF exits OCCURS 50,
          name  TYPE c LENGTH 5,
          param LIKE c_exit_param_none,
          title TYPE c LENGTH 60,
        END OF exits.

  exits-name  = 'U100'.
  exits-param = c_exit_param_none.
  exits-title = TEXT-100.             " Cost center from CSKS
  APPEND exits.

  exits-name  = 'U101'.
  exits-param = c_exit_param_field.
  exits-title = TEXT-101.             " Cost center from CSKS
  APPEND exits.

  " begin of insertion                                          "wms092357
  exits-name  = 'U200'.
  exits-param = c_exit_param_field.
  exits-title = TEXT-200.             " Cons. transaction type
  APPEND exits.                       " from xref1/2
  " end of insertion                                            "wms092357

  exits-name  = 'U800'.
  exits-param = c_exit_param_class.
  exits-title = TEXT-800.
  APPEND exits.

  exits-name  = 'U801'.
  exits-param = c_exit_param_class.
  exits-title = TEXT-801.
  APPEND exits.

  exits-name  = 'U802'.
  exits-param = c_exit_param_class.
  exits-title = TEXT-802.
  APPEND exits.

  " -----------------------------------------------------------------------
  " PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES -
  "        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         -
  "  EXITS-NAME  = 'U102'.
  "  EXITS-PARAM = C_EXIT_PARAM_CLASS.
  "  EXITS-TITLE = TEXT-102.             "Sum is used for the reference.
  "  APPEND EXITS.

  " ----------------------------------------------------------------------
  " EXIT EXAMPLES FROM PUBLIC SECTOR INDUSTRY SOLUTION
  "
  " PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINE
  " TO ENABLE PUBLIC SECTOR EXAMPLE SUBSTITUTION EXITS
  " ----------------------------------------------------------------------
  INCLUDE rggbs_ps_titles.

  REFRESH etab.
  LOOP AT exits.
    etab = exits.
    APPEND etab.
  ENDLOOP.
ENDFORM.

" eject
" ----------------------------------------------------------------------
"       FORM U100                                                     -
" ----------------------------------------------------------------------
"       Reads the cost-center from the CSKS table .                   -
" ----------------------------------------------------------------------
FORM u100.
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*  SELECT * FROM CSKS
*            WHERE KOSTL EQ COBL-KOSTL
*              AND KOKRS EQ COBL-KOKRS.
*    IF CSKS-DATBI >= SY-DATUM AND
*       CSKS-DATAB <= SY-DATUM.
*
*      MOVE CSKS-ABTEI TO COBL-KOSTL.
*
*    ENDIF.
*  ENDSELECT.
ENDFORM.

" eject
" ----------------------------------------------------------------------
"       FORM U101                                                     -
" ----------------------------------------------------------------------
"       Reads the cost-center from the CSKS table for accounting      -
"       area '0001'.                                                  -
"       This exit uses a parameter for the cost_center so it can      -
"       be used irrespective of the table used in the callup point.   -
" ----------------------------------------------------------------------
FORM u101 USING cost_center.
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*  SELECT * FROM CSKS
*            WHERE KOSTL EQ COST_CENTER
*              AND KOKRS EQ '0001'.
*    IF CSKS-DATBI >= SY-DATUM AND
*       CSKS-DATAB <= SY-DATUM.
*
*      MOVE CSKS-ABTEI TO COST_CENTER .
*
*    ENDIF.
*  ENDSELECT.
ENDFORM.

" eject
" ----------------------------------------------------------------------
"       FORM U102                                                     -
" ----------------------------------------------------------------------
"       Inserts the sum of the posting into the reference field.      -
"       This exit can be used in FI for the complete document.        -
"       The complete data is passed in one parameter.                 -
" ----------------------------------------------------------------------


*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*FORM u102 USING bool_data TYPE gb002_015.
*DATA: SUM(10) TYPE C.
*
*    LOOP AT BOOL_DATA-BSEG INTO BSEG
*                    WHERE    SHKZG = 'S'.
*       BSEG-ZUONR = 'Test'.
*       MODIFY BOOL_DATA-BSEG FROM BSEG.
*       ADD BSEG-DMBTR TO SUM.
*    ENDLOOP.
*
*    BKPF-XBLNR = TEXT-001.
*    REPLACE '&' WITH SUM INTO BKPF-XBLNR.
*
*ENDFORM.


" ----------------------------------------------------------------------
" EXIT EXAMPLES FROM PUBLIC SECTOR INDUSTRY SOLUTION
" -
" PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINE
" TO ENABLE PUBLIC SECTOR EXAMPLE SUBSTITUTION EXITS
" ----------------------------------------------------------------------
" INCLUDE rggbs_ps_forms.

FORM u800 USING bool_data TYPE gb002_015.
  SELECT SINGLE * FROM zfi_c0001 WHERE bukrs = @bool_data-bkpf-bukrs INTO @DATA(zfi_c01).
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  IF NOT line_exists( bool_data-bseg[ xauto = 'X' ] ).
    RETURN.
  ENDIF.

  CASE zfi_c01-sub_option.
    WHEN '1'. " Referenz des Beleges substituieren
      DATA(sgtxt) = bool_data-bkpf-xblnr.
    WHEN '2'. " Buchungstext mit höchsten Betrag substituieren
      SORT bool_data-bseg BY dmbtr DESCENDING.
      ASSIGN bool_data-bseg[ 1 ]-sgtxt TO FIELD-SYMBOL(<sgtxt>).
      IF sy-subrc = 0.
        sgtxt = <sgtxt>.
      ENDIF.

    WHEN OTHERS.
      " do nothing
      EXIT.
  ENDCASE.

  LOOP AT bool_data-bseg ASSIGNING FIELD-SYMBOL(<bseg>) WHERE xauto = 'X'.
    <bseg>-sgtxt = sgtxt.
  ENDLOOP.
ENDFORM.

FORM u801 USING bool_data TYPE gb002_015.
  DATA lv_lifnr TYPE lfb1-lifnr.

  CHECK line_exists( bool_data-bseg[ koart = 'K' ] ).
  CLEAR lv_lifnr.
  lv_lifnr = bool_data-bseg[ koart = 'K' ]-lifnr.
  SELECT SINGLE zwels FROM lfb1 WHERE lifnr = @lv_lifnr AND bukrs = @bool_data-bkpf-bukrs INTO @DATA(zwels).
  IF sy-subrc = 0.
    bool_data-bseg[ koart = 'K' ]-zlsch = zwels.
  ENDIF.
ENDFORM.

FORM u802 USING bool_data TYPE gb002_015.

ENDFORM.

*eject
* begin of insertion                                          "wms092357
*&---------------------------------------------------------------------*
*&      Form  u200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM u200 USING e_rmvct TYPE bseg-bewar.
  PERFORM xref_to_rmvct
    USING    bkpf
             bseg
             1
    CHANGING e_rmvct.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  xref_to_rmvct
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM xref_to_rmvct
  USING    is_bkpf      TYPE bkpf
           is_bseg      TYPE bseg
           i_xref_field TYPE i
  CHANGING c_rmvct      TYPE rmvct.

  DATA l_msgv TYPE symsgv.
  STATICS st_rmvct TYPE HASHED TABLE OF rmvct WITH UNIQUE DEFAULT KEY.

  " either bseg-xref1 or bseg-xref2 must be used as source...
  IF i_xref_field <> 1 AND i_xref_field <> 2.
    MESSAGE x000(gk) WITH 'UNEXPECTED VALUE I_XREF_FIELD ='
      i_xref_field '(MUST BE = 1 OR = 2)' ''.
  ENDIF.
  IF st_rmvct IS INITIAL.
    SELECT trtyp FROM t856 INTO TABLE st_rmvct.
  ENDIF.
  IF i_xref_field = 1.
    c_rmvct = is_bseg-xref1.
  ELSE.
    c_rmvct = is_bseg-xref2.
  ENDIF.
  IF c_rmvct IS INITIAL.
    WRITE i_xref_field TO l_msgv LEFT-JUSTIFIED.
    CONCATENATE TEXT-m00 l_msgv INTO l_msgv SEPARATED BY space.
    " cons. transaction type is not specified => send an error message...
    MESSAGE e123(g3) WITH l_msgv.
    " Bitte geben Sie im Feld &1 eine Konsolidierungsbewegungsart an
  ENDIF.
* c_rmvct <> initial...
  READ TABLE st_rmvct TRANSPORTING NO FIELDS FROM c_rmvct.
  IF sy-subrc IS INITIAL.
    RETURN.
  ENDIF.
  " cons. transaction type does not exist => send error message...
  WRITE i_xref_field TO l_msgv LEFT-JUSTIFIED.
  CONCATENATE TEXT-m00 l_msgv INTO l_msgv SEPARATED BY space.
  MESSAGE e124(g3) WITH c_rmvct l_msgv.
  " KonsBewegungsart &1 ist ungültig (bitte Eingabe im Feld &2 korrigieren
ENDFORM.
" end of insertion                                            "wms092357
