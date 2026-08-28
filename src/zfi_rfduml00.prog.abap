*&---------------------------------------------------------------------*
*& Report ZFI_RFDUML00
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_rfduml00 NO STANDARD PAGE HEADING.

"----------------------------------------------------------------------
" GLOBALE TYPEN
"----------------------------------------------------------------------
"--- KNC1 Rohdaten: alle 16 Perioden für Umsatzsummierung ---
TYPES: BEGIN OF ty_knc1_raw,
         kunnr TYPE kunnr,
         bukrs TYPE bukrs,
         um01u TYPE knc1_view-um01u,
         um02u TYPE knc1_view-um02u,
         um03u TYPE knc1_view-um03u,
         um04u TYPE knc1_view-um04u,
         um05u TYPE knc1_view-um05u,
         um06u TYPE knc1_view-um06u,
         um07u TYPE knc1_view-um07u,
         um08u TYPE knc1_view-um08u,
         um09u TYPE knc1_view-um09u,
         um10u TYPE knc1_view-um10u,
         um11u TYPE knc1_view-um11u,
         um12u TYPE knc1_view-um12u,
         um13u TYPE knc1_view-um13u,
         um14u TYPE knc1_view-um14u,
         um15u TYPE knc1_view-um15u,
         um16u TYPE knc1_view-um16u,
       END OF ty_knc1_raw.

"--- Ausgabestruktur für den ALV ---
TYPES: BEGIN OF ty_output,
         bukrs     TYPE bukrs,
         kunnr     TYPE kunnr,
         akont     TYPE knb1-akont,
         name1     TYPE kna1-name1,
         brsch     TYPE but0is-ind_sector,
         brsch_txt TYPE tb038b-text,
         land1     TYPE kna1-land1,
         pstlz     TYPE kna1-pstlz,
         ort01     TYPE kna1-ort01,
         stras     TYPE kna1-stras,
         regio     TYPE kna1-regio,
         waers     TYPE t001-waers,
         umsatz    TYPE knc1_view-um01u,
       END OF ty_output,

       tt_output TYPE STANDARD TABLE OF ty_output WITH EMPTY KEY.
TYPES tt_knc1 TYPE STANDARD TABLE OF ty_knc1_raw WITH EMPTY KEY.

"----------------------------------------------------------------------
" KLASSE – VORDEKLARATION (benötigt, da Selektion nach CLASS...END)
"----------------------------------------------------------------------
CLASS lcl_report DEFINITION DEFERRED.

"----------------------------------------------------------------------
" SELEKTIONSBILD
"----------------------------------------------------------------------
TABLES: kna1, knb1.

SELECTION-SCREEN BEGIN OF BLOCK b_sel WITH FRAME TITLE TEXT-b01.
  SELECT-OPTIONS:
    s_kunnr FOR kna1-kunnr,   " Debitorenkonto
    s_bukrs FOR knb1-bukrs.   " Buchungskreis
SELECTION-SCREEN END OF BLOCK b_sel.

SELECTION-SCREEN BEGIN OF BLOCK b_par WITH FRAME TITLE TEXT-b02.
  PARAMETERS:
    p_gjahr TYPE gjahr DEFAULT sy-datum(4),  " Geschäftsjahr
    p_plow  TYPE monat DEFAULT '01',         " Periode von
    p_phigh TYPE monat DEFAULT '16'.         " Periode bis
SELECTION-SCREEN END OF BLOCK b_par.

"----------------------------------------------------------------------
" KLASSENIMPLEMENTIERUNG
"----------------------------------------------------------------------
CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS run.

  PRIVATE SECTION.
    "--- Datenbeschaffung ---
    CLASS-METHODS get_data
      RETURNING VALUE(rt_output) TYPE tt_output.

    CLASS-METHODS get_knc1
      RETURNING VALUE(rt_knc1) TYPE tt_knc1.

    CLASS-METHODS sum_periods
      IMPORTING is_knc1       TYPE ty_knc1_raw
      RETURNING VALUE(rv_sum) TYPE knc1-um01u.

    "--- ALV-Ausgabe ---
    CLASS-METHODS show_alv
      IMPORTING it_data TYPE tt_output.
ENDCLASS.


CLASS lcl_report IMPLEMENTATION.
  "--------------------------------------------------------------------
  METHOD run.
    "--------------------------------------------------------------------
    DATA(lt_data) = get_data( ).

    IF lt_data IS INITIAL.
      MESSAGE 'Keine Daten für die gewählte Selektion gefunden.' TYPE 'I'.
      RETURN.
    ENDIF.

    show_alv( lt_data ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  METHOD get_data.
    "--------------------------------------------------------------------
    "--- 1. Stammdaten: KNA1 + KNB1 + T001 in einem SELECT ---
    SELECT knb1~bukrs,
           knb1~kunnr,
           knb1~akont,
           kna1~name1,
**           kna1~brsch,
           kna1~land1,
           kna1~pstlz,
           kna1~ort01,
           kna1~stras,
           kna1~regio,
           t001~waers,
           but0is~ind_sector AS brsch,
           tb038b~text       AS brsch_txt
      FROM knb1
             INNER JOIN
               kna1 ON kna1~kunnr = knb1~kunnr
                 INNER JOIN
                   t001 ON t001~bukrs = knb1~bukrs
                     LEFT OUTER JOIN
                       but0is ON  but0is~partner = kna1~kunnr
                              AND but0is~isdef   = @abap_true
                         LEFT OUTER JOIN
                           tb038b ON  tb038b~ind_sector = but0is~ind_sector
                                  AND tb038b~istype     = but0is~istype
                                  AND tb038b~spras      = @sy-langu
      WHERE knb1~kunnr IN @s_kunnr
        AND knb1~bukrs IN @s_bukrs
      INTO TABLE @DATA(lt_master).

    "--- 2. Umsätze aus KNC1 ---
    DATA(lt_knc1) = get_knc1( ).
    SORT lt_knc1 BY kunnr
                    bukrs.  " Für BINARY SEARCH

    "--- 3. Stammdaten + Umsatz zusammenführen ---
    LOOP AT lt_master ASSIGNING FIELD-SYMBOL(<m>).

      DATA(ls_out) = VALUE ty_output( bukrs     = <m>-bukrs
                                      kunnr     = <m>-kunnr
                                      akont     = <m>-akont
                                      name1     = <m>-name1
                                      brsch     = <m>-brsch
                                      brsch_txt = <m>-brsch_txt
                                      land1     = <m>-land1
                                      pstlz     = <m>-pstlz
                                      ort01     = <m>-ort01
                                      stras     = <m>-stras
                                      regio     = <m>-regio
                                      waers     = <m>-waers ).

      "--- Passenden KNC1-Satz suchen (Binärsuche nach Sort oben) ---
      READ TABLE lt_knc1 ASSIGNING FIELD-SYMBOL(<k>)
           WITH KEY kunnr = <m>-kunnr
                    bukrs = <m>-bukrs
           BINARY SEARCH.

      IF sy-subrc = 0.
        ls_out-umsatz = sum_periods( <k> ).
      ENDIF.
      IF ls_out-umsatz <> 0.
        APPEND ls_out TO rt_output.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  "--------------------------------------------------------------------
  METHOD get_knc1.
    "--------------------------------------------------------------------
    "--- KNC1_VIEW statt KNC1 (S/4HANA-Empfehlung -- zeigt auf ACDOCA)
    "--- Alle 16 Periodenfelder lesen, Filterung erfolgt in sum_periods ---
    SELECT kunnr, bukrs, um01u, um02u, um03u, um04u, um05u, um06u, um07u, um08u, um09u, um10u, um11u, um12u, um13u,
           um14u, um15u, um16u
      FROM knc1_view
      WHERE kunnr IN @s_kunnr
        AND bukrs IN @s_bukrs
        AND gjahr  = @p_gjahr
      INTO TABLE @rt_knc1.
  ENDMETHOD.

  "--------------------------------------------------------------------
  METHOD sum_periods.
    "--------------------------------------------------------------------
    "--- Nur die Perioden innerhalb des gewählten Bereichs summieren ---
    FIELD-SYMBOLS <val> TYPE any.
    DATA lv_fname TYPE string.
    DATA lv_idx   TYPE n LENGTH 2. " TYPE n → automatisches Zero-Padding

    DATA(lv_from) = CONV i( p_plow  ).   " z.B. '01' → 1
    DATA(lv_to)   = CONV i( p_phigh ).   " z.B. '16' → 16

    DO 16 TIMES.
      IF sy-index < lv_from OR sy-index > lv_to.
        CONTINUE.
      ENDIF.

      "--- Feldnamen dynamisch aufbauen: UM01U, UM02U, ... UM16U ---
      "--- TYPE n paddet automatisch: 1 → '01', 9 → '09', 16 → '16' ---
      lv_idx   = sy-index.
      lv_fname = |UM{ lv_idx }U|.
      ASSIGN COMPONENT lv_fname OF STRUCTURE is_knc1 TO <val>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      rv_sum += <val>.
    ENDDO.
  ENDMETHOD.

  "--------------------------------------------------------------------
  METHOD show_alv.
    "--------------------------------------------------------------------
    DATA lo_salv TYPE REF TO cl_salv_table.

    DATA(lt_work) = it_data.   " lokale Kopie – factory braucht CHANGING

    TRY.
        "--- ALV-Instanz erzeugen -----------------------------------
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_salv
                                CHANGING  t_table      = lt_work ).

        "============================================================
        " TOOLBAR: Alle Funktionen aktivieren
        " → Hier steckt der entscheidende Unterschied zu RFDUML00!
        " → Enthält: Excel-Export, CSV, Filter, Sortierung, Layout
        "============================================================
        lo_salv->get_functions( )->set_all( abap_true ).

        "--- Spaltenbreiten automatisch optimieren ------------------
        DATA(lo_cols) = lo_salv->get_columns( ).
        lo_cols->set_optimize( abap_true ).

        "--- Spaltenüberschriften setzen ----------------------------
        TRY.
            lo_cols->get_column( 'BUKRS'  )->set_long_text( 'Buchungskreis'  ).
            lo_cols->get_column( 'KUNNR'  )->set_long_text( 'Debitor'        ).
            lo_cols->get_column( 'AKONT'  )->set_long_text( 'Abstimmkonto'   ).
            lo_cols->get_column( 'NAME1'  )->set_long_text( 'Kundenname'     ).
            lo_cols->get_column( 'BRSCH'  )->set_long_text( 'Branche'        ).
            lo_cols->get_column( 'BRSCH_TXT' )->set_long_text( 'Bez. Branche' ).
            lo_cols->get_column( 'LAND1'  )->set_long_text( 'Land'           ).
            lo_cols->get_column( 'PSTLZ'  )->set_long_text( 'Postleitzahl'   ).
            lo_cols->get_column( 'ORT01'  )->set_long_text( 'Ort'            ).
            lo_cols->get_column( 'STRAS'  )->set_long_text( 'Strasse'        ).
            lo_cols->get_column( 'REGIO'  )->set_long_text( 'Region'         ).
            lo_cols->get_column( 'WAERS'  )->set_long_text( 'Waehrung'       ).
            lo_cols->get_column( 'UMSATZ' )->set_long_text( 'Umsatz'         ).
          CATCH cx_salv_not_found ##NO_HANDLER. " - Spalte fehlt: ignorieren
        ENDTRY.

        "--- Sortierung + Zwischensummen per Buchungskreis/Konto ----
        DATA(lo_sorts) = lo_salv->get_sorts( ).
        lo_sorts->add_sort( columnname = 'BUKRS'
                            subtotal   = abap_true ).
        lo_sorts->add_sort( columnname = 'AKONT'
                            subtotal   = abap_true ).
        lo_sorts->add_sort( columnname = 'KUNNR' ).

        "--- Gesamtsumme Umsatz (Fußzeile) --------------------------
        lo_salv->get_aggregations( )->add_aggregation( columnname = 'UMSATZ' ).

        "--- Darstellungsoptionen -----------------------------------
        DATA(lo_disp) = lo_salv->get_display_settings( ).
        lo_disp->set_striped_pattern( abap_true ).
        lo_disp->set_list_header( |Debitoren-Umsätze  –  GJ: { p_gjahr }  Perioden: { p_plow }–{ p_phigh }| ).

        "--- Ausgabe ------------------------------------------------
        lo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_text( ) TYPE 'E'.
      CATCH cx_salv_data_error INTO DATA(lx_de).
        MESSAGE lx_de->get_text( ) TYPE 'E'.
      CATCH cx_salv_existing INTO DATA(lx_ext).
        MESSAGE lx_ext->get_text( ) TYPE 'E'.
      CATCH cx_salv_not_found INTO DATA(lx_salv).
        MESSAGE lx_salv->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

"----------------------------------------------------------------------
" SELEKTIONSBILD – VALIDIERUNG
"----------------------------------------------------------------------

AT SELECTION-SCREEN.
  IF p_plow > p_phigh.
    MESSAGE 'Von-Periode darf nicht groesser als Bis-Periode sein!' TYPE 'E'.
  ENDIF.

  "----------------------------------------------------------------------
  " HAUPTPROGRAMM
  "----------------------------------------------------------------------

START-OF-SELECTION.
  lcl_report=>run( ).
