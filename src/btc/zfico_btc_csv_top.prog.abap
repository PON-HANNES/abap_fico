*&---------------------------------------------------------------------*
*& Include          ZFICO_BTC_CSV_TOP
*&---------------------------------------------------------------------*
"--- Variables
DATA gv_fehler TYPE c LENGTH 1.
DATA gv_text   TYPE c LENGTH 30.
DATA gv_saknr  TYPE n LENGTH 10.
"--- Structures
DATA gs_felder TYPE zfico_batch_struc.
DATA gs_upddat TYPE zfico_batch_struc_sap.
DATA gs_hkont  TYPE bseg-hkont.
"--- Tables
DATA gt_felder TYPE STANDARD TABLE OF zfico_batch_struc.                          " itab für upload daten mit feldstruktur
DATA gt_upddat TYPE TABLE OF zfico_batch_struc_sap.                          " itab für btci daten

DATA(gt_intab) = VALUE string_table( ).
