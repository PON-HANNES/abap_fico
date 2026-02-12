FUNCTION zfi_process_00002050.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_REGUH) LIKE  REGUH STRUCTURE  REGUH
*"     VALUE(I_GJAHR) LIKE  REGUD-GJAHR
*"     VALUE(I_NACHA) LIKE  FINAA-NACHA
*"     VALUE(I_AFORN) LIKE  T042B-AFORN
*"  CHANGING
*"     VALUE(C_ITCPO) LIKE  ITCPO STRUCTURE  ITCPO
*"     VALUE(C_ARCHIVE_INDEX) LIKE  TOA_DARA STRUCTURE  TOA_DARA
*"       DEFAULT SPACE
*"     VALUE(C_ARCHIVE_PARAMS) LIKE  ARC_PARAMS STRUCTURE  ARC_PARAMS
*"       DEFAULT SPACE
*"----------------------------------------------------------------------
  DATA: ls_toa_dara   TYPE toa_dara.
  DATA: ls_pri_params TYPE pri_params.
  DATA: ls_arc_params TYPE arc_params.
  DATA: lv_ar_object  TYPE saeobjart.

  DATA: BEGIN OF ls_avis_id,
          bukrs TYPE reguh-zbukr,
          belnr TYPE reguh-vblnr,
          gjahr TYPE regud-gjahr,
        END OF ls_avis_id.

  CHECK i_reguh-xvorl EQ space.

**  SELECT SINGLE ar_object INTO lv_ar_object
**        FROM /sbk/fi_c000006
**        WHERE bukrs = i_reguh-zbukr.
**
**  CHECK sy-subrc = 0.


  CASE i_reguh-laufi+5(1).

    WHEN space OR '*'.     "maschineller Zahllauf oder Online-Erstellung

      ls_avis_id-bukrs = i_reguh-zbukr.
      ls_avis_id-belnr = i_reguh-vblnr.
      ls_avis_id-gjahr = i_gjahr.

      ls_toa_dara-function   = 'DARA'.
      ls_toa_dara-mandant    = sy-mandt.
      ls_toa_dara-sap_object = 'BKPF'.
      ls_toa_dara-ar_object  = lv_ar_object.
      ls_toa_dara-object_id  = ls_avis_id.
      ls_toa_dara-reserve(6) = 'COMMIT'.
      ls_toa_dara-notiz      = 'Zahlungsavis'(001).

* Set Archive_Index

      c_archive_index = ls_toa_dara.

    WHEN OTHERS.

      EXIT.

  ENDCASE.

* Set Archive Mode

  c_itcpo-tdarmod = '3'.


  DATA: valid.

* Collect Print- and Archive-Parameters

  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
*     ARCHIVE_ID             = C_CHAR_UNKNOWN
      archive_info           = '000'
      archive_mode           = c_itcpo-tdarmod
      archive_text           = 'Zahlungsavis'(001)
      ar_object              = ls_toa_dara-ar_object
*     ARCHIVE_REPORT         = C_CHAR_UNKNOWN
*     AUTHORITY              = C_CHAR_UNKNOWN
*     COPIES                 = C_NUM3_UNKNOWN
*     COVER_PAGE             = C_CHAR_UNKNOWN
*     DATA_SET               = C_CHAR_UNKNOWN
*     DEPARTMENT             = C_CHAR_UNKNOWN
*     destination            = C_CHAR_UNKNOWN
*     expiration             = 0
      immediately            = c_itcpo-tdimmed
*     IN_ARCHIVE_PARAMETERS  = ' '
*     IN_PARAMETERS          = ' '
*     LAYOUT                 = C_CHAR_UNKNOWN
*     LINE_COUNT             = C_INT_UNKNOWN
*     LINE_SIZE              = C_INT_UNKNOWN
*     list_name              = ' '
*     list_text              = ' '
*     mode                   = C_CHAR_UNKNOWN
      new_list_id            = c_itcpo-tdnewid
      no_dialog              = 'X'
      receiver               = sy-uname
*     RELEASE                = C_CHAR_UNKNOWN
*     REPORT                 = C_CHAR_UNKNOWN
*     SAP_COVER_PAGE         = C_CHAR_UNKNOWN
      sap_object             = ls_toa_dara-sap_object
*     TYPE                   = C_CHAR_UNKNOWN
    IMPORTING
      out_archive_parameters = ls_arc_params
      out_parameters         = ls_pri_params
      valid                  = valid
    EXCEPTIONS
      archive_info_not_found = 1
      invalid_print_params   = 2
      invalid_archive_params = 3
      OTHERS                 = 4.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Set Archive_Parameters

  c_archive_params = ls_arc_params.

  CLEAR ls_arc_params.
  CLEAR ls_pri_params.




ENDFUNCTION.
