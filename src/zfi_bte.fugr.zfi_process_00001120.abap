FUNCTION zfi_process_00001120.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BKDF) TYPE  BKDF OPTIONAL
*"  TABLES
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEG STRUCTURE  BSEG
*"      T_BKPFSUB STRUCTURE  BKPF_SUBST
*"      T_BSEGSUB STRUCTURE  BSEG_SUBST
*"      T_BSEC STRUCTURE  BSEC OPTIONAL
*"  CHANGING
*"     REFERENCE(I_BKDFSUB) TYPE  BKDF_SUBST OPTIONAL
*"----------------------------------------------------------------------
  DATA lv_date TYPE sy-datum.

  CHECK line_exists( t_bkpf[ tcode = 'F110' ] ). " Payment run

  CLEAR lv_date.

  LOOP AT t_bsegsub ASSIGNING FIELD-SYMBOL(<sub>) WHERE sgtxt IS INITIAL.
    ASSIGN t_bkpf[ 1 ] TO FIELD-SYMBOL(<bkpf>).
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    SPLIT <bkpf>-bktxt AT '-' INTO DATA(str1) DATA(str2).
    lv_date = str1.

    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING  date_internal            = lv_date
      IMPORTING  date_external            = str1
      EXCEPTIONS date_internal_is_invalid = 1
                 OTHERS                   = 2.
    IF sy-subrc <> 0.
      " Implement suitable error handling here
    ENDIF.
    CONCATENATE TEXT-m01 str1 TEXT-m02 str2 INTO <sub>-sgtxt SEPARATED BY space.
  ENDLOOP.
ENDFUNCTION.
