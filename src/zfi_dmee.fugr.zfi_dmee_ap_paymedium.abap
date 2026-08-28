FUNCTION zfi_dmee_ap_paymedium.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_FPAYH) TYPE  FPAYH
*"     VALUE(I_FPAYHX) TYPE  FPAYHX
*"  TABLES
*"      T_FPAYP STRUCTURE  FPAYP
*"      T_PAYMENT_DETAILS STRUCTURE  FPM_PAYD
*"  CHANGING
*"     REFERENCE(C_XAVIS_REQ)
*"----------------------------------------------------------------------
  CONSTANTS lc_kd   TYPE char2  VALUE 'KD'.              " Kundenummer

  LOOP AT t_payment_details ASSIGNING FIELD-SYMBOL(<details>) WHERE type = '1'.
    IF <details>-line = '1'. " Erste Zeile
      CONTINUE.
    ELSE.
      REPLACE ALL OCCURRENCES OF lc_kd IN <details>-text WITH ''.
      REPLACE ALL OCCURRENCES OF i_fpayh-eikto IN <details>-text WITH ''.
      CONDENSE <details>-text.
      SHIFT <details>-text LEFT DELETING LEADING ' '.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
