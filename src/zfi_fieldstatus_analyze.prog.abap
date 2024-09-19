*&---------------------------------------------------------------------*
*& Report ZFI_FIELDSTATUS_ANALYZE                                      *
*&---------------------------------------------------------------------*
*& Date:   18.09.2024                                                  *
*& Author: Hannes Maisch (HANNESM)                                     *
*& Company:                                                            *
*& Requested from:                                                     *
*& Description: Analysis program of the various field status groups.   *
*&---------------------------------------------------------------------*
*& Change History                                                      *
*& Date        | Author   | CR &  Description                          *
*&---------------------------------------------------------------------*
*& 19.09.2024  | HANNESM  | Adjustment of the output of the report to  *
*&             |          | use the OO-ALV output logic.               *
*&---------------------------------------------------------------------*
REPORT zfi_fieldstatus_analyze.
TYPES: BEGIN OF lty_sel,
         bukrs TYPE bukrs,
         hkont TYPE skb1-saknr,
         fstav TYPE t004f-bukrs,
         fstag TYPE t004f-fstag,
         bschl TYPE tbsl-bschl,
       END OF lty_sel.

CONSTANTS lc_fauna TYPE tmodf-fauna VALUE 'SKB1-FAUS1'.

DATA gs_sel          TYPE lty_sel.
DATA gt_out          TYPE STANDARD TABLE OF zfi_tmodf_s.
DATA string_pos      TYPE i.
DATA incoming_string TYPE c LENGTH 200.
DATA lr_column       TYPE REF TO cl_salv_column_table.


SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  PARAMETERS p_r01 TYPE boole_d RADIOBUTTON GROUP g01 DEFAULT 'X' USER-COMMAND xxx.
  SELECT-OPTIONS so_bukrs FOR gs_sel-bukrs MEMORY ID buk MODIF ID z01.
  SELECT-OPTIONS so_hkont FOR gs_sel-hkont MODIF ID z01.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS p_r02 TYPE boole_d RADIOBUTTON GROUP g01.
  SELECT-OPTIONS so_fstav FOR gs_sel-fstav NO-EXTENSION NO INTERVALS OBLIGATORY DEFAULT '0001' MODIF ID z02.
  SELECT-OPTIONS so_fstag FOR gs_sel-fstag MODIF ID z02.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS p_r03 TYPE boole_d RADIOBUTTON GROUP g01.
  SELECT-OPTIONS so_bschl FOR gs_sel-bschl MODIF ID z03.
SELECTION-SCREEN END OF BLOCK b01.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_r01 = abap_true.
      IF screen-group1 = 'Z02' OR screen-group1 = 'Z03'.
        screen-input = 0.
      ENDIF.
    ELSEIF p_r02 = abap_true.
      IF screen-group1 = 'Z01' OR screen-group1 = 'Z03'.
        screen-input = 0.
      ENDIF.
    ELSEIF p_r03 = abap_true.
      IF screen-group1 = 'Z01' OR screen-group1 = 'Z02'.
        screen-input = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

INITIALIZATION.

START-OF-SELECTION.
  " --- OO ALV Display
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = DATA(mr_out)
                              CHANGING  t_table      = gt_out[]   ).
    CATCH cx_salv_msg.
  ENDTRY.

  SELECT tmodf~ggrup,
         tmodg~ftext,
         tmodp~modif,
         tmodp~ftext AS text
    FROM tmodf AS tmodf
           INNER JOIN
             tmodg AS tmodg ON tmodg~fauna = tmodf~fauna AND tmodg~ggrup = tmodf~ggrup
               INNER JOIN
                 tmodo AS tmodo ON tmodo~ggrup = tmodf~ggrup
                   INNER JOIN
                     tmodp AS tmodp ON tmodp~fauna = tmodo~fauna AND tmodp~modif = tmodo~modif
    WHERE tmodf~fauna = @lc_fauna
      AND tmodg~spras = @sy-langu
      AND tmodo~fauna = @lc_fauna
      AND tmodp~spras = @sy-langu
    INTO TABLE @DATA(tmodf).

  IF p_r01 = abap_true.
    REFRESH: so_fstag, so_fstav.
    SELECT fstva FROM t001 WHERE bukrs IN @so_bukrs GROUP BY fstva INTO TABLE @DATA(bukrs).
    IF sy-subrc <> 0.
      LEAVE LIST-PROCESSING.
    ENDIF.
    IF so_hkont[] IS NOT INITIAL.
      SELECT fstag FROM skb1 WHERE bukrs IN @so_bukrs AND saknr IN @so_hkont GROUP BY fstag INTO TABLE @DATA(skb1).
    ENDIF.
    LOOP AT bukrs ASSIGNING FIELD-SYMBOL(<bukrs>).
      APPEND VALUE #( sign   = 'I'
                      option = 'EQ'
                      low    = <bukrs>-fstva ) TO so_fstav.
      IF skb1 IS NOT INITIAL.
        LOOP AT skb1 ASSIGNING FIELD-SYMBOL(<skb1>).
          APPEND VALUE #( sign   = 'I'
                          option = 'EQ'
                          low    = <skb1>-fstag ) TO so_fstag.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ELSEIF p_r02 = abap_true.
    REFRESH: so_hkont, so_bukrs, so_bschl.
  ELSEIF p_r03 = abap_true.
    REFRESH: so_bukrs, so_fstag, so_fstav, so_hkont.
  ENDIF.

  IF p_r03 = abap_false.
    SELECT * FROM t004f WHERE bukrs IN @so_fstav AND fstag IN @so_fstag INTO TABLE @DATA(t004f).
    IF sy-subrc <> 0.
      LEAVE LIST-PROCESSING.
    ENDIF.
    SORT t004f BY bukrs ASCENDING
                  fstag ASCENDING.

    LOOP AT t004f ASSIGNING FIELD-SYMBOL(<t004f>).
      incoming_string        = <t004f>-faus1.
      incoming_string+90(50) = <t004f>-faus2.
      AT NEW fstag.
        APPEND INITIAL LINE TO gt_out ASSIGNING FIELD-SYMBOL(<out>).
      ENDAT.
      <out>-fstag = <t004f>-fstag.
      <out>-fstva = <t004f>-bukrs.
      PERFORM fill_output USING <out>.
    ENDLOOP.
  ELSE.
    SELECT * FROM tbsl WHERE bschl IN @so_bschl AND faus1 <> @space INTO TABLE @DATA(bschl).
    IF sy-subrc <> 0.
      LEAVE LIST-PROCESSING.
    ENDIF.
    SORT bschl BY bschl ASCENDING.
    LOOP AT bschl ASSIGNING FIELD-SYMBOL(<bschl>).
      incoming_string = <bschl>-faus1.
      incoming_string+90(50) = <bschl>-faus2.
      AT NEW bschl.
        APPEND INITIAL LINE TO gt_out ASSIGNING <out>.
      ENDAT.
      <out>-bschl = <bschl>-bschl.
      PERFORM fill_output USING <out>.
    ENDLOOP.
  ENDIF.

  CHECK gt_out IS NOT INITIAL.

  " -- Zebramuster aktivieren
  mr_out->get_display_settings( )->set_striped_pattern( cl_salv_display_settings=>true ).

  " --- Funktionen (Toolbar)
  mr_out->get_functions( )->set_all( abap_true ).

  " --- Optimale Spalatenbreite
  mr_out->get_columns( )->set_optimize( abap_true ).

  " --- Optimale Spalatenbreite
  mr_out->get_columns( )->set_optimize( abap_true ).

  mr_out->display( ).


FORM fill_output USING out TYPE zfi_tmodf_s.
  DATA lv_column TYPE lvc_fname.
  DATA lv_lg_txt TYPE scrtext_l.

  LOOP AT tmodf ASSIGNING FIELD-SYMBOL(<tmodf>).
    string_pos = <tmodf>-modif.
    string_pos -= 1.

    ASSIGN COMPONENT <tmodf>-modif OF STRUCTURE out TO FIELD-SYMBOL(<value>).

    IF <value> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    CASE incoming_string+string_pos(1).
      WHEN '+'. " Obligatory
        <value> = TEXT-m01.
      WHEN '.'. " Optional
        <value> = TEXT-m02.
      WHEN '-'. " Suppress
        <value> = TEXT-m03.
      WHEN OTHERS.
        " do nothing
    ENDCASE.

    CLEAR lr_column.
    CLEAR lv_column.
    CLEAR lv_lg_txt.
    TRY.
        lv_column = <tmodf>-modif.
        lr_column ?= mr_out->get_columns( )->get_column( columnname = lv_column ).

        lr_column->set_short_text( value = <tmodf>-text(10) ). " 10-Zeichen
        lr_column->set_medium_text( value = <tmodf>-text(20) ). " 20-Zeichen
        lv_lg_txt = <tmodf>-text.
        lr_column->set_long_text( value = lv_lg_txt ). " 40-Zeichen

      CATCH cx_salv_not_found INTO DATA(error).
**        MESSAGE error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      CATCH cx_salv_existing INTO DATA(error2).
**        MESSAGE error2->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      CATCH cx_salv_data_error INTO DATA(error3).
**        MESSAGE error3->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDLOOP.
ENDFORM.
