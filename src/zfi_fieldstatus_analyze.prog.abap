*&---------------------------------------------------------------------*
*& Report ZFI_FIELDSTATUS_ANALYZE                                      *
*&---------------------------------------------------------------------*
*& Date:   18.09.2024                                                  *
*& Author: Hannes Maisch (HANNESM)                                     *
*& Company:                                                            *
*& Requested from:                                                     *
*& Description: Analyseprogramm der diversen Feldstatusgruppen         *
*&---------------------------------------------------------------------*
*& Change History                                                      *
*& Date        | Author   | CR &  Description                          *
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
DATA ok_code_0100    TYPE sy-ucomm.
DATA gv_title        TYPE sy-title.
DATA gt_out          TYPE STANDARD TABLE OF zfi_tmodf_s.
DATA string_pos      TYPE i.
DATA incoming_string TYPE c LENGTH 200.


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
  gv_title = sy-title.

START-OF-SELECTION.
  TRY.
      DATA(go_grid) = NEW zbc_grid_alv( 'CONTAINER' ).
    CATCH zcx_grid_alv INTO DATA(error).
      MESSAGE error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
      EXIT.
  ENDTRY.
  DATA(lt_fcat) = go_grid->generate_fcat( it_outtab = gt_out ).

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

  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  go_grid->handle_user_command( e_ucomm = ok_code_0100 ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLE_0100' WITH gv_title.

  TRY.
      go_grid->set_table_for_first_display( EXPORTING iv_repid        = sy-repid
                                            CHANGING  it_outtab       = gt_out[]
                                                      it_fieldcatalog = lt_fcat[]                       ).
    CATCH zcx_grid_alv INTO DATA(error2). " Ausnahmeklasse für GRID-ALV-Klasse
      MESSAGE error2->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
      EXIT.
  ENDTRY.

ENDMODULE.

FORM fill_output USING out TYPE zfi_tmodf_s.
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
    ASSIGN lt_fcat[ fieldname = <tmodf>-modif ] TO FIELD-SYMBOL(<ls_fcat>).
    IF sy-subrc = 0.
      <ls_fcat>-scrtext_s = <tmodf>-text.
      <ls_fcat>-scrtext_m = <tmodf>-text.
      <ls_fcat>-scrtext_l = <tmodf>-text.
    ENDIF.
  ENDLOOP.
ENDFORM.
