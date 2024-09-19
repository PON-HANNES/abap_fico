*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_C0001.......................................*
DATA:  BEGIN OF STATUS_ZFI_C0001                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_C0001                     .
CONTROLS: TCTRL_ZFI_C0001
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFI_C0001                     .
TABLES: ZFI_C0001                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
