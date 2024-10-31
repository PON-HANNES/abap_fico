*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_C0002.......................................*
DATA:  BEGIN OF STATUS_ZFI_C0002                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_C0002                     .
CONTROLS: TCTRL_ZFI_C0002
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFI_C0002                     .
TABLES: ZFI_C0002                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
