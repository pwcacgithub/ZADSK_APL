**&---------------------------------------------------------------------*
**& Include          ZINFI_EXCEL_BIT_CREAT_LOAD_SEL
**&---------------------------------------------------------------------*
*
****Selection Screen for Source Upload
*SELECTION-SCREEN BEGIN OF BLOCK dest WITH FRAME TITLE TEXT-100.
*  PARAMETERS: p_int AS LISTBOX VISIBLE LENGTH 1000 USER-COMMAND upd.
*  PARAMETERS: p_file TYPE rlgrap-filename.
*SELECTION-SCREEN END OF BLOCK dest.
*
****Selection Scenario for MJE selection
*SELECTION-SCREEN BEGIN OF BLOCK type WITH FRAME TITLE TEXT-200.
*  PARAMETERS: p_mje AS LISTBOX VISIBLE LENGTH 1000 USER-COMMAND mje.
*SELECTION-SCREEN END OF BLOCK type.
*
*
*****Selection scenario for Process
*SELECTION-SCREEN BEGIN OF BLOCK load WITH FRAME TITLE TEXT-300.
*  SELECTION-SCREEN BEGIN OF LINE.
*    SELECTION-SCREEN COMMENT 1(16) TEXT-011.
*    PARAMETERS:r6 RADIOBUTTON GROUP rad2  USER-COMMAND sel DEFAULT 'X'.
*    SELECTION-SCREEN COMMENT 20(10) TEXT-008.
*    PARAMETERS:r7 RADIOBUTTON GROUP rad2.
*    SELECTION-SCREEN COMMENT 33(10) TEXT-009.
*    PARAMETERS:r8 RADIOBUTTON GROUP rad2.
*    SELECTION-SCREEN COMMENT 46(15) TEXT-010.
*  SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK load.
