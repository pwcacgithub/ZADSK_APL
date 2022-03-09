**&---------------------------------------------------------------------*
**& Include          ZINFI_CASHAPP_SEL
**&---------------------------------------------------------------------*
**-----------------------------------------------------------------------------------------------------------------------------------------*
**                                                          MODIFICATION HISTORY                                                           |
**-----------------------------------------------------------------------------------------------------------------------------------------*
** Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
**-----------------------------------------------------------------------------------------------------------------------------------------*
** 09-APR-2021 | 486861      | CF.ENH.762     | DFDK903594   | Clear On Account AR Postings against Open Invoice                           |
**-----------------------------------------------------------------------------------------------------------------------------------------*
****For on account open items
*SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
*  SELECTION-SCREEN BEGIN OF LINE.
*    SELECTION-SCREEN POSITION 1.
*    SELECTION-SCREEN COMMENT 1(30) TEXT-007 FOR FIELD p_buspar.
*    PARAMETERS : p_buspar TYPE gpart_kk LOWER CASE.
*    SELECTION-SCREEN POSITION 60.
*    SELECTION-SCREEN COMMENT 60(21) TEXT-010 FOR FIELD p_postdt.
*    PARAMETERS : p_postdt TYPE dfkkop-budat OBLIGATORY LOWER CASE.
*  SELECTION-SCREEN END OF LINE.
*
*  SELECTION-SCREEN BEGIN OF LINE.
*    SELECTION-SCREEN POSITION 1.
*    SELECTION-SCREEN COMMENT 1(30) TEXT-008 FOR FIELD p_conact.
*    PARAMETERS : p_conact TYPE vkont_kk LOWER CASE.
*    SELECTION-SCREEN POSITION 60.
*    SELECTION-SCREEN COMMENT 60(21) TEXT-011 FOR FIELD p_curr.
*    PARAMETERS : p_curr TYPE blwae_kk OBLIGATORY.
*  SELECTION-SCREEN END OF LINE.
*
*  SELECTION-SCREEN BEGIN OF LINE.
*    SELECTION-SCREEN POSITION 1.
*    SELECTION-SCREEN COMMENT 1(30) TEXT-009 FOR FIELD p_bukrs.
*    PARAMETERS:p_bukrs  TYPE bukrs LOWER CASE.
*  SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK b1.
*
****For open invoice items
*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
*  SELECT-OPTIONS : s_buspar FOR gv_buspar NO INTERVALS LOWER CASE.
*  SELECT-OPTIONS : s_conact FOR gv_conact NO INTERVALS LOWER CASE.
*  SELECT-OPTIONS : s_comp FOR gv_comp NO INTERVALS LOWER CASE.
*SELECTION-SCREEN END OF BLOCK b2.
*
*SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-013.
*  SELECT-OPTIONS : s_fidoc FOR gv_fidoc NO INTERVALS LOWER CASE.
*  SELECT-OPTIONS : s_inv FOR gv_inv NO INTERVALS LOWER CASE.
**  SELECT-OPTIONS : s_ponum FOR gv_ponum NO INTERVALS LOWER CASE.
*
*  PARAMETERS: p_amount TYPE betrw_kk.
*SELECTION-SCREEN END OF BLOCK b3.
**
****For BP and GL items
*SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-003.
*  SELECTION-SCREEN BEGIN OF LINE.
*    SELECTION-SCREEN POSITION 1.
*    PARAMETERS: p_check AS CHECKBOX USER-COMMAND checkbtn.
*    SELECTION-SCREEN COMMENT 4(30) TEXT-004.
*    SELECTION-SCREEN COMMENT 40(14) TEXT-006 FOR FIELD p_bpfile.
*    PARAMETERS : p_bpfile TYPE rlgrap-filename.
*  SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN END OF BLOCK b4.
