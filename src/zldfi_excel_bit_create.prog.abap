*&---------------------------------------------------------------------*
*& Report ZLDFI_EXCEL_BIT_CREATE
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-MAR-2021 | 486859              | CF.ENH.320     | DFDK903320   | This program read the Excel file template and process the ZMJE      |
*                                                                     transaction items to Create BIT & Invoice, Posting Document Number  |
*-----------------------------------------------------------------------------------------------------------------------------------------*
REPORT zldfi_excel_bit_create MESSAGE-ID zfi_msgs.

*** Global Declarations
INCLUDE zinfi_excel_bit_creat_load_top.

*** Selection Screen Declarations
INCLUDE zinfi_excel_bit_creat_load_sel.

*** Sub Routine Definitions
INCLUDE zinfi_excel_bit_creat_load_sub.

**--------------------------------------------------------------*
**Initialization
**--------------------------------------------------------------*
*
*INITIALIZATION.
**--Pass Default values on selection screen
*  PERFORM f_initialize.
***--Get constants for the program.
**  PERFORM f_get_constants USING sy-cprog.
***--Collect all the required constant values
**  PERFORM f_collect_constants.
*
**&---------------------------------------------------------------------*
**&  AT Selection screen on value request
**&---------------------------------------------------------------------*
*AT SELECTION-SCREEN.
*
****Selection screen process
*  PERFORM f_selection_screens.
*
**&---------------------------------------------------------------------*
**&  AT Selection screen on value request
**&---------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*
*  PERFORM f_f4_file_process.
*
**----------------------------------------------------------------------*
** Start of Selection
**----------------------------------------------------------------------*
*START-OF-SELECTION.
*
****Source type, file, file type and file mismatch error process
*  PERFORM f_file_errors_log.
*
**** Create Object for the Utility Class
*  CREATE OBJECT go_utility.
*
*****Bit Read Process****
*  IF gv_file_errors IS INITIAL.
*    PERFORM f_bit_read.
*    IF gv_read_success IS NOT INITIAL.
*      PERFORM f_process_data.
*    ENDIF.
*  ENDIF.
*
****BIT Creation Process****
*  IF gv_selection = gc_s OR gv_selection = gc_l.
*    IF gv_read_success IS NOT INITIAL.
****Commit Flag is initial for Simulation mode
*      IF gv_selection = gc_s.
*        gv_no_commit = abap_true.
*      ENDIF.
*      PERFORM f_bit_create.
*    ELSEIF gt_error_log IS INITIAL.
****Error, if Read is not processed
*      gw_error_log-msgid = gc_messageid.
*      gw_error_log-msgno = gc_002.
*      gw_error_log-msgty = gc_error.
*      gw_error_log-msgv1 = TEXT-014.
*
*      APPEND gw_error_log TO gt_error_log.
*      CLEAR gw_error_log.
*
*      IF gt_error_log IS NOT INITIAL.
*
*        PERFORM f_log_create.
*        PERFORM f_log_show.
*
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
**----------------------------------------------------------------------*
** End of Selection
**----------------------------------------------------------------------*
*END-OF-SELECTION.
*
**** Display Output
*  PERFORM f_alv_display_output.
*
**** Clear global variables
*  PERFORM f_clear_global_var.
