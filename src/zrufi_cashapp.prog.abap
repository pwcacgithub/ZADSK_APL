*&---------------------------------------------------------------------*
*& Report ZRUFI_CASHAPP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 09-APR-2021 | 486861      | CF.ENH.762     | DFDK903594   | Clear On Account AR Postings against Open Invoice                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
REPORT zrufi_cashapp MESSAGE-ID zfi_msgs.

***Data Declaration
INCLUDE zinfi_cashapp_top.

***Selection screen design
INCLUDE zinfi_cashapp_sel.

***Processing logic for cash application
INCLUDE zinfi_cashapp_sub.
*
***INITIALIZATION.
******* Get the constants for the program
***  PERFORM f_get_constants USING sy-cprog.
***
****** Collect all constants
***  PERFORM f_collect_constants.
*
*AT SELECTION-SCREEN OUTPUT.
****Make input field for file editable or uneditable
****based on checkbox
*  PERFORM f_screen_modification.
*
*
*AT SELECTION-SCREEN.
****If both business partner and contract account are blank
****Issue error message
*  IF p_buspar IS INITIAL AND p_conact IS INITIAL.
*    MESSAGE e157.
*  ENDIF.
*
*  IF sy-ucomm = 'ONLI'.
****If file check box is clicked but no file provided
*    IF p_check IS NOT INITIAL AND p_bpfile IS INITIAL.
*       MESSAGE e164.
*    ENDIF.
*  ENDIF.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bpfile.
****F4 for file location to upload
*  PERFORM f_get_filelocation.
*
*
*START-OF-SELECTION.
*
****Instantiate event class
*  CREATE OBJECT go_alv_events.
****Create Object for the Utility Class
*  CREATE OBJECT go_utility.
*
****Get on-act open items
*  PERFORM f_fill_onact_data.
*
****Get invoice open items
*  PERFORM f_fill_openinvoice_data.
*
*
****Get data from CSV file
*  PERFORM f_get_data_from_file.
*
****Get GL account from config for difference amount
*  PERFORM f_get_difference_gl.
*
****Perform Simulation before actual posting
*  PERFORM f_simulate_post CHANGING gv_error.
*
*End-OF-SELECTION.
****Display ALV for further processing
*  PERFORM f_display_result.
