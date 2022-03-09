*&---------------------------------------------------------------------*
*& Report ZIOFI_CHARGE_IMPORT_TENROX
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 01-SEP-2020 | 477237              | CF.INT.017     | DFDK900449   | Charge Import to Tenrox                                             |
*-----------------------------------------------------------------------------------------------------------------------------------------*
REPORT ziofi_charge_import_tenrox MESSAGE-ID zfi_msgs.

*** Global Declarations
INCLUDE zinfi_charge_import_tenrox_top.

*** Selection Screen Declarations
INCLUDE zinfi_charge_import_tenrox_sel.

*** Sub Routine Definitions
INCLUDE zinfi_charge_import_tenrox_sub.

**----------------------------------------------------------------------*
** Initialization Event
**----------------------------------------------------------------------*
*INITIALIZATION.
**** Get constants for the program.
*  PERFORM f_get_constants USING sy-cprog.
*
**** Collect all constants
*  PERFORM f_collect_constants.
*
**----------------------------------------------------------------------*
** At Selection Screen
**----------------------------------------------------------------------*
*AT SELECTION-SCREEN.
*  PERFORM f_validate_rundt.
*
**----------------------------------------------------------------------*
** Start of Selection
**----------------------------------------------------------------------*
*START-OF-SELECTION.
**** Create Object for the Utility Class
*  CREATE OBJECT go_utility.
*
**** Fetch the data
*  PERFORM f_fetch_data.
*
**** Check the Test Run Checkbox
*  IF p_test IS INITIAL.
***** Send the data
*    PERFORM f_send_data.
*
*    IF gv_error_flag IS INITIAL.
**** Set the Last Run Date
*      PERFORM f_set_last_run_date.
*
**** Update Tenrox Log table with new/error records
*      PERFORM f_upd_log_table.
*    ELSE.
*      CLEAR gt_data.
*      WRITE gv_message.
*      MESSAGE e137.
*    ENDIF.
*  ENDIF.
*
**----------------------------------------------------------------------*
** End of Selection
**----------------------------------------------------------------------*
*END-OF-SELECTION.
**** Display Output
*  PERFORM f_display_output.
*
**** Clear global variables
*  PERFORM f_clear_global_var.
