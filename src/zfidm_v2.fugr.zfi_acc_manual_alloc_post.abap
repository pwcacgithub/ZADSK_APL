*&---------------------------------------------------------------------*
*& Function Module: ZFI_ACC_MANUAL_ALLOC_POST
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer           | RICEFW/Defect# | Transport# | Description                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 02-JUN-2020 | 477237              | CF.CNV.047     | DFDK900167 | Accounting: Post Manual Cost Allocation                               |
*-----------------------------------------------------------------------------------------------------------------------------------------*

FUNCTION zfi_acc_manual_alloc_post.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DOC_HEADER) TYPE  BAPIDOCHDRU12P
*"     VALUE(IGNORE_WARNINGS) TYPE  BAPIIW-IGNWARN DEFAULT SPACE
*"     VALUE(TESTRUN) TYPE  CHAR01 DEFAULT 'X'
*"  EXPORTING
*"     VALUE(DOC_NO) TYPE  BAPIDOCHDRU12P-DOC_NO
*"  TABLES
*"      DOC_ITEMS STRUCTURE  BAPIMAITM
*"      RETURN STRUCTURE  BAPIRET2
*"      CUSTOMER_FIELDS STRUCTURE  BAPIEXTC OPTIONAL
*"----------------------------------------------------------------------

*** Data Declaration
  DATA: ls_return TYPE bapiret2.

*** Call 'BAPI_ACC_MANUAL_ALLOC_POST' to post Actual Hours
  CALL FUNCTION 'BAPI_ACC_MANUAL_ALLOC_POST'
    EXPORTING
      doc_header      = doc_header
      ignore_warnings = ignore_warnings
    IMPORTING
      doc_no          = doc_no
    TABLES
      doc_items       = doc_items[]
      return          = return[]
      customer_fields = customer_fields[].

*** Check the TestRun Parameter
  IF testrun IS NOT INITIAL.
*** Rollback the changes
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = ls_return.
  ENDIF.

*** Append the messages to Return Parameter
  IF ls_return IS NOT INITIAL.
    APPEND ls_return TO return.
    CLEAR ls_return.
  ENDIF.

ENDFUNCTION.
