*&---------------------------------------------------------------------*
*& Function Module: ZFI_FKK_CM_DUNNING_DATA_CREATE
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 12-OCT-2020 | 485235              | CF.CNV.018     | DFDK900167   | Create Dunning History - Dunning by Collection Strategy             |
*-----------------------------------------------------------------------------------------------------------------------------------------*
FUNCTION ZFI_FKK_CM_DUNNING_DATA_CREATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_COLLDUNNHEADER) TYPE  FKKCMMIG
*"     VALUE(TESTRUN) TYPE  CHAR01 DEFAULT 'X'
*"  TABLES
*"      IT_ITEMS STRUCTURE  FKKOP
*"      IT_REDUCTIONS STRUCTURE  FKKMARED OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

*** Data Declaration
  DATA: ls_return TYPE bapiret2.

*** Call 'FKK_CM_DUNNING_DATA_CREATE' to create Dunning Data
  CALL FUNCTION 'FKK_CM_DUNNING_DATA_CREATE'
    EXPORTING
      is_colldunnheader       = is_colldunnheader
    TABLES
      it_items                = it_items[]
      it_reductions           = it_reductions[]
      return                  = return[].

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
