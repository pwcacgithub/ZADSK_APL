*&---------------------------------------------------------------------*
*& Function Module: ZFI_COSTACTPLN_POSTPRIMCOST
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 02-JUN-2020 | 477237              | CF.CNV.045/    | DFDK900167   | Costs and Activity Type Planning for Data Migration                 |
*             |                     | CF.CNV.046/    |              |                                                                     |
*             |                     | CF.CNV.056     |              |                                                                     |
*-----------------------------------------------------------------------------------------------------------------------------------------*

FUNCTION ZFI_COSTACTPLN_POSTPRIMCOST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(HEADERINFO) TYPE  BAPIPLNHDR
*"     VALUE(DELTA) TYPE  BAPI6031_CTRLDATA-DELTA DEFAULT SPACE
*"     VALUE(TESTRUN) TYPE  CHAR01 DEFAULT 'X'
*"  TABLES
*"      INDEXSTRUCTURE STRUCTURE  BAPIACPSTRU
*"      COOBJECT STRUCTURE  BAPIPCPOBJ
*"      PERVALUE STRUCTURE  BAPIPCPVAL OPTIONAL
*"      TOTVALUE STRUCTURE  BAPIPCPTOT OPTIONAL
*"      CONTRL STRUCTURE  BAPIPCPCTRL OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

*** Data Declaration
  DATA: ls_return TYPE bapiret2.

*** Call 'BAPI_COSTACTPLN_POSTPRIMCOST' to perform Cost & Activity Planning
  CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
    EXPORTING
      headerinfo     = headerinfo
      delta          = delta
    TABLES
      indexstructure = indexstructure[]
      coobject       = coobject[]
      pervalue       = pervalue[]
      totvalue       = totvalue[]
      contrl         = contrl[]
      return         = return[].

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
