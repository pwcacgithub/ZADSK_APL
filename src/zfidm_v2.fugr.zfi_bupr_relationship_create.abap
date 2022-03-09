*&---------------------------------------------------------------------*
*& Function Module: ZFI_BUPR_RELATIONSHIP_CREATE
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-AUG-2020 | 477237              | CF.CNV.024     | DFDK900167   | Create BP Relationship                                              |
*-----------------------------------------------------------------------------------------------------------------------------------------*

FUNCTION zfi_bupr_relationship_create.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BUSINESSPARTNER1) LIKE  BAPIBUS1006_HEAD-BPARTNER
*"     VALUE(BUSINESSPARTNER2) LIKE  BAPIBUS1006_HEAD-BPARTNER
*"     VALUE(RELATIONSHIPCATEGORY) LIKE  BUT050-RELTYP
*"     VALUE(RELATIONSHIPTYPE) LIKE  BUT050-RELKIND OPTIONAL
*"     VALUE(VALIDFROMDATE) LIKE  BAPIBUS1006002_HEADER-VALIDFROMDATE
*"       DEFAULT SY-DATLO
*"     VALUE(VALIDUNTILDATE) LIKE  BAPIBUS1006002_HEADER-VALIDUNTILDATE
*"       DEFAULT '99991231'
*"     VALUE(DIFFERENTIATIONTYPEVALUE) LIKE  BUT050-DFTVAL OPTIONAL
*"     VALUE(XDFREL) LIKE  BUT050-XDFREL OPTIONAL
*"     VALUE(TESTRUN) TYPE  CHAR01 DEFAULT 'X'
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

*** Data Declaration
  DATA: ls_return TYPE bapiret2.

*** Call Standard BAPI to add the Relationship
  CALL FUNCTION 'BAPI_BUPR_RELATIONSHIP_CREATE'
    EXPORTING
      businesspartner1         = businesspartner1
      businesspartner2         = businesspartner2
      relationshipcategory     = relationshipcategory
      relationshiptype         = relationshiptype
      validfromdate            = validfromdate
      validuntildate           = validuntildate
      differentiationtypevalue = differentiationtypevalue
      xdfrel                   = xdfrel
    TABLES
      return                   = return[].

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
