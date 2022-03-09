*&---------------------------------------------------------------------*
*& Function Module: ZFI_BUPR_CONTP_CREATE
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 09-JUN-2020 | 477237              | CF.CNV.022     | DFDK900167   | Create Contact Person Relationship Address                          |
*-----------------------------------------------------------------------------------------------------------------------------------------*

FUNCTION zfi_bupr_contp_create.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BUSINESSPARTNER) LIKE  BAPIBUS1006_HEAD-BPARTNER
*"     VALUE(CONTACTPERSON) LIKE  BAPIBUS1006_HEAD-BPARTNER
*"     VALUE(VALIDFROMDATE) LIKE  BAPIBUS1006002_HEADER-VALIDFROMDATE
*"       DEFAULT '00010101'
*"     VALUE(VALIDUNTILDATE) LIKE  BAPIBUS1006002_HEADER-VALIDUNTILDATE
*"       DEFAULT '99991231'
*"     VALUE(DEFAULTRELATIONSHIP) LIKE
*"        BAPIBUS1006002_HEADER-DEFAULTRELATIONSHIP OPTIONAL
*"     VALUE(ADDRESSGUID) LIKE  BAPIBUS1006002_ADDRESSES-ADDRESSGUID
*"       OPTIONAL
*"     VALUE(CENTRALDATA) LIKE  BAPIBUS1006002_CENTRAL STRUCTURE
*"        BAPIBUS1006002_CENTRAL OPTIONAL
*"     VALUE(ADDRESSDATA) LIKE  BAPIBUS1006002_ADDRESS STRUCTURE
*"        BAPIBUS1006002_ADDRESS OPTIONAL
*"     VALUE(DUPLICATE_MESSAGE_TYPE) LIKE
*"        BAPIBUS1006_HEAD-CONTROLDUPLICATEMESSAGE OPTIONAL
*"     VALUE(TESTRUN) TYPE  CHAR01 DEFAULT 'X'
*"  TABLES
*"      BAPIADTEL STRUCTURE  BAPIADTEL OPTIONAL
*"      BAPIADFAX STRUCTURE  BAPIADFAX OPTIONAL
*"      BAPIADTTX STRUCTURE  BAPIADTTX OPTIONAL
*"      BAPIADTLX STRUCTURE  BAPIADTLX OPTIONAL
*"      BAPIADSMTP STRUCTURE  BAPIADSMTP OPTIONAL
*"      BAPIADRML STRUCTURE  BAPIADRML OPTIONAL
*"      BAPIADX400 STRUCTURE  BAPIADX400 OPTIONAL
*"      BAPIADRFC STRUCTURE  BAPIADRFC OPTIONAL
*"      BAPIADPRT STRUCTURE  BAPIADPRT OPTIONAL
*"      BAPIADSSF STRUCTURE  BAPIADSSF OPTIONAL
*"      BAPIADURI STRUCTURE  BAPIADURI OPTIONAL
*"      BAPIADPAG STRUCTURE  BAPIADPAG OPTIONAL
*"      BAPIAD_REM STRUCTURE  BAPIAD_REM OPTIONAL
*"      BAPICOMREM STRUCTURE  BAPICOMREM OPTIONAL
*"      ADDRESSDUPLICATES STRUCTURE  BAPIBUS1006002_ADDR_DUPLICATES
*"       OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

*** Data Declaration
  DATA: ls_return TYPE bapiret2.

*** Call 'BAPI_BUPR_CONTP_CREATE' to create Contact Person Relationship Address
  CALL FUNCTION 'BAPI_BUPR_CONTP_CREATE'
    EXPORTING
      businesspartner        = businesspartner
      contactperson          = contactperson
      validfromdate          = validfromdate
      validuntildate         = validuntildate
      defaultrelationship    = defaultrelationship
      addressguid            = addressguid
      centraldata            = centraldata
      addressdata            = addressdata
      duplicate_message_type = duplicate_message_type
    TABLES
      bapiadtel              = bapiadtel[]
      bapiadfax              = bapiadfax[]
      bapiadttx              = bapiadttx[]
      bapiadtlx              = bapiadtlx[]
      bapiadsmtp             = bapiadsmtp[]
      bapiadrml              = bapiadrml[]
      bapiadx400             = bapiadx400[]
      bapiadrfc              = bapiadrfc[]
      bapiadprt              = bapiadprt[]
      bapiadssf              = bapiadssf[]
      bapiaduri              = bapiaduri[]
      bapiadpag              = bapiadpag[]
      bapiad_rem             = bapiad_rem[]
      bapicomrem             = bapicomrem[]
      addressduplicates      = addressduplicates[]
      return                 = return[].

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
