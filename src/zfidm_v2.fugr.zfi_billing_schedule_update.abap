*&---------------------------------------------------------------------*
*& Function Module: ZFI_BILLING_SCHEDULE_UPDATE
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 21-JUL-2020 | 477237              | CF.CNV.054     | DFDK900167   | Open Order (Consulting) - Load Billing Plan                         |
*-----------------------------------------------------------------------------------------------------------------------------------------*

FUNCTION zfi_billing_schedule_update.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TESTRUN) TYPE  CHAR01 DEFAULT 'X'
*"  TABLES
*"      IT_FPLA STRUCTURE  FPLAVB
*"      IT_FPLT STRUCTURE  FPLTVB
*"      IT_RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

*** Internal Table Declaration
  DATA: lt_fpla_old TYPE STANDARD TABLE OF fplavb,
        lt_fplt_old TYPE STANDARD TABLE OF fpltvb,
        lt_return   TYPE STANDARD TABLE OF bapiret2.

*** Structure Declaration
  DATA: ls_bapisdhd1x TYPE bapisdhd1x.

*** Fetch the Billing Plan Header from FPLA table
  IF it_fpla[] IS NOT INITIAL.
    SELECT * FROM fpla
      INTO TABLE @DATA(lt_fpla)
      FOR ALL ENTRIES IN @it_fpla[]
      WHERE fplnr = @it_fpla-fplnr.
    IF sy-subrc EQ 0.
      SORT lt_fpla BY fplnr.

*** Check Billing Plan Items already exist in FPLT table
      IF it_fplt[] IS NOT INITIAL.
        SELECT COUNT(*) FROM fplt
          INTO @DATA(lv_fplt_count)
          FOR ALL ENTRIES IN @it_fplt[]
          WHERE fplnr = @it_fplt-fplnr
            AND fpltr = @it_fplt-fpltr.
        IF sy-subrc EQ 0.
*** Add Error Message
          it_return-type = 'E'.
          it_return-message = 'Billing Plan Item already exist'(003).
          APPEND it_return.
          CLEAR it_return.
        ENDIF.
      ENDIF.

      IF it_fplt[] IS NOT INITIAL.
*** Fetch the Internal & External Milestone No. from mapping table ZTFI_MLST_MAP
        SELECT ext_mlst_zaehl, mlst_zaehl
          FROM ztfi_mlst_map
          INTO TABLE @DATA(lt_ztfi_mlst_map)
          FOR ALL ENTRIES IN @it_fplt[]
          WHERE ext_mlst_zaehl = @it_fplt-mlstn.
        IF sy-subrc EQ 0.
          SORT lt_ztfi_mlst_map BY ext_mlst_zaehl mlst_zaehl.
        ELSE.
*** No error handling needed here, as there might be cases exist, where Billing Plan have no Milestone
        ENDIF.

*** Replace the External Milestone No. with Internal Milestone No.
        LOOP AT it_fplt ASSIGNING FIELD-SYMBOL(<fs_fplt>).
          TRY .
              DATA(lw_ztfi_mlst_map) = lt_ztfi_mlst_map[ ext_mlst_zaehl = <fs_fplt>-mlstn ].
              <fs_fplt>-mlstn = lw_ztfi_mlst_map-mlst_zaehl.
            CATCH cx_sy_itab_line_not_found.
*** Add Error Message if Billing Plan have Milestone which is not available in Mapping Table
              IF <fs_fplt>-mlstn IS NOT INITIAL.
                it_return-type = 'E'.
                it_return-message = 'External Milestone not available in ZTFI_MLST_MAP Mapping Table'(004).
                APPEND it_return.
                CLEAR it_return.
              ENDIF.
          ENDTRY.
        ENDLOOP.
      ENDIF.

      IF lt_fpla[] IS NOT INITIAL.
        lt_fpla_old = CORRESPONDING #( lt_fpla ).
      ENDIF.

*** Call BILLING_SCHEDULE_SAVE to update the Billing Plan
*** if no errors
      IF it_return[] IS INITIAL.
        CALL FUNCTION 'BILLING_SCHEDULE_SAVE'
          TABLES
            fpla_new = it_fpla
            fpla_old = lt_fpla_old
            fplt_new = it_fplt
            fplt_old = lt_fplt_old.
      ENDIF.

*** Check if Test/Update Mode
      IF testrun IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.

*** Change the Sales Order once the Billin Plan Update is done
*** to update the Revenue Documents
        LOOP AT it_fpla INTO DATA(lw_fpla).
          ls_bapisdhd1x-updateflag = 'U'.
          CALL FUNCTION 'SD_SALESDOCUMENT_CHANGE'
            EXPORTING
              salesdocument     = lw_fpla-vbeln
              order_header_inx  = ls_bapisdhd1x
              simulation        = testrun
            TABLES
              return            = lt_return
            EXCEPTIONS
              incov_not_in_item = 1
              OTHERS            = 2.
          IF sy-subrc <> 0.
*** Fill Return Parameter with error message
            it_return-type = 'E'.
            it_return-message = 'Error in updating the Sales Order'(006).
            APPEND it_return.
            CLEAR it_return.
          ELSE.
            LOOP AT lt_return INTO DATA(lw_return)
              WHERE type CA 'AEX'.
*** Fill Return Parameter with error message
              it_return-type = 'E'.
              it_return-message = lw_return-message.
              APPEND it_return.
              CLEAR it_return.
            ENDLOOP.
          ENDIF.
          CLEAR:  ls_bapisdhd1x,
                  lt_return.
        ENDLOOP.

*** Check any error occured in updating the Sales Order
*** Rollback if any error, else do Commit
        IF it_return[] IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING wait = 'X'.
        ENDIF.
      ENDIF.

    ELSE.
*** Fill Return Parameter with error message
      it_return-type = 'E'.
      it_return-message = 'Billing Plan Number not found'(005).
      APPEND it_return.
      CLEAR it_return.
    ENDIF.
  ENDIF.

ENDFUNCTION.
