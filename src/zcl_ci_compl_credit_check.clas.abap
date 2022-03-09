class ZCL_CI_COMPL_CREDIT_CHECK definition
  public
  create public .

public section.

  class-methods COMPLIANCE_CHECK
    importing
      !IV_KUNNR type KUNNR
      !IV_VKORG type VKORG optional
      !IV_VTWEG type VTWEG optional
      !IV_SPART type SPART optional
    returning
      value(RV_SUCCESS) type ABAP_BOOLEAN .
  class-methods CREDIT_CHECK
    importing
      !IV_BILLPLANNO type BILLPLANNO_KK
      !IV_BILLPLAN_ITEM type BILLPLANITEM_KK optional
    returning
      value(RV_SUCCESS) type ABAP_BOOLEAN .
  class-methods DO_RELEASE_BILLPLAN_LOCKS
    importing
      !IV_BILLPLANNO type BILLPLANNO_KK
      !IV_COMMIT type BOOLEAN optional
      !IV_EXTEND_VAL type BOOLEAN optional
      !IV_LOCK_REASON type LOCKR_KK default 'L'
      !IV_LOCKOBJ type LOOBJ_KK
    exporting
      !EV_SUCEESS type BOOLEAN
      !ET_RETURN type BAPIRETTAB .
  class-methods LOCK_BILLPLAN_ITEM
    importing
      !IV_BILLPLANNO type BILLPLANNO_KK
      !IV_BILLPLAN_ITEM type BILLPLANITEM_KK
      !IV_LOCK_REASON type LOCKR_KK default 'L'
      !IV_FDATE type FDATE_KK default SY-DATUM
      !IV_NEXT_FDATE_CYCLE type ABAP_BOOLEAN optional
      !IV_TDATE type TDATE_KK optional
      !IV_COMMIT type ABAP_BOOLEAN optional
    exporting
      !EV_SUCCESS type ABAP_BOOLEAN
      !ET_RETURN type BAPIRETTAB .
  class-methods CALC_TODATE
    importing
      !IV_FDATE type FDATE_KK default SY-DATUM
      !IV_BILL_CYCLE type CYCLE_KK
    returning
      value(RV_NEW_DATE) type FDATE_KK .
  class-methods GET_RISK_CLASS
    importing
      !IV_PARTNER type BU_PARTNER
    returning
      value(RV_RISK_CLASS) type UKM_RISK_CLASS .
  class-methods COMPLIANCE_CHECK_EMAIL
    importing
      !IV_BILLPLANNO type BILLPLANNO_KK
      !IV_BILLPLAN_ITEM type BILLPLANITEM_KK optional
    returning
      value(RV_SUCCESS) type ABAP_BOOLEAN .
  class-methods IS_BIPITEM_COMPLIANCE_LOCKED
    importing
      !IV_BILLPLANNO type BILLPLANNO_KK
      !IV_BILLPLAN_ITEM type BILLPLANITEM_KK
      !IV_LOCKDATE type DATS default SY-DATLO
    returning
      value(RV_LOCKED) type ABAP_BOOLEAN .
  class-methods UPDATE_BILLPLAN_BILLING_DATE
    importing
      !IV_BILLPLANNO type BILLPLANNO_KK
      !IV_BILLPLAN_ITEM type BILLPLANITEM_KK optional
      !IV_COMMIT type ABAP_BOOLEAN optional
    exporting
      !EV_SUCCESS type ABAP_BOOLEAN .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CI_COMPL_CREDIT_CHECK IMPLEMENTATION.


  METHOD calc_todate.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD CALC_TODATE                                                                                                              |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 23-Dec-2020 |482601              |CF.INT.108     | DFDK901462     | Preform CALC_TODATE                                          |                                     |
*&-----------------------------------------------------------------------------------------------------------------------------------------*

*-----------------------------------------------------------------------------------------------------------------------------------------*
* CONSTANTS       (c_*)                                                                                                                   |
*-----------------------------------------------------------------------------------------------------------------------------------------*
    CONSTANTS: lc_anually    TYPE cycle_kk           VALUE 'ANNL',
               lc_semi_anual TYPE cycle_kk           VALUE 'SANN',
               lc_qrtly      TYPE cycle_kk           VALUE 'QART',
               lc_monthly    TYPE cycle_kk           VALUE 'MONT',
               lc_weekly     TYPE cycle_kk           VALUE 'WEEK',
               lc_daily      TYPE cycle_kk           VALUE 'DAIL',
               lc_onetime    TYPE cycle_kk           VALUE 'ONET',
               lc_sign       TYPE t5a4a-split        VALUE '+'.
*-----------------------------------------------------------------------------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Data Declarations                                                                                                                       |
*-----------------------------------------------------------------------------------------------------------------------------------------*
    DATA:lv_years  TYPE t5a4a-dlyyr VALUE '1',
         lv_months TYPE t5a4a-dlymo,
         lv_days   TYPE t5a4a-dlydy.
*-----------------------------------------------------------------------------------------------------------------------------------------*

    CASE iv_bill_cycle.
      WHEN lc_anually.
        CLEAR: lv_days,lv_months.
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = iv_fdate
            days      = lv_days
            months    = lv_months
            years     = lv_years
            signum    = lc_sign
          IMPORTING
            calc_date = rv_new_date.
      WHEN lc_semi_anual.
        CLEAR: lv_years, lv_days.
        lv_months = '6'.
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = iv_fdate
            days      = lv_days
            months    = lv_months
            years     = lv_years
            signum    = lc_sign
          IMPORTING
            calc_date = rv_new_date.
      WHEN lc_qrtly.
        CLEAR: lv_years, lv_days.
        lv_months = '3'.
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = iv_fdate
            days      = lv_days
            months    = lv_months
            years     = lv_years
            signum    = lc_sign
          IMPORTING
            calc_date = rv_new_date.
      WHEN lc_monthly.
        CLEAR: lv_years, lv_days.
        lv_months = '1'.
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = iv_fdate
            days      = lv_days
            months    = lv_months
            years     = lv_years
            signum    = lc_sign
          IMPORTING
            calc_date = rv_new_date.
      WHEN lc_weekly.
        CLEAR: lv_months,lv_years.
        lv_days = '7'.
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = iv_fdate
            days      = lv_days
            months    = lv_months
            years     = lv_years
            signum    = lc_sign
          IMPORTING
            calc_date = rv_new_date.
      WHEN lc_daily OR lc_onetime.
        CLEAR: lv_months,lv_years.
        lv_days = '1'.
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = iv_fdate
            days      = lv_days
            months    = lv_months
            years     = lv_years
            signum    = lc_sign
          IMPORTING
            calc_date = rv_new_date.
    ENDCASE.

  ENDMETHOD.


  METHOD compliance_check.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD COMPLIANCE_CHECK                                                                                                                  |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 23-Dec-2020 |482601              |CF.INT.108     | DFDK901462     | Preform Compliance Check                                                 |                                     |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*------------------------------------------------------------------------------------------------------------------------------------------*
* Constants Declaration                                                                                                                         |
*------------------------------------------------------------------------------------------------------------------------------------------*
    CONSTANTS: lc_zx TYPE char2 VALUE 'ZX'.
*------------------------------------------------------------------------------------------------------------------------------------------*

*   perform the compliance check
    SELECT SINGLE kunnr, lifsd, faksd FROM kna1 WHERE kunnr = @iv_kunnr INTO @DATA(ls_kna1).
    IF ls_kna1-lifsd NE lc_zx AND ls_kna1-faksd NE lc_zx.
      IF iv_vkorg IS NOT INITIAL AND iv_vtweg IS NOT INITIAL AND iv_spart IS NOT INITIAL.
        SELECT SINGLE kunnr, lifsd, faksd FROM knvv WHERE kunnr = @iv_kunnr
                                                      AND vkorg = @iv_vkorg AND vtweg = @iv_vtweg AND spart = @iv_spart
                                                      AND ( lifsd = @lc_zx OR faksd = @lc_zx ) INTO @DATA(ls_knvv).
        IF sy-subrc NE 0.
          rv_success = abap_true.
        ENDIF.
      ELSE.
        rv_success = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD compliance_check_email.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD COMPLIANCE_CHECK_EMAIL                                                                                                           |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 23-Dec-2020 |482601              |CF.INT.108     | DFDK901462     | Preform Send email when compliance check failes                      |                                     |
*&-----------------------------------------------------------------------------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Declaration for Constants
*----------------------------------------------------------------------*
    CONSTANTS: lc_cwsub     TYPE string     VALUE 'ZTT_SUB_COMPLIANCE_FAIL',
               lc_cwbod     TYPE string     VALUE 'ZTT_BODY_COMPLIANCE_FAIL',
               lc_b         TYPE char1      VALUE 'B',
               lc_s         TYPE char1      VALUE 'S',
               lc_bip       TYPE so_text255 VALUE '&BIPNO&',
               lc_bipitem   TYPE so_text255 VALUE '&BIPITEM&',
               lv_rtype_001 TYPE n          VALUE '001'.
*----------------------------------------------------------------------*
* Declaration for Variables
*----------------------------------------------------------------------*
    DATA: lv_size        TYPE so_obj_len,
          lv_sub         TYPE so_obj_des,
          lv_string      TYPE string,
          lv_body        TYPE thead-tdname,
          lv_subject     TYPE thead-tdname,
          lv_data_string TYPE string,
          lv_4103        TYPE abap_encod,
          lv_retcode     TYPE i,
          lv_code        TYPE abap_encod,
          lv_err_str     TYPE string,
          lv_mail        TYPE string,
          lv_rec_type    TYPE n VALUE '001'.

    DATA: lt_adrs            TYPE TABLE OF string,
          ltpgm_const_values TYPE TABLE OF zspgm_const_values,
          lterror_const      TYPE TABLE OF zserror_const.

    DATA: lt_attach_attr  TYPE TABLE OF zsca_packlist,
          lt_return       TYPE TABLE OF bapiret2,
          lt_attachment   TYPE solix_tab,
          lt_body         TYPE bcsy_text,
          lt_text_replace TYPE zttca_email_textsymbol_replace.
*----------------------------------------------------------------------*
* Declaration for Reference Objects
*----------------------------------------------------------------------*
    DATA:
      go_mail TYPE REF TO zcl_ca_utility.
*----------------------------------------------------------------------*

    CREATE OBJECT go_mail.

* FM used to retrieve all constants
    CALL FUNCTION 'ZUTIL_PGM_CONSTANTS'
      EXPORTING
        im_pgmid               = 'ZCI_COMPLIANCE_MAIL_REC'
      TABLES
        t_pgm_const_values     = ltpgm_const_values
        t_error_const          = lterror_const
      EXCEPTIONS
        ex_no_entries_found    = 1
        ex_const_entry_missing = 2
        OTHERS                 = 3.


    lv_mail     = VALUE #( ltpgm_const_values[ const_name = 'ZCI_COMPLIANCE_MAIL_REC' ]-low OPTIONAL ).
    lv_rec_type = VALUE #( ltpgm_const_values[ const_name = 'ZCI_COMPLIANCE_MAIL_REC_TYPE' ]-low OPTIONAL ).

    IF lv_mail IS INITIAL.
*     get the email of executed user
      SELECT SINGLE smtp_addr FROM adr6  AS a
                        INNER JOIN usr21 AS u ON u~persnumber = a~persnumber
                                             AND u~addrnumber = a~addrnumber
                             WHERE u~bname = @sy-uname
                              INTO @lv_mail.
      lv_rec_type = lv_rtype_001.

    ENDIF.

***-Assign values to replace in email text
    lt_text_replace = VALUE #(
                      ( key_type = lc_s name = lc_bip       value = iv_billplanno )
                      ( key_type = lc_b name = lc_bip       value = iv_billplanno )
                      ( key_type = lc_b name = lc_bipitem   value = iv_billplan_item ) ) .

** get the subject and body of the email
    lv_subject = lc_cwsub.
    lv_body    = lc_cwbod.

    CALL METHOD go_mail->get_email_content
      EXPORTING
        i_text_name_sub  = lv_subject
        i_text_name_body = lv_body
        i_text_replace   = lt_text_replace
      IMPORTING
        e_subject        = lv_sub
        e_body           = lt_body
        et_return        = lt_return.

    IF lv_mail IS NOT INITIAL.


      TRANSLATE lv_mail TO UPPER CASE.
* Send Mail with attachment
      CALL METHOD go_mail->send_email
        EXPORTING
          i_rec_type             = lv_rec_type
          i_receiver             = lv_mail
          i_subject              = lv_sub
          i_body                 = lt_body
          i_attachment_attribute = lt_attach_attr
          i_immediate            = 'X'
        IMPORTING
          e_retcode              = lv_retcode
          e_err_str              = lv_err_str.

      IF lv_retcode EQ 0.
        rv_success   = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD credit_check.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD CREDIT_CHECK                                                                                                                      |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 23-Dec-2020 |482601              |CF.INT.108     | DFDK901462     | Preform Credit Check                                                 |                                     |
*&-----------------------------------------------------------------------------------------------------------------------------------------*

*------------------------------------------------------------------------------------------------------------------------------------------*
* Constants Declaration                                                                                                                         |
*------------------------------------------------------------------------------------------------------------------------------------------*
    CONSTANTS: lc_error_e TYPE bapi_mtype   VALUE 'E',
               lc_ext_ref TYPE scmg_ext_ref VALUE 'CI_DCD',
               lc_objtype TYPE swo_objtyp   VALUE 'CI_INV',
               lc_35      TYPE scmgstatusonr VALUE '35',
               lc_40      TYPE scmgstatusonr VALUE '40'.

*------------------------------------------------------------------------------------------------------------------------------------------*
* Data Declaration                                                                                                                         |
*------------------------------------------------------------------------------------------------------------------------------------------*
    DATA: lt_billplan_i    TYPE fkkbix_bip_i_tab,
          ls_billplan_h    TYPE fkkbix_bip_h,
          lx_lifsk         TYPE vbak-lifsk,
          lx_partner_block TYPE abap_boolean,
          lt_result        TYPE STANDARD TABLE OF bapiret2.
*** Data Declaration
    DATA: lv_audat          TYPE dats,
          lv_credit_sgmnt   TYPE char10,
          lv_logsys         TYPE logsys,
          lv_dcd_case_title TYPE scmg_case_title.
*------------------------------------------------------------------------------------------------------------------------------------------*

*   select the Billplan.
    CALL FUNCTION 'FKK_BIX_BILLPLAN_SELECT_SINGLE'
      EXPORTING
        i_billplanno  = iv_billplanno
      IMPORTING
        e_billplan_h  = ls_billplan_h
        et_billplan_i = lt_billplan_i.

*   Check the Approved DCD Case is available for the Bill plan Item. If available then skip the CREDIT Check.
    DATA(lv_billplanno)    = |{ iv_billplanno ALPHA = IN }|.
    DATA(lv_billplan_item) = |{ iv_billplan_item ALPHA = IN }|.

    lv_dcd_case_title  = lv_billplanno && lv_billplan_item.

    IF iv_billplan_item IS NOT INITIAL.

      CONVERT DATE VALUE #( lt_billplan_i[ billplanitem = lv_billplan_item ]-requestdate_next OPTIONAL )
              TIME '000000' INTO TIME STAMP  DATA(lv_dcd_ctimestmp) TIME ZONE 'UTC'.

      SELECT * FROM scmg_t_case_attr
                      INTO TABLE @DATA(lt_dcd)
                      WHERE case_title = @lv_dcd_case_title
                        AND create_time GE @lv_dcd_ctimestmp.

      IF sy-subrc EQ 0.
*       IF DCD Case with Approved state.
        IF line_exists( lt_dcd[ stat_orderno = lc_35 ] ).
          rv_success = abap_true.
        ELSE. " DCD Case available without approval.
          rv_success = abap_false.
        ENDIF.
        RETURN.
      ENDIF.
    ENDIF.


    IF  ls_billplan_h IS NOT INITIAL.

      SORT lt_billplan_i BY billplanitem DESCENDING.

      TRY.
          DATA(ls_billplan_i) = lt_billplan_i[ 1 ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

*** Bilplanno + Bill plan Item
      DATA(lv_ktext)  = ls_billplan_i-billplanno && ls_billplan_i-billplanitem.

*       Get the logical system
      CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
        IMPORTING
          own_logical_system             = lv_logsys
        EXCEPTIONS
          own_logical_system_not_defined = 1
          OTHERS                         = 2.

*** Get the Sales Data Currency from KNVV
      SELECT SINGLE waers
        FROM knvv
        INTO @DATA(lv_waers)
        WHERE kunnr = @ls_billplan_h-gpart
          AND vkorg = @ls_billplan_i-bukrs.
      IF sy-subrc EQ 0.
        lv_credit_sgmnt  = ls_billplan_i-bukrs && lv_waers.
      ENDIF.

*      Perform the Credit check
      CALL FUNCTION 'ZFI_CREDIT_CHECK'
        EXPORTING
          im_vbeln                    = CONV vbeln_va( |{ ls_billplan_i-billplanno ALPHA = OUT }| )
          im_ktext                    = CONV ktext_v( lv_ktext )
          im_audat                    = sy-datum
          im_partner                  = ls_billplan_h-gpart
          im_netwr                    = CONV netwr_ak( ls_billplan_i-betrw )
          im_waerk                    = ls_billplan_i-waers
          im_credit_sgmnt             = lv_credit_sgmnt
          im_comm_typ                 = ''
          im_logsys                   = lv_logsys
          im_ext_ref                  = lc_ext_ref
          im_objtype                  = lc_objtype      "CI_INV
        IMPORTING
          ex_lifsk                    = lx_lifsk
          ex_partner_block            = lx_partner_block
        TABLES
          t_result                    = lt_result
        EXCEPTIONS
          pgm_contants_not_maintained = 1
          OTHERS                      = 2.

      IF lx_partner_block EQ abap_false AND NOT line_exists( lt_result[ type = lc_error_e ] ).
        rv_success = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD do_release_billplan_locks.
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer         | RICEFW/Defect# | Transport#   | Description |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 12/17/2020  | 484627            | CF.ENH.730     |  DFDK901462  | Enhancement to relase lock on Billing Plan item                       |
*-----------------------------------------------------------------------------------------------------------------------------------------*

*-----------------------------------------------------------------------------------------------------------------------------------------*
* Local Variable/work area/tables Declaration                                                                                                                    |
*-----------------------------------------------------------------------------------------------------------------------------------------*
    DATA: ls_bip_i           TYPE fkkbix_bip_i,
          ls_lock            TYPE dfkklocks,
          lt_billplan_i      TYPE fkkbix_bip_i_tab,
          ls_billplan_h      TYPE fkkbix_bip_h,
          lt_locks_h         TYPE dfkklocks_t,
          lt_locks_i         TYPE dfkklocks_t,
          lt_expansion_gpart TYPE fkloc_exp_gpart,
          lt_expansion_vkont TYPE fkloc_exp_vkont,
          ls_return          TYPE bapiret2.
*-----------------------------------------------------------------------------------------------------------------------------------------*
* CONSTANTS       (c_*)                                                                                                                   |
*-----------------------------------------------------------------------------------------------------------------------------------------*
    CONSTANTS: lc_cat          TYPE dfkklocks-lotyp VALUE '31',  " lock category as 31 -- Billing Plan Item
               lc_process      TYPE dfkklocks-proid VALUE '14',  " Lock process as 14 -- Request Billing Plans
               lc_complete     TYPE dfkkbix_bip_h-status VALUE 'F', " Bill plan status is Complete.
               lc_x            TYPE boolean            VALUE 'X',
               lc_initial_date TYPE dats               VALUE '00000000'.
**    1. Check if the bill plan item is locked today..

    ls_bip_i-billplanno = iv_billplanno.
    ls_bip_i-billplanitem = iv_lockobj+12(8).

    APPEND ls_bip_i TO lt_billplan_i.

    CALL FUNCTION 'FKK_BIX_BIP_LOCKS_GET'
      EXPORTING
*       i_bip_h         =
        it_bip_i        = lt_billplan_i
        i_lockdate      = sy-datlo
        i_x_mass_access = ' '
      IMPORTING
        et_locks_h      = lt_locks_h
        et_locks_i      = lt_locks_i.
    IF lt_locks_i IS INITIAL.
      MESSAGE e016(zcm_msgs) WITH iv_lockobj INTO ls_return-message.
      APPEND ls_return TO et_return.
      EXIT.
    ELSE. "" select the bill plan header and item info to get Next req date and billing cycle.
      ls_lock = VALUE #( lt_locks_i[ loobj1 = iv_lockobj lotyp = lc_cat proid = lc_process lockr = iv_lock_reason ] OPTIONAL ).
      IF ls_lock IS NOT INITIAL.
        REFRESH: lt_billplan_i.
        CALL FUNCTION 'FKK_BIX_BILLPLAN_SELECT_SINGLE'
          EXPORTING
            i_billplanno  = iv_billplanno
          IMPORTING
            e_billplan_h  = ls_billplan_h
            et_billplan_i = lt_billplan_i.
        IF ls_billplan_h-status = lc_complete. "" lock exists but bill plan is already complete. ==>Delete the lock entry.
* delete all locks  - entire bill plan is exhausted.
          CALL FUNCTION 'FKK_BIX_BIP_LOCKS_DELETE'
            EXPORTING
              i_bip_h    = ls_billplan_h
              it_bip_i   = lt_billplan_i
              i_lockdate = lc_initial_date.
          ev_suceess = lc_x.
        ELSE. """ bill plan is not exhausted. check for BIll plan line item. If exhausted, delete the particular Bill plan item.
          DATA(ls_billplan_i) = VALUE #( lt_billplan_i[ billplanno = iv_billplanno billplanitem = iv_lockobj+12(8) ] OPTIONAL ).
          IF ls_billplan_i-status = lc_complete.
            REFRESH: lt_billplan_i.
            APPEND ls_billplan_i TO lt_billplan_i.
            CALL FUNCTION 'FKK_BIX_BIP_LOCKS_DELETE'
              EXPORTING
                i_bip_h    = ls_billplan_h
                it_bip_i   = lt_billplan_i
                i_lockdate = lc_initial_date.
            ev_suceess = lc_x.
          ELSEIF ls_billplan_i IS NOT INITIAL. "Bill plan item is still valid.
*            """ Increase the lock from date to next cycle.
*              i.e. if lock from date is greater than billing plan line item Validity date - delete the lock entry.
*            ls_lock  is old lock lock entry from FM call FKK_BIX_BIP_LOCK_CHECK. Should be updated.
            DATA(ls_lock_new) = ls_lock.
*** Calculate the new from date for the lock.
            CALL METHOD zcl_ci_compl_credit_check=>calc_todate(
              EXPORTING
                iv_fdate      = ls_lock-fdate
                iv_bill_cycle = ls_billplan_i-cycle
              RECEIVING
                rv_new_date   = ls_lock_new-fdate ).
            IF   iv_extend_val = lc_x.  " check if extention of validity is required else delete all the locks.
              IF ls_lock_new-fdate >= ls_billplan_i-valid_to. " lock from date is equal or greater than validity date of bill plan item,
                " delete the lock entry, else update the lock entry.
                APPEND ls_billplan_i TO lt_billplan_i.
                CALL FUNCTION 'FKK_BIX_BIP_LOCKS_DELETE'
                  EXPORTING
                    i_bip_h    = ls_billplan_h
                    it_bip_i   = lt_billplan_i
                    i_lockdate = lc_initial_date.
                ev_suceess = lc_x.
              ELSE.
                APPEND ls_lock_new-gpart TO lt_expansion_gpart.
                APPEND ls_lock_new-vkont TO lt_expansion_vkont.
                CALL FUNCTION 'FKK_DB_LOCK_UPDATE'
                  EXPORTING
                    i_lock             = ls_lock_new " new lock entry
                    i_oldlock          = ls_lock   " old lock entry.
                    it_expansion_gpart = lt_expansion_gpart
                    it_expansion_vkont = lt_expansion_vkont
                    i_x_lock_del       = space
                  EXCEPTIONS
                    update_failed      = 1
                    OTHERS             = 2.
                IF sy-subrc = 0.
                  ev_suceess = lc_x.
                ELSE.
                  MESSAGE s017(zcm_msgs) WITH iv_lockobj INTO ls_return-message.
                  APPEND ls_return TO et_return.
                ENDIF.
              ENDIF.
            ELSE.
              " delete the lock entry for that particular lock type.
              CALL FUNCTION 'FKK_S_LOCK_DELETE'
                EXPORTING
*                 I_APPL_WORK_AREA      = ls_lock-
                  i_loobj1              = ls_lock-loobj1
                  i_gpart               = ls_lock-gpart
                  i_vkont               = ls_lock-vkont
                  i_proid               = ls_lock-proid
                  i_lotyp               = ls_lock-lotyp
                  i_lockr               = ls_lock-lockr
                  i_fdate               = ls_lock-fdate
                  i_tdate               = ls_lock-tdate
                EXCEPTIONS
                  already_exist         = 1
                  imp_data_not_complete = 2
                  no_authority          = 3
                  enqueue_lock          = 4
                  data_protected        = 5
                  partner_blocked       = 6
                  OTHERS                = 7.
              IF sy-subrc <> 0.
* Implement suitable error handling here
              ENDIF.
              ev_suceess = lc_x.
*              APPEND ls_billplan_i TO lt_billplan_i.
*              CALL FUNCTION 'FKK_BIX_BIP_LOCKS_DELETE'
*                EXPORTING
*                  i_bip_h    = ls_billplan_h
*                  it_bip_i   = lt_billplan_i
*                  i_lockdate = lc_initial_date.
*              ev_suceess = lc_x.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF iv_commit = lc_x.
      "" commit work.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD get_risk_class.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD GET_RISK_CLASS                                                                                                             |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 23-Dec-2020 |482601              |CF.INT.108     | DFDK901462     | Preform Get Risk Class                                        |                                     |
*&-----------------------------------------------------------------------------------------------------------------------------------------*

*   get the Risk class of customer.
    SELECT SINGLE risk_class FROM ukmbp_cms WHERE partner = @iv_partner INTO @rv_risk_class.
  ENDMETHOD.


  METHOD is_bipitem_compliance_locked.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD IS_BIPITEM_COMPLIANCE_LOCKED                                                                                                     |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 23-Dec-2020 |482601              |CF.INT.108     | DFDK901462     | Check Whether Bill Plan Item is locked                               |                                     |
*&-----------------------------------------------------------------------------------------------------------------------------------------*

*-----------------------------------------------------------------------------------------------------------------------------------------*
* CONSTANTS       (c_*)                                                                                                                   |
*-----------------------------------------------------------------------------------------------------------------------------------------*
    CONSTANTS: lc_cat           TYPE dfkklocks-lotyp VALUE '31',  " lock category as 31 -- Billing Plan Item
               lc_process       TYPE dfkklocks-proid VALUE '14',  " Lock process as 14 -- Request Billing Plans
               lc_lock_reason_e TYPE lockr_kk  VALUE 'E'.

*------------------------------------------------------------------------------------------------------------------------------------------*
* Data Declaration                                                                                                                         |
*------------------------------------------------------------------------------------------------------------------------------------------*
    DATA: ls_bip_i      TYPE fkkbix_bip_i,
          ls_lock       TYPE dfkklocks,
          lt_billplan_i TYPE fkkbix_bip_i_tab,
          ls_billplan_h TYPE fkkbix_bip_h,
          lt_locks_h    TYPE dfkklocks_t,
          lt_locks_i    TYPE dfkklocks_t.
*------------------------------------------------------------------------------------------------------------------------------------------*

**    1. Check if the bill plan item have compliance locked today.

    ls_bip_i-billplanno = iv_billplanno.
    ls_bip_i-billplanitem = iv_billplan_item.

    APPEND ls_bip_i TO lt_billplan_i.

    CALL FUNCTION 'FKK_BIX_BIP_LOCKS_GET'
      EXPORTING
*       i_bip_h         =
        it_bip_i        = lt_billplan_i
        i_lockdate      = iv_lockdate   "sy-datlo
        i_x_mass_access = ' '
      IMPORTING
        et_locks_h      = lt_locks_h
        et_locks_i      = lt_locks_i.

    ls_lock = VALUE #( lt_locks_i[ lotyp = lc_cat proid = lc_process lockr = lc_lock_reason_e ] OPTIONAL ).
    IF ls_lock IS NOT INITIAL.
      rv_locked = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD lock_billplan_item.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD LOCK_BILLPLAN_ITEM                                                                                                               |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 23-Dec-2020 |482601              |CF.INT.108     | DFDK901462     | Preform Lock Bill Plan Item                                          |                                     |
*&-----------------------------------------------------------------------------------------------------------------------------------------*

*------------------------------------------------------------------------------------------------------------------------------------------*
* Constants Declaration                                                                                                                         |
*------------------------------------------------------------------------------------------------------------------------------------------*
    CONSTANTS: lc_x           TYPE boolean         VALUE 'X',
               lc_cat         TYPE dfkklocks-lotyp VALUE '31',  " lock category as 31 -- Billing Plan Item
               lc_process     TYPE dfkklocks-proid VALUE '14',  " Lock process as 14 -- Request Billing Plans
               lc_lock_reason	TYPE lockr_kk        VALUE 'L'.

*------------------------------------------------------------------------------------------------------------------------------------------*

*------------------------------------------------------------------------------------------------------------------------------------------*
* Data Declaration                                                                                                                         |
*------------------------------------------------------------------------------------------------------------------------------------------*
    DATA: ls_lock            TYPE dfkklocks,
          ls_lock_new        TYPE dfkklocks,
          lt_billplan_i      TYPE fkkbix_bip_i_tab,
          ls_billplan_h      TYPE fkkbix_bip_h,
          lt_expansion_gpart TYPE fkloc_exp_gpart,
          lt_expansion_vkont TYPE fkloc_exp_vkont,
          ls_return          TYPE bapiret2,
          ls_bip_i           TYPE fkkbix_bip_i,
          lt_locks_h         TYPE dfkklocks_t,
          lt_locks_i         TYPE dfkklocks_t,
          lv_lockobj         TYPE loobj_kk,
          lv_tdate           TYPE tdate_kk VALUE '99991231'.
    DATA: con_dummy  TYPE dfkklocks VALUE IS INITIAL.
*------------------------------------------------------------------------------------------------------------------------------------------*

    IF iv_billplanno IS NOT INITIAL AND iv_billplan_item IS NOT INITIAL.

**    1. Check if the bill plan item is locked today..

      ls_bip_i-billplanno = iv_billplanno.
      ls_bip_i-billplanitem = iv_billplan_item.

      APPEND ls_bip_i TO lt_billplan_i.

      CALL FUNCTION 'FKK_BIX_BIP_LOCKS_GET'
        EXPORTING
*         i_bip_h         =
          it_bip_i        = lt_billplan_i
          i_lockdate      = sy-datlo
          i_x_mass_access = ' '
        IMPORTING
          et_locks_h      = lt_locks_h
          et_locks_i      = lt_locks_i.

      DATA(lv_billplanno)    = |{ iv_billplanno ALPHA = IN }|.
      DATA(lv_billplan_item) = |{ iv_billplan_item ALPHA = IN }|.

      lv_lockobj  = lv_billplanno && lv_billplan_item.
      ls_lock = VALUE #( lt_locks_i[ loobj1 = lv_lockobj lotyp = lc_cat proid = lc_process lockr = iv_lock_reason ] OPTIONAL ).

      REFRESH: lt_billplan_i.

*     Select the Bill Plan data
      CALL FUNCTION 'FKK_BIX_BILLPLAN_SELECT_SINGLE'
        EXPORTING
          i_billplanno  = iv_billplanno
        IMPORTING
          e_billplan_h  = ls_billplan_h
          et_billplan_i = lt_billplan_i.

      IF iv_next_fdate_cycle EQ abap_true.
*       calculate the date based on the Bill cycle
        DATA(lv_fdate) = zcl_ci_compl_credit_check=>calc_todate( iv_fdate      = VALUE #( lt_billplan_i[ billplanitem = iv_billplan_item ]-requestdate_next OPTIONAL )
                                                                 iv_bill_cycle = VALUE #( lt_billplan_i[ billplanitem = iv_billplan_item ]-cycle OPTIONAL ) ).
      ELSE.
        lv_fdate = iv_fdate.
      ENDIF.

      IF iv_tdate IS NOT INITIAL.
        lv_tdate = iv_tdate.
      ENDIF.

*     check the number of locks with same reason code
      DATA(lt_locks_it) = lt_locks_i.
      DELETE lt_locks_it WHERE ( loobj1 EQ lv_lockobj AND lotyp EQ lc_cat AND proid EQ lc_process ) AND lockr NE iv_lock_reason.

      IF lines( lt_locks_it ) > 1.
*     Delete all locks if more than 1 lock present for the same reason.
        LOOP AT lt_locks_i INTO DATA(ls_locks_i) WHERE loobj1 = lv_lockobj AND lotyp = lc_cat AND proid = lc_process AND lockr = iv_lock_reason.

          CALL FUNCTION 'FKK_DB_LOCK_UPDATE'
            EXPORTING
              i_lock        = con_dummy " non-opt but not needed here
              i_oldlock     = ls_locks_i   " Lock to be deleted
              i_x_lock_del  = 'X'
            EXCEPTIONS
              update_failed = 1
              OTHERS        = 2.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDLOOP.

        CLEAR ls_lock.
      ENDIF.

*     Prepare the lock data
      ls_lock_new = VALUE #( loobj1 = iv_billplanno && iv_billplan_item
                             lotyp  = lc_cat
                             proid  = lc_process
                             lockr  = SWITCH #( iv_lock_reason
                                                WHEN '' THEN lc_lock_reason
                                                ELSE iv_lock_reason )
                             fdate  = lv_fdate
                             tdate  = lv_tdate
                             gpart  = ls_billplan_h-gpart
                             vkont  = ls_billplan_h-vkont
                           ).

      APPEND ls_billplan_h-gpart TO lt_expansion_gpart.
      APPEND ls_billplan_h-vkont TO lt_expansion_vkont.
*     Update the lock on the bill plan item
      CALL FUNCTION 'FKK_DB_LOCK_UPDATE'
        EXPORTING
          i_lock             = ls_lock_new " new lock entry
          i_oldlock          = ls_lock   " old lock entry.
          it_expansion_gpart = lt_expansion_gpart
          it_expansion_vkont = lt_expansion_vkont
          i_x_lock_del       = space
        EXCEPTIONS
          update_failed      = 1
          OTHERS             = 2.
      IF sy-subrc = 0.
        ev_success = lc_x.
      ELSE.
        MESSAGE s017(zcm_msgs) WITH iv_billplanno && iv_billplan_item INTO ls_return-message.
        APPEND ls_return TO et_return.
      ENDIF.

      IF iv_commit = lc_x.
        "" commit work.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD update_billplan_billing_date.
*** Signature
*IV_BILLPLANNO  TYPE BILLPLANNO_KK
*IV_BILLPLAN_ITEM	TYPE BILLPLANITEM_KK OPTIONAL
*IS_CI_FKKBIX_BIP_I	TYPE CI_FKKBIX_BIP_I
*IV_COMMIT  TYPE ABAP_BOOLEAN OPTIONAL
*EV_SUCCESS	TYPE ABAP_BOOLEAN


**&-----------------------------------------------------------------------------------------------------------------------------------------*
**& METHOD LOCK_BILLPLAN_ITEM                                                                                                               |
**&-----------------------------------------------------------------------------------------------------------------------------------------*
**                                                          MODIFICATION HISTORY                                                            |
**&-----------------------------------------------------------------------------------------------------------------------------------------*
** Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
**&-----------------------------------------------------------------------------------------------------------------------------------------*
** 23-Dec-2020 |482601              |CF.INT.108     | DFDK901462     | Preform Lock Bill Plan Item                                          |                                     |
**&-----------------------------------------------------------------------------------------------------------------------------------------*
*
**------------------------------------------------------------------------------------------------------------------------------------------*
** Constants Declaration                                                                                                                    |
**------------------------------------------------------------------------------------------------------------------------------------------*
*
*
**------------------------------------------------------------------------------------------------------------------------------------------*
*
**------------------------------------------------------------------------------------------------------------------------------------------*
** Data Declaration                                                                                                                         |
**------------------------------------------------------------------------------------------------------------------------------------------*
*    DATA: lt_billplan_i    TYPE fkkbix_bip_i_tab,
*          ls_billplan_h    TYPE fkkbix_bip_h,
*          lt_dfkkbix_bip_i TYPE dfkkbix_bip_i_tab.
**------------------------------------------------------------------------------------------------------------------------------------------*
*
*    IF iv_billplanno IS NOT INITIAL AND is_ci_fkkbix_bip_i IS NOT INITIAL.
**   Select the Bill Plan data
*      CALL FUNCTION 'FKK_BIX_BILLPLAN_SELECT_SINGLE'
*        EXPORTING
*          i_billplanno  = iv_billplanno
*        IMPORTING
*          e_billplan_h  = ls_billplan_h
*          et_billplan_i = lt_billplan_i.
*
*      IF iv_billplan_item IS NOT INITIAL.
*        DATA(ls_billplan_i) = VALUE #( lt_billplan_i[ billplanitem = iv_billplan_item ] OPTIONAL ).
*
*      ELSE.
*        SORT lt_billplan_i BY billplanitem DESCENDING.
*        ls_billplan_i = VALUE #( lt_billplan_i[ 1 ] OPTIONAL ).
*      ENDIF.
*
*      IF is_ci_fkkbix_bip_i-zzbilling_date IS NOT INITIAL.
*        ls_billplan_i-zzbilling_date = is_ci_fkkbix_bip_i-zzbilling_date.
*      ENDIF.
*
*      IF is_ci_fkkbix_bip_i-zzsettlement_start IS NOT INITIAL.
*        ls_billplan_i-zzsettlement_start = is_ci_fkkbix_bip_i-zzsettlement_start.
*      ENDIF.
*
*      IF is_ci_fkkbix_bip_i-zzsettlement_end IS NOT INITIAL.
*        ls_billplan_i-zzsettlement_end = is_ci_fkkbix_bip_i-zzsettlement_end.
*      ENDIF.
*
*      APPEND CORRESPONDING #( ls_billplan_i ) TO lt_dfkkbix_bip_i.
*
*      CALL FUNCTION 'FKK_BIX_BILLPLAN_UPDATE'
*        EXPORTING
**         IT_DFKKBIX_BIP_H =
*          it_dfkkbix_bip_i = lt_dfkkbix_bip_i
**         IT_DFKKBIX_BIP_X =
**         IT_DFKKBIX_BIP_CH       =
**         IT_DFKKBIXBIT_BIP       =
*          i_update_mode    = 'U'.
*      IF sy-subrc EQ 0.
*        ev_success = abap_true.
*      ENDIF.
*
*      IF iv_commit EQ abap_true.
*        COMMIT WORK.
*        IF sy-subrc EQ 0.
*          ev_success = abap_true.
*        ENDIF.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
