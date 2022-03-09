*&---------------------------------------------------------------------*
*& Class: ZCL_FI_CRD_BLK_RELEASE_UTIL
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------*
*                                                    MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------*
* 19-NOV-2020 | 477237        | CF.INT.276     | DFDK900701   | Credit Block Release on DCD Action for ECC Sales Order              |
*-----------------------------------------------------------------------------------------------------------------------------------*
class ZCL_FI_CRD_BLK_RELEASE_UTIL definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_creditblockrelease,
        vbeln   TYPE vbeln,        "Order Number
        partner TYPE bu_partner,   "Partner
        lifsk   TYPE lifsk,        "Block Code
        caseid  TYPE scmg_ext_key, "DCD Case ID
      END OF ty_creditblockrelease .
  types:
    BEGIN OF ty_creditblockreleaseresponse,
        status  TYPE bapi_mtype,   "Status
        message TYPE bapi_msg,     "Message
      END OF ty_creditblockreleaseresponse .
  types:
    BEGIN OF ty_payload,
        creditblockrelease TYPE ty_creditblockrelease,
      END OF ty_payload .
  types:
    BEGIN OF ty_response,
        creditblockupdateresponse TYPE ty_creditblockreleaseresponse,
      END OF ty_response .
  types:
    BEGIN OF ty_logsystem,
        sign   TYPE char01,
        option TYPE char02,
        low    TYPE logsystem,
        high   TYPE logsystem,
      END OF ty_logsystem .
  types:
    ty_logsystem_tab TYPE STANDARD TABLE OF ty_logsystem .
  types:
    BEGIN OF ty_ext_ref,
        sign   TYPE char01,
        option TYPE char02,
        low    TYPE scmg_ext_ref,
        high   TYPE scmg_ext_ref,
      END OF ty_ext_ref .
  types:
    ty_ext_ref_tab TYPE STANDARD TABLE OF ty_ext_ref .

  constants GC_DCD_ACTION_REJECT type CHAR30 value 'REJECT' ##NO_TEXT.
  constants GC_DCD_ACTION_RELEASE type CHAR30 value 'RELEASE' ##NO_TEXT.
  constants GC_ERROR type CHAR1 value 'E' ##NO_TEXT.
  constants GC_SUCCESS type CHAR1 value 'S' ##NO_TEXT.
  data GT_ERROR_CONST type ZTTERROR_CONST .
  data GT_EXT_REF_ECC_ORD type TY_EXT_REF_TAB .
  data GT_LOGSYSTEM type TY_LOGSYSTEM_TAB .
  data GT_MAPPING type ZTTUT_NAME_MAPPING .
  data GT_PGM_CONST_VALUES type ZTTPGM_CONST_VALUES .
  data GV_API_KEY type STRING .
  data GV_API_SECRET type STRING .
  data GV_DCD_REJECT_CODE type LIFSK .
  data GV_EXT_REF_CI_DCD type SCMG_EXT_REF .
  data GV_EXT_REF_ECC_DCD type SCMG_EXT_REF .
  data GV_EXT_REF_ECC_DCD_CC type SCMG_EXT_REF .
  data GV_EXT_REF_ECC_DCD_FLEX type SCMG_EXT_REF .
  data GV_EXT_REF_MJE_DCD type SCMG_EXT_REF .
  data GV_INTERFACE_ID type ZINTERFACEID .
  data GV_MULE_RFC_DEST type RFC_DEST .
  data GV_OBJTYPE_MJE type SWO_OBJTYP .
  data GV_OBJTYPE_VBAK type SWO_OBJTYP .
  data GV_PATH_URI type STRING .
  data GV_PSPAN_ID type STRING .
  data GV_REVREASON type BILL_REVREASON_KK .
  data GV_SPAN_ID type STRING .
  data GV_SRV_MONIKER type STRING .

  methods CONSTRUCTOR .
  methods GET_PGM_CONST_VALUES
    exporting
      value(EX_EXT_REF_ECC_DCD) type SCMG_EXT_REF
      value(EX_LOGSYSTEM) type TY_LOGSYSTEM_TAB
      value(EX_EXT_REF_CI_DCD) type SCMG_EXT_REF
      value(EX_EXT_REF_MJE_DCD) type SCMG_EXT_REF
      value(EX_EXT_REF_ECC_DCD_CC) type SCMG_EXT_REF
      value(EX_EXT_REF_ECC_DCD_FLEX) type SCMG_EXT_REF .
  methods REVERSE_DOC_MJE_DCD
    importing
      !IM_BILLPLANNO type BILLPLANNO_KK
      !IM_LOCKOBJ type LOOBJ_KK
    exporting
      !EX_MESSAGE type ZTTUT_MESSAGE
      !EX_RESPONSE type TY_CREDITBLOCKRELEASERESPONSE .
  methods UNLOCK_INVOICE_MJE_DCD
    importing
      !IM_BILLPLANNO type BILLPLANNO_KK
      !IM_LOCKOBJ type LOOBJ_KK optional
    exporting
      !EX_MESSAGE type ZTTUT_MESSAGE
      !EX_RESPONSE type TY_CREDITBLOCKRELEASERESPONSE .
  methods UPDATE_CREDIT_EXPOSURE
    importing
      !IM_CREDIT_SEGMENT type UKM_CREDIT_SGMNT
      !IM_COMM_TYPE type UKM_COMM_TYP
      !IM_CURRENCY type WAERS
      !IM_AMOUNT type NETWR_AK
      !IM_VALIDITY_DATE type UKM_DATE
      !IM_PARTNER type BU_PARTNER
      !IM_OBJTYPE type SWO_OBJTYP
      !IM_LOGSYS type LOGSYS
      !IM_DOCUMENT type SWO_TYPEID
      !IM_AMOUNT_DIFF_IND type BOOLEAN default SPACE
    exporting
      !EX_RESULT type TY_CREDITBLOCKRELEASERESPONSE .
  methods UPD_BILL_PLAN_ITEM_LOCK
    importing
      !IV_BILLPLANNO type BILLPLANNO_KK
      !IV_LOCKOBJ type LOOBJ_KK
    exporting
      !EX_MESSAGE type ZTTUT_MESSAGE
      !EX_RESPONSE type TY_CREDITBLOCKRELEASERESPONSE .
  methods UPD_EXTL_ORDER_CRD_STATUS
    importing
      value(IM_VBELN) type VBELN_VA
      value(IM_PARTNER) type BU_PARTNER
      value(IM_CASE_ID) type SCMG_EXT_KEY
      value(IM_DCD_ACTION) type CHAR30
    exporting
      value(EX_MESSAGE) type ZTTUT_MESSAGE
      value(EX_RESPONSE) type TY_CREDITBLOCKRELEASERESPONSE .
  methods PUSH_COMMITMENT
    importing
      !IT_ACCOUNT_ITEM_RAW type UKM_T_ACCOUNT_ITEM
    raising
      CX_UKM_CREDIT_COMMITMENT_FAULT .
protected section.
private section.

  methods CALL_MULE_ENDPOINT
    importing
      value(IM_PAYLOAD) type TY_PAYLOAD
      value(IM_HEADER) type TIHTTPNVP
    exporting
      value(EX_MESSAGE) type ZTTUT_MESSAGE
      value(EX_RESPONSE) type TY_RESPONSE .
  methods SET_ABAP_JSON_MAPPING .
  methods SET_HEADER_PARAM
    importing
      value(IM_PAYLOAD) type TY_PAYLOAD
    exporting
      value(EX_HEADER) type TIHTTPNVP .
  methods SET_PAYLOAD
    importing
      value(IM_VBELN) type VBELN_VA
      value(IM_PARTNER) type BU_PARTNER
      value(IM_LIFSK) type LIFSK
      value(IM_CASE_ID) type SCMG_EXT_KEY
    exporting
      value(EX_PAYLOAD) type TY_PAYLOAD .
ENDCLASS.



CLASS ZCL_FI_CRD_BLK_RELEASE_UTIL IMPLEMENTATION.


  METHOD call_mule_endpoint.
*&---------------------------------------------------------------------*
*& Method: CALL_MULE_ENDPOINT
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------*
*                                                    MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------*
* 19-NOV-2020 | 477237        | CF.INT.276     | DFDK900701   | Credit Block Release on DCD Action for ECC Sales Order              |
*-----------------------------------------------------------------------------------------------------------------------------------*

**** Object Declaration
*    DATA: lo_utility TYPE REF TO zcl_ca_utility_v2.
*
**** Create Object for Utility Class
*    CREATE OBJECT lo_utility.
*
**** Set ABAP JSON Mapping table for REST Call
*    IF gt_mapping IS INITIAL.
*      set_abap_json_mapping( ).
*    ENDIF.
*
**** Call the Utility Method to call the Rest API
*    CALL METHOD lo_utility->call_rest_api
*      EXPORTING
*        i_destination = gv_mule_rfc_dest
*        i_url         = gv_path_uri
*        i_in_table    = im_payload
*        i_in_mapping  = gt_mapping
*        i_in_headers  = im_header
*        i_post        = abap_true
*      IMPORTING
*        e_message     = ex_message
*      CHANGING
*        c_resp_table  = ex_response.

  ENDMETHOD.


  METHOD constructor.
*&---------------------------------------------------------------------*
*& Method: CONSTRUCTOR
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------*
*                                                    MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------*
* 19-NOV-2020 | 477237        | CF.INT.276     | DFDK900701   | Credit Block Release on DCD Action for ECC Sales Order              |
* 11-JAN-2020 | 485639        | CF.ENH.749     | DFDK901965   | FSCM to S4 CI for MY Bill plan block release                        |
* 17-MAR-2021 | 486860        | CF.ENH.327     | DFDK903283   | MJE Billing Doc Lock release or Reversal based on DCD Action        |
*-----------------------------------------------------------------------------------------------------------------------------------*

*** Get the Program Constants
    CALL FUNCTION 'ZUTIL_PGM_CONSTANTS'
      EXPORTING
        im_pgmid               = 'ZCL_FI_CRD_BLK_RELEASE_UTIL'
      TABLES
        t_pgm_const_values     = gt_pgm_const_values
        t_error_const          = gt_error_const
      EXCEPTIONS
        ex_no_entries_found    = 1
        ex_const_entry_missing = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          MESSAGE e007(zfi_msgs) WITH 'TVARVC'(001).    "No data found in TVARVC table
        WHEN 2.
          MESSAGE e010(zfi_msgs) WITH 'TVARVC'(001).    "Atleast one constant entry missing in TVARVC table
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

*** Read the Constants values
    TRY .
        gv_ext_ref_ecc_dcd = gt_pgm_const_values[ const_name = 'P_EXT_REF_ECC_DCD' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_EXT_REF_ECC_DCD'(002).           "Constant P_EXT_REF_ECC_DCD not maintained in the ZTUTILITY_CONST table.
    ENDTRY.

    TRY .
        gv_ext_ref_ci_dcd = gt_pgm_const_values[ const_name = 'P_EXT_REF_CI_DCD' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_EXT_REF_CI_DCD'(011).           "Constant P_EXT_REF_CI_DCD not maintained in the ZTUTILITY_CONST table.
    ENDTRY.

    TRY .
        gv_ext_ref_mje_dcd = gt_pgm_const_values[ const_name = 'P_EXT_REF_MJE_DCD' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_EXT_REF_MJE_DCD'(012).           "Constant P_EXT_REF_MJE_DCD not maintained in the ZTUTILITY_CONST table.
    ENDTRY.

    TRY .
        gv_dcd_reject_code = gt_pgm_const_values[ const_name = 'P_LIFSK_DCD_REJECT_CODE' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_LIFSK_DCD_REJECT_CODE'(003).     "Constant P_LIFSK_DCD_REJECT_CODE not maintained in the ZTUTILITY_CONST table.
    ENDTRY.

    TRY .
        gv_mule_rfc_dest = gt_pgm_const_values[ const_name = 'P_RFC_DEST_MULESOFT' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_RFC_DEST_MULESOFT'(004).         "Constant P_RFC_DEST_MULESOFT not maintained in the ZTUTILITY_CONST table.
    ENDTRY.

    TRY .
        gv_path_uri = gt_pgm_const_values[ const_name = 'P_PATH_URI_CREDIT_BLK_REL' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_PATH_URI_CREDIT_BLK_REL'(005).   "Constant P_PATH_URI_CREDIT_BLK_REL not maintained in the ZTUTILITY_CONST table.
    ENDTRY.

    TRY .
        gv_span_id = gt_pgm_const_values[ const_name = 'P_SPAN_ID_CREDIT_BLK_REL' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_SPAN_ID_CREDIT_BLK_REL'(006).   "Constant P_SPAN_ID_CREDIT_BLK_REL not maintained in the ZTUTILITY_CONST table.
    ENDTRY.

    TRY .
        gv_pspan_id = gt_pgm_const_values[ const_name = 'P_PSPAN_ID_CREDIT_BLK_REL' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_PSPAN_ID_CREDIT_BLK_REL'(007).   "Constant P_PSPAN_ID_CREDIT_BLK_REL not maintained in the ZTUTILITY_CONST table.
    ENDTRY.

    TRY .
        gv_srv_moniker = gt_pgm_const_values[ const_name = 'P_SRV_MONIKER_CREDIT_BLK_REL' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_SRV_MONIKER_CREDIT_BLK_REL'(008).   "Constant P_SRV_MONIKER_CREDIT_BLK_REL not maintained in the ZTUTILITY_CONST table.
    ENDTRY.

    TRY .
        gv_revreason = gt_pgm_const_values[ const_name = 'P_BILL_REVREASON_KK_MJE' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_BILL_REVREASON_KK_MJE'(015).
    ENDTRY.

    TRY .
        gv_objtype_vbak = gt_pgm_const_values[ const_name = 'P_OBJTYPE_VBAK' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_OBJTYPE_VBAK'(016).
    ENDTRY.

    TRY .
        gv_objtype_mje = gt_pgm_const_values[ const_name = 'P_OBJTYPE_MJE' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_OBJTYPE_MJE'(017).
    ENDTRY.

    gt_logsystem = VALUE #( FOR lw_pgm_const_values IN gt_pgm_const_values
                    WHERE ( const_name = 'S_LOGSYSTEM_ECC' )                        "ECC Logical System
                          ( sign = 'I' option = 'EQ' low = lw_pgm_const_values-low ) ).

    TRY .
        gv_ext_ref_ecc_dcd_cc = gt_pgm_const_values[ const_name = 'P_EXT_REF_ECC_DCD_CREDIT_CARD' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_EXT_REF_ECC_DCD_CREDIT_CARD'(018).
    ENDTRY.

    TRY .
        gv_ext_ref_ecc_dcd_flex = gt_pgm_const_values[ const_name = 'P_EXT_REF_ECC_DCD_FLEX_ORDER' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_EXT_REF_ECC_DCD_FLEX_ORDER'(019).
    ENDTRY.

    TRY .
        gv_interface_id = gt_pgm_const_values[ const_name = 'P_ZINTERFACEID_CREDIT_BLK_REL' ]-low.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e025(zfi_msgs) WITH 'P_ZINTERFACEID_CREDIT_BLK_REL'(020).
    ENDTRY.

  ENDMETHOD.


  METHOD get_pgm_const_values.
*&---------------------------------------------------------------------*
*& Method: GET_PGM_CONST_VALUES
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------*
*                                                    MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------*
* 19-NOV-2020 | 477237        | CF.INT.276     | DFDK900701   | Credit Block Release on DCD Action for ECC Sales Order              |
* 11-JAN-2020 | 485639        | CF.ENH.749     | DFDK901965   | FSCM to S4 CI for MY Bill plan block release                        |
* 17-MAR-2021 | 486860        | CF.ENH.327     | DFDK903283   | MJE Billing Doc Lock release or Reversal based on DCD Action        |
* 20-MAY-2021 | 477237        | CF.INT.276A    | DFDK906689   | Credit Block Release on DCD created for Payment Term and Flex Order |
*-----------------------------------------------------------------------------------------------------------------------------------*

*** Export the Program Constants
    ex_ext_ref_ecc_dcd      = gv_ext_ref_ecc_dcd.
    ex_ext_ref_ci_dcd       = gv_ext_ref_ci_dcd.          "Added - CF.ENH.749
    ex_ext_ref_mje_dcd      = gv_ext_ref_mje_dcd.         "Added - CF.ENH.327
    ex_ext_ref_ecc_dcd_cc   = gv_ext_ref_ecc_dcd_cc.      "Added - CF.INT.276A
    ex_ext_ref_ecc_dcd_flex = gv_ext_ref_ecc_dcd_flex.    "Added - CF.INT.276A
    ex_logsystem            = gt_logsystem.

  ENDMETHOD.


  METHOD push_commitment.
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                               |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 07-MAY-2021 | 477237       | CF.ENH.327/     |  DFDK905865   | Update Credit Commitment                                                 |
*             |              | CF.INT.279      |               |                                                                          |
*-----------------------------------------------------------------------------------------------------------------------------------------*

*** Data Declaration
    DATA: lo_facade         TYPE REF TO cl_ukm_facade,
          lo_credit_checker TYPE REF TO cl_ukm_credit_checker.

    DATA: lt_item LIKE it_account_item_raw,
          ls_item LIKE LINE OF lt_item.

    DATA: l_dummy,
          l_segment LIKE ls_item-credit_sgmnt,
          l_partner TYPE bupa_partner,
          lt_return TYPE bapiret2_t,
          ls_return TYPE bapiret2.

    CLASS: cl_ukm_cnst_eventing DEFINITION LOAD.

    DATA: lt_reservation TYPE  ukm_t_account_item,
          lt_res_range   TYPE ukm_t_com_type,
          ls_res_range   LIKE LINE OF lt_res_range.

*** Reservation Item
    ls_res_range-option = 'EQ'.
    ls_res_range-sign   = 'I'.
    ls_res_range-low    = 'RES'.
    APPEND ls_res_range TO lt_res_range.

*** Create Object for CL_UKM_FACADE
    lo_facade = cl_ukm_facade=>create( i_activity = cl_ukm_cnst_eventing=>update_obligo ).

*** Get Object for Credit Check
    lo_credit_checker = lo_facade->get_credit_checker( ).

    REFRESH lt_item.
    LOOP AT it_account_item_raw INTO ls_item.
      IF l_partner <> ls_item-partner.
        l_partner = ls_item-partner.
        CALL FUNCTION 'BUPA_DP_AUTHCHECK'
          EXPORTING
            iv_partner          = l_partner
            iv_check_chng_actvt = abap_true
          TABLES
            et_return           = lt_return
          EXCEPTIONS
            blocpart            = 1
            OTHERS              = 2.
        IF sy-subrc NE 0.
        ENDIF.
        LOOP AT lt_return INTO ls_return.
          MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
             WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
             INTO l_dummy.
          RAISE EXCEPTION TYPE cx_ukm_credit_commitment_fault.
        ENDLOOP.
      ENDIF.

      IF ls_item-comm_typ IN lt_res_range AND NOT lt_res_range IS INITIAL.
        APPEND ls_item TO lt_reservation.
        CONTINUE.
      ENDIF.
      APPEND ls_item TO lt_item.
    ENDLOOP.

***  Identify Reservations
    IF NOT lt_reservation IS INITIAL.
      CALL FUNCTION 'UKM_COMMTS_RESERVATION_SAVE'
        EXPORTING
          it_reservation = lt_reservation.
    ENDIF.

*** Update Commitments
    lo_credit_checker->update_commitments( lt_item ).

*** Save the Changes
    CALL METHOD lo_facade->save_all
      EXPORTING
        i_upd_task = space
        i_free_all = 'X'.

  ENDMETHOD.


  METHOD reverse_doc_mje_dcd.
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                               |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-MAR-2021 | 486860       | CF.ENH.327     |  DFDK903283   | Reverse Document for MJE DCD                                              |
*-----------------------------------------------------------------------------------------------------------------------------------------*

*** Initialise exporting parameters
    CLEAR : ex_message, ex_response.

*** Internal table decleration
    DATA: lt_revdocno_tab TYPE  fkkbix_acc_billdocno_tab.

*** Variable decleration
    DATA: lv_cnt_billdoc_reversed TYPE i,
          lv_cnt_reversal_error   TYPE i,
          lv_logsys               TYPE logsys.

*** Constants decleration
    CONSTANTS: lc_appl TYPE applk_kk VALUE 'S',
               lc_s    TYPE char1 VALUE 'S',
               lc_e    TYPE char1 VALUE 'E',
               lc_x    TYPE xfeld VALUE 'X'.

*** Reverse the document
    CALL FUNCTION 'FKK_INV_REV_BILLDOC_SINGLE'
      EXPORTING
        i_applk                = lc_appl
        i_billdocno            = im_billplanno
        i_reason               = gv_revreason
        i_selection_popup      = lc_x
        i_show_results         = lc_x
      IMPORTING
        e_revdocno_tab         = lt_revdocno_tab
        e_cnt_billdoc_reversed = lv_cnt_billdoc_reversed
        e_cnt_reversal_error   = lv_cnt_reversal_error
      EXCEPTIONS
        general_fault          = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      ex_response = VALUE #( status = lc_e
                             message = 'Error in reversing the Billing Document'(101) ).
      RETURN.
    ENDIF.

    IF lv_cnt_reversal_error IS INITIAL AND lv_cnt_billdoc_reversed IS NOT INITIAL.
      READ TABLE lt_revdocno_tab ASSIGNING FIELD-SYMBOL(<lfs_tab>) INDEX 1.
      IF sy-subrc EQ 0 AND <lfs_tab>-reversaldoc IS NOT INITIAL.
        ex_response = VALUE #( status = lc_s ).

*** Get the logical system
        CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
          IMPORTING
            own_logical_system             = lv_logsys
          EXCEPTIONS
            own_logical_system_not_defined = 1
            OTHERS                         = 2.
        IF sy-subrc <> 0.
          ex_response = VALUE #( status = lc_e
                                 message = 'Dedicated logical system is not defined'(102) ).
          RETURN.
        ENDIF.

*** Reverse Credit Exposure
        SELECT SINGLE *
          FROM dfkkinvbill_h
          INTO @DATA(ls_bill_doc)
          WHERE billdocno = @im_billplanno.
        IF sy-subrc EQ 0.
          SELECT SINGLE *
            FROM v_ukm_item
            INTO @DATA(ls_ukm_item)
            WHERE partner   = @ls_bill_doc-gpart
              AND objkey    = @im_billplanno+2(10)
              AND objtype   = @gv_objtype_mje
              AND logsys    = @lv_logsys.
          IF sy-subrc EQ 0.
            CLEAR ls_ukm_item-amount.   "Set Amount Value to Zero
            me->update_credit_exposure(
              EXPORTING im_credit_segment = ls_ukm_item-credit_sgmnt
                        im_comm_type      = ls_ukm_item-comm_typ
                        im_currency       = ls_ukm_item-currency
                        im_amount         = ls_ukm_item-amount
                        im_validity_date  = sy-datum
                        im_partner        = ls_ukm_item-partner
                        im_objtype        = ls_ukm_item-objtype
                        im_logsys         = ls_ukm_item-logsys
                        im_document       = ls_ukm_item-objkey
              IMPORTING ex_result         = ex_response ).
          ELSE.
            ex_response = VALUE #( status = lc_e
                                   message = 'Credit Item not found for the Billing Document'(104) ).
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      ex_response = VALUE #( status = lc_e
                             message = 'Error in reversing the Billing Document'(101) ).
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD set_abap_json_mapping.
*&---------------------------------------------------------------------*
*& Method: SET_ABAP_JSON_MAPPING
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------*
*                                                    MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------*
* 19-NOV-2020 | 477237        | CF.INT.276     | DFDK900701   | Credit Block Release on DCD Action for ECC Sales Order              |
*-----------------------------------------------------------------------------------------------------------------------------------*

*** Add the mapping table
    gt_mapping = VALUE #( ( abap = 'CREDITBLOCKRELEASE' json = 'CreditBlockUpdateRequest' )
                          ( abap = 'VBELN' json = 'SalesOrderNumber' )
                          ( abap = 'PARTNER' json = 'DebtorParty' )
                          ( abap = 'LIFSK' json = 'CreditBlockCode' )
                          ( abap = 'CASEID' json = 'CaseID' )
                        ).

  ENDMETHOD.


  METHOD set_header_param.
*&---------------------------------------------------------------------*
*& Method: SET_HEADER_PARAM
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------*
*                                                    MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------*
* 19-NOV-2020 | 477237        | CF.INT.276     | DFDK900701   | Credit Block Release on DCD Action for ECC Sales Order              |
*-----------------------------------------------------------------------------------------------------------------------------------*

**** Data Declaration
*    DATA: lv_case_id  TYPE scmg_ext_key,
*          lv_vbeln    TYPE vbeln_va,
*          lv_trace_id TYPE string.
*
**** Object Declaration
*    DATA: lo_utility TYPE REF TO zcl_ca_utility.
*
**** Create Object for Utility Class
*    CREATE OBJECT lo_utility.
*
**** Get the API Key and API Secret Key for the Interface ID
*    CALL METHOD lo_utility->get_apikey
*      EXPORTING
*        i_interface_id  = gv_interface_id
*      IMPORTING
*        e_client_id     = DATA(lv_client_id)
*        e_client_secret = DATA(lv_client_secret).
*
*    gv_api_key = lv_client_id.
*    gv_api_secret = lv_client_secret.
*
*    lv_case_id  = im_payload-creditblockrelease-caseid.
*    lv_vbeln    = im_payload-creditblockrelease-vbeln.
*
**** Concatenate Sales Order Number and Case Id to form the Trace Id
*    lv_trace_id = lv_vbeln && |-| && lv_case_id.   "Trace Id
*
**** Add header for HTTP Request
*    ex_header = VALUE #( ( name = 'Content-Type' value = 'application/json' )
*                         ( name = 'x-B3-TraceId' value = lv_trace_id )
*                         ( name = 'x-B3-SpanId' value = gv_span_id )
*                         ( name = 'x-B3-ParentSpanId' value = gv_pspan_id )
*                         ( name = 'x-client-service-moniker' value = gv_srv_moniker )
*                         ( name = 'x-api-key' value = gv_api_key )
*                         ( name = 'x-api-secret' value = gv_api_secret )
*                       ).

  ENDMETHOD.


  METHOD set_payload.
*&---------------------------------------------------------------------*
*& Method: SET_PAYLOAD
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------*
*                                                    MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------*
* 19-NOV-2020 | 477237        | CF.INT.276     | DFDK900701   | Credit Block Release on DCD Action for ECC Sales Order              |
*-----------------------------------------------------------------------------------------------------------------------------------*

*** Data Declaration
    DATA: lv_case_id TYPE scmg_ext_key.

*** Remove leading zeroes for Case Id
    lv_case_id = im_case_id.
    SHIFT lv_case_id LEFT DELETING LEADING '0'.

*** Set Payload Structure
    ex_payload-creditblockrelease-vbeln   = im_vbeln.
    ex_payload-creditblockrelease-partner = im_partner.
    ex_payload-creditblockrelease-lifsk   = im_lifsk.
    ex_payload-creditblockrelease-caseid  = lv_case_id.

  ENDMETHOD.


  METHOD unlock_invoice_mje_dcd.
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                               |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-MAR-2021 | 486860        | CF.ENH.327     |  DFDK903283   | Unlock Invoice  for MJE DCD                                              |
*-----------------------------------------------------------------------------------------------------------------------------------------*

*** Initialise exporting parameters
    CLEAR : ex_message, ex_response.

*** Variable Declaration
    DATA: lv_gpart     TYPE gpart_kk,
          lv_vkont     TYPE vkont_kk,
          lv_xinfbill  TYPE xinfbill_kk,
          lv_billdocno TYPE dfkkinvbill_h-billdocno.

*** Structure Declaration
    DATA :ls_billdoc_h     TYPE dfkkinvbill_h,
          ls_billdoc_h_new TYPE fkkinvbill_h,
          ls_billdoc_h_old TYPE fkkinvbill_h,
          ls_invtrig       TYPE fkkinv_trig,
          ls_fkkvkp        TYPE fkkvkp,
          ls_header        TYPE fkkinvbill_h.

*** Internal Table Declaration
    DATA: lt_billdoc_h    TYPE dfkkinvbill_h_tab,
          lt_item         TYPE fkkinvbill_i_tab,
          lt_taxitem      TYPE fkkinvbill_t_tab,
          lt_additem      TYPE fkkinvbill_a_tab,
          lt_payitem      TYPE fkkinvbill_py_tab,
          lt_invtrig      TYPE fkkinv_trig_tab,
          lt_dfkkinv_trig TYPE dfkkinv_trig_tab.

*** Constants Declaration
    CONSTANTS:lc_s TYPE char1 VALUE 'S',
              lc_e TYPE char1 VALUE 'E'.

*** Get billing document header details
    SELECT *
      FROM dfkkinvbill_h
      INTO @ls_billdoc_h_old
      UP TO 1 ROWS
      WHERE billdocno = @im_billplanno ##ENH_OK.
    ENDSELECT.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_billdoc_h_old TO ls_billdoc_h_new.
*** Item table
      SELECT *
        FROM dfkkinvbill_i
        INTO TABLE @DATA(lt_itm)
        WHERE billdocno = @im_billplanno.     "#EC CI_ALL_FIELDS_NEEDED
      IF sy-subrc <> 0.
        CLEAR : lt_itm.
      ENDIF.
*** Tax Item table
      SELECT *
        FROM dfkkinvbill_t
        INTO TABLE @DATA(lt_taxitm)
        WHERE billdocno = @im_billplanno.     "#EC CI_ALL_FIELDS_NEEDED
      IF sy-subrc <> 0.
        CLEAR : lt_taxitm.
      ENDIF.

*** Add line
      SELECT *
        FROM dfkkinvbill_a
        INTO TABLE @DATA(lt_aitm)
        WHERE billdocno = @im_billplanno.     "#EC CI_ALL_FIELDS_NEEDED
      IF sy-subrc <> 0.
        CLEAR : lt_aitm.
      ENDIF.
      "(re-)create invoicing trigger
      lv_vkont = ls_billdoc_h_old-vkont.
      lv_gpart = ls_billdoc_h_old-gpart.


      IF ls_billdoc_h_old-vkont_inv IS NOT INITIAL
        AND ls_billdoc_h_old-vkont_inv <> lv_vkont.
        lv_vkont = ls_billdoc_h_new-vkont_inv.
        lv_gpart = ls_billdoc_h_new-gpart_inv.
      ELSE.
*     if inFbi triggers are created, their number is also determinated
        CLEAR: ls_billdoc_h_old-xinfbill.
      ENDIF.

*** Get Contract Account Partner-Specific
      CALL FUNCTION 'FKK_ACCOUNT_READ'
        EXPORTING
          i_vkont  = lv_vkont
          i_gpart  = lv_gpart
        IMPORTING
          e_fkkvkp = ls_fkkvkp.

      "Type compatibility
      MOVE-CORRESPONDING ls_billdoc_h_old TO ls_header.
      MOVE-CORRESPONDING lt_itm TO lt_item.
      MOVE-CORRESPONDING lt_taxitm TO lt_taxitem.
      MOVE-CORRESPONDING lt_aitm TO lt_additem.
      "Remove locking
      ls_header-invlock = space.
      ls_header-trigdeleted = abap_false.

      "Update the lock
      CALL METHOD cl_fkkinv_bill_doc=>trigger_for_billdoc_create
        EXPORTING
          x_header      = ls_header
          x_item_tab    = lt_item
          x_taxitem_tab = lt_taxitem
          x_additem_tab = lt_additem
          x_payitem_tab = lt_payitem
          x_fkkvkp      = ls_fkkvkp
        IMPORTING
          y_trigger     = ls_invtrig
          y_trigger_tab = lt_invtrig
          y_xinfbill    = lv_xinfbill
        EXCEPTIONS
          general_fault = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      APPEND ls_invtrig TO lt_invtrig.
      IF ls_billdoc_h_new-vkont_inv IS INITIAL.
        ls_billdoc_h_new-xinfbill = lv_xinfbill.
      ENDIF.

*** Convert invoicing trigger
      CALL FUNCTION 'FKK_INV_TRIGGER_CONVERT_TO_DB'
        EXPORTING
          i_fkkinv_trig_tab  = lt_invtrig
        IMPORTING
          e_dfkkinv_trig_tab = lt_dfkkinv_trig.
      IF NOT lt_dfkkinv_trig IS INITIAL.

*** Invoicing trigger
        CALL FUNCTION 'FKK_INV_TRIGGER_UPDATE'
          EXPORTING
            i_dfkkinv_trig_tab = lt_dfkkinv_trig
            i_update_mode      = cl_fkkinv_co=>co_upd_insert.
      ENDIF.
    ENDIF.

*** Update DB
    CLEAR lt_billdoc_h.
    MOVE-CORRESPONDING ls_billdoc_h_new TO ls_billdoc_h ##ENH_OK.
    "Remove locking and IO deletion indicator
    CLEAR : ls_billdoc_h-invlock, ls_billdoc_h-invstatus, ls_billdoc_h-trigdeleted.
    APPEND ls_billdoc_h TO lt_billdoc_h.

    CALL FUNCTION 'FKK_INV_BILLDOC_UPDATE'
      EXPORTING
        i_dfkkinvbill_h_tab = lt_billdoc_h
        i_update_mode       = 'U'.

    lv_billdocno = ls_billdoc_h_new-billdocno.

*** Write change-document
    CALL FUNCTION 'FKK_BILLDOC_WRITE_CHANGEDOC'
      EXPORTING
        i_billdocno      = lv_billdocno
        i_n_fkkinvbill_h = ls_billdoc_h_new
        i_o_fkkinvbill_h = ls_billdoc_h_old.

    "Check the invoice lock and IO deletion
    "Get billing document header details
    CLEAR ls_billdoc_h_old.
    SELECT invlock FROM dfkkinvbill_h INTO @DATA(lv_lock_status) UP TO 1 ROWS WHERE billdocno = @im_billplanno.
    ENDSELECT.
    IF sy-subrc EQ 0 AND lv_lock_status IS INITIAL.
      ex_response = VALUE #( status = lc_s ).
    ELSE.
      ex_response = VALUE #( status = lc_e
                             message = 'Invoice unlock has not been successful' ).
    ENDIF.

  ENDMETHOD.


  METHOD update_credit_exposure.
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                               |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-MAR-2021 | 486860       | CF.ENH.327     |  DFDK903283   | Update Credit Exposure                                                    |
*-----------------------------------------------------------------------------------------------------------------------------------------*

*** Data Declaration
    DATA: lt_account_item_raw TYPE ukm_t_account_item,
          lt_enqueue          TYPE tt_ukm_item_guid,
          ls_input            TYPE ukm_commitment_notification,
          lv_amount_diff_ind  TYPE ukm_amount_diff_indicator,
          lv_ukm_amount       TYPE ukm_amount_content,
          lv_sap_amount       TYPE bapicurr_d,
          lv_bapi_amount      TYPE bapicurr_d.

*** Check Amount Difference Indicator Flag
    IF im_amount_diff_ind IS INITIAL.
      lv_amount_diff_ind = 'false'(013).    "Update the Credit Exp. Amount as the absolute value
    ELSE.
      lv_amount_diff_ind = 'true'(014).     "Update the Credit Exp. Amount as the delta value
    ENDIF.

*** Set Amount value considering the Currency Type (For Currencies without decimal like JPY)
    lv_sap_amount = im_amount.
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = im_currency
        sap_amount  = lv_sap_amount
      IMPORTING
        bapi_amount = lv_bapi_amount.
    lv_ukm_amount = lv_bapi_amount.

**** Fill the input parameter for Convert Inbound Method
    ls_input-credit_commitment_notification-credit_commitment = VALUE #( ( credit_segment_internal_id                 = im_credit_segment
                                                                           type_code                                  = im_comm_type
                                                                           amount-currency_code                       = im_currency
                                                                           amount-value                               = lv_ukm_amount
                                                                           unsecured_amount-currency_code             = im_currency
                                                                           unsecured_amount-value                     = lv_ukm_amount
                                                                           validity_date                              = im_validity_date
                                                                           amount_balance_indicator                   = 'false'(013)
                                                                           amount_difference_indicator                = lv_amount_diff_ind
                                                                           debtor_party-internal_id-value             = im_partner
                                                                           sales_order_reference-id-scheme_id         = im_objtype
                                                                           sales_order_reference-id-scheme_agency_id  = im_logsys
                                                                           sales_order_reference-id-value             = im_document ) ).

    TRY.
*** Call the Convert Inbound method to prepare the input parameter for Credit Exposure Update
        CALL METHOD cl_ukm_xi_facade=>if_ukm_commitment_push~convert_inbound
          EXPORTING
            is_notification     = ls_input
          IMPORTING
            et_account_item_raw = lt_account_item_raw
            et_enqueue          = lt_enqueue.

*** Call the Credit Exposure Update Method
        CALL METHOD me->push_commitment
          EXPORTING
            it_account_item_raw = lt_account_item_raw.

*** Dequeue the UKM_ITEM Table
        LOOP AT lt_enqueue ASSIGNING FIELD-SYMBOL(<fs_enqueue>).
          CALL FUNCTION 'DEQUEUE_EUKM_ITEM'
            EXPORTING
              mode_ukm_item = 'E'
              client        = sy-mandt
              iguid         = <fs_enqueue>.
        ENDLOOP.

*** Set Success Flag
        ex_result = gc_success.
      CATCH cx_ukm_credit_commitment_fault INTO DATA(lx_ukm_credit_commitment_fault).
*** Set Error Flag and Message
        ex_result-status  = gc_error.
        ex_result-message = 'Error in updating the Credit Exposure Amount'(103).
*** Rollback the work
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDTRY.

  ENDMETHOD.


  METHOD upd_bill_plan_item_lock.
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                               |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 11-JAN-2020 | 485639        | CF.ENH.749     | DFDK901965   | FSCM to S4 CI for MY Bill plan block release                              |
*-----------------------------------------------------------------------------------------------------------------------------------------*

***********************************************************************
** Internal table decleration
***********************************************************************
*    DATA: lt_return  TYPE bapirettab,
*          lt_message TYPE zttut_message.
*
***********************************************************************
** Variable decleration
***********************************************************************
*    DATA: lv_success TYPE boolean.
*
***********************************************************************
** Object decleration
***********************************************************************
*    DATA: lo_compl_credit_check    TYPE REF TO zcl_ci_compl_credit_check.
*
***********************************************************************
** Constants Decleration
***********************************************************************
*    CONSTANTS: lc_e TYPE char1 VALUE 'E',
*               lc_a TYPE char1 VALUE 'A',
*               lc_x TYPE char1 VALUE 'X',
*               lc_s TYPE char1 VALUE 'S'.
*
**** Create Object
*    CREATE OBJECT lo_compl_credit_check.
*
**** Release Bill Plan Locks
*    CALL METHOD lo_compl_credit_check->do_release_billplan_locks
*      EXPORTING
*        iv_billplanno = iv_billplanno
**       iv_commit     =
**       iv_lock_reason = 'L'
*        iv_lockobj    = iv_lockobj
*      IMPORTING
*        ev_suceess    = lv_success
*        et_return     = lt_return.
*    IF lv_success IS INITIAL.
*      LOOP AT lt_return INTO DATA(lw_return) WHERE type = lc_e OR type = lc_a OR type = lc_x.
*        ex_response = VALUE #( status = lw_return-type
*                                   message = lw_return-message ) .
*        EXIT.
*      ENDLOOP.
*    ELSE.
*      ex_response = VALUE #( status = lc_s ).
*    ENDIF.

  ENDMETHOD.


  METHOD upd_extl_order_crd_status..
*&---------------------------------------------------------------------*
*& Method: UPD_EXTL_ORDER_CRD_STATUS.
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------*
*                                                    MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------*
* 19-NOV-2020 | 477237        | CF.INT.276     | DFDK900701   | Credit Block Release/Reject on DCD Action for External Sales Order  |
*-----------------------------------------------------------------------------------------------------------------------------------*

*** Data Declaration
    DATA: lv_lifsk TYPE lifsk.

*** Structure Declaration
    DATA: ls_payload  TYPE ty_payload,
          ls_response TYPE ty_response.

*** Internal Table Declaration
    DATA: lt_header  TYPE tihttpnvp,
          lt_message TYPE zttut_message.

*** Set the Credit Block Code
    CASE im_dcd_action.
      WHEN gc_dcd_action_release.
        lv_lifsk = space.
      WHEN gc_dcd_action_reject.
        lv_lifsk = gv_dcd_reject_code.
    ENDCASE.

*** Set Payload
    set_payload( EXPORTING  im_vbeln    = im_vbeln
                            im_partner  = im_partner
                            im_lifsk    = lv_lifsk
                            im_case_id  = im_case_id
                 IMPORTING  ex_payload  = ls_payload ).

*** Set Header Parameters for REST Call
    set_header_param( EXPORTING im_payload  = ls_payload
                      IMPORTING ex_header   = lt_header ).

*** Call Mule Endpoint to Release the ECC Order
    call_mule_endpoint( EXPORTING im_payload  = ls_payload
                                  im_header   = lt_header
                        IMPORTING ex_message  = lt_message
                                  ex_response = ls_response ).

*** Export the Message and Response
    ex_message  = lt_message.
    ex_response = ls_response-creditblockupdateresponse.

  ENDMETHOD.
ENDCLASS.
