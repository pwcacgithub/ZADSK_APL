**&---------------------------------------------------------------------*
**& Include          ZINFI_CASHAPP_TOP
**&---------------------------------------------------------------------*
**-----------------------------------------------------------------------------------------------------------------------------------------*
**                                                          MODIFICATION HISTORY                                                           |
**-----------------------------------------------------------------------------------------------------------------------------------------*
** Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
**-----------------------------------------------------------------------------------------------------------------------------------------*
** 09-APR-2021 | 486861      | CF.ENH.762     | DFDK903594   | Clear On Account AR Postings against Open Invoice                           |
** 01-OCT-2021 | 486861      | CF.ENH.762     | DFDK913579 / | Defect fix# INC3781883 / CFHYP-245                                          |
**                                              DFDK913630                                                                                 |
** 11-OCT-2021 | 486861      | CF.ENH.762D    | DFDK913815   | CF.ENH.762D - INC3765130 - Validations for Manual Entry Fields              |
** 18-OCT-2021 | 486861      | CF.ENH.762E    | DFDK913942   | CF.ENH.762E -OKB9 - FICA to FIGL Transfer (Cost Center)                     |
** 02-NOV-2021 | 488468      | TACME-1270     | DFDK914302   | R1 backlog Enhancements to Cash App                                         |
**--Remove Sales Order and Internal Order Columns from GL Items ALV                                                                        |
**-----------------------------------------------------------------------------------------------------------------------------------------*
*
**** alv event class
*CLASS lcl_alv_events DEFINITION DEFERRED.
*
****Type declaration
*TYPES :
****BP item type declaration
*  BEGIN OF ty_bpitem,
*    gpart  TYPE gpart_kk,
*    vkont  TYPE vkont_kk,
*    vtref  TYPE vtref_kk,
*    bukrs  TYPE bukrs,
*    faedn  TYPE faedn_kk,
*    zzuonr TYPE zpo_number,
*    xblnr  TYPE xblnr_kk,
*    betrw  TYPE betrw_kk,
*    shkzg  TYPE shkzg,
*    optxt  TYPE optxt_kk,
*    prctr  TYPE prctr,
*  END OF ty_bpitem,
****GL item type declaration
*  BEGIN OF ty_glitem,
*    hkont      TYPE hkont_kk,
*    bukrs      TYPE bukrs,
*    betrw      TYPE betrw_kk,
*    shkzg      TYPE shkzg,
*    kostl      TYPE kostl,
*    aufnr      TYPE aufnr,
*    prctr      TYPE prctr,
*    ps_psp_pnr TYPE ps_posid, "ps_psp_pnr,
** Begin of change - 486861 Date: 10/11/2021 Defect # INC3765130
**    kdauf      TYPE kdauf,
*    kdauf      TYPE char10,
** End of change - 486861 Date: 10/11/2021 Defect # INC3765130
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*    sgtxt      TYPE sgtxt,
*    xeiph      TYPE  xeiph_kk,
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*  END OF ty_glitem,
****item type declaration
*  BEGIN OF ty_item,
*    indicator     TYPE char2,
*    gpart         TYPE gpart_kk,
*    vkont         TYPE vkont_kk,
*    vtref         TYPE vtref_kk,
*    bukrs         TYPE bukrs,
*    budat         TYPE char10,
*    zzuonr        TYPE zpo_number,
*    xblnr         TYPE xblnr_kk,
*    betrw         TYPE betrw_kk,
*    shkzg         TYPE shkzg,
*    optxt         TYPE optxt_kk,
*    prctr         TYPE prctr,
*    hkont_gl      TYPE hkont_kk,
*    bukrs_gl      TYPE bukrs,
*    betrw_gl      TYPE betrw_kk,
*    shkzg_gl      TYPE shkzg,
*    kostl_gl      TYPE kostl,
**    aufnr_gl      TYPE aufnr, "DFDK914302
*    prctr_gl      TYPE prctr,
*    ps_psp_pnr_gl TYPE ps_posid, "ps_psp_pnr,
** Begin of change - 486861 Date: 10/11/2021 Defect # INC3765130
**    kdauf_gl      TYPE kdauf,
**    kdauf_gl      TYPE char10,  "DFDK914302
** End of change - 486861 Date: 10/11/2021 Defect # INC3765130
*  END OF ty_item,
****Types for inter compant table
*  BEGIN OF ty_intcomp,
*    bukrs       TYPE bukrs,
*    augbt       TYPE augbt_kk,
*    augbt_ratio TYPE p LENGTH 13 DECIMALS 10,
*    augbt_share TYPE augbt_kk,
*  END OF ty_intcomp,
*  BEGIN OF ty_acctdet,
*    applk TYPE applk_kk,
*    buber TYPE buber_kk,
*    ktopl TYPE ktopl,
*    key01 TYPE keynn_kk,
*    key02 TYPE keynn_kk,
*    key03 TYPE keynn_kk,
*    key04 TYPE keynn_kk,
*    key05 TYPE keynn_kk,
*    key06 TYPE keynn_kk,
*    key07 TYPE keynn_kk,
*    key08 TYPE keynn_kk,
*    fun01 TYPE funnn_kk,
*  END OF ty_acctdet,
*  BEGIN OF ty_output_log,
*    split_no TYPE zsplit_no,        " Split no
*    opbel    TYPE opbel_kk,         " Number of Contract Accts Rec. & Payable Doc.
*    type     TYPE bapi_mtype,       " Message type
*    message  TYPE bapi_msg,         " Message
*  END OF   ty_output_log,
** Begin of change - 486861 Date: 10/11/2021 Defect # INC3765130
*  BEGIN OF ty_error,
*    err_typ TYPE char1,
*    msg     TYPE string,
*    line    TYPE i,
*    alv_tab TYPE char10,
*  END OF ty_error,
** End of change - 486861 Date: 10/11/2021 Defect # INC3765130
****Table type declaration for BAPI parameters
*  tt_fkkcl          TYPE STANDARD TABLE OF fkkcl,
*  tt_fkkopk         TYPE STANDARD TABLE OF fkkopk,
*  tt_fkkop          TYPE STANDARD TABLE OF fkkop,
*  tt_mainselections TYPE STANDARD TABLE OF bapifkkopselhead,
*  tt_openitems      TYPE STANDARD TABLE OF bapidfkkcl,
*  tt_valuesel       TYPE STANDARD TABLE OF bapifkkopseltxt1,
*  tt_amountsel      TYPE STANDARD TABLE OF bapifkkopselnum,
*  tt_intcomp        TYPE STANDARD TABLE OF ty_intcomp,
*  tt_acctdet        TYPE STANDARD TABLE OF ty_acctdet.
*
****On-Act item type declaration
*TYPES BEGIN OF ty_onact.
*INCLUDE  TYPE bapidfkkcl.
*TYPES amount_op TYPE augbt_fkb4.
*TYPES amount_cl TYPE augbt_fkb4.
*TYPES checkbox TYPE char1.
*TYPES celltab TYPE lvc_t_styl. "field to switch editability
*TYPES END OF ty_onact.
*
****Open invoice item type declaration
*TYPES BEGIN OF ty_oninv.
*INCLUDE  TYPE bapidfkkcl.
*TYPES zzuonr   TYPE dfkkop-zzuonr.
*TYPES amount_op TYPE augbt_fkb4.
*TYPES amount_cl TYPE augbt_fkb4.
*TYPES checkbox TYPE char1.
*TYPES celltab TYPE lvc_t_styl. "field to switch editability
*TYPES END OF ty_oninv.
*
*DATA : gt_item        TYPE STANDARD TABLE OF ty_item     ##NEEDED,
*       gt_bpitem      TYPE STANDARD TABLE OF ty_bpitem     ##NEEDED,
*       gt_bpitem_temp TYPE STANDARD TABLE OF ty_bpitem     ##NEEDED,
*       gt_glitem      TYPE STANDARD TABLE OF ty_glitem     ##NEEDED,
*       gt_glitem_temp TYPE STANDARD TABLE OF ty_glitem     ##NEEDED,
*       gt_act         TYPE STANDARD TABLE OF ty_onact     ##NEEDED,
*       gt_inv         TYPE STANDARD TABLE OF ty_oninv     ##NEEDED.
*
**data declarations for ALV Main list
*DATA : gt_fldcat_act       TYPE lvc_t_fcat     ##NEEDED,
*       gt_fldcat_inv       TYPE lvc_t_fcat     ##NEEDED,
*       gt_fldcat_bp        TYPE lvc_t_fcat     ##NEEDED,
*       gt_fldcat_gl        TYPE lvc_t_fcat     ##NEEDED,
*       gt_exclude          TYPE ui_functions     ##NEEDED,
*       gt_pgm_const_values TYPE STANDARD TABLE OF zspgm_const_values     ##NEEDED,
*       gt_error_const      TYPE STANDARD TABLE OF zserror_const     ##NEEDED,
*       gt_output_log       TYPE STANDARD TABLE OF ty_output_log     ##NEEDED,
*       gt_fkkcl_dup        TYPE STANDARD TABLE OF fkkcl ##NEEDED,
*       gw_stable           TYPE lvc_s_stbl     ##NEEDED,
****ALV object reference
*       go_alv_act          TYPE REF TO cl_gui_alv_grid     ##NEEDED,
*       go_alv_inv          TYPE REF TO cl_gui_alv_grid     ##NEEDED,
*       go_alv_bp           TYPE REF TO cl_gui_alv_grid     ##NEEDED,
*       go_alv_gl           TYPE REF TO cl_gui_alv_grid     ##NEEDED,
****Container object reference
*       go_cont_act         TYPE REF TO cl_gui_custom_container     ##NEEDED,
*       go_cont_inv         TYPE REF TO cl_gui_custom_container     ##NEEDED,
*       go_cont_bp          TYPE REF TO cl_gui_custom_container     ##NEEDED,
*       go_cont_gl          TYPE REF TO cl_gui_custom_container     ##NEEDED,
*
*       go_alv_events       TYPE REF TO lcl_alv_events     ##NEEDED,
*       go_utility          TYPE REF TO zcl_ca_utility ##NEEDED,
** Begin of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
****Zero decimal currency
*       gr_zerodec_curr     TYPE RANGE OF waers,
** End of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
** Begin of change - 486861 Date: 10/11/2021 Defect # INC3765130
*       gt_error            TYPE STANDARD TABLE OF ty_error,
*       gr_wbs_status       TYPE RANGE OF j_status.
** End of change - 486861 Date: 10/11/2021 Defect # INC3765130
*
****Global variable declaration
*DATA : gv_buspar       TYPE gpart_kk     ##NEEDED,
*       gv_conact       TYPE vkont_kk     ##NEEDED,
*       gv_comp         TYPE bukrs     ##NEEDED,
*       gv_fidoc        TYPE opbel_kk     ##NEEDED,
*       gv_inv          TYPE xblnr_kk     ##NEEDED,
*       gv_ponum        TYPE zpo_number     ##NEEDED,
*       gv_augbt_act_cl TYPE augbt_kk     ##NEEDED,
*       gv_augbt_act_ps TYPE augbt_kk     ##NEEDED,
*       gv_augbt_inv_cl TYPE augbt_kk     ##NEEDED,
*       gv_augbt_inv_ps TYPE augbt_kk     ##NEEDED,
*       gv_augbt_bp     TYPE augbt_kk     ##NEEDED,
*       gv_augbt_gl     TYPE augbt_kk     ##NEEDED,
*       gv_difference   TYPE augbt_kk     ##NEEDED,
*       gv_waers_act_cl TYPE blwae_kk     ##NEEDED,
*       gv_waers_act_ps TYPE blwae_kk     ##NEEDED,
*       gv_waers_inv_cl TYPE blwae_kk     ##NEEDED,
*       gv_waers_inv_ps TYPE blwae_kk     ##NEEDED,
*       gv_waers_bp     TYPE blwae_kk     ##NEEDED,
*       gv_waers_gl     TYPE blwae_kk     ##NEEDED,
*       gv_diff_curr    TYPE blwae_kk     ##NEEDED,
*       gv_ok_code      TYPE sy-ucomm     ##NEEDED,
*       gv_status       TYPE icons-text     ##NEEDED,
*       gv_error        TYPE char1     ##NEEDED,
*       gv_hkont_gl     TYPE hkont_kk     ##NEEDED,
*       gv_hkont_gl_ex  TYPE hkont_kk     ##NEEDED,
*       gv_hkont_gl_rv  TYPE hkont_kk     ##NEEDED.
*
****Constants from TVARVC table
*DATA : gv_maintrs     TYPE hvorg_kk     ##NEEDED,
*       gv_type        TYPE blart_kk     ##NEEDED,
*       gv_tolgrp      TYPE togru_kkc     ##NEEDED,
*       gv_applk       TYPE applk_kk     ##NEEDED,
*       gv_hvorg       TYPE hvorg_kk     ##NEEDED,
*       gv_tvorg_0010  TYPE tvorg_kk     ##NEEDED,
*       gv_kofiz       TYPE kofiz_kk     ##NEEDED,
*       gv_blart       TYPE blart_kk     ##NEEDED,
*       gv_augrd       TYPE augrd_kk     ##NEEDED,
*       gv_herkf       TYPE herkf_kk     ##NEEDED,
*       gv_buber       TYPE buber_kk     ##NEEDED,
*       gv_ktopl       TYPE ktopl     ##NEEDED,
*       gv_buber_s000  TYPE buber_kk     ##NEEDED,
*       gv_tvorg_0020  TYPE tvorg_kk     ##NEEDED,
*       gv_bukrs_3000  TYPE bukrs     ##NEEDED,
*       gv_cl_split_no TYPE i     ##NEEDED,
*       gv_bp_split_no TYPE i     ##NEEDED,
*       gv_gl_split_no TYPE i     ##NEEDED,
** Begin of change - 486861 Date: 10/11/2021 Defect # INC3765130
*       gv_kokrs       TYPE kokrs.
** End of change - 486861 Date: 10/11/2021 Defect # INC3765130
*
****Global constants declaration
*CONSTANTS : gc_x         TYPE char1 VALUE 'X',
*            gc_s         TYPE char1 VALUE 'S',
*            gc_e         TYPE char1 VALUE 'E',
*            gc_fld_amt   TYPE lvc_fname VALUE 'AMOUNT_CL',
*            gc_fld_chk   TYPE lvc_fname VALUE 'CHECKBOX',
*            gc_fld_betrw TYPE lvc_fname VALUE 'BETRW',
*            gc_fld_shkzg TYPE lvc_fname VALUE 'SHKZG'.
*
*
** Constants for the function code of Tabs
*CONSTANTS: BEGIN OF gc_main_tab,
*             tab1 TYPE sy-ucomm VALUE 'ONACT',
*             tab2 TYPE sy-ucomm VALUE 'ONINV',
*             tab3 TYPE sy-ucomm VALUE 'ONBP',
*             tab4 TYPE sy-ucomm VALUE 'ONGL',
*           END OF gc_main_tab.
****Tabstrip control
*CONTROLS:  main_tab TYPE TABSTRIP.
*
****to check which tab is clicked
*DATA: BEGIN OF gw_main_tab,
*        subscreen   TYPE sy-dynnr,
*        prog        TYPE sy-repid VALUE sy-repid,
*        pressed_tab LIKE sy-ucomm VALUE gc_main_tab-tab1,
*        "To identify the tab pressed
*      END OF gw_main_tab.
*
*
*CLASS lcl_alv_events DEFINITION.
*  PUBLIC SECTION.
*    METHODS:
****Data changed method for On-Act ALV
*      data_changed_act         FOR EVENT data_changed_finished
*        OF cl_gui_alv_grid
*        IMPORTING et_good_cells,
****Data changed method for Open Invoice ALV
*      data_changed_inv         FOR EVENT data_changed_finished
*        OF cl_gui_alv_grid
*        IMPORTING et_good_cells,
****Data changed method for BP Item  ALV
*      data_changed_bp         FOR EVENT data_changed_finished
*        OF cl_gui_alv_grid
*        IMPORTING et_good_cells,
****Data changed method for GL Item ALV
*      data_changed_gl         FOR EVENT data_changed_finished
*        OF cl_gui_alv_grid
*        IMPORTING et_good_cells,
****Event for toolbar
*      on_toolbar
*        FOR EVENT toolbar
*        OF  cl_gui_alv_grid
*        IMPORTING
*          e_object,
****Handle user command for ALV
*      handle_user_command_act FOR EVENT user_command OF cl_gui_alv_grid
*        IMPORTING e_ucomm,
*      "Handle user command for ALV
*      handle_user_command_inv FOR EVENT user_command OF cl_gui_alv_grid
*        IMPORTING e_ucomm.
*
*ENDCLASS.
