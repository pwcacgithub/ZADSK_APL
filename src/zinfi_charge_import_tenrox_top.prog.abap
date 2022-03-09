**&---------------------------------------------------------------------*
**& Include          ZINFI_CHARGE_IMPORT_TENROX_TOP
**&---------------------------------------------------------------------*
**-----------------------------------------------------------------------------------------------------------------------------------------*
**                                                          MODIFICATION HISTORY                                                           |
**-----------------------------------------------------------------------------------------------------------------------------------------*
** Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
**-----------------------------------------------------------------------------------------------------------------------------------------*
** 01-SEP-2020 | 477237              | CF.INT.017     | DFDK900449   | Charge Import to Tenrox                                             |
**-----------------------------------------------------------------------------------------------------------------------------------------*
*
**----------------------------------------------------------------------*
** Constant Declaration
**----------------------------------------------------------------------*
*CONSTANTS: gc_x   TYPE char01 VALUE 'X',
*           gc_a   TYPE char01 VALUE 'A',
*           gc_e   TYPE char01 VALUE 'E',
*           gc_s   TYPE char01 VALUE 'S',
*           gc_w   TYPE char01 VALUE 'W',
*           gc_p   TYPE char01 VALUE 'P',
*           gc_01  TYPE char03 VALUE '-01',
*           gc_aex TYPE char03 VALUE 'AEX'.
*
**----------------------------------------------------------------------*
** Type Declaration
**----------------------------------------------------------------------*
**** Project File Type
*TYPES: BEGIN OF ty_chargeimport,
*         project_definition_nbr  TYPE ps_pspid,        "Project Defenition Number
*         co_object_nm           TYPE char50,   "CO Object Name
*         document_dt            TYPE char10,          "Document Date in MM/DD/YYYY format
*         value_cocur_amt        TYPE zval_co_cur_amt, "Value CO Currency Amount
*         cocur_type             TYPE kwaer,           "CO Currency Type
*         bill_nm                TYPE zbill_nm,        "Bill Flag
*         cost_desc              TYPE kltxt,           "Cost Element Description
*         vendor_nm              TYPE zvendor_nm,      "Vendor Name
*         po_nbr                 TYPE zpo_nbr,         "PO Number
*         cost_desc_detail_nm    TYPE co_sgtxt,        "Segment Text
*         sap_unique_id          TYPE zsap_unique_id,  "SAP Unique ID
*       END OF ty_chargeimport.
*
**----------------------------------------------------------------------*
** Internal Table Declaration
**----------------------------------------------------------------------*
*DATA: gt_pgm_const_values TYPE STANDARD TABLE OF zspgm_const_values,
*      gt_error_const      TYPE STANDARD TABLE OF zserror_const,
*      gt_data             TYPE STANDARD TABLE OF ty_chargeimport,
*      gt_expenses_series  TYPE RANGE OF char04.
*
**----------------------------------------------------------------------*
** Global Variable Declaration
**----------------------------------------------------------------------*
*DATA: gv_kokrs            TYPE kokrs,
*      gv_wrttp            TYPE wrttp,
*      gv_kstar_low        TYPE kstar,
*      gv_kstar_high       TYPE kstar,
*      gv_ktopl            TYPE ktopl,
*      gv_spras            TYPE spras,
*      gv_rfc_dest         TYPE rfc_dest,
*      gv_path_uri         TYPE string,
*      gv_api_key          TYPE string,
*      gv_api_secret       TYPE string,
*      gv_span_id          TYPE string,
*      gv_pspan_id         TYPE string,
*      gv_srv_moniker      TYPE string,
*      gv_channel          TYPE string,
*      gv_topic            TYPE string,
*      gv_event_id         TYPE string,
*      gv_last_run_date    TYPE dats,
*      gv_proj_profl_param TYPE rvari_vnam,
*      gv_error_flag       TYPE abap_boolean,
*      gv_message          TYPE string,
*      gv_interface_id     TYPE zinterfaceid.
*
**----------------------------------------------------------------------*
** Object Declaration
**----------------------------------------------------------------------*
*DATA: go_utility TYPE REF TO zcl_ca_utility_v2.
