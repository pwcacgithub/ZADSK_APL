**&---------------------------------------------------------------------*
**& Include          ZINFI_EXCEL_BIT_CREAT_LOAD_TOP
**&---------------------------------------------------------------------*
*
**&---------------------------------------------------------------------*
******Global Utility Object
**&---------------------------------------------------------------------*
*
*DATA: go_utility TYPE REF TO zcl_ca_utility ##NEEDED.
*
**&---------------------------------------------------------------------*
******Global Types
**&---------------------------------------------------------------------*
*
*DATA: BEGIN OF gs_current ##NEEDED,
*        BEGIN OF s_log,
*          object     TYPE balhdr-object,
*          subobject  TYPE balhdr-subobject,
*          log_handle TYPE balloghndl,
*          open       TYPE xfeld,
*        END OF s_log,
*      END OF gs_current.
*
**&---------------------------------------------------------------------*
******Internal Table Declaration
**&---------------------------------------------------------------------*
*
*DATA : gt_data_it          TYPE TABLE OF zsfi_bit_excel_load ##NEEDED,
*       gt_api_data_it      TYPE fkkbixbit_api_it_all_tab ##NEEDED,
*       gt_pgm_const_values TYPE STANDARD TABLE OF zspgm_const_values ##NEEDED,
*       gt_error_const      TYPE STANDARD TABLE OF zserror_const ##NEEDED,
*       gt_data_py          TYPE fkkbixbit_api_py_all_tab ##NEEDED,
*       gt_data_tx          TYPE fkkbixbit_api_tx_all_tab ##NEEDED,
*       gt_data_tt          TYPE fkkbixbit_api_tt_all_tab ##NEEDED,
*       gt_output           TYPE fkkbix_upload_bit_result_tab ##NEEDED,
*       gt_error_log        TYPE TABLE OF bal_s_msg ##NEEDED.
*
**&---------------------------------------------------------------------*
*********** Work Area Declaration
**&---------------------------------------------------------------------*
*
*DATA : gw_data         TYPE zsfi_bit_excel_load ##NEEDED,
*       gw_error_log    LIKE LINE OF gt_error_log ##NEEDED,
*       gv_presen(1)    TYPE c ##NEEDED,
*       gv_appl(1)      TYPE c ##NEEDED,
*       gv_selection(1) TYPE c ##NEEDED,
*       gv_mje          TYPE bittype_kk ##NEEDED,
*       gv_no_commit    TYPE boolean ##NEEDED,
*       gv_read_success TYPE boolean ##NEEDED,
*       gv_file_errors  TYPE boolean ##NEEDED,
*       gv_filext(4)    TYPE c ##NEEDED,
*       gv_srctype      TYPE srctatype_kk ##NEEDED,
*       gv_taxdate_type TYPE tax_date_type_kk ##NEEDED,
*       gv_taxdet_type  TYPE tax_det_type_kk ##NEEDED,
*       gv_spart        TYPE spart_kk ##NEEDED,
*       gv_mwskz_us     TYPE mwskz ##NEEDED,
*       gv_mwskz_us_E1  TYPE mwskz ##NEEDED,
*       gv_mwskz_non_us TYPE mwskz ##NEEDED,
*       gv_sc03         TYPE sub_process_kk ##NEEDED,
*       gv_sc04         TYPE sub_process_kk ##NEEDED,
*       gv_sc05         TYPE sub_process_kk ##NEEDED,
*       gv_sc06         TYPE sub_process_kk ##NEEDED,
*       gv_sc07         TYPE sub_process_kk ##NEEDED,
*       gv_payer        TYPE parvw ##NEEDED,
*       gv_billto       TYPE parvw ##NEEDED,
*       gv_bill         TYPE flag.
*
**----------------------------------------------------------------------*
** Constant Declaration
**----------------------------------------------------------------------*
*CONSTANTS: gc_x         TYPE char01 VALUE 'X',
*           gc_messageid TYPE symsgid VALUE 'ZFI_MSGS',
*           gc_002       TYPE symsgno VALUE '002',
*           gc_error     TYPE symsgty VALUE 'E',
*           gc_success   TYPE symsgty VALUE 'S',
*           gc_warning   TYPE symsgty VALUE 'W',
*           gc_1         TYPE char1 VALUE '1',
*           gc_2         TYPE char1 VALUE '2',
*           gc_3         TYPE char1 VALUE '3',
*           gc_4         TYPE char1 VALUE '4',
*           gc_5         TYPE char1 VALUE '5',
*           gc_6         TYPE char1 VALUE '6',
*           gc_7         TYPE char1 VALUE '7',
*           gc_r         TYPE c VALUE 'R',
*           gc_s         TYPE c VALUE 'S',
*           gc_l         TYPE c VALUE 'L',
*           gc_zadr      TYPE bittype_kk VALUE 'ZADR',
*           gc_zaot      TYPE bittype_kk VALUE 'ZAOT',
*           gc_zpip      TYPE bittype_kk VALUE 'ZPIP',
*           gc_zopx      TYPE bittype_kk VALUE 'ZOPX',
*           gc_zrcl      TYPE bittype_kk VALUE 'ZRCL',
*           gc_zntr      TYPE bittype_kk VALUE 'ZNTR',
*           gc_zooc      TYPE bittype_kk VALUE 'ZNTR'.
*
**----------------------------------------------------------------------*
**  FIELD-SYMBOLS
**----------------------------------------------------------------------*
*FIELD-SYMBOLS <gt_data>       TYPE STANDARD TABLE ##NEEDED.
