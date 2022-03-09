**&---------------------------------------------------------------------*
**& Include          ZINFI_EXCEL_BIT_CREAT_LOAD_SUB
**&---------------------------------------------------------------------*
**-----------------------------------------------------------------------------------------------------------------------------------------*
**                                                          MODIFICATION HISTORY                                                           |
**-----------------------------------------------------------------------------------------------------------------------------------------*
** Change Date | Developer     | RICEFW/Defect# | Transport#   | Description                                                               |
**-----------------------------------------------------------------------------------------------------------------------------------------*
** 15-Sep-2021 | 486861        | CF.ENH.320     | DFDK913081   | Defect# : Apollo14/SIT-14854 - FICA MJE - profit center is being          |
**                                                               derived incorrectly for commissions an                                    |
** 16-Sep-2021 | 486861        | CF.ENH.320     | DFDK913081   | Defect# : Apollo14/SIT-13318 MJE Billing to Retain Baseline -Japan payment|
**                                                               terms baseline date is not computing correctly                            |
** 04-Oct-2021 | 486861        | CF.ENH.320     | DFDK913616 \ | Defect # CFHYP-228 : MJE Billing to Retain Baseline -Japan payment        |
**                                                DFDK913671                                                                               |
** 02-Nov-2021 | 486861        | CF.ENH.320     | DFDK914239   | Defect # CFHYP-333 : Validataions for cost center,PC,GL,Material and WBS  |
**-----------------------------------------------------------------------------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**& Form f4_file_process
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_f4_file_process .
*
**&---------------------------------------------------------------------*
******Local Structure Declaration
**&---------------------------------------------------------------------*
*  DATA: lv_rc TYPE i.
*  DATA: lt_file_table TYPE filetable,
*        ls_file_table TYPE file_table,
*        lv_filename   TYPE string.
*
**&---------------------------------------------------------------------*
*****Constants
**&---------------------------------------------------------------------*
**  CONSTANTS: lc_filename TYPE string VALUE 'Select a file'.
*  lv_filename = TEXT-090.
*  IF gv_presen IS NOT INITIAL.
*
******* For Presentation server files
*    CALL METHOD cl_gui_frontend_services=>file_open_dialog
*      EXPORTING
*        window_title            = lv_filename
*      CHANGING
*        file_table              = lt_file_table
*        rc                      = lv_rc
*      EXCEPTIONS
*        file_open_dialog_failed = 1
*        cntl_error              = 2
*        error_no_gui            = 3
*        not_supported_by_gui    = 4
*        OTHERS                  = 5.
*
*    IF  sy-subrc  = 0.
*      READ TABLE lt_file_table INTO ls_file_table INDEX 1.
*      p_file = ls_file_table-filename.
*    ENDIF.
*
*
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form selection_screns
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_selection_screens.
*
*  IF r6 IS NOT INITIAL.
*    gv_selection = gc_r.
*  ELSEIF r7 IS NOT INITIAL.
*    gv_selection = gc_s.
*  ELSEIF r8 IS NOT INITIAL.
*    gv_selection = gc_l.
*  ENDIF.
*
*  CASE p_mje.
*    WHEN gc_1.
*      gv_mje = gc_zadr.
*    WHEN gc_2.
*      gv_mje = gc_zaot.
*    WHEN gc_3.
*      gv_mje = gc_zpip.
*    WHEN gc_4.
*      gv_mje = gc_zopx.
*    WHEN gc_5.
*      gv_mje = gc_zrcl.
*    WHEN gc_6.
*      gv_mje = gc_zntr.
*    WHEN gc_7.
*      gv_mje = gc_zooc.
*      gv_bill = abap_true.
*  ENDCASE.
*
*  CASE p_int.
*    WHEN gc_1.
*      gv_presen = abap_true.
*      gv_appl = abap_false.
*  ENDCASE.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form initialize
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_initialize .
**----------------------------------------------------------------------*
** Data Delcaration
**----------------------------------------------------------------------*
*  DATA: lt_ivrm_values TYPE vrm_values.
*  DATA: ls_xvrm_values LIKE LINE OF lt_ivrm_values.
*  DATA lv_name TYPE vrm_id.
*
**----------------------------------------------------------------------*
** Constants
**----------------------------------------------------------------------*
*
*  CONSTANTS : lc_int TYPE vrm_id VALUE 'P_INT',
*              lc_mje TYPE vrm_id VALUE 'P_MJE'.
**              lc_presen_server  TYPE char80 VALUE 'File .on Presentation Server',
**              lc_appl_server    TYPE char80 VALUE 'File on Application Server',
**              lc_accrual_def    TYPE char80 VALUE 'Accrual Posted to Def Revenue',
**              lc_accural_others TYPE char80 VALUE 'Accrual Posted to Others',
**              lc_pip            TYPE char80 VALUE 'PIPs',
**              lc_commission     TYPE char80 VALUE 'Commission',
**              lc_reclass        TYPE char80 VALUE 'Reclass',
**              lc_non_trade      TYPE char80 VALUE 'Non-trade'.
*
******For Source type selection
*  lv_name = lc_int.
*  ls_xvrm_values-key = gc_1.
*  ls_xvrm_values-text  = TEXT-083."lc_presen_server.
*  APPEND ls_xvrm_values TO lt_ivrm_values.
*
*****Set the Drop down values for source selection
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = lv_name
*      values = lt_ivrm_values.
*
*********For MJE scenario selection
*
*  CLEAR lt_ivrm_values.
*
*  lv_name = lc_mje.
*  ls_xvrm_values-key = gc_1.
*  ls_xvrm_values-text  =  TEXT-084."lc_accrual_def.
*  APPEND ls_xvrm_values TO lt_ivrm_values .
*
*  ls_xvrm_values-key = gc_2.
*  ls_xvrm_values-text  =  TEXT-085."lc_accural_others.
*  APPEND ls_xvrm_values TO lt_ivrm_values .
*
*  ls_xvrm_values-key = gc_3.
*  ls_xvrm_values-text  = TEXT-086." lc_pip.
*  APPEND ls_xvrm_values TO lt_ivrm_values.
*
*  ls_xvrm_values-key = gc_4.
*  ls_xvrm_values-text  = TEXT-087." lc_commission.
*  APPEND ls_xvrm_values TO lt_ivrm_values .
*
*
*  ls_xvrm_values-key = gc_5.
*  ls_xvrm_values-text  =  TEXT-088."lc_reclass.
*  APPEND ls_xvrm_values TO lt_ivrm_values .
*
*
*  ls_xvrm_values-key = gc_6.
*  ls_xvrm_values-text  = TEXT-089."  lc_non_trade.
*  APPEND ls_xvrm_values TO lt_ivrm_values .
*
*  ls_xvrm_values-key = gc_7.
*  ls_xvrm_values-text  = TEXT-101."  Open Order conversion.
*  APPEND ls_xvrm_values TO lt_ivrm_values .
*
*****Set the Drop down values for source selection
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = lv_name
*      values = lt_ivrm_values.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form bit_create
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_bit_create .
**&---------------------------------------------------------------------*
******Data Declaration Declaration
**&---------------------------------------------------------------------*
*  DATA : lt_add_params    TYPE fkkbix_add_param_tab,
*         ls_add_params    TYPE fkkbix_add_param,
*         ls_params        TYPE fkkbix_upload_params,
*         lt_msg           TYPE fkkbix_msg_tab,
*         lt_irt_billdocno TYPE fkk_rt_billdocno,
*         ls_error         TYPE xfeld,
*         lt_bit4_it       TYPE  fkkbixbit4_it_all_tab,
*         lt_msg_bix       TYPE fkkbix_msg_disp_tab.
*
**&---------------------------------------------------------------------*
******Constants
**&---------------------------------------------------------------------*
*  CONSTANTS : lc_zjme       TYPE bitcat_kk VALUE 'ZMJE',
*              lc_filename   TYPE param_name_kk VALUE 'FILE_NAME',
*              lc_i          TYPE   char1 VALUE 'I',
*              lc_eq         TYPE char2 VALUE 'EQ',
*              lc_start_col  TYPE i VALUE 15,
*              lc_start_line TYPE i VALUE 6,
*              lc_end_col    TYPE i VALUE 100,
*              lc_end_line   TYPE i VALUE 18,
*              lc_col_width  TYPE i VALUE 62.
*
*
*  DATA(lv_zjme) = lc_zjme.
*
****Pass Dailog process as per the part of standard process for hard error log
*  ls_params-dialog = gc_x.
*  ls_params-dialog_result = gc_x.
*  ls_params-dialog_file_select = gc_x.
*
*  IF gv_no_commit IS NOT INITIAL.
*    ls_params-simurun = gc_x.
*  ELSE.
*    ls_params-simurun = space.
*  ENDIF.
*
******Write logs to SLG1 and Display the Logs as well using FM - FKK_BIX_SHOW_RESULTS
*  PERFORM f_log_open.
*
*  ls_add_params-name = lc_filename.
*  ls_add_params-value = p_file.
*
*  APPEND ls_add_params TO lt_add_params.
*
*  CLEAR ls_error.
*****FM to create BIT - Billing Items
*  CALL FUNCTION 'FKK_BIX_BIT_UPLOAD'
*    EXPORTING
*      it_api_it        = gt_api_data_it
*      it_api_py        = gt_data_py
*      it_api_tx        = gt_data_tx
*      it_api_tt        = gt_data_tt
*      it_add_params    = lt_add_params
*      iv_bitcat        = lv_zjme
*      is_params        = ls_params
*      iv_external_log  = gc_x
*    IMPORTING
*      et_messages      = lt_msg
*      ev_error         = ls_error
*      et_upload_result = gt_output
*    EXCEPTIONS
*      error_message    = 1.
*
*  IF sy-subrc <> 0.
****Do Nothing
*  ENDIF.
*
*****Save the logs to SLG1 with Object FKKBIX as a standard process
*  PERFORM f_log_close USING gc_x.
*
*****If process has errrors then throw the hard errors
*  IF ls_error IS NOT INITIAL AND lt_msg IS NOT INITIAL.
*
*    LOOP AT lt_msg INTO DATA(ls_msg).
*      MOVE-CORRESPONDING ls_msg TO gw_error_log.
*      APPEND gw_error_log TO gt_error_log.
*      CLEAR gw_error_log.
*    ENDLOOP.
*
*    IF gt_error_log IS NOT INITIAL.
*
*      PERFORM f_log_create.
*      PERFORM f_log_show.
*
*    ENDIF.
*
*  ELSE.
*
*    IF gv_selection = gc_s OR gv_selection = gc_l.
*
*      " collect BIT data of all bitcat for Results Popup
*      DATA ls_data_bit TYPE fkkbix_upload_bit_result-data.
*      LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<ls_output_bit>).
*        APPEND LINES OF <ls_output_bit>-data-bit0_srcta       TO ls_data_bit-bit0_srcta.
*        APPEND LINES OF <ls_output_bit>-data-bit2_srcta       TO ls_data_bit-bit2_srcta.
*        APPEND LINES OF <ls_output_bit>-data-bit0_it          TO ls_data_bit-bit0_it.
*        APPEND LINES OF <ls_output_bit>-data-bit1_it          TO ls_data_bit-bit1_it.
*        APPEND LINES OF <ls_output_bit>-data-bit2_it          TO ls_data_bit-bit2_it.
*        APPEND LINES OF <ls_output_bit>-data-bit3_it          TO ls_data_bit-bit3_it.
*        APPEND LINES OF <ls_output_bit>-data-billdocno        TO ls_data_bit-billdocno.
*        APPEND LINES OF <ls_output_bit>-data-bit4key_billed   TO ls_data_bit-bit4key_billed.
*        APPEND LINES OF <ls_output_bit>-data-invdocno         TO ls_data_bit-invdocno.
*        APPEND LINES OF <ls_output_bit>-data-billdoc_invoiced TO ls_data_bit-billdoc_invoiced.
*      ENDLOOP.
*
*      IF ls_data_bit-bit4key_billed IS NOT INITIAL.
*        LOOP AT ls_data_bit-bit4key_billed INTO DATA(ls_bit4key_billed).
*          APPEND VALUE #( sign = lc_i option = lc_eq low = ls_bit4key_billed-billdocno ) TO lt_irt_billdocno.
*        ENDLOOP.
*
****Check Billing DOcument Number
*        CALL FUNCTION 'FKK_BIX_BIT_SELECT'
*          EXPORTING
*            irt_billdocno         = lt_irt_billdocno
*            i_bit4_uninvoiced_req = gc_x
*            i_bit4_invoiced_req   = gc_x
*          IMPORTING
*            et_bit4_it            = lt_bit4_it
*          EXCEPTIONS
*            not_found             = 1
*            OTHERS                = 2.
*        IF sy-subrc = 0.
*          DATA lt_bit_it TYPE fkkbixbit_it_all_tab ##NEEDED.
*          lt_bit_it = CORRESPONDING #( lt_bit4_it MAPPING bit_dbtab = bit4_dbtab ).
*        ENDIF.
*      ENDIF.
*
*      " prepare messages for results popup.
*      PERFORM f_result_load_bit_msg_get USING gv_no_commit
*                                   CHANGING lt_msg_bix.
*****Show the BIT results as a standard process
*      CALL FUNCTION 'FKK_BIX_SHOW_RESULTS'
*        EXPORTING
*          i_gui_title            = TEXT-061
*          iv_screen_start_at_col = lc_start_col
*          iv_screen_start_at_lin = lc_start_line
*          iv_screen_end_at_col   = lc_end_col
*          iv_screen_end_at_lin   = lc_end_line
*          iv_col_width_text      = lc_col_width
*          it_msg_disp            = lt_msg_bix
*          i_fcode_log            = gc_x
*          i_process              = space
*          i_no_zero              = gc_x
*          i_simurun              = gv_no_commit
*          i_log_handle           = gs_current-s_log-log_handle
*          it_bit0_it             = ls_data_bit-bit0_it
*          it_bit2_it             = ls_data_bit-bit2_it
*          it_bit4_it             = lt_bit4_it
*          it_bit0_srcta          = ls_data_bit-bit0_srcta
*          it_bit2_srcta          = ls_data_bit-bit2_srcta
*          it_billdocno           = ls_data_bit-billdocno
*          it_invdocno            = ls_data_bit-invdocno.
*
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form F4_FILE
**&---------------------------------------------------------------------*
*FORM f_bit_read .
*
**&---------------------------------------------------------------------*
******Types
**&---------------------------------------------------------------------*
*  TYPES : BEGIN OF ty_column_table,
*            col         TYPE string,
*            column_name TYPE string,
*            date_check  TYPE string,
*          END OF ty_column_table.
*
**&---------------------------------------------------------------------*
******Internal Table Declaration
**&---------------------------------------------------------------------*
*  DATA: lt_table_fields TYPE TABLE OF dfies,
*        lt_dref         TYPE REF TO data,
*        lt_column_table TYPE STANDARD TABLE OF ty_column_table.
*
**&---------------------------------------------------------------------*
******Local Vairable Declaration
**&---------------------------------------------------------------------*
*  DATA : lv_index             TYPE i,
*         lv_saknr             TYPE saknr,
*         lv_prctr             TYPE prctr,
*         lv_kostl             TYPE kostl,
*         lv_matnr             TYPE matnr,
*         lv_dats              TYPE string,
*         lv_target_date_field TYPE dats,
*         lv_col               TYPE i,
*         lv_error_col         TYPE i,
*         lv_soldto            TYPE kunnr.
*
**&---------------------------------------------------------------------*
******Local Structure Declaration
**&---------------------------------------------------------------------*
*  DATA: ls_dref         TYPE REF TO data,
*        ls_column_table TYPE ty_column_table.
*
**&---------------------------------------------------------------------*
******Field Symbols
**&---------------------------------------------------------------------*
*  FIELD-SYMBOLS : <fs_table>       TYPE any,
*                  <ft_table>       TYPE STANDARD TABLE,
*                  <dyn_field>,
*                  <dyn_field_name>,
*                  <column_value>.
*
**&---------------------------------------------------------------------*
*****Constants
**&---------------------------------------------------------------------*
*  CONSTANTS: lc_dynamic_table TYPE dd02l-tabname VALUE 'FKKBIXBIT_API_IT_ALL',
*             lc_matnr         TYPE string VALUE 'MATNR',
*             lc_glaccount     TYPE string VALUE 'ZZ_HKONT',
*             lc_prctr         TYPE string VALUE 'PRCTR',
*             lc_kostl         TYPE string VALUE 'KOSTL',
*             lc_dats          TYPE dynptype VALUE 'DATS',
*             lc_soldto        TYPE string VALUE 'ZZ_SOLDTO_BP',
*             lc_146           TYPE symsgno VALUE '146',
*             lc_148           TYPE symsgno VALUE '148',
*             lc_149           TYPE symsgno VALUE '149',
*             lc_150           TYPE symsgno VALUE '150',
*             lc_151           TYPE symsgno VALUE '151',
*             lc_152           TYPE symsgno VALUE '152',
*             lc_154           TYPE symsgno VALUE '154',
*             lc_169           TYPE symsgno VALUE '169',
*             lc_174           TYPE symsgno VALUE '174',
** Begin of change - 486861 Date: 09/16/2021 Defect # Apollo14 / SIT-13318
*             lc_197           TYPE symsgno VALUE '197'.
** End of change - 486861 Date: 09/16/2021 Defect # Apollo14 / SIT-13318
*
*  IF gv_presen IS NOT INITIAL.
*
*    IF p_file IS NOT INITIAL.
****Reasd the File using GUI_UPLOAD , Reason is WEB GUI wont able to read the OLE content of Excel
*      PERFORM read_file.
*
*    ENDIF.
*  ENDIF.
*
**create OBJECT lr_descr.
**** --- Assigning fields symbols for Tables
*
*  CREATE DATA lt_dref TYPE TABLE OF (lc_dynamic_table).
*  CREATE DATA ls_dref TYPE (lc_dynamic_table).
**** --- Assign field symbol with table type of DDIC
*  ASSIGN lt_dref->* TO <ft_table>.
**** --- Assign field symbol with Structure type of DDIC
*  ASSIGN ls_dref->* TO <fs_table>.
*
****Read the Field Info of BIX BIT DATA_IT_ALL Fields Info
*  CALL FUNCTION 'DDIF_FIELDINFO_GET'
*    EXPORTING
*      tabname        = lc_dynamic_table
*    TABLES
*      dfies_tab      = lt_table_fields
*    EXCEPTIONS
*      not_found      = 1
*      internal_error = 2
*      OTHERS         = 3.
*
*  IF sy-subrc <> 0.
****Do Nothing
*  ENDIF.
*
*  SORT lt_table_fields ASCENDING BY fieldname.
*
******Fetch the Column Header details from Row 2 of Excel and push to Internal Table
*  LOOP AT <gt_data> ASSIGNING FIELD-SYMBOL(<ls_data>) FROM 2 TO 2.
*    "processing columns
*    DO .
*      ASSIGN COMPONENT sy-index OF STRUCTURE <ls_data> TO <dyn_field_name>.
*      IF sy-subrc = 0.
*        lv_col = lv_col + 1.
*
*        ls_column_table-column_name = <dyn_field_name>.
*        APPEND ls_column_table TO lt_column_table.
*        CLEAR: ls_column_table.
*
*      ELSE.
*        EXIT.
*      ENDIF.
*    ENDDO.
*  ENDLOOP.
*
****Process on Actual records from Row 3 to Update BIT class tables for Upload
*  LOOP AT <gt_data> ASSIGNING <ls_data> FROM 3.
*
**** --- Adding count to map for Mandatory field
*    lv_error_col = lv_error_col + 1.
*
*    DO lv_col TIMES.
*      lv_index = sy-index.
*      ASSIGN COMPONENT lv_index OF STRUCTURE <ls_data> TO <dyn_field>.
*      IF <dyn_field> IS ASSIGNED.
*        READ TABLE lt_column_table INTO DATA(ls_columnname) INDEX lv_index.
*        IF sy-subrc = 0.
*          READ TABLE lt_table_fields INTO DATA(ls_table_fields) WITH KEY fieldname = ls_columnname-column_name
*                                                                                                  BINARY SEARCH ##WARN_OK.
*          IF sy-subrc = 0.
*            ASSIGN COMPONENT ls_table_fields-position OF STRUCTURE <fs_table> TO <column_value>.
*            IF sy-subrc = 0.
*              IF ls_table_fields-datatype = lc_dats.
****Convert all Date fields to Internal Format
*                lv_dats = <dyn_field>.
*
*****Formatting the Date using Regex
*                PERFORM date_convert USING lv_dats CHANGING lv_target_date_field .
*                <dyn_field> = ''.
*                <dyn_field> = lv_target_date_field.
*              ENDIF.
*
*****Cost Center conversion
*              IF ls_columnname-column_name = lc_kostl.
*                lv_kostl = <dyn_field>.
*                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    input  = lv_kostl
*                  IMPORTING
*                    output = lv_kostl.
*                <dyn_field> = lv_kostl.
*              ENDIF.
*
*****G/L Account conversion
*              IF ls_columnname-column_name = lc_glaccount.
*                lv_saknr = <dyn_field>.
*                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    input  = lv_saknr
*                  IMPORTING
*                    output = lv_saknr.
*                <dyn_field> = lv_saknr.
*              ENDIF.
*
*****Profit Center conversion
*              IF ls_columnname-column_name = lc_prctr.
*                lv_prctr = <dyn_field>.
*                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    input  = lv_prctr
*                  IMPORTING
*                    output = lv_prctr.
*                <dyn_field> = lv_prctr.
*              ENDIF.
*
****Material conversion
*              IF ls_columnname-column_name = lc_matnr.
*                lv_matnr = <dyn_field>.
*                CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*                  EXPORTING
*                    input        = lv_matnr
*                  IMPORTING
*                    output       = lv_matnr
*                  EXCEPTIONS
*                    length_error = 1
*                    OTHERS       = 2.
*                IF sy-subrc <> 0.
*                  " ***Do Nothing
*                ENDIF.
*                <dyn_field> = lv_matnr.
*              ENDIF.
*
****Business partner conversion
*              IF ls_columnname-column_name = lc_soldto.
*                lv_soldto = <dyn_field>.
*                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    input  = lv_soldto
*                  IMPORTING
*                    output = lv_soldto.
*                IF sy-subrc <> 0.
*                  " ***Do Nothing
*                ENDIF.
*                <dyn_field> = lv_soldto.
*              ENDIF.
*              IF <dyn_field> IS ASSIGNED.
*                <column_value> = <dyn_field>.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDDO.
*
*    MOVE-CORRESPONDING <fs_table> TO gw_data.
****Move Sold-to to the ship-to field if ship-to is blank : 320D
*    IF gw_data-zz_soldto_bp IS NOT INITIAL AND gw_data-zz_shipto_bp IS INITIAL.
*      gw_data-zz_shipto_bp = gw_data-zz_soldto_bp.
*    ENDIF.
*****Throw errors for Mandatory fields****
*
****Business Partner - Tech Field Name - GPART
*    IF gw_data-zz_soldto_bp IS INITIAL. "320D changes
*
*      gw_error_log-msgid = gc_messageid.
*      gw_error_log-msgno = lc_146.
*      gw_error_log-msgty = gc_error.
*      gw_error_log-msgv1 = lv_error_col.
*      APPEND gw_error_log TO gt_error_log.
*      CLEAR  gw_error_log.
*
*    ENDIF.
*
****Bit Amount Tech Field Name -  BIT_AMOUNT
*    IF gw_data-bit_amount IS INITIAL.
*
*      gw_error_log-msgid = gc_messageid.
*      gw_error_log-msgno = lc_148.
*      gw_error_log-msgty = gc_error.
*      gw_error_log-msgv1 = lv_error_col.
*      APPEND gw_error_log TO gt_error_log.
*      CLEAR  gw_error_log.
*
*    ENDIF.
*
****Bit Currency - Tech Field Name - BIT_CURR
*    IF gw_data-bit_curr IS INITIAL.
*
*      gw_error_log-msgid = gc_messageid.
*      gw_error_log-msgno = lc_149.
*      gw_error_log-msgty = gc_error.
*      gw_error_log-msgv1 = lv_error_col.
*      APPEND gw_error_log TO gt_error_log.
*      CLEAR  gw_error_log.
*
*    ENDIF.
*
*****G/L account - Tech Field Name - ZZ_HKONT
*    IF gw_data-zz_hkont IS INITIAL.
*
*      gw_error_log-msgid = gc_messageid.
*      gw_error_log-msgno = lc_152.
*      gw_error_log-msgty = gc_error.
*      gw_error_log-msgv1 = lv_error_col.
*      APPEND gw_error_log TO gt_error_log.
*      CLEAR  gw_error_log.
*    ENDIF.
*
*****JE Purpose - Tech Field Name - ZZ_XREF3
*    IF gw_data-zz_xref3 IS INITIAL.
*
*      gw_error_log-msgid = gc_messageid.
*      gw_error_log-msgno = lc_150.
*      gw_error_log-msgty = gc_error.
*      gw_error_log-msgv1 = lv_error_col.
*      APPEND gw_error_log TO gt_error_log.
*      CLEAR  gw_error_log.
*    ENDIF.
*
*****Company Code - Tech Field Name - BUKRS
*    IF gw_data-bukrs IS INITIAL.
*
*      gw_error_log-msgid = gc_messageid.
*      gw_error_log-msgno = lc_169.
*      gw_error_log-msgty = gc_error.
*      gw_error_log-msgv1 = lv_error_col.
*      APPEND gw_error_log TO gt_error_log.
*      CLEAR  gw_error_log.
*    ENDIF.
*
*****Reference - Tech Field Name - ZZ_UNIQ_REF ** Mandatory for Accural and Re-class Transactions
*    IF gw_data-zz_uniq_ref IS INITIAL AND ( gv_mje = gc_zaot OR gv_mje = gc_zrcl OR gv_mje = gc_zadr ).
*
*      gw_error_log-msgid = gc_messageid.
*      gw_error_log-msgno = lc_154.
*      gw_error_log-msgty = gc_error.
*      gw_error_log-msgv1 = lv_error_col.
*      APPEND gw_error_log TO gt_error_log.
*      CLEAR  gw_error_log.
*
*    ENDIF.
****Invoice Title - Tech Field Name - ZZ_INV_TITLE ** Mandatory for PIPs, commissions & non-trade
*    IF gw_data-zz_inv_title IS INITIAL AND ( gv_mje = gc_zpip OR gv_mje = gc_zopx OR gv_mje = gc_zntr ).
*      gw_error_log-msgid = gc_messageid.
*      gw_error_log-msgno = lc_151.
*      gw_error_log-msgty = gc_error.
*      gw_error_log-msgv1 = lv_error_col.
*      APPEND gw_error_log TO gt_error_log.
*      CLEAR  gw_error_log.
*    ENDIF.
** Begin of change - 486861 Date: 09/16/2021 Defect # Apollo14 / SIT-13318
****PO number - Tech Field Name - ZZPO_NUMBER ** Mandatory for all scenarios
*    IF gw_data-zzpo_number IS INITIAL.
*      gw_error_log-msgid = gc_messageid.
*      gw_error_log-msgno = lc_197.
*      gw_error_log-msgty = gc_error.
*      gw_error_log-msgv1 = lv_error_col.
*      APPEND gw_error_log TO gt_error_log.
*      CLEAR  gw_error_log.
*    ENDIF.
** End of change - 486861 Date: 09/16/2021 Defect # Apollo14 / SIT-13318
*
*    APPEND gw_data TO gt_data_it.
*
*    CLEAR : gw_data,
*            lv_matnr,
*            lv_saknr,
*            lv_dats,
*            ls_column_table,
*            ls_table_fields,
*            ls_columnname,
*            lv_index,
*            lv_soldto.
*
*  ENDLOOP.
**** Changes for 320D ***
****Get payer and bill to details
*  PERFORM f_fill_payer_billto_details.
*
*****Process Based on Error or Success Record.
*  gv_read_success = abap_true.
*
*  IF  gv_selection = gc_r.
*    gw_error_log-msgid = gc_messageid.
*    gw_error_log-msgno = gc_002.
*    gw_error_log-msgty = gc_success.
*    gw_error_log-msgv1 = TEXT-019.
*
*    APPEND gw_error_log TO gt_error_log.
*    CLEAR gw_error_log.
*
*    PERFORM f_log_create.
*    PERFORM f_log_show.
*  ENDIF.
*
*ENDFORM. " F4_FILE
*
**---------------------------------------------------------------------*
** Form DATE_CONVERT
**---------------------------------------------------------------------*
*FORM date_convert USING iv_date_string TYPE string CHANGING cv_date TYPE datum .
*
**&---------------------------------------------------------------------*
******Local value Declaration
**&---------------------------------------------------------------------*
*  DATA: lv_convert_date(10) TYPE c.
*
**&---------------------------------------------------------------------*
*****Constants
**&---------------------------------------------------------------------*
*  CONSTANTS: lc_dymd TYPE tumls_date_format VALUE 'DYMD',
*             lc_ddmy TYPE tumls_date_format VALUE 'DDMY'.
*
*  CLEAR cv_date.
*
*  lv_convert_date = iv_date_string .
*
*  "date format MM/DD/YYYY
*  FIND REGEX '^\d{4}[/|-]\d{1,2}[/|-]\d{1,2}$' IN lv_convert_date.
*  IF sy-subrc = 0.
*    CALL FUNCTION '/SAPDMC/LSM_DATE_CONVERT'
*      EXPORTING
*        date_in             = lv_convert_date
*        date_format_in      = lc_dymd
*        to_output_format    = ' '
*        to_internal_format  = gc_x
*      IMPORTING
*        date_out            = lv_convert_date
*      EXCEPTIONS
*        illegal_date        = 1
*        illegal_date_format = 2
*        no_user_date_format = 3
*        OTHERS              = 4.
*
*
*    IF sy-subrc = 0.
*      cv_date = lv_convert_date .
*    ENDIF.
*
*  ELSE.
*
*    " date format DD/MM/YYYY
*    FIND REGEX '^\d{1,2}[/|-]\d{1,2}[/|-]\d{4}$' IN lv_convert_date.
*    IF sy-subrc = 0.
*      CALL FUNCTION '/SAPDMC/LSM_DATE_CONVERT'
*        EXPORTING
*          date_in             = lv_convert_date
*          date_format_in      = lc_ddmy
*          to_output_format    = ' '
*          to_internal_format  = gc_x
*        IMPORTING
*          date_out            = lv_convert_date
*        EXCEPTIONS
*          illegal_date        = 1
*          illegal_date_format = 2
*          no_user_date_format = 3
*          OTHERS              = 4.
*
*
*      IF sy-subrc = 0.
*        cv_date = lv_convert_date .
*      ENDIF.
*
*    ENDIF.
*  ENDIF.
*
*ENDFORM .
**---------------------------------------------------------------------*
** Form READ_FILE
**---------------------------------------------------------------------*
*FORM read_file .
*
**&---------------------------------------------------------------------*
******Local Structure Declaration
**&---------------------------------------------------------------------*
*  DATA : lv_filename      TYPE string,
*         lt_records       TYPE solix_tab,
*         lv_headerxstring TYPE xstring,
*         lv_filelength    TYPE i.
*
**&---------------------------------------------------------------------*
*****Constants
**&---------------------------------------------------------------------*
*  CONSTANTS: lc_filetype TYPE char10 VALUE 'BIN'.
*
*  lv_filename = p_file.
*
*  CALL FUNCTION 'GUI_UPLOAD'
*    EXPORTING
*      filename                = lv_filename
*      filetype                = lc_filetype
*    IMPORTING
*      filelength              = lv_filelength
*      header                  = lv_headerxstring
*    TABLES
*      data_tab                = lt_records
*    EXCEPTIONS
*      file_open_error         = 1
*      file_read_error         = 2
*      no_batch                = 3
*      gui_refuse_filetransfer = 4
*      invalid_type            = 5
*      no_authority            = 6
*      unknown_error           = 7
*      bad_data_format         = 8
*      header_not_allowed      = 9
*      separator_not_allowed   = 10
*      header_too_long         = 11
*      unknown_dp_error        = 12
*      access_denied           = 13
*      dp_out_of_memory        = 14
*      disk_full               = 15
*      dp_timeout              = 16
*      OTHERS                  = 17.
*
*  CASE sy-subrc.
*    WHEN 0.
*****Do nothing
*    WHEN 1.
*      MESSAGE e016 WITH  'Unable to upload excel file, please try again'(018).
*    WHEN 2.
*      MESSAGE e016 WITH 'Unable to Read the File'(017).
*    WHEN OTHERS.
*      MESSAGE e016 WITH 'Unable to Read the File'(017).
*  ENDCASE.
*
*  "convert binary data to xstring
*  "if you are using cl_fdt_xl_spreadsheet in odata then skips this step
*  "as excel file will already be in xstring
*  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
*    EXPORTING
*      input_length = lv_filelength
*    IMPORTING
*      buffer       = lv_headerxstring
*    TABLES
*      binary_tab   = lt_records
*    EXCEPTIONS
*      failed       = 1
*      OTHERS       = 2.
*
*  IF sy-subrc <> 0.
*    "Implement suitable error handling here
*  ENDIF.
*
*  DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .
*
*  TRY .
*      lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
*                              document_name = lv_filename
*                              xdocument     = lv_headerxstring ) .
*    CATCH cx_fdt_excel_core ##NO_HANDLER.
*      "Implement suitable error handling here
*  ENDTRY .
*
*  "Get List of Worksheets
*  lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
*    IMPORTING
*      worksheet_names = DATA(lt_worksheets) ).
*
*  IF NOT lt_worksheets IS INITIAL.
*    READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX 1.
*
*    IF sy-subrc = 0.
*
*      DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
*                                               lv_woksheetname ).
*      "now you have excel work sheet data in dyanmic internal table
*      ASSIGN lo_data_ref->* TO <gt_data>.
*
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_alv_display_output
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_alv_display_output .
*
****-Call the method to display the results in ALV Report format
*
*  IF gv_read_success IS NOT INITIAL.
*    IF gv_selection = gc_r.
*      TRY .
*          go_utility->display_alv( CHANGING c_datatab = gt_data_it ).
*        CATCH cx_root INTO DATA(lr_text).
*          DATA(lv_msg) = lr_text->get_text( ) .
*          MESSAGE e002 WITH lv_msg .
*      ENDTRY .
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form log_create
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_log_create .
**----------------------------------------------------------------------*
** Local Data Declaration
**----------------------------------------------------------------------*
*  DATA: ls_log_handle TYPE balloghndl,
*        ls_log        TYPE bal_s_log,
*        ls_msg        TYPE bal_s_msg,
*        lv_line       TYPE balcntcum.
*
**----------------------------------------------------------------------*
** Constant Declaration
**----------------------------------------------------------------------*
*  CONSTANTS: lc_probclass TYPE  balprobcl VALUE '1'.
*
** create an initial log file
*  CALL FUNCTION 'BAL_LOG_CREATE'
*    EXPORTING
*      i_s_log      = ls_log
*    IMPORTING
*      e_log_handle = ls_log_handle
*    EXCEPTIONS
*      OTHERS       = 1.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  lv_line = lines( gt_error_log ).
*
*  LOOP AT gt_error_log INTO DATA(ls_error_log).
*
*    MOVE-CORRESPONDING ls_error_log TO ls_msg.
*
**   derive prob class & count
*    ls_msg-probclass = lc_probclass.
*    ls_msg-msg_count = lv_line.
*
**   add message to log file
*    CALL FUNCTION 'BAL_LOG_MSG_ADD'
*      EXPORTING
*        i_log_handle = ls_log_handle
*        i_s_msg      = ls_msg
*      EXCEPTIONS
*        OTHERS       = 1.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    CLEAR ls_error_log.
*
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form log_show
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_log_show .
*
**&---------------------------------------------------------------------*
*********** Work Area Declaration
**&---------------------------------------------------------------------*
*  DATA:l_s_display_profile TYPE bal_s_prof.
*
**----------------------------------------------------------------------*
** Constant Declaration
**----------------------------------------------------------------------*
*  CONSTANTS: lc_log TYPE slis_handl VALUE 'LOG'.
*
** get a prepared profile
*  CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
*    IMPORTING
*      e_s_display_profile = l_s_display_profile
*    EXCEPTIONS
*      OTHERS              = 1.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
** use grid for display if wanted
*  l_s_display_profile-use_grid = gc_x.
*
** set report to allow saving of variants
*  l_s_display_profile-disvariant-report = sy-repid.
*  l_s_display_profile-disvariant-handle = lc_log.
*
** call display function module
*  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
*    EXPORTING
*      i_s_display_profile = l_s_display_profile
*    EXCEPTIONS
*      OTHERS              = 1.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form file_errors
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_file_errors_log.
*
**&---------------------------------------------------------------------*
*****Constants
**&---------------------------------------------------------------------*
*  CONSTANTS: lc_xlsx(4) TYPE c VALUE 'XLSX'.
*
*  IF gv_selection IS INITIAL.
*
*    gw_error_log-msgid = gc_messageid.
*    gw_error_log-msgno = gc_002.
*    gw_error_log-msgty = gc_error.
*    gw_error_log-msgv1 = TEXT-013.
*
*    APPEND gw_error_log TO gt_error_log.
*
*  ENDIF.
*
*  IF p_int IS INITIAL AND gt_error_log IS INITIAL.
*
*    gw_error_log-msgid = gc_messageid.
*    gw_error_log-msgno = gc_002.
*    gw_error_log-msgty = gc_error.
*    gw_error_log-msgv1 = TEXT-014.
*
*    APPEND gw_error_log TO gt_error_log.
*
*  ENDIF.
*
*  IF p_mje IS INITIAL AND gt_error_log IS INITIAL.
*
*    gw_error_log-msgid = gc_messageid.
*    gw_error_log-msgno = gc_002.
*    gw_error_log-msgty = gc_error.
*    gw_error_log-msgv1 =  TEXT-015.
*
*    APPEND gw_error_log TO gt_error_log.
*  ENDIF.
*
*  IF p_file IS INITIAL AND gt_error_log IS INITIAL.
*
*    gw_error_log-msgid = gc_messageid.
*    gw_error_log-msgno = gc_002.
*    gw_error_log-msgty = gc_error.
*    gw_error_log-msgv1 = TEXT-016.
*
*    APPEND gw_error_log TO gt_error_log.
*  ENDIF.
*
*  IF p_file IS NOT INITIAL.
*
*    CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
*      EXPORTING
*        filename  = p_file
*      IMPORTING
*        extension = gv_filext.
*
*    TRANSLATE gv_filext TO UPPER CASE.
*
*    IF gv_filext NE lc_xlsx AND gt_error_log IS INITIAL.
*
*      gw_error_log-msgid = gc_messageid.
*      gw_error_log-msgno = gc_002.
*      gw_error_log-msgty = gc_error.
*      gw_error_log-msgv1 = TEXT-001.
*
*      APPEND gw_error_log TO gt_error_log.
*    ENDIF.
*  ENDIF.
*
*
*  IF gt_error_log IS NOT INITIAL.
*
*    gv_file_errors = abap_true.
*
*    PERFORM f_log_create.
*    PERFORM f_log_show.
*
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form fetch_cust_fields_data
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_process_data .
*
**&---------------------------------------------------------------------*
******Types
**&---------------------------------------------------------------------*
*  TYPES : BEGIN OF ty_prps,
*            pspnr TYPE ps_posnr,
*            posid TYPE ps_posid,
*            bukrs TYPE bukrs,
*          END OF ty_prps.
*
*
**&---------------------------------------------------------------------*
******Data Declaration
**&---------------------------------------------------------------------*
*  DATA: lt_data_accu_revsal TYPE TABLE OF zsfi_bit_excel_load,
*        lt_conv_prps        TYPE TABLE OF ty_prps,
*        ls_conv_prps        TYPE ty_prps,
*        lv_number(12)       TYPE n,
*        ls_api_data_it      TYPE fkkbixbit_api_it_all,
*        ls_nriv             TYPE nriv,
*        lv_nrlevel          TYPE nrlevel.
*
**----------------------------------------------------------------------*
** Constants
**----------------------------------------------------------------------*
*  CONSTANTS : lc_x                TYPE c VALUE 'X',
*              lc_s                TYPE applk_kk VALUE 'S',
*              lc_mje              TYPE bitcat_kk VALUE 'ZMJE',
*              lc_companycode      TYPE bukrs VALUE '3000',
*              lc_companycode_1000 TYPE bukrs VALUE '1000',
*              lv_nr_obj           TYPE nrobj VALUE 'FKKSRCTAID',
*              lc_0                TYPE n VALUE '0',
*              lc_6                TYPE i VALUE 6,
*              lc_kokrs            TYPE kokrs VALUE '2000',
*              lv_nr               TYPE nrnr VALUE '02',
*              lc_jpy              TYPE bit_curr_kk VALUE 'JPY',
*              lc_krw              TYPE bit_curr_kk VALUE 'KRW'.
*
****Fetch Contract Account, company code, Account determin. ID
*
*  IF gt_data_it IS NOT INITIAL.
*
******Fetch Contract Account Number based on Business Partner
*    SELECT vkont,
*      gpart,
*      opbuk,
*      kofiz_sc,
*      zahlkond_sc
*      FROM fkkvkp
*      INTO TABLE @DATA(lt_fkkvkp)
*      FOR ALL ENTRIES IN @gt_data_it
*      WHERE gpart = @gt_data_it-gpart AND
*            opbuk = @gt_data_it-bukrs
*      ORDER BY PRIMARY KEY.
*
*
*****We are getting PS_PSP_PNR in PS_POSID , as standard struc - FKKBIXBIT_API_IT_ALL dont have PS_PSP_PNR
*    LOOP AT gt_data_it INTO DATA(ls_data).
*
*      ls_conv_prps-posid =    ls_data-ps_posid.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
*        EXPORTING
*          input         = ls_data-ps_posid
*        IMPORTING
*          output        = ls_conv_prps-pspnr
** Begin of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*        EXCEPTIONS
*          error_message = 4.
*      IF sy-subrc EQ 0.
** End of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
****Pass Bukrs also for WBS elements
*        ls_conv_prps-bukrs = ls_data-bukrs.
*        APPEND ls_conv_prps TO lt_conv_prps.
*        CLEAR : ls_conv_prps, ls_data.
** Begin of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*      ENDIF.
** End of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*    ENDLOOP.
*
****Validate the WBS based on on company code, WBS Element
*
*    SELECT pspnr,
*           posid,
*           pbukr,
*           prctr,
*           kostl FROM prps
*      INTO TABLE @DATA(lt_prps)
*      FOR ALL ENTRIES IN @lt_conv_prps
*      WHERE pspnr = @lt_conv_prps-pspnr
*        AND pbukr = @lt_conv_prps-bukrs
*      ORDER BY PRIMARY KEY.
*
****Fetch Profit Center based on KOSTL
** Begin of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
****check cost center entered
*    DATA(lt_item) = gt_data_it[].
*    DELETE lt_item WHERE kostl IS INITIAL.
*    IF lt_item IS NOT INITIAL.
** End of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*      SELECT kokrs,                                     "#EC CI_GENBUFF
*             kostl,
*             datbi,
** Begin of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*             datab,
*             bukrs,
** End of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*             prctr FROM csks
*        INTO TABLE @DATA(lt_csks)
*        FOR ALL ENTRIES IN @gt_data_it
*        WHERE kokrs = @lc_kokrs
*          AND kostl = @gt_data_it-kostl
*          AND bukrs = @gt_data_it-bukrs
*        ORDER BY PRIMARY KEY.
** Begin of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*      IF sy-subrc EQ 0.
*        SORT lt_csks DESCENDING BY kostl datbi bukrs .
*      ENDIF.
*    ENDIF.
** End of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
****Fetch Profit Center based on matnr
*    SELECT matnr,
*           werks,
*           prctr FROM marc
*      INTO TABLE @DATA(lt_marc)
*      FOR ALL ENTRIES IN @gt_data_it
*      WHERE matnr = @gt_data_it-matnr
*        AND werks = @gt_data_it-bukrs
*      ORDER BY PRIMARY KEY.
*
*  ENDIF.
****Sort the tables for Binay search for fields
*  DATA(lt_fkkvkp_sort) = lt_fkkvkp.
*
*  SORT lt_fkkvkp_sort ASCENDING BY gpart opbuk.
*  DELETE ADJACENT DUPLICATES FROM lt_fkkvkp_sort COMPARING gpart opbuk.
*
*  SORT lt_prps ASCENDING BY posid pbukr.
*  DELETE ADJACENT DUPLICATES FROM lt_prps COMPARING posid pbukr.
*
*  LOOP AT gt_data_it ASSIGNING FIELD-SYMBOL(<fs_data>).
*
*    READ TABLE lt_fkkvkp_sort INTO DATA(ls_fkkvkp) WITH KEY gpart = <fs_data>-gpart opbuk = <fs_data>-bukrs BINARY SEARCH.
****Pass Contract Account, company code, Account determin. ID
*    IF sy-subrc = 0.
*      <fs_data>-vkont = ls_fkkvkp-vkont.
*      <fs_data>-kofiz = ls_fkkvkp-kofiz_sc.
*      <fs_data>-zz_term = ls_fkkvkp-zahlkond_sc.
*    ENDIF.
*
*    IF <fs_data>-zz_xref1_hd IS INITIAL.
*      <fs_data>-zz_xref1_hd = sy-uname.
*    ENDIF.
*
****copy from BITDATE from excel upload
**    IF <fs_data>-BITDATE IS NOT INITIAL.
**      <fs_data>-bill_first = <fs_data>-bitdate.
**    ELSE.
*    <fs_data>-bill_first = sy-datum.
**    ENDIF.
*    IF <fs_data>-zz_shipto_bp IS INITIAL.
*      <fs_data>-zz_shipto_bp = <fs_data>-gpart.
*    ENDIF.
*
*****Pass cost center for all transactions except commissions , for commissions its Mandatory from Excel
*
*    READ TABLE lt_conv_prps INTO DATA(ls_prps_conv) WITH KEY posid = <fs_data>-ps_posid.
*
*    IF <fs_data>-ps_posid IS NOT INITIAL AND ls_prps_conv-pspnr IS NOT INITIAL.
** Begin of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*      "changing code to work properly as we have to sort table with different fields
**      READ TABLE lt_prps INTO DATA(ls_prps) WITH KEY pspnr = ls_prps_conv-pspnr pbukr = <fs_data>-bukrs BINARY SEARCH.
*      READ TABLE lt_prps INTO DATA(ls_prps) WITH KEY pspnr = ls_prps_conv-pspnr pbukr = <fs_data>-bukrs.
** End of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*      IF ls_prps-kostl IS NOT INITIAL.
*        <fs_data>-kostl = ls_prps-kostl.
*      ENDIF.
*    ENDIF.
*
*****Profit Center Derivation logic
*
*    IF <fs_data>-ps_posid IS NOT INITIAL.
*
*      IF ls_prps-prctr IS NOT INITIAL.
*        <fs_data>-prctr =  ls_prps-prctr.
*      ENDIF.
*
*    ELSEIF <fs_data>-ps_posid IS INITIAL AND <fs_data>-kostl IS NOT INITIAL.
*
*      READ TABLE lt_csks INTO DATA(ls_csks) WITH KEY kostl = <fs_data>-kostl BINARY SEARCH.
*      IF ls_csks-prctr  IS NOT INITIAL.
*        <fs_data>-prctr = ls_csks-prctr.
*        CLEAR : ls_csks.
*      ENDIF.
*
*    ELSEIF <fs_data>-ps_posid IS INITIAL AND <fs_data>-kostl IS INITIAL AND <fs_data>-prctr IS NOT INITIAL.
****Do Nothing as profit center  from excel
*    ELSEIF <fs_data>-ps_posid IS INITIAL AND <fs_data>-kostl IS INITIAL AND <fs_data>-matnr IS NOT INITIAL AND <fs_data>-prctr IS INITIAL.
*
*      READ TABLE lt_marc INTO DATA(ls_marc) WITH KEY matnr = <fs_data>-matnr BINARY SEARCH.
*      IF ls_marc-prctr IS NOT INITIAL.
*        <fs_data>-prctr = ls_marc-prctr.
*        CLEAR : ls_marc.
*      ENDIF.
*    ENDIF.
*
****Pass Bit class
*    <fs_data>-bitcat = lc_mje.
*
****pass subprocess
*    IF gv_mje = gc_zaot.
*      <fs_data>-subprocess = gv_sc04.
*    ELSEIF  gv_mje = gc_zpip.
*      <fs_data>-subprocess = gv_sc03.
*    ELSEIF  gv_mje = gc_zopx.
*      <fs_data>-subprocess = gv_sc06.
*    ELSEIF  gv_mje = gc_zrcl.
*      <fs_data>-subprocess = gv_sc05.
*    ELSEIF  gv_mje = gc_zntr.
*      <fs_data>-subprocess = gv_sc07.
*    ELSEIF  gv_mje = gc_zooc.
*      <fs_data>-subprocess = gv_sc07.
*    ELSEIF  gv_mje = gc_zadr.
*      <fs_data>-subprocess = gv_sc04.
*    ENDIF.
*
****Pass Source Trans.ID
*    IF gv_selection = gc_s AND lv_nrlevel IS INITIAL.
*
*****For simulation Mode , dont increase the Number range , fetch th current range add + 1
*      CALL FUNCTION 'NUMBER_GET_INFO'
*        EXPORTING
*          nr_range_nr        = lv_nr
*          object             = lv_nr_obj
*        IMPORTING
*          interval           = ls_nriv
*        EXCEPTIONS
*          interval_not_found = 1
*          object_not_found   = 2
*          OTHERS             = 3.
*      IF sy-subrc <> 0.
****Do Nothing
*      ENDIF.
*      lv_nrlevel = ls_nriv-nrlevel.
*
****Conversion Exit for Source Id
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = lv_nrlevel
*        IMPORTING
*          output = <fs_data>-srctaid.
*
*    ELSEIF gv_selection = gc_l.
*
****For Real Load , increment the number range
*      CALL FUNCTION 'NUMBER_GET_NEXT'
*        EXPORTING
*          nr_range_nr             = lv_nr
*          object                  = lv_nr_obj
*        IMPORTING
*          number                  = lv_number
*        EXCEPTIONS
*          interval_not_found      = 1
*          number_range_not_intern = 2
*          object_not_found        = 3
*          quantity_is_0           = 4
*          quantity_is_not_1       = 5
*          interval_overflow       = 6
*          buffer_overflow         = 7
*          OTHERS                  = 8.
*      IF sy-subrc <> 0.
****Do Nothing
*      ENDIF.
****Conversion Exit for Source Id
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = lv_number
*        IMPORTING
*          output = <fs_data>-srctaid.
*    ELSE.
*
*      lv_nrlevel = lv_nrlevel + 1.
*
****Conversion Exit for Source Id
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = lv_nrlevel
*        IMPORTING
*          output = <fs_data>-srctaid.
*
*    ENDIF.
*    <fs_data>-srctatype = gv_srctype.
*
*****pass bit type
*    <fs_data>-bittype = gv_mje.
*
****Pass Application Area
*    <fs_data>-applk = lc_s.
*
****pass Bill Starting From, Date of Origin, Time of Origin
*    <fs_data>-bill_first = sy-datum.
*    <fs_data>-bittime = sy-uzeit.
*
*    <fs_data>-postrel = lc_x.
*    <fs_data>-printrel = lc_x.
*    <fs_data>-spart = gv_spart.
*
*****Tax Determination Process******
*    <fs_data>-tax_included = space.
*
*    IF gv_mje = gc_zpip OR gv_mje = gc_zopx OR gv_mje = gc_zntr OR gv_mje = gc_zooc.
*      <fs_data>-tax_date_type = gv_taxdate_type.
*    ELSEIF  gv_mje = gc_zaot OR gv_mje = gc_zrcl OR gv_mje = gc_zadr.
*      <fs_data>-tax_date_type = space.
*    ENDIF.
*
*    IF gv_mje = gc_zaot OR gv_mje = gc_zrcl OR gv_mje = gc_zadr.
*      <fs_data>-tax_det_type = space.
*    ELSEIF  gv_mje = gc_zpip OR gv_mje = gc_zopx OR gv_mje = gc_zntr OR gv_mje = gc_zooc.
*      <fs_data>-tax_det_type = gv_taxdet_type.
*    ENDIF.
*
*    IF gv_mje = gc_zpip OR gv_mje = gc_zopx OR gv_mje = gc_zntr OR gv_mje = gc_zooc.
*
****If Company code - 3000 then pass the tax code
*
*      IF <fs_data>-bukrs = lc_companycode.
*        <fs_data>-mwskz = gv_mwskz_us.
*      ELSEIF <fs_data>-bukrs = lc_companycode_1000.
*        <fs_data>-mwskz = gv_mwskz_us_e1.
*      ELSE.
*        <fs_data>-mwskz = gv_mwskz_non_us.
*      ENDIF.
*
*    ELSEIF  gv_mje = gc_zaot OR gv_mje = gc_zrcl OR gv_mje = gc_zadr.
*      <fs_data>-mwskz = space.
*    ENDIF.
*
*    IF gv_bill = abap_true.
*      IF <fs_data>-bitdate IS NOT INITIAL.
*        <fs_data>-bill_first = <fs_data>-bitdate.
*      ENDIF.
*    ENDIF.
*
** Begin of change - 486861 Date: 09/15/2021 Defect # Apollo14 / SIT-13318
****Prefix PO number with "M_" for Acruals
*    IF ( gv_mje = gc_zaot OR gv_mje = gc_zadr )
*      AND ( <fs_data>-bill_basedate IS NOT INITIAL OR <fs_data>-bitdate IS NOT INITIAL ).
** Begin of change - 486861 Date: 10/04/2021 Defect # CFHYP-228
*      IF <fs_data>-zzpo_number IS NOT INITIAL.
*        <fs_data>-zzpo_number = |M_{ <fs_data>-zzpo_number }|.
*      ENDIF.
** End of change - 486861 Date: 10/04/2021 Defect # CFHYP-228
**            <fs_data>-zzpo_number = |M_{ <fs_data>-zzpo_number }|.
*    ENDIF.
** End of change - 486861 Date: 09/15/2021 Defect # Apollo14 / SIT-13318
*
****Pass the Bit Date and Bill base date
*    IF <fs_data>-bitdate IS INITIAL.
*      <fs_data>-bitdate = sy-datum.
*    ENDIF.
*
*    IF <fs_data>-bill_basedate IS INITIAL.
*      <fs_data>-bill_basedate = <fs_data>-bitdate.
*    ENDIF.
*
*    IF <fs_data>-currexc_date IS INITIAL.
*      <fs_data>-currexc_date = <fs_data>-bill_basedate.
*    ENDIF.
*
*
*****Check for Accural Reversal MJE transactions
*    IF ( gv_mje = gc_zaot OR gv_mje = gc_zadr ) AND <fs_data>-zz_rev_date IS NOT INITIAL AND <fs_data>-zz_rev_reason IS NOT INITIAL .
*      MOVE-CORRESPONDING <fs_data> TO gw_data.
*      IF gw_data-currexc_date IS INITIAL.
*        gw_data-currexc_date = <fs_data>-bill_basedate.
*      ENDIF.
*      gw_data-bill_first = <fs_data>-zz_rev_date.
*      gw_data-bukrs  = <fs_data>-bukrs.
*      gw_data-bit_amount = - <fs_data>-bit_amount.
*      gw_data-bitdate = <fs_data>-zz_rev_date.
*      gw_data-bill_basedate = <fs_data>-bill_basedate.
*      gw_data-zz_rev_tid = <fs_data>-srctaid.
*
****Source Transaction Id
*      IF gv_selection = gc_l.
*
****For Real Load , increment the number range for reversal as well
*        CALL FUNCTION 'NUMBER_GET_NEXT'
*          EXPORTING
*            nr_range_nr             = lv_nr
*            object                  = lv_nr_obj
*          IMPORTING
*            number                  = lv_number
*          EXCEPTIONS
*            interval_not_found      = 1
*            number_range_not_intern = 2
*            object_not_found        = 3
*            quantity_is_0           = 4
*            quantity_is_not_1       = 5
*            interval_overflow       = 6
*            buffer_overflow         = 7
*            OTHERS                  = 8.
*        IF sy-subrc <> 0.
****Do Nothing
*        ENDIF.
****Conversion Exit for Source Id
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = lv_number
*          IMPORTING
*            output = gw_data-srctaid.
*      ELSE.
*        lv_nrlevel = lv_nrlevel + 1.
*        gw_data-srctaid =  lv_nrlevel.
*
*      ENDIF.
*
*      APPEND gw_data TO lt_data_accu_revsal.
*      CLEAR gw_data.
*    ENDIF.
*  ENDLOOP.
*
*****Pushing Accural Reversal MJE transactions
*  IF lt_data_accu_revsal IS NOT INITIAL.
*    APPEND  LINES OF lt_data_accu_revsal TO gt_data_it.
*  ENDIF.
*
****Fetch data from Item class ZMJE - Billable for Duplicate check based on Unique Reference ID
*  IF gt_data_it IS NOT INITIAL.
*
****Fetch Unique Reference ID - ZZ_UNIQ_REF data from item class ZMJE - Billable for Duplicate check
*    SELECT vkont,
*           srctatype,
*           srctaid,
*           bitpackuuid,
*           bitpackcno,
*          zz_uniq_ref
*     FROM /1fe/0zmje2it
*     INTO TABLE @DATA(lt_/1fe/0zmje2it)
*     FOR ALL ENTRIES IN @gt_data_it
*     WHERE vkont = @gt_data_it-vkont AND
*           subprocess = @gt_data_it-subprocess AND
*           gpart = @gt_data_it-gpart AND
*           zz_uniq_ref = @gt_data_it-zz_uniq_ref
*     ORDER BY PRIMARY KEY.
*
*    SORT lt_/1fe/0zmje2it ASCENDING BY zz_uniq_ref.
*    DELETE ADJACENT DUPLICATES FROM  lt_/1fe/0zmje2it COMPARING zz_uniq_ref.
*
****Fetch Unique Reference ID - ZZ_UNIQ_REF data from Items for Class ZMJE - Billed for Duplicate check
*    SELECT billcrdate,                                  "#EC CI_NOFIELD
*           billdocno,
*           bit_grpno,
*           bitbillcno,
*           zz_uniq_ref
*     FROM /1fe/0zmje4it00
*    INTO TABLE @DATA(lt_/1fe/0zmje4it00)
*    FOR ALL ENTRIES IN @gt_data_it
**    WHERE bitdate = @gt_data_it-bitdate AND      "Deletion for defect : SIT-14826 - DFDK912416 - 08/19/2021
*     WHERE subprocess = @gt_data_it-subprocess AND "Addition for defect : SIT-14826 - DFDK912416 - 08/19/2021
*           gpart = @gt_data_it-gpart AND
*           zz_uniq_ref = @gt_data_it-zz_uniq_ref
*    ORDER BY PRIMARY KEY.
*
*    SORT lt_/1fe/0zmje4it00 ASCENDING BY zz_uniq_ref.
*    DELETE ADJACENT DUPLICATES FROM lt_/1fe/0zmje4it00 COMPARING zz_uniq_ref.
** Begin of change - 486861 Date: 09/15/2021 Defect # Apollo14 /SIT-14854
*    DATA(lt_refdata_check) = gt_data_it.
*    DATA(lt_temp) = gt_data_it.
*    CLEAR lt_temp.
** Begin of change - 486861 Date: 09/15/2021 Defect # Apollo14 / SIT-13318
** Begin of change - 486861 Date: 10/04/2021 Defect # CFHYP-228
**    SORT lt_refdata_check BY bill_basedate zzpo_number.
**    DELETE ADJACENT DUPLICATES FROM lt_refdata_check COMPARING bill_basedate zzpo_number.
*    SORT lt_refdata_check BY gpart vkont zzpo_number bill_basedate.
*    DELETE ADJACENT DUPLICATES FROM lt_refdata_check COMPARING gpart vkont zzpo_number bill_basedate.
** End of change - 486861 Date: 10/04/2021 Defect # CFHYP-228
*    LOOP AT lt_refdata_check ASSIGNING FIELD-SYMBOL(<fs_refdata_check>).
*      lt_temp = VALUE #( FOR lw_check IN lt_refdata_check
*                           WHERE ( zzpo_number = <fs_refdata_check>-zzpo_number
** Begin of change - 486861 Date: 10/04/2021 Defect # CFHYP-228
*                                   AND gpart = <fs_refdata_check>-gpart
*                                   AND vkont = <fs_refdata_check>-vkont )
*                            ( gpart = lw_check-gpart
*                              vkont = lw_check-vkont
** End of change - 486861 Date: 10/04/2021 Defect # CFHYP-228
*                             bill_basedate  = lw_check-bill_basedate
*                             zzpo_number  = lw_check-zzpo_number ) ).
*      DATA(lv_len) = lines( lt_temp ).
*      IF lv_len GT 1.
*        <fs_refdata_check>-bill_basedate = abap_true.
*      ENDIF.
*      CLEAR : lt_temp.
*    ENDLOOP.
** Begin of change - 486861 Date: 10/04/2021 Defect # CFHYP-228
**    SORT lt_refdata_check BY zzpo_number.
**    DELETE ADJACENT DUPLICATES FROM lt_refdata_check COMPARING zzpo_number.
*    SORT lt_refdata_check BY gpart vkont zzpo_number bill_basedate.
*    DELETE ADJACENT DUPLICATES FROM lt_refdata_check COMPARING gpart vkont zzpo_number bill_basedate.
** End of change - 486861 Date: 10/04/2021 Defect # CFHYP-228
****Commented for # Apollo14 / SIT-13318
**    SORT lt_refdata_check BY bill_basedate zz_uniq_ref.
**    DELETE ADJACENT DUPLICATES FROM lt_refdata_check COMPARING bill_basedate zz_uniq_ref.
**    LOOP AT lt_refdata_check ASSIGNING FIELD-SYMBOL(<fs_refdata_check>).
**      lt_temp = VALUE #( FOR lw_check IN lt_refdata_check
**                           WHERE ( zz_uniq_ref = <fs_refdata_check>-zz_uniq_ref )
**                           ( bill_basedate  = lw_check-bill_basedate
**                             zz_uniq_ref  = lw_check-zz_uniq_ref ) ).
**      DATA(lv_len) = lines( lt_temp ).
**      IF lv_len GT 1.
**        <fs_refdata_check>-bill_basedate = abap_true.
**      ENDIF.
**      CLEAR : lt_temp.
**    ENDLOOP.
**    SORT lt_refdata_check BY zz_uniq_ref.
**    DELETE ADJACENT DUPLICATES FROM lt_refdata_check COMPARING zz_uniq_ref.
** End of change - 486861 Date: 09/15/2021 Defect # Apollo14 / SIT-13318
** End of change - 486861 Date: 09/15/2021 Defect # Apollo14 /SIT-14854
*
** Begin of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
****check profit center entered
*    CLEAR : lt_item.
*    lt_item = gt_data_it[].
*    DELETE lt_item WHERE prctr IS INITIAL.
*    IF lt_item IS NOT INITIAL.
****Check profit center entered
*      SELECT prctr,datbi,kokrs,datab
*        FROM cepc INTO TABLE @DATA(lt_cepc)
*        FOR ALL ENTRIES IN @lt_item
*        WHERE prctr = @lt_item-prctr
*          AND kokrs = @lc_kokrs.
*      IF sy-subrc = 0.
*        SORT lt_cepc DESCENDING BY prctr datbi.
*      ENDIF.
*    ENDIF.
*
**** Check GL account
*    CLEAR : lt_item.
*    lt_item = gt_data_it[].
*    DELETE lt_item WHERE zz_hkont IS INITIAL.
*    IF lt_item IS NOT INITIAL.
****Check GL account entered
*      SELECT bukrs,saknr
*        FROM skb1 INTO TABLE @DATA(lt_skb1)
*        FOR ALL ENTRIES IN @lt_item
*        WHERE bukrs = @lt_item-bukrs
*          AND saknr = @lt_item-zz_hkont.
*      IF sy-subrc = 0.
*        SORT lt_skb1 BY bukrs saknr.
*      ENDIF.
*    ENDIF.
*
**** Check material number
*    CLEAR : lt_item.
*    lt_item = gt_data_it[].
*    DELETE lt_item WHERE matnr IS INITIAL.
*    IF lt_item IS NOT INITIAL.
****Check material number entered
*      SELECT matnr,vkorg
*        FROM mvke INTO TABLE @DATA(lt_mvke)
*        FOR ALL ENTRIES IN @lt_item
*        WHERE matnr = @lt_item-matnr
*          AND vkorg = @lt_item-bukrs.
*      IF sy-subrc = 0.
*        SORT lt_mvke BY matnr vkorg.
*      ENDIF.
*    ENDIF.
** End of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*****Duplicate check for Accrual and Reclass MJE trnasactions
*    LOOP AT gt_data_it ASSIGNING FIELD-SYMBOL(<ls_data_check>).
*
*      IF gv_mje = gc_zaot OR gv_mje = gc_zrcl OR gv_mje = gc_zadr
*        OR gv_mje = gc_zpip OR gv_mje = gc_zopx OR gv_mje = gc_zntr. "Addition for defect : SIT-14826 - DFDK912416 - 08/19/2021
*
*        READ TABLE lt_/1fe/0zmje2it INTO DATA(ls_/1fe/0zmje2it) WITH KEY zz_uniq_ref = <ls_data_check>-zz_uniq_ref BINARY SEARCH ##NEEDED.
*        IF sy-subrc = 0.
*          DATA(gv_check) = abap_true.
*        ENDIF.
*
*        READ TABLE lt_/1fe/0zmje4it00 INTO DATA(ls_/1fe/0zmje4it00) WITH KEY zz_uniq_ref = <ls_data_check>-zz_uniq_ref BINARY SEARCH ##NEEDED.
*
*        IF sy-subrc = 0.
*          gv_check = abap_true.
*        ENDIF.
*      ENDIF.
*
*****Cost Center - Tech Field Name - KOSTL
*      DATA(lv_hkont) = <ls_data_check>-zz_hkont.
*      SHIFT lv_hkont LEFT DELETING LEADING lc_0.
*
****Divide 100 for currency JPY and KRW
*      IF ( <ls_data_check>-bit_amount IS NOT INITIAL ) AND
*              ( <ls_data_check>-bit_curr EQ lc_jpy OR <ls_data_check>-bit_curr EQ lc_krw ).
*        <ls_data_check>-bit_amount = <ls_data_check>-bit_amount / 100.
*      ENDIF.
*
****Mandatory error for payer and bill to
*      IF <ls_data_check>-gpart IS INITIAL AND <ls_data_check>-zz_billto_bp IS INITIAL.
*        <ls_data_check>-status = TEXT-302.
*        <ls_data_check>-error_message = TEXT-305.
****Mandatory error for bill to
*      ELSEIF <ls_data_check>-gpart IS NOT INITIAL AND <ls_data_check>-zz_billto_bp IS INITIAL.
*        <ls_data_check>-status = TEXT-302.
*        <ls_data_check>-error_message = TEXT-304.
****Mandatory error for payer
*      ELSEIF <ls_data_check>-gpart IS INITIAL AND <ls_data_check>-zz_billto_bp IS NOT INITIAL.
*        <ls_data_check>-status = TEXT-302.
*        <ls_data_check>-error_message = TEXT-303.
****Mandatory error for Contract Number
*      ELSEIF <ls_data_check>-vkont IS INITIAL.
*        <ls_data_check>-status = TEXT-302.
*        <ls_data_check>-error_message = TEXT-301.
****Mandatory error for Duplicates records from Billiable and BIT item tables
*      ELSEIF gv_check IS NOT INITIAL.
*        <ls_data_check>-status = TEXT-064.
*        <ls_data_check>-error_message = TEXT-065.
*
******Mandatory check for KOSTL based on G/L Account with start value as '6'
*      ELSEIF <ls_data_check>-kostl IS INITIAL AND lv_hkont+0(1) = lc_6.
*        <ls_data_check>-status = TEXT-302.
*        <ls_data_check>-error_message = TEXT-080.
****Invoice Title - Tech Field Name - ZZ_INV_TITLE ** Mandatory for PIPs, commissions & non-trade
*      ELSEIF <ls_data_check>-zz_inv_title IS INITIAL AND ( gv_mje = gc_zpip OR gv_mje = gc_zopx OR gv_mje = gc_zntr ).
*        <ls_data_check>-status = TEXT-302.
*        <ls_data_check>-error_message = TEXT-306.
** Begin of change - 486861 Date: 10/04/2021 Defect # CFHYP-228
*      ELSEIF <ls_data_check>-zzpo_number IS INITIAL.
*        <ls_data_check>-status = TEXT-302.
*        <ls_data_check>-error_message = TEXT-308.
** End of change - 486861 Date: 10/04/2021 Defect # CFHYP-228
*      ELSE.
** Begin of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
****Check if cost center is valid one
*        IF <ls_data_check>-kostl IS NOT INITIAL.
*          CLEAR : ls_csks.
*          "Binary search is not possible as the fields are sorted based on different field as per requirement
*          READ TABLE lt_csks INTO ls_csks WITH KEY kostl = <ls_data_check>-kostl
*                                                         bukrs = <ls_data_check>-bukrs.
*          IF sy-subrc = 0.
****Check if cost center is valid at posting date
*            IF ls_csks-datbi LT <ls_data_check>-bitdate.
*              <ls_data_check>-status = TEXT-302.
*              <ls_data_check>-error_message = TEXT-310.
*              PERFORM f_message_build USING <ls_data_check>-kostl '' '' '' '' <ls_data_check>-bitdate ''
*                                      CHANGING <ls_data_check>-error_message.
*              DATA(lv_error_chk) = abap_true.
****Check if cost center is valid at base line date
*            ELSEIF ls_csks-datbi LT <ls_data_check>-bill_basedate.
*              <ls_data_check>-status = TEXT-302.
*              <ls_data_check>-error_message = TEXT-310.
*              PERFORM f_message_build USING <ls_data_check>-kostl '' '' '' '' <ls_data_check>-bill_basedate ''
*                                      CHANGING <ls_data_check>-error_message.
*              lv_error_chk = abap_true.
****Check if cost center is valid at current date
*            ELSEIF ls_csks-datbi LT sy-datum.
*              <ls_data_check>-status = TEXT-302.
*              <ls_data_check>-error_message = TEXT-310.
*              PERFORM f_message_build USING <ls_data_check>-kostl '' '' '' '' sy-datum ''
*                                      CHANGING <ls_data_check>-error_message.
*              lv_error_chk = abap_true.
*            ENDIF.
*          ELSE.
****If cost center is not valid
*            <ls_data_check>-status = TEXT-302.
*            <ls_data_check>-error_message = TEXT-309.
*            PERFORM f_message_build USING <ls_data_check>-kostl '' '' '' '' '' <ls_data_check>-bukrs
*                                    CHANGING <ls_data_check>-error_message.
*            lv_error_chk = abap_true.
*          ENDIF.
*        ENDIF.
****Check for profit center if valid
*        IF <ls_data_check>-prctr IS NOT INITIAL.
*          READ TABLE lt_cepc INTO DATA(ls_cepc) WITH KEY prctr = <ls_data_check>-prctr.
*          IF sy-subrc = 0.
****Check if profit center is valid at posting date
*            IF ls_cepc-datbi LT <ls_data_check>-bitdate.
*              <ls_data_check>-status = TEXT-302.
*              <ls_data_check>-error_message = TEXT-312.
*              PERFORM f_message_build USING '' <ls_data_check>-prctr '' '' '' <ls_data_check>-bitdate ''
*                                      CHANGING <ls_data_check>-error_message.
*              lv_error_chk = abap_true.
****Check if profit center is valid at baseline date
*            ELSEIF ls_cepc-datbi LT <ls_data_check>-bill_basedate.
*              <ls_data_check>-status = TEXT-302.
*              <ls_data_check>-error_message = TEXT-312.
*              PERFORM f_message_build USING '' <ls_data_check>-prctr '' '' '' <ls_data_check>-bill_basedate ''
*                                      CHANGING <ls_data_check>-error_message.
*              lv_error_chk = abap_true.
****Check if profit center is valid at current date
*            ELSEIF ls_cepc-datbi LT sy-datum.
*              <ls_data_check>-status = TEXT-302.
*              <ls_data_check>-error_message = TEXT-312.
*              PERFORM f_message_build USING '' <ls_data_check>-prctr '' '' '' sy-datum ''
*                                      CHANGING <ls_data_check>-error_message.
*              lv_error_chk = abap_true.
*            ENDIF.
*          ELSE.
****If profit center is not valid
*            <ls_data_check>-status = TEXT-302.
*            <ls_data_check>-error_message = TEXT-311.
*            PERFORM f_message_build USING '' <ls_data_check>-prctr '' '' '' '' ''
*                                    CHANGING <ls_data_check>-error_message.
*            lv_error_chk = abap_true.
*          ENDIF.
*        ENDIF.
****Check if WBS element is valid
*        IF <ls_data_check>-ps_posid IS NOT INITIAL.
*          CLEAR : ls_prps_conv,ls_prps.
*          READ TABLE lt_conv_prps INTO ls_prps_conv WITH KEY posid = <ls_data_check>-ps_posid.
**
*          IF sy-subrc = 0 AND <ls_data_check>-ps_posid IS NOT INITIAL AND ls_prps_conv-pspnr IS NOT INITIAL.
**      "changing code to work properly as we have to sort table with different fields
*            READ TABLE lt_prps INTO ls_prps WITH KEY pspnr = ls_prps_conv-pspnr pbukr = <fs_data>-bukrs.
*            IF sy-subrc <> 0.
*              <ls_data_check>-status = TEXT-302.
*              <ls_data_check>-error_message = TEXT-315.
*              PERFORM f_message_build USING '' '' <ls_data_check>-ps_posid '' '' '' <ls_data_check>-bukrs
*                                      CHANGING <ls_data_check>-error_message.
*              lv_error_chk = abap_true.
*            ENDIF.
*            ELSE.
*              <ls_data_check>-status = TEXT-302.
*              <ls_data_check>-error_message = TEXT-315.
*              PERFORM f_message_build USING '' '' <ls_data_check>-ps_posid '' '' '' <ls_data_check>-bukrs
*                                      CHANGING <ls_data_check>-error_message.
*              lv_error_chk = abap_true.
*          ENDIF.
*        ENDIF.
****Check if GL account is valid at posting date
*        IF <ls_data_check>-zz_hkont IS NOT INITIAL.
*          READ TABLE lt_skb1 INTO DATA(ls_skb1) WITH KEY bukrs = <ls_data_check>-bukrs
*                                                         saknr = <ls_data_check>-zz_hkont BINARY SEARCH.
*          IF sy-subrc <> 0.
*            <ls_data_check>-status = TEXT-302.
*            <ls_data_check>-error_message = TEXT-313.
*            PERFORM f_message_build USING '' '' '' <ls_data_check>-zz_hkont '' '' <ls_data_check>-bukrs
*                                    CHANGING <ls_data_check>-error_message.
*            lv_error_chk = abap_true.
*          ENDIF.
*        ENDIF.
****Check if material is valid at posting date
*        IF <ls_data_check>-matnr IS NOT INITIAL.
*          READ TABLE lt_mvke INTO DATA(ls_mvke) WITH KEY matnr = <ls_data_check>-matnr
*                                                         vkorg = <ls_data_check>-bukrs BINARY SEARCH.
*          IF sy-subrc <> 0.
*            <ls_data_check>-status = TEXT-302.
*            <ls_data_check>-error_message = TEXT-314.
*            PERFORM f_message_build USING '' '' '' '' <ls_data_check>-matnr '' <ls_data_check>-bukrs
*                                    CHANGING <ls_data_check>-error_message.
*            lv_error_chk = abap_true.
*          ENDIF.
*        ENDIF.
** End of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*
** Begin of change - 486861 Date: 10/05/2021 Defect # CFHYP-228
*        READ TABLE lt_refdata_check INTO DATA(lw_refdata_check) WITH KEY gpart = <ls_data_check>-gpart
*                                                                         vkont = <ls_data_check>-vkont
*                                                                         zzpo_number = <ls_data_check>-zzpo_number.
*        IF sy-subrc = 0 AND lw_refdata_check-bill_basedate = abap_true.
*          <ls_data_check>-status = TEXT-302.
*          <ls_data_check>-error_message = TEXT-307.
*          CLEAR : lw_refdata_check.
** Begin of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*          lv_error_chk = abap_true.
*        ENDIF.
**        ELSE.
*        IF lv_error_chk IS INITIAL.
** End of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*****Push the data to BIT UPLOAD API UPLOAD format
*          <ls_data_check>-status = TEXT-066.
*          MOVE-CORRESPONDING <ls_data_check> TO ls_api_data_it ##ENH_OK.
*          APPEND ls_api_data_it TO gt_api_data_it.
*          CLEAR : ls_api_data_it.
*        ENDIF.
*
*      ENDIF.
*
******Push the data to BIT UPLOAD API UPLOAD format
**        <ls_data_check>-status = TEXT-066.
**        MOVE-CORRESPONDING <ls_data_check> TO ls_api_data_it ##ENH_OK.
**        APPEND ls_api_data_it TO gt_api_data_it.
**        CLEAR : ls_api_data_it.
**      ENDIF.
** End of change - 486861 Date: 10/05/2021 Defect # CFHYP-228
** Begin of change - 486861 Date: 09/15/2021 Defect # Apollo14 /SIT-14854
** Begin of change - 486861 Date: 09/15/2021 Defect # Apollo14 / SIT-13318
** Begin of change - 486861 Date: 10/04/2021 Defect # CFHYP-228
**      READ TABLE lt_refdata_check INTO DATA(lw_refdata_check) WITH KEY zzpo_number = <ls_data_check>-zzpo_number.
**      READ TABLE lt_refdata_check INTO DATA(lw_refdata_check) WITH KEY gpart = <ls_data_check>-gpart
**                                                                       vkont = <ls_data_check>-vkont
**                                                                       zzpo_number = <ls_data_check>-zzpo_number.
**      IF sy-subrc = 0 AND lw_refdata_check-bill_basedate = abap_true.
**        <ls_data_check>-status = TEXT-302.
**        <ls_data_check>-error_message = TEXT-307.
**        CLEAR : lw_refdata_check.
**      ENDIF.
**      READ TABLE lt_refdata_check INTO DATA(lw_refdata_check) WITH KEY zz_uniq_ref = <ls_data_check>-zz_uniq_ref.
**      IF sy-subrc = 0 AND lw_refdata_check-bill_basedate = abap_true.
**        <ls_data_check>-status = TEXT-302.
**        <ls_data_check>-error_message = TEXT-307.
**        CLEAR : lw_refdata_check.
**      ENDIF.
** End of change - 486861 Date: 09/15/2021 Defect # Apollo14 / SIT-13318
** End of change - 486861 Date: 09/15/2021 Defect # Apollo14 /SIT-14854
*      CLEAR :  gv_check,
*               ls_/1fe/0zmje2it,
*               ls_/1fe/0zmje4it00,
*               lv_hkont,
** Begin of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*               ls_csks,ls_cepc,ls_skb1,ls_mvke,
*               lv_error_chk.
** End of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
*
*    ENDLOOP.
*
*  ENDIF.
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form f_clear_global_var
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_clear_global_var .
*
*  CLEAR : gt_data_it,
*            gt_api_data_it,
*            gt_error_log,
*            gt_data_py,
*            gt_data_tx,
*            gt_data_tt,
*            gt_output,
*            gt_error_log.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form result_load_bit_msg_get
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> GV_NO_COMMIT
**&      <-- LT_MSG_BIX
**&---------------------------------------------------------------------*
*FORM f_result_load_bit_msg_get  USING    p_gv_no_commit TYPE xfeld
*                              CHANGING et_msg_disp TYPE fkkbix_msg_disp_tab.
*
**&---------------------------------------------------------------------*
*********** Local Strcutures Declaration
**&---------------------------------------------------------------------*
*  DATA: ls_msg_disp TYPE fkkbix_msg_disp.
*
*  " echt oder simu
*  IF p_gv_no_commit = gc_x.
*    MESSAGE w077(fkkbix2) INTO ls_msg_disp-msgtx.
*    ls_msg_disp-msgty = gc_warning.
*    APPEND ls_msg_disp TO et_msg_disp.
*
*    CLEAR ls_msg_disp.
*    APPEND ls_msg_disp TO et_msg_disp .
*  ENDIF.
*
*  LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<ls_output>).
*
*    IF lines( gt_output ) > 1.
*      CLEAR ls_msg_disp.
*      " space line
*      APPEND ls_msg_disp TO et_msg_disp .
*    ENDIF.
*
*    DATA(lv_space) = |\|     |.
*
*    IF <ls_output>-error = space.
*      ls_msg_disp-msgty = gc_success.
*    ELSE.
*      ls_msg_disp-msgty = gc_error.
*    ENDIF.
*
*    " BITCAT
*    ls_msg_disp-msgtx = TEXT-038 && | | && <ls_output>-bitcat && |: |.
*    APPEND ls_msg_disp TO et_msg_disp .
*
*    IF <ls_output>-error = gc_x.
*      MESSAGE i820(fkkbix) INTO ls_msg_disp-msgtx.
*      ls_msg_disp-msgtx = lv_space && ls_msg_disp-msgtx.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*
*    " number of loaded items
*    IF p_gv_no_commit = space.
*      DATA(lv_cnt_all) = <ls_output>-cnt-it + <ls_output>-cnt-it0 + <ls_output>-cnt-it1 + <ls_output>-cnt-it3 + lines( <ls_output>-data-bit4key_billed ).
*    ELSE.
*      lv_cnt_all = <ls_output>-cnt-it + <ls_output>-cnt-it0 + <ls_output>-cnt-it1 + <ls_output>-cnt-it3.
*    ENDIF.
*    IF lv_cnt_all > 0.
*      ls_msg_disp-msgtx = |\|  | && TEXT-039.
*      ls_msg_disp-cntxx = lv_cnt_all.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*
*    " bit2
*    IF <ls_output>-cnt-it > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-040.
*      ls_msg_disp-cntxx = <ls_output>-cnt-it.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*    IF <ls_output>-cnt-py > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-041.
*      ls_msg_disp-cntxx = <ls_output>-cnt-py.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*    IF <ls_output>-cnt-tx > 0.
*      ls_msg_disp-msgtx = lv_space &&  TEXT-042.
*      ls_msg_disp-cntxx = <ls_output>-cnt-tx.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*    IF <ls_output>-cnt-tt > 0.
*      ls_msg_disp-msgtx = lv_space &&  TEXT-043.
*      ls_msg_disp-cntxx = <ls_output>-cnt-tt.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*
*    " bit1
*    IF <ls_output>-cnt-it1 > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-044.
*      ls_msg_disp-cntxx = <ls_output>-cnt-it1.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*    IF <ls_output>-cnt-py1 > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-045.
*      ls_msg_disp-cntxx = <ls_output>-cnt-py1.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*    IF <ls_output>-cnt-tx1 > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-046.
*      ls_msg_disp-cntxx = <ls_output>-cnt-tx1.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*    IF <ls_output>-cnt-tt1 > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-047.
*      ls_msg_disp-cntxx = <ls_output>-cnt-tt1.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*
*    " bit0
*    IF <ls_output>-cnt-it0 > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-048.
*      ls_msg_disp-cntxx = <ls_output>-cnt-it0.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*    IF <ls_output>-cnt-py0 > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-049.
*      ls_msg_disp-cntxx = <ls_output>-cnt-py0.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*    IF <ls_output>-cnt-tx0 > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-050.
*      ls_msg_disp-cntxx = <ls_output>-cnt-tx0.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*    IF <ls_output>-cnt-tt0 > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-050.
*      ls_msg_disp-cntxx = <ls_output>-cnt-tt0.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*
*    " bit3
*    IF <ls_output>-cnt-it3 > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-051.
*      ls_msg_disp-cntxx = <ls_output>-cnt-it3.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*    IF <ls_output>-cnt-py3 > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-052.
*      ls_msg_disp-cntxx = <ls_output>-cnt-py3.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*    IF <ls_output>-cnt-tx3 > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-053.
*      ls_msg_disp-cntxx = <ls_output>-cnt-tx3.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*    IF <ls_output>-cnt-tt3 > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-054.
*      ls_msg_disp-cntxx = <ls_output>-cnt-tt3.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*
*    " billdocno
*    IF <ls_output>-cnt-billdocno > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-055.
*      ls_msg_disp-cntxx = <ls_output>-cnt-billdocno.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*
*    " invdocno
*    IF <ls_output>-cnt-invdocno > 0.
*      ls_msg_disp-msgtx = lv_space && TEXT-056.
*      ls_msg_disp-cntxx = <ls_output>-cnt-invdocno.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*
*    IF <ls_output>-cnt-it_err > 0.
*      CLEAR ls_msg_disp.
*      ls_msg_disp-msgty = gc_error.
*      ls_msg_disp-msgtx = lv_space && TEXT-057.
*      ls_msg_disp-cntxx = <ls_output>-cnt-it_err.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*
*    IF <ls_output>-cnt-py_err > 0.
*      CLEAR ls_msg_disp.
*      ls_msg_disp-msgty = gc_error.
*      ls_msg_disp-msgtx = lv_space && TEXT-058.
*      ls_msg_disp-cntxx = <ls_output>-cnt-py_err.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*
*    IF <ls_output>-cnt-tx_err > 0.
*      CLEAR ls_msg_disp.
*      ls_msg_disp-msgty = gc_error.
*      ls_msg_disp-msgtx = lv_space && TEXT-059.
*      ls_msg_disp-cntxx = <ls_output>-cnt-tx_err.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*
*    IF <ls_output>-cnt-srcta_err > 0.
*      CLEAR ls_msg_disp.
*      ls_msg_disp-msgty = gc_error.
*      ls_msg_disp-msgtx = lv_space && TEXT-060.
*      ls_msg_disp-cntxx = <ls_output>-cnt-srcta_err.
*      APPEND ls_msg_disp TO et_msg_disp.
*    ENDIF.
*
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form log_open
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_log_open .
*
**----------------------------------------------------------------------*
** Constant Declaration
**----------------------------------------------------------------------*
*  CONSTANTS: lc_object TYPE  balobj_d VALUE 'FKKBIX',
*             lc_subobj TYPE  balsubobj VALUE 'CREATE'.
*
*  IF gs_current-s_log-open = space.
*
*    cl_fkkinv_log=>create( EXPORTING x_object     = lc_object
*                                     x_subobject  = lc_subobj
*                           IMPORTING y_log_handle = gs_current-s_log-log_handle ).
*    MESSAGE i091(fkkbix2)  INTO cl_fkkinv_log=>str.
*    cl_fkkinv_log=>add_msg( ).
*
*    gs_current-s_log-open = gc_x.
*
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form log_close
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> P_
**&---------------------------------------------------------------------*
*FORM f_log_close USING iv_save  TYPE xfeld.   " save the log or not
*
*  IF gs_current-s_log-open = gc_x.
*
*    MESSAGE i099(fkkbix2) INTO cl_fkkinv_log=>str.
*    cl_fkkinv_log=>add_msg( ).
*    IF iv_save = gc_x.
*      cl_fkkinv_log=>save( ).
*    ENDIF.
*    cl_fkkinv_log=>close( ).
*
*    gs_current-s_log-open = space.
*
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_get_constants
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> SY_CPROG
**&---------------------------------------------------------------------*
*FORM f_get_constants  USING im_pgmid TYPE syst_cprog.
****-Call ZUTIL_PGM_CONSTANTS Utility FM to fetch the constants
*  CALL FUNCTION 'ZUTIL_PGM_CONSTANTS'
*    EXPORTING
*      im_pgmid               = im_pgmid
*    TABLES
*      t_pgm_const_values     = gt_pgm_const_values
*      t_error_const          = gt_error_const
*    EXCEPTIONS
*      ex_no_entries_found    = 1
*      ex_const_entry_missing = 2
*      OTHERS                 = 3.
*  IF sy-subrc <> 0.
*    CASE sy-subrc.
*      WHEN 1.
**--No data found in TVARVC table
*        MESSAGE e007 WITH 'TVARVC'(062).
*      WHEN 2.
**--Atleast one constant entry missing in TVARVC table
*        MESSAGE e010 WITH 'TVARVC'(062).
*      WHEN OTHERS.
*    ENDCASE.
*  ENDIF.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_collect_constants
**&---------------------------------------------------------------------*
**& collect TVARVC values
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM f_collect_constants .
*
*  TRY .
*      gv_mwskz_us = gt_pgm_const_values[
*                                    const_name = 'P_MWSKZ_ZMJE' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_MWSKZ_ZMJE'(067).
*  ENDTRY.
*
*  TRY .
*      gv_mwskz_non_us = gt_pgm_const_values[
*                                    const_name = 'P_MWSKZ_NON_US_ZMJE' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_MWSKZ_NON_US_ZMJE'(078).
*  ENDTRY.
*
*  TRY .
*      gv_spart = gt_pgm_const_values[
*                                    const_name = 'P_SPART_KK_ZMJE' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_SPART_KK_ZMJE'(068).
*  ENDTRY.
*
*  TRY .
*      gv_srctype = gt_pgm_const_values[
*                                    const_name = 'P_SRCTATYPE_KK_ZMJE' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_SRCTATYPE_KK_ZMJE'(069).
*  ENDTRY.
*
*  TRY .
*      gv_sc03 = gt_pgm_const_values[
*                                    const_name = 'P_SUB_PROCESS_KK03_ZMJE' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_SUB_PROCESS_KK03_ZMJE'(070).
*  ENDTRY.
*  TRY .
*      gv_sc04 = gt_pgm_const_values[
*                                    const_name = 'P_SUB_PROCESS_KK04_ZMJE' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_SUB_PROCESS_KK04_ZMJE'(071).
*  ENDTRY.
*
*  TRY .
*      gv_sc05 = gt_pgm_const_values[
*                                    const_name = 'P_SUB_PROCESS_KK05_ZMJE' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_SUB_PROCESS_KK05_ZMJE'(072).
*  ENDTRY.
*
*  TRY .
*      gv_sc06 = gt_pgm_const_values[
*                                    const_name = 'P_SUB_PROCESS_KK06_ZMJE' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_SUB_PROCESS_KK06_ZMJE'(073).
*  ENDTRY.
*
*  TRY .
*      gv_sc07 = gt_pgm_const_values[
*                                    const_name = 'P_SUB_PROCESS_KK07_ZMJE' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_SUB_PROCESS_KK07_ZMJE'(074).
*  ENDTRY.
*
*  TRY .
*      gv_taxdate_type = gt_pgm_const_values[
*                                    const_name = 'P_TAX_DATE_TYPE_KK_ZMJE' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_TAX_DATE_TYPE_KK_ZMJE'(076).
*  ENDTRY.
*
*  TRY .
*      gv_taxdet_type  = gt_pgm_const_values[
*                                    const_name = 'P_TAX_DET_TYPE_KK_ZMJE' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_TAX_DET_TYPE_KK_ZMJE'(077).
*  ENDTRY.
*
**** 320D changes
*
*  TRY .
*      gv_payer  = gt_pgm_const_values[
*                                    const_name = 'P_PARVW_PAYER' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_PARVW_PAYER'(081).
*  ENDTRY.
*
*
*  TRY .
*      gv_billto  = gt_pgm_const_values[
*                                    const_name = 'P_PARVW_BILLTO' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_PARVW_BILLTO'(082).
*  ENDTRY.
*
*  TRY .
*      gv_mwskz_us_e1  = gt_pgm_const_values[
*                                    const_name = 'P_MWSKZ_ZMJE_E1' ]-low .
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025 WITH 'P_MWSKZ_ZMJE_E1'(091).
*  ENDTRY.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_fill_payer_billto_details
**&---------------------------------------------------------------------*
**& Fill Payer and Bill to details based on Sold to detail
**&---------------------------------------------------------------------*
*FORM f_fill_payer_billto_details .
****Variable declaration
*  DATA : lv_error_col TYPE i.
*
****Constants declaration
*  CONSTANTS : lc_178 TYPE symsgno VALUE '178',
*              lc_179 TYPE symsgno VALUE '179',
*              lc_184 TYPE symsgno VALUE '184'.
*
*  IF gt_data_it IS NOT INITIAL.
****Get payer and bill to details from KNVP
*    SELECT kunnr,
*           vkorg,
*           parvw,
*           kunn2
*      FROM knvp
*      INTO TABLE @DATA(lt_knvp)
*      FOR ALL ENTRIES IN @gt_data_it
*      WHERE kunnr = @gt_data_it-zz_soldto_bp
*      AND vkorg = @gt_data_it-bukrs
*      AND ( parvw = @gv_billto OR parvw = @gv_payer ).
*    IF sy-subrc = 0.
****Sort the KNVP table for read
*      SORT lt_knvp BY kunnr vkorg parvw.
*    ENDIF.
*    CLEAR : lv_error_col.
****Loop through gt_data_it to update payer and bill to
*    LOOP AT gt_data_it ASSIGNING FIELD-SYMBOL(<fs_data_it>).
**** --- Adding count to map for Mandatory field
*      lv_error_col = lv_error_col + 1.
****Fill payer
*      READ TABLE lt_knvp INTO DATA(lw_knvp) WITH KEY kunnr = <fs_data_it>-zz_soldto_bp
*                                                     vkorg = <fs_data_it>-bukrs
*                                                     parvw = gv_payer BINARY SEARCH.
*      IF sy-subrc = 0.
*        <fs_data_it>-gpart = lw_knvp-kunn2.
*        CLEAR : lw_knvp.
*      ENDIF.
****Fill Bill to
*      READ TABLE lt_knvp INTO lw_knvp WITH KEY kunnr = <fs_data_it>-zz_soldto_bp
*                                             vkorg = <fs_data_it>-bukrs
*                                             parvw = gv_billto BINARY SEARCH.
*      IF sy-subrc = 0.
*        <fs_data_it>-zz_billto_bp = lw_knvp-kunn2.
*        CLEAR : lw_knvp.
*      ENDIF.
*
*      CLEAR : gw_error_log,lw_knvp.
*    ENDLOOP.
*
*  ENDIF.
*ENDFORM.
** Begin of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
**&---------------------------------------------------------------------*
**& Form f_message_build
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> <LS_DATA_CHECK>_KOSTL
**&      --> <LS_DATA_CHECK>_BITDATE
**&      <-- <LS_DATA_CHECK>_ERROR_MESSAGE
**&---------------------------------------------------------------------*
*FORM f_message_build  USING    iv_kostl TYPE kostl
*                               iv_prctr TYPE prctr
*                               iv_wbs   TYPE any
*                               iv_gl    TYPE saknr
*                               iv_matnr TYPE matnr
*                               iv_date TYPE any "bitdate_kk
*                               iv_bukrs  TYPE bukrs
*                      CHANGING cv_error_message TYPE bapi_msg.
*
*  IF iv_kostl IS NOT INITIAL.
*    REPLACE '&1' WITH iv_kostl INTO cv_error_message .
*  ENDIF.
*  IF iv_prctr IS NOT INITIAL.
*    REPLACE '&1' WITH iv_prctr INTO cv_error_message .
*  ENDIF.
*  IF iv_wbs IS NOT INITIAL.
*    REPLACE '&1' WITH iv_wbs INTO cv_error_message .
*  ENDIF.
*  IF iv_gl IS NOT INITIAL.
*    REPLACE '&1' WITH iv_gl INTO cv_error_message .
*  ENDIF.
*  IF iv_matnr IS NOT INITIAL.
*    REPLACE '&1' WITH iv_matnr INTO cv_error_message .
*  ENDIF.
*
*  IF  iv_bukrs IS NOT INITIAL.
*    REPLACE '&2' WITH iv_bukrs INTO cv_error_message.
*  ENDIF.
*  IF  iv_date IS NOT INITIAL.
*    REPLACE '&2' WITH iv_date INTO cv_error_message.
*  ENDIF.
*  CONDENSE cv_error_message.
*ENDFORM.
** End of change - 486861 Date: 11/02/2021 Defect # CFHYP-333
