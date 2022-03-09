**&---------------------------------------------------------------------*
**& Include          ZINFI_CHARGE_IMPORT_TENROX_SUB
**&---------------------------------------------------------------------*
**-----------------------------------------------------------------------------------------------------------------------------------------*
**                                                          MODIFICATION HISTORY                                                           |
**-----------------------------------------------------------------------------------------------------------------------------------------*
** Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
**-----------------------------------------------------------------------------------------------------------------------------------------*
** 01-SEP-2020 | 477237              | CF.INT.017     | DFDK900449   | Charge Import to Tenrox                                             |
**-----------------------------------------------------------------------------------------------------------------------------------------*
*
**&---------------------------------------------------------------------*
**& Form F_GET_CONSTANTS
**&---------------------------------------------------------------------*
**& Fetch the constants for the program
**&---------------------------------------------------------------------*
*FORM f_get_constants  USING im_pgmid TYPE char40.
*
**** Get the Program Constants for the Program
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
*        MESSAGE e007 WITH 'TVARVC'(001).      "No data found in & table
*      WHEN 2.
*        MESSAGE e010 WITH 'TVARVC'(001).      "Atleast one constant entry missing in & table
*      WHEN OTHERS.
*    ENDCASE.
*  ENDIF.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form F_COLLECT_CONSTANTS
**&---------------------------------------------------------------------*
**& Collect the value of the Constants in variables
**&---------------------------------------------------------------------*
*FORM f_collect_constants.
*
**** Read the Constants values
*  TRY .
*      gv_kokrs          = gt_pgm_const_values[ const_name = 'P_KOKRS_2000' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_KOKRS_2000'(002).
*  ENDTRY.
*
*  TRY .
*      gv_wrttp          = gt_pgm_const_values[ const_name = 'P_WRTTP_04' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_WRTTP_04'(003).
*  ENDTRY.
*
*  TRY .
*      gv_kstar_low      = gt_pgm_const_values[ const_name = 'S_KSTAR_600000_699999' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'S_KSTAR_600000_699999'(004).
*  ENDTRY.
*
*  TRY .
*      gv_kstar_high     = gt_pgm_const_values[ const_name = 'S_KSTAR_600000_699999' ]-high.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'S_KSTAR_600000_699999'(004).
*  ENDTRY.
*
*  TRY .
*      gv_ktopl          = gt_pgm_const_values[ const_name = 'P_KTOPL_ADCA' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_KTOPL_ADCA'(005).
*  ENDTRY.
*
*  TRY .
*      gv_spras          = gt_pgm_const_values[ const_name = 'P_SPRAS_E' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_SPRAS_E'(006).
*  ENDTRY.
*
*  TRY .
*      gv_rfc_dest       = gt_pgm_const_values[ const_name = 'P_RFC_DEST_MULESOFT' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_RFC_DEST_MULESOFT'(007).
*  ENDTRY.
*
**  TRY .
**      gv_api_key        = gt_pgm_const_values[ const_name = 'P_API_KEY_CHARGE_IMPORT' ]-low.
**    CATCH cx_sy_itab_line_not_found.
**      MESSAGE e025 WITH 'P_API_KEY_CHARGE_IMPORT'(008).
**  ENDTRY.
**
**  TRY .
**      gv_api_secret     = gt_pgm_const_values[ const_name = 'P_API_SECRET_CHARGE_IMPORT' ]-low.
**    CATCH cx_sy_itab_line_not_found.
**      MESSAGE e025 WITH 'P_API_SECRET_CHARGE_IMPORT'(009).
**  ENDTRY.
*
*  TRY .
*      gv_path_uri       = gt_pgm_const_values[ const_name = 'P_PATH_URI_CHARGE_IMPORT' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_PATH_URI_CHARGE_IMPORT'(010).
*  ENDTRY.
*
*  TRY .
*      gv_pspan_id       = gt_pgm_const_values[ const_name = 'P_PSPAN_ID_CHARGE_IMPORT' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_PSPAN_ID_CHARGE_IMPORT'(011).
*  ENDTRY.
*
*  TRY .
*      gv_span_id        = gt_pgm_const_values[ const_name = 'P_SPAN_ID_CHARGE_IMPORT' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_SPAN_ID_CHARGE_IMPORT'(012).
*  ENDTRY.
*
*  TRY .
*      gv_srv_moniker    = gt_pgm_const_values[ const_name = 'P_SRV_MONIKER_CHARGE_IMPORT' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_SRV_MONIKER_CHARGE_IMPORT'(013).
*  ENDTRY.
*
*  TRY .
*      gv_event_id       = gt_pgm_const_values[ const_name = 'P_EVENT_ID_CHARGE_IMPORT' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_EVENT_ID_CHARGE_IMPORT'(014).
*  ENDTRY.
*
*  TRY .
*      gv_channel        = gt_pgm_const_values[ const_name = 'P_CHANNEL_CHARGE_IMPORT' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_CHANNEL_CHARGE_IMPORT'(015).
*  ENDTRY.
*
*  TRY .
*      gv_topic          = gt_pgm_const_values[ const_name = 'P_TOPIC_CHARGE_IMPORT' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_TOPIC_CHARGE_IMPORT'(016).
*  ENDTRY.
*
*  TRY .
*      gv_last_run_date  = gt_pgm_const_values[ const_name = 'P_LAST_RUN_DATE_TENROX' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_LAST_RUN_DATE_TENROX'(017).
*  ENDTRY.
*
*  IF line_exists( gt_pgm_const_values[ const_name = 'S_PROFIDPROJ_CONSULTING_PROJ' ] ). "-low.
*    gv_proj_profl_param = 'S_PROFIDPROJ_CONSULTING_PROJ'(018).
*  ELSE.
*    MESSAGE e025 WITH 'S_PROFIDPROJ_CONSULTING_PROJ'(018).
*  ENDIF.
*
*  TRY .
*      gv_interface_id  = gt_pgm_const_values[ const_name = 'P_ZINTERFACEID_TENROX_CHG_IMP' ]-low.
*    CATCH cx_sy_itab_line_not_found.
*      MESSAGE e025 WITH 'P_ZINTERFACEID_TENROX_CHG_IMP'(019).
*  ENDTRY.
*
*  gt_expenses_series = VALUE #( FOR lw_pgm_const_values IN gt_pgm_const_values
*                                WHERE ( const_name = 'S_EXPENSES_SERIES_SKIP_TENROX' )
*                                ( sign = lw_pgm_const_values-sign
*                                  option = lw_pgm_const_values-opti
*                                  low = lw_pgm_const_values-low
*                                  high = lw_pgm_const_values-high ) ).
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form F_VALIDATE_RUNDT
**&---------------------------------------------------------------------*
**& Validate the Run Date
**&---------------------------------------------------------------------*
*FORM f_validate_rundt.
*
**** Data Declaration
*  DATA: lv_date TYPE char10.
*
**** Compare with Run Date with Current Date
*  IF p_rundt GT sy-datum.
*    MESSAGE e106.    "Program cannot be run for a future date
*  ENDIF.
*
**** Compare Run Date with Last Run Date
*  IF gv_last_run_date GT p_rundt.
*    WRITE |{ gv_last_run_date DATE = USER }| TO lv_date.
*    MESSAGE e107 WITH lv_date.    "Program already run for a later date, MM/DD/YYYY
*  ELSEIF gv_last_run_date EQ p_rundt.
*    WRITE |{ gv_last_run_date DATE = USER }| TO lv_date.
*    MESSAGE w108 WITH lv_date.   "Program already run for the entered date, MM/DD/YYYYY
*  ENDIF.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form F_FETCH_DATA
**&---------------------------------------------------------------------*
**& Fetch the data
**&---------------------------------------------------------------------*
*FORM f_fetch_data.
*
**** Fetch the Charge Import from CDS View 'ZFI_CPV_CHARGEIMPORT'
**** based on the Posting Date
*  SELECT  project_definition_nbr,
*          expenses_series,
*          co_object_nm,
*          document_dt,
*          value_cocur_amt,
*          cocur_type,
*          bill_nm,
*          cost_desc,
*          vendor_nm,
*          po_nbr,
*          cost_desc_detail_nm,
*          sap_unique_id
*    FROM zfi_cpv_chargeimport(  p_kokrs      = @gv_kokrs,
*                                p_wrttp      = @gv_wrttp,
*                                p_kstar_low  = @gv_kstar_low,
*                                p_kstar_high = @gv_kstar_high,
*                                p_ktopl      = @gv_ktopl,
*                                p_spras      = @gv_spras,
*                                p_name       = @gv_proj_profl_param )
*    INTO TABLE @DATA(lt_data)
*    WHERE expenses_series NOT IN @gt_expenses_series
**      AND budat GE @gv_last_run_date
**      AND budat LT @p_rundt.
*      AND cpudt GE @gv_last_run_date
*      AND cpudt LT @p_rundt.
*
**** Fetch the Error Records from the Log Table
**** using the CDS View 'ZFI_CPV_TENROXERRORLOG'
*  SELECT  belnr,
*          buzei
*    FROM zfi_cpv_tenroxerrorlog
*    INTO TABLE @DATA(lt_error_data). "#EC CI_NOWHERE   "Condition already added in the CDS View
*  IF sy-subrc EQ 0 AND lt_error_data IS NOT INITIAL.
**** Fetch the Charge Import from CDS View 'ZFI_CPV_CHARGE_IMPORT_TENROX'
**** for the Error Records
*    SELECT  project_definition_nbr,
*            expenses_series,
*            co_object_nm,
*            document_dt,
*            value_cocur_amt,
*            cocur_type,
*            bill_nm,
*            cost_desc,
*            vendor_nm,
*            po_nbr,
*            cost_desc_detail_nm,
*            sap_unique_id
*      FROM zfi_cpv_chargeimport(  p_kokrs      = @gv_kokrs,
*                                  p_wrttp      = @gv_wrttp,
*                                  p_kstar_low  = @gv_kstar_low,
*                                  p_kstar_high = @gv_kstar_high,
*                                  p_ktopl      = @gv_ktopl,
*                                  p_spras      = @gv_spras,
*                                  p_name       = @gv_proj_profl_param )
*      APPENDING TABLE @lt_data
*      FOR ALL ENTRIES IN @lt_error_data
*      WHERE expenses_series NOT IN @gt_expenses_series
*        AND belnr = @lt_error_data-belnr
*        AND buzei = @lt_error_data-buzei.          "#EC CI_NO_TRANSFORM
*  ENDIF.
*
**** Remove Duplicates
*  SORT lt_data BY sap_unique_id.
*  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING sap_unique_id.
*
**** Fill the Global Data Table
*  MOVE-CORRESPONDING lt_data TO gt_data.
*  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
**** Convert the Project Defenition to External Format
*    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
*      EXPORTING
*        input  = <fs_data>-project_definition_nbr
*      IMPORTING
*        output = <fs_data>-project_definition_nbr.
*    CONCATENATE <fs_data>-project_definition_nbr gc_01 INTO <fs_data>-project_definition_nbr.
*
**** Remove blank space
*    CONDENSE <fs_data>-value_cocur_amt.
*  ENDLOOP.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form F_SEND_DATA
**&---------------------------------------------------------------------*
**& Send the data
**&---------------------------------------------------------------------*
*FORM f_send_data.
*
**** Type Declaration
*  TYPES: BEGIN OF ty_meta,
*           count     TYPE char10,
*           offset    TYPE char10,
*           remaining TYPE char10,
*         END OF ty_meta.
*
*  TYPES: BEGIN OF ty_eventbody,
*           meta TYPE ty_meta,
*           data TYPE ty_chargeimport OCCURS 0,
*         END OF ty_eventbody.
*
*  TYPES : BEGIN OF ty_payload,
*            eventheader TYPE zstut_mule_pub_header,
*            eventbody   TYPE ty_eventbody,
*          END OF ty_payload.
*
**** Internal Table Declaration
*  DATA: lt_header  TYPE tihttpnvp,
*        lt_mapping TYPE zttut_name_mapping,
*        lt_message TYPE zttut_message.
*
**** Structure Declaration
*  DATA: ls_payload TYPE ty_payload.
*
**** Set Header Parameters
*  PERFORM f_set_header_param CHANGING lt_header.
*
**** Set Mapping Table
*  PERFORM f_set_mapping_tab CHANGING lt_mapping.
*
**** Add data to Payload structure
*  ls_payload = VALUE #( eventheader-adsk_res_event =
*                          VALUE #(  eventid   = gv_event_id
*                                    channel   = gv_channel
*                                    topic     = gv_topic
*                                    sequence  = '1'
*                                    date_time = sy-datum
*                                    action    = ''
*                                    resource-resource_id = ''
*                                    resource-resource_uri = ''
*                                    test = 'false'
*                                    encrypteventbody = 'false' )
*                        eventbody =
*                          VALUE #( meta = VALUE #( count = '1' offset = '0' remaining = '0' )
*                                   data = gt_data ) ).
*
**** Call the Utility Method to call the Rest API
*  CALL METHOD go_utility->call_rest_api
*    EXPORTING
*      i_destination = gv_rfc_dest
*      i_url         = gv_path_uri
*      i_in_table    = ls_payload
*      i_in_mapping  = lt_mapping
*      i_in_headers  = lt_header
*      i_post        = abap_true
*    IMPORTING
*      e_message     = lt_message.
*
*  LOOP AT lt_message INTO DATA(lw_message) WHERE type CA gc_aex.
*    gv_error_flag = gc_x.
*    IF gv_message IS INITIAL.
*      gv_message = lw_message-response.
*    ELSE.
*      CONCATENATE gv_message lw_message-response
*        INTO gv_message
*        SEPARATED BY space.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form F_SET_HEADER_PARAM
**&---------------------------------------------------------------------*
**& Set Header Parameters
**&---------------------------------------------------------------------*
*FORM f_set_header_param CHANGING ct_header TYPE tihttpnvp.
*
**** Get the API Key and API Secret Key for the Interface ID
*  CALL METHOD go_utility->get_apikey
*    EXPORTING
*      i_interface_id  = gv_interface_id
*    IMPORTING
*      e_client_id     = DATA(lv_client_id)
*      e_client_secret = DATA(lv_client_secret).
*
*  gv_api_key = lv_client_id.
*  gv_api_secret = lv_client_secret.
*
**** Add header for HTTP Request
*  ct_header = VALUE #( ( name = 'Content-Type' value = 'application/json' )
*                       ( name = 'x-B3-TraceId' value = p_rundt )
*                       ( name = 'x-client-service-moniker' value = gv_srv_moniker )
*                       ( name = 'x-B3-SpanId' value = gv_span_id )
*                       ( name = 'x-B3-ParentSpanId' value = gv_pspan_id )
*                       ( name = 'x-api-key' value = gv_api_key )
*                       ( name = 'x-api-secret' value = gv_api_secret )
*                      )   ##NO_TEXT .
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form F_SET_MAPPING_TAB
**&---------------------------------------------------------------------*
**& Set Mapping Table
**&---------------------------------------------------------------------*
*FORM f_set_mapping_tab CHANGING ct_mapping TYPE zttut_name_mapping.
*
**** Add the mapping table
*  go_utility->get_mule_pub_sub_api_header( CHANGING i_in_mapping = ct_mapping ).    "Mule Pub Sub Mapping
*  ct_mapping =  VALUE #( BASE ct_mapping
*                              ( abap = 'META' json = 'meta' )
*                              ( abap = 'COUNT' json = 'count' )
*                              ( abap = 'OFFSET' json = 'offset' )
*                              ( abap = 'REMAINING' json = 'remaining' )
*                              ( abap = 'DATA' json = 'data' )
*                              ( abap = 'PROJECT_DEFINITION_NBR' json = 'projectDefinitionNumber' )
*                              ( abap = 'CO_OBJECT_NM' json = 'coObjectNm' )
*                              ( abap = 'DOCUMENT_DT' json = 'documentDate' )
*                              ( abap = 'VALUE_COCUR_AMT' json = 'valueCocurAmount' )
*                              ( abap = 'COCUR_TYPE' json = 'cocurType' )
*                              ( abap = 'BILL_NM' json = 'billName' )
*                              ( abap = 'COST_DESC' json = 'costDesc' )
*                              ( abap = 'VENDOR_NM' json = 'vendorNm' )
*                              ( abap = 'PO_NBR' json = 'poNbr' )
*                              ( abap = 'COST_DESC_DETAIL_NM' json = 'costDescDetailNm' )
*                              ( abap = 'SAP_UNIQUE_ID' json = 'sapUniqueId' ) ).
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form F_SET_LAST_RUN_DATE
**&---------------------------------------------------------------------*
**& Update Last Run Date
**&---------------------------------------------------------------------*
*FORM f_set_last_run_date.
*
**** Data Decclaration
*  DATA: ls_tvarvc TYPE tvarvc.
*
**** Set the Last Run Date
*  ls_tvarvc-name  = 'P_LAST_RUN_DATE_TENROX'.
*  ls_tvarvc-type  = 'P'.
*  ls_tvarvc-numb  = 0000.
*  ls_tvarvc-low   = p_rundt.
*
**** Call Enqueue FM for Table locking
*  CALL FUNCTION 'ENQUEUE_ESVARVC'
*    EXPORTING
*      mode_tvarvc    = 'E'
*      mandt          = sy-mandt
*      name           = 'P_LAST_RUN_DATE_TENROX'
*      type           = 'P'
*      numb           = 0000
*      _wait          = 'X'
*    EXCEPTIONS
*      foreign_lock   = 1
*      system_failure = 2
*      OTHERS         = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
**** Modify the TVARVC Table
*    MODIFY tvarvc FROM ls_tvarvc.
*    CLEAR ls_tvarvc.
*  ENDIF.
*
**** Call Dequeue FM for Table lock release
*  CALL FUNCTION 'DEQUEUE_E_LOCK_TVARVC'
*    EXPORTING
*      mode_tvarvc = 'E'
*      mandt       = sy-mandt.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form F_UPD_LOG_TABLE
**&---------------------------------------------------------------------*
**& Update Tenrox Log Table
**&---------------------------------------------------------------------*
*FORM f_upd_log_table.
*
**** Data Declaration
*  DATA: lt_ztfi_tenrox_log TYPE STANDARD TABLE OF ztfi_tenrox_log.
*
**** Get the data sent
*  lt_ztfi_tenrox_log = CORRESPONDING #( gt_data ).
*
**** Set Run Date, Process Flag and Clear Error Message
*  LOOP AT lt_ztfi_tenrox_log ASSIGNING FIELD-SYMBOL(<fs_ztfi_tenrox_log>).
*    <fs_ztfi_tenrox_log>-run_date = p_rundt.
*    <fs_ztfi_tenrox_log>-process_flag = 'N'.
*    <fs_ztfi_tenrox_log>-error_msg = ''.
*  ENDLOOP.
*
**** Call Enqueue FM for Table locking
*  CALL FUNCTION 'ENQUEUE_E_TABLE'
*    EXPORTING
*      tabname        = 'ZTFI_TENROX_LOG'
*      _wait          = 'X'
*    EXCEPTIONS
*      foreign_lock   = 1
*      system_failure = 2
*      OTHERS         = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
**** Modify the Error Log Table
*    MODIFY ztfi_tenrox_log FROM TABLE lt_ztfi_tenrox_log.
*    REFRESH lt_ztfi_tenrox_log.
*  ENDIF.
*
**** Call Dequeue FM for Table lock release
*  CALL FUNCTION 'DEQUEUE_E_TABLE'
*    EXPORTING
*      tabname = 'ZTFI_TENROX_LOG'.
*
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form F_DSIPLAY_OUTPUT
**&---------------------------------------------------------------------*
**& Display Output
**&---------------------------------------------------------------------*
*FORM f_display_output.
*
*  IF gt_data IS NOT INITIAL.
*    go_utility->display_alv( CHANGING c_datatab = gt_data ).
*  ELSE.
*    MESSAGE s033 DISPLAY LIKE gc_e.   "No data found for the entered selections.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form F_CLEAR_GLOBAL_VAR
**&---------------------------------------------------------------------*
**& Clear Global Variable
**&---------------------------------------------------------------------*
*FORM f_clear_global_var.
*
**** Clear global data
*  CLEAR:  gt_data,
*          gv_error_flag,
*          gv_message.
*
*ENDFORM.
