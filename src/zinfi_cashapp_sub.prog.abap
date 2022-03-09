**&---------------------------------------------------------------------*
**& Include          ZINFI_CASHAPP_SUB
**&---------------------------------------------------------------------*
**-----------------------------------------------------------------------------------------------------------------------------------------*
**                                                          MODIFICATION HISTORY                                                           |
**-----------------------------------------------------------------------------------------------------------------------------------------*
** Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
**-----------------------------------------------------------------------------------------------------------------------------------------*
** 09-APR-2021 | 486861      | CF.ENH.762     | DFDK903594   | Clear On Account AR Postings against Open Invoice                           |
** 09-SEP-2021 | 485639      | CF.ENH.762     | DFDK913063   | Defect fix# INC3730087/CFHYP-103                                            |
** 01-OCT-2021 | 486861      | CF.ENH.762     | DFDK913579 / | Defect fix# INC3781883 / CFHYP-245                                          |
**                                              DFDK913630                                                                                 |
** 11-OCT-2021 | 486861      | CF.ENH.762D    | DFDK913815   | CF.ENH.762D - INC3765130 - Validations for Manual Entry Fields              |
** 18-OCT-2021 | 486861      | CF.ENH.762E    | DFDK913942   | CF.ENH.762E -OKB9 - FICA to FIGL Transfer (Cost Center)                     |
** 21-OCT-2021 | 486861      | CF.ENH.762C    | DFDK914060   | CF.ENH.762c: Add statistical key field to Cash App / Defect# 312,331        |
** 02-NOV-2021 | 488468      | TACME-1270     | DFDK914302   | R1 backlog Enhancements to Cash App                                         |
**--Remove Sales Order and Internal Order Columns from GL Items ALV                                                                        |
**-----------------------------------------------------------------------------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**& Form f_fill_onact_data
**&---------------------------------------------------------------------*
**& Form to get On-Act open data
**&---------------------------------------------------------------------*
*FORM f_fill_onact_data .
*
****Local data declaration
*  DATA: "BAPI parameter tables
*    lt_mainselections TYPE STANDARD TABLE OF  bapifkkopselhead,
*    lt_openitems      TYPE STANDARD TABLE OF  bapidfkkcl,
*    lt_return         TYPE bapiret2,
*    lt_valuesel       TYPE STANDARD TABLE OF bapifkkopseltxt1,
*    lt_amountsel      TYPE STANDARD TABLE OF bapifkkopselnum,
*    "variables for output table
*    lw_act            TYPE ty_onact,
*    lv_selnum         TYPE selnr_kk VALUE 0.
*
****Constants for data filtering - will be moved to TVARVC
*  CONSTANTS : lc_comp   TYPE selfn_kk VALUE 'COMP_CODE',
*              lc_amt_cl TYPE lvc_fname VALUE 'AMOUNT_CL'.
*
*
*  lv_selnum = lv_selnum + 1.
*  IF p_conact IS NOT INITIAL OR p_buspar IS NOT INITIAL.
****Map the item values to BAPI Item table
*    DATA(lw_mainsel) = VALUE bapifkkopselhead( selection_number = lv_selnum
*                                               buspartner = p_buspar
*                                               cont_acct = p_conact ).
*    APPEND lw_mainsel TO lt_mainselections.
*  ENDIF.
*
****Fill the Value selection table for company code
*  IF p_bukrs IS NOT INITIAL.
*    DATA(lw_valuesel) = VALUE bapifkkopseltxt1( selection_number = lv_selnum
*                                            field_name = lc_comp
*                                            from_value = p_bukrs ).
*    APPEND lw_valuesel TO lt_valuesel .
*  ENDIF.
*
****Call BAPI to get open items
*  PERFORM f_call_bapi USING     lt_mainselections
*                                lt_valuesel
*                                lt_amountsel
*                      CHANGING  lt_openitems
*                                lt_return.
*
*
****Sort open item table ascending by main transaction,
****subtransactiona nd document type
*  SORT  lt_openitems BY main_trans doc_type.
****Read the first document fulfilling condition main trans = 0060,
****document type = DZ
*  READ TABLE lt_openitems INTO DATA(lw_openitems) WITH KEY main_trans = gv_maintrs
*                                                           doc_type = gv_type BINARY SEARCH.
*  IF sy-subrc = 0.
*    DATA(lv_index) = sy-tabix.
*    CLEAR : lw_openitems,gv_augbt_act_cl,gv_augbt_act_ps.
*    LOOP AT lt_openitems INTO lw_openitems FROM lv_index.
****Filter open item table where main trans = 0060 and document type = DZ
*      IF lw_openitems-main_trans = gv_maintrs AND lw_openitems-doc_type = gv_type.
*        MOVE-CORRESPONDING lw_openitems TO lw_act.
****Fill on-act total variable
*        gv_augbt_act_cl = gv_augbt_act_cl + lw_openitems-amount_clear.
*        lw_act-amount_op = lw_openitems-amount.
*        lw_act-amount_cl = lw_openitems-amount_clear.
*        APPEND lw_act TO gt_act.
*      ELSE.
*        EXIT.
*      ENDIF.
*      CLEAR : lw_act,lw_openitems.
*    ENDLOOP.
*    gv_augbt_act_ps = gv_augbt_act_cl * -1.
*  ENDIF.
*
*  CLEAR : lv_index.
****Make gross clearing field uneditable initially.
*  LOOP AT gt_act ASSIGNING FIELD-SYMBOL(<fs_act>).
*    APPEND VALUE #( fieldname = lc_amt_cl
*                    style = cl_gui_alv_grid=>mc_style_disabled )
*                TO <fs_act>-celltab.
*  ENDLOOP.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_fill_openinvoice_data
**&---------------------------------------------------------------------*
**& Form to get open invoice data
**&---------------------------------------------------------------------*
*FORM f_fill_openinvoice_data.
*
*  DATA :
****BAPI item table declaration
*    lt_mainselections TYPE STANDARD TABLE OF  bapifkkopselhead,
*    lt_openitems      TYPE STANDARD TABLE OF  bapidfkkcl,
*    lt_openitems_inv  TYPE STANDARD TABLE OF  bapidfkkcl,
*    lt_return         TYPE bapiret2,
*    lt_bussel         TYPE STANDARD TABLE OF  bapifkkopselhead,
*    lt_consel         TYPE STANDARD TABLE OF  bapifkkopselhead,
*    lt_valuesel       TYPE STANDARD TABLE OF bapifkkopseltxt1,
*    lt_amountsel      TYPE STANDARD TABLE OF bapifkkopselnum,
*    lw_inv            TYPE ty_oninv,
*    lv_selnum         TYPE selnr_kk VALUE 0.
****Constants for data filtering - will be moved to TVARVC
*  CONSTANTS : lc_amount TYPE selfn_kk    VALUE 'AMOUNT',
*              lc_amt_cl TYPE lvc_fname   VALUE 'AMOUNT_CL',
** Begin of change - 486861 Date: 09/14/2021 Defect # INC3730087/CFHYP-103
*              lc_comp   TYPE selfn_kk VALUE 'COMP_CODE'.
*  DATA : lt_openitems_fursel      TYPE STANDARD TABLE OF  bapidfkkcl.
** End of change - 486861 Date: 09/14/2021 Defect # INC3730087/CFHYP-103
*
****Map the business partner values to BAPI Item table
*  lv_selnum = lv_selnum + 1.
*  IF p_buspar IS NOT INITIAL.
****Append business partner fron on-Act section
*    DATA(lw_buspar) = VALUE bapifkkopselhead( selection_number = lv_selnum
*                                       buspartner = p_buspar ).
*
*    APPEND lw_buspar TO lt_bussel .
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*  ELSE.
*    CLEAR : lv_selnum.
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*  ENDIF.
*
** Begin of change - 486861 Date: 09/14/2021 Defect # INC3730087/CFHYP-103
*  lt_mainselections = lt_bussel.
****Fill contract account table for main selection for "BAPI_CTRACCONTRACTACCOUNT_GOI1"
*  lv_selnum = lv_selnum + 1.
*  IF p_conact IS NOT INITIAL.
****Append contract account fron on-Act section
*    DATA(lw_conact) = VALUE bapifkkopselhead( selection_number = lv_selnum
*                                       cont_acct = p_conact ) .
*
*    APPEND lw_conact TO lt_consel.
*  ENDIF.
*  APPEND LINES OF lt_consel TO lt_mainselections.
*
****Fill the Value selection table for company code
*  IF p_bukrs IS NOT INITIAL.
*    CLEAR : lv_selnum.
*    lv_selnum = lv_selnum + 1.
*    DATA(lw_valuesel) = VALUE bapifkkopseltxt1( selection_number = lv_selnum
*                                            field_name = lc_comp
*                                            from_value = p_bukrs ).
*    APPEND lw_valuesel TO lt_valuesel .
*  ENDIF.
*
****Call BAPI to get open items for General selection
*  PERFORM f_call_bapi USING     lt_mainselections
*                                lt_valuesel
*                                lt_amountsel
*                      CHANGING  lt_openitems
*                                lt_return.
****Processing for selection parameters in further selection
****  Clear all internal table
*  CLEAR : lt_bussel,lt_consel,lt_mainselections,
*          lt_valuesel,lv_selnum.
*
*  IF s_buspar IS NOT INITIAL.
****Fill business partner table for main selection for "BAPI_CTRACCONTRACTACCOUNT_GOI1"
*    LOOP AT s_buspar INTO DATA(lw_buspar_opt).
*      lv_selnum = lv_selnum + 1.
*      lw_buspar = VALUE bapifkkopselhead( selection_number = lv_selnum
*                                                buspartner = lw_buspar_opt-low ).
*
*      APPEND lw_buspar TO lt_bussel .
*      CLEAR : lw_buspar,lw_buspar_opt.
*    ENDLOOP.
*  ENDIF.
****Copy the BPs to main selection
*  lt_mainselections = lt_bussel.
*
****Fill contract account table for main selection for "BAPI_CTRACCONTRACTACCOUNT_GOI1"
*  IF s_conact IS NOT INITIAL.
*    LOOP AT s_conact INTO DATA(lw_conact_opt).
*      lv_selnum = lv_selnum + 1.
*      lw_conact = VALUE bapifkkopselhead( selection_number = lv_selnum
*                                                cont_acct = lw_conact_opt-low ) .
*      APPEND lw_conact TO lt_consel.
*      CLEAR: lw_conact,lw_conact_opt.
*    ENDLOOP.
*  ENDIF.
****Copy the contract acounts to main selection
*  APPEND LINES OF lt_consel TO lt_mainselections.
*
****Fill the Value selection table for company code
*  IF s_comp IS NOT INITIAL.
*    CLEAR : lv_selnum.
*    LOOP AT s_comp INTO DATA(lw_bukrs_opt).
*      lv_selnum = lv_selnum + 1.
*      DATA(lw_valuesel_fursel) = VALUE bapifkkopseltxt1( selection_number = lv_selnum
*                                              field_name = lc_comp
*                                              from_value = lw_bukrs_opt-low ).
*      APPEND lw_valuesel_fursel TO lt_valuesel .
*      CLEAR:lw_valuesel_fursel.
*    ENDLOOP.
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*    DATA: lr_bukrs TYPE RANGE OF bukrs.
****    Copy general selection comp code
*    lr_bukrs = VALUE #(  ( sign   = 'I'
*                         option = 'EQ'
*                         low    = p_bukrs ) ).
*    APPEND LINES OF lr_bukrs TO s_comp[].
** End of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
*
*  ENDIF.
*
****Map the amount filter values to BAPI Item table
*  IF p_amount IS NOT INITIAL.
*    CLEAR : lv_selnum.
*    lv_selnum = lv_selnum + 1.
****Fill amount selection table for "BAPI_CTRACCONTRACTACCOUNT_GOI1"
*    DATA(lw_amountsel) = VALUE bapifkkopselnum( selection_number = lv_selnum
*                                                field_name = lc_amount
*                                                from_value = p_amount
*                                                currency = p_curr
*                                                currency_iso = p_curr ) .
*
*    APPEND lw_amountsel TO lt_amountsel.
*    CLEAR : lw_amountsel.
*  ENDIF.
****To avoid error "Formally incorrect selection table transferred" in BAPI
*  DATA(lv_maintrans) = lines( lt_mainselections ).
*  DATA(lv_valuetab)  = lines( lt_valuesel ) .
*  CLEAR : lt_bussel.
*
*  IF lv_maintrans < lv_valuetab.
*    lv_selnum = lv_maintrans.
*    READ TABLE lt_mainselections INTO DATA(lw_main) INDEX 1.
*    IF sy-subrc = 0.
*
*    ENDIF.
*    DO ( lv_valuetab - lv_maintrans ) TIMES.
*      lv_selnum = lv_selnum + 1.
*      lw_buspar = VALUE bapifkkopselhead( selection_number = lv_selnum
*                                                buspartner = lw_main-buspartner ).
*
*      APPEND lw_buspar TO lt_bussel .
*      CLEAR : lw_buspar.
*    ENDDO.
*    APPEND LINES OF lt_bussel TO lt_mainselections.
*  ENDIF.
*  IF lt_mainselections IS NOT INITIAL OR lt_valuesel IS NOT INITIAL.
****Call BAPI to get open items for further selection parameters
*    PERFORM f_call_bapi USING     lt_mainselections
*                                  lt_valuesel
*                                  lt_amountsel
*                        CHANGING  lt_openitems_fursel
*                                  lt_return.
*    IF lt_openitems_fursel IS NOT INITIAL.
*      APPEND LINES OF lt_openitems_fursel TO lt_openitems.
*      SORT lt_openitems.
*      DELETE ADJACENT DUPLICATES FROM lt_openitems COMPARING ALL FIELDS.
*    ENDIF.
*  ENDIF.
*
** End of change - 486861 Date: 09/14/2021 Defect # INC3730087/CFHYP-103
****Sort open item table
*  SORT  lt_openitems BY main_trans doc_type.
*
****Filter open item table where main trans NE 0060 and document type NE DZ
*  LOOP AT lt_openitems INTO DATA(lw_openitems).
** Begin of change - 485639 Date: 09/14/2021 Defect # INC3730087/CFHYP-103
**    IF lw_openitems-main_trans <> gv_maintrs AND  lw_openitems-doc_type <> gv_type.
*    IF ( lw_openitems-main_trans = gv_maintrs ) AND ( lw_openitems-doc_type = gv_type ).
*      CONTINUE.
*    ELSE.
** End of change - 485639 Date: 09/14/2021 Defect # INC3730087/CFHYP-103
*      APPEND lw_openitems TO lt_openitems_inv.
*    ENDIF.
*  ENDLOOP.
*
*  REFRESH gt_excluded[].
*  IF lt_openitems_inv IS NOT INITIAL.
*
****Fetch PO number "ZZUONR" from table DFKKOP
*    SELECT  opbel,
*            opupw,
*            opupk,
*            opupz,
*            bukrs,
*            gpart,
*            vkont,
*            betrw,
*            augbt,
*            xblnr,
*            zzuonr
*          FROM dfkkop
*          FOR ALL ENTRIES IN @lt_openitems_inv
*          WHERE opbel = @lt_openitems_inv-doc_no
*            AND opupw = @lt_openitems_inv-rep_item
*            AND opupk = @lt_openitems_inv-item
*            AND opupz = @lt_openitems_inv-sub_item
*            AND bukrs    IN   @s_comp
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
**            AND opbel    IN   @s_fidoc
**            AND xblnr    IN   @s_inv
**            AND zzuonr   IN   @s_ponum
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
*          INTO TABLE @DATA(lt_openinv).
*    IF sy-subrc = 0.
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
**** Filter based on FICA documents
*      IF s_fidoc IS NOT INITIAL.
*        SORT lt_openinv BY opbel.
*        SORT s_fidoc BY low.
*        DELETE ADJACENT DUPLICATES FROM s_fidoc COMPARING low.
*        SORT s_fidoc BY low.
*        DATA(lt_openinv_temp) = lt_openinv.
*        CLEAR lt_openinv_temp.
****   Logic to delete lt_openinv for performance improvement.
*        LOOP AT s_fidoc INTO DATA(lw_fidoc).
*          READ TABLE lt_openinv TRANSPORTING NO FIELDS WITH KEY opbel = lw_fidoc-low BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            DATA(lv_tabix) = sy-tabix.
*            LOOP AT lt_openinv ASSIGNING FIELD-SYMBOL(<fs_openinv>) FROM lv_tabix.
*              IF <fs_openinv>-opbel EQ lw_fidoc-low.
*                APPEND <fs_openinv> TO lt_openinv_temp.
*              ELSE.
*                EXIT.
*              ENDIF.
*            ENDLOOP.
**--->Start of changes DFDK914302
*          ELSE.
*           clear lwa_excluded.
*           lwa_excluded-opbel = lw_fidoc-low.
*           APPEND lwa_excluded to gt_excluded.
**<---End of changes DFDK914302
*          ENDIF.
*          CLEAR : lv_tabix.
*        ENDLOOP.
*        DELETE lt_openinv WHERE opbel IS NOT INITIAL.
*        lt_openinv = lt_openinv_temp.
*        CLEAR:lt_openinv_temp.
*      ENDIF.
**** Filter based on XBLNR/CI Inv number
*      IF s_inv IS NOT INITIAL.
*        SORT lt_openinv BY xblnr.
*        SORT s_inv BY low.
*        DELETE ADJACENT DUPLICATES FROM s_inv COMPARING low.
*        SORT s_inv BY low.
*        lt_openinv_temp = lt_openinv.
*        CLEAR lt_openinv_temp.
****   Logic to delete lt_openinv for performance improvement.
*        LOOP AT s_inv INTO DATA(lw_inv_so).
*          READ TABLE lt_openinv TRANSPORTING NO FIELDS WITH KEY xblnr = lw_inv_so-low BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            lv_tabix = sy-tabix.
*            LOOP AT lt_openinv ASSIGNING <fs_openinv> FROM lv_tabix.
*              IF <fs_openinv>-xblnr EQ lw_inv_so-low.
*                APPEND <fs_openinv> TO lt_openinv_temp.
*              ELSE.
*                EXIT.
*              ENDIF.
*            ENDLOOP.
**--->Start of changes DFDK914302
*          ELSE.
*           clear lwa_excluded.
*           lwa_excluded-xblnr = lw_inv_so-low.
*           APPEND lwa_excluded to gt_excluded.
**<---End of changes DFDK914302
*          ENDIF.
*          CLEAR : lv_tabix.
*        ENDLOOP.
*        DELETE lt_openinv WHERE opbel IS NOT INITIAL.
*        lt_openinv = lt_openinv_temp.
*        CLEAR:lt_openinv_temp.
*      ENDIF.
**** Filter based on ZZUONR/PO number
*      IF s_ponum IS NOT INITIAL.
*        SORT lt_openinv BY zzuonr.
*        SORT s_ponum BY low.
*        DELETE ADJACENT DUPLICATES FROM s_ponum COMPARING low.
*        SORT s_ponum BY low.
*        lt_openinv_temp = lt_openinv.
*        CLEAR lt_openinv_temp.
****   Logic to delete lt_openinv for performance improvement.
*        LOOP AT s_ponum INTO DATA(lw_ponum).
*          READ TABLE lt_openinv TRANSPORTING NO FIELDS WITH KEY zzuonr = lw_ponum-low BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            lv_tabix = sy-tabix.
*            LOOP AT lt_openinv ASSIGNING <fs_openinv> FROM lv_tabix.
*              IF <fs_openinv>-zzuonr EQ lw_ponum-low.
*                APPEND <fs_openinv> TO lt_openinv_temp.
*              ELSE.
*                EXIT.
*              ENDIF.
*            ENDLOOP.
**--->Start of changes DFDK914302
*          ELSE.
*           clear lwa_excluded.
*           lwa_excluded-zzuonr = lw_ponum-low.
*           APPEND lwa_excluded to gt_excluded.
**<---End of changes DFDK914302
*          ENDIF.
*          CLEAR : lv_tabix.
*        ENDLOOP.
*        DELETE lt_openinv WHERE opbel IS NOT INITIAL.
*        lt_openinv = lt_openinv_temp.
*        CLEAR:lt_openinv_temp.
*      ENDIF.
*
****Check if invoice table is blank
*      IF lt_openinv IS NOT INITIAL.
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
*
*        CLEAR : lw_openitems,gv_augbt_inv_cl,gv_augbt_inv_ps.
****Sort open item tables by document number and item
*        SORT lt_openinv BY opbel opupw opupk opupz.
*        SORT lt_openitems_inv BY doc_no rep_item item sub_item.
*
****Update open invoice table  with pO number (ZZUNOR)
*        LOOP AT lt_openinv INTO DATA(lw_open) .
*          READ TABLE lt_openitems_inv INTO lw_openitems WITH KEY  doc_no = lw_open-opbel
*                                                                  rep_item = lw_open-opupw
*                                                                  item = lw_open-opupk
*                                                                  sub_item = lw_open-opupz BINARY SEARCH.
*          IF sy-subrc = 0.
*            MOVE-CORRESPONDING lw_openitems TO lw_inv.
*            lw_inv-zzuonr = lw_open-zzuonr.
*            lw_inv-amount_op = lw_openitems-amount.
*            lw_inv-amount_cl = lw_openitems-amount_clear.
****Fill final total table for open invoice items
*            gv_augbt_inv_cl = gv_augbt_inv_cl + lw_openitems-amount_clear.
*
*            APPEND lw_inv TO gt_inv.
*          ENDIF.
*          CLEAR : lw_open,lw_openitems.
*        ENDLOOP.
*        gv_augbt_inv_ps = gv_augbt_inv_cl * -1.
****Initially make the gross clearing field uneditable
*        LOOP AT gt_inv ASSIGNING FIELD-SYMBOL(<fs_inv>).
*          APPEND VALUE #( fieldname = lc_amt_cl
*                  style = cl_gui_alv_grid=>mc_style_disabled )
*              TO <fs_inv>-celltab.
*        ENDLOOP.
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
*      ENDIF.
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
*    ENDIF.
*
*  ENDIF.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_call_bapi
**&---------------------------------------------------------------------*
**& Form to get open items based on business partner/contract account
**&---------------------------------------------------------------------*
*FORM f_call_bapi  USING     it_mainselections TYPE tt_mainselections
*                            it_valuesel TYPE tt_valuesel
*                            it_amountsel TYPE tt_amountsel
*                  CHANGING  it_openitems TYPE tt_openitems
*                            cs_return TYPE bapiret2.
*
****Data declaration for BAPI header
*  DATA : lv_clearingcurrency    TYPE  blwae_kk,
*         lv_clearingcurrencyiso TYPE  isocd,
*         lv_clearingdate        TYPE  bldat.
*
*  CONSTANTS : lc_enque TYPE bapiflag VALUE 'X'.
****Map the selection screen value to BAPI header variable
*  lv_clearingcurrency = p_curr.
*  lv_clearingcurrencyiso = p_curr.
*  lv_clearingdate = p_postdt.
*
*
****Call BAPI to get open items
*  CALL FUNCTION 'BAPI_CTRACCONTRACTACCOUNT_GOI1'
*    EXPORTING
*      clearingcurrency    = lv_clearingcurrency
*      clearingcurrencyiso = lv_clearingcurrencyiso
*      clearingdate        = lv_clearingdate
*      enqueue             = lc_enque
*    IMPORTING
*      return              = cs_return
*    TABLES
*      mainselections      = it_mainselections
*      openitems           = it_openitems
*      valueselections     = it_valuesel
*      amountselections    = it_amountsel.
** Begin of change - 485639 Date: 09/14/2021 Defect # INC3730087/CFHYP-103
*  IF cs_return-type = 'E'.
*    MESSAGE ID cs_return-id TYPE cs_return-type NUMBER cs_return-number
*     WITH cs_return-message_v1
*          cs_return-message_v2
*          cs_return-message_v3
*          cs_return-message_v4
*          INTO DATA(lv_message).
*    MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
** End of change - 485639 Date: 09/14/2021 Defect # INC3730087/CFHYP-103
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_display_result
**&---------------------------------------------------------------------*
**& Form to call screen 9000 for ALV display
**&---------------------------------------------------------------------*
*FORM f_display_result .
*  "Call screen for ALV output
*  CALL SCREEN 9000.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Module STATUS_9000 OUTPUT
**&---------------------------------------------------------------------*
**& PBO Module for Screen 9000
**&---------------------------------------------------------------------*
*MODULE status_9000 OUTPUT.
****Call custom PF status for screen 9000
*  SET PF-STATUS 'ZSTANDARD'.
*  SET TITLEBAR 'ZFI_CASHAPP_TITLE'.
*
*  CONSTANTS : lc_cont_onact TYPE char7 VALUE 'ACT_OPN',
*              lc_cont_oninv TYPE char7 VALUE 'INV_OPN',
*              lc_cont_bp    TYPE char7 VALUE 'BP_ITEM',
*              lc_cont_gl    TYPE char7 VALUE 'GL_ITEM',
*              lc_scr_onact  TYPE sy-dynnr VALUE '9001',
*              lc_scr_oninv  TYPE sy-dynnr VALUE '9002',
*              lc_scr_bp     TYPE sy-dynnr VALUE '9003',
*              lc_scr_gl     TYPE sy-dynnr VALUE '9004'.
*
****Currency for global currency fields
*  gv_waers_act_cl = p_curr.
*  gv_waers_inv_cl = p_curr.
*  gv_waers_act_ps = p_curr.
*  gv_waers_inv_ps = p_curr.
*  gv_waers_bp = p_curr.
*  gv_waers_gl = p_curr.
*  gv_diff_curr = p_curr.
*
****Make refresh grid as stable
*  gw_stable-row = abap_true.
****Exlude standard ALV tool bar for on-Act and open invoice ALVs
*  PERFORM f_exclude_functions CHANGING gt_exclude.
*
****Make pressed tab(ONACT) as active tab inially after program execution
*  main_tab-activetab = gw_main_tab-pressed_tab.
*
*  CASE gw_main_tab-pressed_tab.
*    WHEN gc_main_tab-tab1.
****Check if there is no custom container in screen 9000
*      IF go_cont_act IS INITIAL.
****Creating object of container
*        CREATE OBJECT go_cont_act
*          EXPORTING
*            container_name = lc_cont_onact.
*
*        gw_main_tab-subscreen = lc_scr_onact.
*****On-Act ALV display
*        PERFORM f_on_account_alv.
*      ENDIF.
*
*
*    WHEN gc_main_tab-tab2.
*      IF go_cont_inv IS INITIAL .
****Creating object of container
*        CREATE OBJECT go_cont_inv
*          EXPORTING
*            container_name = lc_cont_oninv.
*        gw_main_tab-subscreen = lc_scr_oninv.
*****On-invoice ALV display
*        PERFORM f_on_invoice_alv.
*      ENDIF.
*    WHEN gc_main_tab-tab3.
*      IF go_cont_bp IS INITIAL.
****Creating object of container
*        CREATE OBJECT go_cont_bp
*          EXPORTING
*            container_name = lc_cont_bp.
*
*        gw_main_tab-subscreen = lc_scr_bp.
*****BP items ALV display
*        PERFORM f_bpitem_alv.
*      ENDIF.
*    WHEN gc_main_tab-tab4.
*      IF go_cont_gl IS INITIAL.
****Creating object of container
*        CREATE OBJECT go_cont_gl
*          EXPORTING
*            container_name = lc_cont_gl.
*
*        gw_main_tab-subscreen = lc_scr_gl.
*****GL Items ALV display
*        PERFORM f_glitem_alv.
*      ENDIF.
*  ENDCASE.
*
*ENDMODULE.
**&---------------------------------------------------------------------*
**&      Module  USER_COMMAND_9000  INPUT
**&---------------------------------------------------------------------*
**      PAI USER command module for screen 9000
**----------------------------------------------------------------------*
*MODULE user_command_9000 INPUT.
*
****logic to process foron screen buttons
*  CASE sy-ucomm.
**--->Start of change DFDK914302
*    WHEN 'ZEXCL_INV'.
*
**<---End of change DFDK914302
*
****When user clicked BACK/EXIT/CANCEL button on screen
*    WHEN '&F03' OR '&F15' OR '&F12'.
*      SET SCREEN 0.
*      LEAVE SCREEN.
*    WHEN  '&F20'.
*
****Check if data changed and not hit enter before save
*      IF go_alv_act IS BOUND.
*        CALL METHOD go_alv_act->check_changed_data.
*      ENDIF.
*      IF go_alv_inv IS BOUND.
*        CALL METHOD go_alv_inv->check_changed_data.
*      ENDIF.
*      IF go_alv_bp IS BOUND.
*        CALL METHOD go_alv_bp->check_changed_data.
*      ENDIF.
*      IF go_alv_gl IS BOUND.
*        CALL METHOD go_alv_gl->check_changed_data.
*      ENDIF.
****When user clicked SAVE button
*      PERFORM f_save_function.
*  ENDCASE.
*
*ENDMODULE.
**&---------------------------------------------------------------------*
**&      Module  MAIN_TAB_ACTIVE_TAB_GET  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE main_tab_active_tab_get INPUT.
*
****Event to open respective tab after clicking on tab
****Update the ALV values over tab change event
*  CASE gw_main_tab-pressed_tab.
*    WHEN gc_main_tab-tab1.
*      CALL METHOD go_alv_act->check_changed_data.
*    WHEN gc_main_tab-tab2.
*      CALL METHOD go_alv_inv->check_changed_data.
*    WHEN gc_main_tab-tab3.
*      CALL METHOD go_alv_bp->check_changed_data.
*    WHEN gc_main_tab-tab4.
*      CALL METHOD go_alv_gl->check_changed_data.
*  ENDCASE.
*
****Act on tab clik on ALV
*  CASE sy-ucomm.
*    WHEN gc_main_tab-tab1.
*      gw_main_tab-pressed_tab = gc_main_tab-tab1.
*      gw_main_tab-subscreen = lc_scr_onact.
*    WHEN gc_main_tab-tab2.
*      gw_main_tab-pressed_tab = gc_main_tab-tab2.
*      gw_main_tab-subscreen = lc_scr_oninv.
*    WHEN gc_main_tab-tab3.
*      gw_main_tab-pressed_tab = gc_main_tab-tab3.
*      gw_main_tab-subscreen = lc_scr_bp.
*    WHEN gc_main_tab-tab4.
*      gw_main_tab-pressed_tab = gc_main_tab-tab4.
*      gw_main_tab-subscreen = lc_scr_gl.
*  ENDCASE.
*
*ENDMODULE.
**&---------------------------------------------------------------------*
**& Form f_alv_layout
**&---------------------------------------------------------------------*
**& Form to set ALV layout
**&---------------------------------------------------------------------*
*FORM f_alv_layout USING    iv_check
*                  CHANGING cs_layout TYPE lvc_s_layo.
*
*  "constants for identification
*  CONSTANTS : lc_act     TYPE char2 VALUE 'AC',
*              lc_inv     TYPE char2 VALUE 'IN',
*              lc_bp      TYPE char2 VALUE 'BP',
*              lc_gl      TYPE char2 VALUE 'GL',
*              lc_selmode TYPE char1 VALUE 'A'.
*
****Check if layout for onaccount alv
*  IF iv_check = lc_act.
****For alv grid heading text
*    DATA(lv_len) = lines( gt_act ).
*    DATA(lv_str) = |{ lv_len } { TEXT-058 } { p_curr } ) |.
*    cs_layout-grid_title = |{ TEXT-059 } { lv_str } )|.
****Hide standard ALV selection row
*    cs_layout-no_rowmark = abap_true.
*    "to make gross clearing cell editable based on checkbox
*    cs_layout-stylefname = TEXT-063.
****Check if layout for open invoice ALV
*  ELSEIF iv_check = lc_inv.
*    CLEAR : lv_len,lv_str.
*    lv_len = lines( gt_inv ).
*    lv_str = |{ lv_len } { TEXT-058 } { p_curr } ) |.
*    cs_layout-grid_title = |{ TEXT-060 } { lv_str } )|.
*    cs_layout-no_rowmark = abap_true.
*    "to make gross clearing cell editable based on checkbox
*    cs_layout-stylefname = TEXT-063.
****Check if layout for open BP item ALV
*  ELSEIF iv_check = lc_bp .
*    cs_layout-grid_title = |{ TEXT-061 } { p_curr } )|.
*    IF gt_bpitem IS INITIAL.
*      cs_layout-edit = abap_true.
*    ELSE.
*      cs_layout-no_toolbar = abap_true.
*    ENDIF.
****Check if layout for open GL item ALV
*  ELSEIF iv_check = lc_gl .
*    cs_layout-grid_title = |{ TEXT-062 } { p_curr } )|.
*    IF gt_glitem IS INITIAL.
*      cs_layout-edit = abap_true.
*    ELSE.
*      cs_layout-no_toolbar = abap_true.
*    ENDIF.
*  ENDIF.
*
*  cs_layout-smalltitle = abap_true .
*  cs_layout-zebra = abap_true.
*  cs_layout-sel_mode = lc_selmode.
*  cs_layout-cwidth_opt = gc_x.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form alv_fieldcat
**&---------------------------------------------------------------------*
**& Call field catelog form for different ALV
**&---------------------------------------------------------------------*
*FORM f_alv_fieldcat USING iv_check.
*
*  "constants declaration for ALV grid caller identification
*  CONSTANTS : lc_act TYPE char2 VALUE 'AC',
*              lc_inv TYPE char2 VALUE 'IN',
*              lc_bp  TYPE char2 VALUE 'BP',
*              lc_gl  TYPE char2 VALUE 'GL'.
*
** format of field catalog
** type of alv,column pos,field name,field desc,editable,ref_tabname,ref_field,checkbox
****Field catelog for on-act ALV
*  IF iv_check = lc_act.
*    PERFORM f_build_fcat USING iv_check '1' TEXT-014   TEXT-038 gc_x '' '' gc_x 3 ''.
*    PERFORM f_build_fcat USING iv_check '2' TEXT-015   TEXT-009 '' '' '' '' 4 ''.
*    PERFORM f_build_fcat USING iv_check '3' TEXT-016   TEXT-007 '' '' '' '' 10 '==ALPHA'.
*    PERFORM f_build_fcat USING iv_check '4' TEXT-017   TEXT-008 '' '' '' '' 20 '==ALPHA'.
*    PERFORM f_build_fcat USING iv_check '5' TEXT-018   TEXT-039 '' '' '' '' 12 '==ALPHA'.
*    PERFORM f_build_fcat USING iv_check '6' TEXT-064   TEXT-040 '' '' '' '' 8 ''.
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
*    PERFORM f_build_fcat USING iv_check '7' TEXT-023   TEXT-087  '' '' '' '' 16 '==ALPHA'.
*    PERFORM f_build_fcat USING iv_check '8' TEXT-020   TEXT-041 '' TEXT-054 TEXT-055 '' '' ''.
*    PERFORM f_build_fcat USING iv_check '9' TEXT-021   TEXT-042 '' TEXT-054 TEXT-055 '' '' ''.
**    PERFORM f_build_fcat USING iv_check '7' TEXT-020   TEXT-041 '' TEXT-054 TEXT-055 '' '' ''.
**    PERFORM f_build_fcat USING iv_check '8' TEXT-021   TEXT-042 '' TEXT-054 TEXT-055 '' '' ''.
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
*  ENDIF.
****Field catelog for on-invoice ALV
*  IF iv_check = lc_inv.
*    PERFORM f_build_fcat USING iv_check '1' TEXT-014   TEXT-038 gc_x '' '' gc_x 3 ''.
*    PERFORM f_build_fcat USING iv_check '2' TEXT-015   TEXT-009 '' '' '' '' 4 ''.
*    PERFORM f_build_fcat USING iv_check '3' TEXT-016   TEXT-007 '' '' '' '' 10 '==ALPHA'.
*    PERFORM f_build_fcat USING iv_check '4' TEXT-017   TEXT-008 '' '' '' '' 20 '==ALPHA'.
*    PERFORM f_build_fcat USING iv_check '5' TEXT-018   TEXT-039 '' '' '' '' 12 '==ALPHA'.
*    PERFORM f_build_fcat USING iv_check '6' TEXT-022   TEXT-043  '' '' '' '' 35 ''.
*    PERFORM f_build_fcat USING iv_check '7' TEXT-023   TEXT-044  '' '' '' '' 16 '==ALPHA'.
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
*    PERFORM f_build_fcat USING iv_check '8' TEXT-064   TEXT-040 '' '' '' '' 8 ''.
*    PERFORM f_build_fcat USING iv_check '9' TEXT-088   TEXT-089 '' '' '' '' 8 ''.
*    PERFORM f_build_fcat USING iv_check '10' TEXT-090   TEXT-091  '' '' '' '' 3 ''.
*    PERFORM f_build_fcat USING iv_check '11' TEXT-092   TEXT-093  '' '' '' '' 3 ''.
*    PERFORM f_build_fcat USING iv_check '12' TEXT-020   TEXT-041 '' TEXT-054 TEXT-055 '' '' ''.
*    PERFORM f_build_fcat USING iv_check '13' TEXT-021   TEXT-042 '' TEXT-054 TEXT-055 '' '' ''.
**    PERFORM f_build_fcat USING iv_check '8' TEXT-020   TEXT-041 '' TEXT-054 TEXT-055 '' '' ''.
**    PERFORM f_build_fcat USING iv_check '9' TEXT-021   TEXT-042 '' TEXT-054 TEXT-055 '' '' ''.
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
*  ENDIF.
****Field catelog for BP item ALV
*  IF iv_check = lc_bp.
*    PERFORM f_build_fcat USING iv_check '1' TEXT-024   TEXT-007 '' '' '' '' 10 '==ALPHA'.
*    PERFORM f_build_fcat USING iv_check '2' TEXT-025   TEXT-008 '' '' '' '' 20 '==ALPHA'.
*    PERFORM f_build_fcat USING iv_check '3' TEXT-026   TEXT-045 '' '' '' '' 20 '==ALPHA'.
*    PERFORM f_build_fcat USING iv_check '4' TEXT-027   TEXT-009 '' '' '' '' 4 ''.
*    PERFORM f_build_fcat USING iv_check '5' TEXT-019   TEXT-057 '' TEXT-054 TEXT-019 '' 8 ''.
*    PERFORM f_build_fcat USING iv_check '6' TEXT-022   TEXT-043 '' '' '' '' 35 ''.
*    PERFORM f_build_fcat USING iv_check '7' TEXT-028   TEXT-056 '' '' '' '' 16 '==ALPHA'.
*    PERFORM f_build_fcat USING iv_check '8' TEXT-029   TEXT-041 '' TEXT-054 TEXT-055 '' '' ''.
*    PERFORM f_build_fcat USING iv_check '9' TEXT-030   TEXT-046 '' '' '' '' 1 ''.
*    PERFORM f_build_fcat USING iv_check '10' TEXT-031   TEXT-047 '' '' '' '' 50 ''.
*    PERFORM f_build_fcat USING iv_check '11' TEXT-032   TEXT-048 '' '' '' '' 10 '==ALPHA'.
*  ENDIF.
****Field catelog for GL item ALV
*  IF iv_check = lc_gl.
*    PERFORM f_build_fcat USING iv_check '1' TEXT-033   TEXT-049 '' '' '' '' 10 '==ALPHA'.
*    PERFORM f_build_fcat USING iv_check '2' TEXT-027   TEXT-009 '' '' '' '' 4 ''.
*    PERFORM f_build_fcat USING iv_check '3' TEXT-029   TEXT-041 '' TEXT-054 TEXT-055 '' '' ''.
*    PERFORM f_build_fcat USING iv_check '4' TEXT-030   TEXT-046 '' '' '' '' 1 ''.
*    PERFORM f_build_fcat USING iv_check '5' TEXT-034   TEXT-050 '' '' '' '' 10 ''.
**    PERFORM f_build_fcat USING iv_check '6' TEXT-035   TEXT-051 '' '' '' '' 12 '==ALPHA'. "DFDK914302
*    PERFORM f_build_fcat USING iv_check '7' TEXT-032   TEXT-048 '' '' '' '' 10 '==ALPHA'.
*    PERFORM f_build_fcat USING iv_check '8' TEXT-036   TEXT-052 '' '' '' '' 24 '==ABPSP'.
** Begin of change - 486861 Date: 10/11/2021 Defect # INC3765130
**    PERFORM f_build_fcat USING iv_check '9' TEXT-037   TEXT-053 '' '' '' '' 10 '==ALPHA'. "DFDK914302
**    PERFORM f_build_fcat USING iv_check '9' TEXT-037   TEXT-053 '' '' '' '' 10 ''.
** End of change - 486861 Date: 10/11/2021 Defect # INC3765130
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_build_fcat
**&---------------------------------------------------------------------*
**& build field catalog
**&---------------------------------------------------------------------*
*FORM f_build_fcat  USING  iv_check
*                          iv_col_pos
*                          iv_fieldname
*                          iv_scrtext_l
*                          iv_edit
*                          iv_ref_tab
*                          iv_ref_field
*                          iv_checkbox
*                          iv_outputlen
*                          iv_editmask.
*
*  "constants to identify alv
*  CONSTANTS : lc_act TYPE char2 VALUE 'AC',
*              lc_inv TYPE char2 VALUE 'IN',
*              lc_bp  TYPE char2 VALUE 'BP',
*              lc_gl  TYPE char2 VALUE 'GL'.
*
*  "Fill the field catalog
*  DATA(lw_fldcat_act) = VALUE lvc_s_fcat( col_pos    = iv_col_pos
*                                          fieldname  = iv_fieldname
*                                          scrtext_l  = iv_scrtext_l
*                                          checkbox   = iv_checkbox
*                                          edit       = iv_edit
*                                          ref_table  = iv_ref_tab
*                                          outputlen  = iv_outputlen
*                                          ref_field  = iv_ref_field
*                                          edit_mask  = iv_editmask  ).
*
*
*
*  CASE iv_check.
*    WHEN lc_act.
****Append on-act fieldcatelog
*      APPEND lw_fldcat_act TO gt_fldcat_act.
*    WHEN lc_inv.
****Append open invoice field catelog
*      APPEND lw_fldcat_act TO gt_fldcat_inv.
*    WHEN lc_bp.
*      IF iv_fieldname = TEXT-019.
*        lw_fldcat_act-f4availabl = gc_x.
*      ENDIF.
****Append BP item field catelog
*      APPEND lw_fldcat_act TO gt_fldcat_bp.
*    WHEN lc_gl.
****Append GL item field catelog
*      APPEND lw_fldcat_act TO gt_fldcat_gl.
*  ENDCASE.
*
*  CLEAR lw_fldcat_act.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_on_account_alv
**&---------------------------------------------------------------------*
**& Form to show ALV display for on-act open items
**&---------------------------------------------------------------------*
*FORM f_on_account_alv .
*
****Local data declaration
*  DATA : lw_layout TYPE lvc_s_layo.
*
****Constant declaration
*  CONSTANTS : lc_alvtyp   TYPE char2 VALUE 'AC'.
*
****Creating object of alv
*  CREATE OBJECT go_alv_act
*    EXPORTING
*      i_parent      = go_cont_act
*      i_appl_events = gc_x.
*
****alv layout
*  PERFORM f_alv_layout USING lc_alvtyp CHANGING lw_layout.
****alv field catalogue
*  PERFORM f_alv_fieldcat USING lc_alvtyp.
*
**** Register ALV mdification events
*  CALL METHOD go_alv_act->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*
*
****Set ALV event handler method
*  SET HANDLER go_alv_events->data_changed_act          FOR go_alv_act.
*  SET HANDLER go_alv_events->on_toolbar                FOR go_alv_act.
*  SET HANDLER go_alv_events->handle_user_command_act   FOR go_alv_act.
*
*
*
****Display the ALV grid
*  CALL METHOD go_alv_act->set_table_for_first_display
*    EXPORTING
*      is_layout            = lw_layout
*      it_toolbar_excluding = gt_exclude
*    CHANGING
*      it_outtab            = gt_act "gt_onact
*      it_fieldcatalog      = gt_fldcat_act.
*
*
**  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_on_invoice_alv
**&---------------------------------------------------------------------*
**& Form to show ALV display for on-invoice open items
**&---------------------------------------------------------------------*
*FORM f_on_invoice_alv .
*
****local data declaration
*  DATA : lw_layout TYPE lvc_s_layo.
*
****Constant declaration
*  CONSTANTS : lc_alvtyp   TYPE char2 VALUE 'IN'.
*
****Creating object of alv
*  CREATE OBJECT go_alv_inv
*    EXPORTING
*      i_parent      = go_cont_inv
*      i_appl_events = gc_x.
*
*
****alv layout
*  PERFORM f_alv_layout USING lc_alvtyp CHANGING lw_layout.
****alv field catalogue
*  PERFORM f_alv_fieldcat USING lc_alvtyp.
*
****Register cell modification event for grid
*  CALL METHOD go_alv_inv->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
****Handle data changed event and on toolbar event for grid
*  SET HANDLER go_alv_events->data_changed_inv          FOR go_alv_inv.
*  SET HANDLER go_alv_events->on_toolbar                FOR go_alv_inv.
*  SET HANDLER go_alv_events->handle_user_command_inv   FOR go_alv_inv.
*
****Display the ALV grid
*  CALL METHOD go_alv_inv->set_table_for_first_display
*    EXPORTING
*      is_layout            = lw_layout
*      it_toolbar_excluding = gt_exclude
*    CHANGING
*      it_outtab            = gt_inv
*      it_fieldcatalog      = gt_fldcat_inv.
**  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_bpitem_alv
**&---------------------------------------------------------------------*
**& Form to show ALV display for BP items
**&---------------------------------------------------------------------*
*FORM f_bpitem_alv .
*
****Local data declaration
*  DATA : lw_layout TYPE lvc_s_layo.
*
****Constant declaration
*  CONSTANTS : lc_alvtyp   TYPE char2 VALUE 'BP'.
*
****Creating object of alv
*  CREATE OBJECT go_alv_bp
*    EXPORTING
*      i_parent      = go_cont_bp
*      i_appl_events = gc_x.
*
****ALV layout create
*  PERFORM f_alv_layout USING lc_alvtyp CHANGING lw_layout.
*
****ALV field catalogue
*  PERFORM f_alv_fieldcat USING lc_alvtyp.
*
****If no file is uploaded the fill the table with 1 blank lines for user inputs
*  IF gt_bpitem IS INITIAL.
*    DO 1 TIMES.
*      APPEND INITIAL LINE TO gt_bpitem.
*    ENDDO.
*  ENDIF.
*
**Exclude toolbaar without add/delet row
*  DATA(lt_exclude) = gt_exclude[].
****Exclude delete,append,insert functionality from tool bar
*  LOOP AT lt_exclude ASSIGNING FIELD-SYMBOL(<fs_exclude>).
*    IF <fs_exclude> = cl_gui_alv_grid=>mc_fc_loc_delete_row.
*      CLEAR <fs_exclude>.
*    ENDIF.
*    IF <fs_exclude> = cl_gui_alv_grid=>mc_fc_loc_append_row.
*      CLEAR <fs_exclude>.
*    ENDIF.
*    IF <fs_exclude> = cl_gui_alv_grid=>mc_fc_loc_insert_row.
*      CLEAR <fs_exclude>.
*    ENDIF.
*  ENDLOOP.
*
****Register cell modification event for grid
*  CALL METHOD go_alv_bp->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
****Handle cell modification event
*  SET HANDLER go_alv_events->data_changed_bp FOR go_alv_bp.
*
****Display the ALV grid
*  CALL METHOD go_alv_bp->set_table_for_first_display
*    EXPORTING
*      is_layout            = lw_layout
*      it_toolbar_excluding = lt_exclude
*    CHANGING
*      it_outtab            = gt_bpitem
*      it_fieldcatalog      = gt_fldcat_bp.
**  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_glitem_alv
**&---------------------------------------------------------------------*
**& Form to show ALV display for GL items
**&---------------------------------------------------------------------*
*FORM f_glitem_alv .
*
****Local data declaration
*  DATA : lw_layout TYPE lvc_s_layo.
*
****Constant declaration
*  CONSTANTS : lc_alvtyp   TYPE char2 VALUE 'GL'.
*
****Creating object of alv
*  CREATE OBJECT go_alv_gl
*    EXPORTING
*      i_parent      = go_cont_gl
*      i_appl_events = gc_x.
*
****alv layout
*  PERFORM f_alv_layout USING lc_alvtyp CHANGING lw_layout.
****alv field catalogue
*  PERFORM f_alv_fieldcat USING lc_alvtyp.
*
****If no file is uploaded the fill the table with 3 blank lines for user inputs
*  IF gt_glitem IS INITIAL.
*    DO 1 TIMES.
*      APPEND INITIAL LINE TO gt_glitem.
*    ENDDO.
*  ENDIF.
*
**Exclude toolbaar without add/delet row
*  DATA(lt_exclude) = gt_exclude[].
*
*  LOOP AT lt_exclude ASSIGNING FIELD-SYMBOL(<fs_exclude>).
*    IF <fs_exclude> = cl_gui_alv_grid=>mc_fc_loc_delete_row.
*      CLEAR <fs_exclude>.
*    ENDIF.
*    IF <fs_exclude> = cl_gui_alv_grid=>mc_fc_loc_append_row.
*      CLEAR <fs_exclude>.
*    ENDIF.
*    IF <fs_exclude> = cl_gui_alv_grid=>mc_fc_loc_insert_row.
*      CLEAR <fs_exclude>.
*    ENDIF.
*  ENDLOOP.
*
****Register cell modification event for grid
*  CALL METHOD go_alv_gl->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
****Handle cell modification event
*  SET HANDLER go_alv_events->data_changed_gl  FOR go_alv_gl.
*
****Display the ALV grid
*  CALL METHOD go_alv_gl->set_table_for_first_display
*    EXPORTING
*      is_layout            = lw_layout
*      it_toolbar_excluding = lt_exclude
*    CHANGING
*      it_outtab            = gt_glitem
*      it_fieldcatalog      = gt_fldcat_gl.
*
**  ENDIF.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_event_data_changed_act
**&---------------------------------------------------------------------*
**& Form to handle data changed event for on-act alv grid
**&---------------------------------------------------------------------*
*FORM f_event_data_changed_act  USING it_data_changed TYPE lvc_t_modi.
*
****Check if any cell is modified
*  IF it_data_changed IS NOT INITIAL .
****Check if amount cell is modified
*    LOOP AT it_data_changed INTO DATA(lw_modcell).
****Check if change happend in gross clearing field
*      IF lw_modcell-fieldname = gc_fld_amt.
*
*        CONDENSE lw_modcell-value NO-GAPS.
****Select the row in output table which is changed
*        READ TABLE gt_act ASSIGNING FIELD-SYMBOL(<fs_onact>) INDEX lw_modcell-row_id.
*        IF sy-subrc = 0.
*          IF <fs_onact>-checkbox = gc_x.
****If check box clicked changed the amount field with the value given by user
*            <fs_onact>-amount_cl = lw_modcell-value.
****Check if the clearing amount sign mismatch with open item amount sign
****and change accordingly
*            IF <fs_onact>-amount_op < 0 AND <fs_onact>-amount_cl > 0.
*              <fs_onact>-amount_cl = <fs_onact>-amount_cl * -1.
*            ELSEIF <fs_onact>-amount_op > 0 AND <fs_onact>-amount_cl < 0.
*              <fs_onact>-amount_cl = <fs_onact>-amount_cl * -1.
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.
*
*      ELSEIF lw_modcell-fieldname = gc_fld_chk.
*
****Check if check box is clicked
*        READ TABLE gt_act ASSIGNING FIELD-SYMBOL(<fs_onactchk>) INDEX lw_modcell-row_id.
*        IF sy-subrc = 0.
*          <fs_onactchk>-checkbox = lw_modcell-value.
****If check box clicked copy the open amount to clearing amount
*          <fs_onactchk>-amount_cl = <fs_onactchk>-amount_op.
*        ELSE.
****Clear error flag if value rectified
****If check box un clicked clear the clearing amount
*          CLEAR :<fs_onactchk>-amount_cl.
*        ENDIF.
*      ENDIF.
*
*
*      CLEAR:lw_modcell.
*    ENDLOOP.
*
****Check if gross clearing field is ediatble for check box clicked
****and make it editable and uneditable based on checkbox status
*    LOOP AT gt_act ASSIGNING FIELD-SYMBOL(<fs_act>).
*
*      READ TABLE <fs_act>-celltab ASSIGNING FIELD-SYMBOL(<fs_celltab>) INDEX 1.
*      IF sy-subrc = 0 .
*        IF <fs_celltab>-fieldname = gc_fld_amt.
*          IF <fs_act>-checkbox = gc_x.
*            <fs_celltab>-style = cl_gui_alv_grid=>mc_style_enabled.
*          ELSE.
*            <fs_celltab>-style = cl_gui_alv_grid=>mc_style_disabled.
*
*            CLEAR : <fs_act>-amount_cl.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*
*    CLEAR : gv_augbt_act_cl,gv_augbt_act_ps.
****Calculate total On-act open item clearing amount
*    LOOP AT gt_act INTO DATA(lw_onact).
*      IF lw_onact-checkbox = gc_x.
****Modify total clearing amount
*        gv_augbt_act_cl  = gv_augbt_act_cl + lw_onact-amount_cl.
*        CLEAR lw_onact.
*      ENDIF.
*    ENDLOOP.
*    gv_augbt_act_ps = gv_augbt_act_cl * -1.
*
****Calculate the difference of credit and debit
*    gv_difference = ( gv_augbt_act_cl * -1 ) + ( gv_augbt_inv_cl * -1 ) + gv_augbt_bp + gv_augbt_gl.
*
****Perform Simulation before actual posting
*    PERFORM f_simulate_post CHANGING gv_error.
*
*
****Refresh grid display
*    CALL METHOD go_alv_act->refresh_table_display
*      EXPORTING
*        is_stable = gw_stable.
*
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_event_data_changed_inv
**&---------------------------------------------------------------------*
**& Form to handle data changed event for open invoice alv grid
**&---------------------------------------------------------------------*
*FORM f_event_data_changed_inv  USING it_data_changed TYPE lvc_t_modi.
*
****Check if any cell is modified
*  IF it_data_changed IS NOT INITIAL.
****Check if amount cell is modified
*    LOOP AT it_data_changed INTO DATA(lw_modcell).
****Check if change happend in gross clearing field
*      IF lw_modcell-fieldname = gc_fld_amt.
*
*        CONDENSE lw_modcell-value NO-GAPS.
****Select the row in output table which is changed
*        READ TABLE gt_inv ASSIGNING FIELD-SYMBOL(<fs_oninv>) INDEX lw_modcell-row_id.
*        IF sy-subrc = 0.
****If check box clicked changed the amount field with the value given by user
*          IF <fs_oninv>-checkbox = gc_x.
****If check box clicked changed the amount field with the value given by user
*            <fs_oninv>-amount_cl = lw_modcell-value.
****Check if the clearing amount sign mismatch with open item amount sign
****and change accordingly
*            IF <fs_oninv>-amount_op < 0 AND <fs_oninv>-amount_cl > 0.
*              <fs_oninv>-amount_cl = <fs_oninv>-amount_cl * -1.
*            ELSEIF <fs_oninv>-amount_op > 0 AND <fs_oninv>-amount_cl < 0.
*              <fs_oninv>-amount_cl = <fs_oninv>-amount_cl * -1.
*            ENDIF.
*
*          ENDIF.
*        ENDIF.
*
*      ELSEIF lw_modcell-fieldname = gc_fld_chk.
*
****Check if check box is clicked
*        READ TABLE gt_inv ASSIGNING FIELD-SYMBOL(<fs_oninvchk>) INDEX lw_modcell-row_id.
*        IF sy-subrc = 0.
*          <fs_oninvchk>-checkbox = lw_modcell-value.
****Check if the clearing amount sign mismatch with open item amount sign
*          IF <fs_oninvchk>-checkbox = gc_x.
****If check box clicked copy the open amount to clearing amount
*            <fs_oninvchk>-amount_cl = <fs_oninvchk>-amount_op.
*          ELSE.
****Clear error flag if value rectified
****If check box un clicked clear the clearing amount
*            CLEAR : <fs_oninvchk>-amount_cl.
*          ENDIF.
*        ENDIF.
*      ENDIF.
****Clear variables
*      CLEAR:lw_modcell.
*    ENDLOOP.
*
****Check if gross clearing field is ediatble for check box clicked
****and make it editable and uneditable based on checkbox status
*    LOOP AT gt_inv ASSIGNING FIELD-SYMBOL(<fs_inv>).
*
*      READ TABLE <fs_inv>-celltab ASSIGNING FIELD-SYMBOL(<fs_celltab>) INDEX 1.
*      IF sy-subrc = 0 .
*        IF <fs_celltab>-fieldname = gc_fld_amt.
*          IF <fs_inv>-checkbox = gc_x.
*            <fs_celltab>-style = go_alv_inv->mc_style_enabled.
*          ELSE.
*            <fs_celltab>-style = cl_gui_alv_grid=>mc_style_disabled.
*            CLEAR : <fs_inv>-amount_cl.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*
*    CLEAR : gv_augbt_inv_cl,gv_augbt_inv_ps.
****Calculate total value based on checkbox
*    LOOP AT gt_inv INTO DATA(lw_oninv).
*      IF lw_oninv-checkbox = gc_x.
****Modify total clearing amount
*        gv_augbt_inv_cl  = gv_augbt_inv_cl + lw_oninv-amount_cl.
*        CLEAR lw_oninv.
*      ENDIF.
*    ENDLOOP.
*    gv_augbt_inv_ps = gv_augbt_inv_cl * -1.
****Calculate the difference of credit and debit
*    gv_difference = ( gv_augbt_act_cl * -1 ) + ( gv_augbt_inv_cl * -1 ) + gv_augbt_bp + gv_augbt_gl.
****Perform Simulation before actual posting
*    PERFORM f_simulate_post CHANGING gv_error.
*
*
***Refresh grid display
*    CALL METHOD go_alv_inv->refresh_table_display
*      EXPORTING
*        is_stable = gw_stable.
*
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_get_filelocation
**&---------------------------------------------------------------------*
**& Form to chose file to be uploaded from application server
**&---------------------------------------------------------------------*
*FORM f_get_filelocation .
*
****Choose file from application server
*  CALL FUNCTION 'F4_FILENAME'
*    EXPORTING
*      program_name  = syst-cprog
*      dynpro_number = syst-dynnr
*    IMPORTING
*      file_name     = p_bpfile.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_get_data_from_file
**&---------------------------------------------------------------------*
**& Form to get data from file into table
**&---------------------------------------------------------------------*
*FORM f_get_data_from_file .
*
****Data declaration
*  DATA: lv_file_str TYPE string,
*        lv_ptdate   TYPE dats,
*        lv_day      TYPE char2,
*        lv_mon      TYPE char2,
*        lv_year     TYPE char4.
*
****Constants declaration
*  CONSTANTS : lc_source TYPE char1 VALUE 'P',
*              lc_x      TYPE abap_encod VALUE 'X',
*              lc_bp     TYPE char2 VALUE 'BP',
*              lc_gl     TYPE char2 VALUE 'GL',
*              lc_debit  TYPE shkzg VALUE 'D',
*              lc_credit TYPE shkzg VALUE 'C'.
*
*  IF p_bpfile IS NOT INITIAL.
*
****Pass file location to string variable
*    lv_file_str = p_bpfile.
*
**** Read the file based on the source of the file
*    go_utility->read_file( EXPORTING  i_filename        = lv_file_str
*                                      i_source          = lc_source
*                                      i_delimiter       = ','
*                                      i_hdr             = lc_x
*                           CHANGING   e_datatab         = gt_item
*                           EXCEPTIONS cannot_open_file  = 1
*                                      invalid_delimeter = 2
*                                      error_in_read     = 3
*                                      invalid_source    = 4
*                                      OTHERS            = 5  ).
*    IF sy-subrc <> 0.
****Check if file has any issues
*    ENDIF.
*
****Check for mandatory fields
*    CLEAR : gv_augbt_gl,gv_augbt_bp.
*    IF gt_item IS NOT INITIAL.
****Sort Item table with indicator
*      SORT gt_item BY indicator.
*      LOOP AT gt_item INTO DATA(lw_item).
****If indicator field is blank raise error message
*        IF lw_item-indicator IS INITIAL.
*          MESSAGE e158 .
*        ENDIF.
*
*        CASE lw_item-indicator.
****Check if any of the mandatory field is blank and raise error message
*          WHEN lc_bp.
*            IF lw_item-gpart IS INITIAL OR lw_item-vkont IS INITIAL OR
*               lw_item-bukrs IS INITIAL OR lw_item-betrw IS INITIAL OR
*              lw_item-shkzg IS INITIAL.
*              MESSAGE e161 WITH lc_bp.
*            ENDIF.
****Check if any of the mandatory field is blank and raise error message
*          WHEN lc_gl.
*            IF lw_item-hkont_gl IS INITIAL OR lw_item-bukrs_gl IS INITIAL OR
*              lw_item-betrw_gl IS INITIAL OR lw_item-shkzg_gl IS INITIAL.
*              MESSAGE e161 WITH lc_gl.
*            ENDIF.
*        ENDCASE.
*
****Date to internal format
*        SPLIT lw_item-budat AT '/' INTO lv_mon lv_day lv_year.
*        IF lv_mon < 10.
*          lv_mon = |0{ lv_mon }|.
*        ENDIF.
*        IF lv_day < 10.
*          lv_day = |0{ lv_day }|.
*        ENDIF.
*        lv_ptdate = |{ lv_year }{ lv_mon }{ lv_day }|.
****Fill BP item table where the indicator is BP
*        IF lw_item-indicator = lc_bp.
*          APPEND INITIAL LINE TO gt_bpitem ASSIGNING FIELD-SYMBOL(<fs_bpitem>).
*
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = lw_item-gpart
*            IMPORTING
*              output = <fs_bpitem>-gpart.
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = lw_item-vkont
*            IMPORTING
*              output = <fs_bpitem>-vkont.
*
**          <fs_bpitem>-gpart  = lw_item-gpart.
**          <fs_bpitem>-vkont  = lw_item-vkont.
*          <fs_bpitem>-vtref  = lw_item-vtref.
*          <fs_bpitem>-bukrs  = lw_item-bukrs.
*          <fs_bpitem>-faedn  = lv_ptdate."lw_item-budat.
*          <fs_bpitem>-zzuonr  = lw_item-zzuonr.
*          <fs_bpitem>-xblnr  = lw_item-xblnr.
*          <fs_bpitem>-shkzg  = lw_item-shkzg.
****Check if debit/credit indicator
*          IF <fs_bpitem>-shkzg = lc_credit.
*            <fs_bpitem>-betrw = lw_item-betrw * -1.
*          ELSEIF <fs_bpitem>-shkzg = lc_debit.
*            <fs_bpitem>-betrw = lw_item-betrw.
*          ENDIF.
*          gv_augbt_bp = gv_augbt_bp + <fs_bpitem>-betrw.
*          <fs_bpitem>-optxt  = lw_item-optxt.
*          <fs_bpitem>-prctr  = lw_item-prctr.
*
****Fill GL item table where the indicator is GL
*        ELSEIF lw_item-indicator = lc_gl.
*          APPEND INITIAL LINE TO gt_glitem ASSIGNING FIELD-SYMBOL(<fs_glitem>).
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = lw_item-hkont_gl
*            IMPORTING
*              output = <fs_glitem>-hkont.
*          <fs_glitem>-bukrs  = lw_item-bukrs_gl.
*          <fs_glitem>-shkzg  = lw_item-shkzg_gl.
****Check if debit/credit indicator
*          IF <fs_glitem>-shkzg = lc_credit.
*            <fs_glitem>-betrw = lw_item-betrw_gl * -1.
*          ELSEIF <fs_glitem>-shkzg = lc_debit.
*            <fs_glitem>-betrw = lw_item-betrw_gl.
*          ENDIF.
*          gv_augbt_gl = gv_augbt_gl + <fs_glitem>-betrw.
*          <fs_glitem>-kostl  = lw_item-kostl_gl.
**          <fs_glitem>-aufnr  = lw_item-aufnr_gl.  "DFDK914302
*          <fs_glitem>-prctr  = lw_item-prctr_gl.
*          <fs_glitem>-ps_psp_pnr  = lw_item-ps_psp_pnr_gl.
**          <fs_glitem>-kdauf  = lw_item-kdauf_gl.  "DFDK914302
*
*        ENDIF.
*        CLEAR: lw_item,lv_mon,lv_day,lv_year,lv_ptdate.
*      ENDLOOP.
*    ENDIF.
*
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_post_document
**&---------------------------------------------------------------------*
**& Form to simulate data with FI document posting
**&---------------------------------------------------------------------*
*FORM f_save_function .
*
****Data declaration
*  DATA : lv_fikey        TYPE fikey_kk,
*         lw_fkkko        TYPE fkkko,
*         lt_fkkop        TYPE STANDARD TABLE OF fkkop,
*         lt_fkkopk       TYPE STANDARD TABLE OF fkkopk,
*         lt_fkkcl        TYPE STANDARD TABLE OF fkkcl,
*         lt_fkkop_split  TYPE STANDARD TABLE OF fkkop,
*         lt_fkkopk_split TYPE STANDARD TABLE OF fkkopk,
*         lt_fkkcl_split  TYPE STANDARD TABLE OF fkkcl,
*         lv_loop_cnt     TYPE i,
*         lv_flag         TYPE c,
*         lv_message      TYPE string.
*
*  CONSTANTS : lc_green  TYPE char1 VALUE 'G',
*              lc_yellow TYPE char1 VALUE 'Y'.
*
** Begin of change - 486861 Date: 10/11/2021 Defect # INC3765130
****Check for BP tab
****  Clear error table
*  CLEAR : gt_error.
*  IF gt_bpitem IS NOT INITIAL.
*    PERFORM f_validate_bp_manualentry.
*  ENDIF.
****Check for GL tab
*  IF gt_glitem IS NOT INITIAL.
*    PERFORM f_validate_gl_manualentry.
*  ENDIF.
*  IF gt_error IS NOT INITIAL.
*    PERFORM f_display_manualentry_error.
*  ELSE.
** End of change - 486861 Date: 10/11/2021 Defect # INC3765130
*
****Perform Simulation before actual posting
*    PERFORM f_simulate_post CHANGING gv_error.
*
****If total amount difference is nil or within tolerance limit
****  proceed with document posting
*    IF gv_error = lc_green OR gv_error = lc_yellow.
****generate fi key
*      PERFORM f_generate_fikey CHANGING lv_fikey.
****Create fkkko header structure
*      PERFORM f_fill_fkkko USING lv_fikey CHANGING lw_fkkko .
****Create fkkcl clearing table for BAPI
*      PERFORM f_fill_fkkcl CHANGING lt_fkkcl.
****Create fkkop table for BAPI
*      PERFORM f_fill_fkkop CHANGING lt_fkkop.
****Create fkkopk table for BAPI
*      PERFORM f_fill_fkkopk USING lv_fikey gv_error
*                            CHANGING lt_fkkopk.
****Changes for spliting logic
*      SORT : lt_fkkcl,
*             lt_fkkop.
*      DATA(lv_fkkcl_len) = lines( lt_fkkcl ).
*      DATA(lv_fkkop_len) = lines( lt_fkkop ).
*      DATA(lv_fkkopk_len) = lines( lt_fkkopk ).
*
*      IF lv_fkkcl_len > gv_cl_split_no OR lv_fkkop_len > gv_bp_split_no OR lv_fkkopk_len > gv_gl_split_no.
*
*        DATA(lt_fkkcl_bukrs) = lt_fkkcl.
*        SORT lt_fkkcl_bukrs BY bukrs.
*        DELETE ADJACENT DUPLICATES FROM lt_fkkcl_bukrs COMPARING bukrs.
*        DATA(lv_bukrs_lines) = lines( lt_fkkcl_bukrs ).
*        IF lv_bukrs_lines = 1.
*          READ TABLE lt_fkkcl_bukrs INTO DATA(lw_bukrs) INDEX 1.
*          IF sy-subrc EQ 0.
*            DATA(lv_gl_bukrs) = lw_bukrs-bukrs.
*          ENDIF.
*
****Get looping counter for split documents
*          PERFORM f_looping_counter USING lv_fkkcl_len
*                                          lv_fkkop_len
*                                          lv_fkkopk_len
*                                          CHANGING lv_loop_cnt.
*          gt_fkkcl_dup   = lt_fkkcl.
*          DO lv_loop_cnt TIMES.
*            IF lv_flag IS INITIAL.
*
*
****Get split documents numbers as per TVARVC split limit
*              PERFORM f_get_split_docs CHANGING lt_fkkop
*                                                lt_fkkopk
*                                                lt_fkkcl
*                                                lt_fkkop_split
*                                                lt_fkkopk_split
*                                                lt_fkkcl_split
*                                                lv_fikey
*                                                lv_gl_bukrs.
*              IF lv_loop_cnt = sy-index.
*                DATA(lv_final_proc) = abap_true.
*              ENDIF.
****Call to post fi document
*              PERFORM f_post_document_split USING lt_fkkop_split
*                                                  lt_fkkopk_split
*                                                  lt_fkkcl_split
*                                                  lw_fkkko
*                                                  lv_fikey
*                                                  lv_final_proc
*                                                  sy-index
*                                         CHANGING lv_flag.
*              CLEAR: lt_fkkop_split,lt_fkkopk_split,lt_fkkcl_split.
*            ELSE.
*              MESSAGE e185 INTO lv_message.
*              APPEND VALUE #( split_no = sy-index
*                              type = gc_e
*                              message = lv_message )
*                      TO gt_output_log.
*
*            ENDIF.
*          ENDDO.
*        ELSE.
*          MESSAGE TEXT-t23 TYPE 'E'.
*        ENDIF.
*        IF gt_output_log IS NOT INITIAL.
*          CALL METHOD go_utility->display_alv
*            CHANGING
*              c_datatab = gt_output_log.
*          LEAVE TO SCREEN 0.
*        ENDIF.
*      ELSE.
*
**Call to post fi document
*        PERFORM f_post_document USING  lt_fkkop
*                                       lt_fkkopk
*                                       lt_fkkcl
*                                       lw_fkkko
*                                       lv_fikey.
*      ENDIF.
*
*    ELSE.
****If difference is too high for clearing raise error message
*      MESSAGE e166 WITH gv_difference p_curr.
*    ENDIF.
*
** Begin of change - 486861 Date: 10/11/2021 Defect # INC3765130
*  ENDIF.
** End of change - 486861 Date: 10/11/2021 Defect # INC3765130
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_exclude_functions
**&---------------------------------------------------------------------*
**& Form to fill exclude toolbar functions for ALV grid
**&---------------------------------------------------------------------*
*FORM f_exclude_functions  CHANGING it_exclude TYPE ui_functions.
*
****Data declaration
*  DATA: lv_exclude TYPE ui_func.
*
****Exclude certain buttons from ALV toolbar
*  lv_exclude = cl_gui_alv_grid=>mc_fc_refresh.
****Append exclude function to table
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_fc_info.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_fc_print.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_mb_export.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_fc_views.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_mb_variant.
*  APPEND lv_exclude TO it_exclude.
*  lv_exclude = cl_gui_alv_grid=>mc_fc_sum.
*  APPEND lv_exclude TO it_exclude.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_on_toolbar
**&---------------------------------------------------------------------*
**& Form to asdd Select All and Deselect All buttons to ALV toolbar
**&---------------------------------------------------------------------*
*FORM f_on_toolbar  USING    iv_object TYPE REF TO cl_alv_event_toolbar_set.
*
****Data declaration
*  DATA: lw_toolbar  TYPE stb_button.
*
*  CONSTANTS : lc_selall   TYPE syst_ucomm VALUE 'SELALL',
*              lc_deselect TYPE syst_ucomm VALUE 'DESELALL'.
*
**** Append a separator to normal toolbar
*  CLEAR lw_toolbar.
*  lw_toolbar-butn_type = 3.
*  APPEND lw_toolbar TO iv_object->mt_toolbar.
*  CLEAR lw_toolbar.
*
*
**** Append select all Button & Icon for your function
*  lw_toolbar-function = lc_selall.
*  lw_toolbar-icon = icon_select_all.
*  lw_toolbar-disabled = ''.
****append select all button to button toolbar
*  APPEND lw_toolbar TO iv_object->mt_toolbar.
*  CLEAR lw_toolbar.
*
*  lw_toolbar-butn_type = 3.
*  APPEND lw_toolbar TO iv_object->mt_toolbar.
*  CLEAR lw_toolbar.
*
**** Append deselect all Button & Icon for your function
*  lw_toolbar-function = lc_deselect.
*  lw_toolbar-icon = icon_deselect_all.
*  lw_toolbar-disabled = space.
****append deselect all button to button toolbar
*  APPEND lw_toolbar TO iv_object->mt_toolbar.
*  CLEAR lw_toolbar.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_handle_user_command
**&---------------------------------------------------------------------*
**& Handle user command for select all or deselect all on on-act ALV grid
**&---------------------------------------------------------------------*
*FORM f_handle_user_command_act  USING    iv_ucomm.
*
****Local data declaration
*  DATA : lt_filter_entries TYPE lvc_t_fidx, "Filtered entries,
*         lv_sel_valid      TYPE char1,
*         lv_sel_tabix      TYPE sy-tabix.
*
*  CONSTANTS : lc_selall   TYPE syst_ucomm VALUE 'SELALL',
*              lc_deselect TYPE syst_ucomm VALUE 'DESELALL',
*              lc_fld_amt  TYPE lvc_fname VALUE 'AMOUNT_CL'.
*
****Check if select all or deselect all buttons clicked on ALV
*  CASE iv_ucomm.
*    WHEN lc_selall.
****Check if ALV object is not intial
*      IF go_alv_act IS NOT INITIAL.
*        CALL METHOD go_alv_act->check_changed_data
*          IMPORTING
*            e_valid = lv_sel_valid.
*      ENDIF.
****Check if any values changed in ALV
*      IF lv_sel_valid = gc_x.
*        CALL METHOD go_alv_act->get_filtered_entries
*          IMPORTING
*            et_filtered_entries = lt_filter_entries.
****Loop through on account items for any row changed data
*        LOOP AT gt_act ASSIGNING FIELD-SYMBOL(<fs_onact>).
*          lv_sel_tabix = sy-tabix.
*          READ TABLE lt_filter_entries FROM lv_sel_tabix TRANSPORTING NO FIELDS.
*          IF sy-subrc IS NOT INITIAL.
*            <fs_onact>-checkbox = abap_true.
*          ENDIF.
*
****Making all clearing fields editable or unediatble based on checkbox
*          READ TABLE <fs_onact>-celltab ASSIGNING FIELD-SYMBOL(<fs_celltab>) INDEX 1.
*          IF sy-subrc = 0 .
****Check if check box clicked and if clicked make clearing field editable
*            IF <fs_celltab>-fieldname = lc_fld_amt.
*              IF <fs_onact>-checkbox = gc_x.
*                <fs_celltab>-style = cl_gui_alv_grid=>mc_style_enabled.
*                <fs_onact>-amount_cl = <fs_onact>-amount_op.
*              ELSE.
****Check if check box clicked and if not clicked make clearing field uneditable
*                <fs_celltab>-style = cl_gui_alv_grid=>mc_style_disabled.
*                CLEAR : <fs_onact>-amount_cl.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*
*    WHEN lc_deselect.
****Logic for deselect all button
****Loop through on account items
*      LOOP AT gt_act ASSIGNING <fs_onact>.
*        <fs_onact>-checkbox = abap_false.
****Making all clearing fields editable or unediatble based on checkbox
*        READ TABLE <fs_onact>-celltab ASSIGNING <fs_celltab> INDEX 1.
*        IF sy-subrc = 0 .
****Check if check box clicked and if clicked make clearing field editable
*          IF <fs_celltab>-fieldname = lc_fld_amt.
*            IF <fs_onact>-checkbox = gc_x.
*              <fs_celltab>-style = cl_gui_alv_grid=>mc_style_enabled.
*              <fs_onact>-amount_cl = <fs_onact>-amount_op.
*            ELSE.
****Check if check box clicked and if not clicked make clearing field uneditable
*              <fs_celltab>-style = cl_gui_alv_grid=>mc_style_disabled.
*              CLEAR : <fs_onact>-amount_cl.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*
*  ENDCASE.
*
*  CLEAR : gv_augbt_act_cl,gv_augbt_inv_ps.
*  LOOP AT gt_act INTO DATA(lw_onact).
*    IF lw_onact-checkbox = gc_x.
*      gv_augbt_act_cl  = gv_augbt_act_cl + lw_onact-amount_cl.
*      CLEAR lw_onact.
*    ENDIF.
*  ENDLOOP.
*  gv_augbt_act_ps = gv_augbt_act_cl * -1.
****Calculate the difference of credit and debit
*  gv_difference = ( gv_augbt_act_cl * -1 ) + ( gv_augbt_inv_cl * -1 ) + gv_augbt_bp + gv_augbt_gl.
****Perform Simulation before actual posting
*  PERFORM f_simulate_post CHANGING gv_error.
*
*
*  "refresh alv display
*  CALL METHOD go_alv_act->refresh_table_display
*    EXPORTING
*      is_stable = gw_stable.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_handle_user_command_inv
**&---------------------------------------------------------------------*
**& Handle user command for select all or deselect all on open invoice ALV grid
**&---------------------------------------------------------------------*
*FORM f_handle_user_command_inv    USING    iv_ucomm.
*
****Local data declaration
*  DATA : lt_filter_entries TYPE lvc_t_fidx, "Filtered entries,
*         lv_sel_valid      TYPE char1,
*         lv_sel_tabix      TYPE sy-tabix.
*
*  CONSTANTS : lc_selall   TYPE syst_ucomm VALUE 'SELALL',
*              lc_deselect TYPE syst_ucomm VALUE 'DESELALL',
*              lc_fld_amt  TYPE lvc_fname VALUE 'AMOUNT_CL'.
*
****Check if select all or deselect all buttons clicked on ALV
*  CASE iv_ucomm.
*    WHEN lc_selall.
****Check if ALV object is not intial
*      IF go_alv_inv IS NOT INITIAL.
*        CALL METHOD go_alv_inv->check_changed_data
*          IMPORTING
*            e_valid = lv_sel_valid.
*      ENDIF.
****Check if any values changed in ALV
*      IF lv_sel_valid = gc_x.
*        CALL METHOD go_alv_inv->get_filtered_entries
*          IMPORTING
*            et_filtered_entries = lt_filter_entries.
****Loop through open invoice items for any row changed data
*        LOOP AT gt_inv ASSIGNING FIELD-SYMBOL(<fs_oninv>).
*          lv_sel_tabix = sy-tabix.
*          READ TABLE lt_filter_entries FROM lv_sel_tabix TRANSPORTING NO FIELDS.
*          IF sy-subrc IS NOT INITIAL.
*            <fs_oninv>-checkbox = abap_true.
*          ENDIF.
****Making all clearing fields editable or unediatble based on checkbox
*          READ TABLE <fs_oninv>-celltab ASSIGNING FIELD-SYMBOL(<fs_celltab>) INDEX 1.
*          IF sy-subrc = 0 .
****Check if check box clicked and if clicked make clearing field editable
*            IF <fs_celltab>-fieldname = lc_fld_amt.
*              IF <fs_oninv>-checkbox = gc_x.
*                <fs_celltab>-style = cl_gui_alv_grid=>mc_style_enabled.
*                <fs_oninv>-amount_cl = <fs_oninv>-amount_op.
*              ELSE.
****Check if check box clicked and if not clicked make clearing field uneditable
*                <fs_celltab>-style = cl_gui_alv_grid=>mc_style_disabled.
*                CLEAR : <fs_oninv>-amount_cl.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*
*    WHEN lc_deselect.
****Logic for deselect all button
****Loop through open invoice items
*      LOOP AT gt_inv ASSIGNING <fs_oninv>.
*        <fs_oninv>-checkbox = abap_false.
****Making all clearing fields editable or unediatble based on checkbox
*        READ TABLE <fs_oninv>-celltab ASSIGNING <fs_celltab> INDEX 1.
*        IF sy-subrc = 0 .
****Check if check box clicked and if clicked make clearing field editable
*          IF <fs_celltab>-fieldname = lc_fld_amt.
*            IF <fs_oninv>-checkbox = gc_x.
*              <fs_celltab>-style = cl_gui_alv_grid=>mc_style_enabled.
*              <fs_oninv>-amount_cl = <fs_oninv>-amount_op.
*            ELSE.
****Check if check box clicked and if not clicked make clearing field uneditable
*              <fs_celltab>-style = cl_gui_alv_grid=>mc_style_disabled.
*              CLEAR : <fs_oninv>-amount_cl.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*
*  ENDCASE.
*
****Fill total invoice variable with inovice item total
*  CLEAR : gv_augbt_inv_cl.
*  LOOP AT gt_inv INTO DATA(lw_oninv).
*    IF lw_oninv-checkbox = gc_x.
*      gv_augbt_inv_cl  = gv_augbt_inv_cl + lw_oninv-amount_cl.
*      CLEAR lw_oninv.
*    ENDIF.
*  ENDLOOP.
*  gv_augbt_inv_ps = gv_augbt_inv_cl * -1.
****Calculate the difference of credit and debit
*  gv_difference = ( gv_augbt_act_cl * -1 ) + ( gv_augbt_inv_cl * -1 ) + gv_augbt_bp + gv_augbt_gl.
****Perform Simulation before actual posting
*  PERFORM f_simulate_post CHANGING gv_error.
*
**  Refresh output
*  CALL METHOD go_alv_inv->refresh_table_display
*    EXPORTING
*      is_stable = gw_stable.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_simulate_post
**&---------------------------------------------------------------------*
**& SImulte data before posting to check data correctness
**&---------------------------------------------------------------------*
*FORM f_simulate_post  CHANGING    iv_error.
*
****data declaration
*  DATA: lv_icon_name  TYPE char20.
*  CONSTANTS : lc_green  TYPE char1 VALUE 'G',
*              lc_yellow TYPE char1 VALUE 'Y',
*              lc_red    TYPE char1 VALUE 'R'.
*
****Calculate the difference of credit and debit
*  gv_difference = ( gv_augbt_act_cl * -1 ) + ( gv_augbt_inv_cl * -1 ) + gv_augbt_bp + gv_augbt_gl.
*
** Begin of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
***For currency conversion
*  gv_difference = COND #( WHEN p_curr IN gr_zerodec_curr
*                      THEN gv_difference / 100
*                      ELSE  gv_difference )    .
** End of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
*
*
****Process using difference amount
*  CASE gv_difference.
****If difference amount is nil
*    WHEN 0.
*      lv_icon_name = 'ICON_GREEN_LIGHT'.
*      iv_error = lc_green.
****If any difference exist
*    WHEN OTHERS.
****Check if the difference amount is within or outside tolerance amount
*      PERFORM f_check_tolerance_amount CHANGING iv_error.
****Raise error traffic light if difference outside tolerance
*      IF iv_error = lc_red.
*        lv_icon_name = 'ICON_RED_LIGHT'.
*      ELSEIF iv_error = lc_yellow.
****Raise yellow traffic light if difference within tolerance
*        lv_icon_name = 'ICON_YELLOW_LIGHT'.
*      ENDIF.
*  ENDCASE.
*
****Create traffic light with above fetched lights
*  CALL FUNCTION 'ICON_CREATE'
*    EXPORTING
*      name                  = lv_icon_name
*      add_stdinf            = 'X'
*    IMPORTING
*      result                = gv_status
*    EXCEPTIONS
*      icon_not_found        = 1
*      outputfield_too_short = 2
*      OTHERS                = 3.
*  IF sy-subrc <> 0.
****Do nothing
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_event_data_changed_bp
**&---------------------------------------------------------------------*
**& Form to handle data changed event for BP Item alv grid
**&---------------------------------------------------------------------*
*FORM f_event_data_changed_bp  USING it_data_changed TYPE lvc_t_modi.
*
*  CONSTANTS : lc_debit  TYPE char1 VALUE 'D',
*              lc_credit TYPE char1 VALUE 'C'.
*
****Check if new line created or single field changed
*  DATA(lv_line) = lines( it_data_changed ).
*
****Check if any cell is modified
*  IF it_data_changed IS NOT INITIAL AND lv_line = 1.
****Check if amount cell is modified
*    LOOP AT it_data_changed INTO DATA(lw_modcell).
****Check if change happend in gross clearing field
*      IF lw_modcell-fieldname = gc_fld_betrw.
*        CONDENSE lw_modcell-value NO-GAPS.
****Select the row in output table which is changed
*        READ TABLE gt_bpitem  ASSIGNING FIELD-SYMBOL(<fs_bpitem>) INDEX lw_modcell-row_id.
*        IF sy-subrc = 0.
*          IF lw_modcell-value < 0 AND <fs_bpitem>-shkzg = lc_debit.
****Changed the amount field with the value given by user
*            <fs_bpitem>-betrw = lw_modcell-value * -1.
*          ELSEIF lw_modcell-value > 0 AND <fs_bpitem>-shkzg = lc_credit.
****Changed the amount field with the value given by user
*            <fs_bpitem>-betrw = lw_modcell-value * -1.
*          ELSE.
*            <fs_bpitem>-betrw = lw_modcell-value.
*          ENDIF.
*        ENDIF.
*      ENDIF.
****Check if change happend in shkzg field
*      IF lw_modcell-fieldname = gc_fld_shkzg.
*        CONDENSE lw_modcell-value NO-GAPS.
****Select the row in output table which is changed
*        READ TABLE gt_bpitem  ASSIGNING <fs_bpitem> INDEX lw_modcell-row_id.
*        IF sy-subrc = 0.
*          IF <fs_bpitem>-betrw < 0 AND lw_modcell-value = lc_debit.
****Changed the amount field with the value given by user
*            <fs_bpitem>-betrw = <fs_bpitem>-betrw * -1.
*          ELSEIF <fs_bpitem>-betrw > 0 AND lw_modcell-value = lc_credit.
****Changed the amount field with the value given by user with sign
*            <fs_bpitem>-betrw = <fs_bpitem>-betrw * -1.
*          ENDIF.
*        ENDIF.
*
*      ENDIF.
*      CLEAR:lw_modcell.
*    ENDLOOP.
*
*
*    CLEAR :gv_augbt_bp.
****Fil total value for BP items
*    LOOP AT gt_bpitem INTO DATA(lw_bpitem).
*      gv_augbt_bp  = gv_augbt_bp + lw_bpitem-betrw.
*      CLEAR lw_bpitem.
*    ENDLOOP.
*
****Calculate the difference of credit and debit
*    gv_difference = ( gv_augbt_act_cl * -1 ) + ( gv_augbt_inv_cl * -1 ) + gv_augbt_bp + gv_augbt_gl.
****Perform Simulation before actual posting
*    PERFORM f_simulate_post CHANGING gv_error.
*
****Refresh alv display
*    CALL METHOD go_alv_bp->refresh_table_display
*      EXPORTING
*        is_stable = gw_stable.
*
*  ENDIF.
*
****Below logic will work if any line item deleted from ALV.
*  DATA(lv_count) = lines( gt_bpitem ).
*  DATA(lv_count_temp) = lines( gt_bpitem_temp ).
****check if main table is added with new line
*  IF lv_count_temp < lv_count.
*    CLEAR : gt_bpitem_temp[].
*    gt_bpitem_temp = gt_bpitem.
** Begin of change - 486861 Date: 10/11/2021 Defect # INC3765130
**  ELSEIF lv_count_temp > lv_count.
*  ELSEIF lv_count_temp >= lv_count.
** End of change - 486861 Date: 10/11/2021 Defect # INC3765130
****check if main table is deleted with line
****Calculate the difference of credit and debit
*    gv_difference = gv_difference - gv_augbt_bp.
*    CLEAR : gv_augbt_bp.
*    LOOP AT gt_bpitem INTO DATA(lw_bpitem_temp).
*      gv_augbt_bp  = gv_augbt_bp + lw_bpitem_temp-betrw.
*      CLEAR lw_bpitem_temp.
*    ENDLOOP.
*
*    CLEAR : gt_bpitem_temp[].
*    gt_bpitem_temp = gt_bpitem.
****Calculate the difference of credit and debit
*    gv_difference = ( gv_augbt_act_cl * -1 ) + ( gv_augbt_inv_cl * -1 ) + gv_augbt_bp + gv_augbt_gl.
*
****Perform Simulation before actual posting
*    PERFORM f_simulate_post CHANGING gv_error.
*
****Refresh alv display
*    CALL METHOD go_alv_bp->refresh_table_display
*      EXPORTING
*        is_stable = gw_stable.
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_event_data_changed_gl
**&---------------------------------------------------------------------*
**& Form for data changed event for GL item ALV
**&---------------------------------------------------------------------*
*FORM f_event_data_changed_gl  USING it_data_changed TYPE lvc_t_modi.
*
*  CONSTANTS : lc_credit TYPE char1 VALUE 'C',
*              lc_debit  TYPE char1 VALUE 'D'.
*
****Check if new line created or single field changed
*  DATA(lv_line) = lines( it_data_changed ).
*
****Check if any cell is modified
*  IF it_data_changed IS NOT INITIAL AND lv_line = 1.
****Check if amount cell is modified
*    LOOP AT it_data_changed INTO DATA(lw_modcell).
****Check if change happend in gross clearing field
*      IF lw_modcell-fieldname = gc_fld_betrw.
*        CONDENSE lw_modcell-value NO-GAPS.
****Select the row in output table which is changed
*        READ TABLE gt_glitem  ASSIGNING FIELD-SYMBOL(<fs_glitem>) INDEX lw_modcell-row_id.
*        IF sy-subrc = 0.
*          IF lw_modcell-value < 0 AND <fs_glitem>-shkzg = lc_debit.
****Changed the amount field with the value given by user
*            <fs_glitem>-betrw = lw_modcell-value * -1.
*          ELSEIF lw_modcell-value > 0 AND <fs_glitem>-shkzg = lc_credit.
****Changed the amount field with the value given by user
*            <fs_glitem>-betrw = lw_modcell-value * -1.
*          ELSE.
*            <fs_glitem>-betrw = lw_modcell-value.
*          ENDIF.
*        ENDIF.
*      ENDIF.
****Check if change happend in shkzg field
*      IF lw_modcell-fieldname = gc_fld_shkzg.
*        CONDENSE lw_modcell-value NO-GAPS.
****Select the row in output table which is changed
*        READ TABLE gt_glitem  ASSIGNING <fs_glitem> INDEX lw_modcell-row_id.
*        IF sy-subrc = 0.
*          IF <fs_glitem>-betrw > 0 AND lw_modcell-value = lc_credit.
****Changed the amount field with the value given by user
*            <fs_glitem>-betrw = <fs_glitem>-betrw * -1.
*          ELSEIF <fs_glitem>-betrw < 0 AND lw_modcell-value = lc_debit.
****Changed the amount field with the value given by user
*            <fs_glitem>-betrw = <fs_glitem>-betrw * -1.
*          ENDIF.
*
*        ENDIF.
*      ENDIF.
*
****Clear variables
*      CLEAR:lw_modcell.
*    ENDLOOP.
*
*    CLEAR : gv_augbt_gl.
****Fill total amount
*    LOOP AT gt_glitem INTO DATA(lw_glitem).
*      gv_augbt_gl  = gv_augbt_gl + lw_glitem-betrw.
*      CLEAR lw_glitem.
*    ENDLOOP.
*
****Calculate the difference of credit and debit
*    gv_difference = ( gv_augbt_act_cl * -1 ) + ( gv_augbt_inv_cl * -1 ) + gv_augbt_bp + gv_augbt_gl.
****Perform Simulation before actual posting
*    PERFORM f_simulate_post CHANGING gv_error.
*
****Refresh alv display
*    CALL METHOD go_alv_gl->refresh_table_display
*      EXPORTING
*        is_stable = gw_stable.
*  ENDIF.
*
****Below logic will work if any line item deleted from ALV.
*  DATA(lv_count) = lines( gt_glitem ).
*  DATA(lv_count_temp) = lines( gt_glitem_temp ).
****check if main table is added with new line
*  IF lv_count_temp < lv_count.
*    CLEAR : gt_glitem_temp[].
*    gt_glitem_temp = gt_glitem.
** Begin of change - 486861 Date: 10/11/2021 Defect # INC3765130
**  ELSEIF lv_count_temp > lv_count.
*  ELSEIF lv_count_temp >= lv_count.
** End of change - 486861 Date: 10/11/2021 Defect # INC3765130
****check if main table is deleted with line
****Calculate the difference of credit and debit
*    gv_difference = gv_difference - gv_augbt_gl.
*    CLEAR : gv_augbt_gl.
*    LOOP AT gt_glitem INTO DATA(lw_glitem_temp).
*      gv_augbt_gl  = gv_augbt_gl + lw_glitem_temp-betrw.
*      CLEAR lw_glitem_temp.
*    ENDLOOP.
*
*    CLEAR : gt_glitem_temp[].
*    gt_glitem_temp = gt_glitem.
****Calculate the difference of credit and debit
*    gv_difference = ( gv_augbt_act_cl * -1 ) + ( gv_augbt_inv_cl * -1 ) + gv_augbt_bp + gv_augbt_gl.
*
****Perform Simulation before actual posting
*    PERFORM f_simulate_post CHANGING gv_error.
*
****Refresh alv display
*    CALL METHOD go_alv_gl->refresh_table_display
*      EXPORTING
*        is_stable = gw_stable.
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_generate_fikey
**&---------------------------------------------------------------------*
**& Form to generate reconcilliation FI key for document posting
**&---------------------------------------------------------------------*
*FORM f_generate_fikey  CHANGING    cv_fikey.
*
*  DATA: lw_return      TYPE bapiret2,
*        lv_reckey      TYPE fikey_kk,
*        lr_fikey       TYPE RANGE OF fikey_kk,
*        lv_seq_char    TYPE char4,
*        lv_sequence(4) TYPE n.
*
*  CONSTANTS : lc_e   TYPE char1 VALUE 'E',
*              lc_seq TYPE char4 VALUE '0001'.
*
****Concatenate E with MMDDYY to generate fi key
*  CONCATENATE lc_e sy-datum+4(2) sy-datum+6(2) sy-datum+2(2) '*' INTO DATA(lv_seq).
*
*  lr_fikey = VALUE #( ( sign   = 'I'
*                        option = 'CP'
*                        low    = lv_seq ) ).
*
****Prepare the new Reconciliation key in the format “M” + MMDDYY + “-sequence”
*  SELECT right( fikey , 4 ) AS sequence FROM dfkksumc  WHERE fikey IN @lr_fikey
*                                                         AND cpudt = @sy-datum
*                                                       INTO TABLE @DATA(lt_reckey).
*
*  IF sy-subrc = 0.
*    SORT lt_reckey DESCENDING BY sequence.
*    DATA(lv_last_reckey) = lt_reckey[ 1 ].
*    lv_sequence = lv_last_reckey + 1.
*    lv_seq_char = lv_sequence.
*  ELSE.
*    lv_seq_char = lc_seq.
*  ENDIF.
****Concatenate M with MMDDYY to generate fi key
*  CONCATENATE lc_e sy-datum+4(2) sy-datum+6(2) sy-datum+2(2) '-' lv_seq_char INTO lv_reckey.
*
****Check if the new key format exists in the system
*  CALL FUNCTION 'BAPI_CTRACRECKEY_EXISTCHECK'
*    EXPORTING
*      reconciliationkey = lv_reckey
*    IMPORTING
*      return            = lw_return.
****If key does not exist create new key
*  IF lw_return IS NOT INITIAL.
*    CALL FUNCTION 'BAPI_CTRACRECKEY_CREATE'
*      EXPORTING
*        newreconciliationkey = lv_reckey
*      IMPORTING
*        reconciliationkey    = cv_fikey
*        return               = lw_return.
*
****Commit the new key generation
*    COMMIT WORK.
*
*    WAIT UP TO 2 SECONDS.
*  ENDIF.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_fill_fkkko
**&---------------------------------------------------------------------*
**& Fill FKKKO table for BAPI "FKK_CREATE_DOC_AND_CLEAR"
**&---------------------------------------------------------------------*
*FORM f_fill_fkkko  USING  iv_fikey
*                  CHANGING cs_fkkko TYPE fkkko.
*
****Fill header FKKKO structor for BAPI "FKK_CREATE_DOC_AND_CLEAR
*  cs_fkkko-fikey   =  iv_fikey.
*  cs_fkkko-applk   =  gv_applk.
*  cs_fkkko-blart   =  gv_blart.
*  cs_fkkko-herkf   =  gv_herkf.
*  cs_fkkko-waers   =  p_curr.
*  cs_fkkko-bldat   =  p_postdt.
*  cs_fkkko-budat   =  p_postdt .
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_fill_fkkop
**&---------------------------------------------------------------------*
**& Fill FKKOP table for BAPI "FKK_CREATE_DOC_AND_CLEAR"
**&---------------------------------------------------------------------*
*FORM f_fill_fkkop  CHANGING it_fkkop TYPE tt_fkkop.
*
****Data declaration
*  DATA : lv_item_number TYPE opupk_kk VALUE 0,
*         lv_key01       TYPE keynn_kk,
*         lw_curr        TYPE icurr,
*         lt_hkont       TYPE tt_acctdet.
*
*  CONSTANTS : lc_segment  TYPE segmt_kk VALUE 'SEGMENT1'.
*
****Get hkont from config
*  PERFORM f_get_glaccount_fkkop CHANGING lt_hkont.
****Loop through BP final table into BAPI FKKOP table
*  LOOP AT gt_bpitem INTO DATA(lw_bpitem).
*
*    IF lw_bpitem-bukrs IS NOT INITIAL.
****increment item number
*      lv_item_number = lv_item_number + 1.
****Fill fkkop structure for final fkkop table
*      DATA(lw_fkkop) = VALUE fkkop( opupw  = 0
*                                    opupk  = lv_item_number
*                                    opupz  = 0
*                                    bukrs  = lw_bpitem-bukrs
*                                    gpart  = lw_bpitem-gpart
*                                    vkont  = lw_bpitem-vkont
*                                    applk  = gv_applk
*                                    hvorg  = gv_hvorg
*                                    kofiz  = gv_kofiz
*                                    bldat  = p_postdt
*                                    budat  = p_postdt
*                                    waers  = p_curr
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                              betrw = COND #( WHEN  p_curr IN gr_zerodec_curr THEN lw_bpitem-betrw / 100
*                                              ELSE lw_bpitem-betrw )
*                              skfbt = COND #( WHEN  p_curr IN gr_zerodec_curr THEN lw_bpitem-betrw / 100
*                                              ELSE lw_bpitem-betrw )
**                                    betrw  = lw_bpitem-betrw
**                                    skfbt  =  lw_bpitem-betrw
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                                    segment = lc_segment
*                                    blart = gv_blart
*                                    zzuonr =  lw_bpitem-zzuonr
*                                    optxt =  lw_bpitem-optxt
*                                    prctr =  lw_bpitem-prctr
*                                    xblnr =  lw_bpitem-xblnr ).
*      IF lw_bpitem-faedn IS INITIAL.
*        lw_fkkop-faedn  = p_postdt.
*        lw_fkkop-faeds  = p_postdt.
*      ELSE.
*        lw_fkkop-faedn  = lw_bpitem-faedn.
*        lw_fkkop-faeds  = lw_bpitem-faedn.
*      ENDIF.
*      lv_key01 = lw_bpitem-bukrs.
*      READ TABLE lt_hkont INTO DATA(lw_hkont) WITH KEY key01 = lv_key01 BINARY SEARCH.
*      IF sy-subrc = 0.
*        lw_fkkop-hkont  = lw_hkont-fun01.
*      ENDIF.
****Convert amount to local transaction currency
*      CLEAR : lw_curr.
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**      PERFORM f_curr_conv USING lw_bpitem-betrw
**                                lw_bpitem-bukrs
**                                p_curr
**                          CHANGING lw_curr.
*      PERFORM f_curr_conv USING lw_fkkop-betrw
*                                lw_fkkop-bukrs
*                                p_curr
*                          CHANGING lw_curr.
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*      lw_fkkop-betrh   = lw_curr-dmbtr.
*      lw_fkkop-betr2   = lw_curr-dmbe2.
*
****if debit
*      IF lw_fkkop-betrh > 0.
*        lw_fkkop-tvorg  = gv_tvorg_0010.
****If credit
*      ELSEIF lw_fkkop-betrh < 0.
*        lw_fkkop-tvorg  = gv_tvorg_0020.
*      ENDIF.
****append fkkop table
*      APPEND lw_fkkop TO it_fkkop.
*    ENDIF.
****Clear variables
*    CLEAR : lw_bpitem,lw_curr,lw_hkont,lv_key01.
*  ENDLOOP.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_fill_fkkopk
**&---------------------------------------------------------------------*
**& Fill FKKOP table for API "FKK_CREATE_DOC_AND_CLEAR"
**&---------------------------------------------------------------------*
*FORM f_fill_fkkopk  USING    iv_fikey
*                             iv_error
*                    CHANGING it_fkkopk TYPE tt_fkkopk.
*
****Data declaration
*  DATA : lt_intcomp     TYPE tt_intcomp,
*         lv_item_number TYPE opupk_kk VALUE 0,
*         lw_curr        TYPE icurr,
*         lv_share       TYPE augbt_kk,
*         lv_remaining   TYPE augbt_kk.
*
****Constant declaration
*  CONSTANTS : lc_y       TYPE char1 VALUE 'Y',
*              lc_segment TYPE segmt_kk VALUE 'SEGMENT1'.
*
****Fill final table FKKOPK from GL item table
*  LOOP AT gt_glitem INTO DATA(lw_glitem).
*    IF lw_glitem-bukrs IS NOT INITIAL.
****increment item number
*      lv_item_number = lv_item_number + 1.
*      DATA(lw_fkkopk) = VALUE fkkopk( opupk      = lv_item_number
*                                      bukrs      = lw_glitem-bukrs
*                                      hkont      = lw_glitem-hkont
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                                      betrw = COND #( WHEN    p_curr IN gr_zerodec_curr THEN lw_glitem-betrw / 100
*                                                      ELSE lw_glitem-betrw )
**                                      betrw      = lw_glitem-betrw
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                                      segment    = lc_segment
*                                      fikey      = iv_fikey
*                                      prctr      = lw_glitem-prctr
*                                      kostl      = lw_glitem-kostl
*                                      ps_psp_pnr = lw_glitem-ps_psp_pnr
*                                      kdauf      = lw_glitem-kdauf
*                                      aufnr      = lw_glitem-aufnr
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*                                      sgtxt      = lw_glitem-sgtxt
*                                      xeiph      = lw_glitem-xeiph
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*                                       ).
*
****Convert amount to local transaction currency
*      CLEAR : lw_curr.
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**      PERFORM f_curr_conv USING lw_glitem-betrw
**                                lw_glitem-bukrs
**                                p_curr
**                          CHANGING lw_curr.
*      PERFORM f_curr_conv USING lw_fkkopk-betrw
*                                lw_fkkopk-bukrs
*                                p_curr
*                          CHANGING lw_curr.
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*      lw_fkkopk-betrh   = lw_curr-dmbtr.
*      lw_fkkopk-betr2   = lw_curr-dmbe2.
*
*
****Append FKKOPK structure to final BAPI table FKKOPK
*      APPEND lw_fkkopk TO it_fkkopk.
*    ENDIF.
*    CLEAR : lw_glitem,lw_fkkopk.
*  ENDLOOP.
*
****Check if total difference amount is within tolerance limit
*  IF iv_error = lc_y.
****Get the company code from clearing items
*    PERFORM f_intcomp_bukrs_amount CHANGING lt_intcomp.
*
*    DATA(lv_line) = lines( lt_intcomp ).
****Loop through the company code wise amount
*    LOOP AT lt_intcomp INTO DATA(lw_intcomp).
****Calculate amount share for each company code
**      lv_share = ( gv_difference / lv_tot_ratio ) * lw_intcomp-augbt_ratio.
*      IF sy-tabix EQ lv_line.
*        lv_share = gv_difference - lv_remaining.
*      ELSE.
*        lv_share = gv_difference * lw_intcomp-augbt_ratio.
*        lv_remaining = lv_remaining + lv_share.
*      ENDIF.
*
*
****check if expense GL account or revenue GL needs to be taken
*      IF lv_share < 0.
*        DATA(lv_hkont) = gv_hkont_gl_rv.
*      ELSEIF lv_share > 0.
*        lv_hkont = gv_hkont_gl_ex.
*      ENDIF.
*      lv_item_number = lv_item_number + 1.
*
****Add extra GL line for posting
*      lw_fkkopk = VALUE fkkopk( opupk  = lv_item_number
*                                bukrs  = lw_intcomp-bukrs "lv_bukrs
*                                hkont  = lv_hkont
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                              betrw  = lv_share * -1
** End of change - 486861 Date: 10/0]1/2021 Defect # INC3781883/CFHYP-245
*                                segment = lc_segment
*                                fikey  = iv_fikey
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*                                sgtxt      = lw_glitem-sgtxt
*                                xeiph      = lw_glitem-xeiph
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*                                ).
****Convert amount to local transaction currency
*      CLEAR : lw_curr.
*      PERFORM f_curr_conv USING lv_share
*                                lw_intcomp-bukrs
*                                p_curr
*                          CHANGING lw_curr.
*      lw_fkkopk-betrh   = lw_curr-dmbtr * -1.
*      lw_fkkopk-betr2   = lw_curr-dmbe2 * -1.
****Append to final gl table
*      APPEND lw_fkkopk TO it_fkkopk.
****Clear variables
*      CLEAR : lv_share,lw_intcomp,
*              lw_curr,lw_fkkopk.
*
*    ENDLOOP.
*
*  ENDIF.
*
****Delete lines without GL account
*  DELETE it_fkkopk WHERE hkont IS INITIAL.
*
****Clear variables
*  CLEAR : lw_fkkopk,lv_hkont,lw_curr,"lw_intcomp,lt_intcomp,
*          lv_item_number ,lw_glitem,lv_share.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_fill_fkkcl
**&---------------------------------------------------------------------*
**& Fill FKKCL table for BAPI "FKK_CREATE_DOC_AND_CLEAR"
**&---------------------------------------------------------------------*
*FORM f_fill_fkkcl CHANGING it_fkkcl TYPE tt_fkkcl.
*
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*  TYPES: BEGIN OF ty_comp,
*           bukrs TYPE bukrs,
*         END OF ty_comp.
*  DATA : lt_comp     TYPE STANDARD TABLE OF ty_comp,
*         lt_comp_act TYPE STANDARD TABLE OF ty_comp,
*         lt_comp_inv TYPE STANDARD TABLE OF ty_comp.
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
****Data declaration
*  DATA : lw_curr  TYPE icurr.
*  DATA(lt_act) = gt_act.
*
*
*  SORT lt_act BY doc_no rep_item item sub_item.
*  IF lt_act IS NOT INITIAL.
*    SELECT opbel,
*           opupw,
*           opupk,
*           opupz,
*           zzuonr,
*           zzpo_type,
*           zzwht_initialize
*           INTO TABLE @DATA(lt_dfkkop_custom)
*           FROM dfkkop
*           FOR ALL ENTRIES IN @lt_act
*           WHERE opbel = @lt_act-doc_no
*           AND   opupw = @lt_act-rep_item
*           AND   opupk = @lt_act-item
*           AND   opupz = @lt_act-sub_item
*           ORDER BY PRIMARY KEY.
*    IF sy-subrc IS INITIAL.
** Do nothing
*    ENDIF.
*  ENDIF.
** Begin of change - 486861 Date: 10/05/2021 Defect # INC3781883/CFHYP-245
*  lt_comp_act =  VALUE #( FOR lw_act IN gt_act
*                           WHERE ( comp_code IS NOT INITIAL )
*                           ( bukrs  = lw_act-comp_code ) ).
*  lt_comp_inv = VALUE #( FOR lw_opinv IN gt_inv
*                           WHERE ( comp_code IS NOT INITIAL )
*                           ( bukrs  = lw_opinv-comp_code ) ).
*
*  APPEND LINES OF lt_comp_act TO lt_comp.
*  APPEND LINES OF lt_comp_inv TO lt_comp.
*  IF lt_comp IS NOT INITIAL.
*    SELECT bukrs,waers
*      FROM t001
*      INTO TABLE @DATA(lt_t001)
*      FOR ALL ENTRIES IN @lt_comp
*      WHERE bukrs = @lt_comp-bukrs.
*  ENDIF.
** End of change - 486861 Date: 10/05/2021 Defect # INC3781883/CFHYP-245
*
****Fill table FKKCL for BAPI "FKK_CREATE_DOC_AND_CLEAR" from table on-act table
*  LOOP AT gt_act INTO DATA(lw_onact).
*    IF lw_onact-checkbox = gc_x.
** Begin of change - 486861 Date: 10/05/2021 Defect # INC3781883/CFHYP-245
*      READ TABLE lt_t001 INTO DATA(lw_comp) WITH KEY bukrs = lw_onact-comp_code.
** End of change - 486861 Date: 10/05/2021 Defect # INC3781883/CFHYP-245
****Append field values to FKKCL table
*      DATA(lw_fkkcl) = VALUE fkkcl( opbel   = lw_onact-doc_no
*                                    opupw   = lw_onact-rep_item
*                                    opupk   = lw_onact-item
*                                    opupz   = lw_onact-sub_item
*                                    bukrs   = lw_onact-comp_code
*                                    gpart   = lw_onact-buspartner
*                                    vkont   = lw_onact-cont_acct
*                                    applk   = lw_onact-appl_area
*                                    hvorg   = lw_onact-main_trans
*                                    tvorg   = lw_onact-sub_trans
*                                    kofiz   = lw_onact-actdeterid
*                                    hkont   = lw_onact-g_l_acct
*                                    bldat   = lw_onact-doc_date
*                                    budat   = lw_onact-post_date
*                                    waers   = p_curr
*                                    faedn   = lw_onact-net_date
*                                    faeds   = lw_onact-disc_due
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**                                    betrh   = lw_onact-amount_loc_curr
*                                    betrh   = COND #( WHEN    lw_comp IS NOT INITIAL AND lw_comp-waers IN gr_zerodec_curr
*                                                      THEN lw_onact-amount_loc_curr / 100
*                                                      ELSE lw_onact-amount_loc_curr )
*                                    betrw   = COND #( WHEN    p_curr IN gr_zerodec_curr
*                                                      THEN lw_onact-amount_op / 100
*                                                      ELSE lw_onact-amount_op )
**                                    betrw   = lw_onact-amount_op
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                                    betr2   = lw_onact-amount_par2_curr
*                                    blart   = lw_onact-doc_type
*                                    augdt   = p_postdt
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**                                    augbw   = lw_onact-amount_cl
**                                    naugw   = lw_onact-amount_cl
*                                    augbw   = COND #( WHEN    p_curr IN gr_zerodec_curr
*                                                      THEN lw_onact-amount_cl / 100
*                                                      ELSE lw_onact-amount_cl )
*                                    naugw   = COND #( WHEN    p_curr IN gr_zerodec_curr
*                                                      THEN lw_onact-amount_cl / 100
*                                                      ELSE lw_onact-amount_cl )
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                                    segment = lw_onact-segment
*                                    prctr   = lw_onact-profit_ctr
*                                    gsber   = lw_onact-bus_area
*                                    bupla   = lw_onact-bus_place
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**                                    netth   = lw_onact-amount_loc_curr
*                                    netth   = COND #( WHEN    lw_comp IS NOT INITIAL AND lw_comp-waers IN gr_zerodec_curr
*                                                      THEN lw_onact-amount_loc_curr / 100
*                                                      ELSE lw_onact-amount_loc_curr )
**                                    nettw   = lw_onact-amount_op
*                                    nettw   = COND #( WHEN    p_curr IN gr_zerodec_curr
*                                                      THEN lw_onact-amount_op / 100
*                                                      ELSE lw_onact-amount_op )
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                                    nett2   = lw_onact-amount_par2_curr
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**                                    netto   = lw_onact-amount_oitem
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                                    qsshb   = lw_onact-w_tax_base
*                                    qbshb   = lw_onact-w_tax_amount
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**                                    pswbt   = lw_onact-amount_gl
*                                    pswsl   = lw_onact-currency_gl
*                                    pswbt   = COND #( WHEN    lw_onact-currency_gl  IN gr_zerodec_curr
*                                                      THEN lw_onact-amount_gl / 100
*                                                      ELSE lw_onact-amount_gl )
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                                    vtpos   = lw_onact-contract_pos
*                                    vtref   = lw_onact-contract
*                                    vtre2   = lw_onact-contract2
*                                    spart   = lw_onact-division
*                                    mwskz   = lw_onact-tax_code
*                                    xblnr   = lw_onact-ref_doc_no
*                                    sctax   = lw_onact-portion_tax_amount_loc
*                                    sttax   = lw_onact-stat_tax_amount
*                                    oriwa   = lw_onact-currency_oitem
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**                                    oribt   = lw_onact-amount_oitem
*                                    netto   = COND #( WHEN    lw_onact-currency_oitem  IN gr_zerodec_curr
*                                                      THEN lw_onact-amount_oitem / 100
*                                                      ELSE lw_onact-amount_oitem )
*                                    oribt   = COND #( WHEN    lw_onact-currency_oitem  IN gr_zerodec_curr
*                                                      THEN lw_onact-amount_oitem / 100
*                                                      ELSE lw_onact-amount_oitem )
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                                    xaktp   = gc_x
*                                    augrd   = gv_augrd
*                                    stakz   = lw_onact-stat_key
*                                    grkey   = lw_onact-grouping
*                                    opsta   = lw_onact-dun_indic
*                                    xragl   = lw_onact-reverseclg
*                                    qsskz   = lw_onact-w_tax_code
*                                    xdetv   = lw_onact-manual_distribution
*                                    xchng   = lw_onact-summ_item_chnged
*                                    verdp   = lw_onact-summ_item_number
*                                    azvdg   = lw_onact-doc_group_for_summ
*                                    qssta   = lw_onact-w_tax_status
*                                    optxt   = lw_onact-text
*                                    rfupk   = lw_onact-reference_item
*                                    qsptp   = lw_onact-w_tax_licat
*                                    ).
*      IF lw_onact-stat_key IS INITIAL.
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*        lw_fkkcl-skfbt  = COND #( WHEN    p_curr IN gr_zerodec_curr THEN lw_onact-amount_op / 100
*                                  ELSE lw_onact-amount_op )   .
**          lw_fkkcl-skfbt   = lw_onact-amount_op.
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*      ENDIF.
** Fill custom fields
*      TRY .
*          DATA(lw_custom_fields) = lt_dfkkop_custom[ opbel = lw_onact-doc_no
*                                                     opupw = lw_onact-rep_item
*                                                     opupk = lw_onact-item
*                                                     opupz = lw_onact-sub_item ].
*          lw_fkkcl-zzuonr           = lw_custom_fields-zzuonr.
*          lw_fkkcl-zzpo_type        = lw_custom_fields-zzpo_type.
*          lw_fkkcl-zzwht_initialize = lw_custom_fields-zzwht_initialize.
*        CATCH cx_sy_itab_line_not_found.
*
*      ENDTRY.
*
****Convert amount to local transaction currency
*      CLEAR : lw_curr, lw_custom_fields.
** Begin of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
*      PERFORM f_curr_conv USING lw_fkkcl-augbw
*                                lw_fkkcl-bukrs
*                                p_curr
*                          CHANGING lw_curr.
**        PERFORM f_curr_conv USING lw_onact-amount_cl
**                                  lw_onact-comp_code
**                                  p_curr
**                            CHANGING lw_curr.
** End of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
**      lw_fkkcl-augbh   = lw_curr-dmbtr.
*      IF lw_fkkcl-betrw IS NOT INITIAL.
*        lw_fkkcl-augbh   = lw_fkkcl-betrh * ( lw_fkkcl-augbw / lw_fkkcl-betrw ).
*      ENDIF.
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
*      lw_fkkcl-augb2   = lw_curr-dmbe2.
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
**      lw_fkkcl-naugh   = lw_curr-dmbtr.
*      IF lw_fkkcl-betrw IS NOT INITIAL.
*        lw_fkkcl-naugh   = lw_fkkcl-betrh * ( lw_fkkcl-naugw / lw_fkkcl-betrw ).
*      ENDIF.
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
*      lw_fkkcl-naug2   = lw_curr-dmbe2.
** Begin of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
*      lw_fkkcl-augbo = ( lw_fkkcl-netto / lw_fkkcl-betrw ) * lw_fkkcl-augbw.
*      lw_fkkcl-naugo = ( lw_fkkcl-netto / lw_fkkcl-betrw ) * lw_fkkcl-augbw.
**        lw_fkkcl-augbo = ( lw_onact-amount_oitem / lw_onact-amount_op ) * lw_onact-amount_cl.
**        lw_fkkcl-naugo = ( lw_onact-amount_oitem / lw_onact-amount_op ) * lw_onact-amount_cl.
** End of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
****Append fkkcl to table
*      APPEND lw_fkkcl TO it_fkkcl.
*    ENDIF.
****Clear variables
*    CLEAR : lw_onact,lw_fkkcl.
** Begin of change - 486861 Date: 10/05/2021 Defect # INC3781883/CFHYP-245
*    CLEAR : lw_comp.
** End of change - 486861 Date: 10/05/2021 Defect # INC3781883/CFHYP-245
*  ENDLOOP.
*
*
*  CLEAR : lw_fkkcl,lw_curr, lt_dfkkop_custom.
*  DATA(lt_inv) = gt_inv.
*  SORT lt_inv BY doc_no rep_item item sub_item.
*  IF lt_inv IS NOT INITIAL.
*    SELECT opbel,
*           opupw,
*           opupk,
*           opupz,
*           zzuonr,
*           zzpo_type,
*           zzwht_initialize
*           INTO TABLE @lt_dfkkop_custom
*           FROM dfkkop
*           FOR ALL ENTRIES IN @lt_inv
*           WHERE opbel = @lt_inv-doc_no
*           AND   opupw = @lt_inv-rep_item
*           AND   opupk = @lt_inv-item
*           AND   opupz = @lt_inv-sub_item
*           ORDER BY PRIMARY KEY.
*    IF sy-subrc IS INITIAL.
** Do nothing
*    ENDIF.
*  ENDIF.
****Fill table FKKCL for BAPI "FKK_CREATE_DOC_AND_CLEAR" from table open invoice table
*  LOOP AT gt_inv INTO DATA(lw_inv).
*    IF lw_inv-checkbox = gc_x.
** Begin of change - 486861 Date: 10/05/2021 Defect # INC3781883/CFHYP-245
*      READ TABLE lt_t001 INTO lw_comp WITH KEY bukrs = lw_inv-comp_code.
** End of change - 486861 Date: 10/05/2021 Defect # INC3781883/CFHYP-245
****Append field values to FKKCL table
*      lw_fkkcl = VALUE fkkcl( opbel   = lw_inv-doc_no
*                              opupw   = lw_inv-rep_item
*                              opupk   = lw_inv-item
*                              opupz   = lw_inv-sub_item
*                              bukrs   = lw_inv-comp_code
*                              gpart   = lw_inv-buspartner
*                              vkont   = lw_inv-cont_acct
*                              applk   = lw_inv-appl_area
*                              hvorg   = lw_inv-main_trans
*                              tvorg   = lw_inv-sub_trans
*                              kofiz   = lw_inv-actdeterid
*                              hkont   = lw_inv-g_l_acct
*                              bldat   = lw_inv-doc_date
*                              budat   = lw_inv-post_date
*                              waers   = p_curr
*                              faedn   = lw_inv-net_date
*                              faeds   = lw_inv-disc_due
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**                              betrh   = lw_inv-amount_loc_curr
*                              betrh   = COND #( WHEN    lw_comp IS NOT INITIAL AND lw_comp-waers IN gr_zerodec_curr
*                                                THEN lw_inv-amount_loc_curr / 100
*                                                ELSE lw_inv-amount_loc_curr )
**                                 betrw   = lw_inv-amount_op
*                              betrw   = COND #( WHEN   p_curr IN gr_zerodec_curr
*                                                THEN lw_inv-amount_op / 100
*                                                ELSE lw_inv-amount_op )
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                              betr2   = lw_inv-amount_par2_curr
*                              blart   = lw_inv-doc_type
*                              augdt   = p_postdt
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**                                 augbw   = lw_inv-amount_cl
**                                 naugw   = lw_inv-amount_cl
*                              augbw = COND #( WHEN    p_curr IN gr_zerodec_curr
*                                              THEN lw_inv-amount_cl / 100
*                                              ELSE lw_inv-amount_cl )
*                              naugw = COND #( WHEN    p_curr IN gr_zerodec_curr
*                                              THEN lw_inv-amount_cl / 100
*                                              ELSE lw_inv-amount_cl )
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                              segment = lw_inv-segment
*                              prctr   = lw_inv-profit_ctr
*                              gsber   = lw_inv-bus_area
*                              bupla   = lw_inv-bus_place
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**                             netth   = lw_inv-amount_loc_curr
*                              netth   = COND #( WHEN    lw_comp IS NOT INITIAL AND lw_comp-waers IN gr_zerodec_curr
*                                                THEN lw_inv-amount_loc_curr / 100
*                                                ELSE lw_inv-amount_loc_curr )
**                                 nettw   = lw_inv-amount_op
*                             nettw = COND #( WHEN    p_curr IN gr_zerodec_curr
*                                             THEN lw_inv-amount_op / 100
*                                             ELSE lw_inv-amount_op )
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                              nett2   = lw_inv-amount_par2_curr
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**                              netto   = lw_inv-amount_oitem
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                              qsshb = lw_inv-w_tax_base
*                              qbshb = lw_inv-w_tax_amount
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**                              pswbt = lw_inv-amount_gl
*                              pswbt   = COND #( WHEN lw_inv-currency_gl  IN gr_zerodec_curr
*                                                THEN lw_inv-amount_gl / 100
*                                                ELSE lw_inv-amount_gl )
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                              pswsl = lw_inv-currency_gl
*                              vtpos = lw_inv-contract_pos
*                              vtref = lw_inv-contract
*                              vtre2 = lw_inv-contract2
*                              spart = lw_inv-division
*                              mwskz = lw_inv-tax_code
*                              xblnr = lw_inv-ref_doc_no
*                              sctax = lw_inv-portion_tax_amount_loc
*                              sttax = lw_inv-stat_tax_amount
*                              oriwa = lw_inv-currency_oitem
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
**                                    oribt   = lw_inv-amount_oitem
*                              netto   = COND #( WHEN    lw_inv-currency_oitem  IN gr_zerodec_curr
*                                                THEN lw_inv-amount_oitem / 100
*                                                ELSE lw_inv-amount_oitem )
*                              oribt   = COND #( WHEN    lw_inv-currency_oitem  IN gr_zerodec_curr
*                                                THEN lw_inv-amount_oitem / 100
*                                                ELSE lw_inv-amount_oitem )
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*                              xaktp   = gc_x
*                              augrd   = gv_augrd
*                              stakz   = lw_inv-stat_key
*                              grkey   = lw_inv-grouping
*                              opsta   = lw_inv-dun_indic
*                              xragl   = lw_inv-reverseclg
*                              qsskz   = lw_inv-w_tax_code
*                              xdetv   = lw_inv-manual_distribution
*                              xchng   = lw_inv-summ_item_chnged
*                              verdp   = lw_inv-summ_item_number
*                              azvdg   = lw_inv-doc_group_for_summ
*                              qssta   = lw_inv-w_tax_status
*                              optxt   = lw_inv-text
*                              rfupk   = lw_inv-reference_item
*                              qsptp   = lw_inv-w_tax_licat
*                              ).
*      IF lw_inv-stat_key IS INITIAL.
** Begin of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*        lw_fkkcl-skfbt  = COND #( WHEN    p_curr IN gr_zerodec_curr THEN lw_inv-amount_op / 100
*                                  ELSE lw_inv-amount_op )   .
**        lw_fkkcl-skfbt   = lw_inv-amount_op.
** End of change - 486861 Date: 10/01/2021 Defect # INC3781883/CFHYP-245
*      ENDIF.
** Fill custom fields
*      TRY .
*          lw_custom_fields = lt_dfkkop_custom[ opbel = lw_inv-doc_no
*                                               opupw = lw_inv-rep_item
*                                               opupk = lw_inv-item
*                                               opupz = lw_inv-sub_item ].
*          lw_fkkcl-zzuonr           = lw_custom_fields-zzuonr.
*          lw_fkkcl-zzpo_type        = lw_custom_fields-zzpo_type.
*          lw_fkkcl-zzwht_initialize = lw_custom_fields-zzwht_initialize.
*        CATCH cx_sy_itab_line_not_found.
*
*      ENDTRY.
****Convert amount to local transaction currency
*      CLEAR : lw_curr, lw_custom_fields.
** Begin of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
*      PERFORM f_curr_conv USING lw_fkkcl-augbw
*                                lw_fkkcl-bukrs
*                                p_curr
*                          CHANGING lw_curr.
**          PERFORM f_curr_conv USING lw_inv-amount_cl
**                                    lw_inv-comp_code
**                                    p_curr
**                              CHANGING lw_curr.
** End of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
**      lw_fkkcl-augbh   = lw_curr-dmbtr.
*      IF lw_fkkcl-betrw IS NOT INITIAL.
*        lw_fkkcl-augbh   = lw_fkkcl-betrh * ( lw_fkkcl-augbw / lw_fkkcl-betrw ).
*      ENDIF.
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
*      lw_fkkcl-augb2   = lw_curr-dmbe2.
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
**      lw_fkkcl-naugh   = lw_curr-dmbtr.
*      IF lw_fkkcl-betrw IS NOT INITIAL.
*        lw_fkkcl-naugh   = lw_fkkcl-betrh * ( lw_fkkcl-naugw / lw_fkkcl-betrw ).
*      ENDIF.
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-388 : CF.ENH.762C/Defect-312,331
*      lw_fkkcl-naug2   = lw_curr-dmbe2.
** Begin of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
*      lw_fkkcl-augbo = ( lw_fkkcl-netto / lw_fkkcl-betrw ) * lw_fkkcl-augbw.
*      lw_fkkcl-naugo = ( lw_fkkcl-netto / lw_fkkcl-betrw ) * lw_fkkcl-augbw.
**          lw_fkkcl-augbo = ( lw_inv-amount_oitem / lw_inv-amount_op ) * lw_inv-amount_cl.
**          lw_fkkcl-naugo = ( lw_inv-amount_oitem / lw_inv-amount_op ) * lw_inv-amount_cl.
** End of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
*      APPEND lw_fkkcl TO it_fkkcl.
*      CLEAR:lw_fkkcl.
*    ENDIF.
*    CLEAR : lw_inv.
** Begin of change - 486861 Date: 10/05/2021 Defect # INC3781883/CFHYP-245
*    CLEAR : lw_comp.
** End of change - 486861 Date: 10/05/2021 Defect # INC3781883/CFHYP-245
*  ENDLOOP.
*
*  IF it_fkkcl[] IS INITIAL.
*    MESSAGE e167.
*    RETURN.
*  ENDIF.
** Begin of change - 486861 Date: 10/05/2021 Defect # INC3781883/CFHYP-245
*  CLEAR : lt_comp_act,lt_comp_inv,lt_comp,lt_t001.
** End of change - 486861 Date: 10/05/2021 Defect # INC3781883/CFHYP-245
*  CLEAR: lw_curr,lw_fkkcl.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_curr_conv
**&---------------------------------------------------------------------*
**& Form for currency conversion to local currency
**&---------------------------------------------------------------------*
*FORM f_curr_conv  USING    iv_amount_cl
*                           iv_comp_code
*                           iv_curr
*                  CHANGING cs_curr.
*
****Object reference declaration
*  DATA(lr_inst) = NEW cl_epic_ebr_fi_service( ).
*****Read the local and parallel currencies for the company code and PERFORM the currency conversion
*  DATA(lw_curr) = VALUE icurr( wrbtr = iv_amount_cl
*                                waers = iv_curr
*                                wwert = p_postdt
*                                bukrs = iv_comp_code ).
****Call method to convert currency
*  lr_inst->currencies_convert( EXPORTING  is_curr      = lw_curr
*                               IMPORTING  es_curr      = cs_curr
*                               EXCEPTIONS system_error = 1
*                               OTHERS                  = 2 ).
*  IF sy-subrc <> 0.
*    "Do nothing
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_post_document
**&---------------------------------------------------------------------*
**& Form to post FICA document
**&---------------------------------------------------------------------*
*FORM f_post_document  USING   it_fkkop  TYPE tt_fkkop
*                              it_fkkopk TYPE tt_fkkopk
*                              it_fkkcl  TYPE tt_fkkcl
*                              is_fkkko
*                              iv_fikey.
****Data declaration
*  DATA: lt_enqtab TYPE TABLE OF ienqtab,
*        lw_enqtab TYPE ienqtab,
*        lv_docno  TYPE opbel_kk.
*
****Fill enque table for business partner
*  LOOP AT it_fkkcl INTO DATA(lw_fkkcl) .
*    lw_enqtab = VALUE #(   gpart = lw_fkkcl-gpart
*                           bukrs = lw_fkkcl-bukrs ).
*
*    APPEND lw_enqtab TO lt_enqtab.
*    CLEAR : lw_enqtab.
*    lw_enqtab = VALUE #(   vkont = lw_fkkcl-vkont
*                           bukrs = lw_fkkcl-bukrs ).
*    APPEND lw_enqtab TO lt_enqtab.
*    CLEAR : lw_enqtab.
*  ENDLOOP.
*
*  IF it_fkkop IS NOT INITIAL.
*    LOOP AT it_fkkop INTO DATA(lw_fkkop).
*
*      lw_enqtab = VALUE #(   gpart = lw_fkkop-gpart
*                             bukrs = lw_fkkop-bukrs ).
*      APPEND lw_enqtab TO lt_enqtab.
*      CLEAR : lw_enqtab.
*
*      lw_enqtab = VALUE #(   vkont = lw_fkkop-vkont
*                             bukrs = lw_fkkop-bukrs ).
*      APPEND lw_enqtab TO lt_enqtab.
*
*      CLEAR : lw_fkkop.
*    ENDLOOP.
*  ENDIF.
****Call function module to enque all business partners for document posting
*  CALL FUNCTION 'FKK_OPEN_ITEM_ENQUEUE'
*    TABLES
*      t_enqtab = lt_enqtab.
*
****Check if someone blocks the business partner/contract account/company code
*  CLEAR : lw_enqtab.
*  DELETE lt_enqtab WHERE xenqe IS INITIAL.
*  READ TABLE lt_enqtab INTO lw_enqtab INDEX 1.
*  IF NOT lw_enqtab-xenqe IS INITIAL OR lw_enqtab-xenqm = 'X'.
*    MESSAGE e162 WITH lw_enqtab-uname.
*  ENDIF.
*
****Call BAPI to post document
*  CALL FUNCTION 'FKK_CREATE_DOC_AND_CLEAR'
*    EXPORTING
*      i_fkkko       = is_fkkko
*      i_update_task = gc_x
*    IMPORTING
*      e_opbel       = lv_docno
*    TABLES
*      t_fkkop       = it_fkkop
*      t_fkkopk      = it_fkkopk
*      t_fkkcl       = it_fkkcl
*    EXCEPTIONS
*      error_message = 1.
****Call BAPI commit with FI key close if document created successfully
*  IF sy-subrc = 0 AND lv_docno IS NOT INITIAL.
****Commit the poting
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*
*    CALL FUNCTION 'BAPI_CTRACRECKEY_CLOSE'
*      EXPORTING
*        reconciliationkey = iv_fikey.
****Commit the key closure
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*
****Call function module to enque all business partners for document posting
*    CALL FUNCTION 'FKK_OPEN_ITEM_DEQUEUE'.
*
****Clear global variables
*    PERFORM f_clear_global_var.
****    Check if any exception occured in update task
*    IF it_fkkcl IS NOT INITIAL.
*      SELECT opbel
*            FROM dfkkop
*        INTO TABLE @DATA(lt_dfkkop) ##NEEDED
*        FOR ALL ENTRIES IN @it_fkkcl
*        WHERE opbel = @it_fkkcl-opbel
*          AND   opupk = @it_fkkcl-opupk
*          AND   opupw = @it_fkkcl-opupw
*          AND   opupz = @it_fkkcl-opupz
*          AND   betrw = @it_fkkcl-betrw
*          AND   augwa = @space.
*      IF sy-subrc <> 0.
****Raise success message with document number
*        MESSAGE s159 WITH lv_docno.
*      ELSE.
****Raise success message with document number
*        MESSAGE e185 .
*      ENDIF.
*    ENDIF.
*
****Go back to selection screen after document cretaion
*    SET SCREEN 0.
*    LEAVE SCREEN.
*  ELSE.
****If document not posted raise error message
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
****Call function module to enque all business partners for document posting
*    CALL FUNCTION 'FKK_OPEN_ITEM_DEQUEUE'.
*
****Display error message in popup
*    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
*      EXPORTING
*        i_msgid = sy-msgid
*        i_msgty = sy-msgty
*        i_msgno = sy-msgno
*        i_msgv1 = sy-msgv1
*        i_msgv2 = sy-msgv2
*        i_msgv3 = sy-msgv3
*        i_msgv4 = sy-msgv4.
*
****Go back to selection screen after document cretaion
*    SET SCREEN 0.
*    LEAVE SCREEN.
*
*  ENDIF.
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_check_tolerance_amout
**&---------------------------------------------------------------------*
**& Check if difference amount is within tolerance limit for posting
**&---------------------------------------------------------------------*
*FORM f_check_tolerance_amount  CHANGING cv_error.
*
*  DATA: lv_debit      TYPE augbt_kk,
*        lv_credit     TYPE augbt_kk,
*        lv_difference TYPE augbt_kk,
*        lw_curr       TYPE icurr,
*        lv_dbperc     TYPE betrw_kk,
*        lv_augbt_act  TYPE betrw_kk,
*        lv_augbt_inv  TYPE betrw_kk.
*
*  CONSTANTS : lc_yellow TYPE char1 VALUE 'Y',
*              lc_red    TYPE char1 VALUE 'R'.
*****get total debit and total credit amount based on tabs
*  PERFORM f_get_debit_credit CHANGING lv_debit lv_credit.
*
****Get tolerance group detail from table TFK043
*  SELECT  togru,
*          waers,
*          difbh,
*          difbs,
*          difph,
*          difps,
*          xundp
*    FROM tfk043
*    INTO @DATA(lw_tolerance)
*    UP TO 1 ROWS
*    WHERE togru = @gv_tolgrp.
*  ENDSELECT.
*  IF sy-subrc = 0.
*
** Begin of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
***For currency conversion
*    lv_debit = COND #( WHEN p_curr IN gr_zerodec_curr
*                        THEN lv_debit / 100
*                        ELSE  lv_debit )    .
*
*    lv_credit = COND #( WHEN p_curr IN gr_zerodec_curr
*                        THEN lv_credit / 100
*                        ELSE  lv_credit )    .
*
** End of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
****Convert debit amount to tolerance currency
*    PERFORM f_curr_conv USING lv_debit
*                              gv_bukrs_3000
*                               p_curr
*                CHANGING lw_curr.
*    CLEAR lv_debit .
****Convert to local currency
*    lv_debit = lw_curr-dmbtr.
*    CLEAR : lw_curr.
****Convert credit amount to tolerance currency
*    PERFORM f_curr_conv USING lv_credit
*                              gv_bukrs_3000
*                              p_curr
*                     CHANGING lw_curr.
****Convert to local currency
*    CLEAR lv_credit .
*    lv_credit = lw_curr-dmbtr.
*    CLEAR : lw_curr.
*
****Convert difference amount to tolerance currency
*    PERFORM f_curr_conv USING gv_difference
*                              gv_bukrs_3000
*                              p_curr
*                     CHANGING lw_curr.
****Convert to local currency
*    CLEAR lv_difference .
*    lv_difference = lw_curr-dmbtr.
*    CLEAR : lw_curr.
*
****Check if debit amount is greater than credit amount
*    IF lv_debit > lv_credit.
****Take expense percentage of the debit amount
*      lv_dbperc = ( lv_debit * lw_tolerance-difph ) / 100.
****Check if calculated percentage amount is higher than expense amount
*      IF lv_dbperc > lw_tolerance-difbh.
****Expense amount is the max allowed tolerance
*        IF abs( lv_difference ) > lw_tolerance-difbh.
****If difference amount is greater than expense amount raise
****Red traffic light
*          cv_error = lc_red.
*        ELSE.
****If difference amount is smaller than expense amount raise
****Yellow traffic light
*          cv_error = lc_yellow.
*        ENDIF.
*      ELSE.
****Calculated percentage amount of debit is max allowed tolerance
*        IF lv_dbperc >= abs( lv_debit - lv_credit ).
****If percentage amount is greater than difference amount
****Yellow traffic light
*          cv_error = lc_yellow.
*        ELSE.
****If percentage amount is smaller than difference amount
****Red traffic light
*          cv_error = lc_red.
*        ENDIF.
*      ENDIF.
*    ELSE.
****Take revenue percentage of the crebit amount if credit amount is higher than debit
*      lv_dbperc = ( lv_credit * lw_tolerance-difps ) / 100.
****Check if calculated percentage amount is higher than revenue amount
*      IF lv_dbperc > lw_tolerance-difbs.
****Revenue amount is the max allowed tolerance
*        IF abs( lv_difference ) > lw_tolerance-difbs.
****If difference amount is greater than revenue amount raise
****Red traffic light
*          cv_error = lc_red.
*        ELSE.
****If difference amount is smaller than revenue amount raise
****Yellow traffic light
*          cv_error = lc_yellow.
*        ENDIF.
*      ELSE.
****Calculated percentage amount of credit is the max allowed tolerance
*        IF lv_dbperc >= abs( lv_credit - lv_debit ).
****If percentage amount is greater than difference amount
****Yellow traffic light
*          cv_error = lc_yellow.
*        ELSE.
****If percentage amount is smaller than difference amount
****Red traffic light
*          cv_error = lc_red.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form F_GET_CONSTANTS
**&---------------------------------------------------------------------*
**& Fetch the constants for the program
**&---------------------------------------------------------------------*
*FORM f_get_constants  USING   ip_pgmid TYPE char40.
*
**** Get the Constants for the program
*  CALL FUNCTION 'ZUTIL_PGM_CONSTANTS'
*    EXPORTING
*      im_pgmid               = ip_pgmid
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
*        MESSAGE e007 WITH 'TVARVC'(t01).
*      WHEN 2.
*        MESSAGE e010 WITH 'TVARVC'(t01).
*      WHEN OTHERS.
*    ENDCASE.
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form F_COLLECT_CONSTANTS
**&---------------------------------------------------------------------*
**& Collect the value of the Constants in variables
**&---------------------------------------------------------------------*
*FORM f_collect_constants .
*
***** Read the constants
*  READ TABLE gt_pgm_const_values INTO DATA(lw_pgm_const_values) WITH KEY const_name = TEXT-t02.
*  IF sy-subrc = 0.
*    gv_maintrs = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_HVORG_0060'(t02).     "Constant P_HVORG_0060 not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t04.
*  IF sy-subrc = 0.
*    gv_type = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_BLART_DZ'(t04).     "Constant P_BLART_DZ not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t05.
*  IF sy-subrc = 0.
*    gv_tolgrp = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_TOGRU_A001'(t05).     "Constant P_TOGRU_A001 not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t06.
*  IF sy-subrc = 0.
*    gv_applk = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_APPLK_S'(t06).     "Constant P_APPLK_S not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t07.
*  IF sy-subrc = 0.
*    gv_hvorg = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_HVORG_ZPAY'(t07).     "Constant P_HVORG_ZMIG not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t08.
*  IF sy-subrc = 0.
*    gv_tvorg_0010 = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_TVORG_0010'(t08).     "Constant P_TVORG_0115 not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t09.
*  IF sy-subrc = 0.
*    gv_kofiz = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_KOFIZ_A0'(t09).     "Constant P_KOFIZ_A0 not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t10.
*  IF sy-subrc = 0.
*    gv_blart = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_BLART_AB'(t10).     "Constant P_BLART_AB not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t11.
*  IF sy-subrc = 0.
*    gv_augrd = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_AUGRD_08'(t11).     "Constant P_AUGRD_08 not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t12.
*  IF sy-subrc = 0.
*    gv_herkf = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_HERKF_03'(t12).     "Constant P_HERKF_01 not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t14.
*  IF sy-subrc = 0.
*    gv_buber = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_BUBER_0060'(t14).     "Constant P_BUBER_0060 not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t15.
*  IF sy-subrc = 0.
*    gv_ktopl = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_KTOPL_ADCA'(t15).     "Constant P_KTOPL_ADCA not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t16.
*  IF sy-subrc = 0.
*    gv_buber_s000 = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_BUBER_S000'(t16).     "Constant P_HVORG_0060 not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t18.
*  IF sy-subrc = 0.
*    gv_tvorg_0020 = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_TVORG_0020'(t18).     "Constant P_TVORG_0020 not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t19.
*  IF sy-subrc = 0.
*    gv_bukrs_3000 = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_COMP_3000'(t19).     "Constant P_COMP_3000 not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t20.
*  IF sy-subrc = 0.
*    gv_cl_split_no = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_CLEARING_SPLIT_NO'(t20).     "Constant P_CLEARING_SPLIT_NO not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t21.
*  IF sy-subrc = 0.
*    gv_bp_split_no = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_BP_SPLIT_NO'(t21).     "Constant P_BP_SPLIT_NO not maintained in the ZTUTILITY_CONST table
*  ENDIF.
*
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t22.
*  IF sy-subrc = 0.
*    gv_gl_split_no = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_GL_SPLIT_NO'(t22).     "Constant P_GL_SPLIT_NO not maintained in the ZTUTILITY_CONST table
*  ENDIF.
**GL Account
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t24.
*  IF sy-subrc = 0.
*    gv_hkont_gl = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_HKONT_KK_POST_GL'(t24).     "Constant P_HKONT_KK_POST_GL not maintained in the ZTUTILITY_CONST table
*  ENDIF.
** Begin of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
****Zero decimal currency
*  TRY .
*      gr_zerodec_curr = VALUE #( FOR lw_pgm_const IN gt_pgm_const_values
*                          WHERE ( const_name = 'S_ZERODEC_CURR' )
*                          ( sign   = 'I'
*                            option = 'EQ'
*                            low    = lw_pgm_const-low ) ).
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025(zfi_msgs) WITH 'S_ZERODEC_CURR'(065).
*  ENDTRY.
** End of change - 486861 Date: 10/04/2021 Defect # INC3781883/CFHYP-245
** Begin of change - 486861 Date: 10/11/2021 Defect # INC3765130
****Cost center
*  CLEAR : lw_pgm_const_values.
*  READ TABLE gt_pgm_const_values INTO lw_pgm_const_values WITH KEY const_name = TEXT-t25.
*  IF sy-subrc = 0.
*    gv_kokrs = lw_pgm_const_values-low.
*  ELSE.
*    MESSAGE e025 WITH 'P_KOKRS_CSHAPP'(t25).     "Constant P_KOKRS_CSHAPP not maintained in the ZTUTILITY_CONST table
*  ENDIF.
****WBS element status
*  TRY .
*      gr_wbs_status = VALUE #( FOR lw_pgm_const IN gt_pgm_const_values
*                       WHERE ( const_name = TEXT-t26 ) "'S_WBS_STATUS_CSHAPP' )
*                             ( sign   = 'I'
*                               option = 'EQ'
*                               low    = lw_pgm_const-low ) ).
*    CATCH cx_sy_itab_line_not_found .
*      MESSAGE e025(zfi_msgs) WITH 'S_WBS_STATUS_CSHAPP'(t26).
*  ENDTRY.
** End of change - 486861 Date: 10/11/2021 Defect # INC3765130
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_clear_global_var
**&---------------------------------------------------------------------*
**& Clear all global variables.
**&---------------------------------------------------------------------*
*FORM f_clear_global_var .
*
****Clear global tables entries
*  CLEAR :    gt_item[],
*             gt_bpitem[],
*             gt_glitem[],
*             gt_act[],
*             gt_inv[],
****Clear global variables
*             gv_buspar,
*             gv_conact,
*             gv_comp,
*             gv_fidoc,
*             gv_inv,
*             gv_ponum,
*             gv_augbt_act_cl,
*             gv_augbt_inv_cl,
*             gv_augbt_act_ps,
*             gv_augbt_inv_ps,
*             gv_augbt_bp,
*             gv_augbt_gl,
*             gv_difference,
*             gv_waers_act_cl,
*             gv_waers_inv_cl,
*             gv_waers_act_ps,
*             gv_waers_inv_ps,
*             gv_waers_bp,
*             gv_waers_gl,
*             gv_diff_curr,
*             gv_ok_code,
*             gv_status,
*             gv_error.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_get_difference_gl
**&---------------------------------------------------------------------*
**& Get GL account for difference amount
**&---------------------------------------------------------------------*
*FORM f_get_difference_gl .
*
****Get difference gl account from config
*  SELECT  fun02,
*          fun03
*    FROM tfk033d
*    INTO (@gv_hkont_gl_ex,@gv_hkont_gl_rv)
*    UP TO 1 ROWS
*    WHERE applk = @gv_applk
*    AND buber = @gv_buber
*    AND ktopl = @gv_ktopl
*    ORDER BY PRIMARY KEY.
*  ENDSELECT.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_get_glaccount_fkkop
**&---------------------------------------------------------------------*
**& Find GL account from config for posting area S000
**&---------------------------------------------------------------------*
*FORM f_get_glaccount_fkkop  CHANGING ct_hkont TYPE tt_acctdet.
*
*  TYPES : BEGIN OF ty_key01,
*            key01 TYPE char12,
*          END OF ty_key01.
*
*  DATA : lt_key01 TYPE STANDARD TABLE OF ty_key01,
*         ls_key01 TYPE ty_key01.
*
****Get company code
*  LOOP AT gt_bpitem INTO DATA(lw_bpitem).
*    ls_key01-key01  = lw_bpitem-bukrs.
*    APPEND ls_key01 TO lt_key01.
*    CLEAR lw_bpitem.
*  ENDLOOP.
*
*  IF lt_key01 IS NOT INITIAL.
*
****Get bp item gl account from config
*    SELECT applk,
*          buber,
*          ktopl,
*          key01,
*          key02,
*          key03,
*          key04,
*          key05,
*          key06,
*          key07,
*          key08,
*          fun01
*      FROM tfk033d
*      INTO TABLE @ct_hkont
*      FOR ALL ENTRIES IN @lt_key01
*      WHERE applk = @gv_applk
*      AND buber = @gv_buber_s000
*      AND ktopl = @gv_ktopl
*      AND key01 = @lt_key01-key01
*      AND key03 = @gv_kofiz
*      AND key04 = @gv_hvorg.
*    IF sy-subrc EQ 0.
*      SORT ct_hkont BY key01.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_screen_modification
**&---------------------------------------------------------------------*
**& Form to make input field for file editable or uneditable
**&---------------------------------------------------------------------*
*FORM f_screen_modification .
*
****Make input field for file editable or uneditable
****based on checkbox
*  LOOP AT SCREEN.
*    IF p_check = abap_true.
*      IF screen-name = 'P_BPFILE'.
*        screen-input = 1.
*        MODIFY SCREEN.
*        EXIT.
*      ENDIF.
*    ELSE.
*      IF screen-name = 'P_BPFILE'.
*        CLEAR : p_bpfile.
*        screen-input = 0.
*        MODIFY SCREEN.
*        EXIT.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_intcomp_bukrs_amount
**&---------------------------------------------------------------------*
**& Calculate proportionate share of tolerance amount for each BUKRS
**&---------------------------------------------------------------------*
*FORM f_intcomp_bukrs_amount CHANGING ct_intcomp TYPE tt_intcomp.
*
**** types for BUKRS OPBEL amount data
*  TYPES : BEGIN OF ty_intcomp_doc,
*            bukrs TYPE bukrs,
*            opbel TYPE opbel_kk,
*            augbt TYPE augbt_kk,
*          END OF ty_intcomp_doc.
*
*  DATA : lt_inv          TYPE STANDARD TABLE OF ty_oninv,
*         lt_intcomp      TYPE STANDARD TABLE OF ty_intcomp_doc,
*         lt_intcomp_temp TYPE STANDARD TABLE OF ty_intcomp_doc,
*         lv_amt_cl       TYPE augbt_kk,
*         lv_amt_tot      TYPE augbt_kk.
****Fill temporary table for calculation
*  lt_inv[] = gt_inv[].
*
****fill inter company table for calculation
*  LOOP AT lt_inv INTO DATA(lw_inv) WHERE checkbox = abap_true.
*    APPEND INITIAL LINE TO lt_intcomp ASSIGNING FIELD-SYMBOL(<fs_intcomp>).
*    <fs_intcomp>-bukrs = lw_inv-comp_code.
*    <fs_intcomp>-opbel = lw_inv-doc_no.
*    <fs_intcomp>-augbt = lw_inv-amount_cl.
****Clear work area
*    CLEAR : lw_inv.
*  ENDLOOP.
*
*  UNASSIGN : <fs_intcomp>.
*
*  IF lt_intcomp IS NOT INITIAL.
****Sort table with comp code and doc number
*    SORT lt_intcomp BY bukrs opbel.
****fill table with subtotal values based on document number
*    LOOP AT lt_intcomp INTO DATA(lw_intcomp).
*      lv_amt_cl = lv_amt_cl + lw_intcomp-augbt.
****When new document number triggered append to temporary internal table
*      AT END OF opbel.
*        APPEND INITIAL LINE TO lt_intcomp_temp ASSIGNING <fs_intcomp>.
*        <fs_intcomp> = lw_intcomp.
*        <fs_intcomp>-augbt = lv_amt_cl.
*        CLEAR : <fs_intcomp>-opbel.
*        CLEAR : lv_amt_cl,lw_intcomp.
*      ENDAT.
*    ENDLOOP.
*
*    UNASSIGN : <fs_intcomp>.
*
*    DELETE lt_intcomp_temp WHERE augbt < 0.
*    SORT lt_intcomp_temp BY bukrs.
****fill table with subtotal values based on company code
*    LOOP AT lt_intcomp_temp INTO lw_intcomp.
*      lv_amt_cl = lv_amt_cl + lw_intcomp-augbt.
*      lv_amt_tot = lv_amt_tot + lw_intcomp-augbt.
****At the end of comp code add into exporting table
*      AT END OF bukrs.
*        APPEND INITIAL LINE TO ct_intcomp ASSIGNING FIELD-SYMBOL(<fs_comp>).
*        <fs_comp>-bukrs = lw_intcomp-bukrs.
*        <fs_comp>-augbt = lv_amt_cl.
*        CLEAR : lv_amt_cl,lw_intcomp.
*      ENDAT.
*    ENDLOOP.
*  ENDIF.
*
*  UNASSIGN : <fs_comp>.
*
*  IF ct_intcomp IS NOT INITIAL.
*    SORT ct_intcomp BY augbt ASCENDING.
****Calculate and update ratio values for amount proportion
*    LOOP AT ct_intcomp ASSIGNING <fs_comp>.
**      IF sy-tabix = 1.
*****Take the minimum amount for ratio calculation
**        lv_amt_cl = <fs_comp>-augbt.
**      ENDIF.
*
*      <fs_comp>-augbt_ratio = <fs_comp>-augbt / lv_amt_tot.
****Calculate total ratio
**      cv_tot_ratio = cv_tot_ratio + <fs_comp>-augbt_ratio.
*    ENDLOOP.
*  ENDIF.
****Clear variables.
*  CLEAR : lv_amt_cl,lw_intcomp,lt_inv,
*          lt_intcomp,lt_intcomp_temp.
*ENDFORM.
**---------------------------------------------------------------------*
**       CLASS lcl_alv_events IMPLEMENTATION
**---------------------------------------------------------------------*
**
**---------------------------------------------------------------------*
*CLASS lcl_alv_events IMPLEMENTATION.
**---------------------------------------------------------------------*
**       METHOD data_changed_act                                       *
**---------------------------------------------------------------------*
*  METHOD data_changed_act.
*    PERFORM f_event_data_changed_act USING et_good_cells.
*
*
*  ENDMETHOD.                    "data_changed
**---------------------------------------------------------------------*
**       METHOD data_changed_inv                                       *
**---------------------------------------------------------------------*
*  METHOD data_changed_inv.
*    PERFORM f_event_data_changed_inv USING et_good_cells.
*
*  ENDMETHOD.                    "data_changed
**---------------------------------------------------------------------*
**       METHOD data_changed_bp                                        *
**---------------------------------------------------------------------*
*  METHOD data_changed_bp.
*    PERFORM f_event_data_changed_bp USING et_good_cells.
*
*  ENDMETHOD.                    "data_changed
**---------------------------------------------------------------------*
**       METHOD data_changed_gl                                        *
**---------------------------------------------------------------------*
*  METHOD data_changed_gl.
*    PERFORM f_event_data_changed_gl USING et_good_cells.
*
*  ENDMETHOD.                    "data_changed
**---------------------------------------------------------------------*
**       METHOD on_toolbar                                             *
**---------------------------------------------------------------------*
*  METHOD on_toolbar.
*    PERFORM f_on_toolbar USING e_object.
*  ENDMETHOD.                    "on_toolbar
**---------------------------------------------------------------------*
**       METHOD handle_user_command_act                                *
**---------------------------------------------------------------------*
*  METHOD handle_user_command_act .
*    PERFORM f_handle_user_command_act USING e_ucomm.
*
*  ENDMETHOD.                    "handle_user_command
**---------------------------------------------------------------------*
**       METHOD handle_user_command_inv                                *
**---------------------------------------------------------------------*
*  METHOD handle_user_command_inv .
*    PERFORM f_handle_user_command_inv USING e_ucomm.
*
*  ENDMETHOD.                    "handle_user_command
*
*ENDCLASS.
**&---------------------------------------------------------------------*
**& Form f_get_debit_credit
**&---------------------------------------------------------------------*
**& Calculate total debit and credit amounts considering each tab
**&---------------------------------------------------------------------*
*FORM f_get_debit_credit  CHANGING cv_debit
*                                  cv_credit.
****Data declaration
*  DATA: lv_debit  TYPE augbt_kk,
*        lv_credit TYPE augbt_kk.
*
****Delete when line not checked
*  DATA(lt_act) = gt_act[].
*  DELETE lt_act WHERE checkbox NE gc_x.
****Delete when line not checked
*  DATA(lt_inv) = gt_inv[].
*  DELETE lt_inv WHERE checkbox NE gc_x.
*
****Get total credit and debit from on account tab
*  LOOP AT lt_act INTO DATA(lw_act).
*    IF lw_act-checkbox = gc_x.
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
**      lv_debit = COND #( WHEN lw_act-amount_cl > 0 THEN lw_act-amount_cl ).
**      lv_credit = COND #( WHEN lw_act-amount_cl < 0 THEN lw_act-amount_cl ).
**      cv_debit = cv_debit + lv_debit.
**      cv_credit = cv_credit + lv_credit.
*      lv_credit = COND #( WHEN lw_act-amount_cl > 0 THEN lw_act-amount_cl ).
*      lv_debit = COND #( WHEN lw_act-amount_cl < 0 THEN lw_act-amount_cl ).
*      cv_debit = cv_debit + abs( lv_debit ).
*      cv_credit = cv_credit + abs( lv_credit ).
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*    ENDIF.
*    CLEAR : lv_credit,lv_debit.
*  ENDLOOP.
*
****Get total credit and debit from open invoice tab
*  LOOP AT lt_inv INTO DATA(lw_inv).
*    IF lw_inv-checkbox = gc_x.
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
**      lv_debit = COND #( WHEN lw_inv-amount_cl > 0 THEN lw_inv-amount_cl ).
**      lv_credit = COND #( WHEN lw_inv-amount_cl < 0 THEN lw_inv-amount_cl ).
*
**      cv_debit = cv_debit + lv_debit.
**      cv_credit = cv_credit + lv_credit.
*
*      lv_credit = COND #( WHEN lw_inv-amount_cl > 0 THEN lw_inv-amount_cl ).
*      lv_debit = COND #( WHEN lw_inv-amount_cl < 0 THEN lw_inv-amount_cl ).
*
*      cv_debit = cv_debit + abs( lv_debit ).
*      cv_credit = cv_credit + abs( lv_credit ).
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*    ENDIF.
*    CLEAR : lv_credit,lv_debit.
*  ENDLOOP.
*
****Get total credit and debit from BP item tab
*  LOOP AT gt_bpitem INTO DATA(lw_bp).
*    lv_debit = COND #( WHEN lw_bp-betrw > 0 THEN lw_bp-betrw ).
*    lv_credit = COND #( WHEN lw_bp-betrw < 0 THEN lw_bp-betrw ).
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
**    cv_debit = cv_debit + lv_debit.
**    cv_credit = cv_credit + lv_credit.
*    cv_debit = cv_debit + abs( lv_debit ).
*    cv_credit = cv_credit + abs( lv_credit ).
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*    CLEAR : lv_credit,lv_debit.
*  ENDLOOP.
*
****Get total credit and debit from GL item tab
*  LOOP AT gt_glitem INTO DATA(lw_gl).
*    lv_debit = COND #( WHEN lw_gl-betrw > 0 THEN lw_gl-betrw ).
*    lv_credit = COND #( WHEN lw_gl-betrw < 0 THEN lw_gl-betrw ).
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
**    cv_debit = cv_debit + lv_debit.
**    cv_credit = cv_credit + lv_credit.
*    cv_debit = cv_debit + abs( lv_debit ).
*    cv_credit = cv_credit + abs( lv_credit ).
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*    CLEAR : lv_credit,lv_debit.
*  ENDLOOP.
****Take absolute values withour sign
*  cv_debit = abs( cv_debit ).
*  cv_credit = abs( cv_credit ).
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_looping_counter
**&---------------------------------------------------------------------*
**& Get highest looping counter for document split
**&---------------------------------------------------------------------*
*FORM f_looping_counter  USING    iv_fkkcl_len TYPE i
*                                 iv_fkkop_len TYPE i
*                                 iv_fkkopk_len TYPE i
*                        CHANGING cv_loop_cnt TYPE i.
****Getting the loop counter for clearing items
*  IF iv_fkkcl_len > gv_cl_split_no.
*    DATA(lv_cl_div) = iv_fkkcl_len DIV gv_cl_split_no.
*    DATA(lv_cl_mod) = iv_fkkcl_len MOD gv_cl_split_no.
*    IF lv_cl_mod = 0.
*      DATA(lv_cl_cnt) = lv_cl_div.
*    ELSE.
*      lv_cl_cnt = lv_cl_div + 1.
*    ENDIF.
*  ENDIF.
*
****Getting the loop counter for BP items
*  IF iv_fkkop_len > gv_bp_split_no.
*    DATA(lv_bp_div) = iv_fkkop_len DIV gv_bp_split_no.
*    DATA(lv_bp_mod) = iv_fkkop_len MOD gv_bp_split_no.
*    IF lv_bp_mod = 0.
*      DATA(lv_bp_cnt) = lv_bp_div.
*    ELSE.
*      lv_bp_cnt = lv_bp_div + 1.
*    ENDIF.
*  ENDIF.
*
****Getting the loop counter for GL items
*  IF iv_fkkopk_len > gv_gl_split_no.
*    DATA(lv_gl_div) = iv_fkkopk_len DIV gv_gl_split_no.
*    DATA(lv_gl_mod) = iv_fkkopk_len MOD gv_gl_split_no.
*    IF lv_gl_mod = 0.
*      DATA(lv_gl_cnt) = lv_gl_div.
*    ELSE.
*      lv_gl_cnt = lv_gl_div + 1.
*    ENDIF.
*  ENDIF.
*
****Get biggest loop counter among all three
*  DATA(lv_value) = nmax( val1 = lv_cl_cnt val2 = lv_bp_cnt ).
*  cv_loop_cnt = nmax( val1 = lv_value val2 = lv_gl_cnt ).
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_get_split_docs
**&---------------------------------------------------------------------*
**& Get split document list as per TVARVC split limit
**&---------------------------------------------------------------------*
*FORM f_get_split_docs  CHANGING ct_fkkop        TYPE tt_fkkop
*                                ct_fkkopk       TYPE tt_fkkopk
*                                ct_fkkcl        TYPE tt_fkkcl
*                                ct_fkkop_split  TYPE tt_fkkop
*                                ct_fkkopk_split TYPE tt_fkkopk
*                                ct_fkkcl_split  TYPE tt_fkkcl
*                                iv_fikey
*                                iv_gl_bukrs.
*
*  DATA : lw_curr              TYPE icurr,
*         lv_fkkcl_tot         TYPE augbt_kk     ##NEEDED,
*         lv_bp_tot            TYPE augbt_kk     ##NEEDED,
*         lv_gl_tot            TYPE augbt_kk     ##NEEDED,
*         lv_difference        TYPE augbt_kk     ##NEEDED,
*         lv_item_number       TYPE opupk_kk  VALUE 0,
*         lv_fkkop_item_number TYPE opupk_kk  VALUE 0.
*
*  CONSTANTS : lc_segment TYPE segmt_kk VALUE 'SEGMENT1'.
*
****  Get FKKCL items for the TVARVC limit
*  LOOP AT ct_fkkcl ASSIGNING FIELD-SYMBOL(<fs_fkkcl>).
*    DATA(lw_fkkcl) = <fs_fkkcl>.
*    DATA(lw_fkkcl_check) = <fs_fkkcl>.
*    lv_fkkcl_tot = lv_fkkcl_tot + lw_fkkcl-augbw.
*    APPEND lw_fkkcl TO ct_fkkcl_split.
*    CLEAR : <fs_fkkcl>,
*            lw_fkkcl.
*    DATA(lv_len) = lines( ct_fkkcl_split ).
*    IF lv_len = gv_cl_split_no.
*      DELETE ct_fkkcl WHERE opbel IS INITIAL.
**Validate If still items available with common OPBEL
*      READ TABLE ct_fkkcl TRANSPORTING NO FIELDS WITH KEY opbel = lw_fkkcl_check-opbel
*                                                          BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
**This loop is required to add the items which is having OPBEL common into one split
*        LOOP AT ct_fkkcl ASSIGNING FIELD-SYMBOL(<fs_fkkcl_dup>) WHERE opbel = lw_fkkcl_check-opbel.
*          DATA(lw_fkkcl_dup) = <fs_fkkcl_dup>.
*          lv_fkkcl_tot = lv_fkkcl_tot + lw_fkkcl_dup-augbw.
*          APPEND lw_fkkcl_dup TO ct_fkkcl_split.
*          CLEAR: <fs_fkkcl_dup>, lw_fkkcl_dup.
*        ENDLOOP.
*      ENDIF.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
*  DELETE ct_fkkcl WHERE opbel IS INITIAL.
*  CLEAR : lv_len.
*
****  Get FKKOP items for the TVARVC limit
*  LOOP AT ct_fkkop ASSIGNING FIELD-SYMBOL(<fs_fkkop>).
*    lv_fkkop_item_number = lv_fkkop_item_number + 1.
*    <fs_fkkop>-opupk = lv_fkkop_item_number.
*    DATA(lw_fkkop) = <fs_fkkop>.
*    lv_bp_tot = lv_bp_tot + lw_fkkop-betrw.
*    APPEND lw_fkkop TO ct_fkkop_split.
*    CLEAR : <fs_fkkop>,
*            lw_fkkop.
*    lv_len = lines( ct_fkkop_split ).
*    IF lv_len = gv_bp_split_no.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
*  DELETE ct_fkkop WHERE gpart IS INITIAL.
*  CLEAR : lv_len.
*
****  Get FKKOP items for the TVARVC limit
*  LOOP AT ct_fkkopk ASSIGNING FIELD-SYMBOL(<fs_fkkopk>).
*    lv_item_number = lv_item_number + 1.
*    <fs_fkkopk>-opupk = lv_item_number.
*    DATA(lw_fkkopk) = <fs_fkkopk>.
*    lv_gl_tot = lv_gl_tot + lw_fkkopk-betrw.
*    APPEND lw_fkkopk TO ct_fkkopk_split.
*    CLEAR : <fs_fkkopk>.
*    lv_len = lines( ct_fkkopk_split ).
*    IF lv_len = gv_gl_split_no.
*      EXIT.
*    ENDIF.
*    CLEAR : lw_fkkopk.
*  ENDLOOP.
*  DELETE ct_fkkopk WHERE hkont IS INITIAL.
*
**Check first and last line
*  IF ct_fkkcl_split IS NOT INITIAL.
*    DATA(lw_fkkcl_first) = ct_fkkcl_split[ 1 ].
*    DATA(lv_lines) = lines( ct_fkkcl_split ).
*    DATA(lw_fkkcl_last) = ct_fkkcl_split[ lv_lines ].
*  ENDIF.
*
*  lv_difference = ( lv_fkkcl_tot * -1 ) + lv_bp_tot + lv_gl_tot.
*  IF lv_difference IS NOT INITIAL.
*
****check if expense GL account or revenue GL needs to be taken
*    DATA(lv_hkont) = gv_hkont_gl.
*    lv_item_number = lv_item_number + 1.
*
****Add extra GL line for posting
*    DATA(lw_fkkopk_add) = VALUE fkkopk( opupk  = lv_item_number
*                              bukrs  = iv_gl_bukrs
*                              hkont  = lv_hkont
*                              betrw  = lv_difference * -1
*                              segment = lc_segment
*                              fikey  = iv_fikey ).
****Convert amount to local transaction currency
*    CLEAR : lw_curr.
*    PERFORM f_curr_conv USING lv_difference
*                              lw_fkkopk_add-bukrs
*                              p_curr
*                        CHANGING lw_curr.
*
*    lw_fkkopk_add-betrh   = lw_curr-dmbtr * -1.
*    lw_fkkopk_add-betr2   = lw_curr-dmbe2 * -1.
****Append to final gl table
*    APPEND lw_fkkopk_add TO ct_fkkopk_split.
*  ENDIF.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_post_document_split
**&---------------------------------------------------------------------*
**& Post split document list
**&---------------------------------------------------------------------*
*FORM f_post_document_split  USING  it_fkkop  TYPE tt_fkkop
*                                   it_fkkopk TYPE tt_fkkopk
*                                   it_fkkcl  TYPE tt_fkkcl
*                                   is_fkkko
*                                   iv_fikey
*                                   iv_final_proc
*                                   iv_split_no
*                          CHANGING c_flag    TYPE c.
*
****Data declaration
*  DATA: lt_enqtab  TYPE TABLE OF ienqtab,
*        lw_enqtab  TYPE ienqtab,
*        lv_docno   TYPE opbel_kk,
*        lv_message TYPE string.
*
****Fill enque table for business partner
*  LOOP AT it_fkkcl INTO DATA(lw_fkkcl) .
*    lw_enqtab = VALUE #(   gpart = lw_fkkcl-gpart
*                           bukrs = lw_fkkcl-bukrs ).
*
*    APPEND lw_enqtab TO lt_enqtab.
*    CLEAR : lw_enqtab.
*    lw_enqtab = VALUE #(   vkont = lw_fkkcl-vkont
*                           bukrs = lw_fkkcl-bukrs ).
*    APPEND lw_enqtab TO lt_enqtab.
*    CLEAR : lw_enqtab.
*  ENDLOOP.
*
*  IF it_fkkop IS NOT INITIAL.
*    LOOP AT it_fkkop INTO DATA(lw_fkkop).
*
*      lw_enqtab = VALUE #(   gpart = lw_fkkop-gpart
*                             bukrs = lw_fkkop-bukrs ).
*      APPEND lw_enqtab TO lt_enqtab.
*      CLEAR : lw_enqtab.
*
*      lw_enqtab = VALUE #(   vkont = lw_fkkop-vkont
*                             bukrs = lw_fkkop-bukrs ).
*      APPEND lw_enqtab TO lt_enqtab.
*
*      CLEAR : lw_fkkop.
*    ENDLOOP.
*  ENDIF.
****Call function module to enque all business partners for document posting
*  CALL FUNCTION 'FKK_OPEN_ITEM_ENQUEUE'
*    TABLES
*      t_enqtab = lt_enqtab.
*
****Check if someone blocks the business partner/contract account/company code
*  CLEAR : lw_enqtab.
*  DELETE lt_enqtab WHERE xenqe IS INITIAL.
*  READ TABLE lt_enqtab INTO lw_enqtab INDEX 1.
*  IF NOT lw_enqtab-xenqe IS INITIAL OR lw_enqtab-xenqm = 'X'.
*    MESSAGE e162 WITH lw_enqtab-uname.
*  ENDIF.
*
****Call BAPI to post document
*  CALL FUNCTION 'FKK_CREATE_DOC_AND_CLEAR'
*    EXPORTING
*      i_fkkko       = is_fkkko
*      i_update_task = gc_x
*    IMPORTING
*      e_opbel       = lv_docno
*    TABLES
*      t_fkkop       = it_fkkop
*      t_fkkopk      = it_fkkopk
*      t_fkkcl       = it_fkkcl
*    EXCEPTIONS
*      error_message = 1.
****Call BAPI commit with FI key close if document created successfully
*  IF sy-subrc = 0 AND lv_docno IS NOT INITIAL.
****Commit the poting
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
****Check if final split for processing
*    IF iv_final_proc = abap_true.
*
*      CALL FUNCTION 'BAPI_CTRACRECKEY_CLOSE'
*        EXPORTING
*          reconciliationkey = iv_fikey.
****Commit the key closure
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.
*
****Call function module to enque all business partners for document posting
*      CALL FUNCTION 'FKK_OPEN_ITEM_DEQUEUE'.
*
****Clear global variables
*      PERFORM f_clear_global_var.
*    ENDIF.
****    Check if any exception occured in update task
*    IF it_fkkcl IS NOT INITIAL.
*      SELECT opbel
*            FROM dfkkop
*        INTO TABLE @DATA(lt_dfkkop) ##NEEDED
*        FOR ALL ENTRIES IN @it_fkkcl
*        WHERE opbel = @it_fkkcl-opbel
*          AND   opupk = @it_fkkcl-opupk
*          AND   opupw = @it_fkkcl-opupw
*          AND   opupz = @it_fkkcl-opupz
*          AND   betrw = @it_fkkcl-betrw
*          AND   augwa = @space.
*      IF sy-subrc <> 0.
****Raise success message with document number
*        MESSAGE s159 WITH lv_docno INTO lv_message.
*
*        APPEND VALUE #( split_no = iv_split_no
*                        opbel    = lv_docno
*                        type     = gc_s
*                        message  = lv_message )
*                        TO gt_output_log.
*
*      ELSE.
****Raise error message in case any exception occured in update task
*
*        c_flag = abap_true.
*        MESSAGE e185 INTO lv_message.
*        APPEND VALUE #( split_no = iv_split_no
*                        type = gc_e
*                        message = lv_message )
*                TO gt_output_log.
*      ENDIF.
*    ELSE.
****Raise success message with document number
*      MESSAGE s159 WITH lv_docno INTO lv_message.
*
*      APPEND VALUE #( split_no = iv_split_no
*                      opbel    = lv_docno
*                      type     = gc_s
*                      message  = lv_message )
*                      TO gt_output_log.
*    ENDIF.
*  ELSE.
****If document not posted raise error message
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
****Call function module to enque all business partners for document posting
*    CALL FUNCTION 'FKK_OPEN_ITEM_DEQUEUE'.
*    c_flag = abap_true.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      INTO lv_message
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    APPEND VALUE #( split_no = iv_split_no
*                    type = gc_e
*                    message = lv_message )
*            TO gt_output_log.
*  ENDIF.
*
*ENDFORM.
** Begin of change - 486861 Date: 10/11/2021 Defect # INC3765130
**&---------------------------------------------------------------------*
**& Form f_validate_bp_manualentry
**&---------------------------------------------------------------------*
**& BP table manual entries validation
**&---------------------------------------------------------------------*
*FORM f_validate_bp_manualentry .
*
****BP items
*  DATA(lt_bpitem) = gt_bpitem[].
*
*  DELETE lt_bpitem WHERE prctr IS INITIAL.
*  IF lt_bpitem IS NOT INITIAL.
****    Get prctr from table CEPC
*    SELECT prctr,datbi,kokrs
*      FROM cepc INTO TABLE @DATA(lt_cepc)
*      FOR ALL ENTRIES IN @lt_bpitem
*      WHERE prctr = @lt_bpitem-prctr
*        AND datbi GE @p_postdt
*        AND kokrs = @gv_kokrs
*        AND datab LE @p_postdt.
*    IF sy-subrc EQ 0.
*      SORT lt_cepc BY prctr.
*    ENDIF.
*  ENDIF.
*
****Get contract account
*  CLEAR : lt_bpitem.
*  lt_bpitem = gt_bpitem[].
*  DELETE lt_bpitem WHERE vkont IS INITIAL.
*  IF lt_bpitem IS NOT INITIAL.
*
*    SELECT vkont,stdbk
*      FROM fkkvkp INTO TABLE @DATA(lt_fkkvkp)
*      FOR ALL ENTRIES IN @lt_bpitem
*      WHERE vkont = @lt_bpitem-vkont
*        AND stdbk = @lt_bpitem-bukrs.
*    IF sy-subrc EQ 0.
*      SORT lt_fkkvkp BY vkont stdbk.
*    ENDIF.
*  ENDIF.
****    Check validation for BP items
*  LOOP AT gt_bpitem INTO DATA(lw_bp).
*    DATA(lv_tabix) = sy-tabix.
*    CLEAR : lv_tabix.
*    lv_tabix = sy-tabix.
*    IF lw_bp-prctr IS NOT INITIAL.
*      READ TABLE lt_cepc TRANSPORTING NO FIELDS WITH KEY prctr = lw_bp-prctr BINARY SEARCH.
*      IF sy-subrc <> 0.
*        APPEND VALUE #( err_typ = '1'
*                        msg = TEXT-066
*                        line = lv_tabix
*                        alv_tab = TEXT-073 )
*                        TO gt_error.
*      ENDIF.
*    ENDIF.
*
*    IF lw_bp-vkont IS NOT INITIAL.
*      READ TABLE lt_fkkvkp TRANSPORTING NO FIELDS WITH KEY vkont = lw_bp-vkont
*                                                         stdbk = lw_bp-bukrs BINARY SEARCH.
*      IF sy-subrc NE 0.
*        APPEND VALUE #( err_typ = '1'
*                        msg = TEXT-072
*                        line = lv_tabix
*                        alv_tab = TEXT-073 )
*                        TO gt_error.
*      ENDIF.
*    ENDIF.
*
*    CLEAR:lw_bp.
*  ENDLOOP.
*
*  CLEAR : lt_bpitem,lt_cepc,lt_fkkvkp,
*          lw_bp,lv_tabix.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_validate_gl_manualentry
**&---------------------------------------------------------------------*
**& GL table manual entries validation
**&---------------------------------------------------------------------*
*FORM f_validate_gl_manualentry .
*
*  DATA :lr_prps       TYPE RANGE OF ps_posnr,
*        lr_vbeln      TYPE RANGE OF vbeln,
*        lv_vbeln      TYPE vbeln,
*        lv_msg_val    TYPE string,
*        lv_tabix      TYPE sy-tabix,
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*        lv_fld_stat   TYPE t004f-faus1,
*        lt_result_tab TYPE match_result_tab,
*        lv_modif      TYPE scrfgrp1,
*        lv_message    TYPE string.
*  CONSTANTS : lc_fauna TYPE fauna_modi VALUE 'SKB1-FAUS1',
*              lc_bseg  TYPE tabnm      VALUE 'BSEG',
*              lc_koart TYPE koart      VALUE 'S',
*              lc_kostl TYPE fieldname  VALUE 'KOSTL',
*              lc_wbs   TYPE fieldname  VALUE 'PROJK',
*              lc_sgtxt TYPE fieldname  VALUE 'SGTXT'.
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*
*  DATA(lt_glitem) = gt_glitem[].
*  DELETE lt_glitem WHERE prctr IS INITIAL.
*  IF lt_glitem IS NOT INITIAL.
****Check profit center entered
*    SELECT prctr,datbi,kokrs
*      FROM cepc INTO TABLE @DATA(lt_cepc_gl)
*      FOR ALL ENTRIES IN @lt_glitem
*      WHERE prctr = @lt_glitem-prctr
*        AND datbi GE @p_postdt
*        AND kokrs = @gv_kokrs
*        AND datab LE @p_postdt.
*  ENDIF.
****check cost center entered
*  CLEAR : lt_glitem.
*  lt_glitem = gt_glitem[].
*  DELETE lt_glitem WHERE kostl IS INITIAL.
*  IF lt_glitem IS NOT INITIAL.
*    SELECT kokrs,kostl,datbi
*      FROM csks INTO TABLE @DATA(lt_csks)
*      FOR ALL ENTRIES IN @lt_glitem
*      WHERE kokrs = @gv_kokrs
*        AND kostl = @lt_glitem-kostl
*        AND datbi GE @p_postdt
*        AND datab LE @p_postdt
*        AND bkzkp = ''
*        AND pkzkp = ''
*        AND bukrs = @lt_glitem-bukrs.
*    IF sy-subrc EQ 0.
*      SORT lt_csks BY kostl.
*    ENDIF.
*  ENDIF.
****Check order number entered
*  CLEAR : lt_glitem.
*  lt_glitem = gt_glitem[].
*  DELETE lt_glitem WHERE aufnr IS INITIAL.
*  IF lt_glitem IS NOT INITIAL.
*    SELECT aufnr,bukrs
*      FROM aufk INTO TABLE @DATA(lt_aufk)
*      FOR ALL ENTRIES IN @lt_glitem
*      WHERE aufnr = @lt_glitem-aufnr
*        AND bukrs = @lt_glitem-bukrs
*        AND idat3 = '00000000'.
*  ENDIF.
*
*  CLEAR : lt_glitem.
*  lt_glitem = gt_glitem[].
*  DELETE lt_glitem WHERE ps_psp_pnr IS INITIAL.
**
*  LOOP AT lt_glitem INTO DATA(lw_gl).
*    APPEND VALUE #( sign = 'I'
*                    option = 'EQ'
*                    low = lw_gl-ps_psp_pnr )
*                TO lr_prps.
*    CLEAR : lw_gl.
*  ENDLOOP.
*
*  IF lr_prps IS NOT INITIAL.
****    Get WBS elements from PRPS table
*    SELECT pspnr,
*           objnr,
*           pbukr,
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*           belkz
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*        FROM prps
*      INTO TABLE @DATA(lt_prps)
*      FOR ALL ENTRIES IN @lt_glitem
*      WHERE pspnr IN @lr_prps
*      AND pbukr = @lt_glitem-bukrs
*      ORDER BY PRIMARY KEY.
*    IF sy-subrc EQ 0.
****Sort PRPS table with wbs element and object number
*      SORT lt_prps BY pspnr objnr pbukr.
****Get status of WBS element from table JEST
*      SELECT objnr,stat,inact
*        FROM jest
*        INTO TABLE @DATA(lt_jest)
*        FOR ALL ENTRIES IN @lt_prps
*        WHERE objnr = @lt_prps-objnr
*           AND stat IN @gr_wbs_status
*           AND inact = ''
*        ORDER BY PRIMARY KEY.
*      IF sy-subrc EQ 0.
****Sort status table
*        SORT lt_jest BY objnr.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
****Check for sales order number entered
*  CLEAR : lt_glitem.
*  lt_glitem = gt_glitem[].
*  DELETE lt_glitem WHERE kdauf IS INITIAL.
*  IF lt_glitem IS NOT INITIAL.
*    CLEAR : lw_gl.
*    LOOP AT lt_glitem INTO lw_gl.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = lw_gl-kdauf
*        IMPORTING
*          output = lv_vbeln.
*
*      APPEND VALUE #( sign = 'I'
*                      option = 'EQ'
*                      low = lv_vbeln )
*                  TO lr_vbeln.
*      CLEAR : lw_gl,lv_vbeln.
*    ENDLOOP.
*    IF lr_vbeln IS NOT INITIAL.
****Get sales order number from VBAK
*      SELECT vbeln
*        FROM vbak
*        INTO TABLE @DATA(lt_vbak)
*        WHERE vbeln IN @lr_vbeln
*        ORDER BY PRIMARY KEY.
*      IF sy-subrc EQ 0.
*        SORT lt_vbak BY vbeln.
*      ENDIF.
*
*    ENDIF.
****Get sales order number from CRMS4D_SERV_H
*    SELECT zzecc_vbeln_srh
*      FROM crms4d_serv_h
*      INTO TABLE @DATA(lt_crm)
*      FOR ALL ENTRIES IN @lt_glitem
*      WHERE zzecc_vbeln_srh = @lt_glitem-kdauf.
*    IF sy-subrc EQ 0.
*      SORT lt_crm BY zzecc_vbeln_srh.
*    ENDIF.
*
*  ENDIF.
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
****Get mandatory field list from TMODU
*  SELECT fauna,
*         modif,
*         tabnm,
*         feldn,
*         koart
*    FROM tmodu
*    INTO TABLE @DATA(lt_tmodu)
*    WHERE fauna = @lc_fauna
*    AND tabnm   = @lc_bseg
*    AND koart   = @lc_koart.
*  IF sy-subrc = 0.
*    SORT lt_tmodu.
*  ENDIF.
*
****Get cost center details from OKB9 table to auto populate
*  CLEAR : lt_glitem.
*  lt_glitem = gt_glitem.
*  DELETE lt_glitem WHERE bukrs IS INITIAL.
*  IF lt_glitem IS NOT INITIAL.
*    SELECT bukrs,
*           kstar,
*           kokrs,
*           kostl
*      FROM tka3a
*      INTO TABLE @DATA(lt_tka3a)
*      FOR ALL ENTRIES IN @lt_glitem
*      WHERE bukrs = @lt_glitem-bukrs
*        AND kstar = @lt_glitem-hkont
*        AND kokrs = @gv_kokrs.
*    IF sy-subrc = 0.
*      SORT lt_tka3a BY bukrs kstar kokrs.
*    ENDIF.
*  ENDIF.
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*
****Loop GL item table for any issue in manual entries
*  LOOP AT gt_glitem INTO lw_gl.
*    CLEAR : lv_tabix.
*    lv_tabix = sy-tabix.
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
****    Mandatory field check for GL account and company code combination on GL tab
*    IF lw_gl-hkont IS NOT INITIAL AND lw_gl-bukrs IS NOT INITIAL.
*      CALL FUNCTION 'GET_FIELDSTATUS_FOR_GL_ACCT'
*        EXPORTING
*          i_account_nr    = lw_gl-hkont
*          i_company_code  = lw_gl-bukrs
*          i_account_type  = lc_koart
*        IMPORTING
*          e_field_status  = lv_fld_stat
*        EXCEPTIONS
*          no_account_type = 1
*          no_account      = 2
*          no_company_code = 3
*          no_master_data  = 4
*          no_status_found = 5
*          no_depr_area    = 6
*          OTHERS          = 7.
*      IF sy-subrc EQ 0.
*        "Find all positions of + (mandatory sign)
*        FIND ALL OCCURRENCES OF '+' IN lv_fld_stat RESULTS lt_result_tab.
*
*        LOOP AT lt_result_tab INTO DATA(lw_result_tab).
****      Get the actual position
*          IF lw_result_tab-offset GE 9 AND lw_result_tab-offset LT 99.
*            lv_modif = |0{ lw_result_tab-offset + 1 }|.
*          ELSEIF lw_result_tab-offset LT 9 AND lw_result_tab-offset GE 0.
*            lv_modif = |00{ lw_result_tab-offset + 1 }|.
*          ENDIF.
****       Read TMODU table to check which field is mandatory
*          READ TABLE lt_tmodu INTO DATA(lw_tmodu) WITH KEY fauna = lc_fauna
*                                                           modif = lv_modif
*                                                           tabnm = lc_bseg
*                                                           koart = lc_koart BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            CASE lw_tmodu-feldn.
****           Auto-populate for mandatry cost center or display error message
*              WHEN lc_kostl.
*                IF lw_gl-kostl IS INITIAL.
*                  READ TABLE lt_tka3a INTO DATA(lw_tka3a) WITH KEY bukrs = lw_gl-bukrs
*                                                                   kstar = lw_gl-hkont
*                                                                   kokrs = gv_kokrs BINARY SEARCH.
*                  IF sy-subrc EQ 0.
*                    lw_gl-kostl = lw_tka3a-kostl.
*                    MODIFY gt_glitem FROM lw_gl TRANSPORTING kostl.
*                    CLEAR lw_gl-kostl.
*                  ELSE.
*                    APPEND VALUE #( err_typ = '1'
*                                     msg = TEXT-082
*                                     line = lv_tabix
*                                     alv_tab = TEXT-074 )
*                                     TO gt_error.
*                  ENDIF.
*                ENDIF.
****          Check if WBS is mandatory and display error if blank
*              WHEN lc_wbs.
*                IF lw_gl-ps_psp_pnr IS INITIAL.
*
*                  APPEND VALUE #( err_typ = '1'
*                                   msg = TEXT-083
*                                   line = lv_tabix
*                                   alv_tab = TEXT-074 )
*                                   TO gt_error.
*                ENDIF.
****           Auto-populate item text if SGTXT is mandatory in GL
*              WHEN lc_sgtxt.
*                lw_gl-sgtxt = 'FICA Transfer'(084).
*                lw_gl-xeiph = abap_true.
*                MODIFY gt_glitem FROM lw_gl TRANSPORTING sgtxt xeiph.
****           Check if any other field is mandatory and not provided
*              WHEN OTHERS.
*                lv_message = TEXT-085.
*                REPLACE ALL OCCURRENCES OF '%%' IN lv_message WITH lw_gl-hkont.
*                REPLACE ALL OCCURRENCES OF '**' IN lv_message WITH lw_tmodu-feldn.
*                REPLACE ALL OCCURRENCES OF '##' IN lv_message WITH lw_gl-hkont.
*                APPEND VALUE #( err_typ = '1'
*                                 msg = lv_message
*                                 line = lv_tabix
*                                 alv_tab = TEXT-074 )
*                                 TO gt_error.
*            ENDCASE.
*          ENDIF.
*        ENDLOOP.
*
*      ENDIF.
*    ENDIF.
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*
*    IF lw_gl-prctr IS NOT INITIAL.
****Validation for profit center
*      READ TABLE lt_cepc_gl TRANSPORTING NO FIELDS WITH KEY prctr = lw_gl-prctr BINARY SEARCH.
*      IF sy-subrc <> 0.
*        APPEND VALUE #( err_typ = '1'
*                        msg = TEXT-066
*                        line = lv_tabix
*                        alv_tab = TEXT-074 )
*                        TO gt_error.
*      ENDIF.
*    ENDIF.
****Validation for cost center
*    IF lw_gl-kostl IS NOT INITIAL.
*      READ TABLE lt_csks TRANSPORTING NO FIELDS WITH KEY kostl = lw_gl-kostl BINARY SEARCH.
*      IF sy-subrc <> 0.
*        APPEND VALUE #( err_typ = '1'
*                        msg = TEXT-069
*                        line = lv_tabix
*                        alv_tab = TEXT-074 )
*                        TO gt_error.
*      ENDIF.
*    ENDIF.
****Validation for order number
*    IF lw_gl-aufnr IS NOT INITIAL.
*      READ TABLE lt_aufk TRANSPORTING NO FIELDS WITH KEY aufnr = lw_gl-aufnr BINARY SEARCH.
*      IF sy-subrc <> 0.
*        APPEND VALUE #( err_typ = '1'
*                        msg = TEXT-070
*                        line = lv_tabix
*                        alv_tab = TEXT-074 )
*                        TO gt_error.
*      ENDIF.
*    ENDIF.
****Validation for WBS element
*    IF lw_gl-ps_psp_pnr IS NOT INITIAL.
****Check WBS
*      READ TABLE lt_prps INTO DATA(lw_prps) WITH KEY pspnr = lw_gl-ps_psp_pnr
*                                                     pbukr = lw_gl-bukrs BINARY SEARCH.
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*      IF sy-subrc EQ 0 AND lw_prps-belkz = abap_true.
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*        READ TABLE lt_jest TRANSPORTING NO FIELDS WITH KEY objnr = lw_prps-objnr BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          APPEND VALUE #( err_typ = '1'
*                          msg = TEXT-068
*                          line = lv_tabix
*                          alv_tab = TEXT-074 )
*                          TO gt_error.
*        ENDIF.
*      ELSEIF sy-subrc EQ 0 AND lw_prps-belkz IS INITIAL.
*        APPEND VALUE #( err_typ = '1'
*                        msg = TEXT-086
*                        line = lv_tabix
*                        alv_tab = TEXT-074 )
*                        TO gt_error.
*      ELSE.
*        APPEND VALUE #( err_typ = '1'
*                        msg = TEXT-068
*                        line = lv_tabix
*                        alv_tab = TEXT-074 )
*                        TO gt_error.
*      ENDIF.
*    ENDIF.
****Validation for sales order number
*    IF lw_gl-kdauf IS NOT INITIAL.
*      CLEAR :lv_vbeln.
***conert to internal VBELN format
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = lw_gl-kdauf
*        IMPORTING
*          output = lv_vbeln.
*      READ TABLE lt_vbak TRANSPORTING NO FIELDS WITH KEY vbeln = lv_vbeln BINARY SEARCH.
*      IF sy-subrc NE 0.
*        READ TABLE lt_crm TRANSPORTING NO FIELDS WITH KEY zzecc_vbeln_srh = lw_gl-kdauf BINARY SEARCH.
*        IF sy-subrc NE 0.
*          APPEND VALUE #( err_typ = '1'
*                          msg = TEXT-071
*                          line = lv_tabix
*                          alv_tab = TEXT-074 )
*                          TO gt_error.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*    CLEAR:lw_gl,
** Begin of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*           lv_message,lv_fld_stat,
*           lw_tka3a,lw_tmodu,lw_result_tab.
** End of change - 486861 Date: 10/18/2021 : APLCFR1BL-1235 : CF.ENH.762E
*  ENDLOOP.
*
*  CLEAR : lr_vbeln,lr_prps,lw_gl,lv_vbeln,
*          lv_msg_val,lv_tabix,lt_glitem,
*          lt_cepc_gl,lt_prps,lt_vbak,lt_crm,
*          lt_jest,lt_aufk,lt_csks.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_display_manualentry_error
**&---------------------------------------------------------------------*
**& Display error messages for BP and GL manual entries
**&---------------------------------------------------------------------*
*FORM f_display_manualentry_error .
*
*  DATA : lo_alv       TYPE REF TO cl_salv_table,
*         lr_columns   TYPE REF TO cl_salv_columns_table,
*         lr_column    TYPE REF TO cl_salv_column_table,
*         lr_functions TYPE REF TO cl_salv_functions_list.
*
****Initiate error tab
*  TRY.
*      cl_salv_table=>factory(
*        IMPORTING
*          r_salv_table = lo_alv
*        CHANGING
*          t_table      = gt_error[] ).
*
*    CATCH cx_salv_msg.
*  ENDTRY.
*
****Change column wise changes
*  TRY.
*      lr_columns = lo_alv->get_columns( ).
*      lr_columns->set_exception_column( value = TEXT-075 ).
*      lr_column ?= lr_columns->get_column( TEXT-075 ).
*      lr_column->set_output_length('9').
*      lr_column ?= lr_columns->get_column( TEXT-076 ).
*      lr_column->set_long_text( TEXT-079 ).
*      lr_column->set_output_length('40').
*      lr_column ?= lr_columns->get_column( TEXT-077 ).
*      lr_column->set_long_text( TEXT-080 ).
*      lr_column->set_output_length('7').
*      lr_column ?= lr_columns->get_column( TEXT-078 ).
*      lr_column->set_long_text( TEXT-081 ).
*      lr_column->set_output_length('10').
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.
*
*
*  lr_functions = lo_alv->get_functions( ).
*  lr_functions->set_all( gc_x ).
*
*  IF lo_alv IS BOUND.
****setup popup size
*    lo_alv->set_screen_popup(
*      start_column = 2
*      end_column  = 62
*      start_line  = 1
*      end_line    = 10
*      ).
****Display error alv
*    lo_alv->display( ).
*
*  ENDIF.
*
*ENDFORM.
** End of change - 486861 Date: 10/11/2021 Defect # INC3765130
