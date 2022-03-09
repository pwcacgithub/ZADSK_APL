class ZCL_CA_UTILITY_V2 definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF gs_name_mapping,
        abap TYPE abap_compname,
        json TYPE string,
      END OF gs_name_mapping .
  types:
    gt_name_mappings TYPE HASHED TABLE OF gs_name_mapping WITH UNIQUE KEY abap .

  constants GC_APP_SERVER type CHAR1 value 'A' ##NO_TEXT.
  constants GC_LOC_SERVER type CHAR1 value 'P' ##NO_TEXT.
  constants GC_CHAR_X type CHAR1 value 'X' ##NO_TEXT.
  constants GC_FLD_FNAME type CHAR5 value 'FNAME' ##NO_TEXT.
  constants GC_FLD_ROWNO type CHAR6 value 'ROWCNT' ##NO_TEXT.
  data GF_IS_ERROR type ABAP_BOOL .
  constants GC_LPATH type CHAR1 value 'L' ##NO_TEXT.
  constants GC_ERROR_PATH type CHAR6 value 'error/' ##NO_TEXT.

  methods READ_CSV_FILE
    importing
      !I_FILENAME type RLGRAP-FILENAME
      !I_DELIMITER type CHAR1 optional
      !I_CODEPAGE type ABAP_ENCOD optional
      !I_HDR type ABAP_ENCOD optional
    changing
      !E_DATATAB type TABLE
    exceptions
      CANNOT_OPEN_FILE
      INVALID_DELIMETER
      ERROR_IN_READ
      INVALID_SOURCE .
  methods READ_FILE
    importing
      !I_FILENAME type STRING
      !I_SOURCE type CHAR1
      !I_DELIMITER type CHAR1 optional
      !I_CODEPAGE type ABAP_ENCOD optional
      !I_HDR type ABAP_ENCOD optional
    changing
      !E_DATATAB type TABLE
    exceptions
      CANNOT_OPEN_FILE
      INVALID_DELIMETER
      ERROR_IN_READ
      INVALID_SOURCE .
  methods FCAT_FROM_INTERNAL_TABLE
    importing
      !I_TABLE type ANY TABLE
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  methods DISPLAY_ALV
    changing
      !C_DATATAB type TABLE .
  methods LAYOUT_SET
    importing
      !I_SALVTAB type ref to CL_SALV_TABLE .
  methods COLUMNS_SET
    importing
      !I_SALVTAB type ref to CL_SALV_TABLE .
  methods OBJTREF_CREATE
    importing
      !IF_LIST type SAP_BOOL optional
    exporting
      !R_SALVREF type ref to CL_SALV_TABLE
    changing
      !C_DATATAB type TABLE .
  methods ADD_TO_LOG
    importing
      !I_MESSAGE type STRING
      !I_MTYPE type BAPI_MTYPE
      !I_MSGNO type SYMSGNO optional
      !I_MSGID type SYMSGID optional
      !I_MSGV1 type SYMSGV optional
      !I_MSGV2 type SYMSGV optional
      !I_MSGV3 type SYMSGV optional
      !I_MSGV4 type SYMSGV optional
      !I_PARAM type BAPI_PARAM optional
      !I_FIELD type BAPI_FLD optional .
  class-methods SELECT_FILE
    importing
      value(I_SOURCE) type CHAR1
      !I_GUI_INITIAL_DIRECTORY type STRING optional
      value(I_APPPATH_TYPE) type CHAR1 optional
      !I_TITLE type STRING default 'Select File'
      !I_GUI_EXTENSION type STRING optional
      !I_GUI_EXT_FILTER type STRING optional
    exporting
      !E_FILENAME type STRING
    exceptions
      F4_HELP_FAILED .
  methods WRITE_FILE
    importing
      !I_FILENAME type STRING
      !I_SOURCE type CHAR1
      !I_DELIMITER type CHAR1 optional
      !I_ALTDELIM type CHAR2 optional
      !I_ADDHEADER type ABAP_BOOL optional
      !I_FIELDS type TABLE optional
      !I_ACCESSMODE type CHAR1 default 'O'
    changing
      !I_DATATAB type TABLE
    exceptions
      INVALID_DELIMITER
      INVALID_SOURCE
      WRITE_ERROR .
  class-methods CONVERT_TABLE_TO_STRING
    importing
      !I_DELIMITER type ANY
      !I_DATATAB type TABLE
      !I_FCAT type LVC_T_FCAT
    exporting
      !E_DATATAB type TABLE .
  methods SEND_MAIL
    importing
      !I_REC_TYPE type N default 0
      !I_RECEIVER type STRING
      !I_SEN_TYPE type N default 0
      !I_SENDER type STRING optional
      !I_PRIORITY type SO_SND_PRI optional
      !I_CONTENT type SO_OBJ_TP default 'RAW'
      !I_SUBJECT type SO_OBJ_DES
      !I_BODY type BCSY_TEXT
      !I_ATTACHMENT_ATTRIBUTE type ZTTCA_PACKLIST optional
      !I_ATTACHMENT type SOLIX_TAB optional
      !I_IMMEDIATE type CHAR1 optional
    exporting
      !E_RETCODE type I
      !E_ERR_STR type STRING .
  methods GET_EMAIL_CONTENT
    importing
      !I_TEXT_NAME_SUB type THEAD-TDNAME
      !I_TEXT_NAME_BODY type THEAD-TDNAME
      !I_TEXT_REPLACE type ZTTCA_EMAIL_TEXTSYMBOL_REPLACE
      !I_LANGUAGE type SPRAS optional
    exporting
      !E_SUBJECT type SO_OBJ_DES
      !E_BODY type BCSY_TEXT
      !ET_RETURN type BAPIRET2_T .
  methods IDOC_TRACE
    importing
      !IM_IDOCS type ZTIDOC_NUMBERS
      !IM_RECV_PARTNER type EDI_RCVPRN
    exporting
      !EX_IDOC_STATUS type ZTIDOC_TRACE_STATUS
    exceptions
      EXC_NO_PARNTER_DEFINITION
      EXC_NO_PORT
      EXC_NO_LOGDES_IN_PORT
      EXC_DEST_DOES_NOT_EXIST
      EXC_R2_SYSTEM
      EXC_EXTERNAL_SYSTEM
      EXC_NO_IDOCS_REQSTD_FOR_TRACE .
  methods ARCHIVE_FILE
    importing
      !I_FILE type STRING
      !ADD_TIME_STAMP type ABAP_BOOL optional
      !I_ARCH_PATH type STRING optional
    returning
      value(R_ARCH_PATH) type STRING
    exceptions
      ERROR_ARCHIVING_FILE .
  methods COPY_FILE
    importing
      !I_SRC_FILE type STRING
      !I_TRG_FILE type STRING
    returning
      value(R_COPY_ERROR) type ABAP_BOOL .
  methods DELETE_FILE
    importing
      !I_FILENAME type STRING
    exceptions
      DELETE_ERROR .
  methods CALL_REST_API
    importing
      !I_DESTINATION type RFCDEST
      !I_URL type STRING
      !I_IN_TABLE type DATA
      !I_IN_MAPPING type GT_NAME_MAPPINGS optional
      !I_IN_HEADERS type TIHTTPNVP optional
      !I_GET type C optional
      !I_POST type C optional
    exporting
      !E_MESSAGE type ZTTUT_MESSAGE
    changing
      !C_RESP_TABLE type ANY optional .
  methods GET_YETI_API_FLD_MAP
    changing
      !I_IN_MAPPING type ZTTUT_NAME_MAPPING .
  methods GET_MULE_PUB_SUB_API_HEADER
    changing
      !I_IN_MAPPING type ZTTUT_NAME_MAPPING .
  methods GET_APIKEY
    importing
      value(I_INTERFACE_ID) type ZINTERFACEID
    exporting
      !E_CLIENT_SECRET type HTTP_EXT_USER
      !E_CLIENT_ID type Z_OTDP_E_CLIENT_ID
    exceptions
      CLIENTID_KEY_NOT_FOUND .
  methods SEND_MAIL_MIME
    importing
      !I_REC_TYPE type N default 0
      !I_RECEIVER type STRING
      !I_SEN_TYPE type N default 0
      !I_SENDER type STRING optional
      !I_PRIORITY type SO_SND_PRI optional
      !I_SUBJECT type SO_OBJ_DES
      !I_ATTACHMENT_ATTRIBUTE type ZTTCA_PACKLIST optional
      !I_ATTACHMENT type SOLIX_TAB optional
      !I_IMMEDIATE type CHAR1 optional
      !I_MULTIREL_SERVICE type ref to CL_GBT_MULTIRELATED_SERVICE
    exporting
      !E_RETCODE type I
      !E_ERR_STR type STRING .
protected section.
private section.

  constants GC_DELMS type CHAR5 value ',|T;' ##NO_TEXT.
  data GT_PRSLOG type BAPIRET2_T .
  data GF_LIST type SAP_BOOL .
  data GR_TABLREF type ref to CL_SALV_TABLE .
  constants GC_ARCHIVE_PATH type CHAR8 value 'archive/' ##NO_TEXT.
  constants GC_BSLASH type CHAR1 value '/' ##NO_TEXT.

  methods ADD_ERROR
    importing
      !I_MESSAGE type STRING .
  methods GET_FPATH
    importing
      !I_FILE type STRING
      !I_TYPE type CHAR01
    exporting
      !E_FPATH type STRING
      !E_FNAME type STRING
      !E_FULLPATH type STRING .
ENDCLASS.



CLASS ZCL_CA_UTILITY_V2 IMPLEMENTATION.


  METHOD write_file.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD WRITE_FILE                                                                                                                       |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-JAN-2019 |Richa Beri          |               | D4SK900211     | Write file to Application Server/Presentation Server                 |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for Internal tables
*----------------------------------------------------------------------*
    DATA:
      ltpgm_const_values TYPE TABLE OF zspgm_const_values,
      lterror_const      TYPE TABLE OF zserror_const.

*----------------------------------------------------------------------*
* Declaration for Table and Range
*----------------------------------------------------------------------*
    DATA:
      lt_string TYPE TABLE OF string,
      lr_file   TYPE RANGE OF string,
      lt_fcat   TYPE lvc_t_fcat.
*----------------------------------------------------------------------*
* Declaration for Work area
*----------------------------------------------------------------------*
    DATA:
      lw_string     TYPE          string,
      lv_ref_line   TYPE REF TO   data,
      lo_exceptions TYPE REF TO cx_root.
*----------------------------------------------------------------------*
* Declaration for Field symbols
*----------------------------------------------------------------------*
    FIELD-SYMBOLS:
      <lfs_data_tab> TYPE STANDARD TABLE,
      <lfs_input>    TYPE any, " Generic input file structure
      <lfs_fld>      TYPE any.
*----------------------------------------------------------------------*
* Declaration for variables
*----------------------------------------------------------------------*
    DATA:
      lv_filename TYPE string,
      lv_fname    TYPE salfile-longname,
      lv_len1     TYPE i,
      lv_dir      TYPE string,
      lv_file     TYPE string,
      lv_len      TYPE i,
      lv_cnt      TYPE i,
      lv_tabix    TYPE sy-tabix,
      lv_string   TYPE string,
      lv_temp     TYPE string,
      lv_delm     TYPE string , " char2 ,
      lv_t        TYPE char1,
      lv_o        TYPE char1,
      lv_a        TYPE char1.

* Get the value for the constants
    CALL FUNCTION 'ZUTIL_PGM_CONSTANTS'
      EXPORTING
        im_pgmid               = 'ZCL_CA_UTILITY=>WRITE_FILE'
      TABLES
        t_pgm_const_values     = ltpgm_const_values
        t_error_const          = lterror_const
      EXCEPTIONS
        ex_no_entries_found    = 1
        ex_const_entry_missing = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          MESSAGE e007(zfi_msgs) WITH 'TVARVC'(001).
        WHEN 2.
          MESSAGE e010(zfi_msgs) WITH 'TVARVC'(001).
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_t) WITH KEY const_name = 'P_T'.
    IF sy-subrc = 0.
      lv_t = lw_t-low.
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_o) WITH KEY const_name = 'P_O'.
    IF sy-subrc = 0.
      lv_o = lw_o-low.
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_a) WITH KEY const_name = 'P_A'.
    IF sy-subrc = 0.
      lv_a = lw_a-low.
    ENDIF.

***---> Check i_delim with I_ALTDELIM
    IF i_delimiter IS NOT INITIAL .
     lv_delm = i_delimiter.
    ELSEIF i_altdelim IS NOT INITIAL .
     lv_delm = i_altdelim .
    ENDIF .

    lv_filename = i_filename.

* Check Delimeter is valid
* Use tab char util for excel
    IF lv_delm IS INITIAL OR
       lv_delm CA  gc_delms.

    ELSE.
      RAISE invalid_delimiter.
    ENDIF.

    IF lv_delm = lv_t.
      lv_delm = cl_abap_char_utilities=>horizontal_tab.
    ENDIF.

* Get internal table into String table with Separator
    CALL METHOD me->fcat_from_internal_table
      EXPORTING
        i_table = i_datatab
      RECEIVING
        rt_fcat = lt_fcat.

* Conver parametrs to string
    CALL METHOD me->convert_table_to_string
      EXPORTING
        i_delimiter = lv_delm
        i_datatab   = i_datatab
        i_fcat      = lt_fcat
      IMPORTING
        e_datatab   = lt_string.


    IF i_addheader = gc_char_x.

      LOOP AT i_fields ASSIGNING <lfs_input>.
        IF sy-tabix = 1.
          MOVE <lfs_input> TO lv_string.
          CONTINUE.
        ENDIF.
        CLEAR lv_temp.
        MOVE <lfs_input> TO lv_temp.
        CONCATENATE lv_string lv_temp
               INTO lv_string SEPARATED BY lv_delm.
      ENDLOOP.

      INSERT lv_string  INTO lt_string INDEX 1.
    ENDIF.

* File Download
    CASE i_source.

* Server File - Application Server
      WHEN gc_app_server.
        TRY.
* Download the file to app server
            IF i_accessmode EQ lv_o.
* Create new file
              OPEN DATASET lv_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
            ELSEIF i_accessmode EQ lv_a.
* Append to the existing file
              OPEN DATASET lv_filename FOR APPENDING IN TEXT MODE ENCODING DEFAULT.
            ENDIF.
            IF sy-subrc <> 0.
              RAISE write_error.
            ENDIF.
            LOOP AT lt_string INTO lw_string.
              TRANSFER lw_string TO lv_filename.
            ENDLOOP.
            CLOSE DATASET lv_filename.

            IF sy-subrc <> 0.
              RAISE write_error.
            ENDIF.
          CATCH cx_root INTO lo_exceptions.
            RAISE write_error.
        ENDTRY.

* Local File - Presentation Server
      WHEN gc_loc_server.

        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            filename                = lv_filename
            write_field_separator   = gc_char_x
          CHANGING
            data_tab                = lt_string
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            not_supported_by_gui    = 22
            error_no_gui            = 23
            OTHERS                  = 24.
        IF sy-subrc <> 0.
          RAISE write_error.
        ENDIF.
      WHEN OTHERS.
        RAISE invalid_delimiter.
    ENDCASE.

  ENDMETHOD.


  method SEND_MAIL_MIME.
*{   INSERT         DFDK907373                                        1
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD SEND_MAIL_MIME                                                                                                                   |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 01-JUN-2021 |477237              |               | DFDK907372     | Send Mail using MIME Object                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for referance variables
*----------------------------------------------------------------------*
    DATA:
      lr_send_request  TYPE REF TO cl_bcs,
      lr_document      TYPE REF TO cl_document_bcs,
      lr_recipient     TYPE REF TO if_recipient_bcs,
      lr_sender        TYPE REF TO if_sender_bcs,
      lr_bcs_exception TYPE REF TO cx_bcs,
      lr_ca_utility    TYPE REF TO zcl_ca_utility.
*----------------------------------------------------------------------*
* Declaration for variables
*----------------------------------------------------------------------*
    DATA:
      lv_sent_to_all TYPE os_boolean,
      lv_idx1        TYPE i,
      lv_idx2        TYPE i,
      lv_temp_idx    TYPE i,
      lv_xstring     TYPE xstring,
      lv_error       TYPE string,
      lv_size        TYPE so_obj_len,
      lv_sysid       TYPE ad_symbdst,
      lt_attach      TYPE solix_tab.

*----------------------------------------------------------------------*
* Declaration for Internal Tables
*----------------------------------------------------------------------*
    DATA:
      lt_adrs            TYPE TABLE OF string,
      lt_sender_adrs     TYPE TABLE OF string,
      ltpgm_const_values TYPE TABLE OF zspgm_const_values,
      lterror_const      TYPE TABLE OF zserror_const.

*----------------------------------------------------------------------*
* Declaration for Work Areas
*----------------------------------------------------------------------*
    DATA:
      lw_receiver   TYPE ad_smtpadr,
      lw_sender     TYPE ad_smtpadr,
      lw_username   TYPE ad_uname,
      lw_attribute  TYPE zsca_packlist,
      lw_dl_list    TYPE so_obj_nam,
      lw_attachment TYPE solix,
      lw_adrs       LIKE LINE OF lt_adrs.

* FM used to retrieve all constants
    CALL FUNCTION 'ZUTIL_PGM_CONSTANTS'
      EXPORTING
        im_pgmid               = 'ZCL_CA_UTILITY=>SEND_MAIL'
      TABLES
        t_pgm_const_values     = ltpgm_const_values
        t_error_const          = lterror_const
      EXCEPTIONS
        ex_no_entries_found    = 1
        ex_const_entry_missing = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          MESSAGE e007(zfi_msgs) WITH 'TVARVC'(046).
        WHEN 2.
          MESSAGE e010(zfi_msgs) WITH 'TVARVC'(046).
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_000) WITH KEY const_name = 'P_000'.
    IF sy-subrc NE 0.
      MESSAGE e025(zfi_msgs) WITH 'P_000'(047).
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_001) WITH KEY const_name = 'P_001'.
    IF sy-subrc NE 0.
      MESSAGE e025(zfi_msgs) WITH 'P_001'(048).
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_002) WITH KEY const_name = 'P_002'.
    IF sy-subrc NE 0.
      MESSAGE e025(zfi_msgs) WITH 'P_002'(049).
    ENDIF.

    TRY.
*--Create persistent send request
        lr_send_request = cl_bcs=>create_persistent( ).
**********************************************************************
*--------------------------------------------------------------------*
**********************************************************************
* Add Sender (e-mail address)
        IF i_sender IS NOT INITIAL.
          CASE i_sen_type.
            WHEN lw_000-low. "Email Address
              SPLIT i_sender AT ';' INTO TABLE lt_sender_adrs.
              LOOP AT lt_sender_adrs INTO lw_sender.

                lr_sender = cl_cam_address_bcs=>create_internet_address(
                                              lw_sender ).
                "-- Add Sender
                CALL METHOD lr_send_request->set_sender
                  EXPORTING
                    i_sender = lr_sender.
                CLEAR lw_sender.
              ENDLOOP.
            WHEN lw_001-low. "Distribution List

              SPLIT i_sender AT ';' INTO TABLE lt_sender_adrs.
              LOOP AT lt_sender_adrs INTO lw_sender.
                lr_sender = cl_cam_address_bcs=>create_internet_address(
                                              lw_sender ).
                "-- Add Recepient
                CALL METHOD lr_send_request->set_sender
                  EXPORTING
                    i_sender = lr_sender.
                CLEAR lw_sender.
              ENDLOOP.
            WHEN lw_002-low. "SAP User
              lv_sysid = sy-sysid.
              SPLIT i_sender AT ';' INTO TABLE lt_sender_adrs.
              LOOP AT lt_sender_adrs INTO lw_username.

                lr_sender = cl_sapuser_bcs=>create( lw_username ).
*-- Add Sender
                CALL METHOD lr_send_request->set_sender
                  EXPORTING
                    i_sender = lr_sender.
                CLEAR lw_username.
              ENDLOOP.
          ENDCASE.
        ENDIF.
**********************************************************************
*--------------------------------------------------------------------*
**********************************************************************
* add recipient (e-mail address)
* Create a internet address if the receiver type is INT
* Check whether we have an Email Receiver, Distribution List, or SAPUser

        CASE i_rec_type.
          WHEN lw_000-low. "Email Address
            SPLIT i_receiver AT ';' INTO TABLE lt_adrs.
            LOOP AT lt_adrs INTO lw_receiver.

              lr_recipient = cl_cam_address_bcs=>create_internet_address(
                                            lw_receiver ).
              "-- Add Recepient
              CALL METHOD lr_send_request->add_recipient
                EXPORTING
                  i_recipient = lr_recipient.
            ENDLOOP.
          WHEN lw_001-low. "Distribution List

            SPLIT i_receiver AT ';' INTO TABLE lt_adrs.
            LOOP AT lt_adrs INTO lw_receiver.
              lr_recipient = cl_cam_address_bcs=>create_internet_address(
                                            lw_receiver ).
              "-- Add Recepient
              CALL METHOD lr_send_request->add_recipient
                EXPORTING
                  i_recipient = lr_recipient.
            ENDLOOP.
          WHEN lw_002-low. "SAP User
            lv_sysid = sy-sysid.

            SPLIT i_receiver AT ';' INTO TABLE lt_adrs.
            LOOP AT lt_adrs INTO lw_username.

              lr_recipient = cl_sapuser_bcs=>create( lw_username ).

*-- Add Recepient
              CALL METHOD lr_send_request->add_recipient
                EXPORTING
                  i_recipient = lr_recipient.
            ENDLOOP.
        ENDCASE.

* Create the document with the MIME Object
        lr_document = cl_document_bcs=>create_from_multirelated(
                i_subject           = i_subject
                i_multirel_service  = i_multirel_service ).

* Attachment handling with the packing list
* Check contents of attachement is not empty
* If the contents are there for the attcahemnt then the packing
* list should not be empty
        READ TABLE i_attachment_attribute INTO lw_attribute INDEX 1.
        IF sy-subrc = 0 AND lw_attribute = 'XML'.

          DATA: lv_length TYPE i.
          CALL METHOD cl_bcs_convert=>solix_to_xstring
            EXPORTING
              it_solix   = lt_attach
            RECEIVING
              ev_xstring = lv_xstring.

          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer        = lv_xstring
            IMPORTING
              output_length = lv_length
            TABLES
              binary_tab    = lt_attach.

          MOVE lv_length TO lv_size.
          CALL METHOD lr_document->add_attachment
            EXPORTING
              i_attachment_type    = lw_attribute-doc_type
              i_attachment_subject = lw_attribute-obj_descr
              i_attachment_size    = lv_size
              i_att_content_hex    = lt_attach.

        ELSE.

          IF i_attachment IS NOT INITIAL.
            IF i_attachment_attribute IS NOT INITIAL.
              LOOP AT i_attachment_attribute INTO lw_attribute.
                CLEAR : lv_idx1, lv_idx2, lv_temp_idx.
                lv_idx1 = lw_attribute-body_start.
                lv_temp_idx = lw_attribute-body_num.
                lv_idx2 = ( lv_idx1 + lv_temp_idx ) - 1.

* Loop across the attachement contents internal table and transfer the data
* for particular attachments into another internal table and attach it to
* the mail sending class : cl_document_bcs.
                REFRESH lt_attach.
                LOOP AT i_attachment INTO lw_attachment FROM lv_idx1 TO lv_idx2.
                  APPEND lw_attachment TO lt_attach.
                  CLEAR lw_attachment.
                ENDLOOP.

* Now the attachement data for particular attachment is ready in lt_attach so
* now we add the attachment to the docuement.
* Calculate the size of the attachment after converting it to xstring (binary content)
                CLEAR lv_size.
                CALL METHOD cl_bcs_convert=>solix_to_xstring
                  EXPORTING
                    it_solix   = lt_attach
                  RECEIVING
                    ev_xstring = lv_xstring.

                lv_size = xstrlen( lv_xstring ).
                CALL METHOD lr_document->add_attachment
                  EXPORTING
                    i_attachment_type    = lw_attribute-doc_type
                    i_attachment_subject = lw_attribute-obj_descr
                    i_attachment_size    = lv_size
                    i_att_content_hex    = lt_attach.
                CLEAR lw_attribute.
              ENDLOOP.
            ELSE.

* If attachement attribute is not given then send the mail as
* default dpcument type RAW.
              CALL METHOD cl_bcs_convert=>solix_to_xstring
                EXPORTING
                  it_solix   = i_attachment
                RECEIVING
                  ev_xstring = lv_xstring.

              lv_size = xstrlen( lv_xstring ).
              CALL METHOD lr_document->add_attachment
                EXPORTING
                  i_attachment_type    = 'RAW'
                  i_attachment_subject = i_subject
                  i_attachment_size    = lv_size
                  i_att_content_hex    = i_attachment.

            ENDIF.
          ENDIF.
        ENDIF.

*Add document to send request
        CALL METHOD lr_send_request->set_document( lr_document ).

*Set the priority
        IF i_priority IS NOT INITIAL.
          CALL METHOD lr_send_request->set_priority
            EXPORTING
              i_priority = i_priority.
        ENDIF.


* Send Immediately
        CALL METHOD lr_send_request->set_send_immediately( i_immediate ).

* Send document
        CALL METHOD lr_send_request->send(
          EXPORTING
            i_with_error_screen = abap_true
          RECEIVING
            result              = lv_sent_to_all ).

        IF lv_sent_to_all = abap_true.
          e_retcode = 0.
          IF i_immediate IS NOT INITIAL.
            COMMIT WORK.
          ENDIF.
        ELSE.
          e_retcode = 4.
        ENDIF.

*Exception handling
      CATCH cx_bcs INTO lr_bcs_exception.
        e_retcode = 4.
        lv_error = lr_bcs_exception->get_text( ).
        e_err_str = lv_error.
    ENDTRY.

*}   INSERT
  endmethod.


  METHOD send_mail.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD SEND_MAIL                                                                                                                        |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-JAN-2019 |Richa Beri          |               | D4SK900211     | Send Mail                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 06-SEP-2019 |Richa Beri          |DFT1POST-58    | D4SK906048     | Change Logic for Distribution List for email trigger                 |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for referance variables
*----------------------------------------------------------------------*
    DATA:
      lr_send_request  TYPE REF TO cl_bcs,
      lr_document      TYPE REF TO cl_document_bcs,
      lr_recipient     TYPE REF TO if_recipient_bcs,
      lr_sender        TYPE REF TO if_sender_bcs,
      lr_bcs_exception TYPE REF TO cx_bcs,
      lr_ca_utility    TYPE REF TO zcl_ca_utility.
*----------------------------------------------------------------------*
* Declaration for variables
*----------------------------------------------------------------------*
    DATA:
      lv_sent_to_all TYPE os_boolean,
      lv_idx1        TYPE i,
      lv_idx2        TYPE i,
      lv_temp_idx    TYPE i,
      lv_xstring     TYPE xstring,
      lv_error       TYPE string,
      lv_size        TYPE so_obj_len,
      lv_sysid       TYPE ad_symbdst,
      lt_attach      TYPE solix_tab.

*----------------------------------------------------------------------*
* Declaration for Internal Tables
*----------------------------------------------------------------------*
    DATA:
      lt_adrs            TYPE TABLE OF string,
      lt_sender_adrs     TYPE TABLE OF string,
      ltpgm_const_values TYPE TABLE OF zspgm_const_values,
      lterror_const      TYPE TABLE OF zserror_const.

*----------------------------------------------------------------------*
* Declaration for Work Areas
*----------------------------------------------------------------------*
    DATA:
      lw_receiver   TYPE ad_smtpadr,
      lw_sender     TYPE ad_smtpadr,
      lw_username   TYPE ad_uname,
      lw_attribute  TYPE zsca_packlist,
      lw_dl_list    TYPE so_obj_nam,
      lw_attachment TYPE solix,
      lw_adrs       LIKE LINE OF lt_adrs.

* FM used to retrieve all constants
    CALL FUNCTION 'ZUTIL_PGM_CONSTANTS'
      EXPORTING
        im_pgmid               = 'ZCL_CA_UTILITY=>SEND_MAIL'
      TABLES
        t_pgm_const_values     = ltpgm_const_values
        t_error_const          = lterror_const
      EXCEPTIONS
        ex_no_entries_found    = 1
        ex_const_entry_missing = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          MESSAGE e007(zfi_msgs) WITH 'TVARVC'(046).
        WHEN 2.
          MESSAGE e010(zfi_msgs) WITH 'TVARVC'(046).
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_000) WITH KEY const_name = 'P_000'.
    IF sy-subrc NE 0.
      MESSAGE e025(zfi_msgs) WITH 'P_000'(047).
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_001) WITH KEY const_name = 'P_001'.
    IF sy-subrc NE 0.
      MESSAGE e025(zfi_msgs) WITH 'P_001'(048).
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_002) WITH KEY const_name = 'P_002'.
    IF sy-subrc NE 0.
      MESSAGE e025(zfi_msgs) WITH 'P_002'(049).
    ENDIF.

    TRY.
*--Create persistent send request
        lr_send_request = cl_bcs=>create_persistent( ).
**********************************************************************
*--------------------------------------------------------------------*
**********************************************************************
* Add Sender (e-mail address)
        IF i_sender IS NOT INITIAL.
          CASE i_sen_type.
            WHEN lw_000-low. "Email Address
              SPLIT i_sender AT ';' INTO TABLE lt_sender_adrs.
              LOOP AT lt_sender_adrs INTO lw_sender.

                lr_sender = cl_cam_address_bcs=>create_internet_address(
                                              lw_sender ).
                "-- Add Sender
                CALL METHOD lr_send_request->set_sender
                  EXPORTING
                    i_sender = lr_sender.
                CLEAR lw_sender.
              ENDLOOP.
            WHEN lw_001-low. "Distribution List

              SPLIT i_sender AT ';' INTO TABLE lt_sender_adrs.
              LOOP AT lt_sender_adrs INTO lw_sender.
                lr_sender = cl_cam_address_bcs=>create_internet_address(
                                              lw_sender ).
                "-- Add Recepient
                CALL METHOD lr_send_request->set_sender
                  EXPORTING
                    i_sender = lr_sender.
                CLEAR lw_sender.
              ENDLOOP.
            WHEN lw_002-low. "SAP User
              lv_sysid = sy-sysid.
              SPLIT i_sender AT ';' INTO TABLE lt_sender_adrs.
              LOOP AT lt_sender_adrs INTO lw_username.

                lr_sender = cl_sapuser_bcs=>create( lw_username ).
*-- Add Sender
                CALL METHOD lr_send_request->set_sender
                  EXPORTING
                    i_sender = lr_sender.
                CLEAR lw_username.
              ENDLOOP.
          ENDCASE.
        ENDIF.
**********************************************************************
*--------------------------------------------------------------------*
**********************************************************************
* add recipient (e-mail address)
* Create a internet address if the receiver type is INT
* Check whether we have an Email Receiver, Distribution List, or SAPUser

        CASE i_rec_type.
          WHEN lw_000-low. "Email Address
            SPLIT i_receiver AT ';' INTO TABLE lt_adrs.
            LOOP AT lt_adrs INTO lw_receiver.

              lr_recipient = cl_cam_address_bcs=>create_internet_address(
                                            lw_receiver ).
              "-- Add Recepient
              CALL METHOD lr_send_request->add_recipient
                EXPORTING
                  i_recipient = lr_recipient.
            ENDLOOP.
          WHEN lw_001-low. "Distribution List

            SPLIT i_receiver AT ';' INTO TABLE lt_adrs.
            LOOP AT lt_adrs INTO lw_receiver.
              lr_recipient = cl_cam_address_bcs=>create_internet_address(
                                            lw_receiver ).
              "-- Add Recepient
              CALL METHOD lr_send_request->add_recipient
                EXPORTING
                  i_recipient = lr_recipient.
            ENDLOOP.
          WHEN lw_002-low. "SAP User
            lv_sysid = sy-sysid.

            SPLIT i_receiver AT ';' INTO TABLE lt_adrs.
            LOOP AT lt_adrs INTO lw_username.

              lr_recipient = cl_sapuser_bcs=>create( lw_username ).

*-- Add Recepient
              CALL METHOD lr_send_request->add_recipient
                EXPORTING
                  i_recipient = lr_recipient.
            ENDLOOP.
        ENDCASE.

* create subject and body of the mail
* Create the document with both subject and body.
        lr_document = cl_document_bcs=>create_document(
                i_type    = i_content
                i_text    = i_body
                i_length  = '12'
                i_subject = i_subject ).

* Attachment handling with the packing list
* Check contents of attachement is not empty
* If the contents are there for the attcahemnt then the packing
* list should not be empty
        READ TABLE i_attachment_attribute INTO lw_attribute INDEX 1.
        IF sy-subrc = 0 AND lw_attribute = 'XML'.

          DATA: lv_length TYPE i.
          CALL METHOD cl_bcs_convert=>solix_to_xstring
            EXPORTING
              it_solix   = lt_attach
            RECEIVING
              ev_xstring = lv_xstring.

          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer        = lv_xstring
            IMPORTING
              output_length = lv_length
            TABLES
              binary_tab    = lt_attach.

          MOVE lv_length TO lv_size.
          CALL METHOD lr_document->add_attachment
            EXPORTING
              i_attachment_type    = lw_attribute-doc_type
              i_attachment_subject = lw_attribute-obj_descr
              i_attachment_size    = lv_size
              i_att_content_hex    = lt_attach.

        ELSE.

          IF i_attachment IS NOT INITIAL.
            IF i_attachment_attribute IS NOT INITIAL.
              LOOP AT i_attachment_attribute INTO lw_attribute.
                CLEAR : lv_idx1, lv_idx2, lv_temp_idx.
                lv_idx1 = lw_attribute-body_start.
                lv_temp_idx = lw_attribute-body_num.
                lv_idx2 = ( lv_idx1 + lv_temp_idx ) - 1.

* Loop across the attachement contents internal table and transfer the data
* for particular attachments into another internal table and attach it to
* the mail sending class : cl_document_bcs.
                REFRESH lt_attach.
                LOOP AT i_attachment INTO lw_attachment FROM lv_idx1 TO lv_idx2.
                  APPEND lw_attachment TO lt_attach.
                  CLEAR lw_attachment.
                ENDLOOP.

* Now the attachement data for particular attachment is ready in lt_attach so
* now we add the attachment to the docuement.
* Calculate the size of the attachment after converting it to xstring (binary content)
                CLEAR lv_size.
                CALL METHOD cl_bcs_convert=>solix_to_xstring
                  EXPORTING
                    it_solix   = lt_attach
                  RECEIVING
                    ev_xstring = lv_xstring.

                lv_size = xstrlen( lv_xstring ).
                CALL METHOD lr_document->add_attachment
                  EXPORTING
                    i_attachment_type    = lw_attribute-doc_type
                    i_attachment_subject = lw_attribute-obj_descr
                    i_attachment_size    = lv_size
                    i_att_content_hex    = lt_attach.
                CLEAR lw_attribute.
              ENDLOOP.
            ELSE.

* If attachement attribute is not given then send the mail as
* default dpcument type RAW.
              CALL METHOD cl_bcs_convert=>solix_to_xstring
                EXPORTING
                  it_solix   = i_attachment
                RECEIVING
                  ev_xstring = lv_xstring.

              lv_size = xstrlen( lv_xstring ).
              CALL METHOD lr_document->add_attachment
                EXPORTING
                  i_attachment_type    = 'RAW'
                  i_attachment_subject = i_subject
                  i_attachment_size    = lv_size
                  i_att_content_hex    = i_attachment.

            ENDIF.
          ENDIF.
        ENDIF.

*Add document to send request
        CALL METHOD lr_send_request->set_document( lr_document ).

*Set the priority
        IF i_priority IS NOT INITIAL.
          CALL METHOD lr_send_request->set_priority
            EXPORTING
              i_priority = i_priority.
        ENDIF.


* Send Immediately
        CALL METHOD lr_send_request->set_send_immediately( i_immediate ).

* Send document
        CALL METHOD lr_send_request->send(
          EXPORTING
            i_with_error_screen = abap_true
          RECEIVING
            result              = lv_sent_to_all ).

        IF lv_sent_to_all = abap_true.
          e_retcode = 0.
          IF i_immediate IS NOT INITIAL.
            COMMIT WORK.
          ENDIF.
        ELSE.
          e_retcode = 4.
        ENDIF.

*Exception handling
      CATCH cx_bcs INTO lr_bcs_exception.
        e_retcode = 4.
        lv_error = lr_bcs_exception->get_text( ).
        e_err_str = lv_error.
    ENDTRY.

  ENDMETHOD.


  METHOD select_file.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD SELECT_FILE                                                                                                                      |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-JAN-2019 |Richa Beri          |               | D4SK900211     | F4 help for filename from App Server/Presentation Server             |
*&-----------------------------------------------------------------------------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Declaration for Variables
*----------------------------------------------------------------------*
    DATA : lv_appl    TYPE as4flag,
           lv_pre     TYPE as4flag,
           lv_al11    TYPE as4flag,
           lv_logical TYPE as4flag,
           lv_msg1    TYPE msgv1.

    IF i_source = gc_app_server.
      lv_appl = gc_char_x.
    ENDIF.

* Based on the source of file, Application or Presentation, select the variable
    CASE i_apppath_type.
      WHEN gc_app_server.
        lv_al11 = gc_char_x.
      WHEN gc_lpath.
        lv_logical = gc_char_x.
    ENDCASE.

    IF lv_al11 IS INITIAL AND lv_logical IS INITIAL.
      lv_al11 = gc_char_x.
      lv_logical = gc_char_x.
    ENDIF.

* Call method for F4 help
    CALL METHOD cl_rsan_ut_files=>f4
      EXPORTING
        i_applserv              = lv_appl
        i_title                 = i_title
        i_applserv_logical      = lv_logical
        i_applserv_al11         = lv_al11
        i_gui_extension         = i_gui_extension
        i_gui_ext_filter        = i_gui_ext_filter
        i_gui_initial_directory = i_gui_initial_directory
      CHANGING
        c_file_name             = e_filename
      EXCEPTIONS
        failed                  = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
* Raise exception
      RAISE f4_help_failed.
    ENDIF.

  ENDMETHOD.


  METHOD read_file.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD READ_FILE                                                                                                                        |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-JAN-2019 |Richa Beri          |               | D4SK900211     | Read the file from Application Server/Presentation Server            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for Internal tables
*----------------------------------------------------------------------*
    DATA:
      lt_split           TYPE TABLE OF char200,
      lt_string          TYPE TABLE OF string,
      ltpgm_const_values TYPE TABLE OF zspgm_const_values,
      lterror_const      TYPE TABLE OF zserror_const.

*----------------------------------------------------------------------*
* Declaration for work areas
*----------------------------------------------------------------------*
    DATA:
      lw_string TYPE          string,
      lw_split  TYPE          char200,
      lw_flcat  TYPE LINE OF  lvc_t_fcat.

*----------------------------------------------------------------------*
* Declaration for Reference Objects
*----------------------------------------------------------------------*
    DATA:
      lo_ref_line   TYPE REF TO   data,
      lo_exceptions TYPE REF TO cx_root.

*----------------------------------------------------------------------*
* Declaration for Field Symbols
*----------------------------------------------------------------------*
    FIELD-SYMBOLS:
      <fs_data_tab> TYPE STANDARD TABLE,
      <fs_input>    TYPE any, " Generic input file structure
      <fs_fld>      TYPE any.

*----------------------------------------------------------------------*
* Declaration for Variables
*----------------------------------------------------------------------*
    DATA:
      lv_filename TYPE string,
      lv_len      TYPE i,
      lv_file     TYPE string,
      lv_tabix    TYPE sy-tabix,
      lv_delm     TYPE char1,
      lv_t        TYPE char1.

* Get the value for the constants
    CALL FUNCTION 'ZUTIL_PGM_CONSTANTS'
      EXPORTING
        im_pgmid               = 'ZCL_CA_UTILITY=>READ_FILE'
      TABLES
        t_pgm_const_values     = ltpgm_const_values
        t_error_const          = lterror_const
      EXCEPTIONS
        ex_no_entries_found    = 1
        ex_const_entry_missing = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          MESSAGE e007(zfi_msgs) WITH 'TVARVC'(001).
        WHEN 2.
          MESSAGE e010(zfi_msgs) WITH 'TVARVC'(001).
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_t) WITH KEY const_name = 'P_T'.
    IF sy-subrc = 0.
      lv_t = lw_t-low.
    ENDIF.

* Refresh internal table and clears work area
    REFRESH: lt_split, lt_string.
    CLEAR:lw_string, lw_split, lw_flcat,
          lv_filename, lv_len, lv_file, lv_tabix, lv_delm.

    lv_delm = i_delimiter.
    lv_filename = i_filename.

* Check Delimeter is valid
    IF lv_delm IS INITIAL OR
       lv_delm CA gc_delms.

    ELSE.
      RAISE invalid_delimeter.
    ENDIF.

* Use tab char util for excel
    IF lv_delm = lv_t.
      lv_delm = cl_abap_char_utilities=>horizontal_tab.
    ENDIF.

    CASE i_source.
* Server File - Application Server
      WHEN gc_app_server.
        TRY.
            IF i_codepage IS NOT INITIAL.
              OPEN DATASET lv_filename FOR INPUT IN LEGACY
                             TEXT MODE CODE PAGE i_codepage .
            ELSE.
              OPEN DATASET lv_filename FOR INPUT IN TEXT MODE
                                             ENCODING DEFAULT.
            ENDIF.

            IF sy-subrc <> 0.
              RAISE cannot_open_file.
            ENDIF.

* read the record from file and move it to string type variable
            DO.
              READ DATASET lv_filename INTO  lv_file .
              IF sy-subrc NE 0.
                EXIT.
              ELSE.
                APPEND lv_file TO lt_string.
              ENDIF.
            ENDDO.

            CLOSE DATASET  lv_filename.
          CATCH cx_root INTO lo_exceptions.
            RAISE cannot_open_file.
        ENDTRY.

* Local File - Presentation Server
      WHEN gc_loc_server.

        CALL METHOD cl_gui_frontend_services=>gui_upload
          EXPORTING
            filename                = lv_filename
            codepage                = i_codepage
          CHANGING
            data_tab                = lt_string
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            not_supported_by_gui    = 17
            error_no_gui            = 18
            OTHERS                  = 19.
        IF sy-subrc <> 0.
          RAISE error_in_read.
        ENDIF.

      WHEN OTHERS.
        RAISE invalid_source.
    ENDCASE.

* If the file has header row delete the first row
    IF i_hdr = gc_char_x.
      DELETE lt_string INDEX 1.
    ENDIF.

* Create Output structure and Fill dynamically
    ASSIGN: e_datatab TO <fs_data_tab>.
    CALL METHOD me->fcat_from_internal_table
      EXPORTING
        i_table = e_datatab
      RECEIVING
        rt_fcat = DATA(it_fcat).
    DESCRIBE TABLE it_fcat LINES DATA(lv_lines).

    CREATE DATA lo_ref_line LIKE LINE OF <fs_data_tab>.
    ASSIGN lo_ref_line->* TO <fs_input>.

    "--> Write File Data to Internal Table
    LOOP AT lt_string INTO lw_string.
      CLEAR lv_tabix.

* If File name Column exsists assign File Name
      ASSIGN COMPONENT gc_fld_fname OF STRUCTURE <fs_input> TO <fs_fld>.
      IF sy-subrc = 0.
        lv_tabix  = lv_tabix + 1.
        <fs_fld>  = i_filename.
      ENDIF.

* If Row  Column exsists assign Row
      ASSIGN COMPONENT gc_fld_rowno OF STRUCTURE <fs_input> TO <fs_fld>.
      IF sy-subrc = 0.
        lv_tabix  = lv_tabix + 1.
        <fs_fld>     = sy-tabix.
      ENDIF.

* Assign Value to Column Dynamically
      IF lv_delm IS NOT INITIAL.
        SPLIT lw_string  AT lv_delm INTO TABLE lt_split.
        LOOP AT lt_split INTO lw_split.
          lv_tabix = lv_tabix + 1.
          ASSIGN COMPONENT lv_tabix OF STRUCTURE
                          <fs_input> TO <fs_fld>.
          IF sy-subrc EQ 0.
            <fs_fld> = lw_split.
          ENDIF.
        ENDLOOP.

* String Table
      ELSEIF lv_lines = 1.
        <fs_fld> = lw_string.
      ELSE.
        LOOP AT it_fcat INTO lw_flcat WHERE fieldname NE gc_fld_fname OR
                                            fieldname NE gc_fld_rowno.

          ASSIGN COMPONENT lw_flcat-fieldname OF STRUCTURE <fs_input> TO <fs_fld>.
          <fs_fld>  = lw_string.
          lv_len   = lw_flcat-intlen.
          SHIFT lw_string BY lv_len PLACES.
        ENDLOOP.
      ENDIF.

      APPEND <fs_input> TO e_datatab.
      CLEAR <fs_input>.
    ENDLOOP.
    UNASSIGN: <fs_data_tab>, <fs_input>.

  ENDMETHOD.


 METHOD read_csv_file.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD READ_CSV_FILE                                                                                                                        |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 31-JUL-2019 |Sneha Sharma        |               | D4SK904566     | Read the  CSV file from Application Server/Presentation Server       |
* 02-SEP-2019 |Siddhartha Deepak   | DFT1POST-371  | D4SK906905     | Defect where uploaded field value arenot getting cleared             |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for structure
*----------------------------------------------------------------------*
   TYPES: BEGIN OF ty_intern_struc,
            row        TYPE  kcd_ex_row_n,
            col        TYPE  kcd_ex_col_n,
            value(241) TYPE  c,
          END   OF ty_intern_struc.

*----------------------------------------------------------------------*
* Declaration for Internal tables
*----------------------------------------------------------------------*
   DATA:
     lt_split           TYPE TABLE OF char200,
     lt_string          TYPE TABLE OF string,
     ltpgm_const_values TYPE TABLE OF zspgm_const_values,
     lterror_const      TYPE TABLE OF zserror_const,
     lt_intern          TYPE  STANDARD TABLE OF  ty_intern_struc INITIAL SIZE 0.

*----------------------------------------------------------------------*
* Declaration for work areas
*----------------------------------------------------------------------*
   DATA:
     lw_string TYPE          string,
     lw_split  TYPE          char200,
     lw_flcat  TYPE LINE OF  lvc_t_fcat.

*----------------------------------------------------------------------*
* Declaration for Variables
*----------------------------------------------------------------------*
   DATA:
     lv_filename TYPE rlgrap-filename,
     lv_len      TYPE i,
     lv_file     TYPE string,
     lv_tabix    TYPE sy-tabix,
     lv_delm     TYPE char1,
     lv_t        TYPE char1,
     lv_index    TYPE i,
     lw_intern   TYPE ty_intern_struc.

*----------------------------------------------------------------------*
* Declaration for Field Symbols
*----------------------------------------------------------------------*
   FIELD-SYMBOLS:
     <fs_data_tab> TYPE STANDARD TABLE,
     <fs_input>    TYPE any, " Generic input file structure
     <fs_fld>      TYPE any.

*----------------------------------------------------------------------*
* Declaration for Reference Objects
*----------------------------------------------------------------------*
   DATA:
     lo_ref_line   TYPE REF TO   data,
     lo_exceptions TYPE REF TO cx_root.


* Get the value for the constants
   CALL FUNCTION 'ZUTIL_PGM_CONSTANTS'
     EXPORTING
       im_pgmid               = 'ZCL_CA_UTILITY=>READ_FILE'
     TABLES
       t_pgm_const_values     = ltpgm_const_values
       t_error_const          = lterror_const
     EXCEPTIONS
       ex_no_entries_found    = 1
       ex_const_entry_missing = 2
       OTHERS                 = 3.
   IF sy-subrc <> 0.
     CASE sy-subrc.
       WHEN 1.
         MESSAGE e007(zfi_msgs) WITH 'TVARVC'(001).
       WHEN 2.
         MESSAGE e010(zfi_msgs) WITH 'TVARVC'(001).
       WHEN OTHERS.
     ENDCASE.
   ENDIF.

   READ TABLE ltpgm_const_values INTO DATA(lw_t) WITH KEY const_name = 'P_T'.
   IF sy-subrc = 0.
     lv_t = lw_t-low.
   ENDIF.

* Passing the values to the delimeter and filename
   lv_delm = i_delimiter.
   lv_filename = i_filename.

* Check Delimeter is valid
   IF lv_delm IS INITIAL OR
      lv_delm CA gc_delms.

   ELSE.
     RAISE invalid_delimeter.
   ENDIF.


*To upload the CSV files
   CALL FUNCTION 'Z_KCD_CSV_FILE_TO_INTERN_CONV'
     EXPORTING
       i_filename      = lv_filename
       i_separator     = lv_delm
     TABLES
       e_intern        = lt_intern
     EXCEPTIONS
       upload_csv      = 1
       upload_filetype = 2
       OTHERS          = 3.
   IF sy-subrc <> 0.
* Implement suitable error handling here
     RAISE error_in_read.
   ENDIF.


* If the file has header row delete the first row
   IF i_hdr = gc_char_x.
     DELETE lt_intern WHERE row = 1. "INDEX 1.
   ENDIF.


* Create Output structure and Fill dynamically
   ASSIGN: e_datatab TO <fs_data_tab>.
   CALL METHOD me->fcat_from_internal_table
     EXPORTING
       i_table = e_datatab
     RECEIVING
       rt_fcat = DATA(it_fcat).
   DESCRIBE TABLE it_fcat LINES DATA(lv_lines).

   CREATE DATA lo_ref_line LIKE LINE OF <fs_data_tab>.
   ASSIGN lo_ref_line->* TO <fs_input>.


*Pass the values to rows and columns of the table.
   LOOP AT lt_intern INTO lw_intern.
     MOVE : lw_intern-col TO lv_index.
     ASSIGN COMPONENT lv_index OF STRUCTURE <fs_input> TO <fs_fld>.
     REPLACE ALL OCCURRENCES OF '"' IN lw_intern-value  WITH space.
     CONDENSE lw_intern-value .
     MOVE : lw_intern-value TO <fs_fld>.
     AT END OF row.
       APPEND <fs_input> TO e_datatab.
       CLEAR: <fs_input>.                       "D4SK906905 "DFT1POST-371 Defect fix
     ENDAT.
   ENDLOOP.
   UNASSIGN: <fs_input>, <fs_data_tab>, <fs_input>.

 ENDMETHOD.


  METHOD objtref_create.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD OBJTREF_CREATE                                                                                                                   |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-JAN-2019 |Richa Beri          |               | D4SK900211     | Object Refrence Creation                                             |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for Reference Objects
*----------------------------------------------------------------------*
    DATA:
      lo_except TYPE REF TO cx_salv_msg.
*----------------------------------------------------------------------*
* Declaration for Reference Objects
*----------------------------------------------------------------------*
    DATA:
      lf_list TYPE sap_bool.

    IF if_list IS NOT INITIAL.
      lf_list = if_list.
    ELSE.
      lf_list = gf_list.
    ENDIF.

    TRY.
        CALL METHOD cl_salv_table=>factory
          EXPORTING
            list_display = lf_list
          IMPORTING
            r_salv_table = gr_tablref
          CHANGING
            t_table      = c_datatab.

      CATCH cx_salv_msg INTO lo_except.
        me->add_error( 'Exception in CL_SALV_TABLE=>FACTORY').
        me->add_error( lo_except->get_text( ) ).
        gf_is_error = abap_true.
    ENDTRY.
    r_salvref = gr_tablref.

  ENDMETHOD.


  METHOD layout_set.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD LAYOUT_SET                                                                                                                       |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-JAN-2019 |Richa Beri          |               | D4SK900211     | Layout Set                                                           |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for Reference Objects
*----------------------------------------------------------------------*
    DATA:
      lo_functions TYPE REF TO cl_salv_functions_list,
      lo_columns   TYPE REF TO cl_salv_columns_table.

    CHECK i_salvtab IS BOUND.

    lo_columns = i_salvtab->get_columns( ).
    lo_columns->set_optimize( abap_true ).

    lo_functions = i_salvtab->get_functions( ).
    lo_functions->set_all( if_salv_c_bool_sap=>true ).

  ENDMETHOD.


  METHOD idoc_trace.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD IDOC_TRACE                                                                                                                |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 04-APR-2019 |Ragadeep Jonna      |               | D4SK900211     | This method will do a RFC call to the Target system and get the Idoc |
*                                                                   | status in Target system                                              |
* 23-MAY-2019 |Fathima Ahmed       |               | D4SK902429     | Added error message when the FM IDOC_DATE_TIME_GET is not successful |
* 25-JUN-2019 |Satish Kesavalu     |               | D4SK903345     | Changed the Import Parameter to receive Receiver Partner                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for Data Objects
*----------------------------------------------------------------------*
    DATA : gv_asyn_rfc TYPE rfcdes-rfcdest,
           gv_syn_rfc  TYPE rfcdes-rfcdest,
           gv_message  TYPE char200.    "+added by 477670 | D4SK902429
*----------------------------------------------------------------------*
* Declaration for Table Types
*----------------------------------------------------------------------*
    TYPES : BEGIN OF ty_edidc,
              docnum TYPE edidc-docnum,
              status TYPE edidc-status,
              rcvprt TYPE edidc-rcvprt,
              rcvprn TYPE edidc-rcvprn,
              sndprt TYPE edidc-sndprt,
              sndprn TYPE edidc-sndprn,
              credat TYPE edidc-credat,
              cretim TYPE edidc-cretim,
              upddat TYPE edidc-upddat,
              updtim TYPE edidc-updtim,
            END OF ty_edidc.
*----------------------------------------------------------------------*
* Declaration for Work Area
*----------------------------------------------------------------------*
    DATA : gw_edidc             TYPE ty_edidc,
           gw_idocinfo          TYPE bdi_zmoni1,
           gw_idocs             TYPE zsidoc_numbers,
           gw_idoc_trace_status TYPE zsidoc_trace_status.
*----------------------------------------------------------------------*
* Declaration for Internal Tables
*----------------------------------------------------------------------*
    DATA : gt_edidc    TYPE TABLE OF ty_edidc,
           gt_idocinfo TYPE TABLE OF bdi_zmoni1.
*----------------------------------------------------------------------*
* Declaration for Constants
*----------------------------------------------------------------------*
    DATA : gc_ls                    TYPE char2 VALUE 'LS'.
    REFRESH : gt_idocinfo, gt_edidc.
    IF NOT im_idocs[] IS INITIAL.
**** Get the IDOC Header details
      SELECT docnum
             status
             rcvprt
             rcvprn
             sndprt
             sndprn
             credat
             cretim
             upddat
             updtim
        FROM edidc
        INTO TABLE gt_edidc
        FOR ALL ENTRIES IN im_idocs
        WHERE docnum = im_idocs-docnum
          AND rcvprn = im_recv_partner.

      IF sy-subrc = 0.

        LOOP AT gt_edidc INTO gw_edidc.

          CLEAR : gw_idocinfo.
          gw_idocinfo-docnum_s = gw_edidc-docnum.
          APPEND gw_idocinfo TO gt_idocinfo.

        ENDLOOP.

****      Determine the Earlier Start date of the IDOC's to trace
        SORT gt_edidc BY credat.
        CLEAR : gw_edidc.
        READ TABLE gt_edidc INTO gw_edidc INDEX 1.
        IF sy-subrc = 0.
****      Determine RFC Destination based on Receiver partner

          CALL FUNCTION 'RFC_DATA_DETERMINE_FOR_CHECKS'
            EXPORTING
              rcvprn                = im_recv_partner
              rcvprt                = gc_ls
            IMPORTING
              destination_asynch    = gv_asyn_rfc
              destination_synch     = gv_syn_rfc
*             rfc_type              =
            EXCEPTIONS
              no_parnter_definition = 1
              no_port               = 2
              no_logdes_in_port     = 3
              dest_does_not_exist   = 4
              r2_system             = 5
              external_system       = 6
              OTHERS                = 7.
          IF sy-subrc = 0 AND NOT gv_asyn_rfc IS INITIAL.

            CALL FUNCTION 'IDOC_DATE_TIME_GET'
              DESTINATION gv_syn_rfc
              EXPORTING
                selection_date        = gw_edidc-credat
                source_system         = gw_edidc-sndprn
              TABLES
                t_idocinfo            = gt_idocinfo
*------------------------+Changes done by 477670 | D4SK902429------------------------------------------*
              EXCEPTIONS
                system_failure        = 1 MESSAGE gv_message
                communication_failure = 2 MESSAGE gv_message.
            IF sy-subrc <> 0.
              MESSAGE i778(b1) WITH 'IDOC_DATE_TIME_GET' gv_syn_rfc '' gv_message.
              RAISE exc_external_system.
            ENDIF.
*------------------------+Changes done by 477670 | D4SK902429------------------------------------------*
          ELSE.
**** Raise Exceptions
            CASE sy-subrc.
              WHEN 1.
                RAISE exc_no_parnter_definition.
              WHEN 2.
                RAISE exc_no_port.
              WHEN 3.
                RAISE exc_no_logdes_in_port.
              WHEN 4.
                RAISE exc_dest_does_not_exist.
              WHEN 5.
                RAISE exc_r2_system.
              WHEN 6.
                RAISE exc_external_system.

            ENDCASE.
          ENDIF.

        ENDIF.

      ENDIF.
      SORT gt_edidc BY docnum.
*** Pass the status of every input IDoc's, If NO
      LOOP AT im_idocs INTO gw_idocs.

*** Get the Target IDOC# and IDOC Status
        CLEAR : gw_idocinfo.
        READ TABLE gt_idocinfo INTO gw_idocinfo WITH KEY docnum_s = gw_idocs-docnum.
        IF sy-subrc = 0.
          CLEAR : gw_idoc_trace_status.

***      Get Source IDOC Status
          CLEAR : gw_edidc.
          READ TABLE gt_edidc INTO gw_edidc WITH KEY docnum = gw_idocs-docnum BINARY SEARCH.
          IF sy-subrc = 0.
            gw_idoc_trace_status-source_idoc_stat = gw_edidc-status.

            gw_idoc_trace_status-source_idoc      = gw_idocs-docnum.
            gw_idoc_trace_status-target_idoc      = gw_idocinfo-docnum_r.
            gw_idoc_trace_status-target_idoc_stat = gw_idocinfo-status_r.
            APPEND gw_idoc_trace_status TO ex_idoc_status.
          ENDIF.
        ELSE.
***     Source IDOC's could not be found in the EDIDC table
          CLEAR : gw_idoc_trace_status.
          gw_idoc_trace_status-source_idoc      = gw_idocs-docnum.
          APPEND gw_idoc_trace_status TO ex_idoc_status.

        ENDIF.

      ENDLOOP.

    ELSE.

*** No IDOCs requested for tracing
      RAISE exc_no_idocs_reqstd_for_trace.

    ENDIF.

  ENDMETHOD.


  METHOD get_yeti_api_fld_map.
*------------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*------------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect#   | Transport#     | Description                                                        |
*------------------------------------------------------------------------------------------------------------------------------------------*
* 04-Jan-2020 |Satish Kesavalu     |APL              | DFDK901926     | YETI Field Mappings Added                                          |
*------------------------------------------------------------------------------------------------------------------------------------------*
*** Added the mapping table for YETI API
    i_in_mapping = VALUE #( ( abap = 'DATA' json = 'Data' )
                          ( abap = 'APP_KEY' json = 'app_key' )
                          ( abap = 'STATUS' json = 'status' )
                          ( abap = 'HOST' json = 'host' )
                          ( abap = 'CHECK' json = 'check' )
                          ( abap = 'ALERT_ENV' json = 'alert_env' )
                          ( abap = 'DESTINATION' json = 'destination' )
                          ( abap = 'U_CATEGORY' json = 'u_category' )
                          ( abap = 'U_SEVERITY' json = 'u_severity' )
                          ( abap = 'U_SUBCATEGORY' json = 'u_subcategory' )
                          ( abap = 'U_EVENT_DETAIL' json = 'u_event_detail' )
                          ( abap = 'SHORT_DESCRIPTION' json = 'short_description' )
                          ( abap = 'SLACK' json = 'slack' )
                          ( abap = 'PAGERDUTY' json = 'pagerduty' )
                          ( abap = 'U_CI_DEF' json = 'u_ci_def' )
                          ( abap = 'PARTITIONKEY' json = 'partitionKey' )
                          ( abap = 'DATAPOINT' json = 'Datapoint' )
                          ( abap = 'DEVICE_IPS' json = 'Device_ips' )
                          ( abap = 'INSTANCE' json = 'Instance' )
                          ( abap = 'LOCATION' json = 'Location' )
                          ( abap = 'SUPPORT_TIER' json = 'Support_Tier' )
                          ( abap = 'SUPPORT_GROUP' json = 'Support_Group' )
                          ( abap = 'THRESHOLD' json = 'Threshold' )
                          ( abap = 'VALUE' json = 'Value' )
                          ( abap = 'U_CALLER' json = 'u_caller' )
                          ( abap = 'PARTITION_KEY' json = 'PartitionKey' )
                        ).


  ENDMETHOD.


  METHOD get_mule_pub_sub_api_header.
*------------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*------------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect#   | Transport#     | Description                                                        |
*------------------------------------------------------------------------------------------------------------------------------------------*
* 04-Jan-2020 |Satish Kesavalu     |APL              | DFDK901926     | Mule Pub/Sub API Field Mappings Added                              |
*------------------------------------------------------------------------------------------------------------------------------------------*
*** Added mapping table for Mule Pub/Sub API

    i_in_mapping = VALUE #( ( abap = 'EVENTHEADER' json = 'eventHeader' )
                          ( abap = 'ADSK_RES_EVENT' json = '@@adsk.resource-event-header.v1' )
                          ( abap = 'EVENTID' json = 'eventId' )
                          ( abap = 'CHANNEL' json = 'channel' )
                          ( abap = 'TOPIC' json = 'topic' )
                          ( abap = 'SEQUENCE' json = 'sequence' )
                          ( abap = 'DATE_TIME' json = 'date-time' )
                          ( abap = 'ACTION' json = 'action' )
                          ( abap = 'RESOURCE' json = 'resource' )
                          ( abap = 'RESOURCE_ID' json = 'resource-id' )
                          ( abap = 'RESOURCE_URI' json = 'resource-uri' )
                          ( abap = 'TEST' json = 'test' )
                          ( abap = 'ENCRYPTEVENTBODY' json = 'encryptEventBody' )
                          ( abap = 'EVENTBODY' json = 'eventBody' )
                        ).

  ENDMETHOD.


  METHOD get_fpath.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD GET_FPATH                                                                                                                        |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 30-MAY-2019 |Sugeeth Sudhendran  |               | D4SK902468     | Get Archive File Path                                                |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for Constant
*----------------------------------------------------------------------*
    CONSTANTS: lc_a    TYPE c      VALUE 'A',
               lc_e    TYPE c      VALUE 'E',
               lc_err  TYPE char3  VALUE 'Err',
               lc_uscr TYPE c      VALUE '_',
               lc_dot  TYPE c      VALUE '.'.

*----------------------------------------------------------------------*
* Declaration for Variable
*----------------------------------------------------------------------*
    DATA: lv_path  TYPE string,
          lv_dummy TYPE string,
          lv_spath TYPE string,
          lv_file  TYPE string,
          lv_len   TYPE i.

    CLEAR: lv_path,lv_dummy,lv_spath,lv_file,lv_len.

    IF i_type EQ lc_a.
      lv_spath = gc_archive_path.
    ELSEIF i_type EQ lc_e.
      lv_spath = gc_error_path.
    ENDIF.

    CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = i_file
      IMPORTING
        stripped_name = lv_file
        file_path     = lv_path
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.

    IF sy-subrc = 0.

      IF i_type = lc_e.
        IF lv_file+0(3) = lc_err.
          SPLIT lv_file AT lc_uscr INTO lv_dummy lv_file.
        ENDIF.
        CONCATENATE lc_err sy-datum sy-timlo lc_uscr lv_file INTO lv_file.
      ELSEIF i_type = lc_a.
        SPLIT lv_file AT lc_dot INTO lv_file lv_dummy.
        IF sy-subrc = 0.
          CONCATENATE lv_file lc_uscr sy-datum sy-timlo lc_dot lv_dummy INTO lv_file.
          CONDENSE lv_file.
        ENDIF.
      ENDIF.

      e_fname = lv_file.
      "-- Read the character length of the directory
      lv_len = strlen( lv_path ).
      lv_len = lv_len - 1.
      IF lv_path+lv_len(1) NE gc_bslash .

        CONCATENATE  lv_path
                     gc_bslash
                     lv_spath
                     lv_file INTO e_fullpath.

        CONCATENATE  lv_path
                     gc_bslash
                     lv_spath INTO e_fpath.
      ELSE.
        CONCATENATE  lv_path lv_spath lv_file  INTO e_fullpath.
        CONCATENATE  lv_path lv_spath INTO e_fpath.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_email_content.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD GET_EMAIL_CONTENT                                                                                                                |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-JAN-2019 |Richa Beri          |               | D4SK900211     | Get the content of the email                                         |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for Work area
*----------------------------------------------------------------------*
    DATA :
      lw_tline        TYPE tline,
      lw_body         TYPE soli,
      lw_text_replace TYPE zsca_email_textsymbol_replace,
      lw_return       TYPE bapiret2,
      ls_head         TYPE thead.

*----------------------------------------------------------------------*
* Declaration for Table
*----------------------------------------------------------------------*
    DATA :
      lt_tline           TYPE TABLE OF tline,
      ltpgm_const_values TYPE TABLE OF zspgm_const_values,
      lterror_const      TYPE TABLE OF zserror_const.

*----------------------------------------------------------------------*
* Declaration for variables
*----------------------------------------------------------------------*
    DATA :
      lv_count  TYPE i,
      lv_id     TYPE tdid,
      lv_object TYPE tdobject,
      lv_lang   TYPE spras.

    REFRESH:lt_tline.
    CLEAR  :lw_tline,lw_body.

* FM used to retrieve all constants
    CALL FUNCTION 'ZUTIL_PGM_CONSTANTS'
      EXPORTING
        im_pgmid               = 'ZCL_CA_UTILITY=>GET_EMAIL_CONTENT'
      TABLES
        t_pgm_const_values     = ltpgm_const_values
        t_error_const          = lterror_const
      EXCEPTIONS
        ex_no_entries_found    = 1
        ex_const_entry_missing = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          MESSAGE e007(zfi_msgs) WITH 'TVARVC'(046).
        WHEN 2.
          MESSAGE e010(zfi_msgs) WITH 'TVARVC'(046).
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_sub) WITH KEY const_name = 'P_S'.
    IF sy-subrc NE 0.
      MESSAGE e025(zfi_msgs) WITH 'P_S'(050).
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_b) WITH KEY const_name = 'P_B'.
    IF sy-subrc NE 0.
      MESSAGE e025(zfi_msgs) WITH 'P_B'(051).
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_id) WITH KEY const_name = 'P_TDID_ST'.
    IF sy-subrc = 0.
      lv_id = lw_id-low.
    ELSE.
      MESSAGE e025(zfi_msgs) WITH 'P_TDID_ST'(052).
    ENDIF.

    READ TABLE ltpgm_const_values INTO DATA(lw_object) WITH KEY const_name = 'P_TDOBJECT_TEXT'.
    IF sy-subrc = 0.
      lv_object = lw_object-low.
    ELSE.
      MESSAGE e025(zfi_msgs) WITH 'P_TDOBJECT_TEXT'(053).
    ENDIF.

**--> Map the language to read the SO10 texts
    IF i_language IS NOT INITIAL .
      lv_lang = i_language .
    ELSE .
      lv_lang = sy-langu .
    ENDIF .

* Get Subject Maintained as standard text
    CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
      EXPORTING
        client                  = sy-mandt
        id                      = lv_id            " ST
        language                = lv_lang          " sy-langu         " EN
        name                    = i_text_name_sub
        object                  = lv_object        " TEXT
      IMPORTING
        header                  = ls_head
      TABLES
        lines                   = lt_tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      lw_return-type = sy-msgty.
      lw_return-id   = sy-msgid.
      lw_return-number = sy-msgno.
      lw_return-message_v1 = sy-msgv1.
      lw_return-message_v2 = sy-msgv2.
      lw_return-message_v3 = sy-msgv3.
      lw_return-message_v4 = sy-msgv4.
      MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
               WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
               INTO lw_return-message.
      APPEND lw_return TO et_return.
      CLEAR lw_return.
      RETURN.
    ENDIF.

* Text Symbol Replace for Subject
    READ TABLE i_text_replace WITH KEY key_type = lw_sub-low TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CALL FUNCTION 'INIT_TEXTSYMBOL'. ##FM_SUBRC_OK
      LOOP AT i_text_replace INTO lw_text_replace WHERE key_type = lw_sub-low.
        CALL FUNCTION 'SET_TEXTSYMBOL'
          EXPORTING
            header  = ls_head
            name    = lw_text_replace-name
            value   = lw_text_replace-value
            replace = abap_true.
        CLEAR : lw_text_replace.
      ENDLOOP.
      DESCRIBE TABLE lt_tline LINES lv_count.

      CALL FUNCTION 'REPLACE_TEXTSYMBOL'
        EXPORTING
          endline   = lv_count
          startline = 1
        TABLES
          lines     = lt_tline.
    ENDIF.

* Take first 50 characters as subject
    READ TABLE lt_tline INTO lw_tline INDEX 1 .
    IF sy-subrc = 0.
      e_subject = lw_tline-tdline+0(50).
    ENDIF.

    REFRESH:lt_tline.
    CLEAR:lw_tline,lv_count,ls_head.

* Get Body maintained as standard text
    CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
      EXPORTING
        client                  = sy-mandt
        id                      = lv_id            " ST
        language                = lv_lang          "sy-langu          " EN
        name                    = i_text_name_body
        object                  = lv_object        " TEXT
      IMPORTING
        header                  = ls_head
      TABLES
        lines                   = lt_tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      lw_return-type = sy-msgty.
      lw_return-id   = sy-msgid.
      lw_return-number = sy-msgno.
      lw_return-message_v1 = sy-msgv1.
      lw_return-message_v2 = sy-msgv2.
      lw_return-message_v3 = sy-msgv3.
      lw_return-message_v4 = sy-msgv4.
      MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
               WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
               INTO lw_return-message.
      APPEND lw_return TO et_return.
      CLEAR lw_return.
      RETURN.
    ENDIF.

* Text Symbol Replace for Body
    READ TABLE i_text_replace WITH KEY key_type = lw_b-low TRANSPORTING NO FIELDS BINARY SEARCH.
    CALL FUNCTION 'INIT_TEXTSYMBOL'. ##FM_SUBRC_OK
    IF sy-subrc = 0.
      LOOP AT i_text_replace INTO lw_text_replace WHERE key_type = lw_b-low.
        CALL FUNCTION 'SET_TEXTSYMBOL'
          EXPORTING
            header  = ls_head
            name    = lw_text_replace-name
            value   = lw_text_replace-value
            replace = abap_true.
        CLEAR : lw_text_replace.
      ENDLOOP.
      DESCRIBE TABLE lt_tline LINES lv_count.

      CALL FUNCTION 'REPLACE_TEXTSYMBOL'
        EXPORTING
          endline   = lv_count
          startline = 1
        TABLES
          lines     = lt_tline.
    ENDIF.

    LOOP AT lt_tline INTO lw_tline.
      lw_body-line = lw_tline-tdline.
      APPEND lw_body TO e_body.
      CLEAR:lw_tline,lw_body.
    ENDLOOP.

    CLEAR : lv_count,ls_head.
    REFRESH: lt_tline.

  ENDMETHOD.


 METHOD get_apikey.
*&------------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                             |
*------------------------------------------------------------------------------------------------------------------------------------------ |
* Change Date |Developer           |RICEFW/Defect#   | Transport#     | Description                                                         |
*------------------------------------------------------------------------------------------------------------------------------------------ |
* 05/13/2021  | 463912             |SERAC-482        |   DFDK906092   | This method gets the API keys from  table ZTUTILITY_APIKEY          |
* 05/25/2021  | 463912             |SERAC-482        |   DFDK906092   | TABLE Client id data element changed and errors introducted         |                                                                      can be commented
*&------------------------------------------------------------------------------------------------------------------------------------------*


   IF i_interface_id IS NOT INITIAL.
*** Get the Client ID and Client Secret from the Utility table for API Keys
     SELECT SINGLE client_id client_secret
       FROM ztutility_apikey
       INTO ( e_client_id, e_client_secret )
       WHERE interface_id = i_interface_id.
     IF sy-subrc <> 0.
       RAISE  clientid_key_not_found.
     ENDIF.
   ENDIF.

 ENDMETHOD.


  METHOD fcat_from_internal_table.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD FCAT_FROM_INTERNAL_TABLE                                                                                                                       |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-JAN-2019 |Richa Beri          |               | D4SK900211     | Field Catalog for Internal Table                                     |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
    DATA:
      lt_table TYPE REF TO data.

    CREATE DATA lt_table LIKE i_table.
    ASSIGN lt_table->* TO FIELD-SYMBOL(<fs_table>).
    TRY.
        cl_salv_table=>factory( IMPORTING
                                  r_salv_table   = DATA(v_ref_salv_table)
                                CHANGING
                                  t_table        = <fs_table>  ).
        rt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns      = v_ref_salv_table->get_columns( )      " ALV Filter
            r_aggregations = v_ref_salv_table->get_aggregations( ) " ALV Aggregations
    ).
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD display_alv.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD DISPLAY_ALV                                                                                                                      |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-JAN-2019 |Richa Beri          |               | D4SK900211     | Display the data in ALV Report                                       |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 31-MAY-2019 |Fathima Ahmed       |               | D4SK902468     | Save Layout in ALV report added                                      |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for Reference Objects
*----------------------------------------------------------------------*
    DATA:
      lo_root    TYPE REF TO cx_root,
      lo_tablref TYPE REF TO cl_salv_table,
*---------------------Begin of changes by 477670 | D4SK902468----------------*
      lo_layout  TYPE REF TO cl_salv_layout.

    DATA: lv_key   TYPE salv_s_layout_key.
*---------------------End of changes by 477670 | D4SK902468------------------*

    TRY.
* Create SALV Reference
        CALL METHOD me->objtref_create
          IMPORTING
            r_salvref = lo_tablref
          CHANGING
            c_datatab = c_datatab.

        IF lo_tablref IS NOT BOUND.
          RETURN.
        ENDIF.

* Layout Settings
        me->layout_set( lo_tablref ).

* Set ALV table Columns
        me->columns_set( lo_tablref ).

*---------------------Begin of changes by 477670 | D4SK902468----------------*
*Save layout
        lo_layout = lo_tablref->get_layout( ).
        lv_key-report = sy-repid.
        lo_layout->set_key( lv_key ).
        lo_layout->set_save_restriction(  cl_salv_layout=>restrict_none ).
*---------------------End of changes by 477670 | D4SK902468------------------*

* Display ALV
        lo_tablref->display( ).

      CATCH cx_root INTO lo_root.
        me->add_error( i_message = lo_root->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD delete_file.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD DELETE_FILE                                                                                                                      |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 30-MAY-2019 |Sugeeth Sudhendran  |               | D4SK902468     | Delete file from Application Server                                  |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for Reference Objects
*----------------------------------------------------------------------*
    DATA: lo_exceptions TYPE REF TO cx_root.

    TRY.
        DELETE DATASET i_filename.
        IF sy-subrc NE 0.
          RAISE delete_error.
        ENDIF.
      CATCH cx_root INTO lo_exceptions.
        RAISE delete_error.
    ENDTRY.

  ENDMETHOD.


  METHOD copy_file.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD COPY_FILE                                                                                                                        |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 30-MAY-2019 |Sugeeth Sudhendran  |               | D4SK902468     | Copies File from source to target path                               |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for Variable
*----------------------------------------------------------------------*
    DATA: lv_source TYPE dxfile-filename,
          lv_target TYPE dxfile-filename.

    CLEAR: lv_source,lv_target.

    MOVE i_src_file TO lv_source.
    MOVE i_trg_file TO lv_target.

    CALL FUNCTION 'DX_FILE_COPY_NO_CONVERSION'
      EXPORTING
        source_filename  = lv_source
        source_pc        = ' '
        target_filename  = lv_target
        target_pc        = ' '
      EXCEPTIONS
        file_read_error  = 1
        file_write_error = 2
        canceled_by_user = 3
        db_error         = 4
        no_authority     = 5
        OTHERS           = 6.
    IF sy-subrc <> 0.
      r_copy_error = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD convert_table_to_string.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD CONVERT_TABLE_TO_STRING                                                                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-JAN-2019 |Richa Beri          |               | D4SK900211     | Convert internal table to string                                     |
*&-----------------------------------------------------------------------------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Declaration for Field Symbols
*----------------------------------------------------------------------*
    FIELD-SYMBOLS:
      <fs_input> TYPE any,
      <fs_fld>   TYPE any.

*----------------------------------------------------------------------*
* Declaration for Variables
*----------------------------------------------------------------------*
    DATA:
      lv_tabix  TYPE sy-tabix,
      lv_string TYPE  string,
      lv_constr TYPE string.
*----------------------------------------------------------------------*
* Declaration for Work area
*----------------------------------------------------------------------*
    DATA:
      wa_fcat   TYPE LINE OF  lvc_t_fcat.

* Loop the input internal table data and build output
    LOOP AT i_datatab ASSIGNING <fs_input>.
      CLEAR : lv_string,lv_tabix.
      IF i_delimiter IS INITIAL.
        MOVE <fs_input> TO lv_string.
      ELSE.
        LOOP AT i_fcat INTO wa_fcat.
          lv_tabix = lv_tabix + 1.
          ASSIGN COMPONENT lv_tabix OF STRUCTURE <fs_input> TO <fs_fld>.
          IF sy-subrc EQ 0.
            MOVE <fs_fld> TO lv_constr.
            IF sy-tabix EQ '1'.
              CONCATENATE lv_string lv_constr
                     INTO lv_string.
            ELSE.
              CONCATENATE lv_string lv_constr
                     INTO lv_string SEPARATED BY i_delimiter.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
      APPEND lv_string TO e_datatab.
    ENDLOOP.
    UNASSIGN: <fs_input>,<fs_fld>.

  ENDMETHOD.


  METHOD columns_set.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD COLUMNS_SET                                                                                                                      |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-JAN-2019 |Richa Beri          |               | D4SK900211     | Columns Set                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for Reference Objects
*----------------------------------------------------------------------*
    DATA:
      lo_columns TYPE REF TO cl_salv_columns_table,
      lo_column  TYPE REF TO cl_salv_column_table.

*----------------------------------------------------------------------*
* Declaration for Internal tables
*----------------------------------------------------------------------*
    DATA:
      lt_column_ref TYPE salv_t_column_ref.

*----------------------------------------------------------------------*
* Declaration for Work Area
*----------------------------------------------------------------------*
    DATA:
      lw_column_ref TYPE salv_s_column_ref.

* Getting Column properties
    lo_columns = i_salvtab->get_columns( ).
    IF lo_columns IS NOT BOUND.
      RETURN.
    ENDIF.
    REFRESH : lt_column_ref.
    CLEAR   : lw_column_ref.
    lt_column_ref = lo_columns->get( ).

    LOOP AT lt_column_ref INTO lw_column_ref.
      TRY.
          lo_column ?= lo_columns->get_column( lw_column_ref-columnname ).
        CATCH cx_salv_not_found.
      ENDTRY.

* Making column invisible / in this case the MANDT/Client field
      IF lo_column->get_ddic_datatype( ) = 'CLNT'(002).
        lo_column->set_technical( if_salv_c_bool_sap=>true ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD call_rest_api.
*------------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*------------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect#   | Transport#     | Description                                                        |
*------------------------------------------------------------------------------------------------------------------------------------------*
* 08-May-2020 |Rohit Kaikala       |TACME-100        | D1DK900835     | Call using REST API                                                |
* 26-Jun-2020 |Rohit Kaikala       |TACME-100        | D1DK901246     | Checking if response is sent or not                                |
* 09-Jul-2020 |Rohit Kaikala       |TACME-100        | D1DK901348     | Corrected Response parsing to handle Time stamp                    |
* 11-Nov-2020 |Rohit Kaikala       |APL              | DFDK900126     | Hashes in the response are corrected                               |
* 04-Jan-2020 |Satish Kesavalu     |APL              | DFDK901926     | YETI Field Mappings +Response code Added                                          |
*------------------------------------------------------------------------------------------------------------------------------------------*
    DATA : lcl_http_client   TYPE REF TO if_http_client,
           lcl_rest_client   TYPE REF TO cl_rest_http_client,
           lv_http_status    TYPE string,
           lv_body           TYPE string,
           ls_message        TYPE zsut_message,
           lo_out            TYPE REF TO data,
           lcl_json          TYPE REF TO /ui2/cl_json,
           lcl_fdt_json      TYPE REF TO cl_fdt_json,
           lo_response       TYPE REF TO if_rest_entity,
           lo_request        TYPE REF TO if_rest_entity,
           lo_resp_table     TYPE REF TO data,
           lv_content_length TYPE  string,
           lv_location       TYPE  string,
           lv_str            TYPE string,
           lv_content_type   TYPE  string,
           lv_status         TYPE  i,
           lv_status_code    TYPE char3.

*----------------------------------------------------------------------*
* Declaration for Range Tables
*----------------------------------------------------------------------*
    DATA : r_resp_code  TYPE RANGE OF char3,
           gs_resp_code LIKE LINE OF r_resp_code.

**** Populate response code
    CLEAR : gs_resp_code.
    gs_resp_code-sign = 'I'.
    gs_resp_code-option = 'EQ'.
    gs_resp_code-low = '200'.
    APPEND gs_resp_code TO r_resp_code.

    gs_resp_code-low = '201'.
    APPEND gs_resp_code TO r_resp_code.
* Creation of Http object
    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = i_destination
      IMPORTING
        client                   = lcl_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.
    IF sy-subrc <> 0.
      ls_message-code = sy-msgid && sy-msgno.
      ls_message-type = 'E'. "Error
      ls_message-response = sy-msgv1 && sy-msgv2 &&
                           sy-msgv3 && sy-msgv4.
      APPEND ls_message TO e_message.
      CLEAR ls_message.
      EXIT.
    ENDIF.

    TRY.
        CREATE OBJECT lcl_rest_client
          EXPORTING
            io_http_client = lcl_http_client.
      CATCH cx_sy_create_object_error.
        ls_message-code = sy-msgid && sy-msgno.
        ls_message-type = 'E'. "Error
        ls_message-response = sy-msgv1 && sy-msgv2 &&
                             sy-msgv3 && sy-msgv4.
        APPEND ls_message TO e_message.
        CLEAR ls_message.
        EXIT.
    ENDTRY.

    TRY .
* Set Version
        lcl_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).
      CATCH cx_sy_assign_error.
        ls_message-code = sy-msgid && sy-msgno.
        ls_message-type = 'E'. "Error
        ls_message-response = sy-msgv1 && sy-msgv2 &&
                             sy-msgv3 && sy-msgv4.
        APPEND ls_message TO e_message.
        CLEAR ls_message.
        EXIT.
    ENDTRY.

* When http object are created
    IF lcl_http_client IS BOUND AND lcl_rest_client IS BOUND.
      TRY .
          CALL METHOD cl_http_utility=>set_request_uri
            EXPORTING
              request = lcl_http_client->request    " HTTP Framework (iHTTP) HTTP Request
              uri     = i_url.                     " URI String (in the Form of /path?query-string)
        CATCH cx_sy_assign_error.
          ls_message-code = sy-msgid && sy-msgno.
          ls_message-type = 'E'. "Error
          ls_message-response = sy-msgv1 && sy-msgv2 &&
                               sy-msgv3 && sy-msgv4.
          APPEND ls_message TO e_message.
          CLEAR ls_message.
          EXIT.
      ENDTRY.
    ENDIF.

* Map the records sent
    TRY.
        IF i_in_table IS NOT INITIAL.
          CALL METHOD /ui2/cl_json=>serialize
            EXPORTING
              data          = i_in_table
              name_mappings = i_in_mapping
            RECEIVING
              r_json        = lv_body.
        ENDIF.
      CATCH cx_sy_create_object_error.
        ls_message-code = sy-msgid && sy-msgno.
        ls_message-type = 'E'. "Error
        ls_message-response = sy-msgv1 && sy-msgv2 &&
                             sy-msgv3 && sy-msgv4.
        APPEND ls_message TO e_message.
        CLEAR ls_message.
        EXIT.
    ENDTRY.

    TRY .
        lo_request = lcl_rest_client->if_rest_client~create_request_entity( ).
        lo_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
        lo_request->set_string_data( lv_body ).

        IF i_in_headers[] IS NOT INITIAL.
          CALL METHOD lcl_rest_client->if_rest_client~set_request_headers "Set the header for the call
            EXPORTING
              it_header_fields = i_in_headers.
        ENDIF.

      CATCH cx_st_error.
        ls_message-code = sy-msgid && sy-msgno.
        ls_message-type = 'E'. "Error
        ls_message-response = sy-msgv1 && sy-msgv2 &&
                             sy-msgv3 && sy-msgv4.
        APPEND ls_message TO e_message.
        CLEAR ls_message.
        EXIT.
    ENDTRY.

    TRY .
        IF i_post IS NOT INITIAL. " For posting records
          lcl_rest_client->if_rest_resource~post( lo_request ). " POST
        ELSEIF i_get IS NOT INITIAL.
          lcl_rest_client->if_rest_resource~get( ). " GET
        ENDIF.
* Collect response
        lo_response = lcl_rest_client->if_rest_client~get_response_entity( ).
        IF lo_response IS NOT BOUND.
          ls_message-code = sy-msgid && sy-msgno.
          ls_message-type = 'E'. "Error
          ls_message-response = sy-msgv1 && sy-msgv2 &&
                               sy-msgv3 && sy-msgv4.
          APPEND ls_message TO e_message.
          CLEAR ls_message.
          EXIT.
        ENDIF.
        ls_message-code = lv_status = lo_response->get_header_field( '~status_code' ).
        ls_message-response = lo_response->get_string_data( ).
* Begin of DFDK900126
        CALL METHOD cl_crm_email_utility_base=>convert_html_to_plain_text
          EXPORTING
            iv_html       = ls_message-response
          IMPORTING
            ev_plain_text = lv_str.
* End of DFDK900126
        CLEAR : lv_status_code.
        lv_status_code = ls_message-code.
        IF NOT lv_status_code IN r_resp_code. "200.

          ls_message-type = 'E'. "Error

          MOVE lv_str TO ls_message-response .
          APPEND ls_message TO e_message.
          CLEAR ls_message.

        ELSE.
          ls_message-type = 'S'. "Success

*Variable DATA contains the JSON data string
          TRY .
              CREATE OBJECT lcl_fdt_json.

              CALL METHOD cl_fdt_json=>json_to_data
                EXPORTING
                  iv_json = lv_str " Change DFDK900126
*                 iv_json = lv_body "For Table
*  IMPORTING
*                 ev_meta =
                CHANGING
                  ca_data = c_resp_table.
            CATCH cx_json_illegal_syntax.
*              ls_message-code = sy-msgid && sy-msgno.
              ls_message-type = 'E'. "Error
              ls_message-response = sy-msgid && sy-msgno && sy-msgv1 && sy-msgv2 &&
                                   sy-msgv3 && sy-msgv4.
              APPEND ls_message TO e_message.
              CLEAR ls_message.
              EXIT.
          ENDTRY.
          APPEND ls_message TO e_message.
        ENDIF.

        lv_content_length = lo_response->get_header_field( 'content-length' ).
        lv_location = lo_response->get_header_field( 'location' ).
        lv_content_type = lo_response->get_header_field( 'content-type' ).

      CATCH cx_xslt_system_error.
        ls_message-code = sy-msgid && sy-msgno.
        ls_message-type = 'E'. "Error
        ls_message-response = sy-msgv1 && sy-msgv2 &&
                             sy-msgv3 && sy-msgv4.
        APPEND ls_message TO e_message.
        CLEAR ls_message.
        EXIT.
    ENDTRY.
  ENDMETHOD.


  METHOD archive_file.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD ARCHIVE_FILE                                                                                                                     |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 30-MAY-2019 |Sugeeth Sudhendran  |               | D4SK902468     | Archive File from source to target path                              |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for Constant
*----------------------------------------------------------------------*
    CONSTANTS: lc_a   TYPE c      VALUE 'A',
               lc_dot TYPE c      VALUE '.'.

*----------------------------------------------------------------------*
* Declaration for Variable
*----------------------------------------------------------------------*
    DATA: lv_string     TYPE string,
          lv_path       TYPE string,
          lv_filename   TYPE string,
          lv_target     TYPE string,
          lv_extnesion  TYPE string,
          lv_len        TYPE i,
          lv_msg1       TYPE msgv1,
          lv_msg2       TYPE msgv2,
          lv_msg3       TYPE msgv3,
          lv_msg4       TYPE msgv4,
          lv_copy_error TYPE abap_bool.


    CLEAR: lv_len,lv_filename ,lv_path,lv_target.

    IF i_arch_path IS NOT INITIAL.
      lv_target = i_arch_path.
    ELSE.
      me->get_fpath( EXPORTING
                      i_file = i_file
                      i_type = lc_a
                    IMPORTING
                      e_fullpath = lv_target ).
    ENDIF.

    IF add_time_stamp EQ abap_true.

      SPLIT lv_target AT lc_dot INTO lv_filename lv_extnesion.
      IF sy-subrc = 0.
        CONCATENATE lv_filename sy-datum sy-timlo lc_dot lv_extnesion INTO lv_target.
        CONDENSE lv_target.
      ENDIF.

    ENDIF.

    lv_copy_error = me->copy_file( EXPORTING i_src_file = i_file
                                       i_trg_file = lv_target ).

    IF lv_copy_error = abap_false.
      me->delete_file( EXPORTING i_filename = i_file ).
      r_arch_path = lv_target.
    ELSE.
      RAISE error_archiving_file.
    ENDIF.

  ENDMETHOD.


  METHOD add_to_log.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD ADD_TO_LOG                                                                                                                       |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-JAN-2019 |Richa Beri          |               | D4SK900211     | Addition to LOg                                                      |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Declaration for Work Area
*----------------------------------------------------------------------*
    DATA:
      lw_prclog LIKE LINE OF gt_prslog.

    lw_prclog-type = i_mtype.
    lw_prclog-id = i_msgid.
    lw_prclog-message    = i_message.
    lw_prclog-message_v1 = i_msgv1.
    lw_prclog-message_v2 = i_msgv2.
    lw_prclog-message_v3 = i_msgv3.
    lw_prclog-message_v4 = i_msgv4.
    lw_prclog-parameter  = i_param.
    lw_prclog-number     = i_msgno.
    lw_prclog-field      = i_field.

    APPEND lw_prclog TO gt_prslog.

  ENDMETHOD.


  METHOD add_error.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD ADD_ERROR                                                                                                                        |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                            |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date |Developer           |RICEFW/Defect# | Transport#     | Description                                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*
* 17-JAN-2019 |Richa Beri          |               | D4SK900211     | Private Method to add error                                          |
*&-----------------------------------------------------------------------------------------------------------------------------------------*

    me->add_to_log( EXPORTING i_mtype = 'E'
                              i_message = i_message ).

  ENDMETHOD.
ENDCLASS.
