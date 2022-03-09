*&---------------------------------------------------------------------*
*& Function Module: ZFI_RFC_CREATE_TEXT
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------------------------------------------------------*
*                                                          MODIFICATION HISTORY                                                           |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* Change Date | Developer           | RICEFW/Defect# | Transport#   | Description                                                         |
*-----------------------------------------------------------------------------------------------------------------------------------------*
* 03-AUG-2020 | 477237              | CF.CNV.020/     | DFDK900167   | Create Long Text through RFC                                       |
*             |                     | CF.CNV.054/     |              |                                                                    |
*             |                     | CF.CNV.015/     |              |                                                                    |
*             |                     | CF.CNV.024/     |              |                                                                    |
*-----------------------------------------------------------------------------------------------------------------------------------------*
FUNCTION zfi_rfc_create_text_v2 .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      TEXT_LINES STRUCTURE  IBIPTEXTLN
*"      MESSAGES STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

*** Data Declaration
  DATA: lt_flines TYPE STANDARD TABLE OF tline,
        lw_flines TYPE tline.

*** Process the input text lines
  LOOP AT text_lines[] INTO DATA(lw_text_lines)
    GROUP BY (  tdobject  = lw_text_lines-tdobject
                tdname    = lw_text_lines-tdname
                tdid      = lw_text_lines-tdid
                tdspras   = lw_text_lines-tdspras )
    ASCENDING
    ASSIGNING FIELD-SYMBOL(<fs_flines>).

    LOOP AT GROUP <fs_flines> ASSIGNING FIELD-SYMBOL(<fs_fline>).
      lw_flines-tdformat  = <fs_fline>-tdformat.
      lw_flines-tdline    = <fs_fline>-tdline.
      APPEND lw_flines TO lt_flines.
      CLEAR lw_flines.
    ENDLOOP.

*** Call the 'CREATE_TEXT' FM to update the text to the system
    CALL FUNCTION 'CREATE_TEXT'
      EXPORTING
        fid       = <fs_fline>-tdid
        flanguage = <fs_fline>-tdspras
        fname     = <fs_fline>-tdname
        fobject   = <fs_fline>-tdobject
        fformat   = space
      TABLES
        flines    = lt_flines
      EXCEPTIONS
        no_init   = 1
        no_save   = 2
        OTHERS    = 3.
    IF sy-subrc <> 0.
*** Raise the Exceptions
      CASE sy-subrc.
        WHEN '1'.
          messages-type = 'E'.
          messages-message = TEXT-001 && | - |
            && |TDOBJECT:| && <fs_fline>-tdobject && |, |
            && |TDNAME:| && <fs_fline>-tdname && |, |
            && |TDID:| && <fs_fline>-tdid && |, |
            && |TDSPRAS:| && <fs_fline>-tdspras && |. |.
          APPEND messages.
          CLEAR messages.
        WHEN '2'.
          messages-type = 'E'.
          messages-message = TEXT-002 && | - |
            && |TDOBJECT:| && <fs_fline>-tdobject && |, |
            && |TDNAME:| && <fs_fline>-tdname && |, |
            && |TDID:| && <fs_fline>-tdid && |, |
            && |TDSPRAS:| && <fs_fline>-tdspras && |. |.
          APPEND messages.
          CLEAR messages.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    CLEAR: lt_flines.

  ENDLOOP.

ENDFUNCTION.
