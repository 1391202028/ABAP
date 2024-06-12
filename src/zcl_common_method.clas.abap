class ZCL_COMMON_METHOD definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_textb,
        tdline TYPE tline-tdline,
      END OF ty_textb .

  data:
    lv_year  TYPE c LENGTH 4 .
  data:
    lv_month TYPE c LENGTH 2 .
  data:
    lv_day   TYPE c LENGTH 2 .

  class-methods CONVERT_STRING_TO_ITAB
    importing
      !IV_STRING type STRING
    returning
      value(T_ITAB) type ZBCTLINE .
  class-methods READ_TEXT
    importing
      !IV_ID type TDID
      !IV_LANGUAGE type SPRAS
      !IV_NAME type TDOBNAME
      !IV_OBJECT type TDOBJECT
    exporting
      value(IV_MESSAGE) type SYMSGV
    returning
      value(EV_STRING) type STRING .
  class-methods SAVE_TEXT
    importing
      !IV_STRING type STRING
      !IV_NAME type TDOBNAME
      !IV_OBJECT type TDOBJECT
      !IV_ID type TDID
    exporting
      value(EV_MESSAGE) type SYMSGV
    returning
      value(EV_TYPE) type BAPI_MTYPE .
  class-methods DOWNLOAD_TEMPLATE
    importing
      !IV_TEMPLATE type W3OBJID
      !IV_FNAME type STRING
    exporting
      !IV_MESSAGE type STRING .
  class-methods DROP_LIST
    importing
      !IV_ID type VRM_ID
      !IV_VALUES type VRM_VALUES .
  class-methods OPEN_DIALOG
    importing
      value(IV_TITLE) type STRING optional
    returning
      value(EV_FILE) type STRING .
  class-methods CONVERT_STRING_TO_ITAB1
    importing
      value(IV_STRING) type STRING
      value(IV_LENGTH) type SY-TABIX
      !ET_TABLE type STANDARD TABLE .
  class-methods SCREEN_F4
    importing
      value(IT_ITAB) type STANDARD TABLE
      value(IV_FIELD) type DFIES-FIELDNAME
      value(IV_REPID) type SY-REPID default SY-REPID
      value(IV_DYNNR) type SY-DYNNR
      value(IV_DYNPROFIELD) type HELP_INFO-DYNPROFLD
      value(IV_ORG) type DDBOOL_D default 'S'
    returning
      value(ES_RETURN) type DDSHRETVAL .
  class-methods SEARCH_HELP
    importing
      value(IV_TABNAME) type DFIES-TABNAME
      value(IV_FIELDNAME) type DFIES-FIELDNAME
      value(IV_SHLP) type SHLPNAME
      value(IV_SHLPPARAM) type SHLPFIELD
      value(IV_REPID) type SY-REPID default SY-REPID
      value(IV_SCREEN) type SY-DYNNR default SY-DYNNR
      value(IV_DYNPROFIELD) type HELP_INFO-DYNPROFLD .
  class-methods DELETE_LZERO
    importing
      value(IV_STRING) type STRING
    returning
      value(EV_STRING) type STRING .
  class-methods READ_DYNP_VALUE
    importing
      !IV_FIELD type DYNFNAM
      !IV_CPROG type D020S-PROG optional
      !IV_DYNNR type D020S-DNUM optional
    exporting
      !EV_FIELD_VALUE type DYNFIELDVALUE .
  class-methods GET_DOMAIN_DESC
    importing
      !IV_DOMNAME type DOMNAME
      !IV_DOMVALUE type DOMVALUE_L
    returning
      value(EV_VALUE) type VAL_TEXT .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_COMMON_METHOD IMPLEMENTATION.


  METHOD convert_string_to_itab.
    DATA: lv_length TYPE sy-tabix VALUE '132'.
    DATA: lt_textb TYPE TABLE OF ty_textb.
    DATA: ls_tline TYPE tline.
    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = iv_string
      IMPORTING
        length    = lv_length
      TABLES
        ftext_tab = lt_textb.
    MOVE-CORRESPONDING lt_textb[] TO t_itab[].
    ls_tline-tdformat = '*'.
    MODIFY t_itab FROM ls_tline TRANSPORTING tdformat WHERE tdformat IS INITIAL.
  ENDMETHOD.


  METHOD convert_string_to_itab1.
    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = iv_string
      IMPORTING
        length    = iv_length
      TABLES
        ftext_tab = et_table.
  ENDMETHOD.


  METHOD delete_lzero.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = iv_string
      IMPORTING
        output = ev_string.
    IF ev_string+0(1) = '0'.
      SHIFT iv_string LEFT DELETING LEADING '0'.
      ev_string = iv_string.
    ENDIF.
  ENDMETHOD.


  METHOD download_template.
    DATA: ls_wwwdatatab     TYPE wwwdatatab,
          lt_mime           TYPE TABLE OF w3mime,
          lv_filename       TYPE string,
          lv_path           TYPE string,
          lv_fullpath       TYPE string,
          window_title      TYPE string,
          default_file_name TYPE string.
    DATA: lv_destination TYPE rlgrap-filename,
          lv_subrc       TYPE sy-subrc.
    CLEAR: ls_wwwdatatab,lt_mime[],lv_filename,lv_path,lv_fullpath,window_title,default_file_name.
    ls_wwwdatatab-relid = 'MI'.         "IMPORT/EXPORT    ݱ  е
    ls_wwwdatatab-objid = iv_template.    "SAP WWW    ض         SMW0  WEB RFC Ķ
*    ls_wwwdatatab-text  = pv_text.  "WWWDATA     Ķ  ı
    window_title = '选择文件'.
    default_file_name = iv_fname.
    CALL FUNCTION 'WWWDATA_IMPORT'                            "#EC *
      EXPORTING
        key               = ls_wwwdatatab
      TABLES
        mime              = lt_mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2
        OTHERS            = 3.
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title         = window_title
        default_extension    = 'xlsx'
        default_file_name    = default_file_name
*       file_filter          = 'EXCEL'
      CHANGING
        filename             = lv_filename
        path                 = lv_path
        fullpath             = lv_fullpath
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    SELECT SINGLE relid objid FROM wwwdata INTO CORRESPONDING FIELDS OF ls_wwwdatatab
           WHERE srtf2 = 0 AND relid = 'MI' AND objid = iv_template.
    IF sy-subrc NE 0 OR ls_wwwdatatab-objid EQ space.
      iv_message = '模板不存在，未找到模板'.
    ELSE.
      IF lv_fullpath IS NOT INITIAL.
        lv_destination = lv_fullpath.
        CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
          EXPORTING
            key         = ls_wwwdatatab
            destination = lv_destination
          IMPORTING
            rc          = lv_subrc.
        IF lv_subrc <> 0.
          iv_message = '下载失败'.
        ELSE.
          iv_message = '下载成功'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD drop_list.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = iv_id
        values          = iv_values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
  ENDMETHOD.


  METHOD get_domain_desc.
    SELECT SINGLE ddtext FROM dd07v INTO @ev_value WHERE domname     = @iv_domname
                                                   AND   domvalue_l  = @iv_domvalue
                                                   AND   DDLANGUAGE  = @sy-langu.
  ENDMETHOD.


  METHOD open_dialog.
    DATA: lt_filetable TYPE filetable,
          lv_rc        TYPE i.
    iv_title = '选择文件'.
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = iv_title
*       file_filter             = '(*.XLSX)'
        multiselection          = space
      CHANGING
        file_table              = lt_filetable
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    IF sy-subrc = 0.                      "Judge open success
      CHECK NOT lt_filetable[] IS INITIAL.
      DATA(ls_filetable) = lt_filetable[ 1 ].
      ev_file = ls_filetable-filename.
    ENDIF.
  ENDMETHOD.


  METHOD read_dynp_value.
    DATA: lt_dyfields TYPE TABLE OF dynpread.
    lt_dyfields[] = VALUE #( ( fieldname = iv_field ) ).
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = iv_cprog
        dynumb               = iv_dynnr
      TABLES
        dynpfields           = lt_dyfields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc = 0 AND lt_dyfields[] IS NOT INITIAL.
      ev_field_value = lt_dyfields[ 1 ]-fieldvalue.
    ENDIF.

  ENDMETHOD.


  METHOD read_text.
    DATA: lt_tdline TYPE TABLE OF tline.
    CLEAR: lt_tdline[],ev_string,iv_message.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = iv_id
        language                = iv_language
        name                    = iv_name
        object                  = iv_object
      TABLES
        lines                   = lt_tdline
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
      iv_message = '读取长文本失败'.
    ELSE.
      LOOP AT lt_tdline INTO DATA(ls_tline).
        ev_string = ev_string && ls_tline-tdline.
        CLEAR: ls_tline.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD save_text.
    DATA: ls_tdhead TYPE thead.
    DATA(lt_tdline) = convert_string_to_itab( iv_string = iv_string ).
    ls_tdhead-tdid = iv_id.
    ls_tdhead-tdobject = iv_object.
    ls_tdhead-tdname = iv_name.
    ls_tdhead-tdspras = sy-langu.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = ls_tdhead
        savemode_direct = 'X'
      TABLES
        lines           = lt_tdline
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
      ev_message = '保存失败'.
      ev_type = 'E'.
    ELSE.
      ev_message = '保存成功'.
      ev_type = 'S'.
    ENDIF.

  ENDMETHOD.


  METHOD screen_f4.
    DATA: lt_return TYPE TABLE OF ddshretval.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = iv_field
        dynpprog        = iv_repid
        dynpnr          = iv_dynnr
        dynprofield     = iv_dynprofield
        window_title    = 'Selection'
        value_org       = iv_org
      TABLES
        value_tab       = it_itab
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    READ TABLE lt_return INTO es_return INDEX 1.
  ENDMETHOD.


  METHOD search_help.
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = iv_tabname
        fieldname         = iv_fieldname
        searchhelp        = iv_shlp
        shlpparam         = iv_shlpparam
        dynpprog          = iv_repid
        dynpnr            = iv_screen
        dynprofield       = iv_dynprofield
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
  ENDMETHOD.
ENDCLASS.
