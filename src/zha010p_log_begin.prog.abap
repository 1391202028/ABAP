*&---------------------------------------------------------------------*
*& 包含               ZHA010P_LOG_BEGIN
*&---------------------------------------------------------------------*
lv_zz_log_is_run = 'X'.

"读取重运行程序的运行ID
ASSIGN ('(ZHA010P_LOG_MONITOR)GV_ZZ_LOG_ID') TO FIELD-SYMBOL(<fs_zz_log_id>).
IF <fs_zz_log_id> IS ASSIGNED.
  lv_zz_log_id = <fs_zz_log_id>.
ENDIF.

"读取重运行程序的命令
ASSIGN ('(ZHA010P_LOG_MONITOR)GV_NEW_ID') TO FIELD-SYMBOL(<fs_new_id>).
IF <fs_new_id> IS ASSIGNED .
  lv_zz_log_is_run = <fs_new_id>.
  IF <fs_new_id> IS NOT INITIAL.
    lv_zz_log_id_old = lv_zz_log_id.
    CLEAR: lv_zz_log_id.
  ENDIF.
ENDIF.

CLEAR: lt_zz_log_idata_save[].

CALL FUNCTION 'SYSTEM_CALLSTACK'
  IMPORTING
*   callstack    = callstack
    et_callstack = lt_zz_log_callstack.
"read first function name
CLEAR: ls_zz_log_callstack.
READ TABLE lt_zz_log_callstack INTO ls_zz_log_callstack WITH KEY eventtype = 'FUNC'.
IF lt_zz_log_callstack IS INITIAL.
  EXIT.
ENDIF.

"判断是否需要日志
SELECT COUNT(*) FROM ztha010_c001 WHERE fcnam = ls_zz_log_callstack-eventname AND inact = 'X'.
IF sy-subrc = 0.
  CLEAR: lv_zz_log_is_run.
ENDIF.

IF lv_zz_log_is_run IS NOT INITIAL. "运行标志
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_zz_log_fupararef
    FROM fupararef
    WHERE funcname = ls_zz_log_callstack-eventname .

  LOOP AT lt_zz_log_fupararef INTO ls_zz_log_fupararef WHERE paramtype NE 'E'.
    CLEAR: ls_zz_log_idata_save.
    ls_zz_log_idata_save-fcnam  = ls_zz_log_callstack-eventname. "函数名称
    ls_zz_log_idata_save-patyp  = ls_zz_log_fupararef-paramtype.
    ls_zz_log_idata_save-param  = ls_zz_log_fupararef-parameter.
    ls_zz_log_idata_save-refty  = ls_zz_log_fupararef-structure.
    ls_zz_log_idata_save-defva  = ls_zz_log_fupararef-defaultval.
    ls_zz_log_idata_save-rtype  = ls_zz_log_fupararef-type.
    ls_zz_log_idata_save-class  = ls_zz_log_fupararef-class.
    ls_zz_log_idata_save-rclas  = ls_zz_log_fupararef-ref_class.

    IF ls_zz_log_fupararef-paramtype NE 'T'.
      ASSIGN (ls_zz_log_fupararef-parameter) TO <fs_zz_log_variable>.
      IF <fs_zz_log_variable> IS ASSIGNED.
        IF <fs_zz_log_variable> IS NOT INITIAL OR ls_zz_log_fupararef-defaultval IS NOT INITIAL.
*         lv_zz_log_string = /ui2/cl_json=>serialize( data = <fs_zz_log_variable> compress = abap_true numc_as_string = 'X' pretty_name = abap_true ).
          lv_zz_log_string = /ui2/cl_json=>serialize( data = <fs_zz_log_variable> compress = abap_true numc_as_string = 'X' ).
        ENDIF.
      ENDIF.
    ELSE.
      lv_zz_log_table_name = ls_zz_log_fupararef-parameter && '[]'.
      ASSIGN (lv_zz_log_table_name) TO <fs_zz_log_variable_tab>.
      IF <fs_zz_log_variable_tab> IS ASSIGNED.
        IF <fs_zz_log_variable_tab> IS NOT INITIAL.
*         lv_zz_log_string = /ui2/cl_json=>serialize( data = <fs_zz_log_variable_tab> compress = abap_true numc_as_string = 'X' pretty_name = abap_true ).
          lv_zz_log_string = /ui2/cl_json=>serialize( data = <fs_zz_log_variable_tab> compress = abap_true numc_as_string = 'X' ).
        ENDIF.
      ENDIF.
    ENDIF.

    WHILE strlen( lv_zz_log_string ) > 0.
      IF strlen( lv_zz_log_string ) >= 1024 .
        ls_zz_log_idata_save-jsons = lv_zz_log_string.
        APPEND ls_zz_log_idata_save TO lt_zz_log_idata_save.
        SHIFT lv_zz_log_string BY 1024 PLACES.
      ELSE.
        ls_zz_log_idata_save-jsons = lv_zz_log_string.
        APPEND ls_zz_log_idata_save TO lt_zz_log_idata_save.
        CLEAR: lv_zz_log_string.
      ENDIF.
    ENDWHILE.
  ENDLOOP.

  "生成一个log id.
  IF lv_zz_log_id IS INITIAL.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZHA010_LOG'
*       QUANTITY                = '1'
*       SUBOBJECT               = ' '
*       TOYEAR                  = '0000'
*       IGNORE_BUFFER           = ' '
      IMPORTING
        number                  = lv_zz_log_id
*       QUANTITY                =
*       RETURNCODE              =
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
  ENDIF.

  "网页计算机名称就是IP地址
  CALL 'ThUsrInfo' ID 'OPCODE'   FIELD lv_zz_log_opcode_usr_attr
                   ID 'TERMINAL' FIELD lv_zz_log_terminal.

  DATA: lv_zz_log_regex TYPE string.
  lv_zz_log_regex = '((?:(?:25[0-5]|2[0-4]\d|((1\d{2})|([1-9]?\d)))\.){3}(?:25[0-5]|2[0-4]\d|((1\d{2})|([1-9]?\d))))'.
  IF NOT contains( val = lv_zz_log_terminal regex = lv_zz_log_regex ).
*    call method cl_gui_frontend_services=>get_ip_address
*      receiving
*        ip_address           = lv_zz_log_ip
*      exceptions
*        cntl_error           = 1
*        error_no_gui         = 2
*        not_supported_by_gui = 3
*        others               = 4.
*    if sy-subrc <> 0.
*      clear: lv_zz_log_ip.
*    endif.
  ELSE.
    lv_zz_log_ip = lv_zz_log_terminal.
  ENDIF.

  "赋值抬头
  ls_zz_log_save-logid  = lv_zz_log_id.
  ls_zz_log_save-fcnam  = ls_zz_log_callstack-eventname.
  ls_zz_log_save-erdat  = sy-datum.
  ls_zz_log_save-erzet  = sy-uzeit.
  ls_zz_log_save-ernam  = sy-uname.
  ls_zz_log_save-terml  = lv_zz_log_terminal.
  ls_zz_log_save-ipadr  = lv_zz_log_ip.
  ls_zz_log_save-oldid  = lv_zz_log_id_old.
  ls_zz_log_save-msgty  = 'I'.
  ls_zz_log_save-msgtx  = '开始调用'.
  GET TIME STAMP FIELD ls_zz_log_save-begtm."开始时间戳

  "赋值行项目
  LOOP AT lt_zz_log_idata_save INTO ls_zz_log_idata_save.
    ls_zz_log_idata_save-logid = lv_zz_log_id.
    ls_zz_log_idata_save-linno = sy-tabix.
    MODIFY lt_zz_log_idata_save FROM ls_zz_log_idata_save.
  ENDLOOP.

  "把数据弄成异步提交模式保存，避免事务提交。
  CALL FUNCTION 'ZHA010_LOG_SAVE' STARTING NEW TASK 'ZHA010_BEGIN'
    EXPORTING
      is_loghd = ls_zz_log_save
    TABLES
      it_logdi = lt_zz_log_idata_save.
ENDIF.
