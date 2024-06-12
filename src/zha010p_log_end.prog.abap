*&---------------------------------------------------------------------*
*& 包含               ZHA010P_LOG_END
*&---------------------------------------------------------------------*
IF lv_zz_log_id IS NOT INITIAL. "运行标志
  CLEAR: lt_zz_log_edata_save[].

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
*     callstack    = callstack
      et_callstack = lt_zz_log_callstack.

  "read first function name
  CLEAR: ls_zz_log_callstack.
  READ TABLE lt_zz_log_callstack INTO ls_zz_log_callstack WITH KEY eventtype = 'FUNC'.
  IF lt_zz_log_callstack IS INITIAL.
    EXIT.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_zz_log_fupararef
    FROM fupararef
    WHERE funcname = ls_zz_log_callstack-eventname .

  LOOP AT lt_zz_log_fupararef INTO ls_zz_log_fupararef WHERE paramtype NE 'I'.
    CLEAR: ls_zz_log_edata_save.
    ls_zz_log_edata_save-fcnam  = ls_zz_log_callstack-eventname. "函数名称
    ls_zz_log_edata_save-patyp  = ls_zz_log_fupararef-paramtype.
    ls_zz_log_edata_save-param  = ls_zz_log_fupararef-parameter.
    ls_zz_log_edata_save-refty  = ls_zz_log_fupararef-structure.
    ls_zz_log_edata_save-defva  = ls_zz_log_fupararef-defaultval.
    ls_zz_log_edata_save-rtype  = ls_zz_log_fupararef-type.
    ls_zz_log_edata_save-class  = ls_zz_log_fupararef-class.
    ls_zz_log_edata_save-rclas  = ls_zz_log_fupararef-ref_class.

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
        ls_zz_log_edata_save-jsons = lv_zz_log_string.
        APPEND ls_zz_log_edata_save TO lt_zz_log_edata_save.
        SHIFT lv_zz_log_string BY 1024 PLACES.
      ELSE.
        ls_zz_log_edata_save-jsons = lv_zz_log_string.
        APPEND ls_zz_log_edata_save TO lt_zz_log_edata_save.
        CLEAR: lv_zz_log_string.
      ENDIF.
    ENDWHILE.
  ENDLOOP.

  "赋值行项目
  LOOP AT lt_zz_log_edata_save INTO ls_zz_log_edata_save.
    ls_zz_log_edata_save-logid = lv_zz_log_id.
    ls_zz_log_edata_save-linno = sy-tabix.
    MODIFY lt_zz_log_edata_save FROM ls_zz_log_edata_save.
  ENDLOOP.

  "把数据弄成异步提交模式保存，避免事务提交。
  CALL FUNCTION 'ZHA010_LOG_SAVE' STARTING NEW TASK 'ZHA010_END'
    TABLES
      it_logde = lt_zz_log_edata_save.
ENDIF.
