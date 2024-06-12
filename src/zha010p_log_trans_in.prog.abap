*&---------------------------------------------------------------------*
*& 包含               ZHA010P_LOG_TRANS_IN
*&---------------------------------------------------------------------*
DATA: lo_cl_abap_typedescr TYPE REF TO cl_abap_typedescr.
DATA: lo_cl_abap_elemdescr TYPE REF TO cl_abap_elemdescr.
DATA: lo_cl_abap_refdescr  TYPE REF TO cl_abap_refdescr.
DATA: lo_cl_abap_structdescr  TYPE REF TO cl_abap_structdescr.
DATA: lo_cl_abap_tabledescr   TYPE REF TO cl_abap_tabledescr.
DATA: lo_cl_abap_classdescr   TYPE REF TO cl_abap_classdescr.
DATA: lo_cl_abap_intfdescr    TYPE REF TO cl_abap_intfdescr.
DATA: ls_zz_trans_comp_descr  TYPE abap_componentdescr.
DATA: lt_zz_trans_comp_tab    TYPE cl_abap_structdescr=>component_table.
DATA: lv_zz_trans_table_name  TYPE string.
DATA: ls_zz_trans_fupararef TYPE fupararef,
      lt_zz_trans_fupararef TYPE TABLE OF fupararef.
DATA: lo_zz_trans_root_error  TYPE REF TO cx_root.

"因为通用名称，所以变量必须特殊
DATA: ls_zz_trans_callstack TYPE sys_calls,
      lt_zz_trans_callstack TYPE sys_callst.
DATA: lv_zz_trans_funtion_name TYPE string.

FIELD-SYMBOLS: <fs_zz_trans_field> TYPE any.
FIELD-SYMBOLS: <fs_zz_trans_workarea> TYPE any.
FIELD-SYMBOLS: <fs_zz_trans_tab> TYPE ANY TABLE.

CALL FUNCTION 'SYSTEM_CALLSTACK'
  IMPORTING
*   callstack    = callstack
    et_callstack = lt_zz_trans_callstack.

"read first function name
CLEAR: ls_zz_trans_callstack.
READ TABLE lt_zz_trans_callstack INTO ls_zz_trans_callstack WITH KEY eventtype = 'FUNC'.
IF lt_zz_trans_callstack IS INITIAL.
  EXIT.
ENDIF.

SELECT *
  INTO CORRESPONDING FIELDS OF TABLE lt_zz_trans_fupararef
  FROM fupararef
  WHERE funcname = ls_zz_trans_callstack-eventname
   AND paramtype <> 'X'.

"判断是否需要转
SELECT COUNT(*) FROM ztha010_c001 WHERE fcnam = ls_zz_trans_callstack-eventname AND incov = 'X'.
IF sy-subrc = 0.
  CLEAR: lt_zz_trans_fupararef[].
ENDIF.

LOOP AT lt_zz_trans_fupararef INTO ls_zz_trans_fupararef WHERE paramtype NE 'E'.
  IF ls_zz_trans_fupararef-paramtype = 'I' AND ls_zz_trans_fupararef-reference = 'X'."非值传递的传入值，不修改例程
    CONTINUE.
  ENDIF.

  IF ls_zz_trans_fupararef-paramtype NE 'T'.
    ASSIGN (ls_zz_trans_fupararef-parameter) TO <fs_zz_trans_workarea>.
    IF <fs_zz_trans_workarea> IS ASSIGNED.
      CALL METHOD zha010_cl_common=>conversion_exit_input
        CHANGING
          p_data = <fs_zz_trans_workarea>.
    ENDIF.
  ELSE.
    lv_zz_trans_table_name = ls_zz_trans_fupararef-parameter && '[]'.
    ASSIGN (lv_zz_trans_table_name) TO <fs_zz_trans_tab>.
    IF <fs_zz_trans_tab> IS ASSIGNED.
      CALL METHOD zha010_cl_common=>conversion_exit_input
        CHANGING
          p_data = <fs_zz_trans_tab>.
    ENDIF.
  ENDIF.
ENDLOOP.
