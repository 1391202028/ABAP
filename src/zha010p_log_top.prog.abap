*&---------------------------------------------------------------------*
*& 包含               ZHA010P_LOG_TOP
*&---------------------------------------------------------------------*
"因为通用名称，所以变量必须特殊
DATA: ls_zz_log_callstack TYPE sys_calls,
      lt_zz_log_callstack TYPE sys_callst.

DATA: ls_zz_log_save TYPE ztha010_loghd.

DATA: ls_zz_log_idata_save TYPE ztha010_logdi,
      lt_zz_log_idata_save TYPE TABLE OF ztha010_logdi.

DATA: ls_zz_log_edata_save TYPE ztha010_logde,
      lt_zz_log_edata_save TYPE TABLE OF ztha010_logde.

DATA: ls_zz_log_fupararef TYPE fupararef,
      lt_zz_log_fupararef TYPE TABLE OF fupararef.

DATA: lv_zz_log_table_name TYPE string.

DATA: lv_zz_log_id TYPE ztha010_loghd-logid.
DATA: lv_zz_log_id_old TYPE ztha010_loghd-oldid.
DATA: lv$logid$ TYPE ztha010_logdi-logid.

DATA: lv_zz_log_opcode_usr_attr(1) TYPE x VALUE 5,
      lv_zz_log_terminal           TYPE usr41-terminal.
DATA: lv_zz_log_ip TYPE string.

"判断是否产生new logid
DATA: lv_zz_log_is_run TYPE abap_bool.

FIELD-SYMBOLS: <fs_zz_log_variable> TYPE any.
FIELD-SYMBOLS: <fs_zz_log_variable_tab> TYPE ANY TABLE.

DATA: lv_zz_log_string TYPE string.

CONSTANTS lc_exclude_exit TYPE string VALUE 'ABPSP/ABPSN/EXCRT'.
