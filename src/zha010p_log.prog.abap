*&---------------------------------------------------------------------*
*& Report ZHA010P_LOG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zha010p_log.
"统一管理程序
INCLUDE zha010p_log_top.
INCLUDE zha010p_log_begin.
INCLUDE zha010p_log_end.
INCLUDE zha010p_log_trans_in.
INCLUDE zha010p_log_trans_out.

*&---------------------------------------------------------------------*
*& Form blog
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM blog USING  iv_logid   TYPE ztha010_loghd-logid
                 is_reqif   TYPE zsha010_reqinfo.
  IF iv_logid IS NOT INITIAL.
    "必须要调用 zha010p_log_begin 后才能调用该函数
    CALL FUNCTION 'ZHA010_LOG_BEGIN_UPD' STARTING NEW TASK 'ZHA010_BLOG'
      EXPORTING
        iv_logid = iv_logid
        is_reqif = is_reqif.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form elog
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM elog USING  iv_logid   TYPE ztha010_loghd-logid
                 is_reqif   TYPE zsha010_reqinfo
                 is_retif   TYPE zsha010_retinfo.
  IF iv_logid IS NOT INITIAL.
    "必须要调用 zha010p_log_begin 后才能调用该函数
    CALL FUNCTION 'ZHA010_LOG_END_UPD' STARTING NEW TASK 'ZHA010_ELOG'
      EXPORTING
        iv_logid = iv_logid
        is_reqif = is_reqif
        is_retif = is_retif.
  ENDIF.
ENDFORM.
