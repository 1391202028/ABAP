*&---------------------------------------------------------------------*
*& Report ZHA010P_LOG_DELETE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zha010p_log_delete.
PARAMETERS: p_edate TYPE sy-datum.

INITIALIZATION.
  p_edate = sy-datum - 180.

END-OF-SELECTION.
  DELETE FROM ztha010_logdi WHERE logid IN ( SELECT logid FROM ztha010_loghd WHERE erdat <= p_edate ).
  DELETE FROM ztha010_logde WHERE logid IN ( SELECT logid FROM ztha010_loghd WHERE erdat <= p_edate ).
  DELETE FROM ztha010_loghd WHERE erdat <= p_edate .

  COMMIT WORK AND WAIT.
  WRITE:/ '函数日志删除成功'.
