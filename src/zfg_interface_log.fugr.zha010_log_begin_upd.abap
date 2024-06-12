FUNCTION zha010_log_begin_upd.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IV_LOGID) TYPE  ZEHA010_LOGID
*"     VALUE(IS_REQIF) TYPE  ZSHA010_REQINFO OPTIONAL
*"----------------------------------------------------------------------
  DATA: ls_loghd TYPE ztha010_loghd.

  CHECK is_reqif IS NOT INITIAL.
  CLEAR: ls_loghd.
  DO 3 TIMES.
    SELECT SINGLE  *
       INTO CORRESPONDING FIELDS OF ls_loghd
       FROM ztha010_loghd
       WHERE logid = iv_logid.
    IF ls_loghd IS INITIAL.
      WAIT UP TO 1 SECONDS.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  MOVE-CORRESPONDING is_reqif TO ls_loghd.
  MODIFY ztha010_loghd FROM ls_loghd.
  COMMIT WORK AND WAIT.

ENDFUNCTION.
