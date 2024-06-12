FUNCTION zha010_log_end_upd.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IV_LOGID) TYPE  ZEHA010_LOGID
*"     VALUE(IS_REQIF) TYPE  ZSHA010_REQINFO OPTIONAL
*"     VALUE(IS_RETIF) TYPE  ZSHA010_RETINFO OPTIONAL
*"----------------------------------------------------------------------
  DATA: ls_loghd TYPE ztha010_loghd.
  DATA: lv_d_beg TYPE dats,
        lv_t_beg TYPE tims,
        lv_d_end TYPE dats,
        lv_t_end TYPE tims,
        lv_secds TYPE int4.

  CLEAR: ls_loghd,lv_d_beg,lv_t_beg,lv_d_end,lv_t_end,lv_secds.
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

  GET TIME STAMP FIELD ls_loghd-endtm ."结束时间戳

  IF ls_loghd-begtm IS INITIAL.
    ls_loghd-begtm = ls_loghd-endtm.
  ENDIF.

  CONVERT TIME STAMP ls_loghd-begtm TIME ZONE 'UTC+8' INTO DATE lv_d_beg TIME lv_t_beg.
  CONVERT TIME STAMP ls_loghd-endtm TIME ZONE 'UTC+8' INTO DATE lv_d_end TIME lv_t_end.

  "开始结束差值
  CALL FUNCTION 'SALP_SM_CALC_TIME_DIFFERENCE'
    EXPORTING
      date_1  = lv_d_end
      time_1  = lv_t_end
      date_2  = lv_d_beg
      time_2  = lv_t_beg
    IMPORTING
      seconds = lv_secds.

  ls_loghd-csmtm = lv_secds.
  MOVE-CORRESPONDING is_reqif TO ls_loghd.
  MOVE-CORRESPONDING is_retif TO ls_loghd.
  MODIFY ztha010_loghd FROM ls_loghd.
  COMMIT WORK AND WAIT.

ENDFUNCTION.
