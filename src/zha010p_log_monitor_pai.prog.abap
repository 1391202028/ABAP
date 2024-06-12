*&---------------------------------------------------------------------*
*& 包含               ZHA010P_LOG_MONITOR_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&F03' OR '&F05' OR '&F12'.
      LEAVE TO SCREEN 0.
    WHEN 'HIDE'.
      PERFORM frm_hide.
    WHEN 'RESEND'.
      gv_new_id = 'X'.   "生成新的日志ID
      PERFORM frm_resend.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 50
          text       = '等待2秒数据库刷新'.

      WAIT UP TO 1 SECONDS.
      PERFORM frm_ready_report.
      CALL METHOD g_alv->refresh_table_display( is_stable = VALUE #( row = 'X' col = 'X' ) ).
    WHEN 'RESEND2'.
      gv_new_id = ' '.
      PERFORM frm_resend.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 50
          text       = '等待2秒数据库刷新'.
      WAIT UP TO 1 SECONDS.
      PERFORM frm_ready_report.
      CALL METHOD g_alv->refresh_table_display( is_stable = VALUE #( row = 'X' col = 'X' ) ).
    WHEN OTHERS.
  ENDCASE.
ENDMODULE. " USER_COMMAND_0100 INPUT
