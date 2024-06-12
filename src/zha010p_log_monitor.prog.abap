*&---------------------------------------------------------------------*
*& Report ZHA010P_LOG_MONITOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zha010p_log_monitor.
INCLUDE zha010p_log_monitor_top.
INCLUDE zha010p_log_monitor_class.
INCLUDE zha010p_log_monitor_pbo.
INCLUDE zha010p_log_monitor_pai.
INCLUDE zha010p_log_monitor_f01.

INITIALIZATION.
  s_erdat[] = VALUE #( ( sign = 'I'
                       option = 'BT'
                          low = sy-datum - 1
                         high = sy-datum ) ).


END-OF-SELECTION.
  PERFORM frm_pre_fieldcat.
  PERFORM frm_pre_fieldcat_down.
* perform frm_pre_fieldcat_auto tables gt_fieldcat_context using gs_context.

  PERFORM frm_ready_report.
  CALL SCREEN 100.
