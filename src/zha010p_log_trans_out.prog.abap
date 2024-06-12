*&---------------------------------------------------------------------*
*& 包含               ZHA010P_LOG_TRANS_OUT
*&---------------------------------------------------------------------*
"判断是否需要日志
SELECT COUNT(*) FROM ztha010_c001 WHERE fcnam = ls_zz_trans_callstack-eventname AND incov = 'X'.
IF sy-subrc = 0.
  CLEAR: lt_zz_trans_fupararef[].
ENDIF.
LOOP AT lt_zz_trans_fupararef INTO ls_zz_trans_fupararef WHERE paramtype NE 'I'.

  IF ls_zz_trans_fupararef-paramtype NE 'T'.
    ASSIGN (ls_zz_trans_fupararef-parameter) TO <fs_zz_trans_workarea>.
    IF <fs_zz_trans_workarea> IS ASSIGNED.
      IF <fs_zz_trans_workarea> IS NOT INITIAL.
        CALL METHOD zha010_cl_common=>conversion_exit_output
          CHANGING
            p_data = <fs_zz_trans_workarea>.
      ENDIF.
    ENDIF.
  ELSE.
    lv_zz_trans_table_name = ls_zz_trans_fupararef-parameter && '[]'.
    ASSIGN (lv_zz_trans_table_name) TO <fs_zz_trans_tab>.
    IF <fs_zz_trans_tab> IS ASSIGNED.
      IF <fs_zz_trans_tab> IS NOT INITIAL.
        CALL METHOD zha010_cl_common=>conversion_exit_output
          CHANGING
            p_data = <fs_zz_trans_tab>.
      ENDIF.
    ENDIF.
  ENDIF.

ENDLOOP.
