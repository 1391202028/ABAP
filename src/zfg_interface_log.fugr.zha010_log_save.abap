FUNCTION zha010_log_save.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IS_LOGHD) TYPE  ZTHA010_LOGHD OPTIONAL
*"  TABLES
*"      IT_LOGDI STRUCTURE  ZTHA010_LOGDI OPTIONAL
*"      IT_LOGDE STRUCTURE  ZTHA010_LOGDE OPTIONAL
*"----------------------------------------------------------------------

  IF is_loghd IS NOT INITIAL.
    MODIFY ztha010_loghd FROM is_loghd.
    COMMIT WORK AND WAIT.
  ENDIF.
  IF it_logdi[] IS NOT INITIAL.
    MODIFY ztha010_logdi FROM TABLE it_logdi.
    COMMIT WORK AND WAIT.
  ENDIF.

  IF it_logde[] IS NOT INITIAL.
    MODIFY ztha010_logde FROM TABLE it_logde.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFUNCTION.
