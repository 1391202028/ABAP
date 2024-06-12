FUNCTION zfm_FI_digit_convert.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_DIGITIN) TYPE  C
*"  EXPORTING
*"     REFERENCE(E_ZHDIGITOUT) TYPE  C
*"----------------------------------------------------------------------
  CASE i_digitin.
    WHEN '0'.
      e_zhdigitout = '零'.
    WHEN '1'.
      e_zhdigitout = '壹'.
    WHEN '2'.
      e_zhdigitout = '贰'.
    WHEN'3'.
      e_zhdigitout = '叁'.
    WHEN'4'.
      e_zhdigitout = '肆'.
    WHEN'5'.
      e_zhdigitout = '伍'.
    WHEN'6'.
      e_zhdigitout = '陆'.
    WHEN'7'.
      e_zhdigitout = '柒'.
    WHEN'8'.
      e_zhdigitout = '捌'.
    WHEN'9'.
      e_zhdigitout = '玖'.
    WHEN OTHERS.
      e_zhdigitout = 'X'.
  ENDCASE.

ENDFUNCTION.
