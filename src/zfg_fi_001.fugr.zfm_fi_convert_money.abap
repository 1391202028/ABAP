FUNCTION zfM_FI_convert_money.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_MONEYIN) TYPE  BWERT OPTIONAL
*"  EXPORTING
*"     VALUE(E_MONEYOUT) TYPE  STRING
*"----------------------------------------------------------------------
  DATA: l_len    TYPE i,   "数字长度
        flg_zero TYPE i, "标记：如果为1表示上个数字位为0，否则非0
        ctr_pos  TYPE i, "数字计数器
        l_monin  TYPE string,
        l_digit  TYPE c,  "存储一个数字
        l_fnout  TYPE c,  "输出汉字
        l_mod    TYPE i.    "模数输出

  l_monin = abs( i_moneyin ).
  e_moneyout = ''.
  IF abs( i_moneyin ) NE 0.
    l_len = floor( log10( l_monin ) ) + 1.
*&处理小数
    l_monin = l_monin * 100.
    l_digit = l_monin - ( l_monin DIV 10 ) * 10.
    IF l_digit NE 0.
      CALL FUNCTION 'ZFM_FI_DIGIT_CONVERT'
        EXPORTING
          i_digitin    = l_digit
        IMPORTING
          e_zhdigitout = l_fnout.
      CONCATENATE l_fnout '分' INTO  e_moneyout.
    ENDIF.
    l_monin = l_monin DIV 10.
    l_digit = l_monin - l_monin DIV 10 * 10.
    IF  l_monin NE 0 AND l_digit EQ 0 AND e_moneyout NE ''.
      CONCATENATE '零' e_moneyout INTO e_moneyout.
    ELSEIF l_digit NE 0.
      CALL FUNCTION 'ZFM_FI_DIGIT_CONVERT'
        EXPORTING
          i_digitin    = l_digit
        IMPORTING
          e_zhdigitout = l_fnout.
      CONCATENATE l_fnout '角' e_moneyout INTO e_moneyout.
    ENDIF.

    l_monin = l_monin DIV 10.
* 处理整数
    DO l_len TIMES.
      ctr_pos = ctr_pos + 1.
      l_digit = l_monin - l_monin DIV 10 * 10.
      l_monin = l_monin DIV 10.
      l_mod = ctr_pos MOD 4.
      CASE l_mod.
        WHEN 1 .
          CASE ctr_pos.
            WHEN 1.
              IF e_moneyout EQ ''.
                IF l_digit EQ 0.
                  CONCATENATE '元整' e_moneyout INTO e_moneyout.
                  flg_zero = 1.
                ELSE.
                  CALL FUNCTION 'ZFM_FI_DIGIT_CONVERT'
                    EXPORTING
                      i_digitin    = l_digit
                    IMPORTING
                      e_zhdigitout = l_fnout.
                  CONCATENATE l_fnout '元整' e_moneyout INTO e_moneyout .
                  flg_zero = 0.
                ENDIF.
              ELSE.
                IF l_digit EQ 0 AND e_moneyout+1(1) EQ '角'.
                  CONCATENATE '元零' e_moneyout INTO e_moneyout.
                  flg_zero = 1.
                ELSEIF l_digit EQ 0 AND e_moneyout(1) EQ '零'.
                  CONCATENATE '元' e_moneyout INTO e_moneyout.
                  flg_zero = 1.
                ELSE.
                  CALL FUNCTION 'ZFM_FI_DIGIT_CONVERT'
                    EXPORTING
                      i_digitin    = l_digit
                    IMPORTING
                      e_zhdigitout = l_fnout.
                  CONCATENATE l_fnout '元' e_moneyout INTO e_moneyout .
                  flg_zero = 0.
                ENDIF.
              ENDIF.
            WHEN 5.
              l_mod = l_monin MOD 1000.
              IF ( l_mod NE 0 ) AND ( l_digit EQ 0 ) AND (
                   flg_zero EQ 0 ).
                CONCATENATE '万零'  e_moneyout  INTO  e_moneyout.
                flg_zero = 1.
              ELSEIF ( l_mod NE 0 ) AND ( l_digit EQ 0 )
                AND ( flg_zero EQ 1 ).
                CONCATENATE '万'  e_moneyout  INTO  e_moneyout.
                flg_zero = 1.
              ELSEIF ( l_digit NE 0 ).
                CALL FUNCTION 'ZFM_FI_DIGIT_CONVERT'
                  EXPORTING
                    i_digitin    = l_digit
                  IMPORTING
                    e_zhdigitout = l_fnout.
                CONCATENATE l_fnout '万'  e_moneyout  INTO  e_moneyout.
                flg_zero = 0.
              ENDIF.
            WHEN 9.
              IF ( l_digit EQ 0 ) AND ( flg_zero EQ 0 ).
                CONCATENATE '亿零'  e_moneyout  INTO  e_moneyout.
                flg_zero = 1.
              ELSEIF ( l_digit EQ 0 ) AND ( flg_zero EQ 1 ).
                CONCATENATE '亿'  e_moneyout  INTO  e_moneyout.
                flg_zero = 1.
              ELSEIF ( l_digit NE 0 ).
                CALL FUNCTION 'ZFM_FI_DIGIT_CONVERT'
                  EXPORTING
                    i_digitin    = l_digit
                  IMPORTING
                    e_zhdigitout = l_fnout.
                CONCATENATE l_fnout '亿' e_moneyout  INTO  e_moneyout.
                flg_zero = 0.
              ENDIF.
            WHEN OTHERS.
              IF ( l_digit EQ 0 ) AND ( flg_zero EQ 0 ).
                CONCATENATE '万零'  e_moneyout  INTO  e_moneyout.
                flg_zero = 1.
              ELSEIF ( l_digit EQ 0 ) AND ( flg_zero EQ 1 ).
                CONCATENATE '万'  e_moneyout  INTO  e_moneyout.
                flg_zero = 1.
              ELSEIF ( l_digit NE 0 ).
                CALL FUNCTION 'ZFM_FI_DIGIT_CONVERT'
                  EXPORTING
                    i_digitin    = l_digit
                  IMPORTING
                    e_zhdigitout = l_fnout.
                CONCATENATE l_fnout '万'  e_moneyout  INTO  e_moneyout.
                flg_zero = 0.
              ENDIF.
          ENDCASE.
        WHEN 2.
          IF ( l_digit EQ 0 ) AND ( flg_zero EQ 0 ).
            CONCATENATE '零' e_moneyout INTO e_moneyout.
            flg_zero = 1.
          ELSEIF ( l_digit NE 0 ).
            CALL FUNCTION 'ZFM_FI_DIGIT_CONVERT'
              EXPORTING
                i_digitin    = l_digit
              IMPORTING
                e_zhdigitout = l_fnout.
            CONCATENATE l_fnout '拾'  e_moneyout INTO e_moneyout.
            flg_zero = 0.
          ELSE.
            flg_zero = 1.
          ENDIF .

        WHEN 3.
          IF ( l_digit EQ 0 ) AND ( flg_zero EQ 0 ).
            CONCATENATE '零' e_moneyout INTO e_moneyout.
            flg_zero = 1.
          ELSEIF ( l_digit NE 0 ).
            CALL FUNCTION 'ZFM_FI_DIGIT_CONVERT'
              EXPORTING
                i_digitin    = l_digit
              IMPORTING
                e_zhdigitout = l_fnout.
            CONCATENATE l_fnout '佰'  e_moneyout INTO e_moneyout.
            flg_zero = 0.
          ELSE.
            flg_zero = 1.
          ENDIF .
        WHEN 0.
          IF ( l_digit EQ 0 ) AND ( flg_zero EQ 0 ).
            CONCATENATE '零' e_moneyout INTO e_moneyout.
            flg_zero = 1.
          ELSEIF ( l_digit NE 0 ).
            CALL FUNCTION 'ZFM_FI_DIGIT_CONVERT'
              EXPORTING
                i_digitin    = l_digit
              IMPORTING
                e_zhdigitout = l_fnout.
            CONCATENATE l_fnout '仟'  e_moneyout INTO e_moneyout.
            flg_zero = 0.
          ELSE.
            flg_zero = 1.
          ENDIF .
*  WHEN OTHERS.
      ENDCASE.
    ENDDO.
  ELSE.
    e_moneyout = '零元整'.
  ENDIF.

ENDFUNCTION.
