FUNCTION zfm_fi_document_post.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  CHANGING
*"     VALUE(CT_DATA) TYPE  ZTFI0001
*"----------------------------------------------------------------------
  DATA:ls_head       TYPE bapiache09,
       ls_customer   TYPE bapiacpa09,
       lt_return     TYPE TABLE OF bapiret2,
       ls_return     TYPE bapiret2,
       lt_gl         TYPE TABLE OF bapiacgl09,
       lt_receivable TYPE TABLE OF bapiacar09,
       lt_payable    TYPE TABLE OF bapiacap09,
       lt_tax        TYPE TABLE OF bapiactx09,
       ls_tax        TYPE bapiactx09,
       lt_curr       TYPE TABLE OF bapiaccr09,
       ls_curr       TYPE bapiaccr09,
       lt_criteria   TYPE TABLE OF bapiackec9,
       ls_criteria   TYPE bapiackec9,
       lt_extension2 TYPE TABLE OF bapiparex,
       ls_extension2 TYPE bapiparex,
       ls_accit      TYPE zsfi0003,
       lv_type       TYPE bapiache09-obj_type,
       lv_key        TYPE bapiache09-obj_key,
       lv_sys        TYPE bapiache09-obj_sys.
*汇率
  DATA:ls_exch     TYPE bapi1093_0,
       ls_return_a TYPE bapiret1.
  DATA:lv_msg      TYPE string.
  "DATA:ls_zsfi0004 TYPE zsfi0004.
  SELECT FROM t001 FIELDS bukrs,waers ORDER BY bukrs INTO TABLE @DATA(lt_t001) .

  LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs>).
    "ls_zsfi0004 = CORRESPONDING #( <fs> ).
    ls_head-username        = sy-uname.
    ls_head-comp_code       = <fs>-bukrs.
    ls_head-doc_date        = <fs>-bldat.   "凭证日期
    ls_head-pstng_date      = <fs>-budat. "记账日期
    ls_head-fisc_year       = <fs>-budat+0(4). "会计年度
    ls_head-fis_period      = <fs>-budat+4(2).   "期间
    ls_head-ref_doc_no_long = <fs>-xblnr.  "参考凭证号
    ls_head-glo_ref1_hd     = <fs>-xref1_hd.  "参考代码1
    ls_head-header_txt      = <fs>-bktxt.  "抬头文本
    ls_head-doc_type        = <fs>-blart.  "凭证类型

    "一次性客户
    ls_customer-name        = <fs>-name.
    ls_customer-name_2      = <fs>-name_2.
    ls_customer-city        = <fs>-ort01.
    ls_customer-country     = <fs>-land1.
    LOOP AT <fs>-items INTO DATA(ls_item).
      CASE ls_item-bschl.
        WHEN '40' OR '50'."总账
          "PERFORM frm_set_zz TABLES lt_gl USING ls_item ls_zsfi0004.
        WHEN '21' OR '31' OR '29' OR '39'."供应商
          "PERFORM frm_set_gys TABLES lt_payable USING ls_item ls_zsfi0004.
        WHEN '01' OR '11' OR '09' OR '19'."客户
          "PERFORM frm_set_kh TABLES lt_receivable USING ls_item ls_zsfi0004.
        WHEN OTHERS.
      ENDCASE.
      CASE ls_item-bschl.
        WHEN '50'
          OR '11'
          OR '19'
          OR '31'
          OR '39'
          OR '75'.
          ls_item-dmbtr = ls_item-dmbtr * -1.  "本币金额
          ls_item-wrbtr = ls_item-wrbtr * -1.
        WHEN OTHERS.
      ENDCASE.
      READ TABLE lt_t001 INTO DATA(ls_t001) WITH KEY bukrs = <fs>-bukrs BINARY SEARCH.
      IF sy-subrc = 0.
        IF <fs>-kursf IS INITIAL.
          CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
            EXPORTING
              rate_type  = 'M'
              from_curr  = <fs>-waers
              to_currncy = ls_t001-waers
              date       = <fs>-budat
            IMPORTING
              exch_rate  = ls_exch
              return     = ls_return_a.

          IF <fs>-waers = ls_t001-waers.
            ls_curr-itemno_acc = ls_item-buzei.  "会计凭证行项目编号
            ls_curr-currency = <fs>-waers.  "货币码
            ls_curr-amt_doccur = ls_item-wrbtr / ls_exch-exch_rate.  "以凭证货币计的金额
            ls_curr-amt_base = ls_item-wrbtr.  "以凭证货币计的金额
            APPEND ls_curr TO lt_curr.
            CLEAR ls_curr.
          ELSE.
            ls_curr-itemno_acc = ls_item-buzei.  "会计凭证行项目编号
            ls_curr-currency = <fs>-waers.  "货币码
            ls_curr-curr_type = '00'.         "凭证金额
            ls_curr-amt_doccur = ls_item-wrbtr / ls_exch-exch_rate.
            APPEND ls_curr TO lt_curr.
            CLEAR ls_curr.

            ls_curr-itemno_acc = ls_item-buzei.  "会计凭证行项目编号
            ls_curr-currency = ls_t001-waers.  "货币码
            ls_curr-curr_type = '10'.          "本位币金额
            ls_curr-amt_doccur = ls_item-wrbtr.
            APPEND ls_curr TO lt_curr.
            CLEAR ls_curr.
          ENDIF.
        ELSE.
          IF ls_t001-waers <> <fs>-waers .
            ls_curr-itemno_acc = ls_item-buzei.  "会计凭证行项目编号
            ls_curr-currency = <fs>-waers.  "货币码
            ls_curr-curr_type = '00'.       "凭证金额
            ls_curr-amt_doccur = ls_item-wrbtr.
            APPEND ls_curr TO lt_curr.
            CLEAR ls_curr.

            ls_curr-itemno_acc =  ls_item-buzei.  "会计凭证行项目编号
            ls_curr-currency = ls_t001-waers.  "货币码
            ls_curr-curr_type = '10'.          "本位币金额
            ls_curr-amt_doccur = ls_item-wrbtr * <fs>-kursf.
            APPEND ls_curr TO lt_curr.
            CLEAR ls_curr.
          ELSE.
            ls_curr-itemno_acc = ls_item-buzei.  "会计凭证行项目编号
            ls_curr-currency = <fs>-waers.  "货币码
            ls_curr-amt_doccur = ls_item-wrbtr.  "以凭证货币计的金额
            ls_curr-amt_base = ls_item-wrbtr.  "以凭证货币计的金额
            APPEND ls_curr TO lt_curr.
            CLEAR ls_curr.
          ENDIF.
        ENDIF.
      ENDIF.
      "COPA特性分配
      IF ls_item-hkont+0(4) = '6001' OR ls_item-hkont+0(4) = '6401'
          OR ls_item-hkont+0(4) = '6051' OR ls_item-hkont+0(4) = '6402' .
        ls_criteria-itemno_acc = ls_item-buzei.
        ls_criteria-fieldname = 'KNDNR'.
        IF ls_item-zzkunnr IS NOT INITIAL.
          ls_item-zzkunnr = |{ ls_item-zzkunnr ALPHA = IN }|.
          ls_criteria-character = ls_item-zzkunnr .
          APPEND ls_criteria TO lt_criteria.
          CLEAR ls_criteria.
        ENDIF.
      ENDIF.
*      ls_accit-posnr   = ls_item-buzei.  "会计凭证行项目编号
*      ls_accit-bschl   = ls_item-bschl.  "过账代码
*      ls_accit-rstgr   = ls_item-rstgr."原因代码
*      ls_accit-zzrstgr = ls_item-rstgr. "原因代码
*      ls_accit-umskz   = ls_item-umskz."特殊总账标识
*      ls_accit-xnegp   = ls_item-xngep."标识：负过账
*      ls_accit-zuonr   = ls_item-zuonr."分配
*      ls_accit-xref1   = ls_item-xref1."参考码1
*      ls_accit-xref2   = ls_item-xref2."参考码2
*      ls_accit-xref3   = ls_item-xref3."参考码3
*      ls_accit-sgtxt   = ls_item-sgtxt."项目文本
*      ls_accit-zzmwskz = ls_item-zzmwskz."税码
*      ls_accit-zzkunnr = ls_item-zzkunnr."客户
*      ls_accit-zzlifnr = ls_item-zzlifnr."供应商
*      ls_accit-ebeln   = ls_item-ebeln. "采购凭证
*      ls_extension2-structure = 'ZSFI0003'.
*      ls_extension2+30(960) = ls_accit.
*      APPEND ls_extension2 TO lt_extension2.
      CLEAR:ls_extension2,
            ls_accit.
    ENDLOOP.
*    DATA:ls_zzrbkp_append TYPE zzrbkp_append.
*    ls_zzrbkp_append-zzjsfp = '12125125'.
*    EXPORT zzjsfp = ls_zzrbkp_append-zzjsfp TO MEMORY ID 'JSFP'.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader    = ls_head
      IMPORTING
        obj_type          = lv_type
        obj_key           = lv_key
        obj_sys           = lv_sys
      TABLES
        accountgl         = lt_gl
        accountreceivable = lt_receivable
        accountpayable    = lt_payable
        accounttax        = lt_tax
        currencyamount    = lt_curr
        criteria          = lt_criteria
        return            = lt_return
        extension2        = lt_extension2.
    IF lt_return[] IS NOT INITIAL.

      CLEAR lv_msg.
      LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.

        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = ls_return-id
            msgnr               = ls_return-number
            msgv1               = ls_return-message_v1
            msgv2               = ls_return-message_v2
            msgv3               = ls_return-message_v3
            msgv4               = ls_return-message_v4
          IMPORTING
            message_text_output = lv_msg.

        CONCATENATE <fs>-message lv_msg INTO <fs>-message SEPARATED BY ' '.
        CLEAR:lv_msg,
              ls_return.

      ENDLOOP.

      IF <fs>-message IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CONCATENATE '创建凭证失败!原因是：' <fs>-message INTO <fs>-message.
        <fs>-mtype = 'E'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        <fs>-belnr = lv_key+0(10).
        <fs>-gjahr = <fs>-budat+0(4).
        <fs>-mtype = 'S'.
        <fs>-message = '会计凭证' && lv_key+0(10) && ',公司代码' && ls_head-comp_code && ',会计年度' && ls_head-fisc_year  && '创建成功'.
      ENDIF.
    ENDIF.
  ENDLOOP.



ENDFUNCTION.
FORM frm_set_zz TABLES pt_gl STRUCTURE bapiacgl09
                 USING ps_itab STRUCTURE zsfi0002.
                      " ps_head STRUCTURE zsfi0004.
  DATA:ls_gl         TYPE bapiacgl09.
  IF ps_itab-hkont IS NOT INITIAL.
    ls_gl-gl_account = |{ ps_itab-hkont ALPHA = IN }|.   "总账科目
  ENDIF.
  ls_gl-itemno_acc = ps_itab-buzei.   "行项目
  ls_gl-item_text = ps_itab-sgtxt.    "项目文本
*  ls_gl-doc_type = ps_head-blart.     "凭证类型
*  ls_gl-comp_code = ps_head-bukrs.    "公司代码
*  ls_gl-fisc_year = ps_head-budat+0(4)."会计年度
*  ls_gl-pstng_date = ps_head-budat.    "记账日期
  ls_gl-alloc_nmbr = ps_itab-zuonr.   "分配号
  ls_gl-orderid = |{ ps_itab-aufnr ALPHA = IN }|.
  IF ps_itab-kostl IS NOT INITIAL.
    ls_gl-costcenter = |{ ps_itab-kostl ALPHA = IN }|.   "成本中心
  ENDIF.
  IF ps_itab-prctr IS NOT INITIAL.
    ls_gl-profit_ctr = |{ ps_itab-prctr ALPHA = IN }|.   "利润中心
  ENDIF.
  IF ps_itab-zzkunnr IS NOT INITIAL.
    ls_gl-customer = |{ ps_itab-zzkunnr ALPHA = IN }|.
  ENDIF.
  IF ps_itab-zzlifnr IS NOT INITIAL.
    ls_gl-vendor_no = |{ ps_itab-zzlifnr ALPHA = IN }|.
  ENDIF.
  ls_gl-ref_key_1 = ps_itab-xref1.      "业务伙伴参考码
  ls_gl-ref_key_2 = ps_itab-xref2.      "业务伙伴参考码
  ls_gl-ref_key_3 = ps_itab-xref3.      "业务伙伴参考码
  ls_gl-tax_code = ps_itab-zzmwskz.       "销售/购买税代码
  ls_gl-po_number = ps_itab-ebeln.
  APPEND ls_gl TO pt_gl.
  CLEAR ls_gl.
ENDFORM.
FORM frm_set_gys TABLES pt_payable STRUCTURE bapiacap09
                 USING ps_itab STRUCTURE zsfi0002.
                     "  ps_head STRUCTURE zsfi0004.
  DATA:ls_payable    TYPE bapiacap09.
  ls_payable-itemno_acc = ps_itab-buzei.       "会计凭证行项目编号
  IF ps_itab-zzlifnr IS NOT INITIAL.
    ls_payable-vendor_no = |{ ps_itab-zzlifnr ALPHA = IN }|.       "供应商科目编号
  ENDIF.
  IF ps_itab-hkont IS NOT INITIAL.
    ls_payable-gl_account = |{ ps_itab-hkont ALPHA = IN }|.   "总账科目
  ENDIF.
  ls_payable-ref_key_1 = ps_itab-xref1.      "业务伙伴参考码
  ls_payable-ref_key_2 = ps_itab-xref2.      "业务伙伴参考码
  ls_payable-ref_key_2 = ps_itab-xref3.      "业务伙伴参考码
  "ls_payable-comp_code = ps_head-bukrs.    "公司代码
  ls_payable-alloc_nmbr = ps_itab-zuonr.  "分配号
  ls_payable-item_text = ps_itab-sgtxt.    "项目文本
  ls_payable-sp_gl_ind = ps_itab-umskz.    "特殊总分类帐标志
  ls_payable-tax_code = ps_itab-zzmwskz.    "税码
  IF ps_itab-prctr IS NOT INITIAL.
    ls_payable-profit_ctr = |{ ps_itab-prctr ALPHA = IN }|.   "利润中心
  ENDIF.
  APPEND ls_payable TO pt_payable.
  CLEAR ls_payable.
ENDFORM.
FORM frm_set_kh TABLES pt_receivable STRUCTURE bapiacar09
                 USING ps_itab STRUCTURE zsfi0002.
                       "ps_head STRUCTURE zsfi0004.
  DATA ls_receivable TYPE bapiacar09.
  ls_receivable-itemno_acc = ps_itab-buzei.       "会计凭证行项目编号
  IF ps_itab-zzkunnr IS NOT INITIAL.
    ls_receivable-customer = |{ ps_itab-zzkunnr ALPHA = IN }|.       "客户编号
  ENDIF.
  IF ps_itab-hkont IS NOT INITIAL.
    ls_receivable-gl_account = |{ ps_itab-hkont ALPHA = IN }|.   "总账科目
  ENDIF.
  ls_receivable-ref_key_1 = ps_itab-xref1.      "业务伙伴参考码
  ls_receivable-ref_key_2 = ps_itab-xref2.      "业务伙伴参考码
  ls_receivable-ref_key_2 = ps_itab-xref3.      "业务伙伴参考码
  "ls_receivable-comp_code = ps_head-bukrs.    "公司代码
  ls_receivable-alloc_nmbr = ps_itab-zuonr.  "分配号
  ls_receivable-item_text = ps_itab-sgtxt.    "项目文本
  ls_receivable-sp_gl_ind = ps_itab-umskz.    "特殊总分类帐标志
  ls_receivable-tax_code = ps_itab-zzmwskz.    "税码
  ls_receivable-pmnttrms = ps_itab-zterm.
  ls_receivable-bline_date = ps_itab-zfbdt.
  IF ps_itab-prctr IS NOT INITIAL.
    ls_receivable-profit_ctr = |{ ps_itab-prctr ALPHA = IN }|.   "利润中心
  ENDIF.
  APPEND ls_receivable TO pt_receivable.
  CLEAR ls_receivable.

ENDFORM.
