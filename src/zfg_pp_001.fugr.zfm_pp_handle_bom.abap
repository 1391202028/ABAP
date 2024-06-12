FUNCTION zfm_pp_handle_bom.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  CHANGING
*"     VALUE(IT_DATA) TYPE  ZTPP0001
*"----------------------------------------------------------------------
  DATA: lt_stko   TYPE TABLE OF stko_api02,
        lt_stpo   TYPE TABLE OF stpo_api02,
        lt_csline TYPE TABLE OF csltx_line,
        ls_csline TYPE csltx_line.
  DATA: ls_mstko TYPE stko_api01,
        lt_mstpo TYPE TABLE OF stpo_api03,
        ls_mstpo TYPE stpo_api03.
  DATA: lv_fname      TYPE thead-tdname,
        lv_identifier TYPE cs_ident,
        lt_line       TYPE TABLE OF tline.
  DATA: lt_bomgrp TYPE STANDARD TABLE OF bapi1080_bgr_c,
        lt_bomvar TYPE STANDARD TABLE OF bapi1080_bom_c,
        lt_items  TYPE STANDARD TABLE OF bapi1080_itm_c,
        lt_matrel TYPE STANDARD TABLE OF bapi1080_mbm_c,
        lt_suirel TYPE STANDARD TABLE OF bapi1080_rel_sui_itm_c,
        lt_itmrel TYPE STANDARD TABLE OF bapi1080_rel_itm_bom_c,
        lt_text   TYPE TABLE OF bapi1080_txt_c,
        lt_ltext  TYPE TABLE OF bapi1080_txt_c,
        lt_return TYPE STANDARD TABLE OF bapiret2,
        ls_bomgrp TYPE bapi1080_bgr_c,
        ls_bomvar TYPE bapi1080_bom_c,
        ls_items  TYPE bapi1080_itm_c,
        ls_matrel TYPE bapi1080_mbm_c,
        ls_itmrel TYPE bapi1080_rel_itm_bom_c,
        ls_suirel TYPE bapi1080_rel_sui_itm_c,
        ls_text   TYPE bapi1080_txt_c,
        ls_return TYPE bapiret2.
  DATA: lv_message  TYPE string,
        lv_message1 TYPE string.
  DATA: lt_message TYPE TABLE OF messages.
  DATA: lv_datuv      TYPE csap_mbom-datuv,
        lv_valid_from TYPE csap_mbom-datuv,
        lv_valid_to   TYPE csap_mbom-datub.
  SELECT FROM @it_data AS a
         INNER JOIN mast AS b ON a~matnr = b~matnr
                            AND a~stlan = b~stlan
                            AND a~stlal = b~stlal
                            AND a~werks = b~werks
         FIELDS b~matnr,
                b~werks,
                b~stlan,
                b~stlal,
                b~stlnr
         INTO TABLE @DATA(lt_data).
  SORT lt_data BY matnr werks stlan stlal.
  SELECT FROM @it_data AS a
         INNER JOIN aenr AS b ON a~aennr = b~aennr
         FIELDS a~aennr,
                b~datuv
         INTO TABLE @DATA(lt_datuv).
  SORT lt_datuv BY aennr.
  LOOP AT it_data ASSIGNING FIELD-SYMBOL(<fs>).
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = <fs>-matnr
      IMPORTING
        output       = <fs>-matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.


    READ TABLE lt_data TRANSPORTING NO FIELDS WITH KEY matnr = <fs>-matnr
                                                       werks = <fs>-werks
                                                       stlan = <fs>-stlan
                                                       stlal = <fs>-stlal BINARY SEARCH.
    IF sy-subrc = 0."BOM已存在
      IF <fs>-dflag = 'X'."判断是否打删除标记，有删除标记直接删除
        CALL FUNCTION 'CSAP_MAT_BOM_DELETE'
          EXPORTING
            material           = <fs>-matnr
            plant              = <fs>-werks
            bom_usage          = <fs>-stlan
            alternative        = <fs>-stlal
            change_no          = <fs>-aennr
            fl_commit_and_wait = 'X'
          EXCEPTIONS
            error              = 1
            OTHERS             = 2.
        IF sy-subrc = 0.
          <fs>-mtype = 'S'.
          <fs>-message = 'BOM删除成功'.
        ELSE.
          <fs>-mtype = 'E'.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                        INTO <fs>-message.
          CONDENSE <fs>-message.
        ENDIF.
      ELSE."无删除标记，先读取原来数据，与原有数据判断不相同，修改
        READ TABLE lt_datuv INTO DATA(ls_datuv) WITH KEY aennr = <fs>-aennr BINARY SEARCH.
        IF  sy-subrc = 0.
          lv_valid_to = lv_valid_from = ls_datuv-datuv.
        ENDIF.
        CALL FUNCTION 'CSAP_MAT_BOM_READ'
          EXPORTING
            material    = <fs>-matnr
            plant       = <fs>-werks
            bom_usage   = <fs>-stlan
            alternative = <fs>-stlal
            valid_from  = lv_valid_from
            valid_to    = lv_valid_to
          TABLES
            t_stpo      = lt_stpo
            t_stko      = lt_stko
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.
        IF sy-subrc = 0.
          ls_mstko = CORRESPONDING #( lt_stko[ 1 ] ).
          IF ls_mstko-base_quan <> <fs>-bmeng AND <fs>-bmeng IS NOT INITIAL."父件需求量
            ls_mstko-base_quan = <fs>-bmeng.
          ENDIF.
          IF ls_mstko-base_unit <> <fs>-bmein AND <fs>-bmein IS NOT INITIAL. "父件单位
            ls_mstko-base_quan = <fs>-bmein.
          ENDIF.
          IF ls_mstko-alt_text <> <fs>-stktx  AND <fs>-stktx IS NOT INITIAL."可选BOM文本
            ls_mstko-alt_text = <fs>-stktx.
          ENDIF.
          IF ls_mstko-laboratory <> <fs>-labor AND <fs>-labor IS NOT INITIAL."实验室
            ls_mstko-laboratory = <fs>-labor.
          ENDIF.
          IF ls_mstko-bom_text <> <fs>-bom_text AND <fs>-bom_text IS NOT INITIAL.
            ls_mstko-bom_text = <fs>-bom_text.
          ENDIF.

*          "抬头长文本
*          IF <fs>-herader_txt IS NOT INITIAL.
*            ls_csline-object_id = '0'.
*            ls_csline-tdformat = '*'.
*            ls_csline-tdline = ls_mstko-bom_text .
*            APPEND ls_csline TO lt_csline.
*            lt_line = zcl_common_method=>convert_string_to_itab( iv_string = <fs>-herader_txt ).
*            LOOP AT lt_line INTO DATA(ls_line).
*              ls_csline-tdformat = ls_line-tdformat.
*              ls_csline-tdline = ls_line-tdline.
*              APPEND ls_csline TO lt_csline.
*              CLEAR:ls_csline-tdline.
*            ENDLOOP.
*          ENDIF.
          LOOP AT lt_stpo ASSIGNING FIELD-SYMBOL(<fs_stpo>).
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = <fs_stpo>-component
              IMPORTING
                output       = <fs_stpo>-component
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.
          ENDLOOP.
          lt_mstpo = CORRESPONDING #( lt_stpo[] ).
          "行项目修改
          LOOP AT <fs>-items INTO DATA(ls_item).
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = ls_item-idnrk
              IMPORTING
                output       = ls_item-idnrk
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.
            READ TABLE lt_mstpo ASSIGNING FIELD-SYMBOL(<fs_mstpo>) WITH KEY component    = ls_item-idnrk.
            IF sy-subrc = 0.
              IF ls_item-postp <> <fs_mstpo>-item_categ AND ls_item-postp IS NOT INITIAL."行项目类别
                <fs_mstpo>-item_categ = ls_item-postp.
              ENDIF.
              IF ls_item-idnrk <> <fs_mstpo>-component."组件物料
                <fs_mstpo>-component = ls_item-idnrk.
              ENDIF.
              IF ls_item-menge <> <fs_mstpo>-comp_qty AND ls_item-menge IS NOT INITIAL."组件需求数量
                <fs_mstpo>-comp_qty = ls_item-menge.
              ENDIF.
              IF ls_item-meins <> <fs_mstpo>-comp_unit AND ls_item-meins IS NOT INITIAL."组件单位
                <fs_mstpo>-comp_unit = ls_item-meins.
              ENDIF.
              IF ls_item-potx1 <> <fs_mstpo>-item_text1."行项目文本1
                <fs_mstpo>-item_text1 = ls_item-potx1.
              ENDIF.
              IF ls_item-potx2 <> <fs_mstpo>-item_text2. "行项目文本2
                <fs_mstpo>-item_text2 = ls_item-potx2.
              ENDIF.
              IF ls_item-lgort <> <fs_mstpo>-issue_loc."库存地点
                <fs_mstpo>-issue_loc = ls_item-lgort.
              ENDIF.
              IF ls_item-sortf <> <fs_mstpo>-sortstring. "排序字符串
                <fs_mstpo>-sortstring = ls_item-sortf.
              ENDIF.
              IF ls_item-alpgr <> <fs_mstpo>-ai_group. "替代组
                <fs_mstpo>-ai_group = ls_item-alpgr.
              ENDIF.
              IF ls_item-alprf <> <fs_mstpo>-ai_prio. "替代优先级
                <fs_mstpo>-ai_prio = ls_item-alprf.
              ENDIF.
              IF ls_item-alpst <> <fs_mstpo>-ai_strateg. "替代策略
                <fs_mstpo>-ai_strateg = ls_item-alpst.
              ENDIF.
              IF ls_item-ewahr <> <fs_mstpo>-usage_prob. "替代使用可能性
                <fs_mstpo>-usage_prob = ls_item-ewahr.
              ENDIF.
              IF ls_item-ausch <> <fs_mstpo>-comp_scrap. "组件报废率
                <fs_mstpo>-comp_scrap = ls_item-ausch.
              ENDIF.
              IF ls_item-sanka <> <fs_mstpo>-rel_cost. "成本核算标识
                <fs_mstpo>-rel_cost = ls_item-sanka.
              ENDIF.
              IF ls_item-avoau <> <fs_mstpo>-op_scrap. "工序报废
                <fs_mstpo>-op_scrap = ls_item-avoau.
              ENDIF.
              IF ls_item-netau <> <fs_mstpo>-op_net_ind. "净废品标识
                <fs_mstpo>-op_net_ind = ls_item-netau.
              ENDIF.
              IF ls_item-kzkup <> <fs_mstpo>-co_product. "联产品标识
                <fs_mstpo>-co_product = ls_item-kzkup.
              ENDIF.
              IF ls_item-dspst <> <fs_mstpo>-expl_type."展开类型.
                <fs_mstpo>-expl_type = ls_item-dspst.
              ENDIF.
              IF <fs_mstpo>-identifier IS INITIAL.

              ENDIF.
              ls_csline-identifier = <fs_mstpo>-identifier.
              lt_line = zcl_common_method=>convert_string_to_itab( iv_string = ls_item-item_txt ).

              IF ls_item-zflag = 'D'.                   "删除标记
                <fs_mstpo>-fldelete = abap_true.
              ENDIF.
            ELSE.
              IF ls_item-zflag = 'I'.
                ls_mstpo = CORRESPONDING #( ls_item MAPPING item_categ = postp   "项目类别
                                                            item_no    = posnr   "项目编号
                                                            component  = idnrk   "组件编码
                                                            comp_qty   = menge   "组件数量
                                                            comp_unit  = meins   "组件单位
                                                            item_text1 = potx1   "项目文本1
                                                            item_text2 = potx2   "项目文本2
                                                            issue_loc  = lgort   "库存地点
                                                            sortstring = sortf   "排序码
                                                            ai_group   = alpgr   "替代组
                                                            ai_strateg = alpst   "替代策略
                                                            ai_prio    = alprf   "替代优先级
                                                            usage_prob = ewahr   "使用可能性
                                                            comp_scrap = ausch   "组件报废率
                                                            rel_cost   = sanka   "成本核算标识
                                                            op_scrap   = avoau   "工序报废
                                                            op_net_ind = netau   "净废品标识
                                                            co_product = kzkup    "联产品标识
                                                            expl_type  = dspst ). "展开类型
                APPEND ls_mstpo TO lt_mstpo.
              ELSE.
                <fs>-mtype = 'E'.
                <fs>-message = '未查找到对应组件:' && ls_item-idnrk.
                RETURN.
              ENDIF.
            ENDIF.
          ENDLOOP.
          "行项目长文本
          CLEAR:lv_identifier.
          LOOP AT lt_mstpo ASSIGNING <fs_mstpo>.
            IF <fs_mstpo>-usage_prob = 0.
              CLEAR:<fs_mstpo>-usage_prob.
            ENDIF.
            IF <fs_mstpo>-ai_prio = '00'.
              CLEAR:<fs_mstpo>-ai_prio.
            ENDIF.
            READ TABLE <fs>-items INTO ls_item WITH KEY idnrk = <fs_mstpo>-component.
            IF sy-subrc = 0 AND ls_item-item_txt IS NOT INITIAL.
              lv_identifier = lv_identifier + 1.
              <fs_mstpo>-identifier = lv_identifier.
              lt_line = zcl_common_method=>convert_string_to_itab( iv_string = ls_item-item_txt ).
              ls_csline-identifier = lv_identifier.
              ls_csline-tdformat = '*'.
              ls_csline-tdline = ls_item-potx1.
              ls_csline-object_id = '2'.
              APPEND ls_csline TO lt_csline.
              ls_csline-tdline = ls_item-potx2.
              APPEND ls_csline TO lt_csline.
              LOOP AT lt_line INTO DATA(ls_line).
                ls_csline-identifier = lv_identifier.
                ls_csline-tdformat = ls_line-tdformat.
                ls_csline-tdline = ls_line-tdline.
                APPEND ls_csline TO lt_csline.
                CLEAR:ls_csline-tdline.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
          lv_datuv = <fs>-datuv.
          SORT lt_mstpo BY item_categ item_no.
          CALL FUNCTION 'CALO_INIT_API'
            EXPORTING
              data_reset_sign          = ' '
            EXCEPTIONS
              log_object_not_found     = 1
              log_sub_object_not_found = 2
              other_error              = 3
              OTHERS                   = 4.
          CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
            EXPORTING
              material           = <fs>-matnr
              plant              = <fs>-werks
              bom_usage          = <fs>-stlan
              alternative        = <fs>-stlal
              valid_from         = lv_datuv
              change_no          = <fs>-aennr
              i_stko             = ls_mstko
              fl_commit_and_wait = ' '
              fl_bom_create      = 'X'
              fl_complete        = 'X'
              fl_new_item        = 'X'
              fl_default_values  = 'X'
            TABLES
              t_stpo             = lt_mstpo
              t_ltx_line         = lt_csline
            EXCEPTIONS
              error              = 1
              OTHERS             = 2.
          IF sy-subrc = 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
            <fs>-mtype = 'S'.
            <fs>-message = 'BOM修改成功'.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            CALL FUNCTION 'CALO_LOG_READ_MESSAGES'
              EXPORTING
                log_class               = '4'
                language                = sy-langu
              TABLES
                messages_and_parameters = lt_message
              EXCEPTIONS
                warning                 = 1
                error                   = 2
                OTHERS                  = 3.
            LOOP AT lt_message INTO DATA(ls_message) WHERE msg_type = 'E' OR msg_type = 'A' OR msg_type = 'X'.
              <fs>-message = <fs>-message && ls_message-msg_txt.
            ENDLOOP.
            <fs>-mtype = 'E'.
            CONDENSE <fs>-message.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE."BOM不存在，创建新BOM

      "抬头参数
      ls_mstko = VALUE #( base_quan = <fs>-bmeng base_unit = <fs>-bmein
                          bom_status = <fs>-stlst alt_text = <fs>-stktx
                          laboratory = <fs>-labor bom_text = <fs>-bom_text  ).
      LOOP AT <fs>-items ASSIGNING FIELD-SYMBOL(<fs_item>).
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = <fs_item>-idnrk
          IMPORTING
            output       = <fs_item>-idnrk
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.
      ENDLOOP.
      lt_mstpo[] = CORRESPONDING #( <fs>-items MAPPING item_categ = postp   "项目类别
                                                       item_no    = posnr   "项目编号
                                                       component  = idnrk   "组件编码
                                                       comp_qty   = menge   "组件数量
                                                       comp_unit  = meins   "组件单位
                                                       item_text1 = potx1   "项目文本1
                                                       item_text2 = potx2   "项目文本2
                                                       issue_loc  = lgort   "库存地点
                                                       sortstring = sortf   "排序码
                                                       ai_group   = alpgr   "替代组
                                                       ai_strateg = alpst   "替代策略
                                                       ai_prio    = alprf   "替代优先级
                                                       usage_prob = ewahr   "使用可能性
                                                       comp_scrap = ausch   "组件报废率
                                                       rel_cost   = sanka   "成本核算标识
                                                       op_scrap   = avoau   "工序报废
                                                       op_net_ind = netau   "净废品标识
                                                       co_product = kzkup   "联产品标识
                                                       expl_type  = dspst ). "展开类型
      LOOP AT lt_mstpo ASSIGNING <fs_mstpo>.
        IF <fs_mstpo>-usage_prob = 0.
          CLEAR:<fs_mstpo>-usage_prob.
        ENDIF.
        IF <fs_mstpo>-ai_prio = '00'.
          CLEAR:<fs_mstpo>-ai_prio.
        ENDIF.
        READ TABLE <fs>-items INTO ls_item WITH KEY postp = <fs_mstpo>-item_categ
                                                    posnr =  <fs_mstpo>-item_no.
        IF sy-subrc = 0 AND ls_item-item_txt IS NOT INITIAL.
          lv_identifier = lv_identifier + 1.
          <fs_mstpo>-identifier = lv_identifier.
          lt_line = zcl_common_method=>convert_string_to_itab( iv_string = ls_item-item_txt ).
          ls_csline-identifier = lv_identifier.
          ls_csline-tdformat = '*'.
          ls_csline-tdline = ls_item-potx1.
          ls_csline-object_id = '2'.
          APPEND ls_csline TO lt_csline.
          ls_csline-tdline = ls_item-potx2.
          APPEND ls_csline TO lt_csline.
          LOOP AT lt_line INTO ls_line.
            ls_csline-identifier = lv_identifier.
            ls_csline-tdformat = ls_line-tdformat.
            ls_csline-tdline = ls_line-tdline.
            APPEND ls_csline TO lt_csline.
            CLEAR:ls_csline-tdline.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
      lv_datuv = <fs>-datuv.
      SORT lt_mstpo BY item_categ item_no.
      CALL FUNCTION 'CALO_INIT_API'
        EXPORTING
          data_reset_sign          = ' '
        EXCEPTIONS
          log_object_not_found     = 1
          log_sub_object_not_found = 2
          other_error              = 3
          OTHERS                   = 4.
      CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
        EXPORTING
          material           = <fs>-matnr
          plant              = <fs>-werks
          bom_usage          = <fs>-stlan
          alternative        = <fs>-stlal
          valid_from         = lv_datuv
          i_stko             = ls_mstko
          fl_commit_and_wait = ' '
          fl_bom_create      = 'X'
          fl_complete        = 'X'
          fl_default_values  = 'X'
        TABLES
          t_stpo             = lt_mstpo
          t_ltx_line         = lt_csline
        EXCEPTIONS
          error              = 1
          OTHERS             = 2.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        <fs>-mtype = 'S'.
        <fs>-message = 'BOM创建成功'.
      ELSE.
        CALL FUNCTION 'CALO_LOG_READ_MESSAGES'
          EXPORTING
            log_class               = '4'
            language                = sy-langu
          TABLES
            messages_and_parameters = lt_message
          EXCEPTIONS
            warning                 = 1
            error                   = 2
            OTHERS                  = 3.
        LOOP AT lt_message INTO ls_message WHERE msg_type = 'E' OR msg_type = 'A' OR msg_type = 'X'.
          <fs>-message = <fs>-message && ls_message-msg_txt.
        ENDLOOP.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        <fs>-mtype = 'E'.
        CONDENSE <fs>-message.
      ENDIF.
    ENDIF.
    CLEAR:lv_fname,lt_mstpo[],lt_csline[],ls_csline.
    SELECT SINGLE stlnr FROM mast INTO <fs>-stlnr
                        WHERE matnr = <fs>-matnr
                        AND  werks  = <fs>-werks
                        AND  stlan  = <fs>-stlan
                        AND  stlal  = <fs>-stlal.
  ENDLOOP.

ENDFUNCTION.
