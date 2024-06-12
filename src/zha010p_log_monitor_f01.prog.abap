*&---------------------------------------------------------------------*
*& 包含               ZHA010P_LOG_MONITOR_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_PRE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_pre_fieldcat.
  CLEAR gs_fieldcat.
  REFRESH gt_fieldcat.

  "动态字段名称赋值
  DATA: lo_tabdescr TYPE REF TO cl_abap_structdescr.
  DATA: ls_field_in TYPE dfies,
        lt_dfies    TYPE ddfields.

  lo_tabdescr ?= cl_abap_structdescr=>describe_by_data( gs_out ).
  CALL METHOD cl_salv_data_descr=>read_structdescr
    EXPORTING
      r_structdescr = lo_tabdescr
    RECEIVING
      t_dfies       = lt_dfies.

*  "===自动赋值fieldcat===
*  loop at lt_dfies into ls_field_in.
*    clear: gs_fieldcat.
*    move-corresponding ls_field_in to gs_fieldcat.
*    gs_fieldcat-ref_field = ls_field_in-fieldname. "参照类型字段
*    gs_fieldcat-ref_table = ls_field_in-tabname.   "参照类型的表,搜索帮助，域控制
*    if gs_fieldcat-coltext is initial.         "当COLTEXT不存在显示FILEDTEXT，如字段没域，只有手工描述的情况
*      gs_fieldcat-coltext = ls_field_in-fieldtext.
*    endif.
*    case ls_field_in-fieldname.
*      when 'MANDT' or 'SEL'.
*        continue.
*      when others.
*    endcase.
*    append gs_fieldcat to gt_fieldcat.
*  endloop.

  "===手动赋值fieldcat===
  DEFINE set_fieldcat.
    READ TABLE lt_dfies INTO ls_field_in WITH KEY fieldname = &1.
    IF sy-subrc = 0.
      CLEAR: gs_fieldcat.
      MOVE-CORRESPONDING ls_field_in TO gs_fieldcat.

      gs_fieldcat-coltext = &2.
      gs_fieldcat-reptext = &2.
      gs_fieldcat-scrtext_l = &2.
      gs_fieldcat-scrtext_m = &2.
      gs_fieldcat-scrtext_s = &2.
      gs_fieldcat-edit = &3.
      gs_fieldcat-ref_table = &4.
      gs_fieldcat-ref_field = &5.
      gs_fieldcat-lowercase      = 'X'.
      gs_fieldcat-no_out = &6.

      IF gs_fieldcat-ref_field IS INITIAL.
        gs_fieldcat-ref_field = ls_field_in-fieldname. "参照类型字段
      ENDIF.
      IF gs_fieldcat-ref_table IS INITIAL.
        gs_fieldcat-ref_table = ls_field_in-tabname.   "参照类型的表,搜索帮助，域控制
      ENDIF.
      IF gs_fieldcat-coltext IS INITIAL.               "当coltext不存在显示filedtext，如字段没域，只有手工描述的情况
        gs_fieldcat-coltext = ls_field_in-fieldtext.
      ENDIF.
      IF gs_fieldcat-outputlen IS INITIAL.             "当coltext不存在显示filedtext，如字段没域，只有手工描述的情况
        gs_fieldcat-outputlen = 40.
      ENDIF.
      APPEND gs_fieldcat TO gt_fieldcat.
    ENDIF.
  END-OF-DEFINITION.

  set_fieldcat 'LOGID' '日志号' '' '' '' ''."不赋值COLTEXT,带出数据元素的名称
  set_fieldcat 'FCNAM' '函数名' '' '' '' ''.
  set_fieldcat 'STEXT' '函数描述' '' '' '' ''.
  set_fieldcat 'SNDPN' '发送方' '' '' '' ''.
  set_fieldcat 'RCVPN' '接收方' '' '' '' ''.
  set_fieldcat 'REQID' '请求编号' '' '' '' ''.
  set_fieldcat 'IMGID' '传入消息号' '' '' '' ''.
  set_fieldcat 'KEYI1' '关键值I1' '' '' '' ''.
  set_fieldcat 'KEYI2' '关键值I2' '' '' '' ''.
  set_fieldcat 'KEYI3' '关键值I3' '' '' '' ''.
  set_fieldcat 'OLDID' '原日志号' '' '' '' ''.
  set_fieldcat 'ERDAT' '创建日期' '' '' '' ''.
  set_fieldcat 'ERZET' '创建时间' '' '' '' ''.
  set_fieldcat 'ERNAM' '创建人' '' '' '' ''.
  set_fieldcat 'RETID' '返回编号' '' '' '' 'X'.
  set_fieldcat 'OMGID' '传出消息号' '' '' '' 'X'.
  set_fieldcat 'MSGTY' '消息类型' '' '' '' ''.
  set_fieldcat 'MSGTX' '消息文本' '' '' '' ''.
  set_fieldcat 'KEYO1' '关键值O1' '' '' '' ''.
  set_fieldcat 'KEYO2' '关键值O2' '' '' '' ''.
  set_fieldcat 'KEYO3' '关键值O3' '' '' '' ''.
  set_fieldcat 'ICON1' '传入数据-转换展示' '' '' '' ''.
  set_fieldcat 'ICON2' '传入数据-初始展示' '' '' '' ''.
  set_fieldcat 'ICON3' '返回数据-转换展示' '' '' '' ''.
  set_fieldcat 'ICON4' '返回数据-初始展示' '' '' '' ''.
  set_fieldcat 'BEGTM' '开始时戳' '' '' '' ''.
  set_fieldcat 'ENDTM' '结束时戳' '' '' '' ''.
  set_fieldcat 'CSMTM' '耗时(秒)' '' '' '' ''.
  set_fieldcat 'TERML' '终端' '' '' '' ''.
  set_fieldcat 'IPADR' 'IP地址' '' '' '' 'X'.

  "其他特殊处理
  LOOP AT gt_fieldcat INTO gs_fieldcat.
    IF  gs_fieldcat-fieldname = 'LOGID' .
      gs_fieldcat-fix_column = 'X'.
      gs_fieldcat-hotspot = 'X'.
    ENDIF.
    IF gs_fieldcat-fieldname CS 'ICON'.
      gs_fieldcat-icon = 'X'.
      gs_fieldcat-hotspot = 'X'.
    ENDIF.
    MODIFY gt_fieldcat FROM gs_fieldcat.
  ENDLOOP.


  DEFINE set_sort.  "定义排序宏
    CLEAR gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos = 1.
    gs_sort-up = 'X'.
    gs_sort-subtot = 'X'.
    APPEND gs_sort TO gt_sort.
  END-OF-DEFINITION.
ENDFORM.                    " FRM_PRE_FIELDCAT

FORM frm_pre_fieldcat_down.
  gt_fieldcat_down[] = VALUE #(
                                ( fieldname = 'ICON'
                                  coltext = ''
                                  outputlen = 4
                                  icon = 'X'
                                 )
                                ( fieldname = 'VALUE'
                                  coltext = '数据'
                                  outputlen = 60
                                  hotspot = 'X'
                                 )
                               ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_pre_fieldcat_auto TABLES it_fieldcat USING data.
  CLEAR gs_fieldcat.
  REFRESH it_fieldcat.

  "动态字段名称赋值
  DATA: lo_tabdescr TYPE REF TO cl_abap_structdescr.
  DATA: ls_field_in TYPE dfies,
        lt_dfies    TYPE ddfields.

  lo_tabdescr ?= cl_abap_structdescr=>describe_by_data( data ).
  CALL METHOD cl_salv_data_descr=>read_structdescr
    EXPORTING
      r_structdescr = lo_tabdescr
    RECEIVING
      t_dfies       = lt_dfies.

  "===自动赋值fieldcat===
  LOOP AT lt_dfies INTO ls_field_in.
    CLEAR: gs_fieldcat.
    MOVE-CORRESPONDING ls_field_in TO gs_fieldcat.
    gs_fieldcat-ref_field = ls_field_in-fieldname. "参照类型字段
    gs_fieldcat-ref_table = ls_field_in-tabname.   "参照类型的表,搜索帮助，域控制
    IF gs_fieldcat-coltext IS INITIAL.         "当COLTEXT不存在显示FILEDTEXT，如字段没域，只有手工描述的情况
      gs_fieldcat-coltext = ls_field_in-fieldtext.
    ENDIF.
    CASE ls_field_in-fieldname.
      WHEN 'MANDT' OR 'SEL'.
        CONTINUE.
      WHEN OTHERS.
    ENDCASE.
    APPEND gs_fieldcat TO it_fieldcat.
  ENDLOOP.

  "===手动赋值fieldcat===
  DEFINE set_fieldcat.
    READ TABLE lt_dfies INTO ls_field_in WITH KEY fieldname = &1.
    IF sy-subrc = 0.
      CLEAR: gs_fieldcat.
      MOVE-CORRESPONDING ls_field_in TO gs_fieldcat.

      gs_fieldcat-coltext = &2.
      gs_fieldcat-edit = &3.
      gs_fieldcat-ref_table = &4.
      gs_fieldcat-ref_field = &5.
      gs_fieldcat-lowercase      = 'X'.

      IF gs_fieldcat-ref_field IS INITIAL.
        gs_fieldcat-ref_field = ls_field_in-fieldname.  "参照类型字段
      ENDIF.
      IF gs_fieldcat-ref_table IS INITIAL.
        gs_fieldcat-ref_table = ls_field_in-tabname.    "参照类型的表,搜索帮助，域控制
      ENDIF.
      IF gs_fieldcat-coltext IS INITIAL.                "当coltext不存在显示filedtext，如字段没域，只有手工描述的情况
        gs_fieldcat-coltext = ls_field_in-fieldtext.
      ENDIF.
      IF gs_fieldcat-outputlen IS INITIAL.              "当coltext不存在显示filedtext，如字段没域，只有手工描述的情况
        gs_fieldcat-outputlen = 40.
      ENDIF.
      APPEND gs_fieldcat TO it_fieldcat.
    ENDIF.
  END-OF-DEFINITION.


  "其他特殊处理
  LOOP AT it_fieldcat INTO gs_fieldcat.

    MODIFY it_fieldcat FROM gs_fieldcat.
  ENDLOOP.

  DEFINE set_sort.  "定义排序宏
    CLEAR gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos = 1.
    gs_sort-up = 'X'.
    gs_sort-subtot = 'X'.
    APPEND gs_sort TO gt_sort.
  END-OF-DEFINITION.
ENDFORM.                    " FRM_PRE_FIELDCAT

FORM exclude_tb_functions CHANGING pt_exclude TYPE ui_functions .
  DATA ls_exclude TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row . "
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste . "
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_check . "
  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_filter . "过滤器
*  append ls_exclude to pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_current_variant . "布局更改
*  append ls_exclude to pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_average ."平均值
*  append ls_exclude to pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_sum ."求和
*  append ls_exclude to pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_export .   "导出
*  append ls_exclude to pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut .        "剪切
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row . "删除行
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row . "插入行
  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh .  "刷新
*  append ls_exclude to pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_find ."查找
*  append ls_exclude to pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_asc .  "升序排列
*  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_dsc .  "降序排列
*  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_views .   "视图
  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_print ."打印
*  append ls_exclude to pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_detail . "详细按钮
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph . "显示图形
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info . "最终用户文档
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row . "附加行
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row . "复制行
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy . "复制文本
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo . "撤消
  APPEND ls_exclude TO pt_exclude.

ENDFORM .


*&---------------------------------------------------------------------*
*& Form frm_set_scren
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_scren .

  gv_repid = sy-repid.

  IF g_docking_container IS INITIAL.
    CREATE OBJECT g_docking_container
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        extension = 400
        side      = cl_gui_docking_container=>dock_at_right.
  ENDIF.
  IF g_alv IS INITIAL.
    CREATE OBJECT g_alv
      EXPORTING
        i_parent = cl_gui_container=>screen0.

    PERFORM exclude_tb_functions CHANGING gt_toolbar. "隐藏某些按钮
    gs_layout-cwidth_opt = 'X'.
    gs_layout-box_fname = 'SEL'.
    gs_layout-sel_mode = 'A'.

    CREATE OBJECT event.
    SET HANDLER event->handle_user_command
                event->handle_double_click
                event->handle_hotspot_click
         FOR g_alv.

    CALL METHOD g_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = gt_toolbar
      CHANGING
        it_outtab            = gt_out
        it_fieldcatalog      = gt_fieldcat.
    CALL METHOD g_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
    CALL METHOD g_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
  ENDIF.

  IF g_tree IS INITIAL.
    CREATE OBJECT g_tree
      EXPORTING
        parent              = g_docking_container
        node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single
        item_selection      = ''
        no_html_header      = 'X'
        no_toolbar          = ''.
    DATA l_hierarchy_header TYPE treev_hhdr.
    l_hierarchy_header-heading = '类型'.
    l_hierarchy_header-tooltip = '类型'.
    l_hierarchy_header-width = 28.
    l_hierarchy_header-width_pix = ' '.
    CALL METHOD g_tree->set_table_for_first_display
      EXPORTING
        i_structure_name    = 'GS_OUT_DOWN'
        is_hierarchy_header = l_hierarchy_header
      CHANGING
        it_fieldcatalog     = gt_fieldcat_down
        it_outtab           = gt_out_down. "table must be empty !

    PERFORM register_events.
*   SET_TREE
    PERFORM set_tree.
*   触发事件。
    CALL METHOD g_tree->update_calculations.
    CALL METHOD g_tree->update_calculations.
    "Send data to frontend.
    CALL METHOD g_tree->frontend_update.
  ENDIF.

  g_docking_container->set_visible( gv_hide ).
ENDFORM.
FORM register_events.
*§4. Event registration: tell ALV Tree which events shall be passed
*    from frontend to backend.
  DATA: lt_events        TYPE        cntl_simple_events,
        l_event          TYPE        cntl_simple_event,
        l_event_receiver TYPE REF TO lcl_event_receiver.

  CALL METHOD g_tree->get_registered_events
    IMPORTING
      events = lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  l_event-appl_event = 'X'.
  APPEND l_event TO lt_events.

  CALL METHOD g_tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.
*--------------------
**§4d. Register events on backend (ABAP Objects event handling)
  CREATE OBJECT l_event_receiver.
  SET HANDLER l_event_receiver->node_double_click FOR g_tree.

ENDFORM.                               " register_events
**&---------------------------------------------------------------------*
**&      Form  fill_tree
**&---------------------------------------------------------------------*
FORM set_tree .
  DATA: l_root_key_i TYPE lvc_nkey,
        l_root_key_o TYPE lvc_nkey.
  DATA: lv_text TYPE lvc_value.

  IF gt_detail[] IS INITIAL.
    CLEAR: gs_out_down.
    PERFORM tree_add USING '' '未选择' CHANGING l_root_key_i.
  ELSE.
    CLEAR: gs_out_down.
    PERFORM tree_add USING '' '传入数据' CHANGING l_root_key_i.
    PERFORM tree_add USING '' '传出数据' CHANGING l_root_key_o.
    LOOP AT gt_detail INTO gs_detail WHERE io = 'I'.
      lv_text = gs_detail-param. "文本
      CLEAR: gs_out_down.
      MOVE-CORRESPONDING gs_detail TO gs_out_down.

      IF gs_detail-object IS NOT INITIAL.
        ASSIGN gs_detail-object->* TO FIELD-SYMBOL(<fs_data>).
        PERFORM frm_get_reflection USING gs_detail-param l_root_key_i <fs_data>. "读取最后反射
      ENDIF.
    ENDLOOP.
    LOOP AT gt_detail INTO gs_detail WHERE io = 'O'.
      lv_text = gs_detail-param. "文本
      CLEAR: gs_out_down.
      MOVE-CORRESPONDING gs_detail TO gs_out_down.

      IF gs_detail-object IS NOT INITIAL.
        ASSIGN gs_detail-object->* TO <fs_data>.

        PERFORM frm_get_reflection USING gs_detail-param l_root_key_o <fs_data>. "读取最后反射

      ENDIF.
    ENDLOOP.

    READ TABLE gt_detail TRANSPORTING NO FIELDS WITH KEY io = 'I'.
    IF sy-subrc = 0 .
      g_tree->expand_node( l_root_key_i ).
    ENDIF.
    READ TABLE gt_detail TRANSPORTING NO FIELDS WITH KEY io = 'O'.
    IF sy-subrc = 0 .
      g_tree->expand_node( l_root_key_o ).
    ENDIF.
  ENDIF.

ENDFORM.                    " fill_tree

"反射
FORM frm_get_reflection USING iv_name
                              iv_last_key TYPE lvc_nkey
                              i_data.
  DATA: lv_key TYPE lvc_nkey.
  DATA: lv_text TYPE lvc_value.

  DATA: dref_str    TYPE REF TO data,
        dref_tab    TYPE REF TO data,
        dref_i      TYPE REF TO data,
        itab_type   TYPE REF TO cl_abap_tabledescr,
        struct_type TYPE REF TO cl_abap_structdescr,
        elem_type   TYPE REF TO cl_abap_elemdescr,
        table_type  TYPE REF TO cl_abap_tabledescr,
        comp_tab    TYPE cl_abap_structdescr=>component_table WITH HEADER LINE.
  FIELD-SYMBOLS :<fs_itab> TYPE ANY TABLE.

  DATA: co_empty_ref TYPE REF TO data.
  DATA: abap_type TYPE REF TO cl_abap_typedescr.

  lv_text = iv_name.

  CASE cl_abap_typedescr=>describe_by_data( i_data )->kind.
    WHEN 'R'.
      ASSIGN i_data->* TO FIELD-SYMBOL(<fs_ref_data>).
      PERFORM frm_get_reflection USING iv_name iv_last_key <fs_ref_data>.
    WHEN 'S'.
      CLEAR: gs_out_down.
      gs_out_down-icon = icon_draw_linear.
      PERFORM tree_add USING iv_last_key lv_text CHANGING lv_key. "加入树

      struct_type ?= cl_abap_typedescr=>describe_by_data( i_data ).
      comp_tab[] = struct_type->get_components( )."组成结构体的各个字段组件
      LOOP AT comp_tab.
        ASSIGN COMPONENT comp_tab-name OF STRUCTURE i_data TO FIELD-SYMBOL(<fs_data>).
        IF <fs_data> IS ASSIGNED.
          PERFORM frm_get_reflection USING comp_tab-name lv_key <fs_data>. "递归。
          UNASSIGN <fs_data>.
        ENDIF.
      ENDLOOP.
    WHEN 'T'.
      CLEAR: gs_out_down.
      gs_out_down-icon = icon_table_settings.
      PERFORM tree_add USING iv_last_key lv_text CHANGING lv_key. "加入树

      ASSIGN i_data TO <fs_itab>.
      LOOP AT <fs_itab> ASSIGNING FIELD-SYMBOL(<fs_line>).
        lv_text = '行' && sy-tabix.
        PERFORM frm_get_reflection USING lv_text lv_key <fs_line>. "递归。
      ENDLOOP.
    WHEN 'E'. "追加行
      elem_type ?= cl_abap_typedescr=>describe_by_data( i_data ).
      CLEAR: gs_out_down.
      gs_out_down-value = i_data.
      PERFORM tree_add USING iv_last_key lv_text CHANGING lv_key. "加入树
    WHEN OTHERS.
  ENDCASE.
ENDFORM.

FORM tree_add_last_key TABLES it_table USING iv_per_key .

  DATA: l_root_key TYPE lvc_nkey,
        l_file_key TYPE lvc_nkey,
        l_last_key TYPE lvc_nkey.
  DATA: lv_text TYPE lvc_value.

  LOOP AT it_table ASSIGNING FIELD-SYMBOL(<fs_line>).
    lv_text = sy-tabix.
    CLEAR: gs_out_down.
    CALL METHOD cl_abap_container_utilities=>fill_container_c
      EXPORTING
        im_value               = <fs_line>
      IMPORTING
        ex_container           = gs_out_down-value
      EXCEPTIONS
        illegal_parameter_type = 1
        OTHERS                 = 2.

    PERFORM tree_add USING iv_per_key lv_text CHANGING l_last_key.
  ENDLOOP.
ENDFORM.

FORM tree_add USING root_key TYPE lvc_nkey
                     text TYPE lvc_value
               CHANGING out_key TYPE lvc_nkey.
  CALL METHOD g_tree->add_node
    EXPORTING
      i_relat_node_key = root_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = text
      is_outtab_line   = gs_out_down
    IMPORTING
      e_new_node_key   = out_key.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_ready_report
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_ready_report .
  PERFORM frm_get_data.           "获取数据
  PERFORM frm_deal_data.          "处理数据
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_get_data .
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE gt_out
    FROM ztha010_loghd
    WHERE logid     IN s_logid
      AND keyi1     IN s_keyi1
      AND keyi2     IN s_keyi2
      AND keyi3     IN s_keyi3
      AND keyo1     IN s_keyo1
      AND keyo2     IN s_keyo2
      AND keyo3     IN s_keyo3
      AND erdat     IN s_erdat
      AND erzet     IN s_erzet
      AND ernam     IN s_ernam
      AND reqid     IN s_reqid
      AND imgid     IN s_imgid
      AND msgtx     IN s_msgtx
      AND msgty     IN s_msgty
      AND terml     IN s_terml
      AND fcnam     IN s_fcnam .
ENDFORM.                   " FRM_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*----------------------------------------------------------------------*
FORM frm_deal_data.
  DATA: ls_tftit TYPE tftit,
        lt_tftit TYPE TABLE OF tftit.

  CALL METHOD zha010_cl_common=>get_text_table
    EXPORTING
      table_name    = 'TFTIT'
      intab_field   = 'FCNAM'
      outtab_field  = 'FUNCNAME'
      langu_field   = 'SPRAS'
      other_sql     = ''
      object_struct = gs_out
      object_table  = gt_out
    IMPORTING
      text_table    = lt_tftit.

  LOOP AT gt_out ASSIGNING <fs_out>.
    READ TABLE lt_tftit INTO ls_tftit WITH KEY funcname = <fs_out>-fcnam BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_out>-stext = ls_tftit-stext.
    ENDIF.
    <fs_out>-icon1 = icon_parameter_import.
    <fs_out>-icon2 = icon_parameter_import.
    <fs_out>-icon3 = icon_parameter_result.
    <fs_out>-icon4 = icon_parameter_result.
  ENDLOOP.

  SORT gt_out BY logid DESCENDING.
ENDFORM.                    "FRM_GET_DATA
*&---------------------------------------------------------------------*
*& Form FRM_HOTSPOT_CLICK_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_hotspot_click_out USING iv_id.
  DATA: ls_data_i TYPE ztha010_logdi,
        ls_data_e TYPE ztha010_logde,
        lt_data_i TYPE TABLE OF ztha010_logdi,
        lt_data_e TYPE TABLE OF ztha010_logde.
  DATA: lv_json_string TYPE string.
  "动态创建工作区、内表
  FIELD-SYMBOLS  <fs_aber>    TYPE any.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_data_i
    FROM ztha010_logdi
   WHERE logid = gs_out-logid.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_data_e
    FROM ztha010_logde
   WHERE logid = gs_out-logid.

  SELECT *
    INTO TABLE @DATA(lt_funct)
    FROM funct
   WHERE spras = @sy-langu
     AND funcname = @gs_out-fcnam.

  CLEAR: gt_detail[].

  "获取内容
  LOOP AT lt_data_i INTO ls_data_i.
    READ TABLE gt_detail ASSIGNING FIELD-SYMBOL(<fs_detail>) WITH KEY io = 'I' param = ls_data_i-param.
    IF sy-subrc <> 0.
      CLEAR: gs_detail.
      gs_detail-io = 'I'.
      MOVE-CORRESPONDING ls_data_i TO gs_detail.
      APPEND gs_detail TO gt_detail.
    ENDIF.
  ENDLOOP.
  LOOP AT lt_data_e INTO ls_data_e.
    READ TABLE gt_detail ASSIGNING <fs_detail> WITH KEY io = 'O' param = ls_data_e-param.
    IF sy-subrc <> 0.
      CLEAR: gs_detail.
      gs_detail-io = 'O'.
      MOVE-CORRESPONDING ls_data_e TO gs_detail.
      APPEND gs_detail TO gt_detail.
    ENDIF.
  ENDLOOP.

  "读取描述
  LOOP AT gt_detail ASSIGNING <fs_detail>.
    READ TABLE lt_funct INTO DATA(ls_funct) WITH KEY parameter = <fs_detail>-param.
    IF sy-subrc = 0.
      <fs_detail>-stext = ls_funct-stext.
    ENDIF.

    "读取传入值
    CLEAR: lv_json_string.
    LOOP AT lt_data_i INTO ls_data_i WHERE param = <fs_detail>-param.
      lv_json_string = lv_json_string && ls_data_i-jsons.
    ENDLOOP.
    IF lv_json_string IS NOT INITIAL AND ls_data_i-refty IS NOT INITIAL.
      IF ls_data_i-rtype = 'X'.
        CREATE DATA <fs_detail>-object TYPE (ls_data_i-refty).
      ELSE.
        CREATE DATA <fs_detail>-object TYPE TABLE OF (ls_data_i-refty).
      ENDIF.
      ASSIGN <fs_detail>-object->* TO <fs_aber>.

      /ui2/cl_json=>deserialize( EXPORTING json        = lv_json_string
*                                           pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                 CHANGING  data        = <fs_aber> ).
    ENDIF.

    "读取传出值
    CLEAR: lv_json_string.
    LOOP AT lt_data_e INTO ls_data_e WHERE param = <fs_detail>-param.
      lv_json_string = lv_json_string && ls_data_e-jsons.
    ENDLOOP.
    IF lv_json_string IS NOT INITIAL AND ls_data_e-refty IS NOT INITIAL..
      IF ls_data_e-rtype = 'X'.
        CREATE DATA <fs_detail>-object TYPE (ls_data_e-refty).
      ELSE.
        CREATE DATA <fs_detail>-object TYPE TABLE OF (ls_data_e-refty).
      ENDIF.
      ASSIGN <fs_detail>-object->* TO <fs_aber>.
      /ui2/cl_json=>deserialize( EXPORTING json        = lv_json_string
*                                          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                 CHANGING  data        = <fs_aber> ).
    ENDIF.

  ENDLOOP.
  CALL METHOD g_tree->free.
  CLEAR g_tree.
  FREE g_tree.

  gv_hide = 'X'.
ENDFORM.


FORM frm_display_json.
  DATA: ls_data TYPE ztha010_logdi,
        lt_data TYPE TABLE OF ztha010_logdi.
  DATA: lv_json_string TYPE string.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_data
    FROM ztha010_logdi
    WHERE logid = gs_out-logid.
  READ TABLE lt_data INTO ls_data INDEX 1.
  IF sy-subrc = 0.
    "读取函数名称
    SELECT *
      INTO TABLE @DATA(lt_funct)
      FROM funct
      WHERE funcname = @ls_data-fcnam
        AND spras = @sy-langu.
  ENDIF.

  SORT lt_data BY linno.
  LOOP AT lt_data INTO ls_data.
    CLEAR: ls_data-linno .
    MODIFY lt_data FROM ls_data.
  ENDLOOP.

  PERFORM frm_write_json TABLES lt_data lt_funct USING `输入数据` 'I'.
  PERFORM frm_write_json TABLES lt_data lt_funct USING `更改数据` 'C'.
  PERFORM frm_write_json TABLES lt_data lt_funct USING `表数据` 'T'.

  cl_demo_output=>end_section( ).
  cl_demo_output=>display( ).
ENDFORM.

FORM frm_display_return_json.
  DATA: ls_data TYPE ztha010_logdi,
        lt_data TYPE TABLE OF ztha010_logdi.
  FIELD-SYMBOLS: <ls_data> TYPE ztha010_logdi.
  DATA: lv_json_string TYPE string.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_data
    FROM ztha010_logde
    WHERE logid = gs_out-logid.
  READ TABLE lt_data INTO ls_data INDEX 1.
  IF sy-subrc = 0.
    "读取函数名称
    SELECT *
      INTO TABLE @DATA(lt_funct)
      FROM funct
     WHERE funcname = @ls_data-fcnam
       AND spras = @sy-langu.
  ENDIF.

  SORT lt_data BY linno.
  LOOP AT lt_data INTO ls_data.
    CLEAR: ls_data-linno.
    MODIFY lt_data FROM ls_data.
  ENDLOOP.

  PERFORM frm_write_json TABLES lt_data lt_funct USING `输入数据` 'I'.
  PERFORM frm_write_json TABLES lt_data lt_funct USING `输出数据` 'E'.
  PERFORM frm_write_json TABLES lt_data lt_funct USING `更改数据` 'C'.
  PERFORM frm_write_json TABLES lt_data lt_funct USING `表数据` 'T'.

  cl_demo_output=>end_section( ).
  cl_demo_output=>display( ).
ENDFORM.
FORM frm_write_json TABLES it_data STRUCTURE ztha010_logdi
                           it_funct STRUCTURE funct
                     USING iv_name TYPE string
                           iv_paramtype TYPE char01.
  DATA: lv_name TYPE string.

  DATA: lv_json_string TYPE string.
  cl_demo_output=>next_section( iv_name ).
  LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data>) WHERE patyp = iv_paramtype.
    lv_json_string = lv_json_string && <ls_data>-jsons.
    AT END OF param.
      READ TABLE it_funct INTO DATA(ls_funct) WITH KEY parameter = <ls_data>-param.
      IF sy-subrc = 0.
        lv_name = `[` && ls_funct-parameter && `] ` && ls_funct-stext.
      ENDIF.

      PERFORM frm_transdisplay_data USING <ls_data> lv_name lv_json_string.
      CLEAR: lv_json_string.
    ENDAT.
  ENDLOOP.
ENDFORM.

FORM frm_transdisplay_data USING iv_data TYPE ztha010_logdi
                                 iv_name TYPE string
                                 iv_json_data TYPE string.
  "动态创建工作区、内表
  DATA           object     TYPE REF TO data.
  FIELD-SYMBOLS  <fs_aber>    TYPE any.

  IF iv_data-rtype = 'X'.
    CREATE DATA object TYPE (iv_data-refty).
    ASSIGN object->* TO <fs_aber>.
  ELSE.
    CREATE DATA object TYPE TABLE OF (iv_data-refty).
    ASSIGN object->* TO <fs_aber>.
  ENDIF.

  /ui2/cl_json=>deserialize( EXPORTING json        = iv_json_data
*                                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case "update by luf
                             CHANGING  data        = <fs_aber> ).
  cl_demo_output=>write_data( value = <fs_aber> name = iv_name ).
ENDFORM.

FORM frm_display_json_old.

  DATA: ls_data TYPE ztha010_logdi,
        lt_data TYPE TABLE OF ztha010_logdi.
  DATA: lv_post_data TYPE string.

  DATA: lv_json_string TYPE string.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_data
    FROM ztha010_logdi
    WHERE logid = gs_out-logid.

  SORT lt_data BY linno.
  LOOP AT lt_data INTO ls_data.
    CLEAR: ls_data-linno.
    MODIFY lt_data FROM ls_data.
  ENDLOOP.

  cl_demo_output=>next_section( `输入数据` ).
  LOOP AT lt_data INTO ls_data WHERE patyp = 'I'.
    lv_post_data = ls_data-param && `  参照类型(` && ls_data-refty && ')'.
    lv_json_string = lv_json_string && ls_data-jsons.
    AT END OF param.
      cl_demo_output=>write_data( value = lv_json_string name = lv_post_data ).
      CLEAR: lv_json_string.
    ENDAT.
  ENDLOOP.

  cl_demo_output=>next_section( `更改数据` ).
  LOOP AT lt_data INTO ls_data WHERE patyp = 'C'.
    lv_post_data = ls_data-param && `  参照类型(` && ls_data-refty && ')'.
    lv_json_string = lv_json_string && ls_data-jsons.
    AT END OF param.
      cl_demo_output=>write_data( value = lv_json_string name = lv_post_data ).
      CLEAR: lv_json_string.
    ENDAT.
  ENDLOOP.

  cl_demo_output=>next_section( `表数据` ).
  LOOP AT lt_data INTO ls_data WHERE patyp = 'T'.

    lv_post_data = ls_data-param && `  参照类型(` && ls_data-refty && ')'.
    lv_json_string = lv_json_string && ls_data-jsons.
    AT END OF param.
      cl_demo_output=>write_data( value = lv_json_string name = lv_post_data ).
      CLEAR: lv_json_string.
    ENDAT.
  ENDLOOP.

  cl_demo_output=>end_section( ).
  cl_demo_output=>display( ).
ENDFORM.


FORM frm_display_return_json_old.
  DATA: ls_data TYPE ztha010_logdi,
        lt_data TYPE TABLE OF ztha010_logdi.
  DATA: lv_post_data TYPE string.

  DATA: lv_json_string TYPE string.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_data
    FROM ztha010_logde
    WHERE logid = gs_out-logid.

  SORT lt_data BY linno.
  LOOP AT lt_data INTO ls_data.
    CLEAR: ls_data-linno .
    MODIFY lt_data FROM ls_data.
  ENDLOOP.

  cl_demo_output=>next_section( `输入数据` ).
  LOOP AT lt_data INTO ls_data WHERE patyp = 'I'.
    lv_post_data = ls_data-param && `  参照类型(` && ls_data-refty && ')'.
    lv_json_string = lv_json_string && ls_data-jsons.
    AT END OF param.
      cl_demo_output=>write_data( value = lv_json_string name = lv_post_data ).
      CLEAR: lv_json_string.
    ENDAT.
  ENDLOOP.

  cl_demo_output=>next_section( `输出数据` ).
  LOOP AT lt_data INTO ls_data WHERE patyp = 'E'.
    lv_post_data = ls_data-param && `  参照类型(` && ls_data-refty && ')'.
    lv_json_string = lv_json_string && ls_data-jsons.
    AT END OF param.
      cl_demo_output=>write_data( value = lv_json_string name = lv_post_data ).
      CLEAR: lv_json_string.
    ENDAT.
  ENDLOOP.

  cl_demo_output=>next_section( `更改数据` ).
  LOOP AT lt_data INTO ls_data WHERE patyp = 'C'.
    lv_post_data = ls_data-param && `  参照类型(` && ls_data-refty && ')'.
    lv_json_string = lv_json_string && ls_data-jsons.
    AT END OF param.
      cl_demo_output=>write_data( value = lv_json_string name = lv_post_data ).
      CLEAR: lv_json_string.
    ENDAT.
  ENDLOOP.

  cl_demo_output=>next_section( `表数据` ).
  LOOP AT lt_data INTO ls_data WHERE patyp = 'T'.

    lv_post_data = ls_data-param && `  参照类型(` && ls_data-refty && ')'.
    lv_json_string = lv_json_string && ls_data-jsons.
    AT END OF param.
      cl_demo_output=>write_data( value = lv_json_string name = lv_post_data ).
      CLEAR: lv_json_string.
    ENDAT.

  ENDLOOP.

  cl_demo_output=>end_section( ).
  cl_demo_output=>display( ).
ENDFORM.

FORM frm_resend.
  DATA: et_row_no   TYPE lvc_t_roid.
  DATA: ls_data TYPE ztha010_logdi,
        lt_data TYPE TABLE OF ztha010_logdi.
  FIELD-SYMBOLS: <ls_data> TYPE ztha010_logdi.

  DATA: ls_ptab TYPE abap_func_parmbind,
        lt_ptab TYPE abap_func_parmbind_tab.
  DATA: ls_etab TYPE abap_func_excpbind,
        lt_etab TYPE abap_func_excpbind_tab.
  DATA: BEGIN OF ls_id,
          logid TYPE ztha010_loghd-logid,
          fcnam TYPE ztha010_loghd-fcnam,
          msgty TYPE ztha010_loghd-msgty,
        END OF ls_id,
        lt_id LIKE TABLE OF ls_id.
  DATA: lv_post_data TYPE string.
  DATA: lv_json_string TYPE string.

  DATA           object     TYPE REF TO data.
  FIELD-SYMBOLS  <fs_aber>    TYPE any.

  CLEAR: lt_ptab[],
         lt_data[].
  CALL METHOD g_alv->get_selected_rows
    IMPORTING
      et_row_no = et_row_no.
  LOOP AT et_row_no INTO DATA(es_row_no).
    READ TABLE gt_out INTO gs_out INDEX es_row_no-row_id.
    IF sy-subrc = 0.
      CLEAR: ls_id.
      MOVE-CORRESPONDING gs_out TO ls_id.
      APPEND ls_id TO lt_id.
    ENDIF.
  ENDLOOP.

  IF lt_id[] IS NOT INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_data
      FROM ztha010_logdi
      FOR ALL ENTRIES IN lt_id
      WHERE logid = lt_id-logid.

    SORT lt_data BY logid linno.
    LOOP AT lt_data INTO ls_data.
      CLEAR: ls_data-linno.
      MODIFY lt_data FROM ls_data.
    ENDLOOP.
  ENDIF.


  LOOP AT lt_id INTO ls_id.
    IF gv_new_id = '' AND  ls_id-msgty = 'S'. "普通重发，成功的数据禁止重发
      CONTINUE.
    ELSE.
      SELECT SINGLE * INTO @DATA(lt_loghd_tmp)
        FROM ztha010_loghd
       WHERE oldid = @ls_id-logid.
      IF sy-subrc = 0.                   "如果重发过，禁止再次重发
        CONTINUE.
      ENDIF.
    ENDIF.
    CLEAR: lt_ptab[],
           lt_etab[].

    gv_zz_log_id = ls_id-logid. "赋值当前ID
    LOOP AT lt_data ASSIGNING <ls_data> WHERE logid = ls_id-logid AND patyp <> 'X'.
      "动态创建工作区、内表
      IF <ls_data>-rtype = 'X'.
        CREATE DATA object TYPE (<ls_data>-refty).
        ASSIGN object->* TO <fs_aber>.
      ELSE.
        CREATE DATA object TYPE TABLE OF (<ls_data>-refty).
        ASSIGN object->* TO <fs_aber>.
      ENDIF.

      lv_json_string = lv_json_string && <ls_data>-jsons.

      AT END OF param.

        /ui2/cl_json=>deserialize( EXPORTING json        = lv_json_string
                                             pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                   CHANGING  data        = <fs_aber> ).

        ls_ptab-value = object.
        ls_ptab-name      = <ls_data>-param.
        CASE <ls_data>-patyp.
          WHEN 'I'.
            ls_ptab-kind      = abap_func_exporting.
          WHEN 'C'.
            ls_ptab-kind      = abap_func_changing.
          WHEN 'T'.
            ls_ptab-kind      = abap_func_tables.
          WHEN OTHERS.
        ENDCASE.

        INSERT ls_ptab INTO TABLE lt_ptab.
        CLEAR: ls_ptab.
        CLEAR: lv_json_string.
      ENDAT.
    ENDLOOP.

    "异常
    LOOP AT lt_data ASSIGNING <ls_data> WHERE logid = ls_id-logid AND patyp = 'X'.
      CLEAR: ls_etab.
      ls_etab-name = <ls_data>-param.
      INSERT ls_etab INTO TABLE lt_etab.
    ENDLOOP.

    "调用函数
    TRY .
        CALL FUNCTION ls_id-fcnam
          PARAMETER-TABLE lt_ptab
          EXCEPTION-TABLE lt_etab.
      CATCH cx_root INTO gcl_root .
        DATA(lv_text) = gcl_root->if_message~get_longtext( ).
        MESSAGE i001(00) WITH '函数' ls_id-fcnam '异常，' lv_text.
    ENDTRY.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_hide
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_hide .
  IF gv_hide = 'X'.
    gv_hide = ''.
  ELSE.
    gv_hide = 'X'.
  ENDIF.
ENDFORM.
