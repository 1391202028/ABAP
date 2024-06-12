*&---------------------------------------------------------------------*
*& 包含               ZHA010P_LOG_MONITOR_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_event DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      handle_onf4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display,
      handle_modify
        FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,
      handle_change
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,
      handle_hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,
      handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid "屏幕中的双击事件，可以具体到某行某列，即使设置热点也必须双击
        IMPORTING e_row e_column es_row_no.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_event IMPLEMENTATION.

  METHOD handle_toolbar.
*    data:gs_toolbar type stb_button.             "按钮
*    gs_toolbar-function =  'LALL'.       "为按钮分配功能码
*    gs_toolbar-icon     =  icon_select_all.  "为按钮分配图标
**    GS_TOOLBAR-TEXT     =  'ALL'.      "为按钮分配文本
**    GS_TOOLBAR-BUTN_TYPE = '0'.           "定义按钮类型，不填时默认为0
*    append gs_toolbar to e_object->mt_toolbar. "添加按钮到工具栏
*
*    gs_toolbar-function =  'LSAL'.       "为按钮分配功能码
*    gs_toolbar-icon     =  icon_deselect_all.  "为按钮分配图标
**    GS_TOOLBAR-TEXT     =  'ALL'.      "为按钮分配文本
**    GS_TOOLBAR-BUTN_TYPE = '0'.           "定义按钮类型，不填时默认为0
*    append gs_toolbar to e_object->mt_toolbar. "添加按钮到工具栏


  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.
*    PERFORM HANDLE_USER_COMMAND_LOWER USING E_UCOMM.
    CASE e_ucomm.
*      when 'LALL'.
*        loop at gt_out into gs_out.
*          gs_out-mark = 'X'.
*          modify gt_out from gs_out.
*        endloop.
*        call method g_alv->refresh_table_display.
*      when 'LSAL'.
*        loop at gt_out into gs_out.
*          gs_out-mark = ''.
*          modify gt_out from gs_out.
*        endloop.
*        call method g_alv->refresh_table_display.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                           "handle_user_command
  METHOD handle_onf4.

  ENDMETHOD.
  METHOD handle_modify.

  ENDMETHOD.
  METHOD handle_change.

  ENDMETHOD.

  METHOD handle_hotspot_click.
    READ TABLE gt_out INTO gs_out INDEX es_row_no-row_id.
    IF sy-subrc = 0.
      CASE e_column_id.
        WHEN 'LOGID'.
          PERFORM frm_hotspot_click_out USING gs_out-logid.
          cl_gui_cfw=>set_new_ok_code( 'A' ).
        WHEN 'ICON1'.
          PERFORM frm_display_json.
        WHEN 'ICON2'.
          PERFORM frm_display_json_old.
        WHEN 'ICON3'.
          PERFORM frm_display_return_json.
        WHEN 'ICON4'.
          PERFORM frm_display_return_json_old.
        WHEN OTHERS.
      ENDCASE.

*     call method G_TREE->refresh_table_display.
    ENDIF.
  ENDMETHOD.


  METHOD handle_double_click.
    READ TABLE gt_out INTO gs_out INDEX es_row_no-row_id.
    IF sy-subrc = 0.
      SET PARAMETER ID 'LIB' FIELD gs_out-fcnam.
      CALL TRANSACTION 'SE37' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
    ENDIF.
  ENDMETHOD.
*-----------------------------------------------------------------
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS: node_double_click
      FOR EVENT node_double_click
      OF cl_gui_alv_tree
      IMPORTING node_key sender.
ENDCLASS.                    "lcl_event_receiver DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD node_double_click.
    DATA: lt_children TYPE lvc_t_nkey.
    CALL METHOD sender->get_children
      EXPORTING
        i_node_key  = node_key
      IMPORTING
        et_children = lt_children.
*   perform dclik using node_key .
  ENDMETHOD.
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
