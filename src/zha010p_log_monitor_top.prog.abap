*&---------------------------------------------------------------------*
*& 包含               ZHA010P_LOG_MONITOR_TOP
*&---------------------------------------------------------------------*
TABLES: ztha010_loghd.
TYPE-POOLS: slis, vrm.
CLASS lcl_event DEFINITION DEFERRED.
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: g_docking_container TYPE REF TO cl_gui_docking_container.
DATA: g_alv TYPE REF TO cl_gui_alv_grid.
DATA: g_tree TYPE REF TO cl_gui_alv_tree.

DATA: gs_fieldcat      TYPE lvc_s_fcat, "字段工作区
      gt_fieldcat      TYPE TABLE OF lvc_s_fcat,
      gt_fieldcat_down TYPE TABLE OF lvc_s_fcat.

DATA: gt_sort TYPE slis_t_sortinfo_alv WITH HEADER LINE . "排序工作表

DATA: gt_listheader  TYPE slis_t_listheader, "ALV 表头
      gs_layout      TYPE lvc_s_layo,
      gs_layout_down TYPE lvc_s_layo.                  "ALV布局工作区
DATA: gt_toolbar TYPE ui_functions,
      gs_toolbar TYPE ui_func.

DATA: event TYPE REF TO lcl_event.

DATA: gt_stbl TYPE lvc_s_stbl.
DATA: ok_code  LIKE sy-ucomm,
      gv_repid LIKE          sy-repid.
DATA: go_continer TYPE REF TO cl_gui_custom_container.

DATA: gs_events TYPE slis_alv_event,
      gt_events TYPE slis_t_event.

DATA: BEGIN OF gs_out.
        INCLUDE STRUCTURE ztha010_loghd.
DATA:   stext TYPE tftit-stext,
        icon1 TYPE icon-internal,
        icon2 TYPE icon-internal,
        icon3 TYPE icon-internal,
        icon4 TYPE icon-internal,
      END OF gs_out,
      gt_out LIKE STANDARD TABLE OF gs_out.
FIELD-SYMBOLS: <fs_out> LIKE gs_out.

DATA: BEGIN OF gs_out_down,
        icon  TYPE char04,
        value TYPE string,
      END OF gs_out_down.
DATA: gt_out_down   LIKE TABLE OF gs_out_down.

DATA: BEGIN OF gs_detail,
        node_key TYPE lvc_nkey,
        io       TYPE char01, "I传入,O传出
        patyp    LIKE ztha010_logdi-patyp,
        param    LIKE ztha010_logdi-param,
        refty    LIKE ztha010_logdi-refty,
        rtype    LIKE ztha010_logdi-rtype,
        stext    LIKE funct-stext,
        object   TYPE REF TO data,
      END OF gs_detail.
DATA: gt_detail   LIKE TABLE OF gs_detail.

DATA: event_receiver TYPE REF TO lcl_event_receiver.

DATA: gv_new_id TYPE abap_bool.
DATA: gv_zz_log_id TYPE ztha010_loghd-logid.
DATA: gcl_root TYPE REF TO cx_root.

DATA: gv_hide TYPE char01.

SELECTION-SCREEN BEGIN OF BLOCK bk01 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:   s_logid    FOR ztha010_loghd-logid,
                    s_reqid    FOR ztha010_loghd-reqid,
                    s_imgid    FOR ztha010_loghd-imgid,
                    s_fcnam    FOR ztha010_loghd-fcnam,
                    s_keyi1    FOR ztha010_loghd-keyi1,
                    s_keyi2    FOR ztha010_loghd-keyi2,
                    s_keyi3    FOR ztha010_loghd-keyi3,
                    s_keyo1    FOR ztha010_loghd-keyo1,
                    s_keyo2    FOR ztha010_loghd-keyo2,
                    s_keyo3    FOR ztha010_loghd-keyo3,
                    s_erdat    FOR ztha010_loghd-erdat,
                    s_erzet    FOR ztha010_loghd-erzet,
                    s_ernam    FOR ztha010_loghd-ernam,
                    s_msgty    FOR ztha010_loghd-msgty,
                    s_msgtx    FOR ztha010_loghd-msgtx,
                    s_terml    FOR ztha010_loghd-terml.
SELECTION-SCREEN END OF BLOCK bk01.
