class ZHA010_CL_COMMON definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  class-methods GET_TEXT_TABLE
    importing
      !TABLE_NAME type STRING
      !INTAB_FIELD type STRING
      !OUTTAB_FIELD type STRING
      !LANGU_FIELD type CHAR10
      !OTHER_SQL type STRING
      !OBJECT_STRUCT type ANY optional
      !OBJECT_TABLE type STANDARD TABLE
    exporting
      value(TEXT_TABLE) type STANDARD TABLE .
  class-methods SET_FIELDCAT
    importing
      !IM_OBJECT_STRUCT type ANY
      !IM_FIELDNAME type FIELDNAME
      !IM_COLTEXT type LVC_TXTCOL
      !IM_XML type STRING
    returning
      value(OUT_FIELDCAT) type LVC_S_FCAT .
  class-methods CREATE_XLS_FROM_ITAB
    importing
      !IT_FIELDCAT type LVC_T_FCAT
      !IT_SORT type LVC_T_SORT optional
      !IT_FILT type LVC_T_FILT optional
      !IS_LAYOUT type LVC_S_LAYO optional
      !I_XLSX type FLAG
      !I_DEFALUT_NAME type STRING
    exporting
      !E_XSTRING type XSTRING
    changing
      !CT_DATA type STANDARD TABLE .
  class-methods CONVERSION_EXIT_INPUT
    changing
      !P_DATA type ANY .
  class-methods CONVERSION_EXIT_OUTPUT
    changing
      !P_DATA type ANY .
  class-methods GET_TIMESTAMP
    returning
      value(EV_TIMESTAMP) type STRING .
  class-methods CONVERT_PASSWORD
    importing
      value(IV_PASSWORD) type STRING
    returning
      value(EV_PASSWORD) type STRING .
  class-methods GET_ESB_TOKEN
    exporting
      !EV_TYPE type BAPI_MTYPE
      !EV_MESSAGE type BAPI_MSG
    returning
      value(EV_TOKEN) type STRING .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZHA010_CL_COMMON IMPLEMENTATION.


  METHOD conversion_exit_input.

    DATA: lv_exclude_exit TYPE string VALUE 'ABPSP/ABPSN/EXCRT'. "按照项目需求做增强

    DATA: lo_cl_abap_typedescr TYPE REF TO cl_abap_typedescr.
    DATA: lo_cl_abap_elemdescr TYPE REF TO cl_abap_elemdescr.
    DATA: lo_cl_abap_refdescr TYPE REF TO cl_abap_refdescr.
    DATA: lo_cl_abap_structdescr TYPE REF TO cl_abap_structdescr.
    DATA: lo_cl_abap_tabledescr TYPE REF TO cl_abap_tabledescr.
    DATA: lo_cl_abap_classdescr TYPE REF TO cl_abap_classdescr.
    DATA: lo_cl_abap_intfdescr TYPE REF TO cl_abap_intfdescr.

    DATA: ls_zz_trans_comp_descr    TYPE abap_componentdescr.
    DATA: lt_zz_trans_comp_tab    TYPE cl_abap_structdescr=>component_table.
    DATA: lv_zz_trans_table_name TYPE string.
    DATA: ls_zz_trans_fupararef TYPE fupararef,
          lt_zz_trans_fupararef TYPE TABLE OF fupararef.
    DATA: lo_zz_trans_root_error TYPE REF TO cx_root.

    "因为通用名称，所以变量必须特殊
    DATA: lv_zz_trans_funtion_name TYPE string.

    FIELD-SYMBOLS: <fs_zz_trans_field> TYPE any.
    FIELD-SYMBOLS: <fs_zz_trans_workarea> TYPE any.
    FIELD-SYMBOLS : <pt_data> TYPE ANY TABLE.

    IF p_data IS NOT INITIAL.
      "反射
      lo_cl_abap_typedescr = cl_abap_typedescr=>describe_by_data( p_data ).
      CASE lo_cl_abap_typedescr->kind.
        WHEN 'E'. "元素
          lo_cl_abap_elemdescr ?= lo_cl_abap_typedescr.
          IF lo_cl_abap_elemdescr->edit_mask IS NOT INITIAL AND lv_exclude_exit NS lo_cl_abap_elemdescr->edit_mask+2. "存在转换例程,且不在排除范围
            lv_zz_trans_funtion_name = 'CONVERSION_EXIT_' && lo_cl_abap_elemdescr->edit_mask+2 && '_INPUT'.
            TRY .
                CALL FUNCTION lv_zz_trans_funtion_name
                  EXPORTING
                    input         = p_data
                  IMPORTING
                    output        = p_data
                  EXCEPTIONS
                    error_message = 99.
                IF sy-subrc <> 0.

                ENDIF.
              CATCH cx_root INTO lo_zz_trans_root_error.
            ENDTRY.
          ENDIF.
        WHEN 'S'."结构
          "反射
          lo_cl_abap_structdescr ?= cl_abap_typedescr=>describe_by_data( p_data ).
          lt_zz_trans_comp_tab[] = lo_cl_abap_structdescr->get_components( )."组成结构体的各个字段组件
          LOOP AT lt_zz_trans_comp_tab INTO ls_zz_trans_comp_descr WHERE as_include = 'X'. "递归INCLUDE结构
            lo_cl_abap_structdescr ?= ls_zz_trans_comp_descr-type.
            APPEND LINES OF lo_cl_abap_structdescr->get_components( ) TO lt_zz_trans_comp_tab[].
          ENDLOOP.
          LOOP AT lt_zz_trans_comp_tab INTO ls_zz_trans_comp_descr.
            IF ls_zz_trans_comp_descr-as_include = 'X'.
              DELETE lt_zz_trans_comp_tab.
              CONTINUE.
            ENDIF.

            "存在例程
            ASSIGN COMPONENT ls_zz_trans_comp_descr-name OF STRUCTURE p_data TO <fs_zz_trans_field>.
            IF <fs_zz_trans_field> IS ASSIGNED.
              conversion_exit_input( CHANGING p_data = <fs_zz_trans_field> ).
              UNASSIGN <fs_zz_trans_field>.
            ENDIF.

          ENDLOOP.

        WHEN 'T'."结构

          IF p_data IS NOT INITIAL.

            ASSIGN p_data TO <pt_data>.

            IF <pt_data> IS ASSIGNED.
              "转换处理
              LOOP AT <pt_data> ASSIGNING <fs_zz_trans_workarea>.
                conversion_exit_input( CHANGING p_data = <fs_zz_trans_workarea> ).
              ENDLOOP.
            ENDIF.

          ENDIF.

        WHEN OTHERS.
      ENDCASE.

    ENDIF.
  ENDMETHOD.


  METHOD conversion_exit_output.
    DATA: lv_exclude_exit TYPE string VALUE 'ABPSP/ABPSN/EXCRT'. "按照项目需求做增强

    DATA: lo_cl_abap_typedescr TYPE REF TO cl_abap_typedescr.
    DATA: lo_cl_abap_elemdescr TYPE REF TO cl_abap_elemdescr.
    DATA: lo_cl_abap_refdescr TYPE REF TO cl_abap_refdescr.
    DATA: lo_cl_abap_structdescr TYPE REF TO cl_abap_structdescr.
    DATA: lo_cl_abap_tabledescr TYPE REF TO cl_abap_tabledescr.
    DATA: lo_cl_abap_classdescr TYPE REF TO cl_abap_classdescr.
    DATA: lo_cl_abap_intfdescr TYPE REF TO cl_abap_intfdescr.

    DATA: ls_zz_trans_comp_descr    TYPE abap_componentdescr.
    DATA: lt_zz_trans_comp_tab    TYPE cl_abap_structdescr=>component_table.
    DATA: lv_zz_trans_table_name TYPE string.
    DATA: ls_zz_trans_fupararef TYPE fupararef,
          lt_zz_trans_fupararef TYPE TABLE OF fupararef.
    DATA: lo_zz_trans_root_error TYPE REF TO cx_root.

    "因为通用名称，所以变量必须特殊
    DATA: lv_zz_trans_funtion_name TYPE string.

    FIELD-SYMBOLS: <fs_zz_trans_field> TYPE any.
    FIELD-SYMBOLS: <fs_zz_trans_workarea> TYPE any.
    FIELD-SYMBOLS : <pt_data> TYPE ANY TABLE.

    IF p_data IS NOT INITIAL.
      "反射
      lo_cl_abap_typedescr = cl_abap_typedescr=>describe_by_data( p_data ).
      CASE lo_cl_abap_typedescr->kind.
        WHEN 'E'. "元素
          lo_cl_abap_elemdescr ?= lo_cl_abap_typedescr.
          IF lo_cl_abap_elemdescr->edit_mask IS NOT INITIAL AND lv_exclude_exit NS lo_cl_abap_elemdescr->edit_mask+2. "存在转换例程,且不在排除范围
            lv_zz_trans_funtion_name = 'CONVERSION_EXIT_' && lo_cl_abap_elemdescr->edit_mask+2 && '_OUTPUT'.
            TRY .
                CALL FUNCTION lv_zz_trans_funtion_name
                  EXPORTING
                    input         = p_data
                  IMPORTING
                    output        = p_data
                  EXCEPTIONS
                    error_message = 99.
                IF sy-subrc <> 0.

                ENDIF.
              CATCH cx_root INTO lo_zz_trans_root_error.
            ENDTRY.
          ENDIF.
        WHEN 'S'."结构
          "反射
          lo_cl_abap_structdescr ?= cl_abap_typedescr=>describe_by_data( p_data ).
          lt_zz_trans_comp_tab[] = lo_cl_abap_structdescr->get_components( )."组成结构体的各个字段组件
          LOOP AT lt_zz_trans_comp_tab INTO ls_zz_trans_comp_descr WHERE as_include = 'X'. "递归INCLUDE结构
            lo_cl_abap_structdescr ?= ls_zz_trans_comp_descr-type.
            APPEND LINES OF lo_cl_abap_structdescr->get_components( ) TO lt_zz_trans_comp_tab[].
          ENDLOOP.
          LOOP AT lt_zz_trans_comp_tab INTO ls_zz_trans_comp_descr.
            IF ls_zz_trans_comp_descr-as_include = 'X'.
              DELETE lt_zz_trans_comp_tab.
              CONTINUE.
            ENDIF.

            "存在例程
            ASSIGN COMPONENT ls_zz_trans_comp_descr-name OF STRUCTURE p_data TO <fs_zz_trans_field>.
            IF <fs_zz_trans_field> IS ASSIGNED.
              conversion_exit_output( CHANGING p_data = <fs_zz_trans_field> ).
              UNASSIGN <fs_zz_trans_field>.
            ENDIF.

          ENDLOOP.

        WHEN 'T'."结构

          ASSIGN p_data TO <pt_data>.

          IF <pt_data> IS ASSIGNED.
            "转换处理
            LOOP AT <pt_data> ASSIGNING <fs_zz_trans_workarea>.
              conversion_exit_output( CHANGING p_data = <fs_zz_trans_workarea> ).
            ENDLOOP.
          ENDIF.

        WHEN OTHERS.
      ENDCASE.

    ENDIF.
  ENDMETHOD.


  METHOD convert_password.
    DATA:lv_xstring  TYPE xstring.
    CALL METHOD cl_http_utility=>if_http_utility~decode_x_base64
      EXPORTING
        encoded = iv_password
      RECEIVING
        decoded = lv_xstring.
    CALL METHOD cl_http_utility=>if_http_utility~decode_utf8
      EXPORTING
        encoded   = lv_xstring
      RECEIVING
        unencoded = ev_password.
  ENDMETHOD.


  METHOD create_xls_from_itab.
    DATA: ls_xml_choice TYPE if_salv_bs_xml=>s_type_xml_choice.
    DATA: mt_fcat TYPE lvc_t_fcat.
    DATA: mt_data       TYPE REF TO data.
    DATA: m_flavour TYPE string.
    DATA: m_version TYPE string.
    DATA: mo_result_data TYPE REF TO cl_salv_ex_result_data_table.
    DATA: mo_columns  TYPE REF TO cl_salv_columns_table.
    DATA: mo_aggreg   TYPE REF TO cl_salv_aggregations.
    DATA: mo_salv_table  TYPE REF TO cl_salv_table.
    DATA: m_file_type TYPE salv_bs_constant.
    FIELD-SYMBOLS <tab> TYPE ANY TABLE.

    GET REFERENCE OF ct_data INTO mt_data.

*if we didn&apos;t pass fieldcatalog we need to create it
    IF it_fieldcat[] IS INITIAL.
      ASSIGN mt_data->* TO <tab>.
      TRY .
          cl_salv_table=>factory(
            EXPORTING
              list_display = abap_false
            IMPORTING
              r_salv_table = mo_salv_table
            CHANGING
              t_table      = <tab> ).
        CATCH cx_salv_msg.

      ENDTRY.
      "get colums &amp; aggregation infor to create fieldcat
      mo_columns  = mo_salv_table->get_columns( ).
      mo_aggreg   = mo_salv_table->get_aggregations( ).
      mt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
        r_columns      = mo_columns
        r_aggregations = mo_aggreg ).

    ELSE.
*else we take the one we passed
      mt_fcat[] = it_fieldcat[].
    ENDIF.


    IF cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_25 OR
       cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_26.

      mo_result_data = cl_salv_ex_util=>factory_result_data_table(
        r_data         = mt_data
        s_layout       = is_layout
        t_fieldcatalog = mt_fcat
        t_sort         = it_sort
        t_filter       = it_filt
      ).

      CASE cl_salv_bs_a_xml_base=>get_version( ).
        WHEN if_salv_bs_xml=>version_25.
          m_version = if_salv_bs_xml=>version_25.
        WHEN if_salv_bs_xml=>version_26.
          m_version = if_salv_bs_xml=>version_26.
      ENDCASE.

      "if we flag i_XLSX then we&apos;ll create XLSX if not then MHTML excel file
      IF i_xlsx IS NOT INITIAL.
        m_file_type = if_salv_bs_xml=>c_type_xlsx.
      ELSE.
        m_file_type = if_salv_bs_xml=>c_type_mhtml.
      ENDIF.


      m_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export.
      "transformation of data to excel
      CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
        EXPORTING
          xml_type      = m_file_type
          xml_version   = m_version
          r_result_data = mo_result_data
          xml_flavour   = m_flavour
          gui_type      = if_salv_bs_xml=>c_gui_type_gui
        IMPORTING
          xml           = e_xstring
          t_msg         = DATA(lt_msg).        "YI3K229857.

      ls_xml_choice = VALUE #( key = 31
                              text = 'Excel（Office 2007 XLSX 格式）'
                              frontend = 'Y'
                              default_file_name = 'content.xml'
                              initial_directory = 'C:\TEMP'
                              version = '02'
                              xml_type = '10'
                              gui_type = '02'

      ).

      "需要自定名称，把代码抽取出来
*      cl_salv_export_xml_dialog=>download(
*                        exporting
*                          s_xml_choice = ls_xml_choice
*                          xml          = e_xstring
*                        ).


      DATA:
        l_frontend          TYPE lvc_front,
        l_default_extension TYPE string,
        l_initial_directory TYPE string,
        l_mode              TYPE flag,
        l_length            TYPE i,
        l_default_file_name TYPE string,
        l_ok                TYPE c.

      DATA: s_title       TYPE string,
            s_mask        TYPE char255,
            s_mask1       TYPE string,
            l_user_action TYPE i.

*... get default extension
      CALL METHOD cl_alv_bds=>create_mask_for_filefilter
        EXPORTING
          i_frontend          = ls_xml_choice-frontend
        IMPORTING
          e_default_extension = l_default_extension
        CHANGING
          c_mask              = s_mask.

      s_mask1 = s_mask.

*... determine PC appl.: only possible in Windows GUI due to REGEX calls
*      if cl_salv_export_xml_dialog=>gui_type                eq cl_salv_export_xml_dialog=>c_gui_type_windows.
      DATA: l_application TYPE string.

      l_application = cl_salv_bs_xml_utils=>get_pc_application(
        ls_xml_choice-frontend ).
*      else.
      IF ls_xml_choice-frontend EQ cl_alv_bds=>mc_mhtml_frontend.
        l_default_extension = cl_alv_bds=>mc_excel_extension.
*<<< YI2K140157 Excel file extension  opens Excel in client
*    mask adapted to extension in JAVA and HTML Gui
        s_mask1 = 'Excel (*.XLS)|*.XLS'.
      ENDIF.
*      endif.

      CONCATENATE i_defalut_name '.' l_default_extension INTO l_default_file_name.

*... call Filedownload Dialog and download file
      CALL FUNCTION 'XML_EXPORT_DIALOG'
        EXPORTING
          i_xml                      = e_xstring
          i_default_extension        = l_default_extension
          i_initial_directory        = l_initial_directory
          i_default_file_name        = l_default_file_name
          i_mask                     = s_mask1
          i_application              = l_application
        EXCEPTIONS
          application_not_executable = 1
          OTHERS                     = 2.

      IF sy-subrc <> 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_esb_token.
    TYPES:BEGIN OF ty_data,
            account TYPE string,
            secret  TYPE string,
          END OF ty_data.

    TYPES:BEGIN OF ty_token,
            token      TYPE string,
            expires_in TYPE string,
          END OF ty_token.
    TYPES:BEGIN OF ty_return,
            code    TYPE string,
            message TYPE string,
            data    TYPE ty_token,
          END OF ty_return.
    DATA:lv_len           TYPE i, "发送报文长度
         lv_url           TYPE string, "接口地址
         lo_http_client   TYPE REF TO if_http_client, "http客户端
         lv_send_json     TYPE string, "发送的JSON
         lv_rec_json      TYPE string, "接收的JSON
         lv_authorization TYPE string,
         lv_code          TYPE i, "HTTP 返回状态
         lv_reason        TYPE string. " HTTP 状态描述
    DATA:lv_password TYPE string,
         lv_username TYPE string,
         lv_ein01    TYPE string,
         lv_ein02    TYPE string.
    DATA:ls_data TYPE ty_data.
    DATA:ls_return TYPE ty_return.
    DATA(ls_pretty) = /ui2/cl_json=>pretty_mode-camel_case.
    SELECT SINGLE exusr,
                  expwd,
                  exurl,
                  ein01,
                  ein02 FROM ztha001_c003
                  INTO (@lv_username,@lv_password,@lv_url,@lv_ein01,@lv_ein02 )
                  WHERE exsys = 'ESB' AND exnod = 'GET_ESB_TOKEN'.
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
        proxy_host         = 'proxy'
        proxy_service      = '3128'
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.
    lv_password = convert_password( lv_password ).
    "不显示登录界面
    lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.
    CALL METHOD lo_http_client->authenticate
      EXPORTING
        username = lv_username
        password = lv_password.
    "设定调用方法 get,post
    lo_http_client->request->set_method( if_http_request=>co_request_method_post  ).
    "设定传输请求内容格式以及编码格式
    lo_http_client->request->set_content_type( content_type = 'application/json; charset=utf-8' ).
    ls_data = VALUE #( account = lv_ein01 secret = lv_ein02 ).
    lv_send_json = /ui2/cl_json=>serialize( data           = ls_data
                                            compress       = abap_true
                                            numc_as_string = abap_true
                                            pretty_name    = ls_pretty ).
    lv_len = strlen( lv_send_json ).
    CALL METHOD lo_http_client->request->set_cdata
      EXPORTING
        data   = lv_send_json
        offset = 0
        length = lv_len.
    "发送
    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1                  " Communication Error
        http_invalid_state         = 2                  " Invalid state
        http_processing_failed     = 3                  " Error when processing method
        http_invalid_timeout       = 4                  " Invalid Time Entry
        OTHERS                     = 5.
    "接收
    CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.
    "获取返回的状态信息
    CLEAR:lv_code,lv_reason.
    CALL METHOD lo_http_client->response->get_status
      IMPORTING
        code   = lv_code
        reason = lv_reason.
    "获取返回的JSON
    CLEAR:lv_rec_json,ls_return.
    lv_rec_json = lo_http_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = lv_rec_json
        pretty_name = ls_pretty
      CHANGING
        data        = ls_return ).
    "关闭接口
    CALL METHOD lo_http_client->close.
    IF ls_return-code IS INITIAL.
      ev_type = 'E'.
      ev_message = 'TOKEN获取失败'.
    ELSE.
      ev_type = ls_return-code.
      ev_message = ls_return-message.
      ev_token = ls_return-data-token.
    ENDIF.

  ENDMETHOD.


  METHOD get_text_table.
**&   TABLE_NAME ------数据库的表名，必输
**&   INTAB_FIELD ------GS_OUT 对应的字段（空格分割）
**&   OUTTAB_FIELD ------返回文本表的字段（空格分割）
**&   LANGU_FIELD ------语言字段，如SPRAS
**&   OTHER_SQL ------其他SQL控制条件
    DATA: lc_error TYPE REF TO cx_root.
    TRY .

        DATA: dref_str    TYPE REF TO data,
              dref_tab    TYPE REF TO data,
              dref_i      TYPE REF TO data,
              itab_type   TYPE REF TO cl_abap_tabledescr,
              struct_type TYPE REF TO cl_abap_structdescr,
              elem_type   TYPE REF TO cl_abap_elemdescr,
              table_type  TYPE REF TO cl_abap_tabledescr,
              souce_type  TYPE REF TO cl_abap_typedescr,
              comp_tab    TYPE cl_abap_structdescr=>component_table,
              comp_descr  TYPE abap_componentdescr.
        FIELD-SYMBOLS :<fs_itab> TYPE ANY TABLE.

        DATA: lv_stq TYPE string.

        DATA: ls_field_in TYPE string,
              lt_field_in LIKE TABLE OF ls_field_in.
        DATA: ls_field_out TYPE string,
              lt_field_out LIKE TABLE OF ls_field_in.
        SPLIT intab_field AT ` ` INTO TABLE lt_field_in.
        SPLIT outtab_field AT ` ` INTO TABLE lt_field_out.


        struct_type ?= cl_abap_typedescr=>describe_by_data( object_struct ).
        comp_tab[] = struct_type->get_components( )."组成结构体的各个字段组件


        LOOP AT comp_tab INTO comp_descr WHERE as_include = 'X'. "递归INCLUDE结构
          struct_type ?= comp_descr-type.
          APPEND LINES OF struct_type->get_components( ) TO comp_tab[].
        ENDLOOP.

        DELETE comp_tab WHERE as_include = 'X'.

        LOOP AT comp_tab INTO comp_descr.
          READ TABLE lt_field_in INTO ls_field_in WITH KEY table_line = comp_descr-name. "删除其他组件
          IF sy-subrc NE 0.
            DELETE comp_tab.
            CONTINUE.
          ENDIF.
        ENDLOOP.
* 动态创建结构类型对象
        struct_type = cl_abap_structdescr=>create( comp_tab[] ).
        CREATE DATA dref_str TYPE HANDLE struct_type."

**=========动态创建内表
* 基于结构类型对象创建内表类型对象
        itab_type = cl_abap_tabledescr=>create( struct_type ).
        CREATE DATA dref_tab TYPE HANDLE itab_type.
        ASSIGN dref_tab->* TO <fs_itab>.


        DATA: lv_sql TYPE string. "查找的select语句

        "内表赋值排序，删除重复项
        <fs_itab> = CORRESPONDING #( object_table ).
        SORT <fs_itab>.
        DELETE ADJACENT DUPLICATES FROM <fs_itab>.

        CLEAR: text_table[].
        IF <fs_itab> IS NOT INITIAL.
          LOOP AT lt_field_in INTO ls_field_in.
            READ TABLE lt_field_out INTO ls_field_out INDEX sy-tabix.
            IF sy-subrc = 0.
              lv_sql = lv_sql && ` AND ` && ls_field_out && ` = <FS_ITAB>-` && ls_field_in.
            ELSE.
              lv_sql = lv_sql && ` AND ` && ls_field_in && ` = <FS_ITAB>-` && ls_field_in.
            ENDIF.
          ENDLOOP.
          IF langu_field IS NOT INITIAL.
            lv_sql = lv_sql && ` AND ` && langu_field && ` = '` && sy-langu && `'`.
          ENDIF.
          IF other_sql IS NOT INITIAL.
            lv_sql = lv_sql && ` ` && other_sql.
          ENDIF.
          "删除LV_SQL的第一个AND
          REPLACE FIRST OCCURRENCE OF 'AND' IN lv_sql WITH ''.
          "查找数据
          SELECT *
            INTO CORRESPONDING FIELDS OF TABLE text_table
            FROM (table_name)
            FOR ALL ENTRIES IN <fs_itab>
            WHERE (lv_sql).
        ENDIF.
        "排序，为了二分法
        DATA: lt_sort_field TYPE abap_sortorder_tab,
              ls_sort_field TYPE abap_sortorder.
        LOOP AT lt_field_out INTO ls_field_out.
          ls_sort_field-name = ls_field_out.
          ls_sort_field-descending = ''.
          APPEND ls_sort_field TO lt_sort_field.
        ENDLOOP.
        SORT text_table BY (lt_sort_field).

        CLEAR: <fs_itab>.
        FREE <fs_itab>.
        UNASSIGN <fs_itab>.

      CATCH cx_root INTO lc_error.
        DATA: out     TYPE REF TO cl_demo_text.
        DATA: text_line  TYPE cl_demo_text=>t_line.

        out = cl_demo_text=>new( ).
        " out = cl_demo_text=>GET_HANDLE( ). "一些老版本使用的代码

        text_line = '传入的数据库表数据库的表名:' && table_name.
        out->add_line( text_line ).
        out->add_line( ' ' ).

        out->add_line( '错误信息:' ).


        DATA(lv_error_msg) = lc_error->get_text( ).
        text_line = lv_error_msg.
        out->add_line( text_line ).
        out->display( ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_timestamp.
    DATA:lv_tstmp  TYPE timestampl,
         lv_tstmp2 TYPE tzntstmpl,
         lv_rtstmp TYPE tzntstmpl.
    lv_tstmp2 = '19700101000000'.
    GET TIME STAMP FIELD lv_tstmp.
    TRY.
        CALL METHOD cl_abap_tstmp=>subtract
          EXPORTING
            tstmp1 = lv_tstmp
            tstmp2 = lv_tstmp2
          RECEIVING
            r_secs = lv_rtstmp.
      CATCH cx_parameter_invalid_range .
      CATCH cx_parameter_invalid_type .
    ENDTRY.
    ev_timestamp = lv_rtstmp.
    ev_timestamp = ev_timestamp+0(10) && ev_timestamp+11(3)..
  ENDMETHOD.


  METHOD set_fieldcat.
    "动态字段名称赋值
    DATA: lo_tabdescr TYPE REF TO cl_abap_structdescr.
    DATA: ls_field_in TYPE dfies,
          lt_dfies    TYPE ddfields.
    DATA: lv_fieldname TYPE fieldname,
          lv_xml       TYPE string.

    lo_tabdescr ?= cl_abap_structdescr=>describe_by_data( im_object_struct ).
    CALL METHOD cl_salv_data_descr=>read_structdescr
      EXPORTING
        r_structdescr = lo_tabdescr
      RECEIVING
        t_dfies       = lt_dfies.

    lv_fieldname = im_fieldname.
    lv_xml        = im_xml.
    "转换大写
    TRANSLATE lv_fieldname TO UPPER CASE.
    TRANSLATE lv_xml TO UPPER CASE.

    DATA: BEGIN OF ls_parameter,
            attribute TYPE string,
            value     TYPE string,
          END OF ls_parameter,
          lt_parameter LIKE TABLE OF ls_parameter.
    DATA: lv_buffer TYPE xstring.

    CLEAR: lt_parameter[].

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_xml
      IMPORTING
        buffer = lv_buffer
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    DATA(reader) = cl_sxml_string_reader=>create( lv_buffer ).
    DO.
      DATA(node) = reader->read_next_node( ).
      IF node IS INITIAL.
        EXIT.
      ENDIF.
      IF node IS INSTANCE OF if_sxml_value_node.
        DATA(value_node) = CAST if_sxml_value_node( node ).
        ls_parameter-value = value_node->get_value( ).
        APPEND ls_parameter TO lt_parameter.
      ENDIF.
      IF node IS INSTANCE OF if_sxml_open_element.
        DATA(element_node) = CAST if_sxml_open_element( node ).
        CLEAR: ls_parameter.
        DATA(element)  = element_node->get_attributes( ).
        DATA(qname) = element_node->if_sxml_named~qname.
        ls_parameter-attribute = qname-name.
      ENDIF.
    ENDDO.


    FIELD-SYMBOLS <f_field> TYPE any.
    READ TABLE lt_dfies INTO ls_field_in WITH KEY fieldname = lv_fieldname.
    IF sy-subrc = 0.
      CLEAR: out_fieldcat.
      MOVE-CORRESPONDING ls_field_in TO out_fieldcat.

      out_fieldcat-coltext = im_coltext.
      LOOP AT lt_parameter INTO ls_parameter.
        ASSIGN COMPONENT ls_parameter-attribute OF STRUCTURE out_fieldcat TO <f_field>.
        IF <f_field> IS ASSIGNED.
          <f_field> = ls_parameter-value.
          UNASSIGN <f_field>.
        ENDIF.
      ENDLOOP.
      out_fieldcat-lowercase      = 'X'.

      IF out_fieldcat-ref_table IS INITIAL.
        out_fieldcat-ref_table = ls_field_in-tabname.   "参照类型的表,搜索帮助，域控制
      ENDIF.
      IF out_fieldcat-ref_field IS INITIAL.
        out_fieldcat-ref_field = ls_field_in-fieldname. "参照类型字段
      ENDIF.
      IF out_fieldcat-coltext IS INITIAL.         "当coltext不存在显示filedtext，如字段没域，只有手工描述的情况
        out_fieldcat-coltext = ls_field_in-fieldtext.
      ENDIF.
      IF out_fieldcat-outputlen IS INITIAL.         "默认长度
        out_fieldcat-outputlen = 40.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
