class ZCL_DYNAMIC_OBJECT definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  class-data T_COMP type ABAP_COMPONENT_TAB .

  class-methods ADD_COMP_BY_TABLE_LINE
    importing
      !P_DATA type ANY TABLE
      !IV_NAME type C optional
      !IV_AS_INCLUDE type ABAP_BOOL optional
      !IV_SUFFIX type C optional .
  class-methods ADD_COMP_BY_DATA
    importing
      !P_DATA type ANY
      !IV_NAME type C optional
      !IV_AS_INCLUDE type ABAP_BOOL optional
      !IV_SUFFIX type C optional .
  class-methods ADD_COMP_BY_DATA_REF
    importing
      !P_DATA_REF type ref to DATA
      !IV_NAME type C optional
      !IV_AS_INCLUDE type ABAP_BOOL optional
      !IV_SUFFIX type C optional .
  class-methods ADD_COMP_BY_NAME
    importing
      !P_NAME type ANY
      !IV_NAME type C optional
      !IV_AS_INCLUDE type ABAP_BOOL optional
      !IV_SUFFIX type C optional .
  class-methods CREATE_STRUCT
    returning
      value(RO_DATA) type ref to DATA .
  class-methods CREATE_TABLE
    returning
      value(RO_DATA) type ref to DATA .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DYNAMIC_OBJECT IMPLEMENTATION.


  METHOD ADD_COMP_BY_DATA.
    DATA lw_comp LIKE LINE OF t_comp.

    lw_comp-type ?= cl_abap_typedescr=>describe_by_data(  p_data  = p_data ).
    lw_comp-name = iv_name.
    lw_comp-as_include = iv_as_include.
    lw_comp-suffix = iv_suffix.
    APPEND lw_comp TO t_comp.
  ENDMETHOD.


  METHOD ADD_COMP_BY_DATA_REF.
    DATA lw_comp LIKE LINE OF t_comp.

    lw_comp-type ?= cl_abap_typedescr=>describe_by_data_ref(  p_data_ref ).
    lw_comp-name = iv_name.
    lw_comp-as_include = iv_as_include.
    lw_comp-suffix = iv_suffix.
    APPEND lw_comp TO t_comp.
  ENDMETHOD.


  METHOD ADD_COMP_BY_NAME.
    DATA lw_comp LIKE LINE OF t_comp.

    lw_comp-type ?= cl_abap_typedescr=>describe_by_name(  p_name  = p_name ).
    lw_comp-name = iv_name.
    lw_comp-as_include = iv_as_include.
    lw_comp-suffix = iv_suffix.
    APPEND lw_comp TO t_comp.
  ENDMETHOD.


  METHOD ADD_COMP_BY_TABLE_LINE.
    DATA lw_comp LIKE LINE OF t_comp.
    DATA lo_wa TYPE REF TO data.
    FIELD-SYMBOLS <lfs_wa> TYPE any.

    CREATE DATA lo_wa LIKE LINE OF p_data.
    ASSIGN lo_wa->* TO <lfs_wa>.

    lw_comp-type ?= cl_abap_typedescr=>describe_by_data(  p_data  = <lfs_wa> ).
    lw_comp-name = iv_name.
    lw_comp-as_include = iv_as_include.
    lw_comp-suffix = iv_suffix.
    APPEND lw_comp TO t_comp.
  ENDMETHOD.


  METHOD CREATE_STRUCT.
    DATA lr_struct TYPE REF TO cl_abap_structdescr.

    lr_struct = cl_abap_structdescr=>create( p_components = t_comp ) .
    CREATE DATA ro_data TYPE HANDLE lr_struct.
    CLEAR t_comp.
  ENDMETHOD.


  METHOD CREATE_TABLE.
    DATA lr_table TYPE REF TO cl_abap_tabledescr.

    lr_table = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( p_components = t_comp p_strict = '' ) ).
    CREATE DATA ro_data TYPE HANDLE lr_table.
    CLEAR t_comp.
  ENDMETHOD.
ENDCLASS.
