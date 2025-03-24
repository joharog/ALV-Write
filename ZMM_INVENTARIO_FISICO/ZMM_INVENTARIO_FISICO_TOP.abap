*&---------------------------------------------------------------------*
*& Include          ZMM_INVENTARIO_FISICO_TOP
*&---------------------------------------------------------------------*

TABLES: t001, t001w, t001l, mara, ekpo, sscrfields.


*----------------------------------------------------------------------*
*                     G L O B A L  D A T A
*----------------------------------------------------------------------*
DATA: gt_exclude TYPE TABLE OF sy-ucomm.

DATA: gt_inv     TYPE TABLE OF zstr_inv_fis WITH HEADER LINE,
      gs_inv     TYPE zstr_inv_fis,
      gt_centros TYPE TABLE OF ztmm_inv_fis_gas,
      gs_centro  TYPE ztmm_inv_fis_gas.


DATA: gv_msg   TYPE string,
      gv_ok    TYPE char1,
      gv_show  TYPE char1,
      gv_ucom  TYPE syucomm,
      gv_total TYPE menge_d,
      tsl      TYPE timestamp.

DATA: rg_bukrs TYPE RANGE OF t001-bukrs,
      rg_werks TYPE RANGE OF t001w-werks,
      rg_lgort TYPE RANGE OF t001l-lgort,
      rg_matnr TYPE RANGE OF mara-matnr.

DATA: rs_bukrs LIKE LINE OF rg_bukrs,
      rs_werks LIKE LINE OF rg_werks,
      rs_lgort LIKE LINE OF rg_lgort,
      rs_matnr LIKE LINE OF rg_matnr.


*----------------------------------------------------------------------*
*                         C O N S T A N T S
*----------------------------------------------------------------------*
CONSTANTS: i_check   TYPE char4 VALUE '@01@',
           i_display TYPE char4 VALUE '@10@',
           i_exit    TYPE char4 VALUE '@02@'.

DATA: smp_dyntxt TYPE smp_dyntxt.


*----------------------------------------------------------------------*
*                  S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------*
SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs LIKE t001-bukrs,
              p_werks LIKE t001w-werks,
              p_lgort LIKE t001l-lgort,
              p_matnr LIKE glspc-matnr,
              p_menge LIKE ekpo-menge,
              p_ersda LIKE mara-ersda  OBLIGATORY DEFAULT sy-datum.

  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN PUSHBUTTON /1(18) p_but1 USER-COMMAND but1.
  SELECTION-SCREEN PUSHBUTTON 25(18) p_but2 USER-COMMAND ONLI.
SELECTION-SCREEN: END OF BLOCK b1.

INITIALIZATION.
  p_but1 = |{ i_check }| & | AGREGAR |.
  p_but2 = |{ i_display }| & | VISUALIZAR |.
