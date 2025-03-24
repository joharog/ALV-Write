*&---------------------------------------------------------------------*
*& Report ZMM_INVENTARIO_FISICO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_inventario_fisico.

INCLUDE zmm_inventario_fisico_top.
INCLUDE zmm_inventario_fisico_cls.
INCLUDE zmm_inventario_fisico_f01.


*----------------------------------------------------------------------*
*                  A T  S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF sscrfields-ucomm EQ 'BUT1'.
    gv_ucom = sscrfields-ucomm.
  ELSEIF sscrfields-ucomm EQ 'ONLI'.
    gv_ucom = sscrfields-ucomm.
  ENDIF.

  IF gv_ucom EQ 'BUT1'.
    PERFORM data_val.
    PERFORM save_mat.
  ENDIF.

*----------------------------------------------------------------------*
*              S T A R T  -  O F  -  S E L E C T I O N                 *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF gv_ucom EQ 'ONLI'.
    PERFORM get_data.
    PERFORM show_data.
  ENDIF.
END-OF-SELECTION.
