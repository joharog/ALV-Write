*&---------------------------------------------------------------------*
*& Include          ZMM_INVENTARIO_FISICO_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form data_val
*&---------------------------------------------------------------------*
FORM data_val.

  CLEAR: gv_msg, gv_ok.

  IF p_bukrs IS NOT INITIAL.
    SELECT SINGLE * FROM t001 WHERE bukrs EQ p_bukrs.
    IF sy-subrc NE 0.
      gv_msg = |Sociedad | & |{ p_bukrs }| & | no existe|.
    ELSE.
      IF p_werks IS NOT INITIAL.
        SELECT SINGLE * FROM t001w WHERE werks EQ p_werks.
        IF sy-subrc NE 0.
          gv_msg = |Centro | & |{ p_werks }| & | no existe|.
        ELSE.
          IF p_lgort IS NOT INITIAL.
            SELECT SINGLE * FROM t001l WHERE lgort EQ p_lgort AND werks EQ p_werks.
            IF sy-subrc NE 0.
              gv_msg = |No existe Almacén | & |{ p_lgort }| & | en el Centro | & |{ p_werks }|.
            ELSE.
              IF p_matnr IS NOT INITIAL.
                SELECT SINGLE * FROM mara WHERE matnr EQ p_matnr AND mtart EQ 'ZGAS'.
                IF sy-subrc NE 0.
                  DATA(lv_matnr) = |{ p_matnr ALPHA = OUT }|.
                  CONDENSE lv_matnr NO-GAPS.
                  gv_msg = |Material | & |{ lv_matnr }| & | no asignado al tipo ZGAS|.
                ELSE.
                  IF p_menge IS NOT INITIAL.
                    gv_ok = abap_true.
                  ELSE.
                    gv_msg = 'Rellene el campo obligatorio "Cantidad"'.
                  ENDIF.
                ENDIF.
              ELSE.
                gv_msg = 'Rellene el campo obligatorio "Material"'.
              ENDIF.
            ENDIF.
          ELSE.
            gv_msg = 'Rellene el campo obligatorio "Almacén"'.
          ENDIF.
        ENDIF.
      ELSE.
        gv_msg = 'Rellene el campo obligatorio "Centro"'.
      ENDIF.
    ENDIF.
  ELSE.
    gv_msg = 'Rellene el campo obligatorio "Sociedad"'.
  ENDIF.


  IF gv_ok EQ abap_false.
    MESSAGE gv_msg TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form save_mat
*&---------------------------------------------------------------------*
FORM save_mat.

  DATA: ls_inv TYPE ztmm_inv_fis_gas,
        lt_inv TYPE TABLE OF ztmm_inv_fis_gas.

  CLEAR: ls_inv.
  REFRESH lt_inv.

  IF gv_ok IS NOT INITIAL.

    ls_inv-bukrs = p_bukrs.
    ls_inv-monat = p_ersda+4(2).
    ls_inv-werks = p_werks.
    ls_inv-lgort = p_lgort.
    ls_inv-matnr = p_matnr.
    ls_inv-menge = p_menge.
*  ls_inv-meins = p_meins.
    ls_inv-ersda = p_ersda.
    APPEND ls_inv TO lt_inv.

    MODIFY ztmm_inv_fis_gas FROM TABLE lt_inv.
    COMMIT WORK AND WAIT.

    DATA(lv_matnr) = |{ p_matnr ALPHA = OUT }|.
    CONDENSE lv_matnr NO-GAPS.
    gv_msg = |Material | & |{ lv_matnr } | & |registrado en inventario.|.

    MESSAGE gv_msg TYPE 'I'.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
FORM get_data.

  REFRESH: gt_centros.
  DATA: rg_ersda TYPE RANGE OF ztmm_inv_fis_gas-ersda,
        rg_matnr TYPE RANGE OF ztmm_inv_fis_gas-matnr,
        rg_werks TYPE RANGE OF ztmm_inv_fis_gas-werks,
        rg_lgort TYPE RANGE OF ztmm_inv_fis_gas-lgort,
        rg_bukrs TYPE RANGE OF ztmm_inv_fis_gas-bukrs.

  IF p_ersda IS NOT INITIAL.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = p_ersda )
                    TO rg_ersda.
  ENDIF.

  IF p_matnr IS NOT INITIAL.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = p_matnr )
                    TO rg_matnr.
  ENDIF.

  IF p_werks IS NOT INITIAL.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = p_werks )
                    TO rg_werks.
  ENDIF.

  IF p_lgort IS NOT INITIAL.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = p_lgort )
                    TO rg_lgort.
  ENDIF.

  IF p_bukrs IS NOT INITIAL.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = p_bukrs )
                    TO rg_bukrs.
  ENDIF.

  SELECT * FROM ztmm_inv_fis_gas INTO TABLE gt_centros
    WHERE ersda IN rg_ersda
      AND matnr IN rg_matnr
      AND werks IN rg_werks
      AND lgort IN rg_lgort
      AND bukrs IN rg_bukrs.

  IF sy-subrc EQ 0.
    gv_show = abap_true.

  ELSE.
    gv_msg = |No se encontraron datos con parametros ingresados. | & |{ p_ersda+6(2) }.{ p_ersda+4(2) }.{ p_ersda(4) }|.
    MESSAGE gv_msg TYPE 'I'.
    EXIT.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form show_data
*&---------------------------------------------------------------------*
FORM show_data.

  DATA: lt_collect TYPE TABLE OF ztmm_inv_fis_gas,
        ls_collect TYPE ztmm_inv_fis_gas.

  REFRESH: lt_collect, gt_inv.

  IF gt_centros[] IS NOT INITIAL.

    SORT gt_centros BY werks.

    LOOP AT gt_centros INTO DATA(ls_centros).
      CLEAR: ls_centros-bukrs, ls_centros-monat, ls_centros-lgort,
             ls_centros-matnr, ls_centros-ersda.
      COLLECT ls_centros INTO lt_collect.
    ENDLOOP.

    LOOP AT lt_collect INTO ls_collect.
      DATA(fix_centros) = gt_centros.
      DELETE fix_centros WHERE werks NE ls_collect-werks.
      MOVE-CORRESPONDING fix_centros TO gs_inv-datos_c.
      gs_inv-centros = ls_collect-werks.
      gs_inv-total = ls_collect-menge.
      APPEND gs_inv TO gt_inv.
    ENDLOOP.

    CLEAR: ls_centros, ls_collect, gs_inv.

* Mostrar Cabecera
    WRITE:   / sy-uline(75),
             / sy-vline, 3  'Centro',
            10 sy-vline, 13 'Almacen',
            22 sy-vline, 26 'Material',
            37 sy-vline, 40 'Fecha Crea.',
            52 sy-vline, 60 'Cantidad',
            75 sy-vline,
             / sy-uline(75).

* Mostrar Posiciones
    LOOP AT gt_inv INTO gs_inv.
      gv_total += gs_inv-total.
      LOOP AT gs_inv-datos_c INTO DATA(ls_datos_c) WHERE werks EQ gs_inv-centros.
        WRITE: / sy-vline,   3 ls_datos_c-werks,
               10 sy-vline, 13 ls_datos_c-lgort,
               22 sy-vline, 26 ls_datos_c-matnr,
               37 sy-vline, 40 ls_datos_c-ersda,
               52 sy-vline, 60 ls_datos_c-menge,
               75 sy-vline.
      ENDLOOP.
      WRITE:    / sy-uline(75),
                / sy-vline, 3 gs_inv-centros,
               60 gs_inv-total,
               75 sy-vline,
                / sy-uline(75).
    ENDLOOP.

* Mostrar Total todo los Centros
    WRITE: / sy-uline(75),
           /   ' TOTAL', 59 gv_total.

  ENDIF.
ENDFORM.
