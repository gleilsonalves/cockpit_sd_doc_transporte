*----------------------------------------------------------------------*
*       CLASS gcl_functions_act DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_functions_act DEFINITION.

  PUBLIC SECTION.

    METHODS m_etapa
      IMPORTING
        i_step TYPE c.

  PRIVATE SECTION.

    METHODS m_monta_bdc
      IMPORTING
        i_dynbegin TYPE bdcdata-dynbegin
        i_name     TYPE bdcdata-fnam
        i_value    TYPE bdcdata-fval.

    METHODS m_checa_etapa
      IMPORTING
        i_rows TYPE salv_t_row
        i_step TYPE c
      EXPORTING
        e_rows TYPE salv_t_row.

    METHODS m_gerar_log
      IMPORTING
        i_msg   TYPE bdcmsgcoll
        i_tknum TYPE vttk-tknum
        i_step  TYPE c.

ENDCLASS.                    "gcl_functions_act DEFINITION

*----------------------------------------------------------------------*
*       CLASS gcl_functions_act IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_functions_act IMPLEMENTATION.

  METHOD m_monta_bdc.

    IF i_dynbegin EQ 'X'.

      MOVE: i_name     TO w_bdcdata-program,
            i_value    TO w_bdcdata-dynpro,
            i_dynbegin TO w_bdcdata-dynbegin.

      APPEND w_bdcdata TO t_bdcdata.

    ELSE.

      MOVE: i_name  TO w_bdcdata-fnam,
            i_value TO w_bdcdata-fval.

      APPEND w_bdcdata TO t_bdcdata.

    ENDIF.

    CLEAR w_bdcdata.

  ENDMETHOD.                    "m_monta_bdc

  METHOD m_checa_etapa.

    DATA l_rows LIKE LINE OF i_rows.

    CLEAR e_rows[].

    IF i_step IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT i_rows INTO l_rows.

      CASE i_step.
        WHEN '1'.

          READ TABLE t_alv INTO w_alv INDEX l_rows.
          IF sy-subrc IS INITIAL.

            IF w_alv-sortl IS NOT INITIAL.
              CLEAR w_log.
              w_log-tknum = w_alv-tknum.
              w_log-datum = sy-datum.
              w_log-uname = sy-uname.
              w_log-uzeit = sy-uzeit.
              w_log-log   = text-001.
              APPEND w_log TO t_log.
            ELSE.
              APPEND l_rows TO e_rows.
            ENDIF.

          ENDIF.

        WHEN '2'.

          READ TABLE t_alv INTO w_alv INDEX l_rows.
          IF sy-subrc IS INITIAL.

            IF w_alv-vhilm IS NOT INITIAL.
              CLEAR w_log.
              w_log-tknum = w_alv-tknum.
              w_log-datum = sy-datum.
              w_log-uname = sy-uname.
              w_log-uzeit = sy-uzeit.
              w_log-log   = text-002.
              APPEND w_log TO t_log.
            ELSEIF w_alv-vhilm IS INITIAL AND w_alv-sortl IS NOT INITIAL.
              APPEND l_rows TO e_rows.
            ELSE.
              MESSAGE i003(zsd) WITH text-e00.
              RETURN.
            ENDIF.

          ENDIF.

        WHEN '3'.

          READ TABLE t_alv INTO w_alv INDEX l_rows.
          IF sy-subrc IS INITIAL.

            IF w_alv-check_embal IS NOT INITIAL.
              CLEAR w_log.
              w_log-tknum = w_alv-tknum.
              w_log-datum = sy-datum.
              w_log-uname = sy-uname.
              w_log-uzeit = sy-uzeit.
              w_log-log   = text-003.
              APPEND w_log TO t_log.
            ELSEIF w_alv-check_embal IS INITIAL AND ( w_alv-vhilm IS NOT INITIAL AND
                                                      w_alv-sortl IS NOT INITIAL ).
              APPEND l_rows TO e_rows.
            ELSE.
              MESSAGE i003(zsd) WITH text-e00.
              RETURN.
            ENDIF.

          ENDIF.

        WHEN '4'.

          READ TABLE t_alv INTO w_alv INDEX l_rows.
          IF sy-subrc IS INITIAL.

            IF w_alv-stdis IS NOT INITIAL.
              CLEAR w_log.
              w_log-tknum = w_alv-tknum.
              w_log-datum = sy-datum.
              w_log-uname = sy-uname.
              w_log-uzeit = sy-uzeit.
              w_log-log   = text-004.
              APPEND w_log TO t_log.
            ELSEIF w_alv-stdis IS INITIAL AND ( w_alv-check_embal IS NOT INITIAL AND
                                                w_alv-vhilm IS NOT INITIAL AND
                                                w_alv-sortl IS NOT INITIAL ).
              APPEND l_rows TO e_rows.
            ELSE.
              MESSAGE i003(zsd) WITH text-e00.
              RETURN.
            ENDIF.

          ENDIF.

        WHEN '5'.

          READ TABLE t_alv INTO w_alv INDEX l_rows.
          IF sy-subrc IS INITIAL.

            IF w_alv-streg IS NOT INITIAL.
              CLEAR w_log.
              w_log-tknum = w_alv-tknum.
              w_log-datum = sy-datum.
              w_log-uname = sy-uname.
              w_log-uzeit = sy-uzeit.
              w_log-log   = text-005.
              APPEND w_log TO t_log.
            ELSEIF w_alv-streg IS INITIAL AND ( w_alv-stdis IS NOT INITIAL AND
                                                w_alv-check_embal IS NOT INITIAL AND
                                                w_alv-vhilm IS NOT INITIAL AND
                                                w_alv-sortl IS NOT INITIAL ).
              APPEND l_rows TO e_rows.
            ELSE.
              MESSAGE i003(zsd) WITH text-e00.
              RETURN.
            ENDIF.

          ENDIF.

        WHEN '6'.

          READ TABLE t_alv INTO w_alv INDEX l_rows.
          IF sy-subrc IS INITIAL.

            IF w_alv-stlbg IS NOT INITIAL.
              CLEAR w_log.
              w_log-tknum = w_alv-tknum.
              w_log-datum = sy-datum.
              w_log-uname = sy-uname.
              w_log-uzeit = sy-uzeit.
              w_log-log   = text-006.
              APPEND w_log TO t_log.
            ELSEIF w_alv-stlbg IS INITIAL AND ( w_alv-streg IS NOT INITIAL AND
                                                w_alv-stdis IS NOT INITIAL AND
                                                w_alv-check_embal IS NOT INITIAL AND
                                                w_alv-vhilm IS NOT INITIAL AND
                                                w_alv-sortl IS NOT INITIAL ).
              APPEND l_rows TO e_rows.
            ELSE.
              MESSAGE i003(zsd) WITH text-e00.
              RETURN.
            ENDIF.

          ENDIF.

        WHEN '7'.

          READ TABLE t_alv INTO w_alv INDEX l_rows.
          IF sy-subrc IS INITIAL.

            IF w_alv-stlad IS NOT INITIAL AND w_alv-status_pick = 'Completo'.
              CLEAR w_log.
              w_log-tknum = w_alv-tknum.
              w_log-datum = sy-datum.
              w_log-uname = sy-uname.
              w_log-uzeit = sy-uzeit.
              w_log-log   = text-007.
              APPEND w_log TO t_log.
            ELSEIF w_alv-status_pick = 'Pendente'.
              CLEAR w_log.
              w_log-tknum = w_alv-tknum.
              w_log-datum = sy-datum.
              w_log-uname = sy-uname.
              w_log-uzeit = sy-uzeit.
              w_log-log   = text-010.
              APPEND w_log TO t_log.
            ELSEIF w_alv-status_pick = 'Completo' AND ( w_alv-stlbg IS NOT INITIAL AND
                                                        w_alv-streg IS NOT INITIAL AND
                                                        w_alv-stdis IS NOT INITIAL AND
                                                        w_alv-check_embal IS NOT INITIAL AND
                                                        w_alv-vhilm IS NOT INITIAL AND
                                                        w_alv-sortl IS NOT INITIAL ).
              APPEND l_rows TO e_rows.
            ENDIF.

          ENDIF.

        WHEN '8'.

          READ TABLE t_alv INTO w_alv INDEX l_rows.
          IF sy-subrc IS INITIAL.

            IF w_alv-stabf IS NOT INITIAL.
              CLEAR w_log.
              w_log-tknum = w_alv-tknum.
              w_log-datum = sy-datum.
              w_log-uname = sy-uname.
              w_log-uzeit = sy-uzeit.
              w_log-log   = text-008.
              APPEND w_log TO t_log.
            ELSEIF w_alv-stabf IS INITIAL AND ( w_alv-stlad IS NOT INITIAL AND
                                                w_alv-stlbg IS NOT INITIAL AND
                                                w_alv-streg IS NOT INITIAL AND
                                                w_alv-stdis IS NOT INITIAL AND
                                                w_alv-check_embal IS NOT INITIAL AND
                                                w_alv-vhilm IS NOT INITIAL AND
                                                w_alv-sortl IS NOT INITIAL ).
              APPEND l_rows TO e_rows.
            ELSE.
              MESSAGE i003(zsd) WITH text-e00.
              RETURN.
            ENDIF.

          ENDIF.

        WHEN '9'.

          READ TABLE t_alv INTO w_alv INDEX l_rows.
          IF sy-subrc IS INITIAL.

            IF w_alv-sttbg IS NOT INITIAL.
              CLEAR w_log.
              w_log-tknum = w_alv-tknum.
              w_log-datum = sy-datum.
              w_log-uname = sy-uname.
              w_log-uzeit = sy-uzeit.
              w_log-log   = text-009.
              APPEND w_log TO t_log.
            ELSEIF w_alv-sttbg IS INITIAL AND ( w_alv-stabf IS NOT INITIAL AND
                                                w_alv-stlad IS NOT INITIAL AND
                                                w_alv-stlbg IS NOT INITIAL AND
                                                w_alv-streg IS NOT INITIAL AND
                                                w_alv-stdis IS NOT INITIAL AND
                                                w_alv-check_embal IS NOT INITIAL AND
                                                w_alv-vhilm IS NOT INITIAL AND
                                                w_alv-sortl IS NOT INITIAL ).
              APPEND l_rows TO e_rows.
            ELSE.
              MESSAGE i003(zsd) WITH text-e00.
              RETURN.
            ENDIF.

          ENDIF.

      ENDCASE.

    ENDLOOP.

    IF t_log[] IS NOT INITIAL.

      MODIFY ztsd_log_cockpit FROM TABLE t_log[].

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.

  ENDMETHOD.                    "m_checa_etapa

  METHOD m_etapa.

    DATA: lt_rows       TYPE salv_t_row,
          lt_rows_aux   TYPE salv_t_row,
          l_rows        LIKE LINE OF lt_rows,
          lv_value(132) TYPE c,
          lv_mode       TYPE c VALUE 'N',
          lv_updat      TYPE c VALUE 'S',
          lv_counter    TYPE i,
          lv_index(2)   TYPE c,
          lv_bdc_field  TYPE bdcdata-fnam,
          lv_del_aux    TYPE c,
          lv_exc_step3  TYPE c.

    DATA: lt_tvarvc TYPE TABLE OF bal_rfield,
          lw_tvarvc TYPE bal_rfield,
          lr_tplst  TYPE RANGE OF vttk-tplst,
          lw_tplst  LIKE LINE OF lr_tplst.

    CONSTANTS: lc_vt02n TYPE sy-tcode VALUE 'VT02N'.

    gref_selection = gref_gr_alv->get_selections( ).
    lt_rows = gref_selection->get_selected_rows( ).

    CLEAR: lr_tplst[], lv_del_aux, lv_exc_step3.

    CASE i_step.
      WHEN '1'.

        CALL SCREEN 0100 STARTING AT 50 10.

        IF gv_tdlnr IS NOT INITIAL.

          me->m_checa_etapa( EXPORTING i_rows = lt_rows[]
                                       i_step = i_step
                             IMPORTING e_rows = lt_rows_aux[] ).
        ELSE.

          RETURN.

        ENDIF.

      WHEN '2'.

        CALL SCREEN 0200 STARTING AT 50 10.

        IF gv_tpveiculo IS NOT INITIAL.

          me->m_checa_etapa( EXPORTING i_rows = lt_rows[]
                                       i_step = i_step
                             IMPORTING e_rows = lt_rows_aux[] ).
        ELSE.

          RETURN.

        ENDIF.

      WHEN '5' OR '7'.

        SELECT sign opti low high
          FROM tvarvc
          INTO TABLE lt_tvarvc
          WHERE name = 'Z_SD_COCKPIT_LC_TRANSP'
            AND type = 'S'.

        IF sy-subrc IS INITIAL.

          LOOP AT lt_tvarvc INTO lw_tvarvc.

            lw_tplst-sign   = lw_tvarvc-sign.
            lw_tplst-option = lw_tvarvc-option.
            lw_tplst-low    = lw_tvarvc-low.
            lw_tplst-high   = lw_tvarvc-high.
            APPEND lw_tplst TO lr_tplst.
            CLEAR lw_tplst.

          ENDLOOP.

        ENDIF.

        me->m_checa_etapa( EXPORTING i_rows = lt_rows[]
                                     i_step = i_step
                           IMPORTING e_rows = lt_rows_aux[] ).

        IF lt_rows_aux[] IS NOT INITIAL.

          CLEAR lv_del_aux.
          LOOP AT lt_rows_aux INTO l_rows.

            READ TABLE t_alv INTO w_alv INDEX l_rows.

            CHECK sy-subrc IS INITIAL.

            READ TABLE t_vttk INTO w_vttk WITH KEY tknum = w_alv-tknum BINARY SEARCH.

            CHECK sy-subrc IS INITIAL.

            IF w_vttk-tplst IN lr_tplst.

              CASE i_step.
                WHEN '5'.

                  IF gv_motor IS INITIAL AND gv_placa IS INITIAL.

                    CALL SCREEN 0500 STARTING AT 50 10.

                    IF gv_motor IS INITIAL AND gv_placa IS INITIAL.

                      lv_del_aux = abap_true.
                      EXIT.

                    ENDIF.

                  ENDIF.

                WHEN '7'.

                  IF gv_carreg_por IS INITIAL.

                    CALL SCREEN 0700 STARTING AT 50 10.

                    IF gv_carreg_por IS INITIAL.

                      lv_del_aux = abap_true.
                      EXIT.

                    ENDIF.

                  ENDIF.

              ENDCASE.

            ELSE.

              CONTINUE.

            ENDIF.

          ENDLOOP.

          IF lv_del_aux IS NOT INITIAL.
            CLEAR lt_rows_aux[].
          ENDIF.

        ENDIF.

      WHEN OTHERS.

        me->m_checa_etapa( EXPORTING i_rows = lt_rows[]
                                     i_step = i_step
                           IMPORTING e_rows = lt_rows_aux[] ).

    ENDCASE.

    IF lt_rows_aux[] IS NOT INITIAL.

      CLEAR lt_rows[].
      lt_rows[] = lt_rows_aux[].

    ELSE.

      MESSAGE e050(zsd) DISPLAY LIKE 'I'.
      RETURN.

    ENDIF.

    LOOP AT lt_rows INTO l_rows.

      READ TABLE t_alv INTO w_alv INDEX l_rows.

      CHECK sy-subrc IS INITIAL.

      CASE i_step.
        WHEN '1'.

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1011' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TKNUM' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_TKAL' ).
          CLEAR lv_value.
          lv_value = w_alv-tknum.
          me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TKNUM' i_value = lv_value ).

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '/00' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).
          CLEAR lv_value.
          lv_value = gv_tdlnr.
          me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TDLNR' i_value = lv_value ).

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_SICH' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

        WHEN '2'.

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1011' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TKNUM' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_TKAL' ).
          CLEAR lv_value.
          lv_value = w_alv-tknum.
          me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TKNUM' i_value = lv_value ).

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_VSEB' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPLV51G' i_value = '6000' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=SICH' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPLV51G                                6010TAB' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'V51VE-BRGEW(01)' ).
          CLEAR lv_value.
          lv_value = gv_tpveiculo.
          me->m_monta_bdc( i_dynbegin = space i_name = 'V51VE-VHILM(01)' i_value = lv_value ).

        WHEN '3'.

          READ TABLE t_vttp WITH KEY tknum = w_alv-tknum TRANSPORTING NO FIELDS BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            CLEAR lv_counter.
            LOOP AT t_vttp INTO w_vttp FROM sy-tabix.

              IF w_vttp-tknum <> w_alv-tknum.
                EXIT.
              ENDIF.

              READ TABLE t_lips WITH KEY vbeln = w_vttp-vbeln TRANSPORTING NO FIELDS BINARY SEARCH.
              IF sy-subrc IS INITIAL.

                LOOP AT t_lips INTO w_lips FROM sy-tabix.

                  IF w_lips-vbeln <> w_vttp-vbeln.
                    EXIT.
                  ENDIF.

                  READ TABLE t_vepo WITH KEY vbeln = w_lips-vbeln
                                             posnr = w_lips-posnr
                                             TRANSPORTING NO FIELDS
                                             BINARY SEARCH.
                  IF sy-subrc IS NOT INITIAL.
                    ADD 1 TO lv_counter.
                  ENDIF.

                ENDLOOP.

              ENDIF.

            ENDLOOP.

          ENDIF.

          CHECK lv_counter > 0.

          IF lv_counter > 10.

            lv_exc_step3 = abap_true.

            DO.

              IF lv_counter LE 0.
                EXIT.
              ENDIF.

              me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1011' ).
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TKNUM' ).
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_TKAL' ).
              CLEAR lv_value.
              lv_value = w_alv-tknum.
              me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TKNUM' i_value = lv_value ).

              me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_VSEB' ).
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

              me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPLV51G' i_value = '6000' ).
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=HU_MARKA' ).
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPLV51G                                6010TAB').
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'V51VE-EXIDV(01)' ).
              me->m_monta_bdc( i_dynbegin = space i_name = 'V51VE-SELKZ(01)' i_value = 'X' ).

              me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPLV51G' i_value = '6000' ).
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=HU_VERP' ).
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPLV51G                                6010TAB').
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'V51VE-EXIDV(01)' ).

              me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPLV51G' i_value = '6000' ).
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=SICH' ).
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPLV51G                                6010TAB').
              me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'V51VE-EXIDV(01)' ).

              CALL TRANSACTION lc_vt02n USING t_bdcdata
                                         MODE lv_mode
                                       UPDATE lv_updat
                                MESSAGES INTO t_msg.

              SUBTRACT 11 FROM lv_counter.
              CLEAR t_bdcdata[].

            ENDDO.

          ELSE.

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1011' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TKNUM' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_TKAL' ).
            CLEAR lv_value.
            lv_value = w_alv-tknum.
            me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TKNUM' i_value = lv_value ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_VSEB' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPLV51G' i_value = '6000' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=HU_VERP' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPLV51G                                6010TAB').
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'V51VP-MATNR(01)' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'V51VE-SELKZ(01)' i_value = 'X' ).

            CLEAR lv_index.
            DO lv_counter TIMES.

              lv_index = lv_index + '01'.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = lv_index
                IMPORTING
                  output = lv_index.

              CONCATENATE 'V51VP-SELKZ(' lv_index ')' INTO lv_bdc_field.
              me->m_monta_bdc( i_dynbegin = space i_name = lv_bdc_field i_value = 'X' ).

            ENDDO.

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPLV51G' i_value = '6000' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=SICH' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPLV51G                                6010TAB' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'V51VP-MATNR(01)' ).

          ENDIF.

        WHEN '4'.

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1011' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TKNUM' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_TKAL' ).
          CLEAR lv_value.
          lv_value = w_alv-tknum.
          me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TKNUM' i_value = lv_value ).

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_ST01' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_SICH' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

        WHEN '5'.

          READ TABLE t_vttk INTO w_vttk WITH KEY tknum = w_alv-tknum BINARY SEARCH.

          CHECK sy-subrc IS INITIAL.

          IF w_vttk-tplst IN lr_tplst.

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1011' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TKNUM' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_TKAL' ).
            CLEAR lv_value.
            lv_value = w_alv-tknum.
            me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TKNUM' i_value = lv_value ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_ST02' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPLV56F' i_value = '0100' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '/EEXIT' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-ADD02' ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_ST02' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPLV56F' i_value = '0100' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=ONLI' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VEKP-NAMBE' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-ADD02' i_value = 'NORMAL' ).
            CLEAR lv_value.
            lv_value = gv_placa.
            me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TEXT1' i_value = lv_value ).
            CLEAR lv_value.
            lv_value = gv_motor.
            me->m_monta_bdc( i_dynbegin = space i_name = 'VEKP-NAMEF' i_value = lv_value ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'VEKP-NAMBE' i_value = '*' ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_SICH' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

          ELSE.

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1011' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TKNUM' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_TKAL' ).
            CLEAR lv_value.
            lv_value = w_alv-tknum.
            me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TKNUM' i_value = lv_value ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_ST02' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_SICH' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

          ENDIF.

        WHEN '6'.

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1011' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TKNUM' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_TKAL' ).
          CLEAR lv_value.
          lv_value = w_alv-tknum.
          me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TKNUM' i_value = lv_value ).

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_ST03' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_SICH' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

        WHEN '7'.

          READ TABLE t_vttk INTO w_vttk WITH KEY tknum = w_alv-tknum BINARY SEARCH.

          CHECK sy-subrc IS INITIAL.

          IF w_vttk-tplst IN lr_tplst.

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1011' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TKNUM' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_TKAL' ).
            CLEAR lv_value.
            lv_value = w_alv-tknum.
            me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TKNUM' i_value = lv_value ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_ST04' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPLV56F' i_value = '0100' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=ONLI' ).
            CLEAR lv_value.
            lv_value = gv_carreg_por.
            me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TEXT2' i_value = lv_value ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_SICH' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

          ELSE.

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1011' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TKNUM' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_TKAL' ).
            CLEAR lv_value.
            lv_value = w_alv-tknum.
            me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TKNUM' i_value = lv_value ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_ST04' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

            me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_SICH' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
            me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

          ENDIF.

        WHEN '8'.

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1011' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TKNUM' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_TKAL' ).
          CLEAR lv_value.
          lv_value = w_alv-tknum.
          me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TKNUM' i_value = lv_value ).

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_ST05' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_SICH' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

        WHEN '9'.

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1011' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TKNUM' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_TKAL' ).
          CLEAR lv_value.
          lv_value = w_alv-tknum.
          me->m_monta_bdc( i_dynbegin = space i_name = 'VTTK-TKNUM' i_value = lv_value ).

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_ST06' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

          me->m_monta_bdc( i_dynbegin = abap_true i_name = 'SAPMV56A' i_value = '1020' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_OKCODE' i_value = '=MM_SICH' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1021G_HEADER_SUBSCREEN1' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_CURSOR' i_value = 'VTTK-TDLNR' ).
          me->m_monta_bdc( i_dynbegin = space i_name = 'BDC_SUBSCR' i_value = 'SAPMV56A                                1025G_HEADER_SUBSCREEN2' ).

      ENDCASE.

      IF lv_exc_step3 IS INITIAL.

        CALL TRANSACTION lc_vt02n USING t_bdcdata
                                   MODE lv_mode
                                 UPDATE lv_updat
                          MESSAGES INTO t_msg.

      ENDIF.

      SORT t_msg BY msgid msgnr msgtyp.
      READ TABLE t_msg INTO w_msg WITH KEY msgid  = 'VW'
                                           msgnr  = '006'
                                           msgtyp = 'S'
                                           BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        CASE i_step.
          WHEN '1'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = gv_tdlnr
              IMPORTING
                output = gv_tdlnr.

            SELECT SINGLE sortl
              FROM lfa1
              INTO w_alv-sortl
              WHERE lifnr = gv_tdlnr.

            IF sy-subrc IS INITIAL.
              MODIFY t_alv FROM w_alv INDEX l_rows.
            ENDIF.
          WHEN '2'.
            w_alv-vhilm = gv_tpveiculo.
            MODIFY t_alv FROM w_alv INDEX l_rows.
          WHEN '3'.
            w_alv-check_embal = 'Embalado'.
            MODIFY t_alv FROM w_alv INDEX l_rows.
          WHEN '4'.
            w_alv-stdis = abap_true.
            MODIFY t_alv FROM w_alv INDEX l_rows.
          WHEN '5'.
            w_alv-streg = abap_true.
            MODIFY t_alv FROM w_alv INDEX l_rows.
          WHEN '6'.
            w_alv-stlbg = abap_true.
            MODIFY t_alv FROM w_alv INDEX l_rows.
          WHEN '7'.
            w_alv-stlad = abap_true.
            MODIFY t_alv FROM w_alv INDEX l_rows.
          WHEN '8'.
            w_alv-stabf = abap_true.
            MODIFY t_alv FROM w_alv INDEX l_rows.
          WHEN '9'.
            w_alv-sttbg = abap_true.
            MODIFY t_alv FROM w_alv INDEX l_rows.
        ENDCASE.

        me->m_gerar_log( i_msg = w_msg i_tknum = w_alv-tknum i_step = i_step ).

      ELSE.

        LOOP AT t_msg INTO w_msg.

          me->m_gerar_log( i_msg = w_msg i_tknum = w_alv-tknum i_step = i_step ).

        ENDLOOP.

      ENDIF.

      IF t_log[] IS NOT INITIAL.

        MODIFY ztsd_log_cockpit FROM TABLE t_log.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

      ENDIF.

      CLEAR: t_bdcdata[], t_msg[].

    ENDLOOP.

    MESSAGE i003(zsd) WITH text-w00.

    gref_gr_alv->refresh( ).

  ENDMETHOD.                    "m_etapa

  METHOD m_gerar_log.

    DATA: lv_msgno TYPE sy-msgno,
          lw_mensg TYPE message.

    WAIT UP TO 1 SECONDS.

    lv_msgno = i_msg-msgnr.

    CALL FUNCTION 'WRITE_MESSAGE'
      EXPORTING
        msgid = i_msg-msgid
        msgno = lv_msgno
        msgty = i_msg-msgtyp
        msgv1 = i_msg-msgv1
        msgv2 = i_msg-msgv2
        msgv3 = i_msg-msgv3
        msgv4 = i_msg-msgv4
        msgv5 = space
      IMPORTING
        messg = lw_mensg.

    CLEAR w_log.
    w_log-tknum = i_tknum.
    w_log-datum = sy-datum.
    w_log-uname = sy-uname.
    w_log-uzeit = sy-uzeit.

    IF i_msg-msgtyp EQ 'S' AND i_msg-msgid = 'VW' AND i_msg-msgnr = '006'.

      CASE i_step.
        WHEN '1'.
          CONCATENATE 'Transportadora definida com sucesso:' lw_mensg-msgtx INTO w_log-log SEPARATED BY space.
        WHEN '2'.
          CONCATENATE 'Tipo de veículo definido com sucesso:' lw_mensg-msgtx INTO w_log-log SEPARATED BY space.
        WHEN '3'.
          CONCATENATE 'Embalar realizado com sucesso:' lw_mensg-msgtx INTO w_log-log SEPARATED BY space.
        WHEN '4'.
          CONCATENATE 'Organizar transporte realizado com sucesso:' lw_mensg-msgtx INTO w_log-log SEPARATED BY space.
        WHEN '5'.
          CONCATENATE 'Registro realizado com sucesso:' lw_mensg-msgtx INTO w_log-log SEPARATED BY space.
        WHEN '6'.
          CONCATENATE 'Inicio do carregamento realizado com sucesso:' lw_mensg-msgtx INTO w_log-log SEPARATED BY space.
        WHEN '7'.
          CONCATENATE 'Fim do carregamento realizado com sucesso:' lw_mensg-msgtx INTO w_log-log SEPARATED BY space.
        WHEN '8'.
          CONCATENATE 'Precessamento transporte realizado com sucesso:' lw_mensg-msgtx INTO w_log-log SEPARATED BY space.
        WHEN '9'.
          CONCATENATE 'Inicio transporte realizado com sucesso:' lw_mensg-msgtx INTO w_log-log SEPARATED BY space.
      ENDCASE.

    ELSE.
      w_log-log = lw_mensg-msgtx.
    ENDIF.

    APPEND w_log TO t_log.

  ENDMETHOD.                    "m_gerar_log

ENDCLASS.                    "gcl_functions_act IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS gcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS on_added_function FOR EVENT if_salv_events_functions~added_function
                                           OF cl_salv_events_table IMPORTING e_salv_function.

ENDCLASS.                    "gcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS gcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_event_handler IMPLEMENTATION.

  METHOD on_added_function.

    DATA lo_functions TYPE REF TO gcl_functions_act.

    CREATE OBJECT lo_functions.
    CLEAR: t_bdcdata[], t_msg[], t_log[].

    CASE e_salv_function.
      WHEN 'TRANSP'. "Transportadora etapa 1

        lo_functions->m_etapa( i_step = '1' ).

      WHEN 'TPVEIC'. "Tipo do Veículo etapa 2

        lo_functions->m_etapa( i_step = '2' ).

      WHEN 'EMBALAR'. "Embalar etapa 3

        lo_functions->m_etapa( i_step = '3' ).

      WHEN 'ORGTRANS'. "Organizar Transp etapa 4

        lo_functions->m_etapa( i_step = '4' ).

      WHEN 'REGISTRO'. "Registro etapa 5

        lo_functions->m_etapa( i_step = '5' ).

      WHEN 'INICARREG'. "Início Carregamento etapa 6

        lo_functions->m_etapa( i_step = '6' ).

      WHEN 'FIMCARREG'. "Fim Carregamento etapa 7

        lo_functions->m_etapa( i_step = '7' ).

      WHEN 'PRCTRANS'. "Processamento para Transporte etapa 8

        lo_functions->m_etapa( i_step = '8' ).

      WHEN 'INITRANS'. "Início de Transporte etapa 9

        lo_functions->m_etapa( i_step = '9' ).

    ENDCASE.

    IF t_log[] IS NOT INITIAL.

      MODIFY ztsd_log_cockpit FROM TABLE t_log.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.

  ENDMETHOD.                    "on_added_function

ENDCLASS.                    "gcl_event_handler IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS gcl_cockpit_doc_transp DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_cockpit_doc_transp DEFINITION.

  PUBLIC SECTION.

    TYPES: r_werks TYPE RANGE OF t001w-werks,
           r_tknum TYPE RANGE OF vttk-tknum,
           r_erdat TYPE RANGE OF vttk-erdat.

    METHODS m_seleciona_dados
      IMPORTING
        i_werks TYPE r_werks
        i_tknum TYPE r_tknum
        i_erdat TYPE r_erdat.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS: m_monta_dados_alv,
             m_chama_alv.

ENDCLASS.                    "gcl_cockpit_doc_transp DEFINITION

*----------------------------------------------------------------------*
*       CLASS gcl_cockpit_doc_transp IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_cockpit_doc_transp IMPLEMENTATION.

  METHOD m_seleciona_dados.

    DATA: lr_shtyp  TYPE RANGE OF vttk-shtyp,
          lw_shtyp  LIKE LINE OF lr_shtyp,
          lt_tvarvc TYPE TABLE OF bal_rfield,
          lw_tvarvc TYPE bal_rfield,
          lw_vttk   LIKE LINE OF t_vttk.

    SELECT tplst
      FROM tvfp
      INTO TABLE t_tvfp
      WHERE werks IN i_werks.

    IF sy-subrc IS INITIAL.

      SELECT sign opti low high
        FROM tvarvc
        INTO TABLE lt_tvarvc
        WHERE name = 'Z_SD_COCKPIT_TPTRANS'
          AND type = 'S'.

      IF sy-subrc IS INITIAL.

        LOOP AT lt_tvarvc INTO lw_tvarvc.

          lw_shtyp-sign   = lw_tvarvc-sign.
          lw_shtyp-option = lw_tvarvc-option.
          lw_shtyp-low    = lw_tvarvc-low.
          lw_shtyp-high   = lw_tvarvc-high.
          APPEND lw_shtyp TO lr_shtyp.
          CLEAR lw_shtyp.

        ENDLOOP.

      ENDIF.

      IF lr_shtyp[] IS NOT INITIAL.

        SELECT tknum tplst stdis streg stlbg stlad stabf sttbg tdlnr
          FROM vttk
          INTO TABLE t_vttk
          FOR ALL ENTRIES IN t_tvfp
          WHERE vbtyp = '8'
            AND tknum IN i_tknum
            AND erdat IN i_erdat
            AND shtyp IN lr_shtyp
            AND tplst = t_tvfp-tplst.

        IF sy-subrc IS INITIAL.

          SORT t_vttk BY tknum.

          LOOP AT t_vttk INTO lw_vttk.

            lw_vttk-tknum2 = lw_vttk-tknum.
            MODIFY t_vttk FROM lw_vttk INDEX sy-tabix.

          ENDLOOP.

          SELECT lifnr sortl
            FROM lfa1
            INTO TABLE t_lfa1
            FOR ALL ENTRIES IN t_vttk
            WHERE lifnr = t_vttk-tdlnr.

          IF sy-subrc IS INITIAL.
            SORT t_lfa1 BY lifnr.
          ENDIF.

          SELECT venum vpobjkey brgew vhilm
            FROM vekp
            INTO TABLE t_vekp
            FOR ALL ENTRIES IN t_vttk
            WHERE vpobj    = '04'
              AND vpobjkey = t_vttk-tknum2.

          IF sy-subrc IS INITIAL.

            SORT t_vekp BY vpobjkey.

            SELECT venum vbeln posnr
              FROM vepo
              INTO TABLE t_vepo
              FOR ALL ENTRIES IN t_vekp
              WHERE venum = t_vekp-venum.

            IF sy-subrc IS INITIAL.
              SORT t_vepo BY vbeln posnr.
            ENDIF.

          ENDIF.

          SELECT tknum vbeln
            FROM vttp
            INTO TABLE t_vttp
            FOR ALL ENTRIES IN t_vttk
            WHERE tknum = t_vttk-tknum.

          IF sy-subrc IS INITIAL.

            SORT t_vttp BY tknum.

            SELECT vbeln kostk
              FROM vbuk
              INTO TABLE t_vbuk
              FOR ALL ENTRIES IN t_vttp
              WHERE vbeln = t_vttp-vbeln.

            IF sy-subrc IS INITIAL.
              SORT t_vbuk BY vbeln.
            ENDIF.

            SELECT vbeln posnr
              FROM lips
              INTO TABLE t_lips
              FOR ALL ENTRIES IN t_vttp
              WHERE vbeln = t_vttp-vbeln.

            IF sy-subrc IS INITIAL.
              SORT t_lips BY vbeln.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ELSE.

      MESSAGE e005(zsd) DISPLAY LIKE 'I'.
      RETURN.

    ENDIF.

    me->m_monta_dados_alv( ).

  ENDMETHOD.                    "m_seleciona_dados

  METHOD m_monta_dados_alv.

    DATA: lv_nok TYPE c,
          lv_complete TYPE c.

    LOOP AT t_vttk INTO w_vttk.

      w_alv-tknum = w_vttk-tknum.
      w_alv-stdis = w_vttk-stdis.
      w_alv-streg = w_vttk-streg.
      w_alv-stlad = w_vttk-stlad.
      w_alv-stlbg = w_vttk-stlbg.
      w_alv-stabf = w_vttk-stabf.
      w_alv-stabf = w_vttk-stabf.
      w_alv-sttbg = w_vttk-sttbg.

      READ TABLE t_vekp INTO w_vekp WITH KEY vpobjkey = w_vttk-tknum2 BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        w_alv-brgew = w_vekp-brgew.
        w_alv-vhilm = w_vekp-vhilm.
      ENDIF.

      READ TABLE t_lfa1 INTO w_lfa1 WITH KEY lifnr = w_vttk-tdlnr BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        w_alv-sortl = w_lfa1-sortl.
      ENDIF.

      READ TABLE t_vttp WITH KEY tknum = w_vttk-tknum TRANSPORTING NO FIELDS BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        CLEAR: lv_complete, lv_nok.
        lv_complete = 'I'.

        LOOP AT t_vttp INTO w_vttp FROM sy-tabix.

          IF w_vttp-tknum <> w_vttk-tknum.
            EXIT.
          ENDIF.

          IF lv_complete EQ 'I' OR lv_complete EQ abap_true.

            READ TABLE t_vbuk WITH KEY vbeln = w_vttp-vbeln TRANSPORTING NO FIELDS BINARY SEARCH.

            IF sy-subrc IS INITIAL.

              LOOP AT t_vbuk INTO w_vbuk FROM sy-tabix.

                IF w_vbuk-vbeln <> w_vttp-vbeln.
                  EXIT.
                ENDIF.

                CASE w_vbuk-kostk.
                  WHEN 'C'.
                    lv_complete = abap_true.
                  WHEN OTHERS.
                    CLEAR lv_complete.
                    EXIT.
                ENDCASE.

              ENDLOOP.

            ENDIF.

          ENDIF.

          IF lv_nok IS INITIAL.

            READ TABLE t_lips WITH KEY vbeln = w_vttp-vbeln TRANSPORTING NO FIELDS BINARY SEARCH.

            IF sy-subrc IS INITIAL.

              LOOP AT t_lips INTO w_lips FROM sy-tabix.

                IF w_lips-vbeln <> w_vttp-vbeln.
                  EXIT.
                ENDIF.

                READ TABLE t_vepo WITH KEY vbeln = w_lips-vbeln
                                           posnr = w_lips-posnr
                                           TRANSPORTING NO FIELDS
                                           BINARY SEARCH.

                IF sy-subrc IS NOT INITIAL.
                  lv_nok = abap_true.
                ENDIF.

              ENDLOOP.

            ENDIF.

          ENDIF.

        ENDLOOP.

        IF lv_nok IS INITIAL.
          w_alv-check_embal = 'Embalado'.
        ENDIF.

        IF lv_complete EQ abap_true.
          w_alv-status_pick = 'Completo'.
        ELSE.
          w_alv-status_pick = 'Pedente'.
        ENDIF.

      ENDIF.

      APPEND w_alv TO t_alv.
      CLEAR w_alv.

    ENDLOOP.


    IF t_alv[] IS NOT INITIAL.

      me->m_chama_alv( ).

    ELSE.

      MESSAGE i049(zsd) DISPLAY LIKE 'E'.
      RETURN.

    ENDIF.

  ENDMETHOD.                    "m_monta_dados_alv

  METHOD m_chama_alv.

    DATA: lo_events TYPE REF TO cl_salv_events_table,
          ls_key    TYPE salv_s_layout_key.

    DATA: lv_text     TYPE string,
          lv_date(12) TYPE c,
          lv_time(10) TYPE c.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = gref_gr_alv
          CHANGING
            t_table      = t_alv.
      CATCH cx_salv_msg.
    ENDTRY.

    gref_functions = gref_gr_alv->get_functions( ).
    gref_functions->set_all( abap_true ).
    gref_cols = gref_gr_alv->get_columns( ).
    gref_cols->set_optimize( abap_true ).

    gref_selection = gref_gr_alv->get_selections( ).
    gref_selection->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    gref_layout = gref_gr_alv->get_layout( ).
    gref_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    ls_key-report = sy-repid.
    gref_layout->set_key( ls_key ).

    CREATE OBJECT gref_header.

    CLEAR lv_text.
    CONCATENATE 'Programa:' sy-cprog INTO lv_text SEPARATED BY space.
    gref_header->create_label( row = 2 column = 1 text = lv_text tooltip = lv_text ).

    CLEAR lv_text.
    CONCATENATE sy-sysid '-' sy-mandt INTO lv_text.
    CONCATENATE 'Ambiente:' lv_text INTO lv_text SEPARATED BY space.
    gref_header->create_label( row = 3 column = 1 text = lv_text tooltip = lv_text ).

    CLEAR lv_text.
    CONCATENATE 'Usuário:' sy-uname INTO lv_text SEPARATED BY space.
    gref_header->create_label( row = 4 column = 1 text = lv_text tooltip = lv_text ).

    WRITE sy-datum TO lv_date.
    WRITE sy-uzeit TO lv_time.
    CLEAR lv_text.
    CONCATENATE 'Data/Hora:' lv_date '|' lv_time INTO lv_text SEPARATED BY space.
    gref_header->create_label( row = 5 column = 1 text = lv_text tooltip = lv_text ).

    gref_gr_alv->set_top_of_list( gref_header ).
    gref_gr_alv->set_top_of_list_print( gref_header ).

    gref_gr_alv->set_screen_status( EXPORTING report        = syst-cprog
                                              pfstatus      = 'ZSD_STATUS_COCKPIT'
                                              set_functions = gref_gr_alv->c_functions_all ).

    gref_display = gref_gr_alv->get_display_settings( ).
    gref_display->set_striped_pattern( cl_salv_display_settings=>true ).

    lo_events = gref_gr_alv->get_event( ).
    SET HANDLER gcl_event_handler=>on_added_function FOR lo_events.

    gref_gr_alv->display( ).

  ENDMETHOD.                    "m_chama_alv

ENDCLASS.                    "gcl_cockpit_doc_transp IMPLEMENTATION

