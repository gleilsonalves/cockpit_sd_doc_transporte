SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-000.

SELECTION-SCREEN SKIP.
SELECT-OPTIONS: s_centro FOR t001w-werks,
                s_doc_tr FOR vttk-tknum,
                s_dt_cri FOR vttk-erdat.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN PUSHBUTTON /1(25) log USER-COMMAND log.

SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  log = 'Relatório Processamento'.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'LOG'.
      CALL TRANSACTION 'ZSD101'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      IF s_centro[] IS INITIAL.
        MESSAGE e257(zsd) WITH 'CENTRO é obrigatório!'.
        STOP.
      ENDIF.
  ENDCASE.
