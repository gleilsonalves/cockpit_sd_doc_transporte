*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'SALVAR'.
      IF gv_tdlnr IS INITIAL.
        MESSAGE e003(zsd) WITH 'Preencher o(s) parametro(s)' DISPLAY LIKE 'I'.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN OTHERS.
      CLEAR gv_tdlnr.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'SALVAR'.
      IF gv_tpveiculo IS INITIAL.
        MESSAGE e003(zsd) WITH 'Preencher o(s) parametro(s)' DISPLAY LIKE 'I'.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN OTHERS.
      CLEAR gv_tpveiculo.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.

  CASE sy-ucomm.
    WHEN 'SALVAR'.
      IF gv_motor IS INITIAL AND gv_placa IS INITIAL.
        MESSAGE e003(zsd) WITH 'Preencher o(s) parametro(s)' DISPLAY LIKE 'I'.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN OTHERS.
      CLEAR: gv_motor, gv_placa.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0700  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0700 INPUT.

  CASE sy-ucomm.
    WHEN 'SALVAR'.
      IF gv_carreg_por IS INITIAL.
        MESSAGE e003(zsd) WITH 'Preencher o(s) parametro(s)' DISPLAY LIKE 'I'.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN OTHERS.
      CLEAR gv_carreg_por.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0700  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS ' '.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS ' '.
*  SET TITLEBAR 'xxx'.

  IF gv_tpveiculo IS INITIAL.
    gv_tpveiculo = 'TRUCK SECO-12TON'.
  ENDIF.

ENDMODULE.                 " STATUS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.

  SET PF-STATUS ' '.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0500  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0700  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0700 OUTPUT.

  SET PF-STATUS ' '.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0700  OUTPUT
