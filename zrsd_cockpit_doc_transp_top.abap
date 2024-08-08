TABLES: t001w, vttk, smp_dyntxt, sscrfields.

TYPES:
      BEGIN OF ty_tvfp,
          tplst TYPE tvfp-tplst,
        END OF ty_tvfp,

      BEGIN OF ty_vttk,
          tknum TYPE vttk-tknum,
          tplst TYPE vttk-tplst,
          stdis TYPE vttk-stdis,
          streg TYPE vttk-streg,
          stlbg TYPE vttk-stlbg,
          stlad TYPE vttk-stlad,
          stabf TYPE vttk-stabf,
          sttbg TYPE vttk-sttbg,
          tdlnr TYPE vttk-tdlnr,
          tknum2 TYPE vekp-vpobjkey,
        END OF ty_vttk,

      BEGIN OF ty_lfa1,
          lifnr TYPE lfa1-lifnr,
          sortl TYPE lfa1-sortl,
        END OF ty_lfa1,

      BEGIN OF ty_vttp,
          tknum TYPE vttp-tknum,
          vbeln TYPE vttp-vbeln,
        END OF ty_vttp,

      BEGIN OF ty_vbuk,
           vbeln TYPE vbuk-vbeln,
           kostk TYPE vbuk-kostk,
         END OF ty_vbuk,

      BEGIN OF ty_lips,
           vbeln TYPE lips-vbeln,
           posnr TYPE lips-posnr,
         END OF ty_lips,

      BEGIN OF ty_vekp,
           venum TYPE vekp-venum,
           vpobjkey TYPE vekp-vpobjkey,
           brgew TYPE vekp-brgew,
           vhilm TYPE vekp-vhilm,
         END OF ty_vekp,

      BEGIN OF ty_vepo,
           venum TYPE vepo-venum,
           vbeln TYPE vepo-vbeln,
           posnr TYPE vepo-posnr,
         END OF ty_vepo.

DATA: t_tvfp TYPE TABLE OF ty_tvfp,
      w_tvfp TYPE ty_tvfp,
      t_vttk TYPE TABLE OF ty_vttk,
      w_vttk TYPE ty_vttk,
      t_lfa1 TYPE TABLE OF ty_lfa1,
      w_lfa1 TYPE ty_lfa1,
      t_vttp TYPE TABLE OF ty_vttp,
      w_vttp TYPE ty_vttp,
      t_vbuk TYPE TABLE OF ty_vbuk,
      w_vbuk TYPE ty_vbuk,
      t_lips TYPE TABLE OF ty_lips,
      w_lips TYPE ty_lips,
      t_vekp TYPE TABLE OF ty_vekp,
      w_vekp TYPE ty_vekp,
      t_vepo TYPE TABLE OF ty_vepo,
      w_vepo TYPE ty_vepo,
      t_alv  TYPE TABLE OF ztsd_alv_cockpit,
      w_alv  TYPE ztsd_alv_cockpit,
      t_log  TYPE TABLE OF ztsd_log_cockpit,
      w_log  TYPE ztsd_log_cockpit.

DATA: gref_functions TYPE REF TO cl_salv_functions,
      gref_cols      TYPE REF TO cl_salv_columns,
      gref_column    TYPE REF TO cl_salv_column,
      gref_display   TYPE REF TO cl_salv_display_settings,
      gref_header    TYPE REF TO cl_salv_form_layout_grid,
      gref_h_flow    TYPE REF TO cl_salv_form_layout_flow,
      gref_gr_alv    TYPE REF TO cl_salv_table,
      gref_selection TYPE REF TO cl_salv_selections,
      gref_layout    TYPE REF TO cl_salv_layout.

DATA: w_bdcdata TYPE bdcdata,
      t_bdcdata TYPE TABLE OF bdcdata,
      t_msg     TYPE TABLE OF bdcmsgcoll,
      w_msg     TYPE bdcmsgcoll.

DATA: gv_tdlnr      TYPE vttk-tdlnr,
      gv_tpveiculo  TYPE vekp-vhilm,
      gv_motor      TYPE vttk-text1,
      gv_placa      TYPE vttk-text2,
      gv_carreg_por TYPE vttk-text2.
