REPORT zrsd_cockpit_doc_transp.

INCLUDE: zrsd_cockpit_doc_transp_top,
         zrsd_cockpit_doc_transp_sel,
         zrsd_cockpit_doc_transp_cls,
         zrsd_cockpit_doc_transp_usei01.

START-OF-SELECTION.

  DATA: go_process TYPE REF TO gcl_cockpit_doc_transp.

  CREATE OBJECT go_process.

  IF go_process IS BOUND.

    go_process->m_seleciona_dados( EXPORTING i_werks = s_centro[]
                                             i_tknum = s_doc_tr[]
                                             i_erdat = s_dt_cri[] ).

  ENDIF.
