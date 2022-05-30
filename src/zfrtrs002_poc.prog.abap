*&---------------------------------------------------------------------*
*& Report  ZFRTRS002
*&
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
* AUTHOR:  Adrian Borja        DATE: 11/29/2011                        *
*                                                                      *
* DESCRIPTION:                                                         *
* This program is a copy of Program ZFRTRI011 for its execution of     *
* Billing from BKPF to be processed in Brand Matrix from custom        *
* table ZFTMATRIXQUEUE. Another program does the retrieval from the    *
* custom table.                                                        *
************************************************************************
* CHANGE HISTORY                                                       *
*                                                                      *
* Mod Date     Changed By     Description                    Chng ID   *
* 11/29/2011   BORJAAD        Initial Creation             GD2K925908  *
*                             R19 SAC FSID4479 CRQ314093               *
* 12/12/2011   BORJAAD        Addition for ID4479          GD2K925966  *
*                             CRQ314093                                *
* 03/06/2012   CABAGJE        INC925755                    GD2K930338  *
*                             Update the table ZMATRIX_BILLING each and*
*                             every Billing Doc. after it processed    *
* 06/06/2012   AGCOPRJE       INC992601                    GD2K937374  *
*                             Backout Fix on INC925755                 *
* 01/02/2017   VISWASRI       to send mails to valid users GD2K9A0T62  *
*                             INC2225340                               *
*&---------------------------------------------------------------------*

REPORT  zfrtrs002_poc MESSAGE-ID zf600.
* INCLUDES
INCLUDE ZFRTRS002_N01.
*INCLUDE <cntn01>.
* Type-pools
TYPE-POOLS abap.
* TABLES
TABLES: bkpf,
        vbrk,
        t9control,
        zmatrix_billing,
        zftmatrixqueue.

TYPES: BEGIN OF t_company,
         bukrs TYPE bukrs,
         date  TYPE d,
         time  TYPE t,
       END OF t_company,
       BEGIN OF t_icompdate_upd,
         zbukrs TYPE t9control-zbukrs,
         zdatex TYPE t9control-zdatex,
         ztimex TYPE t9control-ztimex,
       END OF t_icompdate_upd,
       BEGIN OF t_t9fbm005,
         bukrs TYPE t9fbm005-bukrs,
         type  TYPE t9fbm005-type,
       END OF t_t9fbm005.
FIELD-SYMBOLS: <fs_company>  TYPE t_company,
               <fs_zftmatrixqueue> TYPE zftmatrixqueue.
* DATA
DATA: i_vbrk TYPE STANDARD TABLE OF vbrk,
      x_vbrk TYPE vbrk.
DATA: i_vbrp TYPE STANDARD TABLE OF vbrp,
      x_vbrp TYPE vbrp.
DATA: v_file(100) TYPE c,
      i_zftmatrixqueue   TYPE STANDARD TABLE OF zftmatrixqueue,
      i_zftmatrixqueue2  TYPE STANDARD TABLE OF zftmatrixqueue,
      x_zftmatrixqueue   TYPE zftmatrixqueue,
      x_zftmatrixqueue2  TYPE zftmatrixqueue,
      i_zmatrix_bill_upd TYPE STANDARD TABLE OF zmatrix_billing,
      x_zmatrix_bill_upd TYPE zmatrix_billing,
      v_type             TYPE t9fbm005-type,
      v_type1            TYPE t9fbm005-type,
      v_type2            TYPE t9fbm005-type,
      v_tabix            TYPE sy-tabix.

DATA: i_company         TYPE STANDARD TABLE OF t_company,
      i_zmatbillingexst TYPE STANDARD TABLE OF zmatrix_billing,
      x_zmatbillingexst TYPE zmatrix_billing,
      x_company         TYPE t_company,
      i_t9fbm005        TYPE STANDARD TABLE OF t_t9fbm005,
      x_t9fbm005        TYPE t_t9fbm005,
      i_zt9control      TYPE TABLE OF zt9control,
      x_zt9control      TYPE zt9control,
      x_control         TYPE zt9control.

* Table with the processed log.

DATA: i_log TYPE STANDARD TABLE OF zfs_bm_log,
      x_log TYPE zfs_bm_log.

CONSTANTS: c_x                 TYPE c VALUE 'X',
           c_cmplt             TYPE c VALUE 'C',
           c_error             TYPE c VALUE 'E',
            c_matrix     TYPE sy-repid
                        VALUE 'ZFRTRR047/ZFRTRI011/ZFRTRR008',
           c_fkartp      TYPE t9fbm002-tlevel VALUE 'FKARTPASSB',
           c_a2          TYPE t9fbm002-data_type VALUE 'A2'.

CONSTANTS: c_sta TYPE c VALUE 'S',
           c_err TYPE c VALUE 'E'.

* RANGES
DATA:   r_blart TYPE RANGE OF bkpf-blart,
        r_fkart TYPE RANGE OF vbrk-fkart.

TYPES: BEGIN OF t_sopcklsti1,
        transf_bin TYPE sopcklsti1-transf_bin,
        head_start TYPE sopcklsti1-head_start,
        head_num   TYPE sopcklsti1-head_num,
        body_start TYPE sopcklsti1-body_start,
        body_num   TYPE sopcklsti1-body_num,
        doc_type   TYPE sopcklsti1-doc_type,
        obj_name   TYPE sopcklsti1-obj_name,
        obj_descr  TYPE sopcklsti1-obj_descr,
        obj_langu  TYPE sopcklsti1-obj_langu,
        doc_size   TYPE sopcklsti1-doc_size,
        mess_type  TYPE sopcklsti1-mess_type,
       END OF t_sopcklsti1,

       BEGIN OF t_solisti1,
         line TYPE solisti1-line,
       END OF t_solisti1,

       BEGIN OF t_somlreci1,
         receiver   TYPE somlreci1-receiver,
         rec_type   TYPE somlreci1-rec_type,
         rec_id     TYPE somlreci1-rec_id,
         reply_doc  TYPE somlreci1-reply_doc,
         rec_date   TYPE somlreci1-rec_date,
         proxy_id   TYPE somlreci1-proxy_id,
         retrn_code TYPE somlreci1-retrn_code,
         express    TYPE sosndatti1-express,
         copy       TYPE sosndatti1-copy,
         blind_copy TYPE sosndatti1-blind_copy,
         no_forward TYPE sosndatti1-no_forward,
         no_print   TYPE sosndatti1-no_print,
         to_answer  TYPE sosndatti1-to_answer,
         to_do_expl TYPE sosndatti1-to_do_expl,
         to_do_grp  TYPE sosndatti1-to_do_grp,
         com_type   TYPE soextatti1-com_type,
         lfdnr      TYPE soextatti1-lfdnr,
         fax        TYPE soextatti1-fax,
         country    TYPE soextatti1-country,
         spool_id   TYPE soextatti1-spool_id,
         notif_del  TYPE soextatti1-notif_del,
         notif_read TYPE soextatti1-notif_read,
         notif_ndel TYPE soextatti1-notif_ndel,
         sap_body   TYPE soextatti1-sap_body,
       END OF t_somlreci1.

DATA:   i_message      TYPE STANDARD TABLE OF t_solisti1,
        x_message      TYPE t_solisti1,
        i_packing_list TYPE STANDARD TABLE OF t_sopcklsti1,
        x_packing_list TYPE t_sopcklsti1,
        i_receivers    TYPE STANDARD TABLE OF t_somlreci1,
        x_receivers    TYPE t_somlreci1,
        v_sent_all(1)  TYPE c,
        x_doc_data     TYPE sodocchgi1,
        v_lines        TYPE i.
CONSTANTS: c_int(3) TYPE c VALUE 'INT'.

*********************************************************************
* SELECTION SCREEN
*********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS s_bukrs FOR bkpf-bukrs NO INTERVALS.

PARAMETERS: p_logf  TYPE filename-fileintern OBLIGATORY
                    DEFAULT 'YBM_LOG_FILENAME'.

PARAMETERS: p_billno TYPE i.

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN EVENTS    - validate user input                  *
*----------------------------------------------------------------------*
*insert the default values for the select option s_blart: 'RV' 'RR' DP'
AT SELECTION-SCREEN OUTPUT.

  PERFORM f_insert_default_t9con.

AT SELECTION-SCREEN ON s_bukrs.
  CLEAR: x_company.
  REFRESH: i_company.
  SELECT bukrs
  FROM t001
  INTO TABLE i_company
  WHERE bukrs IN s_bukrs.

  IF sy-subrc EQ 0.
    IF i_company[] IS NOT INITIAL.
      REFRESH: i_t9fbm005.
      SELECT bukrs
             type
      FROM t9fbm005
      INTO TABLE i_t9fbm005
      FOR ALL ENTRIES IN i_company
      WHERE bukrs EQ i_company-bukrs.
      IF sy-subrc EQ 0.
        SORT i_t9fbm005 BY bukrs.
      ENDIF.
    ENDIF.
    LOOP AT i_company ASSIGNING <fs_company>.
      v_tabix = sy-tabix.
      CLEAR: v_type1,
             x_t9fbm005.
      READ TABLE i_t9fbm005
      INTO x_t9fbm005
      WITH KEY bukrs = <fs_company>-bukrs
      BINARY SEARCH.
      IF sy-subrc NE 0.
        MESSAGE e013 WITH <fs_company>-bukrs.
      ELSE.
        MOVE: x_t9fbm005-bukrs TO <fs_company>-bukrs,
              x_t9fbm005-type TO v_type1.
        IF v_tabix NE 1.
          IF v_type1 NE v_type2.
            MESSAGE e014.
          ENDIF.
        ENDIF.
        v_type2 = v_type1.
      ENDIF.
    ENDLOOP.
  ENDIF.
*********************************************************************
*INITIALIZATION
*********************************************************************
INITIALIZATION.

  PERFORM f_initialization.

*********************************************************************
*Begin of Process
*********************************************************************
START-OF-SELECTION.
  PERFORM f_check_selection_entry.
  CLEAR: x_company.
  READ TABLE i_company INTO x_company INDEX 1.
  PERFORM f_get_cc_type USING x_company-bukrs
                        CHANGING v_type.
  PERFORM f_get_billing_items.

*********************************************************************
*End of Process
*********************************************************************
END-OF-SELECTION.
  IF i_zftmatrixqueue[] IS INITIAL.
    MESSAGE s001(00) WITH text-m01.
  ELSE.
    PERFORM f_get_file_data.
    PERFORM create_file_log.
    PERFORM show_log.
  ENDIF.

  IF sy-batch EQ c_x.
    PERFORM f_email_message_body.
    CLEAR v_lines.
    DESCRIBE TABLE i_message LINES v_lines.
    IF v_lines GT 1.
      PERFORM f_send_email.
      CLEAR v_lines.
    ENDIF.
  ENDIF.

  PERFORM f_delete_flag.

*&---------------------------------------------------------------------*
*&      Form  f_email_message_body
*&---------------------------------------------------------------------*
*       Subroutine for creating message body
*          -> this subroutine fills up the table for the message body,
*             this was filled by getting the error messages of the
*             background job, Billing document number, item number,
*             company code, document type,& group name
*----------------------------------------------------------------------*
FORM f_email_message_body.

  CONSTANTS: c_lcomma(1) TYPE c VALUE ','.

  REFRESH: i_message.
  x_message = text-003.
  APPEND x_message TO i_message.
  CLEAR: x_message,
         x_log.

  LOOP AT i_log INTO x_log WHERE type EQ c_error.

    CONCATENATE x_log-vbeln
                x_log-posnr
                x_log-bukrs
                x_log-blart
                x_log-group
                x_log-message
         INTO x_message SEPARATED BY c_lcomma.

    APPEND x_message TO i_message.
    CLEAR: x_message,
           x_log.
  ENDLOOP.

ENDFORM.                    "F_EMAIL_MESSAGE_BODY

*&---------------------------------------------------------------------*
*&      Form  f_initialization
*&---------------------------------------------------------------------*
*       Initialize all the global data
*----------------------------------------------------------------------*
FORM f_initialization.

  CLEAR: i_vbrk,
         i_vbrp,
         v_file.

  REFRESH: i_vbrk,
           i_vbrp.

* Get the Billing Types not to be treated
  CALL FUNCTION 'Z_IC_BRAND_GET_FIXED_VALUES'
    EXPORTING
      i_repid             = c_matrix
      i_type              = c_a2
      i_level             = c_fkartp
    TABLES
      t_range             = r_fkart
    EXCEPTIONS
      data_type_not_found = 1
      no_data_found       = 2
      OTHERS              = 3.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_initialization

*&---------------------------------------------------------------------*
*&      Form  f_get_billing_items
*&---------------------------------------------------------------------*
*       Get the sales invoice data to create the file
*----------------------------------------------------------------------*
FORM f_get_billing_items .
  DATA: v_lcount TYPE i.
  CLEAR: v_lcount.
  CONSTANTS: c_lcmplt TYPE c VALUE 'C'.
  REFRESH: i_zftmatrixqueue.

*Begin of Change BORJAAD 12/12/2011 GD2K925966 ID4479
*  IF p_billno IS NOT INITIAL.
*    SELECT *
*    UP TO p_billno ROWS
*    FROM zftmatrixqueue
*    INTO TABLE i_zftmatrixqueue.
*  ELSE.
*    SELECT *
*    FROM zftmatrixqueue
*    INTO TABLE i_zftmatrixqueue.
*  ENDIF.
  SELECT *
  FROM zftmatrixqueue
  INTO TABLE i_zftmatrixqueue
  WHERE bukrs IN s_bukrs.
*End of Change BORJAAD 12/12/2011 GD2K925966 ID4479
  IF sy-subrc EQ 0.
    SORT i_zftmatrixqueue BY bukrs belnr vbeln.
  ENDIF.


  REFRESH: i_zmatbillingexst.
  IF i_zftmatrixqueue[] IS NOT INITIAL.
    SELECT mandt
           vbeln
           status
           datum
           bukrs
    FROM zmatrix_billing
    INTO TABLE i_zmatbillingexst
    FOR ALL ENTRIES IN i_zftmatrixqueue
    WHERE vbeln EQ i_zftmatrixqueue-vbeln
      AND status EQ c_lcmplt.
    IF sy-subrc EQ 0.
      SORT i_zmatbillingexst BY vbeln.
      CLEAR: x_zmatbillingexst.
      LOOP AT i_zmatbillingexst INTO x_zmatbillingexst.
        DELETE i_zftmatrixqueue WHERE vbeln EQ x_zmatbillingexst-vbeln.
        IF sy-subrc EQ 0.
          "Do nothing
        ENDIF.
        CLEAR: x_zmatbillingexst.
      ENDLOOP.
    ENDIF.
  ENDIF.
*Begin of Insert BORJAAD 12/12/2011 GD2K925966 ID4479
  IF p_billno IS NOT INITIAL.
    REFRESH: i_zftmatrixqueue2.
    CLEAR: x_zftmatrixqueue.
    LOOP AT i_zftmatrixqueue INTO x_zftmatrixqueue.
      IF sy-tabix LE p_billno.
        APPEND x_zftmatrixqueue TO i_zftmatrixqueue2.
      ELSE.
        EXIT.
      ENDIF.
      CLEAR: x_zftmatrixqueue.
    ENDLOOP.
    IF i_zftmatrixqueue2[] IS NOT INITIAL.
      REFRESH: i_zftmatrixqueue.
      MOVE: i_zftmatrixqueue2[] TO i_zftmatrixqueue[].
    ENDIF.
  ENDIF.
*End of Insert BORJAAD 12/12/2011 GD2K925966 ID4479
*-Get the positions.
  IF i_zftmatrixqueue[] IS NOT INITIAL.
    REFRESH: i_vbrk,
             i_vbrp.
    SELECT * INTO TABLE i_vbrk
    FROM vbrk
    FOR ALL ENTRIES IN i_zftmatrixqueue
    WHERE vbeln EQ i_zftmatrixqueue-vbeln.
    IF sy-subrc EQ 0.
      SORT i_vbrk BY vbeln.
    ENDIF.
    REFRESH: i_vbrp.
    SELECT *
    INTO TABLE i_vbrp
    FROM vbrp
    FOR ALL ENTRIES IN i_zftmatrixqueue
    WHERE vbeln EQ i_zftmatrixqueue-vbeln.
    IF sy-subrc EQ 0.
      SORT i_vbrp BY vbeln posnr.
    ENDIF.
  ENDIF.


ENDFORM.                    " f_get_billing_values
*&---------------------------------------------------------------------*
*&      Form  f_get_file_data
*&---------------------------------------------------------------------*
*       Get the file data to create the file which have the information
* to do the accountings.
*----------------------------------------------------------------------*
FORM f_get_file_data.
  DATA: i_1doc TYPE STANDARD TABLE OF vbrp,
        x_det  TYPE zfs_bm_scenario_det.
  IF i_vbrk[] IS NOT INITIAL.
    REFRESH: i_zmatrix_bill_upd.
    SELECT *
    FROM zmatrix_billing
    INTO TABLE i_zmatrix_bill_upd
    FOR ALL ENTRIES IN i_vbrk
    WHERE vbeln EQ i_vbrk-vbeln.
    IF sy-subrc EQ 0.
      SORT i_zmatrix_bill_upd BY vbeln.
    ENDIF.
  ENDIF.
  CLEAR: x_vbrp,
         x_vbrk,
         x_zmatrix_bill_upd.
  LOOP AT i_vbrp INTO x_vbrp.
    AT NEW vbeln.
      CLEAR: x_vbrk.
      READ TABLE i_vbrk
      INTO x_vbrk
      WITH KEY vbeln = x_vbrp-vbeln
                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        "Do nothing
      ENDIF.
      CLEAR: x_zmatrix_bill_upd.
      READ TABLE i_zmatrix_bill_upd
      INTO x_zmatrix_bill_upd
      WITH KEY vbeln = x_vbrp-vbeln
      BINARY SEARCH.
      IF sy-subrc EQ 0.
        "do nothing
      ENDIF.
      REFRESH: i_1doc.
    ENDAT.
    CHECK x_zmatrix_bill_upd-status NE c_cmplt.
    CLEAR: x_det.
    MOVE-CORRESPONDING x_vbrp TO x_det.
    IF x_det IS NOT INITIAL.
      APPEND x_vbrp TO i_1doc.
    ENDIF.

    AT END OF vbeln.
      CHECK i_1doc[] IS NOT INITIAL.
      CALL FUNCTION 'Z_IC_BRAND_MATRIX_POSTING'
        EXPORTING
          im_vbrk                 = x_vbrk
          im_test                 = abap_false
          im_cc_type              = v_type
        TABLES
          t_vbrp                  = i_1doc
          t_log                   = i_log
       EXCEPTIONS
         error_in_document              = 1
         no_commission_distrit_scenario = 2
         OTHERS                         = 3
                .
      IF sy-subrc EQ 0.
        x_zmatrix_bill_upd-status = c_cmplt.
      ENDIF.
      x_zmatrix_bill_upd-vbeln = x_vbrp-vbeln.
      x_zmatrix_bill_upd-datum = sy-datum.
*-----Get the company Code
      IF x_zmatrix_bill_upd-bukrs IS INITIAL.
          x_zmatrix_bill_upd-bukrs = x_vbrk-bukrs.
      ENDIF.
      APPEND x_zmatrix_bill_upd TO i_zmatrix_bill_upd.
* Start of Delete AGCOPRJE 06/06/2012 INC992601 GD2K937374
* Backout INC925755
** Begin of Insert | CABAGJE | INC925755 | GD2K930338 | 03/06/2012
*      MODIFY zmatrix_billing FROM x_zmatrix_bill_upd.
*      IF sy-subrc EQ 0.
*        "Do nothing
*      ENDIF.
** End of Insert | CABAGJE | INC925755 | GD2K930338 | 03/06/2012
* end of Delete AGCOPRJE 06/06/2012 INC992601 GD2K937374
      CLEAR: x_zmatrix_bill_upd,
             i_1doc[].
    ENDAT.

    CLEAR: x_vbrp.
  ENDLOOP.
* Start of change AGCOPRJE 06/06/2012 INC992601 GD2K937374
* Re inserted the code.
** Begin of Delete | CABAGJE | INC925755 | GD2K930338 | 03/06/2012
*- transfer the modify table to update the table each
*- Billing Doc. is processed.
*  IF i_zmatrix_bill_upd[] IS NOT INITIAL.
*    MODIFY zmatrix_billing FROM TABLE i_zmatrix_bill_upd.
*    IF sy-subrc EQ 0.
*      "Do nothing
*    ENDIF.
*  ENDIF.
** End of Delete | CABAGJE | INC925755 | GD2K930338 | 03/06/2012
  IF i_zmatrix_bill_upd[] IS NOT INITIAL.
    MODIFY zmatrix_billing FROM TABLE i_zmatrix_bill_upd.
    IF sy-subrc EQ 0.
      "Do nothing
    ENDIF.
  ENDIF.
* End of change AGCOPRJE 06/06/2012 INC992601 GD2K937374

ENDFORM.                    " f_get_file_data

*&---------------------------------------------------------------------*
*&      Form  INSERT_DEFAULT_T9CON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_insert_default_t9con.
  CONSTANTS: c_level TYPE t9fbm002-tlevel VALUE '01',
             c_ltype  TYPE t9fbm002-data_type VALUE 'A3',
             c_lorigrepid TYPE repid VALUE 'ZFRTRI011'.

  REFRESH: r_blart.

  CALL FUNCTION 'Z_IC_BRAND_GET_FIXED_VALUES'
    EXPORTING
      i_repid             = c_lorigrepid
      i_type              = c_ltype
      i_level             = c_level
    TABLES
      t_range             = r_blart
    EXCEPTIONS
      data_type_not_found = 1
      no_data_found       = 2
      OTHERS              = 3.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_INSERT_DEFAULT_T9CON

*&---------------------------------------------------------------------*
*&      Form  create_file_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_file_log.
  CONSTANTS: c_lerror TYPE c VALUE 'E',
             c_lok    TYPE c VALUE 'S',
             c_lx     TYPE c VALUE 'X',
             c_lsplit TYPE c VALUE ';'.
  DATA: v_lline(255) TYPE c.
  CLEAR: x_log.
  LOOP AT i_log INTO x_log.
    AT FIRST.
      WRITE / text-hd2.
*-----Get the file name
      CALL FUNCTION 'FILE_GET_NAME'
        EXPORTING
          logical_filename = p_logf
          parameter_1      = v_type
        IMPORTING
          file_name        = v_file
        EXCEPTIONS
          file_not_found   = 1
          OTHERS           = 2.
      IF sy-subrc NE 0.
        MESSAGE e008 WITH p_logf.
      ENDIF.
      OPEN DATASET v_file FOR OUTPUT
                   IN TEXT MODE
                   ENCODING DEFAULT.
      IF sy-subrc NE 0.
        MESSAGE e012 WITH v_file.
      ENDIF.

    ENDAT.
    CASE x_log-type.
      WHEN c_lerror.
        CONCATENATE x_log-vbeln x_log-bukrs x_log-group
                    x_log-message INTO v_lline
                                  SEPARATED BY c_lsplit.
      WHEN c_lok.
        CONCATENATE x_log-vbeln x_log-bukrs x_log-belnr
                    x_log-blart x_log-message INTO v_lline
                                              SEPARATED BY c_lsplit.
      WHEN c_lx.
        CONCATENATE x_log-vbeln
                    x_log-posnr
                    x_log-message INTO v_lline
                                  SEPARATED BY c_lsplit.
    ENDCASE.

    TRANSFER v_lline TO v_file.
    AT LAST.
      CLOSE DATASET v_file.
    ENDAT.
    CLEAR: x_log.
  ENDLOOP.

ENDFORM.                    " create_file_log

*&---------------------------------------------------------------------*
*&      Form  f_check_selection_entry
*&---------------------------------------------------------------------*
* Check input in the selection screen to avoid parallel runs.
*----------------------------------------------------------------------*
FORM f_check_selection_entry.

  REFRESH: i_zt9control.

  CLEAR: x_zt9control,
         x_control.

  PERFORM f_check_blank_zbukrs.

  SORT i_company BY bukrs.

  IF i_company[] IS NOT INITIAL.
    SELECT *
    INTO TABLE i_zt9control
    FROM zt9control
    FOR ALL ENTRIES IN i_company
    WHERE zbukrs EQ i_company-bukrs
      AND zrepid EQ sy-repid
      AND zhkont EQ space.

    IF sy-subrc IS INITIAL.
      SORT i_zt9control BY zbukrs
                           zrepid
                           zhkont.
      DELETE ADJACENT DUPLICATES FROM i_zt9control COMPARING ALL FIELDS.
      CLEAR: x_company.
      LOOP AT i_company INTO x_company.
        CLEAR: x_control,
               x_zt9control.

        x_control-zbukrs  = x_company-bukrs.
        x_control-zrepid  = sy-repid.
        x_control-zhkont  = space.
        x_control-zdatex  = sy-datum.
        x_control-ztimex  = sy-uzeit.
        x_control-zusname = sy-uname.
        x_control-zflag   = c_x.
        CLEAR: x_zt9control.
        READ TABLE i_zt9control INTO x_zt9control
        WITH KEY zbukrs = x_control-zbukrs
                 zrepid = sy-repid
                 zhkont = space
        BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          IF x_zt9control-zflag EQ c_x.
            MESSAGE text-e01 TYPE c_sta DISPLAY LIKE c_err.
            IF sy-batch IS NOT INITIAL.
              CLEAR: x_log.
              REFRESH: i_log.
              x_log-type = c_error.
              x_log-message = text-e01.

              APPEND x_log TO i_log.
              CLEAR: x_log.
              PERFORM f_email_message_body.
              CLEAR v_lines.
              DESCRIBE TABLE i_message LINES v_lines.
              IF v_lines GT 1.
                PERFORM f_send_email.
                CLEAR v_lines.
              ENDIF.
            ENDIF.
            LEAVE LIST-PROCESSING.
          ELSE.
            MODIFY zt9control FROM x_control.
            IF sy-subrc IS INITIAL.
              "Do nothing.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: x_company.
      ENDLOOP.

      COMMIT WORK AND WAIT.
    ELSE.
      CLEAR: x_company.
      LOOP AT i_company INTO x_company.
        CLEAR: x_control.

        x_control-zbukrs  = x_company-bukrs.
        x_control-zrepid  = sy-repid.
        x_control-zhkont  = space.
        x_control-zdatex  = sy-datum.
        x_control-ztimex  = sy-uzeit.
        x_control-zusname = sy-uname.
        x_control-zflag   = c_x.

        INSERT INTO zt9control VALUES x_control.
        IF sy-subrc IS INITIAL.
          "Do nothing.
        ENDIF.
        CLEAR: x_company.
      ENDLOOP.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.                    "f_check_selection_entry

*&---------------------------------------------------------------------*
*&      Form f_delete_flag
*&---------------------------------------------------------------------*
* Delete flag from table ZT9CONTROL.
*----------------------------------------------------------------------*
FORM f_delete_flag.

  REFRESH: i_zt9control.

  CLEAR: x_zt9control.

  IF i_company[] IS NOT INITIAL.
    SELECT *
    INTO TABLE i_zt9control
    FROM zt9control
    FOR ALL ENTRIES IN i_company
    WHERE zbukrs EQ i_company-bukrs
      AND zrepid EQ sy-repid
      AND zhkont EQ space
      AND zflag  EQ c_x.

    IF sy-subrc IS INITIAL.
      CLEAR: x_zt9control.
      LOOP AT i_zt9control INTO x_zt9control.
        x_zt9control-zflag = space.
        MODIFY zt9control FROM x_zt9control.
        IF sy-subrc IS INITIAL.
          "Do nothing.
        ENDIF.

        CLEAR: x_zt9control.
      ENDLOOP.

      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

* Company Code parameter is blank when run.
  IF s_bukrs-low IS INITIAL.
    CLEAR: x_zt9control.

    SELECT SINGLE *
    INTO x_zt9control
    FROM zt9control
    WHERE zbukrs EQ space
      AND zrepid EQ sy-repid
      AND zhkont EQ space
      AND zflag  EQ c_x.
*   Remove the status flag X.
    IF sy-subrc IS INITIAL.
      x_zt9control-zflag = space.
      MODIFY zt9control FROM x_zt9control.
      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT.
      ENDIF.

      CLEAR: x_zt9control.
    ENDIF.
  ENDIF.

ENDFORM.                    "f_delete_flag

*&---------------------------------------------------------------------*
*&      Form  f_check_blank_zbukrs
*&---------------------------------------------------------------------*
* If the program is executed without a Company Code, and the next run is
* made using a Company Code, the error message should also be triggered.
*----------------------------------------------------------------------*
FORM f_check_blank_zbukrs.

  CLEAR: x_zt9control,
         x_control.

  SELECT SINGLE *
  INTO x_zt9control
  FROM zt9control
  WHERE zbukrs EQ space
    AND zrepid EQ sy-repid
    AND zhkont EQ space.
* An entry already existing in the table.
  IF sy-subrc IS INITIAL.
    x_control-zbukrs  = space.
    x_control-zrepid  = sy-repid.
    x_control-zhkont  = space.
    x_control-zdatex  = sy-datum.
    x_control-ztimex  = sy-uzeit.
    x_control-zusname = sy-uname.
    x_control-zflag   = c_x.
*   Status was flag X.
    IF x_zt9control-zflag EQ c_x.
      MESSAGE text-e01 TYPE c_sta DISPLAY LIKE c_err.

      IF sy-batch IS NOT INITIAL.
        CLEAR x_log.
        REFRESH i_log.
        x_log-type = c_error.
        x_log-message = text-e01.
        APPEND x_log TO i_log.
        PERFORM f_email_message_body.
        CLEAR v_lines.
        DESCRIBE TABLE i_message LINES v_lines.
        IF v_lines GT 1.
          PERFORM f_send_email.
          CLEAR v_lines.
        ENDIF.
      ENDIF.

      LEAVE LIST-PROCESSING.
*   Company Code parameter is blank when run.
    ELSEIF s_bukrs-low IS INITIAL.
      MODIFY zt9control FROM x_control.
      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
* No entry exist yet.
  ELSE.
*   Company Code parameter is blank when run.
    IF s_bukrs-low IS INITIAL.
      x_control-zbukrs  = space.
      x_control-zrepid  = sy-repid.
      x_control-zhkont  = space.
      x_control-zdatex  = sy-datum.
      x_control-ztimex  = sy-uzeit.
      x_control-zusname = sy-uname.
      x_control-zflag   = c_x.

      INSERT INTO zt9control VALUES x_control.
      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "f_check_blank_zbukrs

*&---------------------------------------------------------------------*
*&      Form  f_mail_execute_program
*&---------------------------------------------------------------------*
* this subroutine executes the sending process,
*if the system is unavailable it will wait for 2 seconds
*----------------------------------------------------------------------*
FORM f_mail_execute_program.

  WAIT UP TO 2 SECONDS.
  SUBMIT rsconn01 WITH mode EQ c_int
                  WITH output EQ c_x
                  AND RETURN.

ENDFORM.                    "F_MAIL_EXECUTE_PROGRAM

*&---------------------------------------------------------------------*
*&      Form  f_get_receivers
*&---------------------------------------------------------------------*
*retrieves the contact details of those who
*will recieve the error email notification
*----------------------------------------------------------------------*
FORM f_get_receivers.

  TYPES: BEGIN OF t_luser,
            agr_name TYPE agr_users-agr_name,
            uname    TYPE agr_users-uname,
         END   OF t_luser,

         BEGIN OF t_lusr21,
            bname TYPE usr21-bname,
            persnumber  TYPE usr21-persnumber,
         END OF t_lusr21,

        BEGIN OF t_ladr6,
            persnumber TYPE adr6-persnumber,
            smtp_addr  TYPE adr6-smtp_addr,
        END OF t_ladr6.

  DATA: i_luser  TYPE STANDARD TABLE OF t_luser,
        i_lusr21 TYPE STANDARD TABLE OF t_lusr21,
        i_ladr6  TYPE STANDARD TABLE OF t_ladr6,
        x_ladr6  TYPE t_ladr6,
        v_lemail TYPE  somlreci1-receiver.

  CONSTANTS: c_lcontroller TYPE string
                VALUE 'RTR_BTRM_CONTROLLER',
             c_lu(1) TYPE c VALUE 'U'.


  CLEAR: x_ladr6,
         i_receivers,
         v_lemail.

  REFRESH: i_luser,
           i_lusr21,
           i_ladr6,
           i_receivers.

  SELECT agr_name
         uname
          FROM agr_users
          INTO TABLE i_luser
          WHERE agr_name EQ c_lcontroller
*Begin of insert VISWASRI INC2225340 GD2K9A0T62
          AND FROM_DAT LE sy-datum
          AND TO_DAT GE sy-datum.
*End of insert VISWASRI INC2225340 GD2K9A0T62

  IF sy-subrc EQ 0
    AND i_luser IS NOT INITIAL.
    SELECT bname
           persnumber
           FROM usr21
           INTO TABLE i_lusr21
               FOR ALL ENTRIES IN i_luser
               WHERE bname EQ i_luser-uname.

    IF sy-subrc EQ 0
      AND i_lusr21 IS NOT INITIAL.
      SELECT persnumber
             smtp_addr
             FROM adr6
             INTO TABLE i_ladr6
                 FOR ALL ENTRIES IN i_lusr21
                 WHERE persnumber EQ i_lusr21-persnumber.
    ENDIF.
  ENDIF.
  CLEAR: x_ladr6.
  LOOP AT i_ladr6 INTO x_ladr6.

    CLEAR v_lemail.

    v_lemail = x_ladr6-smtp_addr.
    x_receivers-receiver = v_lemail.
    x_receivers-rec_type = c_lu.
    x_receivers-com_type = c_int.
    x_receivers-notif_del = c_x.
    x_receivers-notif_ndel = c_x.

    APPEND x_receivers TO i_receivers.
    CLEAR: x_receivers,
           x_ladr6.
  ENDLOOP.

ENDFORM.                    "f_get_receivers

*&---------------------------------------------------------------------*
*&      Form  f_send_email.
*&---------------------------------------------------------------------*
*   creates the email: sender, reciever, and message body
*----------------------------------------------------------------------*
FORM f_send_email.

  DATA: v_lmtitle   TYPE sodocchgi1-obj_descr.
  CONSTANTS: c_lperiod TYPE c VALUE '.',
             c_lf      TYPE c VALUE 'F',
             c_lsaprpt TYPE string VALUE 'SAPRPT',
             c_lraw    TYPE string VALUE 'RAW'.

  CLEAR: v_lmtitle.

* Email Subject.
  CONCATENATE text-004 s_bukrs-low INTO v_lmtitle SEPARATED BY space.
  CONCATENATE v_lmtitle c_lperiod INTO v_lmtitle.
* Fill the document data.
  x_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
  x_doc_data-obj_langu = sy-langu.
  x_doc_data-obj_name  = c_lsaprpt.
  x_doc_data-obj_descr =  v_lmtitle.
  x_doc_data-sensitivty = c_lf.


* Describe the body of the message
  CLEAR i_packing_list.
  REFRESH i_packing_list.
  x_packing_list-transf_bin = space.
  x_packing_list-head_start = 1.
  x_packing_list-head_num = 0.
  x_packing_list-body_start = 1.

  DESCRIBE TABLE i_message LINES x_packing_list-body_num.

  x_packing_list-doc_type = c_lraw.
  APPEND x_packing_list TO i_packing_list.
  CLEAR x_packing_list.

* Add the recipients email address
  PERFORM f_get_receivers.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = x_doc_data
      put_in_outbox              = c_x
      commit_work                = c_x
    IMPORTING
      sent_to_all                = v_sent_all
    TABLES
      packing_list               = i_packing_list
      contents_txt               = i_message
      receivers                  = i_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc EQ 0.
    PERFORM f_mail_execute_program.
  ENDIF.


ENDFORM.                    "F_SEND_EMAIL

*&---------------------------------------------------------------------*
*&      Form  f_get_cc_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_BUKRS  text
*      <--P_V_TYPE  text
*----------------------------------------------------------------------*
FORM f_get_cc_type  USING    pi_bukrs
                    CHANGING po_type.

  SELECT SINGLE type INTO po_type
                     FROM t9fbm005
                    WHERE bukrs EQ pi_bukrs.
  IF sy-subrc EQ 0.
    "Do nothing
  ENDIF.
ENDFORM.                    " f_get_cc_type

*&---------------------------------------------------------------------*
*&      Form  show_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_log .
  CONSTANTS: c_lerror TYPE c VALUE 'E',
             c_lok    TYPE c VALUE 'S',
             c_lx     TYPE c VALUE 'X',
             c_lsplit TYPE c VALUE ';'.
  DATA: v_lline(255) TYPE c.
  WRITE / text-hd1.
  CLEAR: x_log.
  LOOP AT i_log INTO x_log.
    CASE x_log-type.
      WHEN c_lerror.
        CONCATENATE x_log-vbeln x_log-bukrs x_log-group
                    x_log-message INTO v_lline
                                  SEPARATED BY c_lsplit.
        WRITE / v_lline.
      WHEN c_lok.
        CONCATENATE x_log-vbeln x_log-bukrs x_log-belnr
                    x_log-blart x_log-message INTO v_lline
                                              SEPARATED BY c_lsplit.
        WRITE / v_lline.
      WHEN c_lx.
        CONCATENATE x_log-vbeln
                    x_log-posnr
                    x_log-message INTO v_lline
                                  SEPARATED BY c_lsplit.
        FORMAT COLOR COL_NEGATIVE.
        WRITE / v_lline.
        FORMAT COLOR COL_NEGATIVE OFF.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " show_log
