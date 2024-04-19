*&---------------------------------------------------------------------*
*& Report  Z_SAPDS_EXTRACT_VBPA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_SAPDS_EXTRACT_VBPA message-id 26 line-size 255
no standard page heading.
* Z_SAPDS_Extract_VBPA.
PARAMETER DOWNLOAD(1) DEFAULT 'S' lower case. "N-svr,Y-clnt
PARAMETER EXECMODE(1) DEFAULT 'D' lower case. "D-dlg,B-btch
PARAMETER OUT_DIR(230) DEFAULT "output file dir
'\\chobisdv\' lower case.
PARAMETER IN_DIR(230) DEFAULT "input file dir
'\\chobisdv\' lower case.
PARAMETER P_DEST TYPE RFCDES-RFCDEST DEFAULT
'NONE'.
PARAMETER P_PROGID TYPE RFCOPT-RFCEXEC DEFAULT
SPACE.
PARAMETER P_GWHOST TYPE RFCOPT-RFCGWHOST DEFAULT
SPACE.
PARAMETER P_GWSERV TYPE RFCOPT-RFCGWSERV DEFAULT
SPACE.
PARAMETER P_SRVFM(30) DEFAULT "Server Callback function
SPACE.
PARAMETER P_PKGSZ TYPE I DEFAULT "Stream package size
5000.
PARAMETER P_SNC_ON(1) DEFAULT "X-on SPACE-off
SPACE.

PARAMETER $PARAM1 TYPE D.

PARAMETER $PARAM2 TYPE D.

PARAMETER P_DF_VK(40) DEFAULT "ABAP data flow object key
'' lower case.
PARAMETER P_DI_GEN(40) DEFAULT "DI version that generated ABAP
'' lower case.

*** Machine generated ABAP. Do not modify.            ***
*** (C)Copyright Business Objects S.A.  All rights reserved. ***
*
* Date Time:
*    08/18/23 09:44:07
* SAP used for generated this ABAP:
*    Release: 702
*    Host   : chobisdv
*
* ABAP Dataflow Name:
*    Z_SAPDS_Extract_VBPA
* ABAP program name in SAP:
*  Z_SAPDS_EXTRACT_VBPA
* Generated ABAP file name:
*    D:/temp/Z_SAPDS_Extract_VBPA
TABLES VBAK.
TABLES VBPA.

DATA: begin of ITAB3 occurs 0,
MANDT(3) TYPE C,
VBELN(10) TYPE C,
POSNR(6) TYPE N,
PARVW(2) TYPE C,
KUNNR(10) TYPE C,
LIFNR(10) TYPE C,
PERNR(8) TYPE N,
PARNR(10) TYPE N,
ADRNR(10) TYPE C,
ABLAD(25) TYPE C,
LAND1(3) TYPE C,
ADRDA(1) TYPE C,
XCPDK(1) TYPE C,
HITYP(1) TYPE C,
PRFRE(1) TYPE C,
BOKRE(1) TYPE C,
HISTUNR(2) TYPE N,
KNREF(30) TYPE C,
LZONE(10) TYPE C,
HZUOR(2) TYPE N,
STCEG(20) TYPE C,
PARVW_FF(1) TYPE C,
ADRNP(10) TYPE C,
KALE(1) TYPE C.
DATA: end of ITAB3.

data: append_flag(1) value ' ',
      cntbuf type i,
      delimleng type i,last_batch(1) value ' '.

CONSTANTS C_DF_VK(40) VALUE '528'.
CONSTANTS C_DI_GEN(40) VALUE '14.2.14.3160'.
DATA WARN_MSG(50).


start-of-selection.


  IF DOWNLOAD = 'S' OR
     DOWNLOAD = 'N' OR
     DOWNLOAD = 'Y'.
      .
  ELSE.
     DATA: m_xfer_err_msg(700).
     CONCATENATE
'ABAP program does not recognize this new '
'data transfer method: ' DOWNLOAD
'. Regenerate the ABAP program and upload to this system.'
     INTO m_xfer_err_msg.
     MESSAGE  E240(S#) WITH m_xfer_err_msg.
  ENDIF.

  IF EXECMODE = 'B' OR
     EXECMODE = 'D'.
      .
  ELSE.
     DATA: m_exec_err_msg(700).
     CONCATENATE
'ABAP program does not recognize this new '
'execution option: ' EXECMODE
'. Regenerate the ABAP program and upload to this system.'
     INTO m_exec_err_msg.
     MESSAGE  E240(S#) WITH m_exec_err_msg.
  ENDIF.

  IF DOWNLOAD = 'S'.
     PERFORM CONNECT_RFCDEST_TO_PROGID.
  ENDIF.



PERFORM FORM3.
last_batch = 'X'.
PERFORM FORM4.
FREE ITAB3.
  IF DOWNLOAD = 'S'.
     PERFORM DISCONNECT_RFCDEST_FROM_PROGID.
  ENDIF.


end-of-selection.

CLEAR WARN_MSG.

IF NOT P_DF_VK IS INITIAL.
  IF P_DF_VK <> C_DF_VK.
     CONCATENATE '$$Warning$'
                 C_DF_VK
                 '$' INTO WARN_MSG.
  ENDIF.
ENDIF.
IF NOT P_DI_GEN IS INITIAL.
  IF P_DI_GEN <> C_DI_GEN.
     IF WARN_MSG IS INITIAL.
       CONCATENATE '$$Warning$$'
                   C_DI_GEN
                   INTO WARN_MSG.
     ELSE.
       CONCATENATE WARN_MSG
                   C_DI_GEN
                   INTO WARN_MSG.
     ENDIF.
  ENDIF.
ENDIF.

IF NOT WARN_MSG IS INITIAL.
  IF EXECMODE = 'D'.
    WRITE WARN_MSG.
    NEW-LINE.
  ELSE.
    MESSAGE S240(S#) with WARN_MSG.
  ENDIF.
ENDIF.


write  '* Program Complete *'.
write  '(C)Copyright Business Objects S.A.  All rights reserved.'.

FORM FORM3.
DATA ALTMP25(3) TYPE C.
DATA ALTMP26(10) TYPE C.
DATA ALTMP27(6) TYPE N.
DATA ALTMP28(2) TYPE C.
DATA ALTMP29(10) TYPE C.
DATA ALTMP30(10) TYPE C.
DATA ALTMP31(8) TYPE N.
DATA ALTMP32(10) TYPE N.
DATA ALTMP33(10) TYPE C.
DATA ALTMP34(25) TYPE C.
DATA ALTMP35(3) TYPE C.
DATA ALTMP36(1) TYPE C.
DATA ALTMP37(1) TYPE C.
DATA ALTMP38(1) TYPE C.
DATA ALTMP39(1) TYPE C.
DATA ALTMP40(1) TYPE C.
DATA ALTMP41(2) TYPE N.
DATA ALTMP42(30) TYPE C.
DATA ALTMP43(10) TYPE C.
DATA ALTMP44(2) TYPE N.
DATA ALTMP45(20) TYPE C.
DATA ALTMP46(1) TYPE C.
DATA ALTMP47(10) TYPE C.
DATA ALTMP48(1) TYPE C.

DATA VBPA9MANDT LIKE VBPA-MANDT.
DATA VBPA9VBELN LIKE VBPA-VBELN.
DATA VBPA9POSNR LIKE VBPA-POSNR.
DATA VBPA9PARVW LIKE VBPA-PARVW.
DATA VBPA9KUNNR LIKE VBPA-KUNNR.
DATA VBPA9LIFNR LIKE VBPA-LIFNR.
DATA VBPA9PERNR LIKE VBPA-PERNR.
DATA VBPA9PARNR LIKE VBPA-PARNR.
DATA VBPA9ADRNR LIKE VBPA-ADRNR.
DATA VBPA9ABLAD LIKE VBPA-ABLAD.
DATA VBPA9LAND1 LIKE VBPA-LAND1.
DATA VBPA9ADRDA LIKE VBPA-ADRDA.
DATA VBPA9XCPDK LIKE VBPA-XCPDK.
DATA VBPA9HITYP LIKE VBPA-HITYP.
DATA VBPA9PRFRE LIKE VBPA-PRFRE.
DATA VBPA9BOKRE LIKE VBPA-BOKRE.
DATA VBPA9HISTUNR LIKE VBPA-HISTUNR.
DATA VBPA9KNREF LIKE VBPA-KNREF.
DATA VBPA9LZONE LIKE VBPA-LZONE.
DATA VBPA9HZUOR LIKE VBPA-HZUOR.
DATA VBPA9STCEG LIKE VBPA-STCEG.
DATA VBPA9PARVW_FF LIKE VBPA-PARVW_FF.
DATA VBPA9ADRNP LIKE VBPA-ADRNP.
DATA VBPA9KALE LIKE VBPA-KALE.
DATA VBAK9AUDAT LIKE VBAK-AUDAT.
DATA VBAK9VBELN LIKE VBAK-VBELN.



SELECT
  VBPA9~MANDT
  VBPA9~VBELN
  VBPA9~POSNR
  VBPA9~PARVW
  VBPA9~KUNNR
  VBPA9~LIFNR
  VBPA9~PERNR
  VBPA9~PARNR
  VBPA9~ADRNR
  VBPA9~ABLAD
  VBPA9~LAND1
  VBPA9~ADRDA
  VBPA9~XCPDK
  VBPA9~HITYP
  VBPA9~PRFRE
  VBPA9~BOKRE
  VBPA9~HISTUNR
  VBPA9~KNREF
  VBPA9~LZONE
  VBPA9~HZUOR
  VBPA9~STCEG
  VBPA9~PARVW_FF
  VBPA9~ADRNP
  VBPA9~KALE
  VBAK9~AUDAT
  VBAK9~VBELN
into (VBPA9MANDT,
  VBPA9VBELN,
  VBPA9POSNR,
  VBPA9PARVW,
  VBPA9KUNNR,
  VBPA9LIFNR,
  VBPA9PERNR,
  VBPA9PARNR,
  VBPA9ADRNR,
  VBPA9ABLAD,
  VBPA9LAND1,
  VBPA9ADRDA,
  VBPA9XCPDK,
  VBPA9HITYP,
  VBPA9PRFRE,
  VBPA9BOKRE,
  VBPA9HISTUNR,
  VBPA9KNREF,
  VBPA9LZONE,
  VBPA9HZUOR,
  VBPA9STCEG,
  VBPA9PARVW_FF,
  VBPA9ADRNP,
  VBPA9KALE,
  VBAK9AUDAT,
  VBAK9VBELN)
FROM VBPA AS VBPA9
 INNER JOIN VBAK AS VBAK9
 ON ( VBAK9~VBELN = VBPA9~VBELN )
WHERE ( ( VBAK9~AUDAT < $PARAM2 )
 AND ( VBAK9~AUDAT >= $PARAM1 ) ).
ALTMP25 = VBPA9MANDT.
ALTMP26 = VBPA9VBELN.
ALTMP27 = VBPA9POSNR.
ALTMP28 = VBPA9PARVW.
ALTMP29 = VBPA9KUNNR.
ALTMP30 = VBPA9LIFNR.
ALTMP31 = VBPA9PERNR.
ALTMP32 = VBPA9PARNR.
ALTMP33 = VBPA9ADRNR.
ALTMP34 = VBPA9ABLAD.
ALTMP35 = VBPA9LAND1.
ALTMP36 = VBPA9ADRDA.
ALTMP37 = VBPA9XCPDK.
ALTMP38 = VBPA9HITYP.
ALTMP39 = VBPA9PRFRE.
ALTMP40 = VBPA9BOKRE.
ALTMP41 = VBPA9HISTUNR.
ALTMP42 = VBPA9KNREF.
ALTMP43 = VBPA9LZONE.
ALTMP44 = VBPA9HZUOR.
ALTMP45 = VBPA9STCEG.
ALTMP46 = VBPA9PARVW_FF.
ALTMP47 = VBPA9ADRNP.
ALTMP48 = VBPA9KALE.
 move ALTMP25 to ITAB3-MANDT.
 move ALTMP26 to ITAB3-VBELN.
 move ALTMP27 to ITAB3-POSNR.
 move ALTMP28 to ITAB3-PARVW.
 move ALTMP29 to ITAB3-KUNNR.
 move ALTMP30 to ITAB3-LIFNR.
 move ALTMP31 to ITAB3-PERNR.
 move ALTMP32 to ITAB3-PARNR.
 move ALTMP33 to ITAB3-ADRNR.
 move ALTMP34 to ITAB3-ABLAD.
 move ALTMP35 to ITAB3-LAND1.
 move ALTMP36 to ITAB3-ADRDA.
 move ALTMP37 to ITAB3-XCPDK.
 move ALTMP38 to ITAB3-HITYP.
 move ALTMP39 to ITAB3-PRFRE.
 move ALTMP40 to ITAB3-BOKRE.
 move ALTMP41 to ITAB3-HISTUNR.
 move ALTMP42 to ITAB3-KNREF.
 move ALTMP43 to ITAB3-LZONE.
 move ALTMP44 to ITAB3-HZUOR.
 move ALTMP45 to ITAB3-STCEG.
 move ALTMP46 to ITAB3-PARVW_FF.
 move ALTMP47 to ITAB3-ADRNP.
 move ALTMP48 to ITAB3-KALE.
 append ITAB3.
 cntbuf = cntbuf + 1.
 if download = 'N'.
  if cntbuf > 5000.
    perform FORM4.
    clear cntbuf.
    refresh ITAB3.
    append_flag = 'A'.
  endif.
 endif.
 if download = 'S'.
  if cntbuf > P_PKGSZ.
    perform FORM4.
    clear cntbuf.
    refresh ITAB3.
  endif.
 endif.
ENDSELECT.
ENDFORM.

FORM FORM4.
data: outfile(512), ldfile(50).
ldfile = 'Z_SAPDS_Extract_VBPA'.
concatenate out_dir ldfile into outfile
  separated by '/'.
  IF DOWNLOAD = 'S'.
     DATA: error_message(700),mtext(800),iEOP(1).
     CALL FUNCTION P_SRVFM
          DESTINATION P_DEST
          KEEPING LOGICAL UNIT OF WORK
          EXPORTING
            EOS = last_batch
          IMPORTING
            EOP = iEOP
          TABLES
            E_TABLE = ITAB3
          EXCEPTIONS
            READ_ERROR = 1
            SYSTEM_FAILURE = 2
            MESSAGE error_message
            COMMUNICATION_FAILURE = 3
            MESSAGE error_message
            OTHERS = 4.

     IF sy-subrc ne 0.
        Case sy-subrc.
        when 1.
        CONCATENATE
        'Data Services read error. '
        'Check Data Services error log.'
        INTO mtext.
        MESSAGE  E240(S#) WITH mtext.
        when 2.
        CONCATENATE
'SAP System Failure while calling DS remote function: '
      error_message INTO mtext.
        MESSAGE  E240(S#) WITH mtext.
        when 3.
        CONCATENATE
'SAP System Failure while calling DS remote function: '
      error_message INTO mtext.
        MESSAGE  E240(S#) WITH mtext.
        when 4.
        MESSAGE  E240(S#) WITH
'Other SAP System Failure while calling DS remote function.'.
        endcase.
      ENDIF.
      IF iEOP = 'X'.
         PERFORM DISCONNECT_RFCDEST_FROM_PROGID.
         LEAVE PROGRAM.
       ENDIF.
  ELSE.
data  dlmtlen type i value '1'.
data xdlmtlen type i value '1'.
data:
  ht(1) type c,
  xht(1) type x,
  conv type ref to cl_abap_conv_in_ce.
xht = '7F'.
conv = cl_abap_conv_in_ce=>create(
  encoding = '1100'
  input = xht
).
call method conv->read(
  exporting n    = xdlmtlen
  importing data = ht
            len = dlmtlen
).
data return_code type i.
 perform write_delimited_file
           tables   ITAB3
           using    outfile
                    append_flag
                    ht
                    dlmtlen
                    download
           changing return_code.

  case return_code.
    when 1.
      IF EXECMODE = 'D'.
        WRITE: /5 'No line selected'.
      ELSE.
        MESSAGE E047(S#).
      ENDIF.
    when 2.
      IF EXECMODE = 'D'.
        WRITE: /5 'Open File Error -- ', 25 OUTFILE.
      ELSE.
        MESSAGE  E084(E0) WITH OUTFILE.
      ENDIF.
    when 3.
      IF EXECMODE = 'D'.
        WRITE: /5 'Data exceed length limit (8192) '.
      ELSE.
        MESSAGE  E240(S#) WITH
             'Data exceed length limit (8192) '.
      ENDIF.
    when 4.
      IF EXECMODE = 'D'.
        WRITE: /5 'Call function WS_DOWNLOAD error'.
      ELSE.
        MESSAGE  E240(S#) WITH
             'Call function WS_DOWNLOAD error'.
      ENDIF.
  endcase.
 ENDIF.
ENDFORM.

FORM SUBSTRING USING SRC BEG LEN CHANGING RET.

DATA: VA1 TYPE I.
DATA: VA2 TYPE I.
DATA: VA3 TYPE I.

VA3 = STRLEN( SRC ).

IF  BEG = 0.   VA1 = 0.
ELSE.
  IF  BEG < 0.
    VA1 = VA3 + BEG.
    IF  VA1 < 0.   VA1 = 0.
    ENDIF.
  ELSE.          VA1 = BEG - 1.
  ENDIF.
ENDIF.

IF  LEN < 0.   VA2 = 0.
ELSE.          VA2 = VA3 - VA1.
ENDIF.

IF  VA2 > LEN. VA2 = LEN.
ENDIF.

IF  VA2 < 1.   MOVE ''           TO RET.
ELSE.          MOVE SRC+VA1(VA2) TO RET.
ENDIF.

ENDFORM.

form write_delimited_file
           tables   datatab
           using    file
                    append
                    delimit
                    dlength
                    dwnload
          changing rc.

  data: type1,
        appd(1),
        temp(32),
        time1(8),
        date1(10),
        output(8192),
        rcount type i,
        offset type i,
        tablen type i,
        maxlen type i value '8192'.

  data: begin of clientab occurs 0,
             output(8192),
          end of clientab.

  field-symbols: <f>.
  field-symbols <delim1>.
  data delim2(16).
  data l_filename type string.

  appd = append.
  if appd is not initial.
     appd = 'X'.
  endif.
  move file to l_filename.
  describe table datatab lines tablen.


  if dwnload = 'Y'.
     clear clientab. refresh clientab.
     rcount = 0.
  else.
     if appd = space.
   open dataset file for output in text mode ENCODING UTF-8.
     else.
    open dataset file for appending in text mode ENCODING UTF-8.
     endif.
     if sy-subrc <> 0.
         rc = 2. exit.
     endif.
  endif.

  loop at datatab.
    clear: tablen, offset, output.
    do.
      assign component sy-index of
         structure datatab to <f>.
      if sy-subrc <> 0. exit. endif.
      if sy-index > 1.
         assign delimit(dlength) TO <delim1> CASTING TYPE C.
         delim2 = <delim1>.
         write delim2(dlength) to output+offset(dlength).
         add dlength to offset.
      endif.

      describe field <f> type type1.

      if type1 = 'I' or type1 = 'N'.
          type1 = 'P'.
      endif.

      case type1.
        when 'D'.
          if <f> = '00000000'.
             <f> = ' '.
          else.
             move <f> to time1.
             assign time1 to <f>.
          endif.
        when 'F'.
          if <f> = '0.0'.
            temp = '0.0'.
          else.
             write <f> to temp exponent 0.
          endif.
          condense temp no-gaps.
          translate temp using ',.'.
          assign temp to <f>.
        when 'P'.
          if <f> < 0.
             write '-' to output+offset(1).
             add 1 to offset.
             <f> = <f> * ( -1 ).
          endif.
          move <f> to temp.
          condense temp no-gaps.
          translate temp using ',.'.
          assign temp to <f>.
      endcase.

      sy-fdpos = strlen( <f> ).

      tablen = offset + sy-fdpos.
      if tablen > maxlen.
         rc = 3. exit.
      endif.
      write <f> to output+offset(sy-fdpos).
      add sy-fdpos to offset.
    enddo.

    if dwnload = 'Y'.
       clientab-output = output.
       append clientab.
       rcount = rcount + 1.
       if rcount >= 50.
          SY-BATCH = SPACE.
          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              FILENAME = l_filename
              FILETYPE = 'ASC'
              CODEPAGE = '4110'
              APPEND   = appd
              WRITE_FIELD_SEPARATOR = 'X'
*            IMPORTING
*              FILELENGTH =
            TABLES
              DATA_TAB = clientab
            EXCEPTIONS
              OTHERS = 1.
          if sy-subrc <> 0.
             rc = 4.
          endif.
          clear clientab. refresh clientab.
          rcount = 0. appd = 'A'.
       endif.
    else.
       transfer output to file.
    endif.
  endloop.

  if dwnload = 'Y'.
       SY-BATCH = SPACE.
       CALL FUNCTION 'GUI_DOWNLOAD'
         EXPORTING
           FILENAME = l_filename
           FILETYPE = 'ASC'
              CODEPAGE = '4110'
           APPEND   = appd
           WRITE_FIELD_SEPARATOR = 'X'
*         IMPORTING
*           FILELENGTH =
         TABLES
           DATA_TAB = clientab
         EXCEPTIONS
           OTHERS = 1.
          if sy-subrc <> 0.
             rc = 4.
          endif.
  else.
       close dataset file.
  endif.
endform.

FORM CONNECT_RFCDEST_TO_PROGID.
 INCLUDE rfctypes.

 DATA: len     type i,
       R3NAME(4),
       SYSTNR(2),
       uid     LIKE SYS_UID,
       options LIKE RFCOPT,
       isunicode  TYPE n.

 DATA: NTOTAL     LIKE GWY_STRUCT-NOREG,
       GWY_GWHOST LIKE GWY_STRUCT-GWHOST,
       GWY_GWSERV LIKE GWY_STRUCT-GWSERV,
       GWY_TPNAME LIKE GWY_SYSTEM-TPNAME.

 TABLES: RFCSI.

* Check program ID
 IF P_PROGID = SPACE.
    RAISE INVALID_PROGRAM_ID.
 ENDIF.

* determine if the RFC destination authority
  CALL FUNCTION 'RFC_READ_TCPIP_DESTINATION'
       EXPORTING destination = P_DEST
                 authority_check = 'X'
  IMPORTING rfcunicode = isunicode.

* Use current gateway if no info exits
 IF P_GWHOST = SPACE OR P_GWSERV = SPACE.
  CALL FUNCTION 'RFC_SYSTEM_INFO'
      IMPORTING
          RFCSI_EXPORT = RFCSI.

  len = strlen( rfcsi-rfcdest ) - 2.
  systnr = rfcsi-rfcdest+len.
  len = len - 1 - 3.
  r3name = rfcsi-rfcdest+len(3).
  len = len - 1.
  options-rfcgwhost = rfcsi-rfcdest(len).
  CONCATENATE 'sapgw' SYSTNR INTO options-rfcgwserv.
 ELSE.
   options-rfcgwhost = P_GWHOST.
   options-rfcgwserv = P_GWSERV.
 ENDIF.

* Parameters for GWY function call
  GWY_GWHOST = OPTIONS-RFCGWHOST.
  GWY_GWSERV = OPTIONS-RFCGWSERV.
  GWY_TPNAME = P_PROGID.

  DO 10 TIMES.

* Check gateway and server program registered
  CALL FUNCTION 'GWY_GET_NO_REG_PROGRAMS'
     EXPORTING
        GWHOST      = GWY_GWHOST
        GWSERV      = GWY_GWSERV
        TPNAME      = GWY_TPNAME
     IMPORTING
        NOREG_TOTAL = NTOTAL
     EXCEPTIONS
        OTHERS      = 1.

  IF sy-subrc NE 0.
     raise CONNECT_TO_GATEWAY_FAILED.
  ENDIF.

  IF NTOTAL GT 0.
     EXIT.
  ENDIF.

  CALL FUNCTION 'ENQUE_SLEEP'
     EXPORTING
        SECONDS     = 15.

  ENDDO.

  IF NTOTAL = 0.
     raise SERVER_NOT_REGISTERED.
  ENDIF.

  IF NTOTAL GT 1.
     raise DUPLICATE_REG_PROGRAMS.
  ENDIF.

* build new connection to a registered server
  options-rfcexec   = gwy_tpname.
  options-rfcgwhost = gwy_gwhost.
  options-rfcgwserv = gwy_gwserv.
  options-rfchost   = '%%RFCSERVER%%'.
  IF P_SNC_ON = 'X'.
    options-rfcsnc  = 'X'.
  ENDIF.

  CALL 'RFCControl' ID 'CODE' FIELD 'O'
                    ID 'DESTINATION' FIELD P_DEST
                    ID 'TYPE' FIELD rfctype_external_tcp
                    ID 'OPTIONS' FIELD options.

  IF sy-subrc NE 0.
     RAISE CONNECT_TO_REG_SERVER_FAILED.
   ENDIF.

* and set exclusive mode to keep server owned
  CALL FUNCTION 'SYSTEM_SET_REG_SERVER_PROPERTY'
      EXPORTING  destination = P_DEST
                 exclusiv    = 'Y'
      EXCEPTIONS connect_to_reg_server_failed = 1
                 exclusiv_not_supported       = 2.

  IF SY-SUBRC NE 0.
     CASE SY-SUBRC.
          WHEN 1.
              RAISE CONNECT_TO_REG_SERVER_FAILED.
          WHEN 2.
              RAISE EXCLUSIV_NOT_SUPPORTED.
     ENDCASE.
  ENDIF.

ENDFORM.

FORM DISCONNECT_RFCDEST_FROM_PROGID.
* set exclusive mode to E to end the session
  CALL FUNCTION 'SYSTEM_SET_REG_SERVER_PROPERTY'
     EXPORTING
         destination = P_DEST
         exclusiv    = 'E'
     EXCEPTIONS
         connect_to_reg_server_failed = 1
         exclusiv_not_supported       = 2.
ENDFORM.
