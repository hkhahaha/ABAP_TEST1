&---------------------------------------------------------------------*
& Report YTEST003
&---------------------------------------------------------------------*
&
&---------------------------------------------------------------------*
REPORT ytest003.
WRITE '测试的 那日饿哦那个啦安红豆i阿什顿'.
WRITE  /.
WRITE  /.
WRITE  /  TEXT-001.
WRITE /.
DATA:MSG1(20) TYPE C VALUE '测试'.
DATA:MSG2(20) TYPE C VALUE 'ABAP'.
DATA:MSG3(20) TYPE C VALUE '消息！'.

MESSAGE I006(YTEST003) WITH MSG1 MSG2 MSG3.

DATA:BEGIN OF man,
       name(30) TYPE c,
       high     TYPE p DECIMALS 2,
       weight   TYPE p DECIMALS 2,
     END OF man.
FIELD-SYMBOLS <fsa> LIKE man.
DATA man1 LIKE man.
man1-name = '张林'.
man1-high = '1.78'.
man1-weight = 140.
ASSIGN man1 TO <fsa>.
WRITE: / <fsa>-name,
<fsa>-high,
<fsa>-weight.

DATA:BEGIN OF man,
       name(30) TYPE c,
       high     TYPE p DECIMALS 2,
       weight   TYPE p DECIMALS 2,
     END OF man.

FIELD-SYMBOLS <fsa> LIKE man.
DATA man1 LIKE man.

man1-name = '张三'.
man1-high = '1.78'.
man1-weight = '140'.

ASSIGN man1 TO <fsa>.
WRITE: / <fsa>-name,
       <fsa>-high,
       <fsa>-weight.
DATA: c1(2)  TYPE c,
      c2(2)  TYPE c,
      c3(2)  TYPE c,
      c4(2)  TYPE c,
      c5(20) TYPE c,
      c9(2)  TYPE c.

c1 = 'AB'.
c2 = 'CD'.
c3 = 'EF'.
c4 = 'GH'.
c9 = '+'.
CONCATENATE c1 c2 c3 c4 INTO c5.
WRITE c5.
CONCATENATE c1 c2 c3 c4 INTO c5 SEPARATED BY c9.
WRITE / c5.

DATA: c1(2)  TYPE c,
      c2(2)  TYPE c,
      c3(2)  TYPE c,
      c4(2)  TYPE c,
      c5(20) TYPE c VALUE '11 * 22 * 33 * 44',
      c9(2)  TYPE c.

c9 = '*'.
WRITE c5.
SPLIT c5 AT c9 INTO c1 c2 c3 c4.
WRITE: / c1, c2, c3, c4.
DATA: c1(2)  TYPE c,
      c2(2)  TYPE c,
      c3(2)  TYPE c,
      c4(2)  TYPE c,
      c5(20) TYPE c VALUE '11 * 22 * 33 * 44',
      c9(2)  TYPE c.
c9 = '*'.
WRITE c5.
SPLIT c5 AT c9 INTO c1 c2 c3 c4.
WRITE: / c1, c2, c3, c4.

WRITE: 'test1',/,'test2','test3'.
SKIP 5.
ULINE.
DATA:BEGIN OF man,
       name(20) TYPE c,
       high     TYPE p DECIMALS 2,
       weight   TYPE p DECIMALS 2,
     END OF man.

DATA:man1 LIKE TABLE OF man WITH HEADER LINE,
     man2 LIKE TABLE OF man.

man-name = '张三'.
man-high = '1.68'.
man-weight = '120'.
APPEND man TO man1.

man-name = '刘志'.
man-high = '1.78'.
man-weight = '160'.
APPEND man TO man1.

MOVE man1[] TO man2.
LOOP AT man2 INTO man.
  WRITE: / man-name,man-high,man-weight.
ENDLOOP.

LOOP AT man1.
  WRITE: / man1-name,man1-high,man1-weight.
ENDLOOP.
DATA: BEGIN OF man,
        name(20) TYPE c,
        high     TYPE p DECIMALS 2,
        weight   TYPE p DECIMALS 2,
      END OF man.
DATA:man1 LIKE HASHED TABLE OF man WITH UNIQUE KEY name.

man-name = '张三'.
man-high = '1.78'.
man-weight = '120'.
INSERT man INTO TABLE man1.

man-name = '李四'.
man-high = '1.68'.
man-weight = '130'.
INSERT man INTO TABLE man1.

LOOP AT man1 INTO man.
  WRITE: / man-name,man-high,man-weight.
ENDLOOP.

SORT man1 DESCENDING BY weight ASCENDING.
SKIP 2.
ULINE.

LOOP AT man1 INTO man.
  WRITE: / man-name,man-high,man-weight.
ENDLOOP.
DATA:BEGIN OF man,
       name(20) TYPE c,
       high     TYPE p DECIMALS 2,
       weight   TYPE p  DECIMALS 2,
     END OF man.
DATA:man1 LIKE HASHED TABLE OF man WITH UNIQUE KEY name.

man-name = '张三'.
man-high = '1.78'.
man-weight = 160.
INSERT man INTO TABLE man1.

man-name = '刘志'.
man-high = '170'.
INSERT man INTO TABLE man1.

LOOP AT man1 INTO man.
  WRITE: / man-name,man-high,man-weight.
ENDLOOP.

man-name = '张三'.
man-weight = 220.
man-high = '2.22'.
MODIFY TABLE man1 FROM man.

LOOP AT man1 INTO man.
  WRITE: / man-name,man-high,man-weight.
ENDLOOP.
DATA:BEGIN OF man,
       name(20) TYPE c,
       high     TYPE p DECIMALS 2,
       weight   TYPE p DECIMALS 2,
     END OF man.
DATA: man1 LIKE HASHED TABLE OF man WITH UNIQUE KEY name.

man-name = '张三'.
man-high = '1.68'.
man-weight = 120.
INSERT man  INTO TABLE man1.

man-name = '刘志'.
man-high = '1.78'.
man-weight = 160.
INSERT man INTO TABLE man1.

LOOP AT man1 INTO man.
  WRITE:/ man-name,man-high,man-weight.
ENDLOOP.

DELETE man1 WHERE name = '张三'.
SKIP 5.
ULINE.
LOOP AT man1 INTO man.
  WRITE:/ man-name,man-high,man-weight.
ENDLOOP.
DATA:BEGIN OF MAN,
  NAME(20) TYPE C,
  HIGH TYPE P DECIMALS 2,
  WEIGHT TYPE P DECIMALS 2,
  END OF MAN.
DATA:MAN1 LIKE TABLE OF MAN.

MAN-NAME = '张三'.
MAN-high = '1.68'.
MAN-weight = 120.
INSERT man  INTO TABLE MAN1.

MAN-name = '刘志'.
MAN-high = '1.78'.
MAN-weight = 160.
INSERT MAN INTO TABLE MAN1.

LOOP AT MAN1 INTO MAN.
  WRITE:/ MAN-NAME,MAN-high,MAN-weight.
ENDLOOP.

MAN-name = '李志'.
MAN-high = '1.58'.
MAN-weight = 110.
INSERT MAN INTO MAN1 INDEX 2.
ULINE.

LOOP AT MAN1 INTO MAN.
  WRITE:/ MAN-NAME,man-high,MAN-weight.
ENDLOOP.
DATA WA LIKE SPFLI.
WRITE: /.
WRITE:'航班承运人',20'航班连接',60'国家代码',80'起飞城市',100'起飞机场'.
SELECT * INTO WA FROM SPFLI.
  WRITE:/ WA-CARRID UNDER '航班承运人',
          WA-CONNID UNDER '航班连接',
          WA-COUNTRYFR UNDER '国家代码',
          WA-CITYFROM UNDER '起飞城市',
          WA-AIRPFROM UNDER '起飞机场'.

  ENDSELECT.
DATA:BEGIN OF MAN,
  NAME(20) TYPE C,
  HIGH TYPE P DECIMALS 2,
  WEIGHT TYPE P DECIMALS 2,
  END OF MAN.
DATA:MAN1 LIKE TABLE OF MAN.
DATA: NAME TYPE RLGRAP-FILENAME, TYPA TYPE RLGRAP-FILETYPE.

MAN-NAME = '张三'.
MAN-high = '1.68'.
MAN-weight = 120.
INSERT MAN INTO TABLE MAN1.

MAN-NAME = '刘志'.
MAN-high = '1.78'.
MAN-weight = 160.

INSERT MAN  INTO TABLE  MAN1.
MAN-NAME = 'adasd'.
MAN-high = '123.3'.
MAN-weight = 110.
INSERT MAN INTO MAN1 INDEX 2.

NAME = 'C:\Users\huangkai\Desktop'.
TYPA = 'DAT'.
CALL FUNCTION 'DOWNLOAD'
  EXPORTING
    CODEPAGE = 'TESTA'
    FILENAME = NAME
    FILETYPE = TYPA
    ITEM  = '文件测试'
  TABLES
    DATA_TAB = MAN1
  EXCEPTIONS
    INVALID_FILESIZE = 1
    INVALID_TABLE_WIDTH = 2
    INVALID_TYPE = 3
    NO_BATCH = 4
    UNKNOWN_ERROR = 5
    GUI_REFUSE_FILETRANSFER = 6
    OTHERS  = 7.
 IF SY-SUBRC <> 0.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
 ENDIF.

TYPES:BEGIN OF MAN,
  FIELD1 TYPE C LENGTH 5,
  FIELD2 TYPE C LENGTH 4,
  FIELD3 TYPE I,
  END OF MAN.
  TYPES T_TAB TYPE STANDARD TABLE OF MAN WITH NON-UNIQUE DEFAULT KEY.
  DATA gt_itab TYPE T_TAB WITH HEADER LINE.
  gt_itab-FIELD1 = 'ENJOY'.
  gt_itab-FIELD2 = 'ABAP'.
  gt_itab-FIELD3 = '1'.
  APPEND gt_itab.
  READ TABLE gt_itab INDEX 1.
  WRITE:/ gt_itab-FIELD1,gt_itab-FIELD2,gt_itab-FIELD3.

DATA:BEGIN OF GS_LINE,
  COL1 TYPE C,
  COL2 TYPE I,
  END OF GS_LINE.
DATA GT LIKE STANDARD TABLE OF GS_LINE WITH NON-UNIQUE KEY COL1.
DATA GT2 LIKE SORTED TABLE OF GS_LINE WITH NON-UNIQUE KEY COL1.
DATA: BEGIN OF GS,
  COL1 TYPE C,
BREAK-POINT.
WRITE '内容'.

DATA: BEGIN OF GS,
DATA: BEGIN OF GS,
  COL1(3) TYPE C,
  COL2(2) TYPE N,
  COL3 TYPE I,
  END OF GS.
DATA GS2 LIKE STANDARD TABLE OF GS WITH NON-UNIQUE KEY COL1.
GS-COL1 = 'AA'.
GS-COL2 = '17'.
GS-COL3 = 660.
COLLECT GS INTO GS2.
GS-COL1 = 'AL'.
GS-COL2 = '34'.
GS-COL3 = 220.
COLLECT GS INTO GS2.
GS-COL1 = 'AA'.
GS-COL2 = '18'.
GS-COL3 = 280.
COLLECT GS INTO GS2.

LOOP AT GS2 INTO GS.
  WRITE:/ GS-COL1,
          GS-COL2,
          GS-COL3.
ENDLOOP.
DATA:BEGIN OF man,
       name(20) TYPE c,
       high     TYPE p DECIMALS 2,
       weight   TYPE p DECIMALS 2,
     END OF man.
DATA: man1 LIKE TABLE OF man.

CALL FUNCTION 'WS_UPLOAD'
  EXPORTING
    codepage                = 'TEST'
    filename                = 'C:\Users\huangkai\Desktop\TEST1.TXT'
    filetype                = 'DAT'
    item                    = '读放文件'
  TABLES
    data_tab                = man1
  EXCEPTIONS
    conversion_error        = 1
    invalid_table_width     = 2
    invalid_type            = 3
    no_batch                = 4
    unknown_error           = 5
    gui_refuse_filetransfer = 6
    OTHERS                  = 7.
IF sy-subrc <> 0.
ENDIF.

LOOP AT man1 INTO man.
  WRITE: / man-name,
           man-high,
           man-weight.
ENDLOOP.
DATA:S(10)  TYPE C VALUE 'AABBCCDDEE'.
WRITE '接着的字符串__________将被替换。'.
WRITE AT  6(12) S.
DATA: str1(10) TYPE c VALUE 'AABBCCDDEE'.
WRITE '接着的字符串____________将被替换.'.
WRITE AT 14(10) str1.Z
DATA a_spfli TYPE spfli.
DATA ta_spfli TYPE TABLE OF spfli WITH HEADER LINE.
ULINE.
WRITE / '使用工作区'.
ULINE.
SELECT * INTO CORRESPONDING FIELDS OF a_spfli FROM spfli UP TO 5 ROWS.
  WRITE: / a_spfli-connid,a_spfli-carrid,a_spfli-cityfrom,a_spfli-cityto.
ENDSELECT.
ULINE.
WRITE / '使用内表'.
ULINE.
SELECT * INTO CORRESPONDING FIELDS OF
  TABLE ta_spfli FROM spfli UP TO 5 ROWS.
LOOP AT ta_spfli.
  WRITE: / a_spfli-connid,a_spfli-carrid,a_spfli-cityfrom,a_spfli-cityto.
ENDLOOP.
ULINE.
WRITE / '将内表数据转移至工作区'.
ULINE.
LOOP AT ta_spfli INTO a_spfli.
  WRITE: / a_spfli-connid,a_spfli-carrid,a_spfli-cityfrom,a_spfli-cityto.
ENDLOOP.
DATA WA_SPFLI TYPE TABLE OF spfli WITH HEADER LINE.
SELECT * INTO TABLE WA_SPFLI FROM spfli PACKAGE SIZE 4.
LOOP AT WA_SPFLI.
  WRITE: / WA_SPFLI-carrid,WA_SPFLI-cityfrom,WA_SPFLI-carrid.
ENDLOOP.
ULINE.
ENDSELECT.

DATA ytcity TYPE TABLE OF ytcity WITH HEADER LINE.
SELECT * INTO TABLE ytcity FROM ytcity.

LOOP AT ytcity.
  WRITE: / ytcity-yct_id,ytcity-yct_name,ytcity-yct_cuntry.
ENDLOOP.

DATA:BEGIN OF ARCD,
  CARRID TYPE spfli-carrid,
  CONNID TYPE spfli-CONNID,
  FLDATE TYPE SFLIGHT-FLDATE,
  END OF ARCD.
DATA ARCD1 LIKE ARCD.
SELECT spfli~carrid spfli~CONNID SFLIGHT~FLDATE INTO ARCD1 FROM spfli INNER JOIN SFLIGHT
  ON spfli~carrid = SFLIGHT~carrid
    AND spfli~CONNID = SFLIGHT~CONNID UP TO 10 ROWS.
  WRITE: / ARCD1-carrid,ARCD1-CONNID,ARCD1-FLDATE.
ENDSELECT.

WRITE:'123','345','456'.
TABLES SPFLI.
SKIP.



WRITE : / SY-VLINE,(15)'航线承运人',SY-VLINE,(15) '航班连接',SY-VLINE,(15)'国家代码',
          SY-VLINE,(15)'起飞城市',SY-VLINE,(15)'起飞机场',SY-VLINE.


NEW-PAGE LINE-COUNT 10.
SELECT * FROM SPFLI.
WRITE: / SY-VLINE,(15) SPFLI-CARRID,SY-VLINE,(15)SPFLI-CONNID,SY-VLINE,(15)SPFLI-COUNTRYTO,
         SY-VLINE,(15) SPFLI-CITYFROM,SY-VLINE,(15) SPFLI-AIRPFROM,SY-VLINE.


ENDSELECT.
FORMAT COLOR COL_HEADING.
WRITE 'HELLO WORLD'.
WRITE 'WELCOME!'.
FORMAT COLOR COL_HEADING INVERSE ON.
WRITE / 'HELLO WORLD'.
WRITE 'WELCOME'.
TABLES SPFLI.
SKIP.
FORMAT COLOR COL_HEADING.
ULINE AT /(91).
TABLES SPFLI.
SKIP.
FORMAT COLOR COL_HEADING.
ULINE AT /(91).
WRITE: / SY-VLINE,(15)'航线承运人',SY-VLINE,(15)'航班连接',SY-VLINE,
      (15)'国家代码',SY-VLINE,(15)'起飞城市',SY-VLINE,
      (15)'起飞机场',SY-VLINE.
ULINE AT /(91).
FORMAT COLOR OFF.
SELECT * FROM SPFLI.
   IF  SPFLI-CARRID = 'LH'.
     FORMAT COLOR COL_NEGATIVE.
   ELSE.
     FORMAT COLOR OFF.
   ENDIF.
WRITE: / SY-VLINE,(15) SPFLI-CARRID,SY-VLINE,(15) SPFLI-CONNID,SY-VLINE,
      (15)SPFLI-COUNTRYTO,SY-VLINE,(15)SPFLI-CITYFROM,SY-VLINE,(15) SPFLI-AIRPFROM,SY-VLINE.
  ULINE AT /(91).
ENDSELECT.
START-OF-SELECTION.
  WRITE '请按'.
  FORMAT HOTSPOT ON COLOR 6 INVERSE ON.
  WRITE '热点'.
  FORMAT HOTSPOT OFF COLOR OFF.
  AT LINE-SELECTION.
    WRITE 'WELCOME'.
START-OF-SELECTION.
WRITE:'初始列表,SY-LSIND=',SY-LSIND.

AT LINE-SELECTION.
  IF SY-LSIND = 1.
    WRITE: '第二列表，SY-LSIND =',SY-LSIND.
  ENDIF.
START-OF-SELECTION.
SET PF-STATUS'STA1'.
WRITE 'TEST'.
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'OWNSL'.
      WRITE:'已经选择'.
  ENDCASE.
START-OF-SELECTION.
  SET PF-STATUS 'STA1'.
  WRITE:'测试工具条按钮'.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'OWNSEL'.
      IF sy-lsind = 1.
        SET PF-STATUS 'STA6'.
      ENDIF.
      WINDOW STARTING AT 10 10 ENDING AT 30 20.
      WRITE '在子窗口输出！'.
      WRITE:'请注意'.
ENDCASE.
TABLES:spfli,sflight.

START-OF-SELECTION.
  SKIP.

  ULINE AT /(91).
  WRITE: / sy-vline,(15)'航线承运人',sy-vline,(15)'航班连接',sy-vline,
          (15)'国家代码',sy-vline,(15)'起飞城市',sy-vline,
          (15)'起飞机场',sy-vline.
  ULINE AT /(91).
  SELECT * FROM spfli.
    WRITE: / sy-vline,(15) spfli-carrid,sy-vline,
                      (15) spfli-connid,sy-vline,
                      (15) spfli-countryto,sy-vline,
                      (15) spfli-cityfrom,sy-vline,
                      (15) spfli-airpfrom,sy-vline.
    HIDE:spfli-carrid,spfli-connid.
    ULINE AT /(91).
  ENDSELECT.

AT LINE-SELECTION.
  IF sy-lsind = 1.
    SELECT * FROM sflight
      WHERE carrid = spfli-carrid AND connid = spfli-connid.
      WRITE: / sflight-carrid,sy-vline,sflight-connid,sy-vline,
               sflight-fldate,sy-vline,sflight-price.
    ENDSELECT.
  ENDIF.
TABLES:spfli.
DATA:sela(1) TYPE c,
     numl    TYPE i.

START-OF-SELECTION.
  SKIP.
  ULINE AT /(95).
  WRITE: / sy-vline,'',sy-vline,  (15) '航线承运人',sy-vline,
          (15)'航班连接',sy-vline, (15) '国家代码', sy-vline,
          (15)'起飞城市',sy-vline, (15)'起飞机场',sy-vline.
  ULINE AT /(95).
  SELECT * FROM spfli.
    WRITE: / sy-vline,sela AS CHECKBOX,sy-vline,(15)spfli-carrid,
          sy-vline,(15)spfli-connid,sy-vline,(15)spfli-countryto,
          sy-vline,(15)spfli-cityfrom,sy-vline,(15)spfli-airpfrom,sy-vline.
    HIDE:spfli-carrid,spfli-connid,spfli-countryto,spfli-cityfrom,spfli-airpfrom.
    ULINE AT /(95).
    HIDE sela.
  ENDSELECT.

END-OF-SELECTION.
  numl = sy-lsind - 1.

AT LINE-SELECTION.
  IF sy-lsind = 1.
    DO numl TIMES.
      READ LINE sy-index FIELD VALUE sela.
      IF sela = 'X'.
        WRITE: / spfli-carrid,spfli-connid,spfli-countryto,spfli-cityfrom,spfli-airpfrom.
      ENDIF.
    ENDDO.
  ENDIF.
PARAMETERS: A1(10) TYPE C,
            A2     TYPE I.
WRITE: / A1,/ A2.
DATA DATA1(20) TYPE C.
SELECT-OPTIONS D1 FOR DATA1.
LOOP AT D1.
  WRITE: /'SIGN:', D1-SIGN,
          'OPTIONS:', D1-OPTION,
          'LOW:',D1-LOW,
          'HIGH:',D1-HIGH.
ENDLOOP.

  DATA:ok_code TYPE sy-ucomm,
       save_ok LIKE ok_code.
  DATA sp1 LIKE TABLE OF spfli WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE sp1 FROM spfli.


  CALL SCREEN 100.
MODULE user_command_0100 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS1'.
ENDMODULE.
DATA wa_spfli LIKE TABLE OF spfli WITH HEADER LINE.
SELECT * INTO TABLE wa_spfli FROM spfli.
CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
  EXPORTING
    i_structure_name = 'SPFLI'
  TABLES
    t_outtab         = wa_spfli
  EXCEPTIONS
    program_error    = 1
    OTHERS           = 2.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

DATA: S1 TYPE I,
      SUM TYPE I.
      S1 = 123.
WRITE S1.
CONSTANTS PI TYPE P DECIMALS 5 VALUE '3.1415926'.
WRITE PI.
WRITE: / SPACE,
        SY-DATUM.
TYPES:BEGIN OF MYLIST,
  NAME(10) TYPE C,
  AGE(4)   TYPE I,
  END OF MYLIST.
CONSTANTS:CNAME(10) VALUE '生活',
BIRTH_DAY TYPE D VALUE '19960427'.
WRITE: / CNAME,
         BIRTH_DAY.
TABLES:SPFL.
WRITE '第一行'CENTERED.
WRITE /(4) '不知道多少行'.
WRITE '测试内容' RIGHT-JUSTIFIED.

WRITE 'TEST1'.
SKIP 6.
WRITE 'TEST2'.
DATA: F1 VALUE 'Y',
      F2 VALUE 'x'.
WRITE: / 'CHECK F1:',F1 AS CHECKBOX.
WRITE: / 'CHECK F2:',F2 AS CHECKBOX.
DATA:F1(10) VALUE 'ABCDEFG',
      F2(5).
F2 = F1+3(5)."测试的位置
WRITE F2.
DATA: BEGIN OF ADDRESS,
  FIRSTNAME(10) VALUE 'TEST1',
  LASTNAME(10) VALUE 'TEST2',
  TEL(12) VALUE '12345',
  END OF ADDRESS.
DATA:BEGIN OF NAME,
  FIRSTNAME(10),
  LASTNAME(10),
  E_MAIL(30),
  END OF NAME.
MOVE-CORRESPONDING ADDRESS TO NAME.
WRITE / ADDRESS.
WRITE / NAME.
DATA:NAME(20) VALUE 'SOURCE',
      SOURCE(10) VALUE 'LILY',
      TARGET(10).
WRITE (NAME) TO TARGET.
WRITE / TARGET.
DATA N TYPE I VALUE 100.
CLEAR N.
DATA: hours   TYPE i,
      minutes TYPE i,
      t2      TYPE t VALUE '200000',
      t1      TYPE t VALUE '183000'.
hours = ( t2 - t1 ) / 3600. "计算有几小时
minutes = ( t2–t1 ) / 60. "计算几分钟
DATA: STRING(10) VALUE 'ABCDEFG',
      STR1(3) VALUE 'DEF',
      STR2(3) VALUE '123'.
REPLACE STR1 WITH STR2 INTO STRING.
WRITE / STRING.
IF 3 > 8.
  WRITE / '3>8'.
ELSE .
  WRITE / '3!>8'.
  ENDIF.
DATA S(1) type c.
      s = 'a'.
      case s.
      when 'x'.
        WRITE / 'string is x'.
        WHEN others.
        WRITE / 'string is not x'.
      ENDCASE.
DO 2 TIMES.
  WRITE / 'x'.
ENDDO.
DO  varying i from 1 to 10.
  s=s+i.
ENDDO.
WRITE: / ,'1+@+#+4....=',s
DO 3 TIMES.
  IF sy-index = 2.
    CONTINUE.
  ENDIF.
  WRITE  sy-index.
ENDDO.
DO 5 TIMES.
  CHECK SY-INDEX BETWEEN 2 AND 4.
  WRITE / SY-INDEX.
ENDDO.
DO 10 TIMES.
  IF SY-INDEX = 8.
    EXIT.
  ENDIF.
  WRITE / SY-INDEX.
ENDDO.
DATA: BEGIN OF LINE,
  COL1 TYPE I,
  COL2 TYPE I,
  END OF LINE.
DATA ITAB LIKE LINE OCCURS 10.
DO 2 TIMES.
  LINE-COL1 = SY-INDEX.
  LINE-COL2 = SY-INDEX ** 2.
  APPEND LINE TO ITAB.
ENDDO.
LOOP AT ITAB INTO LINE.
  WRITE: / LINE-COL1,LINE-COL2.
ENDLOOP.
DATA:BEGIN OF ITAB OCCURS 3,
  COL1(3) TYPE C,
  COL2 TYPE I,
  END OF ITAB.
ITAB-COL1 = 'ABC'.ITAB-COL2 = 10.
COLLECT ITAB.
ITAB-COL1 = 'XYZ'.ITAB-COL2 = 20.
COLLECT ITAB.
ITAB-COL1 = 'ABC'.ITAB-COL2 = 30.
COLLECT ITAB.
LOOP AT ITAB.
  WRITE: / ITAB-COL1,ITAB-COL2.
ENDLOOP.
DATA:BEGIN OF line,
       col1 TYPE i,
       col2 TYPE i,
     END OF line.
DATA itab LIKE line OCCURS 10.
DO 3 TIMES.
  line-col1 = sy-index * 10.
  line-col2 = sy-index * 20.
  APPEND line TO itab.
ENDDO.
line-col1 = 100.
line-col2 = 200.
INSERT line INTO itab INDEX 2.
LOOP AT itab INTO line.
  WRITE: / sy-tabix,line-col1,line-col2.
endloop.