*&---------------------------------------------------------------------*
*& Report YTEST003
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ytest003.
*WRITE '测试的 那日饿哦那个啦安红豆i阿什顿'.
*WRITE  /.
*WRITE  /.
*WRITE  /  TEXT-001.
*WRITE /.
*DATA:MSG1(20) TYPE C VALUE '测试'.
*DATA:MSG2(20) TYPE C VALUE 'ABAP'.
*DATA:MSG3(20) TYPE C VALUE '消息！'.
*
*MESSAGE I006(YTEST003) WITH MSG1 MSG2 MSG3.

*DATA:BEGIN OF man,
*       name(30) TYPE c,
*       high     TYPE p DECIMALS 2,
*       weight   TYPE p DECIMALS 2,
*     END OF man.
*FIELD-SYMBOLS <fsa> LIKE man.
*DATA man1 LIKE man.
*man1-name = '张林'.
*man1-high = '1.78'.
*man1-weight = 140.
*ASSIGN man1 TO <fsa>.
*WRITE: / <fsa>-name,
*<fsa>-high,
*<fsa>-weight.

*DATA:BEGIN OF man,
*       name(30) TYPE c,
*       high     TYPE p DECIMALS 2,
*       weight   TYPE p DECIMALS 2,
*     END OF man.
*
*FIELD-SYMBOLS <fsa> LIKE man.
*DATA man1 LIKE man.
*
*man1-name = '张三'.
*man1-high = '1.78'.
*man1-weight = '140'.
*
*ASSIGN man1 TO <fsa>.
*WRITE: / <fsa>-name,
*       <fsa>-high,
*       <fsa>-weight.
*DATA: c1(2)  TYPE c,
*      c2(2)  TYPE c,
*      c3(2)  TYPE c,
*      c4(2)  TYPE c,
*      c5(20) TYPE c,
*      c9(2)  TYPE c.
*
*c1 = 'AB'.
*c2 = 'CD'.
*c3 = 'EF'.
*c4 = 'GH'.
*c9 = '+'.
*CONCATENATE c1 c2 c3 c4 INTO c5.
*WRITE c5.
*CONCATENATE c1 c2 c3 c4 INTO c5 SEPARATED BY c9.
*WRITE / c5.

*DATA: c1(2)  TYPE c,
*      c2(2)  TYPE c,
*      c3(2)  TYPE c,
*      c4(2)  TYPE c,
*      c5(20) TYPE c VALUE '11 * 22 * 33 * 44',
*      c9(2)  TYPE c.
*
*c9 = '*'.
*WRITE c5.
*SPLIT c5 AT c9 INTO c1 c2 c3 c4.
*WRITE: / c1, c2, c3, c4.
*DATA: c1(2)  TYPE c,
*      c2(2)  TYPE c,
*      c3(2)  TYPE c,
*      c4(2)  TYPE c,
*      c5(20) TYPE c VALUE '11 * 22 * 33 * 44',
*      c9(2)  TYPE c.
*c9 = '*'.
*WRITE c5.
*SPLIT c5 AT c9 INTO c1 c2 c3 c4.
*WRITE: / c1, c2, c3, c4.

*WRITE: 'test1',/,'test2','test3'.
*SKIP 5.
*ULINE.
*DATA:BEGIN OF man,
*       name(20) TYPE c,
*       high     TYPE p DECIMALS 2,
*       weight   TYPE p DECIMALS 2,
*     END OF man.
*
*DATA:man1 LIKE TABLE OF man WITH HEADER LINE,
*     man2 LIKE TABLE OF man.
*
*man-name = '张三'.
*man-high = '1.68'.
*man-weight = '120'.
*APPEND man TO man1.
*
*man-name = '刘志'.
*man-high = '1.78'.
*man-weight = '160'.
*APPEND man TO man1.
*
*MOVE man1[] TO man2.
*LOOP AT man2 INTO man.
*  WRITE: / man-name,man-high,man-weight.
*ENDLOOP.
*
*LOOP AT man1.
*  WRITE: / man1-name,man1-high,man1-weight.
*ENDLOOP.
*DATA: BEGIN OF man,
*        name(20) TYPE c,
*        high     TYPE p DECIMALS 2,
*        weight   TYPE p DECIMALS 2,
*      END OF man.
*DATA:man1 LIKE HASHED TABLE OF man WITH UNIQUE KEY name.
*
*man-name = '张三'.
*man-high = '1.78'.
*man-weight = '120'.
*INSERT man INTO TABLE man1.
*
*man-name = '李四'.
*man-high = '1.68'.
*man-weight = '130'.
*INSERT man INTO TABLE man1.
*
*LOOP AT man1 INTO man.
*  WRITE: / man-name,man-high,man-weight.
*ENDLOOP.
*
*SORT man1 DESCENDING BY weight ASCENDING.
*SKIP 2.
*ULINE.
*
*LOOP AT man1 INTO man.
*  WRITE: / man-name,man-high,man-weight.
*ENDLOOP.
*DATA:BEGIN OF man,
*       name(20) TYPE c,
*       high     TYPE p DECIMALS 2,
*       weight   TYPE p  DECIMALS 2,
*     END OF man.
*DATA:man1 LIKE HASHED TABLE OF man WITH UNIQUE KEY name.
*
*man-name = '张三'.
*man-high = '1.78'.
*man-weight = 160.
*INSERT man INTO TABLE man1.
*
*man-name = '刘志'.
*man-high = '170'.
*INSERT man INTO TABLE man1.
*
*LOOP AT man1 INTO man.
*  WRITE: / man-name,man-high,man-weight.
*ENDLOOP.
*
*man-name = '张三'.
*man-weight = 220.
*man-high = '2.22'.
*MODIFY TABLE man1 FROM man.
*
*LOOP AT man1 INTO man.
*  WRITE: / man-name,man-high,man-weight.
*ENDLOOP.
*DATA:BEGIN OF man,
*       name(20) TYPE c,
*       high     TYPE p DECIMALS 2,
*       weight   TYPE p DECIMALS 2,
*     END OF man.
*DATA: man1 LIKE HASHED TABLE OF man WITH UNIQUE KEY name.
*
*man-name = '张三'.
*man-high = '1.68'.
*man-weight = 120.
*INSERT man  INTO TABLE man1.
*
*man-name = '刘志'.
*man-high = '1.78'.
*man-weight = 160.
*INSERT man INTO TABLE man1.
*
*LOOP AT man1 INTO man.
*  WRITE:/ man-name,man-high,man-weight.
*ENDLOOP.
*
*DELETE man1 WHERE name = '张三'.
*SKIP 5.
*ULINE.
*LOOP AT man1 INTO man.
*  WRITE:/ man-name,man-high,man-weight.
*ENDLOOP.
*DATA:BEGIN OF MAN,
*  NAME(20) TYPE C,
*  HIGH TYPE P DECIMALS 2,
*  WEIGHT TYPE P DECIMALS 2,
*  END OF MAN.
*DATA:MAN1 LIKE TABLE OF MAN.
*
*MAN-NAME = '张三'.
*MAN-high = '1.68'.
*MAN-weight = 120.
*INSERT man  INTO TABLE MAN1.
*
*MAN-name = '刘志'.
*MAN-high = '1.78'.
*MAN-weight = 160.
*INSERT MAN INTO TABLE MAN1.
*
*LOOP AT MAN1 INTO MAN.
*  WRITE:/ MAN-NAME,MAN-high,MAN-weight.
*ENDLOOP.
*
*MAN-name = '李志'.
*MAN-high = '1.58'.
*MAN-weight = 110.
*INSERT MAN INTO MAN1 INDEX 2.
*ULINE.
*
*LOOP AT MAN1 INTO MAN.
*  WRITE:/ MAN-NAME,man-high,MAN-weight.
*ENDLOOP.
*DATA WA LIKE SPFLI.
*WRITE: /.
*WRITE:'航班承运人',20'航班连接',60'国家代码',80'起飞城市',100'起飞机场'.
*SELECT * INTO WA FROM SPFLI.
*  WRITE:/ WA-CARRID UNDER '航班承运人',
*          WA-CONNID UNDER '航班连接',
*          WA-COUNTRYFR UNDER '国家代码',
*          WA-CITYFROM UNDER '起飞城市',
*          WA-AIRPFROM UNDER '起飞机场'.
*
*  ENDSELECT.
*DATA:BEGIN OF MAN,
*  NAME(20) TYPE C,
*  HIGH TYPE P DECIMALS 2,
*  WEIGHT TYPE P DECIMALS 2,
*  END OF MAN.
*DATA:MAN1 LIKE TABLE OF MAN.
*DATA: NAME TYPE RLGRAP-FILENAME, TYPA TYPE RLGRAP-FILETYPE.
*
*MAN-NAME = '张三'.
*MAN-high = '1.68'.
*MAN-weight = 120.
*INSERT MAN INTO TABLE MAN1.
*
*MAN-NAME = '刘志'.
*MAN-high = '1.78'.
*MAN-weight = 160.
*
*INSERT MAN  INTO TABLE  MAN1.
*MAN-NAME = 'adasd'.
*MAN-high = '123.3'.
*MAN-weight = 110.
*INSERT MAN INTO MAN1 INDEX 2.
*
*NAME = 'C:\Users\huangkai\Desktop'.
*TYPA = 'DAT'.
*CALL FUNCTION 'DOWNLOAD'
*  EXPORTING
*    CODEPAGE = 'TESTA'
*    FILENAME = NAME
*    FILETYPE = TYPA
*    ITEM  = '文件测试'
*  TABLES
*    DATA_TAB = MAN1
*  EXCEPTIONS
*    INVALID_FILESIZE = 1
*    INVALID_TABLE_WIDTH = 2
*    INVALID_TYPE = 3
*    NO_BATCH = 4
*    UNKNOWN_ERROR = 5
*    GUI_REFUSE_FILETRANSFER = 6
*    OTHERS  = 7.
* IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
** WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
* ENDIF.

*TYPES:BEGIN OF MAN,
*  FIELD1 TYPE C LENGTH 5,
*  FIELD2 TYPE C LENGTH 4,
*  FIELD3 TYPE I,
*  END OF MAN.
*  TYPES T_TAB TYPE STANDARD TABLE OF MAN WITH NON-UNIQUE DEFAULT KEY.
*  DATA gt_itab TYPE T_TAB WITH HEADER LINE.
*  gt_itab-FIELD1 = 'ENJOY'.
*  gt_itab-FIELD2 = 'ABAP'.
*  gt_itab-FIELD3 = '1'.
*  APPEND gt_itab.
*  READ TABLE gt_itab INDEX 1.
*  WRITE:/ gt_itab-FIELD1,gt_itab-FIELD2,gt_itab-FIELD3.

*DATA:BEGIN OF GS_LINE,
*  COL1 TYPE C,
*  COL2 TYPE I,
*  END OF GS_LINE.
*DATA GT LIKE STANDARD TABLE OF GS_LINE WITH NON-UNIQUE KEY COL1.
*DATA GT2 LIKE SORTED TABLE OF GS_LINE WITH NON-UNIQUE KEY COL1.
*DATA: BEGIN OF GS,
*  COL1 TYPE C,
*BREAK-POINT.
*WRITE '内容'.

*DATA: BEGIN OF GS,
*DATA: BEGIN OF GS,
*  COL1(3) TYPE C,
*  COL2(2) TYPE N,
*  COL3 TYPE I,
*  END OF GS.
*DATA GS2 LIKE STANDARD TABLE OF GS WITH NON-UNIQUE KEY COL1.
*GS-COL1 = 'AA'.
*GS-COL2 = '17'.
*GS-COL3 = 660.
*COLLECT GS INTO GS2.
*GS-COL1 = 'AL'.
*GS-COL2 = '34'.
*GS-COL3 = 220.
*COLLECT GS INTO GS2.
*GS-COL1 = 'AA'.
*GS-COL2 = '18'.
*GS-COL3 = 280.
*COLLECT GS INTO GS2.
*
*LOOP AT GS2 INTO GS.
*  WRITE:/ GS-COL1,
*          GS-COL2,
*          GS-COL3.
*ENDLOOP.
*DATA:BEGIN OF man,
*       name(20) TYPE c,
*       high     TYPE p DECIMALS 2,
*       weight   TYPE p DECIMALS 2,
*     END OF man.
*DATA: man1 LIKE TABLE OF man.
*
*CALL FUNCTION 'WS_UPLOAD'
*  EXPORTING
*    codepage                = 'TEST'
*    filename                = 'C:\Users\huangkai\Desktop\TEST1.TXT'
*    filetype                = 'DAT'
*    item                    = '读放文件'
*  TABLES
*    data_tab                = man1
*  EXCEPTIONS
*    conversion_error        = 1
*    invalid_table_width     = 2
*    invalid_type            = 3
*    no_batch                = 4
*    unknown_error           = 5
*    gui_refuse_filetransfer = 6
*    OTHERS                  = 7.
*IF sy-subrc <> 0.
*ENDIF.
*
*LOOP AT man1 INTO man.
*  WRITE: / man-name,
*           man-high,
*           man-weight.
*ENDLOOP.
*DATA:S(10)  TYPE C VALUE 'AABBCCDDEE'.
*WRITE '接着的字符串__________将被替换。'.
*WRITE AT  6(12) S.
*DATA: str1(10) TYPE c VALUE 'AABBCCDDEE'.
*WRITE '接着的字符串____________将被替换.'.
*WRITE AT 14(10) str1.Z