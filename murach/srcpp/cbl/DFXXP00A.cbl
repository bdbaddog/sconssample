       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  DFXXP00A.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       COPY OPENKICK.
       01 CALL-NAME    PIC X(10).
      *
       01  WORK-FIELDS.
      *
           05  PACKED-FIELD-1      PIC S9(07)V99  COMP-3.
           05  PACKED-FIELD-2      PIC S9(07)V99  COMP-3.
           05  ALPHA-FIELD-2       REDEFINES PACKED-FIELD-2
                                   PIC X(05).
      *
       01  I-O-AREA                PIC X(100).
      *
       01  START-UP-MESSAGE.
      *
           05  FILLER PIC X(30) VALUE 'ABEND TESTER                  '.
           05  FILLER PIC X(49) VALUE SPACE.
           05  FILLER PIC X(79) VALUE SPACE.
           05  FILLER PIC X(30) VALUE 'Press a PF key to force one of'.
           05  FILLER PIC X(49) VALUE ' the following abends:'.
           05  FILLER PIC X(79) VALUE SPACE.
           05  FILLER PIC X(30) VALUE 'PF1 = ASRA (Data Exception)   '.
           05  FILLER PIC X(49) VALUE SPACE.
           05  FILLER PIC X(30) VALUE 'PF2 = ASRA (Decimal Divide Exc'.
           05  FILLER PIC X(49) VALUE 'eption)'.
           05  FILLER PIC X(30) VALUE 'PF3 = ASRA (Protection Excepti'.
           05  FILLER PIC X(49) VALUE 'on)'.
           05  FILLER PIC X(30) VALUE 'PF4 = ABM0 (Missing Map)      '.
           05  FILLER PIC X(49) VALUE SPACE.
           05  FILLER PIC X(30) VALUE 'PF5 = AEIO (PGMIDERR)         '.
           05  FILLER PIC X(49) VALUE SPACE.
           05  FILLER PIC X(30) VALUE 'PF6 = AEIL (DSIDERR)          '.
           05  FILLER PIC X(49) VALUE SPACE.
           05  FILLER PIC X(79) VALUE SPACE.
           05  FILLER PIC X(30) VALUE 'Or press Enter to exit without'.
           05  FILLER PIC X(49) VALUE ' abending.'.
      *
       01  TERMINATION-MESSAGE.
      *
           05  FILLER PIC X(09) VALUE 'Good Bye.'.
      *
       01  COMMUNICATION-AREA   PIC X(01).
      *
        01  DFHAID. 
           02  DFHNULL         PIC X   VALUE IS X'00'. 
           02  DFHENTER        PIC X   VALUE IS ''''. 
           02  DFHCLEAR        PIC X   VALUE IS '_'. 
           02  DFHPEN          PIC X   VALUE IS '='. 
           02  DFHOPID         PIC X   VALUE IS 'W'. 
           02  DFHMSRE         PIC X   VALUE IS 'X'. 
           02  DFHSTRF         PIC X   VALUE IS ' '. 
           02  DFHPA1          PIC X   VALUE IS '%'. 
           02  DFHPA2          PIC X   VALUE IS '>'. 
           02  DFHPA3          PIC X   VALUE IS ','. 
           02  DFHPF1          PIC X   VALUE IS '1'. 
           02  DFHPF2          PIC X   VALUE IS '2'. 
           02  DFHPF3          PIC X   VALUE IS '3'. 
           02  DFHPF4          PIC X   VALUE IS '4'. 
           02  DFHPF5          PIC X   VALUE IS '5'. 
           02  DFHPF6          PIC X   VALUE IS '6'. 
           02  DFHPF7          PIC X   VALUE IS '7'. 
           02  DFHPF8          PIC X   VALUE IS '8'. 
           02  DFHPF9          PIC X   VALUE IS '9'. 
           02  DFHPF10         PIC X   VALUE IS ':'. 
           02  DFHPF11         PIC X   VALUE IS '#'. 
           02  DFHPF12         PIC X   VALUE IS '@'. 
           02  DFHPF13         PIC X   VALUE IS 'A'. 
           02  DFHPF14         PIC X   VALUE IS 'B'. 
           02  DFHPF15         PIC X   VALUE IS 'C'. 
           02  DFHPF16         PIC X   VALUE IS 'D'. 
           02  DFHPF17         PIC X   VALUE IS 'E'. 
           02  DFHPF18         PIC X   VALUE IS 'F'. 
           02  DFHPF19         PIC X   VALUE IS 'G'. 
           02  DFHPF20         PIC X   VALUE IS 'H'. 
           02  DFHPF21         PIC X   VALUE IS 'I'. 
           02  DFHPF22         PIC X   VALUE IS '$'.                   
           02  DFHPF23         PIC X   VALUE IS '.'.                   
           02  DFHPF24         PIC X   VALUE IS '<'. 
      *
       LINKAGE SECTION.
       COPY DFHEIBLK.
      *
       01  DFHCOMMAREA             PIC X(01).
      *
       01  COMMON-WORK-AREA.
      *
           05  CWA-DATE            PIC 9(06).
      *
       PROCEDURE DIVISION USING DFHEIBLK, DFHCOMMAREA.
            IF EIBRESP EQUAL 111
               CALL "raise" USING BY VALUE 6
            END-IF.
           MOVE LENGTH OF DFHCOMMAREA TO OPENKICKS-NUM1
           SET OPENKICKS-PTR1 TO ADDRESS OF DFHEIBLK
           DISPLAY 'DFHEIBLK:' OPENKICKS-PTR1
           SET OPENKICKS-PTR1 TO ADDRESS OF DFHCOMMAREA
           DISPLAY 'DFHCOMMAREA:' OPENKICKS-PTR1
           SET OPENKICKS-PTR1 TO ADDRESS OF OPENKICKS-DATA1
           DISPLAY 'OPENKICK-DATA1 PTR IN CBL:' OPENKICKS-PTR1
           MOVE 70 TO OPENKICKS-CMD
           MOVE 0 TO OPENKICKS-LINE
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 0 TO OPENKICKS-USED
           MOVE 0 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
           .
      *
       0000-FORCE-USER-ABEND.
      *
           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
      *            EXEC CICS
      *                SEND TEXT FROM(START-UP-MESSAGE)
      *                          ERASE
      *                          FREEKB
      *            END-EXEC
           MOVE 53 TO OPENKICKS-CMD
           SET OPENKICKS-DATA1 TO ADDRESS OF START-UP-MESSAGE
           MOVE 3 TO OPENKICKS-FLAG
           MOVE 2048 TO OPENKICKS-USED
           MOVE 0 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
               WHEN EIBAID = DFHPF1
                   MOVE SPACE TO ALPHA-FIELD-2
                   MOVE 100   TO PACKED-FIELD-1
                   MULTIPLY PACKED-FIELD-1 BY PACKED-FIELD-2
               WHEN EIBAID = DFHPF2
                   MOVE 100  TO PACKED-FIELD-1
                   MOVE ZERO TO PACKED-FIELD-2
                   DIVIDE PACKED-FIELD-2 INTO PACKED-FIELD-1
               WHEN EIBAID = DFHPF3
                   SET ADDRESS OF COMMON-WORK-AREA TO NULL
                   MOVE ZERO TO CWA-DATE
               WHEN EIBAID = DFHPF4
      *            EXEC CICS
      *                SEND MAP('NOMAP1')
      *                     MAPSET('NOSET1')
      *                     FROM(I-O-AREA)
      *                     ERASE
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'NOMAP1' TO OPENKICKS-CHAR8A
           MOVE 'NOSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF I-O-AREA
           MOVE 8 TO OPENKICKS-FLAG
           MOVE 2054 TO OPENKICKS-USED
           MOVE 0 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
               WHEN EIBAID = DFHPF5
      *            EXEC CICS
      *                XCTL PROGRAM('NOPGM1')
      *            END-EXEC
           MOVE 10 TO OPENKICKS-CMD
           MOVE 'NOPGM1' TO OPENKICKS-CHAR8A
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 2 TO OPENKICKS-USED
           MOVE 0 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
           IF EIBRESP EQUAL 0
              GOBACK
           END-IF
               WHEN EIBAID = DFHPF6
      *            EXEC CICS
      *                READ DATASET('NOFILE')
      *                     INTO(I-O-AREA)
      *                     RIDFLD(ALPHA-FIELD-2)
      *            END-EXEC
           MOVE 18 TO OPENKICKS-CMD
           MOVE 'NOFILE' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF I-O-AREA
           MOVE LENGTH OF I-O-AREA TO OPENKICKS-LENGTHOF
           SET OPENKICKS-DATA2 TO ADDRESS OF ALPHA-FIELD-2
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 6146 TO OPENKICKS-USED
           MOVE 0 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
               WHEN EIBAID = DFHENTER OR DFHCLEAR
      *            EXEC CICS
      *                SEND TEXT FROM(TERMINATION-MESSAGE)
      *                          ERASE
      *                          FREEKB
      *            END-EXEC
           MOVE 53 TO OPENKICKS-CMD
           SET OPENKICKS-DATA1 TO ADDRESS OF TERMINATION-MESSAGE
           MOVE 3 TO OPENKICKS-FLAG
           MOVE 2048 TO OPENKICKS-USED
           MOVE 0 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
      *            EXEC CICS
      *                RETURN
      *            END-EXEC
           MOVE 11 TO OPENKICKS-CMD
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 0 TO OPENKICKS-USED
           MOVE 0 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
           GOBACK
           END-EVALUATE.
      *    EXEC CICS
      *        RETURN TRANSID('DFXX')
      *               COMMAREA(COMMUNICATION-AREA)
      *    END-EXEC.
           MOVE 11 TO OPENKICKS-CMD
           MOVE 'DFXX' TO OPENKICKS-CHAR4A
           SET OPENKICKS-DATA1 TO ADDRESS OF COMMUNICATION-AREA
           MOVE LENGTH OF COMMUNICATION-AREA TO OPENKICKS-LENGTHOF
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 2560 TO OPENKICKS-USED
           MOVE 0 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
           GOBACK
           .

       OPENKICKS-ABEND-SUB SECTION.
       OPENKICKS-ABEND-SUB-P.
            MOVE 255 TO OPENKICKS-CMD
          CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
            RETURNING INT
            END-CALL.
            GOBACK.
       OPENKICKS-HANDLE SECTION.
       OPENKICKS-HANDLE-P.
