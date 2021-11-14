       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  CUSTMNT3.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       COPY OPENKICK.
       01 CALL-NAME    PIC X(10).
      *
       01  SWITCHES.
      *
           05  VALID-DATA-SW                 PIC X(01)  VALUE 'Y'.
               88  VALID-DATA                           VALUE 'Y'.
      *
       01  FLAGS.
      *
           05  SEND-FLAG                     PIC X(01).
               88  SEND-ERASE                       VALUE '1'.
               88  SEND-ERASE-ALARM                 VALUE '2'.
               88  SEND-DATAONLY                    VALUE '3'.
               88  SEND-DATAONLY-ALARM              VALUE '4'.
      *
       01  WORK-FIELDS.
      *
           05  RESPONSE-CODE                 PIC S9(08) COMP.
      *
       01  USER-INSTRUCTIONS.
      *
           05  ADD-INSTRUCTION                 PIC X(79) VALUE
               'Type information for new customer.  Then Press Enter.'.
           05  CHANGE-INSTRUCTION              PIC X(79) VALUE
               'Type changes.  Then press Enter.'.
           05  DELETE-INSTRUCTION              PIC X(79) VALUE
               'Press Enter to delete this customer or press F12 to canc
      -        'el.'.
      *
       01  COMMUNICATION-AREA.
      *
           05  CA-CONTEXT-FLAG               PIC X.
               88  PROCESS-KEY-MAP                  VALUE '1'.
               88  PROCESS-ADD-CUSTOMER             VALUE '2'.
               88  PROCESS-CHANGE-CUSTOMER          VALUE '3'.
               88  PROCESS-DELETE-CUSTOMER          VALUE '4'.
           05  CA-CUSTOMER-NUMBER            PIC X(6).
      *
       01  TEMPORARY-STORAGE-FIELDS.
      *
           05  TS-QUEUE-NAME.
               10  TS-TERMINAL-ID            PIC X(4).
               10  FILLER                    PIC X(4)   VALUE 'MNT3'.
           05  TS-ITEM-NUMBER                PIC S9(4)  COMP  VALUE +1.
           05  TS-CUSTOMER-RECORD.
               10  TS-CUSTOMER-NUMBER        PIC X(6).
               10  FILLER                    PIC X(112).
      *
       01  CUSTOMER-MASTER-RECORD.
      *
           05  CM-CUSTOMER-NUMBER      PIC X(6).
           05  CM-FIRST-NAME           PIC X(20).
           05  CM-LAST-NAME            PIC X(30).
           05  CM-ADDRESS              PIC X(30).
           05  CM-CITY                 PIC X(20).
           05  CM-STATE                PIC X(2).
           05  CM-ZIP-CODE             PIC X(10).
      *
      *   Micro Focus BMS Screen Painter (ver MFBM 2.0.11)
      *   MapSet Name   MNTSET1
      *   Date Created  04/11/2001
      *   Time Created  11:53:32
      *  Input Data For Map MNTMAP1
         01 MNTMAP1I.
            03 FILLER                         PIC X(12).
            03 TRANID1L                       PIC S9(4) COMP.
            03 TRANID1F                       PIC X.
            03 FILLER REDEFINES TRANID1F.
               05 TRANID1A                       PIC X.
            03 FILLER                         PIC X(2).
            03 TRANID1I                       PIC X(4).
            03 CUSTNO1L                       PIC S9(4) COMP.
            03 CUSTNO1F                       PIC X.
            03 FILLER REDEFINES CUSTNO1F.
               05 CUSTNO1A                       PIC X.
            03 FILLER                         PIC X(2).
            03 CUSTNO1I                       PIC X(6).
            03 ACTIONL                        PIC S9(4) COMP.
            03 ACTIONF                        PIC X.
            03 FILLER REDEFINES ACTIONF.
               05 ACTIONA                        PIC X.
            03 FILLER                         PIC X(2).
            03 ACTIONI                        PIC X(1).
            03 MSG1L                          PIC S9(4) COMP.
            03 MSG1F                          PIC X.
            03 FILLER REDEFINES MSG1F.
               05 MSG1A                          PIC X.
            03 FILLER                         PIC X(2).
            03 MSG1I                          PIC X(79).
            03 DUMMY1L                        PIC S9(4) COMP.
            03 DUMMY1F                        PIC X.
            03 FILLER REDEFINES DUMMY1F.
               05 DUMMY1A                        PIC X.
            03 FILLER                         PIC X(2).
            03 DUMMY1I                        PIC X(1).
      *  Output Data For Map MNTMAP1
         01 MNTMAP1O REDEFINES MNTMAP1I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TRANID1C                       PIC X.
            03 TRANID1H                       PIC X.
            03 TRANID1O                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 CUSTNO1C                       PIC X.
            03 CUSTNO1H                       PIC X.
            03 CUSTNO1O                       PIC X(6).
            03 FILLER                         PIC X(3).
            03 ACTIONC                        PIC X.
            03 ACTIONH                        PIC X.
            03 ACTIONO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 MSG1C                          PIC X.
            03 MSG1H                          PIC X.
            03 MSG1O                          PIC X(79).
            03 FILLER                         PIC X(3).
            03 DUMMY1C                        PIC X.
            03 DUMMY1H                        PIC X.
            03 DUMMY1O                        PIC X(1).
      *  Input Data For Map MNTMAP2
         01 MNTMAP2I.
            03 FILLER                         PIC X(12).
            03 TRANID2L                       PIC S9(4) COMP.
            03 TRANID2F                       PIC X.
            03 FILLER REDEFINES TRANID2F.
               05 TRANID2A                       PIC X.
            03 FILLER                         PIC X(2).
            03 TRANID2I                       PIC X(4).
            03 INSTR2L                        PIC S9(4) COMP.
            03 INSTR2F                        PIC X.
            03 FILLER REDEFINES INSTR2F.
               05 INSTR2A                        PIC X.
            03 FILLER                         PIC X(2).
            03 INSTR2I                        PIC X(79).
            03 CUSTNO2L                       PIC S9(4) COMP.
            03 CUSTNO2F                       PIC X.
            03 FILLER REDEFINES CUSTNO2F.
               05 CUSTNO2A                       PIC X.
            03 FILLER                         PIC X(2).
            03 CUSTNO2I                       PIC X(6).
            03 LNAMEL                         PIC S9(4) COMP.
            03 LNAMEF                         PIC X.
            03 FILLER REDEFINES LNAMEF.
               05 LNAMEA                         PIC X.
            03 FILLER                         PIC X(2).
            03 LNAMEI                         PIC X(30).
            03 FNAMEL                         PIC S9(4) COMP.
            03 FNAMEF                         PIC X.
            03 FILLER REDEFINES FNAMEF.
               05 FNAMEA                         PIC X.
            03 FILLER                         PIC X(2).
            03 FNAMEI                         PIC X(20).
            03 ADDRL                          PIC S9(4) COMP.
            03 ADDRF                          PIC X.
            03 FILLER REDEFINES ADDRF.
               05 ADDRA                          PIC X.
            03 FILLER                         PIC X(2).
            03 ADDRI                          PIC X(30).
            03 CITYL                          PIC S9(4) COMP.
            03 CITYF                          PIC X.
            03 FILLER REDEFINES CITYF.
               05 CITYA                          PIC X.
            03 FILLER                         PIC X(2).
            03 CITYI                          PIC X(20).
            03 STATEL                         PIC S9(4) COMP.
            03 STATEF                         PIC X.
            03 FILLER REDEFINES STATEF.
               05 STATEA                         PIC X.
            03 FILLER                         PIC X(2).
            03 STATEI                         PIC X(2).
            03 ZIPCODEL                       PIC S9(4) COMP.
            03 ZIPCODEF                       PIC X.
            03 FILLER REDEFINES ZIPCODEF.
               05 ZIPCODEA                       PIC X.
            03 FILLER                         PIC X(2).
            03 ZIPCODEI                       PIC X(10).
            03 MSG2L                          PIC S9(4) COMP.
            03 MSG2F                          PIC X.
            03 FILLER REDEFINES MSG2F.
               05 MSG2A                          PIC X.
            03 FILLER                         PIC X(2).
            03 MSG2I                          PIC X(79).
            03 DUMMY2L                        PIC S9(4) COMP.
            03 DUMMY2F                        PIC X.
            03 FILLER REDEFINES DUMMY2F.
               05 DUMMY2A                        PIC X.
            03 FILLER                         PIC X(2).
            03 DUMMY2I                        PIC X(1).
      *  Output Data For Map MNTMAP2
         01 MNTMAP2O REDEFINES MNTMAP2I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TRANID2C                       PIC X.
            03 TRANID2H                       PIC X.
            03 TRANID2O                       PIC X(4).
            03 FILLER                         PIC X(3).
            03 INSTR2C                        PIC X.
            03 INSTR2H                        PIC X.
            03 INSTR2O                        PIC X(79).
            03 FILLER                         PIC X(3).
            03 CUSTNO2C                       PIC X.
            03 CUSTNO2H                       PIC X.
            03 CUSTNO2O                       PIC X(6).
            03 FILLER                         PIC X(3).
            03 LNAMEC                         PIC X.
            03 LNAMEH                         PIC X.
            03 LNAMEO                         PIC X(30).
            03 FILLER                         PIC X(3).
            03 FNAMEC                         PIC X.
            03 FNAMEH                         PIC X.
            03 FNAMEO                         PIC X(20).
            03 FILLER                         PIC X(3).
            03 ADDRC                          PIC X.
            03 ADDRH                          PIC X.
            03 ADDRO                          PIC X(30).
            03 FILLER                         PIC X(3).
            03 CITYC                          PIC X.
            03 CITYH                          PIC X.
            03 CITYO                          PIC X(20).
            03 FILLER                         PIC X(3).
            03 STATEC                         PIC X.
            03 STATEH                         PIC X.
            03 STATEO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 ZIPCODEC                       PIC X.
            03 ZIPCODEH                       PIC X.
            03 ZIPCODEO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 MSG2C                          PIC X.
            03 MSG2H                          PIC X.
            03 MSG2O                          PIC X(79).
            03 FILLER                         PIC X(3).
            03 DUMMY2C                        PIC X.
            03 DUMMY2H                        PIC X.
            03 DUMMY2O                        PIC X(1).
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
       01  ATTRIBUTE-DEFINITIONS.
      *
           05  ATTR-UNPROT                 PIC X   VALUE ' '.
           05  ATTR-UNPROT-MDT             PIC X   VALUE 'A'.
           05  ATTR-UNPROT-BRT             PIC X   VALUE 'H'.
           05  ATTR-UNPROT-BRT-MDT         PIC X   VALUE 'I'.
           05  ATTR-UNPROT-DARK            PIC X   VALUE '<'.
           05  ATTR-UNPROT-DARK-MDT        PIC X   VALUE '('.
           05  ATTR-UNPROT-NUM             PIC X   VALUE '&'.
           05  ATTR-UNPROT-NUM-MDT         PIC X   VALUE 'J'.
           05  ATTR-UNPROT-NUM-BRT         PIC X   VALUE 'Q'.
           05  ATTR-UNPROT-NUM-BRT-MDT     PIC X   VALUE 'R'.
           05  ATTR-UNPROT-NUM-DARK        PIC X   VALUE '*'.
           05  ATTR-UNPROT-NUM-DARK-MDT    PIC X   VALUE ')'.
           05  ATTR-PROT                   PIC X   VALUE '-'.
           05  ATTR-PROT-MDT               PIC X   VALUE '/'.
           05  ATTR-PROT-BRT               PIC X   VALUE 'Y'.
           05  ATTR-PROT-BRT-MDT           PIC X   VALUE 'Z'.
           05  ATTR-PROT-DARK              PIC X   VALUE '%'.
           05  ATTR-PROT-DARK-MDT          PIC X   VALUE '_'.
           05  ATTR-PROT-SKIP              PIC X   VALUE '0'.
           05  ATTR-PROT-SKIP-MDT          PIC X   VALUE '1'.
           05  ATTR-PROT-SKIP-BRT          PIC X   VALUE '2'.
           05  ATTR-PROT-SKIP-BRT-MDT      PIC X   VALUE '9'.
           05  ATTR-PROT-SKIP-DARK         PIC X   VALUE '@'.
           05  ATTR-PROT-SKIP-DARK-MDT     PIC X   VALUE X'7D'.
      *
           05  ATTR-NO-HIGHLIGHT           PIC X   VALUE X'00'.
           05  ATTR-BLINK                  PIC X   VALUE '1'.
           05  ATTR-REVERSE                PIC X   VALUE '2'.
           05  ATTR-UNDERSCORE             PIC X   VALUE '4'.
      *
           05  ATTR-DEFAULT-COLOR          PIC X   VALUE X'00'.
           05  ATTR-BLUE                   PIC X   VALUE '1'.
           05  ATTR-RED                    PIC X   VALUE '2'.
           05  ATTR-PINK                   PIC X   VALUE '3'.
           05  ATTR-GREEN                  PIC X   VALUE '4'.
           05  ATTR-TURQUOISE              PIC X   VALUE '5'.
           05  ATTR-YELLOW                 PIC X   VALUE '6'.
           05  ATTR-NEUTRAL                PIC X   VALUE '7'.
      *
       LINKAGE SECTION.
       COPY DFHEIBLK.
      *
       01  DFHCOMMAREA                       PIC X(07).
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
       0000-PROCESS-CUSTOMER-MAINT.
      *
           IF EIBCALEN > ZERO
               MOVE DFHCOMMAREA TO COMMUNICATION-AREA
           END-IF.
      *
           MOVE EIBTRMID TO TS-TERMINAL-ID.
      *
           EVALUATE TRUE
      *
               WHEN EIBCALEN = ZERO
                   MOVE LOW-VALUE TO TS-CUSTOMER-RECORD
      *            EXEC CICS
      *                WRITEQ TS QUEUE(TS-QUEUE-NAME)
      *                          FROM(TS-CUSTOMER-RECORD)
      *            END-EXEC
           MOVE 5 TO OPENKICKS-CMD
           MOVE TS-QUEUE-NAME TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA2 TO ADDRESS OF TS-CUSTOMER-RECORD
           MOVE LENGTH OF TS-CUSTOMER-RECORD TO OPENKICKS-LENGTHOF
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 4098 TO OPENKICKS-USED
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
                   MOVE LOW-VALUE TO MNTMAP1O
                   SET SEND-ERASE TO TRUE
                   MOVE -1 TO CUSTNO1L
                   PERFORM 1500-SEND-KEY-MAP
                   SET PROCESS-KEY-MAP TO TRUE
      *
               WHEN EIBAID = DFHPF3
      *            EXEC CICS
      *                DELETEQ TS QUEUE(TS-QUEUE-NAME)
      *            END-EXEC
           MOVE 6 TO OPENKICKS-CMD
           MOVE TS-QUEUE-NAME TO OPENKICKS-CHAR8A
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
      *            EXEC CICS
      *                XCTL PROGRAM('INVMENU')
      *            END-EXEC
           MOVE 10 TO OPENKICKS-CMD
           MOVE 'INVMENU' TO OPENKICKS-CHAR8A
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
      *
               WHEN EIBAID = DFHPF12
                   IF PROCESS-KEY-MAP
      *                EXEC CICS
      *                    DELETEQ TS QUEUE(TS-QUEUE-NAME)
      *                END-EXEC
           MOVE 6 TO OPENKICKS-CMD
           MOVE TS-QUEUE-NAME TO OPENKICKS-CHAR8A
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
      *                EXEC CICS
      *                    XCTL PROGRAM('INVMENU')
      *                END-EXEC
           MOVE 10 TO OPENKICKS-CMD
           MOVE 'INVMENU' TO OPENKICKS-CHAR8A
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
                   ELSE
                       MOVE LOW-VALUE TO MNTMAP1O
                       MOVE -1 TO CUSTNO1L
                       SET SEND-ERASE TO TRUE
                       PERFORM 1500-SEND-KEY-MAP
                       SET PROCESS-KEY-MAP TO TRUE
                   END-IF
      *
               WHEN EIBAID = DFHCLEAR
                   IF PROCESS-KEY-MAP
                       MOVE LOW-VALUE TO MNTMAP1O
                       MOVE -1 TO CUSTNO1L
                       SET SEND-ERASE TO TRUE
                       PERFORM 1500-SEND-KEY-MAP
                   ELSE
                       MOVE LOW-VALUE TO MNTMAP2O
                       MOVE CA-CUSTOMER-NUMBER TO CUSTNO2O
                       EVALUATE TRUE
                           WHEN PROCESS-ADD-CUSTOMER
                               MOVE ADD-INSTRUCTION    TO INSTR2O
                           WHEN PROCESS-CHANGE-CUSTOMER
                               MOVE CHANGE-INSTRUCTION TO INSTR2O
                           WHEN PROCESS-DELETE-CUSTOMER
                               MOVE DELETE-INSTRUCTION TO INSTR2O
                       END-EVALUATE
                       MOVE -1 TO LNAMEL
                       SET SEND-ERASE TO TRUE
                       PERFORM 1400-SEND-DATA-MAP
                   END-IF
      *
               WHEN EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
                   CONTINUE
      *
               WHEN EIBAID = DFHENTER
                   EVALUATE TRUE
                       WHEN PROCESS-KEY-MAP
                           PERFORM 1000-PROCESS-KEY-MAP
                       WHEN PROCESS-ADD-CUSTOMER
                           PERFORM 2000-PROCESS-ADD-CUSTOMER
                       WHEN PROCESS-CHANGE-CUSTOMER
                           PERFORM 3000-PROCESS-CHANGE-CUSTOMER
                       WHEN PROCESS-DELETE-CUSTOMER
                           PERFORM 4000-PROCESS-DELETE-CUSTOMER
                   END-EVALUATE
      *
               WHEN OTHER
                   IF PROCESS-KEY-MAP
                       MOVE LOW-VALUE TO MNTMAP1O
                       MOVE 'That key is unassigned.' TO MSG1O
                       MOVE -1 TO CUSTNO1L
                       SET SEND-DATAONLY-ALARM TO TRUE
                       PERFORM 1500-SEND-KEY-MAP
                   ELSE
                       MOVE LOW-VALUE TO MNTMAP2O
                       MOVE -1 TO LNAMEL
                       MOVE 'That key is unassigned.' TO MSG2O
                       SET SEND-DATAONLY-ALARM TO TRUE
                       PERFORM 1400-SEND-DATA-MAP
                   END-IF
      *
           END-EVALUATE.
      *
      *    EXEC CICS
      *        RETURN TRANSID('MNT3')
      *               COMMAREA(COMMUNICATION-AREA)
      *    END-EXEC.
           MOVE 11 TO OPENKICKS-CMD
           MOVE 'MNT3' TO OPENKICKS-CHAR4A
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

      *
       1000-PROCESS-KEY-MAP.
      *
           PERFORM 1100-RECEIVE-KEY-MAP.
           PERFORM 1200-EDIT-KEY-DATA.
           IF VALID-DATA
               MOVE CUSTNO1I      TO CUSTNO2O
               MOVE CM-LAST-NAME  TO LNAMEO
               MOVE CM-FIRST-NAME TO FNAMEO
               MOVE CM-ADDRESS    TO ADDRO
               MOVE CM-CITY       TO CITYO
               MOVE CM-STATE      TO STATEO
               MOVE CM-ZIP-CODE   TO ZIPCODEO
               MOVE -1            TO LNAMEL
               SET SEND-ERASE TO TRUE
               PERFORM 1400-SEND-DATA-MAP
           ELSE
               MOVE LOW-VALUE TO CUSTNO1O
                                 ACTIONO
               SET SEND-DATAONLY-ALARM TO TRUE
               PERFORM 1500-SEND-KEY-MAP
           END-IF.
      *
       1100-RECEIVE-KEY-MAP.
      *
      *    EXEC CICS
      *        RECEIVE MAP('MNTMAP1')
      *                MAPSET('MNTSET1')
      *                INTO(MNTMAP1I)
      *    END-EXEC.
           MOVE 29 TO OPENKICKS-CMD
           MOVE 'MNTMAP1' TO OPENKICKS-CHAR8A
           MOVE 'MNTSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF MNTMAP1I
           MOVE 0 TO OPENKICKS-FLAG
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
           .

      *
       1200-EDIT-KEY-DATA.
      *
           IF ACTIONI NOT = '1' AND '2' AND '3'
               MOVE -1 TO ACTIONL
               MOVE 'Action must be 1, 2, or 3.' TO MSG1O
               MOVE 'N' TO VALID-DATA-SW
           END-IF.
      *
           IF       CUSTNO1L = ZERO
                 OR CUSTNO1I = SPACE
               MOVE -1 TO CUSTNO1L
               MOVE 'You must enter a customer number.' TO MSG1O
               MOVE 'N' TO VALID-DATA-SW
           END-IF.
      *
           IF VALID-DATA
               MOVE LOW-VALUE TO MNTMAP2O
               EVALUATE ACTIONI
                   WHEN '1'
                       PERFORM 1300-READ-CUSTOMER-RECORD
                       IF RESPONSE-CODE = 13
                           MOVE CM-CUSTOMER-NUMBER TO CA-CUSTOMER-NUMBER
                           MOVE ADD-INSTRUCTION TO INSTR2O
                           SET PROCESS-ADD-CUSTOMER TO TRUE
                           MOVE SPACE TO CUSTOMER-MASTER-RECORD
                       ELSE
                           IF RESPONSE-CODE = 0
                               MOVE 'That customer already exists.' TO
                                   MSG1O
                               MOVE 'N' TO VALID-DATA-SW
                           END-IF
                       END-IF
                   WHEN '2'
                       PERFORM 1300-READ-CUSTOMER-RECORD
                       IF RESPONSE-CODE = 0
                           MOVE CM-CUSTOMER-NUMBER TO CA-CUSTOMER-NUMBER
                           MOVE CHANGE-INSTRUCTION TO INSTR2O
                           SET PROCESS-CHANGE-CUSTOMER TO TRUE
                       ELSE
                           IF RESPONSE-CODE = 13
                               MOVE 'That customer does not exist.' TO
                                   MSG1O
                               MOVE 'N' TO VALID-DATA-SW
                           END-IF
                       END-IF
                   WHEN '3'
                       PERFORM 1300-READ-CUSTOMER-RECORD
                       IF RESPONSE-CODE = 0
                           MOVE CM-CUSTOMER-NUMBER TO CA-CUSTOMER-NUMBER
                           MOVE DELETE-INSTRUCTION TO INSTR2O
                           SET PROCESS-DELETE-CUSTOMER TO TRUE
                           MOVE ATTR-PROT TO LNAMEA
                                             FNAMEA
                                             ADDRA
                                             CITYA
                                             STATEA
                                             ZIPCODEA
                       ELSE
                           IF RESPONSE-CODE = 13
                               MOVE 'That customer does not exist.' TO
                                   MSG1O
                               MOVE 'N' TO VALID-DATA-SW
                           END-IF
                       END-IF
               END-EVALUATE.
      *
       1300-READ-CUSTOMER-RECORD.
      *
      *    EXEC CICS
      *        READ FILE('CUSTMAS')
      *             INTO(CUSTOMER-MASTER-RECORD)
      *             RIDFLD(CUSTNO1I)
      *             RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 18 TO OPENKICKS-CMD
           MOVE 'CUSTMAS' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-MASTER-RECORD
           MOVE LENGTH OF CUSTOMER-MASTER-RECORD TO OPENKICKS-LENGTHOF
           SET OPENKICKS-DATA2 TO ADDRESS OF CUSTNO1I
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 6146 TO OPENKICKS-USED
           MOVE 4 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           MOVE EIBRESP TO RESPONSE-CODE
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
           .

           IF      RESPONSE-CODE NOT = 0
               AND RESPONSE-CODE NOT = 13
               PERFORM 9999-TERMINATE-PROGRAM
           END-IF.
           IF RESPONSE-CODE = 0
               MOVE CUSTOMER-MASTER-RECORD TO TS-CUSTOMER-RECORD
      *        EXEC CICS
      *            WRITEQ TS QUEUE(TS-QUEUE-NAME)
      *                      FROM(TS-CUSTOMER-RECORD)
      *                      ITEM(TS-ITEM-NUMBER)
      *                      REWRITE
      *        END-EXEC
           MOVE 5 TO OPENKICKS-CMD
           MOVE TS-QUEUE-NAME TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA2 TO ADDRESS OF TS-CUSTOMER-RECORD
           MOVE LENGTH OF TS-CUSTOMER-RECORD TO OPENKICKS-LENGTHOF
           MOVE TS-ITEM-NUMBER TO OPENKICKS-NUM3
           MOVE 1 TO OPENKICKS-FLAG
           MOVE 4098 TO OPENKICKS-USED
           MOVE 256 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           MOVE OPENKICKS-NUM3 TO TS-ITEM-NUMBER
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
           END-IF.
      *
       1400-SEND-DATA-MAP.
      *
           MOVE 'MNT3' TO TRANID2O.
      *
           EVALUATE TRUE
               WHEN SEND-ERASE
      *            EXEC CICS
      *                SEND MAP('MNTMAP2')
      *                     MAPSET('MNTSET1')
      *                     FROM(MNTMAP2O)
      *                     ERASE
      *                     CURSOR
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'MNTMAP2' TO OPENKICKS-CHAR8A
           MOVE 'MNTSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF MNTMAP2O
           MOVE 4194312 TO OPENKICKS-FLAG
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
               WHEN SEND-DATAONLY-ALARM
      *            EXEC CICS
      *                SEND MAP('MNTMAP2')
      *                     MAPSET('MNTSET1')
      *                     FROM(MNTMAP2O)
      *                     DATAONLY
      *                     ALARM
      *                     CURSOR
      *        END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'MNTMAP2' TO OPENKICKS-CHAR8A
           MOVE 'MNTSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF MNTMAP2O
           MOVE 4194818 TO OPENKICKS-FLAG
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
           END-EVALUATE.
      *
       1500-SEND-KEY-MAP.
      *
           MOVE 'MNT3' TO TRANID1O.
      *
           EVALUATE TRUE
               WHEN SEND-ERASE
      *            EXEC CICS
      *                SEND MAP('MNTMAP1')
      *                     MAPSET('MNTSET1')
      *                     FROM(MNTMAP1O)
      *                     ERASE
      *                     CURSOR
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'MNTMAP1' TO OPENKICKS-CHAR8A
           MOVE 'MNTSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF MNTMAP1O
           MOVE 4194312 TO OPENKICKS-FLAG
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
               WHEN SEND-ERASE-ALARM
      *            EXEC CICS
      *                SEND MAP('MNTMAP1')
      *                     MAPSET('MNTSET1')
      *                     FROM(MNTMAP1O)
      *                     ERASE
      *                     ALARM
      *                     CURSOR
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'MNTMAP1' TO OPENKICKS-CHAR8A
           MOVE 'MNTSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF MNTMAP1O
           MOVE 4194824 TO OPENKICKS-FLAG
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
               WHEN SEND-DATAONLY-ALARM
      *            EXEC CICS
      *                SEND MAP('MNTMAP1')
      *                     MAPSET('MNTSET1')
      *                     FROM(MNTMAP1O)
      *                     DATAONLY
      *                     ALARM
      *                     CURSOR
      *        END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'MNTMAP1' TO OPENKICKS-CHAR8A
           MOVE 'MNTSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF MNTMAP1O
           MOVE 4194818 TO OPENKICKS-FLAG
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
           END-EVALUATE.
      *
       2000-PROCESS-ADD-CUSTOMER.
      *
           PERFORM 2100-RECEIVE-DATA-MAP.
           PERFORM 2300-WRITE-CUSTOMER-RECORD.
           IF RESPONSE-CODE = 0
               MOVE 'Customer record added.' TO MSG1O
               SET SEND-ERASE TO TRUE
           ELSE
               IF RESPONSE-CODE = 14
                   MOVE 'Another user has added a record with that custo
      -             'mer number.' TO MSG1O
                   SET SEND-ERASE-ALARM TO TRUE
               END-IF
           END-IF.
           MOVE -1 TO CUSTNO1L.
           PERFORM 1500-SEND-KEY-MAP.
           SET PROCESS-KEY-MAP TO TRUE.
      *
       2100-RECEIVE-DATA-MAP.
      *
      *    EXEC CICS
      *        RECEIVE MAP('MNTMAP2')
      *                MAPSET('MNTSET1')
      *                INTO(MNTMAP2I)
      *    END-EXEC.
           MOVE 29 TO OPENKICKS-CMD
           MOVE 'MNTMAP2' TO OPENKICKS-CHAR8A
           MOVE 'MNTSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF MNTMAP2I
           MOVE 0 TO OPENKICKS-FLAG
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
           .

      *
       2300-WRITE-CUSTOMER-RECORD.
      *
           MOVE CUSTNO2I TO CM-CUSTOMER-NUMBER.
           MOVE LNAMEI   TO CM-LAST-NAME.
           MOVE FNAMEI   TO CM-FIRST-NAME.
           MOVE ADDRI    TO CM-ADDRESS.
           MOVE CITYI    TO CM-CITY.
           MOVE STATEI   TO CM-STATE.
           MOVE ZIPCODEI TO CM-ZIP-CODE.
      *    EXEC CICS
      *        WRITE FILE('CUSTMAS')
      *              FROM(CUSTOMER-MASTER-RECORD)
      *              RIDFLD(CM-CUSTOMER-NUMBER)
      *              RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 17 TO OPENKICKS-CMD
           MOVE 'CUSTMAS' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-MASTER-RECORD
           MOVE LENGTH OF CUSTOMER-MASTER-RECORD TO OPENKICKS-LENGTHOF
           SET OPENKICKS-DATA2 TO ADDRESS OF CM-CUSTOMER-NUMBER
           MOVE LENGTH OF CM-CUSTOMER-NUMBER TO OPENKICKS-LENGTHOF1
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 6146 TO OPENKICKS-USED
           MOVE 4 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           MOVE EIBRESP TO RESPONSE-CODE
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
           .

           IF      RESPONSE-CODE NOT = 0
               AND RESPONSE-CODE NOT = 14
               PERFORM 9999-TERMINATE-PROGRAM
           END-IF.
      *
       3000-PROCESS-CHANGE-CUSTOMER.
      *
           PERFORM 2100-RECEIVE-DATA-MAP.
           MOVE CUSTNO2I TO CM-CUSTOMER-NUMBER.
           PERFORM 3100-READ-CUSTOMER-FOR-UPDATE.
           IF RESPONSE-CODE = 0
      *        EXEC CICS
      *            READQ TS QUEUE(TS-QUEUE-NAME)
      *                     INTO(TS-CUSTOMER-RECORD)
      *                     ITEM(TS-ITEM-NUMBER)
      *        END-EXEC
           MOVE 7 TO OPENKICKS-CMD
           MOVE TS-QUEUE-NAME TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA2 TO ADDRESS OF TS-CUSTOMER-RECORD
           MOVE LENGTH OF TS-CUSTOMER-RECORD TO OPENKICKS-LENGTHOF
           MOVE TS-ITEM-NUMBER TO OPENKICKS-NUM3
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 4098 TO OPENKICKS-USED
           MOVE 256 TO OPENKICKS-EXTRA1
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
               IF CUSTOMER-MASTER-RECORD = TS-CUSTOMER-RECORD
                   PERFORM 3200-REWRITE-CUSTOMER-RECORD
                   MOVE 'Customer record updated.' TO MSG1O
                   SET SEND-ERASE TO TRUE
               ELSE
                   MOVE 'Another user has updated the record.  Try again
      -                 '.' TO MSG1O
                   SET SEND-ERASE-ALARM TO TRUE
               END-IF
           ELSE
               IF RESPONSE-CODE = 13
                   MOVE 'Another user has deleted the record.' TO
                       MSG1O
                   SET SEND-ERASE-ALARM TO TRUE
               END-IF
           END-IF.
           MOVE -1 TO CUSTNO1L.
           PERFORM 1500-SEND-KEY-MAP.
           SET PROCESS-KEY-MAP TO TRUE.
      *
       3100-READ-CUSTOMER-FOR-UPDATE.
      *
      *    EXEC CICS
      *        READ FILE('CUSTMAS')
      *             INTO(CUSTOMER-MASTER-RECORD)
      *             RIDFLD(CM-CUSTOMER-NUMBER)
      *             UPDATE
      *             RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 18 TO OPENKICKS-CMD
           MOVE 'CUSTMAS' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-MASTER-RECORD
           MOVE LENGTH OF CUSTOMER-MASTER-RECORD TO OPENKICKS-LENGTHOF
           SET OPENKICKS-DATA2 TO ADDRESS OF CM-CUSTOMER-NUMBER
           MOVE 8 TO OPENKICKS-FLAG
           MOVE 6146 TO OPENKICKS-USED
           MOVE 4 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           MOVE EIBRESP TO RESPONSE-CODE
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
           .

           IF      RESPONSE-CODE NOT = 0
               AND RESPONSE-CODE NOT = 13
               PERFORM 9999-TERMINATE-PROGRAM
           END-IF.
      *
       3200-REWRITE-CUSTOMER-RECORD.
      *
           MOVE LNAMEI   TO CM-LAST-NAME.
           MOVE FNAMEI   TO CM-FIRST-NAME.
           MOVE ADDRI    TO CM-ADDRESS.
           MOVE CITYI    TO CM-CITY.
           MOVE STATEI   TO CM-STATE.
           MOVE ZIPCODEI TO CM-ZIP-CODE.
      *    EXEC CICS
      *        REWRITE FILE('CUSTMAS')
      *                FROM(CUSTOMER-MASTER-RECORD)
      *                RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 27 TO OPENKICKS-CMD
           MOVE 'CUSTMAS' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-MASTER-RECORD
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 2050 TO OPENKICKS-USED
           MOVE 4 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           MOVE EIBRESP TO RESPONSE-CODE
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
           .

           IF RESPONSE-CODE NOT = 0
               PERFORM 9999-TERMINATE-PROGRAM
           END-IF.
      *
       4000-PROCESS-DELETE-CUSTOMER.
      *
           MOVE CA-CUSTOMER-NUMBER TO CM-CUSTOMER-NUMBER.
           PERFORM 3100-READ-CUSTOMER-FOR-UPDATE.
           IF RESPONSE-CODE = 0
      *        EXEC CICS
      *            READQ TS QUEUE(TS-QUEUE-NAME)
      *                     INTO(TS-CUSTOMER-RECORD)
      *                     ITEM(TS-ITEM-NUMBER)
      *        END-EXEC
           MOVE 7 TO OPENKICKS-CMD
           MOVE TS-QUEUE-NAME TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA2 TO ADDRESS OF TS-CUSTOMER-RECORD
           MOVE LENGTH OF TS-CUSTOMER-RECORD TO OPENKICKS-LENGTHOF
           MOVE TS-ITEM-NUMBER TO OPENKICKS-NUM3
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 4098 TO OPENKICKS-USED
           MOVE 256 TO OPENKICKS-EXTRA1
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
               IF CUSTOMER-MASTER-RECORD = TS-CUSTOMER-RECORD
                   PERFORM 4100-DELETE-CUSTOMER-RECORD
                   MOVE 'Customer deleted.' TO MSG1O
                   SET SEND-ERASE TO TRUE
               ELSE
                   MOVE 'Another user has updated the record.  Try again
      -                 '.' TO MSG1O
                   SET SEND-ERASE-ALARM TO TRUE
               END-IF
           ELSE
               IF RESPONSE-CODE = 13
                   MOVE 'Another user has deleted the record.' TO
                       MSG1O
                   SET SEND-ERASE-ALARM TO TRUE
               END-IF
           END-IF.
           MOVE -1 TO CUSTNO1L.
           PERFORM 1500-SEND-KEY-MAP.
           SET PROCESS-KEY-MAP TO TRUE.
      *
       4100-DELETE-CUSTOMER-RECORD.
      *
      *    EXEC CICS
      *        DELETE FILE('CUSTMAS')
      *               RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 23 TO OPENKICKS-CMD
           MOVE 'CUSTMAS' TO OPENKICKS-CHAR8A
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 2 TO OPENKICKS-USED
           MOVE 4 TO OPENKICKS-EXTRA1
           CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
           RETURNING INT
           END-CALL
           MOVE EIBRESP TO RESPONSE-CODE
           IF OPENKICKS-PARAGRAPH NOT EQUAL 0
              IF OPENKICKS-PARAGRAPH EQUAL -1
                  GO TO OPENKICKS-ABEND-SUB
              ELSE
                  GO TO OPENKICKS-HANDLE
              END-IF
           END-IF
           .

           IF  RESPONSE-CODE NOT = 0
               PERFORM 9999-TERMINATE-PROGRAM
           END-IF.
      *
       9999-TERMINATE-PROGRAM.
      *
      *    EXEC CICS
      *        ABEND
      *    END-EXEC.
           MOVE 12 TO OPENKICKS-CMD
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

       OPENKICKS-ABEND-SUB SECTION.
       OPENKICKS-ABEND-SUB-P.
            MOVE 255 TO OPENKICKS-CMD
          CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
            RETURNING INT
            END-CALL.
            GOBACK.
       OPENKICKS-HANDLE SECTION.
       OPENKICKS-HANDLE-P.
