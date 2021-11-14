       IDENTIFICATION  DIVISION.
      *
       PROGRAM-ID.  INVMENU.
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
           05  VALID-DATA-SW               PIC X(01) VALUE 'Y'.
               88  VALID-DATA              VALUE 'Y'.
      *
       01  FLAGS.
      *
           05  SEND-FLAG                   PIC X(01).
               88  SEND-ERASE              VALUE '1'.
               88  SEND-DATAONLY           VALUE '2'.
               88  SEND-DATAONLY-ALARM     VALUE '3'.
      *
       01  PROGRAM-TABLE.
      *
           05  PROGRAM-LIST.
               10  PROGRAM-1               PIC X(08) VALUE 'CUSTINQ1'.
               10  PROGRAM-2               PIC X(08) VALUE 'CUSTMNT2'.
               10  PROGRAM-3               PIC X(08) VALUE 'ORDRENT '.
           05  PROGRAM-NAME                REDEFINES PROGRAM-LIST
                                           OCCURS 3 TIMES
                                           PIC X(08).
      *
       01  SUBSCRIPTS.
           05  ACTION-SUB              PIC 9(01).
      *
       01  END-OF-SESSION-MESSAGE      PIC X(13) VALUE 'Session ended'.
      *
       01  RESPONSE-CODE               PIC S9(08) COMP.
      *
       01  COMMUNICATION-AREA          PIC X(01).
      *
      *   Micro Focus BMS Screen Painter (ver MFBM 2.0.11)
      *   MapSet Name   MENSET1
      *   Date Created  04/11/2001
      *   Time Created  12:46:56
      *  Input Data For Map MENMAP1
         01 MENMAP1I.
            03 FILLER                         PIC X(12).
            03 TRANIDL                        PIC S9(4) COMP.
            03 TRANIDF                        PIC X.
            03 FILLER REDEFINES TRANIDF.
               05 TRANIDA                        PIC X.
            03 FILLER                         PIC X(2).
            03 TRANIDI                        PIC X(4).
            03 ACTIONL                        PIC S9(4) COMP.
            03 ACTIONF                        PIC X.
            03 FILLER REDEFINES ACTIONF.
               05 ACTIONA                        PIC X.
            03 FILLER                         PIC X(2).
            03 ACTIONI                        PIC X(1).
            03 MESSAGEL                       PIC S9(4) COMP.
            03 MESSAGEF                       PIC X.
            03 FILLER REDEFINES MESSAGEF.
               05 MESSAGEA                       PIC X.
            03 FILLER                         PIC X(2).
            03 MESSAGEI                       PIC X(79).
            03 DUMMYL                         PIC S9(4) COMP.
            03 DUMMYF                         PIC X.
            03 FILLER REDEFINES DUMMYF.
               05 DUMMYA                         PIC X.
            03 FILLER                         PIC X(2).
            03 DUMMYI                         PIC X(1).
      *  Output Data For Map MENMAP1
         01 MENMAP1O REDEFINES MENMAP1I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TRANIDC                        PIC X.
            03 TRANIDH                        PIC X.
            03 TRANIDO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 ACTIONC                        PIC X.
            03 ACTIONH                        PIC X.
            03 ACTIONO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 MESSAGEC                       PIC X.
            03 MESSAGEH                       PIC X.
            03 MESSAGEO                       PIC X(79).
            03 FILLER                         PIC X(3).
            03 DUMMYC                         PIC X.
            03 DUMMYH                         PIC X.
            03 DUMMYO                         PIC X(1).
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
       01  DFHCOMMAREA                 PIC X(01).
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
       0000-PROCESS-MASTER-MENU.
      *
      * test by sam
           DISPLAY "EIBCALEN is ", EIBCALEN
           EVALUATE TRUE
      *
               WHEN EIBCALEN = ZERO
                   MOVE LOW-VALUE TO MENMAP1O
                   SET SEND-ERASE TO TRUE
                   PERFORM 1400-SEND-MENU-MAP
      *
               WHEN EIBAID = DFHCLEAR
                   MOVE LOW-VALUE TO MENMAP1O
                   SET SEND-ERASE TO TRUE
                   PERFORM 1400-SEND-MENU-MAP
      *
               WHEN EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
                   CONTINUE
      *
               WHEN EIBAID = DFHPF3 OR DFHPF12
                   PERFORM 2000-SEND-TERMINATION-MESSAGE
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
      *
               WHEN EIBAID = DFHENTER
                   PERFORM 1000-PROCESS-MENU-MAP
      *
               WHEN OTHER
                   MOVE 'Invalid key pressed.' TO MESSAGEO
                   SET SEND-DATAONLY-ALARM TO TRUE
                   PERFORM 1400-SEND-MENU-MAP
      *
           END-EVALUATE.
      *
      * test by sam
           DISPLAY "RETURN TO OPENKICKS"      
      *    EXEC CICS
      *        RETURN TRANSID('MENU')
      *               COMMAREA(COMMUNICATION-AREA)
      *    END-EXEC.
           MOVE 11 TO OPENKICKS-CMD
           MOVE 'MENU' TO OPENKICKS-CHAR4A
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
       1000-PROCESS-MENU-MAP.
      *
           PERFORM 1100-RECEIVE-MENU-MAP.
           PERFORM 1200-EDIT-MENU-DATA.
           IF VALID-DATA
               MOVE ACTIONI TO ACTION-SUB
               PERFORM 1300-BRANCH-TO-PROGRAM
           END-IF.
           SET SEND-DATAONLY-ALARM TO TRUE.
           PERFORM 1400-SEND-MENU-MAP.
      *
       1100-RECEIVE-MENU-MAP.
      *
      *    EXEC CICS
      *        RECEIVE MAP('MENMAP1')
      *                MAPSET('MENSET1')
      *                INTO(MENMAP1I)
      *    END-EXEC.
           MOVE 29 TO OPENKICKS-CMD
           MOVE 'MENMAP1' TO OPENKICKS-CHAR8A
           MOVE 'MENSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF MENMAP1I
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
       1200-EDIT-MENU-DATA.
      *
           IF ACTIONI NOT = '1' AND '2' AND '3'
               MOVE ATTR-REVERSE TO ACTIONH
               MOVE 'You must enter 1, 2, or 3.' TO MESSAGEO
               MOVE 'N' TO VALID-DATA-SW
           END-IF.
      *
       1300-BRANCH-TO-PROGRAM.
      *
      *    EXEC CICS
      *        XCTL PROGRAM(PROGRAM-NAME(ACTION-SUB))
      *        RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 10 TO OPENKICKS-CMD
           MOVE PROGRAM-NAME(ACTION-SUB) TO OPENKICKS-CHAR8A
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
           IF EIBRESP EQUAL 0
              GOBACK
           END-IF
           .

      *
           MOVE 'That program is not available.' TO MESSAGEO.
      *
       1400-SEND-MENU-MAP.
      *
           MOVE 'MENU' TO TRANIDO.
           EVALUATE TRUE
               WHEN SEND-ERASE
                   DISPLAY "SENDFLAG is ERASE "
      *            EXEC CICS
      *                SEND MAP('MENMAP1')
      *                     MAPSET('MENSET1')
      *                     FROM(MENMAP1O)
      *                     ERASE
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'MENMAP1' TO OPENKICKS-CHAR8A
           MOVE 'MENSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF MENMAP1O
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
               WHEN SEND-DATAONLY
                   DISPLAY "SENDFLAG is DATAONLY "
      *            EXEC CICS
      *                SEND MAP('MENMAP1')
      *                     MAPSET('MENSET1')
      *                     FROM(MENMAP1O)
      *                     DATAONLY
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'MENMAP1' TO OPENKICKS-CHAR8A
           MOVE 'MENSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF MENMAP1O
           MOVE 2 TO OPENKICKS-FLAG
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
                   DISPLAY "SENDFLAG is ALARM "
      *            EXEC CICS
      *                SEND MAP('MENMAP1')
      *                     MAPSET('MENSET1')
      *                     FROM(MENMAP1O)
      *                     DATAONLY
      *                     ALARM
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'MENMAP1' TO OPENKICKS-CHAR8A
           MOVE 'MENSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF MENMAP1O
           MOVE 514 TO OPENKICKS-FLAG
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
       2000-SEND-TERMINATION-MESSAGE.
      *
      *    EXEC CICS
      *        SEND TEXT FROM(END-OF-SESSION-MESSAGE)
      *                  ERASE
      *                  FREEKB
      *    END-EXEC.
           MOVE 53 TO OPENKICKS-CMD
           SET OPENKICKS-DATA1 TO ADDRESS OF END-OF-SESSION-MESSAGE
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
