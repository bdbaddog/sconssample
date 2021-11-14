       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  CUSTINQ2.
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
           05  VALID-DATA-SW               PIC X(01)  VALUE 'Y'.
               88  VALID-DATA                         VALUE 'Y'.
           05  CUSTOMER-FOUND-SW           PIC X(01)  VALUE 'Y'.
               88  CUSTOMER-FOUND                     VALUE 'Y'.
      *
       01  FLAGS.
      *
           05  DISPLAY-FLAG                PIC X(01).
               88  DISPLAY-NEW-CUSTOMER               VALUE '1'.
               88  DISPLAY-SPACES                     VALUE '2'.
               88  DISPLAY-LOW-VALUES                 VALUE '3'.
           05  SEND-FLAG                   PIC X(01).
               88  SEND-ERASE                         VALUE '1'.
               88  SEND-DATAONLY                      VALUE '2'.
               88  SEND-DATAONLY-ALARM                VALUE '3'.
      *
       01  COMMUNICATION-AREA.
      *
           05  CA-CUSTOMER-NUMBER          PIC X(06).
      *
       01  RESPONSE-CODE                   PIC S9(08) COMP.
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
      *   MapSet Name   INQSET2
      *   Date Created  04/16/2001
      *   Time Created  15:17:42
      *  Input Data For Map INQMAP2
         01 INQMAP2I.
            03 FILLER                         PIC X(12).
            03 TRANIDL                        PIC S9(4) COMP.
            03 TRANIDF                        PIC X.
            03 FILLER REDEFINES TRANIDF.
               05 TRANIDA                        PIC X.
            03 FILLER                         PIC X(2).
            03 TRANIDI                        PIC X(4).
            03 CUSTNOL                        PIC S9(4) COMP.
            03 CUSTNOF                        PIC X.
            03 FILLER REDEFINES CUSTNOF.
               05 CUSTNOA                        PIC X.
            03 FILLER                         PIC X(2).
            03 CUSTNOI                        PIC X(6).
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
      *  Output Data For Map INQMAP2
         01 INQMAP2O REDEFINES INQMAP2I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TRANIDC                        PIC X.
            03 TRANIDH                        PIC X.
            03 TRANIDO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 CUSTNOC                        PIC X.
            03 CUSTNOH                        PIC X.
            03 CUSTNOO                        PIC X(6).
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
       01  ERROR-PARAMETERS.
      *
           05  ERR-RESP          PIC S9(8)   COMP.
           05  ERR-RESP2         PIC S9(8)   COMP.
           05  ERR-TRNID         PIC X(4).
           05  ERR-RSRCE         PIC X(8).
      *
       LINKAGE SECTION.
       COPY DFHEIBLK.
      *
       01  DFHCOMMAREA                     PIC X(06).
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
       0000-PROCESS-CUSTOMER-INQUIRY.
      *
           IF EIBCALEN > ZERO
               MOVE DFHCOMMAREA TO COMMUNICATION-AREA
           END-IF.
      *
           EVALUATE TRUE
      *
               WHEN EIBCALEN = ZERO
                   MOVE LOW-VALUE TO CA-CUSTOMER-NUMBER
                   MOVE LOW-VALUE TO INQMAP2O
                   SET SEND-ERASE TO TRUE
                   PERFORM 1500-SEND-INQUIRY-MAP
      *
               WHEN EIBAID = DFHCLEAR
                   MOVE LOW-VALUE TO CA-CUSTOMER-NUMBER
                   MOVE LOW-VALUE TO INQMAP2O
                   SET SEND-ERASE TO TRUE
                   PERFORM 1500-SEND-INQUIRY-MAP
      *
               WHEN EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
                   CONTINUE
      *
               WHEN EIBAID = DFHPF3 OR DFHPF12
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
               WHEN EIBAID = DFHENTER
                   PERFORM 1000-DISPLAY-SELECTED-CUSTOMER
      *
               WHEN EIBAID = DFHPF5
                   PERFORM 2000-DISPLAY-FIRST-CUSTOMER
      *
               WHEN EIBAID = DFHPF6
                   PERFORM 3000-DISPLAY-LAST-CUSTOMER
      *
               WHEN EIBAID = DFHPF7
                   PERFORM 4000-DISPLAY-PREV-CUSTOMER
      *
               WHEN EIBAID = DFHPF8
                   PERFORM 5000-DISPLAY-NEXT-CUSTOMER
      *
               WHEN OTHER
                   MOVE LOW-VALUE TO INQMAP2O
                   MOVE 'Invalid key pressed.' TO MESSAGEO
                   SET SEND-DATAONLY-ALARM TO TRUE
                   PERFORM 1500-SEND-INQUIRY-MAP
      *
           END-EVALUATE.
      *
      *    EXEC CICS
      *        RETURN TRANSID('INQ2')
      *               COMMAREA(COMMUNICATION-AREA)
      *    END-EXEC.
           MOVE 11 TO OPENKICKS-CMD
           MOVE 'INQ2' TO OPENKICKS-CHAR4A
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
       1000-DISPLAY-SELECTED-CUSTOMER.
      *
           PERFORM 1100-RECEIVE-INQUIRY-MAP.
           PERFORM 1200-EDIT-CUSTOMER-NUMBER.
           IF VALID-DATA
               PERFORM 1300-READ-CUSTOMER-RECORD
               IF CUSTOMER-FOUND
                   SET DISPLAY-NEW-CUSTOMER TO TRUE
                   PERFORM 1400-DISPLAY-INQUIRY-RESULTS
                   MOVE CM-CUSTOMER-NUMBER TO CA-CUSTOMER-NUMBER
               ELSE
                   SET DISPLAY-SPACES TO TRUE
                   PERFORM 1400-DISPLAY-INQUIRY-RESULTS
               END-IF
           ELSE
               SET DISPLAY-LOW-VALUES TO TRUE
               PERFORM 1400-DISPLAY-INQUIRY-RESULTS
           END-IF.
      *
       1100-RECEIVE-INQUIRY-MAP.
      *
      *    EXEC CICS
      *        RECEIVE MAP('INQMAP2')
      *                MAPSET('INQSET2')
      *                INTO(INQMAP2I)
      *    END-EXEC.
           MOVE 29 TO OPENKICKS-CMD
           MOVE 'INQMAP2' TO OPENKICKS-CHAR8A
           MOVE 'INQSET2' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF INQMAP2I
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
           INSPECT INQMAP2I
               REPLACING ALL '_' BY SPACE.
      *
       1200-EDIT-CUSTOMER-NUMBER.
      *
           IF       CUSTNOL = ZERO
                 OR CUSTNOI = SPACE
               MOVE 'N' TO VALID-DATA-SW
               MOVE 'You must enter a customer number.' TO MESSAGEO
           END-IF.
      *
       1300-READ-CUSTOMER-RECORD.
      *
      *    EXEC CICS
      *        READ FILE('CUSTMAS')
      *             INTO(CUSTOMER-MASTER-RECORD)
      *             RIDFLD(CUSTNOI)
      *             RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 18 TO OPENKICKS-CMD
           MOVE 'CUSTMAS' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-MASTER-RECORD
           MOVE LENGTH OF CUSTOMER-MASTER-RECORD TO OPENKICKS-LENGTHOF
           SET OPENKICKS-DATA2 TO ADDRESS OF CUSTNOI
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

      *
           IF RESPONSE-CODE = 13
               MOVE 'N' TO CUSTOMER-FOUND-SW
               MOVE 'That customer does not exist.' TO MESSAGEO
           ELSE
               IF RESPONSE-CODE NOT = 0
                   PERFORM 9999-TERMINATE-PROGRAM
               END-IF
           END-IF.
      *
       1400-DISPLAY-INQUIRY-RESULTS.
      *
           IF DISPLAY-NEW-CUSTOMER
               MOVE CM-CUSTOMER-NUMBER TO CUSTNOO
               MOVE CM-LAST-NAME       TO LNAMEO
               MOVE CM-FIRST-NAME      TO FNAMEO
               MOVE CM-ADDRESS         TO ADDRO
               MOVE CM-CITY            TO CITYO
               MOVE CM-STATE           TO STATEO
               MOVE CM-ZIP-CODE        TO ZIPCODEO
               MOVE SPACE              TO MESSAGEO
               SET SEND-DATAONLY       TO TRUE
           ELSE
               IF DISPLAY-SPACES
                   MOVE LOW-VALUE TO CUSTNOO
                   MOVE SPACE     TO LNAMEO
                                     FNAMEO
                                     ADDRO
                                     CITYO
                                     STATEO
                                     ZIPCODEO
                   SET SEND-DATAONLY-ALARM TO TRUE
               ELSE
                   IF DISPLAY-LOW-VALUES
                       SET SEND-DATAONLY-ALARM TO TRUE
                   END-IF
               END-IF
           END-IF.
      *
           PERFORM 1500-SEND-INQUIRY-MAP.
      *
       1500-SEND-INQUIRY-MAP.
      *
           MOVE 'INQ2' TO TRANIDO.
      *
           EVALUATE TRUE
               WHEN SEND-ERASE
      *            EXEC CICS
      *                SEND MAP('INQMAP2')
      *                     MAPSET('INQSET2')
      *                     FROM(INQMAP2O)
      *                     ERASE
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'INQMAP2' TO OPENKICKS-CHAR8A
           MOVE 'INQSET2' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF INQMAP2O
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
      *            EXEC CICS
      *                SEND MAP('INQMAP2')
      *                     MAPSET('INQSET2')
      *                     FROM(INQMAP2O)
      *                     DATAONLY
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'INQMAP2' TO OPENKICKS-CHAR8A
           MOVE 'INQSET2' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF INQMAP2O
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
      *            EXEC CICS
      *                SEND MAP('INQMAP2')
      *                     MAPSET('INQSET2')
      *                     FROM(INQMAP2O)
      *                     DATAONLY
      *                     ALARM
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'INQMAP2' TO OPENKICKS-CHAR8A
           MOVE 'INQSET2' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF INQMAP2O
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
       2000-DISPLAY-FIRST-CUSTOMER.
      *
           MOVE LOW-VALUE TO CM-CUSTOMER-NUMBER
                             INQMAP2O.
           PERFORM 2100-START-CUSTOMER-BROWSE.
           IF CUSTOMER-FOUND
               PERFORM 2200-READ-NEXT-CUSTOMER
           END-IF.
           PERFORM 2300-END-CUSTOMER-BROWSE.
           IF CUSTOMER-FOUND
               SET DISPLAY-NEW-CUSTOMER TO TRUE
               PERFORM 1400-DISPLAY-INQUIRY-RESULTS
               MOVE CM-CUSTOMER-NUMBER TO CA-CUSTOMER-NUMBER
           ELSE
               SET DISPLAY-SPACES TO TRUE
               PERFORM 1400-DISPLAY-INQUIRY-RESULTS
           END-IF.
      *
       2100-START-CUSTOMER-BROWSE.
      *
      *    EXEC CICS
      *        STARTBR FILE('CUSTMAS')
      *                RIDFLD(CM-CUSTOMER-NUMBER)
      *                RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 20 TO OPENKICKS-CMD
           MOVE 'CUSTMAS' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF CM-CUSTOMER-NUMBER
           MOVE LENGTH OF CM-CUSTOMER-NUMBER TO OPENKICKS-LENGTHOF
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

      *
           IF RESPONSE-CODE = 0
               MOVE 'Y' TO CUSTOMER-FOUND-SW
               MOVE SPACE TO MESSAGEO
           ELSE
               IF RESPONSE-CODE = 13
                   MOVE 'N' TO CUSTOMER-FOUND-SW
                   MOVE 'There are no customers in the file.'
                       TO MESSAGEO
               ELSE
                   PERFORM 9999-TERMINATE-PROGRAM
               END-IF
           END-IF.
      *
       2200-READ-NEXT-CUSTOMER.
      *
      *    EXEC CICS
      *        READNEXT FILE('CUSTMAS')
      *                 INTO(CUSTOMER-MASTER-RECORD)
      *                 RIDFLD(CM-CUSTOMER-NUMBER)
      *                 RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 21 TO OPENKICKS-CMD
           MOVE 'CUSTMAS' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-MASTER-RECORD
           SET OPENKICKS-DATA2 TO ADDRESS OF CM-CUSTOMER-NUMBER
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

      *
           EVALUATE RESPONSE-CODE
               WHEN 0
                   MOVE 'Y' TO CUSTOMER-FOUND-SW
               WHEN 20
                   MOVE 'N' TO CUSTOMER-FOUND-SW
                   MOVE 'There are no more records in the file.'
                       TO MESSAGEO
               WHEN OTHER
                   PERFORM 9999-TERMINATE-PROGRAM
           END-EVALUATE.
      *
       2300-END-CUSTOMER-BROWSE.
      *
      *    EXEC CICS
      *        ENDBR FILE('CUSTMAS')
      *              RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 25 TO OPENKICKS-CMD
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

      *
           IF RESPONSE-CODE NOT = 0
               PERFORM 9999-TERMINATE-PROGRAM
           END-IF.
      *
       3000-DISPLAY-LAST-CUSTOMER.
      *
           MOVE HIGH-VALUE TO CM-CUSTOMER-NUMBER.
           MOVE LOW-VALUE  TO INQMAP2O.
           PERFORM 2100-START-CUSTOMER-BROWSE.
           IF CUSTOMER-FOUND
               PERFORM 3100-READ-PREV-CUSTOMER
           END-IF.
           PERFORM 2300-END-CUSTOMER-BROWSE.
           IF CUSTOMER-FOUND
               SET DISPLAY-NEW-CUSTOMER TO TRUE
               PERFORM 1400-DISPLAY-INQUIRY-RESULTS
               MOVE CM-CUSTOMER-NUMBER TO CA-CUSTOMER-NUMBER
           ELSE
               SET DISPLAY-SPACES TO TRUE
               PERFORM 1400-DISPLAY-INQUIRY-RESULTS
           END-IF.
      *
       3100-READ-PREV-CUSTOMER.
      *
      *    EXEC CICS
      *        READPREV FILE('CUSTMAS')
      *                 INTO(CUSTOMER-MASTER-RECORD)
      *                 RIDFLD(CM-CUSTOMER-NUMBER)
      *                 RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 22 TO OPENKICKS-CMD
           MOVE 'CUSTMAS' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-MASTER-RECORD
           SET OPENKICKS-DATA2 TO ADDRESS OF CM-CUSTOMER-NUMBER
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

      *
           EVALUATE RESPONSE-CODE
               WHEN 0
                   MOVE 'Y' TO CUSTOMER-FOUND-SW
               WHEN 20
                   MOVE 'N' TO CUSTOMER-FOUND-SW
                   MOVE 'There are no more records in the file.'
                       TO MESSAGEO
               WHEN OTHER
                   PERFORM 9999-TERMINATE-PROGRAM
           END-EVALUATE.
      *
       4000-DISPLAY-PREV-CUSTOMER.
      *
           MOVE CA-CUSTOMER-NUMBER TO CM-CUSTOMER-NUMBER.
           MOVE LOW-VALUE          TO INQMAP2O.
           PERFORM 2100-START-CUSTOMER-BROWSE.
           IF CUSTOMER-FOUND
               PERFORM 2200-READ-NEXT-CUSTOMER
               PERFORM 3100-READ-PREV-CUSTOMER
               PERFORM 3100-READ-PREV-CUSTOMER
           END-IF.
           PERFORM 2300-END-CUSTOMER-BROWSE.
           IF CUSTOMER-FOUND
               SET DISPLAY-NEW-CUSTOMER TO TRUE
               PERFORM 1400-DISPLAY-INQUIRY-RESULTS
               MOVE CM-CUSTOMER-NUMBER TO CA-CUSTOMER-NUMBER
           ELSE
               SET DISPLAY-LOW-VALUES TO TRUE
               PERFORM 1400-DISPLAY-INQUIRY-RESULTS
           END-IF.
      *
       5000-DISPLAY-NEXT-CUSTOMER.
      *
           MOVE CA-CUSTOMER-NUMBER TO CM-CUSTOMER-NUMBER.
           MOVE LOW-VALUE          TO INQMAP2O.
           PERFORM 2100-START-CUSTOMER-BROWSE.
           IF CUSTOMER-FOUND
               PERFORM 2200-READ-NEXT-CUSTOMER
               PERFORM 2200-READ-NEXT-CUSTOMER
           END-IF.
           PERFORM 2300-END-CUSTOMER-BROWSE.
           IF CUSTOMER-FOUND
               SET DISPLAY-NEW-CUSTOMER TO TRUE
               PERFORM 1400-DISPLAY-INQUIRY-RESULTS
               MOVE CM-CUSTOMER-NUMBER TO CA-CUSTOMER-NUMBER
           ELSE
               SET DISPLAY-LOW-VALUES TO TRUE
               PERFORM 1400-DISPLAY-INQUIRY-RESULTS
           END-IF.
      *
       9999-TERMINATE-PROGRAM.
      *
           MOVE EIBRESP  TO ERR-RESP.
           MOVE EIBRESP2 TO ERR-RESP2.
           MOVE EIBTRNID TO ERR-TRNID.
           MOVE EIBRSRCE TO ERR-RSRCE.
      *
      *    EXEC CICS
      *        XCTL PROGRAM('SYSERR')
      *             COMMAREA(ERROR-PARAMETERS)
      *    END-EXEC.
           MOVE 10 TO OPENKICKS-CMD
           MOVE 'SYSERR' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF ERROR-PARAMETERS
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 2050 TO OPENKICKS-USED
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
