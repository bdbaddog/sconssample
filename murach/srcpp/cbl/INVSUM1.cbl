       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. INVSUM1.
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
           05  INVOICE-EOF-SW          PIC X(01)    VALUE 'N'.
               88  INVOICE-EOF                      VALUE 'Y'.
           05  FIRST-RECORD-SW         PIC X(01)    VALUE 'Y'.
               88  FIRST-RECORD                     VALUE 'Y'.
      *
       01  WORK-FIELDS.
      *
           05  INVOICE-COUNT           PIC S9(05)    COMP-3  VALUE ZERO.
           05  INVOICE-TOTAL           PIC S9(07)V99 COMP-3  VALUE ZERO.
      *
       01  RESPONSE-CODE               PIC S9(08)    COMP.
      *
      *   Micro Focus BMS Screen Painter (ver MFBM 2.0.11)
      *   MapSet Name   SUMSET1
      *   Date Created  04/16/2001
      *   Time Created  15:17:40
      *  Output Data For Map SUMMAP1
         01 SUMMAP1O.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 TRANIDA                        PIC X.
            03 TRANIDO                        PIC X(4).
            03 FILLER                         PIC X(2).
            03 COUNTA                         PIC X.
            03 COUNTO                         PIC ZZ,ZZ9.
            03 FILLER                         PIC X(2).
            03 FIRSTA                         PIC X.
            03 FIRSTO                         PIC 999999.
            03 FILLER                         PIC X(2).
            03 LASTA                          PIC X.
            03 LASTO                          PIC 999999.
            03 FILLER                         PIC X(2).
            03 TOTALA                         PIC X.
            03 TOTALO                         PIC $$,$$$,$$9.99.
            03 FILLER                         PIC X(2).
            03 MESSAGEA                       PIC X.
            03 MESSAGEO                       PIC X(79).
            03 FILLER                         PIC X(2).
            03 DUMMYA                         PIC X.
            03 DUMMYO                         PIC X(1).
      *
       01  INVOICE-RECORD.
      *
           05  INV-INVOICE-NUMBER              PIC 9(06).
           05  INV-INVOICE-DATE                PIC X(08).
           05  INV-CUSTOMER-NUMBER             PIC X(06).
           05  INV-PO-NUMBER                   PIC X(10).
           05  INV-LINE-ITEM                   OCCURS 10 TIMES.
               10  INV-PRODUCT-CODE            PIC X(10).
               10  INV-QUANTITY                PIC S9(07)     COMP-3.
               10  INV-UNIT-PRICE              PIC S9(07)V99  COMP-3.
               10  INV-AMOUNT                  PIC S9(07)V99  COMP-3.
           05  INV-INVOICE-TOTAL               PIC S9(07)V99  COMP-3.
      *
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
       01 CALL-NAME    PIC X(10).
       01  DFHCOMMAREA PIC X(1).
       PROCEDURE DIVISION USING DFHEIBLK, DFHCOMMAREA.
            IF EIBRESP EQUAL 111
               CALL "raise" USING BY VALUE 6
            END-IF.
           MOVE 0 TO OPENKICKS-NUM1
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
       0000-PREPARE-INVOICE-SUMMARY.
      *
           MOVE LOW-VALUE TO SUMMAP1O.
           PERFORM 1000-START-INVOICE-BROWSE.
           PERFORM 2000-READ-NEXT-INVOICE
               UNTIL INVOICE-EOF.
           PERFORM 3000-END-INVOICE-BROWSE.
           PERFORM 4000-SEND-SUMMARY-MAP.
      *
      *    EXEC CICS
      *        RETURN TRANSID('MENU')
      *    END-EXEC.
           MOVE 11 TO OPENKICKS-CMD
           MOVE 'MENU' TO OPENKICKS-CHAR4A
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 512 TO OPENKICKS-USED
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
       1000-START-INVOICE-BROWSE.
      *
           MOVE 0 TO INV-INVOICE-NUMBER
      *
      *    EXEC CICS
      *        STARTBR FILE('INVOICE')
      *                RIDFLD(INV-INVOICE-NUMBER)
      *                RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 20 TO OPENKICKS-CMD
           MOVE 'INVOICE' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF INV-INVOICE-NUMBER
           MOVE LENGTH OF INV-INVOICE-NUMBER TO OPENKICKS-LENGTHOF
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
           IF RESPONSE-CODE = 13
               MOVE 'Y' TO INVOICE-EOF-SW
           ELSE
               IF RESPONSE-CODE NOT = 0
                   PERFORM 9999-TERMINATE-PROGRAM
               END-IF
           END-IF.
      *
       2000-READ-NEXT-INVOICE.
      *
      *    EXEC CICS
      *        READNEXT FILE('INVOICE')
      *                 INTO(INVOICE-RECORD)
      *                 RIDFLD(INV-INVOICE-NUMBER)
      *                 RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 21 TO OPENKICKS-CMD
           MOVE 'INVOICE' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF INVOICE-RECORD
           SET OPENKICKS-DATA2 TO ADDRESS OF INV-INVOICE-NUMBER
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
                   MOVE INV-INVOICE-NUMBER TO LASTO
                   ADD 1 TO INVOICE-COUNT
                   ADD INV-INVOICE-TOTAL TO INVOICE-TOTAL
                   IF FIRST-RECORD
                       MOVE INV-INVOICE-NUMBER TO FIRSTO
                       MOVE 'N' TO FIRST-RECORD-SW
                   END-IF
               WHEN 20
                   MOVE 'Y' TO INVOICE-EOF-SW
               WHEN OTHER
                   PERFORM 9999-TERMINATE-PROGRAM
           END-EVALUATE.
      *
       3000-END-INVOICE-BROWSE.
      *
      *    EXEC CICS
      *        ENDBR FILE('INVOICE')
      *              RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 25 TO OPENKICKS-CMD
           MOVE 'INVOICE' TO OPENKICKS-CHAR8A
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
       4000-SEND-SUMMARY-MAP.
      *
           MOVE 'SUM1'        TO TRANIDO.
           MOVE INVOICE-COUNT TO COUNTO.
           MOVE INVOICE-TOTAL TO TOTALO.
      *
      *    EXEC CICS
      *        SEND MAP('SUMMAP1')
      *             MAPSET('SUMSET1')
      *             FROM(SUMMAP1O)
      *             ERASE
      *    END-EXEC.
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'SUMMAP1' TO OPENKICKS-CHAR8A
           MOVE 'SUMSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF SUMMAP1O
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
           .

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
