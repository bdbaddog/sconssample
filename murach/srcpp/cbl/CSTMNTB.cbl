       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  CSTMNTB.
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
           05  RESPONSE-CODE                   PIC S9(08) COMP.
      *
       01  COMMUNICATION-AREA.
      *
           05  CA-CONTEXT-FLAG                 PIC X(01).
               88  PROCESS-KEY-MAP                        VALUE '1'.
               88  PROCESS-ADD-CUSTOMER                   VALUE '2'.
               88  PROCESS-CHANGE-CUSTOMER                VALUE '3'.
               88  PROCESS-DELETE-CUSTOMER                VALUE '4'.
           05  CA-ACTION-FLAG                  PIC X(01).
               88  ADD-REQUEST                            VALUE '1'.
               88  CHANGE-REQUEST                         VALUE '2'.
               88  DELETE-REQUEST                         VALUE '3'.
           05  CA-CUSTOMER-RECORD.
               10  CA-CUSTOMER-NUMBER          PIC X(06).
               10  CA-FIRST-NAME               PIC X(20).
               10  CA-LAST-NAME                PIC X(30).
               10  CA-ADDRESS                  PIC X(30).
               10  CA-CITY                     PIC X(20).
               10  CA-STATE                    PIC X(02).
               10  CA-ZIP-CODE                 PIC X(10).
           05  CA-SAVE-CUSTOMER-MASTER         PIC X(118).
           05  CA-RETURN-CONDITION             PIC X(01).
               88  PROCESS-OK                             VALUE '1'.
               88  PROCESS-ERROR                          VALUE '2'.
               88  PROCESS-SEVERE-ERROR                   VALUE '3'.
           05  CA-RETURN-MESSAGE               PIC X(79).
           05  CA-ERROR-PARAMETERS.
               10  CA-ERR-RESP                 PIC S9(08) COMP.
               10  CA-ERR-RESP2                PIC S9(08) COMP.
               10  CA-ERR-RSRCE                PIC X(08).
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
       LINKAGE SECTION.
       COPY DFHEIBLK.
      *
       01  DFHCOMMAREA                         PIC X(334).
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
       0000-PROCESS-CUSTOMER-RECORD.
      *
           IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA
               SET PROCESS-SEVERE-ERROR TO TRUE
               PERFORM 9000-SET-ERROR-INFO
           ELSE
               MOVE DFHCOMMAREA TO COMMUNICATION-AREA
               EVALUATE TRUE
                   WHEN PROCESS-KEY-MAP
                       PERFORM 1000-PROCESS-CUSTOMER-KEY
                   WHEN PROCESS-ADD-CUSTOMER
                       PERFORM 2000-PROCESS-ADD-CUSTOMER
                   WHEN PROCESS-CHANGE-CUSTOMER
                       PERFORM 3000-PROCESS-CHANGE-CUSTOMER
                   WHEN PROCESS-DELETE-CUSTOMER
                       PERFORM 4000-PROCESS-DELETE-CUSTOMER
               END-EVALUATE
           END-IF.
      *
           MOVE COMMUNICATION-AREA TO DFHCOMMAREA.
      *    EXEC CICS
      *        RETURN
      *    END-EXEC.
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
           .

      *
       1000-PROCESS-CUSTOMER-KEY.
      *
           PERFORM 1100-READ-CUSTOMER-RECORD.
           EVALUATE RESPONSE-CODE
               WHEN 0
                   IF ADD-REQUEST
                       SET PROCESS-ERROR TO TRUE
                       MOVE 'That customer already exists.' TO
                           CA-RETURN-MESSAGE
                   ELSE
                       SET PROCESS-OK TO TRUE
                       MOVE CUSTOMER-MASTER-RECORD TO CA-CUSTOMER-RECORD
                       MOVE CUSTOMER-MASTER-RECORD TO
                           CA-SAVE-CUSTOMER-MASTER
                       MOVE SPACE TO CA-RETURN-MESSAGE
                   END-IF
               WHEN 13
                   IF ADD-REQUEST
                       SET PROCESS-OK TO TRUE
                   ELSE
                       SET PROCESS-ERROR TO TRUE
                       MOVE 'That customer does not exist.' TO
                           CA-RETURN-MESSAGE
                   END-IF
               WHEN OTHER
                   SET PROCESS-SEVERE-ERROR TO TRUE
                   PERFORM 9000-SET-ERROR-INFO
           END-EVALUATE.
      *
       1100-READ-CUSTOMER-RECORD.
      *
      *    EXEC CICS
      *        READ FILE('CUSTMAS')
      *             INTO(CUSTOMER-MASTER-RECORD)
      *             RIDFLD(CA-CUSTOMER-NUMBER)
      *             RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 18 TO OPENKICKS-CMD
           MOVE 'CUSTMAS' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-MASTER-RECORD
           MOVE LENGTH OF CUSTOMER-MASTER-RECORD TO OPENKICKS-LENGTHOF
           SET OPENKICKS-DATA2 TO ADDRESS OF CA-CUSTOMER-NUMBER
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
       2000-PROCESS-ADD-CUSTOMER.
      *
           MOVE CA-CUSTOMER-RECORD TO CUSTOMER-MASTER-RECORD.
           PERFORM 2100-WRITE-CUSTOMER-RECORD.
           EVALUATE RESPONSE-CODE
               WHEN 0
                   SET PROCESS-OK TO TRUE
                   MOVE 'Customer record added.' TO CA-RETURN-MESSAGE
               WHEN 14
                   SET PROCESS-ERROR TO TRUE
                   MOVE 'Another user has added a record with that custo
      -                 'mer number.' TO CA-RETURN-MESSAGE
               WHEN OTHER
                   SET PROCESS-SEVERE-ERROR TO TRUE
                   PERFORM 9000-SET-ERROR-INFO
           END-EVALUATE.
      *
       2100-WRITE-CUSTOMER-RECORD.
      *
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

      *
       3000-PROCESS-CHANGE-CUSTOMER.
      *
           PERFORM 3100-READ-CUSTOMER-FOR-UPDATE.
           EVALUATE RESPONSE-CODE
               WHEN 0
                   IF CUSTOMER-MASTER-RECORD = CA-SAVE-CUSTOMER-MASTER
                       MOVE CA-CUSTOMER-RECORD TO
                           CUSTOMER-MASTER-RECORD
                       PERFORM 3200-REWRITE-CUSTOMER-RECORD
                       IF RESPONSE-CODE NOT = 0
                           SET PROCESS-SEVERE-ERROR TO TRUE
                           PERFORM 9000-SET-ERROR-INFO
                       ELSE
                           SET PROCESS-OK TO TRUE
                           MOVE 'Customer record updated.' TO
                               CA-RETURN-MESSAGE
                       END-IF
                   ELSE
                       SET PROCESS-ERROR TO TRUE
                       MOVE 'Another user has updated the record. Try ag
      -                    'ain.' TO CA-RETURN-MESSAGE
                   END-IF
               WHEN 13
                   SET PROCESS-ERROR TO TRUE
                   MOVE 'Another user has deleted the record.'
                       TO CA-RETURN-MESSAGE
               WHEN OTHER
                   SET PROCESS-SEVERE-ERROR TO TRUE
                   PERFORM 9000-SET-ERROR-INFO
           END-EVALUATE.
      *
       3100-READ-CUSTOMER-FOR-UPDATE.
      *
      *    EXEC CICS
      *        READ FILE('CUSTMAS')
      *             INTO(CUSTOMER-MASTER-RECORD)
      *             RIDFLD(CA-CUSTOMER-NUMBER)
      *             UPDATE
      *             RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 18 TO OPENKICKS-CMD
           MOVE 'CUSTMAS' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-MASTER-RECORD
           MOVE LENGTH OF CUSTOMER-MASTER-RECORD TO OPENKICKS-LENGTHOF
           SET OPENKICKS-DATA2 TO ADDRESS OF CA-CUSTOMER-NUMBER
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

      *
       3200-REWRITE-CUSTOMER-RECORD.
      *
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

      *
       4000-PROCESS-DELETE-CUSTOMER.
      *
           PERFORM 3100-READ-CUSTOMER-FOR-UPDATE.
           EVALUATE RESPONSE-CODE
               WHEN 0
                   IF CUSTOMER-MASTER-RECORD = CA-SAVE-CUSTOMER-MASTER
                       PERFORM 4100-DELETE-CUSTOMER-RECORD
                       IF RESPONSE-CODE NOT = 0
                           SET PROCESS-SEVERE-ERROR TO TRUE
                           PERFORM 9000-SET-ERROR-INFO
                       ELSE
                           SET PROCESS-OK TO TRUE
                           MOVE 'Customer record deleted.' TO
                               CA-RETURN-MESSAGE
                       END-IF
                   ELSE
                       SET PROCESS-ERROR TO TRUE
                       MOVE 'Another user has updated the record.  Try a
      -                    'gain.' TO CA-RETURN-MESSAGE
                   END-IF
               WHEN 13
                   SET PROCESS-ERROR TO TRUE
                   MOVE 'Another user has deleted the record.'
                       TO CA-RETURN-MESSAGE
               WHEN OTHER
                   SET PROCESS-SEVERE-ERROR TO TRUE
                   PERFORM 9000-SET-ERROR-INFO
           END-EVALUATE.
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

      *
       9000-SET-ERROR-INFO.
      *
           MOVE EIBRESP  TO CA-ERR-RESP.
           MOVE EIBRESP2 TO CA-ERR-RESP2.
           MOVE EIBRSRCE TO CA-ERR-RSRCE.
       OPENKICKS-ABEND-SUB SECTION.
       OPENKICKS-ABEND-SUB-P.
            MOVE 255 TO OPENKICKS-CMD
          CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
            RETURNING INT
            END-CALL.
            GOBACK.
       OPENKICKS-HANDLE SECTION.
       OPENKICKS-HANDLE-P.
