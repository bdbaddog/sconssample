       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  SYSERR.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       COPY OPENKICK.
       01 CALL-NAME    PIC X(10).
      *
       01  ERROR-MESSAGE.
      *
           05  ERROR-LINE-1.
               10  FILLER      PIC X(20)  VALUE 'A serious error has '.
               10  FILLER      PIC X(20)  VALUE 'occurred.  Please co'.
               10  FILLER      PIC X(20)  VALUE 'ntact technical supp'.
               10  FILLER      PIC X(19)  VALUE 'ort.               '.
           05  ERROR-LINE-2    PIC X(79)  VALUE SPACE.
           05  ERROR-LINE-3.
               10  FILLER      PIC X(11)  VALUE 'EIBRESP  = '.
               10  EM-RESP     PIC Z(08)9.
               10  FILLER      PIC X(59)  VALUE SPACE.
           05  ERROR-LINE-4.
               10  FILLER      PIC X(11)  VALUE 'EIBRESP2 = '.
               10  EM-RESP2    PIC Z(08)9.
               10  FILLER      PIC X(59)  VALUE SPACE.
           05  ERROR-LINE-5.
               10  FILLER      PIC X(11)  VALUE 'EIBTRNID = '.
               10  EM-TRNID    PIC X(04).
               10  FILLER      PIC X(64)  VALUE SPACE.
           05  ERROR-LINE-6.
               10  FILLER      PIC X(11)  VALUE 'EIBRSRCE = '.
               10  EM-RSRCE    PIC X(08).
               10  FILLER      PIC X(60)  VALUE SPACE.
           05  ERROR-LINE-7    PIC X(79)  VALUE SPACE.
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
       01  DFHCOMMAREA         PIC X(20).
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
       0000-DISPLAY-ERROR-MESSAGE.
      *
           MOVE DFHCOMMAREA TO ERROR-PARAMETERS.
           MOVE ERR-RESP  TO EM-RESP.
           MOVE ERR-RESP2 TO EM-RESP2.
           MOVE ERR-TRNID TO EM-TRNID.
           MOVE ERR-RSRCE TO EM-RSRCE.
      *    EXEC CICS
      *        SEND TEXT FROM(ERROR-MESSAGE)
      *                  ERASE
      *                  ALARM
      *                  FREEKB
      *    END-EXEC.
           MOVE 53 TO OPENKICKS-CMD
           SET OPENKICKS-DATA1 TO ADDRESS OF ERROR-MESSAGE
           MOVE 7 TO OPENKICKS-FLAG
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

       OPENKICKS-ABEND-SUB SECTION.
       OPENKICKS-ABEND-SUB-P.
            MOVE 255 TO OPENKICKS-CMD
          CALL OPENKICKS USING BY REFERENCE OPENKICKS-PASSDATA, VALUE 1
            RETURNING INT
            END-CALL.
            GOBACK.
       OPENKICKS-HANDLE SECTION.
       OPENKICKS-HANDLE-P.
