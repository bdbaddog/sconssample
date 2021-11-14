       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  CUSTINQ3.
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
           05  VALID-DATA-SW               PIC X(01)   VALUE 'Y'.
               88  VALID-DATA                          VALUE 'Y'.
           05  CUSTOMER-FOUND-SW           PIC X(01)   VALUE 'Y'.
               88  CUSTOMER-FOUND                      VALUE 'Y'.
           05  MORE-INVOICES-SW            PIC X(01)   VALUE 'Y'.
               88  MORE-INVOICES                       VALUE 'Y'.
      *
       01  FLAGS.
      *
           05  DISPLAY-FLAG                PIC X(01).
               88  DISPLAY-NEW-CUSTOMER                VALUE '1'.
               88  DISPLAY-SPACES                      VALUE '2'.
               88  DISPLAY-LOW-VALUES                  VALUE '3'.
           05  SEND-FLAG                   PIC X(01).
               88  SEND-ERASE                          VALUE '1'.
               88  SEND-DATAONLY                       VALUE '2'.
               88  SEND-DATAONLY-ALARM                 VALUE '3'.
      *
       01  WORK-FIELDS.
      *
           05  INVOICE-SUB                 PIC S9(04) COMP.
      *
       01  INVOICE-LINE.
      *
           05  IL-INVOICE-NUMBER           PIC 9(06).
           05  FILLER                      PIC X(02)   VALUE SPACE.
           05  IL-PO-NUMBER                PIC X(10).
           05  FILLER                      PIC X(02)   VALUE SPACE.
           05  IL-INVOICE-DATE             PIC Z9/99/9999.
           05  FILLER                      PIC X(02)   VALUE SPACE.
           05  IL-INVOICE-TOTAL            PIC Z,ZZZ,ZZ9.99.
      *
       01  COMMUNICATION-AREA.
      *
           05  CA-CUSTOMER-NUMBER          PIC X(06).
      *
       01  RESPONSE-CODE                   PIC S9(08)  COMP.
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
       01  CUSTOMER-INQUIRY-MAP.
      *
           05  FILLER                  PIC X(12).
      *
           05  CIM-L-TRANID            PIC S9(04)  COMP.
           05  CIM-A-TRANID            PIC X(01).
           05  CIM-D-TRANID            PIC X(04).
      *
           05  CIM-L-CUSTNO            PIC S9(04)  COMP.
           05  CIM-A-CUSTNO            PIC X(01).
           05  CIM-D-CUSTNO            PIC X(06).
      *
           05  CIM-L-LNAME             PIC S9(04)  COMP.
           05  CIM-A-LNAME             PIC X(01).
           05  CIM-D-LNAME             PIC X(30).
      *
           05  CIM-L-FNAME             PIC S9(04)  COMP.
           05  CIM-A-FNAME             PIC X(01).
           05  CIM-D-FNAME             PIC X(20).
      *
           05  CIM-L-ADDR              PIC S9(04)  COMP.
           05  CIM-A-ADDR              PIC X(01).
           05  CIM-D-ADDR              PIC X(30).
      *
           05  CIM-L-CITY              PIC S9(04)  COMP.
           05  CIM-A-CITY              PIC X(01).
           05  CIM-D-CITY              PIC X(20).
      *
           05  CIM-L-STATE             PIC S9(04)  COMP.
           05  CIM-A-STATE             PIC X(01).
           05  CIM-D-STATE             PIC X(02).
      *
           05  CIM-L-ZIPCODE           PIC S9(04)  COMP.
           05  CIM-A-ZIPCODE           PIC X(01).
           05  CIM-D-ZIPCODE           PIC X(10).
      *
           05  CIM-INVOICE-LINE        OCCURS 10 TIMES.
      *
               10  CIM-L-INVOICE-LINE  PIC S9(04)  COMP.
               10  CIM-A-INVOICE-LINE  PIC X(01).
               10  CIM-D-INVOICE-LINE  PIC X(44).
      *
           05  CIM-L-MESSAGE           PIC S9(04)  COMP.
           05  CIM-A-MESSAGE           PIC X(01).
           05  CIM-D-MESSAGE           PIC X(79).
      *
           05  CIM-L-DUMMY             PIC S9(04)  COMP.
           05  CIM-A-DUMMY             PIC X(01).
           05  CIM-D-DUMMY             PIC X(01).
      *
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
                   MOVE LOW-VALUE TO CUSTOMER-INQUIRY-MAP
                   SET SEND-ERASE TO TRUE
                   PERFORM 1500-SEND-INQUIRY-MAP
      *
               WHEN EIBAID = DFHCLEAR
                   MOVE LOW-VALUE TO CA-CUSTOMER-NUMBER
                   MOVE LOW-VALUE TO CUSTOMER-INQUIRY-MAP
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
                   MOVE LOW-VALUE TO CUSTOMER-INQUIRY-MAP
                   MOVE 'Invalid key pressed.' TO CIM-D-MESSAGE
                   SET SEND-DATAONLY-ALARM TO TRUE
                   PERFORM 1500-SEND-INQUIRY-MAP
      *
           END-EVALUATE.
      *
      *    EXEC CICS
      *        RETURN TRANSID('INQ3')
      *               COMMAREA(COMMUNICATION-AREA)
      *    END-EXEC.
           MOVE 11 TO OPENKICKS-CMD
           MOVE 'INQ3' TO OPENKICKS-CHAR4A
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
      *        RECEIVE MAP('INQMAP3')
      *                MAPSET('INQSET3')
      *                INTO(CUSTOMER-INQUIRY-MAP)
      *    END-EXEC.
           MOVE 29 TO OPENKICKS-CMD
           MOVE 'INQMAP3' TO OPENKICKS-CHAR8A
           MOVE 'INQSET3' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-INQUIRY-MAP
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
           INSPECT CUSTOMER-INQUIRY-MAP
               REPLACING ALL '_' BY SPACE.
      *
       1200-EDIT-CUSTOMER-NUMBER.
      *
           IF       CIM-L-CUSTNO = ZERO
                 OR CIM-D-CUSTNO = SPACE
               MOVE 'N' TO VALID-DATA-SW
               MOVE 'You must enter a customer number.'
                   TO CIM-D-MESSAGE
           END-IF.
      *
       1300-READ-CUSTOMER-RECORD.
      *
      *    EXEC CICS
      *        READ FILE('CUSTMAS')
      *             INTO(CUSTOMER-MASTER-RECORD)
      *             RIDFLD(CIM-D-CUSTNO)
      *             RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 18 TO OPENKICKS-CMD
           MOVE 'CUSTMAS' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-MASTER-RECORD
           MOVE LENGTH OF CUSTOMER-MASTER-RECORD TO OPENKICKS-LENGTHOF
           SET OPENKICKS-DATA2 TO ADDRESS OF CIM-D-CUSTNO
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
               MOVE 'That customer does not exist.' TO CIM-D-MESSAGE
           ELSE
               IF RESPONSE-CODE NOT = 0
                   PERFORM 9999-TERMINATE-PROGRAM
               END-IF
           END-IF.
      *
       1400-DISPLAY-INQUIRY-RESULTS.
      *
           EVALUATE TRUE
               WHEN DISPLAY-NEW-CUSTOMER
                   MOVE CM-CUSTOMER-NUMBER TO CIM-D-CUSTNO
                   MOVE CM-LAST-NAME       TO CIM-D-LNAME
                   MOVE CM-FIRST-NAME      TO CIM-D-FNAME
                   MOVE CM-ADDRESS         TO CIM-D-ADDR
                   MOVE CM-CITY            TO CIM-D-CITY
                   MOVE CM-STATE           TO CIM-D-STATE
                   MOVE CM-ZIP-CODE        TO CIM-D-ZIPCODE
                   MOVE SPACE              TO CIM-D-MESSAGE
                   PERFORM 1410-START-INVOICE-BROWSE
                   PERFORM 1420-FORMAT-INVOICE-LINE
                       VARYING INVOICE-SUB FROM 1 BY 1
                       UNTIL INVOICE-SUB > 10
                   PERFORM 1440-END-INVOICE-BROWSE
                   SET SEND-DATAONLY TO TRUE
               WHEN DISPLAY-SPACES
                   MOVE LOW-VALUE TO CIM-D-CUSTNO
                   MOVE SPACE     TO CIM-D-LNAME
                                     CIM-D-FNAME
                                     CIM-D-ADDR
                                     CIM-D-CITY
                                     CIM-D-STATE
                                     CIM-D-ZIPCODE
                   PERFORM VARYING INVOICE-SUB FROM 1 BY 1
                           UNTIL INVOICE-SUB > 10
                       MOVE SPACE TO CIM-D-INVOICE-LINE(INVOICE-SUB)
                   END-PERFORM
                   SET SEND-DATAONLY-ALARM TO TRUE
               WHEN DISPLAY-LOW-VALUES
                       SET SEND-DATAONLY-ALARM TO TRUE
           END-EVALUATE.
           PERFORM 1500-SEND-INQUIRY-MAP.
      *
       1410-START-INVOICE-BROWSE.
      *
      *    EXEC CICS
      *        STARTBR FILE('INVPATH')
      *                RIDFLD(CM-CUSTOMER-NUMBER)
      *                EQUAL
      *                RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 20 TO OPENKICKS-CMD
           MOVE 'INVPATH' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF CM-CUSTOMER-NUMBER
           MOVE LENGTH OF CM-CUSTOMER-NUMBER TO OPENKICKS-LENGTHOF
           MOVE 64 TO OPENKICKS-FLAG
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
               MOVE 'N' TO MORE-INVOICES-SW
           ELSE
               IF RESPONSE-CODE NOT = 0
                   PERFORM 9999-TERMINATE-PROGRAM
               END-IF
           END-IF.
      *
       1420-FORMAT-INVOICE-LINE.
      *
           IF MORE-INVOICES
               PERFORM 1430-READ-NEXT-INVOICE
               MOVE INV-INVOICE-NUMBER TO IL-INVOICE-NUMBER
               MOVE INV-PO-NUMBER      TO IL-PO-NUMBER
               MOVE INV-INVOICE-DATE   TO IL-INVOICE-DATE
               MOVE INV-INVOICE-TOTAL  TO IL-INVOICE-TOTAL
               MOVE INVOICE-LINE      TO CIM-D-INVOICE-LINE(INVOICE-SUB)
           ELSE
               MOVE SPACE             TO CIM-D-INVOICE-LINE(INVOICE-SUB)
           END-IF.
      *
       1430-READ-NEXT-INVOICE.
      *
      *    EXEC CICS
      *        READNEXT FILE('INVPATH')
      *                 RIDFLD(CM-CUSTOMER-NUMBER)
      *                 INTO(INVOICE-RECORD)
      *                 RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 21 TO OPENKICKS-CMD
           MOVE 'INVPATH' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF INVOICE-RECORD
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
           IF RESPONSE-CODE = 0
               MOVE 'N' TO MORE-INVOICES-SW
           ELSE
               IF RESPONSE-CODE NOT = 15
                   PERFORM 9999-TERMINATE-PROGRAM
               END-IF
           END-IF.
      *
       1440-END-INVOICE-BROWSE.
      *
      *    EXEC CICS
      *        ENDBR FILE('INVPATH')
      *              RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 25 TO OPENKICKS-CMD
           MOVE 'INVPATH' TO OPENKICKS-CHAR8A
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
       1500-SEND-INQUIRY-MAP.
      *
           MOVE 'INQ3' TO CIM-D-TRANID.
      *
           EVALUATE TRUE
               WHEN SEND-ERASE
      *            EXEC CICS
      *                SEND MAP('INQMAP3')
      *                     MAPSET('INQSET3')
      *                     FROM(CUSTOMER-INQUIRY-MAP)
      *                     ERASE
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'INQMAP3' TO OPENKICKS-CHAR8A
           MOVE 'INQSET3' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-INQUIRY-MAP
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
      *                SEND MAP('INQMAP3')
      *                     MAPSET('INQSET3')
      *                     FROM(CUSTOMER-INQUIRY-MAP)
      *                     DATAONLY
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'INQMAP3' TO OPENKICKS-CHAR8A
           MOVE 'INQSET3' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-INQUIRY-MAP
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
      *                SEND MAP('INQMAP3')
      *                     MAPSET('INQSET3')
      *                     FROM(CUSTOMER-INQUIRY-MAP)
      *                     DATAONLY
      *                     ALARM
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'INQMAP3' TO OPENKICKS-CHAR8A
           MOVE 'INQSET3' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-INQUIRY-MAP
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
                             CUSTOMER-INQUIRY-MAP.
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
               MOVE SPACE TO CIM-D-MESSAGE
           ELSE
               IF RESPONSE-CODE = 13
                   MOVE 'N' TO CUSTOMER-FOUND-SW
                   MOVE 'There are no customers in the file.'
                       TO CIM-D-MESSAGE
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
           IF RESPONSE-CODE = 0
               MOVE 'Y' TO CUSTOMER-FOUND-SW
           ELSE
               IF RESPONSE-CODE = 20
                   MOVE 'N' TO CUSTOMER-FOUND-SW
                   MOVE 'There are no more records in the file.'
                       TO CIM-D-MESSAGE
               ELSE
                   PERFORM 9999-TERMINATE-PROGRAM
               END-IF
           END-IF.
      *
       2300-END-CUSTOMER-BROWSE.
      *
      *    EXEC CICS
      *        ENDBR FILE('CUSTMAS')
      *        RESP(RESPONSE-CODE)
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
           MOVE LOW-VALUE  TO CUSTOMER-INQUIRY-MAP.
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
           IF RESPONSE-CODE = 0
               MOVE 'Y' TO CUSTOMER-FOUND-SW
           ELSE
               IF RESPONSE-CODE = 20
                   MOVE 'N' TO CUSTOMER-FOUND-SW
                   MOVE 'There are no more records in the file.'
                       TO CIM-D-MESSAGE
               ELSE
                   PERFORM 9999-TERMINATE-PROGRAM
               END-IF
           END-IF.
      *
       4000-DISPLAY-PREV-CUSTOMER.
      *
           MOVE CA-CUSTOMER-NUMBER TO CM-CUSTOMER-NUMBER.
           MOVE LOW-VALUE          TO CUSTOMER-INQUIRY-MAP.
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
           MOVE LOW-VALUE          TO CUSTOMER-INQUIRY-MAP.
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
