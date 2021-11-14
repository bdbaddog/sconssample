       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  ORDRENT.
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
           05  VALID-DATA-SW                   PIC X(01) VALUE 'Y'.
               88  VALID-DATA                            VALUE 'Y'.
           05  CUSTOMER-FOUND-SW               PIC X(01) VALUE 'Y'.
               88  CUSTOMER-FOUND                        VALUE 'Y'.
           05  PRODUCT-FOUND-SW                PIC X(01) VALUE 'Y'.
               88  PRODUCT-FOUND                         VALUE 'Y'.
           05  VALID-QUANTITY-SW               PIC X(01) VALUE 'Y'.
               88  VALID-QUANTITY                        VALUE 'Y'.
           05  VALID-NET-SW                    PIC X(01) VALUE 'Y'.
               88  VALID-NET                             VALUE 'Y'.
      *
       01  FLAGS.
      *
           05  SEND-FLAG                       PIC X(01).
               88  SEND-ERASE                          VALUE '1'.
               88  SEND-DATAONLY                       VALUE '2'.
               88  SEND-DATAONLY-ALARM                 VALUE '3'.
           05  FIELD-PROTECTION-FLAG           PIC X(01).
               88  PROTECT-FIELDS                      VALUE '1'.
               88  UNPROTECT-FIELDS                    VALUE '2'.
      *
       01  WORK-FIELDS.
      *
           05  ITEM-SUB            PIC S9(03)  COMP-3  VALUE ZERO.
           05  LINE-ITEM-COUNT     PIC S9(03)  COMP-3  VALUE ZERO.
           05  NET-NUMERIC         PIC 9(07)V99.
           05  QTY-NUMERIC         PIC 9(05).
           05  ABSOLUTE-TIME       PIC S9(15)  COMP-3.
           05  TODAYS-DATE         PIC X(10).
      *
       01  RESPONSE-CODE                     PIC S9(08)  COMP.
      *
       01  COMMUNICATION-AREA.
      *
           05  CA-CONTEXT-FLAG               PIC X(01).
               88  PROCESS-ENTRY                       VALUE '1'.
               88  PROCESS-VERIFY                      VALUE '2'.
           05  CA-TOTAL-ORDERS               PIC S9(03) COMP-3.
           05  CA-INVOICE-RECORD             PIC X(318).
           05  CA-FIELDS-ENTERED.
               10  CA-PO-ENTERED-SW          PIC X(01).
                   88  CA-PO-ENTERED                VALUE 'Y'.
               10  CA-LINE-ITEM              OCCURS 10.
                   15  CA-PCODE-ENTERED-SW   PIC X(01).
                       88  CA-PCODE-ENTERED         VALUE 'Y'.
                   15  CA-QTY-ENTERED-SW     PIC X(01).
                       88  CA-QTY-ENTERED           VALUE 'Y'.
                   15  CA-NET-ENTERED-SW     PIC X(01).
                       88  CA-NET-ENTERED           VALUE 'Y'.
      *
       01  TOTAL-LINE.
      *
           05  TL-TOTAL-ORDERS   PIC ZZ9.
           05  FILLER            PIC X(20) VALUE ' Orders entered.  Pr'.
           05  FILLER            PIC X(20) VALUE 'ess Enter to continu'.
           05  FILLER            PIC X(02) VALUE 'e.'.
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
       01  PRODUCT-MASTER-RECORD.
      *
           05  PRM-PRODUCT-CODE                PIC X(10).
           05  PRM-PRODUCT-DESCRIPTION         PIC X(20).
           05  PRM-UNIT-PRICE                  PIC S9(07)V99  COMP-3.
           05  PRM-QUANTITY-ON-HAND            PIC S9(07)     COMP-3.
      *
      *
       01  INVCTL-RECORD.
      *
           05  INVCTL-RECORD-KEY               PIC X(01).
           05  INVCTL-NEXT-INVOICE-NUMBER      PIC 9(06).
      *
      *
       01  ORDMAP1.
      *
           05  FILLLER                 PIC X(12).
      *
           05  ORD-L-TRANID            PIC S9(04)  COMP.
           05  ORD-A-TRANID            PIC X(01).
           05  ORD-C-TRANID            PIC X(01).
           05  ORD-H-TRANID            PIC X(01).
           05  ORD-D-TRANID            PIC X(04).
      *
           05  ORD-L-INSTR             PIC S9(04)  COMP.
           05  ORD-A-INSTR             PIC X(01).
           05  ORD-C-INSTR             PIC X(01).
           05  ORD-H-INSTR             PIC X(01).
           05  ORD-D-INSTR             PIC X(79).
      *
           05  ORD-L-CUSTNO            PIC S9(04)  COMP.
           05  ORD-A-CUSTNO            PIC X(01).
           05  ORD-C-CUSTNO            PIC X(01).
           05  ORD-H-CUSTNO            PIC X(01).
           05  ORD-D-CUSTNO            PIC X(06).
      *
           05  ORD-L-LNAME             PIC S9(04)  COMP.
           05  ORD-A-LNAME             PIC X(01).
           05  ORD-C-LNAME             PIC X(01).
           05  ORD-H-LNAME             PIC X(01).
           05  ORD-D-LNAME             PIC X(30).
      *
           05  ORD-L-PO                PIC S9(04)  COMP.
           05  ORD-A-PO                PIC X(01).
           05  ORD-C-PO                PIC X(01).
           05  ORD-H-PO                PIC X(01).
           05  ORD-D-PO                PIC X(10).
      *
           05  ORD-L-FNAME             PIC S9(04)  COMP.
           05  ORD-A-FNAME             PIC X(01).
           05  ORD-C-FNAME             PIC X(01).
           05  ORD-H-FNAME             PIC X(01).
           05  ORD-D-FNAME             PIC X(20).
      *
           05  ORD-L-ADDR              PIC S9(04)  COMP.
           05  ORD-A-ADDR              PIC X(01).
           05  ORD-C-ADDR              PIC X(01).
           05  ORD-H-ADDR              PIC X(01).
           05  ORD-D-ADDR              PIC X(30).
      *
           05  ORD-L-CITY              PIC S9(04)  COMP.
           05  ORD-A-CITY              PIC X(01).
           05  ORD-C-CITY              PIC X(01).
           05  ORD-H-CITY              PIC X(01).
           05  ORD-D-CITY              PIC X(20).
      *
           05  ORD-L-STATE             PIC S9(04)  COMP.
           05  ORD-A-STATE             PIC X(01).
           05  ORD-C-STATE             PIC X(01).
           05  ORD-H-STATE             PIC X(01).
           05  ORD-D-STATE             PIC X(02).
      *
           05  ORD-L-ZIPCODE           PIC S9(04)  COMP.
           05  ORD-A-ZIPCODE           PIC X(01).
           05  ORD-C-ZIPCODE           PIC X(01).
           05  ORD-H-ZIPCODE           PIC X(01).
           05  ORD-D-ZIPCODE           PIC X(10).
      *
           05  ORD-LINE-ITEM           OCCURS 10 TIMES.
      *
               10  ORD-L-PCODE         PIC S9(04)  COMP.
               10  ORD-A-PCODE         PIC X(01).
               10  ORD-C-PCODE         PIC X(01).
               10  ORD-H-PCODE         PIC X(01).
               10  ORD-D-PCODE         PIC X(10).
      *
               10  ORD-L-QTY           PIC S9(04)  COMP.
               10  ORD-A-QTY           PIC X(01).
               10  ORD-C-QTY           PIC X(01).
               10  ORD-H-QTY           PIC X(01).
               10  ORD-D-QTY           PIC ZZZZ9
                                       BLANK WHEN ZERO.
               10  ORD-D-QTY-ALPHA     REDEFINES ORD-D-QTY
                                       PIC X(05).
      *
               10  ORD-L-DESC          PIC S9(04)  COMP.
               10  ORD-A-DESC          PIC X(01).
               10  ORD-C-DESC          PIC X(01).
               10  ORD-H-DESC          PIC X(01).
               10  ORD-D-DESC          PIC X(20).
      *
               10  ORD-L-LIST          PIC S9(04)  COMP.
               10  ORD-A-LIST          PIC X(01).
               10  ORD-C-LIST          PIC X(01).
               10  ORD-H-LIST          PIC X(01).
               10  ORD-D-LIST          PIC Z,ZZZ,ZZ9.99
                                       BLANK WHEN ZERO.
      *
               10  ORD-L-NET           PIC S9(04)  COMP.
               10  ORD-A-NET           PIC X(01).
               10  ORD-C-NET           PIC X(01).
               10  ORD-H-NET           PIC X(01).
               10  ORD-D-NET           PIC ZZZZZZ9.99
                                       BLANK WHEN ZERO.
               10  ORD-D-NET-ALPHA     REDEFINES ORD-D-NET
                                       PIC X(10).
      *
               10  ORD-L-AMOUNT        PIC S9(04)  COMP.
               10  ORD-A-AMOUNT        PIC X(01).
               10  ORD-C-AMOUNT        PIC X(01).
               10  ORD-H-AMOUNT        PIC X(01).
               10  ORD-D-AMOUNT        PIC Z,ZZZ,ZZ9.99
                                       BLANK WHEN ZERO.
      *
           05  ORD-L-TOTAL             PIC S9(04)  COMP.
           05  ORD-A-TOTAL             PIC X(01).
           05  ORD-C-TOTAL             PIC X(01).
           05  ORD-H-TOTAL             PIC X(01).
           05  ORD-D-TOTAL             PIC Z,ZZZ,ZZ9.99
                                       BLANK WHEN ZERO.
      *
           05  ORD-L-MESSAGE           PIC S9(04)  COMP.
           05  ORD-A-MESSAGE           PIC X(01).
           05  ORD-C-MESSAGE           PIC X(01).
           05  ORD-H-MESSAGE           PIC X(01).
           05  ORD-D-MESSAGE           PIC X(79).
      *
           05  ORD-L-FKEY              PIC S9(04)  COMP.
           05  ORD-A-FKEY              PIC X(01).
           05  ORD-C-FKEY              PIC X(01).
           05  ORD-H-FKEY              PIC X(01).
           05  ORD-D-FKEY              PIC X(40).
      *
           05  ORD-L-DUMMY             PIC S9(04)  COMP.
           05  ORD-A-DUMMY             PIC X(01).
           05  ORD-C-DUMMY             PIC X(01).
           05  ORD-H-DUMMY             PIC X(01).
           05  ORD-D-DUMMY             PIC X(01).
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
       01  DFHCOMMAREA             PIC X(352).
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
       0000-ENTER-ORDERS.
      *
           IF EIBCALEN > ZERO
               MOVE DFHCOMMAREA TO COMMUNICATION-AREA
           END-IF.
      *
           EVALUATE TRUE
      *
               WHEN EIBCALEN = ZERO
                   MOVE LOW-VALUE TO ORDMAP1
                   MOVE LOW-VALUE TO COMMUNICATION-AREA
                   MOVE ZERO      TO CA-TOTAL-ORDERS
                   MOVE 'Type order details.  Then press Enter.'
                       TO ORD-D-INSTR
                   MOVE 'F3=Exit   F12=Cancel' TO ORD-D-FKEY
                   MOVE -1 TO ORD-L-CUSTNO
                   SET SEND-ERASE TO TRUE
                   PERFORM 1400-SEND-ORDER-MAP
                   SET PROCESS-ENTRY TO TRUE
      *
               WHEN EIBAID = DFHCLEAR
                   MOVE LOW-VALUE TO ORDMAP1
                   MOVE LOW-VALUE TO CA-INVOICE-RECORD
                                     CA-FIELDS-ENTERED
                   MOVE 'Type order details.  Then press Enter.'
                       TO ORD-D-INSTR
                   MOVE 'F3=Exit   F12=Cancel' TO ORD-D-FKEY
                   MOVE -1 TO ORD-L-CUSTNO
                   SET SEND-ERASE TO TRUE
                   PERFORM 1400-SEND-ORDER-MAP
                   SET PROCESS-ENTRY TO TRUE
      *
               WHEN EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
                   CONTINUE
      *
               WHEN EIBAID = DFHPF3
                   PERFORM 3000-SEND-TOTAL-LINE
      *            EXEC CICS
      *                RETURN TRANSID('MENU')
      *            END-EXEC
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
      *
               WHEN EIBAID = DFHPF12
                   IF PROCESS-VERIFY
                       MOVE LOW-VALUE TO ORDMAP1
                       MOVE LOW-VALUE TO CA-INVOICE-RECORD
                                         CA-FIELDS-ENTERED
                       MOVE 'Type order details.  Then press Enter.'
                           TO ORD-D-INSTR
                       MOVE 'F3=Exit   F12=Cancel' TO ORD-D-FKEY
                       MOVE -1 TO ORD-L-CUSTNO
                       SET SEND-ERASE TO TRUE
                       PERFORM 1400-SEND-ORDER-MAP
                       SET PROCESS-ENTRY TO TRUE
                   ELSE
                       IF PROCESS-ENTRY
                           PERFORM 3000-SEND-TOTAL-LINE
      *                    EXEC CICS
      *                        RETURN TRANSID('MENU')
      *                    END-EXEC
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
                       END-IF
                   END-IF
      *
               WHEN EIBAID = DFHENTER
                   IF PROCESS-ENTRY
                       PERFORM 1000-PROCESS-ORDER-MAP
                   ELSE
                       IF PROCESS-VERIFY
                           PERFORM 2000-PROCESS-POST-ORDER
                           SET PROCESS-ENTRY TO TRUE
                       END-IF
                   END-IF
      *
               WHEN EIBAID = DFHPF4
                   IF PROCESS-VERIFY
                       MOVE LOW-VALUE TO ORDMAP1
                       MOVE 'Type corrections.  Then press Enter.'
                           TO ORD-D-INSTR
                       MOVE 'F3=Exit   F12=Cancel' TO ORD-D-FKEY
                       MOVE -1 TO ORD-L-CUSTNO
                       SET UNPROTECT-FIELDS TO TRUE
                       SET SEND-DATAONLY TO TRUE
                       PERFORM 1400-SEND-ORDER-MAP
                       SET PROCESS-ENTRY TO TRUE
                   ELSE
                       IF PROCESS-ENTRY
                           MOVE LOW-VALUE TO ORDMAP1
                           MOVE 'Invalid key pressed.' TO ORD-D-MESSAGE
                           MOVE -1 TO ORD-L-CUSTNO
                           SET SEND-DATAONLY-ALARM TO TRUE
                           PERFORM 1400-SEND-ORDER-MAP
                       END-IF
                   END-IF
      *
               WHEN OTHER
                   MOVE LOW-VALUE TO ORDMAP1
                   MOVE 'Invalid key pressed.' TO ORD-D-MESSAGE
                   MOVE -1 TO ORD-L-CUSTNO
                   SET SEND-DATAONLY-ALARM TO TRUE
                   PERFORM 1400-SEND-ORDER-MAP
      *
           END-EVALUATE.
      *
      * added by sam
           DISPLAY "RETURN WITH COMMAREA: ", CA-TOTAL-ORDERS
           DISPLAY "LENGTH OF COMMAREA IS: ", 
                    LENGTH OF COMMUNICATION-AREA
      *    EXEC CICS
      *        RETURN TRANSID('ORD1')
      *               COMMAREA(COMMUNICATION-AREA)
      *    END-EXEC.
           MOVE 11 TO OPENKICKS-CMD
           MOVE 'ORD1' TO OPENKICKS-CHAR4A
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
       1000-PROCESS-ORDER-MAP.
      *
           PERFORM 1100-RECEIVE-ORDER-MAP.
           PERFORM 1200-EDIT-ORDER-DATA.
      *
           IF VALID-DATA
               PERFORM 1300-FORMAT-INVOICE-RECORD
               MOVE 'Press Enter to post this order.  Or press F4 to ent
      -             'er corrections.' TO ORD-D-INSTR
               MOVE 'F3=Exit   F4=Change   F12=Cancel' TO ORD-D-FKEY
               MOVE SPACE TO ORD-D-MESSAGE
               SET SEND-DATAONLY TO TRUE
               SET PROTECT-FIELDS TO TRUE
               PERFORM 1400-SEND-ORDER-MAP
               SET PROCESS-VERIFY TO TRUE
           ELSE
               MOVE 'Type corrections.  Then press Enter.'
                   TO ORD-D-INSTR
               MOVE 'F3=Exit   F12=Cancel' TO ORD-D-FKEY
               SET SEND-DATAONLY-ALARM TO TRUE
               PERFORM 1400-SEND-ORDER-MAP
           END-IF.
      *
       1100-RECEIVE-ORDER-MAP.
      *
      *    EXEC CICS
      *        RECEIVE MAP('ORDMAP1')
      *                MAPSET('ORDSET1')
      *                INTO(ORDMAP1)
      *    END-EXEC.
           MOVE 29 TO OPENKICKS-CMD
           MOVE 'ORDMAP1' TO OPENKICKS-CHAR8A
           MOVE 'ORDSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF ORDMAP1
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
           INSPECT ORDMAP1
                REPLACING ALL '_' BY SPACE.
      *
       1200-EDIT-ORDER-DATA.
      *
           MOVE ATTR-NO-HIGHLIGHT TO ORD-H-CUSTNO
                                     ORD-H-PO.
           MOVE ZERO TO LINE-ITEM-COUNT
                        INV-INVOICE-TOTAL.
      *
           PERFORM 1220-EDIT-LINE-ITEM
               VARYING ITEM-SUB FROM 10 BY -1
                 UNTIL ITEM-SUB < 1.
      *
           MOVE INV-INVOICE-TOTAL TO ORD-D-TOTAL.
           IF        LINE-ITEM-COUNT = ZERO
                 AND VALID-DATA
               MOVE ATTR-REVERSE TO ORD-H-PCODE(1)
               MOVE -1 TO ORD-L-PCODE(1)
               MOVE 'You must enter at least one line item.'
                   TO ORD-D-MESSAGE
               MOVE 'N' TO VALID-DATA-SW
           END-IF.
      *
           IF        ORD-L-PO = ZERO
                  OR ORD-D-PO = SPACE
               MOVE 'N' TO CA-PO-ENTERED-SW
           ELSE
               MOVE 'Y' TO CA-PO-ENTERED-SW
           END-IF.
      *
           IF       ORD-L-CUSTNO = ZERO
                 OR ORD-D-CUSTNO = SPACE
               MOVE ATTR-REVERSE TO ORD-H-CUSTNO
               MOVE -1 TO ORD-L-CUSTNO
               MOVE 'You must enter a customer number.'
                   TO ORD-D-MESSAGE
               MOVE 'N' TO VALID-DATA-SW
           ELSE
               PERFORM 1210-READ-CUSTOMER-RECORD
               IF CUSTOMER-FOUND
                   MOVE CM-LAST-NAME  TO ORD-D-LNAME
                   MOVE CM-FIRST-NAME TO ORD-D-FNAME
                   MOVE CM-ADDRESS    TO ORD-D-ADDR
                   MOVE CM-CITY       TO ORD-D-CITY
                   MOVE CM-STATE      TO ORD-D-STATE
                   MOVE CM-ZIP-CODE   TO ORD-D-ZIPCODE
               ELSE
                   MOVE SPACE TO ORD-D-LNAME
                                 ORD-D-FNAME
                                 ORD-D-ADDR
                                 ORD-D-CITY
                                 ORD-D-STATE
                                 ORD-D-ZIPCODE
                   MOVE ATTR-REVERSE TO ORD-H-CUSTNO
                   MOVE -1 TO ORD-L-CUSTNO
                   MOVE 'That customer does not exist.'
                       TO ORD-D-MESSAGE
                   MOVE 'N' TO VALID-DATA-SW
               END-IF
           END-IF.
      *
           IF VALID-DATA
               MOVE -1 TO ORD-L-CUSTNO
           END-IF.
      *
       1210-READ-CUSTOMER-RECORD.
      *
      *    EXEC CICS
      *        READ FILE('CUSTMAS')
      *             INTO(CUSTOMER-MASTER-RECORD)
      *             RIDFLD(ORD-D-CUSTNO)
      *             RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 18 TO OPENKICKS-CMD
           MOVE 'CUSTMAS' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF CUSTOMER-MASTER-RECORD
           MOVE LENGTH OF CUSTOMER-MASTER-RECORD TO OPENKICKS-LENGTHOF
           SET OPENKICKS-DATA2 TO ADDRESS OF ORD-D-CUSTNO
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
               IF RESPONSE-CODE = 13
                   MOVE 'N' TO CUSTOMER-FOUND-SW
               ELSE
                   PERFORM 9999-TERMINATE-PROGRAM
               END-IF
           END-IF.
      *
       1220-EDIT-LINE-ITEM.
      *
           MOVE ATTR-NO-HIGHLIGHT TO ORD-H-PCODE(ITEM-SUB)
                                     ORD-H-QTY(ITEM-SUB)
                                     ORD-H-NET(ITEM-SUB).
           MOVE 'N' TO PRODUCT-FOUND-SW.
           MOVE 'N' TO VALID-QUANTITY-SW.
      *
           IF        ORD-L-PCODE(ITEM-SUB) > ZERO
                 AND ORD-D-PCODE(ITEM-SUB) NOT = SPACE
               MOVE 'Y' TO CA-PCODE-ENTERED-SW(ITEM-SUB)
           ELSE
               MOVE 'N' TO CA-PCODE-ENTERED-SW(ITEM-SUB)
           END-IF.
      *
           IF        ORD-L-QTY(ITEM-SUB) > ZERO
                 AND ORD-D-QTY-ALPHA(ITEM-SUB) NOT = SPACE
               MOVE 'Y' TO CA-QTY-ENTERED-SW(ITEM-SUB)
           ELSE
               MOVE 'N' TO CA-QTY-ENTERED-SW(ITEM-SUB)
           END-IF.
      *
           IF        ORD-L-NET(ITEM-SUB) > ZERO
                 AND ORD-D-NET-ALPHA(ITEM-SUB) NOT = SPACE
               MOVE 'Y' TO CA-NET-ENTERED-SW(ITEM-SUB)
           ELSE
               MOVE 'N' TO CA-NET-ENTERED-SW(ITEM-SUB)
           END-IF.
      *
           IF            CA-NET-ENTERED(ITEM-SUB)
                 AND NOT CA-PCODE-ENTERED(ITEM-SUB)
               MOVE ATTR-REVERSE TO ORD-H-PCODE(ITEM-SUB)
               MOVE -1 TO ORD-L-PCODE(ITEM-SUB)
               MOVE 'You cannot enter a net price without a product code
      -        '.' TO ORD-D-MESSAGE
               MOVE 'N' TO VALID-DATA-SW
           END-IF.
      *
           IF CA-NET-ENTERED(ITEM-SUB)
      *        CALL "NUMEDIT" USING ORD-D-NET-ALPHA(ITEM-SUB)
           MOVE "NUMEDIT" to CALL-NAME
               CALL CALL-NAME  USING ORD-D-NET-ALPHA(ITEM-SUB)
                                    NET-NUMERIC
                                    VALID-NET-SW
               IF VALID-NET
                   MOVE NET-NUMERIC TO ORD-D-NET(ITEM-SUB)
               ELSE
                   MOVE ATTR-REVERSE TO ORD-H-NET(ITEM-SUB)
                   MOVE -1 TO ORD-L-NET(ITEM-SUB)
                   MOVE 'Net price must be numeric.' TO ORD-D-MESSAGE
                   MOVE 'N' TO VALID-DATA-SW
                   MOVE 'N' TO VALID-QUANTITY-SW
               END-IF
           END-IF.
      *
           IF            CA-QTY-ENTERED(ITEM-SUB)
                 AND NOT CA-PCODE-ENTERED(ITEM-SUB)
               MOVE ATTR-REVERSE TO ORD-H-PCODE(ITEM-SUB)
               MOVE -1 TO ORD-L-PCODE(ITEM-SUB)
               MOVE 'You cannot enter a quantity without a product code.
      -            ' ' TO ORD-D-MESSAGE
               MOVE 'N' TO VALID-DATA-SW
           END-IF.
      *
           IF CA-QTY-ENTERED(ITEM-SUB)
      *        CALL "INTEDIT" USING ORD-D-QTY-ALPHA(ITEM-SUB)
           MOVE "INTEDIT" to CALL-NAME
               CALL CALL-NAME  USING ORD-D-QTY-ALPHA(ITEM-SUB)
                                    QTY-NUMERIC
                                    VALID-QUANTITY-SW
               IF VALID-QUANTITY
                   IF QTY-NUMERIC > ZERO
                       MOVE QTY-NUMERIC TO ORD-D-QTY(ITEM-SUB)
                   ELSE
                       MOVE ATTR-REVERSE TO ORD-H-QTY(ITEM-SUB)
                       MOVE -1 TO ORD-L-QTY(ITEM-SUB)
                       MOVE 'Quantity must be greater than zero.'
                           TO ORD-D-MESSAGE
                       MOVE 'N' TO VALID-DATA-SW
                       MOVE 'N' TO VALID-QUANTITY-SW
                   END-IF
               ELSE
                   MOVE ATTR-REVERSE TO ORD-H-QTY(ITEM-SUB)
                   MOVE -1 TO ORD-L-QTY(ITEM-SUB)
                   MOVE 'Quantity must be numeric.' TO ORD-D-MESSAGE
                   MOVE 'N' TO VALID-DATA-SW
                   MOVE 'N' TO VALID-QUANTITY-SW
               END-IF
           END-IF.
      *
           IF            CA-PCODE-ENTERED(ITEM-SUB)
                 AND NOT CA-QTY-ENTERED(ITEM-SUB)
               MOVE ATTR-REVERSE TO ORD-H-QTY(ITEM-SUB)
               MOVE -1 TO ORD-L-QTY(ITEM-SUB)
               MOVE 'You must enter a quantity.' TO ORD-D-MESSAGE
               MOVE 'N' TO VALID-DATA-SW
           END-IF.
      *
           IF NOT CA-PCODE-ENTERED(ITEM-SUB)
               MOVE SPACE TO ORD-D-DESC(ITEM-SUB)
               MOVE ZERO  TO ORD-D-LIST(ITEM-SUB)
                             ORD-D-AMOUNT(ITEM-SUB)
           ELSE
               ADD 1 TO LINE-ITEM-COUNT
               PERFORM 1230-READ-PRODUCT-RECORD
               IF PRODUCT-FOUND
                   MOVE PRM-PRODUCT-DESCRIPTION
                                       TO ORD-D-DESC(ITEM-SUB)
                   MOVE PRM-UNIT-PRICE TO ORD-D-LIST(ITEM-SUB)
                   IF NOT CA-NET-ENTERED(ITEM-SUB)
                       MOVE PRM-UNIT-PRICE TO ORD-D-NET(ITEM-SUB)
                                              NET-NUMERIC
                   END-IF
                   IF VALID-QUANTITY AND VALID-NET
                       MULTIPLY NET-NUMERIC BY QTY-NUMERIC
                           GIVING ORD-D-AMOUNT(ITEM-SUB)
                                  INV-AMOUNT(ITEM-SUB)
                           ON SIZE ERROR
                               MOVE ATTR-REVERSE TO ORD-H-QTY(ITEM-SUB)
                               MOVE -1 TO ORD-L-QTY(ITEM-SUB)
                               MOVE 'Line item amount is too large.'
                                   TO ORD-D-MESSAGE
                               MOVE 'N' TO VALID-DATA-SW
                               MOVE ZERO TO ORD-D-AMOUNT(ITEM-SUB)
                                            INV-AMOUNT(ITEM-SUB)
                       END-MULTIPLY
                       ADD INV-AMOUNT(ITEM-SUB) TO INV-INVOICE-TOTAL
                           ON SIZE ERROR
                               MOVE ATTR-REVERSE TO ORD-H-QTY(ITEM-SUB)
                               MOVE -1 TO ORD-L-QTY(ITEM-SUB)
                               MOVE 'Invoice total is too large.'
                                   TO ORD-D-MESSAGE
                               MOVE 'N' TO VALID-DATA-SW
                               MOVE ZERO TO INV-INVOICE-TOTAL
                       END-ADD
                   END-IF
               ELSE
                   MOVE SPACE TO ORD-D-DESC(ITEM-SUB)
                   MOVE ZERO  TO ORD-D-LIST(ITEM-SUB)
                                 ORD-D-AMOUNT(ITEM-SUB)
                   MOVE ATTR-REVERSE TO ORD-H-PCODE(ITEM-SUB)
                   MOVE -1    TO ORD-L-PCODE(ITEM-SUB)
                   MOVE 'That product does not exist.'
                              TO ORD-D-MESSAGE
                   MOVE 'N'   TO VALID-DATA-SW
               END-IF
           END-IF.
      *
       1230-READ-PRODUCT-RECORD.
      *
      *    EXEC CICS
      *        READ FILE('PRODUCT')
      *             INTO(PRODUCT-MASTER-RECORD)
      *             RIDFLD(ORD-D-PCODE(ITEM-SUB))
      *             RESP(RESPONSE-CODE)
      *    END-EXEC.
           MOVE 18 TO OPENKICKS-CMD
           MOVE 'PRODUCT' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF PRODUCT-MASTER-RECORD
           MOVE LENGTH OF PRODUCT-MASTER-RECORD TO OPENKICKS-LENGTHOF
           SET OPENKICKS-DATA2 TO ADDRESS OF ORD-D-PCODE(ITEM-SUB)
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
               MOVE 'Y' TO PRODUCT-FOUND-SW
           ELSE
               IF RESPONSE-CODE = 13
                   MOVE 'N' TO PRODUCT-FOUND-SW
               ELSE
                   PERFORM 9999-TERMINATE-PROGRAM
               END-IF
           END-IF.
      *
       1300-FORMAT-INVOICE-RECORD.
      *
      *    EXEC CICS
      *        ASKTIME ABSTIME(ABSOLUTE-TIME)
      *    END-EXEC.
           MOVE 3 TO OPENKICKS-CMD
           SET OPENKICKS-DATA1 TO ADDRESS OF ABSOLUTE-TIME
           MOVE 0 TO OPENKICKS-FLAG
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

      *
      *    EXEC CICS
      *        FORMATTIME ABSTIME(ABSOLUTE-TIME)
      *        MMDDYYYY(INV-INVOICE-DATE)
      *    END-EXEC.
           MOVE 8 TO OPENKICKS-CMD
           SET OPENKICKS-DATA9 TO ADDRESS OF ABSOLUTE-TIME
           SET OPENKICKS-DATA4 TO ADDRESS OF INV-INVOICE-DATE
           MOVE 0 TO OPENKICKS-FLAG
           MOVE 540672 TO OPENKICKS-USED
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
           MOVE ORD-D-CUSTNO TO INV-CUSTOMER-NUMBER.
           MOVE ORD-D-PO     TO INV-PO-NUMBER.
      *
           PERFORM VARYING ITEM-SUB FROM 1 BY 1
                     UNTIL ITEM-SUB > 10
               IF CA-PCODE-ENTERED(ITEM-SUB)
                   MOVE ORD-D-PCODE(ITEM-SUB)
                             TO INV-PRODUCT-CODE(ITEM-SUB)
                   MOVE ORD-D-QTY(ITEM-SUB)
                             TO INV-QUANTITY(ITEM-SUB)
                   MOVE ORD-D-NET(ITEM-SUB)
                             TO INV-UNIT-PRICE(ITEM-SUB)
               ELSE
                   MOVE SPACE TO INV-PRODUCT-CODE(ITEM-SUB)
                   MOVE ZERO  TO INV-QUANTITY(ITEM-SUB)
                                 INV-UNIT-PRICE(ITEM-SUB)
                                 INV-AMOUNT(ITEM-SUB)
               END-IF
           END-PERFORM.
      *
           MOVE INVOICE-RECORD TO CA-INVOICE-RECORD.
      *
       1400-SEND-ORDER-MAP.
      *
           MOVE 'ORD1' TO ORD-D-TRANID.
      *
           IF PROTECT-FIELDS
               PERFORM 1410-PROTECT-FIELDS
           ELSE
               IF UNPROTECT-FIELDS
                   PERFORM 1420-UNPROTECT-FIELDS
               END-IF
           END-IF.
      *
           EVALUATE TRUE
               WHEN SEND-ERASE
      *            EXEC CICS
      *                SEND MAP('ORDMAP1')
      *                     MAPSET('ORDSET1')
      *                     FROM(ORDMAP1)
      *                     CURSOR
      *                     ERASE
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'ORDMAP1' TO OPENKICKS-CHAR8A
           MOVE 'ORDSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF ORDMAP1
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
               WHEN SEND-DATAONLY
      *            EXEC CICS
      *                SEND MAP('ORDMAP1')
      *                     MAPSET('ORDSET1')
      *                     FROM(ORDMAP1)
      *                     CURSOR
      *                     DATAONLY
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'ORDMAP1' TO OPENKICKS-CHAR8A
           MOVE 'ORDSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF ORDMAP1
           MOVE 4194306 TO OPENKICKS-FLAG
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
      *                SEND MAP('ORDMAP1')
      *                     MAPSET('ORDSET1')
      *                     FROM(ORDMAP1)
      *                     CURSOR
      *                     DATAONLY
      *                     ALARM
      *            END-EXEC
           MOVE 24 TO OPENKICKS-CMD
           MOVE 'ORDMAP1' TO OPENKICKS-CHAR8A
           MOVE 'ORDSET1' TO OPENKICKS-CHAR8B
           SET OPENKICKS-DATA1 TO ADDRESS OF ORDMAP1
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
       1410-PROTECT-FIELDS.
      *
           MOVE ATTR-PROT TO ORD-A-CUSTNO.
           IF CA-PO-ENTERED
               MOVE ATTR-PROT TO ORD-A-PO
           ELSE
               MOVE ATTR-PROT-DARK TO ORD-A-PO
           END-IF.
      *
           PERFORM VARYING ITEM-SUB FROM 1 BY 1
                   UNTIL ITEM-SUB > 10
               IF CA-PCODE-ENTERED(ITEM-SUB)
                   MOVE ATTR-PROT TO ORD-A-PCODE(ITEM-SUB)
               ELSE
                   MOVE ATTR-PROT-DARK TO ORD-A-PCODE(ITEM-SUB)
               END-IF
               IF CA-QTY-ENTERED(ITEM-SUB)
                   MOVE ATTR-PROT TO ORD-A-QTY(ITEM-SUB)
               ELSE
                   MOVE ATTR-PROT-DARK TO ORD-A-QTY(ITEM-SUB)
               END-IF
               IF        CA-NET-ENTERED(ITEM-SUB)
                      OR CA-PCODE-ENTERED(ITEM-SUB)
                   MOVE ATTR-PROT TO ORD-A-NET(ITEM-SUB)
               ELSE
                   MOVE ATTR-PROT-DARK TO ORD-A-NET(ITEM-SUB)
               END-IF
           END-PERFORM.
      *
       1420-UNPROTECT-FIELDS.
      *
           MOVE ATTR-UNPROT-MDT TO ORD-A-CUSTNO.
           IF CA-PO-ENTERED
               MOVE ATTR-UNPROT-MDT TO ORD-A-PO
           ELSE
               MOVE ATTR-UNPROT     TO ORD-A-PO
           END-IF.
      *
           MOVE ATTR-TURQUOISE TO ORD-C-CUSTNO
                                  ORD-C-PO.
      *
           PERFORM VARYING ITEM-SUB FROM 1 BY 1
                   UNTIL ITEM-SUB > 10
               IF CA-PCODE-ENTERED(ITEM-SUB)
                   MOVE ATTR-UNPROT-MDT TO ORD-A-PCODE(ITEM-SUB)
               ELSE
                   MOVE ATTR-UNPROT     TO ORD-A-PCODE(ITEM-SUB)
               END-IF
               IF CA-QTY-ENTERED(ITEM-SUB)
                   MOVE ATTR-UNPROT-MDT TO ORD-A-QTY(ITEM-SUB)
               ELSE
                   MOVE ATTR-UNPROT     TO ORD-A-QTY(ITEM-SUB)
               END-IF
               IF CA-NET-ENTERED(ITEM-SUB)
                   MOVE ATTR-UNPROT-MDT TO ORD-A-NET(ITEM-SUB)
               ELSE
                   MOVE ATTR-UNPROT     TO ORD-A-NET(ITEM-SUB)
               END-IF
               MOVE ATTR-TURQUOISE TO ORD-C-PCODE(ITEM-SUB)
                                      ORD-C-QTY(ITEM-SUB)
                                      ORD-C-NET(ITEM-SUB)
           END-PERFORM.
      *
       2000-PROCESS-POST-ORDER.
      *
           MOVE CA-INVOICE-RECORD TO INVOICE-RECORD.
      *
      * added by sam
           DISPLAY 'START TO CALL GETINV'
      *    EXEC CICS
      *        LINK PROGRAM('GETINV')
      *             COMMAREA(INV-INVOICE-NUMBER)
      *    END-EXEC.
           MOVE 9 TO OPENKICKS-CMD
           SET OPENKICKS-DATA1 TO ADDRESS OF INV-INVOICE-NUMBER
           MOVE LENGTH OF INV-INVOICE-NUMBER TO OPENKICKS-LENGTHOF
           MOVE 'GETINV' TO OPENKICKS-CHAR8A
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
           .

      *
      * added by sam
           DISPLAY 'START TO WRITE INVOICE RECORD'
           PERFORM 2100-WRITE-INVOICE-RECORD.
           DISPLAY 'FINISH WRITE INVOICE RECORD'
           ADD 1 TO CA-TOTAL-ORDERS.
           MOVE 'Type order details.  Then press Enter.'
               TO ORD-D-INSTR.
           MOVE 'Order posted.' TO ORD-D-MESSAGE.
           MOVE 'F3=Exit   F12=Cancel' TO ORD-D-FKEY.
           MOVE -1 TO ORD-L-CUSTNO.
           SET SEND-ERASE TO TRUE.
           PERFORM 1400-SEND-ORDER-MAP.
      *
       2100-WRITE-INVOICE-RECORD.
      *
      *    EXEC CICS
      *        WRITE FILE('INVOICE')
      *              FROM(INVOICE-RECORD)
      *              RIDFLD(INV-INVOICE-NUMBER)
      *    END-EXEC.
           MOVE 17 TO OPENKICKS-CMD
           MOVE 'INVOICE' TO OPENKICKS-CHAR8A
           SET OPENKICKS-DATA1 TO ADDRESS OF INVOICE-RECORD
           MOVE LENGTH OF INVOICE-RECORD TO OPENKICKS-LENGTHOF
           SET OPENKICKS-DATA2 TO ADDRESS OF INV-INVOICE-NUMBER
           MOVE LENGTH OF INV-INVOICE-NUMBER TO OPENKICKS-LENGTHOF1
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
           .

      *
       3000-SEND-TOTAL-LINE.
      *
           MOVE CA-TOTAL-ORDERS TO TL-TOTAL-ORDERS.
      *
      *    EXEC CICS
      *        SEND TEXT FROM(TOTAL-LINE)
      *                  ERASE
      *                  FREEKB
      *    END-EXEC.
           MOVE 53 TO OPENKICKS-CMD
           SET OPENKICKS-DATA1 TO ADDRESS OF TOTAL-LINE
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
