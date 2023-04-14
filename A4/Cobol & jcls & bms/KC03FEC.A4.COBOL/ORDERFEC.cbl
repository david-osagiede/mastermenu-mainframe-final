       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  ORDERFEC.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-CONSTANTS.
           05 WS-YES-CNST                     PIC X VALUE 'Y'.
           05 WS-NO-CNST                      PIC X VALUE 'N'.
           05 WS-OPEN-CNST                    PIC X VALUE 'Y'.
           05 WS-CLOSED-CNST                  PIC X VALUE 'N'.
           05 WS-END-OF-SESSION-MESSAGE       PIC X(13)
               VALUE 'Session ended'.
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
           05  CA-INVOICE-RECORD             PIC X(389).
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
       01 WS-CMF-FILE-STATUS-INFO.
           05 WS-CMF-OPEN                     PIC X.
               88 WS-CMF-OPEN-88                    VALUE 'Y'.
               88 WS-CMF-CLOSED-88                  VALUE 'Y'.
           05 WS-CMF-OPEN-STATUS              PIC S9(8) COMP VALUE 0.
           05 WS-CMF-ENABLE-STATUS            PIC S9(8) COMP VALUE 0.
      *
       01 WS-RESPONSE-CODE                   PIC S9(8)  COMP.
       COPY ERRPARMS.
      *
       01 WS-INV-FILE-STATUS-INFO.
           05 WS-INV-OPEN                     PIC X.
               88 WS-INV-OPEN-88                    VALUE 'Y'.
               88 WS-INV-CLOSED-88                  VALUE 'Y'.
           05 WS-INV-OPEN-STATUS              PIC S9(8) COMP VALUE 0.
           05 WS-INV-ENABLE-STATUS            PIC S9(8) COMP VALUE 0.

      *
       01 WS-PRD-FILE-STATUS-INFO.
           05 WS-PRD-OPEN                     PIC X.
               88 WS-PRD-OPEN-88                    VALUE 'Y'.
               88 WS-PRD-CLOSED-88                  VALUE 'Y'.
           05 WS-PRD-OPEN-STATUS              PIC S9(8) COMP VALUE 0.
           05 WS-PRD-ENABLE-STATUS            PIC S9(8) COMP VALUE 0.
      *
       01  TOTAL-LINE.
      *
           05  TL-TOTAL-ORDERS   PIC ZZ9.
           05  FILLER            PIC X(20) VALUE ' Orders entered.  Pr'.
           05  FILLER            PIC X(20) VALUE 'ess Enter to continu'.
           05  FILLER            PIC X(02) VALUE 'e.'.
      *
       COPY INVOICE.
      *
       COPY CUSTMAS.
      *
       COPY PRODUCT.
      * This was provided and should be used with no changes
       COPY ORDSET1.
      * This comes from BMS generate
      * and should have the changes for your user id
       COPY ORDSFEC.
      *
      * When replacing items for your userid in program,
      * only change 'ORDMAP1' to 'ORDMXXX'
      * and  change 'ORDSET1' TO 'ORDSXXX'
      * DO NOT CHANGE THE FOLLOWING:
      *   INTO(ORDMAP1)
      *   FROM(ORDMAP1)
      *   MOVE LOW-VALUE TO ORDMAP1
      *   INSPECT ORDMAP1 REPLACING ALL '_' BY SPACE.
      *
       COPY DFHAID.
      *
       COPY ATTR.
      *
       COPY SUBWS.
      *
       COPY ERRPARM.
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA             PIC X(423).
      *
       PROCEDURE DIVISION.
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
      *
                   SET UNPROTECT-FIELDS TO TRUE
      *
                   SET SEND-ERASE TO TRUE
                   PERFORM 1400-SEND-ORDER-MAP
                   SET PROCESS-ENTRY TO TRUE
      *
               WHEN EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
                   CONTINUE
      *
               WHEN EIBAID = DFHPF3
                   PERFORM 8100-CMF-CLOSE
                   PERFORM 8500-INV-CLOSE
                   PERFORM 8700-PRD-CLOSE
      *
                   SET UNPROTECT-FIELDS TO TRUE
      *
                   PERFORM 3000-SEND-TOTAL-LINE
                   EXEC CICS
                       RETURN TRANSID('UFEC')
                   END-EXEC
      *
               WHEN EIBAID = DFHPF12
                   PERFORM 8100-CMF-CLOSE
                   PERFORM 8500-INV-CLOSE
                   PERFORM 8700-PRD-CLOSE
      *
                   SET UNPROTECT-FIELDS TO TRUE
      *
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
                           EXEC CICS
                               RETURN TRANSID('UFEC')
                           END-EXEC
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
           EXEC CICS
               RETURN TRANSID('OFEC')
                      COMMAREA(COMMUNICATION-AREA)
           END-EXEC.
      *      ******************************************************************
      * PARAGRAPHS FOR INTEDIT
      ******************************************************************
      *
       9500-INTEDIT.
      *
           MOVE ZERO TO INT-INTEGER-LENGTH.

           INSPECT INT-UNEDITED-NUMBER
               REPLACING LEADING SPACE BY ZERO.

           INSPECT INT-UNEDITED-NUMBER
               TALLYING INT-INTEGER-LENGTH FOR CHARACTERS
                   BEFORE INITIAL SPACE.

           IF INT-UNEDITED-NUMBER(1:INT-INTEGER-LENGTH) NUMERIC
               MOVE INT-UNEDITED-NUMBER(1:INT-INTEGER-LENGTH)
                 TO INT-EDITED-NUMBER
               MOVE 'Y'
                 TO INT-VALID-NUMBER-SW
           ELSE
               MOVE 'N'
                 TO INT-VALID-NUMBER-SW
           END-IF.

      ******************************************************************
      * PARAGRAPHS FOR NUMEDIT
      ******************************************************************
      *
       9700-NUMEDIT.
      *
           MOVE 'Y'  TO NUM-VALID-NUMBER-SW.
           MOVE ZERO TO NUM-INTEGER-PART
                        NUM-DECIMAL-PART
                        NUM-DECIMAL-POS.

           INSPECT NUM-UNEDITED-NUMBER
               TALLYING NUM-DECIMAL-POS FOR CHARACTERS
                   BEFORE INITIAL '.'.

           IF NUM-DECIMAL-POS < 10
               PERFORM 9710-EDIT-DECIMAL-NUMBER
           ELSE
               PERFORM 9720-EDIT-INTEGER
           END-IF.

           IF NUM-VALID-NUMBER
               COMPUTE NUM-EDITED-NUMBER =
                       NUM-INTEGER-PART + NUM-DECIMAL-PART
           END-IF.
      *
       9710-EDIT-DECIMAL-NUMBER.
      *
           MOVE 10 TO NUM-INTEGER-SUB.

           PERFORM 9712-EDIT-INTEGER-PART
               VARYING NUM-UNEDIT-SUB FROM NUM-DECIMAL-POS BY -1
                 UNTIL NUM-UNEDIT-SUB < 1.

           MOVE 1 TO NUM-DECIMAL-SUB.
           ADD  2 TO NUM-DECIMAL-POS.

           PERFORM 9714-EDIT-DECIMAL-PART
               VARYING NUM-UNEDIT-SUB FROM NUM-DECIMAL-POS BY 1
                 UNTIL NUM-UNEDIT-SUB > 10.
      *
       9712-EDIT-INTEGER-PART.
      *
           IF NUM-UNEDITED-CHAR(NUM-UNEDIT-SUB) NUMERIC
               MOVE NUM-UNEDITED-CHAR(NUM-UNEDIT-SUB)
                 TO NUM-INTEGER-CHAR (NUM-INTEGER-SUB)
               SUBTRACT 1        FROM NUM-INTEGER-SUB
           ELSE IF NUM-UNEDITED-CHAR (NUM-UNEDIT-SUB) NOT = SPACE
               MOVE 'N' TO NUM-VALID-NUMBER-SW
           END-IF.
      *
       9714-EDIT-DECIMAL-PART.
      *
           IF NUM-UNEDITED-CHAR(NUM-UNEDIT-SUB) NUMERIC
               MOVE NUM-UNEDITED-CHAR(NUM-UNEDIT-SUB)
                 TO NUM-DECIMAL-CHAR (NUM-DECIMAL-SUB)
               ADD 1               TO NUM-DECIMAL-SUB
           ELSE IF NUM-UNEDITED-CHAR (NUM-UNEDIT-SUB) NOT = SPACE
               MOVE 'N' TO NUM-VALID-NUMBER-SW
           END-IF.
      *
       9720-EDIT-INTEGER.
      *
           INSPECT NUM-UNEDITED-NUMBER
               REPLACING LEADING SPACE BY ZERO.

           MOVE ZERO TO NUM-INTEGER-LENGTH.

           INSPECT NUM-UNEDITED-NUMBER
               TALLYING NUM-INTEGER-LENGTH FOR CHARACTERS
                   BEFORE INITIAL SPACE.

           MOVE 10 TO NUM-INTEGER-SUB.

           PERFORM 9712-EDIT-INTEGER-PART
               VARYING NUM-UNEDIT-SUB FROM NUM-INTEGER-LENGTH BY -1
                 UNTIL NUM-UNEDIT-SUB < 1.

           MOVE ZERO TO NUM-DECIMAL-PART.

      ******************************************************************
      * PARAGRAPHS FOR SYSERR
      ******************************************************************
      * Note: You can delete this paragraph if it is causing a problem,
      *       or is not used anywhere in the ORDER program
      ******************************************************************
      *
       9900-SYSERR.
      *
           MOVE DFHCOMMAREA TO ERROR-PARAMETERS.
           MOVE ERR-RESP  TO EM-RESP.
           MOVE ERR-RESP2 TO EM-RESP2.
           MOVE ERR-TRNID TO EM-TRNID.
           MOVE ERR-RSRCE TO EM-RSRCE.
           EXEC CICS
               SEND TEXT FROM(ERROR-MESSAGE)
                         ERASE
                         ALARM
                         FREEKB
           END-EXEC.
           EXEC CICS
               RETURN
           END-EXEC.
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
           EXEC CICS
               RECEIVE MAP('ORDMFEC')
                       MAPSET('ORDSFEC')
                       INTO(ORDMAP1)
           END-EXEC.
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
           MOVE 'Y' TO VALID-DATA-SW.
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
           PERFORM 8000-CMF-OPEN.
      *
           EXEC CICS
               READ FILE('CMFFEC')
                    INTO(CUSTOMER-MASTER-RECORD)
                    RIDFLD(ORD-D-CUSTNO)
                    RESP(RESPONSE-CODE)
           END-EXEC.
      *
           IF RESPONSE-CODE = DFHRESP(NORMAL)
               MOVE 'Y' TO CUSTOMER-FOUND-SW
           ELSE
               IF RESPONSE-CODE = DFHRESP(NOTFND)
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
                   MOVE ORD-D-NET-ALPHA(ITEM-SUB) TO NUM-UNEDITED-NUMBER
                   PERFORM 9700-NUMEDIT
                   MOVE NUM-EDITED-NUMBER TO EDITED-NUMBER
                        NET-NUMERIC
                   MOVE NUM-VALID-NUMBER-SW TO VALID-NUMBER-SW
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
              MOVE ORD-D-QTY-ALPHA(ITEM-SUB) TO INT-UNEDITED-NUMBER
              PERFORM 9500-INTEDIT
              MOVE INT-EDITED-NUMBER TO EDITED-NUMBER
                      QTY-NUMERIC
              MOVE INT-VALID-NUMBER-SW TO VALID-NUMBER-SW
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
                   MOVE PRD-PRODUCT-DESCRIPTION
                                       TO ORD-D-DESC(ITEM-SUB)
                   MOVE PRD-UNIT-PRICE TO ORD-D-LIST(ITEM-SUB)
                   IF NOT CA-NET-ENTERED(ITEM-SUB)
                       MOVE PRD-UNIT-PRICE TO ORD-D-NET(ITEM-SUB)
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
           PERFORM 8600-PRD-OPEN.
           EXEC CICS
               READ FILE('PRDFEC')
                    INTO(PRODUCT-MASTER-RECORD)
                    RIDFLD(ORD-D-PCODE(ITEM-SUB))
                    RESP(RESPONSE-CODE)
           END-EXEC.
      *
           IF RESPONSE-CODE = DFHRESP(NORMAL)
               MOVE 'Y' TO PRODUCT-FOUND-SW
           ELSE
               IF RESPONSE-CODE = DFHRESP(NOTFND)
                   MOVE 'N' TO PRODUCT-FOUND-SW
               ELSE
                   PERFORM 9999-TERMINATE-PROGRAM
               END-IF
           END-IF.
      *
       1300-FORMAT-INVOICE-RECORD.
      *
           EXEC CICS
               ASKTIME ABSTIME(ABSOLUTE-TIME)
           END-EXEC.
      *
           EXEC CICS
               FORMATTIME ABSTIME(ABSOLUTE-TIME)
               MMDDYYYY(INV-INVOICE-DATE)
           END-EXEC.
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
           MOVE 'OFEC' TO ORD-D-TRANID.
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
                   EXEC CICS
                       SEND MAP('ORDMFEC')
                            MAPSET('ORDSFEC')
                            FROM(ORDMAP1)
                            CURSOR
                            ERASE
                   END-EXEC
               WHEN SEND-DATAONLY
                   EXEC CICS
                       SEND MAP('ORDMFEC')
                            MAPSET('ORDSFEC')
                            FROM(ORDMAP1)
                            CURSOR
                            DATAONLY
                   END-EXEC
               WHEN SEND-DATAONLY-ALARM
                   EXEC CICS
                       SEND MAP('ORDMFEC')
                            MAPSET('ORDSFEC')
                            FROM(ORDMAP1)
                            CURSOR
                            DATAONLY
                            ALARM
                   END-EXEC
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
           EXEC CICS
               LINK PROGRAM('GETINFEC')
                    COMMAREA(INV-INVOICE-NUMBER)
           END-EXEC.
      *
           PERFORM 2100-WRITE-INVOICE-RECORD.
           ADD 1 TO CA-TOTAL-ORDERS.
      *
           MOVE LOW-VALUE TO ORDMAP1.
           MOVE LOW-VALUE TO CA-INVOICE-RECORD
                             CA-FIELDS-ENTERED.
      *
           MOVE 'Type order details.  Then press Enter.'
               TO ORD-D-INSTR.
           MOVE 'Order posted.' TO ORD-D-MESSAGE.
           MOVE 'F3=Exit   F12=Cancel' TO ORD-D-FKEY.
           MOVE -1 TO ORD-L-CUSTNO.
      *
           SET UNPROTECT-FIELDS TO TRUE.
      *
           SET SEND-ERASE TO TRUE.
           PERFORM 1400-SEND-ORDER-MAP.
      *
       2100-WRITE-INVOICE-RECORD.
      *
           PERFORM 8400-INV-OPEN.
           EXEC CICS
               WRITE FILE('INVFEC')
                     FROM(INVOICE-RECORD)
                     RIDFLD(INV-INVOICE-NUMBER)
           END-EXEC.

      *
       3000-SEND-TOTAL-LINE.
      *
           MOVE CA-TOTAL-ORDERS TO TL-TOTAL-ORDERS.
      *
           EXEC CICS
               SEND TEXT FROM(TOTAL-LINE)
                         ERASE
                         FREEKB
           END-EXEC.
      *
       8000-CMF-OPEN.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE'                     TO WS-HA-EXEC-TEXT-T4.
           MOVE 'FILE=CMFFEC'                 TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-TRANSIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE FILE('CMFFEC')
                   OPENSTATUS(WS-CMF-OPEN-STATUS)
                   ENABLESTATUS(WS-CMF-ENABLE-STATUS)
                   RESP        (WS-RESPONSE-CODE)
                   RESP2       (WS-RESPONSE-CODE)
           END-EXEC.
      *
           IF WS-CMF-OPEN-STATUS = DFHVALUE(CLOSED)
               MOVE 'SET FILE OPEN'           TO WS-HA-EXEC-TEXT-T4
               MOVE 'FILE=CMFFEC'             TO WS-HA-EXEC-TEXT-T5
               EXEC CICS
                   SET FILE('CMFFEC') OPEN
               END-EXEC
               MOVE 'Y' TO WS-CMF-OPEN
           ELSE IF WS-CMF-OPEN-STATUS = DFHVALUE(OPEN)
               NEXT SENTENCE
           ELSE
               PERFORM 9000-HANDLE-ABEND
           END-IF.
      *
      *
       8100-CMF-CLOSE.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE'                     TO WS-HA-EXEC-TEXT-T4.
           MOVE 'FILE=CMFFEC'                 TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-FILENOTFOUND)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE FILE    ('CMFFEC')
                   OPENSTATUS  (WS-CMF-OPEN-STATUS)
                   ENABLESTATUS(WS-CMF-ENABLE-STATUS)
                   RESP        (WS-RESPONSE-CODE)
                   RESP2       (WS-RESPONSE-CODE)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               IF WS-CMF-OPEN-STATUS = DFHVALUE(OPEN)
                   MOVE 'SET FILE CLOSED'     TO WS-HA-EXEC-TEXT-T4
                   MOVE 'FILE=CMFFEC'         TO WS-HA-EXEC-TEXT-T5
                   EXEC CICS
                       SET FILE ('CMFFEC') CLOSED
                   END-EXEC
                   MOVE WS-CLOSED-CNST        TO WS-CMF-OPEN
               END-IF
           ELSE
               PERFORM 9000-HANDLE-ABEND
           END-IF.
      *
       8300-SEND-TERMINATION-MSG.
      *
           EXEC CICS
               SEND TEXT FROM(WS-END-OF-SESSION-MESSAGE)
                    ERASE
                    FREEKB
           END-EXEC.
      *
           EXEC CICS
               RETURN
           END-EXEC.
      *
       8400-INV-OPEN.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE'                     TO WS-HA-EXEC-TEXT-T4.
           MOVE 'FILE=INVFEC'                 TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-TRANSIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE FILE('INVFEC')
                   OPENSTATUS(WS-INV-OPEN-STATUS)
                   ENABLESTATUS(WS-INV-ENABLE-STATUS)
                   RESP        (WS-RESPONSE-CODE)
                   RESP2       (WS-RESPONSE-CODE)
           END-EXEC.
      *
           IF WS-INV-OPEN-STATUS = DFHVALUE(CLOSED)
               MOVE 'SET FILE OPEN'           TO WS-HA-EXEC-TEXT-T4
               MOVE 'FILE=INVFEC'             TO WS-HA-EXEC-TEXT-T5
               EXEC CICS
                   SET FILE('INVFEC') OPEN
               END-EXEC
               MOVE 'Y' TO WS-INV-OPEN
           ELSE IF WS-INV-OPEN-STATUS = DFHVALUE(OPEN)
               NEXT SENTENCE
           ELSE
               PERFORM 9000-HANDLE-ABEND
           END-IF.
      *
      *
       8500-INV-CLOSE.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE'                     TO WS-HA-EXEC-TEXT-T4.
           MOVE 'FILE=INVFEC'                 TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-FILENOTFOUND)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE FILE    ('INVFEC')
                   OPENSTATUS  (WS-INV-OPEN-STATUS)
                   ENABLESTATUS(WS-INV-ENABLE-STATUS)
                   RESP        (WS-RESPONSE-CODE)
                   RESP2       (WS-RESPONSE-CODE)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               IF WS-INV-OPEN-STATUS = DFHVALUE(OPEN)
                   MOVE 'SET FILE CLOSED'     TO WS-HA-EXEC-TEXT-T4
                   MOVE 'FILE=INVFEC'         TO WS-HA-EXEC-TEXT-T5
                   EXEC CICS
                       SET FILE ('INVFEC') CLOSED
                   END-EXEC
                   MOVE WS-CLOSED-CNST        TO WS-INV-OPEN
               END-IF
           ELSE
               PERFORM 9000-HANDLE-ABEND
           END-IF.
       8600-PRD-OPEN.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE'                     TO WS-HA-EXEC-TEXT-T4.
           MOVE 'FILE=PRDFEC'                 TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-TRANSIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE FILE('PRDFEC')
                   OPENSTATUS(WS-PRD-OPEN-STATUS)
                   ENABLESTATUS(WS-PRD-ENABLE-STATUS)
                   RESP        (WS-RESPONSE-CODE)
                   RESP2       (WS-RESPONSE-CODE)
           END-EXEC.
      *
           IF WS-PRD-OPEN-STATUS = DFHVALUE(CLOSED)
               MOVE 'SET FILE OPEN'           TO WS-HA-EXEC-TEXT-T4
               MOVE 'FILE=PRDFEC'             TO WS-HA-EXEC-TEXT-T5
               EXEC CICS
                   SET FILE('PRDFEC') OPEN
               END-EXEC
               MOVE 'Y' TO WS-PRD-OPEN
           ELSE IF WS-PRD-OPEN-STATUS = DFHVALUE(OPEN)
               NEXT SENTENCE
           ELSE
               PERFORM 9000-HANDLE-ABEND
           END-IF.
      *
      *
       8700-PRD-CLOSE.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE'                     TO WS-HA-EXEC-TEXT-T4.
           MOVE 'FILE=PRDFEC'                 TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-FILENOTFOUND)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE FILE    ('PRDFEC')
                   OPENSTATUS  (WS-PRD-OPEN-STATUS)
                   ENABLESTATUS(WS-PRD-ENABLE-STATUS)
                   RESP        (WS-RESPONSE-CODE)
                   RESP2       (WS-RESPONSE-CODE)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               IF WS-PRD-OPEN-STATUS = DFHVALUE(OPEN)
                   MOVE 'SET FILE CLOSED'     TO WS-HA-EXEC-TEXT-T4
                   MOVE 'FILE=PRDFEC'         TO WS-HA-EXEC-TEXT-T5
                   EXEC CICS
                       SET FILE ('PRDFEC') CLOSED
                   END-EXEC
                   MOVE WS-CLOSED-CNST        TO WS-PRD-OPEN
               END-IF
           ELSE
               PERFORM 9000-HANDLE-ABEND
           END-IF.
      *
       9000-HANDLE-ABEND.
      *
           MOVE WS-HA-EXEC-TEXT TO WS-HA-HANDLE-ABEND-MSG.
      *
           EXEC CICS
                SEND TEXT FROM(WS-HA-HANDLE-ABEND-MSG)
                    FREEKB
                    ERASE
           END-EXEC.
      *
           EXEC CICS
               RETURN
           END-EXEC.
       9999-TERMINATE-PROGRAM.
      *
           MOVE EIBRESP  TO ERR-RESP.
           MOVE EIBRESP2 TO ERR-RESP2.
           MOVE EIBTRNID TO ERR-TRNID.
           MOVE EIBRSRCE TO ERR-RSRCE.
      *
           EXEC CICS
               XCTL PROGRAM('SYSERR')
                    COMMAREA(ERROR-PARAMETERS)
           END-EXEC.
