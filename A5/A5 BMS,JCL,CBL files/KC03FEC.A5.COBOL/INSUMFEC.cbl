       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. INSUMFEC.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
       01  SWITCHES.
      *
           05  INVOICE-EOF-SW          PIC X(01)    VALUE 'N'.
               88  INVOICE-EOF                      VALUE 'Y'.
           05  FIRST-RECORD-SW         PIC X(01)    VALUE 'Y'.
               88  FIRST-RECORD                     VALUE 'Y'.
      *
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
       01  WORK-FIELDS.
      *
           05  INVOICE-COUNT           PIC S9(05)    COMP-3  VALUE ZERO.
           05  INVOICE-TOTAL           PIC S9(07)V99 COMP-3  VALUE ZERO.
      *
       01  RESPONSE-CODE               PIC S9(08)    COMP.
      *
       COPY SUMSFEC.
      *
       COPY INVOICE.
      *
       COPY ERRPARM.
      *
       PROCEDURE DIVISION.
      *
       0000-PREPARE-INVOICE-SUMMARY.
      *
           PERFORM 8400-INV-OPEN.
           MOVE LOW-VALUE TO SUMMFECO.
           PERFORM 1000-START-INVOICE-BROWSE.
           PERFORM 2000-READ-NEXT-INVOICE
               UNTIL INVOICE-EOF.
           PERFORM 3000-END-INVOICE-BROWSE.
           PERFORM 4000-SEND-SUMMARY-MAP.
      *
           Perform 8500-INV-CLOSE.
           EXEC CICS
               RETURN TRANSID('UFEC')
           END-EXEC.
      *
       1000-START-INVOICE-BROWSE.
      *
           MOVE 0 TO INV-INVOICE-NUMBER
      *
           EXEC CICS
               STARTBR FILE('INVFEC')
                       RIDFLD(INV-INVOICE-NUMBER)
                       RESP(RESPONSE-CODE)
           END-EXEC.
      *
           IF RESPONSE-CODE = DFHRESP(NOTFND)
               MOVE 'Y' TO INVOICE-EOF-SW
           ELSE
               IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
                   PERFORM 9999-TERMINATE-PROGRAM
               END-IF
           END-IF.
      *
       2000-READ-NEXT-INVOICE.
      *

           EXEC CICS
               READNEXT FILE('INVFEC')
                        INTO(INVOICE-RECORD)
                        RIDFLD(INV-INVOICE-NUMBER)
                        RESP(RESPONSE-CODE)
           END-EXEC.
      *
           EVALUATE RESPONSE-CODE
               WHEN DFHRESP(NORMAL)
                   MOVE INV-INVOICE-NUMBER TO LASTO
                   ADD 1 TO INVOICE-COUNT
                   ADD INV-INVOICE-TOTAL TO INVOICE-TOTAL
                   IF FIRST-RECORD
                       MOVE INV-INVOICE-NUMBER TO FIRSTO
                       MOVE 'N' TO FIRST-RECORD-SW
                   END-IF
               WHEN DFHRESP(ENDFILE)
                   MOVE 'Y' TO INVOICE-EOF-SW
               WHEN OTHER
                   PERFORM 9999-TERMINATE-PROGRAM
           END-EVALUATE.
      *
       3000-END-INVOICE-BROWSE.
      *
           EXEC CICS
               ENDBR FILE('INVFEC')
                     RESP(RESPONSE-CODE)
           END-EXEC.
      *
           IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM 9999-TERMINATE-PROGRAM
           END-IF.
      *
       4000-SEND-SUMMARY-MAP.
      *
           MOVE 'SFEC'        TO TRANIDO.
           MOVE INVOICE-COUNT TO COUNTO.
           MOVE INVOICE-TOTAL TO TOTALO.
      *
           EXEC CICS
               SEND MAP('SUMMFEC')
                    MAPSET('SUMSFEC')
                    FROM(SUMMFECO)
                    ERASE
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
