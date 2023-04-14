       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  GETINFEC.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
       COPY INVCTL.
      *
       01 WS-CONSTANTS.
           05 WS-YES-CNST                     PIC X VALUE 'Y'.
           05 WS-NO-CNST                      PIC X VALUE 'N'.
           05 WS-OPEN-CNST                    PIC X VALUE 'Y'.
           05 WS-CLOSED-CNST                  PIC X VALUE 'N'.
           05 WS-END-OF-SESSION-MESSAGE       PIC X(13)
               VALUE 'Session ended'.
      *

      *
       01 WS-CTL-FILE-STATUS-INFO.
           05 WS-CTL-OPEN                     PIC X.
               88 WS-CTL-OPEN-88                    VALUE 'Y'.
               88 WS-CTL-CLOSED-88                  VALUE 'Y'.
           05 WS-CTL-OPEN-STATUS              PIC S9(8) COMP VALUE 0.
           05 WS-CTL-ENABLE-STATUS            PIC S9(8) COMP VALUE 0.
      *
       01 WS-RESPONSE-CODE                   PIC S9(8)  COMP.
      *
       COPY ERRPARMS.
       LINKAGE SECTION.
      *

       01  DFHCOMMAREA   PIC 9(06).
      *
       PROCEDURE DIVISION.
      *
       0000-GET-INVOICE-NUMBER.
      *
           Perform 8600-CTL-OPEN.
           MOVE ZERO TO INVCTL-RECORD-KEY.
           EXEC CICS
               READ FILE('CTLFEC')
                    INTO(INVCTL-RECORD)
                    RIDFLD(INVCTL-RECORD-KEY)
                    UPDATE
           END-EXEC.
           MOVE INVCTL-NEXT-INVOICE-NUMBER TO DFHCOMMAREA.
           ADD 1 TO INVCTL-NEXT-INVOICE-NUMBER.
           EXEC CICS
               REWRITE FILE('CTLFEC')
                       FROM(INVCTL-RECORD)
           END-EXEC.
           Perform 8700-CTL-CLOSE.
           EXEC CICS
               RETURN
           END-EXEC.
      *
       8600-CTL-OPEN.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE'                     TO WS-HA-EXEC-TEXT-T4.
           MOVE 'FILE=CTLFEC'                 TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-TRANSIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE FILE('CTLFEC')
                   OPENSTATUS(WS-CTL-OPEN-STATUS)
                   ENABLESTATUS(WS-CTL-ENABLE-STATUS)
                   RESP        (WS-RESPONSE-CODE)
                   RESP2       (WS-RESPONSE-CODE)
           END-EXEC.
      *
           IF WS-CTL-OPEN-STATUS = DFHVALUE(CLOSED)
               MOVE 'SET FILE OPEN'           TO WS-HA-EXEC-TEXT-T4
               MOVE 'FILE=CTLFEC'             TO WS-HA-EXEC-TEXT-T5
               EXEC CICS
                   SET FILE('CTLFEC') OPEN
               END-EXEC
               MOVE 'Y' TO WS-CTL-OPEN
           ELSE IF WS-CTL-OPEN-STATUS = DFHVALUE(OPEN)
               NEXT SENTENCE
           ELSE
               PERFORM 9000-HANDLE-ABEND
           END-IF.
      *
      *
       8700-CTL-CLOSE.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE'                     TO WS-HA-EXEC-TEXT-T4.
           MOVE 'FILE=CTLFEC'                 TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-FILENOTFOUND)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE FILE    ('CTLFEC')
                   OPENSTATUS  (WS-CTL-OPEN-STATUS)
                   ENABLESTATUS(WS-CTL-ENABLE-STATUS)
                   RESP        (WS-RESPONSE-CODE)
                   RESP2       (WS-RESPONSE-CODE)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               IF WS-CTL-OPEN-STATUS = DFHVALUE(OPEN)
                   MOVE 'SET FILE CLOSED'     TO WS-HA-EXEC-TEXT-T4
                   MOVE 'FILE=CTLFEC'         TO WS-HA-EXEC-TEXT-T5
                   EXEC CICS
                       SET FILE ('CTLFEC') CLOSED
                   END-EXEC
                   MOVE WS-CLOSED-CNST        TO WS-CTL-OPEN
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


