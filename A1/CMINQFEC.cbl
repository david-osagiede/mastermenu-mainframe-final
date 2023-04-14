       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  CMINQFEC.
       AUTHOR. David Osagiede.
       DATE-WRITTEN. 1/13/2023.
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
      *
       01 WS-CMF-FILE-STATUS-INFO.
           05 WS-CMF-OPEN                     PIC X.
               88 WS-CMF-OPEN-88                    VALUE 'Y'.
               88 WS-CMF-CLOSED-88                  VALUE 'Y'.
           05 WS-CMF-OPEN-STATUS              PIC S9(8) COMP VALUE 0.
           05 WS-CMF-ENABLE-STATUS            PIC S9(8) COMP VALUE 0.
      *
       COPY ERRPARMS.
      *

       01  SWITCHES.
      *
           05  VALID-DATA-SW               PIC X    VALUE 'Y'.
               88 VALID-DATA                        VALUE 'Y'.
      *
       01  FLAGS.
      *
           05  SEND-FLAG                   PIC X.
               88  SEND-ERASE                       VALUE '1'.
               88  SEND-DATAONLY                    VALUE '2'.
               88  SEND-DATAONLY-ALARM              VALUE '3'.
      *
       01  COMMUNICATION-AREA              PIC X.
      *
       01  WS-RESPONSE-CODE                PIC S9(8)  COMP.
      *
       COPY CMFFEC.
      *
       COPY INQSFEC.
      *
       COPY DFHAID.
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA                     PIC X.
      *
       PROCEDURE DIVISION.
      *
       0000-PROCESS-CUSTOMER-INQUIRY.
      *
           EVALUATE TRUE
      *
               WHEN EIBCALEN = ZERO
                   MOVE LOW-VALUE TO INQMFECO
                   MOVE 'IFEC'    TO TRANIDO
                   SET SEND-ERASE TO TRUE
                   PERFORM 1400-SEND-CUSTOMER-MAP
      *
               WHEN EIBAID = DFHCLEAR
                   MOVE LOW-VALUE TO INQMFECO
                   MOVE 'IFEC'    TO TRANIDO
                   SET SEND-ERASE TO TRUE
                   PERFORM 1400-SEND-CUSTOMER-MAP
      *
               WHEN EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
                   CONTINUE
      *
      *
           WHEN EIBAID = DFHPF3 OR DFHPF12
               PERFORM 8100-CMF-CLOSE
      *        PERFORM 8200-MENU-RETURN
               PERFORM 8300-SEND-TERMINATION-MSG
      *
      *
               WHEN EIBAID = DFHENTER
                   PERFORM 1000-PROCESS-CUSTOMER-MAP
      *
               WHEN OTHER
                   MOVE LOW-VALUE TO INQMFECO
                   MOVE 'Invalid key pressed.' TO MESSAGEO
                   SET SEND-DATAONLY-ALARM TO TRUE
                   PERFORM 1400-SEND-CUSTOMER-MAP
      *
           END-EVALUATE.
      *
           EXEC CICS
               RETURN TRANSID('IFEC')
                      COMMAREA(COMMUNICATION-AREA)
           END-EXEC.
      *
       1000-PROCESS-CUSTOMER-MAP.
      *
           PERFORM 1100-RECEIVE-CUSTOMER-MAP.
           PERFORM 1200-EDIT-CUSTOMER-DATA.
           IF VALID-DATA
               PERFORM 1300-GET-CUSTOMER-RECORD
           END-IF.
           IF VALID-DATA
               SET SEND-DATAONLY TO TRUE
               PERFORM 1400-SEND-CUSTOMER-MAP
           ELSE
               SET SEND-DATAONLY-ALARM TO TRUE
               PERFORM 1400-SEND-CUSTOMER-MAP
           END-IF.
      *
       1100-RECEIVE-CUSTOMER-MAP.
      *
           EXEC CICS
               RECEIVE MAP('INQMFEC')
                       MAPSET('INQSFEC')
                       INTO(INQMFECI)
           END-EXEC.
      *
       1200-EDIT-CUSTOMER-DATA.
      *
           IF       CUSTNOL = ZERO
                 OR CUSTNOI = SPACE
               MOVE 'N' TO VALID-DATA-SW
               MOVE 'You must enter a customer number.' TO MESSAGEO
           END-IF.
      *
       1300-GET-CUSTOMER-RECORD.
      *
      * ADD NEW PERFORM TO OPEN CMF FILE
      *
           PERFORM 8000-CMF-OPEN.
      *
           EXEC CICS
               READ FILE('CMFFEC')
                    INTO(WS-CUSTOMER-MASTER-RECORD)
                    RIDFLD(CUSTNOI)
                    RESP(WS-RESPONSE-CODE)
           END-EXEC.
           EXEC CICS
               READ FILE('CMFFEC')
                    INTO(WS-CUSTOMER-MASTER-RECORD)
                    RIDFLD(CUSTNOI)
                    RESP(WS-RESPONSE-CODE)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               MOVE SPACE            TO MESSAGEO
               MOVE WS-CM-LAST-NAME  TO LNAMEO
               MOVE WS-CM-FIRST-NAME TO FNAMEO
               MOVE WS-CM-ADDRESS    TO ADDRO
               MOVE WS-CM-CITY       TO CITYO
               MOVE WS-CM-STATE      TO STATEO
               MOVE WS-CM-ZIP-CODE   TO ZIPCODEO
           ELSE IF WS-RESPONSE-CODE = DFHRESP(NOTFND)
               MOVE 'N' TO VALID-DATA-SW
               MOVE 'That customer does not exist.' TO MESSAGEO
               MOVE SPACE TO LNAMEO
                             FNAMEO
                             ADDRO
                             CITYO
                             STATEO
                             ZIPCODEO
           ELSE
               EXEC CICS
                   ABEND
               END-EXEC
           END-IF.
      *
       1400-SEND-CUSTOMER-MAP.
      *
           EVALUATE TRUE
               WHEN SEND-ERASE
                   EXEC CICS
                       SEND MAP('INQMFEC')
                            MAPSET('INQSFEC')
                            FROM(INQMFECO)
                            ERASE
                       END-EXEC
               WHEN SEND-DATAONLY
                   EXEC CICS
                       SEND MAP('INQMFEC')
                            MAPSET('INQSFEC')
                            FROM(INQMFECO)
                            DATAONLY
                       END-EXEC
               WHEN SEND-DATAONLY-ALARM
                   EXEC CICS
                       SEND MAP('INQMFEC')
                            MAPSET('INQSFEC')
                            FROM(INQMFECO)
                            DATAONLY
                            ALARM
                       END-EXEC
           END-EVALUATE.
      * ADD NEW PARAGRAPHS
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
               PERFORM 9999-HANDLE-ABEND
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
                   MOVE 'FILE=CMFFD9'         TO WS-HA-EXEC-TEXT-T5
                   EXEC CICS
                       SET FILE ('CMFFEC') CLOSED
                   END-EXEC
                   MOVE WS-CLOSED-CNST        TO WS-CMF-OPEN
               END-IF
           ELSE
               PERFORM 9999-HANDLE-ABEND
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
       9999-HANDLE-ABEND.
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
      *
