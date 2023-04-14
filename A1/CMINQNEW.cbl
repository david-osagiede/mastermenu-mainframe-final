       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  CMINQYYY.
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
       01 WS-COMMUNICATION-AREA               PIC X VALUE SPACES.
      *
       01 WS-FLAGS.
           05 WS-VALID-DATA-FLAG              PIC X VALUE 'Y'.
               88 WS-VALID-DATA-88                  VALUE 'Y'.
           05 WS-SEND-FLAG                    PIC X.
               88  WS-SEND-ERASE-88                 VALUE '1'.
               88  WS-SEND-DATAONLY-88              VALUE '2'.
               88  WS-SEND-DATAONLY-ALARM-88        VALUE '3'.
      *
       01 WS-RESPONSE-CODES.
           05 WS-RESPONSE-CODE                PIC S9(8)  COMP VALUE 0.
           05 WS-RESPONSE-CODE2               PIC S9(8)  COMP VALUE 0.
      *
       01 WS-CMF-FILE-STATUS-INFO.
           05 WS-CMF-OPEN                     PIC X.
               88 WS-CMF-OPEN-88                    VALUE 'Y'.
               88 WS-CMF-CLOSED-88                  VALUE 'Y'.
           05 WS-CMF-OPEN-STATUS              PIC S9(8) COMP VALUE 0.
           05 WS-CMF-ENABLE-STATUS            PIC S9(8) COMP VALUE 0.
      *
       COPY CMFYYY.
      *
       COPY ERRPARMS.
      *
       COPY INQSYYY.
      *
       COPY DFHAID.
      *
       LINKAGE SECTION.
      *
       01  WS-DFHCOMMAREA                      PIC X.
      *
       PROCEDURE DIVISION.
      *
       0000-PROCESS-CUSTOMER-INQUIRY.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '*'                       TO WS-HA-EXEC-TEXT-T1.
           MOVE WS-HA-UNEXPECTED-ABEND        TO WS-HA-EXEC-TEXT-T2.
           MOVE ALL '*'                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               HANDLE ABEND LABEL(9999-HANDLE-ABEND)
           END-EXEC.
      *
           EVALUATE TRUE
      *
               WHEN EIBCALEN = ZERO
                   MOVE LOW-VALUE             TO INQMYYYO
                   MOVE 'IYYY'                TO TRANIDO
                   SET WS-SEND-ERASE-88       TO TRUE
                   PERFORM 1400-WS-SEND-CUSTOMER-MAP
      *
               WHEN EIBAID = DFHCLEAR
                   MOVE LOW-VALUE             TO INQMYYYO
                   MOVE 'IYYY'                TO TRANIDO
                   SET WS-SEND-ERASE-88       TO TRUE
                   PERFORM 1400-WS-SEND-CUSTOMER-MAP
      *
               WHEN EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
                   CONTINUE
      *
               WHEN EIBAID = DFHPF3 OR DFHPF12
                   PERFORM 8100-CMF-CLOSE
      *            PERFORM 8200-MENU-RETURN
                   PERFORM 8300-SEND-TERMINATION-MSG
      *
               WHEN EIBAID = DFHENTER
                   PERFORM 1000-PROCESS-CUSTOMER-MAP
      *
               WHEN OTHER
                   MOVE LOW-VALUE TO INQMYYYO
                   MOVE 'Invalid key pressed.'
                     TO MESSAGEO
                   SET WS-SEND-DATAONLY-ALARM-88
                    TO TRUE
                   PERFORM 1400-WS-SEND-CUSTOMER-MAP
      *
           END-EVALUATE.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE'                     TO WS-HA-EXEC-TEXT-T4.
           MOVE 'TRANSACTION=IYYY'            TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-TRANSIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE TRANSACTION('IYYY')
                      RESP(WS-RESPONSE-CODE)
                      RESP2(WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               PERFORM 9999-HANDLE-ABEND
           END-IF.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'RETURN '                     TO WS-HA-EXEC-TEXT-T4.
           MOVE 'TRANSACTION=IYYY'            TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-TRANSIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               RETURN TRANSID('IYYY')
                      COMMAREA(WS-COMMUNICATION-AREA)
           END-EXEC.
      *
           MOVE EIBRESP                       TO WS-RESPONSE-CODE.
           MOVE EIBRESP2                      TO WS-RESPONSE-CODE2.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               MOVE 'RETURN TRANSID '         TO WS-RT-MSG-HEADER
               MOVE 'OK             '         TO WS-RT-MSG-NORMAL
               MOVE 'IYYY           '         TO WS-RT-MSG-OTHER
               PERFORM 9100-RESPTEXT
           END-IF.
      *
       1000-PROCESS-CUSTOMER-MAP.
      *
           PERFORM 1100-RECEIVE-CUSTOMER-MAP.
           PERFORM 1200-EDIT-CUSTOMER-DATA.
      *
           IF WS-VALID-DATA-88
               PERFORM 1300-GET-CUSTOMER-RECORD
               SET WS-SEND-DATAONLY-88
                TO TRUE
               PERFORM 1400-WS-SEND-CUSTOMER-MAP
           ELSE
               SET WS-SEND-DATAONLY-ALARM-88
                TO TRUE
               PERFORM 1400-WS-SEND-CUSTOMER-MAP
           END-IF.
      *
       1100-RECEIVE-CUSTOMER-MAP.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'RECEIVE MAP'                 TO WS-HA-EXEC-TEXT-T4.
           MOVE 'MAPSET=INQSYYY'              TO WS-HA-EXEC-TEXT-T5.
           MOVE 'MAP   =INQMYYY'              TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               RECEIVE MAP('INQMYYY')
                       MAPSET('INQSYYY')
                       INTO(INQMYYYI)
                       RESP(WS-RESPONSE-CODE)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               MOVE 'RECEIVE MAP    '         TO WS-RT-MSG-HEADER
               MOVE 'OK             '         TO WS-RT-MSG-NORMAL
               MOVE 'INQSYYY/INQMYYY'         TO WS-RT-MSG-OTHER
               PERFORM 9100-RESPTEXT
           END-IF.
      ***
           INSPECT INQMYYYI
               REPLACING ALL '_' BY SPACE.
      ***
       1200-EDIT-CUSTOMER-DATA.
      *
           IF       CUSTNOL = ZERO
                 OR CUSTNOI = SPACE
               MOVE 'N'                       TO WS-VALID-DATA-FLAG
               MOVE 'You must enter a customer number.'
                 TO MESSAGEO
           END-IF.
      *
       1300-GET-CUSTOMER-RECORD.
      *
           PERFORM 8000-CMF-OPEN.
      *
           EXEC CICS
               READ FILE('CMFYYY')
                    INTO(WS-CUSTOMER-MASTER-RECORD)
                    RIDFLD(CUSTNOI)
                    RESP(WS-RESPONSE-CODE)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               MOVE SPACE                     TO MESSAGEO
               MOVE WS-CM-LAST-NAME           TO LNAMEO
               MOVE WS-CM-FIRST-NAME          TO FNAMEO
               MOVE WS-CM-ADDRESS             TO ADDRO
               MOVE WS-CM-CITY                TO CITYO
               MOVE WS-CM-STATE               TO STATEO
               MOVE WS-CM-ZIP-CODE            TO ZIPCODEO
           ELSE IF WS-RESPONSE-CODE = DFHRESP(NOTFND)
               MOVE 'N'                       TO WS-VALID-DATA-FLAG
               MOVE 'That customer does not exist.'
                 TO MESSAGEO
               MOVE SPACE                     TO LNAMEO
                                                 FNAMEO
                                                 ADDRO
                                                 CITYO
                                                 STATEO
                                                 ZIPCODEO
           ELSE
               MOVE 'READ FILE      '         TO WS-RT-MSG-HEADER
               MOVE 'OK             '         TO WS-RT-MSG-NORMAL
               MOVE 'CMFYYY         '         TO WS-RT-MSG-OTHER
               PERFORM 9100-RESPTEXT
           END-IF.
      *
       1400-WS-SEND-CUSTOMER-MAP.
      *
           EVALUATE TRUE
               WHEN WS-SEND-ERASE-88
                   MOVE SPACES                TO WS-HA-EXEC-TEXT
                   MOVE ALL '='               TO WS-HA-EXEC-TEXT-T1
                   MOVE 'ABEND'               TO WS-HA-EXEC-TEXT-T2
                   MOVE 'EVALUATE WHEN WS-SEND-ERASE-88'
                     TO WS-HA-EXEC-TEXT-T3
                   MOVE 'EXEC CICS SEND MAP'
                     TO WS-HA-EXEC-TEXT-T4
                   MOVE 'MAPSET=INQSYYY'      TO WS-HA-EXEC-TEXT-T5
                   MOVE 'MAP   =INQMYYY'      TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='               TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP('INQMYYY')
                            MAPSET('INQSYYY')
                            FROM(INQMYYYO)
                            ERASE
                            RESP(WS-RESPONSE-CODE)
                   END-EXEC
      *
                   IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                       CONTINUE
                   ELSE
                       MOVE 'SEND MAP       ' TO WS-RT-MSG-HEADER
                       MOVE 'OK             ' TO WS-RT-MSG-NORMAL
                       MOVE 'ERASE          ' TO WS-RT-MSG-OTHER
                       PERFORM 9100-RESPTEXT
                   END-IF
      *
               WHEN WS-SEND-DATAONLY-88
                   MOVE SPACES                TO WS-HA-EXEC-TEXT
                   MOVE ALL '='               TO WS-HA-EXEC-TEXT-T1
                   MOVE 'ABEND'               TO WS-HA-EXEC-TEXT-T2
                   MOVE 'EVALUATE WHEN WS-SEND-DATAONLY-88'
                     TO WS-HA-EXEC-TEXT-T3
                   MOVE 'EXEC CICS SEND MAP'
                     TO WS-HA-EXEC-TEXT-T4
                   MOVE 'MAPSET=INQSYYY'      TO WS-HA-EXEC-TEXT-T5
                   MOVE 'MAP   =INQMYYY'      TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='               TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP('INQMYYY')
                            MAPSET('INQSYYY')
                            FROM(INQMYYYO)
                            DATAONLY
                            RESP(WS-RESPONSE-CODE)
                   END-EXEC
      *
                   IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                       CONTINUE
                   ELSE
                       MOVE 'SEND MAP       ' TO WS-RT-MSG-HEADER
                       MOVE 'OK             ' TO WS-RT-MSG-NORMAL
                       MOVE 'DATAONLY       ' TO WS-RT-MSG-OTHER
                       PERFORM 9100-RESPTEXT
                   END-IF
      *
               WHEN WS-SEND-DATAONLY-ALARM-88
                   MOVE SPACES                TO WS-HA-EXEC-TEXT
                   MOVE ALL '='               TO WS-HA-EXEC-TEXT-T1
                   MOVE 'ABEND'               TO WS-HA-EXEC-TEXT-T2
                   MOVE 'EVALUATE WHEN WS-SEND-DATAONLY-ALARM-88'
                     TO WS-HA-EXEC-TEXT-T3
                   MOVE 'EXEC CICS SEND MAP'
                     TO WS-HA-EXEC-TEXT-T4
                   MOVE 'MAPSET=INQSYYY'      TO WS-HA-EXEC-TEXT-T5
                   MOVE 'MAP   =INQMYYY'      TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='               TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP('INQMYYY')
                            MAPSET('INQSYYY')
                            FROM(INQMYYYO)
                            DATAONLY
                            ALARM
                            RESP(WS-RESPONSE-CODE)
                   END-EXEC
      *
                   IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                       CONTINUE
                   ELSE
                       MOVE 'SEND MAP       ' TO WS-RT-MSG-HEADER
                       MOVE 'OK             ' TO WS-RT-MSG-NORMAL
                       MOVE 'DATAONLY-ALARM ' TO WS-RT-MSG-OTHER
                       PERFORM 9100-RESPTEXT
                   END-IF
      *
           END-EVALUATE.
      *
       8000-CMF-OPEN.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE'                     TO WS-HA-EXEC-TEXT-T4.
           MOVE 'FILE=CMFYYY'                 TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-TRANSIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE FILE('CMFYYY')
                   OPENSTATUS(WS-CMF-OPEN-STATUS)
                   ENABLESTATUS(WS-CMF-ENABLE-STATUS)
                   RESP        (WS-RESPONSE-CODE)
                   RESP2       (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-CMF-OPEN-STATUS = DFHVALUE(CLOSED)
               MOVE 'SET FILE OPEN'           TO WS-HA-EXEC-TEXT-T4
               MOVE 'FILE=CMFYYY'             TO WS-HA-EXEC-TEXT-T5
               EXEC CICS
                   SET FILE('CMFYYY') OPEN
               END-EXEC
               MOVE 'Y' TO WS-CMF-OPEN
           ELSE IF WS-CMF-OPEN-STATUS = DFHVALUE(OPEN)
               NEXT SENTENCE
           ELSE
               PERFORM 9999-HANDLE-ABEND
           END-IF.
      *
       8100-CMF-CLOSE.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE'                     TO WS-HA-EXEC-TEXT-T4.
           MOVE 'FILE=CMFYYY'                 TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-FILENOTFOUND)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE FILE    ('CMFYYY')
                   OPENSTATUS  (WS-CMF-OPEN-STATUS)
                   ENABLESTATUS(WS-CMF-ENABLE-STATUS)
                   RESP        (WS-RESPONSE-CODE)
                   RESP2       (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               IF WS-CMF-OPEN-STATUS = DFHVALUE(OPEN)
                   MOVE 'SET FILE CLOSED'     TO WS-HA-EXEC-TEXT-T4
                   MOVE 'FILE=CMFYYY'         TO WS-HA-EXEC-TEXT-T5
                   EXEC CICS
                       SET FILE ('CMFYYY') CLOSED
                   END-EXEC
                   MOVE WS-CLOSED-CNST        TO WS-CMF-OPEN
               END-IF
           ELSE
               PERFORM 9999-HANDLE-ABEND
           END-IF.
      *
       8200-MENU-RETURN.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE'                     TO WS-HA-EXEC-TEXT-T4.
           MOVE 'PROGRAM=UUMENYYY'            TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-PGMIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE PROGRAM('UUMENYYY')
                   RESP       (WS-RESPONSE-CODE)
                   RESP2      (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               PERFORM 9999-HANDLE-ABEND
           END-IF.
      *
           EXEC CICS
               XCTL PROGRAM('UUMENYYY')
                   RESP(WS-RESPONSE-CODE)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               MOVE 'XCTL PROGRAM   '         TO WS-RT-MSG-HEADER
               MOVE 'OK             '         TO WS-RT-MSG-NORMAL
               MOVE 'UUMENYYY       '         TO WS-RT-MSG-OTHER
               PERFORM 9100-RESPTEXT
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
       9100-RESPTEXT.
      *
           MOVE WS-RESPONSE-CODE              TO WS-RT-RESP-NBR.
           MOVE SPACES                        TO WS-RT-MSG.
           MOVE WS-RT-MSG-HEADER              TO WS-RT-MSG-HDR
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               MOVE WS-RT-MSG-NORMAL          TO WS-RT-MSG-T1
               MOVE WS-RT-MSG-OTHER           TO WS-RT-MSG-T2
           ELSE
               IF WS-RT-RESP-NBR-VALID-88
                   MOVE WS-RT-MSG-NBR-TEXT(WS-RT-RESP-NBR)
                     TO WS-RT-MSG-T1
                   MOVE WS-RT-MSG-OTHER       TO WS-RT-MSG-T2
               ELSE
                   MOVE WS-RT-RESP-NBR-INVALID-MSG
                     TO WS-RT-MSG-T1
                   MOVE WS-RT-MSG-OTHER       TO WS-RT-MSG-T2
               END-IF
      *
               MOVE EIBRESP                   TO WS-EM-RESP
               MOVE EIBRESP2                  TO WS-EM-RESP2
               MOVE EIBTRNID                  TO WS-EM-TRNID
               MOVE EIBRSRCE                  TO WS-EM-RSRCE
      *
               MOVE WS-RT-MSG                 TO WS-EM-MSG
      *
               EXEC CICS
                   SEND TEXT FROM(WS-EM-ERROR-MESSAGE)
                       FREEKB
                       ERASE
               END-EXEC
      *
               STOP RUN
      *
           END-IF.
      *
       9999-HANDLE-ABEND.
      *
           MOVE WS-HA-EXEC-TEXT               TO WS-HA-HANDLE-ABEND-MSG.
      *
           EXEC CICS
               SEND TEXT FROM(WS-HA-HANDLE-ABEND-MSG)
                   FREEKB
                   ERASE
           END-EXEC.
      *
           STOP RUN.
      *