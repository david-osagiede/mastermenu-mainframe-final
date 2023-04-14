       IDENTIFICATION  DIVISION.
      *
       PROGRAM-ID.  UUMENFEC.
       AUTHOR. David Osagiede.
       DATE-WRITTEN. 2023-02-03.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
       01  SWITCHES.
      *
           05  VALID-DATA-SW               PIC X(01) VALUE 'Y'.
               88  VALID-DATA              VALUE 'Y'.
      *
       01  FLAGS.
      *
           05  SEND-FLAG                   PIC X(01).
               88  SEND-ERASE              VALUE '1'.
               88  SEND-DATAONLY           VALUE '2'.
               88  SEND-DATAONLY-ALARM     VALUE '3'.
      *
       01  PROGRAM-TABLE.
      *
           05  PROGRAM-LIST.
               10  PROGRAM-1               PIC X(08) VALUE 'CMINQFEC'.
               10  PROGRAM-2               PIC X(08) VALUE 'CMMNTFEC'.
               10  PROGRAM-3               PIC X(08) VALUE 'CMORDFEC'.
           05  PROGRAM-NAME                REDEFINES PROGRAM-LIST
                                           OCCURS 3 TIMES
                                           PIC X(08).
      *
       01  SUBSCRIPTS.
           05  ACTION-SUB              PIC 9(01).
      *
       01  END-OF-SESSION-MESSAGE      PIC X(13) VALUE 'Session ended'.
      *
       01  RESPONSE-CODE               PIC S9(08) COMP.
      *
       01  COMMUNICATION-AREA          PIC X(01).
      *
       COPY MENSFEC.
      *
       COPY DFHAID.
      *
       COPY ATTR.
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA                 PIC X(01).
      *
       PROCEDURE DIVISION.
      *
       0000-PROCESS-MASTER-MENU.
      *
           EVALUATE TRUE
      *
               WHEN EIBCALEN = ZERO
                   MOVE LOW-VALUE TO MENMFECO
                   SET SEND-ERASE TO TRUE
                   PERFORM 1400-SEND-MENU-MAP
      *
               WHEN EIBAID = DFHCLEAR
                   MOVE LOW-VALUE TO MENMFECO
                   SET SEND-ERASE TO TRUE
                   PERFORM 1400-SEND-MENU-MAP
      *
               WHEN EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
                   CONTINUE
      *
               WHEN EIBAID = DFHPF3 OR DFHPF12
                   PERFORM 2000-SEND-TERMINATION-MESSAGE
                   EXEC CICS
                       RETURN
                   END-EXEC
      *
               WHEN EIBAID = DFHENTER
                   PERFORM 1000-PROCESS-MENU-MAP
      *
               WHEN OTHER
                   MOVE 'Invalid key pressed.' TO MESSAGEO
                   SET SEND-DATAONLY-ALARM TO TRUE
                   PERFORM 1400-SEND-MENU-MAP
      *
           END-EVALUATE.
      *
           EXEC CICS
               RETURN TRANSID('UFEC')
                      COMMAREA(COMMUNICATION-AREA)
           END-EXEC.
      *
       1000-PROCESS-MENU-MAP.
      *
           PERFORM 1100-RECEIVE-MENU-MAP.
           PERFORM 1200-EDIT-MENU-DATA.
           IF VALID-DATA
               MOVE ACTIONI TO ACTION-SUB
               PERFORM 1300-BRANCH-TO-PROGRAM
           END-IF.
           SET SEND-DATAONLY-ALARM TO TRUE.
           PERFORM 1400-SEND-MENU-MAP.
      *
       1100-RECEIVE-MENU-MAP.
      *
           EXEC CICS
               RECEIVE MAP('MENMFEC')
                       MAPSET('MENSFEC')
                       INTO(MENMFECI)
           END-EXEC.
      *
       1200-EDIT-MENU-DATA.
      *
           IF ACTIONI NOT = '1' AND '2' AND '3'
               MOVE ATTR-REVERSE TO ACTIONH
               MOVE 'You must enter 1, 2, or 3.' TO MESSAGEO
               MOVE 'N' TO VALID-DATA-SW
           END-IF.
      *
       1300-BRANCH-TO-PROGRAM.
      *
           EXEC CICS
               XCTL PROGRAM(PROGRAM-NAME(ACTION-SUB))
               RESP(RESPONSE-CODE)
           END-EXEC.
      *
           MOVE 'That program is not available.' TO MESSAGEO.
      *
       1400-SEND-MENU-MAP.
      *
           MOVE 'UFEC' TO TRANIDO.
           EVALUATE TRUE
               WHEN SEND-ERASE
                   EXEC CICS
                       SEND MAP('MENMFEC')
                            MAPSET('MENSFEC')
                            FROM(MENMFECO)
                            ERASE
                   END-EXEC
               WHEN SEND-DATAONLY
                   EXEC CICS
                       SEND MAP('MENMFEC')
                            MAPSET('MENSFEC')
                            FROM(MENMFECO)
                            DATAONLY
                   END-EXEC
               WHEN SEND-DATAONLY-ALARM
                   EXEC CICS
                       SEND MAP('MENMFEC')
                            MAPSET('MENSFEC')
                            FROM(MENMFECO)
                            DATAONLY
                            ALARM
                   END-EXEC
           END-EVALUATE.
      *
       2000-SEND-TERMINATION-MESSAGE.
      *
           EXEC CICS
               SEND TEXT FROM(END-OF-SESSION-MESSAGE)
                         ERASE
                         FREEKB
           END-EXEC.
