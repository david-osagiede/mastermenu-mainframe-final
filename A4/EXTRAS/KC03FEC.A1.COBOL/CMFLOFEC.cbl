       IDENTIFICATION DIVISION.
       PROGRAM-ID. CMFLOFEC.
       AUTHOR. David Osagiede.
       DATE-WRITTEN. 1/13/2023.
      *PROGRAM DESCRIPTION:
      *Checks to see if the CM-FILE open successfully, if
      *it already had data loaded previously or if it failed
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT CM-INPUT ASSIGN TO CMDATA
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-INFILE-SW.

           SELECT CM-FILE ASSIGN TO OUTFILE
               RECORD KEY IS CM-KEY
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-OUTFILE-SW.
      *
       DATA DIVISION.
       FILE SECTION.
       FD CM-INPUT
           RECORDING MODE IS F
           RECORD CONTAINS 118 CHARACTERS
           DATA RECORD IS CM-INPUT-RECORD.
       01 CM-INPUT-RECORD.
           05  CM-KEY-IN.
               10  CM-CUSTOMER-NUMBER        PIC X(6).
           05  CM-FIRST-NAME                 PIC X(20).
           05  CM-LAST-NAME                  PIC X(30).
           05  CM-ADDRESS                    PIC X(30).
           05  CM-CITY                       PIC X(20).
           05  CM-STATE                      PIC X(2).
           05  CM-ZIP-CODE                   PIC X(10).

       FD CM-FILE
           RECORD CONTAINS 118 CHARACTERS
           DATA RECORD IS CM-FILE-RECORD.
       01 CM-FILE-RECORD.
           05  CM-KEY.
               10  CM-CUSTOMER-NUMBER        PIC X(6).
           05  CM-FIRST-NAME                 PIC X(20).
           05  CM-LAST-NAME                  PIC X(30).
           05  CM-ADDRESS                    PIC X(30).
           05  CM-CITY                       PIC X(20).
           05  CM-STATE                      PIC X(2).
           05  CM-ZIP-CODE                   PIC X(10).
      *
       WORKING-STORAGE SECTION.
       01 WS-INFILE-SW                       PIC X(02)  VALUE SPACES.
           88  WS-INFILE-SUCCESS                 VALUE '00'.
           88  WS-INFILE-EOF                     VALUE '10'.
       01 WS-OUTFILE-SW                      PIC X(02)  VALUE SPACES.
           88  WS-OUTFILE-SUCCESS                VALUE '00'.
           88  WS-OUTFILE-IOERROR                VALUE '37'.
           88  WS-OUTFILE-EOF                    VALUE '10'.
       01 WS-CM-REC                          PIC X(118).
       01 WS-EOF-SW-IN                       PIC X(01)  VALUE 'N'.
           88  WS-EOF-IN-NO                      VALUE 'N'.
           88  WS-EOF-IN-YES                     VALUE 'Y'.
       01 WS-EOF-SW-OUT                      PIC X(01)  VALUE 'N'.
           88  WS-EOF-OUT-NO                     VALUE 'N'.
           88  WS-EOF-OUT-YES                    VALUE 'Y'.
       01 WS-NBR                             PIC 9      VALUE 0.
      *
       PROCEDURE DIVISION.
       000-MAIN-PARA.
           MOVE SPACE TO WS-INFILE-SW
                         WS-OUTFILE-SW
                         WS-CM-REC.
      *
           OPEN INPUT CM-INPUT.
           IF WS-INFILE-SUCCESS
               DISPLAY "CM-INPUT OPEN SUCCESSFUL"
           ELSE
               DISPLAY "WS-INFILE-SW=" WS-INFILE-SW
               DISPLAY "CM-INPUT OPEN ERROR"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           OPEN OUTPUT CM-FILE.
           IF WS-OUTFILE-SUCCESS
               DISPLAY "CM-FILE OPEN SUCCESSFUL"
           ELSE IF WS-OUTFILE-IOERROR
               DISPLAY "WS-OUTFILE-SW=" WS-OUTFILE-SW
               DISPLAY "CM-FILE IOERROR - OPEN OUTPUT SHOULD BE "
                       "OPEN INPUT OR I-O OR EXTEND  "
               DISPLAY "- DATA MAY ALREADY HAVE BEEN LOADED PREVIOUSLY"
               DISPLAY "- DELETE AND INITIALIZE FILE TO RELOAD DATA"
               PERFORM 800-PROGRAM-FAILED
           ELSE
               DISPLAY "WS-OUTFILE-SW=" WS-OUTFILE-SW
               DISPLAY "CM-FILE OPENING ERROR"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           READ CM-INPUT
               AT END MOVE 'Y' TO WS-EOF-SW-IN.
      *
           IF WS-EOF-IN-NO THEN
               DISPLAY "1ST READ CM-INPUT-RECORD=" CM-INPUT-RECORD
      *
               PERFORM 100-LOAD-PARA
                   UNTIL WS-EOF-IN-YES
           ELSE
               DISPLAY "NO DATA IN CM-INPUT"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           PERFORM 900-COMPLETED-OK.
      *
       100-LOAD-PARA.
      *
           WRITE CM-FILE-RECORD FROM CM-INPUT-RECORD.
           DISPLAY "WRITE TO CM-FILE".
      *
           READ CM-INPUT
               AT END MOVE 'Y' TO WS-EOF-SW-IN.
      *
           IF WS-EOF-IN-NO THEN
               DISPLAY "NTH READ CM-INPUT-RECORD=" CM-INPUT-RECORD
           ELSE
               DISPLAY "EOF CM-INPUT"
           END-IF.
      *
       800-PROGRAM-FAILED.
           DISPLAY "PROGRAM TERMINATED WITH DIVIDE BY ZERO!".
           DISPLAY "CHECK ERROR MESSAGES IN SYSOUT PART OF JOB SUMMARY".
           COMPUTE WS-NBR = WS-NBR / WS-NBR.
      *
       900-COMPLETED-OK.
           DISPLAY "PROGRAM COMPLETED OK".
           STOP RUN.
      *