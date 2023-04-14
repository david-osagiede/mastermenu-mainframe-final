       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTLLOFEC.
       AUTHOR. David Osagiede.
       DATE-WRITTEN. 3/16/2023.
      *PROGRAM DESCRIPTION:
      *Checks to see if the INVCTL-FILE open successfully, if
      *it already had data loaded previously or if it failed
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT INVCTL-INPUT ASSIGN TO CTLDATA
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-INFILE-SW.

           SELECT INVCTL-FILE ASSIGN TO OUTFILE
               RECORD KEY IS INVCTL-RECORD-KEY-1
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-OUTFILE-SW.
      *
       DATA DIVISION.
       FILE SECTION.
       FD INVCTL-INPUT
           RECORDING MODE IS F
           RECORD CONTAINS 7 CHARACTERS
           DATA RECORD IS INVCTL-INPUT-RECORD.
       01  INVCTL-INPUT-RECORD.
      *
           05  INVCTL-RECORD-KEYGRP.
               10  INVCTL-RECORD-KEY           PIC 9(01).
           05  INVCTL-NEXT-INVOICE-NUMBER      PIC 9(06).
      *
       FD INVCTL-FILE
           RECORD CONTAINS 7 CHARACTERS
           DATA RECORD IS INVCTL-FILE-RECORD.
       01  INVCTL-FILE-RECORD.
      *
           05  INVCTL-RECORD-KEYGRP-1.
               10  INVCTL-RECORD-KEY-1           PIC 9(01).
           05  INVCTL-NEXT-INVOICE-NUMBER-1      PIC 9(06).
      *
       WORKING-STORAGE SECTION.
       01 WS-INFILE-SW                       PIC X(02)  VALUE SPACES.
           88  WS-INFILE-SUCCESS                 VALUE '00'.
           88  WS-INFILE-EOF                     VALUE '10'.
       01 WS-OUTFILE-SW                      PIC X(02)  VALUE SPACES.
           88  WS-OUTFILE-SUCCESS                VALUE '00'.
           88  WS-OUTFILE-IOERROR                VALUE '37'.
           88  WS-OUTFILE-EOF                    VALUE '10'.
       01 WS-CM-REC                          PIC X(7).
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
           OPEN INPUT INVCTL-INPUT.
           IF WS-INFILE-SUCCESS
               DISPLAY "INVCTL-INPUT OPEN SUCCESSFUL"
           ELSE
               DISPLAY "WS-INFILE-SW=" WS-INFILE-SW
               DISPLAY "INVCTL-INPUT OPEN ERROR"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           OPEN OUTPUT INVCTL-FILE.
           IF WS-OUTFILE-SUCCESS
               DISPLAY "INVCTL-FILE OPEN SUCCESSFUL"
           ELSE IF WS-OUTFILE-IOERROR
               DISPLAY "WS-OUTFILE-SW=" WS-OUTFILE-SW
               DISPLAY "INVCTL-FILE IOERROR - OPEN OUTPUT SHOULD BE "
                       "OPEN INPUT OR I-O OR EXTEND  "
               DISPLAY "- DATA MAY ALREADY HAVE BEEN LOADED PREVIOUSLY"
               DISPLAY "- DELETE AND INITIALIZE FILE TO RELOAD DATA"
               PERFORM 800-PROGRAM-FAILED
           ELSE
               DISPLAY "WS-OUTFILE-SW=" WS-OUTFILE-SW
               DISPLAY "INVCTL-FILE OPENING ERROR"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           READ INVCTL-INPUT
               AT END MOVE 'Y' TO WS-EOF-SW-IN.
      *
           IF WS-EOF-IN-NO THEN
               DISPLAY "1ST READ INVCTL-INPUT-RECORD="
               INVCTL-INPUT-RECORD
      *
               PERFORM 100-LOAD-PARA
                   UNTIL WS-EOF-IN-YES
           ELSE
               DISPLAY "NO DATA IN INVCTL-INPUT"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           PERFORM 900-COMPLETED-OK.
      *
       100-LOAD-PARA.
      *
           WRITE INVCTL-FILE-RECORD FROM INVCTL-INPUT-RECORD.
           DISPLAY "WRITE TO INVCTL-FILE".
      *
           READ INVCTL-INPUT
               AT END MOVE 'Y' TO WS-EOF-SW-IN.
      *
           IF WS-EOF-IN-NO THEN
               DISPLAY "NTH READ INVCTL-INPUT-RECORD="
               INVCTL-INPUT-RECORD
           ELSE
               DISPLAY "EOF INVCTL-INPUT"
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