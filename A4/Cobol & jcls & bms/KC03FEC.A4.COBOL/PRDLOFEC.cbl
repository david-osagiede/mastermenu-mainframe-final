       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRDLOFEC.
       AUTHOR. David Osagiede.
       DATE-WRITTEN. 3/17/2023.
      *PROGRAM DESCRIPTION:
      *Checks to see if the PRD-FILE open successfully, if
      *it already had data loaded previously or if it failed
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT INVCTL-INPUT ASSIGN TO PRDDATA
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-INFILE-SW.

           SELECT INVCTL-FILE ASSIGN TO OUTFILE
               RECORD KEY IS PRD-KEY-1
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-OUTFILE-SW.
      *
       DATA DIVISION.
       FILE SECTION.
       FD INVCTL-INPUT
           RECORDING MODE IS F
           RECORD CONTAINS 46 CHARACTERS
           DATA RECORD IS PRODUCT-MASTER-INPUT-RECORD.
       01  PRODUCT-MASTER-INPUT-RECORD.
      *
           05  PRD-KEY.
               10  PRD-PRODUCT-CODE            PIC X(10).
           05  PRD-PRODUCT-DESCRIPTION         PIC X(20).
           05  PRD-UNIT-PRICE                  PIC S9(07)V99.
           05  PRD-QUANTITY-ON-HAND            PIC S9(07).
      *
       FD INVCTL-FILE
           RECORD CONTAINS 46 CHARACTERS
           DATA RECORD IS PRODUCT-MASTER-FILE-RECORD.
       01  PRODUCT-MASTER-FILE-RECORD.
      *
           05  PRD-KEY-1.
               10  PRD-PRODUCT-CODE-1            PIC X(10).
           05  PRD-PRODUCT-DESCRIPTION-1         PIC X(20).
           05  PRD-UNIT-PRICE-1                  PIC S9(07)V99.
           05  PRD-QUANTITY-ON-HAND-1            PIC S9(07).
      *
       WORKING-STORAGE SECTION.
       01 WS-INFILE-SW                       PIC X(02)  VALUE SPACES.
           88  WS-INFILE-SUCCESS                 VALUE '00'.
           88  WS-INFILE-EOF                     VALUE '10'.
       01 WS-OUTFILE-SW                      PIC X(02)  VALUE SPACES.
           88  WS-OUTFILE-SUCCESS                VALUE '00'.
           88  WS-OUTFILE-IOERROR                VALUE '37'.
           88  WS-OUTFILE-EOF                    VALUE '10'.
       01 WS-CM-REC                          PIC X(46).
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
               DISPLAY "PRD-INPUT OPEN SUCCESSFUL"
           ELSE
               DISPLAY "WS-INFILE-SW=" WS-INFILE-SW
               DISPLAY "PRD-INPUT OPEN ERROR"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           OPEN OUTPUT INVCTL-FILE.
           IF WS-OUTFILE-SUCCESS
               DISPLAY "PRD-FILE OPEN SUCCESSFUL"
           ELSE IF WS-OUTFILE-IOERROR
               DISPLAY "WS-OUTFILE-SW=" WS-OUTFILE-SW
               DISPLAY "PRD-FILE IOERROR - OPEN OUTPUT SHOULD BE "
                       "OPEN INPUT OR I-O OR EXTEND  "
               DISPLAY "- DATA MAY ALREADY HAVE BEEN LOADED PREVIOUSLY"
               DISPLAY "- DELETE AND INITIALIZE FILE TO RELOAD DATA"
               PERFORM 800-PROGRAM-FAILED
           ELSE
               DISPLAY "WS-OUTFILE-SW=" WS-OUTFILE-SW
               DISPLAY "PRD-FILE OPENING ERROR"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           READ INVCTL-INPUT
               AT END MOVE 'Y' TO WS-EOF-SW-IN.
      *
           IF WS-EOF-IN-NO THEN
               DISPLAY "1ST READ PRDCTL-INPUT-RECORD="
               PRODUCT-MASTER-INPUT-RECORD
      *
               PERFORM 100-LOAD-PARA
                   UNTIL WS-EOF-IN-YES
           ELSE
               DISPLAY "NO DATA IN PRD-INPUT"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           PERFORM 900-COMPLETED-OK.
      *
       100-LOAD-PARA.
      *
           WRITE PRODUCT-MASTER-FILE-RECORD FROM
           PRODUCT-MASTER-INPUT-RECORD.
           DISPLAY "WRITE TO PRD-FILE".
      *
           READ INVCTL-INPUT
               AT END MOVE 'Y' TO WS-EOF-SW-IN.
      *
           IF WS-EOF-IN-NO THEN
               DISPLAY "NTH READ PRDCTL-INPUT-RECORD="
               PRODUCT-MASTER-INPUT-RECORD
           ELSE
               DISPLAY "EOF PRD-INPUT"
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