       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVLOFEC.
       AUTHOR. David Osagiede.
       DATE-WRITTEN. 3/17/2023.
      *PROGRAM DESCRIPTION:
      *Checks to see if the INV-FILE open successfully, if
      *it already had data loaded previously or if it failed
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT INVOICE-INPUT ASSIGN TO INVDATA
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-INFILE-SW.

           SELECT INVOICE-FILE ASSIGN TO OUTFILE
               RECORD KEY IS INV-KEY-1
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-OUTFILE-SW.
      *
       DATA DIVISION.
       FILE SECTION.
       FD INVOICE-INPUT
           RECORDING MODE IS F
           RECORD CONTAINS 389 CHARACTERS
           DATA RECORD IS INVOICE-INPUT-RECORD.
       01  INVOICE-INPUT-RECORD.
      *
           05  INV-KEY.
               10  INV-INVOICE-NUMBER          PIC 9(06).
           05  INV-INVOICE-DATE                PIC X(08).
           05  INV-CUSTOMER-NUMBER             PIC X(06).
           05  INV-PO-NUMBER                   PIC X(10).
           05  INV-LINE-ITEM                   OCCURS 10 TIMES.
               10  INV-PRODUCT-CODE            PIC X(10).
               10  INV-QUANTITY                PIC S9(07).
               10  INV-UNIT-PRICE              PIC S9(07)V99.
               10  INV-AMOUNT                  PIC S9(07)V99.
           05  INV-INVOICE-TOTAL               PIC S9(07)V99.
      *
       FD INVOICE-FILE
           RECORD CONTAINS 389 CHARACTERS
           DATA RECORD IS INVOICE-FILE-RECORD.
       01  INVOICE-FILE-RECORD.
      *
           05  INV-KEY-1.
               10  INV-INVOICE-NUMBER-1          PIC 9(06).
           05  INV-INVOICE-DATE-1                PIC X(08).
           05  INV-CUSTOMER-NUMBER-1             PIC X(06).
           05  INV-PO-NUMBER-1                   PIC X(10).
           05  INV-LINE-ITEM-1                   OCCURS 10 TIMES.
               10  INV-PRODUCT-CODE-1            PIC X(10).
               10  INV-QUANTITY-1                PIC S9(07).
               10  INV-UNIT-PRICE-1              PIC S9(07)V99.
               10  INV-AMOUNT-1                  PIC S9(07)V99.
           05  INV-INVOICE-TOTAL-1               PIC S9(07)V99.
      *
       WORKING-STORAGE SECTION.
       01 WS-INFILE-SW                       PIC X(02)  VALUE SPACES.
           88  WS-INFILE-SUCCESS                 VALUE '00'.
           88  WS-INFILE-EOF                     VALUE '10'.
       01 WS-OUTFILE-SW                      PIC X(02)  VALUE SPACES.
           88  WS-OUTFILE-SUCCESS                VALUE '00'.
           88  WS-OUTFILE-IOERROR                VALUE '37'.
           88  WS-OUTFILE-EOF                    VALUE '10'.
       01 WS-CM-REC                          PIC X(389).
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
           OPEN INPUT INVOICE-INPUT.
           IF WS-INFILE-SUCCESS
               DISPLAY "INVOICE-INPUT OPEN SUCCESSFUL"
           ELSE
               DISPLAY "WS-INFILE-SW=" WS-INFILE-SW
               DISPLAY "INVOICE-INPUT OPEN ERROR"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           OPEN OUTPUT INVOICE-FILE.
           IF WS-OUTFILE-SUCCESS
               DISPLAY "INVOICE-FILE OPEN SUCCESSFUL"
           ELSE IF WS-OUTFILE-IOERROR
               DISPLAY "WS-OUTFILE-SW=" WS-OUTFILE-SW
               DISPLAY "INVOICE-FILE IOERROR - OPEN OUTPUT SHOULD BE "
                       "OPEN INPUT OR I-O OR EXTEND  "
               DISPLAY "- DATA MAY ALREADY HAVE BEEN LOADED PREVIOUSLY"
               DISPLAY "- DELETE AND INITIALIZE FILE TO RELOAD DATA"
               PERFORM 800-PROGRAM-FAILED
           ELSE
               DISPLAY "WS-OUTFILE-SW=" WS-OUTFILE-SW
               DISPLAY "INVOICE-FILE OPENING ERROR"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           READ INVOICE-INPUT
               AT END MOVE 'Y' TO WS-EOF-SW-IN.
      *
           IF WS-EOF-IN-NO THEN
               DISPLAY "1ST READ INVOICE-INPUT-RECORD="
               INVOICE-INPUT-RECORD
      *
               PERFORM 100-LOAD-PARA
                   UNTIL WS-EOF-IN-YES
           ELSE
               DISPLAY "NO DATA IN INVOICE-INPUT"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           PERFORM 900-COMPLETED-OK.
      *
       100-LOAD-PARA.
      *
           WRITE INVOICE-FILE-RECORD FROM INVOICE-INPUT-RECORD.
           DISPLAY "WRITE TO INVOICE-FILE".
      *
           READ INVOICE-INPUT
               AT END MOVE 'Y' TO WS-EOF-SW-IN.
      *
           IF WS-EOF-IN-NO THEN
               DISPLAY "NTH READ INVOICE-INPUT-RECORD="
               INVOICE-INPUT-RECORD
           ELSE
               DISPLAY "EOF INVOICE-INPUT"
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