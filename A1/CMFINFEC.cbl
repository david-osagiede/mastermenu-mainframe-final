       IDENTIFICATION DIVISION.
       PROGRAM-ID. CMINFEC.
       AUTHOR. DAVID OSAGIEDE.
       DATE-WRITTEN. 1/13/2023.
      *PROGRAM DESCRIPTION:
      *Checks to see if the CM-FILE open successfully
      *or if it failed
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT CM-FILE ASSIGN TO OUTFILE
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS CM-KEY
           FILE STATUS IS WS-OUTFILE-SW.
      *
       DATA DIVISION.
       FILE SECTION.
       FD CM-FILE
           RECORD CONTAINS 118 CHARACTERS
           DATA RECORD IS CM-FILE-RECORD.
       01 CM-FILE-RECORD.
           05  CM-KEY.
               10  CM-CUSTOMER-NUMBER         PIC X(6).
           05  CM-FIRST-NAME                  PIC X(20).
           05  CM-LAST-NAME                   PIC X(30).
           05  CM-ADDRESS                     PIC X(30).
           05  CM-CITY                        PIC X(20).
           05  CM-STATE                       PIC X(2).
           05  CM-ZIP-CODE                    PIC X(10).
      *
       WORKING-STORAGE SECTION.
       01 WS-OUTFILE-SW                       PIC X(02)  VALUE SPACES.
           88  WS-OUTFILE-SUCCESS                 VALUE '00'.
           88  WS-OUTFILE-EOF                     VALUE '10'.
       01 WS-CM-REC                           PIC X(118).
       01 WS-EOF-SW                           PIC X(01)  VALUE 'N'.
           88  WS-EOF-NO                          VALUE 'N'.
           88  WS-EOF-YES                         VALUE 'Y'.
       01 WS-NBR                              PIC 9      VALUE 0.
      *
       PROCEDURE DIVISION.
       000-MAIN-PARA.
           INITIALIZE WS-OUTFILE-SW
                      WS-CM-REC
                      WS-EOF-SW.
           OPEN OUTPUT CM-FILE.
           DISPLAY "WS-OUTFILE-SW=" WS-OUTFILE-SW.
           IF WS-OUTFILE-SUCCESS
               DISPLAY "CM-FILE OPEN SUCCESSFUL"
               MOVE LOW-VALUES               TO   WS-CM-REC
               WRITE CM-FILE-RECORD          FROM WS-CM-REC
               CLOSE CM-FILE
               DISPLAY "PROGRAM COMPLETED OK"
           ELSE
               DISPLAY "CM-FILE OPEN ERROR"
               DISPLAY "PROGRAM FAILED - CHECK ERROR MESSAGES"
               COMPUTE WS-NBR = 1 / 0
           END-IF.
           STOP RUN.