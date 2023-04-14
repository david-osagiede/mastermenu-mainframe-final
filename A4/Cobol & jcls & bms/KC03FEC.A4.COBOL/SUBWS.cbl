      *
       01  UNEDITED-NUMBER.
           05  UNEDITED-CHAR       OCCURS 10   PIC X.
      *
       01  EDITED-NUMBER           PIC 9(07)V99.
      *
       01  VALID-NUMBER-SW         PIC X(01).
           88  VALID-NUMBER        VALUE 'Y'.
      *
      ******************************************************************
      * WORKING-STORAGE FOR INTEDIT
      ******************************************************************
      *
       01  INT-WORK-FIELDS.
      *
           05  INT-INTEGER-PART        PIC 9(05).
           05  INT-INTEGER-LENGTH      PIC S9(03)  COMP-3.
      *
       01  INT-UNEDITED-NUMBER         PIC X(05).
      *
       01  INT-EDITED-NUMBER           PIC 9(05).
      *
       01  INT-VALID-NUMBER-SW         PIC X(01).
           88  INT-VALID-NUMBER        VALUE 'Y'.
      *
      ******************************************************************
      * WORKING-STORAGE FOR NUMEDIT
      ******************************************************************
      *
       01  NUM-WORK-FIELDS.
      *
           05  NUM-INTEGER-PART        PIC 9(10).
           05  NUM-INTEGER-PART-X      REDEFINES   NUM-INTEGER-PART.
               10  NUM-INTEGER-CHAR    PIC X(01)   OCCURS 10.
           05  NUM-DECIMAL-PART        PIC V9(10).
           05  NUM-DECIMAL-PART-X      REDEFINES   NUM-DECIMAL-PART.
               10  NUM-DECIMAL-CHAR    PIC X(01)   OCCURS 10.
           05  NUM-DECIMAL-POS         PIC S9(03)  COMP-3.
           05  NUM-INTEGER-LENGTH      PIC S9(03)  COMP-3.
           05  NUM-INTEGER-SUB         PIC S9(03)  COMP-3.
           05  NUM-DECIMAL-SUB         PIC S9(03)  COMP-3.
           05  NUM-UNEDIT-SUB          PIC S9(03)  COMP-3.
      *
       01  NUM-UNEDITED-NUMBER.
           05  NUM-UNEDITED-CHAR       OCCURS 10   PIC X.
      *
       01  NUM-EDITED-NUMBER           PIC 9(07)V99.
      *
       01  NUM-VALID-NUMBER-SW         PIC X(01).
           88  NUM-VALID-NUMBER        VALUE 'Y'.
      *
      ******************************************************************
      * WORKING-STORAGE FOR SYSERR
      ******************************************************************
      *
       01  ERROR-MESSAGE.
      *
           05  ERROR-LINE-1.
               10  FILLER      PIC X(20)  VALUE 'A serious error has '.
               10  FILLER      PIC X(20)  VALUE 'occurred.  Please co'.
               10  FILLER      PIC X(20)  VALUE 'ntact technical supp'.
               10  FILLER      PIC X(19)  VALUE 'ort.               '.
           05  ERROR-LINE-2    PIC X(79)  VALUE SPACE.
           05  ERROR-LINE-3.
               10  FILLER      PIC X(11)  VALUE 'EIBRESP  = '.
               10  EM-RESP     PIC Z(08)9.
               10  FILLER      PIC X(59)  VALUE SPACE.
           05  ERROR-LINE-4.
               10  FILLER      PIC X(11)  VALUE 'EIBRESP2 = '.
               10  EM-RESP2    PIC Z(08)9.
               10  FILLER      PIC X(59)  VALUE SPACE.
           05  ERROR-LINE-5.
               10  FILLER      PIC X(11)  VALUE 'EIBTRNID = '.
               10  EM-TRNID    PIC X(04).
               10  FILLER      PIC X(64)  VALUE SPACE.
           05  ERROR-LINE-6.
               10  FILLER      PIC X(11)  VALUE 'EIBRSRCE = '.
               10  EM-RSRCE    PIC X(08).
               10  FILLER      PIC X(60)  VALUE SPACE.
           05  ERROR-LINE-7    PIC X(79)  VALUE SPACE.
