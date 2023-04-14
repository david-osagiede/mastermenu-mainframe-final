      ******************************************************************
      * PARAGRAPHS FOR INTEDIT
      ******************************************************************
      *
       9500-INTEDIT.
      *
           MOVE ZERO TO INT-INTEGER-LENGTH.

           INSPECT INT-UNEDITED-NUMBER
               REPLACING LEADING SPACE BY ZERO.

           INSPECT INT-UNEDITED-NUMBER
               TALLYING INT-INTEGER-LENGTH FOR CHARACTERS
                   BEFORE INITIAL SPACE.

           IF INT-UNEDITED-NUMBER(1:INT-INTEGER-LENGTH) NUMERIC
               MOVE INT-UNEDITED-NUMBER(1:INT-INTEGER-LENGTH)
                 TO INT-EDITED-NUMBER
               MOVE 'Y'
                 TO INT-VALID-NUMBER-SW
           ELSE
               MOVE 'N'
                 TO INT-VALID-NUMBER-SW
           END-IF.

      ******************************************************************
      * PARAGRAPHS FOR NUMEDIT
      ******************************************************************
      *
       9700-NUMEDIT.
      *
           MOVE 'Y'  TO NUM-VALID-NUMBER-SW.
           MOVE ZERO TO NUM-INTEGER-PART
                        NUM-DECIMAL-PART
                        NUM-DECIMAL-POS.

           INSPECT NUM-UNEDITED-NUMBER
               TALLYING NUM-DECIMAL-POS FOR CHARACTERS
                   BEFORE INITIAL '.'.

           IF NUM-DECIMAL-POS < 10
               PERFORM 9710-EDIT-DECIMAL-NUMBER
           ELSE
               PERFORM 9720-EDIT-INTEGER
           END-IF.

           IF NUM-VALID-NUMBER
               COMPUTE NUM-EDITED-NUMBER =
                       NUM-INTEGER-PART + NUM-DECIMAL-PART
           END-IF.
      *
       9710-EDIT-DECIMAL-NUMBER.
      *
           MOVE 10 TO NUM-INTEGER-SUB.

           PERFORM 9712-EDIT-INTEGER-PART
               VARYING NUM-UNEDIT-SUB FROM NUM-DECIMAL-POS BY -1
                 UNTIL NUM-UNEDIT-SUB < 1.

           MOVE 1 TO NUM-DECIMAL-SUB.
           ADD  2 TO NUM-DECIMAL-POS.

           PERFORM 9714-EDIT-DECIMAL-PART
               VARYING NUM-UNEDIT-SUB FROM NUM-DECIMAL-POS BY 1
                 UNTIL NUM-UNEDIT-SUB > 10.
      *
       9712-EDIT-INTEGER-PART.
      *
           IF NUM-UNEDITED-CHAR(NUM-UNEDIT-SUB) NUMERIC
               MOVE NUM-UNEDITED-CHAR(NUM-UNEDIT-SUB)
                 TO NUM-INTEGER-CHAR (NUM-INTEGER-SUB)
               SUBTRACT 1        FROM NUM-INTEGER-SUB
           ELSE IF NUM-UNEDITED-CHAR (NUM-UNEDIT-SUB) NOT = SPACE
               MOVE 'N' TO NUM-VALID-NUMBER-SW
           END-IF.
      *
       9714-EDIT-DECIMAL-PART.
      *
           IF NUM-UNEDITED-CHAR(NUM-UNEDIT-SUB) NUMERIC
               MOVE NUM-UNEDITED-CHAR(NUM-UNEDIT-SUB)
                 TO NUM-DECIMAL-CHAR (NUM-DECIMAL-SUB)
               ADD 1               TO NUM-DECIMAL-SUB
           ELSE IF NUM-UNEDITED-CHAR (NUM-UNEDIT-SUB) NOT = SPACE
               MOVE 'N' TO NUM-VALID-NUMBER-SW
           END-IF.
      *
       9720-EDIT-INTEGER.
      *
           INSPECT NUM-UNEDITED-NUMBER
               REPLACING LEADING SPACE BY ZERO.

           MOVE ZERO TO NUM-INTEGER-LENGTH.

           INSPECT NUM-UNEDITED-NUMBER
               TALLYING NUM-INTEGER-LENGTH FOR CHARACTERS
                   BEFORE INITIAL SPACE.

           MOVE 10 TO NUM-INTEGER-SUB.

           PERFORM 9712-EDIT-INTEGER-PART
               VARYING NUM-UNEDIT-SUB FROM NUM-INTEGER-LENGTH BY -1
                 UNTIL NUM-UNEDIT-SUB < 1.

           MOVE ZERO TO NUM-DECIMAL-PART.

      ******************************************************************
      * PARAGRAPHS FOR SYSERR
      ******************************************************************
      * Note: You can delete this paragraph if it is causing a problem,
      *       or is not used anywhere in the ORDER program
      ******************************************************************
      *
       9900-SYSERR.
      *
           MOVE DFHCOMMAREA TO ERROR-PARAMETERS.
           MOVE ERR-RESP  TO EM-RESP.
           MOVE ERR-RESP2 TO EM-RESP2.
           MOVE ERR-TRNID TO EM-TRNID.
           MOVE ERR-RSRCE TO EM-RSRCE.
           EXEC CICS
               SEND TEXT FROM(ERROR-MESSAGE)
                         ERASE
                         ALARM
                         FREEKB
           END-EXEC.
           EXEC CICS
               RETURN
           END-EXEC.
     *