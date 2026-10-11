      *================================================================*
      * STKOPS   - OPERACIJE NAD STEKOM (PUSH / POP)
      * SISTEM   : AMPBOM - PRODUKTRON
      * PREDUSLOV: COPY STKTAB U WORKING-STORAGE.
      *================================================================*
       7100-PUSH.
      *    PREVIŠE NIVOA ZNAČI GREŠKU U PODACIMA (PETLJA U SASTAVNICI)
           IF WS-SP >= WS-STK-MAX
               DISPLAY '*** STEK PREPUNJEN - PROVJERITI SASTAVNICE'
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           ADD 1 TO WS-SP
           MOVE WS-PUSH-PART    TO STK-PART    (WS-SP)
           MOVE WS-PUSH-QTY     TO STK-QTY     (WS-SP)
           MOVE WS-PUSH-QTY-PER TO STK-QTY-PER (WS-SP)
           MOVE WS-PUSH-LEVEL   TO STK-LEVEL   (WS-SP).

       7200-POP.
           MOVE STK-PART    (WS-SP) TO WS-CUR-PART
           MOVE STK-QTY     (WS-SP) TO WS-CUR-QTY
           MOVE STK-QTY-PER (WS-SP) TO WS-CUR-QTY-PER
           MOVE STK-LEVEL   (WS-SP) TO WS-CUR-LEVEL
           SUBTRACT 1 FROM WS-SP.
