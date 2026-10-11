      *================================================================*
      * BOMLOAD  - PARAGRAFI ZA UČITAVANJE SASTAVNICA U TABELU
      * SISTEM   : AMPBOM - PRODUKTRON
      *----------------------------------------------------------------*
      * PREDUSLOV: U PROGRAMU MORA POSTOJATI FD BOM-FILE SA COPY
      *            BOMREC I WORKING-STORAGE SA COPY BOMTAB.
      * KORIŠTENJE: PERFORM 7000-LOAD-BOM-TABLE
      *================================================================*
       7000-LOAD-BOM-TABLE.
           MOVE ZERO TO WS-BOM-COUNT
           MOVE 'N'  TO WS-BOM-EOF-SW
           OPEN INPUT BOM-FILE
           IF NOT FS-BOM-OK
               DISPLAY '*** GRESKA OTVARANJA BOMFILE, STATUS '
                       WS-FS-BOM
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 7010-READ-BOM
           PERFORM UNTIL BOM-EOF
      *        ZAŠTITA OD PREKORAČENJA TABELE
               IF WS-BOM-COUNT >= WS-BOM-MAX
                   DISPLAY '*** TABELA SASTAVNICA PUNA, MAX '
                           WS-BOM-MAX
                   MOVE 12 TO RETURN-CODE
                   STOP RUN
               END-IF
               ADD 1 TO WS-BOM-COUNT
               MOVE BOM-PARENT    TO WT-PARENT    (WS-BOM-COUNT)
               MOVE BOM-COMPONENT TO WT-COMPONENT (WS-BOM-COUNT)
               MOVE BOM-QTY-PER   TO WT-QTY-PER   (WS-BOM-COUNT)
               PERFORM 7010-READ-BOM
           END-PERFORM
           CLOSE BOM-FILE.

       7010-READ-BOM.
           READ BOM-FILE
               AT END SET BOM-EOF TO TRUE
           END-READ.
