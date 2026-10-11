      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    AMPEXPL.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  PRODUKTRON - PROIZVODNJA AUDIO OPREME.
       DATE-WRITTEN.  2000-03-13.
      *================================================================*
      * SISTEM  : AMPBOM - SASTAVNICE I PLANIRANJE MATERIJALA
      * PROGRAM : AMPEXPL - KORAK 020 NOĆNE OBRADE
      * OPIS    : ZA SVAKI PROIZVODNI NALOG RAZVIJA VIŠENIVOVSKU
      *           SASTAVNICU (BOM EXPLOSION) I ZA SVAKU KOMPONENTU
      *           NA SVAKOM NIVOU PIŠE SLOG BRUTO POTREBE.
      *           PRIMJER: 25 POJAČALA AMP-200T -> 25 PLOČA
      *           PREDPOJAČALA -> 150 POTENCIOMETARA POT-10K ITD.
      * ULAZ    : ORDFILE  - PROIZVODNI NALOZI (ORDREC)
      *           BOMFILE  - SASTAVNICE (BOMREC)
      *           PARTMAST - MATIČNI DIJELOVI (SLUČAJNI PRISTUP)
      * IZLAZ   : REQFILE  - BRUTO POTREBE (REQREC)
      * POVRATNI KODOVI (RETURN-CODE):
      *            0 - OBRADA USPJEŠNA
      *            4 - UPOZORENJA (ODBAČENI NALOZI, NEPOZNATE KOMP.)
      *           12 - FATALNA GREŠKA (DATOTEKE, PETLJA U SASTAVNICI)
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2000-06-09 BJ     PRVA VERZIJA
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ORDER-FILE   ASSIGN TO ORDFILE
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-ORD.
           SELECT BOM-FILE     ASSIGN TO BOMFILE
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-BOM.
           SELECT PART-MASTER  ASSIGN TO PARTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS PART-NO
                  FILE STATUS  IS WS-FS-PARTMAST.
           SELECT REQ-FILE     ASSIGN TO REQFILE
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-REQ.

       DATA DIVISION.
       FILE SECTION.
       FD  ORDER-FILE.
           COPY ORDREC.
       FD  BOM-FILE.
           COPY BOMREC.
       FD  PART-MASTER.
           COPY PARTREC.
       FD  REQ-FILE.
           COPY REQREC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-ORD               PIC X(02).
               88  FS-ORD-OK           VALUE '00'.
           05  WS-FS-PARTMAST          PIC X(02).
               88  FS-PARTMAST-OK      VALUE '00'.
           05  WS-FS-REQ               PIC X(02).
               88  FS-REQ-OK           VALUE '00'.

           COPY BOMTAB.
           COPY STKTAB.

       01  WS-SWITCHES.
           05  WS-ORD-EOF-SW           PIC X(01)   VALUE 'N'.
               88  ORD-EOF             VALUE 'Y'.
           05  WS-PART-FOUND-SW        PIC X(01)   VALUE 'N'.
               88  PART-FOUND          VALUE 'Y'.
           05  WS-CHILD-FOUND-SW       PIC X(01)   VALUE 'N'.
               88  CHILD-FOUND         VALUE 'Y'.

       01  WS-CONSTANTS.
      *    MAKSIMALNA DUBINA SASTAVNICE - VIŠE ZNAČI PETLJU U PODACIMA
           05  WS-MAX-LEVEL            PIC 9(02)   VALUE 10.

       01  WS-WORK-FIELDS.
           05  WS-CHILD-QTY            PIC 9(09)V999.
           05  WS-CHILD-LEVEL          PIC 9(02).
           05  WS-RC                   PIC 9(02)   VALUE ZERO.

       01  WS-COUNTERS.
           05  WS-CNT-ORD-READ         PIC 9(07)   COMP-3 VALUE ZERO.
           05  WS-CNT-ORD-OK           PIC 9(07)   COMP-3 VALUE ZERO.
           05  WS-CNT-ORD-REJ          PIC 9(07)   COMP-3 VALUE ZERO.
           05  WS-CNT-REQ              PIC 9(07)   COMP-3 VALUE ZERO.

       01  WS-DISP-CNT                 PIC Z,ZZZ,ZZ9.
       01  WS-DISP-RC                  PIC Z9.

       PROCEDURE DIVISION.
      *================================================================*
      * GLAVNA PETLJA - JEDAN PROLAZ PO NALOGU
      *================================================================*
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-ORDER
               UNTIL ORD-EOF
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
      * OTVARANJE DATOTEKA, UČITAVANJE SASTAVNICA U MEMORIJU
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           DISPLAY 'AMPEXPL  - START RAZVOJA SASTAVNICA'
           PERFORM 7000-LOAD-BOM-TABLE
           MOVE WS-BOM-COUNT TO WS-DISP-CNT
           DISPLAY 'AMPEXPL  - UCITANO VEZA SASTAVNICE: ' WS-DISP-CNT
           OPEN INPUT PART-MASTER
           IF NOT FS-PARTMAST-OK
               DISPLAY 'AMPEXPL  - GRESKA OTVARANJA PARTMAST, STATUS '
                       WS-FS-PARTMAST
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN INPUT ORDER-FILE
           IF NOT FS-ORD-OK
               DISPLAY 'AMPEXPL  - GRESKA OTVARANJA ORDFILE, STATUS '
                       WS-FS-ORD
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT REQ-FILE
           IF NOT FS-REQ-OK
               DISPLAY 'AMPEXPL  - GRESKA OTVARANJA REQFILE, STATUS '
                       WS-FS-REQ
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 8000-READ-ORDER.

      *----------------------------------------------------------------*
      * PROVJERA NALOGA - SAMO GOTOVI PROIZVODI I PODSKLOPOVI SE MOGU
      * PROIZVODITI, KUPOVNI DIJELOVI NE
      *----------------------------------------------------------------*
       2000-PROCESS-ORDER.
           MOVE ORD-PART-NO TO PART-NO
           PERFORM 8100-READ-PART
           EVALUATE TRUE
               WHEN NOT PART-FOUND
                   DISPLAY 'AMPEXPL  - NALOG ' ORD-NO
                           ' ODBACEN: NEPOZNAT PROIZVOD ' ORD-PART-NO
                   PERFORM 2900-REJECT-ORDER
               WHEN PART-PURCHASED
                   DISPLAY 'AMPEXPL  - NALOG ' ORD-NO
                           ' ODBACEN: KUPOVNI DIO ' ORD-PART-NO
                           ' SE NE PROIZVODI'
                   PERFORM 2900-REJECT-ORDER
               WHEN ORD-QTY NOT NUMERIC
               WHEN ORD-QTY = ZERO
                   DISPLAY 'AMPEXPL  - NALOG ' ORD-NO
                           ' ODBACEN: NEISPRAVNA KOLICINA'
                   PERFORM 2900-REJECT-ORDER
               WHEN OTHER
                   PERFORM 3000-EXPLODE-ORDER
           END-EVALUATE
           PERFORM 8000-READ-ORDER.

       2900-REJECT-ORDER.
           ADD 1 TO WS-CNT-ORD-REJ
           IF WS-RC < 4
               MOVE 4 TO WS-RC
           END-IF.

      *----------------------------------------------------------------*
      * RAZVOJ JEDNOG NALOGA: KORIJEN NA STEK, PA OBRADA DOK STEK
      * NE OSTANE PRAZAN
      *----------------------------------------------------------------*
       3000-EXPLODE-ORDER.
           ADD 1 TO WS-CNT-ORD-OK
           MOVE ORD-PART-NO TO WS-PUSH-PART
           MOVE ORD-QTY     TO WS-PUSH-QTY
           MOVE 1           TO WS-PUSH-QTY-PER
           MOVE ZERO        TO WS-PUSH-LEVEL
           PERFORM 7100-PUSH
           PERFORM 3100-EXPAND-NODE
               UNTIL WS-SP = ZERO.

      *----------------------------------------------------------------*
      * SKIDA ČVOR SA STEKA I TRAŽI SVE NJEGOVE KOMPONENTE U TABELI
      *----------------------------------------------------------------*
       3100-EXPAND-NODE.
           PERFORM 7200-POP
           MOVE 'N' TO WS-CHILD-FOUND-SW
           PERFORM VARYING BX FROM 1 BY 1
                   UNTIL BX > WS-BOM-COUNT
               IF WT-PARENT (BX) = WS-CUR-PART
                   SET CHILD-FOUND TO TRUE
                   PERFORM 3200-PROCESS-COMPONENT
               END-IF
           END-PERFORM
      *    PROIZVOD ILI PODSKLOP BEZ SASTAVNICE JE GREŠKA U PODACIMA
           IF NOT CHILD-FOUND
               DISPLAY 'AMPEXPL  - UPOZORENJE: DIO ' WS-CUR-PART
                       ' NEMA SASTAVNICU (NALOG ' ORD-NO ')'
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * JEDNA KOMPONENTA: IZRAČUNAJ KOLIČINU, ZAPIŠI POTREBU, A AKO
      * KOMPONENTA IMA SVOJU SASTAVNICU STAVI JE NA STEK
      *----------------------------------------------------------------*
       3200-PROCESS-COMPONENT.
           COMPUTE WS-CHILD-QTY ROUNDED =
                   WS-CUR-QTY * WT-QTY-PER (BX)
           COMPUTE WS-CHILD-LEVEL = WS-CUR-LEVEL + 1
           MOVE WT-COMPONENT (BX) TO PART-NO
           PERFORM 8100-READ-PART
           IF NOT PART-FOUND
               DISPLAY 'AMPEXPL  - UPOZORENJE: NEPOZNATA KOMPONENTA '
                       WT-COMPONENT (BX) ' U SASTAVNICI ' WS-CUR-PART
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
           ELSE
               PERFORM 3300-WRITE-REQUIREMENT
               IF NOT PART-PURCHASED
                   IF WS-CHILD-LEVEL >= WS-MAX-LEVEL
                       DISPLAY 'AMPEXPL  - *** PREKORACEN MAX NIVO '
                               WS-MAX-LEVEL ' KOD DIJELA ' PART-NO
                       DISPLAY 'AMPEXPL  - *** MOGUCA PETLJA U '
                               'SASTAVNICI - OBRADA PREKINUTA'
                       PERFORM 9100-CLOSE-FILES
                       MOVE 12 TO RETURN-CODE
                       STOP RUN
                   END-IF
                   MOVE PART-NO        TO WS-PUSH-PART
                   MOVE WS-CHILD-QTY   TO WS-PUSH-QTY
                   MOVE WT-QTY-PER(BX) TO WS-PUSH-QTY-PER
                   MOVE WS-CHILD-LEVEL TO WS-PUSH-LEVEL
                   PERFORM 7100-PUSH
               END-IF
           END-IF.

       3300-WRITE-REQUIREMENT.
           MOVE PART-NO        TO REQ-PART-NO
           MOVE WS-CHILD-QTY   TO REQ-QTY
           MOVE ORD-NO         TO REQ-ORD-NO
           MOVE WS-CUR-PART    TO REQ-PARENT
           MOVE WS-CHILD-LEVEL TO REQ-LEVEL
           MOVE ORD-DUE-DATE   TO REQ-DUE-DATE
           WRITE REQ-RECORD
           ADD 1 TO WS-CNT-REQ.

      *----------------------------------------------------------------*
      * POMOĆNI PARAGRAFI ZA ČITANJE
      *----------------------------------------------------------------*
       8000-READ-ORDER.
           READ ORDER-FILE
               AT END
                   SET ORD-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-CNT-ORD-READ
           END-READ.

       8100-READ-PART.
           READ PART-MASTER
               INVALID KEY
                   MOVE 'N' TO WS-PART-FOUND-SW
               NOT INVALID KEY
                   MOVE 'Y' TO WS-PART-FOUND-SW
           END-READ.

      *----------------------------------------------------------------*
      * ZAJEDNIČKI PARAGRAFI (BOM TABELA I STEK)
      *----------------------------------------------------------------*
           COPY BOMLOAD.
           COPY STKOPS.

      *----------------------------------------------------------------*
      * KRAJ OBRADE I STATISTIKA
      *----------------------------------------------------------------*
       9000-TERMINATE.
           PERFORM 9100-CLOSE-FILES
           MOVE WS-CNT-ORD-READ TO WS-DISP-CNT
           DISPLAY 'AMPEXPL  - PROCITANO NALOGA   : ' WS-DISP-CNT
           MOVE WS-CNT-ORD-OK   TO WS-DISP-CNT
           DISPLAY 'AMPEXPL  - RAZVIJENO NALOGA   : ' WS-DISP-CNT
           MOVE WS-CNT-ORD-REJ  TO WS-DISP-CNT
           DISPLAY 'AMPEXPL  - ODBACENO NALOGA    : ' WS-DISP-CNT
           MOVE WS-CNT-REQ      TO WS-DISP-CNT
           DISPLAY 'AMPEXPL  - ZAPISANO POTREBA   : ' WS-DISP-CNT
           MOVE WS-RC TO RETURN-CODE
           MOVE WS-RC TO WS-DISP-RC
           DISPLAY 'AMPEXPL  - KRAJ, RC=' WS-DISP-RC.

       9100-CLOSE-FILES.
           CLOSE ORDER-FILE
                 PART-MASTER
                 REQ-FILE.
