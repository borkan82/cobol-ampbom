      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    AMPCOST.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  PRODUKTRON - PROIZVODNJA AUDIO OPREME.
       DATE-WRITTEN.  2000-03-07.
      *================================================================*
      * SISTEM  : AMPBOM - SASTAVNICE I PLANIRANJE MATERIJALA
      * PROGRAM : AMPCOST - KORAK 040 NOĆNE OBRADE
      * OPIS    : ZA SVAKI GOTOV PROIZVOD (TIP F) ŠTAMPA STRUKTURNU
      *           SASTAVNICU (INDENTED BOM) I RAČUNA MATERIJALNI
      *           TROŠAK JEDNOG KOMADA PO STANDARDNIM CIJENAMA
      *           KUPOVNIH DIJELOVA (COST ROLL-UP).
      *           NA KRAJU ŠTAMPA ZBIRNU STRANU SA CIJENAMA SVIH
      *           PROIZVODA.
      * ULAZ    : PARTMAST - MATIČNI DIJELOVI (DINAMIČKI PRISTUP)
      *           BOMFILE  - SASTAVNICE
      * IZLAZ   : COSTRPT  - IZVJEŠTAJ, 132 KOLONE
      * POVRATNI KODOVI (RETURN-CODE):
      *            0 - OBRADA USPJEŠNA
      *            4 - NEPOZNATA KOMPONENTA U SASTAVNICI
      *           12 - FATALNA GREŠKA
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2000-04-12 BJ     PRVA VERZIJA
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PART-MASTER  ASSIGN TO PARTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS PART-NO
                  FILE STATUS  IS WS-FS-PARTMAST.
           SELECT BOM-FILE     ASSIGN TO BOMFILE
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-BOM.
           SELECT COST-REPORT  ASSIGN TO COSTRPT
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-RPT.

       DATA DIVISION.
       FILE SECTION.
       FD  PART-MASTER.
           COPY PARTREC.
       FD  BOM-FILE.
           COPY BOMREC.
       FD  COST-REPORT.
       01  RPT-LINE                    PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-PARTMAST          PIC X(02).
               88  FS-PARTMAST-OK      VALUE '00'.
           05  WS-FS-RPT               PIC X(02).
               88  FS-RPT-OK           VALUE '00'.

           COPY BOMTAB.
           COPY STKTAB.

       01  WS-SWITCHES.
           05  WS-PM-EOF-SW            PIC X(01)   VALUE 'N'.
               88  PM-EOF              VALUE 'Y'.
           05  WS-PART-FOUND-SW        PIC X(01)   VALUE 'N'.
               88  PART-FOUND          VALUE 'Y'.

      *    LISTA GOTOVIH PROIZVODA - PUNI SE U PRVOM PROLAZU KROZ
      *    PARTMAST, DA SEKVENCIJALNO ČITANJE NE SMETA SLUČAJNOM
       01  WS-PRODUCT-TABLE.
           05  WS-PROD-COUNT           PIC 9(03)   COMP VALUE ZERO.
           05  WS-PROD-MAX             PIC 9(03)   COMP VALUE 100.
           05  WS-PROD-ENTRY           OCCURS 100 TIMES.
               10  PT-PART             PIC X(10).
               10  PT-DESC             PIC X(30).
               10  PT-COST             PIC 9(09)V99.
       01  WS-PX                       PIC 9(03)   COMP.
       01  WS-IX                       PIC S9(04)  COMP.

       01  WS-WORK-FIELDS.
           05  WS-EXT-COST             PIC 9(09)V99.
           05  WS-PROD-TOTAL           PIC 9(09)V99.
           05  WS-LVL-TXT              PIC X(06).
           05  WS-LVL-DIGIT            PIC 9(01).
           05  WS-LVL-POS              PIC 9(02).
           05  WS-RC                   PIC 9(02)   VALUE ZERO.

       01  WS-DISP-RC                  PIC Z9.

      *----------------------------------------------------------------*
      * LINIJE IZVJEŠTAJA
      *----------------------------------------------------------------*
       01  WS-HDR1.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(40)
                   VALUE 'PRODUKTRON - PROIZVODNJA AUDIO OPREME'.
           05  FILLER                  PIC X(76)
                   VALUE 'AMPBOM - MATERIJALNI TROSAK PROIZVODA'.
           05  FILLER                  PIC X(07)   VALUE 'STRANA '.
           05  HDR1-PAGE               PIC ZZZ9.
           05  FILLER                  PIC X(04)   VALUE SPACES.
       01  WS-HDR2.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(10)   VALUE 'PROIZVOD: '.
           05  HDR2-PART               PIC X(10).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  HDR2-DESC               PIC X(30).
           05  FILLER                  PIC X(80)   VALUE SPACES.
       01  WS-HDR3.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(06)  VALUE 'NIVO'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE 'BROJ DIJ.'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(30)  VALUE 'NAZIV'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(01)  VALUE 'T'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(03)  VALUE 'JM'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE '  KOL/SKLP'.
           05  FILLER  PIC X(10)  VALUE '  KOL/PROI'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(12)  VALUE '  STD.CIJENA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(12)  VALUE '       IZNOS'.
           05  FILLER  PIC X(30)  VALUE SPACES.
       01  WS-HDR4.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(101)  VALUE ALL '-'.
           05  FILLER                  PIC X(30)   VALUE SPACES.

       01  WS-DETAIL.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-LEVEL                PIC X(06).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-PART                 PIC X(10).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-DESC                 PIC X(30).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-TYPE                 PIC X(01).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-UOM                  PIC X(03).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-QTY-PER              PIC ZZ,ZZ9.999.
           05  DL-QTY-EXT              PIC ZZ,ZZ9.999.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-UNIT-COST            PIC Z,ZZZ,ZZ9.99
                                       BLANK WHEN ZERO.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-EXT-COST             PIC Z,ZZZ,ZZ9.99
                                       BLANK WHEN ZERO.
           05  FILLER                  PIC X(30)   VALUE SPACES.

       01  WS-PROD-TOTAL-LINE.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(40)
                   VALUE 'MATERIJALNI TROSAK JEDNOG KOMADA (KM):'.
           05  FILLER                  PIC X(47)   VALUE SPACES.
           05  PTL-COST                PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(30)   VALUE SPACES.

       01  WS-SUM-HDR.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(60)
               VALUE 'ZBIRNI PREGLED - MATERIJALNI TROSAK PO PROIZVODU'.
           05  FILLER                  PIC X(71)   VALUE SPACES.
       01  WS-SUM-LINE.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  SL-PART                 PIC X(10).
           05  FILLER                  PIC X(02)   VALUE SPACES.
           05  SL-DESC                 PIC X(30).
           05  FILLER                  PIC X(02)   VALUE SPACES.
           05  SL-COST                 PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(04)   VALUE ' KM'.
           05  FILLER                  PIC X(69)   VALUE SPACES.

       01  WS-PAGE-NO                  PIC 9(04)   VALUE ZERO.

       PROCEDURE DIVISION.
      *================================================================*
      * GLAVNA PETLJA
      *================================================================*
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-COLLECT-PRODUCTS
           PERFORM 3000-COST-PRODUCT
               VARYING WS-PX FROM 1 BY 1
               UNTIL WS-PX > WS-PROD-COUNT
           PERFORM 5000-PRINT-SUMMARY
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           DISPLAY 'AMPCOST  - START OBRACUNA TROSKA MATERIJALA'
           PERFORM 7000-LOAD-BOM-TABLE
           OPEN INPUT PART-MASTER
           IF NOT FS-PARTMAST-OK
               DISPLAY 'AMPCOST  - GRESKA OTVARANJA PARTMAST, STATUS '
                       WS-FS-PARTMAST
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT COST-REPORT
           IF NOT FS-RPT-OK
               DISPLAY 'AMPCOST  - GRESKA OTVARANJA COSTRPT, STATUS '
                       WS-FS-RPT
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF.

      *----------------------------------------------------------------*
      * PRVI PROLAZ: SEKVENCIJALNO KROZ PARTMAST, SKUPI PROIZVODE
      *----------------------------------------------------------------*
       2000-COLLECT-PRODUCTS.
           PERFORM 2100-READ-NEXT-PART
           PERFORM UNTIL PM-EOF
               IF PART-FINISHED
                   IF WS-PROD-COUNT >= WS-PROD-MAX
                       DISPLAY 'AMPCOST  - TABELA PROIZVODA PUNA'
                       MOVE 12 TO RETURN-CODE
                       STOP RUN
                   END-IF
                   ADD 1 TO WS-PROD-COUNT
                   MOVE PART-NO   TO PT-PART (WS-PROD-COUNT)
                   MOVE PART-DESC TO PT-DESC (WS-PROD-COUNT)
                   MOVE ZERO      TO PT-COST (WS-PROD-COUNT)
               END-IF
               PERFORM 2100-READ-NEXT-PART
           END-PERFORM
           DISPLAY 'AMPCOST  - PRONADJENO PROIZVODA: ' WS-PROD-COUNT.

       2100-READ-NEXT-PART.
           READ PART-MASTER NEXT RECORD
               AT END SET PM-EOF TO TRUE
           END-READ.

      *----------------------------------------------------------------*
      * DRUGI PROLAZ: ZA SVAKI PROIZVOD RAZVOJ SASTAVNICE U DUBINU,
      * DJECA SE STAVLJAJU NA STEK OBRNUTIM REDOM DA ŠTAMPA PRATI
      * REDOSLIJED IZ DATOTEKE SASTAVNICA
      *----------------------------------------------------------------*
       3000-COST-PRODUCT.
           MOVE ZERO TO WS-PROD-TOTAL
           PERFORM 3900-PRINT-PRODUCT-HEADER
           MOVE PT-PART (WS-PX) TO WS-PUSH-PART
           MOVE 1               TO WS-PUSH-QTY
           MOVE 1               TO WS-PUSH-QTY-PER
           MOVE ZERO            TO WS-PUSH-LEVEL
           PERFORM 7100-PUSH
           PERFORM 3100-PROCESS-NODE
               UNTIL WS-SP = ZERO
           MOVE WS-PROD-TOTAL TO PT-COST (WS-PX)
           MOVE WS-PROD-TOTAL TO PTL-COST
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-PROD-TOTAL-LINE
                 AFTER ADVANCING 1 LINE.

       3100-PROCESS-NODE.
           PERFORM 7200-POP
           MOVE WS-CUR-PART TO PART-NO
           READ PART-MASTER KEY IS PART-NO
               INVALID KEY
                   MOVE 'N' TO WS-PART-FOUND-SW
               NOT INVALID KEY
                   MOVE 'Y' TO WS-PART-FOUND-SW
           END-READ
           IF NOT PART-FOUND
               DISPLAY 'AMPCOST  - UPOZORENJE: NEPOZNAT DIO '
                       WS-CUR-PART
               MOVE 4 TO WS-RC
           ELSE
               MOVE ZERO TO WS-EXT-COST
               IF PART-PURCHASED
                   COMPUTE WS-EXT-COST ROUNDED =
                           WS-CUR-QTY * PART-STD-COST
                   ADD WS-EXT-COST TO WS-PROD-TOTAL
               ELSE
                   PERFORM 3200-PUSH-CHILDREN
               END-IF
               PERFORM 3300-PRINT-NODE
           END-IF.

      *    OBRNUTI REDOSLIJED: ZADNJE DIJETE NA DNO STEKA
       3200-PUSH-CHILDREN.
           PERFORM VARYING WS-IX FROM WS-BOM-COUNT BY -1
                   UNTIL WS-IX < 1
               IF WT-PARENT (WS-IX) = WS-CUR-PART
                   MOVE WT-COMPONENT (WS-IX) TO WS-PUSH-PART
                   COMPUTE WS-PUSH-QTY ROUNDED =
                           WS-CUR-QTY * WT-QTY-PER (WS-IX)
                   MOVE WT-QTY-PER (WS-IX)   TO WS-PUSH-QTY-PER
                   COMPUTE WS-PUSH-LEVEL = WS-CUR-LEVEL + 1
                   IF WS-PUSH-LEVEL > 5
                       DISPLAY 'AMPCOST  - *** PREDUBOKA SASTAVNICA '
                               '- MOGUCA PETLJA KOD ' WS-CUR-PART
                       MOVE 12 TO RETURN-CODE
                       STOP RUN
                   END-IF
                   PERFORM 7100-PUSH
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      * ŠTAMPA ČVORA: NIVO SE PRIKAZUJE TAČKAMA (.1, ..2, ...3)
      *----------------------------------------------------------------*
       3300-PRINT-NODE.
           MOVE SPACES TO WS-LVL-TXT
           IF WS-CUR-LEVEL > ZERO
               MOVE ALL '.' TO WS-LVL-TXT (1:WS-CUR-LEVEL)
           END-IF
           MOVE WS-CUR-LEVEL TO WS-LVL-DIGIT
           COMPUTE WS-LVL-POS = WS-CUR-LEVEL + 1
           MOVE WS-LVL-DIGIT TO WS-LVL-TXT (WS-LVL-POS:1)
           MOVE WS-LVL-TXT     TO DL-LEVEL
           MOVE PART-NO        TO DL-PART
           MOVE PART-DESC      TO DL-DESC
           MOVE PART-TYPE      TO DL-TYPE
           MOVE PART-UOM       TO DL-UOM
           MOVE WS-CUR-QTY-PER TO DL-QTY-PER
           MOVE WS-CUR-QTY     TO DL-QTY-EXT
           IF PART-PURCHASED
               MOVE PART-STD-COST TO DL-UNIT-COST
           ELSE
               MOVE ZERO          TO DL-UNIT-COST
           END-IF
           MOVE WS-EXT-COST    TO DL-EXT-COST
           WRITE RPT-LINE FROM WS-DETAIL AFTER ADVANCING 1 LINE.

      *    SVAKI PROIZVOD POČINJE NA NOVOJ STRANI
       3900-PRINT-PRODUCT-HEADER.
           ADD 1 TO WS-PAGE-NO
           MOVE WS-PAGE-NO       TO HDR1-PAGE
           MOVE PT-PART (WS-PX)  TO HDR2-PART
           MOVE PT-DESC (WS-PX)  TO HDR2-DESC
           WRITE RPT-LINE FROM WS-HDR1 AFTER ADVANCING PAGE
           WRITE RPT-LINE FROM WS-HDR2 AFTER ADVANCING 2 LINES
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR3 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE.

      *----------------------------------------------------------------*
      * ZBIRNA STRANA
      *----------------------------------------------------------------*
       5000-PRINT-SUMMARY.
           ADD 1 TO WS-PAGE-NO
           MOVE WS-PAGE-NO TO HDR1-PAGE
           WRITE RPT-LINE FROM WS-HDR1 AFTER ADVANCING PAGE
           WRITE RPT-LINE FROM WS-SUM-HDR AFTER ADVANCING 2 LINES
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           PERFORM VARYING WS-PX FROM 1 BY 1
                   UNTIL WS-PX > WS-PROD-COUNT
               MOVE PT-PART (WS-PX) TO SL-PART
               MOVE PT-DESC (WS-PX) TO SL-DESC
               MOVE PT-COST (WS-PX) TO SL-COST
               WRITE RPT-LINE FROM WS-SUM-LINE AFTER ADVANCING 1 LINE
           END-PERFORM
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           MOVE ' *** KRAJ IZVJESTAJA ***' TO RPT-LINE
           WRITE RPT-LINE AFTER ADVANCING 2 LINES.

      *----------------------------------------------------------------*
      * ZAJEDNIČKI PARAGRAFI (BOM TABELA I STEK)
      *----------------------------------------------------------------*
           COPY BOMLOAD.
           COPY STKOPS.

       9000-TERMINATE.
           CLOSE PART-MASTER
                 COST-REPORT
           MOVE WS-RC TO RETURN-CODE
           MOVE WS-RC TO WS-DISP-RC
           DISPLAY 'AMPCOST  - KRAJ, RC=' WS-DISP-RC.
