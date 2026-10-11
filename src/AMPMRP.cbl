      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    AMPMRP.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  PRODUKTRON - PROIZVODNJA AUDIO OPREME.
       DATE-WRITTEN.  2000-05-17.
      *================================================================*
      * SISTEM  : AMPBOM - SASTAVNICE I PLANIRANJE MATERIJALA
      * PROGRAM : AMPMRP - KORAK 030 NOĆNE OBRADE
      * OPIS    : SORTIRA BRUTO POTREBE PO DIJELU I ROKU, SABIRA IH
      *           (KONTROLNI PREKID PO BROJU DIJELA), UPOREĐUJE SA
      *           ZALIHOM I ŠTAMPA IZVJEŠTAJ O MANJKU MATERIJALA.
      *           ZA SVAKI MANJAK RAČUNA:
      *             - KOLIČINU ZA NABAVKU (ZAOKRUŽENO NAVIŠE)
      *             - DATUM DO KOJEG TREBA NARUČITI
      *               (NAJRANIJI ROK NALOGA MINUS ROK NABAVKE)
      *             - VRIJEDNOST NABAVKE PO STANDARDNOJ CIJENI
      *           AKO JE DATUM NARUČIVANJA PROŠAO -> AKCIJA "HITNO!".
      *           MANJKOVI SE PIŠU I U CSV ZA NABAVNU SLUŽBU.
      * ULAZ    : REQFILE  - BRUTO POTREBE (IZ AMPEXPL)
      *           PARTMAST - MATIČNA DATOTEKA DIJELOVA
      *           AMP_RUN_DATE (ENV, OPCIONO) - DATUM OBRADE GGGGMMDD
      * IZLAZ   : MRPRPT   - IZVJEŠTAJ, 132 KOLONE
      *           SHORTCSV - MANJKOVI, CSV SA ; KAO SEPARATOROM
      * POVRATNI KODOVI (RETURN-CODE):
      *            0 - OBRADA USPJEŠNA
      *            4 - U POTREBAMA POSTOJI NEPOZNAT DIO
      *           12 - FATALNA GREŠKA PRI OTVARANJU DATOTEKA
      *----------------------------------------------------------------*
      * NAPOMENA: NETIRANJE JE JEDNONIVOVSKO - ZALIHA PODSKLOPA NE
      *           UMANJUJE POTREBE NJEGOVIH KOMPONENTI.
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2000-10-10 BJ     PRVA VERZIJA
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REQ-FILE     ASSIGN TO REQFILE
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-REQ.
           SELECT SORT-WORK    ASSIGN TO SORTWK.
           SELECT PART-MASTER  ASSIGN TO PARTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS PART-NO
                  FILE STATUS  IS WS-FS-PARTMAST.
           SELECT MRP-REPORT   ASSIGN TO MRPRPT
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-RPT.
           SELECT SHORT-CSV    ASSIGN TO SHORTCSV
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-CSV.

       DATA DIVISION.
       FILE SECTION.
       FD  REQ-FILE.
           COPY REQREC.
      *    ISTI RASPORED KAO REQREC, SAMO SA PREFIKSOM SR-
       SD  SORT-WORK.
           COPY REQREC REPLACING LEADING ==REQ-== BY ==SR-==.
       FD  PART-MASTER.
           COPY PARTREC.
       FD  MRP-REPORT.
       01  RPT-LINE                    PIC X(132).
       FD  SHORT-CSV.
       01  CSV-LINE                    PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-REQ               PIC X(02).
           05  WS-FS-PARTMAST          PIC X(02).
               88  FS-PARTMAST-OK      VALUE '00'.
           05  WS-FS-RPT               PIC X(02).
               88  FS-RPT-OK           VALUE '00'.
           05  WS-FS-CSV               PIC X(02).
               88  FS-CSV-OK           VALUE '00'.

       01  WS-SWITCHES.
           05  WS-SORT-EOF-SW          PIC X(01)   VALUE 'N'.
               88  SORT-EOF            VALUE 'Y'.
           05  WS-PART-FOUND-SW        PIC X(01)   VALUE 'N'.
               88  PART-FOUND          VALUE 'Y'.

      *    DATUM OBRADE
       01  WS-DATES.
           05  WS-ENV-DATE             PIC X(08)   VALUE SPACES.
           05  WS-RUN-DATE             PIC 9(08).
           05  WS-RUN-DATE-X REDEFINES WS-RUN-DATE.
               10  WS-RUN-YYYY         PIC X(04).
               10  WS-RUN-MM           PIC X(02).
               10  WS-RUN-DD           PIC X(02).
           05  WS-ORDER-BY             PIC 9(08).
           05  WS-ORDER-BY-X REDEFINES WS-ORDER-BY.
               10  WS-OB-YYYY          PIC X(04).
               10  WS-OB-MM            PIC X(02).
               10  WS-OB-DD            PIC X(02).
           05  WS-ORDER-BY-INT         PIC S9(09)  COMP.
           05  WS-FIRST-DUE            PIC 9(08).

      *    AKUMULATORI ZA KONTROLNI PREKID
       01  WS-BREAK-FIELDS.
           05  WS-BRK-PART             PIC X(10).
           05  WS-GROSS                PIC 9(09)V999.
           05  WS-NET                  PIC S9(09)V999.
           05  WS-ORDER-QTY            PIC 9(09).
           05  WS-VALUE                PIC 9(11)V99.
           05  WS-ACTION               PIC X(09).
           05  WS-ORDER-BY-TXT         PIC X(10).
           05  WS-RC                   PIC 9(02)   VALUE ZERO.

       01  WS-TOTALS.
           05  WS-TOT-PARTS            PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-TOT-SHORT            PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-TOT-URGENT           PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-TOT-VALUE            PIC 9(11)V99 COMP-3 VALUE ZERO.

      *    KONTROLA STRANICA
       01  WS-PAGE-CONTROL.
           05  WS-PAGE-NO              PIC 9(04)   VALUE ZERO.
           05  WS-LINE-CNT             PIC 9(03)   VALUE 99.
           05  WS-MAX-LINES            PIC 9(03)   VALUE 50.

      *----------------------------------------------------------------*
      * LINIJE IZVJEŠTAJA (132 KOLONE)
      *----------------------------------------------------------------*
       01  WS-HDR1.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(40)
                   VALUE 'PRODUKTRON - PROIZVODNJA AUDIO OPREME'.
           05  FILLER                  PIC X(76)
                   VALUE 'AMPBOM - IZVJESTAJ O POTREBAMA MATERIJALA'.
           05  FILLER                  PIC X(07)   VALUE 'STRANA '.
           05  HDR1-PAGE               PIC ZZZ9.
           05  FILLER                  PIC X(04)   VALUE SPACES.
       01  WS-HDR2.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(40)
                   VALUE 'PROGRAM: AMPMRP'.
           05  FILLER                  PIC X(14)
                   VALUE 'DATUM OBRADE: '.
           05  HDR2-DATE               PIC X(10).
           05  FILLER                  PIC X(67)   VALUE SPACES.
       01  WS-HDR3.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE 'BROJ DIJ.'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(22)  VALUE 'NAZIV'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(01)  VALUE 'T'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(03)  VALUE 'JM'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(11)  VALUE 'BRUTO POTR.'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(09)  VALUE '   ZALIHA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(11)  VALUE 'NETO MANJAK'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(07)  VALUE 'NABAVKA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE 'DOBAVLJAC'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(03)  VALUE 'DAN'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE 'NARUCI DO'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(12)  VALUE '  VRIJEDNOST'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(09)  VALUE 'AKCIJA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
       01  WS-HDR4.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(130)  VALUE ALL '-'.
           05  FILLER                  PIC X(01)   VALUE SPACE.

       01  WS-DETAIL.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-PART                 PIC X(10).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-DESC                 PIC X(22).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-TYPE                 PIC X(01).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-UOM                  PIC X(03).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-GROSS                PIC ZZZ,ZZ9.999.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-ONHAND               PIC Z,ZZZ,ZZ9.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-NET                  PIC ZZZ,ZZ9.999
                                       BLANK WHEN ZERO.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-ORDQTY               PIC ZZZ,ZZ9 BLANK WHEN ZERO.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-SUPPLIER             PIC X(10).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-LEAD                 PIC ZZ9.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-ORDBY                PIC X(10).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-VALUE                PIC Z,ZZZ,ZZ9.99
                                       BLANK WHEN ZERO.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-ACTION               PIC X(09).
           05  FILLER                  PIC X(01)   VALUE SPACE.

       01  WS-TOTAL-LINE.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  TL-LABEL                PIC X(40).
           05  TL-VALUE                PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(74)   VALUE SPACES.
       01  WS-TOTAL-COUNT REDEFINES WS-TOTAL-LINE.
           05  FILLER                  PIC X(01).
           05  FILLER                  PIC X(40).
           05  TC-COUNT                PIC ZZ,ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(77).

       01  WS-DISP-CNT                 PIC Z,ZZZ,ZZ9.
       01  WS-DISP-RC                  PIC Z9.

      *    POMOĆNA POLJA ZA CSV
       01  WS-CSV-FIELDS.
           05  CSV-NET                 PIC Z(8)9.999.
           05  CSV-ORDQTY              PIC Z(8)9.
           05  CSV-VALUE               PIC Z(10)9.99.

       PROCEDURE DIVISION.
      *================================================================*
      * GLAVNA PETLJA: SORT SA IZLAZNOM PROCEDUROM
      *================================================================*
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           SORT SORT-WORK
               ON ASCENDING KEY SR-PART-NO
               ON ASCENDING KEY SR-DUE-DATE
               USING REQ-FILE
               OUTPUT PROCEDURE IS 3000-PRODUCE-REPORT
                                THRU 3000-EXIT
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
      * DATUM OBRADE: IZ AMP_RUN_DATE AKO JE ZADAN (ZA TESTOVE),
      * INAČE SISTEMSKI DATUM
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           DISPLAY 'AMPMRP   - START PLANIRANJA MATERIJALA'
           ACCEPT WS-ENV-DATE FROM ENVIRONMENT 'AMP_RUN_DATE'
           IF WS-ENV-DATE IS NUMERIC
               MOVE WS-ENV-DATE TO WS-RUN-DATE
           ELSE
               MOVE FUNCTION CURRENT-DATE (1:8) TO WS-RUN-DATE
           END-IF
           STRING WS-RUN-YYYY '-' WS-RUN-MM '-' WS-RUN-DD
                  DELIMITED BY SIZE INTO HDR2-DATE
           DISPLAY 'AMPMRP   - DATUM OBRADE: ' HDR2-DATE
           OPEN INPUT PART-MASTER
           IF NOT FS-PARTMAST-OK
               DISPLAY 'AMPMRP   - GRESKA OTVARANJA PARTMAST, STATUS '
                       WS-FS-PARTMAST
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT MRP-REPORT
           IF NOT FS-RPT-OK
               DISPLAY 'AMPMRP   - GRESKA OTVARANJA MRPRPT, STATUS '
                       WS-FS-RPT
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT SHORT-CSV
           IF NOT FS-CSV-OK
               DISPLAY 'AMPMRP   - GRESKA OTVARANJA SHORTCSV, STATUS '
                       WS-FS-CSV
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           MOVE SPACES TO CSV-LINE
           STRING 'PART_NO;DESCRIPTION;TYPE;NET_SHORTAGE;'
                  'ORDER_QTY;UOM;SUPPLIER;ORDER_BY;VALUE_KM;ACTION'
                  DELIMITED BY SIZE INTO CSV-LINE
           END-STRING
           WRITE CSV-LINE.

      *----------------------------------------------------------------*
      * IZLAZNA PROCEDURA SORTA - KONTROLNI PREKID PO BROJU DIJELA
      *----------------------------------------------------------------*
       3000-PRODUCE-REPORT.
           PERFORM 3100-RETURN-SORTED
           PERFORM UNTIL SORT-EOF
               MOVE SR-PART-NO  TO WS-BRK-PART
               MOVE ZERO        TO WS-GROSS
      *        SORT JE PO ROKU, PA JE PRVI SLOG NAJRANIJI ROK
               MOVE SR-DUE-DATE TO WS-FIRST-DUE
               PERFORM UNTIL SORT-EOF
                          OR SR-PART-NO NOT = WS-BRK-PART
                   ADD SR-QTY TO WS-GROSS
                   PERFORM 3100-RETURN-SORTED
               END-PERFORM
               PERFORM 3200-PART-BREAK
           END-PERFORM
           PERFORM 3900-PRINT-TOTALS.
       3000-EXIT.
           EXIT.

       3100-RETURN-SORTED.
           RETURN SORT-WORK
               AT END SET SORT-EOF TO TRUE
           END-RETURN.

      *----------------------------------------------------------------*
      * OBRADA JEDNOG DIJELA: NETIRANJE SA ZALIHOM
      *----------------------------------------------------------------*
       3200-PART-BREAK.
           ADD 1 TO WS-TOT-PARTS
           MOVE WS-BRK-PART TO PART-NO
           READ PART-MASTER
               INVALID KEY
                   MOVE 'N' TO WS-PART-FOUND-SW
               NOT INVALID KEY
                   MOVE 'Y' TO WS-PART-FOUND-SW
           END-READ
           IF NOT PART-FOUND
      *        NE BI SE SMJELO DESITI - AMPEXPL PROVJERAVA DIJELOVE
               DISPLAY 'AMPMRP   - UPOZORENJE: DIO ' WS-BRK-PART
                       ' NE POSTOJI U PARTMAST'
               MOVE 4 TO WS-RC
               MOVE WS-BRK-PART TO PART-NO
               MOVE '*** NEPOZNAT DIO ***' TO PART-DESC
               MOVE '?'    TO PART-TYPE
               MOVE SPACES TO PART-UOM PART-SUPPLIER
               MOVE ZERO   TO PART-ON-HAND PART-STD-COST
                              PART-LEAD-DAYS
           END-IF
           COMPUTE WS-NET = WS-GROSS - PART-ON-HAND
           MOVE ZERO   TO WS-ORDER-QTY WS-VALUE
           MOVE SPACES TO WS-ORDER-BY-TXT
           IF WS-NET > ZERO
               PERFORM 3300-CALC-SHORTAGE
           ELSE
               MOVE ZERO TO WS-NET
               MOVE 'OK'  TO WS-ACTION
           END-IF
           PERFORM 4000-PRINT-DETAIL.

      *----------------------------------------------------------------*
      * MANJAK: KOLIČINA ZA NABAVKU, DATUM NARUČIVANJA, VRIJEDNOST
      *----------------------------------------------------------------*
       3300-CALC-SHORTAGE.
           ADD 1 TO WS-TOT-SHORT
      *    NABAVLJA SE U CIJELIM JEDINICAMA - ZAOKRUŽI NAVIŠE
           MOVE WS-NET TO WS-ORDER-QTY
           IF WS-ORDER-QTY < WS-NET
               ADD 1 TO WS-ORDER-QTY
           END-IF
           COMPUTE WS-VALUE = WS-ORDER-QTY * PART-STD-COST
           ADD WS-VALUE TO WS-TOT-VALUE
      *    NARUČITI NAJKASNIJE: NAJRANIJI ROK - DANI NABAVKE
           COMPUTE WS-ORDER-BY-INT =
                   FUNCTION INTEGER-OF-DATE (WS-FIRST-DUE)
                   - PART-LEAD-DAYS
           COMPUTE WS-ORDER-BY =
                   FUNCTION DATE-OF-INTEGER (WS-ORDER-BY-INT)
           STRING WS-OB-YYYY '-' WS-OB-MM '-' WS-OB-DD
                  DELIMITED BY SIZE INTO WS-ORDER-BY-TXT
           EVALUATE TRUE
               WHEN WS-ORDER-BY < WS-RUN-DATE
                   MOVE 'HITNO!'   TO WS-ACTION
                   ADD 1 TO WS-TOT-URGENT
               WHEN PART-PURCHASED
                   MOVE 'NARUCITI' TO WS-ACTION
               WHEN OTHER
                   MOVE 'IZRADITI' TO WS-ACTION
           END-EVALUATE
           PERFORM 3400-WRITE-CSV.

       3400-WRITE-CSV.
           MOVE WS-NET       TO CSV-NET
           MOVE WS-ORDER-QTY TO CSV-ORDQTY
           MOVE WS-VALUE     TO CSV-VALUE
           MOVE SPACES TO CSV-LINE
           STRING FUNCTION TRIM (PART-NO)         ';'
                  FUNCTION TRIM (PART-DESC)       ';'
                  PART-TYPE                       ';'
                  FUNCTION TRIM (CSV-NET)         ';'
                  FUNCTION TRIM (CSV-ORDQTY)      ';'
                  FUNCTION TRIM (PART-UOM)        ';'
                  FUNCTION TRIM (PART-SUPPLIER)   ';'
                  WS-ORDER-BY-TXT                 ';'
                  FUNCTION TRIM (CSV-VALUE)       ';'
                  FUNCTION TRIM (WS-ACTION)
                  DELIMITED BY SIZE INTO CSV-LINE
           END-STRING
           WRITE CSV-LINE.

      *----------------------------------------------------------------*
      * ŠTAMPA DETALJNE LINIJE SA KONTROLOM STRANICE
      *----------------------------------------------------------------*
       4000-PRINT-DETAIL.
           IF WS-LINE-CNT >= WS-MAX-LINES
               PERFORM 4100-PRINT-HEADERS
           END-IF
           MOVE PART-NO         TO DL-PART
           MOVE PART-DESC       TO DL-DESC
           MOVE PART-TYPE       TO DL-TYPE
           MOVE PART-UOM        TO DL-UOM
           MOVE WS-GROSS        TO DL-GROSS
           MOVE PART-ON-HAND    TO DL-ONHAND
           MOVE WS-NET          TO DL-NET
           MOVE WS-ORDER-QTY    TO DL-ORDQTY
           MOVE PART-SUPPLIER   TO DL-SUPPLIER
           MOVE PART-LEAD-DAYS  TO DL-LEAD
           MOVE WS-ORDER-BY-TXT TO DL-ORDBY
           MOVE WS-VALUE        TO DL-VALUE
           MOVE WS-ACTION       TO DL-ACTION
           WRITE RPT-LINE FROM WS-DETAIL AFTER ADVANCING 1 LINE
           ADD 1 TO WS-LINE-CNT.

       4100-PRINT-HEADERS.
           ADD 1 TO WS-PAGE-NO
           MOVE WS-PAGE-NO TO HDR1-PAGE
           WRITE RPT-LINE FROM WS-HDR1 AFTER ADVANCING PAGE
           WRITE RPT-LINE FROM WS-HDR2 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR3 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           MOVE 5 TO WS-LINE-CNT.

      *----------------------------------------------------------------*
      * ZBIRNI PODACI NA KRAJU IZVJEŠTAJA
      *----------------------------------------------------------------*
       3900-PRINT-TOTALS.
           IF WS-LINE-CNT > WS-MAX-LINES - 7
               PERFORM 4100-PRINT-HEADERS
           END-IF
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           MOVE SPACES TO WS-TOTAL-LINE
           MOVE 'ANALIZIRANO DIJELOVA ..................:' TO TL-LABEL
           MOVE WS-TOT-PARTS TO TC-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE SPACES TO WS-TOTAL-LINE
           MOVE 'DIJELOVA SA MANJKOM ...................:' TO TL-LABEL
           MOVE WS-TOT-SHORT TO TC-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE SPACES TO WS-TOTAL-LINE
           MOVE 'HITNO - ROK NARUCIVANJA PROSAO ........:' TO TL-LABEL
           MOVE WS-TOT-URGENT TO TC-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE SPACES TO WS-TOTAL-LINE
           MOVE 'UKUPNA VRIJEDNOST NABAVKE KM .........:' TO TL-LABEL
           MOVE WS-TOT-VALUE TO TL-VALUE
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE SPACES TO RPT-LINE
           MOVE ' *** KRAJ IZVJESTAJA ***' TO RPT-LINE
           WRITE RPT-LINE AFTER ADVANCING 2 LINES.

      *----------------------------------------------------------------*
      * KRAJ OBRADE
      *----------------------------------------------------------------*
       9000-TERMINATE.
           CLOSE PART-MASTER
                 MRP-REPORT
                 SHORT-CSV
           MOVE WS-TOT-PARTS TO WS-DISP-CNT
           DISPLAY 'AMPMRP   - DIJELOVA ANALIZIRANO : ' WS-DISP-CNT
           MOVE WS-TOT-SHORT TO WS-DISP-CNT
           DISPLAY 'AMPMRP   - DIJELOVA SA MANJKOM  : ' WS-DISP-CNT
           MOVE WS-RC TO RETURN-CODE
           MOVE WS-RC TO WS-DISP-RC
           DISPLAY 'AMPMRP   - KRAJ, RC=' WS-DISP-RC.
