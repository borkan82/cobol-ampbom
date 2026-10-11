      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    AMPLOAD.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  PRODUKTRON - PROIZVODNJA AUDIO OPREME.
       DATE-WRITTEN.  2000-03-27.
      *================================================================*
      * SISTEM  : AMPBOM - SASTAVNICE I PLANIRANJE MATERIJALA
      * PROGRAM : AMPLOAD - KORAK 010 NOĆNE OBRADE
      * OPIS    : UČITAVA MATIČNE PODATKE O DIJELOVIMA IZ SEKVENCIJALNE
      *           DATOTEKE U INDEKSNU MATIČNU DATOTEKU.
      *           SVAKI SLOG SE PROVJERAVA; NEISPRAVNI I DUPLIKATI
      *           SE ODBACUJU I ISPISUJU U LOG POSLA.
      * ULAZ    : PARTIN   - DIJELOVI, LINE SEQUENTIAL, 80 BAJTOVA
      * IZLAZ   : PARTMAST - INDEKSNA DATOTEKA, KLJUČ PART-NO
      * POVRATNI KODOVI (RETURN-CODE):
      *            0 - OBRADA USPJEŠNA
      *            4 - OBRADA ZAVRŠENA, ALI SU NEKI SLOGOVI ODBAČENI
      *           12 - GREŠKA PRI OTVARANJU DATOTEKE, OBRADA PREKINUTA
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2000-06-17 BJ     PRVA VERZIJA
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    IMENA DATOTEKA SE ZADAJU KROZ VARIJABLE DD_PARTIN I
      *    DD_PARTMAST, SLIČNO DD KARTICAMA U JCL-U
           SELECT PART-INPUT   ASSIGN TO PARTIN
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-PARTIN.
           SELECT PART-MASTER  ASSIGN TO PARTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS PART-NO
                  FILE STATUS  IS WS-FS-PARTMAST.

       DATA DIVISION.
       FILE SECTION.
       FD  PART-INPUT.
       01  PART-IN-RECORD              PIC X(80).

       FD  PART-MASTER.
           COPY PARTREC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-PARTIN            PIC X(02).
               88  FS-PARTIN-OK        VALUE '00'.
           05  WS-FS-PARTMAST          PIC X(02).
               88  FS-PARTMAST-OK      VALUE '00'.

       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)   VALUE 'N'.
               88  END-OF-INPUT        VALUE 'Y'.
           05  WS-VALID-SW             PIC X(01)   VALUE 'Y'.
               88  RECORD-VALID        VALUE 'Y'.
               88  RECORD-INVALID      VALUE 'N'.

      *    BROJAČI U PAKOVANOM FORMATU (COMP-3), KAO NA MAINFRAME-U
       01  WS-COUNTERS.
           05  WS-CNT-READ             PIC 9(07)   COMP-3 VALUE ZERO.
           05  WS-CNT-WRITTEN          PIC 9(07)   COMP-3 VALUE ZERO.
           05  WS-CNT-REJECTED         PIC 9(07)   COMP-3 VALUE ZERO.

       01  WS-REJECT-REASON            PIC X(35)   VALUE SPACES.
       01  WS-DISP-CNT                 PIC Z,ZZZ,ZZ9.
       01  WS-DISP-RC                  PIC Z9.

       PROCEDURE DIVISION.
      *================================================================*
      * GLAVNA PETLJA
      *================================================================*
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-RECORD
               UNTIL END-OF-INPUT
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
      * OTVARANJE DATOTEKA I ČITANJE PRVOG SLOGA
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           DISPLAY 'AMPLOAD  - START UCITAVANJA MATICNIH DIJELOVA'
           OPEN INPUT PART-INPUT
           IF NOT FS-PARTIN-OK
               DISPLAY 'AMPLOAD  - GRESKA OTVARANJA PARTIN, STATUS '
                       WS-FS-PARTIN
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
      *    OUTPUT KREIRA MATIČNU DATOTEKU IZNOVA (PUNO UČITAVANJE)
           OPEN OUTPUT PART-MASTER
           IF NOT FS-PARTMAST-OK
               DISPLAY 'AMPLOAD  - GRESKA OTVARANJA PARTMAST, STATUS '
                       WS-FS-PARTMAST
               CLOSE PART-INPUT
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 8000-READ-INPUT.

      *----------------------------------------------------------------*
      * OBRADA JEDNOG ULAZNOG SLOGA
      *----------------------------------------------------------------*
       2000-PROCESS-RECORD.
           MOVE PART-IN-RECORD TO PART-RECORD
           PERFORM 2100-VALIDATE-RECORD
           IF RECORD-VALID
               PERFORM 2200-WRITE-MASTER
           END-IF
           IF RECORD-INVALID
               PERFORM 2300-REJECT-RECORD
           END-IF
           PERFORM 8000-READ-INPUT.

      *----------------------------------------------------------------*
      * PROVJERA ISPRAVNOSTI POLJA - PRVA GREŠKA ODLUČUJE
      *----------------------------------------------------------------*
       2100-VALIDATE-RECORD.
           SET RECORD-VALID TO TRUE
           EVALUATE TRUE
               WHEN PART-NO = SPACES
                   MOVE 'BROJ DIJELA JE PRAZAN'
                     TO WS-REJECT-REASON
                   SET RECORD-INVALID TO TRUE
               WHEN NOT PART-TYPE-VALID
                   MOVE 'NEISPRAVAN TIP DIJELA (F/S/P)'
                     TO WS-REJECT-REASON
                   SET RECORD-INVALID TO TRUE
               WHEN PART-ON-HAND NOT NUMERIC
                   MOVE 'ZALIHA NIJE NUMERICKA'
                     TO WS-REJECT-REASON
                   SET RECORD-INVALID TO TRUE
               WHEN PART-STD-COST NOT NUMERIC
                   MOVE 'CIJENA NIJE NUMERICKA'
                     TO WS-REJECT-REASON
                   SET RECORD-INVALID TO TRUE
               WHEN PART-LEAD-DAYS NOT NUMERIC
                   MOVE 'ROK NABAVKE NIJE NUMERICKI'
                     TO WS-REJECT-REASON
                   SET RECORD-INVALID TO TRUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * UPIS U INDEKSNU DATOTEKU - DUPLI KLJUČ SE ODBACUJE
      *----------------------------------------------------------------*
       2200-WRITE-MASTER.
           WRITE PART-RECORD
               INVALID KEY
                   MOVE 'DUPLIKAT - DIO VEC POSTOJI'
                     TO WS-REJECT-REASON
                   SET RECORD-INVALID TO TRUE
               NOT INVALID KEY
                   ADD 1 TO WS-CNT-WRITTEN
           END-WRITE.

       2300-REJECT-RECORD.
           ADD 1 TO WS-CNT-REJECTED
           DISPLAY 'AMPLOAD  - ODBACEN SLOG ' WS-CNT-READ
                   ' DIO ' PART-IN-RECORD(1:10)
                   ' : ' WS-REJECT-REASON.

      *----------------------------------------------------------------*
      * ČITANJE SLJEDEĆEG ULAZNOG SLOGA
      *----------------------------------------------------------------*
       8000-READ-INPUT.
           READ PART-INPUT
               AT END
                   SET END-OF-INPUT TO TRUE
               NOT AT END
                   ADD 1 TO WS-CNT-READ
           END-READ.

      *----------------------------------------------------------------*
      * ZATVARANJE I STATISTIKA OBRADE
      *----------------------------------------------------------------*
       9000-TERMINATE.
           CLOSE PART-INPUT
                 PART-MASTER
           MOVE WS-CNT-READ     TO WS-DISP-CNT
           DISPLAY 'AMPLOAD  - PROCITANO SLOGOVA  : ' WS-DISP-CNT
           MOVE WS-CNT-WRITTEN  TO WS-DISP-CNT
           DISPLAY 'AMPLOAD  - UPISANO U PARTMAST : ' WS-DISP-CNT
           MOVE WS-CNT-REJECTED TO WS-DISP-CNT
           DISPLAY 'AMPLOAD  - ODBACENO SLOGOVA   : ' WS-DISP-CNT
           IF WS-CNT-REJECTED > ZERO
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           MOVE RETURN-CODE TO WS-DISP-RC
           DISPLAY 'AMPLOAD  - KRAJ, RC=' WS-DISP-RC.
