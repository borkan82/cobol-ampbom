      *================================================================*
      * BOMTAB   - TABELA SASTAVNICA U RADNOJ MEMORIJI
      * SISTEM   : AMPBOM - PRODUKTRON
      *----------------------------------------------------------------*
      * CIJELA DATOTEKA SASTAVNICA SE UČITA JEDNOM (VIDI BOMLOAD),
      * DA SE IZBJEGNE PONOVNO ČITANJE DATOTEKE ZA SVAKI NIVO.
      *================================================================*
       01  WS-BOM-CONTROL.
           05  WS-FS-BOM               PIC X(02).
               88  FS-BOM-OK           VALUE '00'.
           05  WS-BOM-EOF-SW           PIC X(01)   VALUE 'N'.
               88  BOM-EOF             VALUE 'Y'.
           05  WS-BOM-MAX              PIC 9(04)   COMP VALUE 500.
           05  WS-BOM-COUNT            PIC 9(04)   COMP VALUE ZERO.
       01  WS-BOM-TABLE.
           05  WS-BOM-ENTRY            OCCURS 500 TIMES
                                       INDEXED BY BX.
               10  WT-PARENT           PIC X(10).
               10  WT-COMPONENT        PIC X(10).
               10  WT-QTY-PER          PIC 9(05)V999.
