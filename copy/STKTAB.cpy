      *================================================================*
      * STKTAB   - STEK ZA RAZVOJ SASTAVNICE PO NIVOIMA
      * SISTEM   : AMPBOM - PRODUKTRON
      *----------------------------------------------------------------*
      * COBOL NEMA REKURZIJU U OVOM STILU PROGRAMIRANJA, PA SE
      * VIŠENIVOVSKA SASTAVNICA RAZVIJA POMOĆU EKSPLICITNOG STEKA.
      * PUNI SE PREKO WS-PUSH-*, A SKIDA SE U WS-CUR-* (VIDI STKOPS).
      *================================================================*
       01  WS-STACK.
           05  WS-SP                   PIC 9(04)   COMP VALUE ZERO.
           05  WS-STK-MAX              PIC 9(04)   COMP VALUE 200.
           05  WS-STK-ENTRY            OCCURS 200 TIMES.
               10  STK-PART            PIC X(10).
               10  STK-QTY             PIC 9(09)V999.
               10  STK-QTY-PER         PIC 9(05)V999.
               10  STK-LEVEL           PIC 9(02).
      *    POLJA ZA STAVLJANJE NA STEK
       01  WS-PUSH-ITEM.
           05  WS-PUSH-PART            PIC X(10).
           05  WS-PUSH-QTY             PIC 9(09)V999.
           05  WS-PUSH-QTY-PER         PIC 9(05)V999.
           05  WS-PUSH-LEVEL           PIC 9(02).
      *    TRENUTNI ČVOR SKINUT SA STEKA
       01  WS-CUR-ITEM.
           05  WS-CUR-PART             PIC X(10).
           05  WS-CUR-QTY              PIC 9(09)V999.
           05  WS-CUR-QTY-PER          PIC 9(05)V999.
           05  WS-CUR-LEVEL            PIC 9(02).
