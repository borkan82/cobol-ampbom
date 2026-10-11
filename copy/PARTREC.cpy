      *================================================================*
      * PARTREC  - SLOG MATIČNE DATOTEKE DIJELOVA (PARTMAST)
      * SISTEM   : AMPBOM - PRODUKTRON
      * DUŽINA   : 80 BAJTOVA
      * KLJUČ    : PART-NO
      *----------------------------------------------------------------*
      * TIP DIJELA:  F = GOTOV PROIZVOD (POJAČALO, MIKSETA, KUTIJA)
      *              S = PODSKLOP IZ VLASTITE PROIZVODNJE (PLOČE...)
      *              P = KUPOVNI DIO OD DOBAVLJAČA
      *================================================================*
       01  PART-RECORD.
           05  PART-NO                 PIC X(10).
           05  PART-DESC               PIC X(30).
           05  PART-TYPE               PIC X(01).
               88  PART-FINISHED       VALUE 'F'.
               88  PART-SUBASSY        VALUE 'S'.
               88  PART-PURCHASED      VALUE 'P'.
               88  PART-TYPE-VALID     VALUE 'F' 'S' 'P'.
      *    JEDINICA MJERE (KOM, SET, M...)
           05  PART-UOM                PIC X(03).
      *    TRENUTNA ZALIHA NA SKLADIŠTU
           05  PART-ON-HAND            PIC 9(07).
      *    STANDARDNA NABAVNA CIJENA PO JEDINICI (KM)
           05  PART-STD-COST           PIC 9(07)V99.
      *    VRIJEME NABAVKE / IZRADE U DANIMA
           05  PART-LEAD-DAYS          PIC 9(03).
           05  PART-SUPPLIER           PIC X(10).
           05  FILLER                  PIC X(07).
