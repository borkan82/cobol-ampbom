      *================================================================*
      * ORDREC   - SLOG PROIZVODNOG NALOGA
      * SISTEM   : AMPBOM - PRODUKTRON
      * DUŽINA   : 40 BAJTOVA
      *================================================================*
       01  ORDER-RECORD.
           05  ORD-NO                  PIC X(08).
           05  ORD-PART-NO             PIC X(10).
           05  ORD-QTY                 PIC 9(05).
      *    ROK ZAVRŠETKA NALOGA, FORMAT GGGGMMDD
           05  ORD-DUE-DATE            PIC 9(08).
           05  FILLER                  PIC X(09).
