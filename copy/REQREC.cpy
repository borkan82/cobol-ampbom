      *================================================================*
      * REQREC   - SLOG BRUTO POTREBE (RADNA DATOTEKA REQFILE)
      * SISTEM   : AMPBOM - PRODUKTRON
      * DUŽINA   : 50 BAJTOVA
      *----------------------------------------------------------------*
      * PIŠE GA AMPEXPL (RAZVOJ SASTAVNICE), ČITA GA AMPMRP.
      * KOPIRA SE I SA "REPLACING LEADING" ZA SORT SLOG U AMPMRP.
      *================================================================*
       01  REQ-RECORD.
           05  REQ-PART-NO             PIC X(10).
           05  REQ-QTY                 PIC 9(09)V999.
           05  REQ-ORD-NO              PIC X(08).
           05  REQ-PARENT              PIC X(10).
           05  REQ-LEVEL               PIC 9(02).
           05  REQ-DUE-DATE            PIC 9(08).
