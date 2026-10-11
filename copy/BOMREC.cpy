      *================================================================*
      * BOMREC   - SLOG SASTAVNICE (BILL OF MATERIALS)
      * SISTEM   : AMPBOM - PRODUKTRON
      * DUŽINA   : 30 BAJTOVA
      *----------------------------------------------------------------*
      * JEDAN SLOG = JEDNA VEZA RODITELJ -> KOMPONENTA.
      * KOLIČINA JE PO JEDNOM KOMADU RODITELJA, SA 3 DECIMALE
      * (NPR. 0.500 KOMPLETA OTPORNIKA PO MODULU KANALA).
      *================================================================*
       01  BOM-RECORD.
           05  BOM-PARENT              PIC X(10).
           05  BOM-COMPONENT           PIC X(10).
           05  BOM-QTY-PER             PIC 9(05)V999.
           05  FILLER                  PIC X(02).
