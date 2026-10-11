# AMPBOM — Bill of Materials & MRP Batch System (COBOL)

**Owner:** Produktron — manufacturer of tube amplifiers, mixing consoles and speaker cabinets
**Author:** Boris Janjetovic

AMPBOM is a nightly COBOL batch system for an audio equipment manufacturer. It takes production orders (e.g. *25 × 200W tube amplifiers due 20 Oct*), explodes the multi-level bill of materials down to every resistor kit and knob, nets the requirements against stock, and prints a material shortage report telling purchasing **what to order, how much, from whom and by which date**. A second report rolls up the material cost of every product.

---

## Job flow — `AMPNIGHT`

```
  parts.dat ──► STEP010 AMPLOAD ──► PARTMAST (indexed, key PART-NO)
                                         │
  orders.dat ─┐                          │ random reads
  bom.dat ────┴► STEP020 AMPEXPL ◄───────┤
                       │                 │
                       ▼                 │
                  REQFILE.dat            │
                       │                 │
                       ▼                 │
                STEP030 AMPMRP ◄─────────┤   SORT by part + due date
                  │        │             │   control break per part
                  ▼        ▼             │
            MRPRPT.txt  SHORTAGE.csv     │
                                         │
  bom.dat ─────► STEP040 AMPCOST ◄───────┘
                       │
                       ▼
                  COSTRPT.txt
```

| Step | Program | What it does |
|------|---------|--------------|
| STEP000 | (IEFBR14) | Deletes work files from the previous run |
| STEP010 | `AMPLOAD` | Validates part records and loads them into the indexed master. Rejects duplicates and bad part types (RC=4). |
| STEP020 | `AMPEXPL` | Multi-level BOM explosion per production order, using an explicit stack (no recursion). Rejects unknown or purchased-part orders, and aborts with RC=12 on a cyclic BOM. |
| STEP030 | `AMPMRP` | Sorts the requirements, sums them per part, nets against on-hand stock, rounds order quantities up, computes the *order-by* date (earliest due date − lead time) and flags overdue ones as `HITNO!` (urgent). Writes the print report and a CSV for purchasing. |
| STEP040 | `AMPCOST` | Prints the indented BOM of each finished product with a material cost roll-up, plus a summary page. |

Each step sets `RETURN-CODE`. The job script continues on RC ≤ 4 (warnings) and abends on anything higher, like a `COND` parameter in JCL.

## Repository layout

```
src/      COBOL programs (AMPLOAD, AMPEXPL, AMPMRP, AMPCOST)
copy/     Copybooks: record layouts (PARTREC, BOMREC, ORDREC, REQREC),
          in-memory BOM table (BOMTAB), stack (STKTAB) and shared
          procedure code (BOMLOAD, STKOPS)
data/     Sample master data, BOM and production orders (fixed width)
jcl/      AMPNIGHT.sh - JCL-style job, files assigned via DD_* variables
tests/    Regression tests with golden reports, cyclic-BOM test data
```

## Running it

With GnuCOBOL 3.x installed:

```sh
make            # compile all programs
make run        # run the nightly job, output goes to work/
make test       # run the job with a fixed date and compare against golden reports
```

With Docker:

```sh
docker build -t ampbom .
docker run --rm -v "$PWD/work:/app/work" ampbom      # nightly job
docker run --rm ampbom make test                      # tests
```

Set `AMP_RUN_DATE=GGGGMMDD` to run the job "as of" a given date. CI pipelines for GitLab (`.gitlab-ci.yml`) and GitHub Actions (`.github/workflows/ci.yml`) build, test, and publish the reports as artifacts.

## Sample output

Excerpt from `MRPRPT.txt` (material requirements, run date 2001-03-10):

```
 BROJ DIJ.  NAZIV                  T JM  BRUTO POTR.    ZALIHA NETO MANJAK NABAVKA DOBAVLJAC  DAN NARUCI DO    VRIJEDNOST AKCIJA
 ----------------------------------------------------------------------------------------------------------------------------------
 CAB-SPK12  KUTIJA MDF 12IN        P KOM      40.000        25      15.000      15 DRVOPLAST   10 2001-02-24       420.00 NARUCITI
 CAP-470U   KONDENZATOR 470UF 400V P KOM     320.000       400                     ELEKTRON    14                         OK
 MOD-CH01   MODUL KANALA MIKSETE   S KOM     160.000         0     160.000     160 INTERNO      2 2001-02-25              IZRADITI
 POT-10K    POTENCIOMETAR 10K LOG  P KOM     880.000       300     580.000     580 ELEKTRON    14 2001-02-06       638.00 HITNO!
 TUBE-EL34  ELEKTRONSKA CIJEV EL34 P KOM     160.000        80      80.000      80 ELEKTRON    21 2001-01-29     1,160.00 HITNO!
```

Excerpt from `COSTRPT.txt` (indented BOM with cost roll-up):

```
 NIVO   BROJ DIJ.  NAZIV                          T JM    KOL/SKLP  KOL/PROI   STD.CIJENA        IZNOS
 0      MIX-1602   MIKSETA 16 KANALA              F KOM      1.000     1.000
 .1     CHS-MIX01  KUCISTE MIKSETE ALU            P KOM      1.000     1.000        54.00        54.00
 .1     MOD-CH01   MODUL KANALA MIKSETE           S KOM     16.000    16.000
 ..2    POT-10K    POTENCIOMETAR 10K LOG          P KOM      4.000    64.000         1.10        70.40
 ..2    RES-KIT01  OTPORNICI KOMPLET              P SET      0.500     8.000         3.20        25.60
```

The full reports are in `tests/expected/`. They are print files, so pages are separated by form-feed characters.

## Record layouts

**PARTREC** — part master, 80 bytes, key `PART-NO`

| Field | PIC | Meaning |
|-------|-----|---------|
| PART-NO | X(10) | Part number |
| PART-DESC | X(30) | Description |
| PART-TYPE | X(01) | `F` finished product, `S` in-house sub-assembly, `P` purchased |
| PART-UOM | X(03) | Unit of measure |
| PART-ON-HAND | 9(07) | Stock on hand |
| PART-STD-COST | 9(07)V99 | Standard unit cost (DM) |
| PART-LEAD-DAYS | 9(03) | Purchase / build lead time in days |
| PART-SUPPLIER | X(10) | Supplier code |

**BOMREC** — 30 bytes: `BOM-PARENT X(10)`, `BOM-COMPONENT X(10)`, `BOM-QTY-PER 9(05)V999`
**ORDREC** — 40 bytes: `ORD-NO X(08)`, `ORD-PART-NO X(10)`, `ORD-QTY 9(05)`, `ORD-DUE-DATE 9(08)`
**REQREC** — 50 bytes: part, gross quantity `9(09)V999`, order, parent, BOM level, due date

## Design notes

- **BOM explosion without recursion.** The BOM is loaded once into a `WORKING-STORAGE` table. Each order is exploded depth-first with an explicit stack (`STKTAB` / `STKOPS`). A maximum BOM depth guards against cyclic data.
- **Shared code through copybooks.** The BOM loader and the stack operations are procedure-division copybooks used by both `AMPEXPL` and `AMPCOST`. `AMPMRP` reuses the `REQREC` layout for its sort record via `COPY ... REPLACING LEADING`.
- **Classic control-break report.** `SORT ... USING ... OUTPUT PROCEDURE` with page control, a headers/detail/totals structure, and `BLANK WHEN ZERO` edited fields.
- **Date arithmetic** uses `FUNCTION INTEGER-OF-DATE` / `DATE-OF-INTEGER` for the order-by date.
- **Known simplification.** Netting is single-level: stock of a sub-assembly does not reduce the requirements of its components. Full level-by-level MRP netting is a natural next step.
- The sample data intentionally contains a duplicate part, an invalid part type, an unknown product order and an order for a purchased part, so the job finishes with **MAXCC=4** and exercises every rejection path.

## Possible extensions

- Level-by-level netting and planned orders per period (time buckets)
- REST wrapper (e.g. FastAPI) that triggers the batch and serves `SHORTAGE.csv` as JSON
- Export of the part master to PostgreSQL for reporting
