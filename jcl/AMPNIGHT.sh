#!/bin/sh
#======================================================================
# AMPNIGHT - NOĆNA OBRADA SISTEMA AMPBOM (PRODUKTRON)
# AUTOR    : BORIS JANJETOVIC
#----------------------------------------------------------------------
# Simulacija JCL posla na Linuxu. Svaki korak (STEP) pokreće jedan
# COBOL program, a datoteke se dodjeljuju preko DD_* varijabli,
# isto kao DD kartice u JCL-u. Ako korak vrati RC veći od dozvoljenog
# (COND), posao se prekida - kao ABEND na mainframe-u.
#
#   STEP000  IEFBR14  - brisanje radnih datoteka od prethodne obrade
#   STEP010  AMPLOAD  - učitavanje matičnih dijelova u PARTMAST
#   STEP020  AMPEXPL  - razvoj sastavnica po nalozima -> REQFILE
#   STEP030  AMPMRP   - sort, netiranje, izvještaj o manjku + CSV
#   STEP040  AMPCOST  - materijalni trošak proizvoda
#
# Varijable okruženja (opciono):
#   AMP_DATA      - folder sa ulaznim podacima  (default: ./data)
#   AMP_WORK      - folder za izlaz i radne datoteke (default: ./work)
#   AMP_BOMFILE   - alternativna datoteka sastavnica (za testove)
#   AMP_RUN_DATE  - datum obrade GGGGMMDD (default: današnji)
#======================================================================
set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
BIN=$ROOT/bin
DATA=${AMP_DATA:-$ROOT/data}
WORK=${AMP_WORK:-$ROOT/work}
MAXCC=0

mkdir -p "$WORK"

log() {
    echo "AMPNIGHT $(date '+%H:%M:%S') $*"
}

# Pokretanje jednog koraka: step <ime> <program> <max dozvoljeni RC>
step() {
    _name=$1; _prog=$2; _cond=$3
    log "$_name  EXEC PGM=$_prog"
    "$BIN/$_prog"
    _rc=$?
    log "$_name  $_prog  ZAVRSEN, RC=$_rc"
    [ "$_rc" -gt "$MAXCC" ] && MAXCC=$_rc
    if [ "$_rc" -gt "$_cond" ]; then
        log "*** POSAO PREKINUT U KORAKU $_name (RC=$_rc > $_cond) ***"
        log "AMPNIGHT ZAVRSEN, MAXCC=$MAXCC"
        exit "$_rc"
    fi
}

log "===== START POSLA AMPNIGHT ====="
log "PODACI: $DATA"
log "IZLAZ : $WORK"

#---- STEP000: brisanje starih radnih datoteka (IEFBR14 + DISP=DELETE)
log "STEP000  EXEC PGM=IEFBR14"
rm -f "$WORK"/PARTMAST* "$WORK"/REQFILE.dat \
      "$WORK"/MRPRPT.txt "$WORK"/SHORTAGE.csv "$WORK"/COSTRPT.txt

#---- DD kartice zajedničke za sve korake
DD_PARTIN=$DATA/parts.dat
DD_BOMFILE=${AMP_BOMFILE:-$DATA/bom.dat}
DD_ORDFILE=$DATA/orders.dat
DD_PARTMAST=$WORK/PARTMAST.idx
DD_REQFILE=$WORK/REQFILE.dat
DD_MRPRPT=$WORK/MRPRPT.txt
DD_SHORTCSV=$WORK/SHORTAGE.csv
DD_COSTRPT=$WORK/COSTRPT.txt
export DD_PARTIN DD_BOMFILE DD_ORDFILE DD_PARTMAST DD_REQFILE \
       DD_MRPRPT DD_SHORTCSV DD_COSTRPT

#---- STEP010..040: RC do 4 je upozorenje, posao ide dalje
step STEP010 AMPLOAD 4
step STEP020 AMPEXPL 4
step STEP030 AMPMRP  4
step STEP040 AMPCOST 4

log "===== AMPNIGHT ZAVRSEN, MAXCC=$MAXCC ====="
exit "$MAXCC"
