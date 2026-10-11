#!/bin/sh
#======================================================================
# AMPBOM - regresioni testovi
# Autor: Boris Janjetovic
#----------------------------------------------------------------------
# 1. Puna noćna obrada sa fiksnim datumom -> izvještaji se porede sa
#    "zlatnim" izlazima u tests/expected (bajt po bajt).
# 2. Sastavnica sa petljom -> AMPEXPL mora prekinuti posao sa RC=12.
#======================================================================
set -u
ROOT=$(cd "$(dirname "$0")/.." && pwd)
EXP=$ROOT/tests/expected
FAILS=0

pass() { echo "  OK    $*"; }
fail() { echo "  GRESKA $*"; FAILS=$((FAILS + 1)); }

echo "TEST 1: nocna obrada, datum 2001-10-10"
AMP_RUN_DATE=20011010 AMP_WORK=$ROOT/work "$ROOT/jcl/AMPNIGHT.sh" \
    > "$ROOT/work.log" 2>&1
RC=$?
mkdir -p "$ROOT/work" && mv "$ROOT/work.log" "$ROOT/work/AMPNIGHT.log"
# RC=4 je ocekivan: ulazni podaci namjerno sadrze neispravne slogove
if [ "$RC" -eq 4 ]; then pass "MAXCC=4"; else fail "MAXCC=$RC, ocekivano 4"; fi
for f in REQFILE.dat MRPRPT.txt SHORTAGE.csv COSTRPT.txt; do
    if cmp -s "$EXP/$f" "$ROOT/work/$f"; then
        pass "$f"
    else
        fail "$f se razlikuje od ocekivanog:"
        diff "$EXP/$f" "$ROOT/work/$f" | head -20
    fi
done

echo "TEST 2: petlja u sastavnici mora prekinuti posao"
AMP_RUN_DATE=20011010 AMP_WORK=$ROOT/work/cyclic \
AMP_BOMFILE=$ROOT/tests/data/cyclic/bom.dat \
    "$ROOT/jcl/AMPNIGHT.sh" > "$ROOT/work/cyclic.log" 2>&1
RC=$?
if [ "$RC" -eq 12 ] && grep -q "PETLJA" "$ROOT/work/cyclic.log"; then
    pass "AMPEXPL prekinut sa RC=12"
else
    fail "ocekivan RC=12, dobijen RC=$RC"
fi

echo
if [ "$FAILS" -eq 0 ]; then
    echo "SVI TESTOVI PROSLI"
else
    echo "$FAILS TEST(OVA) PALO"
    exit 1
fi
