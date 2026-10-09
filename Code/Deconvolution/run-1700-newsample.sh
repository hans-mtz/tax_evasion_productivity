#!/bin/bash
# Rerun the first-stage pipeline (test, first stage, PF, deconvolution, 1983 reform, stage-2 inputs) and the thesis
# assets on ONE sample rule (Hans, 2026-10-09; 1700-sample-rule.R), with the existing scripts UNCHANGED, except that the
# stage-2 builders get trim_top_pct = 0 (the rule already drops those firm-years) and file names "trim0.005" -> "trim0".
#
# How: a shadow tree (mktemp) whose Code/Products is the output folder and whose other Code/ folders are symlinks to the
# repo; the scripts run from the shadow root, so every "Code/Products/..." read or write lands in the output folder.
# Inputs are COPIED (cp -c, APFS clone) into the output folder, never symlinked, so no script can write through to the
# current products. Thesis assets go to <out>/thesis-assets/{tables,figures}.
#
#   MODE=new (default): out = Code/Products/S1009 (restricted inputs from 1700-sample-rule.R must already be there).
#   MODE=old: validation. out = $OUT (default a temp dir) with the ORIGINAL inputs and the original trim (0.005); the
#             outputs must reproduce the current products exactly (compare with 1701-compare-samples.R mode=validate).
#   STEPS: space-separated list of steps to run (default: all, in order).
set -euo pipefail
export LC_ALL=C
REPO="$(cd "$(dirname "$0")/../.." && pwd)"
MODE=${MODE:-new}
if [ "$MODE" = new ]; then OUT="$REPO/Code/Products/S1009"; else OUT=${OUT:-$(mktemp -d /tmp/s1009-validate.XXXX)}; fi
mkdir -p "$OUT" "$OUT/thesis-assets/tables" "$OUT/thesis-assets/figures" "$OUT/logs"
P="$REPO/Code/Products"

## inputs ---------------------------------------------------------------------------------------------------------
for f in global_vars.RData np-deconv-funs.RData test_data.RData gnr-cd-me.csv 1472-pf-instrument-comparison.csv; do [ -e "$OUT/$f" ] || cp -c "$P/$f" "$OUT/$f"; done
if [ "$MODE" = old ]; then
  for f in 931.1-fs-se-het.RData colombia_data.RData 921-DD2.RData deconv_funs.Rdata; do [ -e "$OUT/$f" ] || cp -c "$P/$f" "$OUT/$f"; done
else
  for f in 931.1-fs-se-het.RData colombia_data.RData 921-DD2.RData deconv_funs.Rdata 1700-sample-rule.RData; do
    [ -e "$OUT/$f" ] || { echo "missing $OUT/$f: run Rscript Code/Deconvolution/1700-sample-rule.R first"; exit 1; }; done
fi

## shadow tree ----------------------------------------------------------------------------------------------------
SH=$(mktemp -d /tmp/s1009-shadow.XXXX)
for f in .Rprofile renv renv.lock; do ln -s "$REPO/$f" "$SH/$f"; done
mkdir -p "$SH/Code/Deconvolution" "$SH/Paper/images"
for d in "$REPO"/Code/*; do b=$(basename "$d"); case "$b" in Products|Deconvolution) ;; *) ln -s "$d" "$SH/Code/$b";; esac; done
ln -s "$OUT" "$SH/Code/Products"
ln -s "$OUT/thesis-assets" "$SH/Thesis"
for f in "$REPO"/Code/Deconvolution/*; do ln -s "$f" "$SH/Code/Deconvolution/$(basename "$f")"; done
## stage-2 builders: no second trim in MODE=new
for s in 1532-stage2-data-final 1546-input-tau-bar 1572-input-sig2eps 1592-input-plant 1598-input-designs 1603-np-deconv-stage2 1604-input-umed-stage2; do
  rm "$SH/Code/Deconvolution/$s.R"
  if [ "$MODE" = new ]; then
    sed -e 's/trim_top_pct <- 0\.005/trim_top_pct <- 0/' -e 's/trim0\.005/trim0/g' "$REPO/Code/Deconvolution/$s.R" > "$SH/Code/Deconvolution/$s.R"
  else
    cp "$REPO/Code/Deconvolution/$s.R" "$SH/Code/Deconvolution/$s.R"
  fi
done
TR=$([ "$MODE" = new ] && echo trim0 || echo trim0.005)
echo "MODE=$MODE OUT=$OUT SHADOW=$SH"

run() { local s=$1; local f; f=$( (ls "$SH"/Code/Deconvolution/$s-*.R "$SH"/Code/Thesis/$s.R 2>/dev/null || true) | head -1)
  [ -n "$f" ] || { echo "no script for step $s"; return 1; }
  echo "[$(date +%H:%M:%S)] $s ($f)"; ( cd "$SH" && Rscript "${f#$SH/}" ) > "$OUT/logs/$s.Rout" 2>&1 || { echo "FAILED $s (see $OUT/logs/$s.Rout)"; return 1; }; }
mk1585() { ( cd "$SH" && Rscript -e "x <- read.csv('Code/Products/1572-stage2-input-designA-tau-sig2eps-$TR.csv', colClasses = c(sic_3 = 'character')); x <- x[x\$corner == 0, ]; write.csv(x, 'Code/Products/1585-stage2-input-designA-interior-$TR.csv', row.names = FALSE, quote = FALSE); cat('1585 interior rows', nrow(x), '\n')" ) > "$OUT/logs/1585.Rout" 2>&1; }

ALL="1501 1510 1514 1512 1513 1516 1517 1520 1522 1531 1523 1524 1521 1530 ch07-overreporting-elasticity-inversion 1532 1546 1572 1585 1592 1598 1603 1604 assets"
for st in ${STEPS:-$ALL}; do
  case $st in
    1585) mk1585 ;;
    1598|1604) run $st; [ "$MODE" = new ] && for x in "$OUT"/$st-stage2-input-*-trim0.csv; do cp -c "$x" "${x%-trim0.csv}-trim0.005.csv"; done
          ;;  # alias: thesis scripts hard-code the trim0.005 name; in S1009 that file has NO extra trim (the rule did it)
    1603) run 1603; cp "$OUT/1603-np-deconv-stage2.RData" "$OUT/1603-np-deconv-stage2-macbook.RData"
          cp "$OUT/1603-np-deconv-stage2-summary.csv" "$OUT/1603-np-deconv-stage2-summary-macbook.csv" ;;
    assets) for a in ch03-summary-stats-table ch03-industry-stats ch03-size-by-jo ch03-sales-tax-by-year-table ch04-evasion-test appF-evasion-test-bootstrap appF-evasion-test-conservative \
                     ch05-overreporting-ratio ch05-overreporting-medians ch06-pf-comparison ch06-pf-testinv-regions ch06-productivity-comparison \
                     appE-pf-all-industries ch07-fiscal-all-table ch07-fiscal-all-unincorp-plot ch07-fiscal-liable-table ch07-fiscal-liable-vs-exempt-plot \
                     ch07-fiscal-llc-table ch07-fiscal-llc-plot ch07-fiscal-prt-table ch07-fiscal-prt-plot appG-fiscal-wedge ch07-overreporting-elasticity appH-elasticity; do
              [ -e "$REPO/Code/Thesis/$a.R" ] && { run "$a" || true; }; done ;;
    *) run "$st" ;;
  esac
done
echo "[$(date +%H:%M:%S)] all done (MODE=$MODE, OUT=$OUT); shadow $SH left for inspection"
