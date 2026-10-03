#!/bin/bash
# Trimming analysis, backup (2026-10-03, Hans): smallest top-M* trim that passes the hard test at (k, kappa) = (0.75, 0.38),
# original 17-row system (drop 1,12,7,5; 331's eps row kept), delta's from the 1631 (0.75, 0.38) fit. One run-1633-fit.sh
# per trim level (0.1-0.4%: 1630 inputs; 0.5%: 1598, the previous trim), all in parallel, NT threads each. R = 500.
cd "$(dirname "$0")"
NT=${NT:-2}
for tr in 0.001 0.002 0.003 0.004 0.005; do
  case $tr in 0.005) IN=1598-stage2-input-designA-interior-plant-k-trim0.005.csv;; *) IN=1630-stage2-input-designA-interior-plant-k-trim$tr.csv;; esac
  IN=$IN TAGP=1635 TAGS=trim$tr NK=500 NT=$NT DROP=1,12,7,5 START=1631-i-notrim-k0.75-kappa0.38-R500 ./run-1633-fit.sh "0.75 0.38" &
done; wait; echo "all done"
