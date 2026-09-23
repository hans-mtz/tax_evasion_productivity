# Code manifest (auto-generated)

Regenerate: `Rscript Code/000-manifest.R`. Scanned 161 `.R` files under `Code/` and 80 `.qmd` files under `Paper/`, `Quarto-Slides/`, `Thesis/`.

Best-effort regex scan of `load`/`save`/`readRDS`/`saveRDS`/`read.csv`/`write.csv`/`ggsave`/`render_png_tt_tbl`/`render_png_etbl`/`source` calls in R, and markdown image refs in qmd. Not a full parser: misses dynamically-built paths (`sprintf`-constructed filenames), positional (non-`file=`) `save()` calls, and non-image asset refs. Treat gaps below as "not caught by the scan", not "doesn't exist".

## Issues

### Broken loads/reads/sources (1)
A script loads/reads/sources a file with no producing script found in this scan, AND the file is not currently on disk. Each of these either needs its producing script re-run, or is stale/dead and safe to ignore once checked.

| Script | Missing target |
|---|---|
| `Code/Deconvolution/930.2-boot-se-het.R` | `Code/Products/930.1-fs-se-het.RData` |

### Unreferenced outputs (116)
A script writes a file that no other scanned script loads/reads, and no qmd references. Informational -- most are final deliverables (consumed by a qmd not caught by this scan's image-ref pattern), superseded intermediate runs, or one-off diagnostics. Not automatically dead.

<details><summary>expand</summary>

| Script | Output |
|---|---|
| `Code/Colombia/main.R` | `Code/session_info.RData` |
| `Code/Deconvolution/1212-stage2-A-prelim-plot.R` | `1212-stage2-A-lhat-lambda.png` |
| `Code/Deconvolution/1213-stage2-A-aux-e.R` | `Code/Products/1213-stage2-A-aux-e.rds` |
| `Code/Deconvolution/1214-stage2-B-prelim-plot.R` | `1214-stage2-B-lhat-lambda.png` |
| `Code/Deconvolution/1215-stage2-profile-CI-explore.R` | `1215-stage2-profile-CI-explore.png` |
| `Code/Deconvolution/1216-stage2-omega-e-percentiles.R` | `Code/Products/1216-stage2-omega-e-percentiles.rds` |
| `Code/Deconvolution/1221-stage2-trim-cutoff-plot.R` | `Code/Products/1221-stage2-trim-cutoff-summary.csv` |
| `Code/Deconvolution/1221-stage2-trim-cutoff-plot.R` | `Paper/images/1221-stage2-trim-cutoff.png` |
| `Code/Deconvolution/1222-stage2-trim-all-gridpoints-plot.R` | `Code/Products/1222-stage2-trim-all-gridpoints.csv` |
| `Code/Deconvolution/1222-stage2-trim-all-gridpoints-plot.R` | `Paper/images/1222-stage2-trim-all-gridpoints-refixed.png` |
| `Code/Deconvolution/1232-stage2-lag2W-multistart-check.R` | `Code/Products/1232-stage2-lag2W-multistart-check.RData` |
| `Code/Deconvolution/1233-stage2-lag2W-directl-check.R` | `Code/Products/1233-stage2-lag2W-directl-check.RData` |
| `Code/Deconvolution/1235-stage2-lag2W-allpoints-lagmseed.R` | `Code/Products/1235-stage2-lag2W-A9-trim0.005-coarse-lagmseed-allpoints.RData` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | `Paper/images/1237-stage2-estimates-so-far-delta1-delta2.png` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | `Paper/tbls/1237-stage2-best-estimates.png` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | `Paper/tbls/1237-stage2-best-estimates.pdf` |
| `Code/Deconvolution/1238-stage2-lhat-lambda-by-seedstrategy-plot.R` | `Paper/images/1238-stage2-A9-lhat-lambda-by-seedstrategy.png` |
| `Code/Deconvolution/1245-stage2-cbm-checkpoint-plot.R` | `Code/Products/1245-stage2-cbm-checkpoint-table.csv` |
| `Code/Deconvolution/1245-stage2-cbm-checkpoint-plot.R` | `Paper/images/1245-stage2-cbm-mcse-vs-n.png` |
| `Code/Deconvolution/1245-stage2-cbm-checkpoint-plot.R` | `Paper/images/1245-stage2-cbm-batchsize-vs-n.png` |
| `Code/Deconvolution/1252-stage2-nkeep3000-plots.R` | `Paper/images/1252-stage2-lag_m-lhat-lambda-nkeep3000.png` |
| `Code/Deconvolution/1254-stage2-lambda-neldermead-test.R` | `Code/Products/1254-lambda-neldermead-test.RData` |
| `Code/Deconvolution/1263-stage2-revgrid-plot.R` | `Paper/images/1263-stage2-revgrid-delta-R.png` |
| `Code/Deconvolution/1264-revenue-finegrid-plot.R` | `Paper/images/1264-revenue-finegrid-full.png` |
| `Code/Deconvolution/1264-revenue-finegrid-plot.R` | `Paper/images/1264-revenue-finegrid-zoom.png` |
| `Code/Deconvolution/1267-revenue-finegrid-noeta-plot.R` | `Paper/images/1267-revenue-finegrid-noeta-full.png` |
| `Code/Deconvolution/1267-revenue-finegrid-noeta-plot.R` | `Paper/images/1267-revenue-finegrid-noeta-zoom.png` |
| `Code/Deconvolution/1269-lambdagrid-noeta-plot.R` | `Paper/images/1269-lambdagrid-noeta-lhat-lambda.png` |
| `Code/Deconvolution/1272-lambdagrid-final-plot.R` | `Paper/images/1272-lambdagrid-final-lhat-lambda.png` |
| `Code/Deconvolution/1274-lambdagrid-supervisor-plot.R` | `Paper/images/1274-lambdagrid-supervisor-lhat-lambda.png` |
| `Code/Deconvolution/1278-lambdagrid-lagm-vs-lag2W-plot.R` | `Paper/images/1278-lambdagrid-lagm-vs-lag2W.png` |
| `Code/Deconvolution/1279-simulated-laffer-headline-plot.R` | `Paper/images/1279-simulated-laffer-headline.png` |
| `Code/Deconvolution/1280-pct-change-from-prev-plot.R` | `Paper/images/1280-pct-change-from-prev.png` |
| `Code/Deconvolution/1285-lambdagrid-slide-plot.R` | `Paper/images/1285-lambdagrid-slide.png` |
| `Code/Deconvolution/1286-deltagrid-heatmap-plot.R` | `Paper/images/1286-deltagrid-heatmap-lag_m.png` |
| `Code/Deconvolution/1292-revgrid-wide-plot.R` | `Paper/images/1292-revgrid-wide-soft-hard.png` |
| `Code/Deconvolution/1293-revgrid-standard-ci-plot.R` | `Paper/images/1293-revgrid-standard-ci.png` |
| `Code/Deconvolution/1294-precompute-cv-loss-centers.R` | `Code/Products/1294-cv-loss-grid-design.csv` |
| `Code/Deconvolution/1296-cv-loss-final-plot.R` | `Code/Products/1296-cv-bounds-final.csv` |
| `Code/Deconvolution/1296-cv-loss-final-plot.R` | `Code/Products/1296-loss-bounds-final.csv` |
| `Code/Deconvolution/1298-cv-14delta-soft-plot.R` | `Paper/images/1298-cv-14delta-soft.png` |
| `Code/Deconvolution/1299-cv-14delta-hard-plot.R` | `Paper/images/1299-cv-14delta-hard.png` |
| `Code/Deconvolution/1300-cv-15delta-hard-plot.R` | `Paper/images/1300-cv-15delta-hard.png` |
| `Code/Deconvolution/1301-headline-pct-table.R` | `Code/Products/1301-headline-pct-table.csv` |
| `Code/Deconvolution/1303-detection-prob-table.R` | `Code/Products/1303-detection-prob-table.csv` |
| `Code/Deconvolution/1303-detection-prob-table.R` | `Paper/tbls/1303-detection-prob-table.png` |
| `Code/Deconvolution/1303-detection-prob-table.R` | `Paper/tbls/1303-detection-prob-table.pdf` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | `Code/Products/1304-headline-estimates-table.csv` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | `Paper/tbls/1304-headline-estimates-table.png` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | `Paper/tbls/1304-headline-estimates-table.pdf` |
| `Code/Deconvolution/1305-lambdagrid-minonly-plot.R` | `Paper/images/1305-lambdagrid-minonly.png` |
| `Code/Deconvolution/1411-sim-export.R` | `Code/Products/msl/%s-msl-input.csv` |
| `Code/Deconvolution/1411-sim-export.R` | `Code/Products/msl/%s-elvis-input.csv` |
| `Code/Deconvolution/1412-msl-mc.R` | `Code/Products/msl/1412-msl-mc-%s-%s.csv` |
| `Code/Deconvolution/1421-msl-normcheck.R` | `Code/Products/msl/1421-normcheck.rds` |
| `Code/Deconvolution/1444-crosstest-table.R` | `Paper/tbls/1444-crosstest-table.png` |
| `Code/Deconvolution/1444-crosstest-table.R` | `Paper/tbls/1444-crosstest-table.pdf` |
| `Code/Deconvolution/1447-msm-cube-table.R` | `Paper/tbls/1447-msm-cube-table.png` |
| `Code/Deconvolution/1447-msm-cube-table.R` | `Paper/tbls/1447-msm-cube-table.pdf` |
| `Code/Deconvolution/1470-net-tax-first-stage-diag.R` | `Code/Products/1470-net-first-stage-diag.csv` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | `Code/Products/1471-net-pf-params.RData` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | `Code/Products/1471-net-pf-params.csv` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | `Paper/tbls/1471-net-pf-params-table.png` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | `Paper/tbls/1471-net-pf-params-table.pdf` |
| `Code/Deconvolution/1472-pf-instrument-comparison.R` | `Code/Products/1472-pf-instrument-stability.csv` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | `Code/Products/1473-pf-testinv-grid.csv` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | `Code/Products/1473-pf-testinv-summary.csv` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | `Code/Products/1473-pf-testinv-overlap.csv` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | `Paper/images/1473-pf-testinv-regions.png` |
| `Code/Deconvolution/1474-pf-joint-efficient-gmm.R` | `Code/Products/1474-pf-joint-gmm.csv` |
| `Code/Deconvolution/1475-pf-joint-gmm-bootstrap.R` | `Code/Products/1475-pf-bootstrap-timing.RData` |
| `Code/Deconvolution/1476-pf-joint-gmm-bootstrap-full.R` | `Code/Products/1476-pf-joint-gmm-ci.csv` |
| `Code/Deconvolution/1477-pf-joint-testinv-regions.R` | `Code/Products/1477-pf-joint-testinv-summary.csv` |
| `Code/Deconvolution/1477-pf-joint-testinv-regions.R` | `Paper/images/1477-pf-joint-testinv-regions.png` |
| `Code/Deconvolution/1478-pf-weakid-check.R` | `Code/Products/1478-pf-weakid-check.RData` |
| `Code/Deconvolution/207-test.R` | `Code/Products/tests_tbl.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | `Code/Products/gnr_fs.RData` |
| `Code/Deconvolution/235-tims-test.R` | `Code/Products/tims-test.RData` |
| `Code/Deconvolution/251-380s-inds.R` | `Code/Products/251-380s-inds.RData` |
| `Code/Deconvolution/251-380s-inds.R` | `Code/Products/251-369-sum-stats.RData` |
| `Code/Deconvolution/251-380s-inds.R` | `Code/Products/density-369-under-trim.png` |
| `Code/Deconvolution/251-380s-inds.R` | `Code/Products/density-369-under-over-trim.png` |
| `Code/Deconvolution/251-plotting.R` | `Paper/images/graphs/coef_test_vs_tax_rates.png` |
| `Code/Deconvolution/251-plotting.R` | `Paper/images/graphs/251-plot_all_but_sales.png` |
| `Code/Deconvolution/251-plotting.R` | `Paper/images/graphs/251-purchases.png` |
| `Code/Deconvolution/251-plotting.R` | `Paper/images/graphs/251-pur-share-sales.png` |
| `Code/Deconvolution/251-plotting.R` | `Paper/images/graphs/251-effective.png` |
| `Code/Deconvolution/251-plotting.R` | `Paper/images/graphs/251-sales.png` |
| `Code/Deconvolution/299-to-fortran-gnr.R` | `Code/Products/to_fortran_CD_GNR.RData` |
| `Code/Deconvolution/490-boot-pf-prod.R` | `Code/Products/boot_pf_prod.RData` |
| `Code/Deconvolution/510-productivity.R` | `Code/Products/productivity.RData` |
| `Code/Deconvolution/920-DD.R` | `Code/Products/920-DD.RData` |
| `Code/Deconvolution/921.5-DD2.R` | `Code/Products/921.5-DD2.RData` |
| `Code/Deconvolution/930-boot-se-het.R` | `930-boot-se-size-plot.png` |
| `Code/Deconvolution/930-boot-se-het.R` | `930-boot-se-size-plot-1.png` |
| `Code/Deconvolution/930.2-boot-se-het.R` | `930.2-boot-se-size-plot.png` |
| `Code/Deconvolution/930.2-boot-se-het.R` | `930.2-boot-se-size-plot-1.png` |
| `Code/Deconvolution/930.5-boot-se-het.R` | `930.5-boot-se-se-plot.png` |
| `Code/Deconvolution/930.5-boot-se-het.R` | `930.5-boot-se-lag-plot.png` |
| `Code/Deconvolution/930.5-boot-se-het.R` | `930.5-boot-se-lag2-plot.png` |
| `Code/Deconvolution/930.5-boot-se-het.R` | `930.5-boot-se-se-plot-1.png` |
| `Code/Deconvolution/930.5-boot-se-het.R` | `930.5-boot-se-se-plot-2.png` |
| `Code/Deconvolution/930.6-boot-se-het.R` | `930.6-boot-se-se-plot.png` |
| `Code/Deconvolution/930.6-boot-se-het.R` | `930.6-boot-se-se-plot-1.png` |
| `Code/Deconvolution/930.6-boot-se-het.R` | `930.6-boot-se-se-plot-2.png` |
| `Code/Deconvolution/931-boot-se-marg.R` | `931-boot-se-mrg-plot.png` |
| `Code/Deconvolution/932-avg-elas.R` | `932-boot-se-mrg-plot.png` |
| `Code/Deconvolution/932.5-avg-elas.R` | `Code/Products/932.5-avg-elas-interactions.RData` |
| `Code/Deconvolution/932.5-avg-elas.R` | `932.5-boot-se-mrg-plot.png` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | `Code/Products/932.6-avg-elas-interactions.RData` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | `932.6-boot-se-mrg-plot.png` |
| `Code/Deconvolution/935-ev-loc.R` | `Code/Products/935-ev-loc.RData` |
| `Code/Deconvolution/935-ev-loc.R` | `935-ev-loc-dept-map.png` |
| `Code/Deconvolution/935-ev-loc.R` | `Paper/tbls/935-evasion-loc-metro.png` |
| `Code/Deconvolution/935-ev-loc.R` | `Paper/tbls/935-evasion-loc-metro.pdf` |
| `Code/Thesis/ch06-pf-comparison.R` | `Thesis/tables/ch06-pf-comparison.pdf` |

</details>

## Code/Thesis: traceability to original scripts

For each `Code/Thesis/*.R` (the new, curated pipeline), its transitive upstream in the legacy `Code/Deconvolution`/`Code/Colombia` tree -- i.e. which original scripts its own inputs were produced by.

### `Code/Thesis/ch06-pf-comparison.R`
- Direct inputs: `Code/Products/1476-pf-joint-gmm-bootstrap.RData`, `Code/Products/deconv_prod_fun_trim.RData`, `Code/Products/1476-pf-joint-gmm-bootstrap.RData`, `Code/Products/deconv_prod_fun_trim.RData`, `Code/Products/1478-pf-joint-testinv-summary-cons.csv`, `Code/Products/1478-pf-joint-testinv-summary-cons.csv`, `Code/Deconvolution/050-render-tbls.R`, `Code/Deconvolution/050-render-tbls.R`
- Produces: `Thesis/tables/ch06-pf-comparison.png`, `Thesis/tables/ch06-pf-comparison.pdf`, `Thesis/tables/ch06-pf-comparison.png`, `Thesis/tables/ch06-pf-comparison.pdf`
- Transitive upstream (legacy scripts this descends from): `Code/Deconvolution/1476-pf-joint-gmm-bootstrap-full.R`, `Code/Deconvolution/1478-pf-weakid-check.R`, `Code/Deconvolution/300-deconv-prod.R`, `Code/Deconvolution/050-render-tbls.R`, `Code/Deconvolution/001-data.R`, `Code/Deconvolution/1477-pf-joint-testinv-regions.R`, `Code/Deconvolution/208-run-vars.R`, `Code/Deconvolution/230-fs.R`, `Code/Deconvolution/425_omega.R`, `Code/Deconvolution/930.1-fs-se-het.R`, `Code/Colombia/10_data_wrangling.R`, `Code/Deconvolution/205_intermediates.R`, `Code/Deconvolution/206-boot-test.R`, `Code/Deconvolution/290-bs-mle-data.R`, `Code/Deconvolution/291-bs-deconv.R`, `Code/Deconvolution/915.1-size.R`, `Code/Colombia/00_reading_data.R`, `Code/Deconvolution/911-all-inds-2.R`, `Code/Deconvolution/915-size.R`

### `Code/Thesis/ch06-pf-testinv-regions.R`
- Direct inputs: `Code/Products/1477-pf-joint-testinv-grid.csv`, `Code/Products/1477-pf-joint-testinv-grid.csv`
- Produces: `Thesis/figures/ch06-pf-testinv-regions.png`, `Thesis/figures/ch06-pf-testinv-regions.png`
- Transitive upstream (legacy scripts this descends from): `Code/Deconvolution/1477-pf-joint-testinv-regions.R`, `Code/Deconvolution/930.1-fs-se-het.R`, `Code/Deconvolution/001-data.R`, `Code/Deconvolution/915.1-size.R`, `Code/Colombia/10_data_wrangling.R`, `Code/Deconvolution/911-all-inds-2.R`, `Code/Deconvolution/915-size.R`, `Code/Colombia/00_reading_data.R`

## Full producer/consumer graph

One row per scanned `load`/`save`/`read.csv`/`write.csv`/`readRDS`/`saveRDS`/`ggsave`/table-png/`source` call. `role` is `produces` (writes/creates) or `consumes` (reads/depends on).

<details><summary>expand (~1107 rows)</summary>

| Script | Role | Target |
|---|---|---|
| `Code/Colombia/00_reading_data.R` | produces | `Code/Products/col_df.RData` |
| `Code/Colombia/00_reading_data.R` | produces | `Code/Products/col_df.RData` |
| `Code/Colombia/10_data_wrangling.R` | produces | `Code/Products/colombia_data.RData` |
| `Code/Colombia/10_data_wrangling.R` | produces | `Code/Products/colombia_data.RData` |
| `Code/Colombia/10_data_wrangling.R` | produces | `Code/Products/colombia_data.RData` |
| `Code/Colombia/10_data_wrangling.R` | produces | `Code/Products/colombia_data.RData` |
| `Code/Colombia/main.R` | produces | `Code/session_info.RData` |
| `Code/Colombia/main.R` | produces | `Code/session_info.RData` |
| `Code/Deconvolution/001-data.R` | produces | `Code/Products/test_data.RData` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | produces | `Code/Products/1100-MSL-opttax.RData` |
| `Code/Deconvolution/1200-stage2-data.R` | produces | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1200-stage2-data.R` | produces | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1205-stage2-warmstart.R` | produces | `Code/Products/1205-stage2-warmstart.RData` |
| `Code/Deconvolution/1205-stage2-warmstart.R` | produces | `Code/Products/1205-stage2-warmstart.RData` |
| `Code/Deconvolution/1206-stage2-eps-targets.R` | produces | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1206-stage2-eps-targets.R` | produces | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1207-stage2-omega-targets.R` | produces | `Code/Products/1207-stage2-omega-targets.RData` |
| `Code/Deconvolution/1207-stage2-omega-targets.R` | produces | `Code/Products/1207-stage2-omega-targets.RData` |
| `Code/Deconvolution/1212-stage2-A-prelim-plot.R` | produces | `1212-stage2-A-lhat-lambda.png` |
| `Code/Deconvolution/1212-stage2-A-prelim-plot.R` | produces | `1212-stage2-A-lhat-lambda.png` |
| `Code/Deconvolution/1213-stage2-A-aux-e.R` | produces | `Code/Products/1213-stage2-A-aux-e.rds` |
| `Code/Deconvolution/1213-stage2-A-aux-e.R` | produces | `Code/Products/1213-stage2-A-aux-e.rds` |
| `Code/Deconvolution/1214-stage2-B-prelim-plot.R` | produces | `1214-stage2-B-lhat-lambda.png` |
| `Code/Deconvolution/1214-stage2-B-prelim-plot.R` | produces | `1214-stage2-B-lhat-lambda.png` |
| `Code/Deconvolution/1215-stage2-profile-CI-explore.R` | produces | `1215-stage2-profile-CI-explore.png` |
| `Code/Deconvolution/1215-stage2-profile-CI-explore.R` | produces | `1215-stage2-profile-CI-explore.png` |
| `Code/Deconvolution/1216-stage2-omega-e-percentiles.R` | produces | `Code/Products/1216-stage2-omega-e-percentiles.rds` |
| `Code/Deconvolution/1216-stage2-omega-e-percentiles.R` | produces | `Code/Products/1216-stage2-omega-e-percentiles.rds` |
| `Code/Deconvolution/1218-stage2-lambda-ci.R` | produces | `Code/Products/1218-stage2-lambda-ci.RData` |
| `Code/Deconvolution/1218-stage2-lambda-ci.R` | produces | `Code/Products/1218-stage2-lambda-ci.RData` |
| `Code/Deconvolution/1221-stage2-trim-cutoff-plot.R` | produces | `Code/Products/1221-stage2-trim-cutoff-summary.csv` |
| `Code/Deconvolution/1221-stage2-trim-cutoff-plot.R` | produces | `Code/Products/1221-stage2-trim-cutoff-summary.csv` |
| `Code/Deconvolution/1221-stage2-trim-cutoff-plot.R` | produces | `Paper/images/1221-stage2-trim-cutoff.png` |
| `Code/Deconvolution/1221-stage2-trim-cutoff-plot.R` | produces | `Paper/images/1221-stage2-trim-cutoff.png` |
| `Code/Deconvolution/1222-stage2-trim-all-gridpoints-plot.R` | produces | `Code/Products/1222-stage2-trim-all-gridpoints.csv` |
| `Code/Deconvolution/1222-stage2-trim-all-gridpoints-plot.R` | produces | `Code/Products/1222-stage2-trim-all-gridpoints.csv` |
| `Code/Deconvolution/1222-stage2-trim-all-gridpoints-plot.R` | produces | `Paper/images/1222-stage2-trim-all-gridpoints-refixed.png` |
| `Code/Deconvolution/1222-stage2-trim-all-gridpoints-plot.R` | produces | `Paper/images/1222-stage2-trim-all-gridpoints-refixed.png` |
| `Code/Deconvolution/1232-stage2-lag2W-multistart-check.R` | produces | `Code/Products/1232-stage2-lag2W-multistart-check.RData` |
| `Code/Deconvolution/1232-stage2-lag2W-multistart-check.R` | produces | `Code/Products/1232-stage2-lag2W-multistart-check.RData` |
| `Code/Deconvolution/1233-stage2-lag2W-directl-check.R` | produces | `Code/Products/1233-stage2-lag2W-directl-check.RData` |
| `Code/Deconvolution/1233-stage2-lag2W-directl-check.R` | produces | `Code/Products/1233-stage2-lag2W-directl-check.RData` |
| `Code/Deconvolution/1235-stage2-lag2W-allpoints-lagmseed.R` | produces | `Code/Products/1235-stage2-lag2W-A9-trim0.005-coarse-lagmseed-allpoints.RData` |
| `Code/Deconvolution/1235-stage2-lag2W-allpoints-lagmseed.R` | produces | `Code/Products/1235-stage2-lag2W-A9-trim0.005-coarse-lagmseed-allpoints.RData` |
| `Code/Deconvolution/1236-stage2-checks-lowlambda-lagm-fine.R` | produces | `Code/Products/1236-stage2-checks-lowlambda-lagm-fine.RData` |
| `Code/Deconvolution/1236-stage2-checks-lowlambda-lagm-fine.R` | produces | `Code/Products/1236-stage2-checks-lowlambda-lagm-fine.RData` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | produces | `Code/Products/1237-stage2-estimates-so-far.csv` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | produces | `Code/Products/1237-stage2-estimates-so-far.csv` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | produces | `Paper/images/1237-stage2-estimates-so-far-delta1-delta2.png` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | produces | `Paper/images/1237-stage2-estimates-so-far-delta1-delta2.png` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | produces | `Paper/tbls/1237-stage2-best-estimates.png` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | produces | `Paper/tbls/1237-stage2-best-estimates.pdf` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | produces | `Paper/tbls/1237-stage2-best-estimates.png` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | produces | `Paper/tbls/1237-stage2-best-estimates.pdf` |
| `Code/Deconvolution/1238-stage2-lhat-lambda-by-seedstrategy-plot.R` | produces | `Paper/images/1238-stage2-A9-lhat-lambda-by-seedstrategy.png` |
| `Code/Deconvolution/1238-stage2-lhat-lambda-by-seedstrategy-plot.R` | produces | `Paper/images/1238-stage2-A9-lhat-lambda-by-seedstrategy.png` |
| `Code/Deconvolution/1245-stage2-cbm-checkpoint-plot.R` | produces | `Code/Products/1245-stage2-cbm-checkpoint-table.csv` |
| `Code/Deconvolution/1245-stage2-cbm-checkpoint-plot.R` | produces | `Code/Products/1245-stage2-cbm-checkpoint-table.csv` |
| `Code/Deconvolution/1245-stage2-cbm-checkpoint-plot.R` | produces | `Paper/images/1245-stage2-cbm-mcse-vs-n.png` |
| `Code/Deconvolution/1245-stage2-cbm-checkpoint-plot.R` | produces | `Paper/images/1245-stage2-cbm-batchsize-vs-n.png` |
| `Code/Deconvolution/1245-stage2-cbm-checkpoint-plot.R` | produces | `Paper/images/1245-stage2-cbm-mcse-vs-n.png` |
| `Code/Deconvolution/1245-stage2-cbm-checkpoint-plot.R` | produces | `Paper/images/1245-stage2-cbm-batchsize-vs-n.png` |
| `Code/Deconvolution/1252-stage2-nkeep3000-plots.R` | produces | `Paper/images/1252-stage2-lag_m-lhat-lambda-nkeep3000.png` |
| `Code/Deconvolution/1252-stage2-nkeep3000-plots.R` | produces | `Paper/images/1252-stage2-lag_m-lhat-lambda-nkeep3000.png` |
| `Code/Deconvolution/1254-stage2-lambda-neldermead-test.R` | produces | `Code/Products/1254-lambda-neldermead-test.RData` |
| `Code/Deconvolution/1254-stage2-lambda-neldermead-test.R` | produces | `Code/Products/1254-lambda-neldermead-test.RData` |
| `Code/Deconvolution/1263-stage2-revgrid-plot.R` | produces | `Paper/images/1263-stage2-revgrid-delta-R.png` |
| `Code/Deconvolution/1263-stage2-revgrid-plot.R` | produces | `Paper/images/1263-stage2-revgrid-delta-R.png` |
| `Code/Deconvolution/1264-revenue-finegrid-plot.R` | produces | `Paper/images/1264-revenue-finegrid-full.png` |
| `Code/Deconvolution/1264-revenue-finegrid-plot.R` | produces | `Paper/images/1264-revenue-finegrid-zoom.png` |
| `Code/Deconvolution/1264-revenue-finegrid-plot.R` | produces | `Paper/images/1264-revenue-finegrid-full.png` |
| `Code/Deconvolution/1264-revenue-finegrid-plot.R` | produces | `Paper/images/1264-revenue-finegrid-zoom.png` |
| `Code/Deconvolution/1267-revenue-finegrid-noeta-plot.R` | produces | `Paper/images/1267-revenue-finegrid-noeta-full.png` |
| `Code/Deconvolution/1267-revenue-finegrid-noeta-plot.R` | produces | `Paper/images/1267-revenue-finegrid-noeta-zoom.png` |
| `Code/Deconvolution/1267-revenue-finegrid-noeta-plot.R` | produces | `Paper/images/1267-revenue-finegrid-noeta-full.png` |
| `Code/Deconvolution/1267-revenue-finegrid-noeta-plot.R` | produces | `Paper/images/1267-revenue-finegrid-noeta-zoom.png` |
| `Code/Deconvolution/1269-lambdagrid-noeta-plot.R` | produces | `Paper/images/1269-lambdagrid-noeta-lhat-lambda.png` |
| `Code/Deconvolution/1269-lambdagrid-noeta-plot.R` | produces | `Paper/images/1269-lambdagrid-noeta-lhat-lambda.png` |
| `Code/Deconvolution/1272-lambdagrid-final-plot.R` | produces | `Paper/images/1272-lambdagrid-final-lhat-lambda.png` |
| `Code/Deconvolution/1272-lambdagrid-final-plot.R` | produces | `Paper/images/1272-lambdagrid-final-lhat-lambda.png` |
| `Code/Deconvolution/1274-lambdagrid-supervisor-plot.R` | produces | `Paper/images/1274-lambdagrid-supervisor-lhat-lambda.png` |
| `Code/Deconvolution/1274-lambdagrid-supervisor-plot.R` | produces | `Paper/images/1274-lambdagrid-supervisor-lhat-lambda.png` |
| `Code/Deconvolution/1278-lambdagrid-lagm-vs-lag2W-plot.R` | produces | `Paper/images/1278-lambdagrid-lagm-vs-lag2W.png` |
| `Code/Deconvolution/1278-lambdagrid-lagm-vs-lag2W-plot.R` | produces | `Paper/images/1278-lambdagrid-lagm-vs-lag2W.png` |
| `Code/Deconvolution/1279-simulated-laffer-headline-plot.R` | produces | `Paper/images/1279-simulated-laffer-headline.png` |
| `Code/Deconvolution/1279-simulated-laffer-headline-plot.R` | produces | `Paper/images/1279-simulated-laffer-headline.png` |
| `Code/Deconvolution/1280-pct-change-from-prev-plot.R` | produces | `Paper/images/1280-pct-change-from-prev.png` |
| `Code/Deconvolution/1280-pct-change-from-prev-plot.R` | produces | `Paper/images/1280-pct-change-from-prev.png` |
| `Code/Deconvolution/1285-lambdagrid-slide-plot.R` | produces | `Paper/images/1285-lambdagrid-slide.png` |
| `Code/Deconvolution/1285-lambdagrid-slide-plot.R` | produces | `Paper/images/1285-lambdagrid-slide.png` |
| `Code/Deconvolution/1286-deltagrid-heatmap-plot.R` | produces | `Paper/images/1286-deltagrid-heatmap-lag_m.png` |
| `Code/Deconvolution/1286-deltagrid-heatmap-plot.R` | produces | `Paper/images/1286-deltagrid-heatmap-lag_m.png` |
| `Code/Deconvolution/1292-revgrid-wide-plot.R` | produces | `Paper/images/1292-revgrid-wide-soft-hard.png` |
| `Code/Deconvolution/1292-revgrid-wide-plot.R` | produces | `Paper/images/1292-revgrid-wide-soft-hard.png` |
| `Code/Deconvolution/1293-revgrid-standard-ci-plot.R` | produces | `Paper/images/1293-revgrid-standard-ci.png` |
| `Code/Deconvolution/1293-revgrid-standard-ci-plot.R` | produces | `Paper/images/1293-revgrid-standard-ci.png` |
| `Code/Deconvolution/1294-precompute-cv-loss-centers.R` | produces | `Code/Products/1294-cv-loss-grid-design.csv` |
| `Code/Deconvolution/1294-precompute-cv-loss-centers.R` | produces | `Code/Products/1294-cv-loss-grid-design.csv` |
| `Code/Deconvolution/1296-cv-loss-final-plot.R` | produces | `Code/Products/1296-cv-bounds-final.csv` |
| `Code/Deconvolution/1296-cv-loss-final-plot.R` | produces | `Code/Products/1296-loss-bounds-final.csv` |
| `Code/Deconvolution/1296-cv-loss-final-plot.R` | produces | `Code/Products/1296-cv-bounds-final.csv` |
| `Code/Deconvolution/1296-cv-loss-final-plot.R` | produces | `Code/Products/1296-loss-bounds-final.csv` |
| `Code/Deconvolution/1298-cv-14delta-soft-plot.R` | produces | `Paper/images/1298-cv-14delta-soft.png` |
| `Code/Deconvolution/1298-cv-14delta-soft-plot.R` | produces | `Paper/images/1298-cv-14delta-soft.png` |
| `Code/Deconvolution/1299-cv-14delta-hard-plot.R` | produces | `Paper/images/1299-cv-14delta-hard.png` |
| `Code/Deconvolution/1299-cv-14delta-hard-plot.R` | produces | `Paper/images/1299-cv-14delta-hard.png` |
| `Code/Deconvolution/1300-cv-15delta-hard-plot.R` | produces | `Paper/images/1300-cv-15delta-hard.png` |
| `Code/Deconvolution/1300-cv-15delta-hard-plot.R` | produces | `Paper/images/1300-cv-15delta-hard.png` |
| `Code/Deconvolution/1301-headline-pct-table.R` | produces | `Code/Products/1301-headline-pct-table.csv` |
| `Code/Deconvolution/1301-headline-pct-table.R` | produces | `Code/Products/1301-headline-pct-table.csv` |
| `Code/Deconvolution/1303-detection-prob-table.R` | produces | `Code/Products/1303-detection-prob-table.csv` |
| `Code/Deconvolution/1303-detection-prob-table.R` | produces | `Code/Products/1303-detection-prob-table.csv` |
| `Code/Deconvolution/1303-detection-prob-table.R` | produces | `Paper/tbls/1303-detection-prob-table.png` |
| `Code/Deconvolution/1303-detection-prob-table.R` | produces | `Paper/tbls/1303-detection-prob-table.pdf` |
| `Code/Deconvolution/1303-detection-prob-table.R` | produces | `Paper/tbls/1303-detection-prob-table.png` |
| `Code/Deconvolution/1303-detection-prob-table.R` | produces | `Paper/tbls/1303-detection-prob-table.pdf` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | produces | `Code/Products/1304-headline-estimates-table.csv` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | produces | `Code/Products/1304-headline-estimates-table.csv` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | produces | `Paper/tbls/1304-headline-estimates-table.png` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | produces | `Paper/tbls/1304-headline-estimates-table.pdf` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | produces | `Paper/tbls/1304-headline-estimates-table.png` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | produces | `Paper/tbls/1304-headline-estimates-table.pdf` |
| `Code/Deconvolution/1305-lambdagrid-minonly-plot.R` | produces | `Paper/images/1305-lambdagrid-minonly.png` |
| `Code/Deconvolution/1305-lambdagrid-minonly-plot.R` | produces | `Paper/images/1305-lambdagrid-minonly.png` |
| `Code/Deconvolution/1411-sim-export.R` | produces | `Code/Products/msl/%s-msl-input.csv` |
| `Code/Deconvolution/1411-sim-export.R` | produces | `Code/Products/msl/%s-elvis-input.csv` |
| `Code/Deconvolution/1411-sim-export.R` | produces | `Code/Products/msl/%s-msl-input.csv` |
| `Code/Deconvolution/1411-sim-export.R` | produces | `Code/Products/msl/%s-elvis-input.csv` |
| `Code/Deconvolution/1412-msl-mc.R` | produces | `Code/Products/msl/1412-msl-mc-%s-%s.csv` |
| `Code/Deconvolution/1412-msl-mc.R` | produces | `Code/Products/msl/1412-msl-mc-%s-%s.csv` |
| `Code/Deconvolution/1421-msl-normcheck.R` | produces | `Code/Products/msl/1421-normcheck.rds` |
| `Code/Deconvolution/1421-msl-normcheck.R` | produces | `Code/Products/msl/1421-normcheck.rds` |
| `Code/Deconvolution/1444-crosstest-table.R` | produces | `Paper/tbls/1444-crosstest-table.png` |
| `Code/Deconvolution/1444-crosstest-table.R` | produces | `Paper/tbls/1444-crosstest-table.pdf` |
| `Code/Deconvolution/1444-crosstest-table.R` | produces | `Paper/tbls/1444-crosstest-table.png` |
| `Code/Deconvolution/1444-crosstest-table.R` | produces | `Paper/tbls/1444-crosstest-table.pdf` |
| `Code/Deconvolution/1447-msm-cube-table.R` | produces | `Paper/tbls/1447-msm-cube-table.png` |
| `Code/Deconvolution/1447-msm-cube-table.R` | produces | `Paper/tbls/1447-msm-cube-table.pdf` |
| `Code/Deconvolution/1447-msm-cube-table.R` | produces | `Paper/tbls/1447-msm-cube-table.png` |
| `Code/Deconvolution/1447-msm-cube-table.R` | produces | `Paper/tbls/1447-msm-cube-table.pdf` |
| `Code/Deconvolution/1470-net-tax-first-stage-diag.R` | produces | `Code/Products/1470-fs-net.RData` |
| `Code/Deconvolution/1470-net-tax-first-stage-diag.R` | produces | `Code/Products/1470-fs-net.RData` |
| `Code/Deconvolution/1470-net-tax-first-stage-diag.R` | produces | `Code/Products/1470-net-first-stage-diag.csv` |
| `Code/Deconvolution/1470-net-tax-first-stage-diag.R` | produces | `Code/Products/1470-net-first-stage-diag.csv` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | produces | `Code/Products/1471-net-pf-params.RData` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | produces | `Code/Products/1471-net-pf-params.RData` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | produces | `Code/Products/1471-net-pf-params.csv` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | produces | `Code/Products/1471-net-pf-params.csv` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | produces | `Paper/tbls/1471-net-pf-params-table.png` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | produces | `Paper/tbls/1471-net-pf-params-table.pdf` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | produces | `Paper/tbls/1471-net-pf-params-table.png` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | produces | `Paper/tbls/1471-net-pf-params-table.pdf` |
| `Code/Deconvolution/1472-pf-instrument-comparison.R` | produces | `Code/Products/1472-pf-instrument-comparison.csv` |
| `Code/Deconvolution/1472-pf-instrument-comparison.R` | produces | `Code/Products/1472-pf-instrument-stability.csv` |
| `Code/Deconvolution/1472-pf-instrument-comparison.R` | produces | `Code/Products/1472-pf-instrument-comparison.csv` |
| `Code/Deconvolution/1472-pf-instrument-comparison.R` | produces | `Code/Products/1472-pf-instrument-stability.csv` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | produces | `Code/Products/1473-pf-testinv-grid.csv` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | produces | `Code/Products/1473-pf-testinv-summary.csv` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | produces | `Code/Products/1473-pf-testinv-overlap.csv` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | produces | `Code/Products/1473-pf-testinv-grid.csv` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | produces | `Code/Products/1473-pf-testinv-summary.csv` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | produces | `Code/Products/1473-pf-testinv-overlap.csv` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | produces | `Paper/images/1473-pf-testinv-regions.png` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | produces | `Paper/images/1473-pf-testinv-regions.png` |
| `Code/Deconvolution/1474-pf-joint-efficient-gmm.R` | produces | `Code/Products/1474-pf-joint-gmm.csv` |
| `Code/Deconvolution/1474-pf-joint-efficient-gmm.R` | produces | `Code/Products/1474-pf-joint-gmm.csv` |
| `Code/Deconvolution/1475-pf-joint-gmm-bootstrap.R` | produces | `Code/Products/1475-pf-bootstrap-timing.RData` |
| `Code/Deconvolution/1475-pf-joint-gmm-bootstrap.R` | produces | `Code/Products/1475-pf-bootstrap-timing.RData` |
| `Code/Deconvolution/1476-pf-joint-gmm-bootstrap-full.R` | produces | `Code/Products/1476-pf-joint-gmm-bootstrap.RData` |
| `Code/Deconvolution/1476-pf-joint-gmm-bootstrap-full.R` | produces | `Code/Products/1476-pf-joint-gmm-bootstrap.RData` |
| `Code/Deconvolution/1476-pf-joint-gmm-bootstrap-full.R` | produces | `Code/Products/1476-pf-joint-gmm-ci.csv` |
| `Code/Deconvolution/1476-pf-joint-gmm-bootstrap-full.R` | produces | `Code/Products/1476-pf-joint-gmm-ci.csv` |
| `Code/Deconvolution/1477-pf-joint-testinv-regions.R` | produces | `Code/Products/1477-pf-joint-testinv-grid.csv` |
| `Code/Deconvolution/1477-pf-joint-testinv-regions.R` | produces | `Code/Products/1477-pf-joint-testinv-summary.csv` |
| `Code/Deconvolution/1477-pf-joint-testinv-regions.R` | produces | `Code/Products/1477-pf-joint-testinv-grid.csv` |
| `Code/Deconvolution/1477-pf-joint-testinv-regions.R` | produces | `Code/Products/1477-pf-joint-testinv-summary.csv` |
| `Code/Deconvolution/1477-pf-joint-testinv-regions.R` | produces | `Paper/images/1477-pf-joint-testinv-regions.png` |
| `Code/Deconvolution/1477-pf-joint-testinv-regions.R` | produces | `Paper/images/1477-pf-joint-testinv-regions.png` |
| `Code/Deconvolution/1478-pf-weakid-check.R` | produces | `Code/Products/1478-pf-weakid-check.RData` |
| `Code/Deconvolution/1478-pf-weakid-check.R` | produces | `Code/Products/1478-pf-weakid-check.RData` |
| `Code/Deconvolution/1478-pf-weakid-check.R` | produces | `Code/Products/1478-pf-joint-testinv-summary-cons.csv` |
| `Code/Deconvolution/1478-pf-weakid-check.R` | produces | `Code/Products/1478-pf-joint-testinv-summary-cons.csv` |
| `Code/Deconvolution/205_intermediates.R` | produces | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/205_intermediates.R` | produces | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/206-boot-test.R` | produces | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/206-boot-test.R` | produces | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/206-boot-test.R` | produces | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/206-boot-test.R` | produces | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/206-boot-test.R` | produces | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/206-boot-test.R` | produces | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/206-boot-test.R` | produces | `Code/Products/boot_test_comp_tbl.RData` |
| `Code/Deconvolution/207-test.R` | produces | `Code/Products/tests_tbl.RData` |
| `Code/Deconvolution/208-run-vars.R` | produces | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/210-deconv-moments-boot.R` | produces | `Code/Products/boot_tax_ev_mmt.RData` |
| `Code/Deconvolution/210-deconv-moments-boot.R` | produces | `Code/Products/boot_tax_ev_mmt.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | produces | `Code/Products/gnr_fs.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | produces | `Code/Products/gnr_fs.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | produces | `Code/Products/gnr_fs.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | produces | `Code/Products/gnr_fs.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | produces | `Code/Products/gnr_fs.RData` |
| `Code/Deconvolution/230-fs.R` | produces | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/230-fs.R` | produces | `Code/Products/fs.RData` |
| `Code/Deconvolution/230-fs.R` | produces | `Code/Products/fs.RData` |
| `Code/Deconvolution/235-tims-test.R` | produces | `Code/Products/tims-test.RData` |
| `Code/Deconvolution/240-fs-boot.R` | produces | `Code/Products/boot_fs.RData` |
| `Code/Deconvolution/240-fs-boot.R` | produces | `Code/Products/boot_fs.RData` |
| `Code/Deconvolution/250-i-elas.R` | produces | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/250-i-elas.R` | produces | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/250-i-elas.R` | produces | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/250-i-elas.R` | produces | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/251-380s-inds.R` | produces | `Code/Products/251-380s-inds.RData` |
| `Code/Deconvolution/251-380s-inds.R` | produces | `Code/Products/251-380s-inds.RData` |
| `Code/Deconvolution/251-380s-inds.R` | produces | `Code/Products/251-369-sum-stats.RData` |
| `Code/Deconvolution/251-380s-inds.R` | produces | `Code/Products/density-369-under-trim.png` |
| `Code/Deconvolution/251-380s-inds.R` | produces | `Code/Products/density-369-under-over-trim.png` |
| `Code/Deconvolution/251-380s-inds.R` | produces | `Code/Products/density-369-under-trim.png` |
| `Code/Deconvolution/251-380s-inds.R` | produces | `Code/Products/density-369-under-over-trim.png` |
| `Code/Deconvolution/251-plotting.R` | produces | `Paper/images/graphs/coef_test_vs_tax_rates.png` |
| `Code/Deconvolution/251-plotting.R` | produces | `Paper/images/graphs/251-plot_all_but_sales.png` |
| `Code/Deconvolution/251-plotting.R` | produces | `Paper/images/graphs/251-purchases.png` |
| `Code/Deconvolution/251-plotting.R` | produces | `Paper/images/graphs/251-pur-share-sales.png` |
| `Code/Deconvolution/251-plotting.R` | produces | `Paper/images/graphs/251-effective.png` |
| `Code/Deconvolution/251-plotting.R` | produces | `Paper/images/graphs/251-sales.png` |
| `Code/Deconvolution/290-bs-mle-data.R` | produces | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/290-bs-mle-data.R` | produces | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/290-bs-mle-data.R` | produces | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/291-bs-deconv.R` | produces | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/291-bs-deconv.R` | produces | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/291-bs-deconv.R` | produces | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/291-bs-deconv.R` | produces | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/299-to-fortran-gnr.R` | produces | `Code/Products/to_fortran_CD_GNR.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | produces | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | produces | `Code/Products/deconv_prod_fun_trim.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | produces | `Code/Products/deconv_prod_fun_trim.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | produces | `Code/Products/deconv_prod_fun_trim.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | produces | `Code/Products/deconv_prod_fun_trim.RData` |
| `Code/Deconvolution/425_omega.R` | produces | `Code/Products/omega_ar1_deconv_mle.RData` |
| `Code/Deconvolution/425_omega.R` | produces | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/425_omega.R` | produces | `Code/Products/omega_ar1_deconv_mle.RData` |
| `Code/Deconvolution/425_omega.R` | produces | `Code/Products/omega_ar1_deconv_mle.RData` |
| `Code/Deconvolution/425_omega.R` | produces | `Code/Products/omega_ar1_deconv_mle.RData` |
| `Code/Deconvolution/490-boot-pf-prod.R` | produces | `Code/Products/boot_pf_prod.RData` |
| `Code/Deconvolution/490-boot-pf-prod.R` | produces | `Code/Products/boot_pf_prod.RData` |
| `Code/Deconvolution/510-productivity.R` | produces | `Code/Products/productivity.RData` |
| `Code/Deconvolution/550-np-prod.R` | produces | `Code/Products/np_productivity.RData` |
| `Code/Deconvolution/550-np-prod.R` | produces | `Code/Products/np_productivity.RData` |
| `Code/Deconvolution/550-np-prod.R` | produces | `Code/Products/np_productivity.RData` |
| `Code/Deconvolution/600-mcmc-h.R` | produces | `Code/Products/mcmc_h.Rdata` |
| `Code/Deconvolution/700-replication.R` | produces | `Code/Products/replication.RData` |
| `Code/Deconvolution/700-replication.R` | produces | `Code/Products/replication.RData` |
| `Code/Deconvolution/900-369-trim.R` | produces | `Code/Products/900-369-trim.RData` |
| `Code/Deconvolution/900-369-trim.R` | produces | `Code/Products/900-369-trim.RData` |
| `Code/Deconvolution/910-all-inds.R` | produces | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/910-all-inds.R` | produces | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/910-all-inds.R` | produces | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/910-all-inds.R` | produces | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/911-all-inds-2.R` | produces | `Code/Products/911-all_inds-2.RData` |
| `Code/Deconvolution/915-size.R` | produces | `Code/Products/915-size.RData` |
| `Code/Deconvolution/915.1-size.R` | produces | `Code/Products/915.1-size.RData` |
| `Code/Deconvolution/917-size.R` | produces | `Code/Products/917-size.RData` |
| `Code/Deconvolution/920-DD.R` | produces | `Code/Products/920-DD.RData` |
| `Code/Deconvolution/920-DD.R` | produces | `Code/Products/920-DD.RData` |
| `Code/Deconvolution/920-DD.R` | produces | `Code/Products/920-DD.RData` |
| `Code/Deconvolution/921-DD2.R` | produces | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/921-DD2.R` | produces | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/921-DD2.R` | produces | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/921.1-DD.R` | produces | `Code/Products/921.1-DD.RData` |
| `Code/Deconvolution/921.5-DD2.R` | produces | `Code/Products/921.5-DD2.RData` |
| `Code/Deconvolution/921.5-DD2.R` | produces | `Code/Products/921.5-DD2.RData` |
| `Code/Deconvolution/930-boot-se-het.R` | produces | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/930-boot-se-het.R` | produces | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/930-boot-se-het.R` | produces | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/930-boot-se-het.R` | produces | `930-boot-se-size-plot.png` |
| `Code/Deconvolution/930-boot-se-het.R` | produces | `930-boot-se-size-plot-1.png` |
| `Code/Deconvolution/930.1-fs-se-het.R` | produces | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | produces | `Code/Products/930.2-boot-se-het.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | produces | `Code/Products/930.2-boot-se-het.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | produces | `Code/Products/930.2-boot-se-het.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | produces | `930.2-boot-se-size-plot.png` |
| `Code/Deconvolution/930.2-boot-se-het.R` | produces | `930.2-boot-se-size-plot-1.png` |
| `Code/Deconvolution/930.5-boot-se-het.R` | produces | `Code/Products/930.5-boot-se-het.RData` |
| `Code/Deconvolution/930.5-boot-se-het.R` | produces | `930.5-boot-se-se-plot.png` |
| `Code/Deconvolution/930.5-boot-se-het.R` | produces | `930.5-boot-se-lag-plot.png` |
| `Code/Deconvolution/930.5-boot-se-het.R` | produces | `930.5-boot-se-lag2-plot.png` |
| `Code/Deconvolution/930.5-boot-se-het.R` | produces | `930.5-boot-se-se-plot-1.png` |
| `Code/Deconvolution/930.5-boot-se-het.R` | produces | `930.5-boot-se-se-plot-2.png` |
| `Code/Deconvolution/930.6-boot-se-het.R` | produces | `Code/Products/930.6-boot-se-het.RData` |
| `Code/Deconvolution/930.6-boot-se-het.R` | produces | `930.6-boot-se-se-plot.png` |
| `Code/Deconvolution/930.6-boot-se-het.R` | produces | `930.6-boot-se-se-plot-1.png` |
| `Code/Deconvolution/930.6-boot-se-het.R` | produces | `930.6-boot-se-se-plot-2.png` |
| `Code/Deconvolution/931-boot-se-marg.R` | produces | `Code/Products/931-boot-se-marg.RData` |
| `Code/Deconvolution/931-boot-se-marg.R` | produces | `931-boot-se-mrg-plot.png` |
| `Code/Deconvolution/932-avg-elas.R` | produces | `Code/Products/932-avg-elas.RData` |
| `Code/Deconvolution/932-avg-elas.R` | produces | `932-boot-se-mrg-plot.png` |
| `Code/Deconvolution/932.5-avg-elas.R` | produces | `Code/Products/932.5-avg-elas-interactions.RData` |
| `Code/Deconvolution/932.5-avg-elas.R` | produces | `Code/Products/932.5-avg-elas-interactions.RData` |
| `Code/Deconvolution/932.5-avg-elas.R` | produces | `932.5-boot-se-mrg-plot.png` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | produces | `Code/Products/932.6-avg-elas-interactions.RData` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | produces | `Code/Products/932.6-avg-elas-interactions.RData` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | produces | `932.6-boot-se-mrg-plot.png` |
| `Code/Deconvolution/935-ev-loc.R` | produces | `Code/Products/935-ev-loc.RData` |
| `Code/Deconvolution/935-ev-loc.R` | produces | `Code/Products/935-ev-loc.RData` |
| `Code/Deconvolution/935-ev-loc.R` | produces | `935-ev-loc-dept-map.png` |
| `Code/Deconvolution/935-ev-loc.R` | produces | `Paper/tbls/935-evasion-loc-metro.png` |
| `Code/Deconvolution/935-ev-loc.R` | produces | `Paper/tbls/935-evasion-loc-metro.pdf` |
| `Code/Thesis/ch06-pf-comparison.R` | produces | `Thesis/tables/ch06-pf-comparison.png` |
| `Code/Thesis/ch06-pf-comparison.R` | produces | `Thesis/tables/ch06-pf-comparison.pdf` |
| `Code/Thesis/ch06-pf-comparison.R` | produces | `Thesis/tables/ch06-pf-comparison.png` |
| `Code/Thesis/ch06-pf-comparison.R` | produces | `Thesis/tables/ch06-pf-comparison.pdf` |
| `Code/Thesis/ch06-pf-testinv-regions.R` | produces | `Thesis/figures/ch06-pf-testinv-regions.png` |
| `Code/Thesis/ch06-pf-testinv-regions.R` | produces | `Thesis/figures/ch06-pf-testinv-regions.png` |
| `Code/Colombia/10_data_wrangling.R` | consumes | `Code/Products/col_df.RData` |
| `Code/Colombia/10_data_wrangling.R` | consumes | `Code/Products/col_df.RData` |
| `Code/Colombia/15_global_vars.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Colombia/15_global_vars.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Colombia/15_global_vars.R` | consumes | `Data/Colombia/ciiu-rev2-en.csv` |
| `Code/Colombia/15_global_vars.R` | consumes | `Data/Colombia/ciiu-rev2-en.csv` |
| `Code/Colombia/90_3pDiff.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Colombia/90_3pDiff.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Colombia/90_3pDiff.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Colombia/90_3pDiff.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Colombia/95_parallel_trends.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Colombia/95_parallel_trends.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Colombia/95_parallel_trends.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Colombia/95_parallel_trends.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/001-data.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/001-data.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/030-np-deconv-funs.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/030-np-deconv-funs.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/1000-opt-tax.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/1000-opt-tax.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1000-opt-tax.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/1000-opt-tax.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Products/1100-MSL-opttax.RData` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Products/1100-MSL-opttax.RData` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Deconvolution/021-deconv-funs.R` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Deconvolution/030-np-deconv-funs.R` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Deconvolution/021-deconv-funs.R` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Deconvolution/030-np-deconv-funs.R` |
| `Code/Deconvolution/1100-MSL-opt-tax.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1200-stage2-data.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1200-stage2-data.R` | consumes | `Code/Products/1100-MSL-opttax.RData` |
| `Code/Deconvolution/1200-stage2-data.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1200-stage2-data.R` | consumes | `Code/Products/1100-MSL-opttax.RData` |
| `Code/Deconvolution/1201-MSM.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1201-MSM.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1201-MSM.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1201-MSM.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1201-MSM.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1201-MSM.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1205-stage2-warmstart.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1205-stage2-warmstart.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1205-stage2-warmstart.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1205-stage2-warmstart.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1205-stage2-warmstart.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1205-stage2-warmstart.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1206-stage2-eps-targets.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1206-stage2-eps-targets.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1206-stage2-eps-targets.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1206-stage2-eps-targets.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1207-stage2-omega-targets.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1207-stage2-omega-targets.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1207-stage2-omega-targets.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1207-stage2-omega-targets.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1207-stage2-omega-targets.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1207-stage2-omega-targets.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1210-stage2-elvis-driver.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1210-stage2-elvis-driver.R` | consumes | `Code/Products/1205-stage2-warmstart.RData` |
| `Code/Deconvolution/1210-stage2-elvis-driver.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1210-stage2-elvis-driver.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1210-stage2-elvis-driver.R` | consumes | `Code/Products/1205-stage2-warmstart.RData` |
| `Code/Deconvolution/1210-stage2-elvis-driver.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1210-stage2-elvis-driver.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1210-stage2-elvis-driver.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1211-stage2-elvis-driver-AB.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1211-stage2-elvis-driver-AB.R` | consumes | `Code/Products/1205-stage2-warmstart.RData` |
| `Code/Deconvolution/1211-stage2-elvis-driver-AB.R` | consumes | `Code/Products/1207-stage2-omega-targets.RData` |
| `Code/Deconvolution/1211-stage2-elvis-driver-AB.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1211-stage2-elvis-driver-AB.R` | consumes | `Code/Products/1205-stage2-warmstart.RData` |
| `Code/Deconvolution/1211-stage2-elvis-driver-AB.R` | consumes | `Code/Products/1207-stage2-omega-targets.RData` |
| `Code/Deconvolution/1211-stage2-elvis-driver-AB.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1211-stage2-elvis-driver-AB.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1213-stage2-A-aux-e.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1213-stage2-A-aux-e.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1216-stage2-omega-e-percentiles.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1216-stage2-omega-e-percentiles.R` | consumes | `Code/Products/1207-stage2-omega-targets.RData` |
| `Code/Deconvolution/1216-stage2-omega-e-percentiles.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1216-stage2-omega-e-percentiles.R` | consumes | `Code/Products/1207-stage2-omega-targets.RData` |
| `Code/Deconvolution/1220-stage2-concave-diag.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1220-stage2-concave-diag.R` | consumes | `Code/Products/1205-stage2-warmstart.RData` |
| `Code/Deconvolution/1220-stage2-concave-diag.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1220-stage2-concave-diag.R` | consumes | `Code/Products/1205-stage2-warmstart.RData` |
| `Code/Deconvolution/1220-stage2-concave-diag.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1220-stage2-concave-diag.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1221-stage2-trim-cutoff-plot.R` | consumes | `Code/Products/1218-stage2-lambda-ci.RData` |
| `Code/Deconvolution/1221-stage2-trim-cutoff-plot.R` | consumes | `Code/Products/1218-stage2-lambda-ci.RData` |
| `Code/Deconvolution/1225-stage2-grid-export.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1225-stage2-grid-export.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1225-stage2-grid-export.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1225-stage2-grid-export.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1232-stage2-lag2W-multistart-check.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1232-stage2-lag2W-multistart-check.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1233-stage2-lag2W-directl-check.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1233-stage2-lag2W-directl-check.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1235-stage2-lag2W-allpoints-lagmseed.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1235-stage2-lag2W-allpoints-lagmseed.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1236-stage2-checks-lowlambda-lagm-fine.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1236-stage2-checks-lowlambda-lagm-fine.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | consumes | `Code/Products/1236-stage2-checks-lowlambda-lagm-fine.RData` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | consumes | `Code/Products/1236-stage2-checks-lowlambda-lagm-fine.RData` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1237-stage2-estimates-so-far-plot-table.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1238-stage2-lhat-lambda-by-seedstrategy-plot.R` | consumes | `Code/Products/1237-stage2-estimates-so-far.csv` |
| `Code/Deconvolution/1238-stage2-lhat-lambda-by-seedstrategy-plot.R` | consumes | `Code/Products/1237-stage2-estimates-so-far.csv` |
| `Code/Deconvolution/1239-stage2-deltagrid-Rvs-Cpp-smoke.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1239-stage2-deltagrid-Rvs-Cpp-smoke.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1241-stage2-deltagrid-lag_m-3dplot.R` | consumes | `Code/Products/1240-deltagrid-lag_m-combined.csv` |
| `Code/Deconvolution/1241-stage2-deltagrid-lag_m-3dplot.R` | consumes | `Code/Products/1240-deltagrid-lag_m-combined.csv` |
| `Code/Deconvolution/1252-stage2-nkeep3000-plots.R` | consumes | `Code/Products/1251-lambdagrid-lag_m-nkeep3000-combined.csv` |
| `Code/Deconvolution/1252-stage2-nkeep3000-plots.R` | consumes | `Code/Products/1250-deltagrid-lag_m-nkeep3000-combined.csv` |
| `Code/Deconvolution/1252-stage2-nkeep3000-plots.R` | consumes | `Code/Products/1251-lambdagrid-lag_m-nkeep3000-combined.csv` |
| `Code/Deconvolution/1252-stage2-nkeep3000-plots.R` | consumes | `Code/Products/1250-deltagrid-lag_m-nkeep3000-combined.csv` |
| `Code/Deconvolution/1254-stage2-lambda-neldermead-test.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1254-stage2-lambda-neldermead-test.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1257-stage2-grid3d-cube-plot.R` | consumes | `Code/Products/1255-grid3d-lag_m-combined.csv` |
| `Code/Deconvolution/1257-stage2-grid3d-cube-plot.R` | consumes | `Code/Products/1255-grid3d-lag_m-combined.csv` |
| `Code/Deconvolution/1259-stage2-grid3d-cube-plot-expanded.R` | consumes | `Code/Products/1258-grid3d-lag_m-combined-full.csv` |
| `Code/Deconvolution/1259-stage2-grid3d-cube-plot-expanded.R` | consumes | `Code/Products/1258-grid3d-lag_m-combined-full.csv` |
| `Code/Deconvolution/1260-stage2-revenue-export.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1260-stage2-revenue-export.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/1260-stage2-revenue-export.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1260-stage2-revenue-export.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/1260-stage2-revenue-export.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1260-stage2-revenue-export.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1263-stage2-revgrid-plot.R` | consumes | `Code/Products/1262-revgrid-lag_m-combined.csv` |
| `Code/Deconvolution/1263-stage2-revgrid-plot.R` | consumes | `Code/Products/1262-revgrid-lag_m-combined.csv` |
| `Code/Deconvolution/1264-revenue-finegrid-plot.R` | consumes | `Code/Products/1264-revenue-baseline-finegrid-lag_m.csv` |
| `Code/Deconvolution/1264-revenue-finegrid-plot.R` | consumes | `Code/Products/1264-revenue-baseline-finegrid-lag_m.csv` |
| `Code/Deconvolution/1267-revenue-finegrid-noeta-plot.R` | consumes | `Code/Products/1266-revenue-baseline-finegrid-noeta-lag_m.csv` |
| `Code/Deconvolution/1267-revenue-finegrid-noeta-plot.R` | consumes | `Code/Products/1266-revenue-baseline-finegrid-noeta-lag_m.csv` |
| `Code/Deconvolution/1269-lambdagrid-noeta-plot.R` | consumes | `Code/Products/1268-lambdagrid-noeta-lag_m-combined.csv` |
| `Code/Deconvolution/1269-lambdagrid-noeta-plot.R` | consumes | `Code/Products/1268-lambdagrid-noeta-lag_m-combined.csv` |
| `Code/Deconvolution/1271-stage2-revenue-export-lag2W.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1271-stage2-revenue-export-lag2W.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/1271-stage2-revenue-export-lag2W.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1271-stage2-revenue-export-lag2W.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/1271-stage2-revenue-export-lag2W.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1271-stage2-revenue-export-lag2W.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1272-lambdagrid-final-plot.R` | consumes | `Code/Products/1270-lambdagrid-lag_m-allcombined.csv` |
| `Code/Deconvolution/1272-lambdagrid-final-plot.R` | consumes | `Code/Products/1270-lambdagrid-lag_m-allcombined.csv` |
| `Code/Deconvolution/1274-lambdagrid-supervisor-plot.R` | consumes | `Code/Products/1270-lambdagrid-lag_m-allcombined.csv` |
| `Code/Deconvolution/1274-lambdagrid-supervisor-plot.R` | consumes | `Code/Products/1270-lambdagrid-lag_m-allcombined.csv` |
| `Code/Deconvolution/1278-lambdagrid-lagm-vs-lag2W-plot.R` | consumes | `Code/Products/1270-lambdagrid-lag_m-allcombined.csv` |
| `Code/Deconvolution/1278-lambdagrid-lagm-vs-lag2W-plot.R` | consumes | `Code/Products/1273-lambdagrid-noeta-lag_2_cal_W.csv` |
| `Code/Deconvolution/1278-lambdagrid-lagm-vs-lag2W-plot.R` | consumes | `Code/Products/1270-lambdagrid-lag_m-allcombined.csv` |
| `Code/Deconvolution/1278-lambdagrid-lagm-vs-lag2W-plot.R` | consumes | `Code/Products/1273-lambdagrid-noeta-lag_2_cal_W.csv` |
| `Code/Deconvolution/1279-simulated-laffer-headline-plot.R` | consumes | `Code/Products/1275-revenue-baseline-finegrid-bestfit-lag_m.csv` |
| `Code/Deconvolution/1279-simulated-laffer-headline-plot.R` | consumes | `Code/Products/1276-revenue-onset-finegrid-lag_m.csv` |
| `Code/Deconvolution/1279-simulated-laffer-headline-plot.R` | consumes | `Code/Products/1275-revenue-baseline-finegrid-bestfit-lag_m.csv` |
| `Code/Deconvolution/1279-simulated-laffer-headline-plot.R` | consumes | `Code/Products/1276-revenue-onset-finegrid-lag_m.csv` |
| `Code/Deconvolution/1279-simulated-laffer-headline-plot.R` | consumes | `Code/Products/1275-revenue-baseline-finegrid-bestfit-lag_m.csv` |
| `Code/Deconvolution/1279-simulated-laffer-headline-plot.R` | consumes | `Code/Products/1276-revenue-onset-finegrid-lag_m.csv` |
| `Code/Deconvolution/1279-simulated-laffer-headline-plot.R` | consumes | `Code/Products/1275-revenue-baseline-finegrid-bestfit-lag_m.csv` |
| `Code/Deconvolution/1279-simulated-laffer-headline-plot.R` | consumes | `Code/Products/1276-revenue-onset-finegrid-lag_m.csv` |
| `Code/Deconvolution/1280-pct-change-from-prev-plot.R` | consumes | `Code/Products/1275-revenue-baseline-finegrid-bestfit-lag_m.csv` |
| `Code/Deconvolution/1280-pct-change-from-prev-plot.R` | consumes | `Code/Products/1276-revenue-onset-finegrid-lag_m.csv` |
| `Code/Deconvolution/1280-pct-change-from-prev-plot.R` | consumes | `Code/Products/1276-revenue-onset-finegrid-lag_m.csv` |
| `Code/Deconvolution/1280-pct-change-from-prev-plot.R` | consumes | `Code/Products/1275-revenue-baseline-finegrid-bestfit-lag_m.csv` |
| `Code/Deconvolution/1280-pct-change-from-prev-plot.R` | consumes | `Code/Products/1276-revenue-onset-finegrid-lag_m.csv` |
| `Code/Deconvolution/1280-pct-change-from-prev-plot.R` | consumes | `Code/Products/1276-revenue-onset-finegrid-lag_m.csv` |
| `Code/Deconvolution/1285-lambdagrid-slide-plot.R` | consumes | `Code/Products/1270-lambdagrid-lag_m-allcombined.csv` |
| `Code/Deconvolution/1285-lambdagrid-slide-plot.R` | consumes | `Code/Products/1273-lambdagrid-noeta-lag_2_cal_W.csv` |
| `Code/Deconvolution/1285-lambdagrid-slide-plot.R` | consumes | `Code/Products/1270-lambdagrid-lag_m-allcombined.csv` |
| `Code/Deconvolution/1285-lambdagrid-slide-plot.R` | consumes | `Code/Products/1273-lambdagrid-noeta-lag_2_cal_W.csv` |
| `Code/Deconvolution/1286-deltagrid-heatmap-plot.R` | consumes | `Code/Products/1283-deltagrid-lag_m-combined.csv` |
| `Code/Deconvolution/1286-deltagrid-heatmap-plot.R` | consumes | `Code/Products/1283-deltagrid-lag_m-combined.csv` |
| `Code/Deconvolution/1287-deltagrid-lag_m-3dplot.R` | consumes | `Code/Products/1283-deltagrid-lag_m-combined.csv` |
| `Code/Deconvolution/1287-deltagrid-lag_m-3dplot.R` | consumes | `Code/Products/1283-deltagrid-lag_m-combined.csv` |
| `Code/Deconvolution/1289-cube-3dplot.R` | consumes | `Code/Products/1288-cube-lag_m-combined.csv` |
| `Code/Deconvolution/1289-cube-3dplot.R` | consumes | `Code/Products/1288-cube-lag_m-combined.csv` |
| `Code/Deconvolution/1292-revgrid-wide-plot.R` | consumes | `Code/Products/1291-revgrid-fixedtheta-wide-lag_m-analyzed.csv` |
| `Code/Deconvolution/1292-revgrid-wide-plot.R` | consumes | `Code/Products/1291-revgrid-fixedtheta-wide-lag_m-analyzed.csv` |
| `Code/Deconvolution/1293-revgrid-standard-ci-plot.R` | consumes | `Code/Products/1290-revgrid-fixedtheta-lag_m-analyzed.csv` |
| `Code/Deconvolution/1293-revgrid-standard-ci-plot.R` | consumes | `Code/Products/1291-revgrid-fixedtheta-wide-lag_m-analyzed.csv` |
| `Code/Deconvolution/1293-revgrid-standard-ci-plot.R` | consumes | `Code/Products/1290-revgrid-fixedtheta-lag_m-analyzed.csv` |
| `Code/Deconvolution/1293-revgrid-standard-ci-plot.R` | consumes | `Code/Products/1291-revgrid-fixedtheta-wide-lag_m-analyzed.csv` |
| `Code/Deconvolution/1294-precompute-cv-loss-centers.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1294-precompute-cv-loss-centers.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/1294-precompute-cv-loss-centers.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1294-precompute-cv-loss-centers.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/1297-cv-hard-vs-soft-plot.R` | consumes | `Code/Products/1294-coarse-cv-combined.csv` |
| `Code/Deconvolution/1297-cv-hard-vs-soft-plot.R` | consumes | `Code/Products/1294-fine-cv-combined.csv` |
| `Code/Deconvolution/1297-cv-hard-vs-soft-plot.R` | consumes | `Code/Products/1294-coarse-cv-combined.csv` |
| `Code/Deconvolution/1297-cv-hard-vs-soft-plot.R` | consumes | `Code/Products/1294-fine-cv-combined.csv` |
| `Code/Deconvolution/1298-cv-14delta-soft-plot.R` | consumes | `Code/Products/1298-cv-all14-combined.csv` |
| `Code/Deconvolution/1298-cv-14delta-soft-plot.R` | consumes | `Code/Products/1298-cv-all14-combined.csv` |
| `Code/Deconvolution/1299-cv-14delta-hard-plot.R` | consumes | `Code/Products/1299-cv-all-final-combined.csv` |
| `Code/Deconvolution/1299-cv-14delta-hard-plot.R` | consumes | `Code/Products/1299-cv-all-final-combined.csv` |
| `Code/Deconvolution/1300-cv-15delta-hard-plot.R` | consumes | `Code/Products/1300-cv-16delta-final.csv` |
| `Code/Deconvolution/1300-cv-15delta-hard-plot.R` | consumes | `Code/Products/1300-cv-16delta-final.csv` |
| `Code/Deconvolution/1301-headline-pct-table.R` | consumes | `Code/Products/1300-cv-16delta-final.csv` |
| `Code/Deconvolution/1301-headline-pct-table.R` | consumes | `Code/Products/1300-cv-16delta-final.csv` |
| `Code/Deconvolution/1301-headline-pct-table.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1301-headline-pct-table.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1303-detection-prob-table.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1303-detection-prob-table.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | consumes | `Code/Products/1288-cube-lag_m-combined.csv` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | consumes | `Code/Products/1273-lambdagrid-noeta-lag_2_cal_W.csv` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | consumes | `Code/Products/1288-cube-lag_m-combined.csv` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | consumes | `Code/Products/1273-lambdagrid-noeta-lag_2_cal_W.csv` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1304-headline-estimates-table.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1305-lambdagrid-minonly-plot.R` | consumes | `Code/Products/1270-lambdagrid-lag_m-allcombined.csv` |
| `Code/Deconvolution/1305-lambdagrid-minonly-plot.R` | consumes | `Code/Products/1273-lambdagrid-noeta-lag_2_cal_W.csv` |
| `Code/Deconvolution/1305-lambdagrid-minonly-plot.R` | consumes | `Code/Products/1270-lambdagrid-lag_m-allcombined.csv` |
| `Code/Deconvolution/1305-lambdagrid-minonly-plot.R` | consumes | `Code/Products/1273-lambdagrid-noeta-lag_2_cal_W.csv` |
| `Code/Deconvolution/1306-deltagrid-minonly-3dplot.R` | consumes | `Code/Products/1283-deltagrid-lag_m-combined.csv` |
| `Code/Deconvolution/1306-deltagrid-minonly-3dplot.R` | consumes | `Code/Products/1283-deltagrid-lag_m-combined.csv` |
| `Code/Deconvolution/1400-msl-sim-harness.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1400-msl-sim-harness.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1401-msl-export.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1401-msl-export.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1401-msl-export.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1401-msl-export.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1401-msl-export.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1401-msl-export.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1411-sim-export.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1411-sim-export.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1412-msl-mc.R` | consumes | `Code/Deconvolution/1410-dgp-model.R` |
| `Code/Deconvolution/1412-msl-mc.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1412-msl-mc.R` | consumes | `Code/Deconvolution/1410-dgp-model.R` |
| `Code/Deconvolution/1412-msl-mc.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1415-calibrate-gamma-psi.R` | consumes | `Code/Deconvolution/1410-dgp-model.R` |
| `Code/Deconvolution/1415-calibrate-gamma-psi.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1415-calibrate-gamma-psi.R` | consumes | `Code/Deconvolution/1410-dgp-model.R` |
| `Code/Deconvolution/1415-calibrate-gamma-psi.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1422-msl-score-check.R` | consumes | `Code/Deconvolution/1410-dgp-model.R` |
| `Code/Deconvolution/1422-msl-score-check.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1422-msl-score-check.R` | consumes | `Code/Deconvolution/1410-dgp-model.R` |
| `Code/Deconvolution/1422-msl-score-check.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1423-msl-exact-obs-check.R` | consumes | `Code/Deconvolution/1410-dgp-model.R` |
| `Code/Deconvolution/1423-msl-exact-obs-check.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1423-msl-exact-obs-check.R` | consumes | `Code/Deconvolution/1410-dgp-model.R` |
| `Code/Deconvolution/1423-msl-exact-obs-check.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1431-msm-sim.R` | consumes | `Code/Products/msl/1410-sim-base-lam1e-4-s070.rds` |
| `Code/Deconvolution/1431-msm-sim.R` | consumes | `Code/Products/msl/1410-sim-base-lam1e-4-s070.rds` |
| `Code/Deconvolution/1431-msm-sim.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1431-msm-sim.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1442-msm-eval-real.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1442-msm-eval-real.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1442-msm-eval-real.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1442-msm-eval-real.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1442-msm-eval-real.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1442-msm-eval-real.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1443-msl-score-test.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1443-msl-score-test.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1444-crosstest-table.R` | consumes | `Code/Products/1442-MSM-eval-lag_m-S250-overID.RData` |
| `Code/Deconvolution/1444-crosstest-table.R` | consumes | `Code/Products/1442-MSM-eval-lag_m-S250-overID.RData` |
| `Code/Deconvolution/1444-crosstest-table.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1444-crosstest-table.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1445-msm-cube.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1445-msm-cube.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1445-msm-cube.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1445-msm-cube.R` | consumes | `Code/Products/1206-stage2-eps-targets.RData` |
| `Code/Deconvolution/1445-msm-cube.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1445-msm-cube.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1447-msm-cube-table.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1447-msm-cube-table.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1450-dgp-321.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1450-dgp-321.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1451-calibrate-321.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1451-calibrate-321.R` | consumes | `Code/Products/1200-stage2-data.RData` |
| `Code/Deconvolution/1451-calibrate-321.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1451-calibrate-321.R` | consumes | `Code/Deconvolution/1450-dgp-321.R` |
| `Code/Deconvolution/1451-calibrate-321.R` | consumes | `Code/Deconvolution/utils-cli.R` |
| `Code/Deconvolution/1451-calibrate-321.R` | consumes | `Code/Deconvolution/1450-dgp-321.R` |
| `Code/Deconvolution/1470-net-tax-first-stage-diag.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/1470-net-tax-first-stage-diag.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/1470-net-tax-first-stage-diag.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1470-net-tax-first-stage-diag.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/1470-net-tax-first-stage-diag.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/1470-net-tax-first-stage-diag.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | consumes | `Code/Products/1470-fs-net.RData` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | consumes | `Code/Products/deconv_prod_fun_trim.RData` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | consumes | `Code/Products/1470-fs-net.RData` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | consumes | `Code/Products/deconv_prod_fun_trim.RData` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1471-net-tax-pf-params.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Deconvolution/1472-pf-instrument-comparison.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/1472-pf-instrument-comparison.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/1472-pf-instrument-comparison.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1472-pf-instrument-comparison.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/1472-pf-instrument-comparison.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/1472-pf-instrument-comparison.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | consumes | `Code/Products/1472-pf-instrument-comparison.csv` |
| `Code/Deconvolution/1473-pf-testinv-regions.R` | consumes | `Code/Products/1472-pf-instrument-comparison.csv` |
| `Code/Deconvolution/1474-pf-joint-efficient-gmm.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1474-pf-joint-efficient-gmm.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1474-pf-joint-efficient-gmm.R` | consumes | `Code/Products/1472-pf-instrument-comparison.csv` |
| `Code/Deconvolution/1474-pf-joint-efficient-gmm.R` | consumes | `Code/Products/1472-pf-instrument-comparison.csv` |
| `Code/Deconvolution/1475-pf-joint-gmm-bootstrap.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/1475-pf-joint-gmm-bootstrap.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/1475-pf-joint-gmm-bootstrap.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1475-pf-joint-gmm-bootstrap.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/1475-pf-joint-gmm-bootstrap.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/1475-pf-joint-gmm-bootstrap.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1476-pf-joint-gmm-bootstrap-full.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/1476-pf-joint-gmm-bootstrap-full.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/1476-pf-joint-gmm-bootstrap-full.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1476-pf-joint-gmm-bootstrap-full.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/1476-pf-joint-gmm-bootstrap-full.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/1476-pf-joint-gmm-bootstrap-full.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1477-pf-joint-testinv-regions.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1477-pf-joint-testinv-regions.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1478-pf-weakid-check.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1478-pf-weakid-check.R` | consumes | `Code/Products/931.1-fs-se-het.RData` |
| `Code/Deconvolution/1478-pf-weakid-check.R` | consumes | `Code/Products/1477-pf-joint-testinv-grid.csv` |
| `Code/Deconvolution/1478-pf-weakid-check.R` | consumes | `Code/Products/1477-pf-joint-testinv-grid.csv` |
| `Code/Deconvolution/205_intermediates.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/205_intermediates.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/205_intermediates.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/205_intermediates.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/205_intermediates.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/205_intermediates.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/205_intermediates.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/205_intermediates.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/205_intermediates.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/205_intermediates.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/206-boot-test.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/206-boot-test.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/206-boot-test.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/206-boot-test.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/206-boot-test.R` | consumes | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/206-boot-test.R` | consumes | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/206-boot-test.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/206-boot-test.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/206-boot-test.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/206-boot-test.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/206-boot-test.R` | consumes | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/206-boot-test.R` | consumes | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/207-test.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/207-test.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/207-test.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/207-test.R` | consumes | `Code/Products/boot_test_comp_tbl.RData` |
| `Code/Deconvolution/207-test.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/207-test.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/207-test.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/207-test.R` | consumes | `Code/Products/boot_test_comp_tbl.RData` |
| `Code/Deconvolution/208-run-vars.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/208-run-vars.R` | consumes | `Code/Products/boot_test_comp_tbl.RData` |
| `Code/Deconvolution/208-run-vars.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/208-run-vars.R` | consumes | `Code/Products/boot_test_comp_tbl.RData` |
| `Code/Deconvolution/210-deconv-moments-boot.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/210-deconv-moments-boot.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/210-deconv-moments-boot.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/210-deconv-moments-boot.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/210-deconv-moments-boot.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/210-deconv-moments-boot.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/210-deconv-moments-boot.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/210-deconv-moments-boot.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/boot_deconv_mle.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/boot_tax_ev_mmt.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/boot_deconv_mle.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/boot_tax_ev_mmt.RData` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/gnr_fs_trim.csv` |
| `Code/Deconvolution/220-gnr-cd-fs.R` | consumes | `Code/Products/gnr_fs_trim_corps.csv` |
| `Code/Deconvolution/230-fs.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/230-fs.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/230-fs.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/230-fs.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/230-fs.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/230-fs.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/230-fs.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/230-fs.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/235-tims-test.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/235-tims-test.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/235-tims-test.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/235-tims-test.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/240-fs-boot.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/240-fs-boot.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/240-fs-boot.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/240-fs-boot.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/240-fs-boot.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/240-fs-boot.R` | consumes | `Code/Products/boot_fs.RData` |
| `Code/Deconvolution/240-fs-boot.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/240-fs-boot.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/240-fs-boot.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/240-fs-boot.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/240-fs-boot.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/240-fs-boot.R` | consumes | `Code/Products/boot_fs.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/250-i-elas.R` | consumes | `Code/Products/boot_tax_ev_2ttst.RData` |
| `Code/Deconvolution/251-380s-inds.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/251-380s-inds.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/251-380s-inds.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/251-380s-inds.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/251-380s-inds.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/251-380s-inds.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/251-plotting.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/251-plotting.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/255-sales-tax.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/255-sales-tax.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/255-sales-tax.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/255-sales-tax.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/255-sales-tax.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/255-sales-tax.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/255-sales-tax.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/255-sales-tax.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/270-expo.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/270-expo.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/270-expo.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/270-expo.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/280-plot-V-n-eps.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/280-plot-V-n-eps.R` | consumes | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/280-plot-V-n-eps.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/280-plot-V-n-eps.R` | consumes | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/290-bs-mle-data.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/290-bs-mle-data.R` | consumes | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/290-bs-mle-data.R` | consumes | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/290-bs-mle-data.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/290-bs-mle-data.R` | consumes | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/290-bs-mle-data.R` | consumes | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/291-bs-deconv.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/291-bs-deconv.R` | consumes | `Code/Products/np-deconv-funs.RData` |
| `Code/Deconvolution/291-bs-deconv.R` | consumes | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/291-bs-deconv.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/291-bs-deconv.R` | consumes | `Code/Products/np-deconv-funs.RData` |
| `Code/Deconvolution/291-bs-deconv.R` | consumes | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/299-to-fortran-gnr.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/299-to-fortran-gnr.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/299-to-fortran-gnr.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/299-to-fortran-gnr.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/299-to-fortran-gnr.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/299-to-fortran-gnr.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/299-to-fortran-gnr.R` | consumes | `Code/Products/intermediates.RData` |
| `Code/Deconvolution/299-to-fortran-gnr.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/deconv_prod_fun_trim.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/deconv_prod_fun_trim.RData` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/gnr-cd-me.csv` |
| `Code/Deconvolution/300-deconv-prod.R` | consumes | `Code/Products/gnr-cd-me.csv` |
| `Code/Deconvolution/425_omega.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/425_omega.R` | consumes | `Code/Products/np-deconv-funs.RData` |
| `Code/Deconvolution/425_omega.R` | consumes | `Code/Products/deconv_prod_fun.RData` |
| `Code/Deconvolution/425_omega.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/425_omega.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/425_omega.R` | consumes | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/425_omega.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/425_omega.R` | consumes | `Code/Products/np-deconv-funs.RData` |
| `Code/Deconvolution/425_omega.R` | consumes | `Code/Products/deconv_prod_fun.RData` |
| `Code/Deconvolution/425_omega.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/425_omega.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/425_omega.R` | consumes | `Code/Products/bs_mle_data.RData` |
| `Code/Deconvolution/490-boot-pf-prod.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/490-boot-pf-prod.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/490-boot-pf-prod.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/490-boot-pf-prod.R` | consumes | `Code/Products/boot_fs.RData` |
| `Code/Deconvolution/490-boot-pf-prod.R` | consumes | `Code/Products/omega_ar1_deconv_mle.RData` |
| `Code/Deconvolution/490-boot-pf-prod.R` | consumes | `Code/Products/deconv_prod_fun.RData` |
| `Code/Deconvolution/490-boot-pf-prod.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/490-boot-pf-prod.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/490-boot-pf-prod.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/490-boot-pf-prod.R` | consumes | `Code/Products/boot_fs.RData` |
| `Code/Deconvolution/490-boot-pf-prod.R` | consumes | `Code/Products/omega_ar1_deconv_mle.RData` |
| `Code/Deconvolution/490-boot-pf-prod.R` | consumes | `Code/Products/deconv_prod_fun.RData` |
| `Code/Deconvolution/510-productivity.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/510-productivity.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/510-productivity.R` | consumes | `Code/Products/omega_ar1_deconv_mle.RData` |
| `Code/Deconvolution/510-productivity.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/510-productivity.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/510-productivity.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/510-productivity.R` | consumes | `Code/Products/omega_ar1_deconv_mle.RData` |
| `Code/Deconvolution/510-productivity.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/550-np-prod.R` | consumes | `Code/Products/np-deconv-funs.Rdata` |
| `Code/Deconvolution/550-np-prod.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/550-np-prod.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/550-np-prod.R` | consumes | `Code/Products/omega_ar1_deconv_mle.RData` |
| `Code/Deconvolution/550-np-prod.R` | consumes | `Code/Products/np_productivity.RData` |
| `Code/Deconvolution/550-np-prod.R` | consumes | `Code/Products/np-deconv-funs.Rdata` |
| `Code/Deconvolution/550-np-prod.R` | consumes | `Code/Products/run-vars.RData` |
| `Code/Deconvolution/550-np-prod.R` | consumes | `Code/Products/fs.RData` |
| `Code/Deconvolution/550-np-prod.R` | consumes | `Code/Products/omega_ar1_deconv_mle.RData` |
| `Code/Deconvolution/550-np-prod.R` | consumes | `Code/Products/np_productivity.RData` |
| `Code/Deconvolution/600-mcmc-h.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/600-mcmc-h.R` | consumes | `Code/Products/mcmc_h.Rdata` |
| `Code/Deconvolution/600-mcmc-h.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/600-mcmc-h.R` | consumes | `Code/Products/mcmc_h.Rdata` |
| `Code/Deconvolution/700-replication.R` | consumes | `Code/Products/replication.RData` |
| `Code/Deconvolution/700-replication.R` | consumes | `Code/Products/replication.RData` |
| `Code/Deconvolution/700-replication.R` | consumes | `Code/Products/gnr_rep.csv` |
| `Code/Deconvolution/700-replication.R` | consumes | `Code/Products/gnr_org.csv` |
| `Code/Deconvolution/700-replication.R` | consumes | `Code/Products/gnr_rep_cd.csv` |
| `Code/Deconvolution/700-replication.R` | consumes | `Code/Products/gnr_org_cd.csv` |
| `Code/Deconvolution/700-replication.R` | consumes | `Code/Products/gnr_rep.csv` |
| `Code/Deconvolution/700-replication.R` | consumes | `Code/Products/gnr_org.csv` |
| `Code/Deconvolution/700-replication.R` | consumes | `Code/Products/gnr_rep_cd.csv` |
| `Code/Deconvolution/700-replication.R` | consumes | `Code/Products/gnr_org_cd.csv` |
| `Code/Deconvolution/900-369-trim.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/900-369-trim.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/900-369-trim.R` | consumes | `Code/Products/np-deconv-funs.RData` |
| `Code/Deconvolution/900-369-trim.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/900-369-trim.R` | consumes | `Code/Products/900-369-trim.RData` |
| `Code/Deconvolution/900-369-trim.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/900-369-trim.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/900-369-trim.R` | consumes | `Code/Products/np-deconv-funs.RData` |
| `Code/Deconvolution/900-369-trim.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/900-369-trim.R` | consumes | `Code/Products/900-369-trim.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Paper/tbls/sic_distribution.csv` |
| `Code/Deconvolution/910-all-inds.R` | consumes | `Paper/tbls/sic_distribution.csv` |
| `Code/Deconvolution/911-all-inds-2.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/911-all-inds-2.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/911-all-inds-2.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/911-all-inds-2.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/911-all-inds-2.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/911-all-inds-2.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/915-size.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/915-size.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/915-size.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/915-size.R` | consumes | `Code/Products/911-all_inds-2.RData` |
| `Code/Deconvolution/915-size.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/915-size.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/915-size.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/915-size.R` | consumes | `Code/Products/911-all_inds-2.RData` |
| `Code/Deconvolution/915.1-size.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/915.1-size.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/915.1-size.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/915.1-size.R` | consumes | `Code/Products/911-all_inds-2.RData` |
| `Code/Deconvolution/915.1-size.R` | consumes | `Code/Products/915-size.RData` |
| `Code/Deconvolution/915.1-size.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/915.1-size.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/915.1-size.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/915.1-size.R` | consumes | `Code/Products/911-all_inds-2.RData` |
| `Code/Deconvolution/915.1-size.R` | consumes | `Code/Products/915-size.RData` |
| `Code/Deconvolution/915.5-size-slides.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/915.5-size-slides.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/915.5-size-slides.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/915.5-size-slides.R` | consumes | `Code/Products/911-all_inds-2.RData` |
| `Code/Deconvolution/915.5-size-slides.R` | consumes | `Code/Products/915-size.RData` |
| `Code/Deconvolution/915.5-size-slides.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/915.5-size-slides.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/915.5-size-slides.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/915.5-size-slides.R` | consumes | `Code/Products/911-all_inds-2.RData` |
| `Code/Deconvolution/915.5-size-slides.R` | consumes | `Code/Products/915-size.RData` |
| `Code/Deconvolution/917-size.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/917-size.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/917-size.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/917-size.R` | consumes | `Code/Products/915-size.RData` |
| `Code/Deconvolution/917-size.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/917-size.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/917-size.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/917-size.R` | consumes | `Code/Products/915-size.RData` |
| `Code/Deconvolution/917.5-size-slides.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/917.5-size-slides.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/917.5-size-slides.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/917.5-size-slides.R` | consumes | `Code/Products/915-size.RData` |
| `Code/Deconvolution/917.5-size-slides.R` | consumes | `Code/Products/917-size.RData` |
| `Code/Deconvolution/917.5-size-slides.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/917.5-size-slides.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/917.5-size-slides.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/917.5-size-slides.R` | consumes | `Code/Products/915-size.RData` |
| `Code/Deconvolution/917.5-size-slides.R` | consumes | `Code/Products/917-size.RData` |
| `Code/Deconvolution/920-DD.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/920-DD.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/920-DD.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/920-DD.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/920-DD.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/920-DD.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/920-DD.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/920-DD.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/920-DD.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/920-DD.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/921-DD2.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/921-DD2.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/921-DD2.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/921-DD2.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/921-DD2.R` | consumes | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/921-DD2.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/921-DD2.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/921-DD2.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/921-DD2.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/921-DD2.R` | consumes | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/921.1-DD.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/921.1-DD.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/921.1-DD.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/921.1-DD.R` | consumes | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/921.1-DD.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/921.1-DD.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/921.1-DD.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/921.1-DD.R` | consumes | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/921.2-het-slides.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/921.2-het-slides.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/921.2-het-slides.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/921.2-het-slides.R` | consumes | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/921.2-het-slides.R` | consumes | `Code/Products/921.1-DD.RData` |
| `Code/Deconvolution/921.2-het-slides.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/921.2-het-slides.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/921.2-het-slides.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/921.2-het-slides.R` | consumes | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/921.2-het-slides.R` | consumes | `Code/Products/921.1-DD.RData` |
| `Code/Deconvolution/921.5-DD2.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/921.5-DD2.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/921.5-DD2.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/921.5-DD2.R` | consumes | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/921.5-DD2.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/921.5-DD2.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/921.5-DD2.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/921.5-DD2.R` | consumes | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/922-DD2-plot.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/922-DD2-plot.R` | consumes | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/922-DD2-plot.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/922-DD2-plot.R` | consumes | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/923-DiD-trends.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/923-DiD-trends.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/923-DiD-trends.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/923-DiD-trends.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/923-DiD-trends.R` | consumes | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/923-DiD-trends.R` | consumes | `Code/Products/colombia_data.RData` |
| `Code/Deconvolution/923-DiD-trends.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/923-DiD-trends.R` | consumes | `Code/Products/910-reg-results.RData` |
| `Code/Deconvolution/923-DiD-trends.R` | consumes | `Code/Products/i_elas.RData` |
| `Code/Deconvolution/923-DiD-trends.R` | consumes | `Code/Products/921-DD2.RData` |
| `Code/Deconvolution/930-boot-se-het.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/930-boot-se-het.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/930-boot-se-het.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/930-boot-se-het.R` | consumes | `Code/Products/915.1-size.RData` |
| `Code/Deconvolution/930-boot-se-het.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/930-boot-se-het.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/930-boot-se-het.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/930-boot-se-het.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/930-boot-se-het.R` | consumes | `Code/Products/915.1-size.RData` |
| `Code/Deconvolution/930-boot-se-het.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/930.1-fs-se-het.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/930.1-fs-se-het.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/930.1-fs-se-het.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/930.1-fs-se-het.R` | consumes | `Code/Products/915.1-size.RData` |
| `Code/Deconvolution/930.1-fs-se-het.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/930.1-fs-se-het.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/930.1-fs-se-het.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/930.1-fs-se-het.R` | consumes | `Code/Products/915.1-size.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/930.2-boot-se-het.R` | consumes | `Code/Products/915.1-size.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | consumes | `Code/Products/930.1-fs-se-het.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | consumes | `Code/Products/930.2-boot-se-het.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/930.2-boot-se-het.R` | consumes | `Code/Products/915.1-size.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | consumes | `Code/Products/930.1-fs-se-het.RData` |
| `Code/Deconvolution/930.2-boot-se-het.R` | consumes | `Code/Products/930.2-boot-se-het.RData` |
| `Code/Deconvolution/930.5-boot-se-het.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/930.5-boot-se-het.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/930.5-boot-se-het.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/930.5-boot-se-het.R` | consumes | `Code/Products/915.1-size.RData` |
| `Code/Deconvolution/930.5-boot-se-het.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/930.5-boot-se-het.R` | consumes | `Code/Products/930.5-boot-se-het.RData` |
| `Code/Deconvolution/930.5-boot-se-het.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/930.5-boot-se-het.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/930.5-boot-se-het.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/930.5-boot-se-het.R` | consumes | `Code/Products/915.1-size.RData` |
| `Code/Deconvolution/930.5-boot-se-het.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/930.5-boot-se-het.R` | consumes | `Code/Products/930.5-boot-se-het.RData` |
| `Code/Deconvolution/930.6-boot-se-het.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/930.6-boot-se-het.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/930.6-boot-se-het.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/930.6-boot-se-het.R` | consumes | `Code/Products/915.1-size.RData` |
| `Code/Deconvolution/930.6-boot-se-het.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/930.6-boot-se-het.R` | consumes | `Code/Products/930.6-boot-se-het.RData` |
| `Code/Deconvolution/930.6-boot-se-het.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/930.6-boot-se-het.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/930.6-boot-se-het.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/930.6-boot-se-het.R` | consumes | `Code/Products/915.1-size.RData` |
| `Code/Deconvolution/930.6-boot-se-het.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/930.6-boot-se-het.R` | consumes | `Code/Products/930.6-boot-se-het.RData` |
| `Code/Deconvolution/931-boot-se-marg.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/931-boot-se-marg.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/931-boot-se-marg.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/931-boot-se-marg.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/931-boot-se-marg.R` | consumes | `Code/Products/931-boot-se-marg.RData` |
| `Code/Deconvolution/931-boot-se-marg.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/931-boot-se-marg.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/931-boot-se-marg.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/931-boot-se-marg.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/931-boot-se-marg.R` | consumes | `Code/Products/931-boot-se-marg.RData` |
| `Code/Deconvolution/932-avg-elas.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/932-avg-elas.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/932-avg-elas.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/932-avg-elas.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/932-avg-elas.R` | consumes | `Code/Products/931-boot-se-marg.RData` |
| `Code/Deconvolution/932-avg-elas.R` | consumes | `Code/Products/932-avg-elas.RData` |
| `Code/Deconvolution/932-avg-elas.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/932-avg-elas.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/932-avg-elas.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/932-avg-elas.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/932-avg-elas.R` | consumes | `Code/Products/931-boot-se-marg.RData` |
| `Code/Deconvolution/932-avg-elas.R` | consumes | `Code/Products/932-avg-elas.RData` |
| `Code/Deconvolution/932.5-avg-elas.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/932.5-avg-elas.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/932.5-avg-elas.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/932.5-avg-elas.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/932.5-avg-elas.R` | consumes | `Code/Products/931-boot-se-marg.RData` |
| `Code/Deconvolution/932.5-avg-elas.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/932.5-avg-elas.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/932.5-avg-elas.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/932.5-avg-elas.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/932.5-avg-elas.R` | consumes | `Code/Products/931-boot-se-marg.RData` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | consumes | `Code/Products/931-boot-se-marg.RData` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/932.6-avg-elas-no-xi.R` | consumes | `Code/Products/931-boot-se-marg.RData` |
| `Code/Deconvolution/935-ev-loc.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/935-ev-loc.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/935-ev-loc.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/935-ev-loc.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/940-corp-uninc.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/940-corp-uninc.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/940-corp-uninc.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/940-corp-uninc.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/940-corp-uninc.R` | consumes | `Code/Products/test_data.RData` |
| `Code/Deconvolution/940-corp-uninc.R` | consumes | `Code/Products/global_vars.RData` |
| `Code/Deconvolution/940-corp-uninc.R` | consumes | `Code/Products/deconv_funs.Rdata` |
| `Code/Deconvolution/940-corp-uninc.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/950-deconv-size.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/950-deconv-size.R` | consumes | `Code/Products/930-boot-se-het.RData` |
| `Code/Deconvolution/950-deconv-size.R` | consumes | `Code/Deconvolution/030-np-deconv-funs.R` |
| `Code/Deconvolution/950-deconv-size.R` | consumes | `Code/Deconvolution/030-np-deconv-funs.R` |
| `Code/Thesis/ch06-pf-comparison.R` | consumes | `Code/Products/1476-pf-joint-gmm-bootstrap.RData` |
| `Code/Thesis/ch06-pf-comparison.R` | consumes | `Code/Products/deconv_prod_fun_trim.RData` |
| `Code/Thesis/ch06-pf-comparison.R` | consumes | `Code/Products/1476-pf-joint-gmm-bootstrap.RData` |
| `Code/Thesis/ch06-pf-comparison.R` | consumes | `Code/Products/deconv_prod_fun_trim.RData` |
| `Code/Thesis/ch06-pf-comparison.R` | consumes | `Code/Products/1478-pf-joint-testinv-summary-cons.csv` |
| `Code/Thesis/ch06-pf-comparison.R` | consumes | `Code/Products/1478-pf-joint-testinv-summary-cons.csv` |
| `Code/Thesis/ch06-pf-comparison.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Thesis/ch06-pf-comparison.R` | consumes | `Code/Deconvolution/050-render-tbls.R` |
| `Code/Thesis/ch06-pf-testinv-regions.R` | consumes | `Code/Products/1477-pf-joint-testinv-grid.csv` |
| `Code/Thesis/ch06-pf-testinv-regions.R` | consumes | `Code/Products/1477-pf-joint-testinv-grid.csv` |

</details>
