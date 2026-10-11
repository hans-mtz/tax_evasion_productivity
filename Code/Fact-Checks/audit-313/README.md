# Audit of industry 313 (Beverages), 2026-10-10/11

Why 313's mean V rose from the approved test's 0.176 to the current 0.390, and the deconvolved E[e/M] to 48 percent. Findings: `Research-log/log.md`, 2026-10-11. Read-only with respect to repo products; run from the repo root (`Rscript Code/Fact-Checks/audit-313/<script>`).

- `01-samples.R`: old `test_data` vs `colombia_data_frame`, 313 (same rows, shares, codes).
- `02-decomp.R` → `02-decomp.csv`: mean V and the sharp region, step by step from the approved test to the current 1510 test (net share, denominators, codes 6–9), nine rejecting industries.
- `03-groups313.R`: 313 groups (corporations, unincorporated, code 8, code 6); code-8 plants.
- `04-productmix.R` → `04-productmix.csv`: 4-digit product-mix diagnostics of the benchmark (reweighting, 4-digit FE).
- `05-taxes-stage2.R`: τ_S and τ_P by group and 4-digit, net-share effect of code 8, test vs stage-2 sample.
- `06-old-fits.R`: old deconvolution fits (292, pooled).
- `07-mix369-351.R`: product mix in 369 and 351.
- `08-ranking-taxes.R` → `08-ranking.csv`, `08-taxes-by-jo.csv`: industry rankings (firm-years, real sales) under codes 0–5 vs all codes; taxes paid by juridical code.
- `09-variance-ratio.R` → `09-variance-ratio.csv`: Var(V | unincorporated) / Var(V | corporation) by industry with a (plant, legal form) bootstrap (the model implies ≥ 1).
