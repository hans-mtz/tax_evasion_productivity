# ELVIS code review 4 (2026-10-01): today's changes to `grid_estimator.cpp` / `1200-stage2-elvis-common.h`

Scope: the 10 items listed in the request, and only those. Source files at their 00:02:59 state; repo binaries `grid_estimator_s2` and `_ind5b` were built after that (00:03), so they match the source. Scratch builds and tests were in `/tmp/elvis-rev4/` (s2 and ind5b flags as in the Makefile; `unit.cpp` and `unit2.cpp` include `grid_estimator.cpp` with `main` renamed). Test inputs: `int.csv` (315 interior firms from 1598, 35 per industry) and `mix.csv` (those 315 plus 120 corner firms from 1596, 60 inside the 9 industries and 60 outside). At most 2 threads. No repository file was changed.

## 1. Summary by severity

**BUG: none found.** Every equivalence test passed (list below). Nothing found invalidates the four 1600 jobs now running.

**RISK**
- R1 (item 8): `audit_p` is only checked against `< 1`. Under `power_nokink` the support caps q at x < c_k, so q < c_k^k = 1/(1+k). At k = 0.75 the cap is 0.571, so p = 0.53 is 93% of the maximum. The moment can only hold if group-G firms sit right at the FOC ceiling (B about 0.07 on average). For k ≥ 0.887 the moment cannot hold at all, and the run would simply reject. At the 1599 start point, row 10 has mean −0.048 (t = −13.5, group k), which means q in G is about 0.05. Watch CEILING EDGE in the 1600 ii_k / ii_v adiag files.
- R2 (items 7, 8): there is no data check for the new input columns. If `audit_g` / `audit_gv` / `umed` are missing, they default to 0 / 0 / −1, so the audit row or the median rows are zero for every firm and get eigen-cut without any message. Verified: `audit_p=0.53` on the 1592 input, which has no `audit_g`, gives row 10 = 0, se 0. The 1600 runs use 1598, which has all three columns, so they are not affected.
- R3 (item 4): when κ is estimated, the start is `x[5] = lambda` (grid value). If `lambdas` falls outside [0.02, kappa_max], NLopt returns −2 (invalid arguments) with 0 evaluations. A CSV row with Lhat = inf is still written, and nothing fails. Verified with `lambdas=25 kappa_max=20`. The 1597 and 1600 starts (κ = 1.82) are inside the bounds.
- R4 (item 3): with `inner_algo=neldermead`, the inner solve almost never stops on its tolerance. It uses `xtol_rel 1e-6` with no ftol, and γ starts at 0. In the test, 29 of 30 inner solves hit the cap of 200 × nf, about 2000 evaluations each. The counters work and report this, but each outer evaluation costs the full budget.
- R5 (item 9, inferred, not tested): the median targets come from the 1521 deconvolution sample (the test's sample, which includes firms with τ_P = 0 and has its own trimming). They are imposed on the stage-2 interior sample (τ_P > 0, top 0.5% of M* trimmed). If f_u differs between these two populations, the median rows are misspecified. The same applies to G: it is defined after the M* trim, so the very largest firms are not in the top 10%.

**MINOR**
- M1 (item 1): 41% of the edge component's draws are redrawn at the floor. `MIX_EDGE_EPS = 1e-10` is far below the floor's reach, which is d/W ≲ floor/k ≈ 1.3e-6. This costs efficiency only; the estimand is unaffected (mean IS weight 1.159 = 1/(1 − 0.412/3), as predicted).
- M2 (item 1): the switch from 2 to 3 components happens at the firm-specific κ where lo crosses 0. There the weights jump from 1/2 to 1/3 and the common-random-number draws are reassigned. The simulated objective therefore has small jumps in κ, of Monte Carlo size. With lo = 0, a third component (log-uniform on [εM*, M*)) would remove them.
- M3: the startup message still says "proposal=mix: 50/50 ..." (line 3994).
- M4 (item 5): some options are accepted and silently ignored. `inner_algo` and `inner_start` are only parsed when `nested=1`, so a typo such as `inner_algo=foo` passes outside nested mode. `mix_umax` without `proposal=mix` is also ignored. The legacy diagnostics `gammagdiag`, `accepttraj`, `psitraj`, `acceptdiag` and `redrawdiag` are not in the refusal list (line 4195), so they accept `sampler=is` / `qform` 4–5 and go on to parse `par`. They are unused.
- M5 (item 10): `ii_v` reuses the group-k D for row 10 (0.0888); the group-v value is about 18% higher in the test. This is harmless for the estimand (Remark 2.3), but ii_k and ii_v are then not each using "its own row's sd".
- M6 (item 7): the industry rows sum exactly to pooled row 1 only when every firm has an industry (jidx ≥ 0). On an input with corner firms outside the 9 industries (1596 "all"), design i's `drop_rows=1` would remove the ε restriction for those firms. 1600 uses the interior-only input, so it is not affected.
- M7 (item 2): `inline double g_h_floor_power` is a C++17 inline variable, and the header is also included by `Code/Rcpp/1200-stage2-elvis*.cpp`. Under C++11/14 that gives a compiler warning (clang) or an error. Not tested.
- M8 (item 8): `audit_p` is parsed inside the `KINK_S` block (lines 3925–3930). A KINK-only build would ignore it silently. No such binary is in use.

**OK: verified by test**
- Every one of these checks was exactly identical unless noted:
  - default vs `h_floor=1e-6`;
  - adiag with 1 vs 2 threads;
  - nested and joint lambdagrid with 1 vs 2 threads (identical CSV apart from `point_seconds`);
  - adiag reproduces each fit's Lhat (joint, nested-NM and nested-LBFGS, with the audit row on);
  - adiag reproduces yesterday's ladder adiag `1597-drop12-7-5-nocorner-adiag-R1000.txt` on the full 12,050-firm input (only the input path differs);
  - `ind_rows=epslnm` reproduces 1588's adiag Lhat 0.0460337 bit for bit (with `seed=add`, the default at that time);
  - industry ε rows sum to row 1 (int.csv: −0.0333100 vs −0.03331);
  - nestedcheck: the cached L equals the regular IS objective to within 1e-8 relative (float cache), with analytic and finite-difference gradients agreeing, for both audit on and `ind_rows=median` on mix.csv.

## 2. Per item

**1. `is_draw` 3-component mixture** (lines 973–1006). *Read and tested.*
- The densities are correct:
  - uniform: p_u = 1/W on (lo, M*];
  - log-uniform: p_l = 1/(U·M) for u < U, else 0;
  - edge: p_e = 1/(LE·d) for d = M − lo ≥ εW, else 0.
- Weights are 1/3 each when lo > 0 and 1/2, 1/2 when lo = 0 (the edge component is never selected then). lw = log(p_u / mixture).
- The floor-edge redraw truncates the mixture and the target to the same set, B > `g_h_floor_power`, using the same expression as `draw_from_rho_power_scale`. The resulting constant cancels in the per-firm self-normalized average.
- Estimand: I checked the weighted CDF at 0.01, 0.1, 0.5 and 0.9 of the support against the uniform, using 60 seeds × 2M draws with lo > 0. All z-scores were below 1.6. The lo = 0 case (κ = 3) agrees too, and E[M²] and E[ln M] match their closed forms.
- Edge cases behave correctly:
  - very small κ (lo just below M*): all draws stay inside the support;
  - u ≥ U: p_l = 0;
  - d < εW: p_e = 0.
- Findings: M1, M2, M3.

**2. `g_h_floor_power` / `h_floor`** (header lines 67–70, 286, 304; cpp lines 998, 4040–4042). *Read and tested.*
- Every power-form use goes through the variable: `B_power_scale` (so h, h' and the κ score), `draw_from_rho_power_scale`, and `is_draw`.
- `h_denom` and `B_exp_scale` keep `H_DENOM_FLOOR`; they are not power forms.
- The parse happens before any computation. The default reproduces the old floor (identical output); `1e-4` moves Lhat slightly, as expected.
- The CEILING EDGE threshold of 1e-3 sits on top of the floored B, which is fine as long as the floor stays below 1e-3.
- Finding: M7.

**3. Nested inner NM; `inner_cap` / `inner_fail`; CSV** (lines 2797–2819, 2853–2857, 2945; writer 3136–3158). *Read and tested.*
- Initial steps are 0.2/D_t on the free γ rows; the budget is 200 × nf. Counts accumulate over passes.
- Non-nested fits default-initialise `inner_cap` and `inner_fail` to −1.
- Header and row column counts match: 31 columns in s2 and 40 in ind5b (checked with awk and R).
- The diff shows that only the lambdagrid header changed; no other mode's writer was touched.
- Finding: R4.

**4. `kappa_max`** (lines 2883, 4586–4587). *Read and tested.*
- It is parsed inside the lambdagrid branch, before `run_lambdagrid_mode`. Lambdagrid is the only mode that estimates κ, nested mode included.
- adiag, rhoD and nestedcheck accept the option and ignore it; they do no estimation.
- Finding: R3.

**5. Guards** (lines 3988–4046, 4195–4201, 4583–4584). *Tested:*
- `proposal=mix` with `sampler=mh` is refused.
- `nested=1` without IS is refused.
- `grid3d` with IS is refused.
- `power_nokink` with uniform ρ is refused, except in `mode=rhoD`, which runs.
- `drop_rows=10` is refused.
- `ind_rows` on s2 is refused.
- `n_passes=1` outside nested mode prints the message and runs 2 passes.
- The new options are refused indirectly in the counterfactual / grid3d / deltagrid / shell / flat modes: audit and `ind_rows` require qform 5 or IND5, and the KINK builds force qform 4 or 5, which those modes refuse. `h_floor` is inert there (linear q).
- Finding: M4.

**6. adiag CEILING EDGE / TAIL** (lines 2327, 2400–2406). *Read and run.*
- CEILING EDGE is the tilted mass with B < 1e-3, filled only in the IS branch (under MH it prints 0, and the label says "IS only").
- The TAIL label is correct for both power_nokink and the kink.

**7. Industry rows (IND5)** (lines 1026–1032, 1104–1109, 2366–2367). *Read, unit-tested and run.*
- The moment function gives each mode its documented formula: ε·lnM, ε, or (1{u ≤ umed} − ½), with 0 when umed = −1, and only the firm's own industry row is non-zero.
- Corner firms (M = M*, u = 0) get +½ in median mode, which matches the formula.
- `a_tail` adds −γ_{13+j} only in epslnm mode. That is correct: ε grows linearly in u and the median row is bounded.
- The default mode reproduces 1588 exactly. In the 1598 data, all 7 medians are positive and 351/352 are −1, which correspond to rows 19 and 20 in jidx order 313, 321, 322, 324, 331, 342, 351, 352, 369.
- Finding: M6.

**8. Audit row** (lines 1043, 3925–3930, 4031–4033). *Unit-tested and run.*
- Row 10 equals `audit_in·(x^k − p)` with x = e/(κ·M̄), matching a direct computation at four values of M, including just above lo, and giving 0 when `audit_in = 0`.
- Every interior draw reaches that line. The beyond-kink early return (row 10 = 1 − s) cannot fire under qform 5: the draw rejects B ≤ floor, which implies x < c_k, and both places compute x from the identical expression.
- `audit_group=v` swaps the group for all firms before any mode runs. In rhoD, only row 10 changes between k and v and between audit off and on.
- Row 10 is auto-dropped when the audit row is off (D printed as 0). It cannot be dropped through `drop_rows`. With the audit row on, it needs D > 0, and the code enforces that.
- There are 11 call sites with the new `umed_in` / `audit_in` arguments, and all pass `f.umed` and `f.audit_g`: lines 1127, 1132, 1143, 1147, 2229, 2303, 2305, 2313, 2339, 2661, 2664.
- Findings: R1, R2, M8.

**9. Input builders** (`1596-input-plant-all.R`, `1598-input-designs.R`). *Read and checked on data.*
- Both rebuild the 1532 `export("designA")` filters and trim line by line, so the row_id order is the same. The joins are checked on M* and V, plus sic and year in 1596, and in 1598 additionally require a finite k.
- 1596's interior row_ids equal 1598's exactly.
- Each group (audit_g, audit_gv) is about 10.0–10.3% per industry (≥ the quantile, so ties are included). It is computed within industry, pooled over years, on the trimmed interior sample.
- Median extraction: the density f_e.np is clipped at 0 and integrated by Riemann sum on 20,001 points, and the median is read off the normalized CDF, which is adequate. The names "313 log_mats_share_net" etc. map to sic_3 correctly.
- Finding: R5.

**10. Run scripts.** *Read; the arithmetic was checked by hand.*
- 1597:
  - the count of live rows is 12 (rows 0–9, 11, 12) minus the dropped rows, and θ has 4 free entries (δ0, δ1, δ2, κ), so maxeval = 400 × (4 + live rows);
  - `kappa_max=20` from rung 3 on;
  - one D, computed on 1596 with all rows live;
  - the binary, inputs, seed, `proposal=mix`, `cluster=plant` and adiag at 1000/4000 all match the header.
- 1600, free dimensions per fit:

  | Fit | Free dims | Breakdown |
  |---|---|---|
  | i | 21 | 4 + 8 base rows + 9 industry rows |
  | i_med | 20 | 4 + 9 + 7 |
  | ii_k, ii_v | 14 | 4 + 10 |

  - x0 lengths are 19 and 28.
- D splicing:
  - the median rows are fields 14–22 of rhoD with rows 19 and 20 set to 1;
  - rows 0–12 come from the ladder D (1597) via 1599, and row 10 is set to 0 in i_med, where the auto-drop accepts a 0;
  - DA = ladder D with row 10 from rhoD (group k);
  - DI = DA plus the ε-industry D values.
- Each fit uses the binary that matches its design.
- Finding: M5.

## 3. Prioritized fixes (not implemented)

1. R1: refuse `audit_p ≥ 1/(1+k)` under power_nokink, or warn when it is close. Also decide whether p = 0.53 at k = 0.75 is a sensible target given the 0.571 cap; at minimum, read CEILING EDGE in the 1600 ii adiag output.
2. R2: when `audit_p ≥ 0`, require the `audit_g` column, and the `audit_gv` column when `audit_group=v`. When `ind_rows=median`, require `umed` and at least one firm with umed ≥ 0. The check belongs next to the existing sig2eps check (line 4045).
3. R3: refuse `lambdas` outside [0.02, kappa_max] in κ-free builds, and fail rather than write Lhat = inf when NLopt returns a negative code with 0 evaluations.
4. R4: give the inner NM a stopping rule it can actually reach, for example `ftol_rel` / `ftol_abs` or an absolute `xtol` on γ; the relative `xtol` never triggers from γ = 0.
5. R5: document or align the population behind the median targets (deconvolution sample vs stage-2 interior sample), and the trim's effect on G.
6. M1/M2: set `MIX_EDGE_EPS` to about floor/k (for example 1e-7) and use 3 constant components even when lo = 0. This changes the draws, so do it only between compared run sets.
7. M3–M8: fix the "50/50" message; parse `inner_algo` / `inner_start` / `mix_umax` unconditionally and refuse them when unused; add the five legacy diagnostics to the refusal list; give ii_v its own row-10 D if a strict like-for-like is wanted; guard `ind_rows=eps` with `drop_rows=1` on inputs that contain corner firms with jidx = −1.
