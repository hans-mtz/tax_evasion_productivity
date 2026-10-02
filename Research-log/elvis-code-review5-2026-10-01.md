# ELVIS code review 5 (2026-10-01): null guard, D = inf, γ step, gamma_init=solve

**Scope.** `diff -u /Users/hans/.claude/jobs/0786c312/tmp/grid_estimator.cpp.bak-1611 Code/C-estimator/grid_estimator.cpp` (176 diff lines) and the code it calls. No source files were edited.

**Scratch.** All scratch files are in `/Users/hans/.claude/jobs/0786c312/tmp/rev5/`:
- binaries `ge_*`, built with each Makefile target's flags, plus `ge_ind5b_old`, built from the backup;
- scripts `wi.sh`, `nc.sh`, `reg.sh`, `scan.sh`, `idn2.sh`;
- outputs `*.out` and `*.csv`.

Every run used 2 threads or fewer and `LC_ALL=C`. The only `lambdagrid` runs used `maxeval=1`.

---

## Findings, ranked by severity

### 1. MEDIUM: the relative null threshold fires on regular directions in the outer δ box
`null_eig` (l. 894) tests `w_k < 1e-12 * max_eig`. That is the relative-cut logic CLAUDE.md calls a porting error, now returning a 1e10 wall instead of dropping the direction. The ψ rows grow with δ, `max_eig` grows with them, and the ε·score_κ direction (row 11, w ≈ 1e-3 at every δ) falls below the threshold.

Reproduction: design i, `ge_ind5b`, `adiag`/`nestedcheck` at the 1604-i-k0.7 point with one δ changed (`scan.sh`, `reg.sh`). All values are inside the `delta_max=100` box the running fits use.

| point | max eig | smallest eig | ratio | old L̂ (backup binary) | new L̂ |
|---|---|---|---|---|---|
| fit (δ2 = 3.01) | 2.87e4 | 8.99e-4 | 3.1e-8 | 0.002543785831 | 0.002543785831 |
| δ2 = 60 | 8.24e8 | 1.72e-3 | 2.1e-12 | – | 21.96 |
| δ2 = 80 | 1.49e9 | 1.15e-3 | 7.7e-13 | – | **1e10** |
| δ2 = 100 | 2.35e9 | 8.07e-4 | 3.4e-13 | 28.58 | **1e10** |
| δ1 = +100 | 5.32e7 | 7.85e-6 | 1.5e-13 | – | **1e10** |

In each case the "NULL" direction loads 0.82–0.98 on row 11, and its z'd is −0.004 to −0.010, which is not a numerical zero.

**What this means.**
- The current optima (δ1 ≈ 21, δ2 ≈ 3) are far from the threshold, so the minimizer is not affected today.
- The objective in the outer box becomes a flat 1e10 plateau where it used to be an ordered 20–600. A Nelder–Mead simplex that expands there loses its ordering. The ratio also depends on the row scaling, so another design or a rescaled row can bring the threshold closer to the operating region.
- The 1e-12 threshold is about 4 orders of magnitude above the true numerical noise floor:
  - At the 1605 point the genuine null eigenvalue is 3.8e-13 with max 3.8e4. That is a ratio of 1e-17. The dsyevr accuracy is eps·‖Ω‖ ≈ 8e-12 in absolute terms, or 2e-16 relative.

**Fix (either):**
- (a) **Preferred.** Do the null test on the equilibrated matrix S⁻¹ΩS⁻¹ with S = diag(√Ω_ii), and rescale d the same way. The CUE form is unchanged when Ω has full rank, and the test becomes scale-invariant.
- (b) Lower `NULL_EIG_REL` to about 1e-14, which is still about 50× the noise floor. None of the rows above would fire. Recheck with `adiag` at the box corners.

### 2. LOW–MEDIUM: the penalty is discontinuous at the threshold and has no gradient
- **Jump at the threshold.** An eigenvalue just above `1e-12·max` with a small |z'd| is kept and contributes 0.5 z'd²/w, which can be tiny. Just below the threshold the objective is 1e10. Example with max = 3.8e4 and z'd = 1e-6: the contribution is 1.3e-5 on one side of the threshold and 1e10 on the other.
- **Flat plateau.** Inside the penalty region both paths are flat: `nested_L` returns a zero gradient (l. 2776), so L-BFGS stops immediately, and a Nelder–Mead simplex with every vertex there stalls.
  - The log already shows this for `gamma_init` with the finite D_med ("stuck at the 1e10 penalty"). The keep-the-start branch (l. 3072) handles it, but the joint NM then starts on the plateau.
  - S1 (D = inf) avoids the plateau in iib. Any other design that hits a genuine null direction will stall the same way.
- **Option, if Hans wants a slope.** For a violated null direction, use 0.5 z'd² / (NULL_EIG_REL·max). In other words, floor the eigenvalue at the threshold instead of returning a constant. This joins the kept branch continuously at the threshold and gives L-BFGS a gradient. For the 1605 configuration it gives 0.0278/(2·3.8e-8) ≈ 3.7e5 (TS ≈ 9e9), not 1e10. That is still a clear rejection; scale the floor down if a larger value is wanted. If the constant 1e10 is kept, record in the code comment that the penalty region is flat on purpose.

### 3. LOW: `rho_D` accepts inf on any live row, including the unbounded ψ and ε rows
- The parse check at l. 4121 is `!(g_rhoD[t] > 0)`, and inf passes it. An inf on ψ or ε removes the quadratic term that makes the tilt proper under `qform=power_nokink`, which is exactly what `rho=uniform` is refused for at l. 4127. The run would then go ahead with no error.
- Only `mode=rhoD` writes inf, and only for the median rows, so nothing is wrong today. The risk is a hand-edited D file, which is how `1610-rhoD-med-boundedout.txt` was assembled: its rows 0–12 are copied from `1599-rhoD-ind.txt`.
- **Fix:** accept inf only on rows 13..13+N_IND−1 when `ind_rows=median` (IND5), and refuse it elsewhere.

### 4. LOW: `gamma_init=solve` is silently ignored under `nested=1`, and nothing about it reaches the output CSV
- The warm start sits in the joint branch, after the `g_nested` branch returns (l. 2966 vs l. 3058). The CLI accepts the combination without a warning.
- The csv records neither f0, f1 nor the warm-start γ; only stdout has them (as the 6-digit print). With `x0` γ = 0 the run is reproducible from its inputs, but the csv alone does not show that the warm start ran.
- **Fix:** refuse or warn on `nested=1 gamma_init=solve`, and add a `gamma_init_L0,gamma_init_L1` pair of csv columns.

### 5. LOW (latent; not a Makefile target): under YEAR_FE, the warm-start cache uses stale year intercepts
- `nested_set_theta` (l. 2898) sets `g_kpow`, `g_kshare` and κ but not `g_d0yr`. Under `-DYEAR_FE`, which still compiles, `gamma_init` would build the cache at whatever `g_d0yr` holds, which is 0 before the first joint evaluation, not x0's intercepts.
- The nested outer objective has the same gap; it predates this diff.
- **Fix:** set `g_d0yr` in `nested_set_theta`, or refuse `gamma_init`/`nested` under YEAR_FE.

### 6. LOW (latent): `nested_inner_obj` zeroes every γ row that is not free
- `nested_inner_obj` (l. 2850) starts from `gam[] = {0}` and fills only the free rows. Rows pinned by equal bounds are therefore evaluated at 0, not at their pinned value.
- This is correct today, because the only pinned γ rows are `drop_rows` and they are pinned at 0 (l. 2960). If a γ row is ever pinned at a non-zero value, `gamma_init` would optimize a different objective from the joint NM.
- **Fix:** carry the full start vector into `NestedInner` and overwrite only the free entries. The f0 at l. 3064 already uses the full `gam`.

### 7. INFO
- **The γ step 0.4 for D = inf rows does not follow the pooled-sd convention.** It is 0.2/0.5, with 0.5 the within-industry maximum sd of a ±½ indicator. The other rows use 0.2/(pooled sd). The pooled sd of a median row is 0.5·√p_j ≈ 0.12–0.3, which would give steps of 0.7–1.7. Harmless after the warm start, where the warm-started median γ's are −34 … +10.
- **Memory.** The `gamma_init` cache is n·R·D_G_A floats: 12,050 × 1,000 × 22 × 4 B = 1.06 GB, or 4.2 GB at R = 4000. It is transient: it is freed at the end of the `if` block, before pass 1. Two concurrent fits peak at about 2 GB extra each during their first ~45 s.
- **`nestedcheck` FD step at large γ.** The step is h = 1e-4·max(1,|γ|), which is too coarse at the warm-started point (|γ| up to 172, gradient about 1e-7). Rows 3, 6 and 9 show sign mismatches there (FD truncation error near a stationary point). At γ = 0 the analytic gradient agrees with FD to a relative error of ≤ 2e-4 on every row (`nc_iib_0.out`). This is not a gradient bug.
- **Other objectives without the guard.** The moment-set-C and `revgrid` objectives (l. 360, 1543, 2115) do not have the guard. They are out of scope, but `revgrid` is the counterfactual engine.
- **Stale comments.** The comment at l. 150 still says γ step = 0.2/D_t. The adiag header comment still says "w_k > 1e-8 * max".

---

## Checks that passed (with evidence)

**Build.** All six Makefile targets compile from the current source with their exact flags: `grid_estimator`, `_eps_ak2`, `_kf3`, `_ind5k`, `_ind5b`, `_s2`. Logs are in `build_*.log`. There are no new warnings; the base target's two `g_audit_*` unused-variable warnings are on lines 836–837, outside the diff. `-DYEAR_FE` and `-DTAU_ROW` also compile. `-DKINK` alone hits the intentional `#error`, which predates the diff.

**Regression without the new options** (`reg.sh`, nestedcheck, 10 digits):
- At the design-i fit, old and new binaries agree bit for bit: 0.002543785831, nested cache 0.00254378585.
- The restructured loops keep the arithmetic order of the kept terms. `gamma_step` equals the old 0.2/D whenever D is finite, and equals 0.2 under `rho=uniform`.
- So old runs reproduce exactly unless Ω had a null direction.

**The guard is consistent across `cue_objective_A_std`, `nested_L` and adiag:**
- Same criterion, same `dnorm`, same eigen call. adiag's L̂ comes from `cue_objective_A_std`, and its NULL lines use the same test.
- At the 1605 point with the old D:
  - old binary: regular 1.055e10 vs nested 9.62e9, a 9 percent disagreement between the two code paths (the lottery);
  - new binary: exactly 1e10 on both paths.
- With D_med = inf: 0.1494086007 on both paths.

**The skip branch for exact identities is robust in practice.** Test: design i with row 1 live (`drop_rows=12,7,5`), so Σ_j ε·1{j} = ε exactly (`idn2.sh`).
- The null direction is skipped at every tested δ.
- The margin 1e-8·max(1,‖d‖)/|z'd| is ≥ 1.7e3, including δ2 = 60, where z'd = −7.8e-8 from eigenvector mixing.
- The ‖d‖ factor (dominated by the ψ rows) scales with that error.

**inf in `rho_D` flows safely.**
- `rho_Q` computes (g − g0)/inf = 0. g is bounded and g0 = 0.5, so 0·inf and inf − inf never occur.
- The `>0` validation passes.
- `gamma_step` returns 0.4.
- `g_rhoD` has no other reader: no printing, and no division outside `rho_Q`.
- `mode=rhoD` prints inf on rows 13–21 for iib, prints 0 on dropped rows, and leaves design i's rows finite (`rhod.out`, `rhod_i.out`).

**`gamma_init` objective = joint objective.**
- The cache is built after `x[3] = g_kfixed` and `x[5] = lambda` (KAPPA_FREE) are written (l. 2958–2963). So k, s and κ match the first joint evaluation.
- It uses the same `base_seed`, `n_keep` and `firm_seed`/`is_draw` stream, and the mixture `lwp` sits in `lq`.
- Check: a `maxeval=1` run (`wi_iib_t2`) gives the joint L̂ at (θ0, γ_solved) = 0.00368693242970951. The printed f1 is 0.00368693, and nestedcheck at that point gives nested 0.003686932275. The relative difference of 4.2e-8 comes from the float cache; at γ = 0 it is 2e-10.

**Globals.**
- `g_inner_nm`/`g_inner_dual` are saved and restored. They can only be set inside the `nested=1` parse block, so they are always 0 on this path, and a race between concurrent points is harmless.
- `g_kpow`/`g_kshare` are left at x[3]/x[4], the values the joint objective writes at its first evaluation.

**Fixed γ rows.** `free_idx` comes from `lower != upper`, so `drop_rows` stay at 0.

**Reproducibility and threads.**
- 1 thread vs 2 threads: identical warm-start γ (max |Δγ| = 0) and identical L̂.
- The 6-thread launch (`1611-iib-k0.7.Rout`) printed the same values: 0.176605 → 0.00368693, 603 evaluations, max|γ| 172.146.
- Sums run in firm order, and the L-BFGS is deterministic.

**Without `gamma_init`.** `g_gamma_init_solve = 0` skips the whole block. The CLI default is `zero`, and any value other than `zero` or `solve` is refused.
