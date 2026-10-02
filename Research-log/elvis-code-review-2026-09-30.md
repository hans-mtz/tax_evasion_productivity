# Review of the post-audit ELVIS code changes (2026-09-30)

Scope: the working-tree `Code/C-estimator/grid_estimator.cpp` (saved 19:32, the source of `grid_estimator_kf5` used by run 1593), `Code/Rcpp/1200-stage2-elvis-common.h`, `1592-input-plant.R`, `run-1593-nokink-nested.sh`, the R drivers 1210/1211 (cut line), and `Paper/sections/9999-elvis.qmd`. I checked them against the audit (`elvis-code-audit-2026-09-30.md`) and the log entries from "(evening) — Code audit; Phase 0" to the end.

"Verified" means I read the code and, where stated, ran it. All runs used a scratch copy of the source (`-DKINK_S -DEPSVAR -DKAPPA_FREE`, the kf5 configuration). The input was a reduced one: plants with `plant_id` ≤ 250 from the 1592 input (2,264 firm-periods, all interior). Other settings were `qform=power_nokink k_fixed=0.75 row6=eps_psi drop_rows=6,7,12 rho=prop21` (D from `1593-rhoD.txt`), `sampler=is`, and at most 2 threads. The start point was the 1585 k0.75 fit, with γ set to 0.5·γ̂ on the live rows (γ = 0 for the fits). Line numbers refer to the current file.

---

## 1. Checklist

| Item | Status | Correct? | Evidence |
|---|---|---|---|
| F1 / 7.1: ρ must satisfy Def. 2.2(ii) | done (`rho=prop21`, not the default) | Yes, conditions (i) and (ii) hold for every γ (see §2.1). The tilt still puts mass where the proposal never goes. | `rho_Q` l. 882–886; MH l. 1083; IS l. 1067; adiag l. 2238/2247/2270 |
| F2 / 7.2: smooth reweighting (IS) | done (`sampler=is`) | Correct as an estimator. With the uniform proposal the weights collapse, and **the objective depends strongly on R** (§2.3). | l. 1061–1074; tested |
| F3 / 7.7: adiag passes `f.jidx` | done | Yes | l. 2235, 2237, 2245, 2268 |
| F4 / 7.3: plant-clustered Ω | done (`cluster=plant`) | Yes. The formula is the standard cluster-robust one. The two independent implementations (double, float cache) agree to 1.4×10⁻⁹. | l. 2112–2119; `nested_L` l. 2628–2635; tested |
| F5 / 7.5: steps, budget, codes | done for `lambdagrid` only | Yes there. `grid3d`, `deltagrid` and all `revgrid*` fits still use NLopt's default steps and fixed maxeval, with no "NOT converged" warning. | l. 2824–2855 vs l. 445, 1536, 1840, 2416, 3496 |
| 7.5 nested solve | done (`nested=1`) | The gradient is correct (tested, clustered and not). **It is not deterministic with more than one thread** (§2.4). The inner result code is ignored. | l. 2549–2819; tested |
| F6 / 7.4: hashed seeds | done (`seed=hash` default) | Yes. The splitmix64 constants and shifts are standard, and all 11 RNG sites go through `firm_seed`. | l. 83–90; grep of all `mt19937_64` sites |
| F7 / 7.6: cut fix everywhere | done in C++; partly in R | C++: `keep_eig`/`omega_div` are used in A, C, R and omegadiag. The R drivers keep Λ > 0 but still use `cov()` (divisor n − 1) and do not remove structurally zero rows before `eigen` (minor, legacy). | l. 74–76, 318, 355, 1426, 1460, 2031; 1210:112–114, 1211:124/136 |
| F8 / 7.8: build flags | done | Yes for the five guards and the `static_assert`. **New regression: the default build (no flags, the Makefile's first target) and the YEAR_FE build no longer compile.** | compile test (§3.1) |
| F9 / 7.8: CLI | done | Whitelist, strict `drop_rows` parsing, pinned k/s in x, `minf = HUGE_VAL`, and the sig2eps and N_IND checks are all right. **`Delta` is missing from the whitelist**, so `revenue_baseline` cannot be used. | l. 3676–3681 vs l. 4036; scripted diff of `get_opt` keys |
| Makefile target per binary | partly | The binary in use (`kf5`, and `kf4`) has no target. The four listed binaries (built 18:09) predate IS, clustering and nesting: they reject `cluster=`/`nested=`, so they fail loudly. | Makefile; probes of each binary |
| 7.9: joint bootstrap / soft test | deliberately deferred | Agree it can wait; see §4 for the one caveat I would put in the text. | — |
| 7.10: derivation note | mostly done | Support equality, the infimum, conditions (i)/(ii) and AK Theorem 4 are fixed. The per-z derivation is **kept, with a caveat paragraph added**, rather than replaced (acceptable). One statement is now too strong (§4). | `git diff` of `9999-elvis.qmd` |
| F10 (OK items) | — | Still OK: burn/keep loop, divisor n under `ak`, TS = 2nL̂, drop mask in all branches, x/par/CSV layouts. | read |
| `qform=power_nokink` | done | Support, redraw and κ interaction are correct (§2.5). Diagnostics are blind to its tail (§2.5). | l. 939, 959–989; common.h l. 297–303 |

---

## 2. Correctness of the new pieces

### 2.1 The Prop. 2.1 dominating measure (verified by reading; algebra)

- **MH (l. 1081–1084).**
  - Target: exp(γ'g)·ρ, where ρ = C·exp(−Q)·uniform. Proposal: the uniform.
  - Independence-MH acceptance is exp(γ'Δg − ΔQ). This is correct, and matches GAUSS `avg_mom`'s ρ(try)/ρ(cur) ratio.
- **IS (l. 1066–1072).**
  - Weight: exp(γ'g − Q − max). C cancels in the self-normalized ratio. Correct.
  - Under `power_nokink` the uniform lives on (lo(κ), M*]. The draws M_j = M* − u_j(M* − lo(κ)) are an inverse-CDF reparametrization, so using common random numbers across κ is legitimate.
- **Condition (ii) holds for every γ, given the live rows.**
  - Q sums ((g_t − g_t(M*))/D_t)² over exactly the live rows (mask l. 884).
  - Dropped rows are identically 0, and their γ is pinned at 0.
  - So γ'g is linear in the live g_t, while Q is quadratic in the same g_t. Hence exp(γ'g − Q) is bounded, and ∫ exp(γ'g) dρ < ∞ for every γ. Differentiability then follows from dominated convergence.
  - This also closes the second channel the audit named (§6.10: B^a with a ≤ −1 at the FOC ceiling), because (ln B)²/D₀² dominates a·ln B.
- **Condition (i) holds:** the Gaussian factor is positive everywhere, so the support is unchanged.
- **ρ depending on θ** (through δ and κ inside g) is allowed; Def. 2.2 writes ρ(·|z; θ). D is fixed across fits, as it should be.
- **The point mass omitted at ū = M\*.** Def. 2.2 and Remark 2.3 require only (i) and (ii). Prop. 2.1's point mass is part of a sufficient construction that "always exists", not part of the definition. Omitting it does not affect validity.
  - Two differences from her construction, both harmless for (ii): her ū is chosen near argmin‖g‖, and ū = M\* is an end point of the curve u ↦ g(u), so the "far from the convex-hull boundary" shortcut she mentions is not obviously available.
- **"The tilt still reaches u ≈ 12–14" is consistent with a proper ρ.** Properness only means the normalizer is finite; it does not keep the mass near ū.
  - On a tail where the γ-weighted part grows like a·u² and the penalty like u⁴/D², the tilted log-density peaks near u\* ≈ D·√(a/2).
  - With the pooled D's from `rhoD` (εlnM D = 5.9–19; these include between-firm level differences, which inflates D) and γ of order 1–10, u\* of 10–40 is expected.
- **What it implies:**
  - (a) The target now sits where the uniform-in-M proposal has probability ~e^{−u} (6×10⁻⁶ at u = 12). The simulated g̃_i rests on one or a few draws, so results depend on chain length or R (§2.3). This is a Monte Carlo problem, not a validity problem.
  - (b) Substantively, the fit places some firms at M/M\* ≈ 10⁻⁶. It uses the ε rows with an extreme ε, which nothing penalizes once the ε-variance row (12) is dropped, as it is in 1593.
  - In my 100-draw nested fit, tilted Var(ε) was 0.89 against 0.18 in the data. This is an "infimum at infinity"/support effect (Remark 2.2: a larger support gives conservative sets), and it shows up as fit.

### 2.2 Clustered Ω (verified by reading and running)

- Ω = n⁻¹ Σ_p S_p S_p', with S_p = Σ_{i∈p}(g̃_i − d̄), and TS = n d̄'Ω⁺d̄. This is the cluster-robust form of Theorem F.1 with the plant as the i.i.d. unit (2,099 clusters ≫ d_g = 9).
- Simulation noise is included, because each firm-period has its own stream.
- `main` rejects `cluster=plant` if any `plant_id` is missing (l. 3744), so `cl = −1` cannot index out of bounds.
- The regular (double) objective and the nested (float cache) objective agree: 0.04797993625 vs 0.04797993618.
- `1592-input-plant.R`: the join is by row_id with four exact equality checks. It is correct.

### 2.3 IS estimator and ESS (verified; tested)

- The self-normalized ratio and Kish's ESS = (Σw)²/Σw² are correct. adiag's IS branch reruns the same stream (`rng2`) and reproduces the fit exactly.
  - Test: nested fit L̂ 0.00970843 and 0.00624245, adiag 0.00970843 and 0.00624245.
- **Test: objective at one fixed (θ, γ) (my nested fit `nest1`) vs R and seed.**

| R (n_keep) | TS, seed 30 | TS, seed 31 | ESS p10 / p50 |
|---|---|---|---|
| 100 | 28.3 | 29.8 | 1.0 / 4.4 |
| 1,000 | 94.1 | 96.3 | 1.0 / 27 |
| 10,000 | 147.2 | 145.4 | 1.1 / 240 |

- **Seed noise is now small (±3%), which is what IS bought. But TS rises by a factor of 5 from R = 100 to R = 10,000**, and at least 10% of firms have ESS = 1 at every R.
  - This is the O(1/R) bias of the self-normalized ratio, plus the simulation part of Ω.
  - It is large because the proposal (uniform in M, which is Schennach's formula with γ₀ = 0) does not cover the tilted mass.
  - The smoke check in the log (ESS median 7.8 of 200) shows the same thing.
- **Consequence:** 1593 (R = 1,000) optimizes an R-specific objective. Its TS will not be stable in R, just as the MH fits were not stable in chain length.
- Schennach p. 360 reweights draws from r(·|z, θ₀, γ₀), the tilt at a pilot point, not from ρ itself. AK also draw under their ρ.

### 2.4 Nested solve (verified; tested)

- **Analytic gradient: correct.**
  - Derivation: dL = v'H̄dγ − n⁻¹Σ_p s_p v'dS_p, with dS_p = Σ_{i∈p}H_i dγ − n_p H̄dγ. This gives ∇L = n⁻¹Σ_i H_i v(1 − s_{p(i)}) + (n⁻¹Σ_p n_p s_p)H̄v, which is exactly l. 2674.
  - The pseudo-inverse is fine: dropped rows are removed before `eig`, and exact collinearities keep d̄ in the range of Ω, so the extra Golub–Pereyra terms vanish.
  - `nestedcheck` under `power_nokink`: analytic vs central FD ≤ 2.4×10⁻⁶ relative on all 9 live rows, both unclustered and plant-clustered. Cache vs regular IS: 1.3×10⁻¹⁰ and 1.4×10⁻⁹.
- **BUG: the outer objective is not deterministic when n_threads > 1.**
  - Cause: `pass2` (l. 2650–2673) accumulates H_i v into per-thread buffers `acc1`/`acc2`. Firms are claimed through an atomic counter, so the floating-point summation order changes from call to call.
  - L-BFGS on the non-convex CUE-in-γ problem amplifies the last-bit gradient differences.
  - **Test:** two identical runs (`nested=1`, R = 100, maxeval 40, 2 threads) gave pass-1 L̂ 0.00757 vs 0.01084 and final L̂ 0.00624 vs 0.00682. Final γ₁ was −66.7 vs −42.1, γ₂ −52.4 vs −5.6, and κ̂ 0.334 vs 0.378.
  - The same runs with 1 thread were bitwise identical (apart from the timing column).
  - So the log's "deterministic within a pass" holds only single-threaded. The 1593 fits (3 threads) are not reproducible, and their seed-30/31 spread mixes in thread-order noise.
  - `nested_build_cache` and `pass1` are deterministic, because they write per-firm slots and reduce in index order.
- **The inner result code is discarded** (`nested_solve_gamma(O->P, O->gam, nullptr)`, l. 2732).
  - The inner maxeval is 300. In my test, 80 outer evaluations used 16,127 inner evaluations (about 200 each), so some solves probably stop at the cap. γ drifted to |γ| up to 169.
  - An inner stop at maxeval makes the outer objective a partially minimized L, and nothing is logged.
- **Best tracking and columns:** correct, and monotone.
  - Pass 2 starts NM at `best_xt`, with `gam_start = best_gam`. Its first evaluation therefore returns ≤ best_f, because L-BFGS is a descent method and the cache is bitwise reproducible for a given θ.
  - Column meanings: `Lhat` = best (θ, γ) over all passes, reproduced by adiag. `Lhat_pass1` = best of pass 1. `convergence`/`iters` = the last pass's outer NM code and count. `convergence_pass1`/`iters_pass1` = pass 1's. `wander` is always 0 in nested mode (not computed, so it means nothing there).
- **Silently ignored under `nested=1`:** `algo`, `algo2` and `sa_time`. Also `n_passes < 2` is raised to 2 (l. 4400).

### 2.5 No-kink sampler (verified by reading; counts from the 1592 input)

- **Support.** M ∈ (max(0, M\* − c_k κ M̄), M\*] (l. 939, common.h l. 297–303). The scale passed is κM̄, and x = e/(κM̄) < c_k, so the "beyond" branch (l. 970) cannot fire. Row 10 is masked and s is pinned at 0.3 (l. 3794). OK.
- **Firms whose support reaches M → 0** (M\* < c_k κ M̄, at κ = 0.511): 53% (k = 0.25), 55%, 56%, 58% (k = 1). For these firms the ψ rows are live down to M → 0, with ψω² ~ u⁴.
  - `rho=uniform` would therefore give an improper tilt for most firms under `power_nokink`, and the default is still `uniform`. 1593 uses `prop21`; nothing stops a run without it.
- **Redraws.**
  - Large firms: x = u·c_k, so B = 1 − u^k. The redraw fires when u ≥ 1 − 10⁻⁶/k, which is about 5–23 times per evaluation at R = 1,000.
  - Because k is fixed, the event does not depend on θ, so common random numbers across θ hold.
  - With `k_free=1` the event depends on k, and each redraw shifts that firm's remaining stream. That is a small discontinuity in k.
- **Diagnostics are blind to this tail.**
  - The adiag TAIL line (`exposed`, `a_i`, l. 2288–2301) is defined only for qform 4. Under qform 5 it prints "exposed 0; a_i>0 0" by construction, although more than half the firms have an unbounded support.
  - It should report the full-support share and the tail exponent of the ψ rows instead.

### 2.6 Seeds, and options used consistently across modes (verified)

- **Seeds.** `firm_seed` is used at every site. `rhoD` (l. 2158), adiag (l. 2232/2241), the nested cache (l. 2584) and the IS fit (l. 1050) all use the same stream and draw order.
  - `rhoD` reads k from par[4] and κ from par[6] (KAPPA_FREE), and applies the mask including row 10 under qform 5. It is consistent with 1593's `run`.
- **`lambdagrid`, `adiag`, `rhoD` and `nestedcheck` all read `rho`, `sampler`, `cluster`, `seed` and `cut`.** These are set in `main` before any mode is dispatched and before any threads start.
- **Globals.** `g_kpow`/`g_kshare` are set per evaluation (l. 2520–2524, 2711–2715). This is safe only because the KINK build refuses more than one lambdagrid point per process (l. 3848–3853).
  - Other modes in a KINK build (`grid3d`, `deltagrid`, `shell`) have no such guard, but they are not used with this build.
- **Silent traps:**
  - (a) `revgrid`, `revgrid_indep`, `revgrid_fixedtheta`, `dvecdiag`, `omegadiag` and `revenue_baseline` run `firm_chain_R` (l. 1343–1383). It hard-codes linear q, uniform ρ, MH, the peso-scale `eps*e` row 6 and no clustering.
    - A KINK build accepts `qform=power_nokink rho=prop21 sampler=is cluster=plant` in these modes and silently runs the old linear-q system.
    - These modes are also the counterfactual engine.
  - (b) `drop_rows` is parsed only inside the power-kink branch (l. 3785), so any other qform ignores it. It is whitelisted, so no error is raised.
  - (c) `rhoD` prints D = 1 for dropped rows (l. 2171). If a later run makes one of those rows live with the same `rho_D` string, it silently gets D = 1.
  - (d) `init_step` and `maxeval` only affect lambdagrid.

---

## 3. Other bugs found

1. **Default and YEAR_FE builds do not compile.**
   - l. 939 (`q_draw`, `if (g_qform == 5) ... g_kpow`) sits outside `#ifdef KINK`, and `g_kpow` exists only in KINK builds.
   - `make all` stops at the first target, `grid_estimator`.
   - The KINK_S, KINK_S+EPSVAR(+IND5) and kf5 configurations compile with 0 errors.
2. **`Delta` is not in the CLI whitelist**, but `revenue_baseline` reads it (l. 4036). That mode rejects its own argument.
3. **Nested non-determinism** (§2.4).

---

## 4. Against Schennach / AK: points the first audit did not raise

- **The IS proposal.**
  - Schennach p. 360 reweights draws from r(·|z, θ₀, γ₀) with exp(γ'g(θ) − γ₀'g(θ₀)) (times ρ(θ)/ρ(θ₀) when ρ depends on θ).
  - AK draw their chain under ρ and reuse it.
  - The code's IS is her formula with γ₀ = 0 and a flat proposal, which is the worst choice once γ̂ is large (§2.3).
- **Remark 2.3 in the note is stated too strongly for how it is used.** Her invariance ("any objective function based on optimizing a function of ĝ(θ, γ)") covers functions of the average ĝ.
  - The CUE statistic also depends on Ω(γ), which is built from the per-observation g̃_i. For a given ĝ the γ is unique (ĝ is the gradient of a strictly convex log-normalizer), but the g̃_i tuples differ across ρ.
  - So the zero set (the identified set) is ρ-invariant. **When the model is rejected, the value of TS (and so any "soft" ranking) can depend on ρ and on D.**
  - The note's "never the point estimate itself" should be qualified to the identified set.
- **The convex inner problem is not exploited.**
  - Schennach (Lemma A.1, supplement §G) and AK solve for γ at fixed θ through a convex problem: Λ(γ) = n⁻¹Σ ln ∫ e^{γ'g} dρ_i, whose gradient is ĝ(γ) and whose Hessian is n⁻¹ΣH_i.
  - The nested inner solve minimizes the non-convex CUE in γ directly. Minimizing Λ (or a Gauss–Newton step on ĝ with W fixed), then evaluating the CUE, would give a unique, start-independent γ(θ) whenever the infimum is interior.
- **7.9 (agree to defer), one caveat.** Taking β, α and σ²_ε as known makes the null a joint one (first-stage values plus model). With TS ≫ critical everywhere this does not change conclusions. For any region boundary that is eventually reported, say so in the text.

---

## 5. Prioritized fixes (not implemented)

1. **Make `nested_L`'s gradient reduction deterministic.**
   - Where: `nested_L`, l. 2650–2673.
   - Change: write h_i and (1 − s_{p(i)})h_i into a per-firm n×D buffer and sum in index order, as `pass1` and `compute_dvec_omega_A` do.
   - Effect: bitwise-reproducible fits for any n_threads. As it stands, the 1593 results (3 threads) cannot be reproduced, and their seed comparison is confounded.
2. **Replace the flat IS proposal, and add an R-invariance check.**
   - Where: `firm_chain_A` IS branch, `nested_build_cache`, adiag.
   - Change: after pass 1, draw per firm from the tilt at (θ̂₁, γ̂₁), for example with a long MH chain or a defensive mixture that is uniform in M plus uniform or exponential in u = ln(M\*/M). Cache the draws and reweight with the proposal density in the weight (Schennach p. 360).
   - Make "TS at R and 4R agree within x%" and "ESS p10 ≥ some floor" acceptance criteria for any reported point.
   - Effect: removes the 5× R-dependence of TS (§2.3).
   - Related, and a modelling decision for Hans: with row 12 dropped, nothing limits the tilted ε dispersion (0.89 vs 0.18 in my test fit).
3. **Fix the build.**
   - Where: wrap l. 939 in `#ifdef KINK`.
   - Add Makefile targets for kf4/kf5 (`-DKINK_S -DEPSVAR -DKAPPA_FREE`) and rebuild the four stale stage-2 binaries.
   - Effect: `make all` works, and the binary in use has a recorded recipe.
4. **Guard the legacy modes.**
   - Where: `main`, before the `revgrid*`/`dvecdiag`/`omegadiag`/`revenue_baseline`/`grid3d`/`deltagrid` dispatch.
   - Reject `qform ≠ linear`, `rho=prop21`, `sampler=is`, `cluster=plant` and `nested=1` there, and reject `drop_rows` outside the power qforms.
   - Effect: the counterfactual cannot silently run the old linear-q, uniform-ρ, unclustered system. The engine still needs to be ported to the new moment path before any counterfactual.
5. **Record the inner γ solve's result.**
   - Where: `nested_outer_obj`, l. 2732.
   - Change: keep the NLopt code, count maxeval stops and report them in the CSV. Consider the convex Λ(γ) solve as the inner start (§4).
   - Effect: shows whether the outer objective is a true profile.
6. **Add `Delta` to the whitelist** (l. 3676–3681).
7. **No-kink diagnostics and defaults.**
   - Change: under qform 5, report the full-support share (about 55%) and the ψ-row tail in adiag's TAIL line, and refuse `qform=power_nokink` with `rho=uniform`.
   - Also store the drop mask alongside `rho_D`, and error if a live row's D was a placeholder.
8. **Port the 7.5 optimizer settings** (steps, maxeval, "NOT converged") to `fit_one_revgrid_point_fixedtheta` (l. 1817–1860) and `grid3d` (l. 3475–3510) before the counterfactual is rebuilt.
9. **Cosmetic and legacy.**
   - Mark `wander` as N/A in nested mode.
   - R drivers: divisor n and removal of structurally zero rows.
   - Note: qualify Remark 2.3 as in §4; optionally replace the per-z derivation rather than annotating it.

## Test commands (scratch only)

`mode=nestedcheck` with cluster none and plant; two `mode=lambdagrid nested=1` runs at 2 threads and two at 1 thread; `mode=adiag` at the fitted points with n_keep 100, 1,000 and 10,000 and seeds 20260830/31; `clang++ -fsyntax-only` over seven flag sets; `get_opt`-key vs whitelist diff; binary option probes. All outputs are in the session scratchpad; nothing in the repository was modified except this report.
