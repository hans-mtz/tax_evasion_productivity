# Third review: the fixes made after the second ELVIS code review (2026-09-30)

**Scope.** This review covers only the changes made after `elvis-code-review-2026-09-30.md`, as described in `log.md` under "2026-09-30 (night) — Second review; fixes; mixture proposal; convex dual start". The files are the working-tree `Code/C-estimator/grid_estimator.cpp` (20:26, md5 `78b852a3…`), `Code/Rcpp/1200-stage2-elvis-common.h` (unchanged since HEAD), `Code/C-estimator/Makefile`, and the Remark 2.3 paragraph of `Paper/sections/9999-elvis.qmd`.

**Setup.** Every build and run used a scratch copy. The binary is `s2`: `-DKINK_S -DEPSVAR -DKAPPA_FREE`, the same as the `grid_estimator_s2` target. The input `sub.csv` is the 1592 input restricted to `plant_id` ≤ 250 (2,264 interior firm-periods). For the guard tests I used `tiny.csv`, the first 200 rows. All runs used the 1594 options: `qform=power_nokink k_fixed=0.75 row6=eps_psi drop_rows=6,7,12 rho=prop21` with D from `1594-rhoD.txt`, plus `sampler=is cluster=plant base_seed=20260830`, and at most 2 threads. Two parameter points were used:

- **"half":** θ from the 1585 k0.75 fit, with γ = 0.5·γ̂ on the live rows.
- **"fit":** the best point of my own nested fit `t1a` (R = 100, κ̂ = 0.293).

Line numbers refer to the current file. **Verified** means I read the code and, where stated, ran it. **Inferred** means reasoning only.

---

## 1. Summary, ranked by severity

**INCONSISTENCY (substantive): the mixture proposal does not remove the R-dependence in general. Verified.**
- At the "fit" point, uniform and mix agree with each other at every R. Both still drift with R.

  | R | TS, uniform | TS, mix |
  |---|---|---|
  | 1,000 | 53.7 | 55.6 |
  | 10,000 | 79.3 | 84.6 |
  | 40,000 | 91.7 | 92.5 |
  | 160,000 | 98.0 | 98.1 |

- The ESS p1 stays at about 1 even at R = 160k.
- The nested fit reported TS = 33 at its own R = 100. Its large-R value is at least 98, so finite-R fits are optimistic.
- Cause (verified with a scratch diagnostic): at this point the tilt piles up at the **FOC-ceiling edge** of the support, not at M → 0.
  - Firms with lo > 0 have their weight at B ≈ 10⁻⁶–10⁻⁴, within a relative distance of 10⁻⁵ of lo.
  - For row 2660, the log-tilt is −43 in the bulk and +16.5 at a distance of 10⁻⁵.
  - The log-uniform-in-M component does not target that edge.
  - For some firms (for example row 3795), the log-tilt is still rising at the numerical floor `H_DENOM_FLOOR`. Their tilted law then sits on the redraw boundary, so the simulated estimand depends on the 10⁻⁶ floor (inferred).
- The log's "TS 321/329/329" result is specific to that point. It does not settle the question.

**INCONSISTENCY (latent): the legacy-mode guard is incomplete. Verified.**
- `grid3d`, `deltagrid`, `shell` and `flat` accept `nested=1`, `sampler=is`, `rho=prop21`, `cluster=plant` and `qform=power_nokink` and run without error.
- In a KINK build these modes ignore `k_fixed`, so `g_kpow` stays at its default of 0.5.
- They also read `x0[10]` into `n_par = 1+D_G_A = 14` slots, an out-of-bounds stack read (l. 3570).
- In the default build, `qform=linear` together with `sampler=is`, `rho=prop21` or `nested=1` is accepted:
  - Non-nested, it silently runs plain MH on the uniform ρ, because the linear branch of `firm_chain_A` ignores IS and ρ.
  - Nested, it silently uses the **exp_scale model with the fixed-scale sampler**: `q_draw` l. 949 and `q_h` l. 1028 fall through to exp_scale when `g_qform == 0`. I ran this: it is accepted and completes.
- None of this touches the current `s2` runs, because the KINK build forces qform 4 or 5 and `lambdagrid` handles them correctly.

**MINOR**
- (a) `mode=rhoD` under `power_nokink` is refused unless `rho=prop21` is given together with a dummy `rho_D` whose live rows are positive. The D check runs before mode dispatch (l. 3974), even though rhoD never uses ρ.
- (b) Stale comments:
  - l. 2184: "Dropped rows get 1"; the code now writes 0.
  - l. 2726–2727: "inner_start=dual, default"; the default is `fixed`.
  - l. 799: "rows 1, 5, 7 never dropped"; they can now be dropped.
- (c) The adiag TAIL line under no-kink still labels a_i > 0 as "(improper tilt)" (l. 2366). Under `prop21`, which no-kink now requires, the tilt is always proper. The ψ-row/ceiling-edge tail diagnostic the review asked for is missing, and that edge is exactly where the mass now sits.
- (d) The inner-solve cap hits and failures are printed but not written to the CSV.
  - The dual step's own result code is not counted.
  - Its evaluations are added into the inner-evaluation total.
- (e) Options accepted and silently ignored:
  - `inner_start`, `mix_umax` without nested or mix;
  - `algo2`, `sa_time` under `nested=1`;
  - `n_passes=1` in non-nested mode (raised to 2 without a message);
  - `drop_rows` outside the power qforms (test: `qform=exp_scale drop_rows=3` runs).
- (f) `wander` is defined differently in the two modes. Nested mode includes κ (and k, s when free); non-nested mode uses only δ and γ.
- (g) Old `rho_D` strings that carry the placeholder 1 (for example `1594-rhoD.txt`, rows 6, 7 and 12) still pass if those rows are made live. The D = 0 sentinel protects only new rhoD output.
- (h) Under mix, floor-edge redraws are about 1.6 times as frequent: an expected 11.5 against 7.0 per evaluation at R = 1,000 and κ = 0.51 on the full input. With κ free, each redraw shifts that firm's random stream, so the outer objective has slightly more small jumps in κ. This is inferred, and small.

**OK (verified)**
- **Nested determinism.**
  - Three nested fits (1 thread, and 2 threads twice) are identical in every column except `point_seconds`.
  - The same holds for two `inner_start=dual` fits (1 vs 2 threads).
  - adiag at 1 vs 2 threads gives byte-identical output apart from the header line.
- **Mixture density and weights.**
  - The formula is correct and is applied identically in all three draw sites.
  - adiag reproduces the fit's L̂ (0.0073733).
  - A 4M-draw unit test reproduces the uniform moments for lo = 0, lo = 0.5, lo = 1 − 10⁻⁶, umax = 0.1 (draws with u ≥ U) and lo = 10⁻¹⁴.
- **The estimand is unchanged by mix.**
  - "half" point: d̄ for uniform and mix agree to 4 digits at R ≥ 10k.
  - "fit" point: the two TS paths converge to each other (98.0 vs 98.1).
- **Gradients.** The analytic CUE gradient and the convex-dual gradient (= d̄) match central finite differences, with relative error ≤ 1.6×10⁻⁶ (the dual matches to 8–9 digits).
- **Defaults and guards.**
  - `inner_start` defaults to `fixed`.
  - Every flag set compiles. The bare `-DKINK` build fails, as intended, at its `#error`.
  - The whitelist matches every `get_opt` key exactly, including `Delta`.
  - The guards for D = 0 on a live row, no-kink without prop21, mix without IS, and `revgrid*`/`dvecdiag`/`revenue_baseline` all reject.
- **Remark 2.3.** The added sentence is accurate. It needs two refinements (§3.6).

## 2. Checklist: second-review findings

| Second-review finding | Status | Evidence |
|---|---|---|
| 1. Nested fits not deterministic with >1 thread | **Fixed** | `hstore` per firm, summed in firm order (l. 2685–2711). `nested_build_cache`, pass 1, `nested_F` and `compute_dvec_omega_A` all write per-firm slots and reduce in index order. Test: t1a = t2a = t2b, and t1d = t2d, on columns 1–27. |
| 2. IS objective depends on R (flat proposal) | **Partly** | `proposal=mix` is correct and leaves the estimand unchanged (tests in §3.2). It covers the M → 0 tail but not the FOC-ceiling edge where the tilt concentrates at the "fit" point: TS rises from 55 to 98 between R = 1k and 160k for both proposals. The review's alternatives were not implemented: a pilot-tilt proposal (Schennach p. 360), or an R-invariance and ESS acceptance rule in code. The 1594 script does run adiag at R and 4R, which helps but is not a rule. |
| 3. Default and YEAR_FE builds broken | **Fixed** | `#ifdef KINK` around l. 940. A syntax check of none, YEAR_FE, TAU_ROW, TAU_ROW+YEAR_FE, KINK_S, +EPSVAR, +KAPPA_FREE, +IND5 and +KAPPA_FREE+IND5 gives 0 errors. A full default link also succeeded. |
| 4. `Delta` missing from the whitelist | **Fixed** | l. 3764. A key diff gives an empty difference both ways. The default build accepts `Delta` (it then asks for `par`, as expected). |
| 5. Counterfactual/legacy modes ignore new options | **Partly** | `revenue_baseline`, `revgrid*`, `dvecdiag` and `omegadiag` refuse them (l. 4126–4130, tested). `grid3d`, `deltagrid`, `shell` and `flat` are **not** guarded (tested), and the default build accepts `qform=linear` with IS/prop21/nested (tested; §1). Porting the counterfactual engine is deliberately deferred. |
| 6a. Inner result code discarded | **Fixed (printed only)** | l. 2809–2811, 2900. Test output: "inner solves at the eval cap 14, failed 1". Not in the CSV. The dual step's code is not tracked. |
| 6b. adiag TAIL blind under no-kink | **Partly** | l. 2334: `exposed` is now "support reaches M → 0". The "(improper tilt)" label is wrong under prop21, and there is no ψ-row or ceiling-edge diagnostic. |
| 6c. `rho=uniform` allowed with no-kink | **Fixed** | l. 3974, tested. Side effect: `mode=rhoD` now needs a dummy `rho_D` (§1 MINOR a). |
| 6d. rhoD placeholders | **Fixed for new output** | rhoD writes 0 (l. 2203). A live row with D = 0 is refused (l. 3968, tested). Old files with a placeholder 1 are not detected. |
| 6e. `wander` = 0 in nested mode | **Fixed** | l. 2895–2898. Test: wander = 272 (θ and γ distance). Its definition differs from non-nested mode (MINOR f). |
| `n_passes=1` | **Fixed as specified** | l. 4513–4514. It is honoured in nested mode only and raised to 2 otherwise, without a message. |
| Makefile target for the binary in use | **Fixed** | Target `grid_estimator_s2`, with the same flags. `make -n all` lists 6 targets. `s2` (20:26:51) is newer than the source (20:26:48). The other 5 binaries are from 19:53, which is fine as long as they are rebuilt before use. |
| §4 Remark 2.3 too strong | **Fixed, could be sharpened** | §3.6. |
| §4 Convex inner problem | **Implemented as an option** | `nested_F` is correct (gradient = d̄, tested). It diverged in the log's test and in mine: γ ≈ 10¹¹, θ stuck at a simplex vertex, L̂ = 0.0258. Keeping `fixed` as the default is right. |
| §4 Pilot-tilt proposal | **Not addressed** | The mixture was chosen instead (see finding 2). |
| 8. Optimizer settings in other modes | **Deferred (agreed)** | — |
| 9. R drivers' n−1 divisor | **Deferred (agreed)** | — |
| 7.9 Joint bootstrap | **Deferred by design** | The caveat that the null is joint (first-stage values plus the model) still needs to go in the text next to any reported region boundary. |

## 3. Details

### 3.1 Determinism (verified)

Every reduction in the nested path writes to per-firm slots and sums in index order:

- `nested_build_cache` (l. 2602–2631): per-firm `G` and `lq`.
- `nested_L`: pass 1 into `gt`, then serial sums for d, S and Ω. Pass 2 into `hstore` (l. 2701), then a serial loop (l. 2706).
- `nested_F`: `lse` and `gt`, then a serial sum.
- `nested_outer_obj` and the `best_*` tracking run serially inside the NLopt callback.
- `compute_dvec_omega_A`: the `Ghat` slots, then a serial sum. `dsyrk` (Accelerate) gave identical results at 1 and 2 threads.
- adiag: per-firm vectors only.

**Test:** `lambdagrid nested=1 n_passes=2 n_keep=100 maxeval=30`, with `proposal=mix`. The 1-thread run and two 2-thread runs are identical in columns 1–27. With `inner_start=dual`, the 1- and 2-thread runs are also identical.

### 3.2 `proposal=mix` (`is_draw`, l. 951–979)

**Density (verified).**
- The uniform component on (lo, M*] has density p_u = 1/(M* − lo).
- The log-uniform component draws u ~ U[0, U) and sets M = M*e^{−u}, so its density in M is 1/(U·M) on (M*e^{−U}, M*].
- Because U ≤ ln(M*/lo), that interval lies inside the support. Draws from the uniform component with u ≥ U correctly get p_l = 0.
- lw = log(p_u / (½p_u + ½p_l)) is right.
- **Floor-edge redraw.** The rejection truncates both the reference uniform and the mixture to the same accepted set. The weights are therefore off by a constant P_mix(A)/P_u(A) per firm, which cancels in the self-normalized ratio.
  - This shows up in the unit test: the mean weight is 1.298 at lo = 10⁻¹⁴, but the weighted moments are still exact.

**Same weighting at all three draw sites (verified).**
- `firm_chain_A`, IS branch (l. 1096–1098): `a = lwp + γ'g − Q`.
- adiag, IS branch (l. 2277–2280): the same, on `rng2`, a fresh copy of the same stream.
- `nested_build_cache` (l. 2622–2627): `lq = −Q + lwp`, with γ'g added in `nested_L` and `nested_F`.

In all three, `is_draw` falls back to `q_draw` with lw = 0 when `proposal=uniform`, so runs made before the mixture are reproduced exactly.

**Unit test (4M draws; target = uniform on (lo, M*], k = 0.75).** E[M], E[M²] and P(M < lo + 0.3(M* − lo)) match the exact values within Monte Carlo error in five cases:

- lo = 0 with U = 25;
- lo = 0.5;
- lo = 1 − 10⁻⁶;
- lo = 0 with umax = 0.1 (most of the uniform draws have u ≥ U);
- lo = 10⁻¹⁴.

No draw fell outside the support.

**Estimand (verified).**
- "half" point: d̄, rows 0–5, for uniform and mix agree to about 4 digits at R = 10k and 40k. The tilted Var(ε) is 0.1304 under both.
- "fit" point: the TS paths in §1 converge to each other, and so do the tilted Var(ε) (0.954 vs 0.953) and the mean tilted u (0.502 vs 0.502).

**Why mix does not fix the R-drift at "fit" (verified for 67 firms with ESS < 1.5 at R = 10k).**
- Every one of these firms has lo > 0 (lo/M* between 0.19 and 0.37).
- The maximum-weight draw sits at a relative distance of 10⁻⁶–10⁻⁴ from lo, with B between 2×10⁻⁶ and 1.4×10⁻⁴.
- The log-tilt profile γ'g − Q (from the `t_edge` scratch harness) rises steeply toward the ceiling, because the ψ rows grow like ln B.
  - The Q penalty eventually dominates, so ρ is proper.
  - For row 2660 the peak is at a distance of about 10⁻⁵.
  - For rows 3548 and 3795 the profile is flat or still rising at the floor.
- The two proposal components put probability of about 10⁻⁵ on that band, so the edge is reached only as R grows.
- **Inferred:** where the tilt peaks at or beyond the floor, the limit depends on `H_DENOM_FLOOR`. The redraw removes the support band B ≤ 10⁻⁶, which is a support truncation in the sense of Def. 2.2(i) (tiny, but it carries the mass at these points).

### 3.3 `nested_F` and `inner_start` (verified)

- F(γ) = n⁻¹Σᵢ log Σⱼ exp(γ'g_ij + lq_ij). For corner firms it is linear (γ'gᵢ).
- Its gradient is n⁻¹Σᵢ g̃ᵢ = d̄. `nestedcheck` matches it to 8–9 digits under both proposals.
- `inner_start` defaults to `fixed` (l. 2755 and l. 3957). The comment at l. 2726–2727 still says the default is dual.
- With dual, the solution diverges (γ ≈ 10¹¹) and the outer NM never moves off a start-simplex vertex, which is consistent with the log.

### 3.4 Guards and small fixes (tested unless stated)

**Builds.** All flag sets compile. Only the bare `-DKINK` build fails, by design at its `#error`.

**Whitelist.** It equals the set of `get_opt` keys exactly. No option is read any other way: `parse_cli` is the only reader of `argv`, and there is no `getenv`.

**rhoD and D checks.**
- rhoD prints 0 for dropped rows.
- A live row with D = 0 is refused, and dropped rows with D = 0 are accepted.
- Under no-kink, rhoD itself needs `rho=prop21` and a dummy `rho_D`.

**Refusals.**
- `power_nokink` with `rho=uniform` is refused in `lambdagrid` and in `rhoD`.
- `revgrid_fixedtheta`, `revenue_baseline` and `dvecdiag` refuse the new options. `omegadiag` shares the same branch (read, not run).
- `grid3d`, `deltagrid` and `shell` do **not** refuse them; all ran to "Saved". See §1.

**Other.**
- Under no-kink, adiag TAIL reports `exposed` = 0.25 at κ = 0.29, with the improper-tilt label (MINOR c).
- The inner cap and failure counts are printed.
- `wander` is now computed in nested mode.

### 3.5 Deferred items

The counterfactual port, the optimizer settings in other modes, the R drivers' n − 1 divisor and the joint bootstrap are all deferred as agreed.

### 3.6 Remark 2.3 sentence

**Accurate.** This matches Schennach p. 355: "the choice of ρ has no effect on the set {ĝ(θ, γ)}" in any finite sample, and so it has no effect on any objective that depends only on ĝ. The CUE's Ω̂ depends on the per-firm tilted means, which do vary with ρ for a given ĝ.

**Two refinements.**
1. **Finite samples.** The ρ-dependence is not limited to "when no θ satisfies the moment conditions" (model rejected).
   - It arises at every θ where the sample ĝ cannot be set to 0 by a finite γ.
   - At θ where ĝ can be set to 0, TS = 0 under any ρ.
   - So in finite samples the confidence-set **boundary** can also depend on ρ and D. Only the population identified set and the asymptotic decision are free of ρ.
2. **Bold sentence.** The bold "never the point estimate itself" sentence directly before the qualification still reads as unconditional. It should say "the identified set", or point to the qualification.

**Optional addition.** At finite R the simulated objective also depends on the proposal (§3.2). This is a simulation effect, not a ρ effect.

### 3.7 New silent bugs introduced by these changes

**None in the s2 path (verified).** Two things introduced alongside these changes:

1. The rhoD chicken-and-egg (MINOR a).
2. The extra redraws under mix (MINOR h).

The `qform=linear` + nested/IS issue (§1) predates these changes but is now more reachable, because `nested` and `sampler` are whitelisted and documented as general options.

## 4. Prioritized fixes (not implemented)

1. **Proposal near the FOC-ceiling edge** (`is_draw`; the three sites pick it up automatically).
   - For lo > 0, add a third component uniform in ln(M − lo), or in ln B, over [ln(floor gap), ln(M* − lo)]. Keep the weight lw = log(p_u/mixture) as now.
   - Alternatively, use Schennach's pilot-tilt proposal: cache draws from the tilt at (θ̂₁, γ̂₁).
   - Enforce an acceptance rule before any TS is reported: TS at R and 4R within x%, and ESS p1 ≥ some floor.
   - Re-evaluate every reported fit at large R (≥ 40k on the full sample if feasible). Fits optimized at R = 100–1,000 understate TS (33 → 98 here).
2. **Floor dependence.** At the final point, run adiag with `H_DENOM_FLOOR` at 10⁻⁶ and at 10⁻⁸ (scratch rebuild). If TS moves, the floor is acting as a support restriction and needs to be stated or removed.
3. **Guard the remaining modes.**
   - In `main`, refuse `nested`, `sampler=is`, `rho=prop21`, `cluster=plant` and `proposal` unless `mode` is one of `lambdagrid`, `adiag`, `rhoD` or `nestedcheck`, and `g_qform ≥ 1`.
   - Refuse KINK builds in `grid3d`, `deltagrid` and `shell`.
   - Refuse `drop_rows` outside the power qforms.
4. **rhoD under no-kink.** Exempt `mode=rhoD` from the ρ checks (l. 3964–3974).
5. **Write to the CSV** the inner cap hits and failures, plus the dual's result code. Count the dual's evaluations separately.
6. **Cosmetic.**
   - Fix the stale comments (l. 799, 2184, 2726–2727).
   - Relabel the TAIL "(improper tilt)" under prop21 and add a ceiling-edge share (weight on B < 10⁻⁴).
   - Print a message when `n_passes=1` is raised to 2.
   - Align the definition of `wander` across modes.
7. **Note.** Apply the two refinements in §3.6.

Scratch artefacts (not in the repo): `C/s2`, `C/diag` (adiag plus the low-ESS printout), `C/t_mix` (mixture unit test), `C/t_edge` (log-tilt edge profile), and the `out-*.csv` and `adiag-*` outputs in the session scratchpad.
