# ELVIS stage-2 code audit (2026-09-30)

Scope: `Code/C-estimator/grid_estimator.cpp` (working tree as of the audit, which includes uncommitted edits: `k_free`/`k_min`, `n_passes`, IND5, dropped-row γ pinning; line numbers refer to that state and may drift by a line or two if the file is edited), `Code/Rcpp/1200-stage2-elvis-common.h`, the input scripts `1532`/`1546`/`1572`, and the derivation sections of `Paper/sections/9999-elvis.qmd`. Current configuration assumed throughout: `-DKINK -DKINK_S -DEPSVAR -DKAPPA_FREE -DIND5`, `cut=ak`, `qform=power_kink`, `row6=eps_psi`, `drop_rows=5,6,7,12`, modes `lambdagrid` and `adiag`, interior-only input `1585-...` (n = 12,050).

References: Schennach (2014) main text (`Lit-Papers/elvis.txt`) and supplement (`elvis_supp.txt`); her GAUSS code (`ELVIS_code/elvisutil.g`, `elvisexample.g`); AK2020 (`AK2020.txt`, `ReplicationAK2020/Appendix_B/cudafunctions/*.jl`, `Appendix_B1/B1_dgp1_10k_2000.jl`).

"Verified" means I read the code/text and checked it. "Inferred" means I derived it from the code, the math, and the fitted values in `Code/Products/1587-*`/`1588-*`, but did not run anything (the task was read-only).

---

## 1. Summary (ranked)

| # | Severity | Finding |
|---|---|---|
| 1 | **INCONSISTENCY (likely wrong results)** | The dominating measure (uniform M on (0, M\*], `q_draw`, l. 851–856) violates Schennach's Definition 2.2, condition 2: the tilt normalizer E_ρ[exp(γ'g)] must exist for **every** γ, and here it does not. Under uniform M, u = ln(M\*/M) is Exp(1), while rows 1, 5, 7, 12 and 13–21 grow like u or u² as M → 0. At **every** 1587 fit γ on row 5 (εlnM) is negative (−0.40 to −0.80), and in 1588 7 of the 9 per-industry εlnM γ's are too. That makes exp(γ'g) ∝ exp(+c·u²) on the beyond-kink tail. The tilted "distribution" is then improper for the ≈44% of interior firms whose M → 0 region lies beyond the kink (M\* > c_k κ M̄; I computed this share from the 1585 input). The simulated objective is finite only because a finite chain rarely proposes M/M\* < ~10⁻⁶ (inferred). The log invokes Remark 2.3 ("dominating-measure shape irrelevant") without its condition (ii). |
| 2 | **INCONSISTENCY (matters for optimization)** | The tilted average is computed by independence Metropolis–Hastings with common random numbers (l. 961–980). For fixed θ, ĝ is then a **step function of γ**: accept/reject flips discretely, and the g values do not depend on γ. Nelder–Mead on a step function stalls on plateaus. Schennach (2014, p. 360) recommends the smooth ratio-of-averages reweighting precisely for this. The derivation note's own eq-elvis-sim is that estimator. This is a plausible driver of the 700–2,500 refit noise and the gains from passes 3–4. |
| 3 | **BUG** | `run_adiag_mode`'s tilted-summary chain calls `moment_g_A_one_exp_scale(..., f.sig2eps)` **without `f.jidx`** (l. 2090, 2094). Under IND5 the per-industry rows 13–21 are therefore zero in that chain, so its tilt ignores γ₁₃…γ₂₁ (and row 5 is dropped). The TS and row t's in the 1588 adiag files are correct (they come from `compute_dvec_omega_A`). But the tilted Var(ε), the u quantiles, "u < 0.05" and "beyond" columns in the 1588 log table come from a **different tilt** than the fit. |
| 4 | **INCONSISTENCY (inference)** | Ω treats the 12,050 firm-periods as i.i.d. (l. 1990–2010). Theorem F.1 requires Assumption F.2 (Zᵢ i.i.d.), but plants repeat across years. The input has no plant column, so Ω cannot be clustered. With persistent ω and plant effects, TS is likely overstated (inferred). |
| 5 | **INCONSISTENCY (optimizer)** | NLopt Nelder–Mead runs with **no explicit initial step**. Per NLopt's default rule (0.25·(ub − lb) for finite boxes), the first simplex moves δ₀, δ₁, δ₂ by about ±30 (box ±60), κ by about 0.4, and each γ by 1 (or by |γᵢ|). There is also `maxeval=2000` per pass for 16–23 free dimensions. The "converged" result codes include 5 (MAXEVAL) and 6 (MAXTIME), which are positive. GAUSS uses a uniform `ptol=2`; AK fix θ and optimize only γ (DE for 100 s, then BOBYQA restarts). |
| 6 | **INCONSISTENCY (Monte Carlo design)** | Each firm's stream is seeded with `base_seed + row_id` (l. 961, 2086). A run at seed s+1 therefore gives firm r the stream that firm r+1 had at seed s. "Different seeds" reuse the same pool of streams, shifted across firms, so the seed-spread comparisons (30/31, 40/41) are not independent replications. GAUSS has the same pattern (`rndseed(myseed+i)`). |
| 7 | **INCONSISTENCY (incomplete fix)** | `cut=ak` is applied only to moment set A (`eig_A_active`/`keep_eig_A`). The counterfactual objective `cue_objective_R_std` (l. 1326–1358, `revgrid*` modes), `cue_objective_C`, the `omegadiag` diagnostic (l. ~1925–1940), and the R drivers `1210`/`1211` (l. 114/124) still use the relative 10⁻⁸·max cut and n − 1. The default is still `cut=rel` (l. 3276). |
| 8 | MINOR | Several build-flag combinations compile but write out of bounds or change the meaning of x[5]; `-DKINK` alone does not compile (`g_dropmask` is undeclared). No Makefile target records the flags behind each binary. |
| 9 | MINOR | CLI: unknown keys are silently ignored. `drop_rows` uses `atoi`, so a malformed token such as "6;7" drops only row 6 and garbage drops row 0. `k_fixed`/`s_fixed` are imposed via equal bounds but never written into x, so a mismatched x0 makes NLopt return INVALID_ARGS with `minf` uninitialized (l. 2390). |
| 10 | OK | Checked and consistent: the MH acceptance ratio (no ρ ratio needed with proposal = ρ); the burn-in/keep loop (identical to GAUSS `avg_mom`); per-firm averaging; centred Ω with divisor n under `ak` (= AK and GAUSS); keep Λ > 0 with dropped rows removed before the eigen-decomposition (= AK); ½ factor and TS = 2nL̂ (= AK `TSMC`, equivalent to GAUSS's J); the drop mask in all three branches (interior, beyond-kink, corner); pinning of dropped-row γ's; the x0/par/CSV layouts for KAPPA_FREE and IND5; the dh/dk and dh/dκ formulas; thread-count invariance. |

Also relevant to inference (Section 3): the "soft test" (min-subtracted statistic vs χ²_{d_g}) has no justification in Schennach's supplement once the hard test rejects everywhere, and stage-1 estimation error (β̂, α̂, σ̂²_{ε,j}) is not reflected in TS.

---

## 2. Derivation check (`9999-elvis.qmd` vs Schennach 2014)

**2.1 The recast-in-M application is valid in structure.** There is one latent per firm-period (M). Every row is a known function of (M, Zᵢ, θ) with Zᵢ = (M\*, 𝒱, 𝒲̃, τ_P, β, M̄, ln τ_P, σ²_{ε,j}, industry). The maps e = M\* − M, ε = ln(M\*/M) − 𝒱 and ω = 𝒲̃ − (1−β)ε are correct. At the true M, ε(M) = u − 𝒱 = ε, and 𝒲̃ = ω + (1−β)ε (checked against `1532`: 𝒲̃ = cal_W − α_K k − α_L l and cal_W = y − β(m\* − 𝒱)). Stage 1 needs to supply only β̂, α̂_K, α̂_L (inside 𝒱 and 𝒲̃) and, for EPSVAR, σ̂²_{ε,j}; f_ε is not needed. That matches Remark 2.3, but these generated regressors matter for inference (§3.5). Theorem 2.1 imposes no smoothness on g, so the kink discontinuity and the piecewise definition of g are allowed.

**2.2 Error: the Lagrangian in "Where the tilt comes from" (l. 65–103) solves the wrong problem.** The note imposes the moment constraint **for each z** (∫g μ(u)du = 0 at fixed z) and concludes that "γ solving g̃(z,θ,γ) = 0" is the multiplier. That would give a z-specific γ(z) and impose the moment conditionally on Z, which is stronger than the model and is infinite-dimensional.

Schennach's Lagrangian (main text, §2.2, eqs. (8)–(11)) instead:
- maximizes the joint entropy ∫∫ ln f · f dρ dπ;
- places a **single** multiplier γ on the **averaged** constraint E_{μ×π}[g] = 0;
- adds a multiplier *function* φ(z) only for the per-z adding-up constraint.

That structure is exactly why γ is finite-dimensional and common to all z. She stresses this contrast herself (p. 359: EL "would require moment conditions to be satisfied at each z, which is not the case in the present case, where they hold after averaging over z"). The theorem as stated in @eq-elvis-thm is correct; only the heuristic is wrong. Fix: replace the per-z KL problem with the joint one.

**2.3 Imprecise: statement of Theorem 2.1 (l. 57).** The note says "holds iff there exists (θ₀, γ₀) solving E[g̃] = 0". Schennach states it with an infimum, inf_γ ‖E g̃‖ = 0 (her eq. (6)), because solutions can sit at ‖γ‖ → ∞ (degenerate conditional distributions at the boundary of 𝒰). This matters here: the beyond-kink mixture and the M → M\* boundary are natural places for such solutions.

**2.4 Error with consequences: Remark 2.3 (l. 107–109).** Two issues:

- (a) The note says any ρ "whose support **covers**" the model support is fine. Definition 2.2 requires supp ρ = 𝒰, and Remark 2.2 says a larger support gives a valid but **conservative** (larger) identified set.
- (b) The note drops Remark 2.3's second condition: the choice of ρ is irrelevant only if "(ii) some moment generating function-like quantity must exist and be twice differentiable", i.e. E_ρ[exp(γ'g)|Z] < ∞ for all γ ∈ ℝ^{d_g} (Definition 2.2.2).

The note's own example ("uniform on the feasible support … only slower convergence") is exactly the choice that fails (ii) in this model; see finding 1 and §6.1. Proposition 2.1 exists to guarantee (ii): ρ ∝ exp(−‖g(u) − g(ū)‖²)·λ.

**2.5 Support (l. 137–146).** This section describes the old linear-q support (M\* − 1/(2λ), M\*), which depends on θ; Remark 2.1 allows that. The current kink design uses the θ-free physical support (0, M\*]. Draws beyond the FOC ceiling are kept as a mixture component that carries only the ε rows, and the row "1{beyond} − s" pins its share. That is a legitimate moment system, E[1{below}·ψ·Z] = 0 plus E[1{beyond}] = s. It is a modelling choice, not Schennach's or AK's support-restriction device: AK restrict the support to the inequality-satisfying set and do not add a mixture. With s free, the share row is exactly identifying, so beyond-kink draws are disciplined only by the ε rows. That is precisely where the impropriety in finding 1 lives.

**2.6 Error: AK Theorem 4 (l. 260).** The note reads AK Theorem 4 as licensing the replacement of an **expectation** inequality (E[e·1{Unincorp}] ≥ 0) by a sampler support restriction. AK's g_I are almost-sure restrictions on the latent (the Afriat inequalities hold draw by draw; their Theorem 4 samples from 1(g_I(x,·) = 0)dη). A pointwise restriction e > 0 is already imposed by M < M\*, which makes the average-sign row redundant, not "replaced". An average inequality cannot, in general, be turned into a support restriction. This concerns only the unified model, which is not implemented.

**2.7 OK.** The use of Proposition B.1/B.2 to license extra independence rows (ε·f(e), ε·lnM, ψ·ln τ_P, ε² − σ²_j) is fine, given the maintained assumptions ε ⊥ (e, M, ω) and ψ ⊥ (ω, M, τ_P), plus, for ε², equal ε variance across legal forms. The table comparing MSL, MSM and ELVIS (l. 162–168) is correct.

---

## 3. Implementation vs Schennach (2014) paper

**3.1 Simulation of g̃ (INCONSISTENCY, finding 2).**
- What the paper says (p. 359–360): draw from exp(γ'g)dρ by Metropolis (eq. (13)). "To facilitate optimization with respect to γ or θ", it recommends evaluating g̃ at any (θ, γ) by reweighting a fixed draw from r(u|z, θ₀, γ₀) with exp(γ'g(u,θ) − γ₀'g(u,θ₀)), "a smooth function of θ and γ by construction". It adds that smoothness "is also important to establish consistency of simulation-based estimators, as it ensures stochastic equicontinuity".
- What the code does (`firm_chain_A`, l. 966–981): a fresh MH chain at each (θ, γ), with the same uniforms (CRN). Holding θ fixed, the g values along the proposal sequence are fixed. Only the accept pattern depends on γ, and it changes by discrete jumps, so ḡ(γ) and Ω(γ) are piecewise constant in γ (and piecewise continuous in θ). Verified by reading; the consequences for Nelder–Mead are inferred.

**3.2 Dominating measure (INCONSISTENCY, finding 1).** Definition 2.2 condition 2 fails for the uniform ρ; see §6.1 for the algebra and the fitted values. Assumptions F.3–F.4 of Theorem F.1 (E g̃ and E g̃² finite) fail wherever the tilt is improper.

**3.3 Objective and test (OK).**
- The supplement defines L̂^CUE_n(θ) = sup_γ −½ ĝ'V̂⁻¹ĝ with an **uncentred** V̂ = n⁻¹Σ g̃ g̃'. Theorem F.1 then uses −2nL̂ vs χ²_{d_g}.
- The code minimizes ½ d̄'Ω⁺d̄ with a centred Ω (divisor n under `ak`) and reports TS = 2nL̂. Signs and scale match. Centred and uncentred forms are monotone transforms of each other (J_c = J_u/(1 − J_u/n)), so they share the minimizer. At TS ≈ 200 and n = 12,050 the difference is about 1.7%, and the centred form is slightly conservative.
- d_g should be the number of active rows. The log does this (e.g. 10 rows → 18.3).

**3.4 Profiling (OK for the hard test; INCONSISTENCY for the soft test).**
- Joint minimization over (η, γ) at a fixed grid value of the parameter of interest, with TS compared to χ²_{d_g}, is exactly the "slightly more conservative" region in the proof of Theorem F.1 (sup over γ, and with nuisance parameters η the region keeps β if some (η, γ) passes). OK.
- The "soft test" 2n(L̂ − L̂_min) vs χ²_{d_g} is the CHT-profiled statistic (S.19). Its critical value must come from CHT subsampling or bootstrap (supplement §F), not from χ²_{d_g}. When the model is rejected at every point (hard TS 200–5,000 vs 18.3), the soft region has no coverage interpretation. It is a relative-fit ranking under misspecification and should be labelled that way, not as a confidence set.

**3.5 Two-step inference (INCONSISTENCY, not in the code).** Theorem F.1 treats Zᵢ as data. Here 𝒱 and 𝒲̃ are built from β̂, α̂_K and α̂_L, and row 12 from σ̂²_{ε,j}. The first-step variance is not propagated, so TS is too large under the null (inferred). A joint (plant-level) bootstrap of stages 1 and 2, or a Murphy–Topel-type correction, is needed for a sized test. CLAUDE.md already notes this for MSL.

**3.6 i.i.d. sampling (INCONSISTENCY, finding 4).** Assumption F.2 requires i.i.d. Zᵢ. Firm-periods of the same plant are dependent, and `1532` exports no plant id.

**3.7 Unbounded γ (OK).** The paper (p. 355) expects solutions at ‖γ‖ → ∞. The code leaves γ unbounded (l. 2370), as it should.

---

## 4. Implementation vs Schennach's GAUSS code

| Step | GAUSS (`elvisutil.g`) | Ours | Verdict |
|---|---|---|---|
| Per-individual seed | `rndseed(myseed+i)`, fixed across evaluations (CRN) | `mt19937_64(base_seed+row_id)` (l. 961) | Same design, and same overlap weakness (finding 6) |
| Start of chain | `guess_un` (example: `rndu`) | one draw from ρ (l. 969) | Same |
| Proposal | `jump_un` (example: independent `rndu`) | independent uniform on (0, M\*] (l. 851–856) | Same |
| Acceptance | `exp(γ'g_try − γ'g_cur)·rho(try)/rho(cur)` | `exp(γ'(g_try − g_cur))` (l. 974–977) | Equivalent: ρ is uniform and equals the proposal, so the ρ ratio is 1. If ρ is changed (§7.1), the ratio must be added, as in GAUSS. |
| Burn-in / keep | `r=-rep[1]+1 … rep[2]`, accumulate if r > 0, divide by rep[2] | identical loop (l. 971–978) | Identical |
| Missing values | `missrv(avg,0)` | none (NaN would propagate) | MINOR |
| Weight matrix | `V = g'g/n − ḡḡ'` (centred, divisor n), `invswp(V)` (sweep inverse, no eigen-cut; if singular, returns −100) | centred Ω/n, keep Λ > 0 | Equivalent on full-rank Ω; singular directions handled like AK |
| Objective | `-ḡ' V⁻¹ ḡ`, maximized (no ½) | `½ d̄'Ω⁺d̄`, minimized; TS = 2nL̂ = n d̄'Ω⁺d̄ | Same J |
| Optimizer | `amoeba` over (nuisance, γ) jointly; initial simplex p0 + ptol·e_i with **ptol = 2 for every coordinate**; stop when simplex size < 10⁻³ and \|f_hi − f_lo\| < ftol = 10⁻³; γ started at 0 at every θ | NLopt NM over (δ, s, κ, γ) jointly; NLopt default steps (finding 5); xtol_rel 10⁻⁴, maxeval 2000, 2–3 passes | Same architecture. The step-size and budget choices differ (§6.5). |

---

## 5. Implementation vs AK2020 code

| Step | AK (`cuda_chainfun.jl`, `cuda_fastoptim.jl`, `B1_dgp1_10k_2000.jl`) | Ours | Verdict |
|---|---|---|---|
| Candidate draws | One MCMC chain **under ρ** per observation (hit-and-run on the polytope, with MH weight `exp(−Σ(ρW)²)`, i.e. a Gaussian-type ρ in the spirit of Prop. 2.1). Stored once (`chainM`), `nfast` = 10,000 candidates, fixed across γ. | iid uniform candidates, regenerated from the same stream at every evaluation (equivalent to fixed) | Equivalent role. **Difference in ρ:** AK's ρ has a Gaussian tail factor; ours is flat, which is the root of finding 1. |
| Tilt step | MH over the fixed candidates, acceptance `logunif < γ'(g_try − g_cur)`, no burn-in, starts at candidate 1 | same acceptance; 1,000 burn-in | Same |
| Accept uniforms | `logunif = log(CuArrays.rand(n,nfast))` **redrawn at every objective call** (CURAND, not reset by `Random.seed!`), so AK's objective is itself stochastic | fixed (CRN) | Ours is Schennach's convention. Neither is smooth in γ. |
| Ω | `numvar = Σ g g'/n; var = numvar − d̄d̄'` (centred, /n) | same under `ak` (l. 2005–2006) | Identical |
| Eigen-cut | `Lambda .> 0`, `inv(An'·var·An)`, `Qn = ½ d'…d` | `keep_eig_A` (l. 799), `eig_A_active` removes dropped rows first (l. 781–797) | Identical; removing the dropped rows = building the system without them. **Verified** that `adiag` uses the same functions (l. 2064–2072). |
| TS | `TSMC = 2*minf*n` vs χ²_{d_g} | `2*n*obj` (l. 2042) | Identical |
| θ | **Fixed** (`theta0 = .8`); only γ is optimized | θ-nuisance (δ₀, δ₁, δ₂, s, κ) optimized jointly with γ | Differs, but GAUSS does the same as ours. Legitimate, but it forfeits the convexity in γ that AK and Schennach exploit (supplement §G: "the problem of finding γ can be cast as a convex optimization problem … For each θ … we use the simplex method"). |
| Optimizer | BlackBoxOptim adaptive DE for 100 s (infinite range), then BOBYQA (xtol_rel 10⁻⁶, infinite bounds), restarted up to 3× **only while TS ≥ crit** | NM × 2–3 passes, optional SA / BOBYQA | Comparable in spirit; see §6.5 |
| Support restrictions | Inequalities imposed on the latent's support (Theorem 4); no γ for them | FOC ceiling **not** imposed; beyond-kink mixture with share row | Design choice (§2.5) |

---

## 6. Silent-bug hunt

**6.1 Improper tilt from the uniform dominating measure (finding 1).**

- **Where:** `q_draw` kink branch, l. 851–856 (`return Mstar * (1.0 - unif(rng))`), together with the rows set on every draw (l. 874–880) and the beyond-kink branch (l. 881–885).

- **Tail of the base measure.** Under ρ = U(0, M\*], u = ln(M\*/M) has density e^{−u} on [0, ∞).

- **Beyond-kink draws** (only ε rows plus the share row are active). Write L = ln M\* + 𝒱 (1585 input: median 9.2, p95 11.8). Then:
  - row 1: ε = u − 𝒱;
  - row 5: εlnM = (u − 𝒱)(ln M\* − u) = −u² + L·u − 𝒱 ln M\*;
  - row 7: εω ≈ −(1−β)u²;
  - row 12: ε² ≈ u²;
  - rows 13+j: −u² for firms in industry j.

  So log[e^{−u}·exp(γ'g)] = a·u² + b·u + const, with a = −γ₅ − (1−β)γ₇ + γ₁₂ − γ_{13+j} over the active rows and b = γ₁ + γ₅·L − 1. For a > 0 the normalizer ∫ exp(γ'g)dρ is **infinite**.

- **Fitted values (read from the CSVs).**
  - In all eight 1587 fits, rows 7 and 12 are dropped and γ₅ (CSV `gamma6`) ∈ [−0.80, −0.40], so a = −γ₅ ∈ [0.40, 0.80] > 0.
  - In 1588 (k = 0.5), the row-13…21 γ's (CSV `gamma14…gamma22`) include −0.11, −0.35, −0.65, −1.58, −0.51, −0.09, −0.72.
  - In 1573, γ₁₂ = +0.22 on ε², which the log already flagged as "wrong sign".

- **Who is affected.** The beyond-kink tail reaches M → 0 whenever M\* > c_k κ M̄. From the 1585 input this is **41–45% of interior firms** at the (k, κ̂) of 1587 (k = 0.4: 41%; 0.5: 45%; 0.75: 44%; 1.0: 45%).

- **Firms entirely below the kink.** The ψ rows are active down to M → 0. ψ ≈ −δ₂(1−β)²u², so ψω² ≈ −δ₂(1−β)⁴u⁴. Properness then needs γ₄δ₂ ≥ 0, which holds at the 1587 fits (γ₄ > 0, δ₂ > 0). But nothing enforces it.

- **Why it looks finite (inferred).** Take the 1587 k = 0.4, seed-30 fit (γ₁ = −0.74, γ₅ = −0.53) at L ≈ 10–12. The exponent falls until u ≈ 6–7 and returns above its u = 0 value only at u ≈ 13–15, i.e. M/M\* ≈ 10⁻⁶–10⁻⁷. With 2,000 proposals per firm, a firm hits that region with probability of about 0.05–0.5%, so a handful of the ~5,300 exposed firms per evaluation. A firm that does jumps there and stays (acceptance ≈ 1 thereafter), contributing ε ≈ 14 and εlnM ≈ −40 to its per-firm average.
  - The share of such firms rises with n_keep and changes with the seed. That fits the heavy-tailed seed noise ("seed 31 +130 to +160") and the non-monotone chain-length results (1568).
  - Under the CUE weight, extreme per-firm averages inflate Ω, which can *lower* TS. That is a known CUE pathology, and here it is reachable (inferred).
  - The population objective at these γ is undefined. The simulated one is an artifact of finite R.

- **Diagnostic to confirm (read-only, cheap).** At a fitted point, compute each firm's a_i (above) and flag a_i > 0 together with M\* > c_k κ M̄. In the adiag chain, also record each firm's maximum kept u and the number of firms with a kept u > 8.

**6.2 MH + CRN makes the objective piecewise constant in γ (finding 2).**
- **Where:** l. 966–981. At fixed θ, `g_try` depends only on the (fixed) proposal, and only the comparison `log(unif) < γ'Δg` depends on γ.
- **Evidence it bites:** pass 1 → pass 3 improvements of 2–4× in L̂ (1588: 0.0247 → 0.0084 at k = 0.5), and the log's refit noise of 700–2,500.

**6.3 adiag ignores IND5 rows in its tilted summaries (BUG, finding 3).**
- **Where:** l. 2090 and 2094, `moment_g_A_one_exp_scale(..., gc, f.sig2eps)` and `(..., gt, f.sig2eps)`, which fall back to the default `jidx = -1` (l. 864). `firm_chain_A` passes `f.jidx` (l. 971, 974), so the fit and the TS part of adiag are right.
- **Affected results:** only builds with IND5, i.e. the 1588 columns Var(ε), u p50/p90/p99, u < 0.05 and beyond.
- **Fix:** add `, f.jidx` to both calls.

**6.4 Eigen-cut fix: correct for set A, incomplete elsewhere (finding 7).**
- Set A is correct: `eig_A_active` removes the dropped rows (l. 781–797), `abstol = -1` under `ak`, `keep_eig_A` keeps w > 0 (l. 799), and Ω is divided by n under `ak` (l. 2005–2006). `adiag` uses the same functions, and the log confirms it reproduces TS.
- Still on the relative cut:
  - `cue_objective_C` (l. ~301);
  - `cue_objective_R_std` (l. 1326–1358), the objective of every `revgrid*` counterfactual mode. It also uses n − 1 (l. 1319);
  - `omegadiag` (l. ~1925, 1938);
  - `1210-stage2-elvis-driver.R:114` and `1211-stage2-elvis-driver-AB.R:124`.
- The `adiag` header comment (l. ~2031) still describes the relative rule. That is cosmetic, since the code is right.
- **Risk:** the counterfactual will be rebuilt on this engine. Unless `cue_objective_R_std` is ported, it silently reintroduces the porting error.

**6.5 Optimizer settings (finding 5).**
- **Where:** l. 2356–2392.
  - No `nlopt_set_initial_step`.
  - δ box ±60 (`DELTA_BOUND`, l. 69).
  - s ∈ [0.02, 0.6].
  - κ ∈ [0.02, 5].
  - γ unbounded.
  - `xtol_rel 1e-4`, `maxeval 2000`, per-pass `maxtime`.
- **NLopt's default initial step** (options.c `nlopt_set_default_initial_step`; stated from the NLopt source and documentation, not run here): 0.25·(ub − lb) when both bounds are finite, capped by 0.75× the distance to a nearer bound; otherwise |x|, or 1 if x = 0. That gives a first simplex with δ₁ and δ₂ displaced by about 30. At δ₂ ≈ 31, ψω² is enormous, so the first dozens of evaluations are spent far outside any plausible region. Every pass restarts with the same huge simplex.
- **Scaling:** γ's natural scale differs by row by orders of magnitude (row sd of εlnM ≫ share row), so a unit γ step is badly scaled for some rows.
- **Result codes:** NLopt's success codes include 5 (MAXEVAL) and 6 (MAXTIME). "All converged" in the log should mean code 4 (or 3). 1588 has pass-1 code 5 at k = 0.5 and k = 1.
- **Dimensions:** the free dimensions in the current IND5 run are 3 δ + s + κ + 18 live γ = 23. Nelder–Mead with 2,000 evaluations per pass is thin for 23 dimensions, even on a smooth objective.

**6.6 Inert parameters.**
- **γ on dropped rows.** Verified: `drop_rows` zeroes rows inside the moment function in every branch (l. 883, 898, 956), so γ_t·0 has no effect on either the tilt or the objective. The current code pins them at 0 (l. 2373–2376).
  - Runs before the pin fix optimized over dead dimensions: 1587 `gamma7`/`gamma8` (rows 6/7) = 2.6/4.2, 7.5/8.2, and so on; 1588 `gamma13` (row 12) = −17.3 and −10.2, `gamma6` (row 5) = 10.6. Those fits spent NM budget and simplex geometry on 3–4 flat directions, but their L̂ values are not biased by it.
- **k when fixed.** Pinned by equal bounds (l. 2362). OK.
- **The grid value `lambda` under KAPPA_FREE.** It is only the start value of κ (l. 2378), yet it is written as the CSV `lambda` column. Cosmetic; `adiag` correctly substitutes `kappa_hat` (l. 3388).
- **s when `s_fixed`.** Pinned. OK.
- **No other inert entries found.** Every live row depends on M, so every live γ affects the tilt, and every live row enters d̄.

**6.7 Drop mask in every branch (OK for the current build).**
- Applied in the interior below-kink branch (l. 898), the beyond-kink branch (l. 883) and the corner branch (l. 956).
- Not applied in the non-kink new-rows path (`qform` 1–3, l. ~902–928) or in `moment_g_A_one` (`qform` 0). Those paths cannot be reached in a KINK build (main rejects `qform ≠ power_kink`), but a future non-kink KINK_S build with `drop_rows` would silently not drop.
- The `g_dropmask` comment (l. 729–732, "rows 1, 5, 7 … never dropped") is stale.

**6.8 Row, γ, par and CSV indexing (OK, one trap).**
- **x layout.** x = (δ₀, δ₁, δ₂, k, s, κ, γ₀…γ_{D−1}), with OG = 3 + N_XFE (l. 2353). `inner_obj` reads k = x[3], s = x[4], κ = x[5] and γ from x[OG + t] (l. 2319–2330). `FitResult.d0yr[0..2]` carries (k, s, κ) through the passes (l. 2396, 2406, 2456).
- **CSV.** The header is `k_hat,s_hat,kappa_hat,gamma1..` (l. 2551–2560), and each row writes `d0yr[0..2]`. Consistent.
- **adiag par.** `NP = 4 + N_XFE + D_G_A` (l. 3373); par = (κ₀, δ₀, δ₁, δ₂, k, s, κ̂, γ…), with `g_kpow = par[4]`, `g_kshare = par[5]`, `par[0] = par[6]` and γ at par + 7. This matches `getpar` in `run-1587`/`run-1588` (columns `lambda … kappa_hat, gamma1..13/22`). Verified for KINK_S+KAPPA_FREE (13 rows) and with IND5 (22 rows).
- **Trap:** CSV columns are 1-based (`gamma(t+1)`, l. 2560) while rows, `drop_rows` and the adiag printout are 0-based. `gamma13` is row 12. The log already records one mislabelled t-table.
- **Cosmetic:** adiag's `loading_on_last_row` (l. 2073) prints row D_G_A − 1, which under IND5 is industry 369's row.

**6.9 IND5 construction (OK).**
- `jidx` is the index among the sorted distinct `sic_3` of interior firms (l. 477–485), and the input's header is unquoted.
- If `sic_3` were missing or quoted, every `jidx` would be −1 and rows 13–21 would be identically zero with no error, apart from the missing "industries …" line.
- Corner firms in non-interior industries get `jidx = −1` and, with row 5 dropped, contribute no εlnM at all. That is irrelevant for the interior-only input but would matter for a full-sample run.
- `N_IND = 9` is hard-coded, and nothing checks that the number of interior industries equals 9. With 10 industries, the 10th would get `jidx = 9` and fail the `jidx < N_IND` test, silently dropping it.

**6.10 Floors and bounded transforms.**
- `H_DENOM_FLOOR = 1e-6` in `B_power_scale` (common.h l. 281–284) binds only when B < 10⁻⁶, a set of x with negligible measure just below c_k. So there is no flat region of consequence.
- The log singularity of ψ at the kink is integrable under ρ. Under the tilt, the density near the kink behaves like B^{a}, where a = γ₀ + γ₂lnM + γ₃ω + γ₄ω² + γ₉lnτ is ψ's total multiplier. This is integrable iff a > −1.
  - If a ≤ −1 for some firm, mass piles at the floor and ψ's value there is set by the floor (ln 10⁻⁶): a second improper-tilt channel.
  - At the 1587 k = 0.4 fit, a ≈ +2.7 at typical ω ≈ 3 and lnM ≈ 9, so this is not binding there. It is not checked anywhere.
- The softsign transforms (`h_prime_bounded_power_scale`, the κ-score at l. 894–895) are bounded in (−1, 1) and polynomially decaying. OK.
- The rows 8 and 11 near-duplicate (corr 0.95–0.99): with keep Λ > 0, the direction separating them (eigenvalue ~10⁻⁴) is kept and its contribution is noise-driven. This is valid, but it adds variance to TS.

**6.11 RNG (finding 6 plus OK items).**
- **CRN within a run holds exactly** in the kink branch: `q_draw` consumes exactly one uniform and the accept step one more, whatever θ and γ are (l. 851–856, 971–977). The non-kink samplers (`draw_from_rho_checked`, `draw_from_rho_power_scale`) redraw recursively, which desynchronizes the stream across θ in rare events. They are not used now.
- **Independence from thread count and firm order:** streams depend only on (`base_seed`, `row_id`), and d̄ is summed in index order. OK.
- **Across base seeds:** streams overlap (finding 6). The SA stream is `base_seed ^ const` (l. ~2414). OK.

**6.12 Build flags (MINOR, finding 8).**
- `-DKINK` without KINK_S does not compile: `g_dropmask` is declared only in the KINK_S block (l. 733) but used at l. 883/898. Runs 1558–1560 are therefore no longer reproducible from this source.
- `-DEPSVAR` without KINK_S compiles. D_G_A is then 9–11, and the corner branch writes `ghat_row[12]` (l. 950) past the end of the array. The kink branch would write `g_out[12]` too. Undefined behaviour.
- `-DIND5` without EPSVAR (or without KINK_S) compiles with D_G_A = 12 and writes `g_out[13+jidx]` out of bounds.
- `-DKAPPA_FREE` without KINK_S compiles. `inner_obj` then reads κ from x[5], which is a γ entry, and gives it the bounds [0.02, 5] (l. 2325, 2368).
- None of these apply to the current `-DKINK -DKINK_S -DEPSVAR -DKAPPA_FREE -DIND5` build.

**6.13 CLI fall-backs (MINOR, finding 9).**
- `parse_cli`/`get_opt` (l. ~499–512) accept any key, so a typo (`drop_row=`, `n_pass=`) silently uses the default.
- `cut` defaults to `rel` (l. 3276) and `row6` to `eps_e` (the peso-scale row).
- `n_passes < 2` is silently raised to 2 (l. 3856).
- `drop_rows` uses `atoi` (l. 3305): "6;7" → 6, "x" → 0 (drops row 0).
- `k_fixed`/`s_fixed` are imposed only via bounds (l. 2362, 2365), not copied into x. A mismatched x0 makes NLopt return INVALID_ARGS; `minf` is uninitialized (l. 2390), so L̂ is garbage, and the negative `convergence` code is the only signal.
- `sig2eps` is not checked for finiteness under EPSVAR. A missing column gives NaN in row 12, which is harmless only when row 12 is dropped.

---

## 7. Proposed modifications (not implemented)

**7.1 Make ρ satisfy Definition 2.2(ii)** (finding 1). *Consistent with:* Schennach Prop. 2.1 and Remark 2.3; AK's Gaussian-type ρ in `gchaincu!`.
- **Location:** `q_draw` (l. 851–856) plus the acceptance step (l. 974–977), or the IS weights in 7.2.
- **Change:** use dρ(M|z; θ) ∝ exp(−‖D⁻¹(g(M; θ) − g(M\*; θ))‖²) dM on (0, M\*]. Here D is a fixed diagonal scaling (row sds at the start point) and ū = M\* is a natural centring point. Keep the uniform proposal and add the ρ ratio to the MH acceptance, as GAUSS does (`rho(try)/rho(cur)`), or add it as an extra IS weight. Because ‖g‖² grows like u⁴ (ε rows) or u⁸ (ψω²), it dominates every linear γ'g, so the tilt is proper for all γ. Remark 2.3 then genuinely makes the shape irrelevant.
- **Expected effect:** removes the improper-tilt region. The objective becomes insensitive to rare extreme draws; seed and chain-length noise should fall.
- **Cheaper interim step:** run the §6.1 diagnostic at the current fits before refitting.

**7.2 Replace MH with self-normalized importance sampling on fixed draws** (finding 2). *Consistent with:* Schennach p. 360; the note's own eq-elvis-sim; Rao-Blackwellization of the independence sampler.
- **Location:** `firm_chain_A` (l. 936–998) and the adiag chain.
- **Change:** per firm, draw R iid u₀₁ once from the firm's stream (CRN), set M_j = M\*(1 − u_j), compute g_j(θ), and form g̃ᵢ = Σ w_j g_j / Σ w_j with w_j = exp(γ'g_j − max_j γ'g_j) (times the ρ weight of 7.1). R = n_burn + n_keep gives the same cost as today.
- **Gradient:** ∂g̃ᵢ/∂γ' = Cov_w(g, g) comes free from the same draws, which enables a quasi-Newton or Newton solve for γ at fixed θ (supplement §G suggests guarded Newton or L-BFGS).
- **Diagnostic:** report the per-firm ESS = (Σw)²/Σw².
- **Expected effect:** smooth objective in γ, and in θ except at the kink. It removes plateau stalling and should shrink the refit noise.
- **Caveat:** the ratio estimator has O(1/R) bias. With √n ≈ 110, R ≥ 2,000 keeps √n·bias small; check the ESS.

**7.3 Cluster Ω by plant** (finding 4). *Consistent with:* Theorem F.1 Assumption F.2 applied at the independent unit; the project's stage-1 plant-clustered tests.
- **Locations:** `1532` export (add `plant`), `read_firm_csv` (read it), `compute_dvec_omega_A` (l. 1953–2010).
- **Change:** Ω = n⁻¹ Σ_p (Σ_{i∈p}(g̃ᵢ − d̄))(Σ_{i∈p}(g̃ᵢ − d̄))', keeping TS = n d̄'Ω⁺d̄.
- **Expected effect:** larger Ω, so TS falls if the within-plant correlation of g̃ᵢ is positive (likely, given persistent ω).

**7.4 Seed streams by hashing, not addition** (finding 6).
- **Location:** l. 961, 2086, and every `rng(base_seed + row_id)`.
- **Change:** `std::seed_seq ss{base_seed, (uint64_t)row_id}; std::mt19937_64 rng(ss);`, or splitmix64(base_seed) ⊕ splitmix64(row_id). The same change applies to GAUSS-style code generally.
- **Expected effect:** seeds 30, 31, 40, 41 become genuinely independent replications, so seed-spread yardsticks become interpretable. It changes every stream, so old fits will not reproduce under the new scheme; keep the old scheme behind a flag if reproducibility matters.

**7.5 Scale and budget the optimizer** (finding 5). *Consistent with:* GAUSS's uniform `ptol` on comparably scaled coordinates; AK's global-then-local-with-restarts scheme.
- **Location:** `run_opt` in `fit_one_grid_point_A_fixedLambda` (l. 2382–2400).
- **Change:**
  - call `nlopt_set_initial_step` with δ: 0.5, s: 0.05, κ: 0.1, γ_t: 0.2/sd_t (sd_t = row t's per-firm sd at the start, or standardize the rows by fixed constants, which leaves CUE and TS unchanged);
  - raise `maxeval` to about 200·dim, or report MAXEVAL/MAXTIME stops as non-converged;
  - once 7.2 is in place, use a nested solve: inner γ by Newton or L-BFGS (convex for fixed θ and W), outer NM over (δ, s, κ) only.
- **Expected effect:** fewer wasted evaluations, less basin-hopping, and "converged" that means converged.

**7.6 Finish the cut fix** (finding 7).
- **Locations:** `cue_objective_R_std` (l. 1326–1358), the Ω divisor in `compute_dvec_omega_R` (l. 1319), `cue_objective_C`, `omegadiag`, R drivers `1210:114` and `1211:124`, and the default `cut` (l. 3276).
- **Change:** route all of these through `eig_A_active`-style code with keep Λ > 0 and divisor n. Make `cut=ak` the default, or require `cut` explicitly.
- **Consistent with:** AK `objMCcu`. **Expected effect:** the rebuilt counterfactual cannot silently regress to the truncated objective.

**7.7 Fix the adiag IND5 call** (finding 3). At l. 2090 and 2094, pass `f.jidx` as the last argument. Re-run the 1588 adiag files: the tilted Var(ε), u-quantile, u < 0.05 and beyond numbers in the 1588 log table change; TS and row t's do not.

**7.8 Guard build flags and CLI** (findings 8–9).
- Add `#error` or `static_assert` checks:
  - EPSVAR ⇒ KINK_S;
  - IND5 ⇒ KINK_S && EPSVAR;
  - KAPPA_FREE ⇒ KINK_S;
  - declare `g_dropmask` for plain KINK, or `#error` on KINK without KINK_S;
  - `static_assert(D_G_A <= 32)` for the bitmask.
- Add a Makefile target per binary that records the flags.
- CLI:
  - reject unknown keys against a whitelist;
  - parse `drop_rows` with `strtol` and full-token validation;
  - write `g_kfixed`/`g_sfixed` into x[3]/x[4] when pinned;
  - initialize `minf = HUGE_VAL`;
  - check that `sig2eps` is finite for interior firms when row 12 is live;
  - assert that the number of interior industries equals `N_IND` under IND5.

**7.9 Inference hygiene** (§3.4–3.5).
- Label the min-subtracted "soft" statistic as a ranking device, not a χ²_{d_g} test, while the hard test rejects everywhere. For a sized profiled test, use CHT subsampling (supplement §F).
- For the final TS, run a plant-level bootstrap over stages 1 and 2 jointly, or at minimum over stage 2 with β̂, α̂ and σ̂² re-estimated per replicate, to account for the generated regressors.

**7.10 Derivation note** (§2).
- Replace the per-z KL derivation with Schennach's joint Lagrangian (single γ on the averaged constraint, φ(z) for adding-up).
- State Theorem 2.1 with the infimum.
- Restore Remark 2.3's condition (ii) and Remark 2.2's "larger support ⇒ conservative".
- Correct the AK Theorem 4 paragraph: it concerns almost-sure restrictions, not expectation inequalities.
