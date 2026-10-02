# Review: why the medians design fails (2026-10-01)

Scope: design i medians. Base rows 0–4, 6, 8, 9, 11 (interior firms), plus nine rows $(1\{u\le m_j\}-\tfrac12)\,1\{j\}$, $u=\ln(M^*/M)$, $m_j$ = the deconvolved median (column `umed`). Binary `grid_estimator_ind5b`, `qform=power_nokink`, `sampler=is`, `proposal=mix`, `rho=prop21`, `cut=ak`.
Tags: **[V-comp]** = verified by computation (adiag runs, scratch in `/Users/hans/.claude/jobs/0786c312/tmp/`), **[V-code]** = verified by reading the code or the papers, **[I]** = inferred.

The research log (2026-10-01, "D diagnosis") already names the pooled D as the cause and proposes two fixes: (a) leave bounded rows out of $\rho$; (b) use a within-industry D. This review confirms (a) and shows why it is exact. It shows that (b) alone is not enough. It adds three findings: the singular-$\Omega$ objective is a lottery, the plateau defeats the optimizer, and the medians carry little information about the mean.

---

## 1. What the outputs show

### 1.1 Fits

| run | targets | TS = 2nL̂ | iters | γ on median rows | median rows saturated* |
|---|---|---|---|---|---|
| 1600-i (design i, reference) | ε·1{j} | 59.8 | 6468 | (ε rows) up to 103 | – |
| 1600-i_med (old, rows 19–20 dropped) | 1598 | 1115 | 2456 | 92.8, −26.8, −13.8, 129.6, 106.7, 16.1, –, –, −62.6 | 313, 324, 331, 342 |
| 1605 k = 0.5 / 0.65 / 0.75 | 1604 | 2973 / 2947 / 2884 | ~400 | all \|γ\| < 3.5 | all 9 |
| 1606-d2 (= 1605 k0.75, old maxeval) | 1604 | 2884 (identical) | 406 | identical | all 9 |
| 1609 MH, k = 0.75 | 1604 | 2930 | 2593 | 89, 38.6, −13.1, 59, −0.4, 31, 131, 141, 32 | all but 322 |

\*Saturated = the row mean equals $0.5\,n_j/n$ to the printed digits, so tilted $P_i(u\le m_j)=1$ for every firm in $j$. Example (1605 k0.75, row 15, 322): $0.17058 = 0.5\times4111/12050$. All nine rows of 1605 match this pattern, with $t$ between 5 and 28. [V-comp, from the archived adiag files]

### 1.2 What happens at the 1605 point
At the 1605 k0.75 point the tilted distribution sits almost entirely at small $u$. Tilted E[u] is 0.064. The largest kept u per firm has a median of 0.14 and a maximum of 0.39. The IS ESS median is 70 of 1000. Tilted E[u] by industry is 0.02–0.17, against E[V] of 0.06–0.39. [V-comp]

### 1.3 Ω is singular, and the objective is a lottery
At the 1605 point $\Omega$ has a null direction. Its eigenvalue is $\pm10^{-12}$, and its loadings are $\pm1/3$ on every median row: the equal-weight sum of the nine rows. Its mean is $\sum_j 0.5\,n_j/n = 0.5$. The CUE contribution $\tfrac12 (z'\bar g)^2/w$ is $\approx 0.0278/(2w)$, so everything depends on the eigenvalue's sign:
- `1605-med-k0.75-adiag-R1000.txt` (MacBook, 3 threads) prints `Lhat = 0.119652`. The same file lists that eigenvalue as kept with contribution $3.68\times10^{10}$. The printout contradicts itself. Two eigendecompositions of one $\Omega$ disagreed on the sign within a single process. [V-comp: read from the file; cause = LAPACK roundoff, I]
- `1605-...-k0.75-adiag-R4000.txt`: L̂ = $1.6\times10^{10}$. The R4000 files for k = 0.5 and 0.65 report the eigenvalue as negative, so it is dropped and L̂ ≈ 0.12. [V-comp]
- My rerun of the same point on the Mac mini at 1 or 2 threads gives L̂ = $1.06\times10^{10}$, deterministically. [V-comp]
- At fixed θ, adding a common constant c ∈ {−2, −1, 0, 1, 2, 4} to the nine median γ's moves L̂ between $1.5\times10^9$, $8.5\times10^9$, $1.1\times10^{10}$, **0.1197**, $2.0\times10^{10}$ and **0.1197**. The eigenvalue flips sign each time ($\pm10^{-12}$). The median row means do not change. [V-comp]

So the "TS ≈ 2,900" of the 1605 fits is not a statistic. It is the value on the lucky branch of a discontinuous function, and Nelder–Mead was steering by that lottery. The correct verdict at such a point is TS = ∞: one combination of moments has zero variance and a mean of 0.5.

### 1.4 Which rows moved in 1600-i_med and 1609
In both runs the rows that are *not* saturated are exactly those whose γ landed near $-1/D_j^2$ (Table 1.4). [V-comp: arithmetic on the fitted γ and the rho_D files]

| industry | $D_j$ (1604) | $1/D_j^2$ | 1600-i_med γ | γ+1/D² | 1609 γ | γ+1/D² |
|---|---|---|---|---|---|---|
| 313 | 0.0712 | 197.2 | 92.8 | 290 (sat) | 89.0 | 286 (sat) |
| 321 | 0.2015 | 24.6 | −26.8 | **−2.1** | 38.6 | 63 (sat) |
| 322 | 0.2791 | 12.8 | −13.8 | **−1.0** | −13.1 | **−0.3** |
| 324 | 0.1406 | 50.6 | 129.6 | 180 (sat) | 59.0 | 110 (sat) |
| 331 | 0.1082 | 85.4 | 106.7 | 191 (sat) | −0.4 | 85 (sat) |
| 342 | 0.1754 | 32.5 | 16.1 | 49 (sat) | 31.4 | 64 (sat) |
| 351 | 0.0647 | 238.8 | – | – | 131.4 | 370 (sat) |
| 352 | 0.1572 | 40.5 | – | – | 141.3 | 182 (sat) |
| 369 | 0.1264 | 62.6 | −62.6 | **0.2** | 32.1 | 95 (sat) |

---

## 2. Implementation versus Schennach (2014) and AK (2020)

### 2.1 The moment itself is correct
- `moment_g_A_one_exp_scale`, IND5 branch: `(log(Mstar/M) <= umed) - 0.5` in slot $13+j$, on every draw. Corner firms get 0.5 ($u=0\le m_j$). Firms without a median get 0. [V-code]
- The IS path in `firm_chain_A` uses the same g for the weights $\exp(\gamma'g-Q_\rho+lw)$ and for the averages. $g(\bar u)$ at $\bar u=M^*$ gives the median row +0.5. [V-code]
- The 1604 input has no corner firms and no missing `umed`. [V-comp]
- Validity: $E[1\{u\le m_j\}-\tfrac12\mid j]=0$ is an ordinary unconditional moment. Schennach (2014, p. 354, after Cor. 2.1) states that Theorem 2.1 needs no assumption on g beyond measurability. It explicitly "covers nonsmooth cases, such as the important case of quantile restrictions". [V-code]
- Lemma A.1 (p. 374) requires that no combination $\eta'g$ be constant in u on a positive-probability set of z. The power_nokink support caps u below $m_j$ for few firms (share of firms with $u_{\max}\le m_j$):
  - κ = 1.94: ≤ 1.4 percent in every industry;
  - κ = 0.42: up to 21 percent in 322.

  So the condition holds. [V-comp]

### 2.2 The dominating measure is valid, but centred at a boundary point
- `rho_Q` implements Schennach's Proposition 2.1 (p. 353, eq. 4) with a D-scaled norm: $d\rho\propto\exp(-\|D^{-1}(g(u)-g(\bar u))\|^2)\,d\lambda$. The step factor $e^{-1/D_j^2}>0$ leaves the support unchanged, so Definition 2.2(i) and (ii) hold and the estimand is unaffected (Remark 2.3, pp. 354–355). [V-code]
- **Exact reparametrization.** For an indicator row $g\in\{-\tfrac12,\tfrac12\}$ and $g(\bar u)=\tfrac12$:

  $$(g-g(\bar u))^2/D^2=(\tfrac12-g)/D^2,$$

  which is *linear* in g. So $\exp(\gamma g-Q)\propto\exp((\gamma+1/D^2)g)\times$(the rest of ρ). Including the row in ρ only relabels γ: $\gamma'=\gamma+1/D_j^2$. [V-code, algebra]
  - [V-comp] I set $D_{\rm med}=10^6$ and $\gamma_{\rm med}\leftarrow\gamma_{\rm med}+1/D^2$. This reproduces the 1605 point exactly: same row means, tilted u, ESS and singular eigenvalue.
  - Hence $\gamma=0$ in the code means $\gamma'=+13$ to $+239$ in the step-free measure. That is a tilt with odds of $e^{13}$ to $e^{239}$ in favour of $u\le m_j$.
- Schennach (p. 353, below Prop. 2.1) says the point mass and centring can be dropped "whenever $\bar u$ can be chosen such that $g(\bar u)$ remains sufficiently far from the boundary of the convex hull of $\{g(u)\}$". For the indicator rows, $g(\bar u)=+\tfrac12$ *is* that boundary. The measure is legitimate, but the implementation sits at the edge Schennach warns about. [V-code]
- Bounded rows contribute nothing to properness (Definition 2.2(ii)): their MGF is finite for every γ. The quadratic penalty only does work on unbounded rows (the ψ and ε rows). [V-code]

### 2.3 The rule for zero eigenvalues
- `cue_objective_A_std` keeps eigenvalues > 0, as AK's `objMCcu` does (`Appendix_B/cudafunctions/cuda_fastoptim.jl`: `inddummy = Lambda .> 0`). It matches AK. [V-code]
- AK's consistency result (Theorem 5, p. 18) for the alternative, however, assumes "the minimal eigenvalue of $V[\tilde h_M(x,\gamma)]$ is uniformly, in γ, bounded away from zero". That fails on the saturated plateau. [V-code]
- Schennach's GAUSS code (`elvisutil.g`, `calc_lnL_el`) uses the exact sweep inverse. A missing inverse returns lnL = −100, so a singular Ω is treated as a *rejection*. Neither rule sees a null direction with a non-zero mean. Ours turns it into roundoff-sign noise, so the code departs from both references on this case. [V-code]

### 2.4 The optimizer
- **γ solver.** Both reference implementations are careful here, and ours is not.
  - Schennach (p. 355) proves the inner problem in γ has no local minima for any fixed positive-definite W. The proof needs the Jacobian $V_\gamma=\partial\tilde g/\partial\gamma'$ (eq. 20, Lemma A.1) to be positive definite. On the plateau the median block of $V_\gamma$ is $\approx n_j/n\cdot P(1-P)\approx e^{-\gamma'}\approx0$, so a zero gradient there does not mean a minimum.
  - AK (Appendix C.5, p. 42) start γ from a global Differential Evolution search (`bboptimize`, range ±1e300 in `FirstApp/procedures/1App_main.jl`), then run BOBYQA twice. They also suggest the convex dual $\min_\gamma E[\ln E_\rho\exp(\gamma'g)\mid x]$ for start values.
  - Ours runs a joint Nelder–Mead over (θ, γ) from γ = 0. The γ step is $0.2/D_t$ (about 3 on the median rows), and the distance to the plateau's edge is $1/D^2$ (13–239). It cannot cross a flat region that wide. [V-code; plateau V-comp]
- **Smoothness.** Schennach (p. 360) asks for an objective that is smooth in θ "provided g is". With the indicator, IS draws that move with κ (bounded-support firms) can flip a row. I perturbed κ by factors of 1+10⁻⁷ … 1+10⁻³: L̂ went 0.163004 → 0.163004 → 0.163004 → 0.163003 → 0.163032 → 0.163181, with no visible jumps. Low priority. [V-comp]

### 2.5 Targets (already in the log; the medians design depends on them)
- 369 (deconvolved E[u] 0.116 vs E[V] 0.361) and 351 violate Var(V) ≥ Var(ε). Their medians come from a deconvolution that misses the mean, which conflicts with row 1 (pooled E[ε] = 0). [V-code: 1603 summary and log]
- The medians are generated regressors. Their first-stage sampling error is not in Ω, so the TS is too small (anti-conservative) by the usual two-step logic. [I]

---

## 3. Root causes, with confidence

1. **The γ origin is shifted by the D-scaled ρ step, so the optimizer starts on a saturated plateau. High confidence; proven by computation.**
   - With the median rows in $Q_\rho$, γ = 0 equals $\gamma'=1/D_j^2$ (13–239). There every firm has $P_i(u\le m_j)=1$ and the median rows have no gradient.
   - Only industries whose γ reached $-1/D_j^2$ ever moved (Table 1.4).
   - MH (1609) has the same failure: the fault is in the measure, not the sampler.
2. **The plateau makes Ω exactly singular, and `cut=ak` turns the singularity into a lottery.** High confidence.
   - When every median row saturates, each row equals $0.5\cdot1\{j\}$. The centred sum is zero for every firm (a dummy-variable trap), with mean 0.5.
   - L̂ then jumps between ≈0.12 and ≈10¹⁰ on the sign of a $10^{-12}$ eigenvalue. The jumps occur under changes in γ that leave every row mean unchanged.
   - The reported TS ≈ 2,900 understates a point that should be rejected outright. Nelder–Mead was misled by the jumps, which is why the 1605 fits ended after ~400 iterations with γ ≈ 0.
3. **The joint Nelder–Mead from γ = 0 cannot leave the plateau.** High confidence as a contributing cause. It follows from 1. Both reference codes take care at this step (§2.4).
4. **Even after the fix, the medians carry little information about E[u|j].** Medium confidence as a cause of a poor final TS. I could not run a fit.
   - [V-comp] With the ρ step cancelled at the 1605 θ, the median rows are near zero (|mean| ≤ 0.035).
   - Yet tilted E[u] is 0.18–0.33 in every industry, against E[V] of 0.06–0.10 in 324, 342 and 352.
   - Each firm's tilted u is spread out, so $P_i\approx\tfrac12$ is reached by dispersion within each firm. The location content that design i gets from $E[\varepsilon\mid j]=0$ then has to come from the pooled row 1 alone.
5. **Problems with the targets in 369 and 351.** Medium confidence that they cost TS; certain that they are inconsistent (§2.5).

Not causes: errors in the moment code, the IS weights, the corner convention, support infeasibility, or IS noise (R1000 and R4000 agree in every archived file). [V-code / V-comp]

---

## 4. Suggestions, ranked, each with a cheap check

**S1. Take the bounded indicator rows out of $Q_\rho$** (mask them in `rho_Q`, or let `mode=rhoD` write D = ∞ / a mask for them). This is the log's proposal (a). Under §2.2 it is an exact reparametrization, so the estimand is unchanged in every finite sample, not just asymptotically. Equivalently, without touching code, start the median γ's at $-1/D_j^2$.
- *Check (done):* adiag at the 1605 k0.75 θ and base γ, with $D_{\rm med}=10^6$:
  - no null eigenvalue (smallest $2.9\times10^{-4}$);
  - median rows unsaturated;
  - ESS median 290 vs 70;
  - L̂ a smooth bowl in a common shift c: c = −40 → 4.95, −4 → 3.77, −1 → 0.85, 0 → 0.22, 1 → 0.19, 2 → 0.34, 4 → 49.8.
- *Next check (needs approval):* a single k = 0.75 fit, otherwise as in 1605.

  Proposal (b) (within-industry D) on its own still leaves an offset $1/(p(1-p))\approx4$–11. It does not help the medians. It may still help the continuous ε·1{j} rows of design i (log: irregular tilts in 313/351, the smallest pooled D).

**S2. Make the objective handle numerically null directions.**
- Treat an eigenvalue below a relative tolerance (e.g. $w_k<10^{-10}\max w$) as null. If $|z_k'\bar g|$ is not also ≈ 0 on that direction, return a large finite penalty, as Schennach's `invswp` → −100 rule does.
- This keeps AK's "> 0" rule for regular directions, with no return to the old relative cut. It also makes the result independent of the sign of roundoff and of the machine.
- *Check:* rerun adiag at the 1605 k0.75 point with γ_med + c, c ∈ {0, 1, 2, 4}. L̂ should be the same (penalized) value at all four instead of alternating 10¹⁰ / 0.12. Repeat on the MacBook with 3 threads.

**S3. Start γ properly and solve γ more robustly.**
- After S1, γ = 0 is the neutral point.
- Add one of: (i) a γ-only warm start at the starting θ by DE + BOBYQA (AK C.5); (ii) the convex dual, now that the plateau is gone (the earlier divergence, log line ~1412, occurred at infeasible θ); (iii) a nested solve with an analytic gradient.
- *Check:* at the 1605 starting θ, a γ-only BOBYQA (a few hundred evaluations) should reach L̂ ≪ 0.19 with all median rows |t| < 2. That shows the remaining misfit sits in θ / the base rows, not the γ search.

**S4. Fix the information content.**
- (i) Use the medians together with design i (ε·1{j} rows plus the median rows), or add share rows (e.g. $1\{u<0.05\}$, which the 1603 deconvolution gives: 342 0.91, 352 0.28, 324 0.16). Both pin location and spread, not only the 50th percentile.
- (ii) Flag or drop the 369 and 351 median targets, or replace them with a deconvolution that satisfies E[u] = E[V].
- *Check:* after S1, run adiag at the design-i fit (1600-i or 1604-i-k0.7) with the median rows added, γ_med = 0 in the step-free measure. If the design-i tilt nearly satisfies the medians, the two designs agree, and the medians are a robustness row set, not a substitute. Needs a build with both row blocks (`D_G_A = 13 + 18`). An approximation: compute per-firm tilted $P_i(u\le m_j)$ in adiag at the design-i point. That is a diagnostic only, not an estimate (Schennach 2022, p. 1250).

**S5. (Low priority) Smooth the indicator:** $\Phi((m_j-u)/h)-\tfrac12$ with h ≈ 0.01. It restores smoothness in θ (Schennach p. 360) at a bias of order $h^2$.
- *Check:* the κ-perturbation probe above shows no visible jumps now. Repeat it after S1 at the fitted point; act only if L̂ shows steps.

**Also worth fixing:** account for first-stage error in the medians, through a joint bootstrap over the 1603 deconvolution and stage 2, or at least a statement that the TS is conditional on the medians.

---

## Scratch commands (reproducible)
`/Users/hans/.claude/jobs/0786c312/tmp/adiag.sh <out> <par> <rhoD>`. It reproduces the 1605 B-string (k = 0.75, 1604 input, n_keep 1000, 2 threads). Helpers `mk.R` and `mk2.R` edit the median γ's and D. Each adiag takes about 1–2 s.
