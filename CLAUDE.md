# Tax Evasion & Productivity — JMP

Job-market paper (economics PhD). Structural model of **tax evasion via overreporting of intermediate inputs**, and its interaction with firm productivity. The user is sharp on econometrics; engage at that level.

**Where things live.** This file = settled definitions, conventions and file pointers only, undated. Dated history (decisions, corrections, rejected approaches, run results, numbers) → `Research-log/log.md` (the pre-2026-09-30 contents of this file are archived verbatim in `Research-log/claude-md-archive-2026-09-30.md`). Current task status and plan → `Thesis/PLAN.md` (shared Hans+Claude tracker; §9b = stage-2 re-estimation). Cross-session status pointers and standing feedback → memory. When something here changes, replace the text; put the "why" in the log.

## The model

- Firm overreports materials: $M^*=M+e$, $e\ge0$ (level); $u=\ln(M^*/M)=\ln(1+e/M)$ (log ratio).
  - **Notation:** the slides and thesis use $u$ for the log ratio. The approved first-stage paper `Paper/sections/56-id-evasion.qmd` (read-only) defines its "$e$" multiplicatively ($M^*=Me^{e}$) — that paper's $e$ **is** $u$. Translate when citing it.
  - Deconvolution recovers $f_u$, never the level $e$ (property of the CD/log-linear identification; under translog the map would need rederiving).
  - Firm-invariance of $f_u$ is not needed for deconvolution (which only needs $\varepsilon\perp u$ and known $f_\varepsilon$); it is a separate, untested claim about what the recovered marginal means. Never test it by regressing $\mathcal V_{it}$ on $M^*_{it}$ (mechanical); bucket by a proxy not in $\mathcal V$ ($K$, $L$) and deconvolve within buckets.
  - $M\perp e\mid\omega$: neither the materials FOC nor the evasion FOC contains the other's choice.
- Production: $Y=K^{\alpha_K}L^{\alpha_L}M^{\beta}\exp(\omega+\varepsilon)$. $\varepsilon$ = **measurement error in output**, $E[\varepsilon]=0$ (supervisors' choice; not GNR's ex-post shock). Productivity is $\exp(\omega)$; no $\mathcal E=E[e^\varepsilon]$ in the FOC; share $\ln(\rho M/PY)=\ln\beta-\varepsilon$; $\hat\beta=\exp(E[s\mid\text{corp}])$ (`first_stage_panel_me`, `big_E <- 1`). `56-id-evasion.qmd` still carries GNR's $\mathcal E$; translate when porting.
- Expected profit with detection probability $q(e)$ (audit + always caught, Allingham–Sandmo) and evasion cost $\kappa(e,\omega)$.
- **Evasion FOC:** $\tau_P\rho_t\big(1-(1+\phi)(q+q'e)\big)=\partial\kappa/\partial e$, $\phi=0$ (Colombian enforcement of the period; Perry and Cárdenas 1986). **SOC (κ linear in $e$):** $-q''e/q'<2$.
- $\rho_t$ = materials price (sector–time); $\tau_P\rho_t$ = benefit shifter (identification of the detection parameters).
- **Two tax rates.** The evasion FOC contains only $\tau_P$ (purchases side; `sales_tax_rate_purchases`). The materials FOC picks up $(1-\tau_S)/(1-\tau_P)$, equivalent to using the net-of-tax log share. $\partial e^*/\partial\tau_P>0$; this sign cannot be checked by regressing $\mathcal V$ on $\tau_P$ (both built from $M^*$) — only via the 1983 reform or the structural estimates. Derivation: `Paper/sections/9999-tax-wedge.qmd`.

### Detection function $q(e)$
- **Open.** Supervisors reopened $q$: a level-$e$ $\lambda$ is not credible (scale problem). Candidates: external audit evidence (Ecuador, Mexico); convex/two-parameter forms; scale-normalized $q(e/\bar M_{j,t-1})$, $\bar M_{j,t-1}$ = lagged industry mean of reported $M^*$ (1981 by leave-one-out). Working note: `Paper/sections/9999-detection-q.qmd`.
- **Current system (under re-estimation, see PLAN §9b):** kinked power $q=(e/(\kappa\bar M_{j,t-1}))^k$ up to the FOC ceiling $c_k=(1+k)^{-1/k}$, draws beyond it get only the $\varepsilon$ rows, share beyond the kink $s$ pinned by a moment (`qform=power_kink`).
- Forms and their properties: linear $q=\lambda e$ (SOC always; ceiling $\lambda e<\tfrac12$; $T\sim U[0,1/\lambda]$ audit-threshold story); exponential $1-e^{-\lambda e}$ (ceiling $\lambda e<1$; Lambert-$W$ closed form); power $(\lambda e)^k$ (nests linear; ceiling $(1+k)^{-1/k}$; closed form $e=\lambda^{-1}([1-C/\tau\rho]/(1+k))^{1/k}$).
- Report detection risk in **relative** terms where possible (under linear $q$, $q(e_A)/q(e_B)=e_A/e_B$ is $\lambda$-free). `Code/Deconvolution/1303-detection-prob-table.R`.

### Cost function $\kappa$
$\kappa_{it}=e_{it}\exp\{\delta_0-\delta_1\omega_{it}+\delta_2\omega_{it}^2+\psi_{it}\}$ — linear in $e$, **convex in $\omega$** (never "U-shaped": evasion-by-size rises roughly linearly to p90–95, then drops sharply at the top). $\omega^*=\delta_1/2\delta_2$. $\psi$ = idiosyncratic cost shock; $E[\psi]=0$ (location normalization; $\delta_0$ absorbs the level).

### Corporations
- Constrained non-evaders: $e=0$ is an institutional constraint (audited statements, third-party reporting), outside the FOC, used only in the first stage; they are the validation sample, never in the stage-2 sample.
- No $\delta_C$ cost shifter (supervisors). ELVIS carries no corner term for $e^*=0$; MSL's likelihood includes the $e^*=0$ mass.

## Estimation

1. **First stage** (`56-id-evasion.qmd`, approved, read-only): corps give $\mathcal V=-\varepsilon$ → $\beta$, $f_\varepsilon$, deconvolution of $f_u$. Observed residuals $\mathcal V_{it}=u_{it}-\varepsilon_{it}$ and $\tilde{\mathcal W}_{it}=\omega_{it}+(1-\beta)\varepsilon_{it}$.
2. **Second stage:** $\theta$ (detection + cost parameters) from unincorporated firms. $e$ and $\varepsilon$ are **one** latent tied by $\mathcal V$; $f_e$ does not reappear in stage 2.

### First stages, current pipeline (`Code/Deconvolution/1500`–`1530`)
- **Sample:** juridical codes 6–9 dropped (6 = stock partnerships, taxed as corps; 7–9 coops, state, other). Corps = code 3; unincorporated = 0, 1, 2, 4, 5. Log materials share **net of sales taxes**. 353 has no estimate.
- **Test for overreporting = test inversion** (`1510`): $\ln\hat D$ from corps taken as given; grid $\mu=E[\mathcal V]$; plant-clustered centred $\Omega$ re-estimated at each candidate; sharp $\chi^2_1$ (headline) / conservative $\chi^2_2$ (appendix). Bootstrap robustness `1514` resamples (plant, legal form) — never reuse `resample_by_group` for grouped resampling (it shifted replicates). Rejecting industries: 313, 321, 322, 324, 331, 342, 369, plus 351, 352 marginal.
- **PF step:** headline single instrument $\tilde{\mathcal W}_{it-2}$ (`lag_2_w_eps`); $m^*_{it-1}$ and the joint system are comparisons. Point = minimum of the same test-inversion statistic whose regions are reported, $\Omega$ re-estimated at each candidate (never two-step). $\beta$ region: 1-D inversion on the corps' share moment. GNR/OLS from the R port `1520`. Scripts `1516`/`1517`/`1522`; tables `Code/Thesis/ch06-pf-comparison.R`, `appE-pf-all-industries.R`. PF lags are row-based within plant (~1.8% span a missing year).
- **Deconvolution industries by ex-ante rule:** 99% sharp region non-empty and excludes 0 (313, 321, 322, 324, 331, 342, 369), on the test's sample (lift the 369 top cut in `first_stage_panel_me`'s saved env, not globally). Report the median of industry means, not the range. `1521`, `ch05-overreporting-ratio.R`.
- **Ch. 7 event study (1983 reform):** `1530` (net share; gross-minus-net wedge in appendix G via `appG-fiscal-wedge.R`). Convert $\Delta u$ at the 1983 level, never $\exp(\Delta u)-1$. Productivity: `1523` (ω deconvolution), `1524` (persistence).
- **Instrument labels:** `lag_m` = $m^*_{it-1}$; `lag_2_w_eps` = $\tilde{\mathcal W}_{it-2}$ (tilde); `lag_2_cal_W` = untilded $\mathcal W_{it-2}$.
- The FOC side needs only nominal ratios (no deflator); the production/AR(1) side needs levels, so price-index quality enters there only (documented, not modelled). Stage-2 `materials = nom_mats/p_gdp` (real); no industry materials deflator exists.

### Stage-2 estimators
- **ELVIS (Schennach 2014) — headline.** Recast in the true materials level $M$ as the single unobservable: $e(M)=M^*-M$, $\varepsilon(M)=\ln(M^*/M)-\mathcal V$, $\omega(M)=\tilde{\mathcal W}-(1-\beta)\varepsilon(M)$, $\psi(M)=h(e)-\delta_0+\delta_1\omega-\delta_2\omega^2$. Entropy tilt $\exp(\gamma'g)$ of a dominating measure on the support; needs only $\hat\beta$ from stage 1 (not $f_\psi$, not $f_\varepsilon$; Remark 2.3). Rationale for headline status: leaves the unobservables' distributions free, whereas MSL imposes parametric $f_\psi,f_\varepsilon,f_\omega$. Derivation: `Paper/sections/9999-elvis.qmd`. Sources: `Lit-Papers/ELVIS.pdf`, `ELVIS_supplement.pdf`, `schennach-2022-measurement-systems.pdf`, Schennach's GAUSS code `Lit-Papers/ELVIS_code/`, Aguiar & Kashaev 2020 (`Lit-Papers/AK2020.pdf`, code `Lit-Papers/ReplicationAK2020/`; Nail Kashaev is a co-author and advisor).
- **MSL — parametric robustness check.** $\varepsilon$-space quadrature (over $s=\ln(1-2\lambda e)$, not $\varepsilon$), likelihood = interior integral + $e^*=0$ branch; $f_\omega$ conditioned on the full lagged information set (Version Cf) to avoid the $\omega$–$k$ bias. Spec `Paper/sections/9999-msl-implementation.qmd`; derivations `9999-density-trans.qmd` (its Step 3 dynamic-likelihood form is wrong; corrected in the implementation note). Two-step SEs need Murphy–Topel or a joint bootstrap.
- **Naive MSM — rejected.** Averaging $\psi(\varepsilon^{(s)})$ over prior draws misses the posterior reweighting; a correct simulated moment needs $f_\psi$, i.e. collapses to MSL. Confirmed empirically on Tim Conley's own variant (`1201-MSM.R`). $\Psi=\psi+\nu$ is not a clean deconvolution.
- **Unified single-stage ELVIS** (all of $\beta,\alpha,\theta$ jointly, $M$ the only latent) — designed in `9999-elvis.qmd`, held in reserve.

### ELVIS implementation conventions
- **$\hat\Omega^-$: keep every eigenvalue $>0$**, $\Omega$ divided by $n$ (AK2020 `objMCcu`, `Lambda.>0`; Schennach's code uses the exact sweep inverse `invswp`). `grid_estimator.cpp` option **`cut=ak`** — always use it (dropped rows are removed from $\Omega$ before the eigendecomposition). The relative cut $w_k>10^{-8}\max$ (the code's default `cut=rel`) is a porting error: when the $\delta$'s scale up it silently discards the $\varepsilon$ directions. Run `adiag` (rank, kept directions, row $t$'s) before trusting any $TS$.
- **Test statistic:** `TS = 2*n*Lhat` (our `Lhat` is AK's positive CUE quadratic), vs $\chi^2_{d_g}$; $n$ = feasibility-filtered sample, $d_g$ = actual row count. Conservative absolute test (Theorem F.1) vs soft min-subtracted test.
- $\gamma$ is **unbounded**; report `max|gamma|`. $\gamma$'s dimension never counts in the order condition.
- **Profiling** nuisance parameters is valid (Schennach): grid a parameter of interest with the others free; no CI for the profiled ones. Parameters jointly of interest are gridded jointly as a surface.
- **Dominating measure:** support is what matters; shape is asymptotically irrelevant (Remark 2.3). Schennach's example uses $\rho=1$ with independent uniform proposals (as ours); AK use hit-and-run proposals with their Gaussian-like term in the MH acceptance.
- **Auxiliary parameters** (E[e], counterfactual revenue) are jointly estimated with their own moment $E[U-\tilde\theta]=0$; never averages over a post-hoc tilted chain (Schennach 2022, p. 1250).
- **Moment design rules:** an anchor (a quantity forced to mean zero by its own moment: $\psi$, $\varepsilon$, $\omega-\mu_{\omega,j}$) times anything is a valid moment; a quantity tautological given the FOCs (e.g. $\psi\cdot e$) is not; rows in pesos must be scaled or replaced (they dominate $\Omega$); rows nonlinear in $e$ near the ceiling need a polynomially-decaying bound ($h'/(1-h')$, softsign), not $\exp$/$\tanh$ (they saturate). $h$ and $h'$ share the floor `H_DENOM_FLOOR=1e-6`.
- Mechanical identities to remember when reading $\Omega$: $\omega=\tilde{\mathcal W}-(1-\beta)\varepsilon$ and $\ln M=(\ln M^*-\mathcal V)-\varepsilon$, so $\varepsilon\ln M$, $\varepsilon\omega$ contain $-\varepsilon^2$ and $\psi\ln M$, $\psi\omega$ contain $\psi\varepsilon$.
- **Seeding:** seed every grid point independently from one validated point (no chaining across points); two NM passes per point; common random numbers within a fit. Refit noise (optimizer) is much larger than MC noise at fixed parameters — compare points only beyond it.
- Confidence sets: Theorem F.1 conservative $\chi^2$ by default; CHT (2007) subsampling is the upgrade path. More moments shrink the identified set; don't cut moments to save compute.
- **Moment rows, current kinked system** (authoritative definitions in the code comments of `grid_estimator.cpp`): 0 $\psi$, 1 $\varepsilon$, 2 $\psi\ln M$, 3 $\psi\omega$, 4 $\psi\omega^2$, 5 $\varepsilon\ln M$, 6 $\varepsilon\psi$ (dropped), 7 $\varepsilon\omega$, 8 $\varepsilon\cdot$score$_k$, 9 $\psi\ln\tau_P$, 10 share beyond kink $-s$, 11 $\varepsilon\cdot$score$_\kappa$, 12 $\varepsilon^2-\sigma^2_{\varepsilon,j}$ (corps' variance by industry; all stage-2 firms). **Design A:** interior firms only in the 9 rejecting industries; all other firms are corner firms with the pooled first stage (they fill only the $\varepsilon$ rows 1, 5, 7, 12). Top 0.5% of interior firms by $M^*$ trimmed.
- **Build flags / binaries** (`Code/C-estimator/`, clang++ `-DKINK -DKINK_S -DEPSVAR [-DKAPPA_FREE]`, NLopt + Accelerate; MacBook build uses `/usr/local`): `grid_estimator_eps_ak2` (13-row system, κ fixed via `lambdas=`), `grid_estimator_kf` (κ estimated). CLI: `cut=ak`, `qform=power_kink`, `k_fixed`, `s_fixed`, `drop_rows`, `row6=eps_psi`, `algo=neldermead`, `algo2=bobyqa`, `sa_time`; modes `lambdagrid` (fit), `adiag` (diagnostics at fixed parameters).

## Counterfactual (methods; all earlier numbers superseded, redo after stage-2 re-estimation)
- Question: revenue response to a change $\Delta$ in the purchases-side rate, $\tilde\tau_P=(1+\Delta)\tau_P$, benefit shifter $\ln((1+\Delta)\tau_P\rho_t)$ in $h$.
- **$\theta$ fixed at one non-rejected operating point; only $\gamma$ free at each $(\Delta,R)$ cell** (AK2020 Appendix F precedent; endorsed by Nail). A free $\theta$ lets $\lambda$ rationalize any $R$. Grid $(\Delta,R)$, $R$ an auxiliary parameter with its own moment; confidence set = tested points passing; AK-style bound plot (`1293-revgrid-standard-ci-plot.R`).
- **Revenue moment with a control variate:** $g^{adj}=(R_i-\hat b\,t1_i/p_t)-(R-\hat b\mu_c)$, $\hat b$ and $\mu_c$ fixed constants (or $\hat b=1$ from the revenue identity, `row9_mode=theory`). Working note `Paper/sections/9999-control-variate.qmd`.
- **Revenue:** $R_i=t1_{it}/p_t-\tilde\tau_P[M_i+(1-q(\tilde e_i))\tilde e_i]$ in real terms ($M^*$ is real, `t1` nominal; deflate `t1` only). **Known bug:** `grid_estimator.cpp`'s revenue code deflates the whole of $R$ — fix before the next counterfactual. Corner firms ($\tau_P=0$): no detection term.
- New evasion per firm is a deterministic function of its latent $M_i$, clipped at 0 (corner solutions do occur as $\Delta$ moves); under non-linear $q$ solve $e'(\Delta)$ by bisection. Under linear $q$ and linear $\kappa$ only: $e'(\Delta)=[e_i+\Delta/(2\lambda)]/(1+\Delta)$ — a firm-invariant intercept, a knife-edge property that any curvature in $q$ or $\kappa$ breaks. Keep $\Delta$ small (types held fixed).
- Optional robustness: $R_i\ge0$ cap ($\equiv$ capping $e'$, since $R_i$ is monotone in $e'$ on the domain).

## Prior empirical exercises (reduced form) → structural ingredient
1. Deconvolution (`700-deconvolving-evasion.qmd`) — is the first stage.
2. 1983 fiscal reform (`300-fiscal-reform.qmd`) — evasion responds to $\tau_P$: motivates identification of the detection parameters.
3. Evasion and size (`400-tax-evasion-het.qmd`, Carrillo 2022) — the pattern convex $\kappa(e,\omega)$ rationalizes.
`500-geo.qmd` (geography) parked.

## File map
- **Thesis/JMP:** `Thesis/` (Quarto book, Western template; read `Thesis/PLAN.md` fresh before touching it; `Thesis/REUSE-LOG.md` maps reused text/code). Assets: `Code/Thesis/chNN-<slug>.R` → `Thesis/figures|tables/`; chapter `.qmd` files contain no computation. `JMP/paper.qmd` includes the Thesis chapters (`when-meta="jmp"` gates; own intro `JMP/sections/01-intro.qmd`, RAP 2; title page `JMP/partials/title.tex`). `Paper/` is superseded by `Thesis/`.
- **Slides:** `Quarto-Slides/Tax-Prod.qmd` (order: `200-model` → `620-ELVIS` → `650-stage2-prelim-results` (audience Tim; no $\eta$/debugging content) → `600-opt-tax` (estimation → counterfactual pipeline) → `700-deconvolving-evasion` → `900-appendix` → `610-MSL` (expert deep dive: MSM rejection, MSL derivation, cross-test)). `300`, `400`, `500-opt-tax-claude` (superseded), `500-geo` commented out.
- **Working notes (not supervisor-reviewed):** `Paper/sections/9999-elvis.qmd`, `9999-msl-implementation.qmd`, `9999-density-trans.qmd`, `9999-detection-q.qmd`, `9999-tax-wedge.qmd`, `9999-control-variate.qmd`. Approved, read-only: `56-id-evasion.qmd`.
- **Code:** `Code/C-estimator/grid_estimator.cpp` (ELVIS engine; moment formulas authoritative in its comments and in `Code/Rcpp/1200-stage2-elvis-common.h`); `Code/C-estimator/msl_estimator.cpp` (MSL, `Makefile.msl`); `Code/Deconvolution/15xx` (re-estimation pipeline and stage-2 runs, `run-15xx-*.sh` launchers, outputs `Code/Products/15xx-*`); `1400`–`1451` (MSL harness, model-faithful DGP `1410`, cross-test `1440`–`1444`, 321 DGP). Stage-2 inputs: `Code/Products/1572-stage2-input-designA-tau-sig2eps-trim0.005.csv`. `Code/MANIFEST.md` (regenerate `Rscript Code/000-manifest.R`) = dependency scan.
- **Compute:** Mac mini (12 threads; run 3 fits × 4 threads). MacBook (Intel, see memory for SSH/IP/screen/caffeinate details; bring results back file by file with a `-macbook` suffix). Detach long runs with `nohup` + `caffeinate -dims -w <pid>`.
- Bibliography: `Quarto-Slides/biblio/b100424.bib`, `b100422.bib`.

## Working conventions
- **Presentation framings:** thesis/job talk — reduced-form exercises as motivating evidence for structural features. Supervisor meetings — does the prior result hold or break; **one topic per meeting**; never bundle the estimation/counterfactual pipeline with a legacy audit.
- **Show planned content before editing** slides, derivation notes and thesis text — draft in chat, agree, then write. Always confirm before erasing anything (the user may want superseded text kept, commented out).
- Slide audience: supervisors first, then job talk — first-year PhD / non-specialist level, intuition before formalism.
- Keep the symbol $h$ generic so slides survive functional-form changes.
- `.qmd`: `$...$`/`$$...$$` math, `{#eq-...}` labels via `@eq-...`; never define a crossref label with raw `\label{}` inside a `{=latex}` block. For layout warnings, compile the kept `.tex` with 2-pass `lualatex -interaction=nonstopmode`.
