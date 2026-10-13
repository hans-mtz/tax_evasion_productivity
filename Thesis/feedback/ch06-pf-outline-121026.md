# PF and productivity (`06-pf-productivity.qmd`): approved outline, 2026-10-12

Built blind (ch. 6 not read): reader state from the intro, chs. 2–5 and the approved ch. 4/5 outlines. Approved by Hans on 2026-10-12 with the decisions below.

## Reader state (at ch. 6's first line)

**Knows:** Cobb–Douglas technology (ch. 2 eq-pf), materials flexible, K and L predetermined or dynamic, ω known to the firm (simultaneity, ch. 2 l. 55), ε measurement error; materials FOC and net share $s=\ln\beta-\varepsilon$ (ch. 2, ch. 4); $\ln\hat\beta=E[s\mid\text{corp}]$ (ch. 4); $\mathcal V=u-\varepsilon$, the test (ch. 4); the overreporting distribution in five industries (321, 322, 324, 342, 369; ch. 5). The intro promises: productivity net of overreporting (l. 19); uncorrected [@Gandhi2020] elasticities larger, productivity more dispersed and less persistent, 90/10 66 percent larger, half-life shorter (l. 21, l. 30); the mechanism (l. 30).

**Asks:**
1. β comes from corporations: how are α_K, α_L and productivity obtained, given simultaneity and misreported materials? (intro l. 19, ch. 2 l. 55)
2. Why do standard methods go wrong, in which direction for β? (intro l. 30)
3. How large are the distortions: elasticity, dispersion, persistence? (intro l. 21, l. 30)
4. Which industries: the same five as ch. 5?
5. (ch. 8) How do the corrected parameters feed the structural model?

**Inconsistency:** intro l. 21 is stale against ch. 5 ("7" industries, "14.5 percent in the median industry", half-life "52 percent"; now five industries, range 4.5–23.1, half-life 63 percent).

**Job:** show how to estimate the remaining production-function parameters and productivity when materials are overreported, and how much ignoring overreporting distorts the materials elasticity and the dispersion and persistence of productivity.

**Dependencies out:** ch. 8 takes $(\beta,\alpha_K,\alpha_L)$ and $\tilde{\mathcal W}_{it}$ from `@sec-pf` (l. 230, 337); intro, abstract, conclusion quote the elasticity and productivity findings; appendix E points back for the method. No ch. 6 equation or table label is cited elsewhere.

## Outline

**Takeaway.** Correcting for overreporting lowers the output elasticity of materials and makes productivity less dispersed and more persistent: in the median of the five industries, the uncorrected estimates [@Gandhi2020] give a 90/10 ratio 66 percent larger and a half-life of productivity shocks 63 percent shorter.

1. **Standard methods treat reported materials as true; overreporting biases them in a known direction.** Reported share $=\ln\beta-\varepsilon+u$: share-based β overstates the elasticity; the productivity residual absorbs overreporting, which varies with productivity (not classical error). (Q2)
2. **β from corporations; the other parameters and productivity from a residual free of overreporting.** $\tilde{\mathcal W}_{it}=\omega_{it}+(1-\beta)\varepsilon_{it}$ for every firm; Markov process for ω and an instrument dated $t-2$ identify $\alpha_K,\alpha_L$; point = minimum of the test-inversion statistic whose region is reported. **Theoretical identification only in the text** (Hans). (Q1; hands $\tilde{\mathcal W}$, $(\beta,\alpha)$ to ch. 8)
3. **Same five industries as ch. 5**; all nine in appendix E. (Q4)
4. **Corrected elasticities are lower than the uncorrected ones in every industry**: corrected vs GNR vs **OLS** (the standard benchmark; Hans). (Q3a)
5. **Corrected productivity is less dispersed and more persistent**: 90/10 and half-life, medians 66 and 63 percent; the productivity distribution is net of measurement error (same deconvolution as ch. 5). (Q3b)

## Exclusions
- Estimation mechanics (moment derivations, grids, re-estimated Ω, instruments $m^*_{it-1}$ and the joint system) → appendix E, new section.
- Single-tax vs net-of-tax first stage → appendix D, next to `@sec-app-single-tax` (Hans: keep single tax in the appendix).
- 313 → out (ch. 5 footnote).
- Productivity premia by group (GNR Table 3) → post-submission (PLAN §5).
- Motivating raw-data figures → none.

## Decisions (Hans, 2026-10-12)
1. Main table keeps OLS with corrected and GNR.
2. Estimation details to appendix E; text keeps the theoretical identification.
3. Single-tax robustness to the appendix (D).
4. Productivity dispersion stated as net of ε.
5. Fix the intro numbers (7 → 5 industries for the PF/productivity claims, half-life 52 → 63, ch. 5 headline).
