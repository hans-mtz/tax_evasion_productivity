# Proof-read: `08-counterfactual.qmd` — 2026-09-23 (revision 2)

This replaces this morning's report. Every typo, misspelling, and grammar or form fix is already applied in the chapter. **This report lists only what needs your call**, because fixing it would change the concept, the argument, or a convention. Line numbers are for the file as of this revision.

Conventions in force (settled today):
- **Single-tax story** in all visible text. Two-tax material is draft-only (`when-meta="draft"`), and the switch comes after Friday (PLAN.md §7b).
- $e_i$ = baseline evasion; $e'_i(\Delta)$ = counterfactual evasion. $\tilde\tau_{it}=(1+\Delta)\tau_{it}$.
- Moment 9 is shown as the conceptual score $\partial h/\partial\lambda\cdot\varepsilon$. The bounded transform is an implementation detail for the appendix.
- **Canadian spelling:** applied *labour*, *endeavour*, *coloured* across `Thesis/` and `JMP/sections/`.

---

### Resolved since revision 2 (applied)
- **A1 → option (a):** l. 327 now says sales-tax revenue $\tilde\tau_{it}P_tY_{it}$ moves only mechanically (output fixed, no evasion response), and that $R$ holds it at baseline $t1_i=\tau_{it}P_tY_{it}$. "Policy-invariant" is replaced with "no evasion response". The Robustness bullet (l. 352) says sales-tax revenue is held at baseline $t1$.
- **"Conservative test" replaces "hard test"** everywhere: ch. 8, appendix A ($TS_{\text{cons}}$), and the labels on the 8 regenerated ch. 8 figures and tables. File names and internal R variables are unchanged. Recorded in PLAN.md §1.
- **Moment-9 transform** is now documented in @sec-app-elvis → Estimation mechanics. The chapter keeps the conceptual score.
- **Two-tax draft block:** the penalty is now $\phi\tau_P\rho_te$ (l. 94), so the "Simplifying" line follows. Remaining typo fixed ("vesion").

### New, from the regenerated assets — RESOLVED (applied)
- N1 and N2 are fixed. Both CI figures now say "Tax-rate shifter Δ". The Laffer footnote no longer cites the log. The Claims caption no longer says $t1$ is "fixed and unaffected" and is wrapped so it isn't cut off. Figure text is about 35% bigger (base 12 → 16pt). Table headers now read $m^*_{it-1}$ and $\tilde{\mathcal W}_{it-2}$.
- Original notes, for reference:
- **N1. Laffer figure** (`ch08-laffer-ci-hard.png`): the x-axis says "Purchases-tax shifter Δ", which is two-tax wording in the single-tax story. The footnote cites "see Research-log/log.md", an internal reference readers can't see. Both come from `Code/Deconvolution/1300-*`, which also feeds the slides. I can override them in the thesis wrapper, as I did for the title.
- **N2. Headline estimates table:** column headers read $m^*_{t-1}$ and $\tilde{\mathcal W}_{t-2}$, without the $it$ subscript the text uses ($m^*_{it-1}$).

## A. Consistency of the counterfactual experiment (most important)

**A1. ~~Is sales-side revenue fixed or not?~~** Resolved (see above).

**A2. Two-tax language still in visible text:**
- l. 54: "…when there are two different sales taxes on sales and purchases, the government can affect the relative prices…" is a two-tax argument inside the single-tax story. Drop it, or move it to the draft block.
- l. 21: "the sales tax *on the purchases* side". Under a single τ, does the government change "the sales tax" or specifically the credit side? This ties to A1.

**A3. The question vs. the optimization claim.** l. 21 asks for the *marginal change* in revenue. l. 46 says "the government chooses the tax rate to *maximize* expected tax revenue". Your own outline note (l. 61) says it's not a maximization. Pick one framing.

**A4. Which object is the headline?** The chapter title says "Revenue", the first section says "Laffer Curve" (l. 27: "downward-sloping Laffer curve"), and the headline result is Claims (l. 327), which slopes *upward* in $\Delta$. A reader will be confused about which curve slopes which way. Make the title, l. 27, and §Results name the same object.

**A5. "with a kink at the current state" (l. 27).** No later section shows or names a kink. The pure-simulation figures that showed it were dropped. Either point to the evidence (the jump from $\Delta=0$ to $+0.5\%$ in @tbl-cf-claims) or reword to "asymmetry".

**A6. $t1$ in visible text (l. 327, 352, 368).** It is now defined at first use ($t1_i=\tau_{it}P_tY_{it}$). It is still the data column's name used as a math symbol, though; a conventional symbol (e.g. $S_i$) would read better. Your call.

**A7. Real vs. nominal (l. 306 vs. l. 332).** The $\theta_C$ moment is written in nominal terms, but results are "real mean claims". One clause on deflation (by $P_t$ at aggregation) would close the gap.

**A8. The intercept, two ways (l. 304 vs. l. 347).** l. 304 calls $\Delta/[2\lambda(1+\Delta)]$ the intercept. l. 347 calls $\Delta/(2\lambda)$ "the common intercept". Both are correct (inside vs. outside the $1/(1+\Delta)$ scaling), but use one.

---

## B. Model and estimation content

**B1. "Convex in productivity" (l. 123, 131) vs. "U-shape" (l. 264).** Both are technically true. Consistent wording helps the reader connect the Carrillo argument to the estimates.

**B2. Carrillo paragraph (l. 131).**
- The main idea is buried. Consider opening with it: *the U-shaped cost rationalizes the hump-shaped evasion-by-size pattern.*
- Carrillo is about **size** and your cost function is about **productivity**. One linking clause is missing.
- The last two sentences (detection cannot explain the decline at the top) are the key argument but read as an afterthought. Say it directly: the decline must come from cost, since detection would have to fall with evasion.

**B3. l. 123: "Since we do not observe enforcement, it is treated as constant".** What does "it" refer to: enforcement intensity $\lambda$, or $q(\cdot)$ itself? Also check that "consistent with a tax authority that audits a firm whenever its overreporting exceeds a random threshold" (my fix this morning) says what you mean.

**B4. Normalizations are not stated (after l. 171).** Moment 1 needs $E[\psi]=0$, with $\delta_0$ absorbing the level. Also, ch. 2 normalizes $E[\exp\varepsilon]=1$, while moment 2 imposes $E[\varepsilon]=0$. If stage 1 re-centres $\varepsilon$, say so here or in ch. 5.

**B5. Which assumptions license the moments (l. 226).** "I leverage independence assumptions". Naming them in one line would let the reader check each row: $\varepsilon\perp(M,e,\omega)$; $\psi\perp(\omega,M)$.

**B6. Notation clash and measure wording (l. 206).**
- $\pi$ is the data distribution here and profit elsewhere (ch. 2, and the draft block).
- "$\mu\times\pi$, the product measure" is imprecise, since $\mu$ is conditional on $Z$. "Joint distribution implied by $\mu$ and $\pi$" is more accurate.

**B7. The warning appears twice (l. 222 and l. 224).** Both say "don't treat the entropy-maximizing distribution as the truth". Keep l. 222's identification argument as its main point, and move the warning to open l. 224, where it motivates the auxiliary parameter.

**B8. Citation (l. 222).** `@Schennach2021, p.1250`: the bib entry is the 2021 report, but p. 1250 is a page in the 2022 *JEL* version. Update the entry or drop the page number.

**B9. Terms used but not defined in this chapter:**
- "Conservative test" is now the single name, but it is still **not defined** in the chapter. One sentence near l. 242 would do: "$TS=2n\hat L_n$ compared directly against $\chi^2_{d_g,.95}$, with no credit for profiling; details in @sec-app-elvis." "Soft test" appears only in l. 256 (the cube count); either define it too or drop that half-sentence from the main text.
- The 0.5% trim (l. 273, 282, 385) is not mentioned in §Estimation. One clause at l. 244 (32,232 is *after* the trim) would do.
- $\hat\Omega^-$ (l. 367) and the control variate (l. 327, 367–372). The CV is introduced only in a comment (l. 316). Add a short intro before Robustness uses it, or cut the references.

**B10. Fixing θ (l. 311).** I added one sentence this morning: θ fixed at the best point, only γ re-optimized. The AK2020 precedent is still only in the comment (l. 308–310). Do you want it cited in the text? Supervisors will ask.

---

## C. Figures and tables (the self-explanatory standard)

| Asset | Referenced in text? | Caption |
|---|---|---|
| @fig-cf-claims (l. 325) | **No** | Short: add axes, what the dots and ✕ marks mean, θ fixed, $\chi^2_{10}$ |
| @tbl-cf-estimates (l. 259) | **No** | **None** |
| @tbl-cf-detection (l. 268) | **No** | **None** |
| @tbl-cf-claims (l. 329) | Yes | OK |
| @tbl-cf-claims-headline (l. 339) | Yes | **None** |
| @fig-cf-laffer (l. 354) | **No** | Short (same as fig-cf-claims) |
| @tbl-cf-ci (l. 356) | **No** | OK |
| @tbl-cf-headline (l. 362) | **No** | **None** |
| @fig-cf-theory-cv (l. 370) | **No** | "Theory-coefficient vs. CV passing points." needs CV defined (see B9) |

---

## D. Style conventions (your call, whole book)

- **"I" vs. "we":** mixed in this chapter (l. 24, 123, 226, 311 use "I"; l. 56, 133, 185, 220 use "we"). "I" is standard for a sole-authored JMP.
- **More Canadian spelling?** I standardized only the *-our/-oured* words. Canadian usage also prefers *modelling* (l. 131; ch. 3, appendix D, JMP intro have *modeling/modeled*) and *grey* (captions l. 325, 354: "gray band"). Say yes and I'll sweep both.
- **Currency:** l. 263 and l. 367 use "\$", while the tables say COP. Pick one.
- **Headers still in outline form:** §Estimates, §Discussion, §Results, §Robustness, §Scope are bullets. Prose is yours. When you write them, lead each paragraph with its main claim. For §Discussion that is "$\hat\lambda$ is small because a thin tail of aggressive evaders identifies it."
