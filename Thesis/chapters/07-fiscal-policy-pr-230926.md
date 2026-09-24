# Proof-read: `07-fiscal-policy.qmd` — 2026-09-23 (revision 2)

Replaces revision 1, which was written when the chapter was still all outline. The chapter now holds your earlier prose, pasted from `Paper/sections/965-DiD.qmd`, the slides `300-fiscal-reform.qmd` and the slides appendix `900-appendix.qmd`, with the model's notation. The source map is in `Thesis/REUSE-LOG.md`. Line numbers refer to the file as of this revision.

## Applied

- **Single-tax story:** $\tau$ instead of $\tau_P$ everywhere in visible text.
- **Draft-only notes:** UPDATE/FILL items, the Open items, and the original outline bullets (one "check coverage" block per section).
- **Prose pasted** under each heading, with two new subsections from your own material: *Changes in true materials cannot explain the pattern* (slides appendix) and *Overreporting before the reform* (`965-DiD`, commented-out pre-trend text, without the parallel-trends paragraphs).
- **Notation:** $s\to\mathcal V$; $\gamma_j$ dropped ($\mathcal V^C=-\varepsilon$); $e\to u=\ln(1+e/M)$; $\mu$ kept (population mean of $u$; u-bar was tried and dropped because a bar reads as a sample mean); $\Delta_t\to\Delta\mu_t$; $\eta\to\zeta=\nu-\varepsilon$. This resolves revision 1's notation-clash table (§B).
- **Terminology and conventions:** "non-Corporations" → "unincorporated firms"; "taxed" → "ST-liable"; "I" instead of "we"; Canadian *behaviour*.
- **Typos:** "onlyt", "rate of the corporate income tax" → "or the", "This decrease in would", "Propietorships".
- **Figure references** repointed to the thesis figures.
- Renders cleanly; all equation and figure cross-references resolve.

## Resolved from revision 1

- **A1 (pre-reform coefficients not zero):** resolved by your framing. With $\mathcal V$ as the outcome, this isn't a DiD design, so no parallel-trends argument is needed. The pre-1983 movement is evasion itself (firms anticipating the reform, behaving well while lobbying), which the new *Overreporting before the reform* subsection explains. Still to write: the framing paragraph itself (O1 below).
- **A2 (why are the difference CIs tight):** checked in the code. SEs are two-way clustered by plant and year (`921.1-DD.R`).
- **Notation clashes (§B):** resolved by the switch to model notation.

---

## Open — need your call

**O1. Write the "not a DiD" paragraph** (draft note in §Empirical approach). Your argument, as noted: since $\varepsilon\perp(e,M)$ and under Cobb-Douglas, the differences with respect to Corporations *are* the evasion changes over time. It's the chapter's key methodological point, and supervisors will remember the switch to $\mathcal V$, so it deserves its own paragraph right after @eq-fiscal-diff-reg. Two facts from the code can support it:
- Corporations enter as one baseline pooled over all years, with no year effects of their own. That is exactly what the argument licenses: under Cobb-Douglas their $\mathcal V$ is $-\varepsilon$, which doesn't move with prices.
- With industry fixed effects, the log-share and $\mathcal V$ regressions give identical year coefficients (O2).

**O2. Outcome variable: RESOLVED (2026-09-23).** Reverted to the log materials share $s_{ijt}$, with $\ln\beta_j$ as industry fixed effects (not $\gamma_j$, which is taken by the AR(1) productivity parameters). The text now matches the code, and because $\ln\beta_j$ is estimated jointly with the year effects, no stage-1 estimate enters the regression and no bootstrap is needed. $\mathcal V$ remains the outcome only in the size-heterogeneity analysis.

**O3. CIT is not in the model: RESOLVED (2026-09-23).** Paragraph replaced: the model's channel is the sales tax (a higher $\tau$ raises the credit on each peso of fictitious inputs); the income tax is presented as an extension of the same logic; and the net effect is ambiguous because the 1983 reform raised the sales tax while cutting income-tax rates.

**O4. Your two sources disagree on ST-liable Proprietorships** (§LLCs vs. proprietorships, last paragraph; draft note in place). The Paper text says they are "not significantly different from Corporations, except from 1986 to 1988". The slides say "an increase between 1986 and 1989". Check against @fig-fiscal-prt.

**O5. Takeaways: RESOLVED (applied 2026-09-23).** Rewritten as prose, framed as *empirical* (not reduced-form) evidence, since it uses the model's structural objects and assumptions. It follows the net-incentive ranking (ST-liable > exempt; LLCs > proprietorships) and flags the exempt-proprietorship exception, linking to @sec-fiscal-jo. It bridges to $\partial e^*/\partial\tau>0$ and to the identification of $\lambda$. The asymmetry claim was dropped: with the LLC cut described as small, it had no evidence behind it. Housekeeping: the five "outline, check coverage" draft blocks for the written sections were removed; the opening's outline block stays.

**O6. The 1983 reference year: RESOLVED (applied 2026-09-23).** 1983 is the **last full year before the reform took effect**:
- Decreto 3541 was issued on 29 December 1983, and the VAT regime applied from 1 April 1984 (art. 92), so 1984 is a partial year.
- The panel's year is the activity year: DANE collects year $x$ in $x+1$ and labels it $x$.
- The observed sales-tax rates confirm both: 6% in 1981–83, 9% in 1984 ($=0.25\times6\%+0.75\times10\%$), 10% from 1985, and 12% in 1991, the year Ley 49 de 1990 took effect.

Details, table and sources are now in ch. 3 (@sec-setting-st, @tbl-st-by-year). In ch. 7, both "1983, the fiscal reform year" phrases and the "year preceding the fiscal reform" phrase now read "1983, the last full year before the reform took effect".

The Bird–Wiesner sentence was also corrected. The mission was on intergovernmental finances, not "the Colombian tax system", and `@Perry1990` doesn't mention it; it is now cited to @JunguitoRincon2004 and @Bolanos2019. The next sentence ("These changes aimed to modernize…") was reworded to match its source, @Sanchez1994 (your lit notes, `Paper/sections/30-lit-rev.qmd`, "### @Sanchez1994"): the aims were to reduce the fiscal deficit without money financing and to reactivate investment through tax incentives, broadening the base and simplifying collection. PDF not in `Lit-Papers/` yet.

**O7. 1986 and 1990 reforms** (Open items). The 1986 paragraph is parked in a draft note; it decides whether the 1986–89 Proprietorship rise gets its own explanation.

**O8. Falsification check** (draft note in §Changes in true materials). `98-fiscal-ref-col`'s sales-timing check (sales grew during 1983, the share only the year after; sales fell in 1986) supports the $M$-alone argument directly. It needs its figure regenerated. Include it or not?

**O9. Standard errors, levels vs. differences (applied 2026-09-23).** The slides' "heterogeneity is constant over time" reading is replaced with the mechanical explanation Salvador gave: the level estimates share one baseline (corporations pooled over all years plus the industry fixed effects), so they covary positively, and the shared error cancels in the differences. Also:
- Industry fixed effects $\gamma_j$ are back in @eq-fiscal-lvl-reg and @eq-fiscal-diff-reg. They absorb $\ln\hat\beta_j$ and its estimation error, so no $\hat\beta$ sampling variance needs to be propagated, and the estimates and SEs equal the log-share regression's exactly.
- Draft note added with two inference checks: only 11 year clusters for two-way clustering (wild cluster bootstrap, or plant-only clustering), and composition (plant fixed effects).

**O10. Income-tax narrative after the Perry–Cárdenas check (2026-09-23).** Ley 9 de 1983 *cut* income-tax rates relative to what firms paid in 1981–82: individuals, hence Proprietorships, got a large cut, and LLCs got 20→18% as compensation for presumptive income. So the net incentive in ST-liable industries is ambiguous for both, and the table evidence fits: LLCs (little net compensation) rose sharply; Proprietorships (compensated almost as much as the ST increase) barely moved in 1984–85. Still to reconcile, for you to decide:
- **Proprietorships in ST-exempt industries rose** (Δ 0.026–0.068***, 1984–88, @tbl-fiscal-prt), but with only an income-tax cut they should have fallen. This is the one result the new narrative doesn't explain, and it also drives the "slight increase" for all unincorporated firms in exempt industries (@tbl-fiscal-liable).
- **The 1986 reform cut the top individual rate again (49→30%),** yet ST-liable Proprietorships rose in 1986–88.
- **The asymmetry takeaway loses its evidence.** LLCs in exempt industries faced a roughly neutral net change (the rate cut offset by presumptive income), not a clear decrease.
- **Text passages that relied on a CIT increase** need rewriting: the incentives paragraph, "the effect of the increase in income tax rates for individuals… dominates", "LLCs faced two opposing effects", and the takeaways bullet "after increases in ST and CIT".

**Update (applied 2026-09-23):** results rewritten to lead with the ST-liable comparison (option C). The exempt-Proprietorship rise is stated as unexpected, with two possible explanations: (1) marginal rates fell much less than average rates at both ends of the distribution (@tbl-marg-tax-1983, computed from Perry and Cárdenas's Cuadro III.1); (2) individuals lost full deductibility of financial expenses in 1982–83 [@Ocampo1983]. Proprietorships' larger SEs are attributed to heterogeneity (record-keeping, process standardization, risk attitudes); LLCs tend to be larger firms with more established processes. Old "income-tax increase dominates" passages removed. Still open from this item: the 1986 reform and the asymmetry takeaway.

Coefficient tables added after each figure (@tbl-fiscal-all, -liable, -llc, -prt; scripts `Code/Thesis/ch07-fiscal-*-table.R`).

## Proof-reading the pasted prose (wording only, your call)

| Where | Current | Suggestion |
|---|---|---|
| Opening (l. 5) | Two sentences, no main message | The chapter's point is missing from the visible text: the reform is the clean test of $\partial e^*/\partial\tau>0$ that the structural model can't get from a regression. It's in the outline note; one sentence here would set up the whole chapter |
| Empirical approach (l. 23) | "Taking the FOC and stacking over industries: for Corporations…" | Slide phrasing. "From the materials FOC, for Corporations, the truth-reporters, …" |
| Empirical approach (l. 49) | $E[\mathcal V\mid D^N=0]=0$ | Relies on $E[\varepsilon]=0$ from stage 1; say so in a clause |
| Empirical approach (l. 78) | "Likewise, the following regression specification is employed to estimate…" | Passive. "Likewise, I estimate @eq-fiscal-diff with…" |
| $M$ section (l. 108) | "How? Suppose $e$ is unchanged." | Slide style. Fold into one sentence: "If $e$ were unchanged, the rise in $u$ would require unincorporated firms to lower their true inputs, through…" |
| $M$ section (l. 108) | "According to the FOC, taxes do not show in the optimal choice of firms." | Name the FOC: "the materials FOC" (under the single tax, $\tau$ cancels from it) |
| $M$ section (l. 108) | "My argument is more transparent" | Tone. "The evasion channel is more direct:" |
| Results, all firms (l. 115) | "reached an average of 8%" | 8% of what? $u\approx e/M$, so "about 8% of true materials" |
| Pre-reform (l. 122, 126) | Bold phrases carried over from the Paper | Bold isn't used in running prose elsewhere in the thesis; unbold |
| Liable vs. exempt (l. 132) | "increase … slightly" / "increased … gradually" | Tense switches between present and past within the paragraph. Pick one (past, for a historical episode) |
| Throughout | "8%" / "eight percent" / "8 percent" | Pick one number style |
| Throughout | "tax evasion" / "overreporting" / "input overreporting" | Used interchangeably. Pick one for the measured object (e.g. "overreporting") and keep "tax evasion" for the behaviour |
| Throughout | "Corporations", "Proprietorships" capitalized | Check against ch. 3–8, where these are lowercase in places |
| LLC vs. PRT (l. 149) | "grandpa's workshop vs. young entrepreneurs" | Slide register; e.g. "family workshops vs. young entrepreneurial firms" |

## Figures (unchanged from revision 1)

- All four are copied slide PNGs (705 px, slide styling, baked-in titles). They need `Code/Thesis/ch07-*.R` scripts (O2), at thesis resolution, Canadian spelling, and with no baked-in title.
- Captions are one line ("LLCs.", "Proprietorships."). Each needs what's plotted (coefficient and 95% CI by year; levels vs. difference to 1983), the sample, and the clustering (two-way, plant and year).
- @fig-fiscal-llc and @fig-fiscal-prt could be one two-panel figure; the text always discusses them together.
