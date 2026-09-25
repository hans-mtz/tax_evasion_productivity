# JMP introduction — three RAP options (reference, 2026-09-24)

Kept for targeting specific audiences when submitting. **Chosen for the JMP: RAP 2 (tax rates).** Earlier review of the pre-RAP intro: `jmp-intro-rap.md` (same folder). Literature map behind the P statements: draft-only table in `Thesis/chapters/09-literature.qmd`.

## A (fixed across all three; only its opening words change to fit each R)

Tax-motivated input overreporting can be separated from productivity in standard firm-level data, and doing so shows it is large, rises with the tax rate that rewards it, and is misread by standard methods as technology. Overreporting is detected in 9 of 20 Colombian manufacturing industries, at 11–25% of true materials where the evidence is strongest. It rose after the 1983 sales-tax increase where the rate went up, and in the estimated model a 0.5% rate increase raises claimed deductions by 16–28%, while a cut becomes distinguishable only at −8%. Ignoring it inflates materials elasticities in all five industries studied.

## RAP 1 — Measurement (public finance; validation samples)

- **Audience:** public finance / tax compliance, measurement-focused journals.
- **P:** Measuring firm evasion has required confidential tax records [@Carrillo2022; @Zumaya2021]. The alternative, validation-sample methods, infers evasion from how reported income departs from an indirect measure of true income for a group that cannot misreport [@Pissarides1989; @Gorodnichenko2009], but these were built for individuals: for firms, the departure also reflects productivity.
- **R:** Can firms' cost overreporting be measured, separately from productivity, from reported inputs and output alone?
- **A opens with:** "Yes. Using the production function as the relation to compare against, and corporations as truth-reporters, …"
- **Checks:** P makes space for R — yes (both routes fail for firms). P idly leads to A — yes ("so can it be done for firms?"). A answers R — yes; the tax-rate and productivity findings become consequences of being able to measure it.
- **Trade-off:** misses much of the tax-rates/revenue literature.

## RAP 2 — Tax rates (public finance; revenue and policy) — CHOSEN

- **Audience:** public finance, tax policy, VAT design.
- **Why chosen (Hans, 2026-09-24):** policy (ch. 7) and the counterfactual (ch. 8) use all industries, while deconvolution and production-function estimates cover a subset; leading with policy moves the testing results and the industry selection to the background. The selection question then moves to ch. 8's assumption that all unincorporated firms with a positive tax rate evade, which ch. 7's pooled all-industry result supports.
- **P (drafted):**

  > How tax revenue responds to tax rates is well measured for individuals' reported income, whose elasticity bundles real responses with evasion and avoidance [@Saez2012; @Chetty2009], and theory implies that rates should be lower where evasion responds more to them [@Cremer1993]. For firms, the evidence comes mainly from bunching at kinks and notches in the tax schedule [@Best2015] and from misreported imports [@FismanWei2004]. How firms' overreporting of inputs, the fraud that the credit on purchases in sales and value-added taxes invites, responds to the tax rate has not been estimated. The difficulty is that overreported inputs cannot be seen in the data: a firm reporting high input use for its output may be overreporting, or it may simply be unproductive.

- **R and A (drafted):**

  > This paper asks whether input overreporting rises with the tax rate, and by how much it moves the deductions firms claim. It does. After Colombia raised its sales-tax rate in most manufacturing industries in 1983, overreporting by unincorporated firms rose, reaching about 8% of true materials by 1987, and the increase was concentrated in the industries where the rate went up. In a structural model estimated on all industries, a 0.5% increase in the sales-tax rate raises claimed deductions by 16 to 28%, far more than the 0.5% mechanical effect, while cuts lower them only gradually: the smallest cut distinguishable from current policy is 8%. As a ballpark, undetected overreporting costs about 3% of the revenue this channel would otherwise collect, and near the current rate a 1% increase in the rate lowers revenue by about 20% (@sec-app-backofenvelope).

- **Ballpark sentence (added 2026-09-24):** point estimates from forward simulation at the fixed operating point (`Code/Deconvolution/1480-revenue-elasticity-backofenvelope.R`): loss of COP 53.32 real per firm-period = 3.04% of potential revenue from the credit channel; revenue elasticity −20.3 approaching from an increase, −9.7 from a cut (kink at the current rate). No confidence intervals yet (deferred, ch. 8 open items), so worded as "ballpark" and kept second to the test-inverted claims result. Body home: appendix A, `@sec-app-backofenvelope` (prose still to write). Single-tax wording ("the rate"); the two-tax ("purchases-tax") wording stays in draft-only notes.
- **Checks:** P makes space for R — yes (known: individual revenue response, theory, firm bunching and imports; missing: input overreporting's response). P idly leads to A — yes (P's last sentence raises "does it respond?", answered by A, and "how can you see it?", answered by the method paragraph). A answers R — yes ("It does" + the counterfactual magnitudes).
- **Notes:** "0.5% increase" is a relative change in the rate (ch. 8 scales it as $(1+\Delta)\tau$). Only the increase is compared with its mechanical effect: the −8% cut's bounds (−15.7% to −5.7%) contain the −8% mechanical effect. The production-function contribution moves to the supporting findings and the literature paragraphs.
- **Planned structure:** 1 hook (VAT credit, revenue at stake) → 2 P → 3 R + A → 4 how (confound + method) → 5 supporting findings (testing, deconvolution, productivity) → 6–8 contribution (tax rates and revenue; measuring firm evasion; validation samples; production functions secondary) → 9 roadmap.

## RAP 3 — The confound (both audiences; matches the title)

- **Audience:** mixed public finance and IO/productivity; a methods-leaning pitch.
- **P:** Validation-sample methods infer evasion by comparing a group that cannot misreport against one that can, and production-function methods infer productivity from reported inputs. For firms, each confounds the other: high input use relative to output can mean overreporting or low productivity, and studies using tax records avoid the problem only with confidential data [@Pissarides1989; @Gorodnichenko2009; @Gandhi2020; @Carrillo2022].
- **R:** How can overreporting be separated from productivity in firm production data, and what does separating them reveal?
- **A opens with:** "By using corporations as truth-reporters to pin down the common technology, so that deviations from it identify overreporting… Doing so shows it is large, rises with the tax rate, and is misread as technology."
- **Checks:** P makes space for R — yes (the confound sits between the two literatures). P idly leads to A — yes, strongly ("so how do you tell them apart?"). A answers R — yes; weakness: a two-part R.
- **Trade-off:** puts the subset-based results (testing, deconvolution, production functions) in front and the all-industry policy results behind.

## Rewriting ballparks (against the 2026-09-24 intro: 6 visible paragraphs, 583 words)

| RAP | Current text rewritten | New words (incl. results + literature) |
|---|---|---|
| 1 | ~40% | ~550 |
| 2 | ~70% | ~650 |
| 3 | ~25% | ~450 |
