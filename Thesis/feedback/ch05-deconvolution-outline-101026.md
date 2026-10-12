# Deconvolution (`05-deconvolution.qmd`): approved outline, 2026-10-10

Built blind (the chapter was not read). The reader state assumes the approved ch. 4 outline (`ch04-testing-outline-101026.md`), not ch. 4's current text. Approved by Hans on 2026-10-10, without the size-bucket section: motivation comes from theory, not observation.

## Reader state (after ch. 4)

**Asks:**
1. The test gives a mean: do all firms overreport a little, or a few a lot? (ch. 4 §3)
2. How can $u$ be separated from $\varepsilon$ when only their difference is observed? (ch. 4 §1.3)
3. What does the separation need: independence, $f_\varepsilon$ from corporations carrying over? (ch. 4 §1.2)
4. In what units is "14.5 percent of true materials", given that $u$ is a log ratio? (intro l. 21)
5. Why 7 industries and not 9? (ch. 4 §3.3)

**Job:** recover the distribution of overreporting, not just its mean, by removing measurement error with the $f_\varepsilon$ that corporations identify, and report how much unincorporated firms overreport.

**Dependencies out:**
- $\mathcal V$ (ch. 8 cites @sec-deconvolution);
- the 7-industry rule (ch. 6);
- the headline 14.5 percent (intro, conclusion);
- likely $E[u]$ by industry as the benchmark for ch. 8's fitted $u$.

## Outline

**Chapter takeaway.** Deconvolution removes output measurement error and recovers the distribution of overreporting. In the median of the 7 industries, unincorporated firms overreport on average 14.5 percent of their true materials.

1. **Why the distribution matters.** The mean hides who overreports, and the model needs the distribution: corner firms, and the extensive margin in ch. 8 (Q1).
2. **The idea.** With $\mathcal V=u-\varepsilon$, $\varepsilon\perp u$ and $f_\varepsilon$ known from corporations, $f_u$ is identified; intuition first, then one equation. The method recovers $f_u$, never the level $e$ (Q2, Q3; the estimator goes to an appendix, per PLAN).
3. **Industries chosen ex ante.** The 99 percent sharp region is non-empty and excludes zero: 7 industries, on the test's sample (Q5).
4. **Results.** Industry means and the median of the means (14.5 percent), with the conversion from $u$ to percent of true materials stated once; one figure of the recovered densities (mass near zero, right tail) (Q1, Q4).

## Exclusions
- The implementation (characteristic functions, bandwidth, grid) → appendix.
- The productivity deconvolution → ch. 6.
- The level $e$ and revenue → ch. 8.
- The response to the rate → ch. 7.
- Size buckets / one distribution or many → out (motivation comes from theory).

## Decisions (agreed)
1. Define the conversion from $u$ once in §4; the intro and conclusion quote it unchanged.

## Updates after the five-industry decision (2026-10-12)
- 7 → 5 industries (321, 322, 324, 342, 369): item 3's rule adds the variance condition ($\mathrm{Var}(\mathcal V\mid\text{uninc.})\ge\mathrm{Var}(\mathcal V\mid\text{corp.})$); 313 fails it (ch. 4's footnote explains 313). Q5 becomes "which industries go on, and why not all nine?"; ch. 4's scope paragraph is commented out, so ch. 5 answers it.
- Headline (Hans, 2026-10-12): a **range** of industry means, 4.5 to 23.1 percent (the table's numbers), not the median 14.5 / 12.4. The median of the five is 369's own mean, and 369's deconvolved $E[u]$ (0.116) misses $E[\mathcal V]$ (0.361).
- Sample: the stage-2 sample (1603) stays (Hans: option b); item 3's "on the test's sample" is dropped.
- Item 4: the densities are interior and unimodal (P10 3–18 percent), not "mass near zero"; describe what the figure shows.

## Audit (2026-10-12, agreed by Hans)

| Outline entry | Status | Where | Change |
|---|---|---|---|
| Chapter opener + takeaway | missing | l. 15 "We can do better however" (old paper) | new opener: distribution, range headline |
| 1. Why the distribution matters (Q1) | missing | — | 2–3 sentences |
| 2. The idea (Q2, Q3) | partial | l. 15–23 | intuition before the equation; "sum" → difference; $f_\varepsilon$ from corporations' residuals; "recovers $f_u$, never $e$"; Q3 conditions **in the text** (Hans): $\varepsilon\perp u$, $f_\varepsilon$ known, separability $\partial q/\partial M=\partial\kappa/\partial M=0$, constant elasticity (draft note l. 27–33) |
| 3. Industries ex ante (Q5) | missing in text | table note only | rule: 1 percent + variance condition, five largest |
| 4. Results (Q1, Q4) | partial | conversion, table, figure | prose; range headline; one sentence that the fit does not impose $E[u]=E[\mathcal V]$ (342, 369: level from the test, shape from the deconvolution) |
| §Estimation + implementation note | misplaced | | **moved** to appendix I `@sec-app-deconvolution` (`appendices/I-deconvolution.qmd`, 2026-10-12) |
| l. 108, 313 sentence | stale | | cut (ch. 4 footnote) |
| draft notes l. 112–123 | stale | | delete; heterogeneity point may become one results sentence |
| JMP heading levels | — | `##` subsections under `## Deconvolving` | `###` in the JMP |

Done 2026-10-12: $E[u]$, $E[\mathcal V]$ columns dropped from `ch05-overreporting-ratio` and `appE-overreporting-ratio-all`; ch. 4 intro 23.2 → 23.1 (the table's number).
Post-submission: mean-constrained deconvolution (PLAN §6).
