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

## Audit
Not started.
