# Proof-read: JMP abstract, paragraphs 2 and 3, second pass, 2026-10-01

**Target audience:** job-market paper abstract; academic but accessible, economists outside the field. Version on disk (`JMP/paper.qmd`, saved 01:42): P1 200 words, P2 133, P3 143, 476 in total. Sentences are numbered T1 to T5 (P2) and V1 to V6 (P3).

## Verdict

P3 now gives the reader the reason the 16 percent matters: methods that use a flexible input assume materials are reported correctly, and materials are exactly what firms have an incentive to overreport (V1 to V3). That answers Tim's "why do you care" point and sets up the finding (V4) and the structural model (V5, V6). Remaining problems: V1 is garbled, V2 is inaccurate for GNR, V6 is ungrammatical and drops a condition without which the elasticity is misleading, and the two numbers in V6 rest on estimates the project has discarded.

## 1. Paragraph 2

No changes needed.

- T4 now reads "...the median input overreporting, as a share of true inputs, amounts to 16 percent". The opening "Across the 7 largest industries" sets the unit, and the verb is active.
- T5 puts the figure last: "...increased the ratio of overreported to true inputs by about 9 percentage points." The stacked "between 1983 and 1987, after a fiscal reform..." is clear with the commas.
- T1 to T3 are unchanged from the first pass.

## 2. Paragraph 3

**V1: garbled compound.**
> "The magnitude of detected overreporting is relevant for the estimation of production functions methods that exploit a flexible input for the identification of productivity [@Levinsohn2003; ...]."

- "estimation of production functions methods" needs "methods of estimating production functions" (or "production-function estimation methods").
- "is relevant for" is a "to be" construction; "matters for" names the action. "Detected overreporting" is the common-parlance phrase Tim flagged; "the overreporting I find" avoids it.
- Suggested: "The magnitude of overreporting I find matters for methods of estimating production functions that exploit a flexible input to identify productivity [@Levinsohn2003; @Doraszelski2013; @Ackerberg2015; @Gandhi2020]."

**V2: "can be inverted" is not true of GNR.**
> "These methods typically assume that materials are flexible and can be inverted to solve for productivity."

- Levinsohn–Petrin, Ackerberg et al. and Doraszelski–Jaumandreu invert the demand for the flexible input; Gandhi et al. use its first-order condition and do not invert (the introduction says "either inverting its demand ... or using its first-order condition"). The citation list includes both, so the sentence overstates for one of them.
- Suggested: "These methods assume that materials are flexible, so that their demand or first-order condition reveals productivity." (Also a double space after "typically" in the source.)

**V3: the key sentence, but "thus ... the overreported input" does not parse.**
> "However, materials are commonly tax-deductible and, thus, the overreported input."

- "are ... the overreported input": plural subject, singular predicate noun. "Thus" also claims that deductibility makes materials *the* overreported input, which is stronger than the incentive claim the paper supports.
- "tax-deductible" after "are" is a predicate adjective: "tax deductible" (no hyphen).
- Suggested: "However, materials are commonly tax deductible, which gives firms an incentive to overreport exactly the input these methods rely on."

**V4:** no change. It follows the citations and states the finding; the comparison behind it is against the Gandhi et al. estimator, which the cited list contains.

**V5: two causal links in one clause.**
> "...to study how government tax revenue changes due to real and fictitious claimed deductions as a response to a counterfactual tax rate."

- "changes due to ... as a response to" stacks two causes, and the chain (rate → deductions → revenue) is hard to follow.
- "Real and fictitious claimed deductions" is accurate for this estimand (total claims = credit on true inputs plus undetected overreporting), so the "fictitious" concern from the first pass is resolved.
- Suggested: "...to study how real and fictitious claimed deductions, and thus government revenue, respond to a counterfactual tax rate."

**V6: grammar, a dropped condition, and status.**
> "I find that the revenue elasticity to the left of the current rate is -9.7 and that revenue losses, as the share of potential net revenue, is 3%."

- Agreement: "revenue losses ... is" should be "are".
- "as the share of" → "as a share of". "3%" → "3 percent" (the project writes "percent" in running text). "-9.7" → "−9.7" (a minus sign).
- **"Holding the tax collected on sales fixed" is dropped.** The introduction states the elasticity under that condition (net revenue = tax on sales, held fixed, minus the credits claimed). Without it, a revenue elasticity of −9.7 reads as an elasticity of total revenue, which would be absurd.
- "Revenue elasticity to the left of the current rate" is jargon for this audience; if kept, say what it means (a cut in the rate raises revenue by about 9.7 percent per 1 percent cut). Confirm the interpretation before using it.
- "Revenue losses" does not say losses from what. The introduction's phrase is "undetected overreporting costs about 3 percent".
- Suggested: "I find that, holding the tax collected on sales fixed, the revenue elasticity to the left of the current rate is −9.7, and that undetected overreporting costs about 3 percent of potential net sales-tax revenue." The scope of the 3 percent is all unincorporated firms in the sample, not the 7 industries.

## 3. Status of the numbers in V6 (not a proof-reading issue)

- **Source:** the back-of-the-envelope appendix (`@sec-app-backofenvelope`, 2026-09-22), computed at the structural point that the plan later discarded (2026-09-28 blocker; 2026-09-30 restart).
- **Units:** the script takes `mean_R_real` from `grid_estimator`, whose revenue code deflates the whole of `R` (known bug). The elasticity is a ratio of two terms that both go through that code, and the 3 percent uses `R_real` in its denominator, so both depend on revenue levels the plan marks as affected.
- **Use:** treat both as placeholders until the counterfactual is rerun (planned Oct 4 to 7, before the Oct 13 send).
- **Consistency:** the introduction (paragraph 3) says "about −9" and the appendix says −9.68. Align them to one rounding.

## 4. Other points

- **Citations in the abstract render.** I tested pandoc's citeproc on a YAML abstract: `[@key]` produces author-year text. `@Doraszelski2013` was not in `references.bib` and would have printed "Doraszelski2013?" in bold; I added the entry (*Review of Economic Studies* 80(4): 1338–1383, DOI 10.1093/restud/rdt011), and the test now renders "Doraszelski and Jaumandreu 2013".
- **Length:** 476 words. Typical limits for a job-market abstract are well below this; consider cutting elsewhere (for example P1's "applies equally" or robustness sentences).
- **P1:** S7 still has the comma before "and a benchmark group" (first pass, section 1).

## Suggested P3 (changes in bold)

> **The magnitude of overreporting I find matters for methods of estimating production functions that exploit a flexible input to identify productivity [@Levinsohn2003; @Doraszelski2013; @Ackerberg2015; @Gandhi2020]. These methods assume that materials are flexible, so that their demand or first-order condition reveals productivity. However, materials are commonly tax deductible, which gives firms an incentive to overreport exactly the input these methods rely on.** I show that failing to correct for overreporting yields output elasticities of materials that are larger, and productivity distributions that are more dispersed and less persistent. Using the production-function parameters, I estimate a structural model of firm input overreporting to study **how real and fictitious claimed deductions, and thus government revenue, respond to a counterfactual tax rate. [Placeholder, pending the rerun: I find that, holding the tax collected on sales fixed, the revenue elasticity to the left of the current rate is −9.7, and that undetected overreporting costs about 3 percent of potential net sales-tax revenue.]**
