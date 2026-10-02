# Proof-read: JMP abstract, paragraph 3 (third pass, revised after Hans's comments), 2026-10-01

Version on disk (`JMP/paper.qmd`, saved 02:20): P1 200 words, P2 133, P3 152, 485 in total. Not rendered by Claude (Quarto cannot open its cache database on the SMB-mounted project; render on the Mac mini). This file replaces the earlier version of this report, whose shortening advice (merging sentences) conflicted with Tim's advice to keep more, shorter sentences.

## What "revenue" means in V6

Net sales-tax revenue: the tax collected on sales minus the credit claimed on purchases (`R_i = t1_i - tau_P [ M_i + (1 - q) e_i ]`, `1480-revenue-elasticity-backofenvelope.R`). The elasticity holds the tax collected on sales fixed. The 3 percent is undetected overreporting as a share of potential (actual plus loss) net sales-tax revenue.

## Corrections to my earlier comments

- **V3, "the incentive":** the definite article is right. It refers to the incentive created by the VAT in the first sentence of the abstract; I wrongly suggested "an incentive".
- **Merging sentences:** Tim preferred more sentences for clarity over cramped ones. The merges I proposed (S5 with S6, S7 with S8, S9 with S10, V1 to V3 with a colon) are withdrawn.

## Current P3, sentence by sentence

**V1:** "...exploit a flexible input for the identification of productivity" → "...to identify productivity" (shorter, active). Otherwise fine.

**V2, V4:** no change.

**V3: an active verb.**
> "However, materials are commonly tax deductible and, thus, the input firms have the incentive to overreport."

"Are ... and, thus, ... have" is a chain of "to be" and "to have". Options, each with a verb that names who acts:

1. **"However, VAT deductions give firms the incentive to overreport materials, the very input these methods rely on."** Recommended: "give" is active, "the incentive" keeps the link to the first sentence, and "the very input these methods rely on" connects back to V2.
2. "However, VAT deductions reward firms for overreporting materials, the very input these methods rely on." (Same pattern as "the credit rewards overreporting".) Drops the explicit link to "the incentive".
3. "However, VAT deductions make materials the input firms have the incentive to overreport." Closest to the current wording.

**V5: too much detail.**
> "Using the production-function parameters, I estimate a structural model of firm input overreporting to study how real and fictitious claimed deductions, and thus government tax revenue, respond to a counterfactual tax rate."

"Real and fictitious claimed deductions" is an intermediate step (the chapter's Claims), and the results reported in V6 are about revenue. Drop it: "Using the production-function parameters, I estimate a structural model of input overreporting."

**V6: awkward, and the elasticity phrase reads as "revenue left".**
> "I find that, holding the tax collected on sales fixed, the revenue left elasticity at the current rate is -9.7 and that net sales-tax revenue losses, as a share of potential net revenue, are 3 percent."

- "net sales-tax revenue losses, as a share of potential net revenue" is clumsy (losses and "net" twice). "-9.7" needs a minus sign (−9.7).
- Following Tim (more, shorter sentences), split V5 and V6 into three sentences:

> Using the production-function parameters, I estimate a structural model of input overreporting. Holding the tax collected on sales fixed, the elasticity of net sales-tax revenue to the left of the current rate is −9.7. Undetected overreporting costs 3 percent of potential net sales-tax revenue.

Three short sentences (48 words) replace V5 and V6 (67 words): clearer and 19 words shorter. If "net" twice is heavy, the last sentence can end "of potential revenue".

- **Status:** −9.7 and 3 percent are placeholders from the discarded structural point and revenue levels with the double-deflation bug. Rerun planned Oct 4 to 7.

## Shortening, respecting Tim's advice

Merging sentences is out. Savings now come from dropping content or rewording inside a sentence:

- V5 and V6 as above: 19 words, no loss of a result.
- V1 "to identify productivity": 3 words.
- P2 "In addition, I find that,": 5 words.
- Further savings would drop whole items (the robustness sentence in P2, the "applies equally to surveys, administrative data, or tax records" sentence in P1), each with a cost to the argument; Hans's call.

## Suggested P3 (changes in bold)

> The magnitude of detected overreporting matters for methods of estimating production functions that exploit a flexible input **to identify productivity** [@Levinsohn2003; @Doraszelski2013; @Ackerberg2015; @Gandhi2020]. These methods typically assume that materials are flexible and their demand or first-order conditions reveal productivity. **However, VAT deductions give firms the incentive to overreport materials, the very input these methods rely on.** I show that failing to correct for overreporting yields output elasticities of materials that are larger, and productivity distributions that are more dispersed and less persistent. **Using the production-function parameters, I estimate a structural model of input overreporting. Holding the tax collected on sales fixed, the elasticity of net sales-tax revenue to the left of the current rate is −9.7. Undetected overreporting costs 3 percent of potential net sales-tax revenue.**
