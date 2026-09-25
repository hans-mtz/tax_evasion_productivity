# RAP review — JMP introduction

Source: `JMP/sections/01-intro.qmd`, visible (non-draft) prose only. Paragraph numbers below count visible paragraphs in the final render, where draft-only blocks disappear.

Title: *Tax Evasion and Productivity: Identifying and Correcting for Cost Overreporting*

## Implied RAP

| Element | Extracted text | Comment |
|---|---|---|
| **P** | Para. 4: "Despite its relevance, the literature on corporate tax evasion has mostly overlooked cost overreporting. The few studies focusing on this tax evasion strategy rely on exploiting detailed administrative data…"; para. 5: "no other study has attempted to structurally identify cost overreporting. A fundamental problem is that… cost overreporting might be naively quantified as low productivity." | Two gaps, arriving only in paragraphs 4–5: (i) measuring overreporting has required confidential tax data; (ii) overreporting and low productivity look the same in the data. The productivity-bias paragraph (para. 2) comes before either gap is stated. |
| **R** | Never stated. Implied by para. 5–6: "How can cost overreporting be identified, separately from productivity, without administrative data?" | Readers must infer it. There are two audiences with different questions: public finance (how much do firms overreport?) and IO/productivity (does overreporting bias productivity estimates?). |
| **A** | Para. 6 (method): "deviations from this common technology identify tax evasion up to measurement error… I jointly recover the distributions of tax evasion and productivity." Para. 7 (results): "I detect overreporting in 9 of the 20 industries tested… firms overreport between 11% and 25% of their true materials on average." Para. 2, last sentence: "ignoring overreporting overstates how dispersed productivity is across firms." | The pieces are good, but A is split across three places, and the headline result arrives in paragraph 7. The fiscal-reform and counterfactual results are in draft-only blocks, so they don't appear in the final render at all. |

## Coherence checks

| Check | Verdict | Reason |
|---|---|---|
| Does P make space for R? | **Partially** | The gaps are real and well chosen, but they come after the reader has already met the productivity-bias argument, and R is never stated. There's also a render problem: paragraph 2 opens "Furthermore, ignoring tax evasion…", but the paragraph it was meant to follow (revenue losses) is draft-only. In the final render, "Furthermore" follows the definition paragraph and has nothing to add to. |
| Does P idly lead readers to A? | **Yes, where it appears** | Para. 5's "cost overreporting might be naively quantified as low productivity" is a strong hook, and readers wonder "so how do you tell them apart?" Para. 6 answers exactly that. The problem is placement, not logic. |
| Does A answer R? | **Partially** | For the implied "How…?" R, the method paragraph answers correctly ("By using the first-order conditions… truth-reporting firms… deconvolution"). But the results paragraph answers a different, "How much…?" question, and the productivity finding answers a third one. Nothing ties the three into one answer. |

## Alternative RAPs

A is kept fixed (the paper's findings); only its opening words adapt to R.

| Alternative | Does P make space for R? | Does P idly lead readers to A? | Does A answer R? |
|---|---|---|---|
| **1 — Measurement, for public finance.** *P:* Cost overreporting through fake invoices is a widespread form of VAT and income-tax fraud [@OECD2017], but measuring it has required confidential administrative data that few researchers can access [@Carrillo2022; @Zumaya2021]. *R:* Can cost overreporting be measured with standard firm-level production data? *A:* Yes. Using corporations as truth-reporting benchmarks and deconvolution, I detect overreporting in 9 of 20 Colombian manufacturing industries, with firms in the five strongest cases overreporting 11–25% of their true materials. | **Yes.** The gap is access to data, and R asks whether it can be bypassed. | **Yes:** "so can it be done without that data?" | **Yes.** "Yes" answers "Can…?", and the numbers show it was done. |
| **2 — Productivity, for IO and production-function readers.** *P:* Productivity estimation assumes reported intermediate inputs reflect what firms actually use [@Gandhi2020; @Ackerberg2015; @Levinsohn2003], yet intermediates are the input firms are most likely to overreport to lower their taxes. *R:* Does tax-motivated input overreporting bias productivity estimates? *A:* Yes. Correcting for it lowers materials elasticities below GNR's in all five industries studied, and productivity turns out far less dispersed (90/10 ratio 1.7–2.8 rather than 3.1–6.7) and more persistent. | **Yes.** The juxtaposition of a standard assumption and a known incentive to violate it is itself the gap. | **Yes, strongly:** "does it matter?" | **Yes.** "Yes" answers "Does…?", backed by the numbers. |
| **3 — The confound, for both audiences (matches the title).** *P:* A firm reporting high input use for its output may be overreporting to evade taxes or may simply be unproductive, and existing work separates the two only with audit or invoice-level data. *R:* How can overreporting be separated from productivity in standard firm-level data? *A:* By using truth-reporting firms to pin down the common technology, so that deviations from it identify overreporting up to measurement error, which deconvolution then removes. Doing so detects overreporting of 11–25% of true materials in the industries with the strongest evidence, and shows that ignoring it overstates productivity dispersion. | **Yes.** P states both the confound and the data limitation, so R follows directly. | **Yes, strongly.** This is the paper's best hook (current para. 5), moved to the front. | **Yes.** "By…" answers "How…?", and the results sentence shows what the method delivers for both audiences. |

Alternative 3 fits the title ("Identifying and Correcting") and serves both audiences with one R; alternatives 1 and 2 can then become the two "why it matters" paragraphs that follow it. Alternative 1 is the natural choice if the paper is pitched primarily as public finance (JEL H26 listed first).
