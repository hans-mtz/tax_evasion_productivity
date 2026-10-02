# Proof-read: JMP abstract, paragraph 1, second pass, 2026-09-30

**Target audience:** job-market paper abstract; academic but accessible, economists outside the field. Second pass on the version now in `JMP/paper.qmd` (199 words), after the first pass (`abstract-p1-pr-300926.md`) and Hans's edits. Sentences are numbered S1 to S10 in order.

## What is fixed since the first pass

- S4 now names the group once ("a benchmark group, a subset of firms that report inputs correctly") and S5 uses "the benchmark firms".
- S10 now reads "the data allow" (plural).
- S5 is a single sentence with the premise first.

## 1. Still open

**S7: "a truth-reporting group" is the last unreconciled name.**
> "My method requires only firm-level input and output data, and a truth-reporting group."

- The group is now "benchmark" in S4 and S5. Repeating the term is what the reader needs; a new label makes them wonder whether a different group is meant. Tim also found "truth-reporting" too aggressive.
- Support for "benchmark" in the literature the abstract builds on: Gorodnichenko, Martinez-Vazquez, and Sabirianova Peter (2009), in the JPE paper the introduction already cites, describe the earlier approach as using "a group of taxpayers who are known to comply (e.g., employees subject to withholding) as a benchmark to assess the true income of another group of taxpayers (e.g., self-employed)" (NBER WP 13719, p. 3; check the page in the published version).
- The comma before "and" splits a two-item object. Suggested: "My method requires only firm-level input and output data and a benchmark group."

**S10: the reason the benchmark is credible is still missing.**
> "During this period, the Colombian sales tax worked as a VAT and the data allow me to identify a subset of firms that report inputs correctly."

- In this literature the credibility of the benchmark rests on the reporting environment. Gorodnichenko et al. drop the approach for Russia because "tax evasion was widespread, with employees quite likely practicing as much tax evasion as the self-employed" (p. 3), and Pissarides and Weber state the group's correct reporting as an assumption (p. 17). A reader who has just met "a benchmark group ... that report inputs correctly" in S4 asks why, and S10 is where the abstract answers.
- "The data allow me to identify" overstates: the data show the legal form; correct reporting is an assumption supported by the setting, which is what the sentence should say.
- S10 repeats S4's phrase almost word for word. That is acceptable (generic method first, Colombian application second) provided S10 adds the reason.
- Suggested (Tim's wording): "During this period, the Colombian sales tax worked as a VAT and, due to government and market scrutiny, a subset of firms reported their inputs correctly."

## 2. Small points

- **S5, repeated noun:** "the same technology as the rest ... parameters of the technology". Replace the second with "its parameters": "Since the benchmark firms share the same technology as the rest, they allow me to estimate its parameters."
- **S5, premise as fact (optional):** "share the same technology" is the identifying assumption. A one-word hedge ("are assumed to share") would be more candid, but the introduction states it as a feature of the approach and abstracts usually do the same. Hans's call.
- **S1 to S3, S6, S8, S9:** no changes.

## Suggested paragraph (changes in bold; S5, S7 and S10 only)

> Value-added taxes (VAT) allow firms to deduct the taxes paid on purchases from the taxes collected on sales, creating an incentive for firms to overreport the purchases of inputs to reduce their tax bill. The fundamental issue in detecting input overreporting stems from two unobservables, true inputs and productivity. Because firms differ in productivity, a firm reporting high input use for a given level of output may be overreporting or less productive. I use a structural production-function approach and a benchmark group, a subset of firms that report inputs correctly. **Since the benchmark firms share the same technology as the rest, they allow me to estimate its parameters.** Using these estimates, I then predict how potentially tax-evading firms would behave without overreporting, and compare the prediction to what they report. **My method requires only firm-level input and output data and a benchmark group.** The method thus applies equally to standard firm-level surveys, administrative data, or confidential tax records. I apply the method to a Colombian manufacturing survey from 1981 to 1991. **During this period, the Colombian sales tax worked as a VAT and, due to government and market scrutiny, a subset of firms reported their inputs correctly.**
