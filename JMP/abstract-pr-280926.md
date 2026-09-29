# Proof-read: JMP abstract (JMP/paper.qmd, `abstract:` field, line 18) — 2026-09-28

**Target audience:** job-market paper abstract — same register as the introduction (academic but accessible). Compared throughout against `JMP/sections/01-intro.qmd`, since the abstract restates several of the intro's own claims and should match its wording/precision where it does.

## 1. Unfilled placeholders — top priority

> "I then estimate a structural model of detection and evasion costs, and find that a X percent increase in the sales-tax rate raises claimed deductions by between X and X percent, while cuts lower them gradually."

The literal "X" / "X and X" were never filled in. The intro (line 9) already has the numbers: a 0.5 percent increase raises claimed deductions by 16 to 28 percent, and the smallest cut distinguishable from current policy is 8 percent. Suggested fix, matching the intro:

> "…find that a 0.5 percent increase in the sales-tax rate raises claimed deductions by between 16 and 28 percent, while cuts lower them only gradually — the smallest cut distinguishable from current policy is 8 percent."

(Adding the 8 percent figure isn't strictly required to fix the placeholder, but the intro treats it as worth quantifying, and leaving "cuts lower them gradually" bare after quantifying the increase reads as asymmetric.)

## 2. Grammar errors

- **"the test does not detect evasion in none of the 2 largest exempt industries"** — double negative ("does not… in none of"). Fix: *"the test does not detect evasion in either of the two largest exempt industries."*

- **"whose incentive to overreport are null"** — subject–verb agreement error ("incentive" is singular; needs "is," not "are"). "Null" is also an odd word choice here — it reads as a statistical term (as in "null hypothesis") in a sentence that's already about test rejections, which risks a momentary misread. Recommend recasting rather than just fixing the verb: *"which have no incentive to overreport."*

  Combined fix for both: *"Furthermore, the test does not detect evasion in either of the two largest exempt industries, which have no incentive to overreport."*

## 3. The statistical-test sentence needs both a parallelism fix and a precision fix

> "The approach also yields a statistical test, valid for any detection probability and evasion cost and can be extended to any common technology, whose rejections can only point to overreporting."

Two problems:
- **Parallelism:** the sentence joins an adjective phrase ("valid for any detection probability and evasion cost") to a verb phrase ("can be extended to any common technology") with a bare "and," inside what's already a modifier on "test." That mismatch is what makes the sentence hard to parse on a first read.
- **Precision — this doesn't say what the intro says.** The intro's version of this same claim (line 13) is more careful: *"it remains valid under any detection probability and cost of evasion that vanish without evasion, and under any common production technology."* Two things differ in the abstract: (a) it drops "that vanish without evasion," which is the actual condition — "any detection probability and evasion cost" without that qualifier overstates the result; (b) "can be extended to any common technology" implies the test would need modification to handle other technologies, whereas the claim is that it's *already* valid under any common technology, no extension needed.

Recommend matching the intro's phrasing directly:

> "The approach also yields a statistical test that remains valid under any detection probability and cost of evasion that vanish without evasion, and under any common production technology, whose rejections can only point to overreporting."

## 4. Hyphenation / terminology consistency with the intro

- **"structural production function approach"** (line 18) vs. the intro's **"structural production-function approach"** (line 11). "Production-function" is a compound modifier before "approach" and should be hyphenated, as the intro already does. Fix: *"structural production-function approach."*
- **"evasion cost"** vs. the term used everywhere else in the paper, **"cost of evasion"** (intro lines 11, 13; and item 3 above once corrected). Recommend *"cost of evasion"* here too for consistency.

## 5. Numeral consistency (same issue flagged in the intro's own proof-read)

Within this one paragraph: *"9 of the 18… including four of the five largest… the 2 largest exempt industries… the seven industries…"* mixes numerals (9, 18, 2, and "20" earlier) with spelled-out numbers (four, five, seven) for the same kind of quantity — industry counts. Recommend converting to numerals throughout, matching the numeral-heavy statistical style used for percentages in the same sentence: *"4 of the 5 largest,"* *"the 7 industries."* (If you make this change here, make the same change in the intro for consistency between the two — see the intro's own proof-read report, `JMP/sections/01-intro-pr-280926.md`, item 5.)

## 6. Minor wording

- **"My method only requires input and output firm data"** — "firm" sitting after "output" is an awkward modifier placement. Suggested fix: *"My method only requires firm-level input and output data."*
- **"a half-life of productivity shocks 1.6 to 3.1 times shorter"** — "N times shorter" is imprecise multiplicative phrasing (a common style-guide flag: "3 times shorter" doesn't unambiguously mean "1/3 as long"). The intro avoids this by stating the same finding from the opposite reference point: *"a half-life of productivity shocks 1.6 to 3.1 times longer"* (when correcting for overreporting, rather than when failing to). Since the abstract's whole final sentence is just the mirror image of the intro's finding (uncorrected-relative-to-corrected vs. corrected-relative-to-uncorrected), recommend flipping the abstract's sentence to match the intro's cleaner, already-precise framing:

  > "Finally, correcting for overreporting yields consistently lower output elasticities of materials and less dispersed, more persistent productivity distributions, with a 90–10 ratio about 40 percent lower on average and a half-life of productivity shocks 1.6 to 3.1 times longer."

  (This also removes the need to separately check that "60 percent larger" and "40 percent lower" are consistent reciprocals of each other — they're close but not exact, likely just because both are cross-industry averages; adopting one direction throughout sidesteps the question.)

- **Optional readability fix**, third sentence: *"…using a structural production-function approach, with corporations, which government and market scrutiny keep from evading, as the truth-reporting benchmark."* Three comma-set-off clauses in quick succession are dense for an abstract. Consider em-dashes for the middle clause: *"…using a structural production-function approach that takes corporations — kept from evading by government and market scrutiny — as the truth-reporting benchmark."*

## Not flagged, checked for consistency

- The opening sentence deliberately echoes the intro's opening almost verbatim — appropriate repetition of the paper's core framing, not an error.
- "Among the 20 largest industries… 9 of the 18… industries liable for the sales tax… 4 of 5 largest… 7 industries… 16 percent" all match the intro and the project's current-state notes; no factual inconsistencies found once the numeral style is unified (item 5).
