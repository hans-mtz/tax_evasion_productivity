# Proof-read: JMP abstract, paragraphs 2 and 3 (restructured), 2026-10-01

**Target audience:** job-market paper abstract; academic but accessible, economists outside the field. Version on disk: P1 200 words, P2 130, P3 77 (407 in total). Sentences are numbered T1 to T5 (P2) and U1 to U3 (P3).

## The restructure, as I read it

- **P1:** the problem, the method, the setting.
- **P2:** detection (T1 to T3), how much (T4), and whether it responds to the tax rate (T5, the 1983 reform moved here).
- **P3:** what correcting for overreporting changes: first production-function estimates (U1), then, using those parameters, the structural model and the revenue counterfactual (U2, U3).

The logic works. P2 now holds all the measurement evidence from the data (test, magnitude, reduced-form response), and P3 holds what the corrected estimates are used for, with "Using the production function parameters" in U2 chaining the production-function results into the structural model. That chain is the dependency the thesis plan states (production-function estimates feed the counterfactual).

## 1. Paragraph 2

**T4: the sentence Tim could not read still has the same ambiguity, and "reaches" changes the meaning.**
> "Across the 7 largest industries where the null is rejected, the median input overreporting, as a share of true inputs, reaches 16 percent."

- "The median input overreporting" does not say what it is the median of (firms? industries?). The number is the median across the seven industries of each industry's average overreporting ratio (16.3 percent; the industry averages run from 4 to 49 percent).
- "Reaches" implies a maximum or a threshold ("up to"). A median does not reach anything; it is a typical value. Use "is".
- Suggested: "Across the 7 largest industries where the null is rejected, the median industry's average overreporting is 16 percent of true inputs."

**T5: the time structure collides.**
> "I find that between 1983 and 1987 after a fiscal reform raised the sales tax in Colombia, firms in affected industries increased the ratio of overreported to true inputs by about 9 percentage points."

- "between 1983 and 1987 after a fiscal reform ..." strings two time phrases together without a comma, so the reader cannot tell whether the dates modify "find", "raised", or "increased".
- Suggested: "I find that, after a fiscal reform raised the sales tax in Colombia, firms in affected industries increased the ratio of overreported to true inputs by about 9 percentage points between 1983 and 1987."
- Accuracy note: the 9 points are for unincorporated firms in the introduction. The abstract avoids legal-form terms (Tim's point), so "firms in affected industries" can be read as including the benchmark firms, whose overreporting is assumed zero. Your call whether that needs a word.

**T1 to T3: no changes.** Tim's points are in: the null is stated in T2 ("correct reporting") and referred back to afterwards ("the null"), "exempt from the sales tax" is spelled out, and there is no double negative. T2's "robust to" is looser than the introduction's "valid", but it is your wording and unchanged.

## 2. Paragraph 3

**U2: "fictitious" changes the estimand.**
> "...how fictitious claimed deductions, and thus government tax revenue, respond to a counterfactual tax rate."

- In ch. 8, Claims is the credit claimed on true inputs plus undetected overreporting: total claimed deductions, not the fictitious part. The 16 to 28 percent in U3 refers to that total. "Fictitious claimed deductions" says the fictitious part responds by 16 to 28 percent, which is a different and larger claim. Drop "fictitious": "how claimed deductions, and thus government tax revenue, respond to a counterfactual tax rate."
- "Using the production function parameters": needs the hyphen ("production-function", as in P1). Optional: "the corrected production-function estimates" says why these parameters feed the model.

**U1: no error, but see section 3 on the opening.** "Failing to correct for overreporting yields..." is clear.

**U3:** no changes (wording matches ch. 8).

## 3. Things Tim raised that are not in the text yet

These may be deliberate, so I list them without recommending.

- **A sentence in money or revenue terms** after T4, so the reader sees why 16 percent matters. Absent.
- **A setup sentence for the production-function result with two or three classic citations** (Tim: "cite a couple of golden oldies", and "you're not accusing anybody"). P3 opens with "Next, I show that failing to correct for overreporting...", which puts the finding before the reason the reader cares.
- **P3 does not open with its main idea.** "Next" attaches U1 to the reform sentence at the end of P2. If a setup sentence is added, that is the natural opening.

## 4. Facts to keep in mind (not errors)

- **T3:** "all 4" among the 5 largest relies on 351 and 352, which reject only marginally (sharp 95 percent regions start at 0.001 and 0.002) and not at the conservative test. The headline is the sharp test, so the sentence holds as written.
- **U3 and the structural numbers:** the 16 to 28 percent comes from the structural estimates that the plan marks as under re-estimation; redo the abstract's numbers when the counterfactual is rerun.
- **Introduction, paragraph 5:** it still says "null hypothesis of no overreporting" while the abstract now says "correct reporting"; Tim is about to go through the introduction.

## Suggested P2 and P3 (changes in bold)

> The approach yields a simple statistical test to detect input overreporting. Under the null hypothesis of correct reporting, the test is robust to a wide family of specifications of the probability of detection and the cost of overreporting. Among the 5 largest industries, the test rejects the null hypothesis in all 4 industries subject to the sales tax, and, as expected, it fails to reject in the industry exempt from the sales tax. **Across the 7 largest industries where the null is rejected, the median industry's average overreporting is 16 percent of true inputs. I find that, after a fiscal reform raised the sales tax in Colombia, firms in affected industries increased the ratio of overreported to true inputs by about 9 percentage points between 1983 and 1987.**
>
> Next, I show that failing to correct for overreporting yields output elasticities of materials that are larger, and productivity distributions that are more dispersed and less persistent. **Using the production-function parameters, I estimate a structural model of firm input overreporting to study how claimed deductions, and thus government tax revenue, respond to a counterfactual tax rate.** I find that a 0.5 percent increase in the sales-tax rate raises claimed deductions by 16 to 28 percent.
