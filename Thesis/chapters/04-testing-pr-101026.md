# Proof-read: ch. 4 opening, Hans's edits of 2026-10-10 (uncommitted diff vs bc6e4d3)

Scope: the five new paragraphs at lines 40–48 (the chapter opening). Audience: thesis/JMP (first-year PhD, non-specialist).

## Must fix

1. **Line 48, the deconvolution number matches no output.** "An average mean of 16.4" has no unit and appears nowhere in the products. From `Code/Products/ch05-overreporting-medians.csv`:
   - the five largest evaders (313, 321, 322, 342, 369): median of industry means 16.2 percent, mean 20.9 percent;
   - the seven industries that reject at 1 percent (the ex-ante rule ch. 5 uses): median 14.5 percent. This is the number in both intros, the abstract and the conclusion.

   Also: "distribution … display" (subject–verb agreement); "top 5" (top by what?); "average mean" (redundant); and "where the test rejects" means the 9 at 5 percent, whereas the deconvolution covers the 7 at 1 percent. Suggestion, matching the intro (l. 21):

   > In the 7 industries where it rejects at the 1 percent level, unincorporated firms overreport on average 14.5 percent of their true materials in the median industry.

2. **Line 40, grammar:** "Since …, then the difference …" — drop "then". There is also a trailing space at the end of the line.
3. **Line 44:** "Because distribution of the output measurement error term" is missing "the", and "term" is redundant.
4. **Line 46:** "hicks-neutral" → "Hicks-neutral".

## Opening: thesis vs JMP

The paragraphs are visible in both renders. "In this section, I show how to test for overreporting and recover its distribution" fits the JMP, where ch. 4 and 5 form one section. In the thesis this is a chapter, and the distribution comes in the next chapter (@sec-deconvolution). Which do you want?
- wrap this version in `content-visible when-meta="jmp"` and write a thesis version; or
- keep one version, with "chapter" and a pointer to @sec-deconvolution in the thesis.

The two opening placeholders and the draft-note bullets below (l. 50–62) are still in the file. Delete them once the opening is settled?

## Paragraph by paragraph

**Line 40 (the idea).**
- "Their materials share" is ambiguous: "their" can mean corporations, or both groups. The difference that measures overreporting is between *unincorporated firms'* share and the elasticity, and the elasticity comes from *corporations'* share. That second step, corporations' share identifying $\beta$, is the link the sentence skips.
- "The difference between a share and an elasticity": the test uses the log difference, $s_{it}-\ln\beta$. "Log" is enough here; the details come in @sec-testing-share.

> In this section, I show how to test for overreporting and recover its distribution. Corporations report materials truthfully and share a common technology with unincorporated firms. The corporations' materials share therefore identifies the output elasticity of materials, and the log difference between unincorporated firms' share and this elasticity measures overreporting up to the output measurement error.

**Line 42 (the test).**
- "The difference of the materials share of gross output forms an observed residual" reads as incomplete: the difference between what? "This difference" carries it over from line 40. Also, "forms" → "is".
- Terms: "correct reporting" here, "truthful reporting" at line 48. Use one ("truthful", which line 40 also uses).
- "is equal to zero" → "is zero"; "greater than zero" → "positive".

> This difference is an observed residual that can be tested. Under the null hypothesis of truthful reporting, its expected value is zero; under the alternative, it is positive.

**Line 44 (the distribution).**
- **Accuracy:** the residual is not a convolution. It *equals* overreporting minus measurement error, and its *distribution* is the convolution of theirs.
- **Order:** the paragraph uses the two conditions (one sentence on corporations, one on independence) before stating them ("Deconvolution requires …"). Descending order: state what deconvolution needs, then show that each condition holds.
- **Gap:** the paragraph says deconvolution needs the two terms to be independent, but never says they are. It states only independence of juridical organization, which is what transfers $f_\varepsilon$ from corporations. Independence of overreporting is the other half of Assumption 2.

> Unincorporated firms' residual equals overreporting minus measurement error, and its distribution is the convolution of theirs. Deconvolution recovers the distribution of one term of such a difference when the two terms are independent and the distribution of the other is known. Both conditions hold. Measurement error is independent of overreporting. Corporations do not overreport: their residual reflects measurement error alone. Because measurement error is independent of juridical organization, unincorporated firms face the same distribution. The distribution of overreporting is therefore identified.

**Line 46 (productivity).**
- **Placement:** this paragraph answers an objection to line 40 (common technology across groups that differ in productivity), but it sits between the deconvolution and the results. Move it right after line 40?
- **Voice:** "preclude us" — the rest of the opening uses "I".
- ~~Accuracy~~ **Withdrawn (Hans):** the point was wrong. Under Hicks neutrality the elasticity is a function of inputs only, common to both groups, and productivity never enters it. This is what makes the test valid for any functional form of the production function. Deconvolution is a separate matter.

> Corporations and unincorporated firms differ in productivity, but this does not prevent me from recovering the output elasticity of materials: under Cobb-Douglas with Hicks-neutral productivity, the elasticity is the same constant, $\beta$, whatever the firm's productivity.

**Line 48 (results).**
- "among the largest 20 industries in Colombia during the sample period" → "Among the 20 largest manufacturing industries", as in the intro (l. 21).
- "in neither exempt industry" → "in neither of the 2 exempt industries": the reader has not yet been told there are two.
- The second sentence: see Must fix 1.

> Among the 20 largest manufacturing industries, the test rejects truthful reporting in 9 of the 18 industries liable for the sales tax and in neither of the 2 exempt industries. In the 7 industries where it rejects at the 1 percent level, unincorporated firms overreport on average 14.5 percent of their true materials in the median industry.

## Optional: structure

The opening leads with a roadmap ("In this section, I show how…") and gives the finding last. The approved outline's takeaway leads with the finding (9 of 18, 0 of 2), so a reader who stops after one paragraph has the result. Your order (method, then result) also works for an opening that introduces two steps. Flagged only because it departs from the outline.

---

# Round 2: Hans's revision of the opening (same day)

Lines 40–48 after the revision. Fixed: the 14.5 percent sentence, "then", "the distribution", "Hicks-neutral", the voice ("me"), the paragraph order, "20 largest manufacturing industries", and independence of overreporting.

- **Line 40:**
  - Typo: "ouput".
  - The sentence joins two claims with "and", and the second has a long subject. Splitting it shows the two steps:

  > Since corporations report materials truthfully and share a common technology with unincorporated firms, the output elasticity of materials can be recovered from corporations. The difference between the materials share of gross output of unincorporated firms and the output elasticity of materials then recovers overreporting up to the output measurement error.

- **Line 44:**
  - "The difference": with the productivity paragraph now in between, its antecedent is two paragraphs back. Write "This difference between the share and the elasticity" (or "The difference for unincorporated firms").
  - "can be tested to be statistically different from a given quantity": "tested to be" is ungrammatical, and "a given quantity" is vague when the next sentence says zero.

  > The difference between the share and the elasticity is an observed residual whose expected value can be tested. Under the null hypothesis of correct reporting, it is zero. Under the alternative, it is positive.

- **Line 46:**
  - "the unincorporated residual" → "the residual of unincorporated firms" (the residual is not unincorporated).
  - "the distribution of the output measurement error term is independent of juridical organization and overreporting": it is the error that is independent, not its distribution, and "term" is redundant. Write "the output measurement error is independent of juridical organization and of overreporting".

---

# Round 3 (same day)

Fixed since round 2: "ouput", the split of line 40, independence of overreporting (now in line 44's text).

- **Line 42:** "an observed residual and whose expected value can be tested to be equal to a given value": "and whose" is ungrammatical, and one tests *whether* a value holds, not "to be equal". With the productivity paragraph moved below, "The difference" now follows its antecedent directly.

  > The difference is an observed residual whose expected value can be tested. Under the null hypothesis of correct reporting, the expected value of this residual is zero. Under the alternative, it is positive.

- **Line 44:**
  - "the unincorporated-firms' residual" → "unincorporated firms' residual" (no hyphen with the possessive), or "the residual of unincorporated firms".
  - Logic: independence of *overreporting* is not what lets corporations reveal $f_\varepsilon$; that needs only independence of juridical organization. Independence of overreporting is what the deconvolution needs, which the next sentence says ("independence of the addends"). Moving "and overreporting" makes each condition do its own job:

  > Because the output measurement error is independent of juridical organization and corporations do not overreport, I can recover the distribution of the measurement error from the residual of corporations. Deconvolution requires the addends to be independent and the distribution of one of them to be known. Measurement error is independent of overreporting, and its distribution is known. It follows that the distribution of overreporting is identified.

---

# Round 4 (2026-10-11)

Fixed since round 3: line 42 (test sentence), line 44 (each independence condition now does its own job). Footnote on 313 added at line 168.

- **Line 48, now stale after the five-industry decision:** the deconvolution covers the five largest industries that reject at 1 percent and meet the variance condition (321, 322, 324, 342, 369), median 12.4 percent. "In the 7 industries … 14.5 percent" no longer matches ch. 5's table. Suggestion:

  > In the 5 largest industries where it rejects at the 1 percent level and the deconvolution applies, unincorporated firms overreport on average 12.4 percent of their true materials in the median industry.

  "The deconvolution applies" leaves the variance condition to ch. 5. Alternatively, line 44 can say it in one sentence, e.g. "The deconvolution also implies that the residual of unincorporated firms varies at least as much as that of corporations; I deconvolve only where this holds."
- **Line 44:** "the unincorporated-firms' residual" → "unincorporated firms' residual" (no hyphen with the possessive).
- **Still open (round 1):** "In this section" fits the JMP's merged section; the thesis chapter covers only the test, with the distribution in @sec-deconvolution.
