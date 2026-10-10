# Proof-read: ch. 8, Hans's edits of 2026-10-10 (uncommitted diff vs HEAD)

Scope: only the sentences Hans changed (lines 24, 26, 29, 31, 191, 193, 230, 246). Audience: thesis/JMP (first-year PhD, non-specialist).

## Opening section

**Line 24** ("I study these questions ... true and fictitious.") Correct. "These questions" agrees with the two questions that close line 21. "The difference between A and B" works, but each term carries an appositive and the sentence ends on a third ("true and fictitious"), so the reader has to work out which commas close which phrase. Optional: put the definitions in parentheses.

> Net sales-tax revenue is the difference between gross sales-tax revenue (the tax collected on firms' sales) and sales-tax refunds (the credit the government pays on the deductions firms claim for their purchases, true and fictitious).

**Line 26** ("Since overreporting affects government revenue through claimed deductions, I decompose the deductions response ...") The motivation is better than before. "The deductions response" is a noun stack.

> ... I decompose the response of claimed deductions into three parts: ...

**Line 29** ("I find that claimed deductions ...") Fine. Checked: 1 + 0.18 + 0.21 = 1.39 and 1 + 0.18 + 0.14 = 1.32.

**Line 31** (revenue paragraph). Three issues:

1. **"Moreover" buries the topic.** The paragraph moves from deductions to revenue, but "Moreover" presents the revenue result as an add-on to the deductions one. Lead with the subject instead.
2. **The scope now comes after the first claim.** The 1.7 percent figure appears before the reader learns that it refers to the nine industries. Under the old order, the scope came first.
3. **Word accuracy and punctuation.** Industries are not "suspicious of overreporting": firms in them are *suspected* of it, or better, the test *detects* it there (the original wording, and the term @sec-testing uses). "At the current rate among the firms in ..." needs a comma after "rate". Also drop "the" before "net sales-tax revenue", which the chapter uses without an article everywhere else.

Suggestion that keeps your order (marginal effect first, then level, then break-even):

> Net sales-tax revenue falls as the rate rises. Among firms in the nine industries where the test detects overreporting (@sec-testing), a 1 percent increase in the rate lowers net sales-tax revenue by about 1.7 percent of gross sales-tax revenue. At the current rate, net sales-tax revenue from these firms is negative. A cut of about 17 percent would make ...

## Government Revenue and Claimed Deductions

**Line 191** (enforcement and penalties).

- Good topic sentence: it now says up front what is held fixed. "Takes ... as constant" → "holds ... fixed". This repeats the verb of the next paragraph, which ties the two together.
- "tax-evasion penalties" / "penalizing firms caught evading" / "Evasion penalties": choose one term. "Penalties for evasion" works in both places.
- Double space before "During the sample period" (left over from the deleted sentence).
- "struggled with inefficiency, as it lacked budget and control over its staff, and it audited little": "struggled with inefficiency" is vague. It is also unclear whether the "as" clause covers "it audited little". The active verbs say it directly:

  > During the sample period, the Colombian tax administration lacked budget and control over its staff, and it audited little.

- "Evasion penalties were also ineffective. Some penalties were trivial ...": the second sentence spells out the first, so a colon fits (it was a colon before):

  > Penalties were also ineffective: some were trivial and others too large to be applied [@PerryCardenas1986, 2:143--146].

- Optional: the paragraph never says why the Colombian evidence justifies holding enforcement fixed. One clause would close the loop, e.g. "..., which is why I hold both at their sample-period levels."

**Line 193** (partial equilibrium).

- **Unclear antecedent:** "it holds fixed" refers back to "the following analysis" two sentences earlier, but the nearest noun is "penalties".
- **Hyphenation:** "partial equilibrium analysis" should be "partial-equilibrium analysis", as in "partial-equilibrium revenue curve" at line 24.
- **Voice:** the rest of the chapter's opening uses "I" (I study, I construct, I decompose, I find). "The following analysis ... it holds ... The analysis lets" switches to an impersonal subject. Either works, but mixing them is noticeable.
- **List:** "..., and the rate on sales, as well as the output measurement error and the overreporting cost shocks" closes the list and then reopens it. "Overreporting cost shocks" is also a new term: the chapter and ch. 2 call $\psi$ the shock to the *cost of evasion*.
- **"changes of true materials"** → "changes in true materials". The sentence also repeats the previous one; the old single sentence was tighter.

Suggestion:

> The analysis is partial equilibrium. I hold fixed the firm's capital, labour, productivity, measurement error in output and shock to the cost of evasion, the prices of output and materials, and the rate on sales. True materials and overreporting respond to the new rate, and output and gross sales-tax revenue respond through true materials.

## Setup

**Line 230**: "the probability of detection and the cost of evasion functions": "cost of evasion functions" can read as "cost of (evasion functions)". Name each object with its symbol:

> ... the objective is to recover the parameters of the probability of detection, $q(e)$, and the cost of evasion, $\kappa_{it}(e,\omega)$.

**Line 246** ("a concave probability of detection and a cost of evasion"): fine, and now consistent with the chapter's terms. "Concave" is right for $0<\lambda_1<1$.

---

# Round 2: Hans's edits after the option-A rearrangement (same day)

Scope: changes since the rearrangement, at lines 175, 567, 575–579, 583, 589–593, 645–663, 697, 732–736.

## Must fix (break the render or the logic)

1. **Broken cross-reference, line 647:** `@tbl-cf-revenueA` should be `@tbl-cf-revenue`. As written it renders as "?@tbl-cf-revenueA".
2. **Typos:**
   - line 577 "$r_{it}(\Delta)^{\beta}$;. Neither" → "$r_{it}(\Delta)^{\beta}$. Neither"
   - line 697 "signitificantly"
   - line 732 "govenrment"
   - line 734 "salex-tax"
3. **"Two parts", line 589:** the equation has three terms (1, materials, overreporting). The opening section (line 26) and the Results ("Of this elasticity, 1 is mechanical") both say three. Also, "For mean arc elasticity of claimed deductions, I decompose it" lacks an article and repeats the object ("it"). Suggestion:

   > I decompose the arc elasticity of mean claimed deductions into the mechanical effect and two behavioural responses, the materials response and the overreporting response. With mean claimed deductions per firm-year $C(\Delta)=\dots$,

4. **Display followed by "where" (lines 590–593):** the blank line before "where" starts a new, indented paragraph in the PDF. Delete the blank line, and end the display with a comma instead of the period. Then: "…the third, the overreporting response."
5. **The reader no longer learns why revenue is negative.**
   - Line 579 moved the level to the design but dropped "Therefore, the negative sign does not come from overreporting."
   - Line 663 ("The negative level is what the survey records…") is now commented out as "already made in a previous subsection".
   - No rendered text makes either point now. The only other mention, line 96, is in the outline comment.

   Keep one of the two. For example, restore the dropped sentence at the end of line 579, and add the 57 percent fact from line 663:

   > … would be negative, with a 95 percent set of $[-590,-42]$ real pesos per firm-year. The negative sign therefore does not come from overreporting: 57 percent of the interior firm-years claim more deductions than they owe in sales tax.

## Design: net sales-tax revenue (lines 175, 575–579)

- **Line 175**, "The second term is claimed deductions, sales-tax refunds paid at the new rate, $\tilde\tau_{P,it}$. The credit applies to…":
  - The two nouns read as a list rather than as one quantity under two names.
  - "The credit" has lost its antecedent.

  A colon makes the equivalence explicit:

  > The second term is claimed deductions: the sales-tax refunds the government pays at the new rate, $\tilde\tau_{P,it}$, on the firm's true materials, $M_{it}(\Delta)$, plus the counterfactual undetected overreporting, $(1-q(\tilde{e}_i(\Delta)))\tilde{e}_i(\Delta)$.

- **Line 575**, "Claimed deductions split into refunds on true materials and the refunds on undetected overreporting":
  - The two halves are not parallel ("refunds" vs "the refunds").
  - A firm-side quantity (deductions) splitting into a government-side one (refunds) mixes the two vocabularies.

  > Claimed deductions split into deductions on true materials and deductions on undetected overreporting.

- **Line 577 (whole economy):**
  - **Footnote:** it now hangs on "the corner firms", but the trimmed top 0.5 percent are interior firms, not corner firms. Write "…and add the trimmed firms and the corner firms.^[…]", or attach the footnote to a separate clause.
  - **Enumeration:** "groups: a) Firms … and b) firms" → "groups: (a) firms … and (b) firms"; "Firms in b)" → "Firms in (b)". There should be no capital letter after the colon.
  - **Forward reference:** "The reported loss share for the whole economy is therefore a lower bound" still points forward to a loss not defined until @sec-cf-gap. This is left for the whole-economy discussion.
- **Line 579 (new location):**
  - **A result in the design:** it reports estimates in the design section. That is defensible, since the elasticities below use $|R(0)|=391$, but say so, or keep only the point estimate here and the sets in the Results.
  - **Undefined counterfactual:** "reported truthfully" now comes before potential revenue is defined in @sec-cf-gap.
  - **Notation:** "($M^*_i=M_i$)" runs opposite to "$M=M^*$" on line 577. Use one order.

## Design: elasticities (line 583)

"…on the same draws of $M_i$. In this way, noise common to both rates cancels in the difference." This is correct. The original "so that" expressed purpose, which is fine under the house rule, so you can keep either version.

## Results (lines 645–697)

- **Headings:** the Results subsection is still titled "Net sales-tax revenue and claimed deductions", while the design subsection is now "Net sales-tax revenue". The Results show no claims levels either (they are draft-only), so the shorter title fits both.
- **Line 647:** besides the label typo, opening with "@fig-cf-revenue and @tbl-cf-revenue show that" makes the figure the subject. The previous version led with the finding and cited the figure in parentheses, which keeps the main idea first.
- **Line 697:** "The changes in the overreporting response to the rate are … asymmetric": the response already *is* the change. "Significantly" is a statistical claim; here it is justified because the 95 percent sets at ±1 percent, $[0.19,0.23]$ and $[0.12,0.16]$, do not overlap. Say so instead:

  > The overreporting response is asymmetric: larger for increases than for cuts, and the 95 percent sets at $\pm1$ percent do not overlap.

## Revenue lost to undetected overreporting (lines 732–736)

- **Line 732:**
  - **Subject:** "The counterfactual is potential net sales-tax revenue": the counterfactual is a scenario (truthful reporting); potential revenue is the quantity measured in it.
  - **Terminology:** "the net revenue" should be "the net sales-tax revenue".

  > The counterfactual is truthful reporting. Potential net sales-tax revenue is the net sales-tax revenue the government would collect without overreporting, $P_i=\tau_{S,it}P_tY_{it}-\tau_{P,it}M_{it}$.

  **Dropped justification:** you removed "True materials are the same in $P_i$ and $R_i(0;M_i)$, because the materials condition contains no overreporting (@eq-foc-m)". It answers the obvious question about $L_i=P_i-R_i(0;M_i)$: would truthful firms buy different materials? Consider restoring it.
- **Line 734:**
  - **Comma:** "The base of the ratio, gross sales-tax revenue is observed" needs a comma after "revenue".
  - **"Cast as a moment with its own auxiliary parameter":** this is correct for a ratio of means through the linear target $E[L_i-\theta G_i]=0$.
- **Line 736:**
  - **"At least 1.5 percent":** this now depends on the lower-bound argument at line 577. A pointer (@sec-cf-design-levels) would help.
  - **"The bases may differ though":** the bases do differ (detected vs undetected overreporting, all taxes vs gross sales-tax revenue). "May" understates it, and the trailing "though" is informal. Restore the original, or shorten it:

  > The bases differ: those figures measure detected overreporting against all taxes, and mine measure undetected overreporting against gross sales-tax revenue.

  - **Terminology:** "all firms in the sample" here vs "the whole economy" at line 577: pick one, as part of the whole-economy discussion.
