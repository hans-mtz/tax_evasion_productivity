# Proof-read: `08-counterfactual.qmd` — 2026-09-23 (revision 3)

Replaces revision 2. Everything you approved is applied (list below). **Only the open items need your call.** Items are referenced by section, since line numbers keep moving.

## Applied in this round

| Item | What changed |
|---|---|
| Your edits | Kept. Form fixes only: space before `{#sec-counterfactual}`; "Claim-Deductions" → "Claimed-Deductions" (to match the title); "changes the sales tax side?" → "…sales tax rate?"; "in the tax-cuts side / tax-increasing side" → "on the tax-cut side / tax-increase side"; "as tax rate changes" → "as the tax rate changes"; "The convex in productivity cost rationalizes" → "The cost of evasion, convex in productivity, rationalizes". Your new draft-only block now uses the same `content-visible when-meta="draft"` form and label as the others. |
| A6 | No $t1$ in visible text. Results now say "sales-tax revenue on sales held at its baseline value, $\tau_{it}P_tY_{it}$". Appendix A (Estimation mechanics) notes that the data record the sales tax each firm pays on its sales. |
| A7 | Deflation note added after the $\theta_C$ moment: claims are nominal COP, deflated by the GDP deflator of each year before averaging. |
| B1 | "U-shape" is gone: ch. 8 says "convex in productivity" (ch. 2 and the conclusion stub too). Saved to memory, and CLAUDE.md is updated. |
| B5 | Assumptions line added before the moment vector: $\varepsilon\perp(M,e,\omega)$, $\psi\perp(\omega,M)$, $E[\varepsilon]=E[\psi]=0$. |
| B6 | $\pi$ → $F_Z$; "product measure" → "joint distribution of $(M_{it},Z_{it})$ implied by $\mu$ and $F_Z$". |
| B7 | One warning only. The identification paragraph no longer repeats it; the next paragraph opens with it and ties it to the counterfactual. |
| B8 | Cites `@Schennach2022` (JEL 60(4):1223–1263, doi 10.1257/jel.20211355), added to `references.bib`. I checked that p. 1250 contains the warning ("One should be careful not to interpret the entropy maximizing distribution as…"). |
| B9 | Conservative test defined after Theorem F.1 (the statistic is bounded above by $\chi^2_{d_g}$, so the test's size is at most its nominal level). The soft test is dropped from ch. 8 and appendix A (old text kept in a comment). The CV revenue robustness material is commented out of ch. 8 (bullets, @fig-cf-laffer, @tbl-cf-ci, @tbl-cf-headline, @fig-cf-theory-cv). Appendix B (control variate) is commented out of `_quarto.yml` and `JMP/paper.qmd`. |
| C | Captions for @fig-cf-claims (full description), @tbl-cf-estimates, @tbl-cf-detection, @tbl-cf-claims-headline. In-text references added for @fig-cf-claims, @tbl-cf-estimates, @tbl-cf-detection. Detection table headers → $m^*_{it-1}$, $\tilde{\mathcal W}_{it-2}$. Claims-headline table: "Statistically guaranteed change" → "Bounds on the change", column widths rebalanced, note shortened (it was clipped). |
| D1 | "I" throughout ch. 8, including the draft blocks ("one cannot interact with an instrument…" where "I" read oddly). |
| D3 | COP instead of \$ in visible text. |

The full book and the JMP both render with no warnings, and all cross-references resolve.

---

## Round 4 (applied)

- **O1, O2 → to-do list.** Three items were added to ch. 8's Robustness draft note: fine-tune the Claims confidence sets (only the coarse grid has run); run the θ-free check on Claims; estimate the back-of-envelope numbers inside ELVIS. A draft note in @sec-app-backofenvelope marks its numbers as JMP-draft ballparks. No runs were launched.
- **O4:** your "inflated claims" works. It is also accurate in magnitude: at +0.5% the legitimate credit rises only 0.5%, a few COP on a baseline of roughly 785, while claims rise by about 170. So nearly all of the increase is inflation.
- **O5:** agreed. Revenue is the motivating question, and the second paragraph ("claims-only Laffer curve") is the bridge to claims.
- **O6:** left as is.
- **D2:** full Canadian spelling: *modelling/modelled* (ch. 3, appendix D, JMP intro), *grey* (ch. 8 captions, appendix A, Claims figure label, regenerated), *analogue* (appendix A). Recorded in PLAN.md §1 and memory. Legacy `Code/Deconvolution/` scripts that also feed the slides are unchanged.

## Open — need your call

**O3.** You'll handle ch. 2's symmetric rationale when you get to ch. 2.

**B2. Carrillo paragraph: connecting the "two sources" argument.** The point is that the model has only two primitives that can produce the drop at the top, and only one of them can do it plausibly. Right now that argument sits in the last two sentences, after the manager-time story. Suggested structure, for you to write:
1. Pattern (Carrillo): evasion rises with size, then drops at the very top.
2. **Two candidate sources in the model:** the drop must come from either the detection probability or the evasion cost.
3. **Detection can't do it:** $q(e)$ depends only on evasion, so to produce a drop at the top it would have to fall as evasion grows, or be lower for the largest firms. Neither is plausible: if anything, the largest firms face more scrutiny.
4. **So the cost must:** evasion cost convex in productivity.
5. Why that is economically sensible: the manager-time and coordination story.

A possible bridge sentence between steps 1 and 3 (your call on wording): *"In the model, only two primitives can generate this pattern: the probability of detection and the cost of evasion."*

**O7 (carried over):**
- A8: "constant term" (Results) vs. "intercept, $\Delta/[2\lambda(1+\Delta)]$" (Design). Pick one term.
- Estimates, Discussion, Results and Scope are still in bullet form.
