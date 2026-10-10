# Testing (`04-testing.qmd`): approved outline, 2026-10-10

Built blind (the chapter was not read). The reader state comes from the intro, the setting and the model; the dependencies from ch. 6–8, appendices E, F and H, and the conclusion. Approved by Hans on 2026-10-10, without the motivating raw-share figure: motivation comes from theory, not observation.

## Reader state

**Knows:**
- The net log share of true materials is $\ln\beta-\varepsilon$ (`@eq-share-net`); productivity drops out (model).
- "If we knew $\ln\beta$, a deviation could separate overreporting from productivity"; @sec-testing identifies $\beta$ and tests (model).
- $M^*=M+e$ with $e\ge0$; no firm underreports (model).
- Corporations are corners by assumption; if they overreport a little, the estimates are a lower bound (setting, model).
- $\varepsilon$ is measurement error in output (model).
- The sample has 40,508 firm-years, 5,915 plants, 28 industries and four juridical organizations; corporations are larger (setting).
- Food (311, 312) is exempt, with a median $\tau_P$ of 0.2 and 0.6 percent vs 9.2 percent (setting).
- The test rejects in 9 of 18 liable industries, decisively in 7, and in neither exempt industry; it is robust to $q$, $\kappa$ and the technology, and one-sided (intro l. 21).

**Asks:**
1. The share equation is in true materials: what is the reported share, and how does $e$ enter?
2. How do corporations give $\ln\beta$, and what must hold for them?
3. What is assumed about $\varepsilon$: the same in both groups, independent of overreporting?
4. Corporations are much larger: doesn't the comparison need similar firms?
5. What exactly is tested, and how is the uncertainty in $\ln\hat\beta$ handled?
6. Why is the test robust and one-sided?
7. Which industries reject, and how much of manufacturing do they cover?
8. 28 industries in the sample vs 18 + 2 in the intro: what about the other 8?

**Inconsistencies:**
- 28 industries in the sample vs 18 + 2 tested.
- Industry counts of 9 (intro, ch. 8), 7 (intro, deconvolution, PF) and 5 (appendix E: "five reported in @sec-pf").
- The model uses the level $e$; later chapters use $u$, which is undefined before ch. 4.
- The lower-bound caveat on corporations already appears twice (setting, model).

**Job:** turn the materials condition into an observable measure of overreporting, $\mathcal V=u-\varepsilon$, with corporations as the benchmark; show that a one-sided test on its mean detects overreporting from production data alone, and where.

**Dependencies out:**
- `@eq-ob-ev` (ch. 6);
- `@tbl-evasion-test` (appendix F);
- $\ln\hat\beta$ (ch. 6, 7);
- $f_\varepsilon$ (ch. 5);
- the assumptions (ch. 5, "Assumption 2");
- $u$ (ch. 5, 6, 8);
- the 9 rejecting industries (ch. 8, appendix E);
- the sharp test (alternatives in appendix F).

## Outline

**Chapter takeaway.** Comparing unincorporated firms' net materials share with the corporate benchmark detects overreporting from production data alone. The test rejects truthful reporting in 9 of 18 liable industries and in neither exempt industry.

1. **From the share to an observable measure of overreporting**
   1. The reported net log share is $\ln\beta+u-\varepsilon$, with $u=\ln(M^*/M)\ge0$ (Q1; $u$ to ch. 5, 6, 8).
   2. Corporations ($u=0$) identify $\ln\beta$ (mean share) and $f_\varepsilon$ (residual). Assumptions: common $\beta$ within an industry; $\varepsilon$ with the same distribution in both groups and independent of $u$; corporations at $e=0$ (cite the setting, don't repeat) (Q2, Q3; $\beta$ to ch. 6, 7; $f_\varepsilon$ to ch. 5).
   3. $\mathcal V=s^*-\ln\beta=u-\varepsilon$ `{#eq-ob-ev}`; $E[\mathcal V]=E[u]$, zero only if no one overreports; productivity and size drop out (Q4; to ch. 5, 6, 8, appendix H).
2. **The test**
   1. Null $E[\mathcal V]=0$ among unincorporated firms, by industry; one-sided, so a rejection can only mean overreporting (Q5, Q6).
   2. Test inversion taking $\ln\hat\beta$ as given (sharp), plant-clustered; conservative and bootstrap versions in appendix F (Q5).
   3. Valid for any $q$, $\kappa$ vanishing at $e=0$ and any common technology; proof in appendix F (Q6).
3. **Results** (`@tbl-evasion-test`)
   1. 9 of 18 liable industries reject, 7 at 1 percent, including 4 of the 5 largest, about two fifths of output (Q7).
   2. Neither exempt industry rejects, as the near-zero $\tau_P$ predicts.
   3. Scope: non-rejections are inconclusive; which industries were tested and why 8 were not; which set goes forward: 9 to ch. 8, and 7 (99 percent region excludes zero) to ch. 5–6 (Q8).

## Exclusions
- Test-inversion mechanics, the conservative $\chi^2_2$ and the bootstrap → appendix F.
- The robustness proof → appendix F.
- The distribution of overreporting → ch. 5.
- The GNR comparison → ch. 6.
- The response to the rate → ch. 7.
- A raw-share motivating figure → out (motivation comes from theory).

## Decisions (agreed)
1. $u$ and $\mathcal V$ are defined in §1; ch. 5 and 6 cite `@eq-ob-ev`.
2. §3.3 states once which industry set each later chapter uses; unifying the sample stays post-submission.
3. Cite the corporate lower-bound caveat; don't repeat it.

## Audit (2026-10-10)

### Step 1: compare

- **Takeaway:** missing (draft-only opening, l. 40–48).
- **1.1:** covered (l. 70–79); $u$ is defined in passing inside the $eta$ section.
- **1.2:** partial.
  - $f_arepsilon$ from corporations is never stated.
  - No assumption on the same distribution of $arepsilon$ across groups.
  - Common technology within the industry is only implied.
  - l. 58 mislabels Assumption 2 as "timing".
- **1.3:** partial; the equation is present, but $E[\mathcal V]=E[u]\ge0$ and "groups needn't be alike" are missing.
- **2.1:** partial; why one-sided, and what a rejection means, are missing.
- **2.2:** covered.
- **2.3:** missing everywhere; draft only (l. 142–153), and appendix F lacks the argument although the intro promises it.
- **3.1, 3.2, 3.3:** missing; draft only (l. 132–140).

### Decisions (Hans, 2026-10-10)
1. Drop the generality framing ($\Gamma$, Spain's LTU, $\Theta^{NE}$); state Assumption 1 for corporations.
2. The robustness argument (2.3) must be shown, since the intro promises it: one sentence in ch. 4, the argument in appendix F.
3. Untested industries: the 20 tested are the 20 largest industries (relevance). Be consistent with ch. 8's corner firms: "the test fails to reject … or their industry is small and was not tested". 20 tested − 9 rejecting = 11, plus 8 untested = ch. 8's 19.
4. JMP: ch. 4 and 5 form one section, so add a `content-visible when-meta="jmp"` opening covering both.

### Step 2: reuse
Not started.
