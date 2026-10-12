# Proof-read: ch. 4, text Claude drafted 2026-10-11 (and the pending 3.2 draft)

Scope: the paragraphs drafted by Claude and applied on Hans's OK (derivation, 1.2, 1.3, 2.1, 2.3, 3.1, the 313 footnote), plus the 3.2 draft from the chat. Hans's opening paragraphs are out of scope. Audience: thesis/JMP (first-year PhD, non-specialist). Appendix F: `Thesis/appendices/F-test-robustness-pr-111026.md`.

## Pass 1

### The reported materials share (l. 68–81)

- **l. 77** (Hans revised this sentence): "The last line follows from @eq-share-net replacing the log share of true materials." can be parsed with "@eq-share-net" as the subject of "replacing". An active verb makes the step explicit:
  > The last line uses @eq-share-net to replace the log share of true materials.
- **l. 68, layout:** in the PDF the inline definition of $s_{it}$ breaks across lines at "ln((1 −". Optional: display it.

### Identifying the output elasticity of materials (l. 108–126)

- **l. 120, grammar:** "Taking expectations among corporations, and using … (Assumption 2), recovers …" uses a gerund phrase as the subject, interrupted by a second gerund phrase. The reader has to hold two clauses before reaching the verb. Lead with the action and give the reason after:
  > Taking expectations among corporations recovers the output elasticity of materials, because measurement error has mean zero and is independent of juridical organization (Assumption 2):
- **l. 126, unclear antecedent:** "The same equation" follows @eq-foc-cd-exp, but $\varepsilon_{it}=\ln\beta-s_{it}$ comes from the corporations' share equation at l. 108. Name the source:
  > With $\ln\beta$ known, the corporations' share also recovers their measurement error, $\varepsilon_{it}=\ln\beta-s_{it}$, and with it its distribution, $f_\varepsilon$.

### The observed residual (l. 138–144)

- **l. 144, ambiguous pronoun:** in "measurement error, which is independent of it", "it" can mean the residual or overreporting. Write "which is independent of overreporting".

### Testing for overreporting (l. 148–152)

- **l. 148:** no issues.
- **l. 150, footnote:** "the mean residual is also 0.23": "also" ties this number to the within-product 0.23, a coincidence (0.227 and 0.226), and suggests a link that isn't there. Drop "also". "The mean also rises because the sample excludes …" names no comparison; "is also higher because" is clearer.
- **l. 152:** no issues. "Therefore" in the second sentence relies on the appendix for the step; acceptable in a summary paragraph.

### Where the test detects overreporting (l. 170–172)

- **l. 170, word accuracy:** "at the margin" can read as the economic margin. "Narrowly" says it directly: "…and, narrowly, Industrial chemicals (351) and Other chemicals (352)."
- **l. 170, subject:** "The first seven also reject" makes the industries reject. The test rejects:
  > In the first seven, the test also rejects at the 1 percent level and under its conservative version (@sec-app-test-robustness).
- **l. 172:** no issues.

### 3.2 draft (chat)

- "Their firms" is vague: "Firms in these industries".
- The last sentence joins two ideas with "and"; the second (the income-tax incentive) carries its own point: the reward was not zero. Give it its own sentence. The income-tax incentive is described in @sec-setting-jo (ch. 3, l. 62), which is the better citation; @sec-fiscal uses it.

Revised draft:

> The test does not detect overreporting in either industry exempt from the sales tax, Food products (311) and Other food products (312). Firms in these industries paid almost no tax on their purchases, and a fictitious peso of materials saved them almost no sales tax (@sec-setting-incentive). The result agrees with the model, which predicts little overreporting where the reward is small. On its own, however, a failure to reject is inconclusive. These firms still had a reason to overreport: fictitious materials also lowered their income tax (@sec-setting-jo).

## Pass 2

Checked the pass-1 corrections and the revised 3.2 draft; citations verified (@sec-setting-incentive holds the exempt-industry rates, ch. 3 l. 50; @sec-setting-jo holds the income-tax incentive, ch. 3 l. 62). The corrections introduce no ", so" and keep the first person.

Missed in pass 1:

- **l. 172, jargon:** "a 5 percent test" → "a test at the 5 percent level".
- **l. 150, footnote:** "Within the same four-digit product" uses classification jargon; "Within the same product (four-digit industry)" is clearer for a non-specialist.
- **3.2 revised draft, logic:** the model's sentence ("predicts little overreporting where the reward is small") and the income-tax sentence pull in different directions on purpose: the second explains why the non-rejection is inconclusive. The order (model, inconclusive, income tax) makes that link; keep it.

---

# Proof-read 2: Hans's edits in commit 95ee12f ("ch4 review")

Scope: the opening paragraphs (l. 40–46), the test section (l. 148–171) and the results (l. 189–207). Audience: thesis/JMP.

## Typos

- l. 40 "This what allows" → "This is what allows".
- l. 152 "implictly" → "implicitly"; "explcit" → "explicit".
- l. 168 "st this value" → "at this value".
- l. 199 "inventives" → "incentives".
- l. 201 `\not\in` → `\notin` (but see the substantive point below).

## Substantive (correctness)

- **l. 201, empty region:** "the test fails to reject correct reporting because the confidence region … is empty" is wrong for the three empty-region industries (323, 356, 384). An empty region means the test rejects every $\mu_0\in[0,1]$, including $\mu_0=0$: the test *rejects* correct reporting there, together with every other value the model allows. Those results are inconclusive because the model itself is rejected, not because of a failure to reject. Also, $\mu_{\mathcal V}\notin[0,1]$ is a claim about the population mean; the data only say that no candidate in $[0,1]$ is accepted. Suggested:
  > In the other nine liable industries, the test is inconclusive. In six, the confidence set contains zero. In three, the set is empty: the mean residual is negative, which the model rules out (@sec-testing-residual), and the test rejects every value of $\mu_{\mathcal V}$ in $[0,1]$. As with any statistical test, a failure to reject is inconclusive.
- **l. 150, detection criterion:** "The test detects overreporting where the set excludes zero" also covers an empty set, which excludes zero. Make it "where the set is non-empty and excludes zero". This matches the ex-ante rule used in later chapters.
- **l. 40, Hicks-neutral sentence:** "In general, the output elasticity of the flexible input of a Hicks-neutral production function does not depend on productivity" holds only *given inputs*. Under a general Hicks-neutral $F$, the elasticity varies with $M$, and $M$ varies with $\omega$. The two groups' different productivities would then shift the corporations' benchmark. Only under Cobb-Douglas is the elasticity a constant. That is what lets the next sentence recover it from corporations "even though [they] are heterogeneous in productivity". Suggested:
  > Under Cobb-Douglas, the output elasticity of materials is a constant, which does not depend on productivity. More generally, with Hicks-neutral productivity the elasticity of a flexible input depends on productivity only through the inputs.

  Or keep only the Cobb-Douglas statement here and leave generality to the robustness paragraph (l. 172).
- **l. 199, exempt industries:** "had no incentives to evade sales taxes" is stronger than ch. 3 (l. 50 of `03-setting-data.qmd`: "almost no reward"; purchase rates 0.2 and 0.6 percent). It also conflicts with the next sentence's "little overreporting where the reward is small". Use "almost no incentive to overreport for the sales tax". "This makes sense" adds nothing that "The result agrees with the model" does not already say. Separately, the "a failure to reject is inconclusive" caveat now sits only in the liable-industry paragraph, so the exempt result reads as confirmation of truthful reporting. Consider moving the caveat so that it covers both paragraphs.

## Clarity and flow

- **l. 40, first content sentence:** "equals the output elasticity of materials and the output error" → "equals the log output elasticity of materials minus the measurement error in output" (the equation has $\ln\beta-\varepsilon$). "Importantly" can go.
- **l. 40–42, repetition:** the last sentence of the opening paragraph ("allows me to recover the elasticity from corporations …") comes before the reader knows why corporations matter, and the next paragraph's first sentence says it again. Consider ending para 1 at "distinguish between overreporting and productivity" and folding the heterogeneity point into para 2:
  > Since corporations report materials truthfully and share a common technology with unincorporated firms, the output elasticity of materials can be recovered from corporations, even though the two groups differ in productivity.
- **l. 150, order:** "The test therefore detects…" comes before the construction is described, and "These values form a confidence set for $\mu_{\mathcal V}$" repeats the first sentence. Suggested:
  > To test the null hypothesis, I build a confidence set for $\mu_{\mathcal V}$ by test inversion: I test every candidate value $\mu_0$ on a grid over $[0,1]$ and keep the values the test does not reject. Inverting a test at the 5 percent level gives a 95 percent confidence set [@CasellaBerger2002, sec. 9.2.1]. The test detects overreporting where the set is non-empty and excludes zero.
- **l. 152:** "Under the null hypothesis, $\beta$ is …" is ambiguous, because "the null hypothesis" was defined two paragraphs earlier as $\mu_{\mathcal V}=0$, and this is the joint hypothesis for one candidate. "the true parameter of the output elasticity" is wordy, and "nonnegative" modifies the wrong noun. Suggested:
  > The test above implicitly takes the corporations' $\beta$ as the true output elasticity. Two moments, one for each group, make this explicit. Let $C_{it}$ equal one for corporations and zero otherwise. If $\beta$ is the output elasticity of materials and $\mu_0\ge0$ is the expected residual of unincorporated firms,
- **l. 166, antecedent:** in "Under the null hypothesis, it is asymptotically chi-squared", "it" now follows $\hat\Omega$. Write "the statistic is". With the citation moved, it now supports only the re-estimated $\hat\Omega$, and the reader no longer learns that this is the S statistic. That is fine if intended.
- **l. 166, $d_g$ undefined in the text:** $d_g$ is defined only in the footnote, and the main text never says that the headline uses $\chi^2_1$. That is the critical value the table rests on. Consider "with one degree of freedom ($d_g=1$)[^dg-df]" in the text.
- **l. 168, footnote:** "That is, taking the corporations' estimate as the true value, the first moment is exactly zero" is a dangling modifier (the moment does not take anything), and "Then, it follows that" is wordy. Suggested:
  > I evaluate the statistic at $\ln\hat\beta$, the corporations' mean log share; that is, I take the corporations' estimate as the true value. At this value the first moment is exactly zero, and the statistic tests only the second: whether the residual of unincorporated firms has mean $\mu_0$. Hence $d_g=1$. @sec-app-test-robustness reports a conservative version of the test with $d_g=2$, and a bootstrap test that carries the sampling error of $\ln\hat\beta$.
- **l. 191:** word order: "I test for overreporting in the 20 largest manufacturing industries by number of firm-years." As a one-sentence paragraph it could open the next paragraph.
- **l. 193, footnote placement (optional):** `[^bevs]` is about Beverages only. Placing it after "Beverages (313)" ties it to its subject.
- **l. 199:** the first two sentences say the same thing. Merge them: "The test does not detect overreporting in either industry exempt from the sales tax, Food products (311) and Other food products (312)."
- **Terminology:** the text says "confidence set" (l. 150), "confidence region" (l. 201) and "test-inversion region" (table caption). Pick one; "confidence set" matches Casella–Berger.
- Trailing spaces at the end of l. 150 and l. 158 (harmless).
