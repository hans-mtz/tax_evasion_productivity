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
