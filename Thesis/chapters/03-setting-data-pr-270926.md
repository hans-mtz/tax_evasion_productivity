# Proof-read: ch. 3, @sec-setting-incentive, paragraphs 4 and 5 (27 Sep 2026)

Paragraph numbering counts prose paragraphs from the chapter opening (P1). Earlier rounds: `03-setting-data-pr-260926.md`.

## P4 (the credit rewards overreporting)

> The credit rewards overreporting materials. An artificial increase in the reported value of materials raises the credit by $\tau_P$, the rate paid on purchases, pesos per peso, and reduces the firm's tax liability by as much. Since capital goods could not be credited and labour was not a taxed purchase, I assume firms overreported raw materials consumed, $M^*_{it}$.

The topic sentence leads, good.

**1. Word order in sentence 2.** The appositive splits the unit: "by $\tau_P$, the rate paid on purchases, pesos per peso" reads as a list. Keep "$\tau_P$ pesos per peso" together and define $\tau_P$ after it:

> An artificial increase in the reported value of materials raises the credit, and so lowers the firm's tax liability, by $\tau_P$ pesos per peso, where $\tau_P$ is the rate paid on purchases.

**2. The logic of sentence 3 has a gap.** Ruling out capital and labour does not single out raw materials: energy, fuels, and repair and maintenance were creditable too. What singles out raw materials is size: a median of 46% of gross output, against about 2% for energy, fuels, and repair and maintenance combined (ch. 3 sample; these rows were dropped from @tbl-sum-stats, so state the numbers in the text or in a footnote).

**3. "I assume firms overreported".** It is a choice of which input to model, and ch. 4 tests for overreporting rather than assuming it. "I focus on" says what you do without presuming the answer.

Suggested sentence 3:

> Capital goods could not be credited and labour was not a taxed purchase, so I focus on raw materials consumed, $M^*_{it}$, by far the largest creditable purchase: a median of 46% of gross output, against about 2% for energy, fuels, and repair and maintenance combined.

## P5 (weak enforcement), active verbs

> Enforcement was weak. Tax evasion in Colombia was high during the sample period [@Sanchez1994], and the tax authority had an inefficient auditing system, was overburdened, and faced legal loopholes [@Perry1990].

Four forms of "to be" ("was weak", "was high", "was overburdened", plus "had" as a near-stative). Active alternatives, same claims:

- "Enforcement was weak" → "Weak enforcement left this reward within reach" (links P5 to P4: the reward exists, and nothing stops firms from taking it).
- "Tax evasion in Colombia was high" → "Tax evasion ran high in Colombia".
- "had an inefficient auditing system, was overburdened" → "audited inefficiently, carried more work than it could handle".

Suggested:

> Weak enforcement left this reward within reach. Tax evasion ran high in Colombia during the sample period [@Sanchez1994], and the tax authority audited inefficiently, carried more work than it could handle, and faced legal loopholes [@Perry1990].

---

# Proof-read: ch. 3, @sec-setting-jo, P1 (why corporations do not overreport) (27 Sep 2026)

> In Colombia, at leas three reason restrained Corporations from overreporting. First, the Superintendent of Corporations closely monitored them by requiring an on-site auditor. Second, public tradable shares inhibit corporations from misbehaving: [...] Evidence shows that stock prices of public corporations fall, on average, in response to news of their involvement in tax shelters becomes public [...]. Third, their shareholders pay individual income tax only on distributed dividends, consequently a corporation has an additional margin: [...]. Partnerships and LLCs have no such margin, since their owners are taxed on all profits. Furthemore, evidence from Ecuador shows that in contrast to LLCs whose overreported around 11 percent of their true inputs, corporations only 2 percent [@Carrillo2022]. So, if anything, by assuming corporations do not overreport, our results would suggest an upper bound of actual evasion.

## Substance (fix first)

**1. "Upper bound" has the wrong direction.** If corporations overreport by $u_c\ge0$, their mean share is $\ln\beta+E[u_c]$, so $\ln\hat D$ is too high and the unincorporated firms' estimated overreporting, $E[\mathcal V]=E[u]-E[u_c]$, is too low. Assuming corporations do not overreport makes the estimates a **lower** bound on actual evasion (for unincorporated firms). Also "our results" → "my estimates" (the chapter uses "I").

**2. The third reason conflicts with P4 of this section (line 60) and with the 1986 reform (line 92).**
- Line 60 says corporations paid 40% at the entity level against 20% for partnerships and LLCs. A higher entity-level rate means a *larger* income-tax reward to overreporting materials, which cuts against the reason as written. The dividend-deferral margin lowers the *owners'* tax, not the corporation's; state why that margin substitutes for overreporting (a legal way to lower the combined tax burden), or the reader will see the 40% rate as a counterargument.
- Ley 75 of 1986 ended double taxation (dividends exempt), so the margin exists only for 1981–86, about half the sample. Say so, or rest the argument on the first two reasons.
- The reason also repeats line 60 ("shareholders paid tax only on the dividends they received, whereas owners of partnerships and LLCs paid tax on their share of the profits"). Keep it in one place and refer to it.

**3. "Restrained" fits reasons 1 and 2 (constraints) but not 3 (an incentive).** A topic sentence covering both: "kept corporations from overreporting" or "reduced corporations' ability and incentive to overreport".

**4. The paragraph uses features the next paragraph defines.** "Tradable shares" and "the Superintendent of Corporations" appear here before line 54 defines corporations (at least five shareholders, tradable shares) and the Superintendent's scope. Either move the definitions paragraph first, or open this paragraph with the claim the section title makes ("I treat corporations as truthful reporters of materials") and keep the three reasons as support. The section outline at line 28 already plans "three reasons first", so the second option keeps your order.

**5. Reason 2 overstates the evidence.** *Sociedades anónimas* have tradable shares, but most were not listed; the stock-price evidence (US, tax shelters) applies to listed firms. Write "tradable shares" rather than "public tradable shares", and say the evidence concerns listed firms. Also "tax shelters" are avoidance, not overreporting: "reputational cost of aggressive tax behaviour" is the accurate bridge.

**6. Reason 1 has no citation.** The on-site auditor (*revisor fiscal*) requirement needs a source (@FiscalSurveyColombia1965 is already cited for the Superintendent's scope at line 54).

## Grammar, spelling, word choice

| Where | Current | Suggested | Why |
|---|---|---|---|
| S1 | at leas three reason | at least three reasons | spelling, plural |
| S1 | Corporations | corporations | common noun |
| S2 (and line 54) | Superintendent of Corporations | Superintendency of Corporations (*Superintendencia de Sociedades*) | the agency, not the official; keep the same term at line 54 |
| S2 | closely monitored them by requiring an on-site auditor | required them to keep an on-site auditor | active, specific verb; "closely monitored … by requiring" is indirect |
| S3 | inhibit corporations from misbehaving | deter corporations from misreporting | "inhibit from" is non-idiomatic; "misbehaving" is vague |
| S3 | colon + HTML comment + "Evidence" | end the sentence with a full stop | the comment removes the text after the colon, leaving "misbehaving: Evidence shows" |
| S4 | in response to news of their involvement in tax shelters becomes public | when news of their involvement in tax shelters becomes public | two constructions merged ("in response to news" / "when news becomes public") |
| S5 | dividends, consequently a corporation | dividends; consequently, a corporation | comma splice |
| S6 | Partnerships and LLCs | unincorporated firms | proprietors are also taxed on all profits; the comparison group is all unincorporated firms |
| S7 | Furthemore | drop, or "Direct evidence from Ecuador…" | spelling; "Furthermore" also hides that this is a new kind of support (evidence, not a reason) |
| S7 | LLCs whose overreported … corporations only 2 percent | LLCs overreported about 11% of their true inputs, corporations only 2% | "whose" → no relative pronoun needed; missing verb for corporations |
| S7 | percent | % | the chapter uses % elsewhere (line 48, 54) |
| S8 | So, if anything, by assuming … our results would suggest | If corporations overreport at all, my estimates are a lower bound on … | see Substance 1; "if anything" + "would suggest" double-hedges |

## Structure

One paragraph carries three ideas: the three reasons, the Ecuador evidence, and the bound. The Ecuador figure is the strongest support you have (direct evidence on the same margin); consider leading the evidence with it, or splitting reasons and evidence + bound into two paragraphs. The bound sentence answers the reader's "what if the assumption fails?" and deserves to close its own paragraph.

---

# Proof-read: ch. 3, @sec-setting-jo, P2 (definitions of the juridical organizations) (27 Sep 2026)

## Citations

Sources checked: @DANE2018 (`Lit-Papers/DANE2018-EAM1992-1994-DDI.pdf`, PDF p. 12, list of juridical organizations 01–12) and the Fiscal Survey notes in `Paper/sections/30-lit-rev.qmd:252-262` (pp. 27–30).

| Claim | Now cited | Source found | Suggestion |
|---|---|---|---|
| Corporation = counterpart of the US corporation | FiscalSurvey | FiscalSurvey pp. 27–30 | keep, add pages |
| ≥5 shareholders, tradable shares of equal value, liability limited to shares | FiscalSurvey | DANE p. 12 ("05. Sociedad anónima … acciones negociables de igual valor … accionistas (no inferior a 5) … responden únicamente por el monto de sus acciones"). Your notes (`Paper/sections/01-notes.qmd:35`) translate this DANE text, not the Survey | add @DANE2018, p. 12, unless the Survey also gives the five-shareholder minimum |
| LLC: max 25, non-tradable stakes, liability up to contributions | DANE for 25 | DANE p. 12 ("04. Sociedad limitada … no excederán de 25 … aportes no representan papeles o títulos libremente negociables … hasta por el monto de sus aportes") | add page |
| LLC: max 20; association of persons; Superintendent only if >1/3 owned by a corporation | FiscalSurvey | FiscalSurvey pp. 27–30 (the 20 isn't in my notes; you have the book) | add pages |
| **Partnerships: jointly and severally liable** | **none** | DANE p. 12: "01. Sociedad colectiva … Todos los socios responden solidaria e ilimitadamente"; "02. Sociedad en comandita simple … socios gestores … solidaria e ilimitadamente … comanditarios … limita(n) su responsabilidad a sus respectivos aportes"; "10. Sociedad de hecho … obligaciones … a cargo de todos los socios de hecho" | add @DANE2018, p. 12 |
| **Proprietorships: individuals who allocate part of their assets** | **none** | DANE p. 12: "09. Empresa unipersonal … persona natural o jurídica que destina parte de sus activos …"; "11. Persona natural o propiedad individual" | add @DANE2018, p. 12, but see Accuracy 2 |
| Partnerships 3.5% of firm-years | table | @tbl-jo-summary | fine |

## Accuracy

1. **Not all partners are jointly and severally liable.** The partnership group is codes 2, 4, 5 (general, de facto, ordinary limited; `Code/Thesis/ch03-sample.R:30`). In ordinary limited partnerships only the managing partners are; the limited partners are liable up to their contributions. Also DANE says *solidaria e ilimitadamente*: "jointly, severally, and without limit" is the part that contrasts with LLCs. Suggested: "Partnerships are associations of two or more persons in which all partners (in general partnerships) or the managing partners (in ordinary limited partnerships) are jointly, severally, and unlimitedly liable for the partnership's operations".
2. **The proprietorship definition you use is DANE's *empresa unipersonal*.** That's a legal entity, and I believe Ley 222 of 1995 created it, after the sample ends (please verify). For 1981–91, proprietorships in the data would be natural persons (DANE code 11, *persona natural o propiedad individual*). The wording "individuals who allocate part of their assets to conduct commercial activities" still describes a natural person operating a business, so it can stay, but cite DANE and don't say "legal entity".
3. **LLC "shares".** LLCs have *cuotas* (partnership stakes), not shares; DANE says contributions are not "freely negotiable securities". Suggested: "whose stakes cannot be traded".

## Grammar and style

| Where | Current | Suggested | Why |
|---|---|---|---|
| S2 | The most important during the period were: corporations, … | Four matter for this study: corporations, … / The four largest were corporations, … | a colon after a verb breaks the sentence; "most important" by which measure? If by firm-years, say so (the table shows it) |
| S4 | An LLC (…), has | An LLC (…) has | stray comma between subject and verb |
| S4 | not exceeding 20 [@FiscalSurvey] or 25 [@DANE2018] | at most 20 [@FiscalSurvey…] or, by the 1990s, 25 [@DANE2018…] | the two sources are 1965 and 1992–94; the dates explain the discrepancy instead of leaving it as a contradiction |
| S4 | it is an association of persons, not of capital | keep | this contrast with "associations of capital" (S3) is effective; the repetition helps |
| S5 | ; they are a small group (3.5% of firm-years), pooled with the other unincorporated firms | . They are a small group (3.5% of firm-years), so I pool them with the other unincorporated firms. | a separate sentence; "so I pool" states that the size is the reason and who does the pooling |

## Structure

The topic sentence leads, and each following sentence defines one organization in the table's order (except partnerships before proprietorships; the table orders Proprietorship, LLC, Partnership, Corporation — consider matching one order in both).

---

# Proof-read: ch. 3, @sec-setting-jo, P3 (income tax by juridical organization) (27 Sep 2026)

> The juridical organizations faced different income-tax rates (@tbl-jo-rules). Since the 1974 reform, corporations were taxed at 40% of their income, and partnerships and LLCs at 20% [@McLure1989; @PerryCardenas1986, 1:23]. Proprietors were subject to the graduated individual schedule, with a top rate of 56% [@PerryCardenas1986, 1:36]. Owners were taxed again at the individual level, but differently: shareholders paid tax only on the dividends they received, whereas owners of partnerships and LLCs paid tax on their share of the profits, whether distributed or not [@McLure1989]. The income of proprietorships was taxed only once, at the individual level. Because materials are a deductible cost, overreporting them also lowers the income tax, so the reward to overreporting varied within industries too, with the juridical organization.

## Facts and citations

**1. Page numbers look off by one (please check against the book).** In the scanned vol. 1 (`Lit-Papers/PerryCardenas1986-DiezAnosReformasTributarias-v1.pdf`), printed page numbers sit at the *bottom* of each page, after the footnotes. The 1974 rates ("las 'anónimas y asimiladas' … y las 'limitadas y asimiladas' … tarifas únicas … de 40% y 20% respectivamente") come right after the "23." page number, so they're on **p. 24**. The fall of the top individual rate "del 56% al 49%" comes after "36.", so it's on **p. 37**. If that holds, the fix is `1:24` and `1:37` here, at line 85 (1:36), in ch. 7 (lines 94, 96), and in the source note of `ch03-jo-rules-table.R`.

**2. The rates are right.** Corporations and similar entities ("anónimas y asimiladas", which include stock partnerships) paid 40%; "limitadas y asimiladas", covering "toda forma de sociedad de personas" (every partnership form), paid 20%. The 56% top rate before 1983 is confirmed too (p. 37).

## Structure

**3. The main idea is in the last sentence.** The paragraph matters for the thesis because the income tax *also* rewards overreporting, by an amount that varies with the juridical organization. That comes last, after four sentences of rates. Leading with it tells the reader why the rates follow ("the income tax also rewarded overreporting, by a different amount for each juridical organization"), and the rates then answer "how much?".

**4. A question the paragraph raises but doesn't answer.** Corporations faced the highest entity-level rate (40%), so their income-tax reward to overreporting was the largest. A reader coming from P1, which assumes corporations report truthfully, will notice. One clause linking back ("…the largest reward, which makes the scrutiny in P1 the operative constraint") or a footnote would close the gap. Your call whether to address it here.

**5. Repetition.** "The income of proprietorships was taxed only once, at the individual level" repeats S3 ("Proprietors were subject to the graduated individual schedule"). Merge them: "Proprietors paid only the graduated individual schedule, with a top rate of 56%."

## Grammar and word choice

| Where | Current | Suggested | Why |
|---|---|---|---|
| S2 | Since the 1974 reform | From the 1974 reform until 1983 | "since" suggests the rates still apply; Ley 9 de 1983 cut the LLC rate to 18% (@sec-setting-st) |
| S2 | taxed at 40% of their income | taxed at a rate of 40% | "40% of their income" can be read as the base, not the rate |
| S4 | Owners were taxed again at the individual level, but differently | Owners of companies were taxed again at the individual level, but differently | proprietors aren't taxed "again"; the subject has to exclude them |
| S6 | lowers | lowered | the paragraph is in the past tense |
| S6 | so the reward to overreporting varied within industries too, with the juridical organization | so the reward to overreporting also varied within industries, by juridical organization | "too, with the …" is awkward; "also … by" reads cleanly |
| S6 | within industries too | (keep "also", but say what it is "also" to) | "too/also" implies variation *across* industries, but the chapter only shows that in @sec-setting-st, after this paragraph; say it ("besides varying across industries with the sales tax") or drop "too" |

---

# Proof-read: ch. 3, @sec-setting-jo, P3 again (Hans's revision) (27 Sep 2026)

> The income tax varied incentives to overreporting within industries by juridical organization (@tbl-jo-rules). Firms could reduce their taxable profits and, with them, the income tax by deducting materials cost. Between 1974 and 1983, partnerships and LLCs paid 20%, and their owners paid individual income tax again on their share of the profits, whether distributed or not [...]. Proprietors paid only the individual income tax, on a graduated schedule with a top rate of 56% [...]. Corporations faced a 40% tax rate between 1974 and 1986, and their shareholders paid individual income tax again exclusively on paid dividends [...]. However, even though corporations might also have had incentives to evade taxes, the government and market scrutiny described above restrained them from overreporting.

Structure works: main idea first (within-industry variation), mechanism second, rates by organization, corporations last with the correction right after. Issues are at sentence level.

| Where | Current | Suggested | Why |
|---|---|---|---|
| S1 | The income tax varied incentives to overreporting within industries by juridical organization | Through the income tax, the incentive to overreport also varied within industries, by juridical organization | "incentives to overreporting" → "incentive to overreport" (infinitive after *incentive to*); "varied" is intransitive, so the tax can't "vary" incentives; "also" ties back to the sales-tax incentive of the previous section |
| S2 | by deducting materials cost | by deducting fictitious materials | as written, the sentence describes a legal deduction; the reader needs the link to *overreported* materials (and "materials cost" would be "materials costs") |
| S3 | partnerships and LLCs paid 20% | partnerships and LLCs paid a rate of 20% | "paid 20%" leaves "of what?" open; say it's a rate |
| S5 | Corporations faced a 40% tax rate | Corporations paid a rate of 40% | same construction as S3 ("paid a rate of"): repetition over variation, so the reader compares like with like |
| S5 | exclusively on paid dividends | only on the dividends they received | "exclusively" is heavier than needed; "paid dividends" can be read as dividends the firm paid out or dividends that were taxed |
| S6 | However, even though corporations might also have had incentives to evade taxes, … restrained them from overreporting | Corporations thus had an incentive to overreport too, but the government and market scrutiny described above restrained them. | "However" and "even though" do the same job, so drop one; "might … have had" undersells a mechanical fact (a deductible cost always lowers the tax); "evade taxes" → "overreport" keeps one term for one thing |

## P3, third pass (27 Sep 2026)

All earlier points addressed. Three small ones left:

| Where | Current | Suggested | Why |
|---|---|---|---|
| S1 | Due to the income tax, | Because of the income tax, | in formal writing "due to" modifies a noun ("the variation was due to …"), not a clause; "because of" is the safe adverbial |
| S2 | by deducting fictitious materials cost | by deducting the cost of fictitious materials | a three-noun stack; the reader briefly parses "fictitious … cost" |
| S5 | only on received dividends | only on the dividends they received | reads more naturally; optional |

---

# Proof-read: ch. 3, @sec-setting-st, P1 (the 1983 reform) (27 Sep 2026)

> The 1983 reform raised the sales-tax rate for most of manufacturing but left food products exempt. It combined the items previously taxed at 6 and 15 percent into a single 10 percent bracket [@Perry1990, p. 182], while food products (industries 311 and 312), exempt since the tax was introduced [@Perry1990, p. 181], stayed exempt. For most firms this was an increase: between 1983 and 1985, the median rate on sales rose from 6% to 10%, and the median rate on purchases, the rate at which each fictitious peso of materials is credited, from 6.6% to 9.9% (@tbl-st-by-year).

Topic sentence leads and S2–S3 answer "how?" and "how much?"; facts check out (Perry and Orozco de Triana 1990, pp. 181–182; the medians match @tbl-st-by-year: $\tau_S$ 6.0% → 10.0%, $\tau_P$ 6.6% → 9.9%).

## Content

**1. The 15% items saw a cut, and a careful reader will stop there.** S2 says goods at 15% moved to 10%, which is a *cut*, right after S1 says the reform "raised" the rate. S3's "For most firms this was an increase" resolves it only implicitly. Say why most firms saw an increase: most firms were at 6% before (the 1981–1983 median $\tau_S$ is 6.0%). E.g. "Most firms were in the 6% bracket, so for them this was an increase: …".

**2. "Most firms" means most *taxed* firms.** The medians in @tbl-st-by-year are over firms with $0<\tau<50\%$, so exempt firms are excluded. "For most taxed firms" (or "most firms in liable industries") matches what the table measures.

## Grammar and style

| Where | Current | Suggested | Why |
|---|---|---|---|
| S2 | 6 and 15 percent into a single 10 percent bracket | 6% and 15% into a single 10% bracket | the chapter uses "%" everywhere else, including S3 |
| S2 | food products …, exempt since the tax was introduced […], stayed exempt | food products …, exempt since the tax was introduced […], remained so | "exempt … exempt" within one clause; "remained so" (or "remained exempt" without the earlier "exempt") avoids the echo. Optional |
| S3 | the rate at which each fictitious peso of materials is credited | the rate at which each fictitious peso of materials was credited | the paragraph is in the past tense |

---

# Proof-read: ch. 3, @sec-setting-st, P0 (takeaway; Hans's version) (28 Sep 2026)

> The 1983 reform altered the reward to overreporting across and within industries. Across industries, the sales-tax rate increased in most liable industries and left exempt industries (food products) untouched. Within industries, the income-tax rates decreased by different amounts for each juridical organization: most for proprietorships, less for LLCs, and not at all for corporations. In liable industries, the two changes pulled in opposite directions: the higher sales tax raised the reward to overreporting, and the lower income tax reduced it. I exploit this varion in @sec-fiscal to test whether overreporting responded to its reward.

Structure works: the takeaway leads, S2 and S3 answer "across how?" and "within how?", S4 gives the net effect, S5 says why the reader cares.

| Where | Current | Suggested | Why |
|---|---|---|---|
| S2 | the sales-tax rate increased in most liable industries and left exempt industries (food products) untouched | the reform raised the sales-tax rate in most liable industries and left exempt industries (food products) untouched | as written, "the rate" is the subject of both verbs, so the *rate* "left industries untouched"; the reform is the actor. "raised" also matches "raised" in S4 |
| S3 | the income-tax rates decreased by different amounts | it cut income-tax rates by different amounts | same actor as S2 (parallel sentences read faster); "cut" matches the next paragraphs ("Ley 9 de 1983 also cut income-tax rates"); drop "the" before a general plural |
| S4 | In liable industries, the two changes pulled in opposite directions | For unincorporated firms in liable industries, the two changes pulled in opposite directions | corporations' income tax didn't change, so for them only the sales tax moved; optional, but it keeps the claim exact |
| S5 | varion | variation | typo |

---

# Proof-read: ch. 3, @sec-setting-st, P1–P3 after the new takeaway paragraph (28 Sep 2026)

With P0 now stating the takeaway, P1 and P3 open by repeating it almost word for word. The rest reads well; the facts were checked in the earlier pass.

**1. P1, S1 repeats P0.** "On the sales tax, the reform raised the rate for most of manufacturing but left food products exempt" restates P0's S2. Start with the detail instead: "On the sales tax, the reform combined the items previously taxed at 6% and 15% into a single 10% bracket [...], while food products (industries 311 and 312), exempt since the tax was introduced [...], remained exempt." "On the sales tax" still tells the reader which of P0's two changes this is.

**2. P3, S1 repeats P0.** "Ley 9 de 1983 also cut income-tax rates, by different amounts for each juridical organization" restates P0's S3. Parallel to P1: "On the income tax, Ley 9 de 1983 cut the individual schedule, faced by proprietorships, by 4.6 percentage points on average, with the top rate falling from 56% to 49%, and the LLC rate from 20% to 18% [...]. The corporate rate stayed at 40% [...]."

**3. Tense, P1 vs P2 (optional).** P1 describes the medians in the past ("rose from 6% to 10%"), P2 in the present ("is 6% in 1981–1983"). Both are defensible (history vs. what the data show), but the same numbers in two tenses across adjacent paragraphs reads as a slip. The chapter's data section uses the present for what the data show; if you keep that rule, P1's "rose" can stay as the policy's effect and P2's present is the data. No change needed if that's the intent.

## "%" vs. "percent" (whole thesis)

- **Current usage is consistent:** "%" throughout the running text (about 200 occurrences across the chapters, appendices, and the JMP intro), "percentage points" spelled out, no "percent" and no "pp." abbreviation for points. (The only "percent" in ch. 3 were the ones in P1, now "%".)
- **Econ journal convention:** the AEA style guide (AER, AEJs, JEP) spells out "percent" in running text ("10 percent", "4.6 percentage points") and keeps "%" for tables, figures, and math. Most economics journals and working-paper series follow the same practice.
- **Decision for Hans:** switching means changing ~200 occurrences in every chapter (not in math, tables, or figures). Doable with a scripted pass plus a manual check, but it touches text you are editing in parallel.
