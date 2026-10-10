# Setting and data (`03-setting-data.qmd`): blind outline, 2026-10-10

Status: DRAFT, not yet agreed with Hans (parked; we may come back to it). Built without reading the chapter: the reader state comes from the intro (`01-intro.qmd`), and the dependencies from the later chapters that cite it.

## Reader state at the chapter's first line

**Knows (intro):**
- The credit on purchases rewards overreporting inputs, and the reward grows with the rate (l. 11).
- Losses are 1.2–2.7 percent of tax revenue, including Colombia (l. 11).
- The 1983 reform raised the sales-tax rate for most manufacturing industries and left a few exempt (l. 15).
- A "purchases-side rate" exists (l. 15).
- Corporations are monitored and face the market, so they are the truth-tellers (l. 19, 28).
- The test covers 9 of 18 liable industries; the 2 exempt food industries "gained almost nothing from the credit" (l. 21).
- Corporations are much larger than unincorporated firms (l. 28).
- The method needs only reported inputs and output (l. 26).
- Roadmap (l. 32): the chapter describes the sales tax and why corporations are truth-tellers.

**Asks:**
1. How did the credit work: one for one? What if the credit exceeds the tax owed? (l. 11, 32)
2. Why materials and not other inputs? (l. 19)
3. What is the "purchases-side rate" vs the sales-tax rate? (l. 15)
4. What did the 1983 reform change, for whom, and when? (l. 15)
5. Why do exempt industries gain almost nothing from the credit, if exemption on sales means the credit is refunded? (l. 21)
6. Why are corporations truthful: what scrutiny, and what evidence? (l. 19, 28)
7. Who are the unincorporated firms, and how different are they from corporations? (l. 28)
8. Which data: survey, years, variables, sample? (l. 26)

**Inconsistencies:**
- "Purchases-side rate" (l. 15) appears before the reader learns there are two rates.
- Exempt yet "gained almost nothing" (l. 21) needs the reason (rate on purchases near zero).
- "Closely monitors corporations" (l. 19) may overstate: an auditor requirement, not on-site audits (Fiscal Survey 1965).
- The roadmap (l. 32) omits the data.

**Job:** establish the three institutional facts the thesis rests on: the credit that rewards overreporting materials, the 1983 reform that changed that reward across industries, and the scrutiny that keeps corporations truthful. Then describe the data in which later chapters measure them.

**Dependencies out:**
- `@sec-setting-incentive`: refunds (ch. 2 l. 61).
- `@sec-setting-jo`:
  - corporate scrutiny (ch. 2 l. 157, ch. 4 l. 52);
  - income-tax rates by juridical organization (ch. 2 l. 90);
  - juridical organizations ordered by size, LLCs closely held (ch. 7 l. 51, 55).
- `@sec-setting-st`:
  - the rate going from 6 to 10 percent, with food exempt (ch. 7 l. 77);
  - enacted December 1983, effective 1 April 1984 (ch. 7 l. 157, 259);
  - the 1986 draft block (ch. 7 l. 68).
- Used without a citation:
  - the sample (codes 6–9 dropped; corporations = 3);
  - the measurement of $\tau_S$ and $\tau_P$ (ch. 2, ch. 4 net share, ch. 8 $\tau_P=0$);
  - real pesos (GDP deflator, ch. 8);
  - industry definitions (18 + 2).

## Outline

**Chapter takeaway.** Colombian manufacturing in 1981–1991 provides an incentive (a sales tax that credits purchases), variation in it (the 1983 reform) and a benchmark (corporations kept from overreporting), all observed in a plant survey that reports inputs, output and the taxes on each.

1. **The sales tax rewarded overreporting materials** `{#sec-setting-incentive}`
   1. The credit lowers the tax one for one, and the firm gets a refund when the credit exceeds the tax owed (Q1; to the model).
   2. Materials are the creditable input: capital goods were not creditable and labour was not taxed (Q2; to the model).
   3. Firms sell and buy at different rates, measured per firm-year from the survey's taxes (Q3; to ch. 2, 4, 8).
2. **The 1983 reform raised the rate for most industries** `{#sec-setting-st}`
   1. The rate went from 6 to 10 percent in liable industries, food stayed exempt; enacted December 1983, effective 1 April 1984 (Q4; to ch. 7).
   2. The exempt industries gained little: their rate on purchases is near zero (311, 312) (Q5; to ch. 4).
   3. The same reform moved income-tax rates by juridical organization in the opposite direction; the 1986 reform followed (to ch. 2 footnote, ch. 7).
3. **Corporations report truthfully** `{#sec-setting-jo}`
   1. Juridical organizations line up by size and ownership; LLCs are closely held (Q7; to ch. 7).
   2. Corporations face audited statements, the Superintendent (auditor requirement, not on-site audits), third-party reporting and the market. Evidence: Carrillo's 1.7 vs 11.5 percent, a lower bound (Q6; to the model, ch. 4).
4. **Data**
   1. The Annual Manufacturing Survey, 1981–1991, by plant: output, materials, capital, labour, taxes on sales and purchases, juridical organization, industry (Q8).
   2. Sample: codes 6–9 dropped (one line each); corporations = 3, unincorporated = 0, 1, 2, 4, 5; three-digit industries (18 liable + 2 exempt); real pesos via the GDP deflator; firm-years by group (to ch. 4–8).
   3. Corporations are larger: size and output share by group (intro l. 28; to ch. 4).

## Exclusions
- Why firms overreport, detection and the cost of evasion: model chapter.
- The raw share gap between corporations and unincorporated firms: ch. 4 (it would preview the test).
- The response to the reform: ch. 7.
- Variable construction (capital stock, deflators): data appendix.
- Literature on the invoice-credit mechanism: ch. 9 / intro.

## Open decisions (with recommendations)
1. The two-rate fact (56 percent of firm-years differ by more than 1 point) is in the model (l. 153): move it to item 1.3, and have the model cite it.
2. The Perry 1990 sales-to-public fact is in the model (l. 165): move it to item 1.2 and keep one sentence in the model.
3. Carrillo 1.7 vs 11.5 percent is in the model (l. 157): keep it only in item 3.2, and have the model cite `@sec-setting-jo`.
4. Item 2.3 holds all 1983 changes; item 3.1 holds the income-tax levels by juridical organization.
5. Data last (institutions, then measurement).
6. Soften intro l. 19 if item 3.2 cannot support "closely monitors".
