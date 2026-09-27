# Proof-read: ch. 3, @sec-setting-data, paragraph 1 (26 Sep 2026)

File: `Thesis/chapters/03-setting-data.qmd`, line 34.

> The Colombian data is a well-known firm-level panel [@Olley1996;@RobertsTybout1997; @Eslava2004;@Das2007;@Gandhi2020]. It comes from the Annual Survey of Manufacturing (EAM) and covers manufacturing plants with more than 10 employees from 1981 to 1991. Besides output, intermediates, capital, and labour, the data include each firm's juridical organization and the sales taxes it paid on sales and purchases.

## 1. Citation: Olley and Pakes (1996) do not use these data (must fix)

Olley and Pakes (1996) estimate a production function for the **US telecommunications equipment industry** (Census plant-level data), not Colombia; see the abstract in `Thesis/biblio/references.bib`, entry `Olley1996`. Drop `@Olley1996` here.

Possibly what you had in mind: Levinsohn and Petrin (2003) use **Chile**, not Colombia. A well-known productivity paper on the Colombian EAM is Fernandes (2007, *Journal of International Economics*, trade liberalization and plant productivity in Colombian manufacturing); not in the bib, and to verify before adding.

Side issue in the bib: the `Olley1996` author field reads `G Steven Olley and Ariel Pakes and G Steven`, which renders a phantom third author if the entry is ever cited (only ch. 3 cites it now). Fix to `Olley, G. Steven and Pakes, Ariel`.

## 2. Main idea: say what the data have been used for

"A well-known firm-level panel" followed by five citations leaves the reader asking *known for what?* The citations are there to lend credibility (a panel trusted in seminal work), so say so; it costs one clause:

> ... a well-known panel, used in seminal work on export dynamics, reallocation, and productivity, and to estimate production functions [@RobertsTybout1997; @Das2007; @Eslava2004; @Gandhi2020].

## 3. Order: name the data first (descending structure)

Sentence 1 praises the data before sentence 2 says what they are. Leading with the source gives the reader the object, then the credibility:

> I use the Colombian Annual Survey of Manufacturing (EAM), a panel of manufacturing plants with more than 10 employees from 1981 to 1991. The panel is well known: it has been used in seminal work on export dynamics, reallocation, and productivity, and to estimate production functions [...].

## 4. Agreement: "data is" vs. "data include"

Sentence 1 treats *data* as singular ("is"), sentence 3 as plural ("include"). Pick one; academic usage favours the plural. The rewrite in 3 avoids the issue.

## 5. Firm vs. plant

The paragraph says "firm-level panel", then "covers manufacturing plants", then "each firm's juridical organization". The EAM's unit is the plant (establishment), and the tables count plants, while the rest of the paper says "firm". Suggest one sentence stating the unit once, e.g. "The unit of observation is the plant; I refer to plants as firms throughout." (Check first whether that is accurate for this panel, i.e. whether multi-plant firms are rare or not identifiable.)

## 6. Precision: "the sales taxes it paid on sales and purchases"

A firm does not pay the sales tax on its purchases to the government; it pays it to its suppliers and credits it against the tax it charges on its sales, which is exactly the mechanism in @sec-setting-incentive. Suggest:

> ... each firm's juridical organization, the sales tax it charged on its sales, and the sales tax it paid on its purchases.

## 7. Cosmetic

Citation lists are missing spaces after some semicolons (`[@Olley1996;@RobertsTybout1997; ...]`). Renders the same, but keep it consistent: `[@a; @b]`.

## Suggested paragraph (all of the above)

> I use the Colombian Annual Survey of Manufacturing (EAM), a panel of manufacturing plants with more than 10 employees from 1981 to 1991. The panel is well known: it has been used in seminal work on export dynamics, reallocation, and productivity, and to estimate production functions [@RobertsTybout1997; @Das2007; @Eslava2004; @Gandhi2020]. Besides output, intermediates, capital, and labour, the data include each plant's juridical organization, the sales tax it charged on its sales, and the sales tax it paid on its purchases.

---

# Round 2: Hans's edits to @sec-setting-data and @sec-setting-incentive (26 Sep 2026)

Hans's edits are in 3.1 (lines 36–41) and 3.2 (lines 47–53); the rest of the chapter is unchanged. Facts checked against Perry and Orozco de Triana (1990), the Colombia chapter of Gillis, Shoup and Sicat (1990), `Lit-Papers/GillisShoupSicat1990-VATDevelopingCountries.pdf`, ch. 16, pp. 180–182 (= @Perry1990).

## 3.1 Data

**1. The definition of $M^*$ is now commented out, and @tbl-sum-stats is no longer cited (line 39).** Two consequences: the reader never learns that $M^*_{it}$ is raw materials, which ch. 4 onward rely on; and the summary-statistics table floats with no reference in the text. Suggest keeping the definition, even without the median:

> The working sample has 40,511 firm-years from 5,916 plants in 29 industries, covering the four juridical organizations described in @sec-setting-jo. The input I treat as overreported, $M^*_{it}$, is raw materials consumed (@tbl-sum-stats).

**2. "28 industries" (line 41).** The ch. 3 sample has **29** industries (`Code/Thesis/ch03-sample.R`: 29 distinct `sic_3`). If 28 is deliberate (e.g. an industry dropped downstream), the sample rule and the tables need to say so; otherwise revert to 29.

**3. Timing paragraph (line 36).** Fine as it stands; the rate check now lives only in @sec-setting-st, which is enough. (Trailing space at the end of the line; harmless.)

## 3.2 The incentive to overreport

**4. P1, "During the period covered in our sample, Colombia was characterized by high levels of tax evasion" (line 47).** Passive ("was characterized by"), and "our" where the rest of the paper uses "I". Suggest:

> Tax evasion in Colombia was high during the sample period [@Sanchez1994].

**5. P2, "Originally ... Originally" and the timing of the credit (line 49).** Your new fact is right, and Perry and Orozco date it: from **1966** manufacturers could credit taxes paid on inputs "incorporated" into the product (Decree 1595); in 1968 the credit was extended to inputs "totally consumed" in production; in 1974 to any purchase except capital goods (Decree 1988) (@Perry1990, p. 180). "Originally" twice in a row reads awkwardly, and "any purchased made" is a typo. Suggest:

> Sales taxes originally targeted the manufacturing sector, on finished goods and imports. From 1966, manufacturers could credit the taxes paid on inputs "incorporated" into the product, and from 1974 the taxes paid on any purchase made by the firm, except the acquisition of capital goods [@Perry1990]. The credits worked through a system of refunds. Consequently, the tax became a kind of value-added tax (VAT).

**6. P3, rates (line 52).** Several issues: "Most industries report in the dataset paying" is ungrammatical and the data show firms, not industries; "payed" → "paid"; "a rate of 15, and luxury goods, 35 percent" drops "percent" after 15; "Records indicate" does not say which records. The records are Perry and Orozco: the 1974 reform set a **15% basic** rate, a **6% preferential** rate for "wage goods" (clothing, footwear, major inputs for popular housing) and capital goods, and **35%** on luxury consumer goods (p. 181). They also note (p. 182) that **most inputs were classified at 15% and certain finished goods at 6%**, which is useful here because it helps reconcile a 15% basic rate with a 6% median in the data. Suggest:

> The 1974 reform set a basic rate of 15 percent, a preferential rate of 6 percent for "wage goods" such as clothing, footwear, and major inputs for popular housing, and a rate of 35 percent on luxury goods; most inputs were classified at 15 percent and certain finished goods at 6 percent [@Perry1990]. In the data, the median firm paid 6 percent on its sales before 1983 (@tbl-st-by-year).

**7. Exclusions, and the "as were inputs" CHECK (line 52).** Resolved from the source: Perry and Orozco say foodstuffs were exempt "from the beginning" (p. 181), and the 1974 reform extended the list of **zero-rated** goods "particularly with respect to transportation equipment, agricultural machinery, equipment, and inputs" (p. 181). So "inputs" is theirs, and it reads as agricultural inputs. Exports, drugs, and textbooks are not in the passages I found; they need their own source or should go. Suggest:

> Foodstuffs were exempt from the beginning, and the 1974 reform zero-rated transportation equipment and agricultural machinery, equipment, and inputs [@Perry1990].

**8. Structure: the section's main idea is buried in P4 (line 58).** The section is titled "The incentive to overreport", but the sentence that states the incentive ("The credit on purchases is what gives firms a reason to overreport materials...") comes fourth, after context, history and rates. Descending order would open with it and let the history and rates answer "how so?":

1. P4 (the credit rewards fictitious materials, at $\tau_P$, plus the income-tax deduction);
2. P2 (how the credit came about, 1966–1974);
3. P3 + exclusions (the rates, and where they came from);
4. P5 (incentives vary across industries and by JO);
5. P1 (weak enforcement) last, or as the closing sentence of P5: it explains why the incentive could be acted on.

## Knock-on in @sec-setting-st

Perry and Orozco also say the 1983 reform combined "items previously taxed at 6 percent and 15 percent into the 10 percent bracket" (p. 182). So "Most ST-liable industries thus moved from 6% to 10%" (line 97) is right for most firms, but goods at 15% saw a **cut** to 10%. Worth a clause in 3.4 so a reader who just read "basic rate 15%" in 3.2 is not confused.

---

# Round 3: chapter opening paragraph (P1), after Hans's edits (26 Sep 2026)

File: `Thesis/chapters/03-setting-data.qmd`, line 34.

> Colombian manufacturing during the 1981–1991 period offers a unique setting to study tax evasion by artificially increasing claimed deductions. First, a value-added tax type which rewards overreporting materials: the sales tax let manufacturers reduce their tax laibility on their sales by claiming addtional artificial deductions (@sec-setting-incentive). Second, a truth-reporting group of firms: corporations, because the government and the market held them under intense scruitiny (@sec-setting-jo). Third, tax rate variation: the 1983 reform raised the sales-tax rate for most industries but left food products exempt (@sec-setting-st). Finally, records of inputs, output, legal form, and taxes paid at the firm level(@sec-setting-data).

The structure is right: the main idea leads, and each of the four items answers "why this setting?" and points to the section that shows it. "Truth-reporting", "claimed deductions" and "government and market scrutiny" now match the abstract and the intro, which is good. Issues, in order:

**1. Typos.** "laibility" → liability; "addtional" → additional; "scruitiny" → scrutiny; "firm level(@sec-setting-data)" needs a space before the parenthesis.

**2. "a unique setting" (sentence 1).** "Unique" is a claim a referee can dispute (other countries had VATs, corporations and reforms). What the paragraph actually shows is that the setting has everything the approach needs. Suggest "a setting well suited to study ...". Also "during the 1981–1991 period" → "in 1981–1991" (shorter, same meaning).

**3. "by artificially increasing claimed deductions" (sentence 1).** Clear, but the paper's term for the behaviour is *overreporting* (inputs); "claimed deductions" is the outcome. Suggest "tax evasion by overreporting deductible inputs", which keeps "deductions" and names the behaviour the paper measures.

**4. "a value-added tax type which rewards" (sentence 2).** "Value-added tax type" is awkward, and "which" → "that" (restrictive). Suggest "a sales tax that worked as a value-added tax and rewarded overreporting materials".

**5. "reduce their tax liability on their sales by claiming additional artificial deductions" (sentence 2).** Two issues. "Their ... their" repeats; and the mechanism in @sec-setting-incentive is a *credit* for the tax paid on purchases, while this sentence calls it a deduction. The abstract bridges the two ("taxes that credit purchases ... every fictitious dollar of deductible inputs"). Suggest keeping the credit visible here, so the reader meets the same mechanism in 3.2: "manufacturers credited the tax paid on their purchases against the tax on their sales, so every fictitious peso of materials lowered the tax bill".

**6. "a truth-reporting group of firms: corporations, because ..." (sentence 3).** Fine; "because" after a colon reads slightly off. Suggest "corporations, which government and market scrutiny kept from evading" (the abstract's own wording).

**7. "tax rate variation" (sentence 4).** Fine. Optionally "variation in the tax rate", parallel with the other items.

**8. Last item lost its point (sentence 5).** "Finally, records of inputs, output, legal form, and taxes paid at the firm level" lists the data but drops *why it matters*: that none of this needs confidential tax records, which is a selling point in the abstract ("does not require confidential tax records and applies equally to standard firm-level surveys"). Suggest restoring it. Also "firm level" vs "plant": fine given the footnote in 3.1.

## Suggested paragraph

> Colombian manufacturing in 1981–1991 offers a setting well suited to study tax evasion by overreporting deductible inputs. First, a sales tax that worked as a value-added tax and rewarded overreporting materials: manufacturers credited the tax paid on their purchases against the tax on their sales, so every fictitious peso of materials lowered the tax bill (@sec-setting-incentive). Second, a truth-reporting group of firms: corporations, which government and market scrutiny kept from evading (@sec-setting-jo). Third, variation in the tax rate: the 1983 reform raised the sales-tax rate for most industries but left food products exempt (@sec-setting-st). Finally, a survey that records inputs, output, legal form, and taxes paid for each firm, so the approach does not require confidential tax records (@sec-setting-data).

---

# Round 4: opening paragraph (P1), line 34 (26 Sep 2026)

The paragraph now reads well: main claim first, four parallel items, each tied to its section, and a closing sentence that states the portability of the method. Three small points, all optional:

**1. "the approach ... the method" (last sentence).** Two nouns for the same thing in one sentence; the reader may wonder whether they differ. Repeat one, and drop the passive "can be applied": "Because the approach uses the production function, it applies to any data with firms' output and inputs, such as surveys, administrative data, or tax records."

**2. "additional artificial deductions" (sentence 2).** "Additional" and "artificial" do the same work. The abstract says "fictitious"; one adjective is enough: "by claiming fictitious deductions".

**3. "overreporting deductible inputs" (sentence 1) vs "overreporting materials" (sentence 2).** Consistent (materials are the deductible input the paper studies), and the move from general to specific is natural. No change needed; flagged only so it stays deliberate.

No grammar or spelling errors left.
