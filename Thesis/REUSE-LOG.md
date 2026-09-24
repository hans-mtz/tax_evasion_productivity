# Reuse log

Where each thesis chapter's content came from: which earlier write-ups (`Paper/sections/`, `Quarto-Slides/sections/`) and code were reused, what was deliberately left out and why, and the facts checked against the code along the way. One entry per chapter, newest decisions inside each entry. Consult this before writing new chapter text or re-running a chapter's regressions. The research log (`Research-log/log.md`) stays for estimation history; this file is only about building the thesis.

## Ch. 7: Fiscal Policy Evaluation, the 1983 reform (`chapters/07-fiscal-policy.qmd`)

Built 2026-09-23 while filling `Thesis/chapters/07-fiscal-policy.qmd` from existing material. Consult this before writing any fiscal-reform text or re-running its regressions, rather than re-searching `Paper/` and the slides.

**Where each ch. 7 section's text came from:**

| Ch. 7 section | Source (verbatim prose, adapted to model notation) |
|---|---|
| Opening | `Paper/sections/965-DiD.qmd` l. 4 + `Quarto-Slides/sections/300-fiscal-reform.qmd` opening line |
| The reform and the incentives | `965-DiD` l. 12–38: ST unification (6/15% → 10%, retail extension, simplified system), individual income tax +8% (max 56 → 49%), LLC CIT 20 → 18%, presumptive income extended to LLCs, "theoretical implications". 1986 reform: `965-DiD` l. 24 (parked in a draft note pending the 1986 decision). More institutional detail (1983/1986/1990, dividend credit, presumptive 2% of gross receipts, banking-system collection): `98-fiscal-ref-col.qmd` |
| Empirical approach | Slides `300-fiscal-reform` (cleanest current derivation: corps/unincorp → stacking → expectations → levels and 1983-difference regressions → liable/exempt split). Equal-CIT assumption and JO-composition caveat: `965-DiD` l. 131–133. Older, longer derivation with the ST/CIT-separated specification: `965-DiD` l. 40–201 |
| Changes in true materials cannot explain the pattern | `Quarto-Slides/sections/900-appendix.qmd`, slide "Empirical Exercises using $\mathcal V$" (`#sec-emp-exrcs`): the $M$-alone argument (if $e$ fixed, the rise needs $M$ to fall; productivity unrelated to taxes; taxes absent from the materials FOC; one-time reform vs. gradual rise). The same slide has the analogous argument for the size-heterogeneity exercise |
| Results: all firms | Slides (results slide + "Why do the standard errors look so different?") |
| Overreporting before the reform | `965-DiD` l. 343–369 (commented out there): corps as a clean comparison group; Wiesner–Bird 1981 report [@Perry1990]; firms signalling compliance during the debate. Parallel-trends paragraphs deliberately not reused (see below). Detrended-outcome robustness: `965-DiD` l. 372–380 + `Code/Deconvolution/923-DiD-trends.R` |
| Results: liable vs. exempt; LLC vs. proprietorship | `965-DiD` l. 233–257 + slides "LLCs vs Proprietorships" (the "grandpa's workshop" line) |
| Takeaways | Slides "Takeaways" (asymmetry claim) |

**Not reused, and why:** `966-DiD-txrt.qmd` (industries grouped by the change in the *effective* sales tax rate, split by sales vs. purchases rates) is two-tax material with unexplained LLC results; it doesn't fit the single-tax story. `961-over-time.qmd` (who evades over time: size/exporter interactions) is superseded by the size-heterogeneity exercise. `98-fiscal-ref-col`'s falsification check (sales growth timing vs. share timing, `Results/Figures/Colombia/log_sales_byy.png`) fits the $M$-alone section but needs its figure regenerated; parked in a draft note.

**Code facts (checked against the scripts, not the write-ups):**
- Regressions: `Code/Deconvolution/921.1-DD.R` (levels `rg_lvl_*`, 1983-differences `rg_lvl_b83_*`, ST-separated `rg_sep_*`), figures from `921.2-het-slides.R` (`Paper/images/921-2-*.png`, copied to `Thesis/figures/ch07-*`). Older versions: `920-DD.R`, `921-DD2.R` (also the tax-rate descriptives), `922-DD2-plot.R`.
- Outcome is `log_mats_share` (the log share), with `| sic_3` industry fixed effects, SEs two-way clustered by plant and year. Corporations are one pooled baseline across all years (no corp year effects).
- No over-time regression on `cal_V` exists anywhere (checked all scripts and git history). The switch to `cal_V` happened only in the size-heterogeneity scripts (`930`–`935`). But `cal_V = log_mats_share - log_D` with `log_D` a per-industry constant (`021-deconv-funs.R`), so with industry fixed effects the year coefficients are numerically identical, up to sample differences between `wip_df` (`910-all-inds.R` filters) and the stage-1 sample.
- `Perry1990` is already in `Thesis/biblio/references.bib`.

**Decisions taken while building it (Hans):**
- Outcome: first written as $\mathcal V$, then **reverted to the log materials share $s_{ijt}$** (Hans, 2026-09-23). The switch to $\mathcal V$ happened only in the size-heterogeneity analysis. With the log share, $\ln\beta_j$ enters naturally as industry fixed effects estimated jointly with the year effects, so no stage-1 estimate enters the regression and no bootstrap or generated-regressor correction is needed. The text now matches the code (`921.1-DD.R`).
- With $\mathcal V$ this is not a DiD causal design: $\varepsilon\perp(e,M)$ plus Cobb-Douglas make the differences with respect to corporations *the* evasion changes, so no parallel-trends argument is needed. The pre-1983 decline is evasion itself (firms anticipating the reform and behaving well while lobbying), then rising after it took effect.
- Notation mapping used in ch. 7: $s$ kept (log materials share), $\gamma_j\to\ln\beta_j$ (industry fixed effects; $\gamma$ is taken by the AR(1) productivity parameters), $e\to u=\ln(1+e/M)$, $\mu$ kept (population mean of $u$; u-bar was tried and dropped because a bar reads as a sample mean), $\Delta_t\to\Delta\mu_t$, $\eta\to\zeta=\nu-\varepsilon$ ($\nu$ = idiosyncratic overreporting).

### Survey timing and reform dates (settled 2026-09-23; now in ch. 3, @sec-setting-st, @tbl-st-by-year)

- **EAM timing:** reference period = the economic year immediately preceding collection. Source: DANE (1995) *Metodología EAM*, p. 40 — `Lit-Papers/Metodologia_1992_1994 (1).pdf`, bib `DANE1995`. The DDI metadata for 1992–94 gives reference 1992–94 and collection 1993–95, and the 1992 file values assets at December 1992 — `Lit-Papers/DANE2018-EAM1992-1994-DDI.pdf` (copied from `Dropbox/COLOMBIA/DANE/Downloads from DANE Colombia/EAM 1992 to 2016/ddi-documentation-spanish-563.pdf`), bib `DANE2018`. So the panel's `year` is the activity year. Hans's earlier guess ("year = previous year's information") does NOT hold.
- **Data check:** median `sales_tax_rate_sales` (0 < rate < 0.5) is 6% (81–83), 9% (84), 10% (85–90), 12% (91). Script `Code/Thesis/ch03-sales-tax-by-year-table.R` → `Thesis/tables/ch03-st-by-year.png`.
- **1983 reform:** Decreto 3541 de 1983 (29 Dec 1983; Diario Oficial 36.452, 18 Jan 1984; issued under art. 53 of Ley 9 de 1983): general rate 10% (art. 6); VAT regime effective 1 April 1984 (art. 92). Sources: DIAN normograma (bib `Decreto3541_1983`; the online compilations are truncated before art. 92); Corte Constitucional, Sentencia C-633/16 quotes art. 92's date (bib `CorteConstitucional2016`); Perry and Orozco de Triana (1990), "the reforms of April 1984" (bib `Perry1990`, now `@incollection`, co-author name fixed; full book at `Lit-Papers/GillisShoupSicat1990-VATDevelopingCountries.pdf`, ch. 16, pp. 180–194).
- **1990 reform:** Ley 49 de 1990 (28 Dec 1990; Diario Oficial 36.615), art. 26: 12% from 1 January 1991. Bib `Ley49_1990` (DIAN normograma).
- **Debate period:** Bird–Wiesner Mission (1978–1981) on intergovernmental finances, report 1981; recommended better tax administration to reduce evasion. Sources: Junguito and Rincón (2004), *Coyuntura Económica* (bib `JunguitoRincon2004`, `Lit-Papers/JunguitoRincon2004-PoliticaFiscalSigloXX.pdf`); Bolaños Bolaños (2019), *Boletín Mexicano de Derecho Comparado* 52(155) (bib `Bolanos2019`, `Lit-Papers/Bolanos2019-ImpuestoRentaColombia.pdf`). `@Perry1990` does not mention the mission; `965-DiD`'s citation of it for the report was wrong.
- **Not found / not used:** `Dropbox/COLOMBIA/columbia.pdf` (also in `Lit-Papers/`) is a scan with no text layer, so it couldn't be searched. The DNP history of 20th-century tax reforms and Sánchez (CEDE 2005) were checked and don't mention the mission.

### 1983 income-tax facts: sources found, and a misreading corrected (2026-09-23)

- **Source:** Perry and Cárdenas (1986), *Diez años de reformas tributarias en Colombia* (Fedesarrollo), vol. 1, p. 36 (PDF p. 57), section "La Ley 9 de 1983, a) Tarifas". Bib `PerryCardenas1986`. PDFs: `Lit-Papers/PerryCardenas1986-DiezAnosReformasTributarias-v{1,2,3}.pdf` (from iCloud `Papers /Colombia/IDL-37082 v.*`; the scanned `Repor_Mayo_1986_Perry_y_Cardenas_{I,II}.pdf` are the same report without a text layer).
  - Ley 9 de 1983 **reduced** income-tax rates, expressly to compensate for inflation creep since 1974. The new scale's real incidence is "superior apenas en aproximadamente un 8% a la de 1974, en la mayoría de las escalas".
  - Top individual rate 56% → 49%; top net-wealth-tax rate 2% → 1.8%.
  - LLC rate 20% → 18%, "con el objeto de compensar en algo el efecto que tendría someterlas a regímenes de presunción" (presumptive income extended to LLCs by the same law).
- **Misreading in the older write-ups:** `Paper/sections/965-DiD.qmd`, `95-colombia-empiric2.qmd`, `98-fiscal-ref-col.qmd` (table) and the slides say individuals' income-tax rate *increased* 8% in 1983 (citing `Ocampo1983`). Both sources say the opposite: a rate **cut**, leaving real incidence ~8% above the 1974 level. Ocampo and Perry (1983), *Coyuntura Económica* 13(1), "B. Rebaja de tarifas", say the 1982–83 cuts reversed nearly all of the increase since 1974 (bib `Ocampo1983`, PDF `Lit-Papers/OcampoPerry1983-ReformaFiscal1982-1983.pdf`). That article also puts the LLC rate at 20% (pre-change).
- **Presumptive income:** the base in ch. 7 ("2 percent of their capital stock") conflicts with the McLure notes (2% of gross receipts, on top of 8% of net wealth). Perry and Cárdenas vol. 1 has the detailed treatment (around PDF pp. 57–60) if needed.
- **Cuadro III.1 transcribed (2026-09-23; checked against the page by Hans):** average individual income-tax rates by taxable income (thousand 1982 pesos: 200, 300, 400, 500, 600, 800, 1,000, 1,500, 2,000), 1982 A (Decreto 2809) vs. 1983 (Decreto 397). Change: −4.63 pp on average across brackets; −22.0% relative, excluding the 200k bracket (−95.3%); range −9.6% to −35.2%. The data live in `Code/Thesis/ch03-income-tax-1983-table.R` (tracked); a local CSV is written to `Code/Products/PerryCardenas1986-CuadroIII1.csv` (CSVs are git-ignored). Table `@tbl-inc-tax-1983` in ch. 3, cited from ch. 7. Note: the OCR had misread the 1983 rate at 1,000 as 24.55; the page says 24.85.

## Ch. 4–5: Testing and deconvolution (`chapters/04-testing.qmd`, `chapters/05-deconvolution.qmd`)

Built 2026-09-24. In the JMP the two chapters read as one section, "Identifying Tax Evasion": ch. 4's H1 and ch. 5's heading switch on `when-meta="jmp"` (ch. 5 becomes an H2). Pandoc warns "Duplicate identifier" for `sec-testing`/`sec-deconvolution` because both conditional headings carry the same id; harmless, the hidden copy is dropped before cross-referencing (checked: one `\label` each in `JMP/paper.tex`, references resolve).

**Where the text came from (visible prose copied, notation translated: the approved paper's multiplicative $e$ is $u=\ln(M^*/M)$ here):**

| Section | Source |
|---|---|
| 4 Non-evaders, identifying $\beta$, observed residual | `Paper/sections/56-id-evasion.qmd` (supervisor-approved; source untouched): Identification Strategy, Assumptions I–II, Identifying the PF parameters, Identifying Tax Evasion (eq-ob-ev) |
| 4 Testing for overreporting | `Paper/sections/200-deconv.qmd`, sec-tax-ev-test (visible paragraphs only). Its results paragraphs (industry counts) come from an older table and were replaced by a draft outline |
| 5 Deconvolution | `56-id-evasion.qmd` ("We can do better however…", convolution definition); `Paper/sections/120-implementation.qmd` (logspline estimator, penalty) |
| 5 Overreporting ratio | `Quarto-Slides/sections/700-deconvolving-evasion.qmd`, "Getting density of the ratio" (eq-ratio-dens, Hogg et al. 2019 Thm 1.7.1) |
| Draft outlines | Robustness of the test and deconvolution conditions (PLAN.md §10); why $u$ and not $e$ (slides 700); implementation facts (from the code) |

**Not reused:** deconvolution by moments and parametric MLE (`200-deconv.qmd`), moments by year, conditional deconvolution (slides 700 backup), translog / two-flexible-inputs derivation (one sentence planned, appendix).

**Code facts (checked against the scripts):**
- Test: `206-boot-test.R` → `boot_test_comp_tbl.RData`. Four variants. Preferred = `pref_tax_ev_test_tbl` ("Fix corps, others"): `resample_by_group` resamples plants separately within corporations and within unincorporated firms; `test_ev_2t_2smpl` takes $\ln D$ from corporations and the mean of $\mathcal V$ over unincorporated firms only. "One-sample" (`test_ev_2t`) averages $\mathcal V$ over all firms, corporations included (313: 0.07 vs. 0.18). Asset `Code/Thesis/ch04-evasion-test.R`.
- Deconvolution: `291-bs-deconv.R` (`full_np_deconv_list` in `bs_mle_data.RData`) deconvolves `fs_list[[.]]$data$cal_V`, which pools ALL firms (the `first_stage_panel` output has no JO filter). Its $f_u$ is therefore diluted by corporations ($u=0$), matching the one-sample test. New `Code/Deconvolution/292-np-deconv-unincorp.R` reruns the same estimator on unincorporated firms only → `np_deconv_unincorp.RData`, used by `Code/Thesis/ch05-overreporting-ratio.R`.
- 369's upper trim (share < 0.75, 10 unincorporated obs, no corporations) exists only in `first_stage_panel_me` (feeds ch. 6/8), not in the test or the deconvolution.
- Sourcing `030-np-deconv-funs.R` from another script re-saves `Code/Products/np-deconv-funs.RData` (it ends with `save(list=ls())`). Load that `.RData` instead, and reset the loaded functions' environment so their helpers resolve (done in `ch05-overreporting-ratio.R`).
- Normalization: $\varepsilon$ is measurement error in output, $E[\varepsilon]=0$ (supervisors' choice, not GNR's output shock), so there is no $\mathcal E$ and $\hat\beta=\exp(E[s\mid\text{corp}])$. The approved paper's $\mathcal E$ equations were dropped when porting; ch. 2's "$E[\exp\varepsilon]=1$" was a misstatement, corrected 2026-09-24.

## Ch. 6: PF parameters and productivity (`chapters/06-pf-productivity.qmd`)

Built 2026-09-24.

| Section | Source |
|---|---|
| Productivity and its Markov process ($\mathcal W$, moments, orthogonality table, $\widetilde{\mathcal W}$) | `Paper/sections/56-id-evasion.qmd`, "Identifying Productivity" (approved; source untouched). Translated: $e\to u$; Markov $h,\delta\to g,\gamma$ (h, δ are taken by $q$/$\kappa$ in @sec-model); "output shock" → measurement error. The approved paper's single-instrument sentence ("labour and capital as instruments for themselves… lag of observed overreported intermediates") replaced by a draft outline of the joint efficient GMM |
| Estimates discussion | `Paper/sections/250-pf.qmd` l. 186 (first sentence kept; the "except one (321)" claim updated: corrected $\hat\beta$ is now below GNR and OLS in all five industries) and l. 190 (verbatim, Canadian spelling) |
| Estimation outline (draft) | CLAUDE.md "PF-step instrument, headline method revised 2026-09-21/22" |

**Not reused:** `250-pf.qmd`'s productivity results (`tbl-omega`, `tbl-prod-comparison`, `tbl-np-prod`): built from the superseded single-instrument estimates. Left as a decision in a draft note (three options). Its l. 184 ("$m^*_{t-1}$ is better than $\mathcal W_{t-2}$…") and the zeros paragraph (l. 188) refer to the old table.

**Ch. 6 update, 2026-09-24 (later):** PF table rebuilt (`ch06-pf-comparison.R`): single-instrument $m^*_{it-1}$ and $\tilde{\mathcal W}_{it-2}$ columns (points from `1472-pf-instrument-comparison.csv`, regions projected from `1473-pf-testinv-grid.csv`), joint efficient GMM (`1478`), GNR and OLS; estimates with the sharp/conservative intervals below. Productivity: `293-omega-deconv-current-pf.R` → `ch06-productivity-comparison.R` (GNR side from `Code/Products/stata-gnr-me-omg-<sic>.csv`, `Code/Stata/020-loop-me.do` + `GNR_code_CD_me.do`, $\omega$ only). `ch06-omega.R` is superseded by the comparison table and not referenced.

## Ch. 8: second instrument dropped (2026-09-24)

`ch08-headline-estimates-table.R` and `ch08-detection-prob-table.R` now report $m^*_{it-1}$ only; the dropped column was `lag_2_cal_W`, the untilded $\mathcal W_{it-2}$ mislabelled with a tilde. Text in ch. 8 edited accordingly (five sentences/bullets), with a draft note. The detection table's blank Median ratio cell (a `format_tt(replace=)` side effect) is fixed.
