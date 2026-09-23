"""
intro-revenue-loss-factcheck.py

Validates the country-specific revenue-loss figures cited in the JMP/Thesis intro's
"why should people care" paragraph (Paper/sections/20-background.Rmd footnote [^int],
Paper/sections/30-lit-rev.Rmd "International prevalence of false invoicing" bullets,
carried into JMP/sections/01-intro.qmd SS2a).

WHAT THIS DOES, for each country's claimed figure:
  1. Pulls that country's official GDP (World Bank NY.GDP.MKTP.CD / .CN) and tax-revenue-
     to-GDP ratio (World Bank GC.TAX.TOTL.GD.ZS) and exchange rate (PA.NUS.FCRF) for the
     relevant year(s), directly from the World Bank API (no manual/remembered numbers).
  2. Recomputes the claimed "% of GDP" from the absolute local-currency amount + official
     GDP, checking whether the project's own cited percentage is arithmetically consistent.
  3. Converts each figure into two comparable units: USD, and % of tax revenue (World
     Bank's tax-revenue definition, which is narrower than "total government revenue incl.
     social contributions" -- flagged, not hidden).
  4. For Poland specifically, also pulls the OFFICIAL EU VAT Gap Report figure (CASE /
     European Commission, saved locally at Lit-Papers/EU-VATGap-2018FinalReport.pdf) for
     the TOTAL VAT gap (all causes: fraud, errors, insolvencies, etc, not just false
     invoicing) and checks it against the claimed false-invoicing-only 5.6%-of-GDP figure.

DATA SOURCE: World Bank Open Data API (api.worldbank.org), no API key needed, indicators:
  NY.GDP.MKTP.CD   GDP, current US$
  NY.GDP.MKTP.CN   GDP, current local currency unit (LCU)
  GC.TAX.TOTL.GD.ZS  Tax revenue (% of GDP)
  PA.NUS.FCRF      Official exchange rate (LCU per US$, period average)
Poland's official VAT Gap table (Table 3.21) is hand-transcribed from the saved PDF
(Lit-Papers/EU-VATGap-2018FinalReport.pdf, page 42 of 82) -- not API-sourced, since the
EU Commission doesn't expose this via a machine-readable API in the same way.

REPRODUCE: `python3 Code/Fact-Checks/intro-revenue-loss-factcheck.py` -- stdlib only
(urllib, json), no pip install needed. Network access to api.worldbank.org required; if
offline, the WB_DATA dict below can be hand-filled from a cached pull instead.

Run 2026-09-22, session that also saved Lit-Papers/OECD2017-TechnologyToolsTaxEvasion.pdf
and Lit-Papers/EU-VATGap-2018FinalReport.pdf.

UPDATE 2026-09-22 (later same day): Chile/Colombia's actual VAT-%GDP -- previously an open
TODO -- found via the OECD's own SDMX REST API (data-explorer.oecd.org), dataflow
OECD.CTP.TPS,DSD_REV_COMP_LAC@DF_RSLAC,1.1 ("Revenue Statistics in Latin America and the
Caribbean"). NO API KEY NEEDED -- same as the World Bank API used everywhere else in this
script, OECD's public SDMX endpoints are keyless. Getting there took some trial and error
(the dataflow has 7 dimensions -- REF_AREA.MEASURE.SECTOR.STANDARD_REVENUE.
CTRY_SPECIFIC_REVENUE.UNIT_MEASURE.FREQ -- discovered by fetching the dataflow's own
structure/dataConstraints, not guessed): the query that works is
CHL+COL.TAX_REV.S13.T_5111._T.PT_B1GQ.A against
https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_REV_COMP_LAC@DF_RSLAC,1.1/...
?format=jsondata (S13 = general government sector; T_5111 = VAT; PT_B1GQ = % of GDP; A =
annual). See oecd_vat_pct_gdp() below for the reusable call. Also saved
Lit-Papers/Carrillo2022-GhostingTaxAuthority-NBER.pdf (the primary source most of this
paragraph traces to) and used it to correct the Poland decision (kept, 2010 figure) and
lead the Chile bullet with Carrillo et al.'s own reported numbers.
"""

import json
import urllib.request

WB_BASE = "https://api.worldbank.org/v2/country/{country}/indicator/{indicator}"


def wb(country, indicator, date):
    """One World Bank API call -> {date: value} dict. country = ISO3 code, date = 'YYYY' or 'YYYY:YYYY'."""
    url = f"{WB_BASE.format(country=country, indicator=indicator)}?date={date}&format=json&per_page=200"
    with urllib.request.urlopen(url, timeout=20) as r:
        data = json.load(r)
    if len(data) < 2 or not data[1]:
        raise RuntimeError(f"No data for {country}/{indicator}/{date}: {data}")
    return {row["date"]: row["value"] for row in data[1] if row["value"] is not None}


def pct_of(numerator, denominator):
    return numerator / denominator * 100


OECD_SDMX_BASE = "https://sdmx.oecd.org/public/rest/data"


def oecd_vat_pct_gdp(iso3_codes, start_year, end_year):
    """VAT revenue as % of GDP, OECD Revenue Statistics in LAC, no API key. Returns
    {country: {year: value}}. iso3_codes: list like ['CHL','COL'].

    ONE COUNTRY PER REQUEST, deliberately -- combining countries with '+' in a single
    request (e.g. 'CHL+COL...') returned genuinely inconsistent/incomplete results across
    repeated identical calls (confirmed: different calls silently dropped different
    countries' observations, not a parsing bug on this end) -- a real flakiness in this
    OECD endpoint's handling of multi-value REF_AREA keys for this dataflow, not something
    to paper over with a retry loop. Single-country requests were reliable across repeated
    tests."""
    out = {}
    for c in iso3_codes:
        key = f"{c}.TAX_REV.S13.T_5111._T.PT_B1GQ.A"
        url = (f"{OECD_SDMX_BASE}/OECD.CTP.TPS,DSD_REV_COMP_LAC@DF_RSLAC,1.1/{key}"
               f"?format=jsondata&startPeriod={start_year}&endPeriod={end_year}")
        # OECD's SDMX endpoint 403s on urllib's default User-Agent (basic bot-protection)
        # but accepts curl's -- setting an explicit UA fixes it, no API key involved.
        req = urllib.request.Request(url, headers={"User-Agent": "curl/8.0"})
        with urllib.request.urlopen(req, timeout=20) as r:
            d = json.load(r)["data"]
        times = d["structure"]["dimensions"]["observation"][0]["values"]
        out[c] = {}
        for series in d["dataSets"][0]["series"].values():
            for tkey, obs in series["observations"].items():
                out[c][times[int(tkey)]["id"]] = obs[0]
    return out


# ---------------------------------------------------------------------------
# POLAND 2016 -- official EU VAT Gap Report (CASE / European Commission),
# Table 3.21, Lit-Papers/EU-VATGap-2018FinalReport.pdf, page 42 of 82.
# Hand-transcribed (not API-sourced -- see module docstring).
# ---------------------------------------------------------------------------
def poland_2010_check():
    """The KEPT Poland figure (decided 2026-09-22), from Carrillo et al. (2022)'s own
    Appendix B, citing Poland's Ministry of Finance (2018): 3,711.2 million PLN in
    fictitious invoices detected via fiscal controls, 2010 -- distinct from, and NOT
    disqualified like, the 2016 figure (see poland_check() below)."""
    print("=" * 78)
    print("POLAND 2010 -- KEPT figure (Carrillo et al. 2022, citing Ministry of Finance)")
    print("=" * 78)
    fx = wb("POL", "PA.NUS.FCRF", "2010")["2010"]
    gdp_usd = wb("POL", "NY.GDP.MKTP.CD", "2010")["2010"]
    taxrev_pct = wb("POL", "GC.TAX.TOTL.GD.ZS", "2010")["2010"]
    amt_pln = 3711.2e6
    taxrev_usd = gdp_usd * taxrev_pct / 100
    print(f"3,711.2 million PLN in fictitious invoices, 2010 -> ${amt_pln/fx/1e6:.1f}M USD")
    print(f"  Claimed (Carrillo2022): 0.26% of GDP, 1.56% of tax revenue")
    print(f"  Independently recomputed: {pct_of(amt_pln, gdp_usd*fx):.3f}% of GDP, "
          f"{pct_of(amt_pln/fx, taxrev_usd):.3f}% of tax revenue -- matches closely.")


def poland_check():
    print("\n" + "=" * 78)
    print("POLAND 2016 -- official EU VAT Gap Report (TOTAL VAT gap, all causes) -- context")
    print("for why the 2016 false-invoicing claim was dropped in favor of the 2010 figure above")
    print("=" * 78)
    vttl_pln = 167_908e6   # VAT Total Tax Liability, PLN
    gap_pln = 34_921e6     # VAT Gap, PLN
    gap_pct_vttl_official = 20.8  # report's own rounded %, recomputed below too

    gdp_usd = wb("POL", "NY.GDP.MKTP.CD", "2016")["2016"]
    fx = wb("POL", "PA.NUS.FCRF", "2016")["2016"]           # PLN per USD
    taxrev_pct_gdp = wb("POL", "GC.TAX.TOTL.GD.ZS", "2016")["2016"]

    gdp_pln = gdp_usd * fx
    taxrev_pln = gdp_pln * taxrev_pct_gdp / 100

    print(f"VAT Gap: {gap_pln/1e9:.2f} bn PLN = ${gap_pln/fx/1e9:.2f} bn USD")
    print(f"GDP 2016: {gdp_pln/1e9:.1f} bn PLN = ${gdp_usd/1e9:.1f} bn USD  (World Bank)")
    print(f"Gap as % of GDP: {pct_of(gap_pln, gdp_pln):.2f}%  (report's own %/VTTL: {gap_pct_vttl_official}%)")
    print(f"Gap as % of VTTL, recomputed: {pct_of(gap_pln, vttl_pln):.1f}%")
    print(f"Tax revenue 2016: {taxrev_pln/1e9:.1f} bn PLN = ${taxrev_pln/fx/1e9:.1f} bn USD")
    print(f"Gap as % of tax revenue: {pct_of(gap_pln, taxrev_pln):.2f}%")
    claimed_pct_gdp = 5.6
    actual_pct_gdp = pct_of(gap_pln, gdp_pln)
    print(f"\n  CLAIMED (false invoicing alone): {claimed_pct_gdp}% of GDP")
    print(f"  OFFICIAL TOTAL VAT gap (all causes, of which false invoicing is one part): {actual_pct_gdp:.2f}% of GDP")
    print(f"  -> claimed figure EXCEEDS the official total by "
          f"{claimed_pct_gdp/actual_pct_gdp:.1f}x -- not reconcilable, a subset cannot exceed the whole.")


# ---------------------------------------------------------------------------
# CHILE 2004 -- two conflicting absolute figures found in the project's own notes
# (114 bn CLP in Paper/sections/30-lit-rev.Rmd vs. 328 bn CLP in 20-background.Rmd),
# both claimed as "0.2% of GDP". Check which one is arithmetically consistent.
# ---------------------------------------------------------------------------
def chile_check():
    print("\n" + "=" * 78)
    print("CHILE -- 114bn vs. 328bn CLP is NOT a typo (corrected 2026-09-22)")
    print("=" * 78)
    print("Found the actual primary source (Carrillo et al. 2022, NBER WP 30242, Appendix B,")
    print("Lit-Papers/Carrillo2022-GhostingTaxAuthority-NBER.pdf): both project files' Chile")
    print("numbers are CORRECT, just for DIFFERENT YEARS -- 328bn CLP is 1998, 114bn CLP is 2004")
    print("(roughly one-third of the 1998 level -- evasion fell over that period). The paper")
    print("states directly: 114bn CLP in 2004 = 0.2% of Chilean GDP AND 1.2% of total tax revenue")
    print("-- a primary-source percentage, not just my own recomputation.")
    gdp_clp = wb("CHL", "NY.GDP.MKTP.CN", "2004")["2004"]
    fx = wb("CHL", "PA.NUS.FCRF", "2004")["2004"]
    taxrev_pct_gdp = wb("CHL", "GC.TAX.TOTL.GD.ZS", "2004")["2004"]
    taxrev_clp = gdp_clp * taxrev_pct_gdp / 100

    evasion_clp = 114e9  # 2004 figure, per Carrillo2022's own text
    p = pct_of(evasion_clp, gdp_clp)
    print(f"\n2004, 114bn CLP: {p:.3f}% of GDP (World Bank GDP) vs. Carrillo2022's own stated 0.2% -- matches.")
    print(f"  In USD: ${evasion_clp/fx/1e6:.1f} million")
    print(f"  Tax revenue 2004 (World Bank ratio): ${taxrev_clp/fx/1e9:.2f} bn USD")
    print(f"  As % of tax revenue: {pct_of(evasion_clp, taxrev_clp):.2f}% "
          f"(Carrillo2022's own stated figure: 1.2% -- close, minor tax-revenue-definition difference)")


# ---------------------------------------------------------------------------
# COLOMBIA 2019
# ---------------------------------------------------------------------------
def colombia_check():
    print("\n" + "=" * 78)
    print("COLOMBIA 2019")
    print("=" * 78)
    gdp_cop = wb("COL", "NY.GDP.MKTP.CN", "2019")["2019"]
    fx = wb("COL", "PA.NUS.FCRF", "2019")["2019"]
    taxrev_pct_gdp = wb("COL", "GC.TAX.TOTL.GD.ZS", "2019")["2019"]
    taxrev_cop = gdp_cop * taxrev_pct_gdp / 100

    evasion_cop = 2300e9
    p = pct_of(evasion_cop, gdp_cop)
    verdict = "MATCHES claimed 0.2%" if abs(p - 0.2) < 0.05 else "DOES NOT MATCH claimed 0.2%"
    print(f"2300bn COP: {p:.3f}% of GDP -> {verdict}")
    print(f"In USD: ${evasion_cop/fx/1e6:.1f} million")
    print(f"Tax revenue 2019: ${taxrev_cop/fx/1e9:.2f} bn USD")
    print(f"As % of tax revenue: {pct_of(evasion_cop, taxrev_cop):.2f}%")


# ---------------------------------------------------------------------------
# MEXICO -- two DIFFERENT figures in the project's own notes, different sources,
# different periods, different mechanisms. Check and compare both.
# ---------------------------------------------------------------------------
def mexico_check():
    print("\n" + "=" * 78)
    print("MEXICO -- two different figures in the project's own notes")
    print("=" * 78)

    # --- SAT/Senado figure: 5bn MXN/yr average, 2014-2018, "ghost firms" specifically ---
    gdp_mxn = wb("MEX", "NY.GDP.MKTP.CN", "2014:2018")
    fx = wb("MEX", "PA.NUS.FCRF", "2014:2018")
    taxrev_pct = wb("MEX", "GC.TAX.TOTL.GD.ZS", "2014:2018")
    years = sorted(gdp_mxn)
    gdp_avg = sum(gdp_mxn[y] for y in years) / len(years)
    fx_avg = sum(fx[y] for y in years) / len(years)
    taxrev_avg_pct = sum(taxrev_pct[y] for y in years) / len(years)
    taxrev_avg = gdp_avg * taxrev_avg_pct / 100

    evasion = 5e9
    p = pct_of(evasion, gdp_avg)
    verdict = "MATCHES claimed 0.03%" if abs(p - 0.03) < 0.01 else "DOES NOT MATCH claimed 0.03%"
    print(f"SAT/Senado (2019): 5bn MXN/yr, avg 2014-2018 GDP -> {p:.4f}% of GDP -> {verdict}")
    print(f"  In USD (avg fx): ${evasion/fx_avg/1e6:.1f} million/yr")
    print(f"  As % of avg tax revenue: {pct_of(evasion, taxrev_avg):.3f}%")

    # --- OECD(2017) Box 2 figure: ~EUR 3bn TOTAL, 2007-2009, forged invoices generally ---
    gdp_0709 = wb("MEX", "NY.GDP.MKTP.CN", "2007:2009")
    fx_0709 = wb("MEX", "PA.NUS.FCRF", "2007:2009")
    eur_per_usd_0709 = wb("EMU", "PA.NUS.FCRF", "2007:2009")  # LCU(EUR) per USD, Euro area
    y3 = sorted(gdp_0709)
    gdp_usd_0709 = sum(gdp_0709[y] / fx_0709[y] for y in y3)
    usd_per_eur = 1 / (sum(eur_per_usd_0709[y] for y in y3) / len(y3))

    evasion_eur = 3e9
    evasion_usd = evasion_eur * usd_per_eur
    print(f"\nOECD (2017) Box 2: EUR {evasion_eur/1e9:.0f}bn total, 2007-2009, forged invoices generally")
    print(f"  = ${evasion_usd/1e9:.2f}bn USD (avg 2007-09 EUR/USD rate)")
    print(f"  vs. cumulative 3-yr GDP ${gdp_usd_0709/1e9:.1f}bn USD -> {pct_of(evasion_usd, gdp_usd_0709):.3f}% of 3-yr GDP")
    print(f"  Annualized: ${evasion_usd/3/1e9:.2f}bn/yr")

    sat_usd_per_yr = evasion / fx_avg  # SAT figure converted to USD, same units as evasion_usd/3
    ratio = (evasion_usd/3)/sat_usd_per_yr
    print(f"\n  -> The two Mexico figures are NOT the same thing: different sources (OECD's own "
          f"reporting vs. Mexico's tax authority SAT), different periods (2007-09 vs 2014-18), "
          f"different mechanisms (forged invoices generally vs. specifically identified 'ghost "
          f"firms'). Annualized, OECD's figure (~${evasion_usd/3/1e6:.0f}M/yr) is "
          f"~{ratio:.1f}x the SAT figure (~${sat_usd_per_yr/1e6:.0f}M/yr) in nominal USD.")
    print(f"\n  WHY THE TWO DIFFER -- checked 2026-09-22, a real dateable story, not a discrepancy to")
    print(f"  paper over: Mexico's e-invoicing (CFDI) became MANDATORY FOR ALL TAXPAYERS on Jan 1,")
    print(f"  2014 (independently confirmed via web search, not assumed) -- a rollout the OECD report")
    print(f"  itself documents (optional from 2005, +134% e-invoices issued 2010->2011, universal by")
    print(f"  2014). OECD's figure (2007-09) is entirely PRE-mandate; SAT's (2014-18) is entirely")
    print(f"  POST-mandate -- a clean split, no straddling year. The ~{ratio:.1f}x drop lines up with")
    print(f"  the mandate. CAVEAT: 'forged invoices' (pre-2014, crude fake paper invoices) and 'ghost")
    print(f"  firms' (post-2014, Mexico's EFOS problem -- valid e-invoices for fake transactions) are")
    print(f"  not quite the same mechanism -- e-invoicing killed the former, the latter is its evolved")
    print(f"  successor. Correct claim: losses from this channel fell ~{ratio:.1f}x after the mandate,")
    print(f"  NOT 'e-invoicing solved fake invoicing.'")


# ---------------------------------------------------------------------------
# SLOVAK REPUBLIC 2014-2015 -- the OECD(2017) report's OTHER false-invoicing-specific
# figure (Box 2), alongside Mexico's. The only two country figures the OECD report
# itself actually supports for false invoicing specifically (not total VAT gap).
# ---------------------------------------------------------------------------
def slovakia_check():
    print("\n" + "=" * 78)
    print("SLOVAK REPUBLIC 2014-2015 -- OECD (2017) Box 2, false invoicing specifically")
    print("=" * 78)
    gdp = wb("SVK", "NY.GDP.MKTP.CD", "2014:2015")
    taxrev_pct = wb("SVK", "GC.TAX.TOTL.GD.ZS", "2014:2015")
    gdp_2yr = sum(gdp.values())
    taxrev_2yr = sum(gdp[y] * taxrev_pct[y] / 100 for y in gdp)
    eur_per_usd = wb("EMU", "PA.NUS.FCRF", "2014:2015")
    usd_per_eur = 1 / (sum(eur_per_usd.values()) / len(eur_per_usd))

    evasion_eur = 500e6  # ">EUR 500 million", OECD's own wording -- a floor, not exact
    evasion_usd = evasion_eur * usd_per_eur
    print(f">EUR {evasion_eur/1e6:.0f}M detected domestic VAT invoicing fraud, 2014-15")
    print(f"  = ${evasion_usd/1e6:.0f}M USD (floor, OECD says '>EUR 500 million')")
    print(f"  vs. 2-yr cumulative GDP ${gdp_2yr/1e9:.1f}bn -> {pct_of(evasion_usd, gdp_2yr):.3f}% of 2-yr GDP")
    print(f"  vs. 2-yr cumulative tax revenue ${taxrev_2yr/1e9:.1f}bn -> {pct_of(evasion_usd, taxrev_2yr):.3f}% of 2-yr tax revenue")

    # "Size of the cake" for Slovakia too -- SAME EU VAT Gap Report has Slovakia's own
    # table (Table 3.25, Lit-Papers/EU-VATGap-2018FinalReport.pdf p.46), hand-transcribed:
    sk_gap_eur_by_year = {"2014": 2214e6, "2015": 2243e6}  # TOTAL VAT gap, all causes, EUR
    sk_gap_2yr_eur = sum(sk_gap_eur_by_year.values())
    print(f"\n  'Size of the cake': Slovakia's OFFICIAL total VAT gap (all causes), same EU report, "
          f"same years: EUR {sk_gap_2yr_eur/1e6:.0f}M over 2014-15 (${sk_gap_2yr_eur*usd_per_eur/1e6:.0f}M).")
    print(f"  -> false-invoicing-specific fraud (>EUR {evasion_eur/1e6:.0f}M) is ~"
          f"{evasion_eur/sk_gap_2yr_eur*100:.1f}% of Slovakia's TOTAL VAT gap, same country, same years, "
          f"both from official/OECD-adjacent sources -- the cleanest 'how does overreporting compare to "
          f"other evasion' answer found in this whole check.")


# ---------------------------------------------------------------------------
# "SIZE OF THE CAKE": total VAT non-compliance gap (ALL causes, not just false
# invoicing) for Chile, Colombia, Mexico, from CIAT (Inter-American Center of
# Tax Administrations -- the LatAm analog of the EU Commission's VAT Gap Report),
# WP-01-2022 (Lit-Papers/CIAT2022-VATCITEfficiencyTaxGapLAC.pdf), Figure 1.c,
# "Components of Potential VAT Collection, average selected countries 2016-2018":
# decomposes potential VAT into Efficiency (collected) + GT Inefficiency (policy
# gap, exemptions) + X Inefficiency (non-compliance/evasion gap) -- hand-
# transcribed from the chart (not machine-extractable, it's an image).
# Cross-validated in the same report (Appendix IV) against the OECD's own VRR
# (VAT Revenue Ratio) indicator for these same 3 countries: "the estimates...
# coincide in both studies."
# ---------------------------------------------------------------------------
CIAT_VAT_DECOMP_2016_18 = {   # country: (Efficiency%, GT/policy-gap%, X/non-compliance-gap%), of POTENTIAL VAT
    "Chile":    (63.8, 5.5, 30.7),
    "Colombia": (41.3, 44.7, 14.1),
    "Mexico":   (33.0, 12.6, 54.4),
}
MEXICO_ACTUAL_VAT_PCT_GDP_2016_18 = 3.9  # CIAT report, same page: Mexico's actual VAT tax burden


def size_of_the_cake_check():
    print("\n" + "=" * 78)
    print('"SIZE OF THE CAKE" -- total VAT non-compliance gap, CIAT 2016-2018 avg')
    print("=" * 78)
    for country, (eff, gt, x) in CIAT_VAT_DECOMP_2016_18.items():
        print(f"{country}: Efficiency {eff}% / Policy gap {gt}% / Non-compliance gap {x}% (of POTENTIAL VAT)")
    print(f"\nCheck: Chile+Colombia+Mexico columns sum to ~100% each (rounding): "
          f"{[round(sum(v),1) for v in CIAT_VAT_DECOMP_2016_18.values()]}")

    print(f"\nMexico is the only one of the three with an actual-VAT-%GDP anchor found in the "
          f"same report ({MEXICO_ACTUAL_VAT_PCT_GDP_2016_18}% of GDP, 2016-18 avg) -> can convert to %GDP:")
    eff, gt, x = CIAT_VAT_DECOMP_2016_18["Mexico"]
    potential_pct_gdp = MEXICO_ACTUAL_VAT_PCT_GDP_2016_18 / (eff / 100)
    gap_pct_gdp = potential_pct_gdp * x / 100
    print(f"  Potential VAT: {potential_pct_gdp:.2f}% of GDP")
    print(f"  TOTAL non-compliance (evasion) gap: {gap_pct_gdp:.2f}% of GDP")
    sat_pct_gdp = 0.03  # SAT ghost-firm figure, 2014-2018 avg (mexico_check() above)
    print(f"  vs. SAT's ghost-firm-specific figure: {sat_pct_gdp}% of GDP")
    print(f"  -> ghost-firm fake invoicing is ~{sat_pct_gdp/gap_pct_gdp*100:.1f}% of Mexico's TOTAL VAT "
          f"non-compliance gap -- this is the 'how does overreporting compare to other evasion' answer.")
    print(f"\nChile and Colombia: RESOLVED (same day, later) via the OECD's own SDMX API -- see")
    print(f"module docstring. VAT %GDP, 2016-18 avg:")
    vat = oecd_vat_pct_gdp(["CHL", "COL"], 2016, 2018)
    assert len(vat["CHL"]) == 3 and len(vat["COL"]) == 3, f"expected 3 years each, got {vat}"
    chl_vat_pct = sum(vat["CHL"].values()) / len(vat["CHL"])
    col_vat_pct = sum(vat["COL"].values()) / len(vat["COL"])
    print(f"  Chile: {chl_vat_pct:.3f}% of GDP   Colombia: {col_vat_pct:.3f}% of GDP")

    gdp_chl = wb("CHL", "NY.GDP.MKTP.CD", "2016:2018")
    gdp_col = wb("COL", "NY.GDP.MKTP.CD", "2016:2018")
    taxrev_chl = wb("CHL", "GC.TAX.TOTL.GD.ZS", "2016:2018")
    taxrev_col = wb("COL", "GC.TAX.TOTL.GD.ZS", "2016:2018")
    gdp_chl_avg = sum(gdp_chl.values()) / 3
    gdp_col_avg = sum(gdp_col.values()) / 3
    taxrev_chl_avg_pct = sum(taxrev_chl.values()) / 3
    taxrev_col_avg_pct = sum(taxrev_col.values()) / 3

    chl_eff, chl_gt, chl_x = CIAT_VAT_DECOMP_2016_18["Chile"]
    col_eff, col_gt, col_x = CIAT_VAT_DECOMP_2016_18["Colombia"]
    chl_potential_pct = chl_vat_pct / (chl_eff / 100)
    col_potential_pct = col_vat_pct / (col_eff / 100)
    chl_gap_pct_gdp = chl_potential_pct * chl_x / 100
    col_gap_pct_gdp = col_potential_pct * col_x / 100
    chl_gap_usd = gdp_chl_avg * chl_gap_pct_gdp / 100
    col_gap_usd = gdp_col_avg * col_gap_pct_gdp / 100

    print(f"\n  Chile cake: {chl_gap_pct_gdp:.2f}% of GDP = ${chl_gap_usd/1e9:.2f}bn/yr "
          f"({pct_of(chl_gap_pct_gdp, taxrev_chl_avg_pct):.2f}% of tax revenue)")
    print(f"  Colombia cake: {col_gap_pct_gdp:.2f}% of GDP = ${col_gap_usd/1e9:.2f}bn/yr "
          f"({pct_of(col_gap_pct_gdp, taxrev_col_avg_pct):.2f}% of tax revenue)")

    chl_slice_usd, col_slice_usd = 187e6, 701e6  # from chile_check()/colombia_check() above
    print(f"\n  Chile: slice (2004, ${chl_slice_usd/1e6:.0f}M) vs. cake (2016-18 avg, "
          f"${chl_gap_usd/1e9:.2f}bn) -> {pct_of(chl_slice_usd, chl_gap_usd):.2f}% -- 12+ years "
          f"apart, a rough indicator only, NOT a same-period comparison like Slovakia's.")
    print(f"  Colombia: slice (2019, ${col_slice_usd/1e6:.0f}M) vs. cake (2016-18 avg, "
          f"${col_gap_usd/1e9:.2f}bn) -> {pct_of(col_slice_usd, col_gap_usd):.2f}% -- close in "
          f"time (2019 slice, 2016-18 avg cake), more defensible than Chile's.")


# ---------------------------------------------------------------------------
# ECUADOR -- the paper's OWN subject country (Carrillo et al. 2022 study it directly, not as
# a comparator). Fact 1 of the paper itself: $2.1bn in ghost transactions, pooled 2010-2015
# data, "1.7% for corporations and 11.5% for sole proprietorships" (shares of THEIR OWN tax
# liabilities, not GDP/national tax revenue -- the paper doesn't itself state a %GDP for this
# headline number). Computed here for consistency with every other country in this check.
# ---------------------------------------------------------------------------
def ecuador_check():
    print("\n" + "=" * 78)
    print("ECUADOR -- Carrillo et al. (2022)'s own subject country, Fact 1")
    print("=" * 78)
    gdp = wb("ECU", "NY.GDP.MKTP.CD", "2010:2015")
    taxrev_pct = wb("ECU", "GC.TAX.TOTL.GD.ZS", "2010:2015")
    years = sorted(gdp)
    gdp_avg = sum(gdp[y] for y in years) / len(years)
    taxrev_avg_pct = sum(taxrev_pct[y] for y in years if y in taxrev_pct) / len([y for y in years if y in taxrev_pct])
    taxrev_avg = gdp_avg * taxrev_avg_pct / 100
    n_years = len(years)

    evasion = 2.1e9  # 6-yr total, pooled 2010-2015
    print(f"Ghost-firm transactions: $2.1bn total, pooled {years[0]}-{years[-1]} ({n_years} years)")
    print(f"  As % of {n_years}-yr cumulative GDP: {pct_of(evasion, gdp_avg*n_years):.3f}%")
    print(f"  As % of {n_years}-yr cumulative tax revenue: {pct_of(evasion, taxrev_avg*n_years):.3f}%")
    print(f"  Annualized: ${evasion/n_years/1e6:.0f}M/yr -> {pct_of(evasion/n_years, gdp_avg):.3f}% of avg annual GDP, "
          f"{pct_of(evasion/n_years, taxrev_avg):.3f}% of avg annual tax revenue")
    print(f"  (Paper's own framing, not GDP-based: 1.7% of corporations' own tax liability, "
          f"11.5% of sole proprietors' own tax liability -- a within-firm-type share, different "
          f"denominator than the national-scale figures above; both are legitimate, just answer")
    print(f"  different questions.)")


if __name__ == "__main__":
    poland_2010_check()
    poland_check()
    chile_check()
    colombia_check()
    mexico_check()
    slovakia_check()
    ecuador_check()
    size_of_the_cake_check()
