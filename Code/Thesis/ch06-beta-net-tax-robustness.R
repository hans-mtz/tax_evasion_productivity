## PRODUCT: Thesis/tables/ch06-beta-net-tax.png := robustness table, beta gross vs net-of-tax first stage
## Reads: Code/Products/1470-net-first-stage-diag.csv (already computed 2026-09-21, NOT re-run here)
## Purpose: the headline PF/ELVIS results (ch. 6, ch. 8) use beta from the GROSS (single-tax) first stage.
## The two-tax-rate model (ch. 2 closing note, ch. 8 counterfactual) shows the evasion FOC only ever needs
## tau_P (already handled correctly everywhere), but the materials-FOC/production-identity side that feeds
## beta picks up a small tau_S/tau_P wedge. This table shows how much beta actually moves, net vs gross, on
## the headline 5 industries -- the evidence for the "small, not zero" claim flagged as a stated limitation
## for supervisors (Thesis/chapters/08-counterfactual.qmd, Scope and limits; Thesis/PLAN.md SS7).
## Deliberately NOT re-running the joint-efficient-GMM alpha_K/alpha_L machinery on the net-of-tax first
## stage this week (that would be a new ELVIS-adjacent estimation run) -- this table isolates exactly the
## quantity in question (beta) using the univariate diagnostic that already exists.
source("Code/Thesis/001-setup.R")

diag <- read.csv(file.path(PRODUCTS_DIR, "1470-net-first-stage-diag.csv")) %>% mutate(sic_3 = as.character(sic_3))

five <- c("331", "322", "369", "313", "321")
f3 <- \(x) sprintf("%.3f", x)

tbl <- diag %>% filter(sic_3 %in% five) %>%
    mutate(sic_3 = factor(sic_3, five)) %>% arrange(sic_3) %>%
    transmute(
        Industry = as.character(sic_3),
        n_corp = as.character(n_corp),
        beta_gross = f3(beta_gross),
        beta_net = f3(beta_net),
        d_beta = f3(d_beta_net_vs_gross)
    )
print(tbl)

## width=1: project default (full book text width). No caption= (Quarto's ![]{#tbl-x} is the single source).
tt_obj <- tt(tbl, align = "lcccc", width = 1,
             notes = "Gross: baseline first stage, $\\ln(M^*_{it}/(P_tY_{it}))$. Net: same estimator, materials share netted of sales taxes on both sides, $\\ln((M^*_{it}-t_{2,it})/(P_tY_{it}-t_{1,it}))$, $t_1=\\tau_S\\cdot\\text{sales}$, $t_2=\\tau_P\\cdot M^*$. Both run on the common sample where both shares are defined. $\\Delta\\hat\\beta$ is small relative to $\\hat\\beta$'s own scale in all five industries.") %>%
    style_tt(i = "notes", fontsize = 0.8)
colnames(tt_obj) <- c("Industry", "$n$ (corps)", "$\\hat\\beta$, gross", "$\\hat\\beta$, net", "$\\Delta\\hat\\beta$")

render_thesis_table(tt_obj, "ch06-beta-net-tax")
cat("Saved: Thesis/tables/ch06-beta-net-tax.{png,pdf}\n")
