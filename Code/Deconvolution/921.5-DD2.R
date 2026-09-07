## %% Setup ---------------------
library(tidyverse)
library(fixest)

load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/910-reg-results.RData") # wip_df, dict
load("Code/Products/921-DD2.RData") # sales_tax_83_group

# Colorblind-friendly palette
tol_cb_palette <- c(
  "#332288", "#76b1cf", "#44AA99", "#117733", "#999933",
  "#DDCC77", "#CC6677", "#882255", "#AA4499"
)
palette(tol_cb_palette)

## %% Focus on tau_1 (Purchase Tax Rate) Changes ---------------------

# Extract tau_1 changes from sales_tax_83_group
tau_1_groups <- sales_tax_83_group %>%
  select(sic_3, tau_1) %>%
  mutate(
    tau_1_simple = case_when(
      sic_3 %in% c("311", "312") ~ "Exempt",  # Manually add exempt industries
      grepl("Increase", tau_1) ~ "Increase",
      grepl("Decrease", tau_1) ~ "Decrease",
      grepl("No Change", tau_1) ~ "No Change",
      # grepl("Exempt", tau_1) ~ "Exempt",
      TRUE ~ tau_1
    )
  )

# Check distribution
tau_1_groups %>%
  ungroup() %>%
  count(tau_1_simple)

## %% Prepare data for DiD analysis ---------------------

wip_df_tau1 <- wip_df %>%
  left_join(tau_1_groups, by = "sic_3") %>%
  mutate(
    # Create interaction term: corp × tau_1_change × year (following 921-DD2.R approach)
    corp_tau1_year = factor(
      ifelse(
        corp == "Corp" | tau_1_simple == "Exempt" | year == "83",
        "Base",
        paste(corp, tau_1_simple, year, sep = ":")
      )
    ),
    # Separate interaction for heterogeneous effects
    corp_tau1_year_het = factor(
      ifelse(
        corp == "Corp" | year == "83",
        "Base",
        paste(corp, tau_1_simple, year, sep = ":")
      )
    )
  )

# Check the factor levels
wip_df_tau1 %>%
  select(corp, tau_1_simple, year, corp_tau1_year) %>%
  distinct() %>%
  arrange(corp, tau_1_simple, year) %>%
  head(20)

## %% DiD Regression: Corp vs Non-Corp by tau_1 changes ---------------------

# Main DiD specification (following 921-DD2.R approach)
did_corp_tau1 <- feols(
  log_mats_share ~ 
    # corp +
    # i(corp, year, ref = "Corp", ref2 = "83") +
    i(corp, year, ref = "Corp") +
    i(corp, tau_1_simple, ref = "Corp", ref2 = "Exempt") +
    i(corp_tau1_year, "Base") | sic_3,
  cluster = ~ plant + year,
  data = wip_df_tau1
)

did_corp_tau1 |> etable(dict = dict)

# Heterogeneous effects (without base DiD)
did_corp_tau1_het <- feols(
  log_mats_share ~ 
    # corp +
    i(corp, tau_1_simple, ref = "Corp") +
    i(corp_tau1_year_het, "Base") | sic_3,
  cluster = ~ plant + year,
  data = wip_df_tau1
)

did_corp_tau1_het |> etable(dict = dict)

## %% Plot: Non-Corp by tau_1 changes (vs Exempt base) ---------------------
# Isolates sales tax effects by comparing to Exempt industries

png(
  file = "Code/Products/921.5-corp-tau1-increase.png",
  width = 820, height = 480
)

# tau_1 Increase
ref_vec_inc <- did_corp_tau1 |> coef() |> names() |>
  grep("corp_tau1_year::Other:Increase:82$", x = _, value = FALSE)
ref_vec_inc <- ref_vec_inc + 1
names(ref_vec_inc) <- "corp_tau1_year::Other:Increase:83"

coefplot(
  did_corp_tau1,
  keep = "corp_tau1_year::Other:Increase:\\d{2}$",
  dict = dict,
  group = list(`Increase` = "^^corp_tau1_year::Other:Increase:"),
  ylim = c(-0.08, 0.13),
  col = 3,
  ref = ref_vec_inc,
  main = "Sales Tax Effect on Input Overreporting (vs. Exempt Industries)\nNon-Corporations",
  grid = FALSE
)

# tau_1 Decrease
ref_vec_dec <- did_corp_tau1 |> coef() |> names() |>
  grep("corp_tau1_year::Other:Decrease:82$", x = _, value = FALSE)
ref_vec_dec <- ref_vec_dec + 1
names(ref_vec_dec) <- "corp_tau1_year::Other:Decrease:83"

coefplot(
  did_corp_tau1,
  keep = "corp_tau1_year::Other:Decrease:\\d{2}$",
  dict = dict,
  group = list(`Decrease` = "^^corp_tau1_year::Other:Decrease:"),
  # ylim = c(-0.08, 0.13),
  add = TRUE,
  col = 2,
  x.shift = 0.2,
  ref = ref_vec_dec,
  grid = FALSE
)

# tau_1 No Change
ref_vec_nc <- did_corp_tau1 |> coef() |> names() |>
  grep("corp_tau1_year::Other:No Change:82$", x = _, value = FALSE)

if (length(ref_vec_nc) > 0) {
  ref_vec_nc <- ref_vec_nc + 1
  names(ref_vec_nc) <- "corp_tau1_year::Other:No Change:83"

  coefplot(
    did_corp_tau1,
    keep = "corp_tau1_year::Other:No Change:\\d{2}$",
    dict = dict,
    group = list(`No Change` = "^^corp_tau1_year::Other:No Change:"),
    add = TRUE,
    col = 7,
    x.shift = 0.4,
    ref = ref_vec_nc,
    grid = FALSE
  )

  legend_labels <- c("tau_1 Increase", "tau_1 Decrease", "tau_1 No Change")
  legend_cols <- c(3, 2, 7)
} else {
  legend_labels <- c("tau_1 Increase", "tau_1 Decrease")
  legend_cols <- c(3, 2)
}

legend(
  "topleft",
  legend = legend_labels,
  col = legend_cols,
  lwd = 2,
  pch = 16
)

abline(v = c(3, 7), lty = 2, col = "gray")
abline(h = 0, lty = 1, col = "black")

dev.off()

## %% Plot: Compare tau_1 Increase vs Decrease vs No Change vs Exempt ---------------------
# All coefficients from did_corp_tau1_het for apples-to-apples comparison
# (all relative to Corporations)

png(
  file = "Code/Products/921.5-corp-tau1-all-changes.png",
  width = 820, height = 480
)

# tau_1 Increase
ref_vec_inc <- did_corp_tau1_het |> coef() |> names() |>
  grep("corp_tau1_year_het::Other:Increase:82$", x = _, value = FALSE)
ref_vec_inc <- ref_vec_inc + 1
names(ref_vec_inc) <- "corp_tau1_year_het::Other:Increase:83"

coefplot(
  did_corp_tau1_het,
  keep = "corp_tau1_year_het::Other:Increase:\\d{2}$",
  dict = dict,
  group = list(`Increase` = "^^corp_tau1_year_het::Other:Increase:"),
  col = 3,
  ref = ref_vec_inc,
  main = "Effect of Purchase Tax Rate Changes on Input Overreporting\nNon-Corporations vs. Corporations",
  grid = FALSE
)

# tau_1 Decrease
ref_vec_dec <- did_corp_tau1_het |> coef() |> names() |>
  grep("corp_tau1_year_het::Other:Decrease:82$", x = _, value = FALSE)
ref_vec_dec <- ref_vec_dec + 1
names(ref_vec_dec) <- "corp_tau1_year_het::Other:Decrease:83"

coefplot(
  did_corp_tau1_het,
  keep = "corp_tau1_year_het::Other:Decrease:\\d{2}$",
  dict = dict,
  group = list(`Decrease` = "^^corp_tau1_year_het::Other:Decrease:"),
  ylim = c(-0.05, 0.15),
  add = TRUE,
  col = 2,
  x.shift = 0.2,
  ref = ref_vec_dec,
  grid = FALSE
)

# tau_1 No Change
ref_vec_nc <- did_corp_tau1_het |> coef() |> names() |>
  grep("corp_tau1_year_het::Other:No Change:82$", x = _, value = FALSE)

if (length(ref_vec_nc) > 0) {
  ref_vec_nc <- ref_vec_nc + 1
  names(ref_vec_nc) <- "corp_tau1_year_het::Other:No Change:83"

  coefplot(
    did_corp_tau1_het,
    keep = "corp_tau1_year_het::Other:No Change:\\d{2}$",
    dict = dict,
    group = list(`No Change` = "^^corp_tau1_year_het::Other:No Change:"),
    add = TRUE,
    col = 7,
    x.shift = 0.4,
    ref = ref_vec_nc,
    grid = FALSE
  )

  legend_labels <- c("tau_1 Increase", "tau_1 Decrease", "tau_1 No Change", "Exempt")
  legend_cols <- c(3, 2, 7, 1)
} else {
  legend_labels <- c("tau_1 Increase", "tau_1 Decrease", "Exempt")
  legend_cols <- c(3, 2, 1)
}

# Exempt industries (from same specification)
coefplot(
  did_corp_tau1_het,
  keep = "corp_tau1_year_het::Other:Exempt:\\d{2}$",
  dict = dict,
  group = list(`Exempt` = "^^corp_tau1_year_het::Other:Exempt:"),
  add = TRUE,
  col = 1,
  x.shift = ifelse(length(ref_vec_nc) > 0, 0.6, 0.4),
  ref.line = 3,
  grid = FALSE
)

legend(
  "topleft",
  legend = legend_labels,
  col = legend_cols,
  lwd = 2,
  pch = 16
)

abline(v = c(3, 7), lty = 2, col = "lightgray")
abline(h = 0, lty = 1, col = "black")

dev.off()

## %% Summary Statistics ---------------------

# Count observations by group
wip_df_tau1 %>%
  group_by(corp, tau_1_simple) %>%
  summarise(
    n_obs = n(),
    n_plants = n_distinct(plant),
    n_industries = n_distinct(sic_3),
    .groups = "drop"
  ) %>%
  arrange(tau_1_simple, corp)

## %% Save results ---------------------

save(
  wip_df_tau1,
  tau_1_groups,
  did_corp_tau1,
  did_corp_tau1_het,
  file = "Code/Products/921.5-DD2.RData"
)

## %% Notes ---------------------

# Expected results:
# - Non-corporations in industries with tau_1 increases should show
#   significant positive coefficients on log_mats_share after 1983
# - This would indicate higher overreporting when purchase tax rates increase
# - Industries with tau_1 decreases or no change should show smaller/no effect
# - Pre-trend (years 81-82) should be close to zero (parallel trends assumption)

## %% Revised Analysis: Drop Exempt, Control for tau_0 ---------------------
# Exempt industries (311, 312) still pay sales taxes on purchases
# Better base: "No Change" industries

# Create revised tau_1 groups excluding exempt

tau_1_groups_revised <- sales_tax_83_group %>%
  select(sic_3, tau_1) %>%
  mutate(
    tau_1_simple = case_when(
      # sic_3 %in% c("311", "312") ~ "Exempt",  # Manually add exempt industries
      grepl("Increase", tau_1) ~ "Increase",
      grepl("Decrease", tau_1) ~ "Decrease",
      grepl("No Change", tau_1) ~ "No Change",
      # grepl("Exempt", tau_1) ~ "Exempt",
      TRUE ~ tau_1
    )
  )

wip_df_tau1_revised <- wip_df %>%
  left_join(tau_1_groups_revised, by = "sic_3") %>%
  filter(!is.na(tau_1_simple)) %>%
  mutate(
    corp_tau1_year_noex = factor(
      ifelse(
        corp == "Corp" | tau_1_simple == "No Change" | year == "83",
        "Base",
        paste(corp, tau_1_simple, year, sep = ":")
      )
    )
  )

# Extract tau_0 changes (sales tax on sales)
tau_0_groups <- sales_tax_83_group %>%
  select(sic_3, tau_0) %>%
  # filter(!sic_3 %in% c("311", "312")) %>%
  mutate(
    tau_0_simple = case_when(
      sic_3 %in% c("311", "312") ~ "Exempt",  # Manually add exempt industries
      grepl("Increase", tau_0) ~ "Increase",
      grepl("Decrease", tau_0) ~ "Decrease",
      grepl("No Change", tau_0) ~ "No Change",
      TRUE ~ tau_0
    )
  )

wip_df_tau1_revised <- wip_df_tau1_revised %>%
  left_join(tau_0_groups, by = "sic_3") %>%
  mutate(
    corp_tau0_year = factor(
      ifelse(
        corp == "Corp" | tau_0_simple == "Exempt" | year == "83",
        "Base",
        paste(corp, tau_0_simple, year, sep = ":")
      )
    )
  )

# Check distribution
wip_df_tau1_revised %>%
  group_by(tau_1_simple, tau_0_simple) %>%
  summarise(
    n_obs = n(),
    n_plants = n_distinct(plant),
    n_industries = n_distinct(sic_3),
    .groups = "drop"
  ) %>%
  arrange(tau_1_simple, tau_0_simple)

## %% Spec 1: Control for tau_0 changes (categorical) ---------------------

did_corp_tau1_ctrl_tau0 <- feols(
  log_mats_share ~
    i(corp, year, ref = "Corp") +
    i(corp, tau_1_simple, ref = "Corp", ref2 = "No Change") +
    i(corp, tau_0_simple, ref = "Corp", ref2 = "No Change") +
    i(corp_tau1_year_noex, "Base") | sic_3,
  cluster = ~ plant + year,
  data = wip_df_tau1_revised
)

did_corp_tau1_ctrl_tau0 |> etable(dict = dict)

## %% Spec 2: Control for tau_0 with corp_tau0_year interaction ---------------------

did_corp_tau1_ctrl_tau0_interact <- feols(
  log_mats_share ~
    i(corp, year, ref = "Corp") +
    i(corp, tau_1_simple, ref = "Corp", ref2 = "No Change") +
    i(corp, tau_0_simple, ref = "Corp", ref2 = "Exempt") +
    i(corp_tau1_year_noex, "Base") +
    i(corp_tau0_year, "Base") | sic_3,
  cluster = ~ plant + year,
  data = wip_df_tau1_revised
)

did_corp_tau1_ctrl_tau0_interact |> etable(dict = dict)

## %% Spec 3: Control for continuous sales_tax_rate_sales ---------------------


did_corp_tau1_ctrl_continuous <- feols(
  log_mats_share ~
    i(corp, year, ref = "Corp") +
    i(corp, tau_1_simple, ref = "Corp", ref2 = "No Change") +
    i(corp, log(lag(sales_tax_rate_sales)), ref = "Corp") +
    i(corp_tau1_year_noex, "Base") | sic_3,
  cluster = ~ plant + year,
  data = wip_df_tau1_revised
)

did_corp_tau1_ctrl_continuous |> etable(dict = dict)

## %% Plot: Revised specification with No Change as base ---------------------

png(
  file = "Code/Products/921.5-corp-tau1-revised.png",
  width = 820, height = 480
)

# tau_1 Increase
ref_vec_inc_rev <- did_corp_tau1_ctrl_tau0 |> coef() |> names() |>
  grep("corp_tau1_year_noex::Other:Increase:82$", x = _, value = FALSE)
ref_vec_inc_rev <- ref_vec_inc_rev + 1
names(ref_vec_inc_rev) <- "corp_tau1_year_noex::Other:Increase:83"

coefplot(
  did_corp_tau1_ctrl_tau0,
  keep = "corp_tau1_year_noex::Other:Increase:\\d{2}$",
  dict = dict,
  group = list(`Increase` = "^^corp_tau1_year_noex::Other:Increase:"),
  ylim = c(-0.08, 0.13),
  col = 3,
  ref = ref_vec_inc_rev,
  main = "Sales Tax Effect on Input Overreporting (vs. No Change Industries)\nNon-Corporations, Controlling for tau_0",
  grid = FALSE
)

# tau_1 Decrease
ref_vec_dec_rev <- did_corp_tau1_ctrl_tau0 |> coef() |> names() |>
  grep("corp_tau1_year_noex::Other:Decrease:82$", x = _, value = FALSE)
ref_vec_dec_rev <- ref_vec_dec_rev + 1
names(ref_vec_dec_rev) <- "corp_tau1_year_noex::Other:Decrease:83"

coefplot(
  did_corp_tau1_ctrl_tau0,
  keep = "corp_tau1_year_noex::Other:Decrease:\\d{2}$",
  dict = dict,
  group = list(`Decrease` = "^^corp_tau1_year_noex::Other:Decrease:"),
  add = TRUE,
  col = 2,
  x.shift = 0.2,
  ref = ref_vec_dec_rev,
  grid = FALSE
)

legend(
  "topleft",
  legend = c("tau_1 Increase", "tau_1 Decrease"),
  col = c(3, 2),
  lwd = 2,
  pch = 16
)

abline(v = c(3, 7), lty = 2, col = "gray")
abline(h = 0, lty = 1, col = "black")

dev.off()

## %% Save revised results ---------------------

save(
  wip_df_tau1,
  wip_df_tau1_revised,
  tau_1_groups,
  tau_1_groups_revised,
  tau_0_groups,
  did_corp_tau1,
  did_corp_tau1_het,
  did_corp_tau1_ctrl_tau0,
  did_corp_tau1_ctrl_tau0_interact,
  did_corp_tau1_ctrl_continuous,
  file = "Code/Products/921.5-DD2.RData"
)
