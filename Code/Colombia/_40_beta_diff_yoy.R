library(tidyverse)
# Load data, global vars, and functions ---------
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/functions.RData")

# Then use expand.grid to get combinations ------

combinations <- expand.grid(
    size,
    c(probs_down, probs_up),
    stringsAsFactors = FALSE
)

# Use lapply with CD PF -------------------------

beta_diff_yoy_list <- lapply(seq_len(nrow(combinations)), function(i) {
    with(
        combinations[i, ],
        get_beta_diff_yoy(
            o = "sales",
            s = Var1,
            p = Var2,
            data = colombia_data_frame
        )
    )
})

# Use lapply with NonLinear PF -------------------------

beta_diff_poly_yoy_list <- lapply(seq_len(nrow(combinations)), function(i) {
    with(
        combinations[i, ],
        get_beta_diff_poly_yoy(
            o = "sales",
            s = Var1,
            p = Var2,
            data = colombia_data_frame
        )
    )
})

# Collecting results in DF ---------------------
export_list <- list(
    CD_yoy = beta_diff_yoy_list,
    NL_yoy = beta_diff_poly_yoy_list
)

results_df_list<-lapply(
    export_list,
    collect_in_df
)

results_df_list[["CD_yoy"]][["PF"]]<- "CD"
results_df_list[["NL_yoy"]][["PF"]]<- "NL"

results <- do.call(rbind, results_df_list)

plot_my_lollypop()
