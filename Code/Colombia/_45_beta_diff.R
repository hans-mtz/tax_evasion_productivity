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

beta_diff_list <- lapply(seq_len(nrow(combinations)), function(i) {
    with(
        combinations[i, ],
        get_beta_diff(
            o = "sales",
            s = Var1,
            p = Var2,
            data = colombia_data_frame
        )
    )
})

# Use lapply with NonLinear PF -------------------------

beta_diff_poly_list <- lapply(seq_len(nrow(combinations)), function(i) {
    with(
        combinations[i, ],
        get_beta_diff_poly(
            o = "sales",
            s = Var1,
            p = Var2,
            data = colombia_data_frame
        )
    )
})

# Collecting results in DF ---------------------
export_list <- list(
    CD = beta_diff_list,
    NL = beta_diff_poly_list
)

results_df_list<-lapply(
    export_list,
    collect_in_df
)

results_df_list[["CD"]][["PF"]]<- "CD"
results_df_list[["NL"]][["PF"]]<- "NL"

results <- do.call(rbind, results_df_list)

plot_my_lollypop(
    plot_title = "All firms, industry FE, overall quantile",
    save_list = list(
        fig_dir = fig_dir,
        ex_name = ex_name,
        local_data = "overall",
        plot_suffix = "png"
    )
)
