library(tidyverse)
# Load data, global vars, and functions ---------
load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
load("Code/Products/functions.RData")

# Combinations ----------------------------------

combinations <- data.frame(
    Size = size,
    Probs = probs
)

# Annual quantile --------------------------------
## Use lapply with CD PF -------------------------

beta_diff_yoy_list <- lapply(seq_len(nrow(combinations)), function(i) {
    with(
        combinations[i, ],
        get_beta_diff_yoy(
            o = "sales",
            s = Size,
            p = Probs,
            data = colombia_data_frame
        )
    )
})

## Use lapply with NonLinear PF -------------------------

beta_diff_poly_yoy_list <- lapply(seq_len(nrow(combinations)), function(i) {
    with(
        combinations[i, ],
        get_beta_diff_poly_yoy(
            o = "sales",
            s = Size,
            p = Probs,
            data = colombia_data_frame
        )
    )
})

## Collecting results in DF ---------------------
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

# save(results, file="")
plot_lollypop(
    save_list = list(
        fig_dir = fig_dir,
        ex_name = ex_name,
        local_data = "yoy_pick",
        plot_suffix = "png"
    )
)
# Overall quantile -------------------------------

rm(results, results_df_list, export_list)

## Use lapply with CD PF -------------------------

beta_diff_list <- lapply(seq_len(nrow(combinations)), function(i) {
    with(
        combinations[i, ],
        get_beta_diff(
            o = "sales",
            s = Size,
            p = Probs,
            data = colombia_data_frame
        )
    )
})

## Use lapply with NonLinear PF -------------------------

beta_diff_poly_list <- lapply(seq_len(nrow(combinations)), function(i) {
    with(
        combinations[i, ],
        get_beta_diff_poly(
            o = "sales",
            s = Size,
            p = Probs,
            data = colombia_data_frame
        )
    )
})

## Collecting results in DF ---------------------
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

## Plotting results ----------------------------
plot_lollypop(
    plot_title = "All firms, industry FE, overall quantile",
    save_list = list(
        fig_dir = fig_dir,
        ex_name = ex_name,
        local_data = "overall_pick",
        plot_suffix = "png"
    )
)
