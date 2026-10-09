## PRODUCT: Code/Products/S1009/ := the inputs of the whole pipeline restricted to ONE sample rule (Hans, 2026-10-09;
##   Research-log "One sample rule for every exercise"), so the existing scripts can be rerun UNCHANGED on it
##   (run-1700-newsample.sh points them at this folder). The current products in Code/Products are not touched.
## Rule:
##   (1) firm-years with finite log output, capital, labour and materials; juridical organization codes 6-9 dropped;
##   (2) materials share above 5 percent of gross output, both gross and net of sales taxes (no 75 percent cap in 369);
##   (3) unincorporated firm-years (codes 0, 1, 2, 4, 5) with reported materials M* above one cutoff are dropped in every
##       industry; corporations are untouched. Cutoff = 99.5th percentile of M* among the interior firms (unincorporated,
##       the nine industries where the test rejects, tau_P > 0) of the sample defined by (1)-(2).
## M* = `materials` (real, nom_mats / p_gdp), the stage-2 M_star.
## Writes (Code/Products/S1009/):
##   1700-sample-rule.RData   key (sic_3, plant, year) of kept rows, cutoff, counts
##   1700-sample-rule.csv     counts by group and industry (no firm-level data)
##   931.1-fs-se-het.RData    df restricted to the key (data_all_ls, fs_all_ls unchanged)
##   colombia_data.RData      colombia_data_frame restricted to the key (jo_class, sum_rows unchanged)
##   921-DD2.RData            wip_df restricted to the key (other objects unchanged)
##   deconv_funs.Rdata        unchanged functions, upper_threshold_cut = Inf (global copy and the functions' environment)
## Check: the interior firm-years the cutoff drops are exactly those the current 0.5% trim drops (1532 rules, 369 cap).
suppressPackageStartupMessages(library(tidyverse))
out_dir <- "Code/Products/S1009"; dir.create(out_dir, showWarnings = FALSE)
nine <- c("313", "321", "322", "324", "331", "342", "351", "352", "369")
rid <- function(d) paste(as.character(d$sic_3), as.integer(as.character(d$plant)), as.integer(as.character(d$year)))

e931 <- new.env(); load("Code/Products/931.1-fs-se-het.RData", envir = e931)
base <- e931$df %>% ungroup() %>%
    mutate(sic_3 = as.character(sic_3),
           s_gross = log(nom_mats / nom_gross_output),
           s_net = suppressWarnings(log((nom_mats - sales_tax_rate_purchases * nom_mats) /
                                        (nom_gross_output - sales_tax_rate_sales * nom_sales)))) %>%
    filter(is.finite(y), is.finite(k), is.finite(l), is.finite(m), !juridical_organization %in% 6:9,
           is.finite(s_gross), s_gross > log(0.05), is.finite(s_net), s_net > log(0.05)) %>%
    mutate(corp = juridical_organization == 3,
           interior = !corp & sic_3 %in% nine & is.finite(sales_tax_rate_purchases) & sales_tax_rate_purchases > 0 &
                      is.finite(materials) & materials > 0)
cutoff <- unname(quantile(base$materials[base$interior], 0.995))
kept <- base %>% filter(corp | (is.finite(materials) & materials <= cutoff))
dropped <- base %>% filter(!corp, !(is.finite(materials) & materials <= cutoff))

## check against the current trim: 1532's interior set (369 cap at 0.75 on the net share), top 0.5% by M*
old_int <- base %>% filter(interior, !(sic_3 == "369" & s_net >= log(0.75)))
old_cut <- unname(quantile(old_int$materials, 0.995))
old_trim <- rid(old_int %>% filter(materials > old_cut))
new_trim <- rid(dropped %>% filter(interior))
cat(sprintf("cutoff %.1f (old-rule cutoff %.1f) | interior %d (old rule %d) | interior dropped %d (old trim %d) | identical sets: %s\n",
            cutoff, old_cut, sum(base$interior), nrow(old_int), length(new_trim), length(old_trim),
            setequal(new_trim, old_trim)))
stopifnot(setequal(new_trim, old_trim))
s2 <- new.env(); load("Code/Products/1532-stage2-data-final.RData", envir = s2)   # cross-check the current stage-2 interior count
cat(sprintf("current stage-2 interior before the trim (1532 rules): %d\n",
            with(s2$stage2_final, sum(!corp & evader & sales_tax_rate_purchases > 0 & is.finite(cal_V_c) & M_star > 0))))

counts <- base %>% mutate(group = case_when(corp ~ "corporations", interior ~ "interior",
                                            sic_3 %in% nine ~ "unincorporated, nine industries, tau_P = 0 or missing",
                                            TRUE ~ "unincorporated, other industries"),
                          drop = !rid(.) %in% rid(kept)) %>%
    group_by(group) %>% summarise(n = n(), dropped = sum(drop), .groups = "drop") %>%
    bind_rows(tibble(group = "total", n = nrow(base), dropped = nrow(base) - nrow(kept))) %>%
    mutate(cutoff = cutoff, kept = n - dropped)
print(as.data.frame(counts))
cat(sprintf("unincorporated dropped: %d of %d (%.2f%%)\n", nrow(dropped), sum(!base$corp), 100 * nrow(dropped) / sum(!base$corp)))
by_ind <- dropped %>% count(sic_3, name = "dropped") %>% arrange(desc(dropped))
cat("dropped by industry:", paste0(by_ind$sic_3, ":", by_ind$dropped, collapse = " "), "\n")
write.csv(bind_rows(counts %>% mutate(sic_3 = "all"), by_ind %>% mutate(group = "unincorporated dropped, by industry")),
          file.path(out_dir, "1700-sample-rule.csv"), row.names = FALSE)

key <- rid(kept)
save(key, cutoff, counts, file = file.path(out_dir, "1700-sample-rule.RData"))

## restricted inputs ------------------------------------------------------------------------------------------------
restrict <- function(d) { d <- d %>% ungroup(); d[rid(d) %in% key, ] }
df <- restrict(e931$df); data_all_ls <- e931$data_all_ls; fs_all_ls <- e931$fs_all_ls
cat(sprintf("931.1 df: %d -> %d\n", nrow(e931$df), nrow(df)))
save(df, data_all_ls, fs_all_ls, file = file.path(out_dir, "931.1-fs-se-het.RData"))

ecol <- new.env(); load("Code/Products/colombia_data.RData", envir = ecol)
colombia_data_frame <- restrict(ecol$colombia_data_frame); jo_class <- ecol$jo_class; sum_rows <- ecol$sum_rows
cat(sprintf("colombia_data_frame: %d -> %d\n", nrow(ecol$colombia_data_frame), nrow(colombia_data_frame)))
save(colombia_data_frame, jo_class, sum_rows, file = file.path(out_dir, "colombia_data.RData"))

edd <- new.env(); load("Code/Products/921-DD2.RData", envir = edd)
n0 <- nrow(edd$wip_df); edd$wip_df <- restrict(edd$wip_df)
cat(sprintf("921 wip_df: %d -> %d\n", n0, nrow(edd$wip_df)))
save(list = ls(edd), envir = edd, file = file.path(out_dir, "921-DD2.RData"))

efun <- new.env(); load("Code/Products/deconv_funs.Rdata", envir = efun)
efun$upper_threshold_cut <- Inf
assign("upper_threshold_cut", Inf, envir = environment(efun$first_stage_panel_me))
save(list = ls(efun), envir = efun, file = file.path(out_dir, "deconv_funs.Rdata"))
chk <- new.env(); load(file.path(out_dir, "deconv_funs.Rdata"), envir = chk)
stopifnot(identical(chk$upper_threshold_cut, Inf),
          identical(get("upper_threshold_cut", envir = environment(chk$first_stage_panel_me)), Inf))
cat("Saved:", out_dir, "(1700-sample-rule.{RData,csv}, 931.1-fs-se-het.RData, colombia_data.RData, 921-DD2.RData, deconv_funs.Rdata)\n")
