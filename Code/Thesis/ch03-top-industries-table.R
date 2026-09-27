## PRODUCT: Thesis/tables/ch03-top-industries.png := top 10 industries by
## revenue (@tbl-top-inds-rev, Setting and Data chapter).
## Rebuilt 2026-09-26 from ch3_ind (ch03-industry-stats.R): ch. 3 sample
## (codes 6-9 dropped), shares over all of manufacturing. Previous version
## read top_10_revenue from global_vars.RData (see ch03-industry-stats.R).
## Conventions: no caption= (Quarto's ![...]{#tbl-...} is the caption);
## escaped % in column names; width vector weights the long name column.
source("Code/Thesis/001-setup.R")
source("Code/Thesis/ch03-industry-stats.R")

top10 <- ch3_ind %>%
    arrange(desc(revenue)) %>%
    mutate(cum_rev = cumsum(rev_share), cum_plants = cumsum(plant_share)) %>%
    slice_head(n = 10)
print(top10 %>% select(sic_3, name, plants, rev_share, cum_rev, plant_share, cum_plants), width = Inf)

tbl_obj <- top10 %>%
    transmute(
        Industry = paste(sic_3, name),
        Plants = format(plants, big.mark = ","),
        `Revenue (\\%)` = sprintf("%.1f", rev_share),
        `Cum. revenue (\\%)` = sprintf("%.1f", cum_rev),
        `Plants (\\%)` = sprintf("%.1f", plant_share),
        `Cum. plants (\\%)` = sprintf("%.1f", cum_plants)
    ) %>%
    tt(width = c(3.2, 0.9, 1.1, 1.4, 1.1, 1.4),
       notes = "Revenue: real sales summed over 1981--1991. Shares are of all manufacturing plants and revenue in the sample.") %>%
    style_tt(i = "notes", fontsize = 0.8)

render_thesis_table(tbl_obj, "ch03-top-industries")
cat("Saved: Thesis/tables/ch03-top-industries.{png,pdf}\n")
