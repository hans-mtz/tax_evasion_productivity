## PRODUCT: Thesis/tables/ch03-corps-by-industry.png := corporations by
## industry, top 20 industries by number of plants (@tbl-corps-by-inds,
## Setting and Data chapter).
## Rebuilt 2026-09-26 from ch3_ind (ch03-industry-stats.R): ch. 3 sample
## (codes 6-9 dropped). Previous version read top_20_inds_table from
## global_vars.RData (see ch03-industry-stats.R).
source("Code/Thesis/001-setup.R")
source("Code/Thesis/ch03-industry-stats.R")

top20 <- ch3_ind %>% arrange(desc(plants)) %>% slice_head(n = 20)
print(top20 %>% select(sic_3, name, plants, corp_plants, corp_plant_pct, corp_rev_pct), n = Inf)

tbl_obj <- top20 %>%
    transmute(
        Industry = paste(sic_3, name),
        Plants = format(plants, big.mark = ","),
        Corporations = format(corp_plants, big.mark = ","),
        `Corporations (\\% of plants)` = sprintf("%.1f", corp_plant_pct),
        `Corporations (\\% of revenue)` = sprintf("%.1f", corp_rev_pct)
    ) %>%
    tt(width = c(3.5, 1, 1.2, 1.4, 1.4),
       notes = "A plant is counted as a corporation if it is one in any year. Revenue: real sales summed over 1981--1991.") %>%
    style_tt(i = "notes", fontsize = 0.8)

render_thesis_table(tbl_obj, "ch03-corps-by-industry")
cat("Saved: Thesis/tables/ch03-corps-by-industry.{png,pdf}\n")
