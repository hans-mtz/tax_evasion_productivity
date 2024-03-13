col_df %>%
    mutate(
        ind_exp =c1+c2+c3+c4+c5+c6,# na.rm=TRUE)+
        gen_exp = c8+c9+c10+c11+c12+c13+c14+c15+c16,# na.rm=TRUE),
        ind_exp_bool = ind_exp==c7,
        gen_exp_bool = gen_exp==c17
    ) %>%
    filter(
        # gen_exp_bool == FALSE,
        ind_exp_bool == FALSE
    ) %>%
    select(
        plant, datayear, sic, c1:c7, ind_exp, c8:c17,gen_exp
    ) %>%
    View()
    summarise(
        ind_bool = sum(ind_exp_bool, na.rm=TRUE),
        gen_bool = sum(gen_exp_bool, na.rm=TRUE),
        n_ind = sum(!is.na(c7)),
        n_gen = sum(!is.na(c17)),
        ind_exp_sum = mean(ind_exp, na.rm=TRUE),
        ind_c7_sum = mean(c7, na.rm = TRUE),
        gen_exp_sum = mean(gen_exp, na.rm=TRUE),
        gen_c17 = mean(c17, na.rm=TRUE)
    )

## Do corps have different technology

# Shares of non-deductibles or no incentives
# Capital, it cannot be deducted from sales taxes
# Most services cannot be deducted: only the following
#

## 