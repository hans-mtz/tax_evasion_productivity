## Load packages and data ####
library(tidyverse)
library(ggplot2)
library(modeest)
library(fixest)


load("Code/Products/colombia_data.RData")
load("Code/Products/global_vars.RData")
# load("Code/Products/functions.RData")
jo_class <- tibble(
    JO_code = 0:9,
    JO_class = c(
        "Proprietorship",
        "Ltd. Co.",
        "Partnership",
        "Corporation",
        "Partnership",
        "Partnership",
        "Corporation",
        # "Stock Co. (Corp.)",
        "Other",
        "Other",
        "Other"
    )
)


## Do firms change JO in the data? ####
# 2% of firms per year; Across industries over time, 2-3%

changes_inds<-colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1)
    ) %>%
    group_by(sic_3) %>%
    summarise(
        # JO_num_mean = mean(JO_num),
        JO_changes_mean = mean(JO_change, na.rm=TRUE)
    )

changes_year<-colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1)
    ) %>%
    group_by(year) %>%
    summarise(
        # JO_num_mean = mean(JO_num),
        JO_changes_mean = mean(JO_change, na.rm=TRUE)
    )

changes_year_main_inds<-colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1)
    ) %>%
    group_by(sic_3,year) %>%
    summarise(
        JO_change_mean = 100*sum(JO_change, na.rm=TRUE)/n()#mean(JO_change, na.rm=TRUE)
    ) %>%
    filter(sic_3 %in% big_industries) %>%
    xtabs(JO_change_mean~year+sic_3, data=.)

changes_tbl <- rbind(changes_year_main_inds[,1:5], `Industry average`=changes_inds[changes_inds$sic_3 %in% big_industries,][[2]]*100)
changes_tbl<- cbind(changes_tbl, `Annual average`=c(changes_year[[2]]*100,100*mean(changes_year[[2]], na.rm=TRUE)))
changes_tbl[2:12,]
## something something -----
colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1),
        num = 1
    ) %>%
    xtabs(JO_change~sic_3+year, data=.)
    # xtabs(num~sic_3+year, data=.)


colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1),
        num = 1,
        avg_total_owners = mean(total_owners, na.rm=TRUE),
        jo_t = lag(juridical_organization)
    ) %>%
    filter(
        sales > 0,
        capital > 0,
        # jo_t %in% c(0,1,3)
    ) %>%
    xtabs(num~jo_t+juridical_organization, data=.)
# Proprietorships moving into LLCs
# Only LLCs move consistently into Corporations

colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1),
        num = 1,
        owners_bin = case_when(
            # is.na(total_owners) ~ NA,
            # total_owners <= 4 ~ "n<= 4",
            total_owners > 4 & total_owners <= 20  ~ "5 <=n<= 20",
            # total_owners > 10 & total_owners <= 15  ~ "n<= 15",
            # total_owners > 15 & total_owners <= 20  ~ "n<= 20",
            # .default = "n> 20",
            total_owners > 20 ~ "n > 20",
            TRUE ~ as.character(total_owners)
        )
    ) %>%
    filter(
        sales > 0,
        capital > 0
    ) %>%
    xtabs(num~owners_bin+juridical_organization, data=.)
# Number of owners does not seem to be trustworthy
# Proprietorships, most indicate more than one owner
# LLCs, more than half indicate 1 or less, but this cannot be N>=2
# Most corporations show 0 owners
colombia_data_frame %>%
    group_by(plant) %>%
    mutate(
        JO_num = length(unique(juridical_organization)),
        JO_change = ifelse(
            juridical_organization==lag(juridical_organization),
            0,1),
        num = 1,
        avg_total_owners = mean(total_owners, na.rm=TRUE)
    ) %>%
    filter(
        sales > 0,
        capital > 0
    ) %>%
    xtabs(num~juridical_organization, data=.)

colombia_data_frame %>%
    ungroup() %>%
    filter( 
        sales > 0, capital >0,
        juridical_organization %in% c(0,1,3) 
    ) %>%
    ggplot(aes(x=log(capital), group= juridical_organization)) +
    geom_density(aes(fill=juridical_organization, alpha=0.5))

colombia_data_frame %>%
    ungroup() %>%
    filter( 
        sales > 0, capital >0,
        juridical_organization%in%c(0,1,3) 
    ) %>%
    ggplot(aes(x=log(capital), group= juridical_organization)) +
    geom_histogram(aes(fill=juridical_organization, alpha=0.5))

colombia_data_frame %>%
  ungroup() %>%
  filter(sales > 0, capital > 0, juridical_organization %in% c(0, 1, 3)) %>%
  ggplot(aes(x=log(capital), group=juridical_organization)) +
  geom_histogram(aes(fill=factor(juridical_organization), alpha=0.5), binwidth = 0.2, position = "identity") +
  scale_fill_manual(values=c("0"="red", "1"="blue", "3"="green")) + # Define your vivid colors here
#   guides(fill=FALSE, alpha=FALSE) + # Remove legends
#   theme(legend.position="none") + # Ensure legend is removed
#   annotate("text", x = Inf, y = Inf, label = "0: Proprietorships\n1: LLCs\n3: Corporations", 
#            hjust = 1.1, vjust = 1, size = 4, color = "black") +
  labs(fill="Juridical Organization")

colombia_data_frame %>%
  ungroup() %>%
  filter(sales > 0, capital > 0, juridical_organization %in% c(0, 1, 3)) %>%
  mutate(juridical_organization = factor(juridical_organization, 
                                         levels = c(0, 1, 3),
                                         labels = c("Proprietorships", "LLCs", "Corporations"))) %>%
  ggplot(aes(x=log(capital), fill=juridical_organization)) +
  geom_histogram(aes(alpha=0.5), binwidth = 0.2, position = "identity") +
  scale_fill_manual(values=c("Proprietorships"="red", "LLCs"="blue", "Corporations"="green")) + # Define your vivid colors here
  guides(alpha=FALSE) + # Remove alpha legend
  theme(legend.title = element_blank()) + # Optionally, remove the legend title
  labs(fill="Juridical Organization Type") +# Label the legend appropriately
  theme_classic()

colombia_data_frame %>%
  ungroup() %>%
  filter(sales > 0, capital > 0, juridical_organization %in% c(0, 1, 3)) %>%
  mutate(juridical_organization = factor(juridical_organization, 
                                         levels = c(0, 1, 3),
                                         labels = c("Proprietorships", "LLCs", "Corporations"))) %>%
  ggplot(aes(x=log(sales), fill=juridical_organization)) +
  geom_histogram(aes(alpha=0.5), binwidth = 0.2, position = "identity") +
  scale_fill_manual(values=c("Proprietorships"="red", "LLCs"="blue", "Corporations"="green")) + # Define your vivid colors here
  guides(alpha=FALSE) + # Remove alpha legend
  theme(legend.title = element_blank()) + # Optionally, remove the legend title
  labs(fill="Juridical Organization Type") +# Label the legend appropriately
  theme_classic()

colombia_data_frame %>%
    ungroup() %>%
    filter( 
        sales > 0, capital >0,
        juridical_organization == c(0,1,3) 
    ) %>%
    ggplot(aes(x=log_sales, group= juridical_organization)) +
    geom_density(aes(fill=juridical_organization, alpha=0.5))

colombia_data_frame %>%
    ungroup() %>%
    filter( 
        sales > 0, capital >0,
        juridical_organization == c(0,1,3) 
    ) %>%
    ggplot(aes(x=log(capital/sales), group= juridical_organization)) +
    geom_density(aes(fill=juridical_organization, alpha=0.5))


data.frame(
    JO_code = 0:9,
    JO_class = c(
        "Proprietorship",
        "Ltd. Co.",
        "Partnership",
        "Corporation",
        "Partnership",
        "Partnership",
        "Stock Co. (Corp.)",
        "Other",
        "Other",
        "Other"
    )
)

jo_class <- tibble(
    JO_code = 0:9,
    JO_class = c(
        "Proprietorship",
        "Ltd. Co.",
        "Partnership",
        "Corporation",
        "Partnership",
        "Partnership",
        "Corporation",
        # "Stock Co. (Corp.)",
        "Other",
        "Other",
        "Other"
    )
)

## Transition Matrix ####

# Using xtabs
transition_matrix <- colombia_data_frame %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    group_by(plant) %>%
    mutate(
        # JO_num = length(unique(juridical_organization)),
        # JO_change = ifelse(
        #     juridical_organization==lag(juridical_organization),
        #     0,1),
        num = 1,
        # avg_total_owners = mean(total_owners, na.rm=TRUE),
        jo_t = lag(JO_class, order_by = year)
    ) %>%
    filter(
        sales > 0,
        capital > 0,
        # jo_t %in% c(0,1,3)
    ) %>%
    xtabs(num~jo_t+JO_class, data=.)
transition_matrix
# getting percentages instead of totals
transition_matrix %>%
    as_tibble() %>%
    pivot_wider(
        values_from = n,
        names_from = JO_class
    ) %>%
    mutate(
        total = Corporation+`Ltd. Co.`+Other+Partnership+Proprietorship,
        across(
            2:6,
            ~round(.x/total*100, digits = 2)
        )#,
    )

# using tidyverse to get percentages instead of xtabs
# Proprietorships and Partnerships moving into LLCs
# Only LLCs move consistently into Corporations
colombia_data_frame %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    ungroup() %>%
    mutate(
        JO_class = factor(
            JO_class, 
            levels = c("Proprietorship","Partnership","Ltd. Co.","Corporation","Other")
            ),
        num = 1
    ) %>%
    group_by(plant) %>%
    mutate(
        # JO_class = factor(
        #     JO_class, 
        #     levels = c("Proprietorship","Partnership","Ltd. Co.","Corporation","Other")),
        # num = 1,
        # # avg_total_owners = mean(total_owners, na.rm=TRUE),
        jo_t = lag(JO_class),
        # reform_1986 = ifelse(year<=86,0,1)
    ) %>%
    filter(
        sales > 0,
        capital > 0,
        # year > 86
        # jo_t %in% c(0,1,3)
    ) %>%
    group_by(jo_t,JO_class) %>%
    summarise(
        n=n()
    ) %>%
    pivot_wider(
        values_from = n,
        names_from = JO_class,
        values_fill = 0
    ) %>%
    mutate(
        total = Corporation+`Ltd. Co.`+Other+Partnership+Proprietorship,
        across(
            1:5,
            ~round(.x/total*100, digits = 1)
        )#,
    ) %>%
    filter(!is.na(jo_t), jo_t!="Other") %>% 
    select(!Other, ` `=jo_t, Total=total)

## Testing if 1986 reforms changed transition probabilities
## added firms after 1981 do not have information on capital
colombia_data_frame %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    ungroup() %>%
    mutate(
        JO_class = factor(
            JO_class, 
            levels = c("Proprietorship","Partnership","Ltd. Co.","Corporation","Other")
            ),
        num = 1
    ) %>%
    group_by(plant) %>%
    mutate(
        # JO_class = factor(
        #     JO_class, 
        #     levels = c("Proprietorship","Partnership","Ltd. Co.","Corporation","Other")),
        # num = 1,
        # # avg_total_owners = mean(total_owners, na.rm=TRUE),
        jo_t = lag(JO_class),
        reform_1986 = ifelse(year<=88,0,1)
    ) %>%
    filter(
        sales > 0,
        # capital > 0,
        # year > 86
        # jo_t %in% c(0,1,3)
    ) %>%
    group_by(jo_t,JO_class, reform_1986) %>%
    summarise(
        n=n()
    ) %>%
    pivot_wider(
        values_from = n,
        names_from = JO_class,
        values_fill = 0
    ) %>%
    mutate(
        total = Corporation+`Ltd. Co.`+Other+Partnership+Proprietorship,
        across(
            2:6,
            ~round(.x/total*100, digits = 1)
        )#,
    ) %>% as.data.frame()

colombia_data_frame %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    ungroup() %>%
    mutate(
        JO_class = factor(
            JO_class, 
            levels = c("Proprietorship","Partnership","Ltd. Co.","Corporation","Other")
            ),
        num = 1
    ) %>%
    group_by(plant) %>%
    mutate(
        # JO_class = factor(
        #     JO_class, 
        #     levels = c("Proprietorship","Partnership","Ltd. Co.","Corporation","Other")),
        # num = 1,
        # # avg_total_owners = mean(total_owners, na.rm=TRUE),
        jo_t = lag(JO_class),
        # reform_1986 = ifelse(year<=86,0,1)
    ) %>%
    filter(
        # is.na(jo_t),
        # # sales > 0,
        # # capital > 0,
        # year > 86,
        plant == 24308
    ) %>%
    select(
        plant, jo_t, JO_class, year, juridical_organization, capital, sales
    )


## Sankey Network diagram
library(networkD3)

links <- transition_matrix %>%
    as.data.frame() %>%
    select(
        source = jo_t,
        target = JO_class,
        value = Freq
    ) %>%
    filter(value!=0)

links$target <- paste(links$target, " ", sep="")

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())
 
# nodes <- data.frame(
#     name = as.character(jo_class$JO_class %>% unique())
# )

links$IDsource <- match(links$source, nodes$name)-1
links$IDtarget <- match(links$target, nodes$name)-1
 
# Make the Network

ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", 
                     sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)


##

colombia_data_frame %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    feols(
        log_share ~ share_sales_tax + polym(m, k, l, degree = 2, raw = TRUE)+factor(JO_class)*factor(year)| csw0(sic_3,metro_area_code),#factor(sic_3)+factor(metro_area_code)+factor(year),
        data = . , cluster = ~sic_3+year
    ) %>%
    etable(
    )

colombia_data_frame %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    feols(
        log(capital/sales) ~ share_sales_tax + polym(m, k, l, degree = 2, raw = TRUE)+factor(JO_class)*factor(year)| csw0(sic_3,metro_area_code),#factor(sic_3)+factor(metro_area_code)+factor(year),
        data = . , cluster = ~sic_3+year
    ) %>%
    etable(
    )


colombia_data_frame %>% 
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    ungroup() %>%
    mutate(
        # sold_energy = purchased_energy+generated_energy-consumed_energy
        total_energy_sold = sum(sold_energy, na.rm=TRUE),
        total_energy_purchased = sum(purchased_energy, na.rm = TRUE)
    ) %>%
    group_by(JO_class, year) %>% 
    summarise(
        # p_purchased = mean(p_energy_purchased, na.rm = TRUE), 
        p_sold_purchase = mean(p_energy_sold/p_energy_purchased, na.rm = TRUE),
        # mean_energy_sold = mean(sold_energy, na.rm = TRUE),
        # total_energy_sold_JO = sum(sold_energy, na.rm = TRUE),
        percent_energy_sold_JO = sum(sold_energy/total_energy_sold, na.rm = TRUE)*100,
        # mean_energy_purchased_JO = mean(purchased_energy, na.rm = TRUE),
        percent_energy_purchased_JO = sum(sold_energy/total_energy_purchased, na.rm = TRUE)*100,
        generated_energy_perc = mean(generated_energy/consumed_energy, na.rm=TRUE)*100
    ) %>%
    filter(is.na(JO_class)==FALSE) %>%
    View()


##

colombia_data_frame %>%
  left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
  ungroup() %>%
  mutate(
    # juridical_organization = factor(juridical_organization, 
    #                                      levels = c(0, 1, 3),
    #                                      labels = c("Proprietorships", "LLCs", "Corporations")),
    JO_class = factor(
            JO_class,
            levels = levels(factor(JO_class))[c(2,1,5,4,3)]
            # c("Ltd. Co.", "Corporation", "Proprietorship","Partnership", "Other")
        )                              
  ) %>%
  filter(sales > 0, capital > 0, JO_class!= "Other") %>%
  ggplot(aes(x=log(capital), fill=JO_class)) +
  geom_histogram(aes(alpha=0.5), binwidth = 0.2, position = "identity") +
  scale_fill_manual(values=c("Proprietorship"="red", "Ltd. Co."="blue", "Partnership"="orange", "Corporation"="green")) + # Define your vivid colors here
  guides(alpha=FALSE) + # Remove alpha legend
  labs(fill="Juridical Organization Type") +# Label the legend appropriately
  theme_classic() +
  theme(legend.title = element_blank(), legend.position = "top") # Optionally, remove the legend title

colombia_data_frame %>%
  ungroup() %>%
  filter(sales > 0, capital > 0, juridical_organization %in% c(0, 1, 3)) %>%
  mutate(juridical_organization = factor(juridical_organization, 
                                         levels = c(0, 1, 3),
                                         labels = c("Proprietorships", "LLCs", "Corporations"))) %>%
  ggplot(aes(x=log(capital/sales), fill=juridical_organization)) +
  geom_histogram(aes(alpha=0.5), binwidth = 0.2, position = "identity") +
  scale_fill_manual(values=c("Proprietorships"="red", "LLCs"="blue", "Corporations"="green")) + # Define your vivid colors here
  guides(alpha=FALSE) + # Remove alpha legend
  labs(fill="Juridical Organization Type") +# Label the legend appropriately
  theme_classic() +
  theme(legend.title = element_blank(), legend.position = "top") # Optionally, remove the legend title

