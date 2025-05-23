## Expenses table ------

exps_tbl<-tribble(
    ~Expenditure, ~Code, ~Services, ~Industrial, ~Deductible,
    "Purchases of accessories and replacement parts of les than one year duration", "c1"," ", " "," ",  
    "Purchases of fuels and lubricants consumed by the establishment", "c2"," ", " ", "$+$",
    "Payments for industrial work by other establishments", "c3"," ", " ", " ",
    "Payment ot domestic workers", "c4"," ", " ", " ",
    "Payments ot third parties for repairs and maintenance", "c5"," ", " ", "$+$",
    "Purchases of raw materials and goods sold without transformation", "c6"," ", " ", "$+$",
    "TOTAL Industrial Expenditures c1:c6", "c7",  " ","$+$", " ",
    "Rent of fixed property", "c8"," ", " ", " ",
    "Payments for professional services", "c9"," ", " "," ", 
    "Machinery rental", "c10", "$-$", " ", "$+$",
    "Insurance, excl. employe benefits", "c11"," ", " ", "$+$",
    "Water, mail, telephone, etc.", "c12"," ", " ", "$+$",
    "Publicity and advertising", "c13"," ", " ", " ",
    "Interest payments", "c14", "$-$"," ", " ", 
    "Royalty payments", "c15" ," ", " ", " ",
    "Other expenditures", "c16"," ", " ", " ",
    "TOTAL General Expenditures c8:c16", "c17", "$+$"," ", " ",
    "TOTAL Expenditure c7+c17", " ", " ", " ", " ",
)


labels <- c(
    # 'capital' = 'Capital',
    # 'mats_serv' = 'Materials & Services',
    # 'skilled_labor' = 'Labor (Skilled)',
    # 'unskilled_labor' = 'Labor (Unskilled)',
    # 'sales' = 'Revenue',
    # 'sales_taxes' = 'Sales Taxes',
    # 'JO_class' = 'J. Org.',
    # 'skilled_wage_bill_share'
    'intermediates_share' = 'Intermediates',
    'mats_serv_share' = 'Materials + Services',
    'mats_deduct_share' = 'Materials + Deductibles',
    'materials_share' = 'Materials',
    'capital_share' = 'Capital',
    'total_expenses_share' = 'Total Expenditure',
    'services_exp_share' = 'Services',
    'industrial_exp_share' = 'Industrial',
    'deductible_exp_share' = 'Deductible'
)

tbl_data<-colombia_data_frame %>%
    ungroup() %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    filter(
        sales > 0 ,
        capital > 0,
        unskilled_labor >0,
        skilled_labor >0,
        total_expenditure >0,
        JO_class != "Other",
        juridical_organization!= 6
        # share_sales_tax <1
    ) %>%
    mutate(
        juridical_organization = factor(juridical_organization),
        sic_3 = factor(sic_3),
        metro_area_code = factor(metro_area_code),
        JO_class = factor(JO_class, levels = c("Proprietorship", "Ltd. Co.", "Corporation", "Partnership")),
        intermediates_share = intermediate_inputs/sales,
        mats_serv_share = mats_serv/sales,
        mats_deduct_share = rowSums(cbind(materials,deductible_expenses), na.rm = TRUE)/sales,
        materials_share = materials/sales,
        capital_share = capital/sales,
        total_expenses_share = total_expenditure/sales,
        services_exp_share = services/total_expenditure,
        industrial_exp_share = industrial_expenditure/total_expenditure,
        deductible_exp_share = deductible_expenses/total_expenditure,
    ) %>%
    select(
        # capital,
        # mats_serv,
        # skilled_labor,
        # unskilled_labor,
        # sales,
        # sales_taxes,
        # age,
        # consumed_energy,
        # deductible_expenses,
        # year,
        `J. Org.`= JO_class,
        share_sales_tax,
        # sic_3,
        # metro_area_code
        ends_with("share") & !starts_with("log") #& !contains("bill")
    )

for (var in names(labels)) {
  tbl_data[[var]] <- haven::labelled(
    tbl_data[[var]], label = labels[var]
    )
}

datasummary_skim(
    tbl_data,
    fun_numeric = list(
        Missing = PercentMissing, 
        Mean = Mean, 
        SD = SD, 
        Q1 = P25,
        Median = Median,
        Q3 = P75
    ),
    fmt = 3,
    # output = 'kableExtra',
    # table.attr = 'data-quarto-disable-processing="true"'
    ) |> 
    group_tt(
        i = list(
            "Share of Revenues" = 1,
            "Share of Total Expenditure" = 10
        )
    ) |>
    style_tt(
        i = c(1,11,15),
        bold = TRUE,
        line = "b"
    ) |>
    style_tt(
        i = 12,
        line = "b",
        line_color = "white"
    )


## GNR Productivity estimates


gnr<-read.csv(
    '/Volumes/SSD Hans 1/Github/gnr/Data/productivity.out',
    header = FALSE
)
names(gnr)<-c("productivity","flex_elasticity")
colombia_311<-read.table(
    '/Volumes/SSD Hans 1/Github/gnr/Data/colombia_R.raw',
    header = FALSE
)
names_colombia_311<-read.table(
    '/Volumes/SSD Hans 1/Github/gnr/Data/variables_R.raw',
    header = FALSE
)
names(colombia_311)<-names_colombia_311[[1]]
data_311<-cbind(colombia_311,gnr)
data_311 |> head()

data<-left_join(data_311,colombia_data_frame[,c("sic_3","juridical_organization","year","plant")])

data %>%
    ungroup() %>%
    left_join(jo_class, by = join_by( juridical_organization == JO_code)) %>%
    # filter(
    #     JO_class != c("Other","Partnership")
    # ) %>%
    group_by(JO_class,year) %>%
    ggplot(
        aes(
            x = factor(year),
            y = productivity,
            # group = factor(JO_class),
            color = factor(JO_class)#,
            # alpha = 
        )
    ) +
    # geom_vline(
    #     xintercept = c("83","86","90"),
    #     colour = "lightgray", #my_colors[["gray"]],
    #     linetype = "dashed"
    # )+
    stat_summary(
        fun = "mean",
        na.rm = TRUE,
        geom = "point",
        shape = 18,
        size = 4#,
        # color = my_colors[["purple"]]
    ) + # add mean points
    stat_summary(
        fun.data = mean_cl_normal,
        geom = "errorbar",
        width = 0.1,
        # color = my_colors[["purple"]],
        show.legend = FALSE
    ) + # add CI bars
    scale_color_manual(values = c("Ltd. Co."="blue","Corporation"="red","Proprietorship"="purple" ))+
    theme_classic()# +
    # theme(legend.position = 'none')

## GNR in R 
install.packages("devtools")
library(devtools)
devtools::install_github("davidjin0/gnrprod")
library(gnrprod)

# load Colombian plant-level data
data <- colombian

# estimate production function parameters and productivity
gnr_fit <- gnrprod(output = "RGO", fixed = c("L", "K"), flex = "RI",
                   share = "share", id = "id", time = "year", data = data)

gnr_fit$data$productivity

data<-cbind(data,gnr_fit$data$productivity)
data$plant<-data$id
data<-left_join(data,colombia_data_frame[,c("sic_3","juridical_organization","year","plant")])
data |> names()
data |> head()
data$productivity <-gnr_fit$data$productivity
