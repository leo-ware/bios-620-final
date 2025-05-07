# %%
library(tidyverse)
library(survey)

# %% [markdown]
# # Data ingestion

# %%
data_raw <- read.csv('adult23csv/adult23.csv', stringsAsFactors=FALSE)
head(data_raw)

# %%
data = data_raw %>%
    select(
        AGEP_A, SEX_A, HISPALLP_A, REGION, SMKEV_A, RATCAT_A, EMPLASTWK_A, REPSTRAIN_A,
        WTFA_A, PSTRAT, PPSU
    ) %>%
    rename(
        age = AGEP_A,
        sex = SEX_A,
        race_eth = HISPALLP_A,
        region = REGION,
        ever_smoker = SMKEV_A,
        poverty = RATCAT_A,
        employed = EMPLASTWK_A,
        repstrain = REPSTRAIN_A,

        .weight = WTFA_A,
        .pseudo_stratum = PSTRAT,
        .pseudo_psu = PPSU,
    ) %>%
    mutate(
        age = relevel(factor(case_when(
            age < 18 ~ "<18",
            (age >= 18 & age <= 24) ~ "18-24",
            (age >= 25 & age <= 44) ~ "25-44",
            (age >= 45 & age <= 64) ~ "45-64",
            age >= 65 ~ "65+",
            TRUE ~ NA
        )), ref = "25-44"),
        sex = factor(case_when(
            sex == 1 ~ "Male",
            sex == 2 ~ "Female",
            TRUE ~ NA
        )),
        race_eth = relevel(factor(case_when(
            race_eth == 1 ~ "Hispanic",
            race_eth == 2 ~ "Non-Hispanic White",
            race_eth == 3 ~ "Non-Hispanic Black",
            race_eth == 4 ~ "Non-Hispanic Asian",
            race_eth %in% c(5, 6, 7) ~ "Non-Hispanic Other",
            TRUE ~ NA
        )), ref = "Non-Hispanic White"),
        region = relevel(factor(case_when(
            region == 1 ~ "Northeast",
            region == 2 ~ "Midwest",
            region == 3 ~ "South",
            region == 4 ~ "West",
            TRUE ~ NA
        )), ref = "Northeast"),
        ever_smoker = case_when(
            ever_smoker %in% c(1) ~ TRUE,
            ever_smoker %in% c(2) ~ FALSE,
            TRUE ~ NA
        ),
        poverty = case_when(
            poverty %in% c(1, 2, 3) ~ TRUE,
            poverty %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14) ~ FALSE,
            TRUE ~ NA
        ),
        employed = case_when(
            employed == 1 ~ TRUE,
            employed == 2 ~ FALSE,
            TRUE ~ NA
        ),
        repstrain = case_when(
            repstrain == 1 ~ TRUE,
            repstrain == 2 ~ FALSE,
            TRUE ~ NA
        )
    )
    
data %>% head()

# %%
design = svydesign(
    id = ~.pseudo_psu,
    strata = ~.pseudo_stratum,
    weights = ~.weight,
    data = data,
    nest = TRUE
)

summary(design)

# %%
write.csv(data, "out/data.csv", row.names = FALSE)

# %% [markdown]
# # Data subsetting

# %%
design_employed = subset(
    design,
    employed == TRUE,
    !is.na(repstrain),
    !is.na(sex),
    !is.na(ever_smoker)
)
design_employed$variables$employed = NULL

design_employed

# %%
nrow(data)
nrow(data %>% filter(!is.na(repstrain)))
nrow(data %>% filter(employed == TRUE, !is.na(repstrain)))
nrow(data %>% filter(
    employed == TRUE,
    !is.na(repstrain),
    !is.na(sex),
    !is.na(ever_smoker)
))

modeling_data = data %>% filter(
    employed == TRUE,
    !is.na(repstrain),
    !is.na(sex),
    !is.na(ever_smoker)
)

# %% [markdown]
# # Table 1

# %%
outcome_var = "repstrain"

comp_cols = colnames(design_employed)[
    !startsWith(colnames(design_employed), ".") &
    !(colnames(design_employed) == outcome_var)
]

comp_cols

# %%
# generate a table 1 for a single variable

t1_var = function(vname) {
    weighted = data.frame(
        svytable(
            as.formula(paste0("~", vname, " + ", outcome_var)),
            design_employed,
            na.rm = FALSE,
            round = TRUE
        )) %>%
        mutate(weighted = "weighted")

    unweighted = modeling_data %>%
        group_by(!!sym(outcome_var), !!sym(vname)) %>%
        summarise(Freq = n(), .groups = "drop") %>%
        mutate(
            weighted = "unweighted",
            !!sym(outcome_var) := factor(!!sym(outcome_var)),
            !!sym(vname) := factor(.data[[vname]])
        )
    

    bind_rows(weighted, unweighted) %>%
        mutate(
            variable = vname,
            outcome_var := factor(case_when(
                !!sym(outcome_var) == TRUE ~ "outcome",
                !!sym(outcome_var) == FALSE ~ "no_outcome",
                TRUE ~ NA
            ))
        ) %>%
        rename(value = !!sym(vname), n = Freq) %>%
        select(variable, value, everything(), -!!sym(outcome_var)) %>%
        pivot_wider(names_from = outcome_var, values_from = n) %>%
        mutate(
            Overall = outcome + no_outcome,
        ) %>%
        pivot_longer(
            cols = c(outcome, no_outcome, Overall),
            names_to = outcome_var,
            values_to = "n"
        ) %>%
        pivot_wider(names_from = c(weighted, !!sym(outcome_var)), values_from = n) %>%
        select(variable, value, everything())
}

# demo the function
t1_var("age")

# %%
# generate table 1 for all variables
t1_res = list()
for (vname in comp_cols) {
    t1_res[[vname]] = t1_var(vname)
}

t1_res = bind_rows(t1_res) %>%
    mutate(
        variable = ifelse(!is.na(lag(variable)) & (variable == lag(variable)), "", variable),
        w_pct_outcome = paste0(round(weighted_outcome / weighted_Overall, 4) * 100, "%"),
        w_pct_no_outcome = paste0(round(weighted_no_outcome / weighted_Overall, 4) * 100, "%")
    ) %>%
    select(
        variable, value,
        unweighted_Overall, weighted_Overall,
        unweighted_outcome, weighted_outcome, w_pct_outcome,
        unweighted_no_outcome, weighted_no_outcome, w_pct_no_outcome
        )
t1_res

# %% [markdown]
# Do $\chi^2$ tests for all variables and put the p-values in a table.

# %%
p_res = list()
for (vname in comp_cols) {    
    test = svychisq(
        as.formula(paste0("~ ", outcome_var, " + ", vname)),
        design = design_employed,
        statistic = "Chisq",
        na.rm = TRUE
    )

    p_res[[vname]] = data.frame(
        variable = vname,
        p_value = unname(test$p.value),
        test_name = test$method
    )
}

p_res = bind_rows(p_res) %>%
    mutate(
        p_value = case_when(
            p_value < 0.001 ~ "<0.001*",
            p_value <= 0.05 & p_value >= 0.001 ~ paste0(round(p_value, 3), "*"),
            p_value >= 0.001 ~ as.character(round(p_value, 3))
        )
    )
p_res

# %%
# combine p-values with descriptive statistics to create final table 1
t1_final = t1_res %>%
    left_join(p_res, by = "variable", ) %>%
    replace_na(list(p_value = "", test_name = ""))

t1_final

# %%
write.csv(t1_final, "out/t1_final.csv", row.names = FALSE)

# %% [markdown]
# # Modeling

# %%
m1 = svyglm(repstrain ~ age + sex + race_eth + region + poverty + ever_smoker, design = design_employed, family = "quasibinomial")
summary(m1)

# %%
m1_coef = data.frame(summary(m1)$coefficients) %>%
    mutate(
        `Odds Ratio` = exp(Estimate),
        `p value` = `Pr...t..`,
    ) %>%
    rownames_to_column(var = "Term") %>%
    mutate(
        `Odds Ratio` = round(`Odds Ratio`, 3),
        `p value` = case_when(
            `p value` < 0.001 ~ "<0.001*",
            `p value` <= 0.05 & `p value` >= 0.001 ~ paste0(round(`p value`, 3), "*"),
            `p value` > 0.05 ~ as.character(round(`p value`, 3))
        ),
        ci_low = exp(Estimate - 1.96 * `Std..Error`),
        ci_high = exp(Estimate + 1.96 * `Std..Error`),
        `95% Confidence Interval` = paste0("(", round(ci_low, 3), ", ", round(ci_high, 3), ")")
    ) %>%
    select(Term, `Odds Ratio`, `95% Confidence Interval`, `p value`)

m1_coef

write.csv(m1_coef, "out/m1_coef.csv", row.names = FALSE)

# %%



