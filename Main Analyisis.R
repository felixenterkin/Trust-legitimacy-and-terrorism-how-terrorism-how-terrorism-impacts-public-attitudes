################################################################################
# Main Analysis 
################################################################################


all_packages <-
  c(
    "dplyr",
    "ggplot2",
    "psych",
    "cowplot", 
    "readr",
    "summarytools", 
    "modelsummary",
    "vtable",
    "broom",
    "flextable",
    "tidyverse",
    "moments"
  )

lapply(all_packages, library, character.only = TRUE)


################################################################################
#### Baseline estimates 
################################################################################

# Regression Tables with and without covariates

Base_models_ESSSE <- list(
  "Trust" = lm(Trust ~ Treatment, data = ESSSE),
  "Trust" = lm(Trust ~ Treatment
               + Age + Gender + Education + HouseholdIncome + EthnicMinority, data = ESSSE),
  "Obligation" = lm(Obligation ~ Treatment, data = ESSSE),
  "Obligation" = lm(Obligation ~ Treatment
                    + Age + Gender + Education + HouseholdIncome + EthnicMinority, data = ESSSE),
  "Moral Alignment" = lm(MoralAlignment ~ Treatment, data = ESSSE),
  "Moral Alignment" = lm(MoralAlignment ~ Treatment
                         + Age + Gender + Education + HouseholdIncome + EthnicMinority, data = ESSSE),
  "Lawfulness" = lm(Lawfulness ~ Treatment, data = ESSSE),  
  "Lawfulness" = lm(Lawfulness ~ Treatment
                    + Age + Gender + Education + HouseholdIncome + EthnicMinority, data = ESSSE),
  "Procedural Fairness" = lm(ProceduralFairness ~ Treatment, data = ESSSE),
  "Procedural Fairness" = lm(ProceduralFairness ~ Treatment
                             + Age + Gender + Education + HouseholdIncome + EthnicMinority, data = ESSSE),
  "Police Effectiveness" = lm(Effectiveness ~ Treatment, data = ESSSE),
  "Police Effectiveness" = lm(Effectiveness ~ Treatment
                              + Age + Gender + Education + HouseholdIncome + EthnicMinority, data = ESSSE)
)
modelsummary(
  Base_models_ESSSE,
  gof_omit = 'BIC|AIC|Log.Lik|RMSE|F',
  stars = TRUE,
  title = 'Baseline Estimates',
  notes = 'Entries are regression coefficients, with standard errors in parenthesis')



Base_models_ESSRU <- list(
  "Trust" = lm(Trust ~ Treatment, data = ESSRU),
  "Trust" = lm(Trust ~ Treatment
               + Age + Gender + Education + HouseholdIncome + EthnicMinority, data = ESSRU),
  "Obligation" = lm(Obligation ~ Treatment, data = ESSRU),
  "Obligation" = lm(Obligation ~ Treatment
                    + Age + Gender + Education + HouseholdIncome + EthnicMinority, data = ESSRU),
  "Moral Alignment" = lm(MoralAlignment ~ Treatment, data = ESSRU),
  "Moral Alignment" = lm(MoralAlignment ~ Treatment
                         + Age + Gender + Education + HouseholdIncome + EthnicMinority, data = ESSRU),
  "Lawfulness" = lm(Lawfulness ~ Treatment, data = ESSRU),  
  "Lawfulness" = lm(Lawfulness ~ Treatment
                    + Age + Gender + Education + HouseholdIncome + EthnicMinority, data = ESSRU),
  "Procedural Fairness" = lm(ProceduralFairness ~ Treatment, data = ESSRU),
  "Procedural Fairness" = lm(ProceduralFairness ~ Treatment
                             + Age + Gender + Education + HouseholdIncome + EthnicMinority, data = ESSRU),
  "Police Effectiveness" = lm(Effectiveness ~ Treatment, data = ESSRU),
  "Police Effectiveness" = lm(Effectiveness ~ Treatment
                              + Age + Gender + Education + HouseholdIncome + EthnicMinority, data = ESSRU)
)
modelsummary(
  Base_models_ESSRU,
  gof_omit = 'BIC|AIC|Log.Lik|RMSE|F',
  stars = TRUE,
  title = 'Baseline Estimates',
  notes = 'Entries are regression coefficients, with standard errors in parenthesis')



################################################################################
## Forest Plot: ATE by Bandwidth
################################################################################

# Estimates for Sweden

Estimates_ESSSE <- expand_grid(variable = c("Trust", "Obligation", "MoralAlignment", "Lawfulness", "ProceduralFairness", "Effectiveness"),
                               bandwidth = c(7,14,21,28,35,42,49,56)) |>
  mutate(models =
           map2(variable, bandwidth,
                ~ lm(reformulate("Treatment", response = .x), 
                     data = ESSSE |> filter(Bandwith <= .y)) |>
                  tidy(conf.int = TRUE)
           )) |>
  unnest(models) |>
  filter(term == "Treatment")

# Estimates for Russia

Estimates_ESSRU <- expand_grid(variable = c("Trust", "Obligation", "MoralAlignment", "Lawfulness", "ProceduralFairness", "Effectiveness"),
                               bandwidth = c(7,14,21,28,35,42,49,56)) |>
  mutate(models =
           map2(variable, bandwidth,
                ~ lm(reformulate("Treatment", response = .x), 
                     data = ESSRU |> filter(Bandwith <= .y)) |>
                  tidy(conf.int = TRUE)
           )) |>
  unnest(models) |>
  filter(term == "Treatment")

Estimates_ESSSE$Event <- "Stocholm Bombing 2010"
Estimates_ESSRU$Event <- "Moscow Domodedovo Airport Bombing 2011"


# combining estimates

Combined_Data <- rbind(
  Estimates_ESSRU |>
    mutate(Event = "Moscow Domodedovo Airport Bombing 2011") |>
    select(Event, variable, bandwidth, estimate, conf.low, conf.high) |>
    mutate(variable = factor(variable, levels = c("Trust", "Obligation", "MoralAlignment", "Lawfulness", "ProceduralFairness", "Effectiveness"))),
  
  Estimates_ESSSE |>
    mutate(Event = "Stocholm Bombing 2010") |>
    select(Event, variable, bandwidth, estimate, conf.low, conf.high) |>
    mutate(variable = factor(variable, levels = c("Trust", "Obligation", "MoralAlignment", "Lawfulness", "ProceduralFairness", "Effectiveness")))
)

# ploting graphic

forest_graphic <- ggplot(Combined_Data, aes(x = variable, y = estimate,
                                            group = bandwidth, label = as.character(bandwidth))) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high, color = as.factor(bandwidth)),
    position = position_dodge(width = 0.75),
    size = 0.45,
    shape = 18
  ) +
  labs(y = "Effect Size (Unstandardized β )",
       x = NULL,
       subtitle = "Average Treatment Effect on Attitudes Towards The Police") +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_x_discrete(
    labels = c("Trust", "Obligation", "Moral Alignment", "Lawfulness", "Procedural Fairness", "Effectiveness")
  ) +
  scale_color_discrete(name = "Bandwidth (Days)")

Forest_Graphic <- forest_graphic + facet_grid(~Event, scales = "free_x", space = "free_x")

print(Forest_Graphic)

ggsave("./Analysis/Figures/Forest_graphic.png")


################################################################################
### REGRESSION TABLES
################################################################################


#### 7 Days

# sweden 
days_7_models_ESSSE <- list(
  "Trust" = lm(Trust ~ Treatment, data = ESSSE |> filter(Bandwith <= 7)),
  "Obligation" = lm(Obligation ~ Treatment, data = ESSSE |> filter(Bandwith <= 7)),
  "Moral Alignment" = lm(MoralAlignment ~ Treatment, data = ESSSE |> filter(Bandwith <= 7)),
  "Lawfulness" = lm(Lawfulness ~ Treatment, data = ESSSE |> filter(Bandwith <= 7)),
  "Procedural Fairness" = lm(ProceduralFairness ~ Treatment, data = ESSSE |> filter(Bandwith <= 7)),
  "Police Effectiveness" = lm(Effectiveness ~ Treatment, data = ESSSE |> filter(Bandwith <= 7))
)
modelsummary(
  days_7_models_ESSSE,
  gof_omit = 'BIC|AIC|Log.Lik|RMSE|F',
  stars = TRUE,
  title = '7 day Estimates ESSSE',
  notes = 'Entries are regression coefficients, with standard errors in parenthesis')

# Russia 

days_7_models_ESSRU <- list(
  "Trust" = lm(Trust ~ Treatment, data = ESSRU |> filter(Bandwith <= 7)),
  "Obligation" = lm(Obligation ~ Treatment, data = ESSRU |> filter(Bandwith <= 7)),
  "Moral Alignment" = lm(MoralAlignment ~ Treatment, data = ESSRU |> filter(Bandwith <= 7)),
  "Lawfulness" = lm(Lawfulness ~ Treatment, data = ESSRU |> filter(Bandwith <= 7)),
  "Procedural Fairness" = lm(ProceduralFairness ~ Treatment, data = ESSRU |> filter(Bandwith <= 7)),
  "Police Effectiveness" = lm(Effectiveness ~ Treatment, data = ESSRU |> filter(Bandwith <= 7))
)
modelsummary(
  days_7_models_ESSRU,
  gof_omit = 'BIC|AIC|Log.Lik|RMSE|F',
  stars = TRUE,
  title = '7 day Estimates ESSRU',
  notes = 'Entries are regression coefficients, with standard errors in parenthesis')


#### 14 Days

# sweden 
days_14_models_ESSSE <- list(
  "Trust" = lm(Trust ~ Treatment, data = ESSSE |> filter(Bandwith <= 14)),
  "Obligation" = lm(Obligation ~ Treatment, data = ESSSE |> filter(Bandwith <= 14)),
  "Moral Alignment" = lm(MoralAlignment ~ Treatment, data = ESSSE |> filter(Bandwith <= 14)),
  "Lawfulness" = lm(Lawfulness ~ Treatment, data = ESSSE |> filter(Bandwith <= 14)),
  "Procedural Fairness" = lm(ProceduralFairness ~ Treatment, data = ESSSE |> filter(Bandwith <= 14)),
  "Police Effectiveness" = lm(Effectiveness ~ Treatment, data = ESSSE |> filter(Bandwith <= 14))
)
modelsummary(
  days_14_models_ESSSE,
  gof_omit = 'BIC|AIC|Log.Lik|RMSE|F',
  stars = TRUE,
  title = '14 day Estimates ESSSE',
  notes = 'Entries are regression coefficients, with standard errors in parenthesis')

# Russia 

days_14_models_ESSRU <- list(
  "Trust" = lm(Trust ~ Treatment, data = ESSRU |> filter(Bandwith <= 14)),
  "Obligation" = lm(Obligation ~ Treatment, data = ESSRU |> filter(Bandwith <= 14)),
  "Moral Alignment" = lm(MoralAlignment ~ Treatment, data = ESSRU |> filter(Bandwith <= 14)),
  "Lawfulness" = lm(Lawfulness ~ Treatment, data = ESSRU |> filter(Bandwith <= 14)),
  "Procedural Fairness" = lm(ProceduralFairness ~ Treatment, data = ESSRU |> filter(Bandwith <= 14)),
  "Police Effectiveness" = lm(Effectiveness ~ Treatment, data = ESSRU |> filter(Bandwith <= 14))
)
modelsummary(
  days_14_models_ESSRU,
  gof_omit = 'BIC|AIC|Log.Lik|RMSE|F',
  stars = TRUE,
  title = '14 day Estimates ESSRU',
  notes = 'Entries are regression coefficients, with standard errors in parenthesis')

#### 21 Days

# sweden 
days_21_models_ESSSE <- list(
  "Trust" = lm(Trust ~ Treatment, data = ESSSE |> filter(Bandwith <= 21)),
  "Obligation" = lm(Obligation ~ Treatment, data = ESSSE |> filter(Bandwith <= 21)),
  "Moral Alignment" = lm(MoralAlignment ~ Treatment, data = ESSSE |> filter(Bandwith <= 21)),
  "Lawfulness" = lm(Lawfulness ~ Treatment, data = ESSSE |> filter(Bandwith <= 21)),
  "Procedural Fairness" = lm(ProceduralFairness ~ Treatment, data = ESSSE |> filter(Bandwith <= 21)),
  "Police Effectiveness" = lm(Effectiveness ~ Treatment, data = ESSSE |> filter(Bandwith <= 21))
)
modelsummary(
  days_21_models_ESSSE,
  gof_omit = 'BIC|AIC|Log.Lik|RMSE|F',
  stars = TRUE,
  title = '21 day Estimates ESSSE',
  notes = 'Entries are regression coefficients, with standard errors in parenthesis')

# Russia 

days_21_models_ESSRU <- list(
  "Trust" = lm(Trust ~ Treatment, data = ESSRU |> filter(Bandwith <= 21)),
  "Obligation" = lm(Obligation ~ Treatment, data = ESSRU |> filter(Bandwith <= 21)),
  "Moral Alignment" = lm(MoralAlignment ~ Treatment, data = ESSRU |> filter(Bandwith <= 21)),
  "Lawfulness" = lm(Lawfulness ~ Treatment, data = ESSRU |> filter(Bandwith <= 21)),
  "Procedural Fairness" = lm(ProceduralFairness ~ Treatment, data = ESSRU |> filter(Bandwith <= 21)),
  "Police Effectiveness" = lm(Effectiveness ~ Treatment, data = ESSRU |> filter(Bandwith <= 21))
)
modelsummary(
  days_21_models_ESSRU,
  gof_omit = 'BIC|AIC|Log.Lik|RMSE|F',
  stars = TRUE,
  title = '21 day Estimates ESSRU',
  notes = 'Entries are regression coefficients, with standard errors in parenthesis')

#### 28 Days

# sweden 
days_28_models_ESSSE <- list(
  "Trust" = lm(Trust ~ Treatment, data = ESSSE |> filter(Bandwith <= 28)),
  "Obligation" = lm(Obligation ~ Treatment, data = ESSSE |> filter(Bandwith <= 28)),
  "Moral Alignment" = lm(MoralAlignment ~ Treatment, data = ESSSE |> filter(Bandwith <= 28)),
  "Lawfulness" = lm(Lawfulness ~ Treatment, data = ESSSE |> filter(Bandwith <= 28)),
  "Procedural Fairness" = lm(ProceduralFairness ~ Treatment, data = ESSSE |> filter(Bandwith <= 28)),
  "Police Effectiveness" = lm(Effectiveness ~ Treatment, data = ESSSE |> filter(Bandwith <= 28))
)
modelsummary(
  days_28_models_ESSSE,
  gof_omit = 'BIC|AIC|Log.Lik|RMSE|F',
  stars = TRUE,
  title = '28 day Estimates ESSSE',
  notes = 'Entries are regression coefficients, with standard errors in parenthesis')

# Russia 

days_28_models_ESSRU <- list(
  "Trust" = lm(Trust ~ Treatment, data = ESSRU |> filter(Bandwith <= 28)),
  "Obligation" = lm(Obligation ~ Treatment, data = ESSRU |> filter(Bandwith <= 28)),
  "Moral Alignment" = lm(MoralAlignment ~ Treatment, data = ESSRU |> filter(Bandwith <= 28)),
  "Lawfulness" = lm(Lawfulness ~ Treatment, data = ESSRU |> filter(Bandwith <= 28)),
  "Procedural Fairness" = lm(ProceduralFairness ~ Treatment, data = ESSRU |> filter(Bandwith <= 28)),
  "Police Effectiveness" = lm(Effectiveness ~ Treatment, data = ESSRU |> filter(Bandwith <= 28))
)
modelsummary(
  days_28_models_ESSRU,
  gof_omit = 'BIC|AIC|Log.Lik|RMSE|F',
  stars = TRUE,
  title = '28 day Estimates ESSRU',
  notes = 'Entries are regression coefficients, with standard errors in parenthesis')



