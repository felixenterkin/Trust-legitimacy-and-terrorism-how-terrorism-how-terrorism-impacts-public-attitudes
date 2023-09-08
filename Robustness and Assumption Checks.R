################################################################################
# Robustness and assumption Checks 
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


###############################################################################
### Non-response to Survey
################################################################################

# Histogram's displaying interview frequency across the fieldwork period

#  This shows any potential shift non-response pattern to the survey itself as oppose
#  to to particular survey items. 


# SWEDEN


sweden_attack = data.frame(date=as.Date(attack_date_ESSSE), event="Stockholm Bombing")

histo_ESSSE <- ggplot(ESSSE, aes(x=Date)) +
  theme(text = element_text(
    face = "bold",
    size = 10
  )) +
  theme_classic() +
  geom_histogram(position="identity", bins = 41) +
  geom_vline(data=sweden_attack, mapping=aes(xintercept=date), color="red") +
  theme(axis.title.y = element_text(colour = "white"),
        axis.title.x=element_blank(),
        axis.ticks.y = element_line(colour = "white"),
        text = element_text(size = 6)) +
  labs(Y = "Frequency of interviews per week",)

print(histo_ESSSE)

ggsave("./Analysis/Figures/Histogram_ESSSE.png")


# Russia 

russia_attack = data.frame(date=as.Date(attack_date_ESSRU), event="Stockholm Bombing")

histo_ESSRU <- ggplot(ESSRU, aes(x=Date)) +
  theme(text = element_text(
    face = "bold",
    size = 10
  )) +
  theme_classic() +
  geom_histogram(position="identity", bins = 41) +
  geom_vline(data=russia_attack, mapping=aes(xintercept=date), color="red") +
  theme(axis.title.y = element_text(colour = "white"),
        axis.title.x=element_blank(),
        axis.ticks.y = element_line(colour = "white"),
        text = element_text(size = 6)) +
  labs(Y = "Frequency of interviews per week",)

print(histo_ESSRU)

ggsave("./Analysis/Figures/Histogram_ESSRU.png")



################################################################################
### Non-response to survey items items 
################################################################################

# creating non-response data frame

ESSSE_with_NA$interviewstart <- as.Date(paste(
  ESSSE_with_NA$inwyys, ESSSE_with_NA$inwmms, ESSSE_with_NA$inwdds, sep ="-"))

ESSSE_with_NA$interviewend <- as.Date(paste(
  ESSSE_with_NA$inwyye, ESSSE_with_NA$inwmme, ESSSE_with_NA$inwdde, sep ="-"))

ESSSE_with_NA <- ESSSE_with_NA |>   filter(interviewstart == interviewend)
ESSSE_with_NA$Date <- ESSSE_with_NA$interviewstart

ESSSE_with_NA$Treatment <- NA
ESSSE_with_NA$Treatment[ESSSE_with_NA$Date >= as.Date(attack_date_ESSSE)] <- 1
ESSSE_with_NA$Treatment[ESSSE_with_NA$Date <= as.Date(attack_date_ESSSE)] <- 0

ESSSE_with_NA <- ESSSE_with_NA |> filter(Date != attack_date_ESSSE)
ESSSE_with_NA$Runner <- as.integer(ESSSE_with_NA$Date - as.Date(attack_date_ESSSE))
ESSSE_with_NA$Bandwith <- abs(ESSSE_with_NA$Runner)

# assessing non-responses in control-treatment group
#     assessing the frequency of non-responses in the outcome variables


# Trust
trust_results <- ESSSE_with_NA |>
  group_by(Treatment) |>
  summarize(missing_pct = sum(is.na(Trust))/n())

# Obligation
obligation_results <- ESSSE_with_NA |>
  group_by(Treatment) |>
  summarize(
    bplcdc_missing_pct = sum(is.na(bplcdc))/n(),
    doplcsy_missing_pct = sum(is.na(doplcsy))/n(),
    dpcstrb_missing_pct = sum(is.na(dpcstrb))/n()
  )

# Moral alignment
moral_alignment_results <- ESSSE_with_NA |>
  group_by(Treatment) |>
  summarize(
    plcrgwr_missing_pct = sum(is.na(plcrgwr))/n(),
    plcipvl_missing_pct = sum(is.na(plcipvl))/n(),
    gsupplc_missing_pct = sum(is.na(gsupplc))/n()
  )

# Lawfulness
Lawfulness_results <- ESSSE_with_NA |>
  group_by(Treatment) |>
  summarize(missing_pct = sum(is.na(plciplt))/n())

# Procedural fairness
procedural_fairness_results <- ESSSE_with_NA |>
  group_by(Treatment) |>
  summarize(
    plcrspc_missing_pct = sum(is.na(plcrspc))/n(),
    plcfrdc_missing_pct = sum(is.na(plcfrdc))/n(),
    plcexdc_missing_pct = sum(is.na(plcexdc))/n()
  )

# Police effectiveness
police_effectiveness_results <- ESSSE_with_NA |>
  group_by(Treatment) |>
  summarize(missing_pct = sum(is.na(plcpvcr))/n())

# Combine all the results into a single table
combined_results <- trust_results |>
  full_join(obligation_results, by = "Treatment") |>
  full_join(moral_alignment_results, by = "Treatment") |>
  full_join(Lawfulness_results, by = "Treatment") |>
  full_join(procedural_fairness_results, by = "Treatment") |>
  full_join(police_effectiveness_results, by = "Treatment")

# Round the results to 3 decimal places
rounded_results <- combined_results |>
  mutate(across(everything(), ~ round(., 3)))

# Pivot to long data table
long_table <- rounded_results |>
  pivot_longer(cols = -Treatment, names_to = "Category", values_to = "Missing_Pct") |>
  mutate(Group = ifelse(Treatment == "Treatment", "Treatment", "Control"))

# Spread the table to have separate columns for Treatment 0 and Treatment 1
spread_table <- long_table |>
  spread(key = Treatment, value = Missing_Pct)


NonResponces_ESSSE <- flextable(spread_table)

save_as_docx(NonResponces_ESSSE, path = "./Analysis/Tables/Nonresponce_ESSSE.docx")



# creating non-response data frame

ESSRU_with_NA$interviewstart <- as.Date(paste(
  ESSRU_with_NA$inwyys, ESSRU_with_NA$inwmms, ESSRU_with_NA$inwdds, sep ="-"))

ESSRU_with_NA$interviewend <- as.Date(paste(
  ESSRU_with_NA$inwyye, ESSRU_with_NA$inwmme, ESSRU_with_NA$inwdde, sep ="-"))

ESSRU_with_NA <- ESSRU_with_NA |>   filter(interviewstart == interviewend)
ESSRU_with_NA$Date <- ESSRU_with_NA$interviewstart

ESSRU_with_NA$Treatment <- NA
ESSRU_with_NA$Treatment[ESSRU_with_NA$Date >= as.Date(attack_date_ESSRU)] <- 1
ESSRU_with_NA$Treatment[ESSRU_with_NA$Date <= as.Date(attack_date_ESSRU)] <- 0

ESSRU_with_NA <- ESSRU_with_NA |> filter(Date != attack_date_ESSRU)
ESSRU_with_NA$Runner <- as.integer(ESSRU_with_NA$Date - as.Date(attack_date_ESSRU))
ESSRU_with_NA$Bandwith <- abs(ESSRU_with_NA$Runner)

# assessing non-responses in control-treatment group by
# assessing the frequency of non-responses in the outcome variables


# Trust
trust_results <- ESSRU_with_NA |>
  group_by(Treatment) |>
  summarize(missing_pct = sum(is.na(Trust))/n())

# Obligation
obligation_results <- ESSRU_with_NA |>
  group_by(Treatment) |>
  summarize(
    bplcdc_missing_pct = sum(is.na(bplcdc))/n(),
    doplcsy_missing_pct = sum(is.na(doplcsy))/n(),
    dpcstrb_missing_pct = sum(is.na(dpcstrb))/n()
  )

# Moral alignment
moral_alignment_results <- ESSRU_with_NA |>
  group_by(Treatment) |>
  summarize(
    plcrgwr_missing_pct = sum(is.na(plcrgwr))/n(),
    plcipvl_missing_pct = sum(is.na(plcipvl))/n(),
    gsupplc_missing_pct = sum(is.na(gsupplc))/n()
  )

# Lawfulness
Lawfulness_results <- ESSRU_with_NA |>
  group_by(Treatment) |>
  summarize(missing_pct = sum(is.na(plciplt))/n())

# Procedural fairness
procedural_fairness_results <- ESSRU_with_NA |>
  group_by(Treatment) |>
  summarize(
    plcrspc_missing_pct = sum(is.na(plcrspc))/n(),
    plcfrdc_missing_pct = sum(is.na(plcfrdc))/n(),
    plcexdc_missing_pct = sum(is.na(plcexdc))/n()
  )

# Police effectiveness
police_effectiveness_results <- ESSRU_with_NA |>
  group_by(Treatment) |>
  summarize(missing_pct = sum(is.na(plcpvcr))/n())

# Combine all the results into a single table
combined_results <- trust_results |>
  full_join(obligation_results, by = "Treatment") |>
  full_join(moral_alignment_results, by = "Treatment") |>
  full_join(Lawfulness_results, by = "Treatment") |>
  full_join(procedural_fairness_results, by = "Treatment") |>
  full_join(police_effectiveness_results, by = "Treatment")


rounded_results <- combined_results |>
  mutate(across(everything(), ~ round(., 3)))


long_table <- rounded_results |>
  pivot_longer(cols = -Treatment, names_to = "Category", values_to = "Missing_Pct") |>
  mutate(Group = ifelse(Treatment == "Treatment", "Treatment", "Control"))


spread_table <- long_table |>
  spread(key = Treatment, value = Missing_Pct)


NonResponces_ESSRU <- flextable(spread_table)

save_as_docx(NonResponces_ESSRU, path = "./Analysis/Tables/Nonresponce_ESSRU.docx")


################################################################################
###  imbalance analysis full bandwidth
################################################################################

var_list <-
  c(
    "Age",
    "Gender",
    "Education",
    "EthnicMinority",
    "HouseholdIncome"
  )


tdv_list <- list()

# loop the variable names through a t-test

for(i in var_list){
  tdv <- t.test(as.formula(paste(i,"~ Treatment")), data = ESSSE)
  tdv_list[[i]] <- tdv
}


for(i in var_list){
  print(tdv_list[[i]])
}


## Summary table Sweden

summary_list <- list()

# Loop  the variable names through a t-test
for (i in var_list) {
  tdv <- t.test(as.formula(paste(i, "~ Treatment")), data = ESSSE)
  summary_result <- tibble(
    Variable = i,
    estimate = round(tdv$estimate, 3),
    statistic = round(tdv$statistic, 3),
    p.value = tdv$p.value,
    df = tdv$parameter
  ) |>
    mutate(p.value = ifelse(p.value < 0.001, "<0.001", format.pval(p.value, digits = 3)))
  
  summary_list[[i]] <- summary_result
}

result_df <- do.call(rbind, summary_list)

summary_table <- kable(result_df, format = "html", caption = "Imbalance Tests Sweden") |>
  kable_styling(bootstrap_options = "basic")

print(summary_table)


tdv_list <- list()

#loop through the variable names and run the t-test
for(i in var_list){
  tdv <- t.test(as.formula(paste(i,"~ Treatment")), data = ESSRU)
  tdv_list[[i]] <- tdv
}
#print results
for(i in var_list){
  print(tdv_list[[i]])
}


## Summary table Russia

summary_list <- list()

# Loop through the variable names and run the t-test
for (i in var_list) {
  tdv <- t.test(as.formula(paste(i, "~ Treatment")), data = ESSRU)
  summary_result <- tibble(
    Variable = i,
    estimate = round(tdv$estimate, 3),
    statistic = round(tdv$statistic, 3),
    p.value = tdv$p.value,
    df = tdv$parameter
  ) |>
    mutate(p.value = ifelse(p.value < 0.001, "<0.001", format.pval(p.value, digits = 3)))
  
  summary_list[[i]] <- summary_result
}

result_df <- do.call(rbind, summary_list)

summary_table <- kable(result_df, format = "html", caption = "Imbalance Tests Russia") |>
  kable_styling(bootstrap_options = "basic")

print(summary_table)

################################################################################
## THE Forest Plot With Covariates
################################################################################

Estimates_ESSSE <- expand_grid(variable = c("Trust", "Obligation", "MoralAlignment", "Lawfulness", "ProceduralFairness", "Effectiveness"),
                               bandwidth = c(7,14,21,28,35,42,49,56)) |>
  mutate(models =
           map2(variable, bandwidth,
                ~ lm(reformulate(c("Treatment","Age", "Gender", "Education", "HouseholdIncome", "EthnicMinority"), response = .x), 
                     data = ESSSE |> filter(Bandwith <= .y)) |>
                  tidy(conf.int = TRUE)
           )) |>
  unnest(models) |>
  filter(term == "Treatment")



Estimates_ESSRU <- expand_grid(variable = c("Trust", "Obligation", "MoralAlignment", "Lawfulness", "ProceduralFairness", "Effectiveness"),
                               bandwidth = c(7,14,21,28,35,42,49,56)) |>
  mutate(models =
           map2(variable, bandwidth,
                ~ lm(reformulate(c("Treatment","Age", "Gender", "Education", "HouseholdIncome", "EthnicMinority"), response = .x), 
                     data = ESSRU |> filter(Bandwith <= .y)) |>
                  tidy(conf.int = TRUE)
           )) |>
  unnest(models) |>
  filter(term == "Treatment")

Estimates_ESSSE$Event <- "Stocholm Bombing 2010"
Estimates_ESSRU$Event <- "Moscow Domodedovo Airport Bombing 2011"


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
  scale_color_discrete(name = "Bandwidth Post-Attack (Days)")

Forest_Graphic <- forest_graphic + facet_grid(~Event, scales = "free_x", space = "free_x")

print(Forest_Graphic)
ggsave("./Analysis/Figures/Forest_graphic.png")


################################################################################
##Inspection of preexisting time trends 
################################################################################

### Sweden

#trust

mod <- lm(Trust ~ Date, data = ESSSE)
coef_value <- coef(mod)["Date"]
p_value <- summary(mod)$coefficients["Date", "Pr(>|t|)"]

a <- ggplot(ESSSE |> filter(Treatment == 0), aes(x = Date, y = Trust)) +
  geom_smooth(method = "loess", color = "black", size = 0.5) +
  labs(y = "Trust in the Police") +
  theme_bw() +
  labs(x = bquote("β =" ~ .(round(coef_value, 3)) ~ ", p =" ~ .(round(p_value, 6))))

a


# Obligation 

mod <- lm(Obligation ~ Date, data = ESSSE |> filter(Treatment == 0))
summary(mod)
coef_value <- coef(mod)["Date"]
p_value <- summary(mod)$coefficients["Date", "Pr(>|t|)"]

b <- ggplot(ESSSE |> filter(Treatment == 0), aes(x = Date, y = Obligation, group = Treatment)) +
  geom_smooth(method = "loess", color = "black", size = 0.5) +
  labs(y = "Obligation") +
  theme_bw() +
  labs(x = bquote("β =" ~ .(round(coef_value, 3)) ~ ", p =" ~ .(round(p_value, 6)))) 

b


# Moral alignment 

mod <- lm(MoralAlignment ~ Date, data = ESSSE |> filter(Treatment == 0))
coef_value <- coef(mod)["Date"]
p_value <- summary(mod)$coefficients["Date", "Pr(>|t|)"]

c <- ggplot(ESSSE |> filter(Treatment == 0), aes(x = Date, y = MoralAlignment, group = Treatment)) +
  geom_smooth(method = "loess", color = "black", size = 0.5) +
  labs(y = "Moral Alignment") +
  theme_bw() +
  labs(x = bquote("β =" ~ .(round(coef_value, 3)) ~ ", p =" ~ .(round(p_value, 6)))) 

c


# Lawfulness 
mod <- lm(Lawfulness ~ Date, data = ESSSE |> filter(Treatment == 0))
summary(mod)
coef_value <- coef(mod)["Date"]
p_value <- summary(mod)$coefficients["Date", "Pr(>|t|)"]
d <- ggplot(ESSSE |> filter(Treatment == 0), aes(x = Date, y = Lawfulness, group = Treatment)) +
  geom_smooth(method = "loess", color = "black", size = 0.5) +
  labs(y = "Lawfulness") +
  theme_bw() +
  labs(x = bquote("β =" ~ .(round(coef_value, 3)) ~ ", p =" ~ .(round(p_value, 6)))) 

d


# Procedural Fairness
mod <- lm(ProceduralFairness ~ Date, data = ESSSE |> filter(Treatment == 0))
summary(mod)
coef_value <- coef(mod)["Date"]
p_value <- summary(mod)$coefficients["Date", "Pr(>|t|)"]
e <- ggplot(ESSSE |> filter(Treatment == 0), aes(x = Date, y = ProceduralFairness, group = Treatment)) +
  geom_smooth(method = "loess", color = "black", size = 0.5) +
  labs(y = "Procedural Fairness") +
  theme_bw() +
  labs(x = bquote("β =" ~ .(round(coef_value, 3)) ~ ", p =" ~ .(round(p_value, 6)))) 

e



# Effectiveness
mod <- lm(Effectiveness ~ Date, data = ESSSE |> filter(Treatment == 0))
summary(mod)
coef_value <- coef(mod)["Date"]
p_value <- summary(mod)$coefficients["Date", "Pr(>|t|)"]

f <- ggplot(ESSSE |> filter(Treatment == 0), aes(x = Date, y = Effectiveness, group = Treatment)) +
  geom_smooth(method = "loess", color = "black", size = 0.5) +
  labs(y = "Effectiveness") +
  theme_bw() +
  labs(x = bquote("β =" ~ .(round(coef_value, 3)) ~ ", p =" ~ .(round(p_value, 6)))) 

f




p <- plot_grid(a,
               b,
               c,
               d,
               e,
               f,
               align=c("none"), nrow = 2, ncol = 3,  axis = "l")
p

ggsave("./Analysis/Figures/pre_time_trends_ESSSE.png")


################################################################################
### Inspection of preexisting time trends Russia 
################################################################################

#trust

mod <- lm(Trust ~ Date, data = ESSRU)
coef_value <- coef(mod)["Date"]
p_value <- summary(mod)$coefficients["Date", "Pr(>|t|)"]

a <- ggplot(ESSRU |> filter(Treatment == 0), aes(x = Date, y = Trust)) +
  geom_smooth(method = "loess", color = "black", size = 0.5) +
  labs(y = "Trust in the Police") +
  theme_bw() +
  labs(x = bquote("β =" ~ .(round(coef_value, 3)) ~ ", p =" ~ .(round(p_value, 6))))

a


# Obligation 

mod <- lm(Obligation ~ Date, data = ESSRU |> filter(Treatment == 0))
summary(mod)
coef_value <- coef(mod)["Date"]
p_value <- summary(mod)$coefficients["Date", "Pr(>|t|)"]

b <- ggplot(ESSRU |> filter(Treatment == 0), aes(x = Date, y = Obligation, group = Treatment)) +
  geom_smooth(method = "loess", color = "black", size = 0.5) +
  labs(y = "Obligation") +
  theme_bw() +
  labs(x = bquote("β =" ~ .(round(coef_value, 3)) ~ ", p =" ~ .(round(p_value, 6)))) 

b


# Moral alignment 

mod <- lm(MoralAlignment ~ Date, data = ESSRU |> filter(Treatment == 0))
coef_value <- coef(mod)["Date"]
p_value <- summary(mod)$coefficients["Date", "Pr(>|t|)"]

c <- ggplot(ESSRU |> filter(Treatment == 0), aes(x = Date, y = MoralAlignment, group = Treatment)) +
  geom_smooth(method = "loess", color = "black", size = 0.5) +
  labs(y = "Moral Alignment") +
  theme_bw() +
  labs(x = bquote("β =" ~ .(round(coef_value, 3)) ~ ", p =" ~ .(round(p_value, 6)))) 

c

# Lawfulness 
mod <- lm(Lawfulness ~ Date, data = ESSRU |> filter(Treatment == 0))
summary(mod)
coef_value <- coef(mod)["Date"]
p_value <- summary(mod)$coefficients["Date", "Pr(>|t|)"]
d <- ggplot(ESSRU |> filter(Treatment == 0), aes(x = Date, y = Lawfulness, group = Treatment)) +
  geom_smooth(method = "loess", color = "black", size = 0.5) +
  labs(y = "Lawfulness") +
  theme_bw() +
  labs(x = bquote("β =" ~ .(round(coef_value, 3)) ~ ", p =" ~ .(round(p_value, 6)))) 

d

# Procedural Fairness
mod <- lm(ProceduralFairness ~ Date, data = ESSRU |> filter(Treatment == 0))
summary(mod)
coef_value <- coef(mod)["Date"]
p_value <- summary(mod)$coefficients["Date", "Pr(>|t|)"]
e <- ggplot(ESSRU |> filter(Treatment == 0), aes(x = Date, y = ProceduralFairness, group = Treatment)) +
  geom_smooth(method = "loess", color = "black", size = 0.5) +
  labs(y = "Procedural Fairness") +
  theme_bw() +
  labs(x = bquote("β =" ~ .(round(coef_value, 3)) ~ ", p =" ~ .(round(p_value, 6)))) 

e

# Effectiveness
mod <- lm(Effectiveness ~ Date, data = ESSRU |> filter(Treatment == 0))
summary(mod)
coef_value <- coef(mod)["Date"]
p_value <- summary(mod)$coefficients["Date", "Pr(>|t|)"]

f <- ggplot(ESSRU |> filter(Treatment == 0), aes(x = Date, y = Effectiveness, group = Treatment)) +
  geom_smooth(method = "loess", color = "black", size = 0.5) +
  labs(y = "Effectiveness") +
  theme_bw() +
  labs(x = bquote("β =" ~ .(round(coef_value, 3)) ~ ", p =" ~ .(round(p_value, 6)))) 

f


p <- plot_grid(a,
               b,
               c,
               d,
               e,
               f,
               align=c("none"), nrow = 2, ncol = 3,  axis = "l")
p

ggsave("./Analysis/Figure/pre_time_trends_ESSRU.png")



p <- plot_grid(a,
               d,
               align=c("none"), nrow = 1, ncol = 2,  axis = "l")
p


################################################################################
#### Placebo check 
################################################################################

## Sweden 

median(ESSSE_c$Date)

placebo_date_ESSSE_c <- as.Date(median(ESSSE_c$Date))  

ESSSE_c$Treatment <- NA

ESSSE_c$TreatmentPlacebo[ESSSE_c$Date > as.Date(placebo_date_ESSSE_c)] <- 1
ESSSE_c$TreatmentPlacebo[ESSSE_c$Date < as.Date(placebo_date_ESSSE_c)] <- 0

ESSSE_c <- ESSSE_c |> filter(Date != placebo_date_ESSSE_c)


## Russia 

median(ESSRU_c$Date)

placebo_date_ESSRU_c <- as.Date(median(ESSRU_c$Date))  

ESSRU_c$Treatment <- NA

ESSRU_c$TreatmentPlacebo[ESSRU_c$Date > as.Date(placebo_date_ESSRU_c)] <- 1
ESSRU_c$TreatmentPlacebo[ESSRU_c$Date <= as.Date(placebo_date_ESSRU_c)] <- 0

ESSRU_c <- ESSRU_c |> filter(Date != placebo_date_ESSRU_c)



# Sweden 


placebo_models_ESSSE_c <- list(
  "Trust" = lm(Trust ~ TreatmentPlacebo, data = ESSSE_c),
  "Obligation" = lm(Obligation ~ TreatmentPlacebo, data = ESSSE_c),
  "Moral Alignment" = lm(MoralAlignment ~ TreatmentPlacebo, data = ESSSE_c),
  "Lawfulness" = lm(Lawfulness ~ TreatmentPlacebo, data = ESSSE_c),
  "Procedural Fairness" = lm(ProceduralFairness ~ TreatmentPlacebo, data = ESSSE_c),
  "Police Effectiveness" = lm(Effectiveness ~ TreatmentPlacebo, data = ESSSE_c)
)
modelsummary(
  placebo_models_ESSSE_c,
  gof_omit = 'BIC|AIC|Log.Lik|RMSE|F',
  stars = TRUE,
  title = 'Baseline Estimates ESSSE',
  notes = 'Entries are regression coefficients, with standard errors in parenthesis')

# Russia 

placebo_models_ESSRU_c <- list(
  "Trust" = lm(Trust ~ TreatmentPlacebo, data = ESSRU_c),
  "Obligation" = lm(Obligation ~ TreatmentPlacebo, data = ESSRU_c),
  "Moral Alignment" = lm(MoralAlignment ~ TreatmentPlacebo, data = ESSRU_c),
  "Lawfulness" = lm(Lawfulness ~ TreatmentPlacebo, data = ESSRU_c),
  "Procedural Fairness" = lm(ProceduralFairness ~ TreatmentPlacebo, data = ESSRU_c),
  "Police Effectiveness" = lm(Effectiveness ~ TreatmentPlacebo, data = ESSRU_c)
)
modelsummary(
  placebo_models_ESSRU_c,
  gof_omit = 'BIC|AIC|Log.Lik|RMSE|F',
  stars = TRUE,
  title = 'Baseline Estimates ESSRU',
  notes = 'Entries are regression coefficients, with standard errors in parenthesis')


# Placebo Forest Plots

Estimates_ESSSE_c <- expand_grid(variable = c("Trust", "Obligation", "MoralAlignment", "Lawfulness", "ProceduralFairness", "Effectiveness"),
                                 bandwidth = c(7,14,21,28,35,42,49,56)) |>
  mutate(models =
           map2(variable, bandwidth,
                ~ lm(reformulate("TreatmentPlacebo", response = .x), 
                     data = ESSSE_c |> filter(Bandwith <= .y)) |>
                  tidy(conf.int = TRUE)
           )) |>
  unnest(models) |>
  filter(term == "TreatmentPlacebo")



Estimates_ESSRU_c <- expand_grid(variable = c("Trust", "Obligation", "MoralAlignment", "Lawfulness", "ProceduralFairness", "Effectiveness"),
                                 bandwidth = c(7,14,21,28,35,42,49,56)) |>
  mutate(models =
           map2(variable, bandwidth,
                ~ lm(reformulate("TreatmentPlacebo", response = .x), 
                     data = ESSRU_c |> filter(Bandwith <= .y)) |>
                  tidy(conf.int = TRUE)
           )) |>
  unnest(models) |>
  filter(term == "TreatmentPlacebo")

Estimates_ESSSE_c$Event <- "Stocholm Bombing 2010"
Estimates_ESSRU_c$Event <- "Moscow Domodedovo Airport Bombing 2011"






Combined_Data <- rbind(
  Estimates_ESSRU_c |>
    mutate(Event = "Russian Control Group") |>
    select(Event, variable, bandwidth, estimate, conf.low, conf.high) |>
    mutate(variable = factor(variable, levels = c("Trust", "Obligation", "MoralAlignment", "Lawfulness", "ProceduralFairness", "Effectiveness"))),
  
  Estimates_ESSSE_c |>
    mutate(Event = "Swedish Control Group") |>
    select(Event, variable, bandwidth, estimate, conf.low, conf.high) |>
    mutate(variable = factor(variable, levels = c("Trust", "Obligation", "MoralAlignment", "Lawfulness", "ProceduralFairness", "Effectiveness")))
)



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
       subtitle = "Placebo Treatment Effect") +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_x_discrete(
    labels = c("Trust", "Obligation", "Moral Alignment", "Lawfulness", "Procedural Fairness", "Effectiveness")
  ) +
  scale_color_discrete(name = "Bandwidth (Days)")

Forest_Graphic_placebo <- forest_graphic + facet_grid(~Event, scales = "free_x", space = "free_x")

print(Forest_Graphic_placebo)        






