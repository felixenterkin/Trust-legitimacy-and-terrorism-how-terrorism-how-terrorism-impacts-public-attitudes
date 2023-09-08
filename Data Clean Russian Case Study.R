################################################################################
# Data Clean Russian Case Study: Domodedovo Airport Bombing
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

ESSR5 <- read.csv("./data/raw_data/ESS-Data-Wizard-subset-round-5.csv")



# selecting only the Russian participants 

ESSRU <- ESSR5 |>  filter(cntry == "RU")

### DEPENDANT VARIABLES 

# removing arbitrary number for refusals and don't know and coding them as NA

# Trust 

ESSRU$Trust <- ifelse(ESSRU$trstplc > 10, NA, ESSRU$trstplc)

# Obligation 

ESSRU$bplcdc <- ifelse(ESSRU$bplcdc > 10, NA, ESSRU$bplcdc)
ESSRU$doplcsy <- ifelse(ESSRU$doplcsy > 10, NA, ESSRU$doplcsy)
ESSRU$dpcstrb <- ifelse(ESSRU$dpcstrb > 10, NA, ESSRU$dpcstrb)

alpha(subset(ESSRU, select = c(bplcdc, doplcsy, dpcstrb)), check.keys =  TRUE)

ESSRU$Obligation <- (ESSRU$bplcdc + ESSRU$doplcsy + ESSRU$dpcstrb)/3

# Moral alignment 

ESSRU$plcrgwr <- ifelse(ESSRU$plcrgwr > 5, NA, ESSRU$plcrgwr)
ESSRU$plcipvl <- ifelse(ESSRU$plcipvl > 5, NA, ESSRU$plcipvl)
ESSRU$gsupplc <- ifelse(ESSRU$gsupplc > 5, NA, ESSRU$gsupplc)

# recording scale 1-5

ESSRU$plcrgwr <- (6 - ESSRU$plcrgwr)
ESSRU$plcipvl <- (6 - ESSRU$plcipvl)
ESSRU$gsupplc <- (6 - ESSRU$gsupplc)

alpha(subset(ESSRU, select = c(plcrgwr, plcipvl, gsupplc)), check.keys =  TRUE)

ESSRU$MoralAlignment <- (ESSRU$plcrgwr + ESSRU$plcipvl + ESSRU$gsupplc)/3  

# Lawfulness 

ESSRU$plccbrb <- ifelse(ESSRU$plccbrb > 10, NA, ESSRU$plccbrb)

# recoding scale 0-10 

ESSRU$plccbrb <- (10 - ESSRU$plccbrb)

ESSRU$Lawfulness <- ESSRU$plccbrb

# Procedural Fairness

ESSRU$plcrspc <- ifelse(ESSRU$plcrspc > 6, NA, ESSRU$plcrspc)
ESSRU$plcfrdc <- ifelse(ESSRU$plcfrdc > 5, NA, ESSRU$plcfrdc)
ESSRU$plcexdc <- ifelse(ESSRU$plcexdc > 5, NA, ESSRU$plcexdc)

alpha(subset(ESSRU, select = c(plcrspc, plcfrdc, plcexdc)), check.keys = TRUE)

ESSRU$ProceduralFairness <- (ESSRU$plcrspc + ESSRU$plcfrdc + ESSRU$plcexdc)/3 

#  Police Effectiveness

ESSRU$plcpvcr <- ifelse(ESSRU$plcpvcr > 10, NA, ESSRU$plcpvcr)

ESSRU$Effectiveness <- ESSRU$plcpvcr

###  SOCIODEMOGRAPHIC COVARIATES 

# Age

ESSRU$Age <- ESSRU$agea

ESSRU$Age <- ifelse(ESSRU$Age == 999, NA, ESSRU$Age)

# Gender 

ESSRU$Gender <- ESSRU$gndr

ESSRU$Gender <- ifelse(ESSRU$Gender == 9, NA, ESSRU$Gender)

ESSRU$Gender[ESSRU$Gender == 1] <- 0
ESSRU$Gender[ESSRU$Gender == 2] <- 1

# Education 

ESSRU$Education <- ESSRU$eduyrs

ESSRU$Education <- ifelse(ESSRU$Education > 70, NA, ESSRU$Education)

# Ethnic minority

ESSRU$EthnicMinority <- ESSRU$blgetmg

ESSRU$EthnicMinority[ESSRU$EthnicMinority == 1] <- 0
ESSRU$EthnicMinority[ESSRU$EthnicMinority == 2] <- 1

ESSRU$EthnicMinority <- ifelse(ESSRU$EthnicMinority > 6, NA, ESSRU$EthnicMinority)

# Household net income

ESSRU$HouseholdIncome <- ESSRU$hinctnta

ESSRU$HouseholdIncome<- ifelse(ESSRU$HouseholdIncome > 70, NA, ESSRU$HouseholdIncome)

ESSRU_with_NA <- ESSRU 

## keeping only complete cases on covariates to hold constant

ESSRU <- ESSRU[complete.cases(
  ESSRU$Trust,
  ESSRU$Obligation,
  ESSRU$MoralAlignment,
  ESSRU$Lawfulness,
  ESSRU$ProceduralFairness,
  ESSRU$Effectiveness,
  ESSRU$Age,
  ESSRU$Gender,
  ESSRU$Education,
  ESSRU$EthnicMinority,
  ESSRU$HouseholdIncome
),]


### BINARY TREATMENT INDICATOR  

ESSRU$interviewstart <- as.Date(paste(ESSRU$inwyys, ESSRU$inwmms, ESSRU$inwdds, sep ="-"))

ESSRU$interviewend <- as.Date(paste(ESSRU$inwyye, ESSRU$inwmme, ESSRU$inwdde, sep ="-"))

# keeping only participants interviewed on same day

ESSRU <- ESSRU |>   filter(interviewstart == interviewend)

ESSRU$Date <- ESSRU$interviewstart

#create endogenous Treatment variable

attack_date_ESSRU <- as.Date("2011-01-24")  

ESSRU$Treatment <- NA

ESSRU$Treatment[ESSRU$Date > as.Date(attack_date_ESSRU)] <- 1
ESSRU$Treatment[ESSRU$Date <= as.Date(attack_date_ESSRU)] <- 0

# Removing those on the day of the attack 

ESSRU <- ESSRU |> filter(Date != attack_date_ESSRU)

#Running variable 

ESSRU$Runner <- as.integer(ESSRU$Date - as.Date(attack_date_ESSRU))

# Bandwidth variable for absolute distance from attack date

ESSRU$Bandwith <- abs(ESSRU$Runner)


################################################################################
# Descriptive statistics 
################################################################################


ESSRU_c<-subset(ESSRU, Date < as.Date(attack_date_ESSRU))

ESSRU_t<-subset(ESSRU, Date > as.Date(attack_date_ESSRU))

namelablesb <- c(
  "Trust in the Police",
  "Obligation to Obey",
  "Moral Alignment with Police",
  "Lawfulness", 
  "Procedural Justice",
  "Police Effectiveness",
  "Age (Years)",
  "Gender",
  "Education (Years)",
  "Household Income (Decile)",
  "Ethnic Minority")

# Pre Attack Group 

pre_attack_summary_ESSRU <- sumtable(ESSRU_c |> 
                                       select(
                                         "Trust",
                                         "Obligation",
                                         "MoralAlignment",
                                         "Lawfulness",
                                         "ProceduralFairness",
                                         "Effectiveness",
                                         "Age",
                                         "Gender",
                                         "Education",
                                         "HouseholdIncome",
                                         "EthnicMinority",
                                       ),
                                     labels = namelablesb,
                                     title='Summary Statistics Pre Attack Group',
                                     summ=c('notNA(x)','mean(x)',  'sd(x)',
                                            'min(x)', 'max(x)'),
                                     summ.names=c('N','Mean','SD', 'Min', 'Max'))
# Post Attack Group 

post_attack_summary_ESSRU <-sumtable(ESSRU_t |> 
                                       select(
                                         "Trust",
                                         "Obligation",
                                         "MoralAlignment",
                                         "Lawfulness",
                                         "ProceduralFairness",
                                         "Effectiveness",
                                         "Age",
                                         "Gender",
                                         "Education",
                                         "HouseholdIncome",
                                         "EthnicMinority",
                                       ),
                                     labels = namelablesb,
                                     title='Summary Statistics Post Attack Group',
                                     summ=c('notNA(x)','mean(x)',  'sd(x)',
                                            'min(x)', 'max(x)'),
                                     summ.names=c('N','Mean','SD', 'Min', 'Max'))