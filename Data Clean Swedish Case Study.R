################################################################################
# Data Clean Swedish Case Study: Stockholm Bombing 2010
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

# filtering just the Swedish participants 

ESSSE <- ESSR5 |>  filter(cntry == "SE")

### DEPENDANT VARIABLES 

# Removing arbitrary numbers for refusals and Don't know responses (e.g., 66,77)
# and coding them as NA so they can be removed from the data set.
# As well as creating composite scores for multi-indicator variables.

# Trust 

ESSSE$Trust <- ifelse(ESSSE$trstplc > 10, NA, ESSSE$trstplc)


# Obligation 

ESSSE$bplcdc <- ifelse(ESSSE$bplcdc > 10, NA, ESSSE$bplcdc)
ESSSE$doplcsy <- ifelse(ESSSE$doplcsy > 10, NA, ESSSE$doplcsy)
ESSSE$dpcstrb <- ifelse(ESSSE$dpcstrb > 10, NA, ESSSE$dpcstrb)

alpha(subset(ESSSE, select = c(bplcdc, doplcsy, dpcstrb)), check.keys =  TRUE)

ESSSE$Obligation <- (ESSSE$bplcdc + ESSSE$doplcsy + ESSSE$dpcstrb)/3


# Moral alignment 

ESSSE$plcrgwr <- ifelse(ESSSE$plcrgwr > 5, NA, ESSSE$plcrgwr)
ESSSE$plcipvl <- ifelse(ESSSE$plcipvl > 5, NA, ESSSE$plcipvl)
ESSSE$gsupplc <- ifelse(ESSSE$gsupplc > 5, NA, ESSSE$gsupplc)

# reverse coding 1-5

ESSSE$plcrgwr <- (6 - ESSSE$plcrgwr)
ESSSE$plcipvl <- (6 - ESSSE$plcipvl)
ESSSE$gsupplc <- (6 - ESSSE$gsupplc)

alpha(subset(ESSSE, select = c(plcrgwr, plcipvl, gsupplc)), check.keys =  TRUE)

ESSSE$MoralAlignment <- (ESSSE$plcrgwr + ESSSE$plcipvl + ESSSE$gsupplc)/3  


# Lawfulness 

ESSSE$plccbrb <- ifelse(ESSSE$plccbrb > 10, NA, ESSSE$plccbrb)

# reverse code 0-10 

ESSSE$plccbrb <- (10 - ESSSE$plccbrb)

ESSSE$Lawfulness <- ESSSE$plccbrb


# Procedural Fairness

ESSSE$plcrspc <- ifelse(ESSSE$plcrspc > 6, NA, ESSSE$plcrspc)
ESSSE$plcfrdc <- ifelse(ESSSE$plcfrdc > 5, NA, ESSSE$plcfrdc)
ESSSE$plcexdc <- ifelse(ESSSE$plcexdc > 5, NA, ESSSE$plcexdc)

alpha(subset(ESSSE, select = c(plcrspc, plcfrdc, plcexdc)), check.keys = TRUE)

ESSSE$ProceduralFairness <- (ESSSE$plcrspc + ESSSE$plcfrdc + ESSSE$plcexdc)/3 


#  Police Effectiveness

ESSSE$plcpvcr <- ifelse(ESSSE$plcpvcr > 10, NA, ESSSE$plcpvcr)

ESSSE$Effectiveness <- ESSSE$plcpvcr


###  SOCIODEMOGRAPHIC COVARIATES 


# Age

ESSSE$Age <- ESSSE$agea

ESSSE$Age <- ifelse(ESSSE$Age == 999, NA, ESSSE$Age)


# Gender 

ESSSE$Gender <- ESSSE$gndr

ESSSE$Gender <- ifelse(ESSSE$Gender == 9, NA, ESSSE$Gender)

# recording to 0-1

ESSSE$Gender[ESSSE$Gender == 1] <- 0
ESSSE$Gender[ESSSE$Gender == 2] <- 1

# Education 

ESSSE$Education <- ESSSE$eduyrs

ESSSE$Education <- ifelse(ESSSE$Education > 70, NA, ESSSE$Education)


# Ethnic minority status

ESSSE$EthnicMinority <- ESSSE$blgetmg

# recoding 0-1

ESSSE$EthnicMinority[ESSSE$EthnicMinority == 1] <- 0
ESSSE$EthnicMinority[ESSSE$EthnicMinority == 2] <- 1

ESSSE$EthnicMinority <- ifelse(ESSSE$EthnicMinority > 6, NA, ESSSE$EthnicMinority)


# Household net income

ESSSE$HouseholdIncome <- ESSSE$hinctnta

ESSSE$HouseholdIncome<- ifelse(ESSSE$HouseholdIncome > 70, NA, ESSSE$HouseholdIncome)

ESSSE_with_NA <- ESSSE 


## keep only complete cases of dependent variables and covariates

ESSSE <- ESSSE[complete.cases(
  ESSSE$Trust,
  ESSSE$Obligation,
  ESSSE$MoralAlignment,
  ESSSE$Lawfulness,
  ESSSE$ProceduralFairness,
  ESSSE$Effectiveness,
  ESSSE$Age,
  ESSSE$Gender,
  ESSSE$Education,
  ESSSE$EthnicMinority,
  ESSSE$HouseholdIncome
),]


### BINARY TREATMENT INDICATOR  

# combining date columns 

ESSSE$interviewstart <- as.Date(paste(ESSSE$inwyys, ESSSE$inwmms, ESSSE$inwdds, sep ="-"))

ESSSE$interviewend <- as.Date(paste(ESSSE$inwyye, ESSSE$inwmme, ESSSE$inwdde, sep ="-"))


# keeping only participants interviewed on same day

ESSSE <- ESSSE |>   filter(interviewstart == interviewend)


# creating an interview date variable

ESSSE$Date <- ESSSE$interviewstart


# creating endogenous treatment variable

attack_date_ESSSE <- as.Date("2010-12-11")  

ESSSE$Treatment <- NA

ESSSE$Treatment[ESSSE$Date > as.Date(attack_date_ESSSE)] <- 1
ESSSE$Treatment[ESSSE$Date <= as.Date(attack_date_ESSSE)] <- 0


# Removing those on the day of the attack 

ESSSE <- ESSSE |> filter(Date != attack_date_ESSSE)


# creating a running variable 

ESSSE$Runner <- as.integer(ESSSE$Date - as.Date(attack_date_ESSSE))


# creating a bandwidth variable for absolute distance (in days) from attack date

ESSSE$Bandwith <- abs(ESSSE$Runner)



################################################################################
# DESCRITPTIVE STATISTICS 
################################################################################


ESSSE_c<-subset(ESSSE, Date < as.Date(attack_date_ESSSE))

ESSSE_t<-subset(ESSSE, Date > as.Date(attack_date_ESSSE))

namelablesb <- c(
  "Trust in the Police",
  "Obligation to Obey",
  "Moral Alignment",
  "Lawfulness", 
  "Procedural Justice",
  "Police Effectiveness",
  "Age (Years)",
  "Gender",
  "Education (Years)",
  "Household Income (Decile)",
  "Ethnic Minority")


# Pre Attack Group 

pre_attack_summary_ESSSE <-sumtable(ESSSE_c |> 
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
                                    summ.names=c('N','Mean','SD', 'Min', 'Max')
)


# Post Attack Group 

post_attack_summary_ESSSE <- sumtable(ESSSE_t |> 
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
                                      summ.names=c('N','Mean','SD', 'Min', 'Max')
)



