getwd()
setwd("/Users/youngbeomcho/Desktop/Young Beom Cho/UST/22-23 Spring/SOSC 4110/Data")

packages <- c("dplyr", "readxl", "coda","devtools","loo","ggplot2",
              "flexmix","vctrs", "bayesplot", "rlang", "data.table",
              "rstan", "brms", "performance")

# install any packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}


#devtools::install_github("sshpa/bayesvl")
#install.packages('rstanarm')

library(dplyr, readxl)
library(data.table)
library(bayesvl)
library(stargazer)
library(flexmix)
library(rstan)
library(MASS)
library(brms)
library(performance)
library(lme4)

# Economic Factors
#1961~2020
ms_df<-read.csv("world_military_spending.csv", skip = 4, header = TRUE)
gdp_df<-read.csv("world_GDP.csv", skip = 4, header = TRUE)
inflation_df<-read.csv("world_inflation.csv", skip = 4, header = TRUE)

korea_ms <- ms_df%>%filter(Country.Code == "KOR")
korea_gdp <- gdp_df%>%filter(Country.Code == "KOR")
korea_inflation <- inflation_df%>%filter(Country.Code == "KOR")

#1994~2020
korea_debt <- readxl::read_excel('external_debt.xlsx', skip = 2)
korea_politics <- readxl::read_excel('Political_NK.xlsx',sheet = 1)
korea_conflict <- readxl::read_excel('Political_NK.xlsx', sheet = 2)

year <- c(rep(1960:2021))
# Preprocessing ms
korea_ms <- korea_ms[-c(1:4)]
colnames(korea_ms) <- year
korea_ms <- cbind(category = 'MilitarySpending', korea_ms)
korea_ms <- korea_ms [1: ncol(korea_ms)-1 ]

# Preprocessing gdp
korea_gdp <- korea_gdp[-c(1:4)]
colnames(korea_gdp) <- year
korea_gdp <- cbind(category = 'GDP', korea_gdp)
korea_gdp <- korea_gdp [1: ncol(korea_gdp)-1 ]

# Preprocessing inflation
korea_inflation <- korea_inflation[-c(1:4)]
colnames(korea_inflation) <- year
korea_inflation <- cbind(category = 'Inflation', korea_inflation)
korea_inflation <- korea_inflation [1: ncol(korea_inflation)-1 ]

# Preprocessing Politics and Conflict
colnames(korea_politics)[1] = "category"
colnames(korea_conflict)[1] = "category"

# Preprocessing debt
korea_debt <- korea_debt[1,]
korea_debt <- korea_debt[,-1]

  # Create new dataset
year_debt <- c(rep(1960:1993))
debt_col <- c('category', year_debt)
buffer_df <- data.frame(matrix(nrow = 1, ncol = length(debt_col))) 
colnames(buffer_df) <- debt_col
buffer_df[1,1] <-'ExternalDebt'
buffer_df[1,2:ncol(buffer_df)] = 0

  # hcat
korea_debt <- cbind(buffer_df, korea_debt)

# Merge Entire Dataset and transpose it
korea_df <- rbind(korea_gdp, korea_inflation, korea_debt, korea_politics, korea_conflict, korea_ms)
korea_df <- transpose(korea_df)
names(korea_df) <- korea_df[1,]
korea_df <- korea_df[-1,]
rownames(korea_df) <- c(rep(1960:2021))

str(korea_df)

korea_df <- transform(korea_df, GDP = as.numeric(GDP), 
                      Inflation = as.numeric(Inflation),
                      #ExternalDebt = as.integer(ExternalDebt),
                      LeftRulingParty = as.integer(LeftRulingParty),
                      RightRulingParty = as.integer(RightRulingParty),
                      NuclearExperiment = as.integer(NuclearExperiment),
                      MissileThreat = as.integer(MissileThreat),
                      EngageinBattle = as.integer(EngageinBattle),
                      MilitarySpending = as.numeric(MilitarySpending)
               )

korea_df[1,2] = korea_df[2,2] # Since Inflation rate in 1960 is missing, we imputed by backward filling method

for (i in 2:nrow(korea_df)){
  korea_df[i,3] <- gsub(',', '', korea_df[i,3])
}

korea_df <- transform(korea_df, ExternalDebt = as.numeric(ExternalDebt))

# Check the relationship between Military Spending with Different Variables


linear_f1960_all <- lm(MilitarySpending ~ GDP + Inflation + LeftRulingParty +
                         NuclearExperiment + MissileThreat+ EngageinBattle, 
                       data = korea_df)
summary(linear_f1960_all)

linear_f1960_econ <- lm(MilitarySpending ~ GDP + Inflation, data = korea_df)
summary(linear_f1960_econ)

# The coefficient result of the RightRulingParty = NA since there is multicolinearity issue with 
# LeftRulingParty, they are inversely proportional to each other
linear_f1960_politics <- lm(MilitarySpending ~ LeftRulingParty + RightRulingParty, data = korea_df)
summary(linear_f1960_politics)


linear_f1960_NK <- lm(MilitarySpending ~ NuclearExperiment + MissileThreat+
                         EngageinBattle, data = korea_df)
summary(linear_f1960_NK)

korea_df_politics <- korea_df %>% 
  dplyr::select('LeftRulingParty', 'RightRulingParty','MilitarySpending')

cor(korea_df_politics) # Checking why the coefficient of RightRulingParty is NA, found there is a multicolinearity issue

summary(lm(MilitarySpending~GDP, data = korea_df))


korea_df_f1994 <- korea_df[which(rownames(korea_df) == "1994"): nrow(korea_df), ]


str(korea_df_f1994)

linear_f1994_all <- lm(MilitarySpending ~ GDP + Inflation + ExternalDebt + LeftRulingParty +
                      NuclearExperiment + MissileThreat+
                      EngageinBattle, data = korea_df_f1994)
summary(linear_f1994_all)

linear_f1994_econ <- lm(MilitarySpending ~ GDP + Inflation + ExternalDebt, data = korea_df_f1994)
summary(linear_f1994_econ)

# Preprocessing data to fit in DAG model

str(korea_df)

korea_df_bayes_1 <- korea_df%>%
  mutate(GDP_diff = GDP - lag(GDP), # Difference in route between years
         GDP_growth = (GDP_diff)/GDP, # growth rate in percent
         Inflation_diff = Inflation - lag(Inflation),
         Inflation_change = Inflation_diff/Inflation,
         Debt_diff = ExternalDebt - lag(ExternalDebt),
         Debt_change = Debt_diff/ExternalDebt,
         MS_diff = MilitarySpending - lag(MilitarySpending),
         MS_change = MS_diff/MilitarySpending) %>%
  dplyr::select(GDP_growth,Inflation_change,Debt_change, LeftRulingParty, RightRulingParty,
         NuclearExperiment, MissileThreat, EngageinBattle, MS_change)

korea_df_bayes_2 <- korea_df%>%
  mutate(GDP_diff = GDP - lag(GDP), # Difference in route between years
         GDP_growth = (GDP_diff)/GDP, # growth rate in percent
         Debt_diff = ExternalDebt - lag(ExternalDebt),
         Debt_change = Debt_diff/ExternalDebt,
         MS_diff = MilitarySpending - lag(MilitarySpending),
         MS_change = MS_diff/MilitarySpending) %>%
  dplyr::select(GDP_growth, Inflation, Debt_change, MS_change, LeftRulingParty, RightRulingParty,
         NuclearExperiment, MissileThreat, EngageinBattle)

korea_df_bayes_1[is.na(korea_df_bayes_1)] <- 0
korea_df_bayes_2[is.na(korea_df_bayes_2)] <- 0

linear_f1960bayes_all <- lm(MS_change ~ GDP_growth + Inflation_change + LeftRulingParty +
                         NuclearExperiment + MissileThreat+
                         EngageinBattle, data = korea_df_bayes_1)
summary(linear_f1960bayes_all)

linear_f1960bayes_econ <- lm(MS_change ~ GDP_growth + Inflation_change, data = korea_df_bayes_1)
summary(linear_f1960_econ)

linear_f1960bayes_politics <- lm(MS_change ~ LeftRulingParty, data = korea_df_bayes_1)
summary(linear_f1960bayes_politics)

linear_f1960bayes_NK <- lm(MS_change ~ NuclearExperiment + MissileThreat+
                        EngageinBattle, data = korea_df_bayes_1)
summary(linear_f1960bayes_NK)

########

linear_f1994bayes_all <- lm(MS_change ~ GDP_growth + Inflation_change + Debt_change + LeftRulingParty +
                         NuclearExperiment + MissileThreat+
                         EngageinBattle, data = korea_df_bayes_1[35:62,1:9])
summary(linear_f1994bayes_all)

linear_f1994bayes_econ <- lm(MS_change ~ GDP_growth + Inflation_change + Debt_change + GDP_growth * Inflation_change, data = korea_df_bayes_1[35:62,1:9])
summary(linear_f1994bayes_econ)

f1960_all <- linear_f1960_all
f1960bayes_all <- linear_f1960bayes_all
f1994_all <- linear_f1994_all
f1994bayes_all <- linear_f1994bayes_all

# stargazer(f1960_all, f1994_all, f1960bayes_all, f1994bayes_all, type = 'html', out = "MS_lm.html")

# Export Dataset for Future Analysis
# write.csv(korea_df_bayes, "/Users/youngbeomcho/Desktop/Young Beom Cho/UST/22-23 Spring/SOSC 4110/Data/korea_dataset.csv", row.names=TRUE, col.names =  TRUE)

#######################
# End of Linear Model #
#######################

# Create the DAG model, will exclude ExternalDebt since it contains data 1994 aftwerwards
just_viz <- bayesvl()
just_viz <- bvl_addNode(just_viz, "GDP", "norm")
just_viz <- bvl_addNode(just_viz, "Inflation", "norm")
just_viz <- bvl_addNode(just_viz, "Debt", "norm")
just_viz <- bvl_addNode(just_viz, "PoliticalParty", "binom")
just_viz <- bvl_addNode(just_viz, "NuclearExperiment", "binom")
just_viz <- bvl_addNode(just_viz, "MissileThreat", "norm")
just_viz <- bvl_addNode(just_viz, "EngageinBattle", "norm")
just_viz <- bvl_addNode(just_viz, "MilitaryExpenditure", "norm")

just_viz <- bvl_addArc(just_viz, "GDP", "MilitaryExpenditure", "slope")
just_viz <- bvl_addArc(just_viz, "Inflation", "MilitaryExpenditure", "slope")
just_viz <- bvl_addArc(just_viz, "PoliticalParty", "MilitaryExpenditure", "slope")
just_viz <- bvl_addArc(just_viz, "NuclearExperiment", "MilitaryExpenditure", "slope")
just_viz <- bvl_addArc(just_viz, "MissileThreat", "MilitaryExpenditure", "slope")
just_viz <- bvl_addArc(just_viz, "EngageinBattle", "MilitaryExpenditure", "slope")
just_viz <- bvl_addArc(just_viz, "Debt", "MilitaryExpenditure", "slope")

just_viz <- bvl_addArc(just_viz, "Debt", "GDP", "slope")
just_viz <- bvl_addArc(just_viz, "Inflation", "GDP", "slope")
just_viz <- bvl_addArc(just_viz, "NuclearExperiment", "PoliticalParty", "slope")
just_viz <- bvl_addArc(just_viz, "MissileThreat", "PoliticalParty", "slope")
just_viz <- bvl_addArc(just_viz, "EngageinBattle", "PoliticalParty", "slope")

just_viz <- bvl_addArc(just_viz, "PoliticalParty", "NuclearExperiment", "slope")
just_viz <- bvl_addArc(just_viz, "PoliticalParty", "MissileThreat", "slope")
just_viz <- bvl_addArc(just_viz, "PoliticalParty", "EngageinBattle", "slope")

bvl_bnPlot(just_viz)

# Create first model to check whether it's working
first_lm <- lm(MS_change ~ GDP_growth , data = korea_df_bayes_1)

first_model <- bayesvl()
first_model <- bvl_addNode(first_model, "GDP_growth", "norm")
first_model <- bvl_addNode(first_model, "MS_change", "norm")
first_model <- bvl_addArc(first_model, "GDP_growth", "MS_change", "slope")
bvl_bnPlot(first_model)

first_model <- bvl_modelFit(first_model, korea_df_bayes_1, warmup = 2000, iter = 10000, chains = 4)
summary(first_model)
bvl_plotTrace(first_model)
bvl_plotIntervals(first_model) 

first_model_brms <- brm(MS_change ~ GDP_growth,
                        korea_df_bayes_1, family = gaussian(), chains = 4,
                         iter = 10000, warmup = 2000)

model_performance(first_lm)
model_performance(first_model_brms)

# Create the DAG model for first dataset, will exclude ExternalDebt since it contains data 1994 aftwerwards
## Create Linear Model for future comparison
second_lm <- lm(MilitarySpending ~ GDP + Inflation + LeftRulingParty +
                 NuclearExperiment + MissileThreat + EngageinBattle + 
                 LeftRulingParty * NuclearExperiment + LeftRulingParty * MissileThreat +
                 LeftRulingParty * EngageinBattle + GDP * Inflation, 
               data = korea_df)

## Create Bayesian network model using bayesvl
second_model <- bayesvl()
second_model <- bvl_addNode(second_model, "GDP", "norm")
second_model <- bvl_addNode(second_model, "Inflation", "norm")
second_model <- bvl_addNode(second_model, "LeftRulingParty", "binom")
second_model <- bvl_addNode(second_model, "NuclearExperiment", "binom")
second_model <- bvl_addNode(second_model, "MissileThreat", "norm")
second_model <- bvl_addNode(second_model, "EngageinBattle", "norm")
second_model <- bvl_addNode(second_model, "MilitarySpending", "norm")

second_model <- bvl_addArc(second_model, "EngageinBattle", "MilitarySpending", "slope")
second_model <- bvl_addArc(second_model, "MissileThreat", "MilitarySpending", "slope")
second_model <- bvl_addArc(second_model, "NuclearExperiment", "MilitarySpending", "slope")
second_model <- bvl_addArc(second_model, "GDP", "MilitarySpending", "slope")
second_model <- bvl_addArc(second_model, "LeftRulingParty", "MilitarySpending", "slope")
second_model <- bvl_addArc(second_model, "Inflation", "MilitarySpending", "slope")

### create transformed node
second_model <- bvl_addNode(second_model, 'GDP_Inflation', 'trans')
second_model <- bvl_addArc(second_model, "GDP", "GDP_Inflation", "*")
second_model <- bvl_addArc(second_model, "Inflation", "GDP_Inflation", "*")
second_model <- bvl_addArc(second_model, "GDP_Inflation", "MilitarySpending", "slope")

second_model <- bvl_addNode(second_model, 'Left_Battle', 'trans')
second_model <- bvl_addArc(second_model, "LeftRulingParty", "Left_Battle", "*")
second_model <- bvl_addArc(second_model, "EngageinBattle", "Left_Battle", "*")
second_model <- bvl_addArc(second_model, "Left_Battle", "MilitarySpending", "slope")

second_model <- bvl_addNode(second_model, 'Left_Missile', 'trans')
second_model <- bvl_addArc(second_model, "LeftRulingParty", "Left_Missile", "*")
second_model <- bvl_addArc(second_model, "MissileThreat", "Left_Missile", "*")
second_model <- bvl_addArc(second_model, "Left_Missile", "MilitarySpending", "slope")

second_model <- bvl_addNode(second_model, 'Left_Nuclear', 'trans')
second_model <- bvl_addArc(second_model, "LeftRulingParty", "Left_Nuclear", "*")
second_model <- bvl_addArc(second_model, "NuclearExperiment", "Left_Nuclear", "*")
second_model <- bvl_addArc(second_model, "Left_Nuclear", "MilitarySpending", "slope")

bvl_bnPlot(second_model)
second_model <- bvl_modelFit(second_model, korea_df, warmup = 2000, iter = 10000, chains = 4)
summary(second_model)
bvl_plotTrace(second_model)
bvl_plotIntervals(second_model)

## create Bayesian Model for future model comparison
second_model_brms <- brm(MilitarySpending ~ GDP + Inflation + LeftRulingParty +
                      NuclearExperiment + MissileThreat + EngageinBattle + 
                      LeftRulingParty * NuclearExperiment + LeftRulingParty * MissileThreat +
                      LeftRulingParty * EngageinBattle + GDP * Inflation, 
                    data = korea_df, family = gaussian(), chains = 4,
                        iter = 10000, warmup = 2000)


# Create the DAG model focusing on economic factors for first dataset, will filter the dataset 1994 afterwards
## Create Linear Model for future comparison
third_lm <- lm(MilitarySpending ~ GDP + Inflation + ExternalDebt + 
                 GDP * Inflation, data = korea_df[36:62,c(1:3, 9)])

## Create Bayesian network model using bayesvl
third_model <- bayesvl()
third_model <- bvl_addNode(third_model, "GDP", "norm")
third_model <- bvl_addNode(third_model, "Inflation", "norm")
third_model <- bvl_addNode(third_model, 'ExternalDebt', 'norm')
third_model <- bvl_addNode(third_model, "MilitarySpending", "norm")

third_model <- bvl_addArc(third_model, "GDP", "MilitarySpending", "slope")
third_model <- bvl_addArc(third_model, "ExternalDebt", "MilitarySpending", "slope")
third_model <- bvl_addArc(third_model, "Inflation", "MilitarySpending", "slope")

third_model <- bvl_addNode(third_model, 'GDP_Inflation', 'trans')
third_model <- bvl_addArc(third_model, "GDP", "GDP_Inflation", "*")
third_model <- bvl_addArc(third_model, "Inflation", "GDP_Inflation", "*")
third_model <- bvl_addArc(third_model, "GDP_Inflation", "MilitarySpending", "slope")

bvl_bnPlot(third_model)
third_model <- bvl_modelFit(third_model, korea_df[36:62,c(1:3, 9)], warmup = 2000, iter = 10000, chains = 4)
summary(third_model)
bvl_plotTrace(third_model)
bvl_plotIntervals(third_model) 
# bvl_plotParams(third_model, 2, 3) 

## create Bayesian Model for future model comparison
third_model_brms <- brm(MilitarySpending ~ GDP + Inflation + ExternalDebt + 
                          GDP * Inflation, data = korea_df[36:62,c(1:3, 9)],
                        family = gaussian(), chains = 4,
                        iter = 10000, warmup = 2000)


# Create the DAG model focusing on non-economic factors for first dataset
## Create Linear Model for future comparison
fourth_lm <- lm(MilitarySpending ~ LeftRulingParty + NuclearExperiment + MissileThreat + 
                   EngageinBattle + LeftRulingParty * NuclearExperiment + 
                   LeftRulingParty * MissileThreat + LeftRulingParty * EngageinBattle, 
                 data = korea_df)

## Create Bayesian network model using bayesvl
fourth_model <- bayesvl()
fourth_model <- bvl_addNode(fourth_model, "LeftRulingParty", "binom")
fourth_model <- bvl_addNode(fourth_model, "NuclearExperiment", "binom")
fourth_model <- bvl_addNode(fourth_model, "MissileThreat", "norm")
fourth_model <- bvl_addNode(fourth_model, "EngageinBattle", "norm")
fourth_model <- bvl_addNode(fourth_model, "MilitarySpending", "norm")

fourth_model <- bvl_addArc(fourth_model, "LeftRulingParty", "MilitarySpending", "slope")
fourth_model <- bvl_addArc(fourth_model, "EngageinBattle", "MilitarySpending", "slope")
fourth_model <- bvl_addArc(fourth_model, "MissileThreat", "MilitarySpending", "slope")
fourth_model <- bvl_addArc(fourth_model, "NuclearExperiment", "MilitarySpending", "slope")

fourth_model <- bvl_addNode(fourth_model, 'Left_Battle', 'trans')
fourth_model <- bvl_addArc(fourth_model, "LeftRulingParty", "Left_Battle", "*")
fourth_model <- bvl_addArc(fourth_model, "EngageinBattle", "Left_Battle", "*")
fourth_model <- bvl_addArc(fourth_model, "Left_Battle", "MilitarySpending", "slope")

fourth_model <- bvl_addNode(fourth_model, 'Left_Missile', 'trans')
fourth_model <- bvl_addArc(fourth_model, "LeftRulingParty", "Left_Missile", "*")
fourth_model <- bvl_addArc(fourth_model, "MissileThreat", "Left_Missile", "*")
fourth_model <- bvl_addArc(fourth_model, "Left_Missile", "MilitarySpending", "slope")

fourth_model <- bvl_addNode(fourth_model, 'Left_Nuclear', 'trans')
fourth_model <- bvl_addArc(fourth_model, "LeftRulingParty", "Left_Nuclear", "*")
fourth_model <- bvl_addArc(fourth_model, "NuclearExperiment", "Left_Nuclear", "*")
fourth_model <- bvl_addArc(fourth_model, "Left_Nuclear", "MilitarySpending", "slope")

bvl_bnPlot(fourth_model)
fourth_model <- bvl_modelFit(fourth_model, korea_df, warmup = 2000, iter = 10000, chains = 4)
summary(fourth_model)
bvl_plotTrace(fourth_model)
bvl_plotIntervals(fourth_model) 

## create Bayesian Model for future model comparison
fourth_model_brms <- brm(MilitarySpending ~ LeftRulingParty + NuclearExperiment + MissileThreat + 
                           EngageinBattle + LeftRulingParty * NuclearExperiment + 
                           LeftRulingParty * MissileThreat + LeftRulingParty * EngageinBattle, 
                         data = korea_df, family = gaussian(), chains = 4,
                          iter = 10000, warmup = 2000)


# Create the DAG model for second dataset, will exclude ExternalDebt since it contains data 1994 aftwerwards
## Create Linear Model for future comparison
fifth_lm <- lm(MS_change ~ GDP_growth + Inflation_change + LeftRulingParty +
                  NuclearExperiment + MissileThreat + EngageinBattle + 
                  LeftRulingParty * NuclearExperiment + LeftRulingParty * MissileThreat +
                  LeftRulingParty * EngageinBattle + GDP_growth * Inflation_change, 
                data = korea_df_bayes_1)

## Create Bayesian network model using bayesvl
fifth_model <- bayesvl()
fifth_model <- bvl_addNode(fifth_model, "GDP_growth", "norm")
fifth_model <- bvl_addNode(fifth_model, "Inflation_change", "norm")
fifth_model <- bvl_addNode(fifth_model, "LeftRulingParty", "binom")
fifth_model <- bvl_addNode(fifth_model, "NuclearExperiment", "binom")
fifth_model <- bvl_addNode(fifth_model, "MissileThreat", "norm")
fifth_model <- bvl_addNode(fifth_model, "EngageinBattle", "norm")
fifth_model <- bvl_addNode(fifth_model, "MS_change", "norm")

fifth_model <- bvl_addArc(fifth_model, "EngageinBattle", "MS_change", "slope")
fifth_model <- bvl_addArc(fifth_model, "MissileThreat", "MS_change", "slope")
fifth_model <- bvl_addArc(fifth_model, "NuclearExperiment", "MS_change", "slope")
fifth_model <- bvl_addArc(fifth_model, "GDP_growth", "MS_change", "slope")
fifth_model <- bvl_addArc(fifth_model, "LeftRulingParty", "MS_change", "slope")
fifth_model <- bvl_addArc(fifth_model, "Inflation_change", "MS_change", "slope")

### create transformed node
fifth_model <- bvl_addNode(fifth_model, 'GDP_Inflation', 'trans')
fifth_model <- bvl_addArc(fifth_model, "GDP_growth", "GDP_Inflation", "*")
fifth_model <- bvl_addArc(fifth_model, "Inflation_change", "GDP_Inflation", "*")
fifth_model <- bvl_addArc(fifth_model, "GDP_Inflation", "MS_change", "slope")

fifth_model <- bvl_addNode(fifth_model, 'Left_Battle', 'trans')
fifth_model <- bvl_addArc(fifth_model, "LeftRulingParty", "Left_Battle", "*")
fifth_model <- bvl_addArc(fifth_model, "EngageinBattle", "Left_Battle", "*")
fifth_model <- bvl_addArc(fifth_model, "Left_Battle", "MS_change", "slope")

fifth_model <- bvl_addNode(fifth_model, 'Left_Missile', 'trans')
fifth_model <- bvl_addArc(fifth_model, "LeftRulingParty", "Left_Missile", "*")
fifth_model <- bvl_addArc(fifth_model, "MissileThreat", "Left_Missile", "*")
fifth_model <- bvl_addArc(fifth_model, "Left_Missile", "MS_change", "slope")

fifth_model <- bvl_addNode(fifth_model, 'Left_Nuclear', 'trans')
fifth_model <- bvl_addArc(fifth_model, "LeftRulingParty", "Left_Nuclear", "*")
fifth_model <- bvl_addArc(fifth_model, "NuclearExperiment", "Left_Nuclear", "*")
fifth_model <- bvl_addArc(fifth_model, "Left_Nuclear", "MS_change", "slope")

bvl_bnPlot(fifth_model)
fifth_model <- bvl_modelFit(fifth_model, korea_df_bayes_1, warmup = 2000, iter = 10000, chains = 4)
summary(fifth_model)
bvl_plotTrace(fifth_model)
bvl_plotIntervals(fifth_model) 

## create Bayesian Model for future model comparison
fifth_model_brms <- brm(MS_change ~ GDP_growth + Inflation_change + LeftRulingParty +
                           NuclearExperiment + MissileThreat + EngageinBattle + 
                           LeftRulingParty * NuclearExperiment + LeftRulingParty * MissileThreat +
                           LeftRulingParty * EngageinBattle + GDP_growth * Inflation_change,
                         data = korea_df_bayes_1, family = gaussian(), chains = 4,
                         iter = 10000, warmup = 2000)


# Create the DAG model focusing on economic factors for second dataset, will filter the dataset 1994 afterwards
## Create Linear Model for future comparison
sixth_lm <- lm(MS_change ~ GDP_growth + Inflation_change + Debt_change + 
                  GDP_growth * Inflation_change, data = korea_df_bayes_1[36:62,c(1:3, 9)])

## Create Bayesian network model using bayesvl
sixth_model <- bayesvl()
sixth_model <- bvl_addNode(sixth_model, "GDP_growth", "norm")
sixth_model <- bvl_addNode(sixth_model, "Inflation_change", "norm")
sixth_model <- bvl_addNode(sixth_model, 'Debt_change', 'norm')
sixth_model <- bvl_addNode(sixth_model, "MS_change", "norm")

sixth_model <- bvl_addArc(sixth_model, "GDP_growth", "MS_change", "slope")
sixth_model <- bvl_addArc(sixth_model, "Debt_change", "MS_change", "slope")
sixth_model <- bvl_addArc(sixth_model, "Inflation_change", "MS_change", "slope")

sixth_model <- bvl_addNode(sixth_model, 'GDP_Inflation', 'trans')
sixth_model <- bvl_addArc(sixth_model, "GDP_growth", "GDP_Inflation", "*")
sixth_model <- bvl_addArc(sixth_model, "Inflation_change", "GDP_Inflation", "*")
sixth_model <- bvl_addArc(sixth_model, "GDP_Inflation", "MS_change", "slope")

bvl_bnPlot(sixth_model)
sixth_model <- bvl_modelFit(sixth_model, korea_df_bayes_1[36:62,c(1:3, 9)], warmup = 2000, iter = 10000, chains = 4)
summary(sixth_model)
bvl_plotTrace(sixth_model)
bvl_plotIntervals(sixth_model) 
# bvl_plotParams(sixth_model, 2, 3) 

## create Bayesian Model for future model comparison
sixth_model_brms <- brm(MS_change ~ GDP_growth + Inflation_change + Debt_change + 
                           GDP_growth * Inflation_change, data = korea_df_bayes_1[36:62,c(1:3, 9)],
                         family = gaussian(), chains = 4,
                         iter = 10000, warmup = 2000)


# Create the DAG model focusing on non-economic factors for second dataset
## Create Linear Model for future comparison
seventh_lm <- lm(MS_change ~ LeftRulingParty + NuclearExperiment + MissileThreat + 
                  EngageinBattle + LeftRulingParty * NuclearExperiment + 
                  LeftRulingParty * MissileThreat + LeftRulingParty * EngageinBattle, 
                data = korea_df_bayes_1)

## Create Bayesian network model using bayesvl
seventh_model <- bayesvl()
seventh_model <- bvl_addNode(seventh_model, "LeftRulingParty", "binom")
seventh_model <- bvl_addNode(seventh_model, "NuclearExperiment", "binom")
seventh_model <- bvl_addNode(seventh_model, "MissileThreat", "norm")
seventh_model <- bvl_addNode(seventh_model, "EngageinBattle", "norm")
seventh_model <- bvl_addNode(seventh_model, "MS_change", "norm")

seventh_model <- bvl_addArc(seventh_model, "LeftRulingParty", "MS_change", "slope")
seventh_model <- bvl_addArc(seventh_model, "EngageinBattle", "MS_change", "slope")
seventh_model <- bvl_addArc(seventh_model, "MissileThreat", "MS_change", "slope")
seventh_model <- bvl_addArc(seventh_model, "NuclearExperiment", "MS_change", "slope")

seventh_model <- bvl_addNode(seventh_model, 'Left_Battle', 'trans')
seventh_model <- bvl_addArc(seventh_model, "LeftRulingParty", "Left_Battle", "*")
seventh_model <- bvl_addArc(seventh_model, "EngageinBattle", "Left_Battle", "*")
seventh_model <- bvl_addArc(seventh_model, "Left_Battle", "MS_change", "slope")

seventh_model <- bvl_addNode(seventh_model, 'Left_Missile', 'trans')
seventh_model <- bvl_addArc(seventh_model, "LeftRulingParty", "Left_Missile", "*")
seventh_model <- bvl_addArc(seventh_model, "MissileThreat", "Left_Missile", "*")
seventh_model <- bvl_addArc(seventh_model, "Left_Missile", "MS_change", "slope")

seventh_model <- bvl_addNode(seventh_model, 'Left_Nuclear', 'trans')
seventh_model <- bvl_addArc(seventh_model, "LeftRulingParty", "Left_Nuclear", "*")
seventh_model <- bvl_addArc(seventh_model, "NuclearExperiment", "Left_Nuclear", "*")
seventh_model <- bvl_addArc(seventh_model, "Left_Nuclear", "MS_change", "slope")

bvl_bnPlot(seventh_model)
seventh_model <- bvl_modelFit(seventh_model, korea_df_bayes_1, warmup = 2000, iter = 10000, chains = 4)
summary(seventh_model)
bvl_plotTrace(seventh_model)
bvl_plotIntervals(seventh_model) 

## create Bayesian Model for future model comparison
seventh_model_brms <- brm(MS_change ~ LeftRulingParty + NuclearExperiment + MissileThreat + 
                           EngageinBattle + LeftRulingParty * NuclearExperiment + 
                           LeftRulingParty * MissileThreat + LeftRulingParty * EngageinBattle, 
                         data = korea_df_bayes_1, family = gaussian(), chains = 4,
                         iter = 10000, warmup = 2000)


# Create the DAG model for third dataset, will exclude ExternalDebt since it contains data 1994 aftwerwards
## Create Linear Model for future comparison
eighth_lm <- lm(MS_change ~ GDP_growth + Inflation + LeftRulingParty +
                 NuclearExperiment + MissileThreat + EngageinBattle + 
                 LeftRulingParty * NuclearExperiment + LeftRulingParty * MissileThreat +
                 LeftRulingParty * EngageinBattle + GDP_growth * Inflation, 
               data = korea_df_bayes_2)

## Create Bayesian network model using bayesvl
eighth_model <- bayesvl()
eighth_model <- bvl_addNode(eighth_model, "GDP_growth", "norm")
eighth_model <- bvl_addNode(eighth_model, "Inflation", "norm")
eighth_model <- bvl_addNode(eighth_model, "LeftRulingParty", "binom")
eighth_model <- bvl_addNode(eighth_model, "NuclearExperiment", "binom")
eighth_model <- bvl_addNode(eighth_model, "MissileThreat", "norm")
eighth_model <- bvl_addNode(eighth_model, "EngageinBattle", "norm")
eighth_model <- bvl_addNode(eighth_model, "MS_change", "norm")

eighth_model <- bvl_addArc(eighth_model, "EngageinBattle", "MS_change", "slope")
eighth_model <- bvl_addArc(eighth_model, "MissileThreat", "MS_change", "slope")
eighth_model <- bvl_addArc(eighth_model, "NuclearExperiment", "MS_change", "slope")
eighth_model <- bvl_addArc(eighth_model, "GDP_growth", "MS_change", "slope")
eighth_model <- bvl_addArc(eighth_model, "LeftRulingParty", "MS_change", "slope")
eighth_model <- bvl_addArc(eighth_model, "Inflation", "MS_change", "slope")

### create transformed node
eighth_model <- bvl_addNode(eighth_model, 'GDP_Inflation', 'trans')
eighth_model <- bvl_addArc(eighth_model, "GDP_growth", "GDP_Inflation", "*")
eighth_model <- bvl_addArc(eighth_model, "Inflation", "GDP_Inflation", "*")
eighth_model <- bvl_addArc(eighth_model, "GDP_Inflation", "MS_change", "slope")

eighth_model <- bvl_addNode(eighth_model, 'Left_Battle', 'trans')
eighth_model <- bvl_addArc(eighth_model, "LeftRulingParty", "Left_Battle", "*")
eighth_model <- bvl_addArc(eighth_model, "EngageinBattle", "Left_Battle", "*")
eighth_model <- bvl_addArc(eighth_model, "Left_Battle", "MS_change", "slope")

eighth_model <- bvl_addNode(eighth_model, 'Left_Missile', 'trans')
eighth_model <- bvl_addArc(eighth_model, "LeftRulingParty", "Left_Missile", "*")
eighth_model <- bvl_addArc(eighth_model, "MissileThreat", "Left_Missile", "*")
eighth_model <- bvl_addArc(eighth_model, "Left_Missile", "MS_change", "slope")

eighth_model <- bvl_addNode(eighth_model, 'Left_Nuclear', 'trans')
eighth_model <- bvl_addArc(eighth_model, "LeftRulingParty", "Left_Nuclear", "*")
eighth_model <- bvl_addArc(eighth_model, "NuclearExperiment", "Left_Nuclear", "*")
eighth_model <- bvl_addArc(eighth_model, "Left_Nuclear", "MS_change", "slope")

bvl_bnPlot(eighth_model)
eighth_model <- bvl_modelFit(eighth_model, korea_df_bayes_2, warmup = 2000, iter = 10000, chains = 4)
summary(eighth_model)
bvl_plotTrace(eighth_model)
bvl_plotIntervals(eighth_model)

## create Bayesian Model for future model comparison
eighth_model_brms <- brm(MS_change ~ GDP_growth + Inflation + LeftRulingParty +
                          NuclearExperiment + MissileThreat + EngageinBattle + 
                          LeftRulingParty * NuclearExperiment + LeftRulingParty * MissileThreat +
                          LeftRulingParty * EngageinBattle + GDP_growth * Inflation, 
                        data = korea_df_bayes_2, family = skew_normal, chains = 4,
                        iter = 10000, warmup = 2000)


# Create the DAG model focusing on economic factors for third dataset, will filter the dataset 1994 afterwards
## Create Linear Model for future comparison
ninth_lm <- lm(MS_change ~ GDP_growth + Debt_change + Inflation + GDP_growth * Inflation,
               data = korea_df_bayes_2[36:62,c(1:9)])

## Create Bayesian network model using bayesvl
ninth_model <- bayesvl()
ninth_model <- bvl_addNode(ninth_model, "GDP_growth", "norm")
ninth_model <- bvl_addNode(ninth_model, "Inflation", "norm")
ninth_model <- bvl_addNode(ninth_model, 'Debt_change', 'norm')
ninth_model <- bvl_addNode(ninth_model, "MS_change", "norm")

ninth_model <- bvl_addArc(ninth_model, "GDP_growth", "MS_change", "slope")
ninth_model <- bvl_addArc(ninth_model, "Debt_change", "MS_change", "slope")
ninth_model <- bvl_addArc(ninth_model, "Inflation", "MS_change", "slope")

ninth_model <- bvl_addNode(ninth_model, 'GDP_Inflation', 'trans')
ninth_model <- bvl_addArc(ninth_model, "GDP_growth", "GDP_Inflation", "*")
ninth_model <- bvl_addArc(ninth_model, "Inflation", "GDP_Inflation", "*")
ninth_model <- bvl_addArc(ninth_model, "GDP_Inflation", "MS_change", "slope")

bvl_bnPlot(ninth_model)
ninth_model <- bvl_modelFit(ninth_model, korea_df_bayes_2[36:62,c(1:9)], warmup = 2000, iter = 10000, chains = 4)
summary(ninth_model)
bvl_plotTrace(ninth_model)
bvl_plotIntervals(ninth_model) 
# bvl_plotParams(ninth_model, 2, 3)

## create Bayesian Model for future model comparison
ninth_model_brms <- brm(MS_change ~ GDP_growth + Debt_change + Inflation + GDP_growth * Inflation,
                         data = korea_df_bayes_2[36:62,c(1:9)], family = gaussian(), chains = 4,
                         iter = 10000, warmup = 2000)

# Try Evaluation Metrics available in bayesvl package
bvl_stanLoo(first_model)
bvl_stanLoo(second_model)
bvl_stanLoo(third_model)
bvl_stanLoo(fourth_model)
bvl_stanLoo(fifth_model)
bvl_stanLoo(sixth_model)
bvl_stanLoo(seventh_model)
bvl_stanLoo(eighth_model)
bvl_stanLoo(ninth_model)

# There wasn't proper metrics to compare the performance between linear model 
# and bayesian model, hence we decide to compare their root mean squared error (rmse),
# since we can't extract rmse from bayesvl, I create the same bayesian model by brms
# and extract rmse from the model and compare the performance

# Within group comparison
model_performance(first_lm)
model_performance(second_lm)
model_performance(third_lm)
model_performance(fourth_lm)
model_performance(fifth_lm)
model_performance(sixth_lm)
model_performance(seventh_lm)
model_performance(eighth_lm)
model_performance(ninth_lm)

model_performance(first_model_brms)
model_performance(second_model_brms)
model_performance(third_model_brms)
model_performance(fourth_model_brms)
model_performance(fifth_model_brms)
model_performance(sixth_model_brms)
model_performance(seventh_model_brms)
model_performance(eighth_model_brms)
model_performance(ninth_model_brms)

# Between group comparison
model_performance(eighth_lm)
model_performance(eighth_model_brms)
