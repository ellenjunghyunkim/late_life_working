####################################################################################################
## project: = Does (Re-)Entering the Labor Market at Advanced Ages Protect Against 
#~ Cognitive Decline? A Panel-Matching Difference-in-differences Approach
## author(s): Jung Hyun Kim
## code started: May, 2021
## last update: November, 2022
####################################################################################################

####################################################################################################
## clear work space
####################################################################################################
rm(list=ls())
## set seed
set.seed(41489)
####################################################################################################
## load packages 
####################################################################################################
x <- c("haven", "plm", "tidyverse", "ggplot2", "ggpubr", "dplyr", "expss", "foreign", "nnet", "reshape2","plyr","zoo",
       "dplyr", "gtools","devtools", "xtable", "compareGroups")
install.packages(x) 
lapply(x, library, character.only = TRUE)

#We want to download the latest version of this software.
if(!require(devtools)) install.packages("devtools")
library(devtools)
install_github("insongkim/PanelMatch", ref = "se_comparison", dependencies=TRUE, force = TRUE)
library("PanelMatch")

#Please set your own directory.
# Set working directory #For example, I created a folder "CognitiveDecline".
setwd("~/Documents/CognitiveDecline")

####################################################################################################
## Download the KLoSA data & clean the variables & create final analytical sample
####################################################################################################
# In this case, I located the KLoSA data file inside of the folder "CognitiveDecline".
KLoSA <- read_dta("KLoSA_STATA_2022v3/d:KLoSA_data.dta")
#extract "w" from every columns that contain it.
names(KLoSA) <- gsub(x = names(KLoSA), pattern = "w", replacement = "")  
#First column is ID and second is WAVE
colnames(KLoSA)[1] <- "id"
colnames(KLoSA)[2] <- "wave"
#convert values to integer/numeric; this is to use PanelMatch function.
KLoSA$id <- as.integer(KLoSA$id)
KLoSA$wave <- as.numeric(KLoSA$wave)

#remove non-responses
KLoSA <- KLoSA[!is.na(KLoSA$A002_age),]
table(KLoSA$wave)

#make dummy variable for working status
KLoSA$working <- ifelse(KLoSA$present_labor == 1, 1, 0)
KLoSA$notworking <- ifelse(KLoSA$present_labor == 1, 0, 1)

#calculate the total number of working.
KLoSA <- 
  KLoSA %>%
  group_by(id) %>%
  dplyr::mutate(total_working = sum(working, na.rm = TRUE))

#Exclude individuals without any working history.
KLoSA <- KLoSA[!KLoSA$total_working == 0,] #5223, 34110
#Include individuals with baseline age >60.
KLoSA <- KLoSA[KLoSA$A002_age > 60,] #4070,19232
#(5223 - 4070)/5223
#Exclude individuals without cognitive score, MMSE.
KLoSA <- KLoSA[!is.na(KLoSA$mmse),]  #4033, 18472
#(4070-4033)/4070

#Create an indicator whether one participated five consecutive waves.
KLoSA <- 
  KLoSA %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(id) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 5) 

#Select only individuals with five consecutive waves participation.
KLoSA <- 
  KLoSA %>% 
  group_by(id, grp) %>% 
  filter(n() >= 5) 
#1872, 12603
#(4033-1872)/4033

KLoSA$age <- as.numeric(KLoSA$A002_age)
#Reverse the order of self-reported health
#with C001 1 is Excellent and 5 is ppor, we reverse the order so that 5 is excellent and1 is poor.
KLoSA$health <- 6 - KLoSA$C001
#asset
KLoSA$asset <- as.numeric(KLoSA$hhnetassets)
#income
KLoSA$income <- as.numeric(KLoSA$hhinc)
#gender
KLoSA$female <- ifelse(KLoSA$gender1 == 5, 1, 0)
#eduation is discrete value from 1 to 4, 'up to elementary school' to 'college or more'.
#age cohort, we cut at birth year 1945.
KLoSA$agecohort <- ifelse(KLoSA$A002y <= 1945, 1, 0)
#spouse/partner
KLoSA$spouse <- ifelse(KLoSA$marital ==1, 1, 0)
#occupation
KLoSA$job <- as.numeric(KLoSA$job)
KLoSA$jobcategory[KLoSA$job == -9] <- NA
KLoSA$jobcategory[KLoSA$job == -8] <- NA

#Occupation skill classification according to International Standard Classification of Occupations 
#(https://ilostat.ilo.org/resources/concepts-and-definitions/classification-occupation/)
#Categorize the occupation by the level of complexity.
#High skill
KLoSA$jobcategory[KLoSA$job == 1 |KLoSA$job == 2 ] <- 3
#Medium (+1 case of armed force)
KLoSA$jobcategory[KLoSA$job >= 4 & KLoSA$job <= 8 | KLoSA$job == 10] <- 2
#Low skill
KLoSA$jobcategory[KLoSA$job == 9] <- 1

#Due to the large missingness of occupation, we use last observation carried forward method to complement the missing data.
library(zoo)
library(dplyr)
KLoSA <- KLoSA %>% 
  group_by(id) %>%
  arrange(wave) %>%
  dplyr::mutate(job.locf = na.locf(job, na.rm = FALSE))  

KLoSA$job.locf <- as.numeric(KLoSA$job.locf)
KLoSA$job.locfcategory[KLoSA$job.locf == -9] <- NA
KLoSA$job.locfcategory[KLoSA$job.locf == -8] <- NA
#High skill
KLoSA$job.locfcategory[KLoSA$job.locf == 1 |KLoSA$job.locf == 2 ] <- 3
#Medium (+1 case of armed force)
KLoSA$job.locfcategory[KLoSA$job.locf >= 4 & KLoSA$job.locf <= 8 | KLoSA$job.locf == 10] <- 2
#Low skill
KLoSA$job.locfcategory[KLoSA$job.locf == 9] <- 1

#financial values, Inflation/Purchase Purchasing power parity adjustment for income/wealth variables
#inflation data from World bank (https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG)
#Purchase power parity from world bank (https://data.worldbank.org/indicator/PA.NUS.PPP)
library(readxl)
inflation <- read_excel("API_FP.CPI.TOTL.ZG_DS2_en_excel_v2_4692956.xls")
ppp <- read_excel("API_PA.NUS.PPP_DS2_en_excel_v2_4684852.xls")
library(tidyverse)

names(inflation) <- inflation  %>% slice(3) %>% unlist()
inflation <- inflation[4:nrow(inflation),]
korea_inflation<- inflation[inflation$`Country Code` == "KOR",]
korea_inflation<- korea_inflation[,which( colnames(inflation)=="2006"):which( colnames(inflation)=="2020")]
korea_inflation <- data.frame(t(korea_inflation))
korea_inflation$year <- row.names(korea_inflation)
colnames(korea_inflation)[colnames(korea_inflation) == 't.korea_inflation.'] <- 'inflation'
korea_inflation$inflation <- as.numeric(korea_inflation$inflation)
korea_inflation$year <- as.numeric(korea_inflation$year)

names(ppp) <- ppp %>% slice(3) %>% unlist()
ppp <- ppp[4:nrow(ppp),]
korea_ppp<- ppp[ppp$`Country Code` == "KOR",]
korea_ppp<- korea_ppp[,which( colnames(ppp)=="2006"):which( colnames(ppp)=="2020")]
korea_ppp <- data.frame(t(korea_ppp))
korea_ppp$year <- row.names(korea_ppp)
colnames(korea_ppp)[colnames(korea_ppp) == 't.korea_ppp.'] <- 'ppp'
korea_ppp$ppp <- as.numeric(korea_ppp$ppp)
korea_ppp$year <- as.numeric(korea_ppp$year)

#inflation and ppp contained data for Korea.
korea_inflation <- left_join(korea_inflation, korea_ppp)

#Calculated cumulated inflation. 
korea_inflation <- korea_inflation %>% 
  mutate(Compound = cumprod(1+inflation/100))
#Select the year corresponding to the biennial wave.
korea_inflation <- korea_inflation[korea_inflation$year == 2006 |korea_inflation$year == 2008 |
                                     korea_inflation$year == 2010|korea_inflation$year == 2012 |
                                     korea_inflation$year == 2014|korea_inflation$year == 2016 | 
                                     korea_inflation$year == 2018|korea_inflation$year == 2020,]
korea_inflation$wave <- (korea_inflation$year - 2004)/2
korea_inflation$wave <- as.integer(korea_inflation$wave)

# Merge the inflation information with the original data
KLoSA <- merge(KLoSA, korea_inflation)
#Adjust the asset, income variables using the cumulative inflation.
KLoSA$inflation_asset <-  KLoSA$asset * 1/KLoSA$Compound
KLoSA$inflation_income <-  KLoSA$income * 1/KLoSA$Compound
KLoSA$ppp_asset <- KLoSA$inflation_asset*(KLoSA$ppp/100)
KLoSA$ppp_income <- KLoSA$inflation_income*(KLoSA$ppp/100)
#Make financial value into tertile to represent relative economic status.
KLoSA <- KLoSA %>% 
  mutate(asset_quantile = ntile(KLoSA$ppp_asset, 3))
KLoSA$ppp_income[KLoSA$ppp_income <0] <- NA
#divide into 3 qunatiles
KLoSA <- KLoSA %>% 
  mutate(income_quantile = ntile(KLoSA$ppp_income, 3))
#We delete unnecessary columns.
# time variables -> intergers, other values -> numeric 
KLoSA$wave <- as.integer(KLoSA$wave)
KLoSA <- data.frame(KLoSA)

#delete unnecessary data frames.
korea_inflation <- NULL
korea_ppp <- NULL
sub <- NULL
ppp <- NULL

####################################################################################################
## Matching by employment history. KLoSA - Entry
####################################################################################################
library(PanelMatch)
#Matching with employment history (entry) with 3 lags and 1 leads.
Working_KL_Before <- PanelMatch(lag = 3, time.id = "wave", unit.id = "id", 
                                treatment = "working", refinement.method = "none", 
                                data = KLoSA, 
                                size.match = 5, qoi = "att", outcome.var = "mmse",
                                lead = 0:1, forbid.treatment.reversal = FALSE, 
                                match.missing = FALSE, listwise.delete = FALSE,
                                use.diagonal.variance.matrix = TRUE,)
library (plyr)
library(dplyr)
summary <- summary(Working_KL_Before$att)
summary <- data.frame(summary$overview)
summary$id <- as.integer(summary$id)
summary <- summary[summary$matched.set.size !=0,]

descriptive_summary <- ldply(Working_KL_Before$att, data.frame)
descriptive_summary <- descriptive_summary %>% separate(.id, c("id", "wave"))
descriptive_summary$id <- as.integer(descriptive_summary$id)
descriptive_summary$wave <- as.integer(descriptive_summary$wave)
descriptive_summary <- left_join(descriptive_summary, summary, by = c("id", "wave"))
colnames(descriptive_summary)[which(names(descriptive_summary) == "X..i..")] <- "control.id"
colnames(descriptive_summary)[which(names(descriptive_summary) == "id")] <- "case.id"

case <- subset(descriptive_summary, select = c("wave", "case.id", "matched.set.size"))
case <- unique(case)
colnames(case)[which(names(case) == "case.id")] <- "id"
colnames(case)[which(names(case) == "wave")] <- "current.wave"

control <- subset(descriptive_summary, select = c("wave", "control.id", "matched.set.size"))
control <- unique(control)
colnames(control)[which(names(control) == "control.id")] <- "id"
colnames(control)[which(names(control) == "wave")] <- "current.wave"

#measure at previous wave.
case$wave <- case$current.wave -  1
control$wave <- control$current.wave -  1

#merge with KLoSA data.
case_statistics <- left_join(case, KLoSA)
control_statistics <- left_join(control, KLoSA)

####################################################################################################
## Matching by employment history. KLoSA - Exit
####################################################################################################

NotWorking_KL_Before<- PanelMatch(lag = 3, time.id = "wave", unit.id = "id", 
                                  treatment = "notworking", refinement.method = "none", 
                                  data = KLoSA, 
                                  size.match = 5, qoi = "att", outcome.var = "mmse",
                                  match.missing = FALSE, listwise.delete = FALSE,
                                  lead = 0:1, forbid.treatment.reversal = FALSE, 
                                  use.diagonal.variance.matrix = TRUE)


summary <- summary(NotWorking_KL_Before$att)
summary <- data.frame(summary$overview)
summary$id <- as.integer(summary$id)
summary <- summary[summary$matched.set.size !=0,]

descriptive_summary <- ldply(NotWorking_KL_Before$att, data.frame)
descriptive_summary <- descriptive_summary %>% separate(.id, c("id", "wave"))
descriptive_summary$id <- as.integer(descriptive_summary$id)
descriptive_summary$wave <- as.integer(descriptive_summary$wave)
descriptive_summary <- left_join(descriptive_summary, summary, by = c("id", "wave"))
colnames(descriptive_summary)[which(names(descriptive_summary) == "X..i..")] <- "control.id"
colnames(descriptive_summary)[which(names(descriptive_summary) == "id")] <- "case.id"

case <- subset(descriptive_summary, select = c("wave", "case.id", "matched.set.size"))
case <- unique(case)
colnames(case)[which(names(case) == "case.id")] <- "id"
colnames(case)[which(names(case) == "wave")] <- "current.wave"

control <- subset(descriptive_summary, select = c("wave", "control.id", "matched.set.size"))
control <- unique(control)
colnames(control)[which(names(control) == "control.id")] <- "id"
colnames(control)[which(names(control) == "wave")] <- "current.wave"

#measure at previous wave.
case$wave <- case$current.wave -  1
control$wave <- control$current.wave -  1

#merge with KLoSA data.
Exit_case_statistics <- left_join(case, KLoSA)
Exit_control_statistics <- left_join(control, KLoSA)

Exit_case_statistics$group <- "3. Exit"
Exit_control_statistics$group <- "4. Active"
case_statistics$group <- "1. Entry"
control_statistics$group <- "2. Inactive"

transition_data <- rbind(Exit_case_statistics, Exit_control_statistics, case_statistics, control_statistics)
#delete the non-matched data.
transition_data <- transition_data[transition_data$matched.set.size !=0,]

#Transform the variable for the descriptive statistics
transition_data$job.locfcategory <- as.factor(transition_data$job.locfcategory)
transition_data$female <- as.factor(transition_data$female)
transition_data$income_quantile <- as.factor(transition_data$income_quantile)
transition_data$asset_quantile <- as.factor(transition_data$asset_quantile)
transition_data$age_category <- case_when(transition_data$age < 65 ~ "-64",
                               transition_data$age >= 65 & transition_data$age < 70 ~ "65-69",
                               transition_data$age >= 70 & transition_data$age < 75 ~ "70-74",
                               transition_data$age >= 75 & transition_data$age < 80 ~ "75-79",
                               transition_data$age >= 80 & transition_data$age < 85 ~ "80-84",
                               transition_data$age >= 85  ~ "85-")
transition_data$agecohort <- as.factor(transition_data$agecohort)
transition_data$Education  <- as.factor(transition_data$edu)
transition_data$spouse <- as.factor(transition_data$spouse)
transition_data$health <- as.factor(transition_data$health)
transition_data <- subset(transition_data, select = c("mmse", "age", "age_category", "agecohort", "female", "Education", "spouse", "asset_quantile", 
                                "income_quantile", "job.locfcategory", "health", "group"))                        

colnames(transition_data) <- c("1.MMSE", "2.Age", "3.Age category", "4.Age cohort", "5.Female", "6.Education", "7.Spouse", "8.Household Asset", 
                    "8.Household Income", "9.Job category", "10.Health", "group")

library(compareGroups)
comparison <- compareGroups(group ~ ., data = transition_data)
comparison_table <- createTable(comparison, show.n = TRUE)
comparison_table
export2latex(comparison_table)







