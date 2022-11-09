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
## Download the RAND HRS file and merge with HRS core 2020
####################################################################################################
#RAND HRS file
HRS<-read_dta("randhrs1992_2018v2_STATA/HRS_data.dta")
#HRS 2020 core file
cognition<-  read_dta("h20sta/cogtition.dta")
age_proxy <-  read_dta("h20sta/age_proxy.dta")
work_status <-  read_dta("h20sta/work_status.dta")

#merge the core files
core2020 <- left_join(cognition, age_proxy)
core2020 <- left_join(core2020, work_status)

#create the cognition score from HRS 2020 file.
#immediate recall
core2020$immediate <- case_when(!is.na(core2020$RD174) ~ core2020$RD174 ,
                                is.na(core2020$RD174) ~ core2020$RD174W )

#delay recall
core2020$delayed <- case_when(!is.na(core2020$RD184) ~ core2020$RD184 ,
                              is.na(core2020$RD184) ~ core2020$RD184W )
#immedaite and delay recall
core2020$rtr20 <- core2020$immediate + core2020$delayed 
#backward counting
core2020$rbwc20  <- case_when(core2020$RD124 ==1 ~ 2,
                              core2020$RD129 ==1 ~ 1,
                              (core2020$RD124 ==5 |core2020$RD124 ==6|core2020$RD124 ==9)& (core2020$RD129 ==5 |core2020$RD129 ==9) ~ 0,)
#serial 7 subtration
core2020$ser1 <- ifelse(core2020$RD142 == 93, 1, 0)
core2020$ser2<- ifelse(core2020$RD143 == 86, 1, 0)
core2020$ser3<- ifelse(core2020$RD144 == 79, 1, 0)
core2020$ser4<- ifelse(core2020$RD145 == 72, 1, 0)
core2020$ser5<- ifelse(core2020$RD146 == 65, 1, 0)
core2020$rser7 <- rowSums(core2020[,which( colnames(core2020)=="ser1" ):which( colnames(core2020)=="ser5" )], na.rm = TRUE)

#this step is to harmonize with other waves.
sub <- subset(core2020, select = c("rtr20", "rbwc20", "rser7"))
core2020$cogtot27 <- rowSums(sub)
core2020$wave <- 15
colnames(core2020)[which(names(core2020) == "RA019")] <- "ragey_e" #current age
colnames(core2020)[which(names(core2020) == "RA009")] <- "rproxy" #proxy status
colnames(core2020)[which(names(core2020) == "RZ123")] <- "rwork"
#subset the needed variable; current age, current proxy status, current working status, current cognition score.
core2020 <- subset(core2020, select = c("hhidpn", "wave", "ragey_e", "rproxy", "rwork", "cogtot27"))
core2020$rproxy <- ifelse(core2020$rproxy == 1, 0, 1)
core2020$rwork <- ifelse(core2020$rwork == 1, 1, 0)

#Subset the data from large RAND HRS file.
HRS <- HRS %>%
  select(hhidpn, wave, ragey_e, rproxy, ragender, raracem, rahispan, rabplace, raedyrs, 
         rwork, rslfmem, hatotb, hitot, rtr20, rbwc20, rser7, rshlt, 
         rjcoccb, rmstat, rabyear)

sub <- subset(HRS, select = c("rtr20", "rbwc20", "rser7"))
HRS$cogtot27 <- rowSums(sub)

a1 <- subset(HRS, select = c("hhidpn", "wave", "cogtot27", "ragey_e", "rproxy", "rwork"))
b1 <- subset(core2020, select = c("hhidpn", "wave", "cogtot27", "ragey_e", "rproxy", "rwork"))
#create a data frame that contains both RAND HRS and HRS core cognition score.
c1 <- rbind(a1, b1)
#and merge to the RAND HRS file.
HRS <- left_join(c1, HRS)

####################################################################################################
## Clean the variables & create final analytical sample
####################################################################################################
#We assume that the items below doesn't change over waves so we carry the value forward.
library(zoo)
library(dplyr)
#race/ethnicity, sex/gender, education years, birth place, birth year.
sel <- c("rahispan","raracem", "ragender", "raedyrs", "rabplace", "rabyear")
HRS <- HRS %>% 
  group_by(hhidpn) %>%
  arrange(wave) %>%
  dplyr::mutate(across(all_of(sel), ~ na.locf(.x, na.rm = FALSE), .names = "{col}"))  

#We focus from wave 8 to 15.
HRS <- HRS[HRS$wave >= 8,]
#remove non-responses
HRS <- HRS[!is.na(HRS$ragey_e),] 
table(HRS$wave)

#make dummy variable for working status
HRS$working <- as.numeric(HRS$rwork)
HRS$notworking <- ifelse(HRS$working == 0, 1, 0)

#calculate the total number of working.
HRS <- 
  HRS %>%
  group_by(hhidpn) %>%
  dplyr::mutate(total_working = sum(rwork, na.rm = TRUE))

#Exclude individuals without any working history.
HRS <- HRS[!HRS$total_working == 0,]
#Include individuals with baseline age >60.
HRS <- HRS[HRS$ragey_e > 60,]
#10674,45987
#(16636 - 10674)/16636
#Exclude individuals without cognitive score, cogtot27.
HRS <- HRS[!is.na(HRS$cogtot27),]
# 10212, 41811
# (10674-10212)/10674

#Exclude individuals without working status, race/ethnic information, birth place.
HRS <- HRS[!is.na(HRS$working),]
HRS <- HRS[!is.na(HRS$raracem),]
HRS <- HRS[!is.na(HRS$rahispan),]
HRS <- HRS[!is.na(HRS$rabplace),]
# 10159 41610
# (10212 - 10159)/10212

#Create an indicator whether one participated five consecutive waves.
HRS <- 
  HRS %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(hhidpn) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 5) 

#Select only individuals with five consecutive waves participation.
HRS <- 
  HRS %>% 
  group_by(hhidpn, grp) %>% 
  filter(n() >= 5)  #4070, 26297
# (10159 - 4070)/10159

HRS$age <- as.numeric(HRS$ragey_e)
#Reverse the order of self-reported health
#shlt gives 1 as excellent 2. Very good 3. Good 4. Fair, 5 poor.
HRS$health <- 6 - HRS$rshlt
#asset
HRS$asset <- as.numeric(HRS$hatotb)
#income
HRS$income <- as.numeric(HRS$hitot)
#gender
HRS$female <- ifelse(HRS$ragender ==2, 1, 0)
#education categorize education harmonizing with HRS
HRS$edu[HRS$raedyrs <= 6] <- 1
HRS$edu[HRS$raedyrs <= 9 & HRS$raedyrs > 6] <- 2
HRS$edu[HRS$raedyrs <= 12 & HRS$raedyrs > 9] <- 3
HRS$edu[HRS$raedyrs >12] <- 4
#age cohort, we cut at birth year 1945.
HRS$agecohort <- ifelse(HRS$rabyear < 1945, 1, 0)
#spouse/partner
HRS$spouse <- ifelse(HRS$rmstat == 1|HRS$rmstat == 3, 1, 0)

#occupation
#Occupation skill classification taken into two sources below.
#(https://ilostat.ilo.org/resources/concepts-and-definitions/classification-occupation/)
#https://www.bls.gov/ooh/home.htm
#Categorize the occupation by the level of complexity.
#High Skill
HRS$jobcategory[HRS$rjcoccb >= 1 &  HRS$rjcoccb <= 6|HRS$rjcoccb >= 8 &  HRS$rjcoccb <= 11] <- 3
#Medium (+ arm force)
HRS$jobcategory[(HRS$rjcoccb >= 12 & HRS$rjcoccb <= 14) | (HRS$rjcoccb >= 16 & HRS$rjcoccb <= 18) |
                  HRS$rjcoccb ==7| HRS$rjcoccb >= 22 & HRS$rjcoccb <= 23|HRS$rjcoccb ==25] <- 2
#Low skill
HRS$jobcategory[HRS$rjcoccb >= 19 & HRS$rjcoccb <= 21 | HRS$rjcoccb ==24] <- 1

#Due to the large missingness of occupation, we use last observation carried forward method to complement the missing data.
library(zoo)
library(dplyr)
HRS <- HRS %>% 
  group_by(hhidpn) %>%
  arrange(wave) %>%
  dplyr::mutate(job.locf = na.locf(rjcoccb, na.rm = FALSE))  

HRS$job.locfcategory[HRS$job.locf >= 1 &  HRS$job.locf <= 6|HRS$job.locf >= 8 &  HRS$job.locf <= 11] <- 3
#Medium (+ arm force)
HRS$job.locfcategory[(HRS$job.locf >= 12 & HRS$job.locf <= 14) | (HRS$job.locf >= 16 & HRS$job.locf <= 18) |
                       HRS$job.locf ==7| HRS$job.locf >= 22 & HRS$job.locf <= 23|HRS$job.locf ==25] <- 2
#Low skill
HRS$job.locfcategory[HRS$job.locf >= 19 & HRS$job.locf <= 21 | HRS$job.locf ==24] <- 1

#financial values, Inflation/Purchase Purchasing power parity adjustment for income/wealth variables
#inflation data from World bank (https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG)
us_inflation<- inflation[inflation$`Country Code` == "USA",]
us_inflation<- us_inflation[,which( colnames(inflation)=="2006"):which( colnames(inflation)=="2020")]
us_inflation <- data.frame(t(us_inflation))
us_inflation$year <- row.names(us_inflation)
colnames(us_inflation)[colnames(us_inflation) == 't.us_inflation.'] <- 'inflation'
us_inflation$inflation <- as.numeric(us_inflation$inflation)
us_inflation$year <- as.numeric(us_inflation$year)

#Calculated cumulated inflation. 
us_inflation <- us_inflation %>% 
  mutate(Compound = cumprod(1+inflation/100))
#Select the year corresponding to the biennial wave.
us_inflation <- us_inflation[us_inflation$year == 2006 |us_inflation$year == 2008 |
                               us_inflation$year == 2010|us_inflation$year == 2012 |
                               us_inflation$year == 2014|us_inflation$year == 2016 | 
                               us_inflation$year == 2018|us_inflation$year == 2020,]

us_inflation$wave <- (us_inflation$year - 1990)/2
us_inflation$wave <- as.integer(us_inflation$wave)
# Merge the inflation information with the original data
HRS <- merge(HRS, us_inflation)

#Adjust the asset, income variables using the cumulative inflation.
HRS$inflation_asset <-  HRS$asset * 1/HRS$Compound
HRS$inflation_income <-  HRS$income * 1/HRS$Compound
#divide into 3 qunatiles
HRS <- HRS %>% 
  mutate(asset_quantile = ntile(HRS$inflation_asset, 3))
HRS <- HRS %>% 
  mutate(income_quantile = ntile(HRS$inflation_income, 3))
HRS$income_quantile[HRS$inflation_income <0] <- NA

#delete unnecessary data frames.
us_inflation <- NULL
sub <- NULL
####################################################################################################
## Specific to US data
####################################################################################################
#Foreign brith dummy
HRS$foreignbirth <- ifelse(HRS$rabplace == 11, 1, 0)
#Ethnicity/Race dummy
HRS$nonhispwhite <- ifelse(HRS$raracem == 1 & HRS$rahispan == 0, 1, 0)
HRS$nonhispblack <- ifelse(HRS$raracem == 2 & HRS$rahispan == 0, 1, 0)
HRS$other <- ifelse(HRS$raracem ==3 & HRS$rahispan == 0, 1, 0)
HRS$hispanic <- ifelse(HRS$rahispan == 1, 1, 0)

#make the variables readable to PanelMatch package and clean environment
HRS$wave <- as.integer(HRS$wave)
HRS$hhidpn <- as.integer(HRS$hhidpn)
HRS <- data.frame(HRS)

####################################################################################################
## Matching by employment history. HRS - Entry
####################################################################################################
library(PanelMatch)
#Matching with employment history (entry) with 3 lags and 1 leads.
Working_HRS_Before <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                                 treatment = "rwork", refinement.method = "none", 
                                 data = HRS, 
                                 size.match = 5, qoi = "att", outcome.var = "cogtot27",
                                 lead = 0:1, forbid.treatment.reversal = FALSE, 
                                 match.missing = FALSE, listwise.delete = FALSE,
                                 use.diagonal.variance.matrix = TRUE)

library (plyr)
library(dplyr)
summary <- summary(Working_HRS_Before$att)
summary <- data.frame(summary$overview)
summary$hhidpn <- as.integer(summary$hhidpn)
summary <- summary[summary$matched.set.size !=0,]

descriptive_summary <- ldply(Working_HRS_Before$att, data.frame)
descriptive_summary <- descriptive_summary %>% separate(.id, c("hhidpn", "wave"))
descriptive_summary$hhidpn <- as.integer(descriptive_summary$hhidpn)
descriptive_summary$wave <- as.integer(descriptive_summary$wave)
descriptive_summary <- left_join(descriptive_summary, summary, by = c("hhidpn", "wave"))
colnames(descriptive_summary)[which(names(descriptive_summary) == "X..i..")] <- "control.id"
colnames(descriptive_summary)[which(names(descriptive_summary) == "hhidpn")] <- "case.id"

case <- subset(descriptive_summary, select = c("wave", "case.id", "matched.set.size"))
case <- unique(case)
colnames(case)[which(names(case) == "case.id")] <- "hhidpn"
colnames(case)[which(names(case) == "wave")] <- "current.wave"

control <- subset(descriptive_summary, select = c("wave", "control.id", "matched.set.size"))
control <- unique(control)
colnames(control)[which(names(control) == "control.id")] <- "hhidpn"
colnames(control)[which(names(control) == "wave")] <- "current.wave"

#measure at previous wave.
case$wave <- case$current.wave -  1
control$wave <- control$current.wave -  1

#merge with HRS data.
case_statistics <- left_join(case, HRS)
control_statistics <- left_join(control, HRS)

####################################################################################################
## Matching by employment history. HRS - Exit
####################################################################################################
NotWorking_HRS_Before <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                                    treatment = "notworking", refinement.method = "none", 
                                    data = HRS, 
                                    size.match = 5, qoi = "att", outcome.var = "cogtot27",
                                    lead = 0:1, forbid.treatment.reversal = FALSE, 
                                    match.missing = FALSE, listwise.delete = FALSE,
                                    use.diagonal.variance.matrix = TRUE)

summary <- summary(NotWorking_HRS_Before$att)
summary <- data.frame(summary$overview)
summary$hhidpn <- as.integer(summary$hhidpn)
summary <- summary[summary$matched.set.size !=0,]

descriptive_summary <- ldply(NotWorking_HRS_Before$att, data.frame)
descriptive_summary <- descriptive_summary %>% separate(.id, c("hhidpn", "wave"))
descriptive_summary$hhidpn <- as.integer(descriptive_summary$hhidpn)
descriptive_summary$wave <- as.integer(descriptive_summary$wave)
descriptive_summary <- left_join(descriptive_summary, summary, by = c("hhidpn", "wave"))
colnames(descriptive_summary)[which(names(descriptive_summary) == "X..i..")] <- "control.id"
colnames(descriptive_summary)[which(names(descriptive_summary) == "hhidpn")] <- "case.id"

case <- subset(descriptive_summary, select = c("wave", "case.id", "matched.set.size"))
case <- unique(case)
colnames(case)[which(names(case) == "case.id")] <- "hhidpn"
colnames(case)[which(names(case) == "wave")] <- "current.wave"

control <- subset(descriptive_summary, select = c("wave", "control.id", "matched.set.size"))
control <- unique(control)
colnames(control)[which(names(control) == "control.id")] <- "hhidpn"
colnames(control)[which(names(control) == "wave")] <- "current.wave"

#measure at previous wave.
case$wave <- case$current.wave -  1
control$wave <- control$current.wave -  1

#merge with HRS data.
Exit_case_statistics <- left_join(case, HRS)
Exit_control_statistics <- left_join(control, HRS)

Exit_case_statistics$group <- "3. Exit"
Exit_control_statistics$group <- "4. Active"
case_statistics$group <- "1. Entry"
control_statistics$group <- "2. Inactive"

transition_data <- rbind(Exit_case_statistics, Exit_control_statistics, case_statistics, control_statistics)
#delete the non-matched data.
transition_data <- transition_data[transition_data$matched.set.size !=0,]

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

#foreign birth
transition_data$foreignbirth <- as.factor(transition_data$foreignbirth)
#Ethnicity/Race dummy
transition_data$ethnicity <- case_when(transition_data$nonhispwhite == 1 ~ "Non-Hispanic white",
                            transition_data$nonhispblack == 1 ~ "Non-Hispanic black",
                            transition_data$hispanic == 1 ~ "Hispanic",
                            transition_data$other == 1 ~ "Non-Hispanic other",)

transition_data <- subset(transition_data, select = c("cogtot27", "age", "age_category", "agecohort", "female", "Education", "spouse", "asset_quantile", 
                                "income_quantile", "job.locfcategory", "health", "group",  "ethnicity", "foreignbirth"))                        

colnames(transition_data) <- c("1.cogtot27", "2.Age", "3.Age category", "4.Age cohort", "5.Female", "6.Education", "7.Spouse", "8.Household Asset", 
                    "8.Household Income", "9.Job category", "10.Health", "group", "11.Race/Ethnicity", "12.Foreign birh")

library(compareGroups)
comparison <- compareGroups(group ~ ., data = transition_data)
comparison_table <- createTable(comparison, show.n = TRUE)
comparison_table
export2latex(comparison_table)

















