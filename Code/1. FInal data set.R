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
KLoSA <- read_dta("KLoSA_STATA_2022v3/KLoSA_data.dta")
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