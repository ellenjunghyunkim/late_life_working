####################################################################################################
## project: = Does (Re-)Entering the Labor Market at Advanced Ages Protect Against 
#~ Cognitive Decline? A Panel-Matching Difference-in-differences Approach
## author(s): Jung Hyun Kim
## code started: May, 2021
## last update: November, 2022
####################################################################################################

#Harmonize the column name
#For KLoSA data
names(KLoSA)[names(KLoSA) == "ppp_asset"] <- "assets"
names(KLoSA)[names(KLoSA) == "ppp_income"] <- "incomes"
names(KLoSA)[names(KLoSA) == "mmse"] <- "cognitivescore"
KLoSA$ethnicity <- "NA"
KLoSA$foreignbirth <- "NA"
#Select the common variable.
keep <- c('working','id', 'edu', 'health', 'cognitivescore','female', 'spouse','age',
          'assets','incomes', 'jobcategory', "agecohort", "wave", "ethnicity", "foreignbirth")
data2 <- KLoSA
data2 <- KLoSA[keep]
data2$group <- "KLoSA"

#For HRS data
names(HRS)[names(HRS) == "hhidpn"] <- "id"
names(HRS)[names(HRS) == "inflation_asset"] <- "assets"
names(HRS)[names(HRS) == "inflation_income"] <- "incomes"
names(HRS)[names(HRS) == "cogtot27"] <- "cognitivescore"
HRS$ethnicity[HRS$raracem == 1 & HRS$rahispan == 0] <- "Non Hispanic White"
HRS$ethnicity[HRS$raracem == 2 & HRS$rahispan == 0] <- "Non Hispanic Black"
HRS$ethnicity[HRS$raracem ==3 & HRS$rahispan == 0 ] <- "Non Hispanic Others"
HRS$ethnicity[HRS$rahispan == 1] <- "Hispanic"
HRS$foreignbirth <-HRS$foreignbirth
data1 <- HRS[keep]
data1$group <- "HRS"

#Merge the two dataset.
KLoSA_HRS <- rbind(data1, data2)
KLoSA_HRS$health <- as.factor(KLoSA_HRS$health)
KLoSA_HRS$foreignbirth <- as.numeric(KLoSA_HRS$foreignbirth)
KLoSA_HRS$edu <- as.factor(KLoSA_HRS$edu)
KLoSA_HRS$jobcategory <- as.factor(KLoSA_HRS$jobcategory)
#Create age category
KLoSA_HRS$agecategory[KLoSA_HRS$age >= 85] <- "85-"
KLoSA_HRS$agecategory[KLoSA_HRS$age >= 75 & KLoSA_HRS$age < 85] <- "75-79"
KLoSA_HRS$agecategory[KLoSA_HRS$age >= 70 & KLoSA_HRS$age < 75] <- "70-74"
KLoSA_HRS$agecategory[KLoSA_HRS$age >= 65 & KLoSA_HRS$age < 70] <- "65-69"
KLoSA_HRS$agecategory[KLoSA_HRS$age < 65] <- "-64"

#Extract the study entry regardless of the wave.
firstobservation <-  KLoSA_HRS  %>%
  group_by(id) %>%
  top_n(1, -wave)

library(stringi)
library(compareGroups)

firstobservation$id<- NULL
firstobservation$wave <- NULL
firstobservation$female <- as.factor(firstobservation$female)
firstobservation$spouse <- as.factor(firstobservation$spouse)
firstobservation$agecohort <- as.factor(firstobservation$agecohort)
firstobservation$foreignbirth <- as.factor(firstobservation$foreignbirth)

comparison <- compareGroups(group ~ ., data = firstobservation)
comparison_table <- createTable(comparison, show.n = TRUE)
comparison_table

#Turn into Latex form.
export2latex(comparison_table)