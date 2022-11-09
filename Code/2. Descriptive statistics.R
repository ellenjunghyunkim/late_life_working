####################################################################################################
## project: = Does (Re-)Entering the Labor Market at Advanced Ages Protect Against 
#~ Cognitive Decline? A Panel-Matching Difference-in-differences Approach
## author(s): Jung Hyun Kim
## code started: May, 2021
## last update: November, 2022
####################################################################################################

####################################################################################################
## Descriptive Statistics of HRS and KLoSA
####################################################################################################

#Harmonize the column name
#For KLoSA data

KLoSA$assets <- KLoSA$ppp_asset
KLoSA$assets <- KLoSA$ppp_income
KLoSA$cognitivescore <- KLoSA$mmse

KLoSA$ethnicity <- "NA"
KLoSA$foreignbirth <- "NA"
#Select the common variable.
keep <- c('working','id', 'edu', 'health', 'cognitivescore','female', 'spouse','age',
          'assets','incomes', 'jobcategory', "agecohort", "wave", "ethnicity", "foreignbirth")
data2 <- KLoSA
data2 <- KLoSA[keep]
data2$group <- "KLoSA"

#For HRS data
HRS$id <- HRS$hhidpn
HRS$assets <- HRS$inflation_asset
HRS$incomes <- HRS$inflation_income
HRS$cognitivescore <- HRS$cogtot27

HRS$ethnicity[HRS$raracem == 1 & HRS$rahispan == 0] <- "Non Hispanic White"
HRS$ethnicity[HRS$raracem == 2 & HRS$rahispan == 0] <- "Non Hispanic Black"
HRS$ethnicity[HRS$raracem ==3 & HRS$rahispan == 0 ] <- "Non Hispanic Others"
HRS$ethnicity[HRS$rahispan == 1] <- "Hispanic"
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


####################################################################################################
## Descriptive Statistics by Labor Force Transition in KLoSA
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


####################################################################################################
## Descriptive Statistics by Labor Force Transition in HRS
####################################################################################################

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
