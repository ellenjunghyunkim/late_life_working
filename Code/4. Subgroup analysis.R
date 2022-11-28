####################################################################################################
## project: = Does (Re-)Entering the Labor Market at Advanced Ages Protect Against 
#~ Cognitive Decline? A Panel-Matching Difference-in-differences Approach
## author(s): Jung Hyun Kim
## code started: May, 2021
## last update: November, 2022
####################################################################################################
library(PanelMatch)
####################################################################################################
## Moderator analysis (Asset, Education, Gender)
####################################################################################################
## Asset
####################################################################################################
#Supgroup analyses by baseline asset level.

firstobservation <- HRS  %>%
  group_by(hhidpn) %>%
  top_n(1, -wave)
firstobservation <- firstobservation %>% 
  mutate(asset_moderator = ntile(firstobservation$inflation_asset, 2))
summary(firstobservation$inflation_asset)
firstobservation <- subset(firstobservation, select = c("hhidpn", "asset_moderator"))
HRS <- left_join(HRS, firstobservation)

Working_HRS_Asset_Before  <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                                 treatment = "working", refinement.method = "none", 
                                 data = HRS, 
                                 covs.formula = ~ female + age + I(age^2) + agecohort + edu2 + edu3 + edu4 
                                 + I(lag(health2, 1:3))+ I(lag(health3, 1:3))+ I(lag(health4, 1:3))+ I(lag(health5, 1:3))
                                 + I(lag(job.locfcategory2, 1:3))+ I(lag(job.locfcategory3, 1:3))
                                 + I(lag(income_quantile2, 1:3))+ I(lag(income_quantile3, 1:3)) + I(lag(cogtot27, 1:3))
                                 + nonhispblack + other + hispanic + foreignbirth,
                                 size.match = 5, qoi = "att", outcome.var = "cogtot27",
                                 lead = 0:1, forbid.treatment.reversal = FALSE, match.missing = FALSE, listwise.delete = FALSE,
                                 use.diagonal.variance.matrix = TRUE)

Working_HRS_Asset  <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                                 treatment = "working", refinement.method = "CBPS.weight", 
                                 data = HRS, 
                                 covs.formula = ~ female + age + I(age^2) + agecohort + edu2 + edu3 + edu4 
                                 + I(lag(health2, 1:3))+ I(lag(health3, 1:3))+ I(lag(health4, 1:3))+ I(lag(health5, 1:3))
                                 + I(lag(job.locfcategory2, 1:3))+ I(lag(job.locfcategory3, 1:3))
                                 + I(lag(income_quantile2, 1:3))+ I(lag(income_quantile3, 1:3)) + I(lag(cogtot27, 1:3))
                                 + nonhispblack + other + hispanic + foreignbirth,
                                 size.match = 5, qoi = "att", outcome.var = "cogtot27",
                                 lead = 0:1, forbid.treatment.reversal = FALSE, match.missing = FALSE, listwise.delete = FALSE,
                                 use.diagonal.variance.matrix = TRUE)

NotWorking_HRS_Asset_Before  <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                                    treatment = "notworking", refinement.method = "none", 
                                    data = HRS, 
                                    covs.formula = ~ female + age + I(age^2) + agecohort + edu2 + edu3 + edu4 
                                    + I(lag(health2, 1:3))+ I(lag(health3, 1:3))+ I(lag(health4, 1:3))+ I(lag(health5, 1:3))
                                    + I(lag(job.locfcategory2, 1:3))+ I(lag(job.locfcategory3, 1:3))
                                    + I(lag(income_quantile2, 1:3))+ I(lag(income_quantile3, 1:3)) + I(lag(cogtot27, 1:3))
                                    + nonhispblack + other + hispanic + foreignbirth,
                                    size.match = 5, qoi = "att", outcome.var = "cogtot27",
                                    lead = 0:1, forbid.treatment.reversal = FALSE, match.missing = FALSE, listwise.delete = FALSE,
                                    use.diagonal.variance.matrix = TRUE)

NotWorking_HRS_Asset  <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                                    treatment = "notworking", refinement.method = "CBPS.weight", 
                                    data = HRS, 
                                    covs.formula = ~ female + age + I(age^2) + agecohort + edu2 + edu3 + edu4 
                                    + I(lag(health2, 1:3))+ I(lag(health3, 1:3))+ I(lag(health4, 1:3))+ I(lag(health5, 1:3))
                                    + I(lag(job.locfcategory2, 1:3))+ I(lag(job.locfcategory3, 1:3))
                                    + I(lag(income_quantile2, 1:3))+ I(lag(income_quantile3, 1:3)) + I(lag(cogtot27, 1:3))
                                    + nonhispblack + other + hispanic + foreignbirth,
                                    size.match = 5, qoi = "att", outcome.var = "cogtot27",
                                    lead = 0:1, forbid.treatment.reversal = FALSE, match.missing = FALSE, listwise.delete = FALSE,
                                    use.diagonal.variance.matrix = TRUE)

ATT_Working_HRS_Asset_Before <- PanelEstimate(sets = Working_HRS_Asset_Before, data = HRS, moderator = "asset_moderator")
ATT_NotWorking_HRS_Asset_Before <- PanelEstimate(sets = NotWorking_HRS_Asset_Before, data = HRS, moderator = "asset_moderator")

ATT_Working_HRS_Asset <- PanelEstimate(sets = Working_HRS_Asset, data = HRS, moderator = "asset_moderator")
ATT_NotWorking_HRS_Asset <- PanelEstimate(sets = NotWorking_HRS_Asset, data = HRS, moderator = "asset_moderator")


####################################################################################################
## Education
####################################################################################################

HRS$collegeormore <- ifelse( HRS$edu1 ==1 | HRS$edu2 ==1 | HRS$edu3 == 1, 1, 2)
Working_HRS_Edu <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                              treatment = "working", refinement.method = "CBPS.weight", 
                              data = HRS, 
                              covs.formula = ~ female + age + I(age^2) + agecohort 
                              + I(lag(health2, 1:3))+ I(lag(health3, 1:3))+ I(lag(health4, 1:3))+ I(lag(health5, 1:3))
                              + I(lag(job.locfcategory2, 1:3))+ I(lag(job.locfcategory3, 1:3))
                              + I(lag(asset_quantile2, 1:3)) + I(lag(asset_quantile3, 1:3))
                              + I(lag(income_quantile2, 1:3))+ I(lag(income_quantile3, 1:3)) + I(lag(cogtot27, 1:3))
                              + nonhispblack + other + hispanic + foreignbirth,
                              size.match = 5, qoi = "att", outcome.var = "cogtot27",
                              lead = 0:1, forbid.treatment.reversal = FALSE, match.missing = FALSE, listwise.delete = FALSE,
                              use.diagonal.variance.matrix = TRUE)

NotWorking_HRS_Edu <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                                 treatment = "notworking", refinement.method = "CBPS.weight", 
                                 data = HRS, 
                                 covs.formula = ~ female + age + I(age^2) + agecohort 
                                 + I(lag(health2, 1:3))+ I(lag(health3, 1:3))+ I(lag(health4, 1:3))+ I(lag(health5, 1:3))
                                 + I(lag(job.locfcategory2, 1:3))+ I(lag(job.locfcategory3, 1:3))
                                 + I(lag(asset_quantile2, 1:3)) + I(lag(asset_quantile3, 1:3))
                                 + I(lag(income_quantile2, 1:3))+ I(lag(income_quantile3, 1:3)) + I(lag(cogtot27, 1:3))
                                 + nonhispblack + other + hispanic + foreignbirth,
                                 size.match = 5, qoi = "att", outcome.var = "cogtot27",
                                 lead = 0:1, forbid.treatment.reversal = FALSE, match.missing = FALSE, listwise.delete = FALSE,
                                 use.diagonal.variance.matrix = TRUE)

ATT_NotWorking_HRS_Edu <- PanelEstimate(sets = NotWorking_HRS_Edu, data = HRS, moderator = "collegeormore")
ATT_Working_HRS_Edu <- PanelEstimate(sets = Working_HRS_Edu, data = HRS, moderator = "collegeormore")

####################################################################################################
## Sex/Gender
####################################################################################################

Working_HRS_Gender <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                                 treatment = "working", refinement.method = "CBPS.weight", 
                                 data = HRS, 
                                 covs.formula = ~  age + I(age^2) + agecohort + edu2 + edu3 + edu4 
                                 + I(lag(health2, 1:3))+ I(lag(health3, 1:3))+ I(lag(health4, 1:3))+ I(lag(health5, 1:3))
                                 + I(lag(job.locfcategory2, 1:3))+ I(lag(job.locfcategory3, 1:3))
                                 + I(lag(asset_quantile2, 1:3)) + I(lag(asset_quantile3, 1:3))
                                 + I(lag(income_quantile2, 1:3))+ I(lag(income_quantile3, 1:3)) + I(lag(cogtot27, 1:3))
                                 + nonhispblack + other + hispanic + foreignbirth,
                                 size.match = 5, qoi = "att", outcome.var = "cogtot27",
                                 lead = 0:1, forbid.treatment.reversal = FALSE, match.missing = FALSE, listwise.delete = FALSE,
                                 use.diagonal.variance.matrix = TRUE)


NotWorking_HRS_Gender  <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                                     treatment = "notworking", refinement.method = "CBPS.weight", 
                                     data = HRS, 
                                     covs.formula = ~  age + I(age^2) + agecohort + edu2 + edu3 + edu4 
                                     + I(lag(health2, 1:3))+ I(lag(health3, 1:3))+ I(lag(health4, 1:3))+ I(lag(health5, 1:3))
                                     + I(lag(job.locfcategory2, 1:3))+ I(lag(job.locfcategory3, 1:3))
                                     + I(lag(asset_quantile2, 1:3)) + I(lag(asset_quantile3, 1:3))
                                     + I(lag(income_quantile2, 1:3))+ I(lag(income_quantile3, 1:3)) + I(lag(cogtot27, 1:3))
                                     + nonhispblack + other + hispanic + foreignbirth,
                                     size.match = 5, qoi = "att", outcome.var = "cogtot27",
                                     lead = 0:1, forbid.treatment.reversal = FALSE, match.missing = FALSE, listwise.delete = FALSE,
                                     use.diagonal.variance.matrix = TRUE)

ATT_Working_HRS_Gender <- PanelEstimate(sets = Working_HRS_Gender, data = HRS, moderator = "ragender")
ATT_NotWorking_HRS_Gender <- PanelEstimate(sets = NotWorking_HRS_Gender, data = HRS, moderator = "ragender")



#########################################################################################################################################

#To make the point red and gather the values closer.

plot.PanelEstimate <- function(x, ylab = "Estimated Effect of Treatment", 
                               xlab = "Time", main = "Estimated Effects of Treatment Over Time", ylim = NULL, ...)
{
  
  pe.object <- x
  plot.data <- summary(pe.object, verbose = F, bias.corrected = F)
  if(is.null(ylim))
  {
    ylim <- c(min(plot.data[, 3]) - abs(mean(plot.data[, 3])), max(plot.data[, 4]) + abs(mean(max(plot.data[, 4]))))
  }
  graphics::plot(x = 1:(nrow(plot.data)), y = plot.data[, 1], pch = 16, cex = 1.5, col = "red",
                 xaxt = "n", ylab = ylab, xlab = xlab, main = main, ylim = ylim, ...)
  #graphics::lines(x = 1:(nrow(plot.data)),y = plot.data[, 1], lty = "twodash", col = "blue")
  
  graphics::segments(1:(nrow(plot.data)), plot.data[,3], 1:(nrow(plot.data)), plot.data[,4], col = "black", lty = "solid")
  graphics::abline(h = 0, lty = "dashed")
}

####################################################################################################
## ATT plots 
####################################################################################################
#ATT plots of HRS moderated by asset
par(oma=c(4,4,0,0),mar=c(3,3,2,2),mfrow=c(2,2))

plot.PanelEstimate(ATT_Working_HRS_Asset$`1`,
                   main = "Stay inactive vs  Entering the labor market - Low assets",
                   ylab = "",
                   xlab = "",
                   xlim=c(0.7,2.3),
                   ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))

plot.PanelEstimate(ATT_Working_HRS_Asset$`2`,
                   main = "Stay inactive vs  Entering the labor market - High assets",
                   ylab = "",
                   xlab = "",
                   xlim=c(0.7,2.3),
                   ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))

plot.PanelEstimate(ATT_NotWorking_HRS_Asset$`1`,
                   main = "Stay active vs  Exiting the labor market - Low assets",
                   ylab = "",
                   xlab = "",
                   xlim=c(0.7,2.3),
                   ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))

plot.PanelEstimate(ATT_NotWorking_HRS_Asset$`2`,
                   main = "Stay active vs  Exiting the labor market - High assets",
                   ylab = "",
                   xlab = "",
                   xlim=c(0.7,2.3),
                   ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))

mtext(text="Waves passed after transition of working status in the US",side=1,line=0,outer=TRUE)
mtext(text="Cognitive Score",side=2,line=2,outer=TRUE)
mtext(text="(HRS-TICS)",side=2,line=1,outer=TRUE)
mtext(text="Left : Below median Asset, Right : Above median Asset",side=1,line=1,outer=TRUE)


#########################################################################################################################################
#ATT plots of HRS moderated by education
par(oma=c(4,4,0,0),mar=c(3,3,2,2),mfrow=c(2,2))

plot(ATT_Working_HRS_Edu$`1`,
     main = "Stay inactive vs  Entering the labor market - Up to HS",
     ylab = "",
     xlab = "",
     xlim=c(0.7,2.3),
     ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))

plot(ATT_Working_HRS_Edu$`2`,
     main = "Stay inactive vs  Entering the labor market - Beyond HS",
     ylab = "",
     xlab = "",
     xlim=c(0.7,2.3),
     ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))

plot(ATT_NotWorking_HRS_Edu$`1`,
     main = "Stay active vs  Exiting the labor market - Up to HS",
     ylab = "",
     xlab = "",
     xlim=c(0.7,2.3),
     ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))

plot(ATT_NotWorking_HRS_Edu$`2`,
     main = "Stay active vs  Exiting the labor market - Beyond HS",
     ylab = "",
     xlab = "",
     xlim=c(0.7,2.3),
     ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))

mtext(text="Waves passed after transition of working status in the US",side=1,line=0,outer=TRUE)
mtext(text="Cognitive Score",side=2,line=2,outer=TRUE)
mtext(text="(HRS-TICS)",side=2,line=1,outer=TRUE)
mtext(text="Left : Up to High School, Right : Beyond High School",side=1,line=1,outer=TRUE)

#########################################################################################################################################
#ATT plots of HRS moderated by gender
par(oma=c(4,4,0,0),mar=c(3,3,2,2),mfrow=c(2,2))

plot(ATT_Working_HRS_Gender$`1`,
     main = "Stay inactive vs  Entering the labor market - Men",
     ylab = "",
     xlab = "",
     xlim=c(0.7,2.3),
     ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))

plot(ATT_Working_HRS_Gender$`2`,
     main = "Stay inactive vs  Entering the labor market - Women",
     ylab = "",
     xlab = "",
     xlim=c(0.7,2.3),
     ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))

plot(ATT_NotWorking_HRS_Gender$`1`,
     main = "Stay active vs  Exiting the labor market - Men",
     ylab = "",
     xlab = "",
     xlim=c(0.7,2.3),
     ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))

plot(ATT_NotWorking_HRS_Gender$`2`,
     main = "Stay active vs  Exiting the labor market - Women",
     ylab = "",
     xlab = "",
     xlim=c(0.7,2.3),
     ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))


mtext(text="Waves passed after transition of working status in the US",side=1,line=0,outer=TRUE)
mtext(text="Cognitive Score",side=2,line=2,outer=TRUE)
mtext(text="(HRS-TICS)",side=2,line=1,outer=TRUE)
mtext(text="Left : Men, Right : Women",side=1,line=1,outer=TRUE)