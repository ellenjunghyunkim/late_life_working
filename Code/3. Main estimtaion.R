####################################################################################################
## project: = Does (Re-)Entering the Labor Market at Advanced Ages Protect Against 
#~ Cognitive Decline? A Panel-Matching Difference-in-differences Approach
## author(s): Jung Hyun Kim
## code started: May, 2021
## last update: November, 2022
####################################################################################################
library(PanelMatch)
####################################################################################################
## Matching by employment history. KLoSA - Entry
####################################################################################################
#Before covariate balancing
Working_KL_Before <- PanelMatch(lag = 3, time.id = "wave", unit.id = "id", 
                                treatment = "working", refinement.method = "none", 
                                data = KLoSA, 
                                size.match = 5, qoi = "att", outcome.var = "mmse",
                                lead = 0:1, forbid.treatment.reversal = FALSE, 
                                match.missing = FALSE, listwise.delete = FALSE,
                                use.diagonal.variance.matrix = TRUE,)

ATT_Working_KL_Before <- PanelEstimate(sets = Working_KL_Before, data = KLoSA)

#After covariate balancing
Working_KL <- PanelMatch(lag = 3, time.id = "wave", unit.id = "id", 
                         treatment = "working", refinement.method = "ps.weight", 
                         data = KLoSA,
                         covs.formula = ~ female + age + I(age^2) +  edu + I(lag(asset_quantile, 1:3)) + I(lag(health, 1:3)) 
                         + I(lag(job.locfcategory, 1:3)) + I(lag(spouse,1:3)) + I(lag(income_quantile,1:3)) + I(lag(mmse,1:3)),
                         size.match = 5, qoi = "att", outcome.var = "mmse",
                         lead = 0:1, forbid.treatment.reversal = FALSE, match.missing = FALSE, listwise.delete = FALSE,
                         use.diagonal.variance.matrix = TRUE)

ATT_Working_KL <- PanelEstimate(sets = Working_KL, data = KLoSA)

####################################################################################################
## Matching by employment history. KLoSA - Exit
####################################################################################################
#Before covariate balancing
NotWorking_KL_Before<- PanelMatch(lag = 3, time.id = "wave", unit.id = "id", 
                                  treatment = "notworking", refinement.method = "none", 
                                  data = KLoSA, 
                                  size.match = 5, qoi = "att", outcome.var = "mmse",
                                  match.missing = FALSE, listwise.delete = FALSE,
                                  lead = 0:1, forbid.treatment.reversal = FALSE, 
                                  use.diagonal.variance.matrix = TRUE)

ATT_NotWorking_KL_Before <- PanelEstimate(sets = NotWorking_KL_Before, data = KLoSA)

#After covariate balancing
NotWorking_KL <- PanelMatch(lag = 3, time.id = "wave", unit.id = "id", treatment = "notworking", refinement.method = "ps.weight", 
                            data = KLoSA,
                            covs.formula = ~ female + age + I(age^2) + agecohort + edu + I(lag(asset_quantile, 1:3)) + I(lag(health, 1:3))
                            + I(lag(job.locfcategory, 1:3)) + I(lag(spouse,1:3)) + I(lag(income_quantile,1:3)) + I(lag(mmse,1:3)),
                            size.match = 5, qoi = "att", outcome.var = "mmse",
                            lead = 0:1, forbid.treatment.reversal = FALSE, match.missing = FALSE, listwise.delete = FALSE,
                            use.diagonal.variance.matrix = TRUE)

ATT_NotWorking_KL <- PanelEstimate(sets = NotWorking_KL, data = KLoSA)

####################################################################################################
## Matching by employment history. HRS - Entry
####################################################################################################
#Before covariate balancing
Working_HRS_Before <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                                 treatment = "rwork", refinement.method = "none", 
                                 data = HRS, 
                                 size.match = 5, qoi = "att", outcome.var = "cogtot27",
                                 lead = 0:1, forbid.treatment.reversal = FALSE, 
                                 match.missing = FALSE, listwise.delete = FALSE,
                                 use.diagonal.variance.matrix = TRUE)

ATT_Working_HRS_Before <- PanelEstimate(sets = Working_HRS_Before, data = HRS)

#After covariate balancing
Working_HRS <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                          treatment = "working", refinement.method = "ps.weight", 
                          data = HRS, 
                          covs.formula = ~ female + age + I(age^2) + agecohort + edu + I(lag(asset_quantile, 1:3)) + I(lag(health, 1:3))
                          + I(lag(job.locfcategory, 1:3))  + I(lag(income_quantile, 1:3)) + I(lag(cogtot27, 1:3)) 
                          + nonhispblack + other + hispanic + foreignbirth,
                          size.match = 5, qoi = "att", outcome.var = "cogtot27",
                          lead = 0:1, forbid.treatment.reversal = FALSE, match.missing = FALSE, listwise.delete = FALSE,
                          use.diagonal.variance.matrix = TRUE)

ATT_Working_HRS <- PanelEstimate(sets = Working_HRS, data = HRS)

####################################################################################################
## Matching by employment history. HRS - Exit
####################################################################################################
#Before covariate balancing
NotWorking_HRS_Before <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                                    treatment = "notworking", refinement.method = "none", 
                                    data = HRS, 
                                    size.match = 5, qoi = "att", outcome.var = "cogtot27",
                                    lead = 0:1, forbid.treatment.reversal = FALSE, 
                                    match.missing = FALSE, listwise.delete = FALSE,
                                    use.diagonal.variance.matrix = TRUE)

ATT_NotWorking_HRS_Before <- PanelEstimate(sets = NotWorking_HRS_Before, data = HRS)
#After covariate balancing

NotWorking_HRS <- PanelMatch(lag = 3, time.id = "wave", unit.id = "hhidpn", 
                             treatment = "notworking", refinement.method = "ps.weight", 
                             data = HRS, 
                             covs.formula = ~ female + age + I(age^2) + agecohort+ edu + I(lag(asset_quantile, 1:3)) + I(lag(health, 1:3))
                             + I(lag(job.locfcategory, 1:3))+ I(lag(income_quantile, 1:3)) + I(lag(cogtot27, 1:3)) 
                             + nonhispblack + other + hispanic + foreignbirth,
                             size.match = 5, qoi = "att", outcome.var = "cogtot27",
                             lead = 0:1, forbid.treatment.reversal = FALSE, match.missing = FALSE, listwise.delete = FALSE,
                             use.diagonal.variance.matrix = TRUE)
ATT_NotWorking_HRS <- PanelEstimate(sets = NotWorking_HRS, data = HRS)


####################################################################################################
## Plot
####################################################################################################

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
## ATT plots for KLoSA and HRS
####################################################################################################
par(oma=c(4,4,0,0),mar=c(3,3,2,2),mfrow=c(2,2))

plot.PanelEstimate(ATT_Working_KL,
                   main = "Stay inactive vs  Entering the labor market - Korea",
                   ylab = "",
                   xlab = "",
                   xlim=c(0.7,2.3),
                   ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))


plot.PanelEstimate(ATT_Working_HRS,
                   main = "Stay inactive vs  Entering the labor market - US",
                   ylab = "",
                   xlab = "",
                   xlim=c(0.7,2.3),
                   ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))

plot.PanelEstimate(ATT_NotWorking_KL,
                   main = "Stay active vs  Exiting the labor market - Korea",
                   ylab = "",
                   xlab = "",
                   xlim=c(0.7,2.3),
                   ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))

plot.PanelEstimate(ATT_NotWorking_HRS,
                   main = "Stay active vs  Exiting the labor market - US",
                   ylab = "",
                   xlab = "",
                   xlim=c(0.7,2.3),
                   ylim = c(-1, 1.2))
axis(1, at = c(1, 2),
     labels = c("t+0", "t+1"))


mtext(text="Waves passed after transition of working status",side=1,line=0,outer=TRUE)
mtext(text="Cognitive Score",side=2,line=2,outer=TRUE)
mtext(text="Left : K-MMSE, Right : HRS-TICS",side=2,line=1,outer=TRUE)
mtext(text="Left : Korea, Right : US",side=1,line=1,outer=TRUE)



####################################################################################################
## Gathering numerical estimation results
####################################################################################################

#KLoSA
#Unadjusted
library(xtable)
ATT_Working_KL_Before_result <- summary(ATT_Working_KL_Before)
print(xtable(ATT_Working_KL_Before_result$summary, digits=3))

#Adjusted
ATT_Working_KL_result <- summary(ATT_Working_KL)
print(xtable(ATT_Working_KL_result$summary, digits=3))

ATT_NotWorking_KL_Before_result <- summary(ATT_NotWorking_KL_Before)
print(xtable(ATT_NotWorking_KL_Before_result$summary, digits=3))

ATT_NotWorking_KL_result <- summary(ATT_NotWorking_KL)
print(xtable(ATT_NotWorking_KL_result$summary, digits=3))

#HRS
#Unadjusted
ATT_Working_HRS_Before_result <- summary(ATT_Working_HRS_Before)
print(xtable(ATT_Working_HRS_Before_result$summary, digits=3))

#Adjusted
ATT_Working_HRS_result <- summary(ATT_Working_HRS)
print(xtable(ATT_Working_HRS_result$summary, digits=3))

ATT_NotWorking_HRS_Before_result <- summary(ATT_NotWorking_HRS_Before)
print(xtable(ATT_NotWorking_HRS_Before_result$summary, digits=3))

ATT_NotWorking_HRS_result <- summary(ATT_NotWorking_HRS)
print(xtable(ATT_NotWorking_HRS_result$summary, digits=3))

####################################################################################################
## Covariate balancing check - KLoSA
####################################################################################################

par(oma=c(4,4,0,0),mar=c(3,3,2,2),mfrow=c(2,2))

get_covariate_balance(Working_KL_Before$att,
                      data = KLoSA,
                      #use.equal.weights = TRUE,
                      covariates = c("mmse", "health", "edu", "asset_quantile", "female", "age"),
                      ylim = c(- 0.5, 0.5),
                      ylab = "",
                      xlab = "",
                      main = "Before - Stay inactive vs  Entering the labor market",
                      legend = FALSE,
                      verbose = TRUE,
                      plot = TRUE)
get_covariate_balance(Working_KL$att,
                      data = KLoSA,
                      #use.equal.weights = TRUE,
                      covariates = c("mmse", "health", "edu", "asset_quantile", "female", "age"),
                      ylim = c(- 0.5, 0.5),
                      main = "After - Stay inactive vs  Entering the labor market",
                      ylab = "",
                      xlab = "",
                      legend = FALSE,
                      verbose = TRUE,
                      plot = TRUE)
get_covariate_balance(NotWorking_KL_Before$att,
                      data = KLoSA,
                      #use.equal.weights = TRUE,
                      covariates = c("mmse", "health", "edu", "asset_quantile", "female", "age"),
                      ylim = c(- 0.5, 0.5),
                      ylab = "",
                      xlab = "",
                      main = "Before - Stay active vs  Exiting the labor market",
                      legend = FALSE,
                      verbose = TRUE,
                      plot = TRUE)
get_covariate_balance(NotWorking_KL$att,
                      data = KLoSA,
                      #use.equal.weights = TRUE,
                      covariates = c("mmse", "health", "edu", "asset_quantile", "female", "age"),
                      ylim = c(- 0.5, 0.5),
                      main = "After - Stay active vs  Exiting the labor market",
                      ylab = "",
                      xlab = "",
                      legend = FALSE,
                      verbose = TRUE,
                      plot = TRUE)

mtext(text="Waves prior to the employment status transition in Korea",side=1,line=0,outer=TRUE)
mtext(text="Left : Before refinement, Right : After refinement",side=1,line=1,outer=TRUE)
mtext(text="Standardized Mean Differences of Covariates",side=2,line=1,outer=TRUE)
mtext(text="(Cognitive Score in black line)",side=2,line=0,outer=TRUE)

####################################################################################################
## Covariate balancing check - HRS
####################################################################################################
par(oma=c(4,4,0,0),mar=c(3,3,2,2),mfrow=c(2,2))

get_covariate_balance(Working_HRS_Before$att,
                      data = HRS,
                      covariates = c("cogtot27","health","edu", "asset_quantile", "female", "age"),
                      #use.equal.weights = TRUE,
                      legend = FALSE,
                      ylab = "",
                      xlab = "",
                      main = "Before - Stay inactive vs  Entering the labor market",
                      ylim = c(- 0.5, 0.5),
                      plot = TRUE)

get_covariate_balance(Working_HRS$att,
                      data = HRS,
                      covariates = c("cogtot27","health","edu", "asset_quantile", "female","age"),
                      #use.equal.weights = TRUE,
                      legend = FALSE,
                      ylab = "",
                      xlab = "",
                      main = "After - Stay inactive vs  Entering the labor market",
                      ylim = c(- 0.5, 0.5),
                      plot = TRUE)
get_covariate_balance(NotWorking_HRS_Before$att,
                      data = HRS,
                      covariates = c("cogtot27","health","edu", "asset_quantile", "female", "age"),
                      #use.equal.weights = TRUE,
                      legend = FALSE,
                      ylab = "",
                      xlab = "",
                      main = "Before - Stay active vs  Exiting the labor market",
                      ylim = c(- 0.5, 0.5),
                      plot = TRUE)
get_covariate_balance(NotWorking_HRS$att,
                      data = HRS,
                      covariates = c("cogtot27","health","edu", "asset_quantile", "female", "age"),
                      #use.equal.weights = TRUE,
                      legend = FALSE,
                      ylab = "",
                      xlab = "",
                      main = "After - Stay active vs  Exiting the labor market",
                      ylim = c(- 0.5, 0.5),
                      plot = TRUE)

mtext(text="Waves prior to the employment status transition in the US",side=1,line=0,outer=TRUE)
mtext(text="Left : Before refinement, Right : After refinement",side=1,line=1,outer=TRUE)
mtext(text="Standardized Mean Differences of Covariates",side=2,line=1,outer=TRUE)
mtext(text="(Cognitive Score in black line)",side=2,line=0,outer=TRUE)

