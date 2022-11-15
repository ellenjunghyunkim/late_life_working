# Description of this data
- The codes and data are for the paper Does (Re-)Entering the Labor Market at Advanced Ages Protect Against Cognitive Decline? A Panel-Matching Difference-in-Differences Approach, Kim, Jung Hyun and Muniz-Terrera, Graciela and Leist, Anja (2022).
- Study flow folder contains a study/data flow chart for an intuitive undersatnding.

## Data
- The RAND HRS Longitudinal File 2018 (V2) used in this study is publicly available on the Health and Retirement Study website (https://hrsdata.isr.umich.edu/data-products/rand-hrs-longitudinal-file-2018).
- HRS 2020 Core Early Release (Version 2.0) is publicly available on the Health and Retirement Study website (https://hrsdata.isr.umich.edu/data-products/2020-hrs-core).
- The Korean Longitudinal Study of Ageing is publicly available on the website (https://survey.keis.or.kr/eng/klosa/databoard/List.jsp) file N.21.
- Inflation data are extracted from the World Bank website (https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG).
- Purchasing power parity data are extracted from the World Bank website (https://data.worldbank.org/indicator/PA.NUS.PPP).

## Software
- STATA code is used to prepare the dataset and R code for the analysis.
- STATA version 17.0, R version 4.2.1 PanelMatch package version 2.0.0 with the latest update (November 2022) are used.

# Steps to reproduce
## Data code (in STATA)
1. KLoSA.do, allows you to keep the variable of interests and merge them across waves of KLoSA data.
2. HRS.do and HRS.core2020.do subset the variables of interest of HRS data. 
3. Download inflation data (API_FP.CPI.TOTL.ZG_DS2_en_excel_v2_4692956.xls) and PPP data (API_PA.NUS.PPP_DS2_en_excel_v2_4684852.xls).

## Main Estimation code (in R)
1. Use 1. FInal data set.R to get the final analytical samples for KLoSA and HRS.
2. Use 2. Descriptive statistics.R to generate the descriptive statistics. (Refer to the study flow chart.)
3. Use 3. Main estimtaion.R to replicate and plot main results.
4. Use 4. Subgroup analysis.R to run subgroup analysis. 
5. Use 5. Sensitivity analysis.R to run two sensitivity analyses. 

If you encounter any errors/problems reproducing the results, please send Jung Hyun an email.


### Updates 
Nov.11.2022, 
- Files: 3. Main estimtaion.R, 4. Subgroup analysis.R, 5. Sensitivity analysis.R 
- Categorical values to dummies with one-hot encoding, change in matching method from PS weight to CBPS weight.
