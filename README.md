# Description of this data
The codes and data are for the paper Does (Re-)Entering the Labor Market at Advanced Ages Protect Against Cognitive Decline? A Panel-Matching Difference-in-Differences Approach, Kim, Jung Hyun and Muniz-Terrera, Graciela and Leist, Anja (2022).

## Data
- The data used in this study are publicly available on the Health and Retirement Study website (http://hrsonline.isr.umich.edu/).
- The Korean Longitudinal Study of Ageing is publicly available on the website (http://survey.keis.or.kr).
- Inflation data are extracted from the World Bank website (https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG).
- Purchasing power parity data are extracted from the World Bank website (https://data.worldbank.org/indicator/PA.NUS.PPP).

## Software
- STATA code is used to prepare the dataset and R code for the analysis.
- STATA version 17.0, R version 4.2.1 PanelMatch package version 2.0.0 with the latest update (November 2022) are used.

# Steps to reproduce
## Data code (in STATA)
1. KLoSA.do, allows you to keep the variable of interests and merge them across waves.
2. HRS.do subset the variables of interest. 
3. Download inflation data (API_FP.CPI.TOTL.ZG_DS2_en_excel_v2_4692956.xls) and PPP data (API_PA.NUS.PPP_DS2_en_excel_v2_4684852.xls).

## Main Estimation code (in R)
1. Use main estimation.R for the first descriptive statistics and main results.
2. Use main plots.R to plot the main results and covariate balancing tests.
3. Use Table 1 Figure A3.R to reproduce Table 1 and Figure A3.
4. Use Table A2 A3.R to generate Table A2 and A3.
5. Use Table A4.R to make Table A4.
6. Use Table 3 Figure A6.R and Figure A7.R for the robustness check.

If you encounter any errors/problems reproducing the results, please send Jung Hyun an email.