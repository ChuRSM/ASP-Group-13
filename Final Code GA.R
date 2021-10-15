
remove(list=ls())
dfUnemployment <- read.csv2("C:/Users/Admin/OneDrive/University/RSM/BLOCK 1/Advanced Statistics and Programing/R/Data/GA data.csv", header=TRUE)
library (plm)
library(stargazer)
library(ggplot2)
library(reshape)
library(plyr)
library(dplyr)
library(foreign)
library(lmtest)

dir<- "C:/Users/apoko/Desktop/BAM/BLOCK 1/Advance Statistics & Programming/Group Assignment/"

dirProg<-paste0(dir, "Programs/")
dirProg
dirData<- paste0(dir, "Data/")
dirData 


#Import Data
dfUnemployment <- read.csv(file=paste0(dirData, "unemp_data.csv"))

dfUnemployment <- read.csv2("C:/Users/Admin/OneDrive/University/RSM/BLOCK 1/Advanced Statistics and Programing/R/Data/GA data.csv", header=TRUE)
str(dfUnemployment)

dfUnemployment$unempl_rate <- as.numeric(dfUnemployment$unempl_rate)
dfUnemployment$vaccinated <- as.numeric(dfUnemployment$vaccinated)
dfUnemployment$diff_unempl <- as.numeric(dfUnemployment$diff_unempl)
dfUnemployment$diff_vacc <- as.numeric(dfUnemployment$diff_vacc)
dfUnemployment$diff_full_vacc <- as.numeric(dfUnemployment$diff_full_vacc)
dfUnemployment$mon_str_index <- as.numeric(dfUnemployment$mon_str_index)
dfUnemployment$income_support <- as.numeric(dfUnemployment$income_support)
dfUnemployment$debt_relief <- as.numeric(dfUnemployment$debt_relief)
dfUnemployment$Total.contribution.of.Travel...Tourism.to.GDP.2019 <- as.numeric(dfUnemployment$Total.contribution.of.Travel...Tourism.to.GDP.2019)
dfUnemployment$fully.vaccinated <- as.numeric(dfUnemployment$fully.vaccinated)
dfUnemployment$country <- as.factor(dfUnemployment$country)


dfUnemployment$c_id <- dfUnemployment$country
dfUnemployment$c_id <- as.factor(dfUnemployment$c_id)
dfUnemployment$c_id <- as.numeric(dfUnemployment$c_id)

dfUnemployment$cat_debt_relief <- round(dfUnemployment$debt_relief)
dfUnemployment$cat_income_support <- round(dfUnemployment$income_support)

dfUnemployment$cat_debt_relief <- as.factor(dfUnemployment$cat_debt_relief)
dfUnemployment$cat_income_support <- as.factor(dfUnemployment$cat_income_support)
#--------------------------------------------------
#            WITH CONTINUOUS VARIABLES            #
#-------------------------------------------------- 

#Separating into Core & Peripheral
veccore<-c("Austria","Belgium","Finland","France", "Germany", "Netherlands")

dfCore <- dfUnemployment[dfUnemployment$country %in% veccore,]

vecperiph <- c("Cyprus","Greece","Ireland ", "Italy","Portugal","Spain")          

dfPeriph <- dfUnemployment[dfUnemployment$country %in% vecperiph,]



dfCore <- dfUnemployment[dfUnemployment$country %in% veccore,]
dfPeriph<-dfUnemployment[dfUnemployment$country %in% vecperiph,]


stargazer(dfUnemployment)

# scatterplots for stationarity
# we only need to plot the continuous variables
ggplot(dfCore, aes(x=period, y=unempl_rate, group=country)) +
  geom_line(aes( color=country), lwd = 1.3)+
  ylab("Unemployment Rate")+
  xlab("Period")

ggplot(dfPeriph, aes(x=period, y=unempl_rate, group=country)) +
  geom_line(aes( color=country),lwd = 1.3)+
  ylab("Unemployment Rate")+
  xlab("Period")

ggplot(dfCore, aes(x=period, y=mon_str_index, group=country)) +
  geom_line(aes( color=country), lwd = 1.3)+
  ylab("Stringency Index")+
  xlab("Period")

ggplot(dfPeriph, aes(x=period, y=mon_str_index, group=country)) +
  geom_line(aes( color=country), lwd = 1.3)+
  ylab("Stringency Index")+
  xlab("Period")



# specify the model. It is the same for both Core and Peripheral countries
mdlU<- unempl_rate ~ mon_str_index + debt_relief  + income_support + Total.contribution.of.Travel...Tourism.to.GDP.2019
mdlU_cat <- unempl_rate ~ mon_str_index + cat_debt_relief  + cat_income_support + Total.contribution.of.Travel...Tourism.to.GDP.2019



# run the Pooled OLS, FE and RE for the whole dataset
rslt.Pooling <- plm(mdlU, data = dfUnemployment, model = "pooling")
rsltFE <- 
  plm(mdlU, data = dfUnemployment, 
      index=c("country", "time"), model = "within")
rsltRE <- 
  plm(mdlU, data = dfUnemployment, 
      index=c("country", "time"), model = "random")


# run the Pooled OLS, FE and RE for the whole dataset, with debt&relief contract and income support as categorical variables
rslt.Pooling2 <- plm(mdlU_cat, data = dfUnemployment, model = "pooling")
rsltFE2 <- 
  plm(mdlU_cat, data = dfUnemployment, 
      index=c("country", "time"), model = "within")
rsltRE2 <- 
  plm(mdlU_cat, data = dfUnemployment, 
      index=c("country", "time"), model = "random")


rslt.PoolingC <- plm(mdlU, data = dfCore, model = "pooling")
rsltFE.Core <- 
  plm(mdlU, data = dfCore, 
      index=c("country", "time"), model = "within")
rsltRE.Core <- 
  plm(mdlU, data = dfCore, 
      index=c("country", "time"), model = "random")


# run the Pooled OLS, FE and RE for the Peripheral country subset
rslt.PoolingP <- plm(mdlU, data = dfPeriph, model = "pooling")
rsltFE.Periph <- 
  plm(mdlU, data = dfPeriph, 
      index=c("country", "time"), model = "within")
rsltRE.Periph <- 
  plm(mdlU, data = dfPeriph, 
      index=c("country", "time"), model = "random")



# present the findings in a table
stargazer(rslt.PoolingC, rsltFE.Core, rsltRE.Core, rslt.PoolingP, rsltFE.Periph, rsltRE.Periph,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type="text")

stargazer(rslt.PoolingC, rsltFE.Core, rsltRE.Core, rslt.PoolingP, rsltFE.Periph, rsltRE.Periph,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE)



stargazer(rslt.Pooling, rslt.Pooling2, rslt.PoolingC, rslt.PoolingP, rsltFE, rsltFE2, rsltFE.Core,  rsltFE.Periph, rsltRE, rsltRE2, rsltRE.Core, rsltRE.Periph, 
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type="text")


stargazer(rslt.Pooling, rslt.Pooling2, rslt.PoolingC, rslt.PoolingP, 
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE)

stargazer(rsltFE, rsltFE2, rsltFE.Core,  rsltFE.Periph, 
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE)

stargazer(rsltRE, rsltRE2, rsltRE.Core, rsltRE.Periph,  
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE)




# specification test between Pooled OLS and Fixed Effects. Significant statistics means that Pooled OLS is better
pFtest(rsltFE.Core, rslt.PoolingC)
pooltest(rslt.PoolingC, rsltFE.Core)

pFtest(rsltFE.Periph, rslt.PoolingP)
pooltest(rslt.PoolingP, rsltFE.Periph)

# Pooled OLS is a better model because p < 0.01 (p = 0.000)


# Breusch-Pagan Lagrange Multiplier for random effects. Null means that there is no panel effect and OLS better would be the better model
plmtest(rslt.PoolingC, type=c("bp"))
plmtest(rslt.PoolingP, type=c("bp"))

# In both cases, RE is preferred because p < 0.01 (p = 0.000)



# country effects
mdlU.country_effects <-  unempl_rate ~ mon_str_index + debt_relief  + income_support + Total.contribution.of.Travel...Tourism.to.GDP.2019 + factor(country)
rslt.Corecountry_effects <- plm(mdlU.country_effects, data = dfCore, 
                             index=c("country", "time"), model = "random")
rslt.Periphcountry_effects <- plm(mdlU.country_effects, data = dfPeriph, 
                             index=c("country", "time"), model = "random")

pFtest(rslt.Corecountry_effects, rsltRE.Core)
pFtest(rslt.Periphcountry_effects, rsltRE.Periph)


# heteroskedasticity

mdlU.homosk <- unempl_rate ~ mon_str_index + debt_relief  + income_support + Total.contribution.of.Travel...Tourism.to.GDP.2019 + factor(country)
bptest(mdlU.homosk, data = dfCore, studentize=F)


mdlU.homosk <- unempl_rate ~ mon_str_index + debt_relief  + income_support + Total.contribution.of.Travel...Tourism.to.GDP.2019 + factor(country)
bptest(mdlU.homosk, data = dfPeriph, studentize=F)
# there is a problem of heteroskedasticity in the Core model, not the Peripheral. Hence we estimate the Core model including robust s.e. (White s.e.)

seWhite <- sqrt(diag(vcovHC(rsltRE.Core, type="HC0")))
seClust <- sqrt(diag(vcovHC(rsltRE.Core, cluster=c("country", "period"))))

stargazer(rsltRE.Core, rsltRE.Core,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, 
          se = list(NULL, seWhite), type="text")

stargazer(rsltRE.Core, rsltRE.Core,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, 
          se = list(NULL, seWhite))




#---------------------------------------------------------
#              WITH CATEGORICAL VARIABLES                #
#---------------------------------------------------------

#CATEGORICAL VARIABLE CANNOT BE ESTIMATED BECAUSE FOR EACH OF THEM YOU NEED TO ESTIMATE TWO COEFFICIENTS SO IT'S LIKE HAVING TWO MORE VARIABLES IN THE MODEL 

# run it again with debt and income as categorical
dfUnemployment$cat_debt_relief <- round(dfUnemployment$debt_relief)
dfUnemployment$cat_income_support <- round(dfUnemployment$income_support)

dfUnemployment$cat_debt_relief <- as.factor(dfUnemployment$cat_debt_relief)
dfUnemployment$cat_income_support <- as.factor(dfUnemployment$cat_income_support)

veccore<-c("Austria","Belgium","Finland","France", "Germany", "Netherlands")

dfCore <- dfUnemployment[dfUnemployment$country %in% veccore,]

vecperiph <- c("Cyprus","Greece","Ireland ", "Italy","Portugal","Spain")          

dfPeriph <- dfUnemployment[dfUnemployment$country %in% vecperiph,]
# specify the model. It is the same for both Core and Peripheral countries
mdlU <- unempl_rate ~ mon_str_index + cat_debt_relief  + cat_income_support + Total.contribution.of.Travel...Tourism.to.GDP.2019



# run the Pooled OLS, FE and RE for the Core country subset
rslt.PoolingC <- plm(mdlU, data = dfCore, model = "pooling")
rsltFE.Core <- 
  plm(mdlU, data = dfCore, 
      index=c("country", "time"), model = "within")
rsltRE.Core <- 
  plm(mdlU, data = dfCore, 
      index=c("country", "time"), model = "random")


# run the Pooled OLS, FE and RE for the Peripheral country subset
rslt.PoolingP <- plm(mdlU, data = dfPeriph, model = "pooling")
rsltFE.Periph <- 
  plm(mdlU, data = dfPeriph, 
      index=c("country", "time"), model = "within")
rsltRE.Periph <- 
  plm(mdlU, data = dfPeriph, 
      index=c("country", "time"), model = "random")



# present the findings in a table
stargazer(rslt.PoolingC, rsltFE.Core, rsltRE.Core, rslt.PoolingP, rsltFE.Periph, rsltRE.Periph,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type="text")

stargazer(rslt.PoolingC, rsltFE.Core, rsltRE.Core, rslt.PoolingP, rsltFE.Periph, rsltRE.Periph,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE)


# specification test between Pooled OLS and Fixed Effects. Significant statistics means that Pooled OLS is better
pooltest(rslt.PoolingC, rsltFE.Core)
pooltest(rslt.PoolingP, rsltFE.Periph)

# Fixed effects is a better model for both subsets because p < 0.01 (p = 0.000)

# Hausman Test for RE vs FE
phtest(rsltFE.Core, rsltRE.Core)
phtest(rsltFE.Periph, rsltRE.Periph)

# In both cases, p > 0.05, meaning that both models are consistent but RE is more efficient as it uses both within and between variation so we choose RE




# heteroskedasticity

mdlU.homosk <- unempl_rate ~ mon_str_index + debt_relief  + income_support + Total.contribution.of.Travel...Tourism.to.GDP.2019 + factor(country)
bptest(mdlU.homosk, data = dfCore, studentize=F)


mdlU.homosk <- unempl_rate ~ mon_str_index + debt_relief  + income_support + Total.contribution.of.Travel...Tourism.to.GDP.2019 + factor(country)
bptest(mdlU.homosk, data = dfPeriph, studentize=F)
# there is a problem of heteroskedasticity in the Core model, not the Peripheral. Hence we estimate the Core model including robust s.e. (White s.e.)



