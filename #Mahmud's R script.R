#R Script

#### Clear memory, set working directory and load libraries
rm(list=ls())


#### Retreive and modify relevant datasets
ScieboPath <- function(NameofDataset)
  paste0("https://hochschule-rhein-waal.sciebo.de/s/ICXJvMpW3QIM2hu/download?path=%2F&files=",NameofDataset)

library(haven)

EL <- read_dta(ScieboPath("EL.dta"))


set.seed(20781)
test_IDs=sample(EL$hhid,size=0.3*nrow(EL),replace=FALSE)
test_data=subset(EL, hhid %in% test_IDs)


### 1. Randomization

### a) t.test for significance in difference in outcome between treated
### and control pupils

t.test(shr_Xfood ~ eligilitystatus, data=EL)
t.test(cpexp30_pae ~ eligilitystatus, data=EL)
t.test(Xeduc_pch ~ eligilitystatus, data=EL)

## b) Regression estimation of variable influence
install.packages("lmtest")
library(lmtest)
RegRandom <- lm(cpexp30_pae~aeligibility,shr_Xfood,data=test_data)
summary(RegRandom)
stargazer(RegRandom, type= "text", title="Linear Regression RCT") 

### c) t-test for difference between treatment and control group
t.test(Xeduc_pch ~ eligilitystatus, data=EL)
t.test(Xeduc ~ eligilitystatus, data=EL)
t.test(Xfood ~ eligilitystatus, data=EL)

RegRandom1 <- lm(cpexp30_pae ~
                   aeligibility+shr_Xfood+rXdrinks+rXheal+Xeduc
                 ,data=test_data)

summary(RegRandom1)

### 2.) Instrumental Variable Regression


### a) Use place of residence (variable: urban) as an instrument

install.packages("AER")
library(AER)

ivreg<-ivreg(cpexp30_pae ~  aeligibility+shr_Xfood+rXdrinks+rXheal+Xeduc|urban, data = test_data)
Mahmud<-na.omit(test_data)    

summary(ivreg)


#Adding Additional variables
ivreg4<-ivreg(cpexp30_pae ~  aeligibility+shr_Xfood+rXdrinks+rXheal+Xeduc|urban+rXheal+Xeduc+shr_Xfood+rXdrinks, data = test_data)
 Mahmud<-na.omit(test_data) 
summary(ivreg4)


#b) Run the same analysis with Xeduc_pch
ivreg5<- ivreg(Xeduc_pch ~ aeligibility+shr_Xfood+rXdrinks+rXheal+Xeduc|urban, data = test_data)
summary(ivreg5)


##C) Diagnostics Test
summary(ivreg5, vcov = NULL, df = Inf, diagnostics = TRUE)



##### 3. Difference in Difference:

#### > ScieboPath <- function(NameofDataset)
+   paste0("https://hochschule-rhein-waal.sciebo.de/s/ICXJvMpW3QIM2hu/download?path=%2F&files=", NameofDataset)
 
> library(haven)


BLEL <- read_dta(ScieboPath("BLEL.dta"))

set.seed(20781)
test_IDs=sample(BLEL$hhid,size=0.3*nrow(BLEL),replace=FALSE)
test_data=subset(BLEL, hhid %in% test_IDs)
head(BLEL)

t.test(cpexp30_pae ~ eligilitystatus, data=EL)

#### a) Run the analysis with cpexp30_pae

Reg1<-lm(cpexp30_pae~aeligibility+Year+I(aeligibility*Year),data=test_data)
> summary(Reg1)


#### b) Run the same analysis with Xeduc_pch

> Reg2<-Reg1<-lm(Xeduc_pch~aeligibility+Year+I(aeligibility*Year),data=test_data)
> summary(Reg2)

### c) Now, redo part(a), this time by controlling

Reg3<-Reg1<-lm(cpexp30_pae~aeligibility+Year+shr_Xfood+rXdrinks+rXheal+Xeduc+I(aeligibility*Year),data=test_data)
> summary(Reg3)



### 4.) Propensity score matching

test_cleandata<-na.omit(test_data)
psmodel< glm(eligilitystatus~cpexp30_pae +urban+Xclothshoe
             summary(psmodel)
             
             ### a) Estimate the Propensity Score
             
             ScieboPath <- function(NameofDataset)
               paste0("https://hochschule-rhein-waal.sciebo.de/s/ICXJvMpW3QIM2hu/download?path=%2F&files=", NameofDataset)
             
             
             #installing packages
             library(dplyr)
             library(lmtest)
             library(AER)
             library(car)
             
             ### a) Estimating Propensity Scores
             #   Probit regression with treatment as dependent variable
             
             Mahmud<-glm(eligilitystatus~regurb+urban+hsize+Xeduc, family=binomial(link="probit"), test_data)
             summary(Mahmud)
             
             Scores<-data.frame(pr_score=predict(Mahmud,type="response"),treatment=Mahmud$model$eligilitystatus)
             head(Scores)
             
             ### b) Define the area of common support and interpret
             
             comsup<-c(min(Scores$pr_score[Scores$treatment==1]),max(Scores$pr_score[Scores$treatment==0]))
             (comsup)
             summary(comsup)
             
             
             ### c) Match participants and non-participants
             
             library("Matching")
             install.packages("MatchIt")
             library(MatchIt)
             m.out<-matchit(eligilitystatus ~ shr_Xeduc,data= test_data, method ="nearest")
             m.out<-matchit(eligilitystatus~shr_Xeduc)
             
             m.out1 <-matchit(aeligilitystatus~cpexp30_pae + urban + Xclolthshoes + Xeduc +Xfood,data = testcleandata,method = "nearest",distance = "probit", caliper = 0.001)
             summary(m.out1)
             m.out2 <-matchit(aeligilitystatus~cpexp30_pae + urban + Xclolthshoes + Xeduc +Xfood,data = testcleandata,method = "nearest",distance = "probit", caliper = 0.1,ratio=1)
             summary(m.out2)
             
             