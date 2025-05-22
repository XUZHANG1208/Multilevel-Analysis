
# Exercise 0.
# fit model with 3-way-interaction
setwd("~/Desktop/r-project/Multilevel-Analysis")
siiData <- read.table("SII.txt",header=T)

# Co-erce schoolid, classid, childid, sex and ses to a factor
siiData$schoolid<-as.factor(siiData$schoolid)
siiData$classid<-as.factor(siiData$classid)
siiData$childid<-as.factor(siiData$childid)
siiData$sex<-as.factor(siiData$sex)
levels(siiData$sex)<-c("male","female")
# two way interaction
fit <- lm(mathgain ~ sex * housepov , data = siiData)
summary(fit)

library(sjPlot)
# select only levels 30, 50 and 70 from continuous variable Barthel-Index
plot_model(fit, type = "pred", terms = c("housepov",  "sex"))


fit <- lm(mathgain ~ sex + yearstea + sex:yearstea, data = siiData)
summary(fit)
# select only levels 30, 50 and 70 from continuous variable Barthel-Index
plot_model(fit, type = "pred", terms = c("yearstea", "sex"))




##### Exercise 1 #####
### a. Empirical Bayes estimates
# model with only main effects in hip fracture
hipData <-read.table("hipfracture2.txt",header = T)

# co-ercion of categorical ariables
hipData$IDNR <-as.factor(hipData$IDNR)
hipData$NEURO <-as.factor(hipData$NEURO)

levels(hipData$NEURO) <-c("not neuro-psy","neuro-psy")

## hip fracture
# remove missing data
hipNonmiss<-hipData[complete.cases(hipData),]


library(sjPlot)
library(nlme)
model<-lme(MMSE ~ NEURO + TIME + AGE , random=~1|IDNR,data=hipNonmiss)
anova.lme(model)
tab <- tab_model(model)
tab

plot_model(model, type = "pred", terms = c("NEURO", "TIME"))






# obtain EB estimates (random intercepts for ID)
# (take first column of 1-column dataframe)
ranef(model)
EBestimID <- ranef(model)[,1]
EBestimID

hist(EBestimID)

### b. predictions 

# level = 0 : predicted mean 
# (given covariates NEURO, AGE and TIME)
predMean <- predict(model,level=0)
predMean
hist(predMean)

# level = 1 : prediction for the individual 
# (given covariates NEURO, AGE and TIME, plus individual random intercept)
predIndiv<-predict(model,level=1)
hist(predIndiv)

dfPredictions<-cbind(predMean,predIndiv)
dfPredictions

#### Exercise 2 ####
# stepwise backward elimination in SII data
# model from Ex3 day 3
# model interactions between sex and other covaraites

setwd("~/Documents/JOB/UANTWERPEN/FLAMES/Multilevel/Data")
siiData <- read.table("SII.txt",header=T)

# Co-erce schoolid, classid, childid, sex and ses to a factor
siiData$schoolid<-as.factor(siiData$schoolid)
siiData$classid<-as.factor(siiData$classid)
siiData$childid<-as.factor(siiData$childid)
siiData$sex<-as.factor(siiData$sex)
levels(siiData$sex)<-c("male","female")


model<-lme(mathgain~ sex*(ses+housepov+yearstea+mathkind),random=~1|schoolid/classid,data=siiData)
tab <- tab_model(model)
tab

ranef(model)
class <- ranef(model)$classid
school <- ranef(model)$schoolid


par(mfrow = c(1, 2))    
hist(class$`(Intercept)`)
hist(school$`(Intercept)`)


# omit least significant term (starting with interation terms)
model2<-update(model,.~.-sex:ses )
anova.lme((model2))
summary(model2)
tab <- tab_model(model2)
tab

model3<-update(model2,.~.-sex:mathkind)
summary(model3)
tab <- tab_model(model3)
tab

# note : housepow must stay in model as main effect, as long as 
# housepov is part of an interaction term
model4<-update(model3,.~.-sex:yearstea)
summary(model4)
tab <- tab_model(model4)
tab
# now, main effect of yearstea can be removed, as 
# yearstea is no longer part of an interaction term
model5<-update(model4,.~.-yearstea)
summary(model5)
tab <- tab_model(model5)
tab

model6<-update(model5,.~.-sex:housepov)
summary(model6)
tab <- tab_model(model6)
tab
# only main effects left
# all effects are independent
model7<-update(model6,.~.-sex)
summary(model7)
tab <- tab_model(model7)
tab

model8<-update(model7,.~.-housepov)
summary(model8)
tab <- tab_model(model8)
tab

par(mfrow = c(1, 1))   
# creating a new model
boxplot(ses ~ as.numeric(schoolid), data=siiData)

model<-lme(mathgain ~ ses + mathkind, random=~1|schoolid/childid,data=siiData)
tab <- tab_model(model)
tab
VarCorr(model)

model2<-lme(mathgain ~ ses + mathkind, random=~1|childid,data=siiData)
tab <- tab_model(model2)
tab
VarCorr(model2)

anova(model, model2)

### the model

model<-lme(mathgain ~ ses + mathkind, random=~1|schoolid/childid,data=siiData)
tab <- tab_model(model)
tab
VarCorr(model)

# EB estimates
ranef(model)
child <- ranef(model)$childid
school <- ranef(model)$schoolid


par(mfrow = c(1, 2))    
hist(child$`(Intercept)`)
hist(school$`(Intercept)`)


