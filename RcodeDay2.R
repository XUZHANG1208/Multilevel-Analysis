setwd("~/Documents/JOB/UANTWERPEN/FLAMES/Multilevel/Data")
siiData <- read.table("SII.txt",header=T)

# Co-erce schoolid, classid, childid, sex and ses to a factor
siiData$schoolid<-as.factor(siiData$schoolid)
siiData$classid<-as.factor(siiData$classid)
siiData$childid<-as.factor(siiData$childid)
siiData$sex<-as.factor(siiData$sex)
levels(siiData$sex)<-c("male","female")


library(nlme)
##### Exercise 1 #####
# Mixed model accounting for clustering within schools
# not (yet within class)
# add school as random effect

# model for ID j within school j
# Yij = (beta0+bi)+beta2*sex+eij
# with bi = random-intercept

# model in R:
# package nlme

modelRE <- lme(mathgain~sex,random=~1|schoolid, data=siiData)
anova.lme(modelRE)
summary(modelRE)
tab <- tab_model(modelRE)
tab

## the same thing using a different function
library(lme4)
modelREr <- lmer(mathgain ~ sex + (1|schoolid), data=siiData)
summary(modelREr)
tab <- tab_model(modelREr)
tab

# variance components 
VarCorr(modelRE)
#               Variance  StdDev  
# (Intercept)  109.2739 10.45341
# Residual    1093.8173 33.07291

totalVariance <- 109.2739 + 1093.8173
betweenSchoolVar <- 109.2739

# correlation within schools
# = percent of variance due to variance between schools
betweenSchoolVar/totalVariance
# 0.09

# 9% of the variance is between schools, the rest is within school


######### Exercise 2 ######
# mixed model for MMSE over time in both neuro groups
# MMSE of jth measurement in ID i   
# Yij = (beta0+bi) + beta1*NEURO + beta2*time+ beta3$NEURO*Time + eij
# random intercept model

#############################
# read in hipfracture dataset
############################

hipData<-read.table("hipfracture2.txt",header = T)

# co-ercion of categorical ariables
hipData$IDNR<-as.factor(hipData$IDNR)
hipData$NEURO<-as.factor(hipData$NEURO)
levels(hipData$NEURO)<-c("not neuro-psy","neuro-psy")




# remove missing data
hipNonmiss<-hipData[complete.cases(hipData),]

# fit mixed model
model<- lme(MMSE~NEURO+TIME+NEURO:TIME,random=~1|IDNR,data=hipNonmiss)

#or
model<- lme(MMSE~NEURO+TIME+NEURO:TIME,random=~1|IDNR,data=hipData,na.action=na.exclude)

VarCorr(model)
#            Variance  StdDev  
#(Intercept) 41.970459 6.478461
#Residual     6.315578 2.513081


totalVariance<-41.97046+6.3156
betwIDvar<-41.97046

betwIDvar/totalVariance
#0.869 

# different evolution over time? Check interaction term
anova.lme(model,type="marginal")
# p=0.536
# --> no different evolution over time
# omit interaction term

model<- lme(MMSE ~ NEURO*TIME,random=~1|IDNR,data=hipData,na.action=na.exclude)
anova.lme(model,type="marginal")

