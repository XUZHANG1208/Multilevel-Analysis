# This script is about... 
# It was created by Roma
# Date: 

library(nlme)

## read the data
setwd("~/Documents/JOB/UANTWERPEN/FLAMES/Multilevel/Data")
siiData <- read.table("SII.txt",header=T)

# Co-erce schoolid, classid, childid, sex and ses to a factor
siiData$schoolid<-as.factor(siiData$schoolid)
siiData$classid<-as.factor(siiData$classid)
siiData$childid<-as.factor(siiData$childid)
siiData$sex<-as.factor(siiData$sex)
levels(siiData$sex)<-c("male","female")

#### Exercise 1 ####
# model difference in mathgain boys-girls
# accounting for school and class
# class = nested within school

#Yijk = beta0 + ai + bi(j) + beta1*sex + eijk
# with  ai = school effect
#       bi(j) = class effect (within school)
library(lme4)
modelLme4<-lmer(mathgain~sex+(1|schoolid/classid),data=siiData)
summary(modelLme4)


model<-lm(mathgain~sex,data=siiData)
summary(model)
plot(mathgain~sex,data=siiData)

#install.packages("sjPlot")
library(sjPlot)

model<-lme(mathgain~sex,random=~1|schoolid/classid,data=siiData)
anova.lme(model)
summary(model)

tab <- tab_model(model)
tab

###### Exercise 2 #######
# Variance components
VarCorr(model)
#               Variance     StdDev   
# schoolid =  pdLogChol(1)          
# (Intercept)   76.9545     8.772372
# classid =   pdLogChol(1)          
# (Intercept)  101.0413    10.051932
# Residual    1026.8606    32.044666


# 2.a fraction of variance between school and class
schoolVar<-76.9545
classVar<-101.0413
residVar<-1026.8606
totalVar<-schoolVar+classVar+residVar

#  fraction of variance due to var between school 
schoolVar/totalVar

# fraction of variance due to var between class
classVar/totalVar

# 2.b 
# correlation between mathgain from same school
schoolVar/totalVar

# 2.c
# correlation between mathgain from same class
# must add variance due to school in numerator
# as students from same class ar also within same school!
(schoolVar+classVar)/totalVar


###### Exercise 3 #####
# model interactions between sex and other covaraites

model2<-lme(mathgain~sex*(ses+housepov+yearstea+mathkind),random=~1|schoolid/classid,data=siiData)
summary(model2)
tab <- tab_model(model2)
tab

# this is shorthand notation for model with all main effects + 
# plus interactions with sex
# long form:

model2<-lme(mathgain ~ sex + yearstea,random=~1|schoolid/classid,data=siiData)
summary(model2)
tab <- tab_model(model2)
tab

model3<-lme(mathgain ~ sex + yearstea + ses,random=~1|schoolid/classid,data=siiData)
summary(model3)
tab <- tab_model(model3)
tab



anova(model2, model3)


###### Exercise 4 ######
hipData<-read.table("hipfracture2.txt",header = T)

# co-ercion of categorical ariables
hipData$IDNR<-as.factor(hipData$IDNR)
hipData$NEURO<-as.factor(hipData$NEURO)
levels(hipData$NEURO)<-c("not neuro-psy","neuro-psy")

## hip fracture
# remove missing data
hipNonmiss<-hipData[complete.cases(hipData),]

# old model
model<- lme(MMSE~NEURO+TIME,random=~1|IDNR,data=hipNonmiss)
tab <- tab_model(model)
tab


par(fmrow= c(1,1))
# fit mixed model with quadratic time effect
hipNonmiss$TIME2<-hipNonmiss$TIME^2
plot(TIME~TIME2, data =hipNonmiss)
model2<- lme(MMSE~NEURO+TIME+TIME2,random=~1|IDNR,data=hipNonmiss)
tab <- tab_model(model2)
tab

anova(model, model2)
anova.lme(model2, model)

###### Exercise 5 ######
# effect of age and neuro
# different effect of age in both NEURO groups = interaction term

model3<- lme(MMSE~TIME+NEURO+AGE+AGE:NEURO,random=~1|IDNR,data=hipNonmiss)
summary(model3)
## plot the table
tab <- tab_model(model3)
tab

# interaction term not sig 
# effect of age (if any) is the same across the two neuro-groups
# omit interaction term
model4<- lme(MMSE~TIME+NEURO+AGE,random=~1|IDNR,data=hipNonmiss)
summary(model4)
anova.lme(model4)
tab <- tab_model(model4)
tab

anova(model4, model3)

library(ggplot2)
#ggplot2::aes(MMSE~TIME+NEURO+AGE)
# Box plot (bp)
library(ggpubr)
bxp <- ggboxplot(hipNonmiss, x = "TIME", y = "MMSE",
                 color = "NEURO", palette = "jco")
bxp


sp <- ggscatter(hipNonmiss, x = "AGE", y = "MMSE",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "NEURO", palette = "jco", # Color by groups "cyl"
                shape = "NEURO"                   # Change point shape by groups "cyl"
)+
  scale_x_continuous(limits = c(63,96))+
  stat_cor(aes(color = NEURO), label.x = 3)  
sp

# I need to visuale the same scatter plot in 5 windows (Y=Time)

library(ggplot2)
library(gridExtra)
theme_set(theme_bw())
# to see that there is no interaction
grid.arrange(
  ggscatter(subset(hipNonmiss, TIME == 1), x = "AGE", y = "MMSE",
            add = "reg.line",               # Add regression line
            conf.int = TRUE,                # Add confidence interval
            color = "NEURO", palette = "jco", # Color by groups "cyl"
            shape = "NEURO",               # Change point shape by groups "cyl"
            main = "Time 1"
  )+
    scale_x_continuous(limits = c(63,96))+
    stat_cor(aes(color = NEURO), label.x = 3),
  ggscatter(subset(hipNonmiss, TIME == 3), x = "AGE", y = "MMSE",
            add = "reg.line",               # Add regression line
            conf.int = TRUE,                # Add confidence interval
            color = "NEURO", palette = "jco", # Color by groups "cyl"
            shape = "NEURO",                   # Change point shape by groups "cyl"
            main = "Time 3"
             )+
    scale_x_continuous(limits = c(63,96))+
    stat_cor(aes(color = NEURO), label.x = 3),
  ggscatter(subset(hipNonmiss, TIME == 5), x = "AGE", y = "MMSE",
            add = "reg.line",               # Add regression line
            conf.int = TRUE,                # Add confidence interval
            color = "NEURO", palette = "jco", # Color by groups "cyl"
            shape = "NEURO",                   # Change point shape by groups "cyl"
            main = "Time 5"
             )+
    scale_x_continuous(limits = c(63,96))+
    stat_cor(aes(color = NEURO), label.x = 3),
  ggscatter(subset(hipNonmiss, TIME == 8), x = "AGE", y = "MMSE",
            add = "reg.line",               # Add regression line
            conf.int = TRUE,                # Add confidence interval
            color = "NEURO", palette = "jco", # Color by groups "cyl"
            shape = "NEURO" ,                  # Change point shape by groups "cyl"
            main = "Time 8"
            )+
    scale_x_continuous(limits = c(63,96))+
    stat_cor(aes(color = NEURO), label.x = 3),
  ncol=2)


#References:
#  https://strengejacke.github.io/sjPlot/articles/table_css.html
#  
