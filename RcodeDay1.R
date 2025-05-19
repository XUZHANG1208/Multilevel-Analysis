setwd("~/Documents/JOB/UANTWERPEN/FLAMES/Multilevel/Data")
#setwd("C:/Documents/JOB/UANTWERPEN/FLAMES/Multilevel/Data")

#install.packages("lme4")
library(lme4)
#install.packages("Matrix")
library(Matrix)

###### Exercises 1 ##### 

# classic ANOVA and regression assume all observations are independent
# here, we have related data - observations are not independent :
  # hip fracture : multiple measurements (of MMSE) over time in the same individual
  # = longitudinal study
  # SII : here there is non-independence at two levels :
    # pupils within one class are not independent. 
    # classes within schools are not independent 
# The purpose of multilevel (or mixed) models is 
# to account for the relatedness between these observations

##### Exercise 2 #####
# read in datasets
#############################
siiData <- read.table("SII.txt",header=T)
#############################

# Co-erce schoolid, classid, childid, sex and ses to a factor
siiData$schoolid<-as.factor(siiData$schoolid)
siiData$classid<-as.factor(siiData$classid)
siiData$childid<-as.factor(siiData$childid)
siiData$sex<-as.factor(siiData$sex)
levels(siiData$sex)<-c("male","female")


#### Exercise 3 ####
# index plot 
# plot average mathgain by school versus schoolID
# start with empty plot

allSchool<-unique(siiData$schoolid)
nSchool<-length(allSchool)


plot(as.numeric(siiData$schoolid),siiData$mathgain,type="n",xlab="School ID",
     ylab="Mathgain",main="Mathgain by school")

# Adjust Y-range
# Individual mathgain values are more exterme than average over school

# range(siiData$mathgain)

plot(as.numeric(siiData$schoolid),siiData$mathgain,type="n",xlab="School ID",
     ylab="Mathgain",main="Mathgain by school", ylim = c(-20,130))

for (i in 1:nSchool){
  tmp<-siiData[siiData$schoolid==allSchool[i],]
  tmp$schoolid<-as.numeric(tmp$schoolid)
  
  #mean for the males	
  tmpMale<-tmp[tmp$sex=="male",]
  points(allSchool[i],mean(tmpMale$mathgain),pch=16)
  
  #mean for the females
  tmpFemale<-tmp[tmp$sex=="female",]
  points(allSchool[i],mean(tmpFemale$mathgain),pch=1)
}

legend("topleft",c("Male","Female"),pch=c(16,1))


####Exercise 4 ####
#############################
# read in hipfracture dataset
############################

hipData<-read.table("hipfracture2.txt",header = T)

# co-ercion of categorical ariables
hipData$IDNR<-as.factor(hipData$IDNR)
hipData$NEURO<-as.factor(hipData$NEURO)
levels(hipData$NEURO)<-c("not neuro-psy","neuro-psy")


#### Exercise 5 ####
# plot over time
# plot patients one-by one in loop

allID<-unique(hipData$IDNR)
nID<-length(allID)

# empty plot of MMSE vs time
# one plot with both groups
plot(MMSE~TIME,data=hipData,type="n")

for(i in 1:nID){
  tmp<-hipData[hipData$IDNR==allID[i],]
  points(MMSE~TIME,data=tmp,col=tmp$NEURO)
  lines(MMSE~TIME,data=tmp,col=tmp$NEURO)
}

# 2 separate plots
par(mfrow=c(1,2))

plot(MMSE~TIME,data=hipData,type="n", main="Non-neuropsy",xlab="Time (days)")
for(i in 1:nID){
  tmp<-hipData[hipData$IDNR==allID[i]&hipData$NEURO=="not neuro-psy",]
  points(MMSE~TIME,data=tmp,col="black")
  lines(MMSE~TIME,data=tmp,col="black")
}

plot(MMSE~TIME,data=hipData,type="n", main="Neuropsy",xlab="Time (days)")
for(i in 1:nID){
  tmp<-hipData[hipData$IDNR==allID[i]&hipData$NEURO=="neuro-psy",]
  points(MMSE~TIME,data=tmp,col="red")
  lines(MMSE~TIME,data=tmp,col="red")
}

