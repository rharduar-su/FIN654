# Author: Rayanna Harduarsingh
# Date: November 18th, 2021

#loading in the csv file
setwd("/Users/rayannaharduarsingh/Documents/FIN EXAM 2")

MSFT<-read.csv("MSFT.csv",header=TRUE,sep=",")
View(MSFT)

MSFT_RET<-MSFT$MSFT
marketEXERT <- MSFT$Market_EXERT
RF<-MSFT$Rf
MSFTEXERT<-MSFT_RET-RF

DATE<-as.Date(as.character(MSFT$DATE),"%Y%m%d")

dim(MSFT)

Model <- lm(MSFTEXERT~marketEXERT)
summary(Model)
Modelsum<-summary(Model)

#Class of Model
class(Modelsum)

#Names of variables
names(Modelsum)

#What is being reported in modelsum
Modelsum

#Get the coefficient panel
Modelsum$coefficients

mdlsum <- summary(Model)
class(mdlsum)
length(mdlsum)

#IBM company specific surprise
mdlresid<-resid(Model)
mdlresid
summary(mdlresid)
class(mdlresid)
length(mdlresid)

#Predicted IBM excess return
mdlpred<-predict(Model)
summary(mdlpred)
class(mdlpred)
length(mdlpred)

testValue<-0
n<-length(marketEXERT) 
#s2: compute test statistics
estcoeff<-Modelsum$coefficients[1] 
eststd<-Modelsum$coefficients[1,2] 
tstats<-(estcoeff-testValue)/eststd 
#or 
tstats<-Modelsum$coefficients[1,3]
#s3: decision rule for two sided test 
decRule<-abs(tstats)>qt(1-0.05/2, n-1-1)
#s4: conclusion
Result<-ifelse(decRule, "Reject", "Can’t Reject")
Result

testValue<-0
Model<-lm(IBMEXERT~marketEXERT)
tstats<-(Model$coefficients[1]-testValue)/summary(Model)[["coefficients"]][1,2]
Result<-ifelse(abs(tstats)>qt(0.975, length(marketEXERT)-1-1),"Reject", "Can't Reject")
Result



testValue<-1
#s2: compute test statistics
estcoeff<-Modelsum$coefficients[2] 
eststd<-Modelsum$coefficients[2,2] 
tstats<-(estcoeff-testValue)/eststd
#s3: decision rule for two sided test 
decRule<-tstats>qt(1-0.05, n-1-1)
#s4: conclusion
Result<-ifelse(decRule, "Reject", "Can’t Reject")
Result



