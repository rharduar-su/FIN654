#Author: Rayanna Harduarsingh
#FIN 654: Case 2

#change location of file

#setwd("H:\\Desktop\\FIN 654\\Case2_RayannaHarduarsingh")
#dir()
file.choose()

#loading in the csv file
setwd("/Users/rayannaharduarsingh/Documents/Case2_RayannaHarduarsingh")

CAPM<-read.csv("Case2CAPM.csv",header=TRUE,sep=",")

#getting the dimensions
dim(CAPM)

#getting the names of the columns
names(CAPM)

#View
View(CAPM)
DATE<-as.Date(as.character(CAPM$DATE),"%Y%m%d")

#Create the excess returns of IBM from 1994 to 2015
ibmRET<-CAPM$IBMRET
marketEXERT <- CAPM$MarketEXRET
RF<-CAPM$RF
IBMEXERT<-ibmRET-RF

#Linear regression of IBM and Market
Model<-lm(IBMEXERT~marketEXERT)
Model
summary(Model)

#Class of Model
class(Model)
length(Model)

#Names of variables
names(Model)

#Formula and get the coefficient panel coefficients
Model$call
Model$coefficients

#Get acess to intercept and slope
Model$coefficients[1]
Model$coefficients[2]

#Summary of the model
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
#Get the coefficient panel

Modelsum$coefficients

#Gooodness of fit
Modelsum$r.squared

#Create a Device
jpeg(filename = "Case2_OLSLINE.jpeg")

#scatterplot
plot(marketEXERT, IBMEXERT,
     main="Scatter Plot of IBM Excess returns Vs. Market Excess returns",
     xlab= "Market Excess returns",
     ylab="IBM Excess returns")
#Add the OLS line
abline(Model, col="blue")
#Close the device and Save
dev.off()

# Step 9: Hypothesis Test: t-stats

# Step 9.1: Is the adjusted returns zero 
#s1: According to the null
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

## Step 9.2: Is the sensitivity higher than one? #s1: According to the null
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

#VSE-R Single-Index Model
#Download risk free rates
install.packages("quantmod")
library(quantmod)
getSymbols('DGS3MO', src='FRED')
getSymbols('SPY', from = "2018-07-31", to = "2020-07-31")
getSymbols('XLV', from = "2018-07-31", to = "2020-07-31")
View(DGS3MO)
RF<-DGS3MO/100/252

# Step2.1 Using for loop to download 11 ETFs

nmlist <- c('XLK', 'XLV', 'XLF', 'XLY', 'XLP', 
            'XLU', 'XLE', 'XLC', 'XLI', 'XLRE', 'XLB')

# Check the 1st element in nmlist
nmlist[1]

#Returns loop

Returns <- list()
for(i in 1: length(nmlist)){
  getSymbols(nmlist[i], from = '2018-07-31', to = '2020-07-31')
  ret <- dailyReturn(Ad(get(nmlist[i])))
  Returns[[i]] <- ret
}
class(Returns)
returns <- do.call(cbind, Returns)
returns <- round(returns, digits = 4)
colnames(returns) <- nmlist


#merge(x,y, z, all=): merge two or more data.frames by common columns or row names.
#all = False only dates with available data from all x, y and z are included in the output (2018-07-31 to 2020-07-31)

#Merging data frames
SPY_return <- dailyReturn(Ad(SPY))
CAPM <- merge(returns, RF, SPY_return, all=FALSE)
CAPM <- CAPM[-c(15)]
CAPM <- CAPM[-c(5)]
CAPM <- subset(CAPM, select = -c(XLY.1))
View(CAPM)
colnames(CAPM)[13] <- "Market"

#Build model for XLV
marketEXERT <- CAPM$Market-CAPM$DGS3MO

XLVmodel <- lm((XLV-DGS3MO)~marketEXERT, data=CAPM)

#XLVmodel<-lm((CAPM$XLV-CAPM$DGS3MO)~marketEXERT)

summary(XLVmodel)

XLVmodel$coefficients
Intercept <- XLVmodel$coefficients[1]
Intercept
Slope <- XLVmodel$coefficients[2]
Slope

Alpha <- as.numeric(Intercept)
Alpha
Beta <- as.numeric(Slope)
Beta

Mdlsum <- summary(XLVmodel)
Mdlsum
Sigma <- Mdlsum$sigma
sigma

Ratio <- Alpha/Sigma
Ratio

#Build models for 11 ETFs and create statistic matrix
colname <- c(" Alpha ", " Sigma ", " Beta ", " Ratio ")
reg_results <- matrix(NA, nrow = 11, ncol = 4, dimnames = list(nmlist, colname))

View(reg_results)
reg_results[1,1]
reg_results[5,3]

for (i in 1: length(nmlist)){
  marketEXERT <- CAPM$Market-CAPM$DGS3MO
  
  Model <- lm((get(nmlist[i])-DGS3MO)~marketEXERT, data = CAPM)
  
  alpha <- as.numeric(Model$coefficients[1])
  sigma <- summary(Model)$sigma
  beta <- as.numeric(Model$coefficients[2])
  ratio <- alpha/sigma
  
  reg_results[i,1]<-alpha
  reg_results[i,2]<-sigma
  reg_results[i,3]<-beta
  reg_results[i,4]<-ratio
}

View(reg_results)











