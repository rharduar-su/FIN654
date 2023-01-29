# Author: Rayanna Harduarsingh
# Date: September 21st, 2021

#set the working directory
#setwd("H:/Desktop/Case 1_RayannaHarduarsingh")
#dir()

setwd("/Users/rayannaharduarsingh/Downloads/Case1CAPM.csv")

#read the csv file
#file.choose()
CAPM <- read.csv("Case1CAPM.csv", 
                 header = TRUE, 
                 sep =",")
View(CAPM)
#getting the dimensions and names of the variables
dim(CAPM)
names(CAPM)
str(CAPM)

#converting date column to date format
class(CAPM$DATE)
DATE <- as.Date(as.character(CAPM$DATE), "%Y%m%d")

#Create the excess returns of IBM
ibmRET <- CAPM$IBMRET
marketEXERT <- CAPM$MarketEXRET
RF <- CAPM$RF
IBMEXERT <-ibmRET-RF
IBMEXERT
#FIG Time-Series Plot of Daily Returns

#IBM
jpeg(filename = "Case1_IBMEXERT.jpeg")
plot(DATE, 
     IBMEXERT, 
     type = "l", 
     xlab = "year", 
     ylab = "daily excess return(%)",
     main = "IBM Excess Return",
     ylim = c(-15, 15))
dev.off()

#Market
jpeg(filename = "Case1_MRKEXERT.jpeg")
plot(DATE, 
     marketEXERT, 
     type = "l", 
     xlab = "year", 
     ylab = "daily excess return(%)",
     main = "Market Excess Return",
     ylim = c(-15, 15))
dev.off()

#Compute the arithmetic mean of IBM excess return

IBMMean <- mean(IBMEXERT)*252

#Compute the standard deviation of IBM Return

IBMSTD <- sd(IBMEXERT)*sqrt(252)

#Compute the Sharpe Ratio of IBM excess return 

IBMSR <- IBMMean / IBMSTD

IBMMean
IBMSTD
IBMSR

#Market mean, standard deviation, sharpe ratio

marketMean <- mean(marketEXERT)*252

marketSTD <- sd(marketEXERT)*sqrt(252)

marketSR <- marketMean / marketSTD

marketMean
marketSTD
marketSR
round(marketSTD, 4)
round(marketSR,4)

#Compute the 5% Value at Risk of IBM & Market excess return
#IBM
IBMVaR <- quantile(IBMEXERT, probs = c(0.05))
#Market
MKTVaR<-quantile(marketEXERT, probs = c(0.05))

#Compute the 5% Expected Shortfall of IBM & Market excess return 
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

#ES uses decimal returns not percentage

#IBM
IBMEXERT_raw <- IBMEXERT/100

IBMES_raw <- ES(IBMEXERT_raw, 
                p=.05, 
                method = "historical")

IBMES <- IBMES_raw*100

#Market
MKT_raw<-marketEXERT/100

MKTES_raw<-ES(MKT_raw, 
              p=.05,
              method="historical") 

MKTES<-MKTES_raw*100

IBMVaR
MKTVaR
IBMES
MKTES

#Compute the correlation
IBMcMarket<-cor(IBMEXERT, marketEXERT)
IBMcMarket

#Boxplot of IBM excess return

jpeg(filename = "Case1_IBMEXERT_Daily.jpeg")
boxplot(IBMEXERT, 
        main = "Daily IBM Excess Returns (percentage)",
        ylab = "daily excess return(%)",
        ylim = c(-15,15))
dev.off()

#Boxplot of Market excess return
jpeg(filename = "Case1_marketEXERT_Daily.jpeg")
boxplot(marketEXERT, 
        main = "Daily Market Excess Returns (percentage)",
        ylab = "daily excess return(%)",
        ylim = c(-15,15))
dev.off()

# Histogram of IBM excess return

jpeg(filename = "Case1_histIBMEXERT.jpeg")
hist(IBMEXERT,
     main="Daily IBM Excess returns (percentage)",
     prob =TRUE,
     xlab = "IBM Excess Return",
     ylab = "Density",
     ylim = c(0,0.25),
     breaks = 50)
dev.off()

# Histogram of Market excess return

jpeg(filename = "Case1_histmarketEXERT.jpeg")
hist(marketEXERT, 
     main = " Daily Market Excess returns (percentage)", 
     prob=TRUE, 
     ylim = c(0,0.25),
     xlab = "Market Excess Return",
     ylab = "Density",
     breaks = 50)
dev.off()


# Compute the skewness and kurtosis of IBM excess return

install.packages("e1071")
library(e1071)

IBMskew <-skewness(IBMEXERT)
IBMkurto <-kurtosis(IBMEXERT)

IBMskew
IBMkurto

# Compute the skewness and kurtosis of Market excess return
MKTskew <-skewness(marketEXERT)
MKTkurto <-kurtosis(marketEXERT)

MKTskew
MKTkurto

#Hypothesis testing

#Jarque-Berra Test
install.packages("tseries")
library(tseries)
jarque.bera.test(IBMEXERT)
jarque.bera.test(marketEXERT)

#Lilliefors Test
install.packages("nortest")
library(nortest)
lillie.test(IBMEXERT)
lillie.test(marketEXERT)

## Construct each column of our table.
Name<-c("Mean:", "Std:", "Skewness:", "Kurtosis:","Sharpe Ratio","Value at Risk","Expected Shortfall","Correlation:" )

IBM<-c(IBMMean, IBMSTD, IBMskew, IBMkurto, IBMSR, IBMVaR, IBMES, IBMcMarket)

Market<-c(marketMean, marketSTD, MKTskew, MKTkurto, marketSR,MKTVaR, MKTES, NA)

#Construct table
summary <- data.frame(round(IBM,4), round(Market,4),row.names =Name,check.names = TRUE)
View(summary)
