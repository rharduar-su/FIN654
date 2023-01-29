#Author: Rayanna Harduarsingh
#VSE Report Code

######################VSE-R Transactions for Investing S&P 500###############################
## Step1.1: Download the return of SPY 

install.packages("quantmod")
library(quantmod)
getSymbols('SPY', from="2019-08-31",to="2021-08-31")

tail(SPY)

## Step1.2 Compute Daily Return 
SPY_return<- dailyReturn(Ad(SPY))
tail(SPY_return)

## Step1.3 Using Utility Function 

#Risk Aversion 
#Calculated score was 14
A <- round(0.5+10/14,2)
A #1.21

Invest<-100000
rf<- 0.02/252 
SPYm<-mean(SPY_return)    
SPYsd<-sd(SPY_return)
y<- (SPYm - rf)/(A*SPYsd^2) 
y
Invest_SPY<- Invest*y
Invest_SPY

price <- 449.04
proposed_shares = Invest_SPY/price
proposed_shares

##################################VSE-R Portfolio Performance################################

#setting working directory 
setwd("/Users/rayannaharduarsingh/Documents/FIN 654 VSE Report")
#loading csv file 
portfolio_performance <- read.csv("Portfolio Performance - Rayanna Harduarsingh.csv", header = TRUE, sep=",")

#remove dollar signs from net worth column
portfolio_performance$Net.Worth = as.numeric(gsub("[\\$,]", "", portfolio_performance$Net.Worth))
#viewing portfolio performance data set
View(portfolio_performance)
class(portfolio_performance$Date)
#Change the class of variables to Date
DATE <- as.Date(as.character(portfolio_performance$Date), "%m/%d/%Y")
DATE
#Computing daily returns
networtht <- portfolio_performance$Net.Worth
networtht_1 <- portfolio_performance$Net.Worth[-1]
Rt <- (networtht-networtht_1)/networtht_1
round(Rt*100, 4)

#Calculating Sharpe Ratio

#getting average daily returns
avg <- mean(Rt)
avg
#getting standard deviation of returns
stdv <- sd(Rt)
stdv
#calculating sharpe ratio
SR <- (avg/stdv)*sqrt(252)
round(SR*100,4) #5.8971

#Plot the Net Wealth from the first day to the last day 
jpeg(filename = "VSE Net Wealth.jpeg")
plot(DATE, 
     networtht, 
     type = "o",
     pch = 19,
     col = "red",
     xlab = "Date", 
     ylab = "Net Wealth",
     main = "LX: Net Wealth; Sharpe Ratio = 5.8971",
     ylim = c(100000, 106000))
axis(1, labels = FALSE)

## Draw the x-axis labels.
text(x= DATE, format(DATE, "%b %d"), 
     ## Rotate the labels by 35 degrees.
     srt = 35,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 1.2)
dev.off()







