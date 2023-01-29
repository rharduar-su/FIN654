## Step1.1: Download the return of SPY VSE_laixu.R install.packages("quantmod")

library(quantmod)
getSymbols('SPY', from="2018-07-31",to="2020-07-31")

tail(SPY)

## Step1.2 Compute Daily Return 
SPY_return<- dailyReturn(Ad(SPY))
tail(SPY_return)

## Step1.3 Using Utility Function 
A <- 1.5
Invest<-100000
rf<- 0.02/252 
SPYm<-mean(SPY_return) 
SPYsd<-sd(SPY_return)
y<- (SPYm -rf)/(A*SPYsd^2) 
Invest_SPY<- Invest*y
Invest_SPY

price <- 449.04
proposed_shares = Invest_SPY/price
proposed_shares

#44904
