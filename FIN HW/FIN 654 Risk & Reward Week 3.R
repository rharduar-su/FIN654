#Risk and Reward (Chap 1.5, 2.4, 5.4, 5.5, 5.6, 5.7)

#Question 1
#Holding-Period Returns (HPR) is the return on an asset or portfolio over the whole period during which it was held. It is defined as
#Please calculate the HPR below

End <- c(140, 110, 80)
Beginning <- c(105, 100, 96)
CD <- c(10, 5, 0)
prob <- c(0.35,0.3,0.35)

#Holding-Period Returns (HPR)
HPR <- 100*((End-Beginning+CD)/(Beginning))
HPR

#The mean of the HPR on stocks is
weighted.mean(HPR, prob)


#Question 2
#You invest $27,000 in a corporate bond selling for $900 per $1,000 par value. Over the coming year, the bond will pay interest of $75 per $1,000 of par value. The price of the bond at the end of the year will depend on the level of interest rates prevailing at that time. You construct the following scenario analysis.
#Your alternative investment is a T-bill that yields a sure rate of return of 5%. Calculate the HPR for each scenario the expected rate of return, and the risk premium on your investment.
P1 <- c(850,915,985)
D1 <- 75
P0 <- 900
investment <- 27000
prb <- c(0.2, 0.5, 0.30)

#Compute Return (HPR)
HPR <- (P1 + D1)/P0-1
HPR <- round((P1 + D1)/P0-1, 4)
HPR

#Final Value (End-ofYear Value)
FVT <- investment*(1+HPR)

#Expectation
ER <- weighted.mean(HPR, prb)
ER

# standard deviation
sig2R <- weighted.mean((HPR-ER)^2, prb)
sigR <- sqrt(sig2R)
sigR <- round(sqrt(sig2R), 4)

# print out results
print(HPR *100)
print(FVT)
print(sigR*100)

#Based on the results above, the Risk Premium is [G]%.

#rate of return
RFR <- 0.05
RP <- ER-RFR
RP*100


#Question 4
#We measure the risk-reward as the difference between the expected HPR on the index stock fund and the risk-free rate. The risk-reward is also called the risk premium.
#Given the risk premium of a given stock is 7%, the current risk-free rate is 3%, what is the expected HPR is _______%
#Add risk premium and risk free rate
#Risk reward is also called risk premium

RP <- 0.07
RFR < 0.03
HPR <- RP + RFR
HPR*100

#Question 5
#	You invest $1 million at the beginning of 2020 in an S&P 500 stock-index fund. If the rate of return in 2020 is -40%, what rate of return in 2021 will be necessary for your portfolio to recover to its original value? _____%
#Compute the final value at the end of year 2020

starting_money<-1000000
first_rate_of_return<- -0.5
value_end_of_year_one<-starting_money*(1+first_rate_of_return)

money_needed_to_be_made<- starting_money - value_end_of_year_one

#To recover 
money_needed_to_be_made/value_end_of_year_one

#Question 6
#Use the annual returns for years 1-3 in the table below, 

#Compute the arithmetic average return. [A]% (Rounded to 2 decimals)

#Compute the standard deviation of returns. [B]% (Rounded to 2 decimals)

#Compute the Sharpe ratio, assuming the risk-free rate was 6% per year.   [C] (Rounded to 2 decimals)
n <- 3
HPR <- c(0.3, 0.08, -0.20)
rf <- 0.02

#Arithmetic average return
R <- mean(HPR)
round(R*100,2)
#Standard deviation of returns
stdv <- sd(HPR)
stdv
round(stdv*100, 2)
#Sharpe Ratio
SR <- (R-rf)/(stdv)
SR
round(SR*100, 2)

#This and the next question are based on the following information.
#Question 7
#The value at risk (VaR) is the loss corresponding to a very low percentile of the entire return distribution. It is another name of quantile of a distribution. Practitioners commonly estimate the 5% VaR, meaning that 95% of the returns will exceed the VaR, and 5% of the returns will be worse.To obtain a sample estimate of VaR, we sort the observations from high to low. The VaR is the 5% percentile of the sample distribution.
#Suppose a sample comprises 84 annual returns. The bottom 6 returns are -50%, -40%, -35%, -30%, -25%, -20%. The VaR is
#Value at Risk (VaR)
sample <- 100
percentile <- 0.05
returns <- c(-0.50,-0.40,-0.35,-0.30,-.25,-0.20)
Var <- sample*percentile
Value_at_Risk<- returns[floor(Var)]*(1-(Var - floor(Var)))+returns[ceiling(Var)]*(Var - floor(Var))
Value_at_Risk*100

#Question 8
#The expected shortfall is a more informative view of downside exposure would focus instead on the the expected loss given that we find ourselves in one of the worst-case scenarios.
#Extending the previous VaR example, we assume equal probabilities for all values. Hence, we need to average across the bottom 5% of the observations. The expected shortfall is ______% (rounded to 2 decimals).

#Expected Shortfall
ES <- ((sum(returns[1:floor(Var)])+ returns[ceiling(Var)]*(Var - floor(Var))) / Var )

round(ES*100,2)

#Question 9
#	Consider the data in the table below, calculate the percentage change in the market-value-weighted index for the portfolio.
# The percentage change is ______%. (If it is a gain, the sign should be positive, if it is a loss, the sign should be negative)
InitialPriceABC <- 25
FinalPriceABC <- 20

InitialPriceXYZ <- 100
FinalPriceXYZ <- 150

InitialValueABC <- 500
InitialValueXYZ <- 100
ReturnABC <- (FinalPriceABC/InitialPriceABC)-1
ReturnXYZ <- (FinalPriceXYZ/InitialPriceXYZ)-1

WeightedABC <- InitialValueABC/(InitialValueABC+InitialValueXYZ) # 5/6
WeightedXYZ <- InitialValueXYZ/(InitialValueABC+InitialValueXYZ) # 1/6

RINDEX <- (WeightedABC * ReturnABC) + (WeightedXYZ * ReturnXYZ)
round(RINDEX*100, 2)





#Risk free return for $100 par value

par_val<-100

rT <- function(price) {
  rfr <- ((par_val/price)-1)
  return(round(rfr*100,2))
}



#Effective Annual Rate most of thw junk is just rounding so the answers match her methods

EAR<- function(price,time){
  EAR1 <- (1+round(((100/price)-1),4))^(1/time)-1
  return(round(EAR1*100,2))
}

#Annual Pecentage Rate Uses the rT as an input

APR<- function(rT,time){
  apr1<-(rT/time)
  return(round(apr1,2))
}


#The numbers from the assignment
price<-c(97.36,95.52,23.30,16.89)
time<-c(0.5,1,25,30)



#####################################################################

#The Questions
rT(price)
EAR(price,time)
APR(rT(price),time)
