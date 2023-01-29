#Capital Asset Pricing Model (CAPM) (Chap 9)

#Question 1
#Data from the last nine decades for the S&P 500 index yield the following statistics: average excess return, 8.3%; standard deviation, 20.3%
#To the extent that these average approximated investor expectations for the period, what must have been the average coefficient of risk aversion? 

#Average Excess Return
AVG_ER <- 0.083

#Standard Deviation
SD <- 0.203

#Coefficient of Risk Aversion
A <- AVG_ER/SD^2
round(A,2)

#Risk Aversion
RA <- 3.5

#Risk Premium
RP <- RA*SD^2
round(RP*100, 2)

#Question 5
# What is the fair expected rate of return for a stock that has a beta of 1.0 if the expected return on the market is 15%?

beta <-1
mkt_exp< -0.15

beta*mkt_exp

#Question 6
# The risk-free rate is 8% and the expected return on the market portfolio is 16%. A firm considers a project that is expected to have a beta of 1.3.
# What is the required rate of return on the project?_____%

rf<-0.08
exp_mkt<-0.16
beta<-1.3

rf+beta*(exp_mkt-rf)

#Question 7

# Suppose that the risk premium on the market portfolio is estimated at 8% with a 
# standard deviation of 22%. What is the required risk premium on a portfolio 
# invested 25% in Toyota and 75% in Ford if they have betas of 1.10 and 1.25, 
# respectively?

mkt_risk_pre<-0.08
mkt_sd<-0.22

W_ford<-0.75
w_toyota<-0.25

ford_beta<-1.25
toyota_beta<-1.10

w_beta<-w_toyota*toyota_beta+W_ford*ford_beta

w_beta*mkt_risk_pre



#Question 8
# The difference between the fair and actually expected rate of return on a stock is called the stock's alpha, denoted by alpha.
# Stock XYZ has an expected return of 12% and risk of beta=1. Stock ABC has expected return of 13% and beta=1.5. The market expected return is 11%, and rf equals 5%.

ER_XYZ <- 12
R_XYZ <- 1

ER_ABC <- 13
R_ABC <- 1.5

RF <- 5

ER_MKT <- 11

A_XYZ <- ER_XYZ - (RF+R_XYZ*(ER_MKT-RF))
A_XYZ

A_ABC <- ER_ABC - (RF+R_ABC*(ER_MKT-RF))
A_ABC
  
#Question 9
#STOCK XYZ IS A BETTER BUY
  
  
  
  
  




