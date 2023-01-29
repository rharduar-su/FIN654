#Optimal Risky Portfolios (Chap 7.1, 7.2, 7.3, 7.4)

#Q1 and Q2 are based on the following informations.

#Question 1
#Suppose the expected return of equity is E left parenthesis r subscript E right parenthesis equals 13 percent sign, the expected return of debt is E left parenthesis r subscript D right parenthesis equals 8 percent sign. The standard deviation of equity is sigma subscript E equals 20 percent sign, the standard deviation of debt is sigma subscript D equals 12 percent sign. Compute the portfolio opportunity set for the debt and equity funds when the correlation coefficient between them is rho equals 0.25.

#The global minimum-variance portfolio is constructed so that:
#Expected return of Debt
ERD <- 0.08

#Expected return of Equity
ERE <- 0.13

#Stanard deviation of Debt
SDD <- 0.12

#Standard deviation of Equity
SDE <- 0.2

#Correltion coefficient
p <- 0.25

#Global Minimum-Variance

#Weight on Debt
WD <- (SDE^2-p*SDD*SDE)/(SDD^2+SDE^2-2*p*SDD*SDE) 
round(WD, 4)

#Weight on Equity
WE <-1-WD
round(WE, 4)

#Question 2
#	Based on the results of the previous question, calculate the expected return and standard deviation of the portfolio.
#Expected Return

ERP <- WD*ERD+WE*ERE
round(ERP*100, 2)

#Standard Deviation/Risk
sG <-sqrt(WD^2*SDD^2+WE^2*SDE^2+2*WD*WE*p*SDD*SDE)
round(sG*100, 2)

#Q3 to Q6 is based on the following informations.

#Question 3
#The universe of available securities includes two risky stock funds, A and B, and T-bills. The data for the universe are as follows:
#The correlation coefficients between funds A and B is -0.2
#The covariance between A and B is minus

ERA <- 0.1
ERB <- 0.3
SDA <- 0.2
SDB <- 0.6
TA <- 0.05
TB <- 0
rf <- TA+TB
rf
p <- -0.2

cov <-  SDA*SDB*p
cov

#Question 4
#Find the optimal risky portfolio, P, and its expected return and standard deviation.

EE1 <- ERA-rf # debt's risk premium
EE1
EE2 <- ERB-rf # equity's risk premium
EE2

#Tangency Portfolio

#Weight on Debt (porportion of A)
WA <-(EE1*SDB^2-EE2*p*SDA*SDB)/(EE1*SDB^2+EE2*SDA^2-(EE1+EE2)*p*SDA*SDB)
round(WA, 4)
#Weight on Debt (porportion of B)
WB <-1-WA
round(WB, 4)

#Expected Return
ER <-WA*ERA+WB*ERB

round(ER*100, 2)

#Standard Deviation/Risk
sP<-sqrt(WA^2*SDA^2+WB^2*SDB^2+2*WA*WB*p*SDA*SDB) # risk
round(sP*100, 2)

#Question 5
#	Based on the results of previous questions, calculate the Sharpe Ratio of this portfolio.
#Sharpe Ratio
SR <-(ER-rf)/sP
round(SR, 4)

#Question 6
#Based on the results of previous questions, how much will an investor with risk aversion A = 5 invest in funds A and B and in T-bills.?
#Risk Aversion
#The fraction of total wealth in A is [A] % (Rounded to 2 decimals)

#The  fraction of total wealth in B is [B]% (Rounded to 2 decimals)

#The  fraction of total wealth in T-bills is [C]% (Rounded to 2 decimals)

RA <- 5

# y0 on optimal risky portfolio
wP <- (ER-rf)/sP^2/RA

#Absolute weight on debt
yA <-wP*WA 
round(yA*100, 2)

#Absolute weight on equity
yB <-wP*WB
round(yB*100, 2)

#Total Wealth
#1-y0  on risk free rate (total wealth)
wF <- 1-wP
round(wF*100, 2)






