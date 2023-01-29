#Finance: Asset Return (Chap 2.1, 2.2, 2.3, 5.1, 5.2, 5.3)

#Question 1
#	Given the price, P(T), of a treasury bond with $100 par value and maturity of T years, we can use the formula below to calculate the total risk-free return available for a horizon of T years ( r subscript f left parenthesis T right parenthesis ).
#To compare investments in different horizon, we can calculate the effective annual rate (EAR), defined as the percentage increase in funds invested over 1-year horizon, by using the following equation:
#Annualized rates on short-term investments (by convention, T<1 year) often are reported using simple rather compound interest. These are called annual percentage rates (APRs),
#Given the example for half-year and 1 year, please complete the form for 25 years and 30 years.

#Par-Value
par <- 100
#Time
t <- 25
#Price
p <- 23.30

#Risk-Free Return
rt <- (par/p)-1
rt

#Effective Annual Rate (EAR)
EAR <- (1+rt)^(1/25)

EAR2 <- EAR-1
EAR2

APR <- rt/t
APR

#Question 2
#The rate of realized return an investor receives from buying a common stock and holding it for a given period of time is equal to the cash dividends received plus the capital gain during the holding period divided by the purchase price of the security. 
#Calculate the realized return over one trading day (h=1 trading day) and realized return over the past week (h=5 trading days). Round to the two decimal places. If the return is not available, enter NA.
#Price
P <- c(100, 100, 105, 115.5, 90, 96.3)

#Dividend
D <- c(0, 0, 0, 0, 25.5, 0)

#Inflow
PT <- P[2:6]
DT <- D[2:6]

#Outflow
P0 <- P[1:5]

#Daily return
R <- round((PT+DT)/P0-1, 4)
R

#Weekly return
Rh5 <- round(prod(R+1)-1, 4)
Rh5
#Print our in percentage
print(R*100)
print(Rh5*100)

###############################################################
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

################################################################
# Finance: Capital Allocation to Risk Assets (Chap 6)

#Question 1
#Consider a risky portfolio. The end-of-year cash flow derived from the portfolio will be either $70,000 or $200,000 with equal probabilities of .5. The alternative risk-free investment in T-bills pays 6% per year. a. If you require a risk premium of 8%, how much will you be willing to pay for the portfolio? (Rounded to two decimals)
Prob <- 0.5
RP <- 0.08
RF <- 0.06
RR <- RP + RF + 1
RR
CF1 <- 70000
CF2 <- 200000

CF <- (Prob*CF1) + (Prob*CF2)
CF

pay<- CF/RR
round(pay,2)

#Question 2
#Investment Management Inc. (IMI) uses the capital market line to make asset allocation recommendations. IMI derives the following forecasts:

#∙ Expected return on the market portfolio: 12% 

#∙ Standard deviation on the market portfolio: 20%

#∙ Risk-free rate: 5% 

#Samuel Johnson seeks IMI’s advice for a portfolio asset allocation. Johnson informs IMI that he wants the standard deviation of the portfolio to equal half of the standard deviation for the market portfolio. Using the capital market line, what expected return can IMI provide subject to Johnson’s risk constraint? ____ % (Rounded to one decimal)
ER <- 0.12
SD <- 0.20
RF <- 0.05

SR <- (ER-RF)/SD

ERP <- RF+(SR*(SD/2))
ERP*100

#The following five questions are based on the following information.
#Question 3
#You manage a risky portfolio with an expected rate of return of 18% and a standard deviation of 28%. The T-bill rate is 8%. Your client chooses to invest 70% of a portfolio in your fund and 30% in an essentially risk-free money market fund. What is the standard deviation of the rate of return on his portfolio?
ERR <- 0.18
SD <- 0.28
RF <- 0.08
investment <- 0.7
SD2 <- SD*investment
SD2*100


#Question 4
#What is the reward-to-volatility (Sharpe) ratio (S) of your risky portfolio?
SR <- (ERR-RF)/SD
round(SR, 4)


#Question 5
#Suppose that your risky portfolio includes the following investments in the given proportions:

#Stock A 25%
#Stock B 32%
#Stock C 43%

#What are the investment proportions of your client’s overall portfolio, including the position in T-bills (30.0% T-bills)? 
stockA <- 0.25
stockB <- 0.32
stockC <- 0.43

stockA*investment*100
stockB*investment*100
stockC*investment*100

#Question 6
#Your client’s degree of risk aversion is A = 3.5.
#What proportion, y, of the total investment should be invested in your fund?
RA <- 3.5
y <- (ERR-RF)/(RA*SD^2)
round(y,4) 


#Question 7
#What is the expected value of the rate of return on your client’s optimized portfolio?
ERC <- y*ERR+(1-y)*RF
round(ERC*100,2)

#Question 8
#You estimate that a passive portfolio, for example, one invested in a risky portfolio that mimics the S&P 500 stock index, yields an expected rate of return of 13% with a standard deviation of 25%. You manage an active portfolio with expected return 18% and standard deviation 28%. The risk-free rate is 8%. Consider again with A = 3.5. a. If this client chose to invest in the passive portfolio as his risky portfolio, what proportion, y, would he select?
ERR <- 0.13
RF <- 0.08
RA <- 3.5
SD <- 0.25

Y <- (ERR-RF)/(RA*(SD^2))
round(Y*100,2)

#Question 9
#	Consider a portfolio that offers an expected rate of return of 12% and a standard deviation of 18%. T-bills offer a risk-free 7% rate of return. What is the maximum level of risk aversion for which the risky portfolio is still preferred to T-bills?
ERR <- 0.12
SD <- 0.18
RF <- 0.07

U <- (ERR-RF)/(0.5*(SD^2))
U

############################################################
#Optimal Risky Portfolios (Chap 7.1, 7.2, 7.3, 7.4)

#Q1 and Q2 are based on the following informations.

#Question 1
#Suppose the expected return of equity is E left parenthesis r subscript E right parenthesis equals 13 percent sign, the expected return of debt is E left parenthesis r subscript D right parenthesis equals 8 percent sign. The standard deviation of equity is sigma subscript E equals 20 percent sign, the standard deviation of debt is sigma subscript D equals 12 percent sign. Compute the portfolio opportunity set for the debt and equity funds when the correlation coefficient between them is rho equals 0.25.

#The global minimum-variance portfolio is constructed so that:
#Expected return of Debt
ERD <- 0.1

#Expected return of Equity
ERE <- 0.2

#Stanard deviation of Debt
SDD <- 0.2

#Standard deviation of Equity
SDE <- 0.4

#Correltion coefficient
p <- 0.3

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
ERB <- 0.2
SDA <- 0.2
SDB <- 0.4
TA <- 0.02
TB <- 0
rf <- TA+TB
rf
p <- 0.3

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

RA <- 4

# y0 on optimal risky portfolio
wP <- (ER-rf)/sP^2/RA
wP

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

##############################################
#Index Models (Chap 8.1, 8.2, 8.3)

#Question 1
#A portfolio management organization analyzes 60 stocks and constructs a 
# mean-variance efficient portfolio using only these 60 securities. How many 
# estimates of expected returns, variances, and covariances are needed to optimize 
# this portfolio?

# [A] estimates of expected returns
# [B] estimates of variances
# [C] estimates of covariances

n<-100 

#exp returns
n
#var
n

#covars
n*(n-1)/2


#Question 2
# A portfolio management organization analyzes 60 stocks and constructs a mean-variance
# efficient portfolio using only these 60 securities. If one could safely assume that
# stock market returns closely resemble a single-index structure, how many estimates
# would be needed?

# [A] estimates of alpha subscript i
# [B] estimates of beta subscript i
# [C] estimates of sigma squared left parenthesis e subscript i right parenthesis
# [D] estimates of Eleft parenthesis r subscript M minus r subscript f right parenthesis
# [E] estimates of sigma subscript M su

#Question 3
# The market index has a standard deviation of 22% and the risk-free rate is 8%. 
#What are the standard deviations of stocks A and B? Assume that risk-free rate is constant. 

#The standard deviation of stock A  is [A]%. (Rounded into two decimals)

#The standard deviation of stock B is [B]%. (Rounded into two decimals )

a_ex<-0.13
b_ex<-0.18
beta_a<-0.8
beta_b<-1.2
a_firm<-0.30
b_firm<-0.40
mkt_sd<-0.22
rf<-0.08


sd_a <- sqrt((beta_a^2)*(mkt_sd^2)+a_firm^2)
round(sd_a*100, 2)

sd_b <- sqrt((beta_b^2)*(mkt_sd^2)+b_firm^2)
round(sd_b*100, 2)

#Question 4
# The market index has a standard deviation of 22% and the risk-free rate is 8%. 
#Suppose that we were to construct a portfolio with proportions:
# Stock A: .30
# Stock B: .45
# T-bills : .25
# Compute the nonsystematic standard deviation of the portfolio.
# 
# the nonsystematic standard deviation of the portfolio is [A]%. (Rounded into two decimals)

wA<-0.30
wB<-0.45
wrf<-0.25

wbeta<-wA*beta_a+wB*beta_b
w_mkt_sd<-(wA+wB)*mkt_sd
wfirm<-wA*a_firm+wB*b_firm

ans<-sqrt((wbeta^2)*(w_mkt_sd^2)+wfirm^2)
round(ans*100,2)


a_firm<-0.30
b_firm<-0.40

x <- (wA^2*a_firm^2) + (wB^2*b_firm^2) + (wrf^2*0)
y <- x^1/2
round(y*100,2)

#correct answer 
a_firm<-30
b_firm<-40

wA<-0.30
wB<-0.45
wrf<-0.25


wfirm <- (wA^2*a_firm^2)+(wB^2*b_firm^2)+(wrf^2)

sqrt(wfirm)

#Question 5
#The data below describe a three-stock financial market that satisfies the single-index model.
#The standard deviation of the market-index portfolio is 25%

ER_a<- 0.10
ER_b<- 0.15
ER_c<- -0.04

beta_a<-1
beta_b<-1.5
beta_c<- -0.5

sd_a<-0.40
sd_b<-0.40
sd_c<-0.50

cap_a<-3000
cap_b<-3000
cap_c<-3000
total_cap<-cap_a+cap_b+cap_c

w_a<-cap_a/total_cap
w_b<-cap_b/total_cap
w_c<-cap_c/total_cap

mkt_sd<-0.25

#A 
#What is the mean excess return of the three-stock value-weighted portfolio?
mer <- w_a*ER_a+w_b*ER_b+w_c*ER_c
round(mer*100,4)

#B
#What is the covariance between stock A and stock B? 
covariance <- beta_a*beta_b*(mkt_sd^2)
covariance

#C
#What is the covariance between stock B and the market-index?
cov <- beta_b*mkt_sd^2
cov

#Break down the variance of stock B into its systematic and firm-specific components.
#D
systematic_risk <- beta_b^2 * mkt_sd^2
systematic_risk

#E
firm_specific <- sd_b^2 - beta_b^2 * mkt_sd^2
firm_specific

###################################################
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

rf<-0.02
exp_mkt<-0.10
beta<-1.5

ERM <- rf+beta*(exp_mkt-rf)
ERM
RP <- ERM-rf
RP
alpha <- ERM-(rf+beta*(exp_mkt-rf))
alpha

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
R_XYZ <- 1.5

ER_ABC <- 20
R_ABC <- 2

RF <- 2

ER_MKT <- 10

A_XYZ <- ER_XYZ - (RF+R_XYZ*(ER_MKT-RF))
A_XYZ

A_ABC <- ER_ABC - (RF+R_ABC*(ER_MKT-RF))
A_ABC

#Question 9
#STOCK XYZ IS A BETTER BUY



