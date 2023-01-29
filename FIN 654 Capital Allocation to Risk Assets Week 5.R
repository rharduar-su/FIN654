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








