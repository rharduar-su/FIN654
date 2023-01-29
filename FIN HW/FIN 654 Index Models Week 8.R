#Index Models (Chap 8.1, 8.2, 8.3)

#Question 1
#A portfolio management organization analyzes 60 stocks and constructs a 
# mean-variance efficient portfolio using only these 60 securities. How many 
# estimates of expected returns, variances, and covariances are needed to optimize 
# this portfolio?

# [A] estimates of expected returns
# [B] estimates of variances
# [C] estimates of covariances

n<-60 

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

ER_a<-0.10
ER_b<-0.02
ER_c<-0.17

beta_a<-1
beta_b<-0.2
beta_c<-1.7

sd_a<-0.40
sd_b<-0.30
sd_c<-0.50

cap_a<-3000
cap_b<-1940
cap_c<-1360
total_cap<-cap_a+cap_b+cap_c

w_a<-cap_a/total_cap
w_b<-cap_b/total_cap
w_c<-cap_c/total_cap

mkt_sd<-0.25
#A 
#What is the mean excess return of the three-stock value-weighted portfolio?
mer <- w_a*ER_a+w_b*ER_b+w_c*ER_c
round(mer,4)

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
