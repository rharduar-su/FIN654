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







