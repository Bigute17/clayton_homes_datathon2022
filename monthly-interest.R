# P = price of house
# i = interest rate %/12
# n = number of months to pay off the loan


mortgage_clac = function(p,i,n){
  p*(i*(1 + i)^n ) / ((1 + i)^n-1)
}
mortgage_clac(100000,0.0385/12,180)

affordability = FULL_DATASET %>%
  mutate(thirty_percent_income = 0.3*Median_Income_USD,
         thirty_percent_income_per_month = thirty_percent_income/12,
         monthly_payment_fifteen_year=mortgage_clac(ZHVI,(Rate/100)/12,180),
         monthly_payment_thirty_year=mortgage_clac(ZHVI,(Rate/100)/12,360),
         qual_income = monthly_payment_thirty_year*4*12,
         affordable_dmy = ifelse(monthly_payment_thirty_year<thirty_percent_income_per_month,1,0),
         
        
         affordability_index = (Median_Income_USD/qual_income)*100)



summary(affordability, na.rm=TRUE)

compoundInterest((3.85/100),periods=15,frequency = c(1,2,12,365), net.value = FALSE)

mortgage_clac = function(p,r,n,t){
  p*(1+r/n)^(n*t)
  }

mortgage_clac(200000,0.0385,2,30)


#new


mortgage_clac = function(pv,i,n){
  (pv*i*((1+i)^n))/(((1+i)^n)-1)
}

mortgage_clac(175000,0.04125,30)
