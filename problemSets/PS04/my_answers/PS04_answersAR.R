# set wd 
setwd("C:/Users/athen/Documents/GitHub/StatsI_Fall2024/problemSets/PS04/my_answers")
getwd()

# Question 1

#initial setup
library(car)
data(Prestige) #prestige of Canadian occupations
help(Prestige)
occupations <- Prestige #change name for clarity
#part a professional = 1, else = 0
occupations$professional <- as.factor(ifelse(occupations$type == "prof", 1, 0))
#part b use * because continuous X dummy
parta_regression <- lm(occupations$prestige ~ occupations$income*occupations$professional)
summary(parta_regression)
#part c
intercept <- round(parta_regression$coefficients[1],3)
income_coef <- round(parta_regression$coefficients[2],3)
professional_coef <- round(parta_regression$coefficients[3],3)
interaction_coef <- round(parta_regression$coefficients[4],3)
cat(intercept, "+", income_coef, "* income", "+", professional_coef, "* professional", "+", interaction_coef, "*income*professional")
#part d
#for every one-unit increase in income, a 0.003 increase in prestige score will occur for those who are not considered part of the professional category (professional = 0)
#part e
#there is a 37.781 increase in prestige score when there is a change from non-professional to professional occupation (professional changes from 0 --> 1), holding income constant.
#part f
intercept <- 21.142
income_coef <- 0.003
income <- 1000
no_income <- 0
prof_coef <- 37.781
professional <- 1
interaction_coef <- -0.002
income_partf <- intercept + income_coef * income + prof_coef * professional + interaction_coef * income * professional #59.923
noincome_partf <- intercept + income_coef * no_income + prof_coef * professional + interaction_coef * no_income * professional #58.923
change_in_income <- income_partf - noincome_partf #59.923 - 58.923
print(change_in_income)
# According to the predicted equation, if someone in a professional occupation (professional = 1) has an income increase of 1000 their prestige score is predicted to be 59.92
#compared to no income but still professional having a prestige score of 58.923. This shows there is only a 1prestige point difference
# in a professional job going from an income of $0 to
#part g
intercept <- 21.142
income_coef <- 0.003
income <- 6000
prof_coef <- 37.781
professional <- 1
notprofessional <- 0
interaction_coef <- -0.002
professional_partg <- intercept + income_coef * income + prof_coef * professional + interaction_coef * income * professional #64.923
notprofessional_partg <- intercept + income_coef * income + prof_coef * notprofessional + interaction_coef * income * notprofessional #39.142
change_in_occupation <- professional_partg - notprofessional_partg #64.923 - 39.142
print(change_in_occupation)
# when a professional occupation has an income of 6000, their prestige score is 64.923 according to the predicted equation, 
# while a non-professional occupation with an income of 6000 only has a 39.142 predicted prestige score. 
# This means that if one is to change from non-professional to professional, holding income constant, there would be a predicted 25.781 increase in prestige score

#Question 2
#part a
# assumptions
# population distribution of y is normal for each combination of values
# standard deviation of the conditional distribution of responses on y is the same at each combination
# the sample is randomly selected
# hypothesis
# Ho: b1 = 0; lawn signs in  precincts do not impact Cuccinelli's vote share
# Ha: b1 != 0; lawn signs in precincts have an impact on Cuccinelli's vote share
# finding t-statistic
assigned_coef <- 0.042
assigned_se <- 0.016
assigned_statistic <- assigned_coef / assigned_se #2.625
# finding p-value
degrees <- 131 - 2 - 1  #N - predictors - 1
assigned_p_value <- 2* pt(t_statistic, df = degrees, lower.tail = FALSE) #0.00972
# conclusion
# reject the null as 0.00972 < 0.05; a one-unit increase in lawn signs in assigned precincts does impact Cuccinelli's vote share 

#part b
# assumptions
# population distribution of y is normal for each combination of values
# standard deviation of the conditional distribution of responses on y is the same at each combination
# the sample is randomly selected
# hypothesis
# Ho: b1 = 0; lawn signs in adjacent precincts do not impact Cuccinelli's vote share
# Ha: b1 != 0; law signs in adjacent precincts have an impact on Cuccinelli's vote share
# finding t-statistic
adjacent_coef <- 0.042
adjacent_se <- 0.013
adjacent_t_statistic <- adjacent_coef / adjacent_se #3.231
# finding p-value
degrees <- 131 - 2 - 1  #N - predictors - 1
adjacent_p_value <- 2* pt(adjacent_t_statistic, df = degrees, lower.tail = FALSE) #0.00157
# conclusion
# reject the null as 0.00157 < 0.05; a one-unit increase in lawn signs in adjacent precincts does impact Cuccinelli's vote share 

#part c
# the coefficient for the constant term is 0.302, with a standard error of 0.011.
# This means that controlling for the two variables, no lawn signs in assigned or adjacent precincts, 
# is expected to result in an average vote share of 0.302 for Cuccinelli.

#part d
#The R^2 value for this regression is 0.094.This means that yard signs do not carry 
#as significant an impact on vote share as other factors may.

