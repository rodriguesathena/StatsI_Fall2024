# set wd 
setwd("C:/Users/athen/Documents/GitHub/StatsI_Fall2024/problemSets/PS04/my_answers")
getwd()

# Question 1
#initial setup
install.packages("car")
library(car)
data(Prestige) #prestige of Canadian occupations
help(Prestige)
occupations <- Prestige #change name for clarity
#part a professional = 1, else = 0
occupations$professional <- as.factor(ifelse(occupations$type == "prof", 1, 0))
#part b use * because continuous X dummy
parta_regression <- lm(occupations$prestige ~ occupations$income*occupations$professional)
summary(parta_regression)
#part c E[Y]= intercept+(income_coefficient×income)+(professional_coefficient×professional)+(interaction_coefficient×income×professional)
intercept <- round(parta_regression$coefficients[1],3)
income_coef <- round(parta_regression$coefficients[2],3)
professional_coef <- round(parta_regression$coefficients[3],3)
interaction_coef <- round(parta_regression$coefficients[4],3)
cat(intercept, "+", income_intercept, "* income", "+", professional_intercept, "* professional", "+", interaction_coef, "*income*professional")
#part d
#for every one-unit increase in income, a 0.003 increase in prestige score will occur for those who are not considered part of the professional category (professional = 0)
#part e
#there is a 37.781 increase in prestige score when there is a change from non-professional to professional occupation (professional changes from 0 --> 1), holding income constant.
#part f
intercept <- 21.142
income_coef <- 0.003
income <- 1000
prof_coef <- 37.781
professional <- 1
interaction_coef <- -0.002
partf_expected <- intercept + income_coef * income + prof_coef * professional + both_coef * income * professional
print(partf_expected) #59.923
# According to the predicted equation, if someone in a professional occupation (professional == 1) has an income increase of 1000 their prestige score is predicted to be 59.923
#part g
intercept <- 21.142
income_coef <- 0.003
income <- 6000
prof_coef <- 37.781
professional <- 1
notprofessional <- 0
interaction_coef <- -0.002
professional_partg <- intercept + income_coef * income + prof_coef * professional + both_coef * income * professional
notprofessional_partg <- intercept + income_coef * income + prof_coef * notprofessional + both_coef * income * notprofessional
change_in_occupation <- professional_partg - notprofessional_partg #64.923 - 39.142
print(change_in_occupation)
# when a professional occupation has an income of 6000, their prestige score is 64.923 according to the predicted equation, 
# while a non-professional occupation with an income of 6000 only has a 39.142 predicted prestige score. 
# This means that if one is to change from non-professional to professional, holding income constant, there would be a predicted 25.781 increase in prestige score

#Question 2



