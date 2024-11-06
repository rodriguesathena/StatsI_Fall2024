# set wd 
setwd("C:/Users/athen/Documents/GitHub/StatsI_Fall2024/problemSets/PS03/my_answers")
getwd()

# read in data
incumbent <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

# Question 1 difference in campaign spending and impact on incumbent vote
# part 1 regression 
question1_regression <- lm(voteshare ~ difflog, data = incumbent)
print(question1_regression)
summary(question1_regression)
#findings
# Based on the model, for every one-unit increase in difflog there is an expected increase of about 0.04 in voteshare
# This shows a positive relationship, as difflog increases, voteshare will generally increase as well
# part 2 scatter plot
question1_scatter <- plot(incumbent$difflog, incumbent$voteshare,  
                         xlab = "difflog", 
                         ylab = "voteshare",  
                         main = "scatter plot between difflog and voteshare") +
                    abline(question1_regression, col="red")
question1_scatter
# part 3 residuals
question1_residuals <- question1_regression$residuals
# part 4 prediction equation y-hat = beta0 + beta1(x)
intercept <- round(question1_regression$coefficients[1],3)
slope <- round(question1_regression$coefficients[2],3)
cat(intercept, "+", slope, "* difflog")

# Question 2 difference in campaign spending and vote share of pres. candidate from incumbent party
# part 1 regression 
question2_regression <- lm(presvote ~ difflog, data = incumbent)
print(question2_regression)
summary(question2_regression)
#findings
# Based on the model, a one-unit increase in difflog will generally see a 0.02 increase in presvote.
# This is a positive relationship that shows when difflog rises presvote values tend to slightly increase.
# part 2 scatter plot
question2_scatter <- plot(incumbent$difflog, incumbent$presvote,  
                            xlab = "difflog", 
                            ylab = "presvote",  
                            main = "scatter plot between difflog and presvote") +
  abline(question2_regression, col="red")
question2_scatter
# part 3 residuals
question2_residuals <- question2_regression$residuals
# part 4 prediction equation y-hat = beta0 + beta1(x)
intercept2 <- round(question2_regression$coefficients[1],3)
slope2 <- round(question2_regression$coefficients[2],3)
cat(intercept2, "+", slope2, "* difflog")

# Question 3 pres. candidate incumbent party vote share association with incumbent success
# part 1 regression 
question3_regression <- lm(voteshare ~ presvote, data = incumbent)
print(question3_regression)
summary(question3_regression)
#findings
# A one-unit increase in presvote is associated with a 0.38 increase in voteshare.
# This is a positive relation showing as presvote increases so does voteshare.
# part 2 scatter plot
question3_scatter <- plot(incumbent$presvote, incumbent$voteshare,  
                          xlab = "presvote", 
                          ylab = "voteshare",  
                          main = "scatter plot between presvote and voteshare") +
  abline(question3_regression, col="red")
question3_scatter
# part 3 residuals
question3_residuals <- question3_regression$residuals
# part 4 prediction equation y-hat = beta0 + beta1(x)
intercept3 <- round(question3_regression$coefficients[1],3)
slope3 <- round(question3_regression$coefficients[2],3)
cat(intercept3, "+", slope3, "* voteshare")

# Question 4
# part 1 regression
question4_regression <- lm(question1_residuals ~ question2_residuals, data = incumbent)
print(question4_regression)
summary(question4_regression)
#findings
#Running this regression shows if the unexplained parts of presvote (question2_residuals) help in explaining the remaining variation in voteshare.
#The positive coefficient of 0.2569 shows that some of the residual vaiation in presvote can help in explaining the residual variation in voteshare.
# part 2 scatter plot
question4_scatter <- plot(question2_residuals, question1_residuals,  
                          xlab = "question 2 residuals", 
                          ylab = "question 1 residuals",  
                          main = "residuals of question 1 and question 2") +
  abline(question4_regression, col="red")
question4_scatter
# part 3 prediction equation y-hat = beta0 + beta1(x)
intercept4 <- round(question4_regression$coefficients[1],3)
slope4 <- round(question4_regression$coefficients[2],3)
cat(intercept4, "+", slope4, "* question2_residuals")

# Question 5 multivariate
# part 1 regression
question5_regression <- lm(incumbent$voteshare ~ incumbent$difflog + incumbent$presvote)
print(question5_regression)
summary(question5_regression)
# When presvote is held constant, a one-unit increase in difflog results in a 0.04 increase in voteshare.
# When difflog is held constant, a one-unit increase in presvote results in a 0.26 increase in voteshare.
# Both presvote and difflog have positive relationships with voteshare, however, presvote has a stronger impact.
# part 2 prediction equation
intercept5 <- round(question5_regression$coefficients[1],3)
slope5difflog <- round(question5_regression$coefficients[2],3)
slope5presvote <- round(question5_regression$coefficients[3],3)
cat(intercept5, "+", slope5difflog, "* difflog", "+", slope5presvote, "* presvote")

