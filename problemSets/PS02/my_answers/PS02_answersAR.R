## PROBLEM SET 2 ##

# Question 1
# Null Hypothesis = there is no relation between socioeconomic class and bribe solicitation
# Alternative Hypothesis = socioeconomic class impacts bribe solicitation
# Setting up Matrix
observed <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow=TRUE)
rownames(observed) <- c("Upper Class", "Lower Class")
colnames(observed) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning")
row_total <- rowSums(observed)
column_total <- colSums(observed)
overall <- sum(observed)
#expected frequency
expected <- round((outer(row_total, column_total) / overall),2)
expected
#overall table
full_table <- matrix(paste("obs:",observed, "| exp:", round(expected, 2)), nrow = nrow(observed),ncol = ncol(observed))
rownames(full_table) <- rownames(observed)
colnames(full_table) <- colnames(observed)
full_table <- cbind(full_table, row_total = paste(row_total))
full_table <- rbind(full_table, c(column_total = paste(column_total), Total = paste(overall)))
rownames(full_table)[nrow(full_table)] <- "Total" 
colnames(full_table)[ncol(full_table)] <- "Total" 
#part a
chi_square_stat <- sum((observed - expected)^2 / expected) #3.801141
round(chi_square_stat, 2) #3.8

#part b
chi_pvalue <- pchisq(chi_square_stat, df = (nrow(observed)-1)*(ncol(observed)-1), lower.tail = FALSE)
chi_pvalue

#Conclusion = We have sufficient evidence, p-value of 0.15 is greater than confidence level of 0.1, to fail to reject the null hypothesis that there is no relation between socioeconomic class and solicitation

#part c
residuals <- round((observed - expected),2)
#standardized_residuals <- round(residuals / sqrt(expected), 3)
standardized_residuals <- round((residuals / (sqrt(expected * (1-(row_total/overall)) %*% t(1-(column_total/overall))))),3)
head(standardized_residuals)

# Question 2
# importing and inspecting data
women <- read.csv("StatsI_Fall2024/problemSets/PS02/my_answers/women.csv")
head(women)
summary(women)

#part a
#Null-Hypothesis: The reservation policy does not impact the amount of new or repaired drinking facilities in villages. Ho : b = 0
#Alternative Hypothesis: The reservation policy does impact the amount of new or repaired drinking water facilities based on male or female leadership in villages.

#part b
bireg <- lm(water~reserved, data = women)
summary(bireg)

#part c
#The bivariate regression shows a coefficient estimate of 9.252 
#and a p-value of 0.0197. This regression interprets that at the 95% confidence level,
#villages with female leaders have about 9 more new or repaired 
#drinking water facilities than male-led villages. 
#According to the regression summary, the p-value of 0.0197 is lower than the significance level
# of 0.05,
# we are given the right evidence to reject 
#the null hypothesis that the reservation policy does not impact
#the amount of new or repaired drinking facilities in villages.
