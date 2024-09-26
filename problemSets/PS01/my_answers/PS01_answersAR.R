#####################
# load libraries
setwd("C:/Users/athen/Documents/GitHub/StatsI_Fall2024/problemSets/PS01/my_answers")
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

#lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# 90% Confidence Interval: point estimate +/- margin of error
#point estimate
ymean <- mean(y) #mean 98.44
df <- length(y)-1
ystderr <- sd(y)/sqrt(length(y)) # standard error 2.618575
confidence <- qt((.9)+ (1-.9)/2, df) # confidence 1.710882
upper_90 <- (ymean + (confidence)*ystderr)  # 102.9201
lower_90 <- (ymean - (confidence)*ystderr)  # 93.95993
# overall
lower_90
ymean
upper_90
# Confidence Interval 93.95993 < x < 102.9201

#hypothesis test a = 0.05
#Step 1 Assumptions: Y is numeric, the sample size is 25
class(y)
length(y)
#Step 2 Null Hypothesis: mean = 100   Alternative Hypothesis: mean > 100
#Step 3 Calculate test statistic
test_stat <- ((ymean - 100)/ystderr)
#Step 4 Calculate p-value
pvalue <- (1-pt(test_stat, df))
pvalue
#p-value: 0.7215
#check work t.test(y, mu = 100, conf.level = 0.95, alternative = "greater")  
# p-value > 0.05 means fail to reject the Null
#####################
# Problem 2
#####################
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
head(expenditure)

# relationships between Y and X1,2,3
#png(file="all_relationships.png")
cor(expenditure$X1,expenditure$Y)
cor(expenditure$X2,expenditure$Y)
cor(expenditure$X3,expenditure$Y)
cor(expenditure$X1,expenditure$X2) 
cor(expenditure$X1,expenditure$X3)
cor(expenditure$X2,expenditure$X3)


par(mfrow =c(2,3))
plot(expenditure$X1,expenditure$Y, col='red', 
     xlab = "per capita personal income in state", 
     ylab = "expenditure per capita on housing assistance", 
     main = "Housing Expenditure vs. Personal Income", 
     text(1450,120 , sprintf("Corr=%s", round(cor(expenditure$X1,expenditure$Y), 4))))
plot(expenditure$X2,expenditure$Y, col='blue', 
     xlab = "# residents 'financially insecure' per 100,000", 
     ylab = "expenditure per capita on housing assistance",  
     main = "Housing Expenditure vs. Financial Insecurity",
     text(200, 120, sprintf("Corr=%s", round(cor(expenditure$X2,expenditure$Y), 4)))) 
plot(expenditure$X3,expenditure$Y, col='orange', 
     xlab = "# residents residing in urban areas per thousand", 
     ylab = "expenditure per capita on housing assistance",  
     main = "Housing Expenditure vs. Urban Residents",
     text(500, 120, sprintf("Corr=%s", round(cor(expenditure$X3,expenditure$Y), 4)))) 
plot(expenditure$X1, expenditure$X2, col='pink',  
     xlab = "per capita personal income in state", 
     ylab = "# residents 'financially insecure' per 100,000",  
     main = "Financial Insecurity vs. Personal Income",
     text(1400, 500, sprintf("Corr=%s", round(cor(expenditure$X1,expenditure$X2), 4)))) 
plot(expenditure$X1, expenditure$X3, col='purple',  
     xlab = "per capita personal income in state", 
     ylab = "# residents residing in urban areas per thousand",  
     main = "Urban Residents vs. Personal Income",
     text(1400, 800, sprintf("Corr=%s", round(cor(expenditure$X1,expenditure$X3), 4)))) 
plot(expenditure$X2, expenditure$X3, col='magenta', 
     xlab = "# residents 'financially insecure' per 100,000", 
     ylab = "# residents residing in urban areas per thousand", 
     main = "Urban Residents vs. Financial Insecurity",
     text(250, 800, sprintf("Corr=%s", round(cor(expenditure$X2,expenditure$X3), 4)))) 

# relationship between Y and Region
#png(file="boxplot_yandregion.png")
par(mfrow =c(1,1))
boxplot(expenditure$Y ~ expenditure$Region, 
        main = "Expenditure on Housing Assistance by Region",
        xlab = "region", 
        ylab = "expenditure per capita w/ housing assistance")
text(1, 75, sprintf("Mean=%s", round(mean(expenditure$Y[expenditure$Region == "1"]), 2)))
text(2, 90, sprintf("Mean=%s", round(mean(expenditure$Y[expenditure$Region == "2"]), 2)))
text(3, 65, sprintf("Mean=%s", round(mean(expenditure$Y[expenditure$Region == "3"]), 2)))
text(4, 90, sprintf("Mean=%s", round(mean(expenditure$Y[expenditure$Region == "4"]), 2)))

# relationship between Y and X1 and Region
#png(file="yx1_byregion.png")

plot(expenditure$X1,expenditure$Y, 
     col=expenditure$Region, pch = expenditure$Region, 
     xlab = "per capita personal income in state", 
     ylab = "expenditure per capita w/ housing assistance", 
     main = "Housing Expenditure vs. Personal Income") 
legend("bottomright", legend = c("1", "2", "3", "4"),
       col = unique(expenditure$Region),     
       pch = unique(expenditure$Region),   
       title = "Region")
cor(expenditure$X1[expenditure$Region == "1"],expenditure$Y[expenditure$Region == "1"])
cor(expenditure$X1[expenditure$Region == "2"],expenditure$Y[expenditure$Region == "2"])
cor(expenditure$X1[expenditure$Region == "3"],expenditure$Y[expenditure$Region == "3"])
cor(expenditure$X1[expenditure$Region == "4"],expenditure$Y[expenditure$Region == "4"])
