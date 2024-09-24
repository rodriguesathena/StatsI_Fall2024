#####################
# load libraries
#set wd()
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
#margin of error
ystderr <- sd(y)/sqrt(length(y)) #standard error 2.618575
confidence <- qt((.9)+ (1-.9)/2, df = length(y)-1)
#lower bound
upper_90 <- (ymean + (confidence)*ystderr)  # 102.7345
lower_90 <- (ymean - (confidence)*ystderr)  # 94.14554
t.test(y, conf.level = 0.9, alternative = "two.sided")
# overall
lower_90
ymean
upper_90

#hypothesis test a = 0.05
#Null Hypothesis: mean = 100   Alternative Hypothesis: mean > 100
t.test(y, mu = 100, conf.level = 0.95, alternative = "greater")   #p-value: 0.7215
# p-value > 0.05 means fail to reject the Null
#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
head(expenditure)
