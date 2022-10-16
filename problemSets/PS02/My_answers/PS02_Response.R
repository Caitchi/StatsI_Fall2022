
#####################
# load libraries
# set wd
# clear global .envir
###################### Packages

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

library(tidyverse)


# set working directory
setwd("~/Desktop/ASDS/StatsI_Fall2022/problemSets/PS02/My_answers/")


#####################
# Problem 1
#####################

### 
# a) 
###

# H0: The variables are statistically independent 
# H1: The variables are statistically dependent 

# Create a matrix of our data and name the rows and columns

driver <- matrix(c(14,6,7,7,7,1),ncol=3,byrow=TRUE)  
colnames(driver) <- c("Not Stopped","Bribe Requested","Stopped/Given Warning")
rownames(driver) <- c("Upper Class","Lower Class")
driver <- as.table(driver)
driver

barplot(driver,legend=TRUE,beside=TRUE,main='Drivers stopped by police')


margin.table(driver)
margin.table(driver,1)
margin.table(driver,2)


# Calculate the table of expected values by multiplying the vectors of the 
# margins and dividing by the total number of observations (The “t” function 
# takes the transpose of the array.)

expected <- as.array(margin.table(driver,1)) %*% t(as.array(margin.table(driver,2))) / margin.table(driver)
expected


#  We need the square of the difference between the two tables divided by the 
# expected values. The sum of all these values is the Chi-squared statistic:

chi <- sum((expected - as.array(driver))^2/expected)
chi


# Check our work using chisq.test() and/or summary

chi2 <- chisq.test(driver, correct=FALSE)
chi2

summary(driver)


### 
# b) 
###


# Calculate the degrees of freedom = (no. columns - 1)(no. rows -1)

df <- (ncol(driver) - 1)*(nrow(driver) - 1) 
df                 

# Find the p-value

p_value <- 1-pchisq(chi, df=2)
p_value


# If a = 0.1, then a p-value of 0.1502306 (which is greater than 0.10) 
# is considered not significant as p > 0.10, so we fail to reject the null hypothesis. 

# Check if this is valid -> one tailed test and not two tailed so don't
# need to divide by 2


### 
# c) 
###



# We calculate the standard residuals by subtracting the #expected values 
# from the observation and dividing by the #square root of the expected values 

std_residuals <- (driver - expected) / sqrt(expected)
std_residuals


# check if R agrees 
str(chi2)
chi2$residuals


### 
# d) 
###


# In this dataset, the standardized residuals indicate that more lower class 
# drivers were approached for a bribe than expected (std res slightly greater than
# 1) , however fewer lower class drivers were stopped and given a warning than 
# expected. A comparable number of upper and lower class drivers were not 
# stopped at all when then numbers in each class are accounted for (std res 
# close to 0). 

# Fewer bribes than expected were solicited from upper class individuals,  
# however more than expected were stopped and given a warning. However, the 
# absolute value of the standardized residuals in these cells were less than 1 
# and so not considered significant. 

# Overall the most significant contributor to the chi squared test were lower 
# class drivers being asked for bribes and lower class drivers stopped and given 
# a warning. However, the relatively small values for the standardized residuals
# support our rejection of the alternate hypothesis that these variables are 
# dependent and do support the adoption of the null hypothesis. 



#####################
# Problem 2
#####################

P2 <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
P2

###
# a)
###

# H0: The reservation policy has no effect on the number of new or repaired 
# drinking water facilities in the villages. 
# H1: On average, the reservation policy has either a positive or negative effect on the 
# number of new or repaired drinking water facilities in the villages.

###
# b)
###

Reserved_Effect_Water <- lm(water ~ reserved, data = P2)
summary(Reserved_Effect_Water)

# As the resulting p-value of 0.0197 for reserved is less than the usual
# significance level a = 0.05, we can conclude that our sample data provides 
# enough evidence to reject the null hypothesis that the reservation policy has 
# no effect on the number of new or repaired drinking water facillities on the 
# village. On average, the the reservation policy has either a positive or negative effect on the 
# number of new or repaired drinking water facilities in the villages.

###
# c)
###

# Convert into a data matrix

matrix_coef <- summary(Reserved_Effect_Water)$coefficients
matrix_coef  

# Subset to extract only the coefficient estimates 

my_estimates <- matrix_coef[ , 1]
my_estimates 

# As the table shows, the coefficient estimate of the reserved variable is 
# 9.252423. 

# This positive coefficient indicates that as the value of the independent 
# variable (reservation policy for women leaders) increases, the mean of the 
# dependent variable (no. new or repaired drinking water facilities since the 
# policy started) also tends to increase. 


# The number of new or repaired drinking water facilities, on average, in a 
# village with a reservation policy is expected to be 9.25 times more
# compared to the number f new or repaired drinking water facilities in a 
# village without a reservation policy.

