
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

# Calculate the degrees of freedom
df <- (ncol(driver) - 1)(nrows(driver) - 1) # Degrees of Freedom = (no. rows - 1)(no. columns -1)
df

# Find the p-value
p_value <- 1-pchisq(chi,df=2)
p_value

# If a = 0.1, then a p-value of 0.1502306 (which is greater than 0.10) 
# is considered not significant as p > 0.10, so we fail to reject the null hypothesis.

### 
# c) 
###

# Use the str() function to see the standardised residuals and then create a 
# table by calling stdres of the chisq.test() function:

chi2 <- chisq.test(driver, correct = FALSE)

str(chi2)
chi2$stdres


### 
# d) 
###

# The standardized residual is a measure of the strength of the difference 
# between observed and expected values. It’s a measure of how significant your 
# cells are to the chi-square value. When you compare the cells, the 
# standardized residual makes it easy to see which cells are contributing the 
# most to the value, and which are contributing the least. 

# (If the value of standardized residual is lower than -2 it means that the cell 
# contains fewer observations that it was expected (the case of variables 
# independence). If the value of standardized residual is higher than 2 it means
# that the cell contains more observations that it was expected.)

# Positive residuals in cells specify a positive association between the 
# corresponding row and column variables.
# Negative residuals imply negative association between the corresponding row 
# and column variables.

# In this case, upper class drivers were more likely not to be stopped, while 
# lower class drivers were less likely to not be stopped.
# Upper class drivers, if stopped, were less likely to be asked for a bribe, 
# while lower class drivers were more likely to be asked for a bribe. 
# Upper class drivers, if stopped, were more likely to be given a warning, 
# and lower class drivers were less likely to be given a warning. 


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

