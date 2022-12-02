#####################
# load libraries
# set wd
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

lapply(c("stringr"),  pkgTest)
lapply(c("stargazer"),  pkgTest)
lapply(c("tidyverse"),  pkgTest)
lapply(c("dplyr"),  pkgTest)
lapply(c("ggplot2"),  pkgTest)
lapply(c("ggpubr"), pkgTest)

# set wd for current folder
setwd("~/Desktop/ASDS/StatsI_Fall2022/problemSets/PS04/My_Answers/")

install.packages("xtable")
library(xtable)

install.packages(car)
library(car)
data(Prestige)
help(Prestige)

################
# Question 1
################


# (a) Create a new variable professional by recoding the variable type so that professionals
# are coded as 1, and blue and white collar workers are coded as 0 (Hint: ifelse).

# create dummy variables

professional <- ifelse(Prestige$type == 'prof', 1, 0)

# create data frame to use for regression

df_new <- cbind(Prestige, professional)

# view data frame
xtable(head(df_new))


# (b) Run a linear model with prestige as an outcome and income, professional, and the
# interaction of the two as predictors (Note: this is a continuous x dummy interaction.)

model1 <- lm(prestige ~ income + professional + (income * professional), data = Prestige)

stargazer(model1, type = "latex", out = "model1.latex", title = 
            "Prestige and income-professional Regression")

# (c) Write the prediction equation based on the result.

prestige = 21.142 + (0.003*income) + (37.781*professional) + (-0.002*37.7812800*0.0031709)


# (d) Interpret the co-efficient for income.

# The coefficient for income is 0.003, which is positive. This indicates that as 
# the value of income increases, the mean of the dependant variable also tends to increase,
# and a one-unit shift in income (holding all other varables constant) causes a 0.003 unit 
# increase in prestige. 


# (e) Interpret the co-efficient for professional.

# The coefficient for professional is 37.781, which is positive. This indicates that
# income is higher for the dummy variable 'professional' than for the reference group 
# (white and blue collar workers), and that type 'professional' indicates a 37.781 
# increase in income.


# (f) What is the effect of a $1,000 increase in income on prestige score for professional
# occupations? In other words, we are interested in the marginal effect of income when
# the variable professional takes the value of 1. Calculate the change in ^y associated
# with a $1,000 increase in income based on your answer for (c).

prestige_1000increase = 21.142 + (0.003*1000) + (37.781*1) + (-0.002*37.7812800*0.0031709)
prestige_1000increase

# (g) What is the effect of changing one's occupations from non-professional to professional
# when her income is $6,000? We are interested in the marginal effect of professional
# jobs when the variable income takes the value of 6; 000. Calculate the change in ^y
# based on your answer for (c).

presitge_notprof = 21.142 + (0.003*6000) + (37.781*0) + (-0.002*37.7812800*0.0031709)
presitge_notprof

presitge_prof = 21.142 + (0.003*6000) + (37.781*1) + (-0.002*37.7812800*0.0031709)
presitge_prof

newPrestige <- presitge_prof - presitge_notprof 
newPrestige

# This means that changing one's occupations from non-professional to professional
# when income is $6,000 leads to a 37.781 increase in prestige


################
# Question 2
################


# (a) Use the results from a linear regression to determine whether having these yard signs
# in a precinct affects vote share (e.g., conduct a hypothesis test with a = .05).

# H0: B1 (the slope of the regression line for the effect of living in a precinct 
# assigned lawn signs on proportion of vote to Ken Cuccinelli) = 0
# H1: B1 (the slope of the regression line for the effect of living in a precinct 
# assigned lawn signs on proportion of vote to Ken Cuccinelli) != 0

# a = 0.05

# get the test statistic 

t1 <- 0.042 / 0.016
t1

# get critical value at 0.05
n1 <- 30

degreesf <- n1 - 2 
degreesf

p_value <- 2*pt(abs(t1), n1-2, lower.tail = F)
p_value

# Since t1 of 2.625 is greater than p value of 0.01, we fail to reject the null 
# hypothesis that the slope of the regression line for the effect of living in a precinct 
# assigned lawn signs on proportion of vote to Ken Cuccinelli  = 0 at a 0.05% significance level. 

# (b) Use the results to determine whether being next to precincts with these yard signs
# affects vote share (e.g., conduct a hypothesis test with a = .05).

# H0: B2 (slope of the regression line for the effect of living in a precinct 
# adjacent to lawn signs on proportion of vote to Ken Cuccinelli) = 0
# H1: B2 (the slope of the regression line for the effect of living in a precinct 
# adjacent to lawn signs on proportion of vote to Ken Cuccinelli) != 0

# a = 0.05

# get the test statistic 

t2 <- 0.042 / 0.013
t2

# get critical value at 0.05
n2 <- 76

degreesf <- n2 - 2 
degreesf

p_value <- 2*pt(abs(t2), n2-2, lower.tail = F)
p_value

# Since t2 of 3.231 is greater than p value of 0.002, we fail to reject the null 
# hypothesis that the slope of the regression line for the effect of living in a precinct 
# adjacent to lawn signs on proportion of vote to Ken Cuccinelli = 0 at a 0.05% significance level. 

# (c) Interpret the coefficient for the constant term substantively.

# In this model, a constant of 0.302 indicates that the value that would be 
# predicted for the proportion of the vote to go to Ken Cucinelli if all the 
# independent variables (yard signs and yard sign adjacent) were simultaneously 
# equal to zero is 0.302. 

# (d) Evaluate the model fit for this regression. What does this tell us about the importance
# of yard signs versus other factors that are not modeled?

# The R^2 for this model is 0.094. This low R^2 value tells us that compared to other factors not 
# modeled in this regression, yard signs have little impact on proportion of vote going to the opponent. 



