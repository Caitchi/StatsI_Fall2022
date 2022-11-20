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

library(tidyverse)


# here is where you load any necessary packages
# ex: stringr

lapply(c("stringr"),  pkgTest)
lapply(c("stargazer"),  pkgTest)
lapply(c("tidyverse"),  pkgTest)
lapply(c("dplyr"),  pkgTest)
lapply(c("ggplot2"),  pkgTest)
lapply(c("ggpubr"), pkgTest)

# set wd for current folder
setwd("~/Desktop/ASDS/StatsI_Fall2022/problemSets/PS03/My_Answers/")

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/incumbents_subset.csv")
head(inc.sub)

################
# Question 1
################

# 1. Run a regression where the outcome variable is voteshare and the explanatory variable
#    is difflog.

Diff_Vote_Reg <- lm(voteshare ~ difflog, data = inc.sub) # fit the model

summary(Diff_Vote_Reg)

stargazer(Diff_Vote_Reg, type = "latex", out = "Diff_Vote_Reg.latex", title = 
            "Difflog-Voteshare Regression")


# 2. Make a scatterplot of the two variables and add the regression line.


ggplot(inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +     # regression line 
  theme_bw() +
  labs(title = "Voteshare as a function of Difflog",
       x = "Difflog",
       y = "Voteshare")



# 3. Save the residuals of the model in a separate object.

inc.sub$residuals <- residuals(Diff_Vote_Reg) # Save the residual values
Voteshare_res <- inc.sub$residuals
head(Voteshare_res) # check that residuals have been correctly assigned to the new object

# 4. Write the prediction equation.

# The mathematical formula of the linear regression can be written as y = b0 + b1*x + e, where:
  
# b0 and b1 are known as the regression beta coefficients or parameters:
#   b0 is the intercept of the regression line; that is the predicted value when x = 0.
#   b1 is the slope of the regression line.

# e is the error term (also known as the residual errors), the part of y that can be explained by the regression model

# The estimated regression line equation can be written as follows: 
# voteshare = 0.58 + 0.04*difflog


################
# Question 2
################

# 1. Run a regression where the outcome variable is presvote and the explanatory variable
#    is difflog.

Diff_Presvote_Reg <- lm(presvote ~ difflog, data = inc.sub) # fit the model

summary(Diff_Presvote_Reg)

stargazer(Diff_Presvote_Reg, type = "latex\vspace{.25cm}", out = "Diff_Presvote_Reg.latex", title = 
            "Difflog-Presvote Regression")

# 2. Make a scatterplot of the two variables and add the regression line.

ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +     # regression line 
  theme_bw() +
  labs(title = "Presvote as a function of Difflog",
       x = "Difflog",
       y = "Presvote")


# 3. Save the residuals of the model in a separate object.

inc.sub$residuals <- residuals(Diff_Presvote_Reg) # Save the residual values
Presvote_res <- inc.sub$residuals
head(Presvote_res) # check the first line t ensure residuals were correctly saved 

# 4. Write the prediction equation.

# The estimated regression line equation can be written as follows: 
# presvote = 0.51 + 0.02*difflog 


################
# Question 3
################

# 1. Run a regression where the outcome variable is voteshare and the explanatory variable
#    is presvote.

Presvote_Voteshare_Reg <- lm(voteshare ~ presvote, data = inc.sub) # fit the model

summary(Presvote_Voteshare_Reg)

stargazer(Presvote_Voteshare_Reg, type = "latex", out = "Presvote_Voteshare_Reg.latex", title = 
            "Presvote-Voteshare Regression")

# 2. Make a scatterplot of the two variables and add the regression line.

ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +     # regression line 
  theme_bw() +
  labs(title = "Voteshare as a function of Presvote",
       x = "Presvote",
       y = "Voteshare")


# 3. Write the prediction equation.

# The estimated regression line equation can be written as follows: 
# voteshare = 0.44 + 0.39*presvote



################
# Question 4
################


# 1. Run a regression where the outcome variable is the residuals from Question 1 and the
#    explanatory variable is the residuals from Question 2.

Q1Resid_Q2Resid_Reg <- lm(Voteshare_res ~ Presvote_res, data = inc.sub) # fit the model

summary(Q1Resid_Q2Resid_Reg)

stargazer(Q1Resid_Q2Resid_Reg, type = "latex", out = "Q1Resid_Q2Resid_Reg.latex", title = 
            "Q1 Residuals-Q2 Residuals Regression")

# 2. Make a scatterplot of the two residuals and add the regression line.

ggplot(inc.sub, aes(x = Presvote_res, y = Voteshare_res)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +     # regression line 
  theme_bw() +
  labs(title = "Voteshare Residuals as a function of Presvote Residuals",
       x = "Presvote Residuals",
       y = "Voteshare Residuals")

# 3. Write the prediction equation.

# The estimated regression line equation can be written as follows: 
# Q1Resid = -4.860e-18 + 2.569e-01*Q2Resid


################
# Question 5
################

# 1. Run a regression where the outcome variable is the incumbent's voteshare and the
#    explanatory variables are difflog and presvote.

Diff_Presvote_Voteshare_Reg <- lm(voteshare ~ difflog + presvote, data = inc.sub) # fit the model

summary(Diff_Presvote_Voteshare_Reg)

stargazer(Diff_Presvote_Voteshare_Reg, type = "latex", out = "Q5.latex", title = 
            "Difflog & Presvote - Voteshare Regression")

# 2. Write the prediction equation.

# The estimated regression line equation can be written as follows: 
# voteshare = 0.45 + (0.04*difflog) + (0.26*presvote)

# 3. What is it in this output that is identical to the output in Question 4? 
#    Why do you think this is the case?

# The Residual Std. Error	in the output for this question is identical to the 
# Residual Std. Error	in the output for Q4. 

# This is because running a regression of the residuals of voteshare ~ difflog against 
# the residuals of presvote ~ difflog as we do in Q4, tells us how much of the unexplained 
# variation in voteshare is influenced by presvote. 

# In Q5, we are essentially showing the same thing in a different way, by running 
# a regression of voteshare against difflog AND presvote, we can see how much of the unexplained 
# variance in voteshare ~ difflog is explained by presvote, and this means that the 
# Residual Std. Error will be the same 


