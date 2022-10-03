################################
# StatsI_Fall2022 Problem Set 01
################################

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
setwd("~/Documents/GitHub/StatsI_Fall2022/problemSets/PS01/template/")

#####################
# Problem 1
#####################

## 01. Find a 90% confidence interval for the average student IQ in the school.

# Create a vector for IQ
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 
       80, 97, 95, 111, 114, 89, 95, 126, 98)

# Confidence Interval = (sample mean)+/-(confidence level value)*(standard error)

# Find sample mean (xbar), standard deviation (sd), sample size (n)
ybar <- mean(y)
sd <- sd(y)
n <- length(y)


# Find the confidence level value (z)
conf.level <- 0.9
z <- qt((1+conf.level)/2, df= length(y)-1) # this gets me to the 95th probability 
# in order to establish a 90% CI
z


# Find the standard error 
se <- sd(y)/sqrt(n)
se

# Multiply the critical value by the standard error
CI <- z*se
CI

# We can now determine the lower and upper confidence interval boundaries.
lowerinterval <- ybar - CI
lowerinterval 

upperinterval <- ybar + CI
upperinterval 

# With repeated sampling, we would anticipate that the mean IQ of students in 
# the school falls between 90 out of 100 re-samplings from our population 

# checking my work using the t.test function: 
t.test(y, conf.level = 0.9, alternative = "two.sided")

## 02. Using the same sample, conduct the appropriate hypothesis test with 
# a = 0:05 to test whether the average student IQ in this school is higher than 
# the average IQ score (100) among all the schools in the country.

# Use a QQ plot to determine if our IQ variable is
# normally distributed
qqnorm(y)
qqline(y,
       distribution = qnorm)

# The scatter plot shows a strong positive correlation so we can conclude that our 
# IQ variable is indeed normally distributed

# State the null hypothesis and alternative hypothesis

# H0: mu <= 100
# H1: mu > 100

mu <- 100

# Set significance level
a = 0.05 

# Use a right-tailed t-test, because population size is unknown, our sample size
# is < 30 and we are specifying the direction i.e. the alternative hypothesis 
# states that the parameter is bigger than the value specified in the null hypothesis


# Start by getting the mean 
ybar <- mean(y)

# Then get the standard deviation
sd <- sd(y)

# Then the standard error
se <- sd(y)/sqrt(length(y)) # Create an object with our standard error
se

# Calculate the degrees of freedom
df <- n-1
df

# Calculate our test statistic
t <- (ybar-mu)/(sd/sqrt(n))
t

# Create a dataframe with relevant info, and label it
data <- c(ybar, se, t, pt(-abs(t), df, lower.tail=FALSE))
names(data) <- c("Mean", "Std Error", "t_score", "p-value")
round(data, 4)

# Test our hypothesis using the t.test function

t.test(y,
       mu = 100, 
       var.equal = FALSE, 
       alternative = "greater",
       conf.level = .9)


# Draw a conclusion

# Our p-value = 0.7215 which is greater than 0.05, which means we cannot 
# reject the null hypothesis, as we cannot conclude that the mean IQ of the
# students in this school is significantly greater than 100.


# Quesion 2


expenditure <- read.delim("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt", header=T)
expenditure

# Examine our data
str(expenditure)
dim(expenditure) # No. participants no. variables
nrow(expenditure)# No. participants
ncol(expenditure) # No. variables
expenditure$STATE
expenditure$Y
expenditure$Region


# Please plot the relationships among Y, X1, X2, and X3? What are the correlations
#among them (you just need to describe the graph and the relationships among them)?


# Region is a categorical variable, yet it is
# currently not structured as a factor. Instead, it is structured as an integer.
# The as.factor() function converts a variable into a factor. We use 
# the $ symbol to tell R which variable in your dataset we want to change. 

expenditure$Region<-as.factor(expenditure$Region)

expenditure$Region<-factor(expenditure$Region,
                           levels=c(1,2,3,4),
                           labels=c("North East", "North Central", "South", "West"))

# We can double check the structure to see if this worked properly.
str(expenditure)

# Quick descriptive info.
with(expenditure, summary(Region))
with(expenditure, summary(STATE))
with(expenditure, summary(Y))

# We can now use the pairs function to plot the relationships between our variables.

# First, we create a new variable expenditure2 so that we are not hindered by 
# our non-numberic variable states

expenditure2 <- expenditure[2:5]

pairs(~ Y + X1 + X2 + X3, data = expenditure2)

with(expenditure2, pairs(~ Y + X1 + X2 + X3))

plot(expenditure2)

# Use the pairs function to plot expenditure 2 i.e. columns 2 to 5. 
pairs(expenditure2,
      # Change points by group
      # Change labels
      labels = c("Housing Assist. Expenditure", "Personal Income", "Financially Insecure Res.", 
                 "People per thousand (UA)"),
      main = "What affects the amount of money communities spend on
      addressing homelessness?") 

# Save as pdf
pdf ("Homelessness.pdf")
pairs(expenditure2,
     # Change points by group
     # Change labels
     labels = c("Housing Assist. Expenditure", "Personal Income", "Financially Insecure Res.", 
                "People per thousand (UA)"),
     main = "What affects the amount of money communities spend on
      addressing homelessness?") 

dev.off()


# Please plot the relationship between Y and Region? On average, which region has the
# highest per capita expenditure on housing assistance?

# First check you can make a basic boxplot
RegionPlot1 = ggplot(expenditure, aes(x = Region, y = Y), 
                     ylab = "SheltersExpenditure") + geom_boxplot()

RegionPlot1


# Make a more attractive boxplot using the qplot() function and specify the type
# of plot with geom() = boxplot
RegionPlot2 = qplot(Region, Y, data=expenditure, geom=("boxplot"), 
                    main="Expenditure on Shelters/Housing Assist. by Region", 
                    xlab="Region", ylab="Shelters Expenditure")
RegionPlot2 


# Save as pdf
pdf ("ExpenditureByRegion.pdf")
RegionPlot2 
dev.off()

# Find the mean per capita expenditure on housing assistance
RegionMeans = with(expenditure, by(Y, Region, mean))
RegionMeans 

# Please plot the relationship between Y and X1? Describe this graph and the 
# relationship. Reproduce the above graph including one more variable Region 
# and display different regions with different types of symbols and colors.

# Check if you can make a basic scatterplot
PersonalIncomePlot1 <- ggplot(expenditure, aes(x = X1, y = Y)) + geom_point() 
  
PersonalIncomePlot1

# Make a more attractive scatter plot using the qplot funtion and specify type 
# of plot with geom = point
PersonalIncomePlot2 <- qplot(X1, Y, data=expenditure, geom=("point"), 
                            main="Expenditure on Shelters/Housing Assist. by 
                            Personal Income",
                            xlab="Personal Income", ylab="Shelters")
                
PersonalIncomePlot2 

# create a pdf
pdf ("PersonalIncomeandHousingAsst.pdf")
PersonalIncomePlot2 
dev.off()



# Once again, use the pairs() function to reproduce the above graph. 

expenditure3 <- expenditure[2:3]

pairs(~ Y + X1, data = expenditure2)

with(expenditure3, pairs(~ Y + X1))

plot(expenditure3)

# To include one more variable 'Region', create an object from the 6th column
Region <- expenditure[, 6]


# To display different regions with different types of symbols and colors, 
# simply include the arguents 'col' and 'pch'.
EffectByRegion <- pairs(expenditure3,
      col = c("red", "cornflowerblue", "purple")[Region],   # Change color by group
      pch = c(8, 18, 1)[Region], 
      cex = 1,
      # Change points by group
      # Change labels
      labels = c("Housing Assist. Expenditure", "Personal Income", "Financially Insecure Res.", 
                 "People per thousand (UA)"),
      main = "What affects the amount of money communities spend on
      addressing homelessness?") 

# Save as pdf
pdf ("EffectByRegion.pdf")
EffectByRegion
dev.off()






