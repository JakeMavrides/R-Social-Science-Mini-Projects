## -----------------------------------------------------------------------------
##
## Course: DSC 295 - 604
##
## Script name: DSC295_604_SP23_MP2_jamavrid
##
## Purpose of script: Analyze the 'coffee' dataset as outlined in Mini Project 1
##                    rubric
##
## Date Created: 4/4/23
##
## Date Due: 3/9/23
##
## Author:   Jake Mavrides
## -----------------------------------------------------------------------------
## Notes: Mini Project 2 -- Will install and analyze GSS2016
##   
## -----------------------------------------------------------------------------
## set working directory
##
setwd("C:/Users/Jake/Desktop/R")    # setting working directory 
getwd()    # checking working directory
##  
## -----------------------------------------------------------------------------
## load up the packages: 
library(readr)
library(dplyr)
library(pastecs)
# load dataset from github url and preview ---------------------------------
GSS2016 <- read_csv("GSS2016.csv")
head(GSS2016)
# Our categorical variables will be: Sex, Race, and Happy
# Our continuous variables will be: Age, Cohort, 
# SEI10 (2010 socioeconomic index), PASEI10 (Father's socioeconomic index 2010)
df <- data.frame(GSS2016$age, GSS2016$cohort, GSS2016$sei10, GSS2016$pasei10, 
                 GSS2016$sex, GSS2016$race, GSS2016$happy)

# Clean continuous variable invalid responses (only occur in Cohort column):
df$GSS2016.cohort[df$GSS2016.cohort == '9999'] = NA
head(df) # view our new df double checking it was created correctly

# Convert categorical columns to factors:
df$GSS2016.sex <- factor(GSS2016$sex,
                         levels=c(1,2),
                         labels=c('Male','Female'))

df$GSS2016.race <- factor(GSS2016$race,
                          levels=c(1,2,3),
                          labels=c('White', 'Black', 'Other'))

df$GSS2016.happy[df$GSS2016.happy == '8'] = NA # Convert non-responses to NA
df$GSS2016.happy[df$GSS2016.happy == '9'] = NA
df$GSS2016.happy <- factor(GSS2016$happy,
                           levels=c(1,2,3),
                           labels=c("Very Happy", "Pretty Happy", "Not too Happy"))

head(df) # Verify work

# Rename columns
colnames(df)[1] = 'Age'
colnames(df)[2] = 'Cohort'
colnames(df)[3] = 'sei10'
colnames(df)[4] = 'pasei10'
colnames(df)[5] = 'Sex'
colnames(df)[6] = 'Race'
colnames(df)[7] = 'Happy'
head(df) # Verify work

# Creating dummy categorical variable where 1 corresponds to a happy surveyer, 
# and 0 corresponds to someone who is 'Not too Happy'
dummycat <- ifelse((df$Happy == 'Very Happy' | df$Happy == 'Pretty Happy'), 1, 0)

# Creating dummy continuous variable where 1 corresponds to a senior citizen (age 65 or older)
# and 0 corresponds to someone younger than 65
dummycont <- ifelse((df$Age >= 65), 1, 0)

# Add variables as columns to our df:
df$dummyHappy = factor(dummycat,
                       levels=c(0,1),
                       labels=c("Not-Happy", "Happy"))
df$seniorStatus = factor(dummycont,
                         levels=c(0,1),
                         labels=c("Not-Senior", "Senior"))

head(df) #Verify columns have been added correctly with correct labels

# --------- end of code from mp1 -------
# Our dummy variables are are : dummyHappy and seniorStatus
# Our continuous variables that we'll examine are : Age, sei10, and pasei10

# Part 1a -- boxplot vs dummy variables
boxplot(df$Age ~ df$dummyHappy, xlab='dummy var for happy', ylab='Age')
boxplot(df$Age ~ df$seniorStatus, xlab='dummy var for seniority', ylab='Age')

boxplot(df$sei10 ~ df$dummyHappy, xlab='dummy var for happy', ylab='socioeconimic index')
boxplot(df$sei10 ~ df$seniorStatus, xlab='seniority', ylab='socioeconimic index')

boxplot(df$pasei10 ~ df$dummyHappy, xlab='dummy var for happy', ylab="father's socioeconimic index")
boxplot(df$pasei10 ~ df$seniorStatus, xlab='seniority', ylab="father's socioeconimic index")

# Comments on boxplots: 
# -- Age: Of course seniors had a much higher average age of around 72 while 
#         non-seniors had an average age of around 42 and also had more variance.
#         Not-happy and Happy vs Age had extremely similar boxplots, however,
#         which suggests age doesn't affect happiness
#
# -- sei10: Happier people tended to have a higher socioeconomic status, but
#           it isn't as drastic as one might expect, and the whiskers have similar lengths
#           Seniors had a similar sei10 rating, just slightly higher than non-seniors
#
# -- pasei10: Happiness levels had even more similar pasei10 ratings than sei10 ratings, 
#             suggesting that father's money has almost no affect on happiness levels.
#             Seniors' pasei10 levels had the same mean as non-seniors, but seniors had notably more outliers
#

# Part 1B: 

# Create numeric versions of our dummy variables so we can run cor.test on them
df$dummyHappyNumeric = factor(dummycat,
                       levels=c(0,1))
df$seniorStatusNumeric = factor(dummycont,
                         levels=c(0,1))
# Age and happiness dummy variable:
cor.test(df$Age, as.numeric(df$dummyHappyNumeric))
# Age and seniority status:
cor.test(df$Age, as.numeric(df$seniorStatusNumeric))
# socioeconomic status and happiness:
cor.test(df$sei10, as.numeric(df$dummyHappyNumeric))
# socioeconomic status and seniority:
cor.test(df$sei10, as.numeric(df$seniorStatusNumeric))
# economic status of father and happiness:
cor.test(df$pasei10, as.numeric(df$dummyHappyNumeric))
# economic status of father and seniority: 
cor.test(df$pasei10, as.numeric(df$seniorStatusNumeric))

#Part 1C:
#
# Our first t-test we can immediately reject any significance when we see the 
#   p-value is very large at .5015
#
# Our second t-test, however, is significant with a p-value ~= 0
# Our second test tells us we can reject the null hypothesis, and therefore
# there is a correlation between age and seniority (obvious when thought about logically)
#
# Our third t-test also gives a very small p-value, meaning there is a 
# significant relationship between socioeconomic10 index rating and happiness,
# however, the relationship is very weakly positive as seen by our cor value of ~.14
#
# Our fourth t-test gives a p-value <.05 with a very very small positive correlation
# between economic status and seniority. The relationship is nearly nonexistant however,
# with a correlation coeffecient of <.05
#
# Our fifth t-test gives a p-value <.05 with a miniscule positive correlation
# between father's socioeconomic index and happiness level
#
# Our last t-test gave a p-value > .05 , and we can therefore reject the hypothesis
# that there is a significant correlation and/or causation between father's
# sei10 score, and seniority status

#Question 2A-- note: I already ran correlations in the last part as it is done automatically
#             by cor.test

# Age and happiness dummy variable:
cor.test(df$Age, as.numeric(df$dummyHappyNumeric))
# Age and seniority status:
cor.test(df$Age, as.numeric(df$seniorStatusNumeric))
# socioeconomic status and happiness:
cor.test(df$sei10, as.numeric(df$dummyHappyNumeric))
# socioeconomic status and seniority:
cor.test(df$sei10, as.numeric(df$seniorStatusNumeric))
# economic status of father and happiness:
cor.test(df$pasei10, as.numeric(df$dummyHappyNumeric))
# economic status of father and seniority: 
cor.test(df$pasei10, as.numeric(df$seniorStatusNumeric))

# 1: Very weak negative correlation with a value of -.01 , 
#    and that would be assuming the p-value is <.05, which it isn't.
# 2: Our cor = .7375, so there is a somewhat strong positive correlation between the two (age and seniority)
# 3: Our cor coef = .145 , suggesting a weak positive relationship
# 4: Our cor coef = .047 , suggesting a weak/nonexistant positive relationship
# 5: Our cor coef = .045 , suggesting another very weak or nonexistant relationship
# 6: Our cor coef = -.034, suggesting a very weak negative relationship
