## -----------------------------------------------------------------------------
##
## Course: DSC 295 - 606
##
## Script name: DSC295_604_SP23_MP1_jamavrid
##
## Purpose of script: Analyze the 'coffee' dataset as outlined in Mini Project 1
##                    rubric
##
## Date Created: 3/7/23
##
## Date Due: 3/8/23
##
## Author:   Jake Mavrides
## -----------------------------------------------------------------------------
## Notes: Mini Project 1 -- Will install and analyze a dataset pertaining to 
##        coffee
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
library(readr)

# load dataset from github url and preview
GSS2016 <- read_csv("GSS2016.csv")
head(GSS2016)
View(GSS2016)

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

# Find cumulative freq of categorical variables:
sexfreq.tb1 <- table(df$Sex)
racefreq.tb1 <- table(df$Race)
happyfreq.tb1 <- table(df$Happy)
dummyhappyfreq.tb1 <- table(df$dummyHappy)
seniorfreq.tb1 <- table(df$seniorStatus)

# Find relative frequency as a % of each freq variable
sexfreq.prop <- prop.table(sexfreq.tb1)
racefreq.prop <- prop.table(racefreq.tb1)
happyfreq.prop <- prop.table(happyfreq.tb1)
dummyhappyfreq.prop <- prop.table(dummyhappyfreq.tb1)
seniorfreq.prop <- prop.table(seniorfreq.tb1)

sexfreq.cumfreq <- cumsum(sexfreq.prop * 100)
sexfreq.cumfreq # Verify this is giving cumulative values
racefreq.cumfreq <- cumsum(racefreq.prop * 100)
happyfreq.cumfreq <- cumsum(happyfreq.prop * 100)
dummyhappyfreq.cumfreq <- cumsum(dummyhappyfreq.prop * 100)
seniorfreq.cumfreq <- cumsum(seniorfreq.prop * 100)

# Create a table for each variable's three variables we just created
sextbl <- table(sexfreq.tb1, sexfreq.prop, sexfreq.cumfreq)
racetbl <- table(racefreq.tb1, racefreq.prop, racefreq.cumfreq)
happytbl <- table(happyfreq.tb1, happyfreq.prop, happyfreq.cumfreq)
# 5: Central tendency and variation
# Find the mean, mode, and median for each of our 4 continuous variables

# Mean values:
mean(df$Age, na.rm=TRUE)
mean(df$Cohort, na.rm=TRUE)
mean(df$sei10, na.rm=TRUE)
mean(df$pasei10, na.rm=TRUE)

# Median values: 
median(df$Age, na.rm=TRUE)
median(df$Cohort, na.rm=TRUE)
median(df$sei10, na.rm=TRUE)
median(df$pasei10, na.rm=TRUE)

# Mode values:
# Function source: https://www.statology.org/mode-in-r/
find_mode <- function(x) {
  uniq <- unique(x)
  tab <- tabulate(match(x, uniq))
  uniq[tab == max(tab)]
}
find_mode(df$Age)
find_mode(df$Cohort)
find_mode(df$sei10)
find_mode(df$pasei10)

# 5b:	Find the range, IQR, and SD for each of our 4 continuous variables
#Range values:
range(df$Age, na.rm=TRUE)[2] - range(df$Age, na.rm=TRUE)[1]
range(df$Cohort, na.rm=TRUE)[2] - range(df$Cohort, na.rm=TRUE)[1]
range(df$sei10, na.rm=TRUE)[2] - range(df$sei10, na.rm=TRUE)[1]
range(df$pasei10, na.rm=TRUE)[2] - range(df$pasei10, na.rm=TRUE)[1]

# 6: Graphs
# We'll create a bar chart for our race frequencies
barplot(racefreq.tb1, main="Races GSS2016", xlab = "Race Recorded", 
        legend.text = TRUE, ylab="Count", 
        col=c('royalblue', 'lightsteelblue', 'steelblue'), las=1)

# We'll create a grouped boxplot for happiness and socioeconomic index (sei10)
boxplot(df$sei10 ~ df$Happy, 
        main="Socioeconimic Index Rating \n by Happiness Level", 
        outpch=16, whisklty=1,
        xlab="Happiness Rating", ylab="SEI2010 Score",
        col=c("lightsteelblue4","royalblue", "steelblue"), outcol="royalblue2",
        legend("top", c("Very Happy", "Pretty Happy", "Not too Happy"), title="legend",
               text.font=4, fill=c("lightsteelblue","steelblue","royalblue2")))
# Note: very confused to as where my legend is , can't seem to resolve issue

## -----------------------------------------------------------------------------

