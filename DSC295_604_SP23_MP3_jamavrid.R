## -----------------------------------------------------------------------------
##
## Course: DSC 295 - 604
##
## Script name: DSC295_604_SP23_MP3_jamavrid
##
## Purpose of script: Mini Project 3 Regression Analysis
##
## Date Created: 4/21
##
## Date Due: 4/24
##
## Author: Jake Mavrides
##
##
## -----------------------------------------------------------------------------
##
## Notes1: First parts of the code are from MP2, in order to catch this version
##        'up to speed'. 
##
## Notes2: pasei10 will be encoded but not used in this particular mini-project
##   
##
## -----------------------------------------------------------------------------
## set working directory
##
  setwd("C:/Users/Jake/Desktop/R")    # setting working directory 
  getwd()    # checking working directory
##  
## -----------------------------------------------------------------------------
## load up the packages: 
##
  library(readr)
  library(dplyr)
  library(pastecs)
  library(Hmisc)
  library(corrplot)
##
##  
## -----------------------------------------------------------------------------

  # load dataset from local filepath and preview -------------------------------
  GSS2016 <- read_csv("GSS2016.csv")
  head(GSS2016)

  unique(GSS2016$cohort)
  # Our categorical variables will be: Sex, Race, and Happy
  # Our continuous variables will be: Age, Cohort, 
  # SEI10 (2010 socioeconomic index), PASEI10 (Father socioeconomic index 2010)
  df <- data.frame(GSS2016$age, GSS2016$cohort, GSS2016$sei10, GSS2016$pasei10, 
                   GSS2016$sex, GSS2016$race, GSS2016$happy)

  # Clean continuous variable's invalid responses
  df$GSS2016.cohort[df$GSS2016.cohort == '9999'] = NA
  
  # verify our cleaning
  unique(df$GSS2016.cohort)
  head(df) # view our new df double checking it was created correctly

  # Convert categorical columns to factors (2c i & ii)
  table(df$GSS2016.sex, df$GSS2016.race) # before
  df$GSS2016.sex <- factor(GSS2016$sex,
                           levels=c(1,2),
                           labels=c('Male','Female'))
  
  df$GSS2016.race <- factor(GSS2016$race,
                            levels=c(1,2,3),
                            labels=c('White', 'Black', 'Other'))
  table(df$GSS2016.sex, df$GSS2016.race) # after
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
  
  # Creating dummy continuous variable where 1 corresponds to a senior citizen (age 65 or older)
  # and 0 corresponds to someone younger than 65
  dummycont <- ifelse((df$Age >= 65), 1, 0)
  
  #2 : Convert dummy variable to factors. 
  table(df$seniorStatus)
  df$seniorStatus = factor(dummycont,
                           levels=c(0,1),
                           labels=c("Not-Senior", "Senior"))
  table(df$seniorStatus) # Check to make sure conversion worked -- (#2a-ii)
  
  # Note: 
  # Our dummy variable is seniorStatus
  # Our categorical variable with 3 categories is Happy
  # Our continuous variables that we'll examine are : Age, sei10, and Cohort
  
  # Next we'll create table of frequencies for dummy variable (#2a-iii):
  senior.tb1 <- table(df$seniorStatus)
  senior.prop <- prop.table(senior.tb1)
  senior.cumpct <- cumsum((senior.prop) * 100)
  seniortbl <- table(senior.tb1, senior.prop, senior.cumpct)
  
  # Clean our categorical variable (#2b - iii)
  table(df$Happy) # Before cleaning and factorization
  df$Happy[df$Happy == 9 | df$Happy == 8] = NA 
  
  # Convert our categorical variable to factor: (#2b-i & ii)
  
  df$Happy <- factor(df$Happy,
                             levels=c(1,2,3),
                             labels=c("Very Happy", "Pretty Happy", 
                                      "Not too Happy"))
  table(df$Happy) # After cleaning and factorization
  
  # Create table of frequencies for Happy variable (#2b-V):
  happy.tb1 <- table(df$Happy)
  happy.prop <- prop.table(happy.tb1)
  happy.cumpct <- cumsum((happy.prop) * 100)
  happytbl1 <- table(happy.prop, happy.tb1)
  happytbl1 # Proportional table
  happytbl2 <- table(happy.tb1, happy.cumpct)
  happytbl2 # Cumulative % table
  
  
  
  # --- Questions 1-2 have been completed ----
  
  
  # 3.1 : Finding mean, min, max, range, and SD for each continuous variable:
  
  summary(df$Age)
  # Age has a mean of 49.33, median of 50.0, min of 18.0, max of 99.0, and an
  # SD of 28.0 (3rd Qu - 1st Qu). The range is 71.0 (99-18)
  
  summary(df$Cohort)
  # cohort (year born) has a mean of 1967, median of 1967, min of 1927, max of
  # 1998, SD of 28years, and range of 71. 
  # Note how year born and age have same SD, range, etc. 
  
  summary(df$sei10)
  #sei10 has a mean of 45.54, median of 39.70, min of 0.0, max of 99.90,
  # SD of 40.25, and range of 99.90. 
  # Note the difference between mean and median suggests right-skewed data
  
  # 3.2 : Pick DV (socioeconomic index 'sei10')
  # 3.3 : Histograms of continuous variables
  histsei10 <- hist(df$sei10,
       col= "cornflowerblue", main="Histogram of Socioeconomic Index Scores", 
       xlab="SEI10 Score")
  histage <- hist(df$Age,
       col= "cornflowerblue", main="Histogram of Ages", 
       xlab="Ages")
  histcohort <- hist(df$Cohort,
                 col= "cornflowerblue", main="Histogram of years born", 
                 xlab="Cohort (year born)")
  # The only outliers I see immediatly are the ages from 95-100 on the age
  # histogram. However, it may be easier to spot individualoutliers if I 
  # reduced bucket size to various numbers.
  
  # 3.4 : Scatterplot of each continuous variable vs SEI10 (DV)
  plot(df$Age, df$sei10,
       pch = 16,
       col = "cornflowerblue",
       main = 'Scatterplot of age vs sei10',
       xlab = 'Age',
       ylab = 'sei10 Score')
  abline(lm(df$sei10 ~ df$Age), col="black") # Line of best fit
  
  plot(df$Cohort, df$sei10,
       pch = 16,
       col = "cornflowerblue",
       main = 'Scatterplot of year-born vs sei10',
       xlab = 'year-born',
       ylab = 'sei10 Score',
       lwd=2)
  abline(lm(df$sei10 ~ df$Cohort), col = "red") # Line of best fit
  # Note : Visually it appears theres hardly any correlation. The line of 
  # best-fit shows there is a mild trend of higher economic-index the older you
  # get. In contrast, the longer ago you were born naturally has a mild
  # downwards trend (age and cohort are directly inversly related)
  
  # 3.5 : Correlations and Multicollinearity
  CorrMat <- df[, c('Age', 'Cohort', 'sei10')]
  head(CorrMat, 4)
  CorrMat <- rcorr(as.matrix(df[, c('Age', 'Cohort', 'sei10')]))
  CorrMat
  coefs <- CorrMat$r
  pvals <- CorrMat$P
  coefs
  pvals
  # We see that all pvals are <<.001 , meaning all correlations are significant
  # The correlation coefficient between age and cohort is nearly -1 and 1,
  # respectively, which is what one would expect. The interaction between
  # Age and sei10 is ~.12 indicating a mild positive correlation
  # Coefficient between cohort and sei10 is ~-.12, indicating a mild negative
  # correlation. This value should be expected, however, because of the direct
  # inverse relationship between cohort and age. 
  # The highest correlation between variables is of course Age and cohort,
  # as they are almost direct inverses. 
  # THERE ARE CERTAINLY multicollinearity issues at play here, as cohort and
  # age are essentially exact inverses.
  
  # 3.6 Lower Matrix Plot: 
  help(corrplot)
  corrplot(coefs, type = 'lower', tl.col = "black")
  
  # Q4 : Linear regression
  reg <- lm(formula = sei10 ~ Age + Cohort + Age * Cohort, data=df)
  reg
  summary(reg)
  # This means our formula line is DV = c1IV1 + c2IV2 + c2IV1*IV2 + intercept
  # So our approximation equation is:
  # y = -21.12336*Age - .91904*Cohort + .01064*Age*Cohort + 1866.21695
  # All of our relationships have very small p-values, indicating significance
  # between all of our variable combinations. It appears age has the largest
  # impact on sei10, as its correlation coefficient is -21. Cohort's coef is
  # noticeably smaller at -9, and the interaction between age and cohort is 
  # minimal. However, this is likely due to their intrinsic nature to cancel 
  # eachother out, as there is a direct negative correlation between age and 
  # cohort. That being said, I'm surprised Cohort's coefficient in the lm isn't
  # closer to the same magnitude as Age's. 
  
  # Q5: Residual analysis: 
  resid(reg)
  hist(resid(reg), xlab='residuals', ylab='count', main='LM Residual Count',
       col = 'cornflowerblue')
  # Distribution looks as though it may be fairly normally distributed.
  # Imagine 'zooming out' away from the graph and I believe it would look 
  # mosly normal. That being said there seems to be a slight leftward skew
  # when looking at purely at the center 6-7 columns.
  
  
## End of the script -----------------------------------------------------------

