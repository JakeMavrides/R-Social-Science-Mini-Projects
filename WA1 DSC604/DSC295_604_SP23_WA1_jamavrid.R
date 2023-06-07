
# SETUP: 
# 
# setwd("C:/Users/Jake/Desktop/R")
# import("GSS2016.csv")
# GSS2016 <- read.csv("C:/Users/Jake/Desktop/R/GSS2016.csv", stringsAsFactors=TRUE)


#Question 1:
#A: The variable represents how surveyed people feel about legalizing cannabis

#B:
table(GSS2016$grass)
#C:
grassrecode <- GSS2016$grass
#D:
grassrecode[GSS2016$grass=='0'] = NA
grassrecode[GSS2016$grass=='8'] = NA
grassrecode[GSS2016$grass=='9'] = NA
table(grassrecode)

#Question 2-6:
#A:
setwd("C:/Users/Jake/Desktop/R")
#B:
sample1<-read.csv("sample1.csv")
#C:
print(sample1)

#A:
mean(sample1$Age, na.rm = TRUE)
#B:
sample1$Age[is.na(sample1$Age)]<-37.5
#C:
print(sample1$Age)

#5:
height1<-sample1$Height

#6:
#A:
medheight<-median(height1, na.rm = TRUE)
#B:
height1[sample1$Height=='0'] = NA
height1[is.na(height1)]<-medheight
#C:
print(height1)
