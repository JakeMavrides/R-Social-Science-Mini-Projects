#setwd("C:/Users/Jake/Desktop/R") #commented out so professor can run
getwd()

#1:
table(GSS2016$race)

# 1B & 1C: 
# Median can not be determined as the column's dont correspond to a gradient 
# in potential values
# Mode of GSS2016$race is column 1, or 'white'

#2:
GSS2016$racerec <- factor(GSS2016$race,
                    levels=c(1,2,3),
                    labels=c('White', 'Black', 'Other'))
table(GSS2016$racerec)
# 2B:
# mode of the distribution is White

#3:
# The valid percent of a sample includes the percentage(s) of the sample
# that you actually have data on, while the full percent is the entire
# sample space's percentage(s).

#4
GSS2016$degreerec <- factor(GSS2016$degree,
                            levels = c(0,1,2,3,4),
                            labels=c("LT HS", "HS", "Some College", "College", "Grad School"))

degreerec2 <- cumsum(table(GSS2016$degreerec))
degreerec2
median(degreerec2)
# The median of degreerec2 is 2005
# The mode of degreerec2 is Grad School, although the mode of a cumulative sum
# will always be the last entry. 
# The (probably more meaningful) mode of GSS2016$degreerec itself is 'HS'
