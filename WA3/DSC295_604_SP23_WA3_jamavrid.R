# Q1:
racerec <- factor(GSS2016$race, levels=c(1,2,3), labels=c("White", "Black", "Other"))
racerec <- table(racerec)
racerec
barplot(racerec)
barplot(racerec[order(racerec, decreasing=TRUE)], main="Races GSS2016", xlab = "Race", ylab="Count")

# Q2:
GSS2016$attendrec <- GSS2016$attend
GSS2016$attendrec[GSS2016$attendrec=="8"] = NA
GSS2016$attendrec[GSS2016$attendrec=="9"] = NA

attendrecfreq.tb1 <- table(GSS2016$attendrec)
attendrecfreq.tb1

attendrecfreq.prop <- prop.table(attendrecfreq.tb1)
attendrecfreq.prop

attendrecfreq.cumpct <- cumsum((attendrecfreq.prop) * 100)
attendrecfreq.cumpct

plot(attendrecfreq.cumpct, type="o", 
     main="Cumulative attendence frequency",
     xlab="Frequency of attendence",
     ylab="Cumulative Percent")

# Although I don't get a median arrow like in the book, drawing a line from the 50th percentile, the median is between 3 and 3.5

# Q3:
GSS2016$hompoprec <- GSS2016$hompop
GSS2016$hompoprec[GSS2016$hompoprec=='98'] = NA
GSS2016$hompoprec[GSS2016$hompoprec=='99'] = NA
boxplot(GSS2016$hompoprec,
        main="House population",
        col="blue")
# High end marker: 11
# Low end marker: 1
# Whisker: 6
# Median: 2

# Q4: 
barplot(sexrace,
        main="SexFreq and Race Plotted",
        legend=rownames(sexrace),
        beside=TRUE)

# Q5:
partnerdegree <- aggregate(partnersrec ~ degreerec, FUN=mean)
mean.partnerdegree <- t(partnerdegree [-1])
mean.partnerdegree
barplot(mean.partnerdegree,
        col='royalblue',
        main="Partner Number by\n Degree",
        xlab="Education/Degree Level",
        ylab="Mean Number of Sex Partners")

