#Question 1
#a)
library(car)
leveneTest(len ~ supp, data = ToothGrowth)
#p value 0.2752>0.05, so We fail to reject the null hypothesis. There is not significant difference in variance between OJ and VC group.
#We now perform one of two two-sample t-tests. If we concluded the two unknown variances were equal, we can use:
# Two-sample t-test (equal variances)
t.test(len ~ supp, data = ToothGrowth, 
       alternative = "two.sided", 
       var.equal = TRUE)

#b)
# Subset data for 2 mg/day
subset_2mg_oj <- subset(ToothGrowth, dose == 2 & supp == "OJ" )
nrow(subset_2mg_oj)
subset_2mg_vc <- subset(ToothGrowth, dose == 2 & supp == "VC" )
nrow(subset_2mg_vc)
# Shapiro-Wilk test for normality
qqnorm(subset_2mg_oj$len)
qqline(subset_2mg_oj$len)
shapiro.test(subset_2mg_oj$len)

qqnorm(subset_2mg_vc$len)
qqline(subset_2mg_vc$len)
shapiro.test(subset_2mg_vc$len)

#Question 2
#a)
worldtemp<-read.csv("D:\\durham_material\\Introduction to Statistics for Data Science\\assignment 2\\worldtemp.csv",
               fileEncoding = "UTF-8")
nrow(worldtemp)

xbar<-mean(worldtemp$Temp)
xbar
s<-sd(worldtemp$Temp)

Tstat<-(xbar-13.9)/(s/sqrt(45))
Tstat
# t stat is 13.83534



#critical value t 0.005,44
qt(0.005, df = 44)
qt(0.995, df = 44)
#2.692278

#p value
t.test(worldtemp$Temp,alternative="two.sided",mu=13.9,conf.level=0.99)
#t = 13.835, df = 44, p-value < 2.2e-16


