##### Project 2 - Cold Storage Problem #######

#=====================Problem 1=====================
#install.packages('readr')
#install.packages('ggplot2')
#install.packages('Rcmdr')
library(readr)
library(ggplot2)
library(Rcmdr)
coldData <- read_csv("C:/Users/daoud/Downloads/PGP DSBA/Fundamental of Business Statistics/week 4/Cold_Storage_Temp_Data.csv")
View(coldData)
names(coldData)
head(coldData)
dim(coldData)
summary(coldData)
str(coldData)
hist(Temperature,col = 'blue')

ggplot(coldData,aes(y=Temperature,x=Season))+
  geom_boxplot(fill='blue', color="red")

## observation:
# we dont have missing values
# our data shape [ 365 , 4 ] 
# we have Season and Month as character , Date and Temperature as numeric
# the mean for Temperature is 3.002 
# possibility of outlier 4.6 

# 1 mean cold storage temperature for 3 season:

round(tapply(Temperature,Season, mean),2)

# 2 overall mean for the full year :

total_mean <- mean(Temperature)
total_mean

# 3 Standard Deviation for the full 
sd<-sd(Temperature)
sd

# 4 Assume Normal distribution, probability of temperature having fallen below 2 C :
below_2 <- pnorm(2,mean = total_mean,sd = sd,lower.tail = TRUE)

# 5	Assume Normal distribution, probability of temperature having going above 4 C :
above_4 <-pnorm(4,mean = total_mean,sd = sd,lower.tail =FALSE)
prob2_4 = above_4*100+below_2*100
prob2_4

# 6 the penalty for the AMC Company:
AMC=0.025*100
prob2_4 >= AMC

## as 3.18% > 2.5% ,the probability of temperature going outside the 2 - 4 C 
#  during the one-year contract was above 2.5% then the penalty is 10% of AMC.


# 7 one-way ANOVA test to determine if there is a significant difference in Cold Storage
#   temperature between rainy, summer and winter seasons and comment on the findings :
# H0 : null hypothesis : ??_rainy = ??_summer = ??_winter 
# Ha : alternative hypothesis : H0 not TRUE 



cold_anova <- aov(Temperature~Season,data = coldData)
summary(cold_anova)


##############
## For the given problem sum of squares due to the factor Season (SSB) is 9.70 
# and the sum of squares due to error (SSW) is 69.29 . 
# The total sum of squares (SST) for the data is (9.70+69.29 = 78.99). 
# Since the factor has 3 levels, 
# DF corresponding to Season is 3 - 1 = 2. Total DF is 365 - 1 = 364. 
# Hence DF due to error is 364 - 2 = 362.
# Mean sum of squares is obtained by dividing the sums of squares by corresponding DF. 
# The value of the F-statistic is  25.3 and the p-value is highly significant.
# Based on the one way ANOVA  we, therefore, 
# reject the null hypothesis that the three population means are identical.
# At least for one Season mean is different from the rest.


hypo_ANO <- TukeyHSD(x=cold_anova,conf.level = 0.95)

round(hypo_ANO$Season,2)

par(oma=c(0,7,0,0))
plot(hypo_ANO,las=1, col = "red")

# the graphical representation of Temperature comparisons with Season from Tukey's HSD 
# the confident intervals not containing 0 is for the difference between Winter-Rainy and Winter-Summer
# This indicates that population means of these pairs of Season are different. 
# from the values of the pairwise Summer-Rainy.
# it may also be concluded that Temperature from Winter Season is significantly less than the other two.
#=====================Problem 2=====================
# import data 
Cold_Mar2018 <- read_csv("C:/Users/daoud/Downloads/PGP DSBA/Fundamental of Business Statistics/week 4/Cold_Storage_Mar2018.csv")
View(Cold_Mar2018)
head(Cold_Mar2018)
summary(Cold_Mar2018)
dim(Cold_Mar2018)

# our data is 35 with 4 
# no Missing Data 
# mean of Temperature is 3.974 

attach(Cold_Mar2018)
hist(Temperature,col = 'grey',main = 'Cold_Mar2018',xlab = 'Temperature')
boxplot(Temperature,col = 'grey',main = 'Cold_Mar2018',xlab = 'Temperature')

# 1 Hypothesis test shall be performed :
#   since the Supervisor desided temperature as Factor , n > 30 then The size of our sample is 'Large' ,
#   so we don't need to worry about whether or not the population is normally distributed.
#   since we don't know the population standard deviation (??), we concluded that T.test is the need action 

# 2 stat the Hypothesis :
# H0: null Hypothesis : ??_Temperature <= 3.9
# H1: alternative Hypothesis : H0 not True  
# lets use T.test with alpha = 0.1 with mean = 3.9  with One tailed test.

t.test(Temperature,mu = 3.9,alternative = 'less',conf.level = 0.90)

# p-value = 0.9953 so p > 0.1  indicates weak evidence against the null hypothesis
# so we fail to reject the null hypothesis .

# 3 my inference : 
#   since the p-value is large, we fail to reject the null hypothesis 
#   of mean Tempreature less tehn 3.9C and can say that from the last 35 days Tempreature 
#   Cold Storage kept the temperature that agreed to .
#   about complaints consumers from dairy products mabey due to procurement side from 
#   where Cold Storage is getting the Dairy Products ? or mabey the right Temperature is Not between 2-4C ?
#   


