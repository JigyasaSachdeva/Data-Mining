#Part1

library(car) # advanced scatter plots 
library(corrplot) # plot correlations 
library(dplyr) # data aggregates 
library(Hmisc) # for correlation test of multiple variables 
library(gplots) # plot means with CI
library(psych) 


#Question 1

View(emp_performance)
dim(emp_performance)  #248 values and 6 variables
str(emp_performance)  
#accuracy, actual_prod and target_prod look like numeric variables and are structured so
#season, type and work_area look like factors with specific levels and are structured so

#Univariate analysis using statistics
summary(emp_performance)
#accuracy: mean(0.4872) is almost equal to the median(0.4886), hence the variable seems normally distributed
#actual_prod: mean(10.94) is slightly more than the median (9.74), hence the variable seems slightly right skewed
#target_prod: the range is very less and the distribution shows that it could be a factor
#season: The maximum observations are from Winter(102), then Fall(70), followed by Spring(45), Summer(31). 
         #observations in winter are almost 1.5 times Fall and Fall is almost 2 times observation is Winter and Summer
#type: employees working locally(163) are twice than employees working remotely(85)
#work_area: Employees with english(79) work area are more than people with spanish(54) work area. 
          #Other work areas account for the maximum observations(115)

#Univariate analysis using plots

#accuracy
hist(emp_performance$accuracy, col="coral", xlab = "Accuracy")
#The variable looks normally distributed
boxplot(emp_performance$accuracy, col="steelblue")
#The variable is normally distributed with outliers below the lower whisker
  #and above the upper whisker

#actual_prod
hist(emp_performance$actual_prod, col="coral", xlab = "Actual product")
#The variable is highly right skewed 
boxplot(emp_performance$actual_prod, col="steelblue")
#And has a few outlietrs above the upper whisker

#target_prod
hist(emp_performance$target_prod, col="coral", xlab = "Target product")
#The distribution looks odd
#The variable looks like a factor
#Checking so, by plotting a table
tp<- table(emp_performance$target_prod)
tp
#5 7.5   8   9  11  14  16 
#5  79  24  34  14  38  54 
#total_prod hence is a factor with 7 levels
#converting the variable into factor
emp_performance$target_prod <- as.factor(emp_performance$target_prod)
str(emp_performance)
#Analysing further
barplot(prop.table(tp), col="steelblue")
#Maximum employees have a target product of 7.5(79) followed by 16(54), 14(38)
    #Employees having target product of 5 is very less (5)
    #This has to be treated 


#season
ts <- table(emp_performance$season)
barplot(prop.table(ts), col="steelblue")
#The maximum employees are in the dseason Winter, followed by Fall

#type
tt <- table(emp_performance$type)
barplot(prop.table(tt), col="steelblue")
#The employees in local are more than employees in remote area

#work_area
tw <- table(emp_performance$work_area)
barplot(prop.table(tw), col="blue")
#People having an other work_area is more than english, followed by spanish


#-----------------------------------------------------------------------------------

#Question 2

#actual_prod is a numeric varaiable and so is accuracy
#for 2 numeric variables:

#stats:
cor(emp_performance$actual_prod, emp_performance$accuracy)
#0.6883099
#The variables are highly positively correlated with a value of 0.68

#plot:
plot(accuracy~actual_prod, data=emp_performance, col="grey", pch=16)
abline(lm(accuracy~actual_prod, data=emp_performance), col="red", lwd=3)
#The relationship looks linearly positive 

#test
#H0- accuracy and actual product values are not related to each other
#H1- accuracy and actual product values are related to each other
x <- lm(accuracy~actual_prod, data=emp_performance)
options(scipen=99)
summary(x)
#the p-value is less than 0.05 and 0.01, therefore we reject the null
#accuracy and actual_prod are related to each other
#The relationship is: with every unit increase in actual product,
      #the accuracy increases by 0.021


#target_prod is a factor and accuracy is numeric
#for 1 factor and 1 numeric: 

#statistics:
emp_performance%>%group_by(target_prod)%>%
  summarise(m=mean(accuracy), mn=median(accuracy), s=sd(accuracy))
#the maximum mean of accuracy is with target product 8 
    #immediately followed by 14, 11 and so on
#except for 7.5 and 16: all the other target products have the means more than the median
#the relationship of accuracy with target product is not clear 


#plot
boxplot(accuracy~target_prod, data=emp_performance, col=2:8)
#The distribution looks odd, not sure if they are related to accuracy

#test
#H0- accuracy and target product are not related to each other
#H1- accuracy and target product are related to each other

aov_tp <- aov(accuracy~target_prod, data=emp_performance)
summary(aov_tp)
#The p-value is 0.00538: which is less than 0.05 and 0.01, hence we reject the null
    #Accuracy and target product are related to each other

plotmeans(emp_performance$accuracy~emp_performance$target_prod, xlab="Target product", 
          ylab="Accuracy", lwd=3, col="red", p=0.99)
#According to the confidence interval plot: 
#there seems to be a statistical difference only amongst a few levels 
      #such as 5 and 7.5, 5 and 8 


#Checking across all the levels of target product
TukeyHSD(aov_tp)
#diff          lwr        upr     p adj
#7.5-5   0.215999695 -0.002398532 0.43439792 0.0547431
#8-5     0.272123752  0.039305841 0.50494166 0.0106876
#9-5     0.190395053 -0.036442971 0.41723308 0.1654542
#11-5    0.222463333 -0.024274527 0.46920119 0.1076050
#14-5    0.266409617  0.041107413 0.49171182 0.0093265
#16-5    0.184970282 -0.036416729 0.40635729 0.1696526
#8-7.5   0.056124058 -0.054260280 0.16650840 0.7375632
#9-7.5  -0.025604642 -0.122743776 0.07153449 0.9863492
#11-7.5  0.006463638 -0.130868418 0.14379569 0.9999994
#14-7.5  0.050409922 -0.043086631 0.14390648 0.6804478
#16-7.5 -0.031029413 -0.114651963 0.05259314 0.9266593
#9-8    -0.081728699 -0.207991940 0.04453454 0.4660394
#11-8   -0.049660419 -0.208929023 0.10960818 0.9678989
#14-8   -0.005714136 -0.129196927 0.11776866 0.9999994
#16-8   -0.087153471 -0.203339207 0.02903227 0.2832160
#11-9    0.032068280 -0.118323865 0.18246043 0.9956230
#14-9    0.076014564 -0.035785745 0.18781487 0.4037154
#16-9   -0.005424771 -0.109109074 0.09825953 0.9999988
#14-11   0.043946284 -0.104119213 0.19201178 0.9748955
#16-11  -0.037493051 -0.179530078 0.10454397 0.9862461
#16-14  -0.081439335 -0.181719086 0.01884042 0.1969103


#The p-value for only 8-5 and 14-5 is less than 0.05 
    #Rest all have a p-value greater than 0.05
    #We fail to reject the null at maximum levels
#5 has very less values, it might as well be clubbed with 7.5

#hence, target product is technically related to accuracy
  #but if we drill down, very few levels show a difference
  #hence, target product is not much related to accuracy
#But actual product is highly related to accuracy

#---------------------------------------------------------------------------------

#Question 3

#season
#H0- season and accuracy are not related
#H1- season and accuracy are related
aov_s <- aov(accuracy~season, data=emp_performance)
summary(aov_s)
#p-value is 0.377, which more than 0.05 and hence we fail to reject the null
    #Therefore, season and accuracy are not related
par(mfrow=c(2,2))
plot(aov_s)
#A few outliers exist- 1, 250, 254, 289
#Q-Q graph is linear: showing normal distribution 


#type
#H0- type and accuracy are not related
#H1- type and accuracy are related
aov_t <- aov(accuracy~type, data=emp_performance)
summary(aov_t)
#p-value is 0.00534, which less than 0.05 and 0.01
  #hence we reject the null
  #Therefore, type and accuracy are related
plot(aov_t)
#A few outliers exist- 1, 219, 250, 254
#Q-Q graph is linear: showing normal distribution 

#work_area
#H0- season and accuracy are not related
#H1- season and accuracy are related
aov_w <- aov(accuracy~work_area, data=emp_performance)
summary(aov_w)
#p-value is 0.277, which more than 0.05 and hence we fail to reject the null
#Therefore, work_area and accuracy are not related
plot(aov_w)
#A few outliers exist- 1, 250, 254, 289
#Q-Q graph is linear: showing normal distribution 


#Therefore, out of : season, type and work_area
    #Only type is related to accuracy 


#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------


#Part 2


View(promos)
dim(promos) #150 values and   5 variables
str(promos)
#sales, online, paper and in_store variables look like numeric and are structured so
#region looks like a factor and is structured so
summary(promos)
#sales has a mean(14.01) more than the median(12.90), hence the variable is right skewed
#region: Maximum observation are of the Midwest store(51), followed by South, West and East
#online variable has a median(159.95) more than the mean(150.84), hence it is slightly left skewed
#paper has a mean(29.18) more than the median(23.93), hence the variable is right skewed
#in_store has almost the same median(22.40) and mean(22.35), indicating normal distribution


#Question 1

#exhaustive model will consist of all the variables other than sales as IV
    #and sales as DV
mod1 <- lm(sales~region+online+paper+in_store, data=promos)
summary(mod1)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-8.5930 -0.6353  0.2350  1.0313  2.7123 
#This shows the univariate analysis of the residual error
#The error has a range of -8 to 2.7

#Coefficients:
#             Estimate     Std. Error t value       Pr(>|t|)    
#(Intercept)    3.015128   0.506528   5.953         0.0000000195 ***
#regionMidwest -0.282320   0.404105  -0.699                0.486    
#regionSouth    0.464575   0.421249   1.103                0.272    
#regionWest     0.477785   0.448051   1.066                0.288    
#online         0.044408   0.001656  26.820 < 0.0000000000000002 ***
#paper         -0.001120   0.007042  -0.159                0.874    
#in_store       0.187849   0.009686  19.393 < 0.0000000000000002 ***

#all categories of region as compared to region East have a p-value greater than 0.05
  #hence, for region; we fail to reject the null
  #region is not related to sales
#paper also has a p-value higher than 0.05 (0.874)
  #hence, for paper; we fail to reject the null
  #paper is not related to sales
#online and in-store have a p-value less than 0.05
 #hence, for online and in-store, we reject the null
  #they are related to the model
  
#the p-value of the model is overall less than 0.05, indicating relationship 
#The adjusted r-Square is 0.8885
    #which shows that 88.85% of the variance in sales can be wxplained by the model
    #which indicates a very high value

#For 1 unit incrrease in online, sales increases by 0.04$
#For 1 unit in crease in store, sales increases by 0.18$


#---------------------------------------------------------------------------------------

#Question 2

#Building model on good IVs: online, in_store
#The reason for choosing these variables have been explained above

mod2 <- lm(sales~online+in_store, data=promos)
summary(mod2)

#The residual error coeffcients remain the same

#Coefficients:
#Estimate Std. Error t value             Pr(>|t|)    
#(Intercept) 3.151619   0.346661   9.091 0.000000000000000622 ***
#  online      0.044353   0.001659  26.731 < 0.0000000000000002 ***
#  in_store    0.186455   0.009280  20.092 < 0.0000000000000002 ***

#Both the  variables have a p-value is less than 0.05 and 0.01, hence the model seems valid

#For 1 unit incrrease in online, sales increases by 0.04$
#For 1 unit in crease in store, sales increases by 0.18$

#The adjusted R-Square now is 0.8868
    #It has decreased by 0.0017.

#With about the same adjusted-r-squared, 
    #the second and the first models explain about 88.8% of the variance in sales
#However, because predictors are significant in the second model, 
    #it conforms to OLS regression assumptions and therefore is more valid.

#---------------------------------------------------------------------------------------

#Question 3

#Regression Diagnostics

#1: 
#Removing residual errors
plot(mod2)
#The outliers seen from the plot are 5,64 and 75

outliers <- c(5,64,75)
promos1<-promos[-outliers,]
#The new dataframe has 147 values 

#Running the new model with removed outliers
mod2.1<- lm(sales~online+in_store, data=promos1)
summary(mod2.1)

#Residual standard error: 1.419 on 144 degrees of freedom
#Multiple R-squared:  0.9191,	Adjusted R-squared:  0.918 
#F-statistic: 817.8 on 2 and 144 DF,  p-value: < 0.00000000000000022

#The adjusted R-Square is now 0.918
  #The value has increased by 0.3
  #Hence this model explains 3% more variance than before
  #This is a better model


#2- 
#The model has 2 Independent Variables which are numeric
#They could be correlated, causing the model's variancve to inflate
#Checking their correlation

cor(promos$online, promos$in_store)
#0.04526652
#The correlation value is 0.045, which is very less
  #Hence they do not seem to be related
#Checking this further
#VIF threshold of this model would be: 1/(1-Adj. R squared) = 1/(1-0.918)
#                                                           = 1/0.082 = 12.19
car::vif(mod2.1)
#online in_store 
#1.009669 1.009669 
#The variables are not multicolinear

#3
#- If there were factors in the model, like in the model 1
#Dummy variables would have to be created for improving the efficiency of the model
#But this doesn't concern mod2.1

#4-
#In OLS regression, DV (here,sales) should be normally distributed
#Checking so
boxplot(promos$sales, col="yellow")
#Even though, there are no outliers
    #the variable is right skewed
#This is a concern as the DV should be normally distributed
#This can be addressed by sampling the data and making sure the sample is statistically similar to the population

#--------------------------------------------------------------------------------------







