#Jigyasa Sachdeva
#IDS 570, Statistics for Management 
#Mid Term Examination
#Masters in Management Information Systems
#Liautaud Business School, University of Illinois at Chicago


#Part 1

#1

#Normally distributed data with mean = 5 at the centre 
#For a portable battery providing charge under 6 hours, we consider the area under the curve (percentile) for less than 6.
lessthan6 <- pnorm(6,mean=5,sd=1.2)
#For a portable battery providing charge under 8 hours, we consider the area under the curve (percentile) for less than 8.
lessthan8 <- pnorm(8,mean=5,sd=1.2)
#Fore a portable battery providing charge between 6 and 8 hours, the probability would be the difference between charge under 8 hours and charge under 6 hours
bw6and8 <- lessthan8 - lessthan6
bw6and8 #0.1961187
#Has 7 decimals
#Rounding off this result to 2 decimal places:
bw6and8 <- round(bw6and8, 2)
bw6and8 #Answer= 0.2 
#The probability that a portable battery provides a charge between 6 and 8 hours is 0.2


#Part 2

#1

View(tix)
#My hypothesis is that Type would be a factor and Gross would be a numeric variable. 
str(tix)
#Variables Gross and tix are shown as characters

#Univariate Analysis
table(tix$Type)
#Shows that there are only 3 production types- Musical, Play and Special with distributions as 1088,393,19 respectively.
#Hence, my hypothesis holds true
#Converting this variable into a factor with 3 levels. 
tix$Type <- as.factor(tix$Type)
describe(tix)
#Shows gross as a numeric variable.
#Hence my hypothesis holds true
#Converting this variable into a numeric data type
tix$Gross <- as.numeric(tix$Gross)
str(tix) #Confirming type conversion
describe(tix$Gross)
#Shows that Gross variable has a mean slightly greater than the median (wrt the range), therefore the data is roughly normally distributed with slight right skewness. 

options(scipen=99) 
plot(density(tix$Gross), col= "blue", lwd=3, main= "Density plot of Gross")
#Hence, variable Gross has outliers on the right as it is right skewed. 

t <- prop.table(table(tix$Type))
t <- round(t,2)
t
#Shows that Type variable is distributed as the probability of Musical is 0.73, Play is 0.26, Special is 0.01
pie(t, main= "Distribution of variable Type")
#The distribution infered from the prop table can be easily visualised here. 

#Bivariate analysis
#To check whether the speculation of 'there is not much difference in Gross amongst the different types of production' is correct or not. 
#Since Type is a factor and Gross is a numeric variable, box plot would be a good method to describe the relationship between the two.
b <- boxplot(tix$Gross~tix$Type, 
             main= "Change in gross wrt Type",
             col=c("orange", "blue", "green"),
             xlab= "Type of Production", 
             ylab= "Gross")
#From the boxplot, we can observe that the Gross value is large for Musical with respect to Play and Special production types
#The Musical type has also the highest individual Gross values, and a lot of outliers above approx 1500000
#Amongst the play and special type, special data type is more highly distributed and has a lesser median than play.
#We can say that Play has a slightly even distribution for Gross variable than Special. (But they are almost similar)

#Since, Musical has a very different distribution than the other two
#And it shows that musical type productions have the highest gross 
#Therefore, the speculation of the company is wrong. 



#2

#(a)

View(cars)
#My hypothesis is that both the variables- price and highway.mpg are integers. 
str(cars)
#Both are integers 
#Describing both the numerical values
#Describing price
describe(cars$price)
#Mean is greater than the median, hence the graph is right skewed. 
hist(cars$price, 
     main= "Frequency distribution of price", 
     col= c("coral", "steelblue"), 
     xlab= "Price", 
     ylab= "Frequency")
#The same can be visualised via the histogram plotted. 
#The frequency of price less than 10000 is the highest, followed by prices between 10000 and 20000
#Describing highway.mpg (Miles per gallon)
describe(cars$highway.mpg)
#The mean is very close to the median, but the kurtosis is very low, hence the variable isn't normally distributed
hist(cars$highway.mpg, 
     main= "Frequency distribution of miles per gallon", 
     col= c("coral", "steelblue"), 
     xlab= "Miles per gallon", 
     ylab= "Frequency")
#The distribution is odd 
#The maximum frequency for miles per gallon between 20-25, and almost similar between 25-35



#(b)

#Identifying outliers in a numeric variable by plotting a boxplot
boxplot(cars$price,
        ylab= "Price",
        main= "Distribution of price",
        col= "orange")
#The outliers exist only above the upper whisker
#For finding the number of outliers we need to find the value of upper whisker and then check how many values above that exists as price in the dataframe cars
#Value of upper whisker = Q3 + 1.5(IQR)
q3 <- fivenum(cars$price)[4]
upper_whisker <- q3 + 1.5*(IQR(cars$price))
upper_whisker #29680.5
#Subsetting the dataframe with values above upper_whisker
outliers <- cars[cars$price>upper_whisker,]
dim(outliers)  #14 2
#There are 14 rows in the outliers table
#Therefore, we have 14 outliers
#Calculating the mean of these outlier values
mean(outliers$price)
#35967.07



#(c)

#Removing outliers by deleting values above the upper_whisker from the dataframe cars
without_outliers <- cars[cars$price<=upper_whisker,]
boxplot(without_outliers$price,
        col="orange",
        main="Price distribution Without outliers") 
#One outlier still exists

#Bivariate analysis
#Relationship between miles per gallon and price
#Examining how price fluctuates with increase in miles per gallon
#My hypothesis is that price should decrease with increase in mpg
s <- plot(without_outliers$price~without_outliers$highway.mpg,
          pch=16,
          col= "darkgrey",
          main= "Relationship between mpg and price",
          xlab= "Miles per gallon",
          ylab= "Price")
abline(lm(without_outliers$price~without_outliers$highway.mpg), col="red", lwd=3)
#My hypothesis holds true as the trend line shows that price decreases with increase in miles per gallon



