#Jigyasa Sachdeva
#Homework 1
#IDS-572, Data Mining, Semester: Spring'20
#Masters in Management Information Systems
#Liautaud Business School, University of Illinois at Chicago

getwd()
setwd("/Users/jigyasasachdeva/Desktop/Second\ Sem/Data\ Mining/Assignments/Assignment\ 1")
#set directory to the location of saved file


#Problem 3

#(a)
#Reading the data in R
College <- read.csv("college.csv")
#Calling the data to view it in a tabular form
View(College)
#Checking variables and rows in the dataset College
dim(College)
# 777  19
#There are 777 observations and 19 variables in the dataset.


#(b)
rownames(College)<-College[,1]
#Performing given command to give all the rows name of the respective university 
#Removing the first column because cannot be used for calculation
install.packages("dplyr")
library(dplyr) #For using 'select' and removing the column
College <- select(College, -c(1))  
View(College)
#First Column removed


#(c)
#Checking the structure of College
str(College)
#All the variables, except Private are numerical variables
#Checking their summary: uses dplyr package

summary(College)

#Apps
#Range of number of applications is 81 through 48094. Mean (3002) is greater than Median (1558)
#Indicating right skewness

#Accept
#Range of number of acceptances is 72 through 26330. Mean (2019) is greater than Median (1110)
#Indicating right skewness

#Enroll
#Range of number of students enrolling in an university are from 35 through 6392
#Mean (780) is greater than median (434). Indicating right skewness

#Top10perc
#Number of students from their top 10% of high school class ranges from 1 through 96
#Mean (27.56) is slightly greater than median (23), indicating almost normal distribution

#Top25perc
#Number of students from their top 20% of high school class ranges from 9 through 100
#Mean (55.8) is almost equal to the median (54), indicating almost normal distribution

#F.Undergrad
#Number of full time undergraduates ranges from 139 through 31643
#Mean (3700) is greater than the median (1707), indicating right skewness

#P.Undergrad
#Number of part time undergraduates ranges from 1 through 21836
#Mean (855.3) is greater than the median (353), indicating right skewness

#Oustate
#Out of state tuition ranges from 2340 through 21700
#Mean (10441) is slighlt greater than the median (9990), indicating right skewness

#Room.Board
#Cost of room and boarding ranges from 1780 through 8124
#Mean (4358) is slighlt greater than the median (4200), indicating almost normally distributed

#Books
#Estimated books cost ranges from 96 through 2340
#Mean (549) is slighlt greater than the median (500), indicating almost normally distributed

#Personal
#Estimated personal spending ranges from 250 through 6800
#Mean (1341) is slighlt greater than the median (1200), indicating right skewness

#PhD
#Number of PhD faculties range from 8 through 103
#Mean (72.66) is slighlt smaller than the median (75), indicating almost normally distributed

#Terminal
#Percentage of faculty with a terminal degree ranges from 24 to 100
#Mean (79.7) is slihghtly smaller than median (82), indicating almost normally distribued

#S.F.Ratio
#Student by faculty ratio ranges from 2.5 through 39.8
#Mean (14.09) is slightly greater than median (13.6), indicating almost normally distributed

#Perc.alumni
#Percent of alumni donating to the university ranges from 0 through 64
#Mean (22.74) is slighlt greater than the median (21), indicating almost normally distributed

#Expend
#Instructional expenditure per student ranges from 3186 through 56233
#Mean (9660) is slighlt greater than the median (8377), indicating right skewness

#Grad Rate
#Graduation rate ranges from 10 through 118
#Mean (65.46) is almost equal to the median (65), indicating normally distributed


#(d)
College$AcceptRate <- College$Accept/College$Apps
#new column created


#(e)
College$Name <- rownames(College)
College %>% top_n(-5, AcceptRate) %>% arrange(AcceptRate) %>% select(Name)
#Top 5 most selective institutions are as follows: 
#1 Princeton University; 
#2 Harvard University; 
#3  Yale University; 
#4 Amherst College; 
#5 Brown University

df1 <- College[College$Private=='No',]
df1 %>% top_n(-5, AcceptRate) %>% arrange(AcceptRate) %>% select(Name)
#Top 5 most selective public institutions are as follows: 
#1  University of Virginia
#2  Rowan College of New Jersey
#3  Stockton College of New Jersey
#4  Montclair State University
#5  University of North Carolina at Chapel Hill


#(f)
public <- College[College$Private=='No',]   #Subset for public universities
private <- College[College$Private=='Yes',]   #Subset for private universities
mean(public$AcceptRate)  #Average of acceptance rate in Public universities
#0.7265305
mean(private$AcceptRate)  #Average of acceptance rate in Private universities
#0.7545812
#On average, Public universities are more selective 


#(g)
College$matricRate <- College$Enroll/College$Accept
#new column matricRate created


#(h)
str(College$AcceptRate)
str(College$matricRate)
#both are numeric
install.packages("ggplot2") #To use ggplot for overlapping scatter plot
library(ggplot2)
ggplot(College, aes(x= AcceptRate, y= matricRate )) + geom_point(aes(colour= Private)) + labs(x="Acceptance Rate ",y="Matriculation rate", title = "Acceptance Rate v/s Matriculation Rate") 


#(i)
options(scipen=99)
public <- College[College$Private=='No',]   #Subset for public universities
private <- College[College$Private=='Yes',]   #Subset for private universities
cor.test(public$AcceptRate, public$matricRate)
# cor= 0.3577088
# p-value = 0.00000008518
cor.test(private$AcceptRate, private$matricRate)
# cor= 0.001939816
#p-value = 0.9633
#For public universities: p-value is less than 0.05: (=0.00000008518) therefore we reject the null hypothesis
#The acceptance rate and marticulation rate are correlated for public universities
#For private universities: p-value is much more than 0.05: (=0.9633) therefore we fail to reject the null hypothesis
#The acceptance rate and marticulation rate are not correlated for private universities

#Therefore: In public universities: applications getting accepting and students enrolling show a similar behaviour as compared to private universities
#i.e. for a public university which accepts more applicants have higher enroll rate and universities which accepts less have lesser enroll rate 


#(j)
pairs(College[, 1:10], col= "steelblue")

#Private
#Private is a categorical variable and 
#hence, it's scatterplot with any numerical variable would not give any useful information.

#Apps
#Apps seems to have a linear relationship with Accept, Enroll, F.Undergrad
#Linear relationship refers to Apps increasing as Accept, Enroll, F.Undergrad increases
#All these graphs have very few outliers

#Accept
#Apps seems to have a linear relationship with Enroll, F.Undergrad
#All these graphs have very few outliers

#Enroll
#Apps seems to have a linear relationship with F.Undergrad with a few outliers

#Top10perc
#Top10perc has a linear relationship with Top25perc unless a threshold is reached
#Beyond the threshold, Top10perc remains the same with increase in Top25perc

#Others do not seem to have any significant linear relationships. 


#(k)
boxplot(College$Outstate~College$Private, 
        xlab= "Private", ylab= "Outstate", 
        col= c("coral", "steelblue"), 
        main= "Outstate V/S Private")
#For a public university: Outstate is significantly less than outstate for a private university
#Outstate for public has a few outliers


#(l) 

#(i)
Elite <- rep("No", nrow(College)) #Repeats 'No' to a new column till the entire length of dataset College
Elite[College$Top10perc> 50] <- "Yes"  
#Wherever Top10perc is greater than 50: Elite variable has 'Yes' substituted 
Elite <- as.factor(Elite)  #Converting the variable in a factor
College <- data.frame(College, Elite)  #Attaching the column to the dataframe

#(ii)
summary(College$Elite)
# No Yes 
# 699  78 
#78 universities are Elite
plot(College$Outstate~College$Elite, xlab= "Elite", ylab= "Outstate", main="Outstate V/S Elite", col= c("coral", "steelblue"))
#For elite universities, average outstate is almost twice than non-elite universities


#(m)
str(College)
dev.off()
par(mfrow=c(2,2))
hist(College$Apps, col= 'coral', main= "Default bins", xlab="Apps")
hist(College$Apps, col= 'coral', main= "20 bins", xlab="Apps", breaks=20)
hist(College$Accept, col= 'steelblue', main= "Default", xlab="Accept")
hist(College$Accept, col= 'steelblue', main= "6 bins", xlab="Accept", breaks=6)




#Problem 4

#(a)
Auto <- read.csv("auto.csv")

table(is.na(Auto))
#FALSE 
#3573 
#There are no 'NA' values

str(Auto)
#after seeing all the variables: horsepower seemed odd
#$ horsepower  : Factor w/ 94 levels "?","100","102",..: 17 35 29 29 24 42 47 46 48 40 ...
#It has a level '?'
#Hence the '?' is the missing values in the dataset

Auto <- Auto[!(Auto$horsepower=="?"), ]
summary(Auto$horsepower)
#? values are removed
#5 rows were deleted


#(b)
summary(Auto)
str(Auto)

#mpg has a range from 9 through 46.6 with mean 23.45: is a numeric variable
str(Auto$mpg) #is numeric: hence quantitative

#cylinders have an odd distribution from 3 to 8: looks like a category
table(Auto$cylinders)   #has 3,4,5,6,8 levels
Auto$cylinders <- factor(Auto$cylinders)  #converted to qualitative

#displacemnt has a range from 68 through 455 with mean 194.4: is a numeric variable
str(Auto$displacement) #is numeric: hence quantitative

str(Auto$horsepower) #shows it as a factor with 94 levels
#94 levels isn't possible
Auto$horsepower <- as.numeric(Auto$horsepower) #converting it into a numeric: quantitaive

#weight has a range from 1613 through 5140 with mean 2978: is a numeric variable
str(Auto$weight) #is numeric: hence quantitative

#acceleration has a range from 8 through 24.8 with mean 15.54: is a numeric variable
str(Auto$acceleration) #is numeric: hence quantitative

table(Auto$year)         #has very few levels: should be a category
Auto$year <- factor(Auto$year)    #hence qualitative

table(Auto$origin)      #has very few levels: should be a category
Auto$origin <- factor(Auto$origin)   #hence qualitative

#name has 304 levels: does not make sense
Auto$name <- as.character(Auto$name)    #neither qualitative nor quantitative

#Hence:
#Qualitative: Cylinders, year, origin
#Quantitaive: mpg, displacement, horsepower, weight, acceleration


#(c)
#The following are the ranges of each quantitative variable:
range(Auto$mpg)
#[1]  9.0 46.6
range(Auto$displacement)
#[1]  68 455
range(Auto$horsepower)
#[1]  2 94
range(Auto$weight)
#[1] 1613 5140
range(Auto$acceleration)
#[1]  8.0 24.8


#(d)
#The following are the pairs of mean and sd of quantitative variables:

mean(Auto$mpg)  #23.44592  
sd(Auto$mpg)   #7.805007
 
mean(Auto$displacement)  #194.412
sd(Auto$displacement)   #104.644

mean(Auto$horsepower)   #52.16071
sd(Auto$horsepower)   #29.49805

mean(Auto$weight)    #2977.584
sd(Auto$weight)    #849.4026

mean(Auto$acceleration)    #15.54133
sd(Auto$acceleration)    #2.758864


#(e)
Auto <- Auto[-(10:85), ]
#Removing 10 through 85 rows
library(psych)
describe(Auto)
#Following are the mean, standard deviation and range of the qualitative variables in the new dataset: 
#vars            mean    sd      range 
#mpg             24.40   7.87    35.6  
#displacement    187.24  99.68   387.0  
#horsepower      51.67   29.77   92.0 
#weight          2935.97 811.30  3348.0  
#acceleration    15.73   2.69    16.3 


#(f)
pairs(Auto[, c(1,3,4,5,6)], col= "steelblue")
# All Continuous variables: 
#mpg: 
#With increase in displacemnt, mpg decreases 
#With increase in weight, mpg decreases 
#displacement:
#With increase in weight, displacement increases
#With increases in acceleration, displacement decreases non-linearly
#horsepower:
#For less weight, horsepower is high. For more weight, horsepower is more. For mean values of weight, horsepower isn't predictable

#All factors:
table(Auto$cylinders, Auto$year)
#With increase in year: frequency of 4 cylinders increases
#Uneven distribution for 6 and 8 cylinders
#3 and 5 cylinders have very less distribution to predict the relationship
table(Auto$cylinders, Auto$origin)
#For 4 cylinders: Frequency in origin 1,2,3 is same: 69,61,69
#For 6 cylinders: Frequency in origin 1 is the max (73)
#For 8 cylinders: Frequency in origin 1 exists (103)
table(Auto$origin, Auto$year)
#In all the years: Frequency of observations in origin 1 is around 20 
#22 19 18 29 14 20 22 18 22 23  6 13 19
#For 2nd origin the frequency of observations amongst years is around 6:
#2  5  4  5  7  6  6  8  4  6  4  8  3  2
#For 2nd origin the frequency of observations amongst years is around 6:
#3  2  4  5  4  6  4  4  6  8  2 13 12  9


#Categorical and numerical data variables: 
boxplot(Auto$mpg~Auto$cylinders, col=rainbow(6))
#gas mileage is high for 4 cylinders, followed by 5 cylinders and 3 cylinders
#mpg is significantly less for 6 and 8 cylinders

boxplot(Auto$mpg~Auto$year, col=rainbow(6))
#In 80-82 years: mpg is high as compared to the rest

boxplot(Auto$mpg~Auto$origin, col=rainbow(6))
#1 origin has lower mpg and for 2 and 3 origin: mpg are almost similar


#(g)
#From the above boxplot: mpg has a relationship with the following:

#gas mileage is high for 4 cylinders, followed by 5 cylinders and 3 cylinders
#mpg is significantly less for 6 and 8 cylinders
#In 80-82 years: mpg is high as compared to the rest
#1 origin has lower mpg and for 2 and 3 origin: mpg are almost similar
#With increase in displacemnt, mpg decreases 
#With increase in weight, mpg decreases 



#Problem 5

#(a)
GunDeaths <- read.csv("gun_deaths.csv")
str(GunDeaths)
na.omit(GunDeaths)
GunDeaths <- read.csv("Gun_Deaths.csv")
GunDeaths$year <- factor(GunDeaths$year)
GunDeaths$month <- factor(GunDeaths$month)
GunDeaths$police <- factor(GunDeaths$police)

deathspermonth <- GunDeaths %>% group_by(month) %>% count()
deathspermonth
#   month   n
#1     1  8273
#2     2  7093
#3     3  8289
#4     4  8455
#5     5  8669
#6     6  8677
#7     7  8989
#8     8  8783
#9     9  8508
#10    10  8406
#11    11  8243
#12    12  8413



#(b)
dev.off()
deathspermonth$month <- month.abb[deathspermonth$month]
deathspermonth$month <- factor(deathspermonth$month, levels= month.abb)
barplot(deathspermonth$n~deathspermonth$month, 
        col= rainbow(12),
        xlab="Month", ylab= "Number of Deaths", main= "Number of Deaths per month")



#(c)
GunDeaths <- na.omit(GunDeaths)
G1 <- GunDeaths %>% group_by(intent) %>% count()
G1$number <- sort(G1$n, decreasing= T)
barplot(G1$number~G1$intent, col= "black", xlab= "Intent", ylab= "Count", main= "Intent frequency")



#(d)
boxplot(GunDeaths$age~GunDeaths$sex, col= c("coral", "steelblue"))
f <- GunDeaths[GunDeaths$sex=="F",]
mean(f$age)
#43.72623



#(e)
levels(GunDeaths$education)
g <- GunDeaths[GunDeaths$year=="2012" & GunDeaths$race== "White" & GunDeaths$education!= "Less than HS",]
nrow(g)
#17944 deaths



#(f)
GunDeaths$season[GunDeaths$month <=3] <- "Winter"
GunDeaths$season[GunDeaths$month>3 & GunDeaths$month<7] <- "Spring"
GunDeaths$season[GunDeaths$month>6 & GunDeaths$month<10] <- "Summer"
GunDeaths$season[GunDeaths$month>9] <- "Fall"
table(GunDeaths$season)
#Fall Spring Summer Winter 
#24388  25045  25548  23034

#Summer has the maximum gun deaths



#(g)
gd <- GunDeaths[GunDeaths$race == "White",]
table(gd$intent)
#Accidental     Homicide      Suicide Undetermined 
#1111         8293        54615          579 

#Whites are more likely to be killed by suicide (54615) than Homicide (8293)

table(GunDeaths$intent, GunDeaths$race)

#             Asian/Pacific Islander Black Hispanic Native American/Native Alaskan White
#Accidental                       12   312      142                             21  1111
#Homicide                        515 18956     5269                            296  8293
#Suicide                         724  3285     3120                            547 54615
#Undetermined                     10   122       72                             14   579

#Compared to blacks: Whites have more deaths due to suicides and lesser deaths due to homicides
#Compared to Hispanic: Whites have more deaths in both Homicide and suicide



#(h)
#police involvement: 1 refers to involved and 0 refers to not involved
table(GunDeaths$police, GunDeaths$year)
#In all the three years: GunDeaths without police were around 33000 and with police were only 470
table(GunDeaths$police, GunDeaths$month)
#In all the months: GunDeaths without police were around 8200 and with police were only 100
table(GunDeaths$police, GunDeaths$intent)
#Police was present only in Homicide (1402) intent cases
table(GunDeaths$police, GunDeaths$sex)
#For female deaths: Compared to police absent incidents(14386) police was present in 63 cases
#For male deaths: Compared to police absent incidents(85010) police was present in 1339 cases
table(GunDeaths$police, GunDeaths$race)
#Police was significantly absent in all the situations
#For place and education also: police was significantly absent during gundeaths
boxplot(GunDeaths$age, GunDeaths$police)
#Police was significantly absent for all age groups as compared to gundeaths without police



#Problem 6


#(a)
SalaryClass <- read.csv("salary-class.csv")
set.seed(99171)
index <- sample(2, nrow(SalaryClass), replace = T, prob = c(0.6,0.4))
TrainData <- SalaryClass[index == 1, ]
TestData <- SalaryClass[index == 2, ]


#(b)
#C&R Decision Tree uses Gini
library(rpart)  #For using rpart library used to make decision trees
str(SalaryClass) 
sc_rpart <- rpart(INCOME ~ ., data = TrainData, parms = list(split = "Gini"))
sc_rpart 
#Terminal nodes are the nodes having a '*' at the end of the each split info
#Hence, there are 5 terminal nodes

library(rpart.plot)     #To make a plot
rpart.plot(sc_rpart)
#Verified: There are 5 leaves of the three
dev.off()

#(c)
#Major Predictors of income in the order of priority are:
#1: MStatus: Root node in the decision tree (Therefore will have the least Gini)
#2: C.Gain: 2nd level branch with Support= 10443/19637, Confidence= 56%
#3: Degree: 2nd level branch with Support= 9021/19637, Confidenc= 44%


#(d)
#No decision rule justify the criteria of support (i.e. at least 5% in the training sample) and simultaneously confidence (more than 75% for “> 50K” or 90% for “≤ 50K”) given
#The three best rules that can be formulated are: 

#Decision Rule 1: 
#If (MSTATUS= Divorced OR Married-spouse-absent OR  Never-married OR Separated OR Widowed) and (C.GAIN< 7139.5) ; Then (Salary Class <=50k)
#Support = 5%, Confidence= 53%

#Decision Rule 2:
#If (MSTATUS= Married-AF-spouse OR Married-civ-spouse) and (DEGREE= 10th, 11th, 12th, 1st-4th, 5th-6th, 7th-8th, 9th, Assoc-acdm, Assoc-voc, HS-grad, Preschool, Some-college) and (C.GAIN< 5095.5) : Then: (Salary Class <=50k)
#Support=29%, Confidence = 31%

#Decision Rule 3:
#If (MSTATUS= Married-AF-spouse OR Married-civ-spouse) and (DEGREE= Bachelors, Doctorate, Masters) : Then: (Salary Class > 50k)
#Support: 71%, Confidence: 14%


#(e)
#Decision Tree 2:
#without pruning: hence keeping minsplit and minbucket as 0 and cp as -1
sc2 <- rpart(INCOME ~ ., data = TrainData, parms = list(split = "gini"), control = rpart.control(minsplit = 0, minbucket = 0, cp = -1))
sc2
#Too many nodes which is beyond plotting

#Decision Tree 3:
#500 records in parent branch refers to minsplit = 500
#100 records in the child branch refers to minbucket = 100
#keeping complexity parameter as 0
sc3 <- rpart(INCOME ~ ., data = TrainData, parms = list(split = "gini"), control = rpart.control(minsplit = 500, minbucket = 100, cp=0))
sc3
dev.off()
rpart.plot(sc3)
#Many more leaf nodes than the previous tree

#Playing around with complexity parameter: cp=0.1 
sc4 <- rpart(INCOME ~ ., data = TrainData, parms = list(split = "gini"), control = rpart.control(minsplit = 500, minbucket = 100, cp=0.1))
sc4
rpart.plot(sc4)
#3 leaf nodes

#cp=-0.1
sc5 <- rpart(INCOME ~ ., data = TrainData, parms = list(split = "gini"), control = rpart.control(minsplit = 500, minbucket = 100, cp=-0.1))
sc5
rpart.plot(sc5)
#Many leaf nodes with complex decisions

#Lower the cp (Till -1), more the over fitting leading to larger tree
#Higher the cp, more the error rate on training data leading to shorter tree
#Default cp =0.01





