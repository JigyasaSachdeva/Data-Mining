#Jigyasa Sachdeva
#Homework 2
#IDS-572, Data Mining, Semester: Spring'20
#Masters in Management Information Systems
#Liautaud Business School, University of Illinois at Chicago


#Problem 2

install.packages ("NHANES")
library (NHANES)
library (dplyr)
glimpse (NHANES)

#a
#Classification Tree to predict sleep trouble
library(rpart)
NHANES$SleepHrsNight <- factor(NHANES$SleepHrsNight)
NHANES <- NHANES[!is.na(NHANES$SleepTrouble),]
#Removing null values in the target variable
#Left with 7772 observations
NHANES <- NHANES[!is.na(NHANES$SleepHrsNight),]
#Left with 7755 variables
NHANES <- NHANES[!is.na(NHANES$Depressed),]
#Left with 660 variables
t1 <- rpart(SleepTrouble~ SleepHrsNight+ Depressed, data=NHANES, parms= list(split="gini"))

#b
summary(t1)
#Splits were selected on the basis of Gini Index 
#The importance of variables is assigned as: SleepHrsNight = 84% and Depressed = 16%
#Which means if SleepHours is removed from the tree, the accuracy reduces by 84%
#And similarly if Depressed is removed, the accuracy is reduced by 16%

#There are 3 terminal nodes
#Node 2- Target Value= No, Loss= 0.23, Support= 88%
#Node 6: Target Value= No, Loss= 0.41, Support= 8%
#Node 7: Target Value= Yes, Loss= 0.65, Support= 4%

#Decision rules for predicting unseen observations:
#Rule 1: If SleepHrsNight >= 6, Then SleepTrouble= No
#Rule 2: If SleepHrsNight NOT >= 6 AND Depression = NONE, Then SleepTrouble = No
#Rule 3: If SleepHrsNight NOT >= 6 AND Depression NOT= NONE, Then SleepTrouble = Yes

library(rpart.plot)
dev.off()
rpart.plot(t1)

#c
prop.table(table(NHANES$SleepTrouble))
#No     Yes 
#0.7403 0.2596
#25.96% people have a trouble sleeping

#Probability of subjects having sleep trouble calculated from the tree is as follows:
#Probability of 7th node with Target = Yes refers to the population having trouble sleeping
#Given P(node)= 0.04174
#Therefore, 4.08% are predicted to have trouble sleeping

#d
index <- sample(2, nrow(NHANES), replace = T, prob = c(0.75,0.25))
TrainData <- NHANES[index == 1, ]
TestData <- NHANES[index == 2, ]
NHANES_rpart <- rpart(SleepTrouble~ SleepHrsNight+ Depressed, data = TrainData, parms = list(split = "gini"))
rpart.plot(NHANES_rpart)
pred_Test_class <- predict(NHANES_rpart, newdata = TestData, type = "class")
pred_Test_class1 <- predict(NHANES_rpart, newdata = TestData, type = "prob")
p1 <- pred_Test_class1[,-1]
p1
install.packages("caret")
library(caret)      #For confusionMatrix function

#Default threshold = 0.5
library(caret)
confusionMatrix(pred_Test_class, TestData$SleepTrouble)

#Threshold = 0.259
probabilities <- predict(NHANES_rpart, newdata = TestData, type = "prob")
p <- probabilities[,-1]
p
predicted.classes <- ifelse(p > 0.259, "Yes", "No")
predicted.classes <- factor(predicted.classes)
confusionMatrix(predicted.classes, TestData$SleepTrouble)


#e

#Install package
install.packages("ROCR")
library(ROCR)
# Calculating the values for ROC curve
TestData$x[TestData$SleepTrouble == "No"] <- 0
TestData$x[TestData$SleepTrouble == "Yes"] <- 1
pred = prediction(p1, TestData$x)
perf = performance(pred,"tpr","fpr")
# Plotting the ROC curve

plot(perf, col = "red", lty = 3, lwd = 3)
dev.off()

#Selecting cut point

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)}
print(opt.cut(perf, pred))


#[,1]
#sensitivity 0.2321839
#specificity 0.9077901
#cutoff      0.4168734

#Hence we take the cut off point as 0.4168 as threshold for person
#having sleep trouble. It is the minimal distance from a point whose false positive rate
#is zero and true positive rate is one



#f
index <- sample(2, nrow(NHANES), replace = T, prob = c(0.75,0.25))
TrainData <- NHANES[index == 1, ]
TestData <- NHANES[index == 2, ]

library(rpart)
library(rpart.plot)
NHANES_TREE <- rpart(SleepTrouble~ ., data = TrainData, parms = list(split = "gini"))
rpart.plot(NHANES_TREE)



#pred_test <- predict(NHANES_rpart, newdata = TestData, type = "class")
pred_test <- predict(NHANES_TREE, newdata = TestData, type = "prob")
p2 <- pred_test[,-1]
p2

prednew = prediction(p2, str(TestData$SleepTrouble))

perfnew = performance(prednew,"tpr","fpr")
# Plotting the ROC curve
plot(perfnew, col = "black", lty = 3, lwd = 3)


dev.off()
par(mfrow=c(1,2))


dev.off()
#AUC of plot with all variables: 
auc <- performance(prednew, "auc")
auc
auc <- unlist(slot(auc, "y.values"))
auc
#0.6187293

#AUC of the plot with 2 variables: SleepDeprived and Depressed: 
auc1 <- performance(pred, "auc")
auc1
auc1 <- unlist(slot(auc1, "y.values"))
auc1
#0.572241

pred_class <- predict(NHANES_TREE, newdata = TestData, type = "class")
confusionMatrix(pred_class, TestData$SleepTrouble)
library(caret)

#Problem 3

spambase <- read.csv("~/Desktop/Second Sem/Data Mining/Assignment 2/spambase.data")
View(spambase)
str(spambase)
which(is.na(spambase))    #No NAs
colSums(is.na(spambase))    #No NAs in each columns

library(funModeling)    #For df_status
df_status(spambase)
#On checking unique, we figure that only X1 has 2 unique values, rest have very large number of unique values
#Therefore: X1 is a category

#(a)

#(1)
spambase$X1 <- factor(spambase$X1)
prop.table(table(spambase$X1))
#0        1 
#0.606087 0.393913 
#The proportion of emails that are spam are 39.39%


#(2)
#Answer- Constant classifiers is Email
#If there was no classifier at all to separate spam, all the mails would have been marked as emails
#Hence the constant classifier would predict all as emails (X1 =0)

#(3)
Error <- sum(spambase$X1!=0)/nrow(spambase)
Error
#0.393913


#(b)

set.seed(1234)
index = sample(1:4600, 2300, replace=FALSE) 
training.data = spambase[index,]
testing.data = spambase[-index,]
dim(training.data)
#2300   58
dim(testing.data)
#2300   58

intersect(training.data, testing.data)
#data frame with 0 columns and 0 rows

prop.table(table(training.data$X1))
#0         1 
#0.6065217 0.3934783 
#39.35% of data in Training set is spam

prop.table(table(testing.data$X1))
#0         1 
#0.6056522 0.3943478 
#39.43% of data in Testing set is spam


#(c)

#(1)
install.packages("tree")
library(tree)
my.tree = tree(X1~., data= training.data)
summary(my.tree)

plot(my.tree)
text(my.tree)

x<- cv.tree(my.tree, method="misclass")
plot(x) 

  

my.tree.seq = prune.tree(my.tree) # Sequence of pruned tree sizes/errors
plot(my.tree.seq) # Plots size vs. error
my.tree.seq$dev # Vector of error rates for prunings, in order 
my.tree.seq$size[which.min(my.tree.seq$dev)] # Size of best tree

dev.off()
tree <- rpart(X1~., data=training.data, control = rpart.control(minbucket =  40))
rpart.plot(tree)

p <- predict(tree, newdata=testing.data, type="class")
library(caret)
confusionMatrix(p, testing.data$X1)
#Error= (False Positive + False Negative) / Total
#Error = 76+189/2300
#Error = 11.52%

summary(tree)
#[1] "X0.778"  "X0.3"    "X0.15"   "X0.32.1" "X0.18"   "X3.756"  "X0.44"   "X0.16"  
#[9] "X278"    "X0.38"   "X0.17"  
#These variables are the part of the tree




#(2)

#Bagging
install.packages("adabag")
library(adabag)
my.bag = bagging(X1~., data= training.data)
summary(my.bag)

p <- predict(my.bag, newdata = testing.data)$error
p
#0.08434783
#8.43%
bag <- sort(my.bag$importance, decreasing=T)
bag


importanceplot(my.bag, horiz= T)
dev.off()

#Random Forest
install.packages("randomForest")
library(randomForest)
r <- randomForest(X1 ~ ., data=training.data, ntree=100, 
                  importance=TRUE, proximity=TRUE)

r
#OOB estimate of  error rate: 5.78%

importance(r)
varImpPlot(r, sort = TRUE)

#(3)
#Everything



#(d)

#(1)
#Random Forest
#Confusion matrix:
#     0   1  class.error
#0 1348  47  0.03369176
#1   80 825  0.08839779


r1 <- randomForest(X1 ~ ., data=training.data, ntree=100, 
                  importance=TRUE, proximity=TRUE)

r1
#Confusion matrix:
#  0   1 class.error
#0 1343  50  0.03589375
#1   81 826  0.08930540




