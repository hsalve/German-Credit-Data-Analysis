#Assignment 1 
#Honey Salve, Parmeen Bindra, Sooraj Holla

#Q1:
#DECISION TREE ANALYSIS
#

install.packages("C50")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("e1071")

library(readxl)

GCdata <- read_excel("C:/Users/Honey/Downloads/GermanCredit_assgt1_S19.xls")
View(GCdata)

summary(GCdata)

#Proportion of Good to bad cases
install.packages('plyr')

library(plyr)

count(GCdata,'RESPONSE')

t <- table(GCdata$RESPONSE) 
# Bar plot 
barplot(t, main = "Bar Plot", xlab = "RESPONSE", ylab = "Frequency",col=c("darkgreen", "dark red"))
ptab<-prop.table(t)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Bar Plot", xlab = "RESPONSE", ylab = "Proportion",col=c("darkgreen", "dark red"))

install.packages("plotrix")
# Pie chart 
library(plotrix)
pie3D(table(GCdata$RESPONSE), labels = levels(GCdata$RESPONSE), explode = 0.1, main = "Proportion of good and bad credit")


#DATA EXPLORATION

#Missing values - NAs
sum(is.na(GCdata$NEW_CAR))  #766 NAs = 766/1000 *100 = 76.6%
sum(is.na(GCdata$USED_CAR))  #897 NAs = 89.7%
sum(is.na(GCdata$FURNITURE))  #819 NAs = 81.9%
sum(is.na(GCdata$`RADIO/TV`))  #720 NAs = 72%
sum(is.na(GCdata$EDUCATION))  #950 NAs = 95%
sum(is.na(GCdata$RETRAINING))  #903 NAs = 90.3%
sum(is.na(GCdata$PERSONAL_STATUS))  #310 NAs = 31%
sum(is.na(GCdata$AGE))  #9 NAs


#HANDLING MISSING VALUES 
GCdata$NEW_CAR[is.na(GCdata$NEW_CAR)] <- 0
GCdata$USED_CAR[is.na(GCdata$USED_CAR)] <- 0
GCdata$FURNITURE[is.na(GCdata$FURNITURE)] <- 0
GCdata$`RADIO/TV`[is.na(GCdata$`RADIO/TV`)] <- 0
GCdata$EDUCATION[is.na(GCdata$EDUCATION)] <- 0
GCdata$RETRAINING[is.na(GCdata$RETRAINING)] <- 0
GCdata$PERSONAL_STATUS[is.na(GCdata$PERSONAL_STATUS)] <- 4

GermanCredit<-GCdata[c(1:30)]
GermanCredit <- GermanCredit[,-c(1)]

GermanCredit$AGE[is.na(GermanCredit$AGE)] <- round(median(GermanCredit$AGE, na.rm = TRUE))

install.packages("gmodels")
library(gmodels)
CrossTable(GermanCredit$RESPONSE, GermanCredit$OWN_RES, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(GermanCredit$RESPONSE, GermanCredit$JOB, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(GermanCredit$RESPONSE, GermanCredit$HISTORY, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(GermanCredit$RESPONSE, GermanCredit$PERSONAL_STATUS, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)


#CHANGE DATA TYPES OF VARIABLES:
cols <- c("RESPONSE","FOREIGN","TELEPHONE","JOB","OWN_RES","RENT","OTHER_INSTALL","PROP_UNKN_NONE","REAL_ESTATE","PRESENT_RESIDENT","GUARANTOR","CO-APPLICANT","PERSONAL_STATUS","EMPLOYMENT","SAV_ACCT","RETRAINING","EDUCATION","RADIO/TV","FURNITURE","USED_CAR","NEW_CAR","HISTORY","CHK_ACCT")
GermanCredit[,cols] <- lapply(GermanCredit[,cols], factor)


#PLOT THE PROPORTION OF RESPONSE VARIABLE
plot(GermanCredit$RESPONSE, col="dark green", xlab="Whether customer has a good or bad credit?",
     ylab="Number of cases", main="Proportion of good / bad credit cases")

#DESCRIPTIVE STATISTICS

library(pastecs)

GCdata_stat<-stat.desc(GCdata)
GCdata_stat

#BIVARIATE ANALYSIS

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)

#OWNS_RES Vs RESPONSE

OwnResdat <- data.frame(table(GermanCredit$OWN_RES, GermanCredit$RESPONSE))
names(OwnResdat) <- c("OWN_RES", "RESPONSE", "Count")
ggplot(data = OwnResdat, aes(x=OWN_RES, y=Count, fill=RESPONSE)) + 
  xlab("Owns a residence? (0 - No, 1 - Yes)") + ylab("Number of Customers") +
  ggtitle("OWN_RES Vs. REPONSE") +
  geom_bar(stat="identity") + 
  scale_fill_manual("CREDIT RESPONSE", values = c("0" = "orange", "1" = "light blue")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", 
                                        size = 2, linetype = "solid"))

#JOB Vs RESPONSE

Jobdat <- data.frame(table(GermanCredit$JOB, GermanCredit$RESPONSE))
names(Jobdat) <- c("JOB", "RESPONSE", "Count")
ggplot(data = Jobdat, aes(x=JOB, y=Count, fill=RESPONSE)) + 
  xlab("Has a job? (0 - No, 1 - Yes)") + ylab("Number of Customers") +
  ggtitle("JOB Vs. REPONSE") +
  geom_bar(stat="identity") + 
  scale_fill_manual("CREDIT RESPONSE", values = c("0" = "lightpink", "1" = "cyan2")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", 
                                        size = 2, linetype = "solid"))

#HISTORY Vs RESPONSE

Historydat <- data.frame(table(GermanCredit$HISTORY, GermanCredit$RESPONSE))
names(Historydat) <- c("HISTORY", "RESPONSE", "Count")
ggplot(data = Historydat, aes(x=HISTORY, y=Count, fill=RESPONSE)) + 
  xlab("Credit History") + ylab("Number of Customers") +
  ggtitle("HISTORY Vs. REPONSE") +
  geom_bar(stat="identity") + 
  scale_fill_manual("CREDIT RESPONSE", values = c("0" = "deepskyblue", "1" = "midnightblue")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", 
                                        size = 2, linetype = "solid"))

#PERSONAL_STATUS Vs RESPONSE

Statusdat <- data.frame(table(GermanCredit$PERSONAL_STATUS, GermanCredit$RESPONSE))
names(Statusdat) <- c("PERSONAL_STATUS", "RESPONSE", "Count")
ggplot(data = Statusdat, aes(x=PERSONAL_STATUS, y=Count, fill=RESPONSE)) + 
  xlab("Whether the customer is single, married or divorced?") + ylab("Number of Customers") +
  ggtitle("PERSONAL STATUS Vs. REPONSE") +
  geom_bar(stat="identity") + 
  scale_fill_manual("CREDIT RESPONSE", values = c("0" = "orange", "1" = "steelblue")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", 
                                        size = 2, linetype = "solid"))


#FOREIGN Vs RESPONSE
dat <- data.frame(table(GermanCredit$FOREIGN, GermanCredit$RESPONSE))
names(dat) <- c("FOREIGN", "RESPONSE", "Count")
ggplot(data=dat, aes(x=FOREIGN, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

#GUARANTOR vs RESPONSE

#NUM_DEPENDANTS vs RESPONSE

#EMPLOYMENT vs RESPONSE 

#AGE vs RESPONSE


#RESPONSE VS CHECKING ACCOUNT STATUS
dat1 <- data.frame(table(GermanCredit$CHK_ACCT, GermanCredit$RESPONSE))
names(dat1) <- c("CHK_ACCT", "RESPONSE", "Count")
ggplot(data=dat1, aes(x=CHK_ACCT, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

#RESPONSE VS AMOUNT
boxplot(AMOUNT ~ RESPONSE, data=GermanCredit, main="Amount and Response", 
        xlab="Response", ylab="Amount",
        col=c("green", "lightblue2"))

#RESPONSE VS DURATION
boxplot(DURATION ~ RESPONSE, data=GermanCredit, main="Duration and Response", 
        xlab="Response", ylab="Duration",
        col=c("yellow", "lightblue"))



#Q2
#DECISION TREE

install.packages("rpart")
library('rpart')

#Model1 - with full data
GCdatamodel1=rpart(RESPONSE ~ ., data=GermanCredit, method="class")

#print the model 
print(GCdatamodel1)

plot(GCdatamodel1,uniform=TRUE, main="Decision Tree on full data")
text(GCdatamodel1,use.n = TRUE, all=TRUE, cex=.7)

#for a better presentation
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot::prp(GCdatamodel1, type=0, extra=3)

GCdatamodel1$variable.importance

#Model2 - with Information Gain
GCdatamodelIG=rpart(RESPONSE ~ ., data=GermanCredit, method="class",  parms = list(split = "information"))
rpart.plot::prp(GCdatamodelIG, type=0, extra=3)
ConfMatIG <- table(pred=predict(GCdatamodelIG,GermanCredit, type="class"), true=GermanCredit$RESPONSE)
accuracyIG <- sum(diag(ConfMatIG))/sum(ConfMatIG)
accuracyIG 

sensitivityIG<- sum(ConfMatIG[2,2])/(ConfMatIG[1,2]+ConfMatIG[2,2])
sensitivityIG

#Model3 - Information Gain with minsplit and minbucket = 3
GCdatamodelIGM=rpart(RESPONSE ~ ., data=GermanCredit, method="class",  parms = list(prior = c(.70,.30), split = "information"),minsplit=10,minbucket=3)
rpart.plot::prp(GCdatamodelIGM, type=0, extra=3)
ConfMatIGM <- table(pred=predict(GCdatamodelIGM,GermanCredit, type="class"), true=GermanCredit$RESPONSE)
accuracyIGM <- sum(diag(ConfMatIGM))/sum(ConfMatIGM)
accuracyIGM 

sensitivityIGM<- sum(ConfMatIGM[2,2])/(ConfMatIGM[1,2]+ConfMatIGM[2,2])
sensitivityIGM

#Model4 - Information Gain with minsplit and minbucket = 10
GCdatamodelIGM2=rpart(RESPONSE ~ ., data=GermanCredit, method="class",  parms = list(prior = c(.70,.30), split = "information"),minsplit=10,minbucket=10)
rpart.plot::prp(GCdatamodelIGM2, type=0, extra=3)
ConfMatIGM2 <- table(pred=predict(GCdatamodelIGM2,GermanCredit, type="class"), true=GermanCredit$RESPONSE)
accuracyIGM2 <- sum(diag(ConfMatIGM2))/sum(ConfMatIGM2)
accuracyIGM2

sensitivityIGM2<- sum(ConfMatIGM2[2,2])/(ConfMatIGM2[1,2]+ConfMatIGM2[2,2])
sensitivityIGM2

#Model5 - with Gini Index
ctrl = rpart.control(maxdepth=10)
GCdatamodelGini=rpart(RESPONSE ~ ., data=GermanCredit, method="class",  parms = list(split = 'gini'), control=ctrl)
rpart.plot::prp(GCdatamodelGini, type=0, extra=3)
GCdatamodelGini$variable.importance

#ACCURACY & SENSITIVITY LEVEL
ConfMatGC <- table(pred=predict(GCdatamodelGini,GermanCredit, type="class"), true=GermanCredit$RESPONSE)
accuracyGC1 <- sum(diag(ConfMatGC))/sum(ConfMatGC)
accuracyGC1 

sensitivity1<- sum(ConfMatGC[2,2])/(ConfMatGC[1,2]+ConfMatGC[2,2])
sensitivity1 

ConfMatGC
n = sum(ConfMatGC) # number of instances
diag = diag(ConfMatGC) # number of correctly classified instances per class 
rowsums = apply(ConfMatGC, 2, sum) # number of instances per class
colsums = apply(ConfMatGC, 1, sum)#Lift chart 
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
precision
recall = diag / rowsums 
recall
f1 = 2 * precision * recall / (precision + recall) 
f1

#Lift chart 
GermanCredit1 <- GermanCredit
library(ROCR)
GermanCredit1$score<-predict(GCdatamodelGini,type='prob',GermanCredit1)
pred<-prediction(GermanCredit1$score[,1],GermanCredit1$RESPONSE)
perf1 <- performance(pred,"lift","rpp")
plot(perf1, main="Lift Curve - Gini Model for ",colorize=T,ylab="Lift value",xlab="Rate of positive predictions for credit responses")
GC_Lift <- plotLift(GermanCredit1$score,GermanCredit1$RESPONSE)


#Q3

#Data Split
nr=nrow(GermanCredit)

trnIndex = sample(1:nr, size = round(0.8*nr), replace=FALSE) 
GCTrain=GermanCredit[trnIndex,] 
GCTest = GermanCredit[-trnIndex,]

GCModel1=rpart(RESPONSE ~ ., data=GCTrain, method="class",control = rpart.control(cp=0.01))
GCModel22=rpart(RESPONSE ~ ., data=GCTest, method="prob",control = rpart.control(cp=0.01))

predTrn=predict(GCModel2, GCTrain, type='class')

print(GCModel2)

summary(GCModel2)

options(scipen=999)
options(digits=4)

rpart.plot::prp(GCModel1, type=0, extra=4, main="Decision Tree for Training set")

#Minsplit = 15
GCModel3=rpart(RESPONSE ~ ., data=GCTrain, method="class",minsplit=15)

rpart.plot::prp(GCModel3, type=0, extra=5, main="Decision Tree for Training set with minsplit = 15")


#maxdepth = 3
GCModel4=rpart(RESPONSE ~ ., data=GCTrain, method="class",maxdepth = 3)

rpart.plot::prp(GCModel4p, type=0, extra=4, main="Decision Tree for Training set with maxdepth = 3")


#Split = Information Gain
GCModel5=rpart(RESPONSE ~ ., data=GCTrain, method="class",parms = list(split = "information"))

rpart.plot::prp(GCModel5, type=0, extra=7, main="Decision Tree for Training set based on Information Gain")


#cp = 0.02
GCModel6=rpart(RESPONSE ~ ., data=GCTrain, method="class",control = rpart.control(cp=0.02))#0.01449275 #0.0126582
rpart.plot::prp(GCModel6, type=0, extra=7, main="Decision Tree for Training set with cp = 0.02")

#C5.0
#develop a tree on the training data

GCModel1=C5.0(RESPONSE ~ ., data=GC_train, method="class")
summary(GCModel1)
predTrn=predict(GCModel1, GC_train, type='class')
table(pred = predTrn, true=GC_train$RESPONSE)

options(scipen=999)
options(digits=4)

plot(GCModel1, uniform=TRUE,  main="Decision Tree for Credit defaulters")
text(GCModel1, use.n=TRUE, all=TRUE, cex=.7)


#Calculations are simialr for every model
cm <- table(pred=predict(GCModel1,GCTest, type="class"), true=GCTrain$RESPONSE)
n = sum(cm) # number of instances
diag = diag(cm)
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy #0.75
precision = diag / colsums 
precision  #0: 0.6316, 1: 0.7778
recall = diag / rowsums 
recall  #0: 0.4, 1: 0.9
f1 = 2 * precision * recall / (precision + recall) 
f1 #0: 0.4898, 1: 0.8344


library(ROCR)
#score test data set
GCTest$score<-predict(GCModel2,type='prob',GCTest)
pred<-prediction(GCTest$score[,2],GCTest$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf, main= "ROC Curve")

GCTest$score<-predict(GCModel2,type='prob',GCTest)
predTst<-prediction(GCTest$score[,2],GCTest$RESPONSE)
perf2 <- performance(predTst,"lift","rpp")
plot(perf2, main="Lift Curve",colorize=T)

##ROC curve : 
perf3 <- performance(predTst,"tpr","fpr")
plot(perf3, main= "ROC Curve",colorize=T)
summary(perf3)

#AUC value
auc.perf = performance(predTst, measure = "auc")
auc.perf@y.values #0.7386



predTest<-rpart(RESPONSE~.,data=GCTrain,method="class",control = rpart.control(cp=0.001))
printcp(predTest)
plotcp(predTest)
plotcp(predTest, minline=TRUE, col="blue", lwd=2, lty=1)# draw line 1 SD above minimum rel.error

pfit<- prune(GCModel4, cp=   GCModel4$cptable[which.min(GCModel4$cptable[,"xerror"]),"CP"])

cp = min(predTest$cptable[8,])
prune.tree.pros = prune(predTest, cp = cp)
plot(as.party(prune.tree.pros))

pfit<- prune(GCModel4, cp=   GCModel4$cptable[which.min(GCModel4$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for Kyphosis")



#Q4

costMatrix <- matrix(c(0,1,5, 0), byrow=TRUE, nrow=2)
colnames(costMatrix) <- c('Predict Good','Predict Bad')
rownames(costMatrix) <- c('Actual Good','Actual Bad')
costMatrix

#Threshold for better performance
CTHRESH=0.65 

predProbTrn=predict(GCModel2, GC_test, type='prob')
summary(predProbTrn)

#Confusion Matrix Table
predTrnThresh = ifelse(predProbTrn [,'1']>= CTHRESH, '1', '0')
ct = table( pred = predTrnThresh, true=GC_test$RESPONSE)
accuracy2 <- sum(diag(ct))/sum(ct)
accuracy2

#Lift and ROC
GC_test1 <- GC_test
library(ROCR)
GC_test1$score<-predict(GCModel1,type='prob',GC_test1)
predTst<-prediction(GC_test1$score[,2],GC_test1$RESPONSE)
perf2 <- performance(predTst,"lift","rpp")
plot(perf2, main="lift curve",colorize=T)
GC_Lift <- plotLift(GC_test1$score,GC_test1$RESPONSE)

#ROC curve 
perf3 <- performance(predTst,"tpr","fpr")
plot(perf3,colorize=T)
summary(perf3)

#AUC (Area under curve)
auc.perf = performance(predTst, measure = "auc")
auc.perf@y.values #0.7781965

#optimal cutoff
cost.perf = performance(predTst, "cost")
predTst@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

#Theoretical threshold
th = costMatrix[2,1]/(costMatrix[2,1] + costMatrix[1,2])
th


#Q6

library(dplyr)

PROFITVAL=100
COSTVAL=-500

scoreTst=predict(GCModel2,GCTest, type="prob")[,'1'] 
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, GCTest$RESPONSE)

prLifts=prLifts[order(-scoreTst) ,] 

#add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`GC_test$RESPONSE`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))

plot(prLifts$cumProfits, color='red')

#Find score coresponding to the max profit

maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))

#Max Profit: 13,000 DM, cutoff prob: 0.77


#Q7

#RANDOM FOREST

#Split data into training and testing sets
#Split data into training and testing sets
GCdataset2 <- GCdataset
colnames(GCdataset2)[colnames(GCdataset2)=="RADIO/TV"] <- "RADIO_Or_TV"
colnames(GCdataset2)[colnames(GCdataset2)=="CO-APPLICANT"] <- "CO_APPLICANT"

TRG_PCT=0.7
nr=nrow(GCdataset2)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) 
#get a random 70%sample of row-indices
GCRTrain=GCdataset2[trnIndex,]   #training data with the randomly selected row-indices
GCRTest = GCdataset2[-trnIndex,]  #test data with the other row-indices

install.packages("randomForest")
library('randomForest')

set.seed(123)

#develop a model with 200 trees, and obtain variable importance
GCRFModel = randomForest(RESPONSE ~ ., data=GCRTrain, ntree=200, importance=TRUE)
print(GCRFModel)
#check the model -- see what OOB error rate it gives

#Variable importance
importance(GCRFModel)
varImpPlot(GCRFModel)

#Draw the ROC curve for the randomForest model
GCRFPerfTest=performance(prediction(predict(BESTMTRYMODEL,GCRTest, type="prob")[,2], GCRTest$RESPONSE), "tpr", "fpr")
plot(GCRFPerfTest,main="ROC Curve for the best Random Forest Model",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

auc = performance((prediction((predict(GCRFModel,GCRTest,type = "prob"))[,2], GCRTest$RESPONSE)), "auc")
auc

#Find the optimal mtry value
mtry <- tuneRF(GCdataset2[,-29],GCdataset2$RESPONSE, ntreeTry=200,stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)

#mtry OOBError
#4.OOB    4    0.249
#5.OOB    5    0.240
#7.OOB    7    0.242

#Best is mtry = 5 since it has the least OOB error. 

#Build model again using best mtry value
set.seed(71)
BESTMTRYMODEL <-randomForest(RESPONSE~.,data=GCdataset2, mtry=5, importance=TRUE,ntree=200)
GCRFMODEL4 <-randomForest(RESPONSE~.,data=GCdataset2, mtry=4, importance=TRUE,ntree=200)
GCRFMODEL7 <-randomForest(RESPONSE~.,data=GCdataset2, mtry=7, importance=TRUE,ntree=200)

print(BESTMTRYMODEL)

importance(BESTMTRYMODEL)
varImpPlot(BESTMTRYMODEL)

#Draw the ROC curve for the randomForest model
pred1=predict(BESTMTRYMODEL,type = "prob")

library(ROCR)
perf = prediction((predict(BESTMTRYMODEL,type = "prob"))[,2], GCRTest$RESPONSE)

# 1. Area under curve
auc = performance(perf, "auc")
auc

# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")

# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for the best Random Forest Model",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

perfROC_dt1Tst=performance(prediction(predict(BESTMTRYMODEL,GCRTest, type="prob")[,2], GCRTest$RESPONSE), "tpr", "fpr")
perfRoc_dt2Tst=performance(prediction(predict(GCModel3,GCTest, type="prob")[,2], GCTest$RESPONSE), "tpr", "fpr")

plot(perfROC_dt1Tst, col='red')
plot(perfRoc_dt2Tst, col='blue', add=TRUE)
plot(perfRoc_rfTst, col='green', add=TRUE)
legend('bottomright', c('DecisionTree-1', 'DecisionTree-2', 'RandomForest'), lty=1, col=c('red', 'blue', 'green'))





