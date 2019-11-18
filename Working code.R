#DATA MINING - ASSIGNMENT 1
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

GCdataset<-GCdata[c(1:30)]
GCdataset <- GCdataset[,-c(1)]

GCdataset$AGE[is.na(GCdataset$AGE)] <- round(median(GCdataset$AGE, na.rm = TRUE))

install.packages("gmodels")
library(gmodels)
CrossTable(GCdataset$RESPONSE, GCdataset$OWN_RES, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(GCdataset$RESPONSE, GCdataset$JOB, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(GCdataset$RESPONSE, GCdataset$HISTORY, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(GCdataset$RESPONSE, GCdataset$PERSONAL_STATUS, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)


#CHANGE DATA TYPES OF VARIABLES:
cols <- c("RESPONSE","FOREIGN","TELEPHONE","JOB","OWN_RES","RENT","OTHER_INSTALL","PROP_UNKN_NONE","REAL_ESTATE","PRESENT_RESIDENT","GUARANTOR","CO-APPLICANT","PERSONAL_STATUS","EMPLOYMENT","SAV_ACCT","RETRAINING","EDUCATION","RADIO/TV","FURNITURE","USED_CAR","NEW_CAR","HISTORY","CHK_ACCT")
GCdataset[,cols] <- lapply(GCdataset[,cols], factor)


#PLOT THE PROPORTION OF RESPONSE VARIABLE
plot(GCdataset$RESPONSE, col="dark green", xlab="Whether customer has a good or bad credit?",
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

OwnResdat <- data.frame(table(GCdataset$OWN_RES, GCdataset$RESPONSE))
names(OwnResdat) <- c("OWN_RES", "RESPONSE", "Count")
ggplot(data = OwnResdat, aes(x=OWN_RES, y=Count, fill=RESPONSE)) + 
  xlab("Owns a residence? (0 - No, 1 - Yes)") + ylab("Number of Customers") +
  ggtitle("OWN_RES Vs. REPONSE") +
  geom_bar(stat="identity") + 
  scale_fill_manual("CREDIT RESPONSE", values = c("0" = "orange", "1" = "light blue")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", 
                                             size = 2, linetype = "solid"))

#JOB Vs RESPONSE

Jobdat <- data.frame(table(GCdataset$JOB, GCdataset$RESPONSE))
names(Jobdat) <- c("JOB", "RESPONSE", "Count")
ggplot(data = Jobdat, aes(x=JOB, y=Count, fill=RESPONSE)) + 
  xlab("Has a job? (0 - No, 1 - Yes)") + ylab("Number of Customers") +
  ggtitle("JOB Vs. REPONSE") +
  geom_bar(stat="identity") + 
  scale_fill_manual("CREDIT RESPONSE", values = c("0" = "lightpink", "1" = "cyan2")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", 
                                        size = 2, linetype = "solid"))

#HISTORY Vs RESPONSE

Historydat <- data.frame(table(GCdataset$HISTORY, GCdataset$RESPONSE))
names(Historydat) <- c("HISTORY", "RESPONSE", "Count")
ggplot(data = Historydat, aes(x=HISTORY, y=Count, fill=RESPONSE)) + 
  xlab("Credit History") + ylab("Number of Customers") +
  ggtitle("HISTORY Vs. REPONSE") +
  geom_bar(stat="identity") + 
  scale_fill_manual("CREDIT RESPONSE", values = c("0" = "deepskyblue", "1" = "midnightblue")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", 
                                        size = 2, linetype = "solid"))

#PERSONAL_STATUS Vs RESPONSE

Statusdat <- data.frame(table(GCdataset$PERSONAL_STATUS, GCdataset$RESPONSE))
names(Statusdat) <- c("PERSONAL_STATUS", "RESPONSE", "Count")
ggplot(data = Statusdat, aes(x=PERSONAL_STATUS, y=Count, fill=RESPONSE)) + 
  xlab("Whether the customer is single, married or divorced?") + ylab("Number of Customers") +
  ggtitle("PERSONAL STATUS Vs. REPONSE") +
  geom_bar(stat="identity") + 
  scale_fill_manual("CREDIT RESPONSE", values = c("0" = "orange", "1" = "steelblue")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", 
                                        size = 2, linetype = "solid"))x


#FOREIGN Vs RESPONSE
dat <- data.frame(table(GCdataset$FOREIGN, GCdataset$RESPONSE))
names(dat) <- c("FOREIGN", "RESPONSE", "Count")
ggplot(data=dat, aes(x=FOREIGN, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

#RESPONSE VS CHECKING ACCOUNT STATUS
dat1 <- data.frame(table(GCdataset$CHK_ACCT, GCdataset$RESPONSE))
names(dat1) <- c("CHK_ACCT", "RESPONSE", "Count")
ggplot(data=dat1, aes(x=CHK_ACCT, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

#RESPONSE VS AMOUNT
boxplot(AMOUNT ~ RESPONSE, data=GCdataset, main="Amount and Response", 
        xlab="Response", ylab="Amount",
        col=c("green", "lightblue2"))

#RESPONSE VS DURATION
boxplot(DURATION ~ RESPONSE, data=GCdataset, main="Duration and Response", 
        xlab="Response", ylab="Duration",
        col=c("yellow", "lightblue"))


#DECISION TREE

install.packages("rpart")
library('rpart')

#Model1 - with full data
GCdatamodel1=rpart(RESPONSE ~ ., data=GCdataset, method="class")

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
GCdatamodelIG=rpart(RESPONSE ~ ., data=GCdataset, method="class",  parms = list(split = "information"))
rpart.plot::prp(GCdatamodelIG, type=0, extra=3)
ConfMatIG <- table(pred=predict(GCdatamodelIG,GCdataset, type="class"), true=GCdataset$RESPONSE)
accuracyIG <- sum(diag(ConfMatIG))/sum(ConfMatIG)
accuracyIG 

sensitivityIG<- sum(ConfMatIG[2,2])/(ConfMatIG[1,2]+ConfMatIG[2,2])
sensitivityIG

#Model3 - Information Gain with minsplit and minbucket = 3
GCdatamodelIGM=rpart(RESPONSE ~ ., data=GCdataset, method="class",  parms = list(prior = c(.70,.30), split = "information"),minsplit=10,minbucket=3)
rpart.plot::prp(GCdatamodelIGM, type=0, extra=3)
ConfMatIGM <- table(pred=predict(GCdatamodelIGM,GCdataset, type="class"), true=GCdataset$RESPONSE)
accuracyIGM <- sum(diag(ConfMatIGM))/sum(ConfMatIGM)
accuracyIGM 

sensitivityIGM<- sum(ConfMatIGM[2,2])/(ConfMatIGM[1,2]+ConfMatIGM[2,2])
sensitivityIGM

#Model4 - Information Gain with minsplit and minbucket = 10
GCdatamodelIGM2=rpart(RESPONSE ~ ., data=GCdataset, method="class",  parms = list(prior = c(.70,.30), split = "information"),minsplit=10,minbucket=10)
rpart.plot::prp(GCdatamodelIGM2, type=0, extra=3)
ConfMatIGM2 <- table(pred=predict(GCdatamodelIGM2,GCdataset, type="class"), true=GCdataset$RESPONSE)
accuracyIGM2 <- sum(diag(ConfMatIGM2))/sum(ConfMatIGM2)
accuracyIGM2

sensitivityIGM2<- sum(ConfMatIGM2[2,2])/(ConfMatIGM2[1,2]+ConfMatIGM2[2,2])
sensitivityIGM2

#Model5 - with Gini Index
ctrl = rpart.control(maxdepth=10)
GCdatamodelGini=rpart(RESPONSE ~ ., data=GCdataset, method="class",  parms = list(split = 'gini'), control=ctrl)
rpart.plot::prp(GCdatamodelGini, type=0, extra=3)
GCdatamodelGini$variable.importance

#ACCURACY & SENSITIVITY LEVEL
ConfMatGC <- table(pred=predict(GCdatamodelGini,GCdataset, type="class"), true=GCdataset$RESPONSE)
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
GCdataset1 <- GCdataset
library(ROCR)
GCdataset1$score<-predict(GCdatamodelGini,type='prob',GCdataset1)
pred<-prediction(GCdataset1$score[,1],GCdataset1$RESPONSE)
perf1 <- performance(pred,"lift","rpp")
plot(perf1, main="Lift Curve - Gini Model for ",colorize=T,ylab="Lift value",xlab="Rate of positive predictions for credit responses")
GC_Lift <- plotLift(GCdataset1$score,GCdataset1$RESPONSE)


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

#Q7
#RANDOM FOREST

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




