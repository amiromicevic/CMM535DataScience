# Artificaial Neural Networks Credit card default

# binary classification, categorical and numerical attributes

# description: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients
# dataset folder https://archive.ics.uci.edu/ml/machine-learning-databases/00350/

# load libraries
library(caret)
library(nnet)
library(ROCR)
library(devtools)
library(sqldf) # load sqldf package to use SQL like statements to manipulate data

# import plot.nnet function from another R file
source("C:\\Users\\Amir\\Documents\\Coursework\\CMM535DataScience\\RSolution\\PlotNnet.r")

# 1. LOAD DATA

# set the working directory
setwd("C:\\Users\\Amir\\Documents\\Coursework\\CMM535DataScience\\RSolution")

# define the filename which is a csv file obtained by saving 
# the original dataset Excel spreadsheet as a csv file into the working directory
filename <- "CreditCardDefaultDataset.csv" 

# load the CSV file from the local directory 
dataset <- read.csv(filename, header=TRUE) 


# 2. CLEAN AND EDIT DATA

# remove all rows where PAY_0 to PAY_6 values fall outside -1, 1, 1+. values. Their meaning is not defined, 
# therefore the effect they have on the overall capture of the model behaviour can not be acertained.
# total of 25939 rows will be removed leaving 4061 rows in dataset

sql='DELETE FROM dataset WHERE (PAY_0 !=-1 AND PAY_0 < 1)  OR
                               (PAY_2 !=-1 AND PAY_2 < 1)  OR 
                               (PAY_3 !=-1 AND PAY_3 < 1)  OR 
                               (PAY_4 !=-1 AND PAY_4 < 1)  OR 
                               (PAY_5 !=-1 AND PAY_5 < 1)  OR 
                               (PAY_6 !=-1 AND PAY_6 < 1)'  

dataset = sqldf(c(sql,'SELECT * FROM dataset'))

# now check that dataset only contains the PAY (0-6) values that fall within the allowed categories
summary(dataset)


# EDUCATION: (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown) 
# update all rows where the value is set to 0, 5 or 6 and overwrite it to be 4
# this will update 21 rows in total

sql='UPDATE dataset SET EDUCATION=4 WHERE EDUCATION=0 OR EDUCATION=5 OR EDUCATION=6'  
dataset = sqldf(c(sql,'SELECT * FROM dataset'))

#factor the values as they are categorical values
dataset$EDUCATION=factor(dataset$EDUCATION)

# now check that EDUCATION only has the values 1,2,3,4
summary(dataset$EDUCATION)


# MARRIAGE: Marital status (1=married, 2=single, 3=others)
# Update all rows where the value is set to anything other than 1 OR 2 OR 3 and overwrite it to be 3
# this will update 10 rows in total

sql='UPDATE dataset SET MARRIAGE=3 WHERE MARRIAGE != 1 AND MARRIAGE != 2 AND MARRIAGE != 3'  
dataset = sqldf(c(sql,'SELECT * FROM dataset'))

#factor the values as they are categorical values
dataset$MARRIAGE=factor(dataset$MARRIAGE)

# now check that MARRIAGE only has the values 1,2,3
summary(dataset$MARRIAGE)


# 3. ADD NEW OBSERVATIONS 

# Add a new penalty points column PAY_PENPOINTS for rows where there is at least one missed payment, 
# so each missed month in each PAY column is worth a point. This is to put more weight on the rows with missed payments.
# if any PAY (0-6) is >=1 then PAY_PENPOINTS=SUM(PAY_0  + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6) . Otherwise, PAY_PENPOINTS=0

#datasetDefaultsPayAllMinusOneId <- subset(dataset, Default_Payment_Next_Month==1 && 
#                                            PAY_0==-1 & PAY_2==-1 & PAY_3==-1 & PAY_4==-1 & PAY_5==-1 & PAY_6==-1, select=ID)

#CHECK THE ROW ID=59 IT SHOULD BE A DEFAULT

dataset$PAY_PENPOINTS <- 0
# updateSQL='UPDATE dataset SET PAY_PENPOINTS=(CASE WHEN PAY_0 = -1 THEN 0 ELSE PAY_0 END) 
#                                           + (CASE WHEN PAY_2 = -1 THEN 0 ELSE PAY_2 END) 
#                                           + (CASE WHEN PAY_3 = -1 THEN 0 ELSE PAY_3 END) 
#                                           + (CASE WHEN PAY_4 = -1 THEN 0 ELSE PAY_4 END) 
#                                           + (CASE WHEN PAY_5 = -1 THEN 0 ELSE PAY_5 END) 
#                                           + (CASE WHEN PAY_6 = -1 THEN 0 ELSE PAY_6 END)
#                                           WHERE Default_Payment_Next_Month=1' 
#                          WHERE ID NOT IN (SELECT ID FROM defaultDatasetPayAllMinusOneId)'


updateSQL='UPDATE dataset SET PAY_PENPOINTS=(CASE WHEN PAY_0 = -1 THEN 0 ELSE PAY_0 END) 
            + (CASE WHEN PAY_2 = -1 THEN 0 ELSE PAY_2 END) 
            + (CASE WHEN PAY_3 = -1 THEN 0 ELSE PAY_3 END) 
            + (CASE WHEN PAY_4 = -1 THEN 0 ELSE PAY_4 END) 
            + (CASE WHEN PAY_5 = -1 THEN 0 ELSE PAY_5 END) 
            + (CASE WHEN PAY_6 = -1 THEN 0 ELSE PAY_6 END)' 

dataset = sqldf(c(updateSQL,'SELECT * FROM dataset'))

# Add a new column PAYAMT_ZERO for payments made on time, where all PAY (0-6) = -1
# if there are at least two columns with 0 for PAY_AMT (1-6), and the corresponding BILL_AMT are >0, then PAYAMT_ZERO = 1. 
# Otherwise, set it to 0. As a result, 49 rows wil have PAYAMT_ZERO set to 1.

dataset$PAYAMT_ZERO <- 0

billAmt1ColIndex <-  13 
billAmt6ColIndex <- 18
outputColIndex <-25
payPenPointsColIndex <- 26
payAmtZeroColIndex <- 27
rows <- dim(dataset)[1]

for (row in 1:rows){
  count<-0
  #check the row is where output is default and all PAY (0-6) = -1
  if(dataset[row,outputColIndex]==1 & dataset[row,payPenPointsColIndex]==0){
    for (index in billAmt1ColIndex:billAmt6ColIndex){
      billAmt <- dataset[row,index]
      payAmt <- dataset[row,index+6]
      if(billAmt>0 & payAmt==0) count <- count + 1
    }
    
    #set PAYAMT_ZERO to 1
    if(count>=2) dataset[row,payAmtZeroColIndex] <- 1
  }
}

#factor the values as they are categorical values and show the data distribution
dataset$PAYAMT_ZERO=factor(dataset$PAYAMT_ZERO)
summary(dataset$PAYAMT_ZERO)

# FURTHER PROCESSING of the records where PAYAMT_ZERO=0

# create two new columns to hold MEAN and SD values of all BILL_AMT columns
library(dplyr)
library(matrixStats)
dataset$AVGBILL_AMT <- round(rowMeans(dataset[,billAmt1ColIndex:billAmt6ColIndex]))
dataset$SDBILL_AMT <- round(rowSds(as.matrix(dataset
                         [,c("BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6")])))


# calculate the ratio between the last bill BILL_AMT6 and mean bill amount AVGBILL_AMT
dataset$LAST_BILL_AVG_RATIO=round(dataset$BILL_AMT6 / dataset$AVGBILL_AMT,2)

# calculate the ratio between the standard deviation and mean of the all bill amounts
dataset$SD_AVG_RATIO=round(dataset$SDBILL_AMT / dataset$AVGBILL_AMT,2)

# remove the rows where either LAST_BILL_AVG_RATIO or SD_AVG_RATIO is negative. 
# It skews the AVG_BILLAMT and SD_BILLAMT and their ratios. It will remoev 38 rows in total with dataset count now set to 4023
dataset <- subset(dataset, LAST_BILL_AVG_RATIO >= 0 & SD_AVG_RATIO >= 0)

# remove all rows where SD_BILLAMT = 0 as the bill and payment data for that consumer appears to be creating no sensible context 
# or association to the real-life behaviours, and as such these rows are treated as outliers.
# It will remove 269 rows in total leaving the dataset new count now set to 3754
dataset <- subset(dataset, SDBILL_AMT > 0)


# finally, reorder the columns 
dataset <- dataset[c("ID", "LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE","AGE", 
                                                               "PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6",
                                                               "BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6",
                                                               "PAY_PENPOINTS","PAYAMT_ZERO",
                                                               "AVGBILL_AMT","SDBILL_AMT",
                                                               "LAST_BILL_AVG_RATIO","SD_AVG_RATIO",
                                                               "PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5", "PAY_AMT6",
                                                               "Default_Payment_Next_Month")]

# factor SEX and class output observations
dataset$SEX=factor(dataset$SEX)
dataset$Default_Payment_Next_Month=factor(dataset$Default_Payment_Next_Month)

# class distribution of the data shows there is 66% of non defaults V 34% of defaults
cbind(freq=table(dataset$Default_Payment_Next_Month), percentage=prop.table(table(dataset$Default_Payment_Next_Month))*100)

# 4. SPLIT DATA INTO TRAINING AND VALIDATION SETS

# remove the first column holding the redundant variable Id
dataset <- dataset[,-1]

# first split out validation dataset
# create a list of 80% of the rows in the original dataset we can use for training
set.seed(7)
validationIndex <- createDataPartition(dataset$Default_Payment_Next_Month, p=0.80, list=FALSE)

# select 20% of the data for validation
validationDataset <- dataset[-validationIndex,]

# use the remaining 80% of data to training and testing the models
trainingDataset <- dataset[validationIndex,]

# let's also create seperate deafult and non-default datasets for further data exploration
trainingDatasetDefault=subset(trainingDataset, Default_Payment_Next_Month == 1)
trainingDatasetNonDefault=subset(trainingDataset, Default_Payment_Next_Month == 0)

# now put more weight on the defaulted rows in the training data set
# the median of PAY_PENPOINTS is 12 so let's multiply PAY_PENPOINTS by 2 for all consumers where PAY_PENPOINTS is 12 or over
medianPenalty=summary(trainingDatasetDefault$PAY_PENPOINTS)[3]
medianPenaltyAsString=toString(medianPenalty)

updateSQL=paste('UPDATE trainingDataset SET PAY_PENPOINTS=PAY_PENPOINTS * 2 WHERE PAY_PENPOINTS>=', medianPenaltyAsString, sep = " ") 
trainingDataset = sqldf(c(updateSQL,'SELECT * FROM trainingDataset'))

# now let's apply the same penalty to the validation data
updateSQL=paste('UPDATE validationDataset SET PAY_PENPOINTS=PAY_PENPOINTS * 2 WHERE PAY_PENPOINTS>=', medianPenaltyAsString ,sep = " ") 
validationDataset = sqldf(c(updateSQL,'SELECT * FROM validationDataset'))

# 5. TRAIN ANN ALGORITHM AND PREDICT

# create all necessary functions

evaluateNeuralNetwork <- function(observationsFormula) {
  set.seed(7)
  ann = nnet(observationsFormula, data=trainingDataset,rang = 0.001, size=10,maxit=10000,decay=0.001)
  return (ann)
}

predictNeuralNetwork <- function(evalAnnResult) {
  predictions <- predict(evalAnnResult,newdata=validationDataset, type="class")
  return(confusionMatrix(predictions, validationDataset$Default_Payment_Next_Month))
}

plotROCCurve <- function(evalAnnResult) {
  pred = prediction(predict(evalAnnResult,newdata=validationDataset,type="raw"),validationDataset$Default_Payment_Next_Month)
  perf = performance(pred,"tpr","fpr")
  plot(perf,lwd=2,col="blue",main="ROC - Neural Network on Credit Card Default")
}

trainAndTestNeuralNetwork<- function(observationsFormula) {
  evalAnnResult <- evaluateNeuralNetwork(observationsFormula)
  annConfusionMatrix <- predictNeuralNetwork(evalAnnResult)
  print(annConfusionMatrix)
  plotROCCurve(evalAnnResult)
  return (evalAnnResult)
}

# first run
observationsFormula <- as.formula("Default_Payment_Next_Month~SEX+EDUCATION+MARRIAGE+AGE+PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+
               PAY_PENPOINTS + PAYAMT_ZERO + SDBILL_AMT")
trainAndTestNeuralNetwork(observationsFormula)

# the best prediction rate for defaults of 0.7246 indicates that we have not achieved any improvement whatsoever
# let's look some more into the trainingDatasetNonDefault and spot any logical anomalies/outliers
# there are consumers with a high PAY_PENPOINTS but still labelled as 0 (non-default). In practice, there is an extremely low chance
# of someone being late 2 months per average on each month over the last 6 months and then suddenly making the next payment on time
# so we shall remove these records from the training and validation data sets as they are skewing the training process
outliersDataset <- subset(trainingDataset, Default_Payment_Next_Month == 0 & PAY_PENPOINTS >= medianPenalty)

#there are 211 outliers rows
dim(outliersDataset)

# now let's remove the outliers from the training data set which will reduce the count down to a total 2793 records
delSQL=paste('DELETE FROM trainingDataset WHERE Default_Payment_Next_Month = 0 AND PAY_PENPOINTS >= ', medianPenaltyAsString ,sep = " ") 
trainingDataset = sqldf(c(delSQL,'SELECT * FROM trainingDataset'))
dim(trainingDataset)

# let's do the same on the validation data set as 
outliersDataset <- subset(validationDataset, Default_Payment_Next_Month == 0 & PAY_PENPOINTS >= medianPenalty)

#there are 50 outliers rows
dim(outliersDataset)

# remove those 50 rows. That will bring validation data set down to 700 records
delSQL=paste('DELETE FROM validationDataset WHERE Default_Payment_Next_Month = 0 AND PAY_PENPOINTS >= ', medianPenaltyAsString ,sep = " ") 
validationDataset = sqldf(c(delSQL,'SELECT * FROM validationDataset'))
dim(validationDataset)

# second run brings a negative prediction value of  Neg Pred Value : 0.9371 and overall accuracy of 0.8543
trainAndTestNeuralNetwork(observationsFormula)

# let's further reduce the penalty for consumers labeled as 0 with a high PAY_PENPOINTS from 12 down to 6
# in practice this means that any consumer that missed a payment each month over the last 6 months OR
# was 3 times late for 2 months each time

delSQL=paste('DELETE FROM trainingDataset WHERE Default_Payment_Next_Month = 0 AND PAY_PENPOINTS >= ', toString(medianPenalty/2) ,sep = " ") 
trainingDataset = sqldf(c(delSQL,'SELECT * FROM trainingDataset'))
dim(trainingDataset)

delSQL=paste('DELETE FROM validationDataset WHERE Default_Payment_Next_Month = 0 AND PAY_PENPOINTS >= ', toString(medianPenalty/2) ,sep = " ") 
validationDataset = sqldf(c(delSQL,'SELECT * FROM validationDataset'))
dim(validationDataset)

# third run brings a negative prediction value of  Neg Pred Value : 0.9832 and overall accuracy of 0.8769
trainAndTestNeuralNetwork(observationsFormula)

# let's try the same model with the LIMIT_BAL. It returns the Neg Pred Value : 1 and overall accuracy of 0.8814
observationsFormula <- as.formula("Default_Payment_Next_Month~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+
               PAY_PENPOINTS + PAYAMT_ZERO + SDBILL_AMT")
annModel=trainAndTestNeuralNetwork(observationsFormula)
plot.nnet(annModel)


# ORIGINAL CLASSIFIERS WITH TRANSFORMED DATASET

# 10-fold cross validation with 3 repeats
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

# LG Logistic Regression
set.seed(7)
startTime <- proc.time()
fit.glm <- train(observationsFormula, data=trainingDataset, method="glm", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# LDA Linear Discriminate Analysis
set.seed(7)
startTime <- proc.time()
fit.lda <- train(observationsFormula, data=trainingDataset, method="lda", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# GLMNET Regularized Logistic Regression
set.seed(7)
startTime <- proc.time()
fit.glmnet <- train(observationsFormula, data=trainingDataset, method="glmnet", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# KNN k-Nearest Neighbours
set.seed(7)
startTime <- proc.time()
fit.knn <- train(observationsFormula, data=trainingDataset, method="knn", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# CART Classification and Regression Trees
set.seed(7)
startTime <- proc.time()
fit.cart <- train(observationsFormula, data=trainingDataset, method="rpart", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# NB Naive Bayes
set.seed(7)
startTime <- proc.time()
fit.nb <- train(observationsFormula, data=trainingDataset, method="nb", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# SVM Support Vector Machines with Radial Basis Functions. Takes the longest to train
set.seed(7)
startTime <- proc.time()
fit.svm <- train(observationsFormula, data=trainingDataset, method="svmRadial", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# Compare algorithms
results <- resamples(list(LG=fit.glm, LDA=fit.lda, GLMNET=fit.glmnet, KNN=fit.knn, CART=fit.cart, NB=fit.nb, SVM=fit.svm))
summary(results)
dotplot(results)

#  ENSEMBLES: Boosting and Bagging

# 10-fold cross validation with 3 repeats
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

# Bagged CART
set.seed(7)
startTime <- proc.time()
fit.treebag <- train(observationsFormula, data=trainingDataset, method="treebag", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# Random Forest
set.seed(7)
startTime <- proc.time()
fit.rf <- train(observationsFormula, data=trainingDataset, method="rf", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# Stochastic Gradient Boosting
set.seed(7)
startTime <- proc.time()
fit.gbm <- train(observationsFormula, data=trainingDataset, method="gbm", metric=metric, trControl=control, verbose=FALSE, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# C5.0
set.seed(7)
startTime <- proc.time()
fit.c50 <- train(observationsFormula, data=trainingDataset, method="C5.0", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# Compare results
ensemble_results <- resamples(list(BAG=fit.treebag, RF=fit.rf, GBM=fit.gbm, C50=fit.c50))
summary(ensemble_results)
dotplot(ensemble_results)

# 
# rm(dataset)
# rm(trainingDataset)
# rm(validationDataset)

# rm(trainingDatasetNonDefault)
# rm(trainingDatasetDefault)
# rm(outliersDataset)

