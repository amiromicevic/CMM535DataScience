# Artificaial Neural Networks Credit card default

# binary classification, categorical and numerical attributes

# description: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients
# dataset folder https://archive.ics.uci.edu/ml/machine-learning-databases/00350/

# load libraries
library(nnet)

# set the working directory
setwd("C:\\Users\\Amir\\Documents\\Coursework\\CMM535DataScience\\RSolution")

# 1. LOAD DATA

# define the filename which is a csv file obtained by saving 
# the original dataset Excel spreadsheet as a csv file into the working directory
filename <- "CreditCardDefaultDataset.csv" 

# load the CSV file from the local directory 
dataset <- read.csv(filename, header=TRUE) 

# 2. RESAMPLE DATA

# first split out validation dataset
# create a list of 80% of the rows in the original dataset we can use for training
set.seed(7)
validationIndex <- createDataPartition(dataset$Default_Payment_Next_Month, p=0.80, list=FALSE)

# select 20% of the data for validation
validationDataset <- dataset[-validationIndex,]

# use the remaining 80% of data to training and testing the models
trainingDataset <- dataset[validationIndex,]


# This method will take all data from the original dataset where the predicted class value corresponds to default 
# (class set to 1) and put it into a temporary dataset Default. Then it will divide the rest of the data 
# from the original dataset where the predicted class value corresponds to non-default (class set to 0) 
# into 5 equal sub-sets, and take an equal amount of data from each. It will put that data into 
# a temporary dataset NonDefault ensuring that it has the same number of rows as Default. 
# Then the data will be merged from the both datasets into a final dataset ResampledTrainingDataSet.

# take all default values and put them into Default dataset
defaultDataset <- trainingDataset[ which(trainingDataset$Default_Payment_Next_Month==1), ]
defaultCount <- dim(defaultDataset)[1]

# divide all non-default values into 5 equal subsets and take random rows from each and 
# put them into NonDefault dataset. Rows count in nonDefault will be equal to the rows count in Default
# for educational purposes, use subset as another way to extract rows based on a condition
nonDefaultDataset <- subset(trainingDataset, Default_Payment_Next_Month==0, select=ID:Default_Payment_Next_Month) 
resampledTrainingDataset <- nonDefaultDataset[0,] # create an emtpy final dataset based on the existing data structure
nonDefaultCount <- dim(nonDefaultDataset)[1]
subsetCount=5
subsetSize=defaultCount%/%subsetCount # size of each subset taken from nonDefault
nfPartitionSize=nonDefaultCount%/%subsetCount # size of each partition in nonDefault dataset from which we take subsets

for (i in 1:subsetCount){
  if(i==1) startIndex <- 1 else startIndex <- (i-1) * nfPartitionSize
  endIndex <-startIndex + nfPartitionSize -1 
  randomDataSample <- nonDefaultDataset[sample(startIndex:endIndex, subsetSize, replace=FALSE),]
  resampledTrainingDataset <- rbind(resampledTrainingDataset, randomDataSample) 
}

dim(resampledTrainingDataset)

# merge Default and NonDefault into ResampledTrainingData dataset
resampledTrainingDataset <- rbind(resampledTrainingDataset, defaultDataset)

# now randomise the order of the rows to mix 0s and 1s
set.seed(1)
resampledTrainingDataset <- resampledTrainingDataset[sample(1:nrow(resampledTrainingDataset)), ]

#show dimensions of the new resampled training data
dim(resampledTrainingDataset)

# class distribution of the new resampled training data
cbind(freq=table(resampledTrainingDataset$Default_Payment_Next_Month), 
      percentage=prop.table(table(resampledTrainingDataset$Default_Payment_Next_Month))*100)

#show summary for the data quality check
summary(resampledTrainingDataset)

# 3. PREPARE DATA

# prepare validation data set by removing the ID column 
# and transforming the class variable into a 2 level factor of 0 and 1
validationDataset <- validationDataset[,-1]
validationDataset$Default_Payment_Next_Month=factor(validationDataset$Default_Payment_Next_Month)

# remove the first column holding the redundant variable Id
resampledTrainingDataset <- resampledTrainingDataset[,-1]

# change the class variable from the integer to a factor with levels [0,1] 
# to enable the training for the binary classifiers
resampledTrainingDataset$Default_Payment_Next_Month=factor(resampledTrainingDataset$Default_Payment_Next_Month)

# 4. EVALUATE ANN ON RESAMPLED TRAINING DATA SET
set.seed(1)
ann = nnet(Default_Payment_Next_Month~., data=resampledTrainingDataset,rang = 0.005, size=23,maxit=10000,decay=0.001)
predictions <- predict(ann,newdata=validationDataset, type="class")
confusionMatrix(predictions, validationDataset$Default_Payment_Next_Month)

# 5. EVALUATE ANN ON FULL TRAINING DATA SET 

filename <- "CreditCardDefaultDataset.csv" 

# load the CSV file from the local directory 
datasetFull <- read.csv(filename, header=TRUE) 

# first split out validation dataset
# create a list of 80% of the rows in the original dataset we can use for training
set.seed(7)
validationIndex <- createDataPartition(datasetFullDefault_Payment_Next_Month, p=0.80, list=FALSE)

# select 20% of the data for validation and factor the output
validationDataset <- datasetFull[-validationIndex,]
validationDataset$Default_Payment_Next_Month=factor(validationDataset$Default_Payment_Next_Month)

# use the remaining 80% of data to training and testing the models and factor the output
trainingDataset <- datasetFull[validationIndex,]
trainingDataset$Default_Payment_Next_Month=factor(trainingDataset$Default_Payment_Next_Month)

# remove the first column holding the redundant variable Id
trainingDataset <- trainingDataset[,-1]
validationDataset <- validationDataset[,-1]


#evaluate ANN
set.seed(1)
#ann = nnet(Default_Payment_Next_Month~., data=resampledTrainingDataset,rang = 0.005, size=23,maxit=10000,decay=0.001)
ann = nnet(Default_Payment_Next_Month~SEX+EDUCATION+MARRIAGE+AGE+PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6, 
           data=trainingDataset,rang = 0.001, size=10,maxit=10000,decay=0.001)
predictions <- predict(ann,newdata=validationDataset, type="class")
confusionMatrix(predictions, validationDataset$Default_Payment_Next_Month)


#table(predictions, validationDataset$Default_Payment_Next_Month)
#table(factor(predictions, levels=min(0):max(1)),
 #     factor(validationDataset$Default_Payment_Next_Month, levels=min(0):max(1)))

# 6. OBSERVATIONS REDUCTION AND SCALING

# let's scale and center the AGE variable to make it more suitable to the sigmoid function of the neural network
library(caret)

trainingDataSetScaled=trainingDataset
trainingDataSetScaled[,5]=(trainingDataSetScaled[,5] - mean(trainingDataSetScaled[,5])) / sd(trainingDataSetScaled[,5])

validationDataSetScaled=validationDataset
validationDataSetScaled$Default_Payment_Next_Month=factor(validationDataSetScaled$Default_Payment_Next_Month)
validationDataSetScaled[,5]=
  (validationDataSetScaled[,5] - mean(validationDataSetScaled[,5])) / sd(validationDataSetScaled[,5])

set.seed(1)
ann = nnet(Default_Payment_Next_Month~SEX+EDUCATION+MARRIAGE+AGE+PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6, 
           data=trainingDataSetScaled,rang = 0.001, size=10,maxit=10000,decay=0.001)
predictions <- predict(ann,newdata=validationDataSetScaled, type="class")
confusionMatrix(predictions, validationDataSetScaled$Default_Payment_Next_Month)

# let's include LIMIT_BAL into prediction, but first we shall standardise its value
trainingDataSetScaled[,1]=(trainingDataSetScaled[,1] - mean(trainingDataSetScaled[,1])) / sd(trainingDataSetScaled[,1])
validationDataSetScaled[,1]=
  (validationDataSetScaled[,1] - mean(validationDataSetScaled[,1])) / sd(validationDataSetScaled[,1])


set.seed(1)
ann = nnet(Default_Payment_Next_Month~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6, 
           data=trainingDataSetScaled,rang = 0.001, size=10,maxit=10000,decay=0.001)
predictions <- predict(ann,newdata=validationDataSetScaled, type="class")
confusionMatrix(predictions, validationDataSetScaled$Default_Payment_Next_Month)


# let's now change the following values EDUCATION, MARRIAGE and PAY(0-6) to only fall within the expected categories
trainingDataSetScaledEdited=trainingDataSetScaled
validationDataSetScaledEdited=validationDataSetScaled

# EDUCATION: (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown) 
# we shall update all rows where the value is set to 0, 5 or 6 and overwrite it to be 4
trainingDataSetScaledEdited$EDUCATION[trainingDataSetScaledEdited$EDUCATION==0] <- 4
trainingDataSetScaledEdited$EDUCATION[trainingDataSetScaledEdited$EDUCATION==5] <- 4
trainingDataSetScaledEdited$EDUCATION[trainingDataSetScaledEdited$EDUCATION==6] <- 4

validationDataSetScaledEdited$EDUCATION[validationDataSetScaledEdited$EDUCATION==0] <- 4
validationDataSetScaledEdited$EDUCATION[validationDataSetScaledEdited$EDUCATION==5] <- 4
validationDataSetScaledEdited$EDUCATION[validationDataSetScaledEdited$EDUCATION==6] <- 4

# now check that EDUCATION only has the values 1,2,3,4
summary(trainingDataSetScaledEdited$EDUCATION)

#factor the values
trainingDataSetScaledEdited$EDUCATION=factor(trainingDataSetScaledEdited$EDUCATION)
validationDataSetScaledEdited$EDUCATION=factor(validationDataSetScaledEdited$EDUCATION)

# MARRIAGE: Marital status (1=married, 2=single, 3=others)
# we shall update all rows where the value is set to anything other than 1||2||3 and overwrite it to be 3
trainingDataSetScaledEdited$MARRIAGE[trainingDataSetScaledEdited$MARRIAGE==0] <- 3
validationDataSetScaledEdited$MARRIAGE[validationDataSetScaledEdited$MARRIAGE==0] <- 3

# now check that MARRIAGE only has the values 1,2,3
summary(trainingDataSetScaledEdited$MARRIAGE)

#factor the values
trainingDataSetScaledEdited$MARRIAGE=factor(trainingDataSetScaledEdited$MARRIAGE)
validationDataSetScaledEdited$MARRIAGE=factor(validationDataSetScaledEdited$MARRIAGE)

set.seed(1)
ann = nnet(Default_Payment_Next_Month~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6, 
           data=trainingDataSetScaledEdited,rang = 0.001, size=10,maxit=10000,decay=0.001)
predictions <- predict(ann,newdata=validationDataSetScaledEdited, type="class")
confusionMatrix(predictions, validationDataSetScaledEdited$Default_Payment_Next_Month)

#PAY -1=pay duly, 1=payment delay for one month, 2=payment delay for two months, ... 8=payment delay for eight months, 9=payment delay for nine months and above
# we shall update all rows where the value is 0 or -2 and set them to -1
trainingDataSetScaledEditedPay=trainingDataSetScaledEdited
validationDataSetScaledEditedPay=validationDataSetScaledEdited

trainingDataSetScaledEditedPay$PAY_0[trainingDataSetScaledEditedPay$PAY_0==-2] <- -1
trainingDataSetScaledEditedPay$PAY_0[trainingDataSetScaledEditedPay$PAY_0==0] <- -1
validationDataSetScaledEditedPay$PAY_0[validationDataSetScaledEditedPay$PAY_0==-2] <- -1
validationDataSetScaledEditedPay$PAY_0[validationDataSetScaledEditedPay$PAY_0==0] <- -1

trainingDataSetScaledEditedPay$PAY_0=factor(trainingDataSetScaledEditedPay$PAY_0)
validationDataSetScaledEditedPay$PAY_0=factor(validationDataSetScaledEditedPay$PAY_0)

summary(trainingDataSetScaledEditedPay$PAY_0)

trainingDataSetScaledEditedPay$PAY_2[trainingDataSetScaledEditedPay$PAY_2==-2] <- -1
trainingDataSetScaledEditedPay$PAY_2[trainingDataSetScaledEditedPay$PAY_2==0] <- -1
validationDataSetScaledEditedPay$PAY_2[validationDataSetScaledEditedPay$PAY_2==-2] <- -1
validationDataSetScaledEditedPay$PAY_2[validationDataSetScaledEditedPay$PAY_2==0] <- -1

trainingDataSetScaledEditedPay$PAY_2=factor(trainingDataSetScaledEditedPay$PAY_2)
validationDataSetScaledEditedPay$PAY_2=factor(validationDataSetScaledEditedPay$PAY_2)

trainingDataSetScaledEditedPay$PAY_3[trainingDataSetScaledEditedPay$PAY_3==-2] <- -1
trainingDataSetScaledEditedPay$PAY_3[trainingDataSetScaledEditedPay$PAY_3==0] <- -1
validationDataSetScaledEditedPay$PAY_3[validationDataSetScaledEditedPay$PAY_3==-2] <- -1
validationDataSetScaledEditedPay$PAY_3[validationDataSetScaledEditedPay$PAY_3==0] <- -1

trainingDataSetScaledEditedPay$PAY_3=factor(trainingDataSetScaledEditedPay$PAY_3)
validationDataSetScaledEditedPay$PAY_3=factor(validationDataSetScaledEditedPay$PAY_3)

trainingDataSetScaledEditedPay$PAY_4[trainingDataSetScaledEditedPay$PAY_4==-2] <- -1
trainingDataSetScaledEditedPay$PAY_4[trainingDataSetScaledEditedPay$PAY_4==0] <- -1
validationDataSetScaledEditedPay$PAY_4[validationDataSetScaledEditedPay$PAY_4==-2] <- -1
validationDataSetScaledEditedPay$PAY_4[validationDataSetScaledEditedPay$PAY_4==0] <- -1

trainingDataSetScaledEditedPay$PAY_4=factor(trainingDataSetScaledEditedPay$PAY_4)
validationDataSetScaledEditedPay$PAY_4=factor(validationDataSetScaledEditedPay$PAY_4)


trainingDataSetScaledEditedPay$PAY_5[trainingDataSetScaledEditedPay$PAY_5==-2] <- -1
trainingDataSetScaledEditedPay$PAY_5[trainingDataSetScaledEditedPay$PAY_5==0] <- -1
trainingDataSetScaledEditedPay$PAY_5[trainingDataSetScaledEditedPay$PAY_5==-2] <- -1
trainingDataSetScaledEditedPay$PAY_5[validationDataSetScaledEditedPay$PAY_5==8] <- 7
validationDataSetScaledEditedPay$PAY_5[validationDataSetScaledEditedPay$PAY_5==8] <- 7

trainingDataSetScaledEditedPay$PAY_5=factor(trainingDataSetScaledEditedPay$PAY_5)
validationDataSetScaledEditedPay$PAY_5=factor(validationDataSetScaledEditedPay$PAY_5)


trainingDataSetScaledEditedPay$PAY_6[trainingDataSetScaledEditedPay$PAY_6==-2] <- -1
trainingDataSetScaledEditedPay$PAY_6[trainingDataSetScaledEditedPay$PAY_6==0] <- -1
validationDataSetScaledEditedPay$PAY_6[validationDataSetScaledEditedPay$PAY_6==-2] <- -1
validationDataSetScaledEditedPay$PAY_6[validationDataSetScaledEditedPay$PAY_6==0] <- -1

trainingDataSetScaledEditedPay$PAY_6=factor(trainingDataSetScaledEditedPay$PAY_6)
validationDataSetScaledEditedPay$PAY_6=factor(validationDataSetScaledEditedPay$PAY_6)

set.seed(1)
ann = nnet(Default_Payment_Next_Month~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6, 
           data=trainingDataSetScaledEditedPay,rang = 0.001, size=10,maxit=10000,decay=0.001)
predictions <- predict(ann,newdata=validationDataSetScaledEditedPay, type="class")
confusionMatrix(predictions, validationDataSetScaledEditedPay$Default_Payment_Next_Month)

# 7. ADD NEW OBSERVATIONS BASED ON THE EXISTING ONES TO IMPROVE DATA QUALITY
# as an experiment, remove all rows where payments are made on time
defaultDatasetPayAllMinusOneId <- subset(defaultDataset, PAY_0==-1 & PAY_2==-1 & PAY_3==-1 & PAY_4==-1 & PAY_5==-1 & PAY_6==-1, select=ID)
defaultDatasetPayAllMinusOne <- subset(defaultDataset, PAY_0==-1 & PAY_2==-1 & PAY_3==-1 & PAY_4==-1 & PAY_5==-1 & PAY_6==-1)

# remove rows where PAY data is outside the ordinal data (-1, 1, 1+...)
defaultDatasetPayOutside <- subset(defaultDataset, PAY_0==0 | PAY_0==-2 | PAY_2==0 | PAY_2==-2 | PAY_3==0 | PAY_3==-2 |
                                     PAY_4==0 | PAY_4==-2 | PAY_5==0 | PAY_5==-2 |PAY_6==0 | PAY_6==-2 , select=ID)


# load sqldf package to use SQL like statements to manipulate data
library(sqldf)

defaultDatasetGood = sqldf('SELECT * FROM defaultDataset WHERE ID NOT IN (SELECT ID FROM defaultDatasetPayOutside)')

# For rows where there is at least one missed payment, add a penalty points column PAY_PENPOINTS
# so each missed month in each PAY column is worth a point. This is to put more weight on the rows with missed payments.
# if any PAY (0-6) is >=1 then PAY_PENPOINTS=SUM(PAY_0  + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6) . Otherwise, PAY_PENPOINTS=0
defaultDatasetGood$PAY_PENPOINTS <- 0
updateSQL='UPDATE defaultDatasetGood SET PAY_PENPOINTS=(CASE WHEN PAY_0 = -1 THEN 0 ELSE PAY_0 END) 
                                                      + (CASE WHEN PAY_2 = -1 THEN 0 ELSE PAY_2 END) 
                                                      + (CASE WHEN PAY_3 = -1 THEN 0 ELSE PAY_3 END) 
                                                      + (CASE WHEN PAY_4 = -1 THEN 0 ELSE PAY_4 END) 
                                                      + (CASE WHEN PAY_5 = -1 THEN 0 ELSE PAY_5 END) 
                                                      + (CASE WHEN PAY_6 = -1 THEN 0 ELSE PAY_6 END) 
                                        WHERE ID NOT IN (SELECT ID FROM defaultDatasetPayAllMinusOneId)'
defaultDatasetGood = sqldf(c(updateSQL,'SELECT * FROM defaultDatasetGood'))

# add a new column PAYAMT_ZERO
# if there are at least two columns with 0 for PAY_AMT (1-6), and the corresponding BILL_AMT are >0, then PAYAMT_ZERO = 1. Otherwise, set it to 0.
# BILL_AMT1 to BILL_AMT6 index 15 to 20, PAY_AMT1 to PAY_AMT6 index 21 to 26
# PAYAMT_ZERO index = 14, PAY_PENPOINTS index = 13

defaultDatasetGood$PAYAMT_ZERO <- 0

rows <- dim(defaultDatasetGood)[1]
for (row in 1:rows){
  count<-0
  #check the row is where all PAY (0-6) = -1
  if(defaultDatasetGood[row,13]==0){
    for (index in 15:20){
      billAmt <- defaultDatasetGood[row,index]
      payAmt <- defaultDatasetGood[row,index+6]
      if(billAmt>0 & payAmt==0) count <- count + 1
    }
    
    #set PAYAMT_ZERO to 1
    if(count>=2) defaultDatasetGood[row,14] <- 1
  }
}

# show number of records where PAYAMT_ZERO is set to 1
dim(defaultDatasetGood[ which(defaultDatasetGood$PAYAMT_ZERO==1), ])[1]

# reorder the columns
defaultDatasetGood <- defaultDatasetGood[c("ID", "LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE","AGE", 
                                                               "PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6","PAY_PENPOINTS","PAYAMT_ZERO",
                                                               "BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6",
                                                               "PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5", "PAY_AMT6",
                                                               "Default_Payment_Next_Month")]


# create two new columns to hold MEAN and SD values of all BILL_AMT columns
library(dplyr)
library(matrixStats)
defaultDatasetPayAllMinusOne$AVGBILL_AMT <- round(rowMeans(defaultDatasetPayAllMinusOne[,13:18]))
defaultDatasetPayAllMinusOne$SDBILL_AMT <- 
  round(rowSds(as.matrix(defaultDatasetPayAllMinusOne
                         [,c("BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6")])))

#defaultDatasetPayAllMinusOne$AVGPLUSSD_AMT=defaultDatasetPayAllMinusOne$AVGBILL_AMT + defaultDatasetPayAllMinusOne$SDBILL_AMT

# calculate the ratio between the last bill BILL_AMT6 and mean bill amount AVGBILL_AMT
defaultDatasetPayAllMinusOne$LAST_BILL_AVG_RATIO=round(defaultDatasetPayAllMinusOne$BILL_AMT6 / defaultDatasetPayAllMinusOne$AVGBILL_AMT,2)

# calculate the ratio between the standard deviation and mean of the all bill amounts
defaultDatasetPayAllMinusOne$SD_AVG_RATIO=round(defaultDatasetPayAllMinusOne$SDBILL_AMT / defaultDatasetPayAllMinusOne$AVGBILL_AMT,2)

# remove the rows where either LAST_BILL_AVG_RATIO or SD_AVG_RATIO is negative. It skews the AVG_BILLAMT and SD_BILLAMT and their ratios.
defaultDatasetPayAllMinusOne <- subset(defaultDatasetPayAllMinusOne, LAST_BILL_AVG_RATIO >= 0 & SD_AVG_RATIO >= 0)

#work out the LAST_AVG_BILL_RATIO mean and SD_AVG_RATIO mean
lastAvgBillRatioMean = round(mean(defaultDatasetPayAllMinusOne$LAST_BILL_AVG_RATIO),2)
sdAvgRatioMean = round(mean(defaultDatasetPayAllMinusOne$SD_AVG_RATIO),2)

# remove all rows where SD_BILLAMT = 0 as the bill and payment data for that consumer appears to be creating no sensible context 
# or association to the real-life behaviours, and as such these rows are treated as outliers.
countSDBILL_AMT_Zero= dim(defaultDatasetPayAllMinusOne[ which(defaultDatasetPayAllMinusOne$SDBILL_AMT==0), ])[1]
defaultDatasetPayAllMinusOne <- subset(defaultDatasetPayAllMinusOne, SDBILL_AMT > 0)


# reorder to put the new column after BIL_AMT6
defaultDatasetPayAllMinusOne <- defaultDatasetPayAllMinusOne[c("ID", "LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE","AGE", 
                                                               "PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6",
                                                               "BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6",
                                                               "AVGBILL_AMT","SDBILL_AMT",
                                                               "LAST_BILL_AVG_RATIO","SD_AVG_RATIO",
                                                               "PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5", "PAY_AMT6",
                                                               "Default_Payment_Next_Month")]

# defaultDatasetGood = sqldf('select * from defaultDataset where ID not in (select id from defaultDatasetGoodPay) 
#                            and ID not in (select id from defaultDatasetPayOutside)')

summary(defaultDatasetPayAllMinusOne)


       
# temptemp <- trainingDataSetScaledEdited[ which(trainingDataSetScaledEdited$EDUCATION==0), ]
# dim(temptemp)

#set.seed(7)
# x <- test[1:24000,4:5]
# preprocessParams <- preProcess(x, method=c("center", "scale"))

# summarize transform parameters 
#print(preprocessParams)
      
# transform the dataset using the parameters 
#transformed <- predict(preprocessParams, test[1:24000,4:5]) 

# summarize the transformed dataset
#summary(transformed)







