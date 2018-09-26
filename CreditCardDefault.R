# Credit card default

# binary classification, categorical and numerical attributes

# description: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients
# dataset folder https://archive.ics.uci.edu/ml/machine-learning-databases/00350/

# load libraries
library(caret)
library(corrplot)

# set the working directory
setwd("C:\\Users\\Amir\\Documents\\Coursework\\CMM535DataScience\\RSolution")

# 1. LOAD DATA

# define the filename which is a csv file obtained by saving 
# the original dataset Excel spreadsheet as a csv file into the working directory
filename <- "CreditCardDefaultDataset.csv" 

# load the CSV file from the local directory 
dataset <- read.csv(filename, header=TRUE) 

# set the column names in the dataset 
#colnames(dataset) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

# split out validation dataset
# create a list of 80% of the rows in the original dataset we can use for training
set.seed(7)
validationIndex <- createDataPartition(dataset$Default_Payment_Next_Month, p=0.80, list=FALSE)

# select 20% of the data for validation
validationDataset <- dataset[-validationIndex,]

# use the remaining 80% of data to training and testing the models
trainingDataset <- dataset[validationIndex,]


# 2. DATA ANALYSIS

# dimensions of dataset
dim(dataset)

# peek
head(dataset, n=20)

# types
sapply(dataset, class)

# remove the first column holding the redundant variable Id
dataset <- dataset[,-1]

# summary 
summary(dataset)

# summary shows us that the mean Default_Payment_Next_Month value is 0.2212 suggesting an unbalanced 
# predicted output or class, which we can further investigate by looking into class distribution

# class distribution
cbind(freq=table(dataset$Default_Payment_Next_Month), 
      percentage=prop.table(table(dataset$Default_Payment_Next_Month))*100)

# 77% of data is classed as a non-default and 23% as a default, which is fairly unbalanced
# given that we are predicting if the client will default ?? INVESTIGATE balancing

# list rows of data that have missing values 
dataset[!complete.cases(dataset),]

# there is no missing values
# now summarize correlations between input variables using the default method pearson
cor.pearson <- cor(dataset[,1:24])

# Find out if there is any correlation equalling or exceeding 700% as set in corMaxLevel variable
# as some algorithms may benefit from removing the highly correlated observations

isDuplicate <- function(obs1ToCheck, obs2ToCheck, corMatrix, matrixRow) {
  if(matrixRow>0){
    for (row in 1:matrixRow){
      obs1=corMatrix[row,2]
      obs2=corMatrix[row,3]
      if(obs1ToCheck==obs2 & obs2ToCheck==obs1){
        return (TRUE)
      }
    }
  }
  return (duplicate)
}

getCorrelations <- function(corPearson, corLevelToCheck) {
  cor.dim <- dim(corPearson)
  rows <- cor.dim[1]
  columns <- cor.dim[2]
  
  corMaxLevel <- corLevelToCheck # suggesting there is a strong positive linear relationship
  corMinLevel <- corLevelToCheck * -1 # suggesting there is strong negative linear relationship
  minCor <- 1
  maxCor <- -1
  
  # create and preallocate correlation matrix to make it efficient
  # then efficiently truncate the matrix once the process is complete
  corMatrix <- matrix(nrow = rows * columns, ncol = 3)
  matrixRow <- 0
  
  for (row in 1:rows){
    for (column in 1:columns){
      if(row!=column){
        tempCor=corPearson[row,column]
        
        if(tempCor>=corMaxLevel | tempCor<=corMinLevel) {
          if(!isDuplicate(colnames(dataset)[row], colnames(dataset)[column], corMatrix, matrixRow)){
            matrixRow <- matrixRow + 1
            corMatrix[matrixRow,1]=tempCor
            corMatrix[matrixRow,2]=colnames(dataset)[row]
            corMatrix[matrixRow,3]=colnames(dataset)[column]
          }
        }
        
        if(tempCor<minCor){
          minCor=tempCor
        }
        else if(tempCor>maxCor){
          maxCor=tempCor
        }
      }
    }
  }
  
  corMatrix <- corMatrix[1:matrixRow,]
  
  print(paste("Min correlation : ", minCor))
  print(paste("Max correlation : ", maxCor))
  print(paste("Correlation matrix equalling or exceeding ", corLevelToCheck))
  corMatrix
}

# printed corMatrix shows the strong correlation between PAY observations amongst themselves, 
# and also between BILL_AMT observations themselves. Given that the correlation exists amongst the same type of the observations,
# but in a different time line, removing any of the observations would not make sense
getCorrelations(cor.pearson, 0.70)

# but we have noticed that PAY observations correlate to the output so let's find the exact intensity of the correlation
getCorrelations(cor.pearson, 0.15)

# 3. UNIVARIATE VISUALISATION

# histograms each attribute
par(mfrow=c(3,3))
for(i in 1:23) {
  hist(dataset[,i], main=names(dataset)[i])
}

# density plot for each attribute
par(mfrow=c(3,3))
for(i in 1:23) {
  plot(density(dataset[,i]), main=names(dataset)[i])
}

# boxplots for each attribute
par(mfrow=c(3,3))
for(i in 1:23) {
  boxplot(dataset[,i], main=names(dataset)[i])
}


# 4. MULTIVARIATE VISUALISATION

# scatterplot matrix
# because the data is discrete (integer values) we need to add some jitter to make the scatter plots useful, 
# otherwise the dots will all be on top of each other.
jittered_x <- sapply(dataset[,1:23], jitter)
pairs(jittered_x, names(dataset[,1:23]), col=dataset$Default_Payment_Next_Month)

# bar plots of each variable by class
par(mfrow=c(1,1))
for(i in 1:23) {
  barplot(table(dataset$Default_Payment_Next_Month,dataset[,i]),
          main=names(dataset)[i], legend.text=unique(dataset$Default_Payment_Next_Month))
}

# correlation plot to create a graphical representation to complement 
# correlation matrix corMatrix created earlier on
correlations <- cor(dataset[,1:23])
corrplot(correlations, method="circle")

# let's use an alternative method crosstabs to identify strong predictors. this should fortify our earlier finding
# that the PAY observations are the strongest predictors
table(dataset[,c('Default_Payment_Next_Month','EDUCATION')])


# 5. EVALUATE ALGORITHMS

# change the class variable from the integer to a factor with levels [0,1] 
# to enable the training for the binary classifiers
trainingDataset$Default_Payment_Next_Month=factor(trainingDataset$Default_Payment_Next_Month)

# 10-fold cross validation with 3 repeats
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

# LG Logistic Regression
set.seed(7)
startTime <- proc.time()
fit.glm <- train(Default_Payment_Next_Month~., data=trainingDataset, method="glm", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# LDA Linear Discriminate Analysis
set.seed(7)
startTime <- proc.time()
fit.lda <- train(Default_Payment_Next_Month~., data=trainingDataset, method="lda", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# GLMNET Regularized Logistic Regression
set.seed(7)
startTime <- proc.time()
fit.glmnet <- train(Default_Payment_Next_Month~., data=trainingDataset, method="glmnet", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# KNN k-Nearest Neighbours
set.seed(7)
startTime <- proc.time()
fit.knn <- train(Default_Payment_Next_Month~., data=trainingDataset, method="knn", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# CART Classification and Regression Trees
set.seed(7)
startTime <- proc.time()
fit.cart <- train(Default_Payment_Next_Month~., data=trainingDataset, method="rpart", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# NB Naive Bayes
set.seed(7)
startTime <- proc.time()
fit.nb <- train(Default_Payment_Next_Month~., data=trainingDataset, method="nb", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# SVM Support Vector Machines with Radial Basis Functions. Takes the longest to train
set.seed(7)
startTime <- proc.time()
fit.svm <- train(Default_Payment_Next_Month~., data=trainingDataset, method="svmRadial", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# Compare algorithms
results <- resamples(list(LG=fit.glm, LDA=fit.lda, GLMNET=fit.glmnet, KNN=fit.knn, CART=fit.cart, NB=fit.nb, SVM=fit.svm))
summary(results)
dotplot(results)

# 6. TRANSFORM

# To increaes accuracy, given lots of skewed distributions, we can apply transform to adjust and normalize these distributions.
# Box-Cox transformation method will be applied to all training algorithms.

# 10-fold cross validation with 3 repeats
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

# LG Logistic Regression
set.seed(7)
startTime <- proc.time()
fit.glm <- train(Default_Payment_Next_Month~., data=trainingDataset, method="glm", metric=metric, preProc=c("BoxCox"), trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# LDA Linear Discriminate Analysis
set.seed(7)
startTime <- proc.time()
fit.lda <- train(Default_Payment_Next_Month~., data=trainingDataset, method="lda", metric=metric, preProc=c("BoxCox"), trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# GLMNET Regularized Logistic Regression
set.seed(7)
startTime <- proc.time()
fit.glmnet <- train(Default_Payment_Next_Month~., data=trainingDataset, method="glmnet", metric=metric, preProc=c("BoxCox"), trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# KNN k-Nearest Neighbours
set.seed(7)
startTime <- proc.time()
fit.knn <- train(Default_Payment_Next_Month~., data=trainingDataset, method="knn", metric=metric, preProc=c("BoxCox"), trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# CART Classification and Regression Trees
set.seed(7)
startTime <- proc.time()
fit.cart <- train(Default_Payment_Next_Month~., data=trainingDataset, method="rpart", metric=metric, preProc=c("BoxCox"), trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# NB Naive Bayes
set.seed(7)
startTime <- proc.time()
fit.nb <- train(Default_Payment_Next_Month~., data=trainingDataset, method="nb", metric=metric, preProc=c("BoxCox"), trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# SVM Support Vector Machines with Radial Basis Functions. Takes the longest to train
set.seed(7)
startTime <- proc.time()
fit.svm <- train(Default_Payment_Next_Month~., data=trainingDataset, method="svmRadial", metric=metric, preProc=c("BoxCox"), trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# Compare algorithms
transform_results <- resamples(list(LG=fit.glm, LDA=fit.lda, GLMNET=fit.glmnet, KNN=fit.knn, CART=fit.cart, NB=fit.nb, SVM=fit.svm))
summary(transform_results)
dotplot(transform_results)


# 7. ENSEMBLES: Boosting and Bagging

# 10-fold cross validation with 3 repeats
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

# Bagged CART
set.seed(7)
startTime <- proc.time()
fit.treebag <- train(Default_Payment_Next_Month~., data=trainingDataset, method="treebag", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# Random Forest
set.seed(7)
startTime <- proc.time()
fit.rf <- train(Default_Payment_Next_Month~., data=trainingDataset, method="rf", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# Stochastic Gradient Boosting
set.seed(7)
startTime <- proc.time()
fit.gbm <- train(Default_Payment_Next_Month~., data=trainingDataset, method="gbm", metric=metric, trControl=control, verbose=FALSE, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# C5.0
set.seed(7)
startTime <- proc.time()
fit.c50 <- train(Default_Payment_Next_Month~., data=trainingDataset, method="C5.0", metric=metric, trControl=control, na.action=na.omit)
print(paste("Time to train algorithm in seconds: ", (proc.time() - startTime)[3]))

# Compare results
ensemble_results <- resamples(list(BAG=fit.treebag, RF=fit.rf, GBM=fit.gbm, C50=fit.c50))
summary(ensemble_results)
dotplot(ensemble_results)


# 8. PREDICT
# prepare validation data set by removing the ID column 
# and transforming the class variable into a 2 level factor of 0 and 1
validationDataset <- validationDataset[,-1]
validationDataset$Default_Payment_Next_Month=factor(validationDataset$Default_Payment_Next_Month)

# to predict using a SVM model, replace fit.c50 in the exmaple below with fit.svm and so on
set.seed(7)
predictions <- predict(fit.c50, newdata=validationDataset)
confusionMatrix(predictions, validationDataset$Default_Payment_Next_Month)


