library(caret)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(e1071)
library(rattle)


#We first read in the data, setting any missing data to "NA" so it can be processed.
testing <- read.csv("D:/Coursera/Repositories/Coursera_Machine_Learning_Project/pml-testing.csv", header = TRUE, sep = ',', na.strings = c("NA","#DIV/0!", ""))
training <- read.csv("D:/Coursera/Repositories/Coursera_Machine_Learning_Project/pml-training.csv", header = TRUE, sep = ',', na.strings = c("NA","#DIV/0!", ""))

#We drop columns that are completely empty
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

#We can drop the first seven columns are they are unnecessary for prediction
training <- training[, -c(1:7)]
testing <- testing[, -c(1:7)]

#We drop any columns that have near zero variance
nearZero <- nearZeroVar(training, saveMetrics = TRUE)
zero.Ind <- sum(nearZero$nzv)

if((zero.Ind > 0))
{
     training <- training[, nearZero$nzv == FALSE]
}

#We now slice the training set into training and testing subsets for cross-validation
inTrain <- createDataPartition(y = training$classe, p = 0.70, list = FALSE)

subTrain <- training[inTrain, ]
validation <- training[-inTrain, ]

#We now train our model using the subsample training set, utilizing k-fold cross-validation
controls <- trainControl(method = "cv", 5)

modFit <- train(classe ~ ., data = subTrain, method = "rf", trControl = controls)

#Predict on the training set
predTrain <- predict(modFit, subTrain)
confusionMatrix(predTrain, subTrain$classe)

#Predict on the validation set
predVal <- predict(modFit, validation)
predCM <- confusionMatrix(predVal, validation$classe)

#Predict on the test set and display the results
results <- predict(modFit, testing)

results

#Do a decision tree just for shits
modFit2 <- rpart(classe ~., data = subTrain, method = "class")
fancyRpartPlot(modFit2)


