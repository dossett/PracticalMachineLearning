library(caret)
library(ggplot2)

#Grab the data and do initial cleanups
raw <- read.csv("pml-training.csv", header = TRUE, na.string=c("NA", "#DIV/0!"))

#first column is just a sequence, not needed
#second coumn is the person being measured, also not needed
raw$X <- NULL
raw$user_name <- NULL

#disregard timestamps
raw <- raw[, ! grepl("timestamp", names(raw))]

#throw out any columns that are completely missing
#source for this nice code: http://stackoverflow.com/a/11330265
raw <- raw[, ! sapply(raw, function(x)all(is.na(x)))]

#Analysis shows that many, many columns are almost completely missing.
#Throw out any column that is > 90% null.  Adapted from above
nullCounts <- sapply(raw, function(x)sum(is.na(x)))
raw <- raw[, nullCounts < (19622 * .9)]

#Git rid of any columns with near zero variance (in practice, one column -- "new window")
raw <- raw[, ! nearZeroVar(raw, saveMetrics = T)[,4]]

# Split into training, testing, and validation.  Never to look at testing until the very end!
# 70% training, 15% validation, and 15% testing
set.seed(1976)
inTrain <- createDataPartition(y=raw$classe, p=.7, list=FALSE)
training <- raw[inTrain,]
testAndVal <- raw[-inTrain,]
inVal <- createDataPartition(y=testAndVal$classe, p=.5, list=FALSE)
validation <- testAndVal[inVal,]
testing <- testAndVal[-inVal,]


#Look at three models -- treebag, random forest, and nb
treeFit <- train(training$classe ~ ., method="rpart", data=training)
randomForestFit <- train(training$classe ~ ., method="rf", data=training)
naiveBayesFit <- train(training$classe ~ ., method="nb", data=training)

confusionMatrix(validation$classe, predict(treeFit, validation))$overall[c(1,3,4,6)]
