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

#Analysis shows that many, many values are missing.
#Throw out any column that is > 90% null.  Adapted from above
nullCounts <- sapply(raw, function(x)sum(is.na(x)))
raw <- raw[, nullCounts < (19622 * .9)]

#Git rid of any columns with near zero variance (in practice, one column -- "new window")
raw <- raw[, ! nearZeroVar(raw, saveMetrics = T)[,4]]

# Split into training and testing.  Never to look at training until the very end
set.seed(1976)
inTrain <- createDataPartition(y=raw$classe, p=.8, list=FALSE)
training <- raw[inTrain,]
testing <- raw[-inTrain,]


#PLAY -- flipped train and test for effeciency
modelFit <- train(testing$classe ~ ., method="rf", data=testing)
confusionMatrix(training$classe, predict(modelFit, training))
