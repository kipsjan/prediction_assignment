library(caret)
training_raw<-read.csv("pml-training.csv")
testing_raw<-read.csv("pml-testing.csv")
#training_raw$classe<-as.factor(training_raw$classe)
dim(training_raw)
#remove parameters that are irrelevant for predicting the classe
trainRemove <- grepl("^user|^X|timestamp|window", names(training_raw))
training1<-training_raw[,!trainRemove]

#restrict to the complete cases
training2<-training1[complete.cases(training_raw),]

# remove rows with missing values
training2<- training1[, colSums(is.na(training1)) == 0]
#testing <- testing1[, colSums(is.na(testing)) == 0]

# Removing near-zero covariates
near_zero<-nearZeroVar(training2, saveMetrics=TRUE)
training3<-training2[,!near_zero[4]]
training_cleaned<-training3
dim(training_cleaned)

inTrain <- createDataPartition(training_cleaned$classe, p=0.05, list=F)
trainData<-training_cleaned[inTrain,]
valData<-training_cleaned[-inTrain,]

mod1<-train(classe~.,data=trainData,method="rf",allowParallel = TRUE)


set.seed(12345)
gbm_control <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
gbm_mod  <- train(classe ~ ., data=trainData, method = "gbm", trControl = gbm_control, verbose = FALSE)

gbm_predict <- predict(gbm_mod, newdata=test_data)
gbm_cm <- confusionMatrix(factor(gbm_predict), factor(test_data$classe))
gbm_cm