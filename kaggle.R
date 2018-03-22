# Load libraries
library(mlbench)
library(caret)
library(caretEnsemble)
library(DMwR)
library(Metrics)
library(plyr)
# Load the dataset
setwd("C:/Users/Zoey/Desktop")
train<-read.csv('train.csv',header=T, na.strings = c("NA", ""))
test<-read.csv('test.csv',header=T, na.strings = c("NA", ""))
#ÌØÕ÷¹¤³Ì
test$Survived<-NA
full<-rbind(train,test)
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)

sub("^a","",c("abcd","dcba"))

features <- c("Pclass",
                "Sex",
                "Age",
                "SibSp",
                "Parch",
                "Fare",
                "Embarked",
              'Survived')
fea<- all[,features]
sapply(fea, function(x) {sum(is.na(x))})
fea$Age[is.na(fea$Age)]= median(fea$Age,na.rm = T)
fea$Fare[is.na(fea$ Fare)]=mean(fea$Fare,na.rm=T)
sapply(fea, function(x) {sum(is.na(x))}) 
fea[is.na(fea)]<-0
fea$Sex = as.numeric(fea$Sex)
fea$Embarked = as.numeric(fea$Embarked)
fea$Survived<-factor(fea$Survived)
ntrain <- fea[1:891,]
ntest <- fea[892:1309,1:7]
TrainData<-ntrain[,1:7]
TrainClasses <- ntrain[,8]
str(TrainData) 
TrainData[] <- lapply(TrainData, function(x) as.numeric(as.character(x)))

# summarize results

set.seed(seed)
stackControl <- trainControl(method="repeatedcv", number=5, repeats=3, savePredictions=TRUE, classProbs=TRUE)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)
 predmlp <- predict(model,data2$inputsTest)
 confusionMatrix(data2$targetsTrain,fitted.values(model))
 preTablemlp<-confusionMatrix(data2$targetsTest,predmlp)
 (accuracy<-sum(diag(preTablemlp))/sum(preTablemlp))

 

 
 control <- trainControl(method="repeatedcv", number=5, repeats=3)
 metric <- "Accuracy"
 fittre <- caret::train(TrainData,TrainClasses, method="treebag", metric=metric, trControl=control)
 fitgbm <- caret::train(TrainData,TrainClasses,method='gbm', metric=metric,trControl=control)

 
 
 set.seed(seed)
 stackControl <- trainControl(method="repeatedcv", number=5, repeats=3, savePredictions=TRUE, classProbs=TRUE)
 stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
 print(stack.glm)
 
 
 
 
 # Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
 solution <- data.frame(PassengerID = test$PassengerId, Survived = pre)
 write.csv(solution, file = 'Solution.csv', row.names = F)
 
 pre<- predict( fitgbm ,ntest,type = "raw")
 