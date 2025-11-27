rm(list=ls())
heart <- read.csv("C:/Users/kheey/Downloads/heart_disease_dataset.csv")
str(heart)
#split dataset
library(caTools)
set.seed(123)
spl <- sample.split(as.factor(heart$Heart.Disease),SplitRatio=0.7) # 70% training
train <- subset(heart,spl==TRUE);
test <- subset(heart,spl==FALSE);

#check balance (imbalanced)
sum(heart$Heart.Disease == 1)
sum(heart$Heart.Disease == 0)


#carts (decently high accuracy but analysis could be not so in depth)
library(rpart)
library(rpart.plot) 
library(caret)
cart1<-rpart(as.factor(Heart.Disease)~.,data=train)
prp(cart1)
p1pred<- predict(cart1,newdata=test,type="class")
conf_matcart1 <- confusionMatrix(as.factor(p1pred), as.factor(test$Heart.Disease), positive="1")
conf_matcart1$overall["Accuracy"] #0.83

printcp(cart1) #most no of split has highest xerror no need prune

set.seed(123)
cart2<-rpart(as.factor(Heart.Disease)~.,data=train,cp=0.001)
prp(cart2)
p2pred<- predict(cart2,newdata=test,type="class")
conf_matcart2 <- confusionMatrix(as.factor(p2pred), as.factor(test$Heart.Disease), positive="1")
conf_matcart2$overall["Accuracy"] #0.87

#randomforest base
library(randomForest)
set.seed(123)
rf1<-randomForest(as.factor(Heart.Disease)~. , data = train)
varImpPlot(rf1) #dominant 2 catergories cant these 2 alone as it wont be enough

library(caret)
predictforest1 <- predict(rf1,newdata=test,type="class")
conf_matrf1 <- confusionMatrix(as.factor(predictforest1), as.factor(test$Heart.Disease), positive="1")
conf_matrf1$overall["Accuracy"]#0.836


#randomforest tree diff
set.seed(123)
rf1500<-randomForest(as.factor(Heart.Disease)~. , data = train,ntree=1500)
predictforest1500 <- predict(rf1500,newdata=test,type="class")
conf_matrf1500 <- confusionMatrix(as.factor(predictforest1500), as.factor(test$Heart.Disease), positive="1")
conf_matrf1500$overall["Accuracy"]#0.843

set.seed(123)
rf300<-randomForest(as.factor(Heart.Disease)~. , data = train,ntree=300)
predictforest300 <- predict(rf300,newdata=test,type="class")
conf_matrf300 <- confusionMatrix(as.factor(predictforest300), as.factor(test$Heart.Disease), positive="1")
conf_matrf300$overall["Accuracy"]#0.84

#no top 2
set.seed(123)
rfnotop2<-randomForest(as.factor(Heart.Disease)~.-Cholesterol-Age.Range , data = train)
predictforestnotop2 <- predict(rfnotop2,newdata=test,type="class")
conf_matnotop2 <- confusionMatrix(as.factor(predictforestnotop2), as.factor(test$Heart.Disease), positive="1")
conf_matnotop2 $overall["Accuracy"]#0.593


#xgboost
library(xgboost) 
library(caret)

train_x <- data.matrix(train[, -16])#heart.disease
train_y <- train[,16]#col 16 heart.disease
test_x <- data.matrix(test[, -16]) 
test_y <- test[, 16]
xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test <- xgb.DMatrix(data = test_x, label = test_y)
watchlist <- list(train = xgb_train, eval = xgb_test)

#finding best no of rounds
params <- list(
  objective = "binary:hinge",
  eval_metric = "auc"
)

set.seed(123)
xgb_model <- xgb.train(
  params = params,
  data = xgb_train,
  watchlist=watchlist,
  nrounds = 500,                # Upper limit
  nfold = 5,                     # 5-fold CV
  early_stopping_rounds = 10,    # Stop if no improvement for 10 rounds
  verbose = 0
)
best_nrounds <- cv_model$best_iteration
cat("Optimal nrounds:", best_nrounds, "\n")

#xgboost noweight
set.seed(123)
xgb_model <- xgb.train(
  params = params,
  data = xgb_train,
  nrounds = 2,
  nfold = 5,                  
  verbose = 0
)
y_pred_prob <- predict(xgb_model, xgb_test)
conf_matxg <- confusionMatrix(as.factor(y_pred_prob), as.factor(test$Heart.Disease), positive="1")
conf_matxg$overall["Accuracy"] #0.843

#xgboost weight
neg<-sum(heart$Heart.Disease == 1)
pos<-sum(heart$Heart.Disease == 0)
weight=neg/pos
params <- list(
  objective = "binary:logisitc",
  eval_metric = "auc",
  scale_pos_weight=weight
)
xgbw_model <- xgb.train(
  data = xgb_train,
  nrounds = 2,
  verbose = 0,
  objective = "binary:logistic"
)
y_pred_weight <- predict(xgbw_model, xgb_test)
y_pred_class <- ifelse(y_pred_weight > 0.5, 1, 0)
conf_matxgw <- confusionMatrix(as.factor(y_pred_class), as.factor(test$Heart.Disease), positive="1")
conf_matxgw$overall["Accuracy"] #0.83333


#catboost
str(heart)
library(catboost)
train$Age.Range <- as.factor(train$Age.Range)
train$Gender <- as.factor(train$Gender)
train$Smoking  <- as.factor(train$Smoking)
train$Alcohol.Intake<- as.factor(train$Alcohol.Intake)
train$Family.History<- as.factor(train$Family.History)
train$Diabetes<- as.factor(train$Diabetes)
train$Obesity<- as.factor(train$Obesity)
train$Exercise.Induced.Angina<- as.factor(train$Exercise.Induced.Angina)
train$Chest.Pain.Type<- as.factor(train$Chest.Pain.Type)

test$Age.Range <- as.factor(test$Age.Range)
test$Gender <- as.factor(test$Gender)
test$Smoking  <- as.factor(test$Smoking)
test$Alcohol.Intake<- as.factor(test$Alcohol.Intake)
test$Family.History<- as.factor(test$Family.History)
test$Diabetes<- as.factor(test$Diabetes)
test$Obesity<- as.factor(test$Obesity)
test$Exercise.Induced.Angina<- as.factor(test$Exercise.Induced.Angina)
test$Chest.Pain.Type<- as.factor(test$Chest.Pain.Type)

train_pool <- catboost.load_pool(data = train[, -16], label = train$Heart.Disease)
test_pool <- catboost.load_pool(data = test[, -16], label = test$Heart.Disease)
params <- list( loss_function = "Logloss", iterations =500,depth = 4,learning_rate = 0.1)
cat_model <- catboost.train(train_pool, params = params)

library(caret)
catpred <- catboost.predict(cat_model, test_pool, prediction_type = "Class")
conf_mat_cat<- confusionMatrix(as.factor(catpred), as.factor(test$Heart.Disease), positive="1")
conf_mat_cat$overall["Accuracy"] #0.833


#use this , feel cart too simnple to capture improtant info
library(randomForest)
set.seed(123)
rf1500<-randomForest(as.factor(Heart.Disease)~. , data = train,ntree=1500)
predictforest1500 <- predict(rf1500,newdata=test,type="class")
conf_matrf1500 <- confusionMatrix(as.factor(predictforest1500), as.factor(test$Heart.Disease), positive="1")
conf_matrf1500$overall["Accuracy"]#0.843
saveRDS(rf1500, "rf_model_heart.rds")




