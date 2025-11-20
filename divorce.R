#prep
rm(list=ls())
divorce <- read.csv("C:/Users/kheey/OneDrive/Desktop/analytics/r/divorce2.csv")

#check if dataset balance or not dont use accuracy can use f1 score if imbalance
head(divorce)
sum(divorce$divorced == 1)
sum(divorce$divorced == 0)


#spliting train and test
library(caTools)
set.seed(123)
spl <- sample.split(as.factor(divorce$divorced),SplitRatio=0.7) # 70% training
train <- subset(divorce,spl==TRUE);
test <- subset(divorce,spl==FALSE);

#check correct splitting
nrow(divorce)
nrow(train)
nrow(test)


#after testing , low f1 score throughout may suggest the need of threshold altering to make up for the
#lack of divorce outcomes in actual


#randomforest
library(randomForest)
set.seed(123)
rf1<-randomForest(as.factor(divorced)~. , data = train)


#importance 
importance_df <- as.data.frame(importance(rf1))
importance_df$Variable <- rownames(importance_df)
importance_sorted <- importance_df[order(-importance_df$MeanDecreaseGini), ]
rownames(importance_sorted) <- NULL   # remove row names
importance_sorted
varImpPlot(rf1)


#prediction of base rf
library(caret)
predictforest1 <- predict(rf1,newdata=test,type="class")
conf_mat <- confusionMatrix(as.factor(predictforest1), as.factor(test$divorced), positive="1")
conf_mat$overall["Accuracy"]#0.605


#try to improve rf (take values above 100 importance score)
set.seed(123)
rf2<-randomForest(as.factor(divorced)~trust_score+ combined_income+
                    communication_score+financial_stress_level+ social_support+
                    age_at_marriage+marriage_duration_years
                    , data = train)

predictforest2 <- predict(rf2,newdata=test,type="class")
conf_mat2 <- confusionMatrix(as.factor(predictforest2), as.factor(test$divorced), positive="1")
conf_mat2$overall["Accuracy"]#0.585

#try to improve rf (take above 160)
set.seed(123)
rf3<-randomForest(as.factor(divorced)~trust_score+ combined_income+
                    communication_score+financial_stress_level+ social_support, data = train)

predictforest3 <- predict(rf3,newdata=test,type="class")
conf_mat3 <- confusionMatrix(as.factor(predictforest3), as.factor(test$divorced), positive="1")
conf_mat3$overall["Accuracy"]#0.571

#lets see if we no of trees affect --> use base
library(randomForest)
set.seed(123)
rf_300<-randomForest(as.factor(divorced)~.,ntree=300, data = train)
predictforest300 <- predict(rf_300,newdata=test,type="class")
conf_mat300 <- confusionMatrix(as.factor(predictforest300), as.factor(test$divorced), positive="1")
conf_mat300$overall["Accuracy"]#0.604

set.seed(123)
rf_1500<-randomForest(as.factor(divorced)~.,ntree=1500, data = train)
predictforest1500 <- predict(rf_1500,newdata=test,type="class")
conf_mat1500 <- confusionMatrix(as.factor(predictforest1500), as.factor(test$divorced), positive="1")
conf_mat1500$overall["Accuracy"]#0.606



#xgboost (slightly better than rf)
library(xgboost) 
library(caret)

train_x <- data.matrix(train[, -22])#all predictors except col 22 ie divorced
train_y <- train[,22]#col 22 divorced
test_x <- data.matrix(test[, -22]) 
test_y <- test[, 22]
xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test <- xgb.DMatrix(data = test_x, label = test_y)

#no weights xgb
params <- list(
  objective = "binary:hinge",
  eval_metric = "auc"
)

xgb_model <- xgb.train(
  params = params,
  data =xgb_train,
  nrounds =14
)
y_pred_prob <- predict(xgb_model, xgb_test)
conf_matxg <- confusionMatrix(as.factor(y_pred_prob), as.factor(test$divorced), positive="1")
conf_matxg$overall["Accuracy"] #0.591


#weights xgb
neg <- sum(train$divorced == 0)
pos <- sum(train$divorced == 1)
scale_pos_weight <- neg / pos

params <- list(
  objective = "binary:logisitc",
  eval_metric = "auc",
  scale_pos_weight=scale_pos_weight
)

xgbw_model <- xgb.train(
  data = xgb_train,
  nrounds = 14,
  verbose = 0,
  objective = "binary:logistic"
)
y_pred_prob <- predict(xgbw_model, xgb_test)
y_pred_class <- ifelse(y_pred_prob > 0.5, 1, 0)#for class use default 0.5 threshold
conf_matxgw <- confusionMatrix(as.factor(y_pred_class), as.factor(test$divorced), positive="1")
conf_matxgw$overall["Accuracy"] #0.606


#neural network xxxxxx (not viable)
library(neuralnet)
library(tidyverse)
train_neural <- as.data.frame(model.matrix(divorced ~ .-1, data=train))
test_neural  <- as.data.frame(model.matrix(divorced ~ .-1, data=test))
train_neural$divorced <- train$divorced
test_neural$divorced  <- test$divorced
#get rid os spaces
colnames(train_neural)[colnames(train_neural) == 'education_levelNo Formal Education'] <- 'education_levelNoFormalEducation'
colnames(train_neural)[colnames(train_neural) == 'education_levelHigh School'] <- 'education_levelHighSchool'
colnames(train_neural)[colnames(train_neural) == 'employment_statusPart-time'] <- 'employment_statusParttime'
colnames(train_neural)[colnames(train_neural) == 'religious_compatibilityNot Religious'] <- 'religious_compatibilityNotReligious'
colnames(train_neural)[colnames(train_neural) == 'religious_compatibilitySame Religion'] <- 'religious_compatibilitySameReligion'
colnames(test_neural)[colnames(test_neural) == 'education_levelNo Formal Education'] <- 'education_levelNoFormalEducation'
colnames(test_neural)[colnames(test_neural) == 'education_levelHigh School'] <- 'education_levelHighSchool'
colnames(test_neural)[colnames(test_neural) == 'employment_statusPart-time'] <- 'employment_statusParttime'
colnames(test_neural)[colnames(test_neural) == 'religious_compatibilityNot Religious'] <- 'religious_compatibilityNotReligious'
colnames(test_neural)[colnames(test_neural) == 'religious_compatibilitySame Religion'] <- 'religious_compatibilitySameReligion'
#model full
modelneural <- neuralnet(divorced~.,data = train_neural, hidden = c(4, 2), linear.output = FALSE)
predneural <- predict(modelneural,test_neural)
y_predneural <- ifelse(predneural > 0.5, 1, 0)
conf_matneural<- confusionMatrix(as.factor(y_predneural), as.factor(test_neural$divorced), positive="1")
conf_matneural$overall["Accuracy"] #0.602

#importance rank 
rfneural<-randomForest(as.factor(divorced)~. , data = train_neural)
importance_df <- as.data.frame(importance(rfneural))
importance_df$Variable <- rownames(importance_df)
importance_sorted <- importance_df[order(-importance_df$MeanDecreaseGini), ]
rownames(importance_sorted) <- NULL   # remove row names
importance_sorted

#model above 100 (no diff from og)
modelneural2 <- neuralnet(divorced~social_support+financial_stress_level+trust_score+age_at_marriage+
                           marriage_duration_years+communication_score+combined_income,data = train_neural, hidden = c(4, 2), linear.output = FALSE)
predneural2 <- predict(modelneural2,test_neural)
y_predneural2 <- ifelse(predneural2 > 0.5, 1, 0)
conf_matneural2<- confusionMatrix(as.factor(y_predneural2), as.factor(test_neural$divorced), positive="1")
conf_matneural2$overall["Accuracy"]#0.602

#model above 160 (no diff from og)
modelneural3 <- neuralnet(divorced~financial_stress_level+trust_score+communication_score+combined_income,data = train_neural, hidden = c(4, 2), linear.output = FALSE)
predneural3 <- predict(modelneural3,test_neural)
y_predneural3 <- ifelse(predneural3 > 0.5, 1, 0)
conf_matneural3<- confusionMatrix(as.factor(y_predneural3), as.factor(test_neural$divorced), positive="1")
conf_matneural3$overall["Accuracy"] #0.602


#catboost
library(catboost)

#manual conversion
train$education_level <- as.factor(train$education_level)
train$employment_status <- as.factor(train$employment_status)
train$religious_compatibility  <- as.factor(train$religious_compatibility)
train$conflict_resolution_style <- as.factor(train$conflict_resolution_style)
train$marriage_type <- as.factor(train$marriage_type)

test$education_level <- as.factor(test$education_level)
test$employment_status <- as.factor(test$employment_status)
test$religious_compatibility  <- as.factor(test$religious_compatibility)
test$conflict_resolution_style <- as.factor(test$conflict_resolution_style)
test$marriage_type <- as.factor(test$marriage_type)


train_pool <- catboost.load_pool(data = train[, -22], label = train$divorced)
test_pool <- catboost.load_pool(data = test[, -22], label = test$divorced)
params <- list( loss_function = "Logloss", iterations =500,depth = 5,learning_rate = 0.1)
cat_model <- catboost.train(train_pool, params = params)

library(caret)
catpred <- catboost.predict(cat_model, test_pool, prediction_type = "Class")
conf_mat_cat<- confusionMatrix(as.factor(catpred), as.factor(test$divorced), positive="1")
conf_mat_cat$overall["Accuracy"] #0.586

#selected
set.seed(123)
rf_1500<-randomForest(as.factor(divorced)~.,ntree=1500, data = train)
predictforest1500 <- predict(rf_1500,newdata=test,type="class")
conf_mat1500 <- confusionMatrix(as.factor(predictforest1500), as.factor(test$divorced), positive="1")
conf_mat1500$overall["Accuracy"]#0.606
saveRDS(rf_1500, "rf_model.rds")


