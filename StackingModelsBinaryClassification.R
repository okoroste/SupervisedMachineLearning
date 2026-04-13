########################
# PERFORMANCE METRICS  #
########################
performance.measures <- function(conf.mat) {
  
  accuracy <- sum(diag(conf.mat)) / sum(conf.mat)
  
  TP <- conf.mat["Yes", "Yes"]
  FP <- conf.mat["Yes", "No"]
  TN <- conf.mat["No", "No"]
  FN <- conf.mat["No", "Yes"]
  
  sensitivity<- ifelse((TP+FN)==0, NA, TP/(TP+FN))
  specificity<- ifelse((TN+FP)==0, NA, TN/(TN+FP))
  precision<- ifelse((TP+FP)==0, NA, TP/(TP+FP))
  
  F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|(precision 
  +sensitivity)==0, NA, 2*precision*sensitivity/(precision+sensitivity))
  
  list(accuracy=accuracy, sensitivity=sensitivity, specificity=specificity,
    precision=precision, F1_score=F1_score)
}

get_metrics<- function(predicted, actual) {
  cm<- table(Predicted=predicted, Actual=actual)
  performance.measures(cm)
}

library(caret)
library(randomForest)
library(xgboost)
library(e1071)
library(neuralnet)
library(ROCR)

pneumonia.data<- read.csv("C:/Users/000110888/OneDrive - CSULB/Desktop/pneumonia_data.csv",
header=TRUE, sep=",")

#################
# PREPROCESSING #
#################

pneumonia.data$pneumonia<- ifelse(pneumonia.data$pneumonia=="yes", "Yes", "No")
pneumonia.data$pneumonia<- factor(pneumonia.data$pneumonia, levels=c("No", "Yes"))

pneumonia.data$gender<- ifelse(pneumonia.data$gender=="M", 1, 0)
pneumonia.data$tobacco_use<- ifelse(pneumonia.data$tobacco_use=="yes", 1, 0)

########################################
# SPLITTING INTO TRAINING/TESTING SETS #
########################################
set.seed(447033)
idx<- createDataPartition(pneumonia.data$pneumonia, p=0.8, list=FALSE)

train<- pneumonia.data[idx,]
test<- pneumonia.data[-idx,]

#############################################################
# MIN-MAX SCALING OF ALL PREDICTORS USING TRAINING SET ONLY #
#############################################################
min_max_scale<- function(x, min_val, max_val) {
  (x-min_val)/(max_val-min_val)
}

age_min<- min(train$age)
age_max<- max(train$age)
train$age<- min_max_scale(train$age, age_min, age_max)
test$age<- min_max_scale(test$age, age_min, age_max)

pm_min<- min(train$PM2_5)
pm_max<- max(train$PM2_5)
train$PM2_5<- min_max_scale(train$PM2_5, pm_min, pm_max)
test$PM2_5<- min_max_scale(test$PM2_5, pm_min, pm_max)

#preparing target variable for different models
train.y.factor<- train$pneumonia
test.y.factor<- test$pneumonia

# numeric outcome for xgboost, neuralnet, and meta-model
train.y.num<- ifelse(train$pneumonia=="Yes", 1, 0)
test.y.num<- ifelse(test$pneumonia=="Yes", 1, 0)

train.x<- data.matrix(subset(train, select=-pneumonia))
test.x<- data.matrix(subset(test, select=-pneumonia))

###################################
# MAKING OUT-OF-FOLD PREDICTIONS  #
###################################
set.seed(559702)
folds<- createFolds(train.y.factor, k=5)

oof.rf<- rep(NA, nrow(train))
oof.xgb<- rep(NA, nrow(train))
oof.svm.linear<- rep(NA, nrow(train))
oof.svm.radial<- rep(NA, nrow(train))
oof.knn<- rep(NA, nrow(train))
oof.nb<- rep(NA, nrow(train))
oof.ann<- rep(NA, nrow(train))

for (i in seq_along(folds)) {
  
  cat("\nProcessing fold", i, "of", length(folds), "...\n")
  
  valid_idx<- folds[[i]]
  
  fold_train<- train[-valid_idx,]
  fold_valid<- train[valid_idx,]
  
  fold_train.x<- data.matrix(subset(fold_train, select=-pneumonia))
  fold_valid.x<- data.matrix(subset(fold_valid, select=-pneumonia))
  
  fold_train.y.factor<- fold_train$pneumonia
  fold_valid.y.factor<- fold_valid$pneumonia
  
  fold_train.y.num<- ifelse(fold_train$pneumonia=="Yes", 1, 0)
  
#fitting random forest classifier
model.rf<- randomForest(pneumonia ~ ., data=fold_train, ntree=150,
mtry=min(4, ncol(fold_train)-1), maxnodes=30)
oof.rf[valid_idx]<- predict(model.rf, newdata=fold_valid, type="prob")[,"Yes"]

#fitting gradient boosting classifier
dtrain_fold<- xgb.DMatrix(data=fold_train.x, label=fold_train.y.num)
dvalid_fold<- xgb.DMatrix(data=fold_valid.x)
model.xgb<- xgb.train(params=list(objective="binary:logistic", eval_metric="auc",
max_depth=6, eta=0.01, subsample=0.8, colsample_bytree=0.5), data=dtrain_fold,
nrounds=300, verbose=0)
oof.xgb[valid_idx]<- predict(model.xgb, newdata=dvalid_fold)
  
#fitting support vector classifier with linear kernel 
model.svm.linear<- svm(x=fold_train.x, y=fold_train.y.factor, kernel="linear", 
probability=TRUE)
oof.svm.linear[valid_idx]<- attr(predict(model.svm.linear, newdata=fold_valid.x,
probability=TRUE), "probabilities")[,"Yes"]
  
#fitting support vector classifier with radial kernel 
model.svm.radial<- svm(x=fold_train.x, y=fold_train.y.factor, kernel="radial",
probability=TRUE)
oof.svm.radial[valid_idx]<- attr(predict(model.svm.radial, newdata=fold_valid.x, 
probability=TRUE), "probabilities")[,"Yes"]
  
#fitting k-nearest neighbor classifier
model.knn<- train(pneumonia ~ ., data=fold_train, method="knn",
trControl=trainControl(method="cv", number=5, classProbs=TRUE))
oof.knn[valid_idx]<- predict(model.knn, newdata=fold_valid, type="prob")[,"Yes"]
  
#fitting naive Bayes classifier
model.nb<- naiveBayes(pneumonia ~ ., data=fold_train)
oof.nb[valid_idx]<- predict(model.nb, newdata=fold_valid, type="raw")[,"Yes"]
  
#fitting artificial neural network classifier
fold_train.ann<- fold_train
fold_valid.ann<- fold_valid
fold_train.ann$pneumonia<- ifelse(fold_train.ann$pneumonia=="Yes", 1, 0)
fold_valid.ann$pneumonia<- ifelse(fold_valid.ann$pneumonia=="Yes", 1, 0)
model.ann<- neuralnet(pneumonia ~ ., data=fold_train.ann, hidden=3,
linear.output=FALSE)
  
ann.prob<- tryCatch({
as.vector(compute(model.ann, subset(fold_valid.ann, select=-pneumonia))$net.result)
}, error=function(e) {
 rep(mean(fold_train.ann$pneumonia), nrow(fold_valid.ann))
  })
  
oof.ann[valid_idx]<- ann.prob
}

#fitting meta-model
stack.train<- data.frame(rf=oof.rf, xgb=oof.xgb, svm_linear=oof.svm.linear,
svm_radial=oof.svm.radial, knn=oof.knn, nb=oof.nb, ann=oof.ann, 
pneumonia=train.y.num)

meta.model<- glm(pneumonia ~ ., data=stack.train, family=binomial)

#fitting models on full training set
rf.biclass<- randomForest(pneumonia ~ ., data=train, ntree=150, 
mtry=min(4, ncol(train)-1), maxnodes=30)

dtrain<- xgb.DMatrix(data=train.x, label=train.y.num)
dtest<- xgb.DMatrix(data=test.x)

xgb.biclass<- xgb.train(params=list(objective="binary:logistic",
eval_metric="auc", max_depth=6, eta=0.01, subsample=0.8, 
colsample_bytree=0.5), data=dtrain, nrounds=300, verbose=0)

svm.class.linear<- svm(x=train.x, y=train.y.factor, kernel="linear",
probability=TRUE)

svm.class.radial<- svm(x=train.x, y=train.y.factor, kernel="radial",
probability=TRUE)

knn.biclass<- train(pneumonia ~ ., data=train, method="knn",
trControl=trainControl(method="cv", number=5, classProbs=TRUE))

nb.biclass<- naiveBayes(pneumonia ~ ., data=train)

train.ann<- train
test.ann<- test
train.ann$pneumonia<- ifelse(train.ann$pneumonia=="Yes", 1, 0)
test.ann$pneumonia<- ifelse(test.ann$pneumonia=="Yes", 1, 0)

ann.biclass<- neuralnet(pneumonia ~ ., data=train.ann, hidden=3,
linear.output=FALSE)

#predicing on testing set
test.rf<- predict(rf.biclass, newdata=test, type="prob")[,"Yes"]
test.xgb<- predict(xgb.biclass, newdata=dtest)
test.svm.linear<- attr(predict(svm.class.linear, newdata=test.x, probability=TRUE),
"probabilities")[,"Yes"]
test.svm.radial<- attr(predict(svm.class.radial, newdata=test.x, probability=TRUE),
"probabilities")[,"Yes"]

test.knn <- predict(knn.biclass, newdata = test, type = "prob")[, "Yes"]
test.nb  <- predict(nb.biclass, newdata = test, type = "raw")[, "Yes"]

test.ann<- tryCatch({
  as.vector(compute(ann.biclass, subset(test.ann, select=-pneumonia))$net.result)
}, error=function(e) {
  rep(mean(train.ann$pneumonia), nrow(test.ann))
})

#stacking predictions
stack.test<- data.frame(rf=test.rf, xgb=test.xgb, svm_linear=test.svm.linear,
svm_radial=test.svm.radial, knn=test.knn, nb=test.nb, ann=test.ann)

stack.prob<- predict(meta.model, newdata=stack.test, type="response")
stack.class<- ifelse(stack.prob>=0.5, "Yes", "No")
stack.class<- factor(stack.class, levels=c("No", "Yes"))

#displaying confusion matrix
conf.mat <- table(Predicted = stack.class, Actual = test.y.factor)

print("Stacking Binary Classifier - Confusion Matrix:")
print(conf.mat)

#displaying performance measures
m<- performance.measures(conf.mat)

cat("Stacking Accuracy:", round(m$accuracy, 4), "\n")
cat("Stacking Sensitivity:", round(m$sensitivity, 4), "\n")
cat("Stacking Specificity:", round(m$specificity, 4), "\n")
cat("Stacking Precision:", round(m$precision, 4), "\n")
cat("Stacking F1-score:", round(m$F1_score, 4), "\n")


pred.class.rf<- factor(ifelse(test.rf>=0.5, "Yes", "No"), levels=c("No", "Yes"))
pred.class.xgb<- factor(ifelse(test.xgb>=0.5, "Yes", "No"), levels=c("No", "Yes"))
pred.class.svm.linear<- factor(ifelse(test.svm.linear>=0.5, "Yes", "No"), 
levels=c("No", "Yes"))
pred.class.svm.radial<- factor(ifelse(test.svm.radial>=0.5, "Yes", "No"), 
levels=c("No", "Yes"))
pred.class.knn<- factor(ifelse(test.knn>=0.5, "Yes", "No"), levels=c("No", "Yes"))
pred.class.nb<- factor(ifelse(test.nb>=0.5, "Yes", "No"), levels=c("No", "Yes"))
pred.class.ann<- factor(ifelse(test.ann>=0.5, "Yes", "No"), levels=c("No", "Yes"))

m.rf<- get_metrics(pred.class.rf, test.y.factor)
m.xgb <- get_metrics(pred.class.xgb, test.y.factor)
m.svm.linear<- get_metrics(pred.class.svm.linear, test.y.factor)
m.svm.radial<- get_metrics(pred.class.svm.radial, test.y.factor)
m.knn<- get_metrics(pred.class.knn, test.y.factor)
m.nb<- get_metrics(pred.class.nb, test.y.factor)
m.ann<- get_metrics(pred.class.ann, test.y.factor)
m.stack<- get_metrics(stack.class, test.y.factor)

results<- data.frame(Model=c("Random Forest", "XGBoost", "SVM Linear", "SVM Radial",
"KNN", "Naive Bayes", "ANN", "Stacked"),
  Accuracy=c(m.rf$accuracy, m.xgb$accuracy, m.svm.linear$accuracy,
               m.svm.radial$accuracy, m.knn$accuracy, m.nb$accuracy,
               m.ann$accuracy, m.stack$accuracy),
  Sensitivity=c(m.rf$sensitivity, m.xgb$sensitivity, m.svm.linear$sensitivity,
               m.svm.radial$sensitivity, m.knn$sensitivity, m.nb$sensitivity,
               m.ann$sensitivity, m.stack$sensitivity),
  Specificity=c(m.rf$specificity, m.xgb$specificity, m.svm.linear$specificity,
               m.svm.radial$specificity, m.knn$specificity, m.nb$specificity,
               m.ann$specificity, m.stack$specificity),
  Precision=c(m.rf$precision, m.xgb$precision, m.svm.linear$precision,
              m.svm.radial$precision, m.knn$precision, m.nb$precision,
              m.ann$precision, m.stack$precision),
  F1_score=c(m.rf$F1_score, m.xgb$F1_score, m.svm.linear$F1_score,
             m.svm.radial$F1_score, m.knn$F1_score, m.nb$F1_score,
             m.ann$F1_score, m.stack$F1_score))

results[,-1]<- round(results[,-1], 4)

print("Performance comparison table:")
print(results)