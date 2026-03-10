pneumonia.data<- read.csv("C:/Users/000110888/OneDrive - CSULB/Desktop/pneumonia_data.csv",
header=TRUE, sep=",")

pneumonia.data$pneumonia<- ifelse(pneumonia.data$pneumonia=="yes",1,0)
pneumonia.data$gender<- ifelse(pneumonia.data$gender=='M',1,0)
pneumonia.data$tobacco_use<- ifelse(pneumonia.data$tobacco_use=='yes',1,0) 
pneumonia.data$age<- (pneumonia.data$age-min(pneumonia.data$age))/
(max(pneumonia.data$age)-min(pneumonia.data$age))
pneumonia.data$PM2_5<- (pneumonia.data$PM2_5-min(pneumonia.data$PM2_5))/
(max(pneumonia.data$PM2_5)-min(pneumonia.data$PM2_5))

#creating training and testing sets stratifying by pneumonia
set.seed(447033)
train<- dplyr::slice_sample(dplyr::group_by(pneumonia.data, pneumonia), 
prop=0.8)
test<- dplyr::anti_join(pneumonia.data, train)

cbind(Count=table(train$pneumonia), 
      Percentage=round(prop.table(table(train$pneumonia)) * 100, 2))
cbind(Count=table(test$pneumonia), 
      Percentage=round(prop.table(table(test$pneumonia)) * 100, 2))

train.x<- data.matrix(train[-5])
train.y<- data.matrix(train[5])
test.x<- data.matrix(test[-5])
test.y<- data.matrix(test[5])

###########################################
# FITTING RANDOM FOREST BINARY CLASSIFIER #
###########################################
library(randomForest)

#fitting random forest on training set
rf.biclass<- randomForest(as.factor(pneumonia) ~ ., data=train, 
ntree=150, mtry=4, maxnodes=30)

#displaying feature importance
rf_imp<- importance(rf.biclass, type=2)
rf_imp_df<- data.frame(Variable=rownames(rf_imp), 
MeanDecreaseGini=rf_imp[,1], row.names=NULL)

print("Random Forest Binary Classifier - Feature Importance:")
rf_imp_df[order(rf_imp_df$MeanDecreaseGini, decreasing=TRUE),]

#computing predicted classes for testing data
pred.class<- predict(rf.biclass, newdata=test)

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("Random Forest Binary Classifier - Confusion Matrix:")
conf.mat

#computing prediction accuracy (TP+TN)/Total
accuracy<- sum(diag(conf.mat))/sum(conf.mat) 

#computing TP, FP, TN, FN
TP<- conf.mat["1","1"]
FP<- conf.mat["1","0"]
TN<- conf.mat["0","0"]
FN<- conf.mat["0","1"]

#computing sensitivity TP/(TP+FN)
sensitivity<- ifelse((TP+FN)==0, NA, TP/(TP+FN))

#computing specificity TN/(TN+FP)
specificity<- ifelse((TN+FP)==0, NA, TN/(TN+FP))

#computing precision TP/(TP+FP)
precision<- ifelse((TP+FP)==0, NA, TP/(TP+FP))

#computing F1-score (using sensitivity instead of recall)
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|
(precision + sensitivity)==0, NA, 
2*precision*sensitivity/(precision+sensitivity))

cat("RF Accuracy:", round(accuracy, 4), "\n")
cat("RF Sensitivity:", round(sensitivity, 4), "\n")
cat("RF Specificity:", round(specificity, 4), "\n")
cat("RF Precision:", round(precision, 4), "\n")
cat("RF F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
pred.prob<- predict(rf.biclass, newdata=test, type="prob")[,2]
pred<- ROCR::prediction(pred.prob, test.y)
perf<- ROCR::performance(pred, "tpr", "fpr")
plot(perf, col="blue", lwd=2, main="ROC Curve for Random 
Forest Binary Classifier")
abline(a=0, b=1, lty=2, col="red")

#computing AUC
auc<- ROCR::performance(pred, "auc")@y.values[[1]]
cat("RF AUC:", round(auc, 4), "\n")

###############################################
# FITTING GRADIENT BOOSTING BINARY CLASSIFIER #
###############################################
library(xgboost)

set.seed(558607)
xgb.biclass<- xgboost(x=train.x, y=as.factor(train.y), 
objective="binary:logistic", nrounds=300, max_depth=6, 
learning_rate=0.01, subsample=0.8, colsample_bytree=0.5)

#outputting feature importance
imp<- xgb.importance(colnames(train.x), model=xgb.biclass)
print("Gradient Boosting Binary Classifier - Feature Importance:")
imp[, c("Feature", "Gain")]

#computing predicted classes for testing data
pred.prob<- predict(xgb.biclass, newdata=test.x)
pred.class<- ifelse(pred.prob>=0.5, 1, 0)

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("Gradient Boosting Binary Classifier - Confusion Matrix:")
conf.mat

#computing prediction accuracy (TP+TN)/Total
accuracy<- sum(diag(conf.mat))/sum(conf.mat) 

#computing TP, FP, TN, FN
TP<- conf.mat["1","1"]
FP<- conf.mat["1","0"]
TN<- conf.mat["0","0"]
FN<- conf.mat["0","1"]

#computing sensitivity TP/(TP+FN)
sensitivity<- ifelse((TP+FN)==0, NA, TP/(TP+FN))

#computing specificity TN/(TN+FP)
specificity<- ifelse((TN+FP)==0, NA, TN/(TN+FP))

#computing precision TP/(TP+FP)
precision<- ifelse((TP+FP)==0, NA, TP/(TP+FP))

#computing F1-score (using sensitivity instead of recall)
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|
(precision + sensitivity)==0, NA, 
2*precision*sensitivity/(precision+sensitivity))

cat("XGBoost Accuracy:", round(accuracy, 4), "\n")
cat("XGBoost Sensitivity:", round(sensitivity, 4), "\n")
cat("XGBoost Specificity:", round(specificity, 4), "\n")
cat("XGBoost Precision:", round(precision, 4), "\n")
cat("XGBoost F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
pred<- ROCR::prediction(pred.prob, test.y)
perf<- ROCR::performance(pred, "tpr", "fpr")
plot(perf, col="blue", lwd=2, main="ROC Curve for XGBoost 
Binary Classifier")
abline(a=0, b=1, lty=2, col="red")

#computing AUC
auc<- ROCR::performance(pred, "auc")@y.values[[1]]
cat("XGB AUC:", round(auc, 4), "\n")

###############################################################
# FITTING SUPPORT VECTOR BINARY CLASSIFIER WITH LINEAR KERNEL #
###############################################################
library(e1071)

svm.class.linear<- svm(x=train.x, y=as.factor(train.y), 
kernel="linear", probability=TRUE)

#displaying feature importance
w<- t(svm.class.linear$coefs) %*% svm.class.linear$SV
importance<- data.frame(Variable=colnames(train.x), 
Importance=abs(as.vector(w)))
importance_sorted<- importance[order(-importance$Importance), ]
print("SVM (Linear Kernel) Binary Classifier - Feature Importance:")
importance_sorted

#computing predicted classes for testing data
pred.class<- predict(svm.class.linear, newdata=test.x)

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("SVM (Linear Kernel) Binary Classifier - Confusion Matrix:")
conf.mat

#computing prediction accuracy (TP+TN)/Total
accuracy<- sum(diag(conf.mat))/sum(conf.mat) 

#computing TP, FP, TN, FN
TP<- conf.mat["1","1"]
FP<- conf.mat["1","0"]
TN<- conf.mat["0","0"]
FN<- conf.mat["0","1"]

#computing sensitivity TP/(TP+FN)
sensitivity<- ifelse((TP+FN)==0, NA, TP/(TP+FN))

#computing specificity TN/(TN+FP)
specificity<- ifelse((TN+FP)==0, NA, TN/(TN+FP))

#computing precision TP/(TP+FP)
precision<- ifelse((TP+FP)==0, NA, TP/(TP+FP))

#computing F1-score 
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|
(precision + sensitivity)==0, NA, 
2*precision*sensitivity/(precision+sensitivity))

cat("SVM (Linear Kernel) Accuracy:", round(accuracy, 4), "\n")
cat("SVM (Linear Kernel) Sensitivity:", round(sensitivity, 4), "\n")
cat("SVM (Linear Kernel) Specificity:", round(specificity, 4), "\n")
cat("SVM (Linear Kernel) Precision:", round(precision, 4), "\n")
cat("SVM (Linear Kernel) F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
pred_obj<- predict(svm.class.linear, newdata = test.x, probability = TRUE)
pred.prob <- attr(pred_obj, "probabilities")[,"1"]
pred.rocr<- ROCR::prediction(pred.prob, as.factor(test.y))
perf<- ROCR::performance(pred.rocr, "tpr", "fpr")

plot(perf, col="blue", lwd=2, main="ROC Curve for 
SVM (Linear Kernel) Binary Classifier")
abline(a=0, b=1, lty=2, col="red")

#computing AUC
auc<- ROCR::performance(pred.rocr, "auc")@y.values[[1]]
cat("SVM (Linear Kernel) AUC:", round(auc, 4), "\n")

###################################################################
# FITTING SUPPORT VECTOR BINARY CLASSIFIER WITH POLYNOMIAL KERNEL #
###################################################################
svm.class.poly<- svm(x=train.x, y=as.factor(train.y), 
kernel="polynomial", probability=TRUE)

#displaying feature importance
w<- t(svm.class.poly$coefs) %*% svm.class.poly$SV
importance<- data.frame(Variable=colnames(train.x), 
Importance=abs(as.vector(w)))
importance_sorted<- importance[order(-importance$Importance), ]
print("SVM (Polynomial Kernel) Binary Classifier - Feature Importance:")
importance_sorted

#computing predicted classes for testing data
pred.class<- predict(svm.class.poly, newdata=test.x)

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("SVM (Polynomial Kernel) Binary Classifier - Confusion Matrix:")
conf.mat

#computing prediction accuracy (TP+TN)/Total
accuracy<- sum(diag(conf.mat))/sum(conf.mat) 

#computing TP, FP, TN, FN
TP<- conf.mat["1","1"]
FP<- conf.mat["1","0"]
TN<- conf.mat["0","0"]
FN<- conf.mat["0","1"]

#computing sensitivity TP/(TP+FN)
sensitivity<- ifelse((TP+FN)==0, NA, TP/(TP+FN))

#computing specificity TN/(TN+FP)
specificity<- ifelse((TN+FP)==0, NA, TN/(TN+FP))

#computing precision TP/(TP+FP)
precision<- ifelse((TP+FP)==0, NA, TP/(TP+FP))

#computing F1-score 
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|
(precision + sensitivity)==0, NA, 
2*precision*sensitivity/(precision+sensitivity))

cat("SVM (Polynomial Kernel) Accuracy:", round(accuracy, 4), "\n")
cat("SVM (Polynomial Kernel) Sensitivity:", round(sensitivity, 4), "\n")
cat("SVM (Polynomial Kernel) Specificity:", round(specificity, 4), "\n")
cat("SVM (Polynomial Kernel) Precision:", round(precision, 4), "\n")
cat("SVM (Polynomial Kernel) F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
pred_obj<- predict(svm.class.poly, newdata=test.x, 
probability=TRUE)
pred.prob<- attr(pred_obj, "probabilities")[,"1"]
pred.rocr<- ROCR::prediction(pred.prob, as.factor(test.y))
perf<- ROCR::performance(pred.rocr, "tpr", "fpr")

plot(perf, col="blue", lwd=2, main="ROC Curve for 
SVM (Polynomial Kernel) Binary Classifier")
abline(a=0, b=1, lty=2, col="red")

#computing AUC
auc<- ROCR::performance(pred.rocr, "auc")@y.values[[1]]
cat("SVM (Polynomial Kernel) AUC:", round(auc, 4), "\n")

###############################################################
# FITTING SUPPORT VECTOR BINARY CLASSIFIER WITH RADIAL KERNEL #
###############################################################
svm.class.radial<- svm(x=train.x, y=as.factor(train.y), 
kernel="radial", probability=TRUE)

#displaying feature importance
w<- t(svm.class.radial$coefs) %*% svm.class.radial$SV
importance<- data.frame(Variable=colnames(train.x), 
Importance=abs(as.vector(w)))
importance_sorted<- importance[order(-importance$Importance), ]
print("SVM (Radial Kernel) Binary Classifier - Feature Importance:")
importance_sorted

#computing predicted classes for testing data
pred.class<- predict(svm.class.radial, newdata=test.x)

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("SVM (Radial Kernel) Binary Classifier - Confusion Matrix:")
conf.mat

#computing prediction accuracy (TP+TN)/Total
accuracy<- sum(diag(conf.mat))/sum(conf.mat) 

#computing TP, FP, TN, FN
TP<- conf.mat["1","1"]
FP<- conf.mat["1","0"]
TN<- conf.mat["0","0"]
FN<- conf.mat["0","1"]

#computing sensitivity TP/(TP+FN)
sensitivity<- ifelse((TP+FN)==0, NA, TP/(TP+FN))

#computing specificity TN/(TN+FP)
specificity<- ifelse((TN+FP)==0, NA, TN/(TN+FP))

#computing precision TP/(TP+FP)
precision<- ifelse((TP+FP)==0, NA, TP/(TP+FP))

#computing F1-score 
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|
(precision + sensitivity)==0, NA, 
2*precision*sensitivity/(precision+sensitivity))

cat("SVM (Radial Kernel) Accuracy:", round(accuracy, 4), "\n")
cat("SVM (Radial Kernel) Sensitivity:", round(sensitivity, 4), "\n")
cat("SVM (Radial Kernel) Specificity:", round(specificity, 4), "\n")
cat("SVM (Radial Kernel) Precision:", round(precision, 4), "\n")
cat("SVM (Radial Kernel) F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
pred_obj<- predict(svm.class.radial, newdata=test.x, 
probability=TRUE)
pred.prob<- attr(pred_obj, "probabilities")[,"1"]
pred.rocr<- ROCR::prediction(pred.prob, as.factor(test.y))
perf<- ROCR::performance(pred.rocr, "tpr", "fpr")

plot(perf, col="blue", lwd=2, main="ROC Curve for 
SVM (Radial Kernel) Binary Classifier")
abline(a=0, b=1, lty=2, col="red")

#computing AUC
auc<- ROCR::performance(pred.rocr, "auc")@y.values[[1]]
cat("SVM (Radial Kernel) AUC:", round(auc, 4), "\n")

################################################################
# FITTING SUPPORT VECTOR BINARY CLASSIFIER WITH SIGMOID KERNEL #
################################################################
svm.class.sigmoid<- svm(x=train.x, y=as.factor(train.y), 
kernel="sigmoid", probability=TRUE)

#displaying feature importance
w<- t(svm.class.sigmoid$coefs) %*% svm.class.sigmoid$SV
importance<- data.frame(Variable=colnames(train.x), 
Importance=abs(as.vector(w)))
importance_sorted<- importance[order(-importance$Importance), ]
print("SVM (Sigmoid Kernel) Binary Classifier - Feature Importance:")
importance_sorted

#computing predicted classes for testing data
pred.class<- predict(svm.class.sigmoid, newdata=test.x)

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("SVM (Sigmoid Kernel) Binary Classifier - Confusion Matrix:")
conf.mat

#computing prediction accuracy (TP+TN)/Total
accuracy<- sum(diag(conf.mat))/sum(conf.mat) 

#computing TP, FP, TN, FN
TP<- conf.mat["1","1"]
FP<- conf.mat["1","0"]
TN<- conf.mat["0","0"]
FN<- conf.mat["0","1"]

#computing sensitivity TP/(TP+FN)
sensitivity<- ifelse((TP+FN)==0, NA, TP/(TP+FN))

#computing specificity TN/(TN+FP)
specificity<- ifelse((TN+FP)==0, NA, TN/(TN+FP))

#computing precision TP/(TP+FP)
precision<- ifelse((TP+FP)==0, NA, TP/(TP+FP))

#computing F1-score 
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|
(precision + sensitivity)==0, NA, 
2*precision*sensitivity/(precision+sensitivity))

cat("SVM (Sigmoid Kernel) Accuracy:", round(accuracy, 4), "\n")
cat("SVM (Sigmoid Kernel) Sensitivity:", round(sensitivity, 4), "\n")
cat("SVM (Sigmoid Kernel) Specificity:", round(specificity, 4), "\n")
cat("SVM (Sigmoid Kernel) Precision:", round(precision, 4), "\n")
cat("SVM (Sigmoid Kernel) F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
pred.prob<- attr(predict(svm.class.sigmoid, newdata=test.x, 
probability=TRUE), "probabilities")[,"1"]
pred.rocr<- ROCR::prediction(pred.prob, as.factor(test.y))
perf<- ROCR::performance(pred.rocr, "tpr", "fpr")

plot(perf, col="blue", lwd=2, main="ROC Curve for 
SVM (Sigmoid Kernel) Binary Classifier")
abline(a=0, b=1, lty=2, col="red")

#computing AUC
auc<- ROCR::performance(pred.rocr, "auc")@y.values[[1]]
cat("SVM (Sigmoid Kernel) AUC:", round(auc, 4), "\n")


################################################
# FITTING k-NEAREST NEIGHBOR BINARY CLASSIFIER #
################################################
library(caret)
set.seed(300133)
#training KNN model -> optimal k=9
print(train(as.factor(pneumonia)~., data=train, method="knn"))

#fitting optimal KNN binary classifier 
knn.biclass<- train(as.factor(pneumonia) ~ ., data=train, 
method="knn", tuneGrid=data.frame(k=9))

#displaying permutation based feature importance using AUC 
#Note: KNN has no built-in feature importance
library(vip)
library(yardstick)
library(dplyr)
library(ggplot2)

#defining function that returns probability of class 1
knn_predprob<- function(object, newdata) {
  probs<- predict(object, newdata=newdata, type="prob")
  probs[,"1"]
}

knn_perm_imp<- vip::vi_permute(object=knn.biclass, 
feature_names=colnames(train.x), train=as.data.frame(train.x), 
target=as.factor(train.y), metric=yardstick::roc_auc_vec, 
pred_wrapper=knn_predprob, nsim=5, smaller_is_better=FALSE)

#sorting in decreasing order of gain
knn_gain<- knn_perm_imp %>%  
  rename(Variable=any_of(c("Variable", "Feature"))) %>%
  transmute(Variable, Gain=-Importance) %>% 
  arrange(desc(Gain))

print("KNN Binary Classifier - Feature Importance:")
knn_gain

#computing predicted classes for testing data
pred.class<- predict(knn.biclass, newdata=test)

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("KNN Binary Classifier - Confusion Matrix:")
conf.mat

#computing prediction accuracy (TP+TN)/Total
accuracy<- sum(diag(conf.mat))/sum(conf.mat)

#computing TP, FP, TN, FN
TP<- conf.mat["1","1"]
FP<- conf.mat["1","0"]
TN<- conf.mat["0","0"]
FN<- conf.mat["0","1"]

#computing sensitivity TP/(TP+FN)
sensitivity<- ifelse((TP+FN)==0, NA, TP/(TP+FN))

#computing specificity TN/(TN+FP)
specificity<- ifelse((TN+FP)==0, NA, TN/(TN+FP))

#computing precision TP/(TP+FP)
precision<- ifelse((TP+FP)==0, NA, TP/(TP+FP))

#computing F1-score 
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|
(precision+sensitivity)==0, NA, 
2*precision*sensitivity/(precision+sensitivity))

cat("KNN Accuracy:", round(accuracy, 4), "\n")
cat("KNN Sensitivity:", round(sensitivity, 4), "\n")
cat("KNN Specificity:", round(specificity, 4), "\n")
cat("KNN Precision:", round(precision, 4), "\n")
cat("KNN F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
pred.prob<- predict(knn.biclass, newdata=test.x, 
type="prob")[,"1"]
pred.rocr<- ROCR::prediction(pred.prob, test.y)
perf<- ROCR::performance(pred.rocr, "tpr", "fpr")

plot(perf, col="blue", lwd=2, main="ROC Curve for KNN 
Binary Classifier")
abline(a=0, b=1, lty=2, col="red")

#computing AUC
auc<- ROCR::performance(pred.rocr, "auc")@y.values[[1]]
cat("KNN AUC:", round(auc, 4), "\n")

#########################################
# FITTING NAIVE BAYES BINARY CLASSIFIER #
#########################################
library(e1071)

nb.biclass<- naiveBayes(pneumonia ~ ., data=train)

#displaying permutation based feature importance using AUC 
#Note: Naive Bayes has no built-in feature importance
library(vip)
library(yardstick)
library(dplyr)
library(ggplot2)

#defining function that returns probability of class 1
nb_predprob<- function(object, newdata) {
  probs<- predict(object, newdata=newdata, type="raw")
  probs[,"1"]
}

nb_perm_imp<- vip::vi_permute(object=nb.biclass, 
feature_names=colnames(train.x), train=as.data.frame(train.x), 
target=as.factor(train.y), metric=yardstick::roc_auc_vec, 
pred_wrapper=nb_predprob, nsim=5, smaller_is_better=FALSE)

#sorting in decreasing order of gain
nb_gain<- nb_perm_imp %>%  
  rename(Variable=any_of(c("Variable", "Feature"))) %>%
  transmute(Variable, Gain=-Importance) %>% 
  arrange(desc(Gain))

print("Naive Bayes Binary Classifier - Feature Importance:")
nb_gain

#computing predicted classes for testing data
pred.class<- predict(nb.biclass, newdata=test)

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("Naive Bayes Binary Classifier - Confusion Matrix:")
conf.mat

#computing prediction accuracy (TP+TN)/Total
accuracy<- sum(diag(conf.mat))/sum(conf.mat)

#computing TP, FP, TN, FN
TP<- conf.mat["1","1"]
FP<- conf.mat["1","0"]
TN<- conf.mat["0","0"]
FN<- conf.mat["0","1"]

#computing sensitivity TP/(TP+FN)
sensitivity<- ifelse((TP+FN)==0, NA, TP/(TP+FN))

#computing specificity TN/(TN+FP)
specificity<- ifelse((TN+FP)==0, NA, TN/(TN+FP))

#computing precision TP/(TP+FP)
precision<- ifelse((TP+FP)==0, NA, TP/(TP+FP))

#computing F1-score 
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|
(precision+sensitivity)==0, NA,
2*precision*sensitivity/(precision+sensitivity))

cat("NB Accuracy:", round(accuracy, 4), "\n")
cat("NB Sensitivity:", round(sensitivity, 4), "\n")
cat("NB Specificity:", round(specificity, 4), "\n")
cat("NB Precision:", round(precision, 4), "\n")
cat("NB F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
pred.prob<- predict(nb.biclass, newdata=test.x, 
type="raw")[,"1"]
pred.rocr<- ROCR::prediction(pred.prob, test.y)
perf<- ROCR::performance(pred.rocr, "tpr", "fpr")

plot(perf, col="blue", lwd=2, main="ROC Curve for 
Naive Bayes Binary Classifier")
abline(a=0, b=1, lty=2, col="red")

#computing AUC
auc<- ROCR::performance(pred.rocr, "auc")@y.values[[1]]
cat("NB AUC:", round(auc, 4), "\n")

#######################################################
# FITTING ARTIFICIAL NEURAL NETWORK BINARY CLASSIFIER #
#######################################################
library(neuralnet)

ann.biclass<- neuralnet(as.factor(pneumonia) ~ ., data=train, 
hidden=3, act.fct="logistic")

#plotting the diagram
plot(ann.biclass)

#displaying permutation based feature importance using AUC 
#Note: ANN has no built-in feature importance
library(vip)
library(yardstick)
library(dplyr)
library(ggplot2)

softmax<- function(x) {  
  exp_x<- exp(x-max(x))  #predicted 0 < p(0), p(1) < 1, and p(0)+p(1)=1
    exp_x/sum(exp_x)
  }

ann_predprob<- function(object, newdata) {
  raw<- neuralnet::compute(object, as.matrix(newdata))$net.result
  probs<- t(apply(raw, 1, softmax))
  as.numeric(probs[, 2]) #returns p(1)
}

auc_metric<- function(truth, estimate) {
  yardstick::roc_auc_vec(truth, estimate, event_level="second")
}

ann_perm_imp<- vip::vi_permute(object=ann.biclass,
feature_names=colnames(train.x), train=train.x, 
target=as.factor(train.y), metric=auc_metric, 
pred_wrapper=ann_predprob, nsim=5, smaller_is_better=FALSE)

#sorting in decreasing order of importance
ann_importance<- ann_perm_imp %>%
transmute(Variable, AUC_drop = Importance) %>%
arrange(desc(AUC_drop))

print("ANN Binary Classifier - Feature Importance:")
ann_importance

#computing predicted classes for testing data
pred.prob<- ann_predprob(ann.biclass, test.x)
pred.class<- ifelse(pred.prob>=0.5, 1, 0)

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("ANN Binary Classifier - Confusion Matrix:")
conf.mat

#computing prediction accuracy (TP+TN)/Total
accuracy<- sum(diag(conf.mat))/sum(conf.mat)

#computing TP, FP, TN, FN
TP<- conf.mat["1","1"]
FP<- conf.mat["1","0"]
TN<- conf.mat["0","0"]
FN<- conf.mat["0","1"]

#computing sensitivity TP/(TP+FN)
sensitivity<- ifelse((TP+FN)==0, NA, TP/(TP+FN))

#computing specificity TN/(TN+FP)
specificity<- ifelse((TN+FP)==0, NA, TN/(TN+FP))

#computing precision TP/(TP+FP)
precision<- ifelse((TP+FP)==0, NA, TP/(TP+FP))

#computing F1-score 
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|
(precision+sensitivity)==0, NA,
2*precision*sensitivity/(precision+sensitivity))

cat("ANN Accuracy:", round(accuracy, 4), "\n")
cat("ANN Sensitivity:", round(sensitivity, 4), "\n")
cat("ANN Specificity:", round(specificity, 4), "\n")
cat("ANN Precision:", round(precision, 4), "\n")
cat("ANN F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
pred.prob<- as.vector(pred.prob)
pred_rocr<- ROCR::prediction(pred.prob, test.y)
perf<- ROCR::performance(pred_rocr, "tpr", "fpr")

plot(perf, col="blue", lwd=2, main="ROC Curve for ANN Model")
abline(a=0, b=1, lty=2, col="red")

#computing AUC
auc<- ROCR::performance(pred_rocr, "auc")@y.values[[1]]
cat("ANN AUC:", round(auc, 4), "\n")

