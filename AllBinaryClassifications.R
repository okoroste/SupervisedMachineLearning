pneumonia.data<- read.csv("C:/Users/000110888/OneDrive - CSULB/Desktop/pneumonia_data.csv",
header=TRUE, sep=",")
pneumonia.data

pneumonia.data$pneumonia<- ifelse(pneumonia.data$pneumonia=="yes",1,0)
pneumonia.data$gender<- ifelse(pneumonia.data$gender=='M',1,0)
pneumonia.data$tobacco_use<- ifelse(pneumonia.data$tobacco_use=='yes',1,0) 

#creating training and testing sets stratifying by pneumonia
set.seed(447033)
train<- dplyr::slice_sample(dplyr::group_by(pneumonia.data, pneumonia), prop=0.8)
test<- dplyr::anti_join(pneumonia.data, train)
nrow(train)
nrow(test)
cbind(Count=table(train$pneumonia), 
      Percentage=round(prop.table(table(train$pneumonia)) * 100, 2))
cbind(Count=table(test$pneumonia), 
      Percentage=round(prop.table(table(test$pneumonia)) * 100, 2))

train.x<- data.matrix(train[-5])
train.y<- data.matrix(train[5])
test.x<- data.matrix(test[-5])
test.y<- data.matrix(test[5])

#####################################################
# FITTING RANDOM FOREST BINARY CLASSIFICATION MODEL #
#####################################################
library(randomForest)

#fitting random forest on training set
rf.class<- randomForest(as.factor(pneumonia) ~ ., data=train, 
ntree=150, mtry=4, maxnodes=30)

#displaying feature importance
rf_imp<- importance(rf.class, type=2)
rf_imp_df<- data.frame(Variable=rownames(rf_imp), 
MeanDecreaseGini=rf_imp[,1], row.names=NULL)

print("Feature Importance:")
rf_imp_df[order(rf_imp_df$MeanDecreaseGini, decreasing=TRUE),]

#computing predicted probabilities 
pred.prob<- predict(rf.class, newdata=test, type="prob")

#classifying with cutoff 0.5
pred.class <- ifelse(pred.prob[,2] >= 0.5, 1, 0)

#computing confusion matrix (force 0/1 levels)
conf.mat<- table(Predicted=factor(pred.class, levels=c(0,1)), 
Actual=factor(test$pneumonia, levels=c(0,1)))
print(conf.mat)

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
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|(precision
+sensitivity)==0, NA, 2*precision*sensitivity/(precision+sensitivity))
cat("RF Accuracy:", round(accuracy, 4), "\n")
cat("RF Sensitivity:", round(sensitivity, 4), "\n")
cat("RF Specificity:", round(specificity, 4), "\n")
cat("RF Precision:", round(precision, 4), "\n")
cat("RF F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
pred<- ROCR::prediction(pred.prob[,2], test$pneumonia)
perf<- ROCR::performance(pred, "tpr", "fpr")
plot(perf, col="blue", lwd=2, main="ROC Curve for Random 
Forest Model")
abline(a=0, b=1, lty=2, col="red")

#computing AUC
auc<- ROCR::performance(pred, "auc")@y.values[[1]]
cat("RF AUC:", round(auc, 4), "\n")

#########################################################
# FITTING GRADIENT BOOSTING BINARY CLASSIFICATION MODEL #
#########################################################
library(xgboost)

set.seed(558607)
xgb.class<- xgboost(x=train.x, y=as.factor(train.y), 
objective="binary:logistic", nrounds=300, max_depth=6, 
learning_rate=0.01, subsample=0.8, colsample_bytree=0.5)

# outputting feature importance
imp<- xgb.importance(colnames(train.x), model=xgb.class)
print(imp[, c("Feature", "Gain")])

# computing confusion matrix
pred.prob<- predict(xgb.class, test.x)
pred.class<- ifelse(pred.prob>=0.5, 1, 0)
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print(conf.mat)

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
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|(precision
                                                       +sensitivity)==0, NA, 2*precision*sensitivity/(precision+sensitivity))
cat("XGBoost Accuracy:", round(accuracy, 4), "\n")
cat("XGBoost Sensitivity:", round(sensitivity, 4), "\n")
cat("XGBoost Specificity:", round(specificity, 4), "\n")
cat("XGBoost Precision:", round(precision, 4), "\n")
cat("XGBoost F1-score:", round(F1_score, 4), "\n")

# plotting ROC curve
pred<- ROCR::prediction(pred.prob, test.y)
perf<- ROCR::performance(pred, "tpr", "fpr")
plot(perf, col="blue", lwd=2, main="ROC Curve for XGBoost Model")
abline(a=0, b=1, lty=2, col="red")

# computing AUC
auc<- ROCR::performance(pred, "auc")@y.values[[1]]
cat("AUC:", round(auc, 4), "\n")


###############################################################
# FITTING SUPPORT VECTOR BINARY CLASSIFIER WITH LINEAR KERNEL #
###############################################################
library(e1071)

svm.class.linear<- svm(x=train.x, y=as.factor(train.y), kernel="linear", 
probability=TRUE)

#displaying feature importance
w<- t(svm.class.linear$coefs) %*% svm.class.linear$SV
importance<- data.frame(Variable=colnames(train.x), Importance=abs(as.vector(w)))
importance_sorted<- importance[order(-importance$Importance), ]
print(importance_sorted)

#computing predicted probabilities for testing data
pred.prob<- attr(predict(svm.class.linear, test.x, probability=TRUE), 
"probabilities")[,"1"]

#classifying with cutoff 0.5
pred.class<- ifelse(pred.prob>=0.5, 1, 0)

#computing confusion matrix 
conf.mat<- table(Predicted=factor(pred.class, levels=c(0,1)), 
Actual=factor(test.y, levels=c(0,1)))

print("Confusion Matrix for SVM with Linear Kernel:")
print(conf.mat)

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
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|(precision
+sensitivity)==0, NA, 2*precision*sensitivity/(precision+sensitivity))

cat("SVM (Linear Kernel) Accuracy:", round(accuracy, 4), "\n")
cat("SVM (Linear Kernel) Sensitivity:", round(sensitivity, 4), "\n")
cat("SVM (Linear Kernel) Specificity:", round(specificity, 4), "\n")
cat("SVM (Linear Kernel) Precision:", round(precision, 4), "\n")
cat("SVM (Linear Kernel) F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
library(ROCR)
pred.prob<- as.vector(pred.prob)
pred.rocr<- ROCR::prediction(pred.prob, test.y)
perf<- ROCR::performance(pred.rocr, "tpr", "fpr")

plot(perf, col="blue", lwd=2, main="ROC Curve for 
SVM Model with Linear Kernel")
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
print(importance_sorted)

#computing predicted probabilities for testing data
pred.prob<- attr(predict(svm.class.poly, test.x, probability=TRUE), 
"probabilities")[,"1"]

#classifying with cutoff 0.5
pred.class<- ifelse(pred.prob>=0.5, 1, 0)

#computing confusion matrix 
conf.mat<- table(Predicted=factor(pred.class, levels=c(0,1)), 
Actual=factor(test.y, levels=c(0,1)))

print("Confusion Matrix for SVM with Polynomial Kernel:")
print(conf.mat)

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
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|(precision
+sensitivity)==0, NA, 2*precision*sensitivity/(precision+sensitivity))

cat("SVM (Polynomial Kernel) Accuracy:", round(accuracy, 4), "\n")
cat("SVM (Polynomial Kernel) Sensitivity:", round(sensitivity, 4), "\n")
cat("SVM (Polynomial Kernel) Specificity:", round(specificity, 4), "\n")
cat("SVM (Polynomial Kernel) Precision:", round(precision, 4), "\n")
cat("SVM (Polynomial Kernel) F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
library(ROCR)
pred.prob<- as.vector(pred.prob)
pred.rocr<- ROCR::prediction(pred.prob, test.y)
perf<- ROCR::performance(pred.rocr, "tpr", "fpr")

plot(perf, col="blue", lwd=2, main="ROC Curve for 
SVM Model with Polynomial Kernel")
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
print(importance_sorted)

#computing predicted probabilities for testing data
pred.prob<- attr(predict(svm.class.radial, test.x, probability=TRUE), 
"probabilities")[,"1"]

#classifying with cutoff 0.5
pred.class<- ifelse(pred.prob>=0.5, 1, 0)

#computing confusion matrix 
conf.mat<- table(Predicted=factor(pred.class, levels=c(0,1)), 
Actual=factor(test.y, levels=c(0,1)))

print("Confusion Matrix for SVM with Radial Kernel:")
print(conf.mat)

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
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|(precision
+sensitivity)==0, NA, 2*precision*sensitivity/(precision+sensitivity))

cat("SVM (Radial Kernel) Accuracy:", round(accuracy, 4), "\n")
cat("SVM (Radial Kernel) Sensitivity:", round(sensitivity, 4), "\n")
cat("SVM (Radial Kernel) Specificity:", round(specificity, 4), "\n")
cat("SVM (Radial Kernel) Precision:", round(precision, 4), "\n")
cat("SVM (Radial Kernel) F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
library(ROCR)
pred.prob<- as.vector(pred.prob)
pred.rocr<- ROCR::prediction(pred.prob, test.y)
perf<- ROCR::performance(pred.rocr, "tpr", "fpr")

plot(perf, col="blue", lwd=2, main="ROC Curve for 
SVM Model with Radial Kernel")
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
print(importance_sorted)

#computing predicted probabilities for testing data
pred.prob<- attr(predict(svm.class.sigmoid, test.x, probability=TRUE), 
"probabilities")[,"1"]

#classifying with cutoff 0.5
pred.class<- ifelse(pred.prob>=0.5, 1, 0)

#computing confusion matrix 
conf.mat<- table(Predicted=factor(pred.class, levels=c(0,1)), 
Actual=factor(test.y, levels=c(0,1)))

print("Confusion Matrix for SVM with Sigmoid Kernel:")
print(conf.mat)

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
F1_score<- ifelse(is.na(precision)|is.na(sensitivity)|(precision
+sensitivity)==0, NA, 2*precision*sensitivity/(precision+sensitivity))

cat("SVM (Sigmoid Kernel) Accuracy:", round(accuracy, 4), "\n")
cat("SVM (Sigmoid Kernel) Sensitivity:", round(sensitivity, 4), "\n")
cat("SVM (Sigmoid Kernel) Specificity:", round(specificity, 4), "\n")
cat("SVM (Sigmoid Kernel) Precision:", round(precision, 4), "\n")
cat("SVM (Sigmoid Kernel) F1-score:", round(F1_score, 4), "\n")

#plotting ROC curve
library(ROCR)
pred.prob<- as.vector(pred.prob)
pred.rocr<- ROCR::prediction(pred.prob, test.y)
perf<- ROCR::performance(pred.rocr, "tpr", "fpr")

plot(perf, col="blue", lwd=2, main="ROC Curve for 
SVM Model with Sigmoid Kernel")
abline(a=0, b=1, lty=2, col="red")

#computing AUC
auc<- ROCR::performance(pred.rocr, "auc")@y.values[[1]]
cat("SVM (Sigmoid Kernel) AUC:", round(auc, 4), "\n")


