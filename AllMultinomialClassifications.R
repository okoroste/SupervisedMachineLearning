#defining performance metrics function

performance.measures<- function(pred.class) {
  
 #computing performance measures for individual classes
  tp<- fp<- tn<- fn<- numeric(5)
  accuracy<- sensitivity<- specificity<- precision<- F1_score<- numeric(5)
  
  class.metrics<- function(class) {
    
    tp.class<- ifelse(pred.class==class & test.y==class,1,0)
    fp.class<- ifelse(pred.class==class & test.y!=class,1,0)
    tn.class<- ifelse(pred.class!=class& test.y!=class,1,0)
    fn.class<- ifelse(pred.class!=class & test.y==class,1,0)

    tp[class]<<- sum(tp.class)
     fp[class]<<- sum(fp.class)
      tn[class]<<- sum(tn.class)
       fn[class]<<- sum(fn.class)
    
    accuracy[class]<<- (tp[class]+tn[class])/(tp[class]+fp[class]+tn[class]+fn[class])
    sensitivity[class]<<- ifelse(tp[class]+fn[class]==0, NA, tp[class]/(tp[class]+fn[class]))
    specificity[class]<<- ifelse(tn[class]+fp[class]==0, NA, tn[class]/(tn[class]+fp[class]))
    precision[class]<<- ifelse(tp[class]+fp[class]==0, NA, tp[class]/(tp[class]+fp[class]))
    F1_score[class]<<- ifelse(is.na(precision[class]) | is.na(sensitivity[class]) |
      precision[class]+sensitivity[class]==0, NA, 2*tp[class]/(2*tp[class]+fn[class]+fp[class]))
    
    cat("Class Performance Measures for Class:", class, "\n")
     cat("Accuracy:", accuracy[class], "\n")
      cat("Sensitivity:", sensitivity[class], "\n")
       cat("Specificity:", specificity[class], "\n")
        cat("Precision:", precision[class], "\n")
         cat("F1-score:", F1_score[class], "\n")
  return(c(tp=tp[class], fp=fp[class], tn=tn[class], fn=fn[class],
           accuracy=accuracy[class], sensitivity=sensitivity[class],
           specificity=specificity[class], precision=precision[class],
           F1_score=F1_score[class]))
}
  
  for (class in 1:5) class.metrics(class)
  
  #computing micro measures
  tp.micro<- sum(tp)
  fp.micro<- sum(fp)
  tn.micro<- sum(tn)
  fn.micro<- sum(fn)
  
  accuracy.micro<- (tp.micro+tn.micro)/(tp.micro+fp.micro+tn.micro+fn.micro)
  sensitivity.micro<- ifelse(tp.micro+fn.micro==0, NA, tp.micro/(tp.micro+fn.micro))
  specificity.micro<- ifelse(tn.micro+fp.micro==0, NA, tn.micro/(tn.micro+fp.micro))
  precision.micro<- ifelse(tp.micro+fp.micro==0, NA, tp.micro/(tp.micro+fp.micro))
  F1_score.micro<- ifelse(is.na(precision.micro) | is.na(sensitivity.micro) |
  precision.micro+sensitivity.micro==0, NA,2*tp.micro/(2*tp.micro+fn.micro+fp.micro))

  print("Micro Performance Measures:")
  cat("Accuracy:", accuracy.micro, "\n")
   cat("Sensitivity:", sensitivity.micro, "\n")
    cat("Specificity:", specificity.micro, "\n")
     cat("Precision:", precision.micro, "\n")
      cat("F1-score:", F1_score.micro, "\n")
  
  #computing macro measures
  accuracy.macro <- mean(accuracy, na.rm=TRUE)
  sensitivity.macro <- mean(sensitivity, na.rm=TRUE)
  specificity.macro <- mean(specificity, na.rm=TRUE)
  precision.macro <- mean(precision, na.rm=TRUE)
  F1_score.macro <- mean(F1_score, na.rm=TRUE)
  
  print("Macro Performance Measures:")
  cat("Accuracy:", accuracy.macro, "\n")
   cat("Sensitivity:", sensitivity.macro, "\n")
    cat("Specificity:", specificity.macro, "\n")
    cat("Precision:", precision.macro, "\n")
     cat("F1-score:", F1_score.macro, "\n")
  
  #computing weighted macro measures
  weight<- numeric(5)
  N<- length(test.y)
  
  for (class in 1:5)  weight[class]<- sum(test.y==class)/N 
  
  accuracy.wmacro<- sum(weight * accuracy, na.rm=TRUE)
  sensitivity.wmacro<- sum(weight * sensitivity, na.rm=TRUE)
  specificity.wmacro<- sum(weight * specificity, na.rm=TRUE)
  precision.wmacro<- sum(weight * precision, na.rm=TRUE)
  F1_score.wmacro<- sum(weight * F1_score, na.rm=TRUE)
  
    print("Weighted Macro Performance Measures:")
   cat("Accuracy:", accuracy.wmacro, "\n")
    cat("Sensitivity:", sensitivity.wmacro, "\n")
     cat("Specificity:", specificity.wmacro, "\n")
      cat("Precision:", precision.wmacro, "\n")
       cat("F1-score:", F1_score.wmacro, "\n")
}


#reading data set
movie.data<- read.csv(file="C:/Users/000110888/OneDrive - CSULB/Desktop/movie_data.csv", 
header=TRUE, sep=",")

#encoding categorical variables into numeric, min-max rescaling
movie.data$gender<- ifelse(movie.data$gender=="M",1,0)
movie.data$member<- ifelse(movie.data$member=="yes",1,0)
movie.data$rating<- as.numeric(ifelse(movie.data$rating=="very bad",1,
ifelse(movie.data$rating=="bad",2, ifelse(movie.data$rating=="okay",3,
ifelse(movie.data$rating=="good",4,5)))))

movie.data$age<- (movie.data$age-min(movie.data$age))/(max(movie.data$age)
-min(movie.data$age))
movie.data$nmovies<- (movie.data$nmovies-min(movie.data$nmovies))/
(max(movie.data$nmovies)-min(movie.data$nmovies))

#creating training and testing sets stratifying by rating
library(caret)
set.seed(187599)
idx<- createDataPartition(movie.data$rating, p=0.8, list=FALSE)
train<- movie.data[idx,]
test<- movie.data[-idx,]

#displaying target value distribution in training and testing sets
cbind(Count=table(train$rating), 
      Percentage=round(prop.table(table(train$rating))*100, 2))
cbind(Count=table(test$rating), 
      Percentage=round(prop.table(table(test$rating))*100, 2))

#separating features and target in training and testing sets
train.x<- data.matrix(train[-5])
train.y<- data.matrix(train[5])
test.x<- data.matrix(test[-5])
test.y<- data.matrix(test[5])

################################################
# FITTING RANDOM FOREST MULTINOMIAL CLASSIFIER #
################################################
library(randomForest)

set.seed(450024)
rf.mclass<- randomForest(as.factor(rating) ~ ., data=train, 
ntree=150, mtry=4, maxnodes=30)

#displaying feature importance
rf_imp<- importance(rf.mclass, type=2)
rf_imp_df<- data.frame(Variable=rownames(rf_imp), 
MeanDecreaseGini=rf_imp[,1], row.names=NULL)

print("Random Forest Multinomial Classifier - Feature Importance:")
rf_imp_df[order(rf_imp_df$MeanDecreaseGini, decreasing=TRUE),]

#computing predicted classes for testing data
pred.class<- as.numeric(predict(rf.mclass, newdata=test))

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("Random Forest Multinomial Classifier - Confusion Matrix:")
conf.mat

#displaying performance measures
print("Random Forest Performance Measures:")
performance.measures(pred.class)

####################################################
# FITTING GRADIENT BOOSTING MULTINOMIAL CLASSIFIER #
####################################################
library(xgboost)

set.seed(558607)

train.y_xgb<- train.y-1 #xgb requires categories 0 through 4
dtrain<- xgb.DMatrix(data=train.x, label=train.y_xgb)
xgb.mclass<- xgb.train(data=dtrain, params=list(num_class=5,
max_depth=6, eta=0.01, objective="multi:softmax"), nrounds=1000)

#displaying feature importance
imp<- xgb.importance(colnames(train.x), model=xgb.mclass)
print("Gradient Boosting Multinomial Classifier - Feature Importance:")
imp[, c("Feature", "Gain")]

#computing predicted classes for testing data
pred.class<- as.numeric(predict(xgb.mclass, test.x))+1

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("Gradient Boosting Multinomial Classifier - Confusion Matrix:")
conf.mat

#displaying performance measures
print("Gradient Boosting Performance Measures:")
performance.measures(pred.class)

####################################################################
# FITTING SUPPORT VECTOR MULTINOMIAL CLASSIFIER WITH LINEAR KERNEL #
####################################################################
library(e1071)

svm.mclass.linear<- svm(x=train.x, y=as.factor(train.y), 
kernel="linear", probability=TRUE)

#displaying permutation feature importance based on average accuracy
baseline<- mean(predict(svm.mclass.linear, train.x)==as.factor(train.y))
perm_imp<- sapply(seq_len(ncol(train.x)), function(j){
  Xp<- train.x
  Xp[,j]<- sample(Xp[,j])
  mean(predict(svm.mclass.linear, Xp)==as.factor(train.y))
})

importance<- data.frame(Variable=colnames(train.x),
Importance=baseline-perm_imp)

print("SVM Linear Kernel) Multinomial Classifier - Feature Importance:")
importance[order(-importance$Importance), ]

#computing predicted classes for testing data
pred.class<- predict(svm.mclass.linear, newdata=test.x)

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("SVM (Linear Kernel) Multinomial Classifier - Confusion Matrix:")
conf.mat

#displaying performance measures
print("SVM (Linear Kernel) Performance Measures:")
performance.measures(pred.class)

########################################################################
# FITTING SUPPORT VECTOR MULTINOMIAL CLASSIFIER WITH POLYNOMIAL KERNEL #
########################################################################
svm.mclass.poly<- svm(x=train.x, y=as.factor(train.y), 
kernel="polynomial", probability=TRUE)

#displaying permutation feature importance based on average accuracy
baseline<- mean(predict(svm.mclass.poly, train.x)==as.factor(train.y))
perm_imp<- sapply(seq_len(ncol(train.x)), function(j){
  Xp<- train.x
  Xp[,j]<- sample(Xp[,j])
  mean(predict(svm.mclass.poly, Xp)==as.factor(train.y))
})

importance<- data.frame(Variable=colnames(train.x),
Importance=baseline-perm_imp)

print("SVM (Polynomial Kernel) Multinomial Classifier - Feature Importance:")
importance[order(-importance$Importance), ]

#computing predicted classes for testing data
pred.class<- predict(svm.mclass.poly, newdata=test.x)

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("SVM (Polynomial Kernel) Multinomial Classifier - Confusion Matrix:")
conf.mat

#displaying performance measures
print("SVM (Polynomial Kernel) Performance Measures:")
performance.measures(pred.class)

####################################################################
# FITTING SUPPORT VECTOR MULTINOMIAL CLASSIFIER WITH RADIAL KERNEL #
####################################################################
svm.mclass.radial<- svm(x=train.x, y=as.factor(train.y), 
kernel="radial", probability=TRUE)

#displaying permutation feature importance based on average accuracy
baseline<- mean(predict(svm.mclass.radial, train.x)==as.factor(train.y))
perm_imp<- sapply(seq_len(ncol(train.x)), function(j){
  Xp<- train.x
  Xp[,j]<- sample(Xp[,j])
  mean(predict(svm.mclass.radial, Xp)==as.factor(train.y))
})

importance<- data.frame(Variable=colnames(train.x),
Importance=baseline-perm_imp)

print("SVM (Radial Kernel) Multinomial Classifier - Feature Importance:")
importance[order(-importance$Importance), ]

#computing predicted classes for testing data
pred.class<- predict(svm.mclass.radial, newdata=test.x)

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("SVM (Radial Kernel) Multinomial Classifier - Confusion Matrix:")
conf.mat

#displaying performance measures
print("SVM (Radial Kernel) Performance Measures:")
performance.measures(pred.class)

#####################################################################
# FITTING SUPPORT VECTOR MULTINOMIAL CLASSIFIER WITH SIGMOID KERNEL #
#####################################################################
svm.mclass.sigmoid<- svm(x=train.x, y=as.factor(train.y), 
kernel="sigmoid", probability=TRUE)

#displaying permutation feature importance based on average accuracy
baseline<- mean(predict(svm.mclass.sigmoid, train.x)==as.factor(train.y))
perm_imp<- sapply(seq_len(ncol(train.x)), function(j){
  Xp<- train.x
  Xp[,j]<- sample(Xp[,j])
  mean(predict(svm.mclass.sigmoid, Xp)==as.factor(train.y))
})

importance<- data.frame(Variable=colnames(train.x),
Importance=baseline-perm_imp)

print("SVM (Sigmoid Kernel) Multinomial Classifier - Feature Importance:")
importance[order(-importance$Importance), ]

#computing predicted classes for testing data
pred.class<- predict(svm.mclass.sigmoid, newdata=test.x)

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("SVM (Sigmoid Kernel) Multinomial Classifier - Confusion Matrix:")
conf.mat

#displaying performance measures
print("SVM (Sigmoid Kernel) Performance Measures:")
performance.measures(pred.class)

#####################################################
# FITTING k-NEAREST NEIGHBOR MULTINOMIAL CLASSIFIER #
#####################################################
library(caret)

set.seed(845445)
#training KNN model -> optimal k=7
print(train(as.factor(rating)~., data=train, method="knn"))

#fitting optimal KNN binary classifier 
knn.mclass<- train(as.factor(rating) ~ ., data=train, method="knn",
tuneGrid=data.frame(k=7))

#displaying permutation feature importance based on average accuracy
baseline<- mean(predict(knn.mclass, train.x)==as.factor(train.y))
perm_imp<- sapply(seq_len(ncol(train.x)), function(j){
  Xp<- train.x
  Xp[,j]<- sample(Xp[,j])
  mean(predict(knn.mclass, Xp)==as.factor(train.y))
})

importance<- data.frame(Variable=colnames(train.x),
Importance=baseline-perm_imp)

print("KNN Multinomial Classifier - Feature Importance:")
importance[order(-importance$Importance), ]

#computing predicted classes for testing data
pred.class<- as.numeric(predict(knn.mclass, newdata=test))

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("KNN Multinomial Classifier - Confusion Matrix:")
conf.mat

#displaying performance measures
print("KNN Performance Measures:")
performance.measures(pred.class)

##############################################
# FITTING NAIVE BAYES MULTINOMIAL CLASSIFIER #
##############################################
library(e1071)

nb.mclass<- naiveBayes(as.factor(rating) ~ . , data=train)

#displaying permutation feature importance based on average accuracy
baseline<- mean(predict(nb.mclass, train.x)==as.factor(train.y))
perm_imp<- sapply(seq_len(ncol(train.x)), function(j){
  Xp<- train.x
  Xp[,j]<- sample(Xp[,j])
  mean(predict(nb.mclass, Xp)==as.factor(train.y))
})

importance<- data.frame(Variable=colnames(train.x),
Importance=baseline-perm_imp)

print("Naive Bayes Multinomial Classifier - Feature Importance:")
importance[order(-importance$Importance), ]

#computing predicted classes for testing data
pred.class<- as.numeric(predict(nb.mclass, newdata=test))

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("Naive Bayes Multinomial Classifier - Confusion Matrix:")
conf.mat

#displaying performance measures
print("Naive Bayes Performance Measures:")
performance.measures(pred.class)

############################################################
# FITTING ARTIFICIAL NEURAL NETWORK MULTINOMIAL CLASSIFIER #
############################################################
library(neuralnet)

set.seed(296707)
ann.mclass<- neuralnet(as.factor(rating) ~ ., data=train, hidden=3, 
linear.output=FALSE)

#plotting the diagram
plot(ann.mclass)

#displaying probability-based permutation feature importance 
baseline_prob<- predict(ann.mclass, train.x)

perm_imp<- sapply(seq_len(ncol(train.x)), function(j){
  Xp<- train.x
  Xp[,j]<- sample(Xp[,j])
  mean(abs(predict(ann.mclass,Xp)-baseline_prob))
})

importance<- data.frame(Variable=colnames(train.x), Importance=perm_imp)

print("ANN Multinomial Classifier - Feature Importance:")
importance[order(-importance$Importance),]

#computing predicted classes for testing data
pred.prob<- predict(ann.mclass, test.x)
pred.class <- max.col(pred.prob) - 1

#displaying confusion matrix 
conf.mat<- table(Predicted=pred.class, Actual=test.y)
print("ANN Multinomial Classifier - Confusion Matrix:")
conf.mat

#displaying performance measures
print("ANN Performance Measures:")
performance.measures(pred.class)

