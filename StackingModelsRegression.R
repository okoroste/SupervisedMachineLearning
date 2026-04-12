library(caret)
library(dplyr)
library(randomForest)
library(xgboost)
library(e1071)
library(neuralnet)

housing.data<- read.csv(file="C:/Users/000110888/OneDrive - CSULB/Desktop/housing_data.csv",
header=TRUE, sep=",")

#encoding ocean_proximity
housing.data$ocean_proximity<- ifelse(
  housing.data$ocean_proximity=="<1H OCEAN", 1,
  ifelse(housing.data$ocean_proximity=="INLAND", 2,
         ifelse(housing.data$ocean_proximity=="NEAR BAY", 3, 4)))

#scaling target variable
housing.data$median_house_value<- housing.data$median_house_value/100000

##########################################
# CREATING 80%/20% TRAINING/TESTING SETS # 
##########################################
set.seed(753388)
idx<- createDataPartition(housing.data$median_house_value, p=0.8,
list=FALSE)

train.raw<- housing.data[idx,]
test.raw<- housing.data[-idx,]

############################################################
# MIN-MAX SCALING OF ALL VARIABLES USING TRAINING SET ONLY #
############################################################
train.mins<- sapply(train.raw, min)
train.maxs<- sapply(train.raw, max)

min_max_scale<- function(df, mins, maxs) {
  scaled.df<- as.data.frame(mapply(
    function(x, mn, mx) {
      if (mx==mn) {
        rep(0, length(x))
      } else {
        (x-mn)/(mx-mn)
      }
    }, df, mins, maxs, SIMPLIFY=FALSE))
  
  names(scaled.df)<- names(df)
  scaled.df
}

train<- min_max_scale(train.raw, train.mins, train.maxs)
test<- min_max_scale(test.raw, train.mins, train.maxs)

#storing train-set target scaling values for inverse transformation
y.min<- train.mins["median_house_value"]
y.max<- train.maxs["median_house_value"]

unscale_y<- function(y.scaled, y.min, y.max) {
  y.scaled*(y.max-y.min)+y.min
}

train.x<- train %>% select(-median_house_value)
train.y<- train$median_house_value
test.x<- test %>% select(-median_house_value)
test.y<- test$median_house_value

#defining function to compute accuracy withing threshold
accuracy_within<- function(actual, predicted, pct) {
mean(abs(actual-predicted)<pct*actual)
}

#creating out-of-fold sets
set.seed(559702)
folds<- createFolds(train.y, k=5)

oof.rf<- rep(NA, nrow(train))
oof.xgb<- rep(NA, nrow(train))
oof.svr.linear<- rep(NA, nrow(train))
oof.svr.radial<- rep(NA, nrow(train))
oof.knn<- rep(NA, nrow(train))
oof.ann<- rep(NA, nrow(train))

for (i in seq_along(folds)) {
  valid_idx<- folds[[i]]
  fold_train<- train[-valid_idx, ]
  fold_valid<- train[valid_idx, ]
  
  fold_train.x<- fold_train %>% select(-median_house_value)
  fold_train.y<- fold_train$median_house_value
  fold_valid.x<- fold_valid %>% select(-median_house_value)

#training random forest regression
model.rf<- randomForest(median_house_value ~ ., data=fold_train,
ntree=60, mtry=5, maxnodes=100)
oof.rf[valid_idx]<- predict(model.rf, newdata=fold_valid.x)
  
#training gradient boosting regression
model.xgb<- xgboost(data=as.matrix(fold_train.x), label=fold_train.y,
max_depth=6, eta=0.01, nrounds=1000, objective="reg:squarederror")
oof.xgb[valid_idx]<- predict(model.xgb, newdata=as.matrix(fold_valid.x))
  
#training support vector regression with linear kernel
model.svr.linear<- svm(median_house_value ~ ., data=fold_train,
kernel="linear", scale=FALSE)
oof.svr.linear[valid_idx]<- predict(model.svr.linear, newdata=fold_valid.x)
  
#training support vector regression with polynomial kernel
model.svr.radial<- svm(median_house_value ~ ., data=fold_train,
kernel="radial", scale=FALSE)
oof.svr.radial[valid_idx]<- predict(model.svr.radial, newdata=fold_valid.x)
  
#training k-nearest neighbor regression
model.knn<- train(median_house_value ~ ., data=fold_train,
method="knn", trControl=trainControl(method="cv", number=5))
oof.knn[valid_idx]<- predict(model.knn, newdata=fold_valid.x)
  
#training artificial neural network
model.ann <- neuralnet(median_house_value ~ ., data=fold_train,
hidden=3, act.fct="logistic", linear.output=TRUE)

ann.pred<- tryCatch({as.vector(compute(model.ann, fold_valid.x)$net.result)
}, error=function(e) {
  rep(mean(fold_train$median_house_value), nrow(fold_valid.x))
})

oof.ann[valid_idx]<- ann.pred

}

#training meta-model 
stack_train<- data.frame(rf=oof.rf, xgb=oof.xgb, svr_linear=oof.svr.linear,
svr_radial=oof.svr.radial, knn=oof.knn, ann=oof.ann, 
median_house_value=train.y)

meta.model<- lm(median_house_value ~ ., data=stack_train)

#training models on full training set 
rf.reg<- randomForest(median_house_value ~ ., data=train, ntree=60, 
mtry=5, maxnodes=100)

xgb.reg<- xgboost(data=as.matrix(train.x), label=train.y, max_depth=6,
eta=0.01, nrounds=1000, objective="reg:squarederror", verbose=0)

svr.linear<- svm(median_house_value ~ ., data=train, kernel="linear",
scale=FALSE)

svr.radial<- svm(median_house_value ~ ., data=train, kernel="radial",
scale=FALSE)

set.seed(402301)
knn.reg<- train(median_house_value ~ ., data=train, method="knn",
trControl=trainControl(method="cv", number=5))

ann.reg<- neuralnet(median_house_value ~ ., data=train, hidden=3, 
act.fct="logistic", linear.output=TRUE)

#predicting on test set 
test.rf<- predict(rf.reg, newdata=test.x)
test.xgb<- predict(xgb.reg, newdata=as.matrix(test.x))
test.svr.linear<- predict(svr.linear, newdata=test.x)
test.svr.radial<- predict(svr.radial, newdata=test.x)
test.knn<- predict(knn.reg, newdata=test.x)
test.ann<- as.vector(predict(ann.reg, test.x))

stack_test<- data.frame(rf=test.rf, xgb=test.xgb, 
svr_linear=test.svr.linear, svr_radial=test.svr.radial,
knn=test.knn, ann=test.ann)

#computing stacked predictions
stack.pred.scaled<- predict(meta.model, newdata=stack_test)

actual.y<- unscale_y(test.y, y.min, y.max)
pred.y<- unscale_y(stack.pred.scaled, y.min, y.max)

#computing accuracy within 10%, 15%, and 20%
acc10<- accuracy_within(actual.y, pred.y, 0.10)
acc15<- accuracy_within(actual.y, pred.y, 0.15)
acc20<- accuracy_within(actual.y, pred.y, 0.20)

cat("Stacking Accuracy within 10%:", round(acc10, 4), "\n")
cat("Stacking Accuracy within 15%:", round(acc15, 4), "\n")
cat("Stacking Accuracy within 20%:", round(acc20, 4), "\n")

#combining outputs into one table

#unscaling individual model predictions
pred.rf<- unscale_y(test.rf, y.min, y.max)
pred.xgb<- unscale_y(test.xgb, y.min, y.max)
pred.svr.linear<- unscale_y(test.svr.linear, y.min, y.max)
pred.svr.radial<- unscale_y(test.svr.radial, y.min, y.max)
pred.knn<- unscale_y(test.knn, y.min, y.max)
pred.ann<- unscale_y(test.ann, y.min, y.max)

#unscaling stacked predictions
stack.pred <- unscale_y(stack.pred.scaled, y.min, y.max)

#combining outputs into one table
results<- data.frame(Model=c("Random Forest", "XGBoost", "SVR Linear",
"SVR Radial", "KNN", "ANN", "Stacked"),
  Acc_10=c(accuracy_within(actual.y, pred.rf, 0.10),
    accuracy_within(actual.y, pred.xgb, 0.10),
    accuracy_within(actual.y, pred.svr.linear, 0.10),
    accuracy_within(actual.y, pred.svr.radial, 0.10),
    accuracy_within(actual.y, pred.knn, 0.10),
    accuracy_within(actual.y, pred.ann, 0.10),
    accuracy_within(actual.y, stack.pred, 0.10)),
  
  Acc_15=c(accuracy_within(actual.y, pred.rf, 0.15),
    accuracy_within(actual.y, pred.xgb, 0.15),
    accuracy_within(actual.y, pred.svr.linear, 0.15),
    accuracy_within(actual.y, pred.svr.radial, 0.15),
    accuracy_within(actual.y, pred.knn, 0.15),
    accuracy_within(actual.y, pred.ann, 0.15),
    accuracy_within(actual.y, stack.pred, 0.15)),
  
  Acc_20=c(accuracy_within(actual.y, pred.rf, 0.20),
    accuracy_within(actual.y, pred.xgb, 0.20),
    accuracy_within(actual.y, pred.svr.linear, 0.20),
    accuracy_within(actual.y, pred.svr.radial, 0.20),
    accuracy_within(actual.y, pred.knn, 0.20),
    accuracy_within(actual.y, pred.ann, 0.20),
    accuracy_within(actual.y, stack.pred, 0.20)))

results<- results %>% mutate(Acc_10=round(Acc_10, 4), 
Acc_15=round(Acc_15, 4), Acc_20=round(Acc_20, 4))

print(results)