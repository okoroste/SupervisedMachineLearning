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
        return((x-mn)/(mx-mn))
      }, df, mins, maxs))
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
####################################
# FITTING RANDOM FOREST REGRESSION #
####################################
rf.reg<- randomForest(median_house_value ~ ., data=train,
ntree=60, mtry=5, maxnodes=100)

#displaying feature importance
imp<- importance(rf.reg, type=2)
imp.sorted<- data.frame(feature=rownames(imp), importance=imp[,1],
row.names=NULL) %>% arrange(desc(importance))

print("RF Regression - Feature Importance:")
print(imp.sorted)

#predicting for testing set
pred.y<- predict(rf.reg, newdata=test.x)

actual.y<- unscale_y(test.y, y.min, y.max)
pred.y<- unscale_y(pred.y, y.min, y.max)

#computing prediction accuracy within 10%, 15%, and 20%
acc10<- accuracy_within(actual.y, pred.y, 0.10)
acc15<- accuracy_within(actual.y, pred.y, 0.15)
acc20<- accuracy_within(actual.y, pred.y, 0.20)

cat("RF Accuracy within 10%:", round(acc10, 4), "\n")
cat("RF Accuracy within 15%:", round(acc15, 4), "\n")
cat("RF Accuracy within 20%:", round(acc20, 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Random Forest 
Regression", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("green", "dodgerblue"))

########################################
# FITTING GRADIENT BOOSTING REGRESSION #
########################################
xgb.reg<- xgboost(data=as.matrix(train.x), label=train.y,
max_depth=6, eta=0.01, nrounds=1000, objective="reg:squarederror", 
verbose=0) 

#displaying feature importance
imp<- xgb.importance(feature_names=colnames(train.x), 
model=xgb.reg)
imp.sorted<- imp %>% as.data.frame() %>%
dplyr::select(Feature, Gain) %>% arrange(desc(Gain))

print("XGB Regression - Feature Importance:")
print(imp.sorted)

#predicting for testing set
pred.y<- predict(xgb.reg, newdata=as.matrix(test.x))

actual.y<- unscale_y(test.y, y.min, y.max)
pred.y<- unscale_y(pred.y, y.min, y.max)

#computing prediction accuracy within 10%, 15%, and 20%
acc10<- accuracy_within(actual.y, pred.y, 0.10)
acc15<- accuracy_within(actual.y, pred.y, 0.15)
acc20<- accuracy_within(actual.y, pred.y, 0.20)

cat("XGB Accuracy within 10%:", round(acc10, 4), "\n")
cat("XGB Accuracy within 15%:", round(acc15, 4), "\n")
cat("XGB Accuracy within 20%:", round(acc20, 4), "\n")

#plotting actual and predicted values
x <- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Gradient 
Boosting Regression", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("green", "dodgerblue"))

########################################################
# FITTING SUPPORT VECTOR REGRESSION WITH LINEAR KERNEL #
########################################################
svr.linear<- svm(median_house_value ~ ., data=train, 
kernel="linear", scale=FALSE)

#displaying feature importance
w<- t(svr.linear$coefs) %*% svr.linear$SV
imp<- data.frame(feature=colnames(svr.linear$SV),
importance=as.vector(w))

imp.sorted<- imp %>% mutate(importance=abs(importance)) %>%
arrange(desc(importance))

print("SVR (Linear) - Feature Importance:")
print(imp.sorted)

#predicting for testing set
pred.y<- predict(svr.linear, newdata=test.x)

actual.y<- unscale_y(test.y, y.min, y.max)
pred.y<- unscale_y(pred.y, y.min, y.max)

#computing prediction accuracy within 10%, 15%, and 20%
acc10<- accuracy_within(actual.y, pred.y, 0.10)
acc15<- accuracy_within(actual.y, pred.y, 0.15)
acc20<- accuracy_within(actual.y, pred.y, 0.20)

cat("SVR (Linear) Accuracy within 10%:", round(acc10, 4), "\n")
cat("SVR (Linear) Accuracy within 15%:", round(acc15, 4), "\n")
cat("SVR (Linear) Accuracy within 20%:", round(acc20, 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Support Vector 
Regression (Linear Kernel)", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lw =2,
col=c("green", "dodgerblue"))

############################################################
# FITTING SUPPORT VECTOR REGRESSION WITH POLYNOMIAL KERNEL #
############################################################
svr.poly<- svm(median_house_value ~ ., data=train, 
kernel="polynomial", scale=FALSE)

#displaying permutation-based feature importance
baseline_pred<- predict(svr.poly, test)
baseline_rmse<- RMSE(baseline_pred, test$median_house_value)

importance<- data.frame(
feature=names(test)[names(test)!="median_house_value"],
increase_rmse=NA)

for (i in 1:nrow(importance)) {
temp_data<- test
temp_data[[importance$feature[i]]]<- sample(temp_data[[importance$feature[i]]])
perm_pred<- predict(svr.poly, temp_data)
perm_rmse<- RMSE(perm_pred, temp_data$median_house_value)
importance$increase_rmse[i]<- perm_rmse-baseline_rmse
}

importance<- importance[order(-importance$increase_rmse),]

print("SVR (Polynomial) - Feature Importance:")
print(importance)

#predicting for testing set
pred.y<- predict(svr.poly, newdata=test.x)

actual.y<- unscale_y(test.y, y.min, y.max)
pred.y<- unscale_y(pred.y, y.min, y.max)

#computing prediction accuracy within 10%, 15%, and 20%
acc10<- accuracy_within(actual.y, pred.y, 0.10)
acc15<- accuracy_within(actual.y, pred.y, 0.15)
acc20<- accuracy_within(actual.y, pred.y, 0.20)

cat("SVR (Polynomial) Accuracy within 10%:", round(acc10, 4), "\n")
cat("SVR (Polynomial) Accuracy within 15%:", round(acc15, 4), "\n")
cat("SVR (Polynomial) Accuracy within 20%:", round(acc20, 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Support Vector 
Regression (Polynomial Kernel)", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("green", "dodgerblue"))

########################################################
# FITTING SUPPORT VECTOR REGRESSION WITH RADIAL KERNEL #
########################################################
svr.radial<- svm(median_house_value ~ ., data=train, 
kernel="radial", scale=FALSE)

#displaying permutation-based feature importance
baseline_pred<- predict(svr.radial, test)
baseline_rmse<- RMSE(baseline_pred, test$median_house_value)

importance<- data.frame(
  feature=names(test)[names(test)!="median_house_value"],
  increase_rmse=NA)

for (i in 1:nrow(importance)) {
temp_data<- test
temp_data[[importance$feature[i]]]<- sample(temp_data[[importance$feature[i]]])
perm_pred<- predict(svr.radial, temp_data)
perm_rmse<- RMSE(perm_pred, temp_data$median_house_value)
importance$increase_rmse[i]<- perm_rmse-baseline_rmse
}

importance<- importance[order(-importance$increase_rmse),]

print("SVR (Radial) - Feature Importance:")
print(importance)

#predicting for testing set
pred.y<- predict(svr.radial, newdata=test.x)

actual.y<- unscale_y(test.y, y.min, y.max)
pred.y<- unscale_y(pred.y, y.min, y.max)

#computing prediction accuracy within 10%, 15%, and 20%
acc10<- accuracy_within(actual.y, pred.y, 0.10)
acc15<- accuracy_within(actual.y, pred.y, 0.15)
acc20<- accuracy_within(actual.y, pred.y, 0.20)

cat("SVR (Radial) Accuracy within 10%:", round(acc10, 4), "\n")
cat("SVR (Radial) Accuracy within 15%:", round(acc15, 4), "\n")
cat("SVR (Radial) Accuracy within 20%:", round(acc20, 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Support Vector 
Regression (Radial Kernel)", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("green", "dodgerblue"))

#########################################################
# FITTING SUPPORT VECTOR REGRESSION WITH SIGMOID KERNEL #
#########################################################
svr.sigmoid<- svm(median_house_value ~ ., data=train, 
kernel="sigmoid", scale=FALSE)

#displaying permutation-based feature importance
baseline_pred<- predict(svr.sigmoid, test)
baseline_rmse<- RMSE(baseline_pred, test$median_house_value)

importance<- data.frame(
  feature=names(test)[names(test)!="median_house_value"],
  increase_rmse=NA)

for (i in 1:nrow(importance)) {
temp_data<- test
temp_data[[importance$feature[i]]]<- sample(temp_data[[importance$feature[i]]])
perm_pred<- predict(svr.sigmoid, temp_data)
perm_rmse<- RMSE(perm_pred, temp_data$median_house_value)
importance$increase_rmse[i]<- perm_rmse-baseline_rmse
}

importance<- importance[order(-importance$increase_rmse),]

print("SVR (Sigmoid) - Feature Importance:")
print(importance)

#predicting for testing set
pred.y<- predict(svr.sigmoid, newdata=test.x)

actual.y<- unscale_y(test.y, y.min, y.max)
pred.y<- unscale_y(pred.y, y.min, y.max)

#computing prediction accuracy within 10%, 15%, and 20%
acc10<- accuracy_within(actual.y, pred.y, 0.10)
acc15<- accuracy_within(actual.y, pred.y, 0.15)
acc20<- accuracy_within(actual.y, pred.y, 0.20)

cat("SVR (Sigmoid) Accuracy within 10%:", round(acc10, 4), "\n")
cat("SVR (Sigmoid) Accuracy within 15%:", round(acc15, 4), "\n")
cat("SVR (Sigmoid) Accuracy within 20%:", round(acc20, 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Support Vector 
Regression (Sigmoid Kernel)", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("green", "dodgerblue"))

#########################################
# FITTING K-NEAREST NEIGHBOR REGRESSION #
#########################################
set.seed(402301)
knn.reg<- train(median_house_value ~ ., data=train, method="knn")

#displaying permutation-based feature importance
baseline_pred<- predict(knn.reg, test)
baseline_rmse<- RMSE(baseline_pred, test$median_house_value)

importance<- data.frame(
 feature=names(test)[names(test)!="median_house_value"],
 increase_rmse=NA
)

for (i in 1:nrow(importance)) {
temp_data<- test
temp_data[[importance$feature[i]]]<- sample(temp_data[[importance$feature[i]]])
perm_pred<- predict(knn.reg, temp_data)
perm_rmse<- RMSE(perm_pred, temp_data$median_house_value)
importance$increase_rmse[i]<- perm_rmse-baseline_rmse
}

importance<- importance[order(-importance$increase_rmse),]

print("KNN Regression - Feature Importance:")
print(importance)

#predicting for testing set
pred.y<- predict(knn.reg, test.x)

actual.y<- unscale_y(test.y, y.min, y.max)
pred.y<- unscale_y(pred.y, y.min, y.max)

#computing prediction accuracy within 10%, 15%, and 20%
acc10<- accuracy_within(actual.y, pred.y, 0.10)
acc15<- accuracy_within(actual.y, pred.y, 0.15)
acc20<- accuracy_within(actual.y, pred.y, 0.20)

cat("KNN Accuracy within 10%:", round(acc10, 4), "\n")
cat("KNN Accuracy within 15%:", round(acc15, 4), "\n")
cat("KNN Accuracy within 20%:", round(acc20, 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="K-Nearest 
Neighbor Regression", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("green", "dodgerblue"))

################################################
# FITTING ARTIFICIAL NEURAL NETWORK REGRESSION #
################################################
ann.reg<- neuralnet(median_house_value ~ ., data=train,
hidden=3, act.fct="logistic", linear.output=TRUE)

#plotting fitted neural net
plot(ann.reg)

#displaying permutation-based feature importance
baseline_pred<- predict(ann.reg, test)
baseline_rmse<- RMSE(as.vector(baseline_pred), test$median_house_value)

importance<- data.frame(
  feature=names(test)[names(test)!="median_house_value"],
  increase_rmse=NA)

for (i in 1:nrow(importance)) {
temp_data<- test
temp_data[[importance$feature[i]]]<- sample(temp_data[[importance$feature[i]]])
perm_pred<- predict(ann.reg, temp_data)
perm_rmse<- RMSE(as.vector(perm_pred), temp_data$median_house_value)
importance$increase_rmse[i]<- perm_rmse-baseline_rmse
}

importance<- importance[order(-importance$increase_rmse),]

print("ANN Regression - Feature Importance:")
print(importance)

#predicting for testing set
pred.y<- predict(ann.reg, test.x)
pred.y<- as.vector(pred.y)

actual.y<- unscale_y(test.y, y.min, y.max)
pred.y<- unscale_y(pred.y, y.min, y.max)

#computing prediction accuracy within 10%, 15%, and 20%
acc10<- accuracy_within(actual.y, pred.y, 0.10)
acc15<- accuracy_within(actual.y, pred.y, 0.15)
acc20<- accuracy_within(actual.y, pred.y, 0.20)

cat("ANN Accuracy within 10%:", round(acc10, 4), "\n")
cat("ANN Accuracy within 15%:", round(acc15, 4), "\n")
cat("ANN Accuracy within 20%:", round(acc20, 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Artificial Neural 
Network Regression", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("green", "dodgerblue"))