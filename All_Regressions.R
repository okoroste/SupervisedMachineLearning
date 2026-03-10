library(caret)
library(dplyr)
library(ggplot2)
library(vip)

housing.data<- read.csv(file="C:/Users/000110888/OneDrive - CSULB/Desktop/housing_data.csv", 
header=TRUE, sep=",")

housing.data$ocean_proximity<- ifelse(housing.data$ocean_proximity=='<1H OCEAN',
1, ifelse(housing.data$ocean_proximity=='INLAND',2, 
ifelse(housing.data$ocean_proximity=='NEAR BAY',3,4)))

housing.data$median_house_value<- housing.data$median_house_value/100000
min<- min(housing.data$median_house_value)
max<- max(housing.data$median_house_value)

mins<- sapply(housing.data, min)
maxs<- sapply(housing.data, max)

housing.scaled<- as.data.frame(scale(housing.data, center=mins, scale=maxs-mins))

set.seed(753388)
idx<- createDataPartition(housing.scaled$median_house_value, p=0.8, list=FALSE)
train<- housing.scaled[idx, ]
test<- housing.scaled[-idx, ]

train.x<- train %>% select(-median_house_value)
train.y<- train$median_house_value
test.x<- test  %>% select(-median_house_value)
test.y<- test$median_house_value

####################################
# FITTING RANDOM FOREST REGRESSION #
####################################
library(randomForest)
rf.reg<- randomForest(median_house_value ~ ., data=train, ntree=60, 
mtry=5, maxnodes=100)

#displaying feature importance
imp<- importance(rf.reg, type=2)     
imp.sorted<- data.frame(feature=rownames(imp), importance=imp[,1],
row.names=NULL) %>% arrange(desc(importance))
print("RF Regression - Feature Importance:")
imp.sorted

#computing prediction accuracy for testing data
pred.y<- predict(rf.reg, newdata=test.x)

#rescaling actual and predicted values to the original scale
actual.y<- (max-min)*test.y+min
pred.y<- (max-min)*pred.y+min

#computing prediction accuracy within 10%, 15%, and 20%
accuracy10<- ifelse(abs(actual.y-pred.y)<0.10*actual.y,1,0) 
accuracy15<- ifelse(abs(actual.y-pred.y)<0.15*actual.y,1,0)
accuracy20<- ifelse(abs(actual.y-pred.y)<0.20*actual.y,1,0)

cat("RF Accuracy within 10%:", round(mean(accuracy10), 4), "\n")
cat("RF Accuracy within 15%:", round(mean(accuracy15), 4), "\n")
cat("RF Accuracy within 20%:", round(mean(accuracy20), 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Random Forest 
Regression", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2, 
       col=c("green","dodgerblue"))

########################################
# FITTING GRADIENT BOOSTING REGRESSION #
########################################
library(xgboost)
xgb.reg<- xgboost(train.x, train.y, max_depth=6, learning_rate=0.01, 
nrounds=1000, objective="reg:squarederror") #nrounds=no. of trees

#displaying feature importance
imp<- xgb.importance(feature_names=colnames(train.x), model=xgb.reg)   
imp.sorted<- imp %>% as.data.frame() %>% dplyr::select(Feature, Gain) %>%
arrange(desc(Gain))
print("XGB Regression - Feature Importance:")
imp.sorted

#computing prediction accuracy for testing data
pred.y<- predict(xgb.reg, newdata=test.x)

#rescaling actual and predicted values to the original scale
actual.y<- (max-min)*test.y+min
pred.y<- (max-min)*pred.y+min

#computing prediction accuracy within 10%, 15%, and 20%
accuracy10<- ifelse(abs(actual.y-pred.y)<0.10*actual.y,1,0) 
accuracy15<- ifelse(abs(actual.y-pred.y)<0.15*actual.y,1,0)
accuracy20<- ifelse(abs(actual.y-pred.y)<0.20*actual.y,1,0)

cat("XGB Accuracy within 10%:", round(mean(accuracy10), 4), "\n")
cat("XGB Accuracy within 15%:", round(mean(accuracy15), 4), "\n")
cat("XGB Accuracy within 20%:", round(mean(accuracy20), 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Gradient Boosting 
Regression", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2, 
       col=c("green","dodgerblue"))

########################################################
# FITTING SUPPORT VECTOR REGRESSION WITH LINEAR KERNEL #
########################################################
library(e1071)
svr.linear<- svm(median_house_value ~ ., data=train, kernel="linear") 

#displaying feature importance
w<- t(svr.linear$coefs) %*% svr.linear$SV
imp<- data.frame(feature=colnames(svr.linear$SV), 
importance=as.vector(w))

imp.sorted<- imp %>% mutate(importance=abs(importance)) %>%
arrange(desc(importance))

print("SVR (Linear) - Feature Importance:")
imp.sorted

#computing prediction accuracy for testing data
pred.y<- predict(svr.linear, newdata=test.x)

#rescaling actual and predicted values to the original scale
actual.y<- (max-min)*test.y+min
pred.y<- (max-min)*pred.y+min

#computing prediction accuracy within 10%, 15%, and 20%
accuracy10<- ifelse(abs(actual.y-pred.y)<0.10*actual.y,1,0) 
accuracy15<- ifelse(abs(actual.y-pred.y)<0.15*actual.y,1,0)
accuracy20<- ifelse(abs(actual.y-pred.y)<0.20*actual.y,1,0)

cat("SVR (Linear) Accuracy within 10%:", round(mean(accuracy10), 4), "\n")
cat("SVR (Linear) Accuracy within 15%:", round(mean(accuracy15), 4), "\n")
cat("SVR (Linear) Accuracy within 20%:", round(mean(accuracy20), 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Support Vector Regression
(Linear Kernel)", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2, 
       col=c("green","dodgerblue"))

############################################################
# FITTING SUPPORT VECTOR REGRESSION WITH POLYNOMIAL KERNEL #
############################################################
library(e1071)
svr.poly<- svm(median_house_value ~ ., data=train, kernel="polynomial") 

#displaying feature importance
w<- t(svr.poly$coefs) %*% svr.poly$SV
imp<- data.frame(feature=colnames(svr.poly$SV), 
importance=as.vector(w))

imp.sorted<- imp %>% mutate(importance=abs(importance)) %>%
arrange(desc(importance))

print("SVR (Polynomial) - Feature Importance:")
imp.sorted

#computing prediction accuracy for testing data
pred.y<- predict(svr.poly, newdata=test.x)

#rescaling actual and predicted values to the original scale
actual.y<- (max-min)*test.y+min
pred.y<- (max-min)*pred.y+min

#computing prediction accuracy within 10%, 15%, and 20%
accuracy10<- ifelse(abs(actual.y-pred.y)<0.10*actual.y,1,0) 
accuracy15<- ifelse(abs(actual.y-pred.y)<0.15*actual.y,1,0)
accuracy20<- ifelse(abs(actual.y-pred.y)<0.20*actual.y,1,0)

cat("SVR (Polynomial) Accuracy within 10%:", round(mean(accuracy10), 4), "\n")
cat("SVR (Polynomial) Accuracy within 15%:", round(mean(accuracy15), 4), "\n")
cat("SVR (Polynomial) Accuracy within 20%:", round(mean(accuracy20), 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Support Vector Regression
(Polynomial Kernel)", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2, 
       col=c("green","dodgerblue"))

########################################################
# FITTING SUPPORT VECTOR REGRESSION WITH RADIAL KERNEL #
########################################################
library(e1071)
svr.radial<- svm(median_house_value ~ ., data=train, kernel="radial") 

#displaying feature importance
w<- t(svr.radial$coefs) %*% svr.radial$SV
imp<- data.frame(feature=colnames(svr.radial$SV), importance=as.vector(w))

imp.sorted<- imp %>% mutate(importance=abs(importance)) %>%
arrange(desc(importance))

print("SVR (Radial) - Feature Importance:")
imp.sorted

#computing prediction accuracy for testing data
pred.y<- predict(svr.radial, newdata=test.x)

#rescaling actual and predicted values to the original scale
actual.y<- (max-min)*test.y+min
pred.y<- (max-min)*pred.y+min

#computing prediction accuracy within 10%, 15%, and 20%
accuracy10<- ifelse(abs(actual.y-pred.y)<0.10*actual.y,1,0) 
accuracy15<- ifelse(abs(actual.y-pred.y)<0.15*actual.y,1,0)
accuracy20<- ifelse(abs(actual.y-pred.y)<0.20*actual.y,1,0)

cat("SVR (Radial) Accuracy within 10%:", round(mean(accuracy10), 4), "\n")
cat("SVR (Radial) Accuracy within 15%:", round(mean(accuracy15), 4), "\n")
cat("SVR (Radial) Accuracy within 20%:", round(mean(accuracy20), 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Support Vector Regression
(Radial Kernel)", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2, 
       col=c("green","dodgerblue"))

#########################################################
# FITTING SUPPORT VECTOR REGRESSION WITH SIGMOID KERNEL #
#########################################################
library(e1071)
svr.sigmoid<- svm(median_house_value ~ ., data=train, kernel="sigmoid", scale=FALSE) 

#displaying feature importance
w<- t(svr.sigmoid$coefs) %*% svr.sigmoid$SV
imp<- data.frame(feature=colnames(svr.sigmoid$SV), importance=as.vector(w))

imp.sorted<- imp %>% mutate(importance=abs(importance)) %>%
arrange(desc(importance))

print("SVR (Sigmoid) - Feature Importance:")
imp.sorted

#computing prediction accuracy for testing data
pred.y<- predict(svr.sigmoid, newdata=test.x)

#rescaling actual and predicted values to the original scale
actual.y<- (max-min)*test.y+min
pred.y<- (max-min)*pred.y+min

#computing prediction accuracy within 10%, 15%, and 20%
accuracy10<- ifelse(abs(actual.y-pred.y)<0.10*actual.y,1,0) 
accuracy15<- ifelse(abs(actual.y-pred.y)<0.15*actual.y,1,0)
accuracy20<- ifelse(abs(actual.y-pred.y)<0.20*actual.y,1,0)

cat("SVR (Sigmoid) Accuracy within 10%:", round(mean(accuracy10), 4), "\n")
cat("SVR (Sigmoid) Accuracy within 15%:", round(mean(accuracy15), 4), "\n")
cat("SVR (Sigmoid) Accuracy within 20%:", round(mean(accuracy20), 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Support Vector Regression
(Sigmoid Kernel)", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2, 
col=c("green","dodgerblue"))

#########################################
# FITTING K-NEAREST NEIGHBOR REGRESSION #
#########################################
print(train(median_house_value ~ ., data=train, method="knn"))
#optimal k=9

#fitting optimal KNN regression
knn.reg<- train(median_house_value ~ ., data=train, 
method="knn", tuneGrid=data.frame(k=9))

#displaying feature importance
baseline_pred<- predict(knn.reg, train)
baseline_rmse<- RMSE(baseline_pred, train$median_house_value)

importance<- data.frame(feature=names(train)[-which(names(train)
=="median_house_value")], increase_rmse=NA)

for (i in 1:nrow(importance)) {
  temp_data<- train
   temp_data[[importance$feature[i]]]<- sample(temp_data[[importance$feature[i]]])
  perm_pred<- predict(knn.reg, temp_data)
   perm_rmse<- RMSE(perm_pred, temp_data$median_house_value)
    importance$increase_rmse[i]<- perm_rmse-baseline_rmse
}

importance<- importance[order(-importance$increase_rmse),]

print("KNN Regression - Feature Importance:")
print(importance)

#computing prediction accuracy for testing data
pred.y<- predict(knn.reg, test.x)

#rescaling actual and predicted values to the original scale
actual.y<- (max-min)*test.y+min
pred.y<- (max-min)*pred.y+min

#computing prediction accuracy within 10%, 15%, and 20%
accuracy10<- ifelse(abs(actual.y-pred.y)<0.10*actual.y,1,0) 
accuracy15<- ifelse(abs(actual.y-pred.y)<0.15*actual.y,1,0)
accuracy20<- ifelse(abs(actual.y-pred.y)<0.20*actual.y,1,0)

cat("KNN Accuracy within 10%:", round(mean(accuracy10), 4), "\n")
cat("KNN Accuracy within 15%:", round(mean(accuracy15), 4), "\n")
cat("KNN Accuracy within 20%:", round(mean(accuracy20), 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="K-Nearest Neighbor 
Regression", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2, 
       col=c("green","dodgerblue"))


################################################
# FITTING ARTIFICIAL NEURAL NETWORK REGRESSION #
################################################
library(neuralnet)
ann.reg<- neuralnet(median_house_value ~ ., data=train, hidden=3,
act.fct="logistic", linear.output=TRUE) 

#plotting the diagram
plot(ann.reg)

#displaying feature importance
baseline_pred<- predict(ann.reg, train)
baseline_rmse<- RMSE(baseline_pred, train$median_house_value)

importance<- data.frame(feature=names(train)[-which(names(train)
=="median_house_value")], increase_rmse=NA)

for (i in 1:nrow(importance)) {
  temp_data<- train
  temp_data[[importance$feature[i]]]<- sample(temp_data[[importance$feature[i]]])
  perm_pred<- predict(ann.reg, temp_data)
  perm_rmse<- RMSE(perm_pred, temp_data$median_house_value)
  importance$increase_rmse[i]<- perm_rmse-baseline_rmse
}

importance<- importance[order(-importance$increase_rmse),]

print("ANN Regression - Feature Importance:")
print(importance)

#computing prediction accuracy for testing data
pred.y<- predict(ann.reg, test.x)

#rescaling actual and predicted values to the original scale
actual.y<- (max-min)*test.y+min
pred.y<- (max-min)*pred.y+min

#computing prediction accuracy within 10%, 15%, and 20%
accuracy10<- ifelse(abs(actual.y-pred.y)<0.10*actual.y,1,0) 
accuracy15<- ifelse(abs(actual.y-pred.y)<0.15*actual.y,1,0)
accuracy20<- ifelse(abs(actual.y-pred.y)<0.20*actual.y,1,0)

cat("ANN Accuracy within 10%:", round(mean(accuracy10), 4), "\n")
cat("ANN Accuracy within 15%:", round(mean(accuracy15), 4), "\n")
cat("ANN Accuracy within 20%:", round(mean(accuracy20), 4), "\n")

#plotting actual and predicted values
x<- 1:length(actual.y)
plot(x, actual.y, type="l", lwd=2, col="green", main="Artificial Neural
Network Regression", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2, 
       col=c("green","dodgerblue"))

