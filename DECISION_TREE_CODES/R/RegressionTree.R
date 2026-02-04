housing.data<- read.csv(file="C:/Users/000110888/OneDrive - CSULB/Desktop/housing_data.csv", 
header=TRUE, sep=",")

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(105388)
library(caret)
sample<- createDataPartition(housing.data$housing_median_age, p=0.8, list=FALSE)
train<- housing.data[sample,]
test<- housing.data[-sample,]

#FITTING FULL REGRESSION TREE WITH RSS SPLITTING 
#install.packages("rpart")
library(rpart)#recursive partitioning and regression trees
reg.tree.full<- rpart(median_house_value ~ housing_median_age 
+ total_rooms	+ total_bedrooms + population + households	
+ median_income + ocean_proximity, data=train,
method="anova", xval=10, cp=0) #xval=# of cross-validations

printcp(reg.tree.full)

#FITTING REGRESSION TREE WITH RSS SPLITTING AND COST-COMPLEXITY PRUNING 
reg.tree.RSS<- rpart(median_house_value ~ housing_median_age 
+ total_rooms	+ total_bedrooms + population + households	
+ median_income + ocean_proximity, data=train,
method="anova", cp=0.026) #pruned tree with 5 leaves (cp=alpha=complexity parameter,
#R_alpha(T)=R(T)+alpha |T|, R=RSS, |T|=no. of leaves

#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(reg.tree.RSS, type=3)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
P_median_house_value<- predict(reg.tree.RSS, newdata=test)

#accuracy within 10%
accuracy10<- ifelse(abs(test$median_house_value-P_median_house_value)<0.10*test$median_house_value,1,0) 
print(mean(accuracy10))

#accuracy within 15%
accuracy15<- ifelse(abs(test$median_house_value-P_median_house_value)<0.15*test$median_house_value,1,0)
print(mean(accuracy15))

#accuracy within 20%
accuracy20<- ifelse(abs(test$median_house_value-P_median_house_value)<0.20*test$median_house_value,1,0)
print(mean(accuracy20))

#########################################################################################

#FITTING REGRESSION TREE WITH CHAID SPLITTING AND COST-COMPLEXITY PRUNING 

#BINNING CONTINOUS PREDICTOR VARIABLES 
#install.packages("dplyr")
library(dplyr)

housing.data<- mutate(housing.data, housing_median_age_cat=ntile(housing_median_age,10),
total_rooms_cat=ntile(total_rooms,10), total_bedrooms_cat=ntile(total_bedrooms,10),
population_cat=ntile(population,10), households_cat=ntile(households,10),	
median_income_cat=ntile(median_income,10), median_house_value_cat=ntile(median_house_value,10))

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(105388)
#install.packages("caret") 
library(caret)    
sample<- createDataPartition(housing.data$housing_median_age, p=0.8, list=FALSE)
train<- housing.data[sample,]
test<- housing.data[-sample,]

#install.packages("partykit")
library(partykit)

reg.tree.CHAID<- ctree(as.factor(median_house_value_cat) ~ as.factor(housing_median_age_cat) + as.factor(total_rooms_cat) 
+ as.factor(total_bedrooms_cat) + as.factor(population_cat) + as.factor(households_cat) + as.factor(median_income_cat) 
+ as.factor(ocean_proximity), data=train)

plot(reg.tree.CHAID)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
predclass<- as.numeric(predict(reg.tree.CHAID, newdata=test))
test<- cbind(test,predclass)

#computing predicted values, mean values per decile in the training set 
aggr.data<- aggregate(train$median_house_value, by=list(train$median_house_value_cat), 
FUN=mean)
#combining observed and predicted values in the testing set
aggr.data$predclass<- aggr.data$Group.1
aggr.data$P_median_house_value<- aggr.data$x
test<- left_join(test, aggr.data, by='predclass')

#accuracy within 10%
accuracy10<- ifelse(abs(test$median_house_value-test$P_median_house_value)<0.10*test$median_house_value,1,0) 
print(mean(accuracy10))

#accuracy within 15%
accuracy15<- ifelse(abs(test$median_house_value-test$P_median_house_value)<0.15*test$median_house_value,1,0)
print(mean(accuracy15))

#accuracy within 20%
accuracy20<- ifelse(abs(test$median_house_value-test$P_median_house_value)<0.20*test$median_house_value,1,0)
print(mean(accuracy20))



