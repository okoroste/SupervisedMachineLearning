pneumonia.data<- read.csv(file="C:/Users/000110888/OneDrive - CSULB/Desktop/pneumonia_data.csv", 
header=TRUE, sep=",")

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(283605)
library(caret)
sample<- createDataPartition(pneumonia.data$pneumonia, p=0.8, list=FALSE)
train<- pneumonia.data[sample,]
test<- pneumonia.data[-sample,]

#FITTING PRUNED BINARY TREE WITH GINI SPLITTING 
library(rpart)
tree.gini<- rpart(pneumonia ~ age + gender + tobacco_use + PM2_5, 
data=train, method="class", parms=list(split="Gini"), maxdepth=4)

library(rpart.plot)
rpart.plot(tree.gini, type=3)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
pred.values<- predict(tree.gini, test)
test<- cbind(test,pred.values)

tp<- matrix(NA, nrow=nrow(test), ncol=99)
tn<- matrix(NA, nrow=nrow(test), ncol=99)

for (i in 1:99) {
  tp[,i]<- ifelse(test$pneumonia=="yes" & test$yes>0.01*i,1,0)
  tn[,i]<- ifelse(test$pneumonia=="no" & test$yes<=0.01*i,1,0)
}

trueclassrate<- matrix(NA, nrow=99, ncol=2)
for (i in 1:99){
  trueclassrate[i,1]<- 0.01*i
  trueclassrate[i,2]<- sum(tp[,i]+tn[,i])/nrow(test)
}

print(trueclassrate[which(trueclassrate[,2]==max(trueclassrate[,2])),])
      
      
#FITTING PRUNED BINARY TREE WITH ENTROPY SPLITTING 
library(rpart)
tree.entropy<- rpart(pneumonia ~ age + gender + tobacco_use + PM2_5, 
data=train, method="class", parms=list(split="entropy"), maxdepth=4)

library(rpart.plot)
rpart.plot(tree.entropy, type=3)
#Note: same tree as gini

#FITTING PRUNED BINARY TREE WITH CHAID SPLITTING  
#BINNING CONTINUOUS PREDICTOR VARIABLES 
library(dplyr)
pneumonia.data<- mutate(pneumonia.data, age.cat=ntile(age,10),
PM2_5.cat=ntile(PM2_5,10))

#CREATING INDICATORS FOR CATEGORICAL VARIABLES 
pneumonia.data$male<- ifelse(pneumonia.data$gender=="M",1,0)
pneumonia.data$tobacco.yes<- ifelse(pneumonia.data$tobacco_use=="yes",1,0)

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS  
set.seed(283605)
library(caret)
sample<- createDataPartition(pneumonia.data$pneumonia, p=0.8, list=FALSE)
train<- pneumonia.data[sample,]
test<- pneumonia.data[-sample,]

#FITTING BINARY CLASSIFICATION TREE
library(partykit)
tree.CHAID<-ctree(as.factor(pneumonia) ~ as.factor(age.cat) + as.factor(male) 
+ as.factor(tobacco.yes) + as.factor(PM2_5.cat), data=train)

#PLOTTING FITTED TREE
plot(tree.CHAID)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
pred.pneumonia<- predict(tree.CHAID, newdata=test)
test<- cbind(test,pred.pneumonia)

truepred<- c()
for (i in 1:nrow(test))
  truepred[i]<- ifelse(test$pneumonia[i]==test$pred.pneumonia[i],1,0)

print(truepredrate<- mean(truepred))


