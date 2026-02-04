pneumonia.data<- read.csv(file="C:/Users/000110888/OneDrive - CSULB/Desktop/pneumonia_data.csv", 
header=TRUE, sep=",")

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
#set.seed(283600)
sample <- sample(c(TRUE, FALSE), nrow(pneumonia.data), 
replace=TRUE, prob=c(0.8,0.2))
train<- pneumonia.data[sample,]
test<- pneumonia.data[!sample,]
nrow(test)

#FITTING PRUNED BINARY TREE WITH GINI SPLITTING 
library(rpart)
tree.gini<- rpart(pneumonia ~ age + gender + tobacco_use + PM2_5, 
data=train, method="class", parms=list(split="Gini"), maxdepth=4)

#COMPUTING CONFUSION MATRIX AND PERFORMANCE MEASURES FOR TESTING DATA
pred.values<- predict(tree.gini, test)
test<- cbind(test,pred.values)

tp<- c()
fp<- c()
tn<- c()
fn<- c()

total<- nrow(test)
for (i in 1:total){
  tp[i]<- ifelse(test$yes[i]>0.5 & test$pneumonia[i]=="yes",1,0)
  fp[i]<- ifelse(test$yes[i]>0.5 & test$pneumonia[i]=="no",1,0)
  tn[i]<- ifelse(test$no[i]>0.5 & test$pneumonia[i]=="no",1,0)
  fn[i]<- ifelse(test$no[i]>0.5 & test$pneumonia[i]=="yes",1,0)
}

print(tp<- sum(tp))
print(fp<- sum(fp))
print(tn<- sum(tn))
print(fn<- sum(fn))
print(total)

print(accuracy<- (tp+tn)/total)
print(misclassrate<- (fp+fn)/total)
print(sensitivity<- tp/(tp+fn))
print(FNR<- fn/(tp+fn))
print(specificity<- tn/(fp+tn))
print(FPR<- fp/(fp+tn))
print(precision<- tp/(tp+fp))
print(NPV<- tn/(fn+tn))
print(F1score<- 2*tp/(2*tp+fn+fp))


