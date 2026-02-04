pneumonia.data<- read.csv(file="C:/Users/000110888/OneDrive - CSULB/Desktop/pneumonia_data.csv", 
header=TRUE, sep=",")

#SPLITTING DATA INTO 80\% TRAINING AND 20\% TESTING SETS
set.seed(283605)
sample <- sample(c(TRUE, FALSE), nrow(pneumonia.data), 
 replace=TRUE, prob=c(0.8,0.2))
train<- pneumonia.data[sample,]
test<- pneumonia.data[!sample,]

#FITTING PRUNED BINARY TREE WITH GINI SPLITTING
library(rpart)
tree.gini<- rpart(pneumonia ~ age + gender + tobacco_use + PM2_5, 
data=train, method="class", parms=list(split="Gini"), maxdepth=4)

#COMPUTING CONFUSION MATRICES AND PERFORMANCE MEASURES FOR TESTING SET
#FOR A RANGE OF CUT-OFFS
pred.values<- predict(tree.gini, test)
test<- cbind(test,pred.values)

tpos<- matrix(NA, nrow=nrow(test), ncol=102)
fpos<- matrix(NA, nrow=nrow(test), ncol=102)
tneg<- matrix(NA, nrow=nrow(test), ncol=102)
fneg<- matrix(NA, nrow=nrow(test), ncol=102)


for (i in 0:101) {
  tpos[,i+1]<- ifelse(test$pneumonia=="yes" & test$yes>=0.01*i,1,0)
  fpos[,i+1]<- ifelse(test$pneumonia=="no" & test$yes>=0.01*i, 1,0)
  tneg[,i+1]<- ifelse(test$pneumonia=="no" & test$yes<0.01*i,1,0)
  fneg[,i+1]<- ifelse(test$pneumonia=="yes" & test$yes<0.01*i,1,0)
}

tp<- c()
fp<- c()
tn<- c()
fn<- c()
accuracy<- c()
misclassrate<- c()
sensitivity<- c()
specificity<- c()
oneminusspec<- c()
cutoff<- c()


for (i in 1:102) {
tp[i]<- sum(tpos[,i])
fp[i]<- sum(fpos[,i])
tn[i]<- sum(tneg[,i])
fn[i]<- sum(fneg[,i])
total<- nrow(test)
accuracy[i]<- (tp[i]+tn[i])/total
misclassrate[i]<- (fp[i]+fn[i])/total
sensitivity[i]<- tp[i]/(tp[i]+fn[i])
specificity[i]<- tn[i]/(fp[i]+tn[i])
oneminusspec[i]<- fp[i]/(fp[i]+tn[i])
cutoff[i]<- 0.01*(i-1)
}

#PLOTTING ROC CURVE
plot(oneminusspec, sensitivity, type="l", lty=1, main="The Receiver 
Operating Characteristic Curve", xlab="1-Specificity", ylab="Sensitivity")
points(oneminusspec, sensitivity, pch=0) #pch=plot character, 0=square

#REPORTING MEASURES FOR THE POINT ON ROC CURVE CLOSEST TO THE IDEAL POINT (0,1)
distance<- c()
for (i in 1:102)
  distance[i]<- sqrt(oneminusspec[i]^2+(1-sensitivity[i])^2)

measures<- cbind(accuracy, misclassrate, sensitivity, specificity, distance, cutoff)
min.dist<- min(distance)
print(measures[which(measures[,5]==min.dist),])

#COMPUTING AREA UNDER THE ROC CURVE
sensitivity<- sensitivity[order(sensitivity)]
oneminusspec<- oneminusspec[order(oneminusspec)]

library(Hmisc) #Harrell Miscellaneous packages
lagx<- Lag(oneminusspec,shift=1)
lagy<- Lag(sensitivity, shift=1)
lagx[is.na(lagx)]<- 0
lagy[is.na(lagy)]<- 0
trapezoid<- (oneminusspec-lagx)*(sensitivity+lagy)/2
print(AUC<- sum(trapezoid))

