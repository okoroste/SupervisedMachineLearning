movie.data<- read.csv(file="C:/Users/000110888/OneDrive - CSULB/Desktop/movie_data.csv", 
header=TRUE, sep=",")

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(566222)
sample <- sample(c(TRUE, FALSE), nrow(movie.data),replace=TRUE, prob=c(0.8,0.2))
train<- movie.data[sample,]
test<- movie.data[!sample,]

#FITTING PRUNED MULTINOMIAL CLASSIFICATION TREE WITH GINI SPLITTING 
library(rpart)
tree.gini<- rpart(rating ~ age + gender + member + nmovies, 
data=train, method="class", parms=list(split="Gini"), maxdepth=4)

#PLOTTING FITTED TREE
library(rpart.plot)
rpart.plot(tree.gini, type=3)

#COMPUTING PREDICTED VALUES FOR TESTING DATA
pred.values<- predict(tree.gini, test)
pred.values

#DETERMINING PREDICTED CLASSES
test<- cbind(test, pred.values)
test$maxprob<- pmax(test$'very bad',test$'bad',test$'okay',
test$'good',test$'very good')

test$predclass<- ifelse(test$maxprob==test$'very bad', 'very bad', 
ifelse(test$maxprob==test$'bad','bad',
ifelse(test$maxprob==test$'okay','okay',
ifelse(test$maxprob==test$'good','good','very good'))))

#DEFINING FUNCTION FOR COMPUTING PERFORMANCE MEASURES
perf.measures<- function() {
  
#COMPUTING PERFORMANCE MEASURES FOR INDIVIDUAL CLASSES

tp<- c()
fp<- c()
tn<- c()
fn<- c()
accuracy<- c()
misclassrate<- c()
sensitivity<- c()
FNR<- c()
specificity<- c()
FPR<- c()
precision<- c()
NPV<- c()
F1score<- c()

class.metrics<- function(class) {
  
  tp.class<- ifelse(test$predclass==class & test$rating==class,1,0)
  fp.class<- ifelse(test$predclass==class & test$rating!=class,1,0)
  tn.class<- ifelse(test$predclass!=class & test$rating!=class,1,0)
  fn.class<- ifelse(test$predclass!=class & test$rating==class,1,0)

  message('CLASS MEASURES:')
  message('class:', class)
  print(paste('tp:', tp[class]<<- sum(tp.class))) 
  #<<- is global assignment, works outside the function
  print(paste('fp:', fp[class]<<- sum(fp.class)))
  print(paste('tn:', tn[class]<<- sum(tn.class)))
  print(paste('fn:', fn[class]<<- sum(fn.class)))
  total<<- nrow(test)

  print(paste('accuracy:', accuracy[class]<<- (tp[class]+tn[class])/total))
  print(paste('misclassrate:', misclassrate[class]<<- (fp[class]+fn[class])/total))
  print(paste('sensitivity:', sensitivity[class]<<- tp[class]/(tp[class]+fn[class])))
  print(paste('FNR:', FNR[class]<<- fn[class]/(tp[class]+fn[class])))
  print(paste('specificity:', specificity[class]<<- tn[class]/(fp[class]+tn[class])))
  print(paste('FPR:', FPR[class]<<- fp[class]/(fp[class]+tn[class])))
  print(paste('precision:', precision[class]<<- tp[class]/(tp[class]+fp[class])))
  print(paste('NPV:', NPV[class]<<- tn[class]/(fn[class]+tn[class])))
  print(paste('F1score:', F1score[class]<<- 2*tp[class]/(2*tp[class]+fn[class]+fp[class])))
}

class.metrics(class='very bad')
class.metrics(class='bad')
class.metrics(class='okay')
class.metrics(class='good')
class.metrics(class='very good')

#COMPUTING MICRO MEASURES 
tp.sum<- sum(tp)
fp.sum<- sum(fp)
tn.sum<- sum(tn)
fn.sum<- sum(fn)

message('MICRO MEASURES:')
print(paste('accuracy:', accuracy.micro<- (tp.sum+tn.sum)/(tp.sum+fp.sum+tn.sum+fn.sum)))
print(paste('misclassrate:', misclassrate.micro<- (fp.sum+fn.sum)/(tp.sum+fp.sum+tn.sum+fn.sum)))
print(paste('sensitivity:', sensitivity.micro<- tp.sum/(tp.sum+fn.sum)))
print(paste('FNR:', FNR.micro<- fn.sum/(tp.sum+fn.sum)))
print(paste('specificity:', specificity.micro<- tn.sum/(fp.sum+tn.sum)))
print(paste('FPR:', FPR.micro<- fp.sum/(fp.sum+tn.sum)))
print(paste('precision:', precision.micro<- tp.sum/(tp.sum+fp.sum)))
print(paste('NPV:', NPV.micro<- tn.sum/(fn.sum+tn.sum)))
print(paste('F1-score:', F1score.micro<- 2*tp.sum/(2*tp.sum+fn.sum+fp.sum)))

#COMPUTING MACRO MEASURES
message('MACRO MEASURES:')
print(paste('accuracy:', accuracy.macro<- mean(accuracy)))
print(paste('misclassrate:', misclassrate.macro<- mean(misclassrate)))
print(paste('sensitivity:', sensitivity.macro<- mean(sensitivity)))
print(paste('FNR:', FNR.macro<- mean(FNR)))
print(paste('specificity:', specificity.macro<- mean(specificity)))
print(paste('FPR:', FPR.macro<- mean(FPR)))
print(paste('precision:', precision.macro<- mean(precision, na.rm=TRUE)))
print(paste('NPV:', NPV.macro<- mean(NPV)))
print(paste('F1-score:', F1score.macro<- mean(F1score)))

#COMPUTING WEIGHTED MACRO MEASURES
weight<- c()

for (class in 1:5) 
weight[class]<- (tp[class]+fn[class])/total

message('WEIGHTED MACRO MEASURES:')
print(paste('accuracy:', accuracy.wmacro<- weight%*%accuracy))
print(paste('misclassrate:', misclassrate.wmacro<- weight%*%misclassrate))
print(paste('sensitivity:', sensitivity.wmacro<- weight%*%sensitivity))
print(paste('FNR:', FNR.wmacro<- weight%*%FNR))
print(paste('specificity:', specificity.wmacro<- weight%*%specificity))
print(paste('FPR:', FPR.wmacro<- weight%*%FPR))
precision[is.na(precision)]<- 0
print(paste('precision:', precision.wmacro<- weight%*%precision))
print(paste('NPV:', NPV.wmacro<- weight%*%NPV))
print(paste('F1-score:', F1score.wmacro<- weight%*%F1score))

}

#COMPUTING PERFORMANCE MEASURES FOR FITTED GINI TREE
perf.measures()

############################################################################

#FITTING PRUNED MULTINOMIAL CLASSIFICATION TREE WITH ENTROPY SPLITTING 
library(rpart)
tree.entropy<- rpart(rating~age + gender + member + nmovies, 
data=train, method="class", parms=list(split="entropy"), maxdepth=4)

#PLOTTING FITTED TREE
rpart.plot(tree.entropy, type=3)
#Note: same as tree.gini

############################################################################

#FITTING PRUNED MULTINOMIAL CLASSIFICATION TREE WITH CHAID SPLITTING  
#BINNING CONTINUOUS PREDICTOR VARIABLES 
library(dplyr)
movie.data<- mutate(movie.data, age.cat=ntile(age,10))

#CREATING INDICATORS FOR CATEGORICAL VARIABLES 
movie.data$male<- ifelse(movie.data$gender=="M",1,0)
movie.data$member.yes<- ifelse(movie.data$member=="yes",1,0)

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS  
set.seed(566222)
sample <- sample(c(TRUE, FALSE), nrow(movie.data), 
replace=TRUE, prob=c(0.8,0.2))
train<- movie.data[sample,]
test<- movie.data[!sample,]

#FITTING MULTINOMIAL CLASSIFICATION TREE WITH CHAID SPLITTING
library(CHAID)
tree.CHAID<- chaid(as.factor(rating) ~ as.factor(age.cat) + 
as.factor(male) + as.factor(member.yes) + as.factor(nmovies), 
data=train)

#PLOTTING FITTED TREE
plot(tree.CHAID, type="simple")

#COMPUTING PREDICTED VALUES FOR TESTING DATA 
pred.values<- predict(tree.CHAID, newdata=test)
pred.values

#COMPUTING PERFORMANCE MEASURES FOR FITTED CHAID TREE
test$predclass<- pred.values
perf.measures()

