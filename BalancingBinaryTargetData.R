#simulation of a data set with binary target variable 
#with classes "majority" and "minority"
n_majority<- 1000
n_minority<- 200

df_majority<- data.frame(x1=rnorm(n_majority, mean=0),
x2=rnorm(n_majority, mean=1), x3=rbinom(n_majority, 1, 0.3),
Class="majority")

df_minority<- data.frame(x1=rnorm(n_minority, mean=1),
x2=rnorm(n_minority, mean=2), x3=rbinom(n_minority, 1, 0.6),
Class="minority")

df<- rbind(df_majority, df_minority)
df$Class<- factor(df$Class, levels=c("majority", "minority"))

table(df$Class)

#######################################
#random subsampling of majority class #
#######################################
set.seed(293024)

maj<- df[df$Class=="majority",]
mino<- df[df$Class=="minority",]
n_min<- nrow(mino)

df_rand_2to1<- rbind(maj[sample(nrow(maj), 2*n_min),], mino) #2:1
df_rand_3to1<- rbind(maj[sample(nrow(maj), 3*n_min),], mino) #3:1
df_rand_4to1<- rbind(maj[sample(nrow(maj), 4*n_min),], mino) #4:1

#shuffling rows
df_rand_2to1<- df_rand_2to1[sample(nrow(df_rand_2to1)),]
df_rand_3to1<- df_rand_3to1[sample(nrow(df_rand_3to1)),]
df_rand_4to1<- df_rand_4to1[sample(nrow(df_rand_4to1)),]

#removing row names
rownames(df_rand_2to1)<- NULL
rownames(df_rand_3to1)<- NULL
rownames(df_rand_4to1)<- NULL

#checking class balance
table(df_rand_2to1$Class)
table(df_rand_3to1$Class)
table(df_rand_4to1$Class)

##########################################################
#propensity score matching subsampling of majority class #
##########################################################

#install.packages("MatchIt")
library(MatchIt)

df_ps<- df
df_ps$mino<- ifelse(df_ps$Class=="minority", 1, 0)

m_2to1<- matchit(mino~x1+x2+x3, data=df_ps, method="nearest", ratio=2, caliper=0.2)
df_match_2to1<- match.data(m_2to1)[,1:4] 

m_3to1<- matchit(mino~x1+x2+x3, data=df_ps, method="nearest", ratio=3, caliper=0.2)
df_match_3to1<- match.data(m_3to1)[,1:4]

m_4to1<- matchit(mino~x1+x2+x3, data=df_ps, method="nearest", ratio=4, caliper=0.2)
df_match_4to1<- match.data(m_4to1)[,1:4]

#shuffling rows
df_match_2to1<- df_match_2to1[sample(nrow(df_match_2to1)),]
df_match_3to1<- df_match_3to1[sample(nrow(df_match_3to1)),]
df_match_4to1<- df_match_4to1[sample(nrow(df_match_4to1)),]

#removing row names
rownames(df_match_2to1)<- NULL
rownames(df_match_3to1)<- NULL
rownames(df_match_4to1)<- NULL

table(df_match_2to1$Class)
table(df_match_3to1$Class)
table(df_match_4to1$Class)

###############################################
# oversampling minority class using SMOTE     #
# (Synthetic Minority Oversampling TEchnique) #
###############################################
#install.packages("smotefamily")
library(smotefamily)

x<- df[, c("x1", "x2", "x3")]
y<- df$Class

set.seed(304533)

smote_out<- SMOTE(X=x, target=y, K=5, dup_size=2) 
#original minority + synthetic minority = n + (dup_size × n) 

df_smote<- smote_out$data
names(df_smote)[ncol(df_smote)]<- "Class"

table(df_smote$Class)
table(df_smote$x3) #0/1 variable is not 0/1 anymore
