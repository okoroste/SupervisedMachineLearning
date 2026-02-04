proc import out=movie 
datafile="C:/Users/000110888/Desktop/movie_data.csv"
dbms=csv replace;
run;

/*SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS*/
proc surveyselect data=movie rate=0.8 seed=118607
out=movie outall method=srs; 
run;

/*GINI SPLITTING AND COST-COMPLEXITY PRUNING */
proc hpsplit data=movie;
class rating gender member;
   model rating=age gender member nmovies;
grow gini;
prune costcomplexity;
partition rolevar=selected(train="1");
output out=predicted;
ID selected;
run;

/*MACRO FOR COMPUTING PERFORMANCE MEASURES*/
%macro perf_measures(dataset);

/*computing confusion matrix*/
data test;
set predicted;
if(selected="0");
maxprob=max(P_ratingbad, P_ratinggood, P_ratingokay, 
P_ratingvery_bad, P_ratingvery_good);
if maxprob=P_ratingvery_good then predclass='very good';
if maxprob=P_ratinggood then predclass='good';
if maxprob=P_ratingokay then predclass='okay';
if maxprob=P_ratingbad then predclass='bad';
if maxprob=P_ratingvery_bad then predclass='very bad';
run;

/*computing total number of rows in test set*/
proc sql;
create table totalrows as
select count(*) as nrows
from test;
quit;

data _null_;
set totalrows;
 call symput('totalrows', nrows);
run;

/*computing performance measures for individual classes*/
%macro class_metrics(class);
data indiv_class;
set test;
tp=(predclass=&class and rating=&class);
fp=(predclass=&class and rating ne &class);
tn=(predclass ne &class and rating ne &class);
fn=(predclass ne &class and rating=&class);
run;

proc sql;
create table confusion as
select sum(tp) as tp, sum(fp) as fp, sum(tn) as tn,
sum(fn) as fn, count(*) as total
from indiv_class;
quit;

proc sql;
create table measures as
select &class as class, tp, fp, tn, fn, 
(tp+tn)/total as accuracy, (fp+fn)/total as 
misclassrate, tp/(tp+fn) as sensitivity, 
fn/(tp+fn) as FNR, tn/(fp+tn) as specificity, 
fp/(fp+tn) as FPR, tp/(tp+fp) as precision, 
tn/(fn+tn) as NPV, 2*tp/(2*tp+fn+fp) as F1score
from confusion;
select * from measures;
quit;

proc append base=&dataset data=measures;
run;

%mend;

%class_metrics('very bad')
%class_metrics('bad')
%class_metrics('okay')
%class_metrics('good')
%class_metrics('very good')

/*computing micro measures*/
proc sql;
create table totals as
select sum(tp) as tp, sum(fp) as fp, sum(tn) as tn, 
sum(fn) as fn
from &dataset;
quit;

proc sql;
select 'micro measures',(tp+tn)/(tp+fp+tn+fn) 
as accuracy, (fp+fn)/(tp+fp+tn+fn) 
as misclassrate, tp/(tp+fn) as sensitivity, 
fn/(tp+fn) as FNR, tn/(fp+tn) as specificity, 
fp/(fp+tn) as FPR, tp/(tp+fp) as precision, 
tn/(fn+tn) as NPV, 2*tp/(2*tp+fn+fp) as F1score
from totals;
quit;

/*computing macro measures*/
proc sql;
select 'macro measures', mean(accuracy) as accuracy, 
mean(misclassrate) as misclassrate, mean(sensitivity) as sensitivity, 
mean(FNR) as FNR, mean(specificity) as specificity, 
mean(FPR) as FPR, mean(precision) as precision, 
mean(NPV) as NPV, mean(F1score) as F1score
from &dataset;
quit;

/*computing weighted macro measures*/
data &dataset;
set &dataset;
weight=(tp+fn)/&totalrows;
w_accuracy=accuracy*weight;
w_misclassrate=misclassrate*weight;
w_sensitivity=sensitivity*weight;
w_FNR=FNR*weight;
w_specificity=specificity*weight;
w_FPR=FPR*weight;
w_precision=precision*weight;
w_FPR=FPR*weight;
w_precision=precision*weight;
w_NPV=NPV*weight;
w_F1score=F1score*weight;
run;

proc sql;
select 'weighted macro measures',sum(w_accuracy) 
as accuracy, sum(w_misclassrate) as misclassrate, 
sum(w_sensitivity) as sensitivity, 
sum(w_FNR) as FNR, sum(w_specificity) as specificity, 
sum(w_FPR) as FPR, sum(w_precision) as precision, 
sum(w_NPV) as NPV, sum(w_F1score) as F1score
from &dataset;
quit;

%mend;

/*COMPUTING PERFORMANCE MEASURES FOR FITTED GINI TREE*/
%perf_measures(ginitree)


/*ENTROPY SPLITTING AND COST-COMPLEXITY PRUNING*/ 
proc hpsplit data=movie;
class rating gender member;
   model rating=age gender member nmovies;
grow entropy;
prune costcomplexity; 
partition rolevar=selected(train="1");
output out=predicted;
ID selected;
run;

/*COMPUTING PERFORMANCE MEASURES FOR FITTED ENTROPY TREE*/
%perf_measures(entropytree)

/*CHAID SPLITTING AND COST-COMPLEXITY PRUNING*/
proc hpsplit data=movie;
class rating gender member;
   model rating=age gender member nmovies;
grow CHAID;
prune costcomplexity; 
partition rolevar=selected(train="1");
output out=predicted;
ID selected;
run;

/*COMPUTING PERFORMANCE MEASURES FOR FITTED CHAID TREE*/
%perf_measures(CHAIDtree)













