proc import out=pneumonia datafile="C:/Users/000110888/OneDrive - CSULB/Desktop/pneumonia_data.csv"
dbms=csv replace;

/*SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS*/
proc surveyselect data=pneumonia rate=0.8 seed=6132208
out=pneumonia outall method=srs; 
run;

/*GINI SPLITTING AND COST-COMPLEXITY PRUNING */
proc hpsplit data=pneumonia maxdepth=4;
class pneumonia gender tobacco_use;
   model pneumonia(event="yes")= age gender tobacco_use PM2_5;
grow gini;
prune costcomplexity; 
partition rolevar=selected(train="1");
output out=predicted;
ID selected;
run;

/*COMPUTING CONFUSION MATRICES AND PERFORMANCE MEASURES
FOR TESTING SET FOR A RANGE OF CUTOFFS*/
data test;
set predicted;
if(selected="0");
run;

data cutoffs;
set test;
do i=0 to 101;
tp=(P_pneumoniayes >= 0.01*i and pneumonia="yes");
fp=(P_pneumoniayes >= 0.01*i and pneumonia="no");
tn=(P_pneumoniayes <  0.01*i and pneumonia="no");
fn=(P_pneumoniayes <  0.01*i and pneumonia="yes");
output;
end;
run;

proc sql;
create table confusion as
select i, sum(tp) as tp, sum(fp) as fp, sum(tn) as tn,
sum(fn) as fn, count(*) as total
from cutoffs
group by i;
quit;

proc sql;
create table measures as
select i, (tp+tn)/total as accuracy, (fp+fn)/total as 
misclassrate, tp/(tp+fn) as sensitivity, tn/(fp+tn) as specificity,
fp/(fp+tn) as oneminusspec
from confusion
group by i;
quit;


/*PLOTTING ROC CURVE*/
title 'The Receiver Operating Characteristic Curve';
proc gplot data=measures;        
symbol v=square interpol=join;
plot sensitivity*oneminusspec/ vaxis=0 to 1 by 0.1 haxis=0 to 1 by 0.1;
label sensitivity="Sensitivity" oneminusspec="1-Specificity";
run;

/*REPORTING MEASURES FOR THE POINT ON ROC CURVE CLOSEST
TO THE IDEAL POINT (0,1)*/
proc sql;
select accuracy, misclassrate, sensitivity, specificity, 
sqrt(oneminusspec**2+(1-sensitivity)**2) as distance, i*0.01 as cutoff
from measures
having distance=min(distance);
quit;

/*COMPUTING AREA UNDER THE ROC CURVE*/
proc sort data=measures;
by oneminusspec;
run;

data AUC;
set measures;
lagx=lag(oneminusspec);
lagy=lag(sensitivity);
if lagx=. then lagx=0;
if lagy=. then lagy=0;
trapezoid=(oneminusspec-lagx)*(sensitivity+lagy)/2;
AUC+trapezoid;
run;

proc print data=AUC (firstobs=102) noobs;
var AUC;
run;



