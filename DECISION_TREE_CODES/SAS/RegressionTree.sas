proc import out=housing
datafile="C:/Users/000110888/OneDrive - CSULB/Desktop/housing_data.csv"
dbms=csv replace;
run;

/*SPLITTING DATA INTO 80% TRAINING AND 20% TESTING*/
proc surveyselect data=housing rate=0.8 seed=677530
out=housing outall method=srs; 
run;

proc print data=housing (obs=10);
run;

/*RSS SPLITTING CRITERION - FULL TREE*/
proc hpsplit data=housing seed=304576;
class ocean_proximity;
model median_house_value = housing_median_age	
total_rooms	total_bedrooms population households	
median_income ocean_proximity;
grow RSS;
partition rolevar=selected(train="1");
run;

/*RSS SPLITTING AND COST-COMPLEXITY PRUNING*/
proc hpsplit data=housing; 
class ocean_proximity;
model median_house_value = housing_median_age	
total_rooms	total_bedrooms population households	
median_income ocean_proximity;
grow RSS;
prune costcomplexity(leaves=5);
partition rolevar=selected(train="1");
output out=predicted;
ID selected;
run;

proc print data=predicted (obs=20);
run;

/*COMPUTING PREDICTION ACCURACY FOR TESTING DATA*/
data test;
set predicted;
if(selected="0");
keep median_house_value P_median_house_value;
run;

data accuracy;
set test;
if(abs(median_house_value-P_median_house_value)<0.10*median_house_value)
then ind10=1; else ind10=0;
if(abs(median_house_value-P_median_house_value)<0.15*median_house_value)
then ind15=1; else ind15=0; 
if(abs(median_house_value-P_median_house_value)<0.20*median_house_value)
then ind20=1; else ind20=0;
run;

proc sql;
 select mean(ind10) as accuracy10, mean(ind15) as accuracy15,
 mean(ind20) as accuracy20
 from accuracy;
 quit;

/*CHAID SPLITTING CRITERION - FULL TREE*/
proc hpsplit data=housing seed=501231; 
class ocean_proximity;
model median_house_value = housing_median_age	
total_rooms	total_bedrooms population households	
median_income ocean_proximity;
grow CHAID;
partition rolevar=selected(train="1");
run;

/*CHAID SPLITTING AND COST-COMPLEXITY PRUNING */
proc hpsplit data=housing seed=501231; 
class ocean_proximity;
model median_house_value = housing_median_age	
total_rooms	total_bedrooms population households	
median_income ocean_proximity;
grow CHAID;
prune costcomplexity (leaves=5); 
partition rolevar=selected(train="1");
output out=predicted;
ID selected;
run;
