
*	Bring a copy of the NEHF runfile into the work directory;

data rf1;
	set covidint.rf_nehf;
run;

*	Select only the APCE related rows;

data rf2;
	set rf1;
	if order > 38 then order = order + 1;
	if rfile = 'mysus.apce' then output;
run;

*	Create the missing PVD row;

data rf_add1;
	set rf1(obs = 1);
	order = 39;
	variable = "e_pvd_h36";
	label = "Diagnosis of PVD (Elixhauser) in Pre-Period"; 
	var1 = "e_pvd_h36";
	ouf = "interim.e_pvd_h36"; 
	lfile = "clean.spine"; 
	rfile = "mysus.apce"; 
	cntvar = "pvd";
	lowerbound = "-1095";
	datevar = "admdate";
	cnttype = "max";
run;

data rf_add2;
	set rf1(obs = 1);
	order = 100;
	variable = "a_covid_h36";
	label = "Diagnosis of COVID-19 in Pre-Period"; 
	var1 = "a_covid_h36";
	ouf = "interim.a_covid_h36"; 
	lfile = "clean.spine"; 
	rfile = "mysus.apce"; 
	cntvar = "covid";
	lowerbound = "-1095";
	datevar = "admdate";
	cnttype = "max";
run;

data rf_add3;
	set rf1(obs = 1);
	order = 101;
	variable = "a_flupneumonia_h36";
	label = "Diagnosis of Influenza or pneumonia in Pre-Period"; 
	var1 = "a_flupneumonia_h36";
	ouf = "interim.a_flupneumonia_h36"; 
	lfile = "clean.spine"; 
	rfile = "mysus.apce"; 
	cntvar = "flupneumonia";
	lowerbound = "-1095";
	datevar = "admdate";
	cnttype = "max";
run;


/*
		max(covid) as covid  label = 'COVID-19', 
		max(flupneumonia) as flupneumonia  label = 'Influenza and pneumonia',
*/


*	Add the additional PVD row to the main runfile;

data rf3;
	set rf2 rf_add1 rf_add2 rf_add3;
run;

proc sort data = rf3 out = rf4;
	by order;
run;

data clean.runfile;
	order + 1;
	set rf4(drop = order);
run;

options obs = max;
*options obs = 100;

data clean.spine;
	set mmpi.mmpiss_bymonth(keep = uid pid datefrom);
	rename datefrom = findexdate;
run;

