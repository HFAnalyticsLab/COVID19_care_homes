/*
SAS code for summarising a data set of APC spells that can be used to try to replicate the NHSE care home discharge figures.
*/

*	Define the libnames used in the programs;
libname mmpi		"C01b - mmpi cleaning\lib\clean"; * Source of the cleaned MMPI SAS data sets;
libname cleansus	"C01 - SUS cleaning\lib\clean"; * Source of the cleaned SUS SAS data sets;
libname	interim		"P059\interim"; * Destination library of any interim files that need to be created and saved;

options obs = max;
*options obs = 10000; * Limit the number of records to read in if testing;


%let mysource = interim.apcs_3_1420_final;
%let myext = 1420_f;

/*################
  ##  DISCHARGE ##
  ################*/

options obs = max;
*options obs = 1000;

proc sql;
	create table interim.dischbytime_&myext as
	select spellenddate, chdisch, count(spellenddate) as freq, dischday, dischmnth, dischyr
	from &mysource
	where dischdest ~= 79
	group by spellenddate, chdisch, dischday, dischmnth, dischyr;
quit;


data 	dc_2014 dc_2015 dc_2016 dc_2017 dc_2018 dc_2019 dc_2020
		do_2014 do_2015 do_2016 do_2017 do_2018 do_2019 do_2020;
	set interim.dischbytime_&myext;
	if dischyr = 2014 then do;
		if chdisch = 1 then output dc_2014;
		else output do_2014;
	end;
	else if dischyr = 2015 then do;
		if chdisch = 1 then output dc_2015;
		else output do_2015;
	end;
	else if dischyr = 2016 then do;
		if chdisch = 1 then output dc_2016;
		else output do_2016;
	end;
	else if dischyr = 2017 then do;
		if chdisch = 1 then output dc_2017;
		else output do_2017;
	end;
	else if dischyr = 2018 then do;
		if chdisch = 1 then output dc_2018;
		else output do_2018;
	end;
	else if dischyr = 2019 then do;
		if chdisch = 1 then output dc_2019;
		else output do_2019;
	end;
	else if dischyr = 2020 then do;
		if chdisch = 1 then output dc_2020;
		else output do_2020;
	end;
run;

proc sql;
	create table allyears as
	select	l.dischmnth, l.dischday, 	
			r1.freq as dischch_2014, r2.freq as dischother_2014, 
			r3.freq as dischch_2015, r4.freq as dischother_2015, 
			r5.freq as dischch_2016, r6.freq as dischother_2016, 
			r7.freq as dischch_2017, r8.freq as dischother_2017, 
			r9.freq as dischch_2018, r10.freq as dischother_2018, 
			r11.freq as dischch_2019, r12.freq as dischother_2019, 
			r13.freq as dischch_2020, r14.freq as dischother_2020 
	from do_2016 as l
	left join dc_2014 as r1 on l.dischday = r1.dischday and l.dischmnth = r1.dischmnth 
	left join do_2014 as r2 on l.dischday = r2.dischday and l.dischmnth = r2.dischmnth 
	left join dc_2015 as r3 on l.dischday = r3.dischday and l.dischmnth = r3.dischmnth  
	left join do_2015 as r4 on l.dischday = r4.dischday and l.dischmnth = r4.dischmnth  
	left join dc_2016 as r5 on l.dischday = r5.dischday and l.dischmnth = r5.dischmnth 
	left join do_2016 as r6 on l.dischday = r6.dischday and l.dischmnth = r6.dischmnth 
	left join dc_2017 as r7 on l.dischday = r7.dischday and l.dischmnth = r7.dischmnth 
	left join do_2017 as r8 on l.dischday = r8.dischday and l.dischmnth = r8.dischmnth 
	left join dc_2018 as r9 on l.dischday = r9.dischday and l.dischmnth = r9.dischmnth 
	left join do_2018 as r10 on l.dischday = r10.dischday and l.dischmnth = r10.dischmnth 
	left join dc_2019 as r11 on l.dischday = r11.dischday and l.dischmnth = r11.dischmnth 
	left join do_2019 as r12 on l.dischday = r12.dischday and l.dischmnth = r12.dischmnth 
	left join dc_2020 as r13 on l.dischday = r13.dischday and l.dischmnth = r13.dischmnth 
	left join do_2020 as r14 on l.dischday = r14.dischday and l.dischmnth = r14.dischmnth 
	order by l.dischmnth, l.dischday;
quit;


data allyears2;
	set allyears;
	dischall_2014 = dischch_2014 + dischother_2014;
	dischall_2015 = dischch_2015 + dischother_2015;
	dischall_2016 = dischch_2016 + dischother_2016;
	dischall_2017 = dischch_2017 + dischother_2017;
	dischall_2018 = dischch_2018 + dischother_2018;
	dischall_2019 = dischch_2019 + dischother_2019;
	dischall_2020 = dischch_2020 + dischother_2020;

run;


proc sql;
	create table disch_1920 as
	select dischmnth, dischday, dischother_2019, dischch_2019, dischall_2019, dischother_2020, dischch_2020, dischall_2020
	from allyears2
	order by dischmnth, dischday;
quit;

proc sql;
	create table totals as
	select	sum(dischall_2014) as all_2014, sum(dischall_2015) as all_2015, sum(dischall_2016) as all_2016, sum(dischall_2017) as all_2017, sum(dischall_2018) as all_2018,
			sum(dischall_2019) as all_2019, sum(dischall_2020) as all_2020
	from allyears2;
quit; 

data disch_q1;
	set allyears2;
	if dischmnth <= 4 then output;
run;

proc sql;
	create table totals_q1_dis as
	select	'Total discharges Q1' as title, sum(dischall_2014) as all_2014, sum(dischall_2015) as all_2015, sum(dischall_2016) as all_2016, sum(dischall_2017) as all_2017,
			sum(dischall_2018) as all_2018, sum(dischall_2019) as all_2019, sum(dischall_2020) as all_2020
	from disch_q1;
quit; 

proc sql;
	create table totals_q1_dis_ch as
	select	'Total carehome dis' as title, sum(dischch_2014) as all_2014, sum(dischch_2015) as all_2015, sum(dischch_2016) as all_2016, sum(dischch_2017) as all_2017,
			sum(dischch_2018) as all_2018, sum(dischch_2019) as all_2019, sum(dischch_2020) as all_2020
	from disch_q1;
quit; 


/*################
  ##  ADMISSION ##
  ################*/

proc sql;
	create table interim.admbytime_&myext as
	select spellstartdate, chadm, count(spellstartdate) as freq, admday, admmnth, admyr
	from &mysource
	group by spellstartdate, chadm, admday, admmnth, admyr;
quit;


data 	ac_2014 ac_2015 ac_2016 ac_2017 ac_2018 ac_2019 ac_2020
		ao_2014 ao_2015 ao_2016 ao_2017 ao_2018 ao_2019 ao_2020;
	set interim.admbytime_&myext;
	if admyr = 2014 then do;
		if chadm = 1 then output ac_2014;
		else output ao_2014;
	end;
	else if admyr = 2015 then do;
		if chadm = 1 then output ac_2015;
		else output ao_2015;
	end;
	else if admyr = 2016 then do;
		if chadm = 1 then output ac_2016;
		else output ao_2016;
	end;
	else if admyr = 2017 then do;
		if chadm = 1 then output ac_2017;
		else output ao_2017;
	end;
	else if admyr = 2018 then do;
		if chadm = 1 then output ac_2018;
		else output ao_2018;
	end;
	else if admyr = 2019 then do;
		if chadm = 1 then output ac_2019;
		else output ao_2019;
	end;
	else if admyr = 2020 then do;
		if chadm = 1 then output ac_2020;
		else output ao_2020;
	end;
run;

proc sql;
	create table allyears_adm as
	select	l.admmnth, l.admday, 	
			r1.freq as admch_2014, r2.freq as admother_2014, 
			r3.freq as admch_2015, r4.freq as admother_2015, 
			r5.freq as admch_2016, r6.freq as admother_2016, 
			r7.freq as admch_2017, r8.freq as admother_2017, 
			r9.freq as admch_2018, r10.freq as admother_2018, 
			r11.freq as admch_2019, r12.freq as admother_2019, 
			r13.freq as admch_2020, r14.freq as admother_2020 
	from ao_2016 as l
	left join ac_2014 as r1 on l.admday = r1.admday and l.admmnth = r1.admmnth 
	left join ao_2014 as r2 on l.admday = r2.admday and l.admmnth = r2.admmnth 
	left join ac_2015 as r3 on l.admday = r3.admday and l.admmnth = r3.admmnth  
	left join ao_2015 as r4 on l.admday = r4.admday and l.admmnth = r4.admmnth  
	left join ac_2016 as r5 on l.admday = r5.admday and l.admmnth = r5.admmnth 
	left join ao_2016 as r6 on l.admday = r6.admday and l.admmnth = r6.admmnth 
	left join ac_2017 as r7 on l.admday = r7.admday and l.admmnth = r7.admmnth 
	left join ao_2017 as r8 on l.admday = r8.admday and l.admmnth = r8.admmnth 
	left join ac_2018 as r9 on l.admday = r9.admday and l.admmnth = r9.admmnth 
	left join ao_2018 as r10 on l.admday = r10.admday and l.admmnth = r10.admmnth 
	left join ac_2019 as r11 on l.admday = r11.admday and l.admmnth = r11.admmnth 
	left join ao_2019 as r12 on l.admday = r12.admday and l.admmnth = r12.admmnth 
	left join ac_2020 as r13 on l.admday = r13.admday and l.admmnth = r13.admmnth 
	left join ao_2020 as r14 on l.admday = r14.admday and l.admmnth = r14.admmnth 
	order by l.admmnth, l.admday;
quit;


data allyears2_adm;
	set allyears_adm;
	admall_2014 = admch_2014 + admother_2014;
	admall_2015 = admch_2015 + admother_2015;
	admall_2016 = admch_2016 + admother_2016;
	admall_2017 = admch_2017 + admother_2017;
	admall_2018 = admch_2018 + admother_2018;
	admall_2019 = admch_2019 + admother_2019;
	admall_2020 = admch_2020 + admother_2020;

run;

proc sql;
	create table adm_1920 as
	select admmnth, admday, admother_2019, admch_2019, admall_2019, admother_2020, admch_2020, admall_2020
	from allyears2_adm
	order by admmnth, admday;
quit;

proc sql;
	create table totals_adm as
	select	sum(admall_2014) as all_2014, sum(admall_2015) as all_2015, sum(admall_2016) as all_2016, sum(admall_2017) as all_2017,
			sum(admall_2018) as all_2018, sum(admall_2019) as all_2019, sum(admall_2020) as all_2020
	from allyears2_adm;
quit; 

data adm_q1;
	set allyears2_adm;
	if admmnth <= 4 then output;
run;

proc sql;
	create table totals_q1_adm as
	select	'Total admissions Q1' as title, sum(admall_2014) as all_2014, sum(admall_2015) as all_2015, sum(admall_2016) as all_2016, sum(admall_2017) as all_2017,
			sum(admall_2018) as all_2018, sum(admall_2019) as all_2019, sum(admall_2020) as all_2020
	from adm_q1;
quit; 

proc sql;
	create table totals_q1_adm_ch as
	select	'Total carehome adm' as title, sum(admch_2014) as all_2014, sum(admch_2015) as all_2015, sum(admch_2016) as all_2016, sum(admch_2017) as all_2017,
			sum(admch_2018) as all_2018, sum(admch_2019) as all_2019, sum(admch_2020) as all_2020
	from adm_q1;
quit; 


data interim.alltotals_&myext;
	length title $ 64;
	set totals_q1_dis totals_q1_dis_ch totals_q1_adm totals_q1_adm_ch;
run;

/*
options obs = max;
*options obs = 1000000;

%let myvar = dischdest;

proc sql;
	create table &myvar as
	select &myvar, count(&myvar) as freq
	from interim.apcs_m1to4
	group by &myvar;
quit;


%let myval = apce_dischdest;

data &myval;
	set values.&myval;
run;

proc sql;
	create table val_&myvar as
	select l.*, r.*
	from &myvar as l
	left join &myval as r on l.&myvar = r.&myvar;
quit;

*/