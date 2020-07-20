
%let mysource = interim.apcs_3_1420_final_2_adm;
%let myext = 1420_f_reg_2;

proc sql;
	create table interim.admbytime_covid1920_&myext as
	select ch_nursing_adm, spellstartdate, chadm, covid_prim, count(spellstartdate) as freq, admday, admmnth, admyr
	from &mysource
	where chadm = 1 and admyr >= 2019
	group by ch_nursing_adm, spellstartdate, chadm, covid_prim, admday, admmnth, admyr
	order by admyr, admmnth, admday, ch_nursing_adm, covid_prim;
quit;


proc sql;
	create table interim.admbytime_em_el_1920 as
	select ch_nursing_adm, spellstartdate, chadm, emergency, elective, count(spellstartdate) as freq, admday, admmnth, admyr
	from &mysource
	where chadm = 1 and admyr >= 2019
	group by ch_nursing_adm, spellstartdate, chadm, emergency, elective, admday, admmnth, admyr
	order by admyr, admmnth, admday, ch_nursing_adm, emergency, elective;
quit;



data 	ac_2014 ac_2015 ac_2016 ac_2017 ac_2018 ac_2019 ac_2020
		ao_2014 ao_2015 ao_2016 ao_2017 ao_2018 ao_2019 ao_2020;
	set interim.admbytime_covid_&myext;
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

/*
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
*/
/*
data interim.joinall_adm_covid;
	set interim.joinall_adm;
run;
*/


proc sql;
	create table allyears_adm as
	select	l.admmnth, l.admday, l.covid_prim, l.ch_nursing_adm, 	
			r1.freq as admch_2014, r2.freq as admother_2014, 
			r3.freq as admch_2015, r4.freq as admother_2015, 
			r5.freq as admch_2016, r6.freq as admother_2016, 
			r7.freq as admch_2017, r8.freq as admother_2017, 
			r9.freq as admch_2018, r10.freq as admother_2018, 
			r11.freq as admch_2019, r12.freq as admother_2019, 
			r13.freq as admch_2020, r14.freq as admother_2020 
	from interim.joinall_adm_covid as l
	left join ac_2014 as r1 on l.admday = r1.admday and l.admmnth = r1.admmnth and l.covid_prim = r1.covid_prim and l.ch_nursing_adm = r1.ch_nursing_adm
	left join ao_2014 as r2 on l.admday = r2.admday and l.admmnth = r2.admmnth  and l.covid_prim = r2.covid_prim and l.ch_nursing_adm = r2.ch_nursing_adm
	left join ac_2015 as r3 on l.admday = r3.admday and l.admmnth = r3.admmnth   and l.covid_prim = r3.covid_prim and l.ch_nursing_adm = r3.ch_nursing_adm
	left join ao_2015 as r4 on l.admday = r4.admday and l.admmnth = r4.admmnth   and l.covid_prim = r4.covid_prim and l.ch_nursing_adm = r4.ch_nursing_adm
	left join ac_2016 as r5 on l.admday = r5.admday and l.admmnth = r5.admmnth  and l.covid_prim = r5.covid_prim and l.ch_nursing_adm = r5.ch_nursing_adm
	left join ao_2016 as r6 on l.admday = r6.admday and l.admmnth = r6.admmnth  and l.covid_prim = r6.covid_prim and l.ch_nursing_adm = r6.ch_nursing_adm
	left join ac_2017 as r7 on l.admday = r7.admday and l.admmnth = r7.admmnth  and l.covid_prim = r7.covid_prim and l.ch_nursing_adm = r7.ch_nursing_adm
	left join ao_2017 as r8 on l.admday = r8.admday and l.admmnth = r8.admmnth  and l.covid_prim = r8.covid_prim and l.ch_nursing_adm = r8.ch_nursing_adm
	left join ac_2018 as r9 on l.admday = r9.admday and l.admmnth = r9.admmnth  and l.covid_prim = r9.covid_prim and l.ch_nursing_adm = r9.ch_nursing_adm
	left join ao_2018 as r10 on l.admday = r10.admday and l.admmnth = r10.admmnth  and l.covid_prim = r10.covid_prim and l.ch_nursing_adm = r10.ch_nursing_adm
	left join ac_2019 as r11 on l.admday = r11.admday and l.admmnth = r11.admmnth  and l.covid_prim = r11.covid_prim and l.ch_nursing_adm = r11.ch_nursing_adm
	left join ao_2019 as r12 on l.admday = r12.admday and l.admmnth = r12.admmnth  and l.covid_prim = r12.covid_prim and l.ch_nursing_adm = r12.ch_nursing_adm
	left join ac_2020 as r13 on l.admday = r13.admday and l.admmnth = r13.admmnth  and l.covid_prim = r13.covid_prim and l.ch_nursing_adm = r13.ch_nursing_adm
	left join ao_2020 as r14 on l.admday = r14.admday and l.admmnth = r14.admmnth  and l.covid_prim = r14.covid_prim and l.ch_nursing_adm = r14.ch_nursing_adm
	order by l.admmnth, l.admday, l.covid_prim, l.ch_nursing_adm	;
quit;

data allyears2_adm;
	length admmnth admday 8;
	length covid_prim 8;
	length ch_nursing_adm $ 2;
	length 	admch_2014 admother_2014 admall_2014 admch_2015	admother_2015	admall_2015
			admch_2016 admother_2016 admall_2016 admch_2017	admother_2017	admall_2017
			admch_2018 admother_2018 admall_2018 admch_2019	admother_2019	admall_2019
			admch_2020 admother_2020 admall_2020 8;
	set allyears_adm;
	admall_2014 = sum(admch_2014, admother_2014); 
	admall_2015 = sum(admch_2015, admother_2015); 
	admall_2016 = sum(admch_2016, admother_2016); 
	admall_2017 = sum(admch_2017, admother_2017); 
	admall_2018 = sum(admch_2018, admother_2018); 
	admall_2019 = sum(admch_2019, admother_2019); 
	admall_2020 = sum(admch_2020, admother_2020); 
run;


proc sql;
	create table adm_1920 as
	select admmnth, admday, covid_prim, ch_nursing_adm, admother_2019, admch_2019, admall_2019, admother_2020, admch_2020, admall_2020
	from allyears2_adm
	order by admmnth, admday;
quit;

proc sort data = adm_1920 out = adm_1920_covid;
	by descending covid_prim descending admmnth descending admday;
run;


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


data interim.alltotals_cp_&myext;
	length title $ 64;
	set totals_q1_adm totals_q1_adm_ch;
run;