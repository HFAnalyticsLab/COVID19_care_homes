
options obs = max;
*options obs = 10000; * Limit the number of records to read in if testing;


%let mysource = interim.apcs_3_1420_final_2;
%let myext = 1420_f_reg_2;

/*################
  ##  DISCHARGE ##
  ################*/
			

options obs = max;
*options obs = 10000;

data interim.apcs_3_1420_final_2;
	set interim.apcs_3_1420_final;
	if rgn19nm = '' then rgn19nm = 'Not known';
	if ch_nursing_disch = '' then ch_nursing_disch = 'NA';
	if ch_nursing_adm = '' then ch_nursing_adm = 'NA';

run;


proc sql;
	create table interim.dischbytime_&myext as
	select rgn19nm, ch_nursing_disch, spellenddate, chdisch, count(spellenddate) as freq, dischday, dischmnth, dischyr
	from &mysource
	where dischdest ~= 79
	group by rgn19nm, ch_nursing_disch, spellenddate, chdisch, dischday, dischmnth, dischyr
	order by dischyr, dischmnth, dischday, rgn19nm, ch_nursing_disch;
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

/*
proc sql;
	create table days as
	select distinct dischday, dischmnth, 1 as myjoin
	from do_2016
	order by dischmnth, dischday;
quit;

proc sql;
	create table rgn19nms as
	select distinct rgn19nm, 1 as myjoin
	from do_2016;
quit;

proc sql;
	create table ch_nurs as
	select distinct ch_nursing_disch, 1 as myjoin
	from do_2016
	where ch_nursing_disch ~= '';
quit;

proc sql;
	create table interim.joinall as
	select l.*, r1.rgn19nm, r2.ch_nursing_disch
	from days as l
	left join rgn19nms as r1 on l.myjoin = r1.myjoin
	left join ch_nurs as r2 on l.myjoin = r2.myjoin
	order by l.dischmnth, l.dischday, r1.rgn19nm, r2.ch_nursing_disch;
quit; 

*/


proc sql;
	create table allyears as
	select	l.dischmnth, l.dischday, l.rgn19nm, l.ch_nursing_disch, 	
			r1.freq as dischch_2014, r2.freq as dischother_2014, 
			r3.freq as dischch_2015, r4.freq as dischother_2015, 
			r5.freq as dischch_2016, r6.freq as dischother_2016, 
			r7.freq as dischch_2017, r8.freq as dischother_2017, 
			r9.freq as dischch_2018, r10.freq as dischother_2018, 
			r11.freq as dischch_2019, r12.freq as dischother_2019, 
			r13.freq as dischch_2020, r14.freq as dischother_2020 
	from interim.joinall as l
	left join dc_2014 as r1 on l.dischday = r1.dischday and l.dischmnth = r1.dischmnth and l.rgn19nm = r1.rgn19nm and l.ch_nursing_disch = r1.ch_nursing_disch
	left join do_2014 as r2 on l.dischday = r2.dischday and l.dischmnth = r2.dischmnth  and l.rgn19nm = r2.rgn19nm and l.ch_nursing_disch = r2.ch_nursing_disch
	left join dc_2015 as r3 on l.dischday = r3.dischday and l.dischmnth = r3.dischmnth   and l.rgn19nm = r3.rgn19nm and l.ch_nursing_disch = r3.ch_nursing_disch
	left join do_2015 as r4 on l.dischday = r4.dischday and l.dischmnth = r4.dischmnth   and l.rgn19nm = r4.rgn19nm and l.ch_nursing_disch = r4.ch_nursing_disch
	left join dc_2016 as r5 on l.dischday = r5.dischday and l.dischmnth = r5.dischmnth  and l.rgn19nm = r5.rgn19nm and l.ch_nursing_disch = r5.ch_nursing_disch
	left join do_2016 as r6 on l.dischday = r6.dischday and l.dischmnth = r6.dischmnth  and l.rgn19nm = r6.rgn19nm and l.ch_nursing_disch = r6.ch_nursing_disch
	left join dc_2017 as r7 on l.dischday = r7.dischday and l.dischmnth = r7.dischmnth  and l.rgn19nm = r7.rgn19nm and l.ch_nursing_disch = r7.ch_nursing_disch
	left join do_2017 as r8 on l.dischday = r8.dischday and l.dischmnth = r8.dischmnth  and l.rgn19nm = r8.rgn19nm and l.ch_nursing_disch = r8.ch_nursing_disch
	left join dc_2018 as r9 on l.dischday = r9.dischday and l.dischmnth = r9.dischmnth  and l.rgn19nm = r9.rgn19nm and l.ch_nursing_disch = r9.ch_nursing_disch
	left join do_2018 as r10 on l.dischday = r10.dischday and l.dischmnth = r10.dischmnth  and l.rgn19nm = r10.rgn19nm and l.ch_nursing_disch = r10.ch_nursing_disch
	left join dc_2019 as r11 on l.dischday = r11.dischday and l.dischmnth = r11.dischmnth  and l.rgn19nm = r11.rgn19nm and l.ch_nursing_disch = r11.ch_nursing_disch
	left join do_2019 as r12 on l.dischday = r12.dischday and l.dischmnth = r12.dischmnth  and l.rgn19nm = r12.rgn19nm and l.ch_nursing_disch = r12.ch_nursing_disch
	left join dc_2020 as r13 on l.dischday = r13.dischday and l.dischmnth = r13.dischmnth  and l.rgn19nm = r13.rgn19nm and l.ch_nursing_disch = r13.ch_nursing_disch
	left join do_2020 as r14 on l.dischday = r14.dischday and l.dischmnth = r14.dischmnth  and l.rgn19nm = r14.rgn19nm and l.ch_nursing_disch = r14.ch_nursing_disch
	order by l.dischmnth, l.dischday, l.rgn19nm, l.ch_nursing_disch	;
quit;

/*
data allyears2;
	set allyears;
	if dischother_2014 = . then dischall_2014 = dischch_2014;
	else dischall_2014 = dischch_2014 + dischother_2014;
	if dischother_2015 = . then dischall_2015 = dischch_2015;
	else dischall_2015 = dischch_2015 + dischother_2015;
	if dischother_2016 = . then dischall_2016 = dischch_2016;
	else dischall_2016 = dischch_2016 + dischother_2016;
	if dischother_2017 = . then dischall_2017 = dischch_2017;
	else dischall_2017 = dischch_2017 + dischother_2017;
	if dischother_2018 = . then dischall_2018 = dischch_2018;
	else dischall_2018 = dischch_2018 + dischother_2018;
	if dischother_2019 = . then dischall_2019 = dischch_2019;
	else dischall_2019 = dischch_2019 + dischother_2019;
	if dischother_2020 = . then dischall_2020 = dischch_2020;
	else dischall_2020 = dischch_2020 + dischother_2020;

run;
*/

data allyears2;
	length dischmnth dischday 8;
	length rgn19nm $ 24;
	length ch_nursing_disch $ 2;
	length 	dischch_2014 dischother_2014 dischall_2014 dischch_2015	dischother_2015	dischall_2015
			dischch_2016 dischother_2016 dischall_2016 dischch_2017	dischother_2017	dischall_2017
			dischch_2018 dischother_2018 dischall_2018 dischch_2019	dischother_2019	dischall_2019
			dischch_2020 dischother_2020 dischall_2020 8;
	set allyears;
	dischall_2014 = sum(dischch_2014, dischother_2014); 
	dischall_2015 = sum(dischch_2015, dischother_2015); 
	dischall_2016 = sum(dischch_2016, dischother_2016); 
	dischall_2017 = sum(dischch_2017, dischother_2017); 
	dischall_2018 = sum(dischch_2018, dischother_2018); 
	dischall_2019 = sum(dischch_2019, dischother_2019); 
	dischall_2020 = sum(dischch_2020, dischother_2020); 
run;
	


proc sql;
	create table disch_1920 as
	select dischmnth, dischday, rgn19nm, ch_nursing_disch, dischother_2019, dischch_2019, dischall_2019, dischother_2020, dischch_2020, dischall_2020
	from allyears2
	order by dischmnth, dischday, rgn19nm, ch_nursing_disch ;
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
/*
proc sql;
	create table interim.admbytime_&myext as
	select spellstartdate, chadm, count(spellstartdate) as freq, admday, admmnth, admyr
	from &mysource
	group by rgn19nm, ch_nursing_adm, spellstartdate, chadm, admday, admmnth, admyr;
quit;
*/
proc sql;
	create table interim.admbytime_&myext as
	select rgn19nm, ch_nursing_adm, spellstartdate, chadm, count(spellstartdate) as freq, admday, admmnth, admyr
	from &mysource
	group by rgn19nm, ch_nursing_adm, spellstartdate, chadm, admday, admmnth, admyr
	order by admyr, admmnth, admday, rgn19nm, ch_nursing_adm;
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
data interim.joinall_adm;
	set interim.joinall;
	rename ch_nursing_disch = ch_nursing_adm;
	rename dischday = admday;
	rename dischmnth = admmnth; 
run;
*/


proc sql;
	create table allyears_adm as
	select	l.admmnth, l.admday, l.rgn19nm, l.ch_nursing_adm, 	
			r1.freq as admch_2014, r2.freq as admother_2014, 
			r3.freq as admch_2015, r4.freq as admother_2015, 
			r5.freq as admch_2016, r6.freq as admother_2016, 
			r7.freq as admch_2017, r8.freq as admother_2017, 
			r9.freq as admch_2018, r10.freq as admother_2018, 
			r11.freq as admch_2019, r12.freq as admother_2019, 
			r13.freq as admch_2020, r14.freq as admother_2020 
	from interim.joinall_adm as l
	left join ac_2014 as r1 on l.admday = r1.admday and l.admmnth = r1.admmnth and l.rgn19nm = r1.rgn19nm and l.ch_nursing_adm = r1.ch_nursing_adm
	left join ao_2014 as r2 on l.admday = r2.admday and l.admmnth = r2.admmnth  and l.rgn19nm = r2.rgn19nm and l.ch_nursing_adm = r2.ch_nursing_adm
	left join ac_2015 as r3 on l.admday = r3.admday and l.admmnth = r3.admmnth   and l.rgn19nm = r3.rgn19nm and l.ch_nursing_adm = r3.ch_nursing_adm
	left join ao_2015 as r4 on l.admday = r4.admday and l.admmnth = r4.admmnth   and l.rgn19nm = r4.rgn19nm and l.ch_nursing_adm = r4.ch_nursing_adm
	left join ac_2016 as r5 on l.admday = r5.admday and l.admmnth = r5.admmnth  and l.rgn19nm = r5.rgn19nm and l.ch_nursing_adm = r5.ch_nursing_adm
	left join ao_2016 as r6 on l.admday = r6.admday and l.admmnth = r6.admmnth  and l.rgn19nm = r6.rgn19nm and l.ch_nursing_adm = r6.ch_nursing_adm
	left join ac_2017 as r7 on l.admday = r7.admday and l.admmnth = r7.admmnth  and l.rgn19nm = r7.rgn19nm and l.ch_nursing_adm = r7.ch_nursing_adm
	left join ao_2017 as r8 on l.admday = r8.admday and l.admmnth = r8.admmnth  and l.rgn19nm = r8.rgn19nm and l.ch_nursing_adm = r8.ch_nursing_adm
	left join ac_2018 as r9 on l.admday = r9.admday and l.admmnth = r9.admmnth  and l.rgn19nm = r9.rgn19nm and l.ch_nursing_adm = r9.ch_nursing_adm
	left join ao_2018 as r10 on l.admday = r10.admday and l.admmnth = r10.admmnth  and l.rgn19nm = r10.rgn19nm and l.ch_nursing_adm = r10.ch_nursing_adm
	left join ac_2019 as r11 on l.admday = r11.admday and l.admmnth = r11.admmnth  and l.rgn19nm = r11.rgn19nm and l.ch_nursing_adm = r11.ch_nursing_adm
	left join ao_2019 as r12 on l.admday = r12.admday and l.admmnth = r12.admmnth  and l.rgn19nm = r12.rgn19nm and l.ch_nursing_adm = r12.ch_nursing_adm
	left join ac_2020 as r13 on l.admday = r13.admday and l.admmnth = r13.admmnth  and l.rgn19nm = r13.rgn19nm and l.ch_nursing_adm = r13.ch_nursing_adm
	left join ao_2020 as r14 on l.admday = r14.admday and l.admmnth = r14.admmnth  and l.rgn19nm = r14.rgn19nm and l.ch_nursing_adm = r14.ch_nursing_adm
	order by l.admmnth, l.admday, l.rgn19nm, l.ch_nursing_adm	;
quit;

data allyears2_adm;
	length admmnth admday 8;
	length rgn19nm $ 24;
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



/*
data allyears2_adm;
	set allyears_adm;
	if admother_2014 = . then admall_2014 = admch_2014;
	else admall_2014 = admch_2014 + admother_2014;
	if admother_2015 = . then admall_2015 = admch_2015;
	else admall_2015 = admch_2015 + admother_2015;
	if admother_2016 = . then admall_2016 = admch_2016;
	else admall_2016 = admch_2016 + admother_2016;
	if admother_2017 = . then admall_2017 = admch_2017;
	else admall_2017 = admch_2017 + admother_2017;
	if admother_2018 = . then admall_2018 = admch_2018;
	else admall_2018 = admch_2018 + admother_2018;
	if admother_2019 = . then admall_2019 = admch_2019;
	else admall_2019 = admch_2019 + admother_2019;
	if admother_2020 = . then admall_2020 = admch_2020;
	else admall_2020 = admch_2020 + admother_2020;

run;


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
*/
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