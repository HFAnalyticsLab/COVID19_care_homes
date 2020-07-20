
%let mysource = interim.apcs_3_1420_final_2_adm;
%let myext = 1420_f_reg_2;


proc sql;
	create table interim.admbytime_em_el_1420 as
	select ch_nursing_adm, spellstartdate, chadm, emergency, elective, count(spellstartdate) as freq, admday, admmnth, admyr
	from &mysource
	where chadm = 1 and admyr >= 2014
	group by ch_nursing_adm, spellstartdate, chadm, emergency, elective, admday, admmnth, admyr
	order by admyr, admmnth, admday, ch_nursing_adm, emergency, elective;
quit;

%let mycsv = 'IAU data\April 2020\01d Care Home Admissions Elective Emergency 1420.csv';
%let mysasfile = interim.admbytime_em_el_1420;

proc export data = &mysasfile outfile = &mycsv
	dbms = csv
	replace;
run;
