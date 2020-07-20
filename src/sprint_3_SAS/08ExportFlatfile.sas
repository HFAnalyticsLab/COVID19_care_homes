

data clean.CHResLTCbyMonth;
	set interim.flatfile;
run;

%let mycsv = 'IAU data\April 2020\CHResLTCbyMonth.csv';
%let mysasfile = clean.CHResLTCbyMonth;

proc export data = &mysasfile outfile = &mycsv
	dbms = csv
	replace;
run;