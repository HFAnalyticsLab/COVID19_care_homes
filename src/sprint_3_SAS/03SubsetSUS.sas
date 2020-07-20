*	Create a data set of care home patients fr;

proc sql;
	create table clean.carehomepids as
	select distinct pid
	from mmpi.mmpiss_bymonth;
quit;

options obs = max;
*options obs = 10000;

%let myds = apce;

proc sql;
	create table mysus.&myds as
	select *
	from cleansus.&myds
	where pid in
		(select pid
		from clean.carehomepids);
quit;

%let myds = apcs;

proc sql;
	create table mysus.&myds as
	select *
	from cleansus.&myds
	where pid in
		(select pid
		from clean.carehomepids);
quit;

%let myds = aea;

proc sql;
	create table mysus.&myds as
	select *
	from cleansus.&myds
	where pid in
		(select pid
		from clean.carehomepids);
quit;