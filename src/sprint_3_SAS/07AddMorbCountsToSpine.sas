options obs = max;
*options obs = 10000;

*	Define the macro for adding individual counts to the basefile (flatfile);

%macro addfiletobase(addfile =);


proc sql;
	create table temp as
	select l.*, r.&addfile
	from basefile as l
	left join interim.&addfile as r
	on l.uid = r.uid;
quit;

data basefile;
	set temp;
run;

%mend addfiletobase;


data basefile;
	set clean.spine(keep = uid);
run;

proc sort data = clean.runfile out = runfile;
	by order;
run;

*	Loop through each of the count files, adding them to the basefile (flatfile);

data _null_;
	set runfile;
	by order;
	if first.order = 1 then do;
		if run = 1 or run = 2 then call execute('%addfiletobase('||'addfile ='||variable||');');
	end;
	if last.order;
run;

*	Join the basefile onto the spine and save as flatfile;

proc sql;
	create table interim.flatfile as
	select l.*, r.*
	from mmpi.mmpiss_bymonth as l
	left join basefile as r on l.uid = r.uid
	order by uid;
quit;

