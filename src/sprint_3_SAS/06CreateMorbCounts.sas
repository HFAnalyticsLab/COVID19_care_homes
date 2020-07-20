
options obs = max;
*options obs = 10000;

*	Compile the macro needed (varCount);

%macro varCount(var1 =, ouf =, keyvar =, cnttype =, cntvar =, lfile =, rfile =, ljoinvar =, rjoinvar =, datevar =, index =, upperbound =, 
		lowerbound =);

proc sql;
	create table temp as
	select l.&keyvar, &cnttype(r.&cntvar) as &var1
	from &lfile as l
	left join &rfile as r
	on l.&ljoinvar = r.&rjoinvar 
		and r.&datevar < intnx('day',l.&index,&upperbound)
		and r.&datevar >= intnx('day',l.&index,&lowerbound)
	group by l.&keyvar;
quit;

data &ouf;
	set temp;
	array change _numeric_;
		do over change;
			if change = . then change = 0;
		end;
run;

%mend varCount;

*	Sorr the amended runfile by order;

proc sort data = clean.runfile out = runfile;
	by order;
run; 

*	Call the macro to create the e_pvd_h36 counts for each uid;

data _null_;
	set runfile;
	by order;
	if first.order = 1 then do;
		if run = 1 and macro = 'varCount' and optionalwhere = '' then do;
			call execute('%varCount('||'var1 ='||var1||', ouf ='||ouf||', keyvar ='||keyvar||', cnttype ='||cnttype||', cntvar ='||cntvar||
				', lfile ='||lfile||', rfile ='||rfile||', ljoinvar ='||ljoinvar||', rjoinvar ='||rjoinvar||
				', datevar ='||datevar||', index = '||index||', upperbound ='||upperbound||', lowerbound ='||lowerbound||');');
		end;
	end;
	if last.order then do;
	end;
run;