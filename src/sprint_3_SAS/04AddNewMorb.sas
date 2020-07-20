options obs = max;
*options obs = 10000;


%let sourceAPCE = mysus.apce_int;

data diagcodes(keep = admdate dc uid);
	set &sourceAPCE(keep = admdate primdiag diag02 diag03 diag04 diag05 diag06 diag07 diag08 diag09 diag10 diag11 diag12
					diag13 diag14 diag15 diag16 diag17 diag18 diag19 diag20 diag21 diag22 diag23 diag24 uid);
	if primdiag ~= '' then do; dc = primdiag; output diagcodes; end;
	if diag02 ~= '' then do; dc = diag02; output diagcodes; end;
	if diag03 ~= '' then do; dc = diag03; output diagcodes; end;
	if diag04 ~= '' then do; dc = diag04; output diagcodes; end;
	if diag05 ~= '' then do; dc = diag05; output diagcodes; end;
	if diag06 ~= '' then do; dc = diag06; output diagcodes; end;
	if diag07 ~= '' then do; dc = diag07; output diagcodes; end;
	if diag08 ~= '' then do; dc = diag08; output diagcodes; end;
	if diag09 ~= '' then do; dc = diag09; output diagcodes; end;
	if diag10 ~= '' then do; dc = diag10; output diagcodes; end;
	if diag11 ~= '' then do; dc = diag11; output diagcodes; end;
	if diag12 ~= '' then do; dc = diag12; output diagcodes; end;
	if diag13 ~= '' then do; dc = diag13; output diagcodes; end;
	if diag14 ~= '' then do; dc = diag14; output diagcodes; end;
	if diag15 ~= '' then do; dc = diag15; output diagcodes; end;
	if diag16 ~= '' then do; dc = diag16; output diagcodes; end;
	if diag17 ~= '' then do; dc = diag17; output diagcodes; end;
	if diag18 ~= '' then do; dc = diag18; output diagcodes; end;
	if diag19 ~= '' then do; dc = diag19; output diagcodes; end;
	if diag20 ~= '' then do; dc = diag20; output diagcodes; end;
	if diag21 ~= '' then do; dc = diag21; output diagcodes; end;
	if diag22 ~= '' then do; dc = diag22; output diagcodes; end;
	if diag23 ~= '' then do; dc = diag23; output diagcodes; end;
	if diag24 ~= '' then do; dc = diag24; output diagcodes; end;
run;

*	Create the table of (present) diagnosis codes;

data dcodes;
	length dc4 $ 4;
	length dc3 $ 3;

	covid = 0;
	flupneumonia = 0;
	mih = 0;
	cogndysf = 0;

	set diagcodes;
	dc4 = substr(dc,1,4);
	dc3 = substr(dc,1,3);
	if dc4 in:('U071', 'U072') then covid = 1;
	if dc3 in:('J09', 'J10', 'J11', 'J12', 'J13', 'J14', 'J15', 'J16', 'J17', 'J18') then flupneumonia = 1;
	else if dc3 = 'F00' then do; mih = 1; end; 
	else if dc3 = 'F01' then do; mih = 1; end; 
	else if dc3 = 'F02' then do; mih = 1; end; 
	else if dc3 = 'F03' then do; mih = 1; end; 
	else if dc3 = 'F04' then do; mih = 1; end; 
	else if dc3 = 'F05' then do; mih = 1; end; 
	else if dc3 = 'F06' then do; mih = 1; end; 
	else if dc3 = 'F07' then do; mih = 1; end; 
	else if dc3 = 'F08' then do; mih = 1; end; 
	else if dc3 = 'F09' then do; mih = 1; end; 
	else if dc3 = 'F17' then do; mih = 1; end; 
	else if dc3 = 'F18' then do; mih = 1; end; 
	else if dc3 = 'F19' then do; mih = 1; end; 
	else if dc3 = 'F20' then do; mih = 1; end; 
	else if dc3 = 'F21' then do; mih = 1; end; 
	else if dc3 = 'F22' then do; mih = 1; end; 
	else if dc3 = 'F23' then do; mih = 1; end; 
	else if dc3 = 'F24' then do; mih = 1; end; 
	else if dc3 = 'F25' then do; mih = 1; end; 
	else if dc3 = 'F26' then do; mih = 1; end; 
	else if dc3 = 'F27' then do; mih = 1; end; 
	else if dc3 = 'F28' then do; mih = 1; end; 
	else if dc3 = 'F29' then do; mih = 1; end; 
	else if dc3 = 'F30' then do; mih = 1; end; 
	else if dc3 = 'F31' then do; mih = 1; end; 
	else if dc3 = 'F32' then do; mih = 1; end; 
	else if dc3 = 'F33' then do; mih = 1; end; 
	else if dc3 = 'F34' then do; mih = 1; end; 
	else if dc3 = 'F35' then do; mih = 1; end; 
	else if dc3 = 'F36' then do; mih = 1; end; 
	else if dc3 = 'F37' then do; mih = 1; end; 
	else if dc3 = 'F38' then do; mih = 1; end; 
	else if dc3 = 'F39' then do; mih = 1; end; 
	else if dc3 = 'F40' then do; mih = 1; end; 
	else if dc3 = 'F41' then do; mih = 1; end; 
	else if dc3 = 'F42' then do; mih = 1; end; 
	else if dc3 = 'F43' then do; mih = 1; end; 
	else if dc3 = 'F44' then do; mih = 1; end; 
	else if dc3 = 'F45' then do; mih = 1; end; 
	else if dc3 = 'F46' then do; mih = 1; end; 
	else if dc3 = 'F47' then do; mih = 1; end; 
	else if dc3 = 'F48' then do; mih = 1; end; 
	else if dc3 = 'F49' then do; mih = 1; end; 
	else if dc3 = 'F50' then do; mih = 1; end; 
	else if dc3 = 'F51' then do; mih = 1; end; 
	else if dc3 = 'F52' then do; mih = 1; end; 
	else if dc3 = 'F53' then do; mih = 1; end; 
	else if dc3 = 'F54' then do; mih = 1; end; 
	else if dc3 = 'F55' then do; mih = 1; end; 
	else if dc3 = 'F56' then do; mih = 1; end; 
	else if dc3 = 'F57' then do; mih = 1; end; 
	else if dc3 = 'F58' then do; mih = 1; end; 
	else if dc3 = 'F59' then do; mih = 1; end; 
	else if dc3 = 'F60' then do; mih = 1; end; 
	else if dc3 = 'F61' then do; mih = 1; end; 
	else if dc3 = 'F62' then do; mih = 1; end; 
	else if dc3 = 'F63' then do; mih = 1; end; 
	else if dc3 = 'F64' then do; mih = 1; end; 
	else if dc3 = 'F65' then do; mih = 1; end; 
	else if dc3 = 'F66' then do; mih = 1; end; 
	else if dc3 = 'F67' then do; mih = 1; end; 
	else if dc3 = 'F68' then do; mih = 1; end; 
	else if dc3 = 'F69' then do; mih = 1; end; 
	else if dc3 = 'F90' then do; mih = 1; end; 
	else if dc3 = 'F91' then do; mih = 1; end; 
	else if dc3 = 'F92' then do; mih = 1; end; 
	else if dc3 = 'F93' then do; mih = 1; end; 
	else if dc3 = 'F94' then do; mih = 1; end; 
	else if dc3 = 'F95' then do; mih = 1; end; 
	else if dc3 = 'F96' then do; mih = 1; end; 
	else if dc3 = 'F97' then do; mih = 1; end; 
	else if dc3 = 'F98' then do; mih = 1; end; 
	else if dc3 = 'F99' then do; mih = 1; end; 
	else if dc3 = 'R40' then do; cogndysf = 1; end; 
	else if dc3 = 'R41' then do; cogndysf = 1; end; 
	else if dc3 = 'R42' then do; cogndysf = 1; end; 
	else if dc3 = 'R43' then do; cogndysf = 1; end; 
	else if dc3 = 'R44' then do; cogndysf = 1; end; 
	else if dc3 = 'R45' then do; cogndysf = 1; end; 
	else if dc3 = 'R46' then do; cogndysf = 1; end; 
	else if dc3 = 'R47' then do; cogndysf = 1; end; 
	else if dc3 = 'R48' then do; cogndysf = 1; end; 
	else if dc3 = 'R49' then do; cogndysf = 1; end; 
run;

proc datasets lib = work memtype = data nolist;
	delete diagcodes;
quit;

proc sql;
	create table maxdcodes as
	select 
		uid, 
		max(covid) as covid  label = 'COVID-19', 
		max(flupneumonia) as flupneumonia  label = 'Influenza and pneumonia',
		max(mih) as mih  label = 'Mental ill health (IPOPAEGP)', 
		max(cogndysf) as cogndysf  label = 'Miscalleneous cognitive dysfunction (IPOPAEGP)'
	from dcodes
	group by uid, admdate;
quit;

proc sql;
	create table maxdcodes_2 as
	select l.uid, r.*
	from &sourceAPCE as l
	left join maxdcodes as r on l.uid = r.uid;
quit;

data maxdcodes_2;
	set maxdcodes_2;
	array change _numeric_;
		do over change;
			if change = . then change = 0;
		end;
run;


*	Delete dcodes file from the work library;

proc datasets lib = work memtype = data nolist;
	delete dcodes;
quit;

proc sql;
	create table mysus.apce as
	select 
		l.uid, l.tnrrcrdid, l.pid, /* start of added vars */ l.flagnhs, l.flagdate, l.flagprov, l.age, l.ethnicityrcrd, l.mobrcrd, l.yobrcrd, l.sexrcrd, /*end of added vars */
		l.maritalstatus, l.lsoa11pidpcd, l.gppraccode, l.pctpidpcd, l.ccgpidpcd, l.nhsnumtrcstatus, l.carersupind, l.legalstatusclass, 
		l.fyc, l.fyn, l.tnractmnth, l.admdate, l.admtime, l.decideadmdate, l.admmthd, l.admsource, l.reforgcode, l.intendmngmt, l.electwaitdur, l.admincat, l.waittimetype, 
		l.dischdate, l.dischtime, l.dischreadydate, l.dischmthd, l.dischdest, l.orgcodeambtrust, l.provcode, l.provcode3, l.pctgppracsus, l.gppraccodesus, l.tnrprovcode, 
		l.tnrprovsitecode, l.pctres, l.pctpcdsus, l.pctpcdsustype, l.pctgpsus, l.comcode, l.tnrcomcode, l.orgcoderesresp, 
		l.tnrdiagcnt, l.primdiag, l.diag02, l.diag03, l.diag04, l.diag05, l.diag06, l.diag07, l.diag08, l.diag09, l.diag10, l.diag11, l.diag12, l.diag13, l.diag14, 
		l.diag15, l.diag16, l.diag17, l.diag18, l.diag19, l.diag20, l.diag21, l.diag22, l.diag23, l.diag24, 
		r.covid, r.flupneumonia, r.mih, r.cogndysf, 
		l.chf, l.cpd, l.plegia, l.tumourmets, l.hiv, l.dementia, l.rheumdis, l.livermild, l.diabchrcomp, l.renaldis, l.cancer, l.livermodsev, l.myocardial, l.peptic, 
		l.cvd, l.diabnonchrcomp, l.pvd, l.arrhythmias, l.valvular, l.pulmcirc, l.hypertenuncomp, l.hypertencomp, l.paralysis, l.neuroother, l.diabuncomp, l.diabcomp, 
		l.hypothyroid, l.renalfail, l.liver, l.pepticnonbleed, l.lymphoma, l.tumournonmets, l.rheumarth, l.coagulopathy, l.obesity, l.weightloss, l.fluid, 
		l.anaemiabloodloss, l.anaemiadeficiency, l.alcoabuse, l.drugabuse, l.psychoses, l.depression, 
		l.frailanxdepress, l.fraildelirium, l.fraildementia, l.fraildepend, l.frailfallsfract, l.frailincont, l.frailmobilprob, l.frailpressulcers, l.frailsenility, 
		l.parrchf, l.parrcpd, l.parrdementia, l.parrdiabchrcomp, l.parrhemipara, l.parrtumourmets, l.parrlivermodsev, l.parrliverother, l.parrcancerother, 
		l.parrpvd, l.parrrenaldis, /* l.dx, * Removed RB 17/07/2017; */ l.tnrdiagall, 
		l.treatfunctcode, l.mainspeccode,/* l.operationstatus, l.tnrproccnt,*/ l.primproc,/* l.proc02, l.proc03, l.proc04, l.proc05, l.proc06, l.proc07, l.proc08, l.proc09, 
		l.proc10, l.proc11, l.proc12, l.proc13, l.proc14, l.proc15, l.proc16, l.proc17, l.proc18, l.proc19, l.proc20, l.proc21, l.proc22, l.proc23, l.proc24, l.tnrprocall, 
		l.ccstartdate01, l.ccstartdate02, l.ccstartdate03, l.ccstartdate04, l.ccstartdate05, l.ccstartdate06, l.ccstartdate07, l.ccstartdate08, l.ccstartdate09, 
		l.ccdischdate01, l.ccdischdate02, l.ccdischdate03, l.ccdischdate04, l.ccdischdate05, l.ccdischdate06, l.ccdischdate07, l.ccdischdate08, l.ccdischdate09, 
		l.ccunitfunct01, l.ccunitfunct02, l.ccunitfunct03, l.ccunitfunct04, l.ccunitfunct05, l.ccunitfunct06, l.ccunitfunct07, l.ccunitfunct08, l.ccunitfunct09, 
		l.procschiu, */l.pidclass, /*l.epinum, l.epistartdate, l.epistarttime, l.epistartdateward, l.epistartloctype, l.epitreatsitecode, l.epienddate, l.epiendtime, 
		l.lastepispellind, l.tnrmngmttype, l.fcedomproc, l.tnrepinum, l.tnrepilos, l.tnrepiadjlos, l.tnrspellid, l.tnrspellidtype, l.firstdaynightadm, l.hospspellnum, 
		l.hospspelldur, l.pctmthrgppracsus, l.nnatcarelevel, l.prevtotpreg, l.numbabies, l.psychpidstatus, l.pbrspellid, l.rttlensus,*/ l.dataset
	from &sourceAPCE as l
	left join maxdcodes_2 as r
	on l.uid = r.uid;
quit;

*	Delete maxcodes from the work library;

proc datasets lib = work memtype = data nolist;
	delete maxdcodes maxdcodes_2;
quit;


