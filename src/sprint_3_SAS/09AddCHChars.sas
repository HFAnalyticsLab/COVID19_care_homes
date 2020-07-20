
data interim.carehome interim.nonch;
	set interim.apcs_3_1420_reg_selected;
	if chidadm ~= '' or chiddisch ~= '' then output interim.carehome;
	else output interim.nonch;
run;

proc sql;
	create table interim.apcs_3_1420_reg_sel_chchar as
	select r1.uid as uid_ach_adm, r1.assigned_tnrchid as assigned_chidadm, r1.tnrchid as tnrchid_bm_adm, r2.uid as uid_ach_disch, r2.assigned_tnrchid as assigned_chiddisch, r2.tnrchid as tnrchid_bm_disch, l.*
	from interim.carehome as l
	left join interim.mmpiss_bymonth_chchars_region_IM as r1 on l.pid = r1.pid and l.spellstartdate >= r1.datefrom and l.spellstartdate <= r1.dateto
	left join interim.mmpiss_bymonth_chchars_region_IM as r2 on l.pid = r2.pid and l.spellenddate >= r2.datefrom and l.spellenddate <= r2.dateto
	order by pid, spellid;
quit;


data interim.apcs_3_1420_reg_sel_pre_prov;
	set interim.apcs_3_1420_reg_sel_chchar interim.nonch;
run;

*	Add the provider codes on;

options obs = max;
*options obs = 100000;

proc sql;
	create table interim.apcs_3_1420_reg_sel_post_prov as
	select r.Organisation_Name, r.Commissioning_Region, r.Higher_Health_Authority, r.source, l.*
	from interim.apcs_3_1420_reg_sel_pre_prov as l
	left join interim.all_provider_codes as r on l.tnrprovsitecode = r.Org_Code
	order by pid, spellid;
quit;




options obs = max;
*options obs = 100000;

proc sql;
	create table interim.apcs_3_1420_reg_sel_post_prov_2 as
	select 	r.Organisation_Name as prov_name, r.Commissioning_Region as prov_reg, r.source as prov_HHA, r.source as source_prov, 
			l.Organisation_Name as site_name, l.Commissioning_Region as site_reg, l.source as site_HHA, l.source as source_site,
			l.*
	from interim.apcs_3_1420_reg_sel_post_prov as l
	left join interim.all_provider_codes as r on l.provcode3 = r.Org_Code
	order by pid, spellid;
quit;


options obs = max;
*options obs = 100000;

data 	interim.carehome(keep = uid spellid pid spellstartdate spellenddate chadm chdisch assigned_chidadm assigned_chiddisch rgn19nm lad19nm regsource deathdate 
							sex dob age ccg15_disch pidclass lsoa11pidpcd gppraccode ccgpidpcd fyn tnractmnth admmthd admsource admincat dischdate dischmthd dischdest 
							provcode provcode3 tnrprovsitecode prov_name prov_reg source_prov site_name 
							primdiag covid covid_prim flupneumonia
							elective elod emergency maternity birth transfer otheradm avoidable bedsore emacuprimcare emchrprimcare emchracs emucsc 
							uidfirstepi uidlastepi uidadm uiddisch admday admmnth admyr dischday dischmnth dischyr  
							rgn19nm_adm rgn19nm_disch rgn19nm_sus uid_ach_adm uid_ach_disch) 
		interim.nonch(keep = uid spellid pid spellstartdate spellenddate chadm chdisch assigned_chidadm assigned_chiddisch rgn19nm lad19nm regsource deathdate 
							sex dob age ccg15_disch pidclass lsoa11pidpcd gppraccode ccgpidpcd fyn tnractmnth admmthd admsource admincat dischdate dischmthd dischdest 
							provcode provcode3 tnrprovsitecode prov_name prov_reg source_prov site_name 
							primdiag covid covid_prim flupneumonia
							elective elod emergency maternity birth transfer otheradm avoidable bedsore emacuprimcare emchrprimcare emchracs emucsc 
							uidfirstepi uidlastepi uidadm uiddisch admday admmnth admyr dischday dischmnth dischyr  
							rgn19nm_adm rgn19nm_disch rgn19nm_sus uid_ach_adm uid_ach_disch)  
		interim.apcs_3_1420_supplementary(keep = uid lad19cd rgn19cd site_reg site_HHA source_site tnrchid_bm_adm tnrchid_bm_disch 
							lsoa_adm lad19cd_adm lad19nm_adm rgn19cd_adm lsoa_disch lad19cd_disch lad19nm_disch rgn19cd_disch lad19cd_sus lad19nm_sus rgn19cd_sus 
							dfadm dtadm chidadm dfdisch dtdisch chiddisch);
	retain uid spellid pid spellstartdate spellenddate chadm chdisch assigned_chidadm assigned_chiddisch rgn19nm lad19nm regsource deathdate 
							sex dob age ccg15_disch pidclass lsoa11pidpcd gppraccode ccgpidpcd fyn tnractmnth admmthd admsource admincat dischdate dischmthd dischdest 
							provcode provcode3 tnrprovsitecode prov_name prov_reg source_prov site_name 
							primdiag covid covid_prim flupneumonia
							elective elod emergency maternity birth transfer otheradm avoidable bedsore emacuprimcare emchrprimcare emchracs emucsc 
							uidfirstepi uidlastepi uidadm uiddisch admday admmnth admyr dischday dischmnth dischyr  
							rgn19nm_adm rgn19nm_disch rgn19nm_sus uid_ach_adm uid_ach_disch;
	uid + 1;
	set interim.apcs_3_1420_reg_sel_post_prov_2(drop = Organisation_Name Commissioning_Region Higher_Health_Authority source);
	covid_prim = 0;
	length dc4 $ 4;
	if covid = 1 then do;
		dc4 = substr(primdiag,1,4);
		if dc4 in:('U071', 'U072') then covid_prim = 1;
	end;
	if assigned_chidadm ~= . or assigned_chiddisch ~= . then output interim.carehome;
	else output interim.nonch;
	output interim.apcs_3_1420_supplementary;
run;


/*
Add care home characteristics
Add covid_pd flag
Rationalise
Add UID
Split
*/


options obs = max;
*options obs = 100000;


proc sql;
	create table interim.carehome_2 as
	select 	l.uid, l.spellid, l.pid, l.spellstartdate, l.spellenddate, l.chadm, l.chdisch, 
			l.assigned_chidadm, r1.ch_beds as ch_beds_adm, r1.ch_type as ch_type_adm, r1.ch_nursing as ch_nursing_adm, r1.ch_residential as ch_residential_adm, 
			l.assigned_chiddisch, r2.ch_beds as ch_beds_disch, r2.ch_type as ch_type_disch, r2.ch_nursing as ch_nursing_disch, r2.ch_residential as ch_residential_disch, 
			l.*
	from interim.carehome as l
	left join interim.mmpiss_bymonth_chchars_region_IM as r1 on l.uid_ach_adm = r1.uid 
	left join interim.mmpiss_bymonth_chchars_region_IM as r2 on l.uid_ach_disch = r2.uid 
order by l.uid;

data interim.apcs_3_1420_unsorted;
	set interim.carehome_2 interim.nonch;
run;

proc sort data = interim.apcs_3_1420_unsorted out = interim.apcs_3_1420_final;
	by uid;
run;





options obs = max;
*options obs = 10000;

data same diff;
	set interim.apcs_3_1420_reg_sel_chchar;
	if tnrchid_bm_adm ~= chidadm or tnrchid_bm_disch ~= chiddisch then output diff;
	else output same;
run;

data interim.diff;
	set diff;
run;

proc sql;
	create table interim.diff_recs as
	select *
	from interim.mmpiss_bymonth_chchars_region_IM
	where pid in
		(select pid
		from interim.diff)
	order by pid;
quit;


data tdiff;
	set diff(keep = uid_ach_adm
assigned_chidadm
tnrchid_bm_adm
assigned_chiddisch
tnrchid_bm_disch
chdisch
chadm
dfadm
dtadm
chidadm
pid
spellstartdate
spellenddate
dfdisch
dtdisch
chiddisch
uiddisch
spellid);
run;


%varlist(inf = interim.apcs_3_1420_reg_sel_chchar, ouf = vlchchar, props = name type length format varnum);