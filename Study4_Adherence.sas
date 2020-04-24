OPTIONS NOFMTERR;
libname pub 'Z:\PBS 10% Sample\Projects\HAART (JC)\pub';
Libname up 'Z:\PBS 10% Sample\Projects\HAART (JC)\up';
libname patients 'Z:\PBS 10% Sample\Source Data';
LIBNAME pbs10 'Z:\PBS 10% Sample\Source Data';
LIBNAME pbsold 'Z:\PBS 10% Sample\Source Data\Processing Files\Previous supply files';
LIBNAME S85 'J:\MPRU\Data Resources\PBS Section 85';

*Mel;
LIBNAME haart 'R:\Pearson\PBS 10% Sample\Projects\HAART (JC)';

proc freq data = up.haart4d_updated;
table file_name;
run;

*Import list of haart medicine codes;
PROC IMPORT OUT= haart_codes 
            DATAFILE= "Z:\PBS 10% Sample\Projects\HAART (JC)\pub\haart_codesv2.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
DATA pub.haart_codesv2; SET haart_codes; RUn;

data pbsdrugmap;
	set s85.pbsdrugmap;
	run;

*Extract all dispensings of haart meds into one file, considering already the observation window of the study;
%MACRO haart;

%DO year=2013 %TO 2018;

PROC SQL; CREATE TABLE y&year.supply AS
	SELECT * FROM PBS10.y&year.supply A
		INNER JOIN pub.haart_codesv2 (KEEP=pbs_code) B
			ON A.item_code=B.pbs_code 
		INNER JOIN pbsdrugmap C
			ON A.item_code=C.item_code
		WHERE '1JAN2013'd<=supp_date<='31DEC2018'd; 
QUIT;
%END;
 
DATA haart2; SET y2013supply; RUN;
%DO year=2014 %TO 2018;
	PROC APPEND DATA=y&year.supply BASE=haart2 FORCE; RUN;
%END;

%MEND; 

%haart;

proc freq data = haart2;
table item_code;
run;

*Merge with ID file;
PROC SQL; CREATE TABLE haart3 AS
	SELECT * FROM haart2 A
		INNER JOIN PBS10.patient_ids B ON
			A.pat_id=B.pat_id; QUIT;

*Create counts;
PROC SQL; CREATE TABLE count AS
	SELECT DISTINCT drug_name, year(supp_date) AS year, COUNT(*) AS disp_cnt LABEL='No. dispensings', 
			COUNT(DISTINCT pat_id) AS pers_cnt LABEL='No. people'
	FROM haart3 GROUP BY year(supp_date), drug_name; QUIT;

	proc export data = count outfile = 'Z:\PBS 10% Sample\Projects\HAART (JC)\haart_count092019.csv' dbms = csv; run;
	
*Merge with authorities file;
PROC SQL; CREATE TABLE haart4a AS
	SELECT * FROM haart3 A
		left outer JOIN PBS10.authority B ON
			A.pat_id=B.pat_id and A.item_code=B.auth_item_code and A.presc_date=B.auth_appr_dte and A.presc_id=B.presc_id; QUIT;

			proc export data = haart4a outfile = 'Z:\PBS 10% Sample\Projects\HAART (JC)\haart_database_incl_authority092019.csv'replace dbms = csv; run;

libname rest 'J:\MPRU\Data Resources\PBS Item Codes Mapped to Restriction';

data pbs_restriction_code_map (drop=ind_id1);
length ind_id 8.;
set rest.pbs_restriction_code_map (rename=ind_id=ind_id1);
	ind_id=ind_id1;
	run;
	proc sort data = pbs_restriction_code_map out= pbs_restriction_code_map_p (drop= pbs_code)nodupkey;
	by ind_id;run;

data pbs_rest;
	set pbs_restriction_code_map;
	if findw(Rest_text, 'HIV') then HIV = 1;
	if findw(Rest_text, 'hepatitis') or findw(Rest_text, 'Hepatitis') then HEP = 1;
	run;

data pbs_rest2 ;
	set pbs_rest;
	if hiv = 1 or hep = 1 then output;
run;
	proc sort data = pbs_rest2 out = pbs_rest1 (drop=pbs_code)nodupkey;
	by ind_id;run;
	
*Merge with authorities file;
PROC SQL; CREATE TABLE haart4b AS
	SELECT * FROM haart4a A
		left outer JOIN pbs_rest1  B ON
			A.rstr_num=B.ind_id ; QUIT;


*renaming variables so they are appended to dataset and I can see what matches the streamlined codes;
data pbs_rest1a (drop=rest_text HIV HEP ind_id);
	set pbs_rest1;
	rest_text1 = rest_text;
	HIV1 = HIV;
	HEP1 = HEP;
	IND_ID1 = IND_ID;
	run;

*Merge with authorities file;
PROC SQL; CREATE TABLE haart4c AS
	SELECT * FROM haart4b A
		left outer JOIN pbs_rest1a  B ON
		 A.STREAM_AUTH_CODE=B.ind_id1; QUIT;

proc freq data = haart4c;
		table ind_id*ind_id1/list missing;
run;
proc sort data = haart4c;
	by pat_id supp_date;run;
	
data miss_restr_text;
	set haart4c;
	if rest_text = '' then output;run;
	
data miss_restr_text2;
	set miss_restr_text;
	where stream_auth_code = . and rstr_num = .;run;

proc sort data = miss_restr_text2 out= miss_restr_text2_p nodupkey;
	by item_code;
run;

data haart4c;
	set haart4c;
	year = year(supp_date);
	month = month(supp_date);
run;

proc freq data = haart4c;
		table HIV*HIV1*HEP*HEP1/list missing;
run;

proc sort data = haart4c out = up.haart4d_updated; *Updated full database;
	by pat_id supp_date;
run;
******;

proc sort data = up.haart4d_updated out = haart_all_HIV_HEP_Codes;
	by pat_id supp_date;
run;
proc sort data = haart_all_HIV_HEP_Codes out = haart_all_HIV_HEP_Codes_p nodupkey;
	by pat_id;
run;

*Keep only those dispensings where there is no hep flag;

data up.haart_hiv_only (drop=hep);
	set haart_all_HIV_HEP_Codes;
	where hep ne 1;
run;

proc sort data = up.haart_hiv_only;
	by pat_id supp_date drug_name;
	run;

*Original dispensings of only HIV patients = up.haart_hiv_only;

	proc freq data = up.haart_hiv_only;
	table item_code;
	run;

*************************************************************
  PREPARE DISPENSING DATABASE FOR ADHERENCE CALCULATIONS
*************************************************************

*To have each medicine dispensed in one line, we have to remove combos; 

proc sort data = up.haart_hiv_only; by pat_id supp_date; run;

*Create a separate dataset only with combos;
data combo_HIV;
	set up.haart_hiv_only;
	if findw(drug_name, '&')or findw(drug_name, '+') or findw(drug_name, 'with') then output;
run;

*Check if selection worked;
proc sort data = combo_HIV; by drug_name; run;

proc freq data = combo_HIV; table drug_name; run; *its looks fine, only combos;

* Remove combinations from main data so don't double count;
data nocombo_HIV;
	set up.haart_hiv_only;
	if findw(drug_name, '&')or findw(drug_name, '+') or findw(drug_name, 'with') then delete;
run;

*Check if selection worked;
proc sort data = nocombo_HIV; by drug_name; run;

proc freq data = nocombo_HIV; table drug_name; run; *its looks fine;

*separate string based on delimiters within the '';
*Creates a new row for each drug in the combo dataset;
data combo_HIV (drop = drug drug2 i);
	length drug $50.;
	set combo_HIV (rename=drug_name=drug_name1 rename=atc_code=atc_code1 rename=form_strength=form_strength1);
	do i=1 by 1 while(scan(drug_name1, i, '+,&,with')^='');
	drug = scan(drug_name1,i,'+,&,with');
	drug2 = compress(drug, '()');
	drug_name = strip(drug2);
	output;
	end;
	run;
	
*Check;
title 'Combination mapping';
proc freq data = combo_HIV; table drug_name1*drug_name/list norow nocol missing;run; title; *worked;

*Flag STR;
data combo_HIV;
set combo_HIV;
if drug_name1 in ('DOLUTEGRAVIR + ABACAVIR + LAMIVUDINE', 'EMTRICITABINE + RILPIVIRINE + TENOFOVIR ALAFENAMIDE', 'TENOFOVIR + EMTRICITABINE + EFAVIRENZ', 'TENOFOVIR + EMTRICITABINE + ELVITEGRAVIR + COBICISTAT', 'TENOFOVIR + EMTRICITABINE + RILPIVIRINE', 'TENOFOVIR ALAFENAMIDE + EMTRICITABINE + ELVITEGRAVIR + COBICISTAT') then STR = 1;
run;

proc contents data = combo_HIV; run;

proc contents data = nocombo_HIV; run;

data combo_HIV;
  length drug_name $200;
  format drug_name $200.;
  set combo_HIV;
  run;

*Merge combination products back onto main data;
data up.haart_hiv4b;
	set combo_HIV nocombo_HIV;
run;

proc sort data = up.haart_hiv4b; by pat_id supp_date drug_name; run;

proc freq data = up.haart_hiv4b; table drug_name item_code; run;

*Get data from up.haart_hiv4b, which does not contain STR and components of MTR are broken into individual components and was updated with last data, up to December 2018;
 
*Calculate duration of dispensings based on pack size and Australian Guidelines for antiretroviral medicines;

*Import list of antiretroviral codes with factors (= denominator adjusting number of pills taken per day based on recommended doses in Australian guidelines);
PROC IMPORT OUT= factor 
            DATAFILE= "Z:\PBS 10% Sample\Projects\HAART (JC)\Quantity_distribution.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
DATA factor; SET factor(keep = item_code factor); RUn;
proc sort data = factor; by ITEM_CODE; run;
proc sort data = up.haart_hiv4b; by ITEM_CODE; RUN;

data up.haart_hiv4c;
merge up.haart_hiv4b (in = a) factor (in = b);
by ITEM_CODE;
if a;
run;

data up.haart_hiv4c;
set up.haart_hiv4c;
duration = ((qty*scripts)/factor);
run;

*Checkings;
proc means data =  up.haart_hiv4c;
var qty;
class drug_name;
run;

proc means data =  up.haart_hiv4c;
var duration;
class drug_name;
run;

proc freq data =  up.haart_hiv4c;
table duration;
run;

*Dataset with dispensings in the observation window;

data up.haart_hiv4c_OW;
set up.haart_hiv4c;
where '1JAN2013'd <=supp_date<='31Dec2018'd;
run;

**********************************************************
                      COHORT SELECTION
**********************************************************;

*Remove duplicate dispensings to create regimens within 30 days;
proc sort data = up.haart_hiv4c_OW out= haart_regimen nodupkey;
	by pat_id supp_date drug_name;
run;

*Transpose to dispensings in 30 days;
data haart_hiv5;
	set haart_regimen;
	retain prev_supp_date treat;
	by pat_id;
	if first.pat_id then do; 
	prev_supp_date = supp_date;
	treat = 1;
	end;
	if (supp_date - prev_supp_date) ge 30 then do;
	prev_supp_date = supp_date;
	treat + 1;
	end;
	format prev_supp_date ddmmyy10.;
	date_diff = supp_date - prev_supp_date;
run;

*Create table with date of supply of treatments;
proc sort data = haart_hiv5; by pat_id supp_date treat; run;

data haart_hiv5_last (keep = pat_id supp_date pharmacy_state YEAR_BIRTH YEAR_DEATH drug_name drug_name1 date_diff treat STR);
	set haart_hiv5;
	by pat_id treat;
	if last.treat then output;
run;

data haart_hiv5_first (keep = pat_id supp_date treat);
	set haart_hiv5;
	by pat_id treat;
	if first.treat then output;
run;

*Check;
proc print data = haart_hiv5;
var pat_id supp_date presc_date drug_name drug_name1 prev_supp_date date_diff treat duration;
where pat_id in (4771442341, 704499561);
run;

*Remove duplicates within 30 days;
proc sort data = haart_hiv5 out = haart_hiv5_u nodupkey; *deleted 8223 observations;
by pat_id treat drug_name;
run;

proc transpose data = haart_hiv5_u out=haart_hiv_30days (drop = _name_);
	var drug_name;
	by pat_id treat;
	run;

*Exclude patients using only tenofovir or lamivudine in the study period. 
Worry about lamivudine or tenofovir disoproxil. Tenofovir in combos is named 'tenofovir' or 'tenofivir alafenamide', so we don't need to consider these others names;
proc freq data = up.haart4d_updated; *Check in the original database without breaking components;
table drug_name;
run;

*Flag patients using only tenofovir or lamivudine in the study period;
data hiv_only_30day; 
	set haart_hiv_30days;
    by pat_id;
	prev_drug = lag(col1);	
	if first.pat_id then prev_drug = '';
	if col1 in ('LAMIVUDINE') and col2 in (' ') and (prev_drug in ('TENOFOVIR DISOPROXIL',' ')) then hep_1only = 0;
	else if col1 in ('LAMIVUDINE') and col2 in (' ') and (prev_drug in ('LAMIVUDINE',' '))then hep_1only = 0;
	else if col1 in ('TENOFOVIR DISOPROXIL') and col2 in (' ') and (prev_drug in ('LAMIVUDINE',' '))then hep_1only = 0;
    else if col1 in ('TENOFOVIR DISOPROXIL') and col2 in (' ') and (prev_drug in ('TENOFOVIR DISOPROXIL',' '))then hep_1only = 0;
	ELSE hep_1only = 1;
run;

*Remove only tenofovir or lamivudine (HEP B patients without restriction code for HEP);
PROC SQL; CREATE TABLE Hep_only AS
	SELECT DISTINCT pat_id, 
    SUM (hep_1only) AS hep 
	FROM hiv_only_30day
GROUP BY pat_id;
QUIT;

data Hep_only;
set Hep_only;
if hep = 0 then hep_only = 1;
run; 

*Merging back into dispensings to exclude patients receiving only hep drugs; 
data HIV_ONLY;
	merge  Hep_only (in=a DROP = HEP) hiv_only_30day (in=b); 
	by pat_id;
	if b;
run;

data haart_hiv_sameday_30;
	set HIV_ONLY;
	drugs_all = catx('/', of col1 - - col9);
	where hep_only = .;
run;

*Assign last supp date to treatments of included patients as suup_date and assign the first supp_date of treatments to control for possible grouping treatments as a regimen when they are a switch;
proc sort data = haart_hiv_sameday_30; by pat_id treat; run;
proc sort data = haart_hiv5_last; by pat_id treat; run;
proc sort data = haart_hiv5_first; by pat_id treat; run;

data Hiv_only_disp (drop = hep_only hep_1only date_diff prev_drug drug_name);
	merge  haart_hiv_sameday_30 (in=a) haart_hiv5_last (in=b) haart_hiv5_first (rename =(supp_date = first_supp_treat) in=c);
	by pat_id treat;
	if a;
run;

*Assign first date of supply;
proc sort data = Hiv_only_disp; by pat_id supp_date; run;

data hiv_cohort_person (keep=pat_id first_supp first_supp_treat1);
	set Hiv_only_disp; 
	by pat_id;
	first_supp = supp_date;
	first_supp_treat1 = first_supp_treat;
	format first_supp first_supp_treat1 ddmmyy.;
    if first.pat_id then output;
run;

*Merge back to include the first_supp date;
data up.Hiv_only_disp;
	merge Hiv_only_disp (in=a) hiv_cohort_person (in=b) ;
	by pat_id;
	if b;
run;

*************Check to remove only incomplete dispensings;
data Hiv_only_disp2;
	set up.Hiv_only_disp;
	N_ARV = 0;
	if findw(drugs_all,'ABACAVIR') then N_ARV + 1;
	if findw(drugs_all,'DIDANOSINE') then N_ARV + 1;
	if findw(drugs_all,'EMTRICITABINE') then N_ARV + 1;
	if findw(drugs_all,'LAMIVUDINE') then N_ARV + 1;
	if findw(drugs_all,'STAVUDINE') then N_ARV + 1;
	if findw(drugs_all,'TENOFOVIR') then N_ARV + 1;
	if findw(drugs_all,'ZALTACIBINE') then N_ARV + 1;
	if findw(drugs_all,'ZIDOVUDINE') then N_ARV + 1;
	if findw(drugs_all,'DELAVIRDINE') then N_ARV + 1;
	if findw(drugs_all,'EFAVIRENZ') then N_ARV + 1;
	if findw(drugs_all,'NEVIRAPINE') then N_ARV + 1;
	if findw(drugs_all,'ETRAVIRINE') then N_ARV + 1;
	if findw(drugs_all,'RILPIVIRINE') then N_ARV + 1;
    if findw(drugs_all,'AMPRENAVIR') then N_ARV + 1;
	if findw(drugs_all,'ATAZANAVIR')then N_ARV + 1;
	if findw(drugs_all,'DARUNAVIR')then N_ARV + 1;
	if findw(drugs_all,'FOSAMPRENAVIR')then N_ARV + 1;
	if findw(drugs_all,'INDINAVIR')then N_ARV + 1;
	if findw(drugs_all,'LOPINAVIR')then N_ARV + 1;
	if findw(drugs_all,'NELFINAVIR')then N_ARV + 1;
	if findw(drugs_all,'SAQUINAVIR')then N_ARV + 1;
	if findw(drugs_all,'TIPRANAVIR') then N_ARV + 1;
	if findw(drugs_all,'ELVITEGRAVIR')then N_ARV + 1;
    if findw(drugs_all,'DOLUTEGRAVIR') then N_ARV + 1;
	if findw(drugs_all,'RALTEGRAVIR') then N_ARV + 1;
	if findw(drugs_all,'ENFUVIRTIDE') then N_ARV + 1;
	if findw(drugs_all,'MARAVIROC') then N_ARV + 1;
run;

Title "Number of ART dispensed in 30 days";
proc freq data= Hiv_only_disp2;
table N_ARV;
run;
title;

proc freq data= Hiv_only_disp2;
table drugs_all;
run;

data Hiv_only_disp2;
	set Hiv_only_disp2;
	NRTI = 0;
	NNRTI = 0;
	PI = 0;
	INSTI = 0;
	EI = 0;
	Enh = 0;
	if findw(drugs_all,'ABACAVIR') then NRTI + 1;
	if findw(drugs_all,'DIDANOSINE') then NRTI + 1;
	if findw(drugs_all,'EMTRICITABINE') then NRTI + 1;
	if findw(drugs_all,'LAMIVUDINE') then NRTI + 1;
	if findw(drugs_all,'STAVUDINE') then NRTI + 1;
	if findw(drugs_all,'TENOFOVIR') then NRTI + 1;
    if findw(drugs_all,'ZALTACIBINE') then NRTI + 1;
	if findw(drugs_all,'ZIDOVUDINE') then NRTI + 1;
	if findw(drugs_all,'DELAVIRDINE') then NNRTI + 1;
	if findw(drugs_all,'EFAVIRENZ') then NNRTI + 1;
	if findw(drugs_all,'NEVIRAPINE') then NNRTI + 1;
	if findw(drugs_all,'ETRAVIRINE') then NNRTI + 1;
	if findw(drugs_all,'RILPIVIRINE') then NNRTI + 1;
    if findw(drugs_all,'AMPRENAVIR') then PI + 1;
	if findw(drugs_all,'ATAZANAVIR')then PI + 1;
	if findw(drugs_all,'DARUNAVIR')then PI + 1;
	if findw(drugs_all,'FOSAMPRENAVIR')then PI + 1;
	if findw(drugs_all,'INDINAVIR')then PI + 1;
	if findw(drugs_all,'LOPINAVIR')then PI + 1;
	if findw(drugs_all,'NELFINAVIR')then PI + 1;
	if findw(drugs_all,'SAQUINAVIR')then PI + 1;
	if findw(drugs_all,'TIPRANAVIR') then PI + 1;
	if findw(drugs_all,'ELVITEGRAVIR')then INSTI + 1;
    if findw(drugs_all,'DOLUTEGRAVIR') then INSTI + 1;
	if findw(drugs_all,'RALTEGRAVIR') then INSTI + 1;
	if findw(drugs_all,'ENFUVIRTIDE') then EI + 1;
	if findw(drugs_all,'MARAVIROC') then EI + 1;
	if findw(drugs_all,'COBICISTAT') then Enh + 1;
	if findw(drugs_all,'RITONAVIR')then Enh + 1;
run;

data Hiv_only_disp2;
set Hiv_only_disp2;
if NRTI = 2 then NRTI_2 = 1;
else NRTI_2 = 0;
run;

proc freq data = Hiv_only_disp2;
table NRTI_2;
run;

*Regimens by class; 
data Hiv_only_disp2;
set Hiv_only_disp2;
length regimen $21.;
if findw(drugs_all,'MARAVIROC') OR findw(drugs_all,'ENFUVIRTIDE') THEN regimen = 'EI-based';
else if findw(drugs_all,'ETRAVIRINE')THEN regimen = 'Etra-based';
ELSE IF NRTI_2 = 1 and NNRTI = 1 and INSTI = 0 and PI = 0 and Enh = 0 and EI = 0 THEN regimen = '2NRTI + NNRTI';
else if NRTI_2 = 1 and NNRTI = 1 and INSTI = 1 and PI = 0 and Enh = 0 and EI = 0 THEN regimen = '2NRTI+NNRTI+INSTI';
else if NRTI_2 = 1 and INSTI = 1 and NNRTI = 0 and PI = 0 and Enh = 0 and EI = 0 THEN regimen = '2NRTI + INSTI';
else if NRTI_2 = 1 and PI = 0 and Enh = 1 and NNRTI = 0 and INSTI = 1 and EI = 0 THEN regimen = '2NRTI + INSTI';
else if NRTI_2 = 1 and PI = 1 and NNRTI = 0 and INSTI = 0 and Enh = 0 and EI = 0 THEN regimen = '2NRTI + PI';
else if NRTI_2 = 1 and PI = 1 and NNRTI = 0 and INSTI = 0 and Enh = 1 and EI = 0 THEN regimen = '2NRTI + PI';
else if NRTI_2 = 1 and PI = 1 and NNRTI = 0 and INSTI = 1 and Enh = 1 and EI = 0 THEN regimen = '2NRTI+INSTI+PI';
else if NRTI_2 = 1 and PI = 1 and NNRTI = 0 and INSTI = 1 and Enh = 0 and EI = 0 THEN regimen = '2NRTI+INSTI+PI';
else if NRTI_2 = 1 and drugs_all in('EMTRICITABINE/TENOFOVIR','EMTRICITABINE/TENOFOVIR DISOPROXIL', 'EMTRICITABINE/TENOFOVIR ALAFENAMIDE')THEN regimen = '2NRTI';
else if NRTI_2 = 0 and NRTI = 3 and INSTI = 0 and NNRTI = 0 and PI = 0 and Enh = 0 and EI = 0 THEN regimen = '3NRTI';
else if NRTI_2 = 0 and PI = 1 and INSTI = 1 and NNRTI = 0 and EI = 0 and Enh = 1 and NRTI = 0 THEN regimen = 'INSTI+PI/Enh';
else if NRTI_2 = 0 and NNRTI = 1 and PI = 1 and INSTI = 0 and Enh = 1 and EI = 0 and NRTI = 0 THEN regimen = 'NNRTI+PI/Enh';
else if NRTI_2 = 0 and NNRTI = 0 and PI = 1 and INSTI = 0 and Enh = 1 and EI = 0 and NRTI = 1 THEN regimen = 'NRTI+PI/Enh';
else if NRTI_2 = 0 and NNRTI = 0 and PI = 1 and INSTI = 0 and Enh = 1 and EI = 0 and NRTI = 0 THEN regimen = 'PI/Enh';
else if NRTI_2 = 0 and NNRTI = 1 and PI = 0 and INSTI = 1 and Enh = 0 and EI = 0 and NRTI = 0 THEN regimen = 'NNRTI+INSTI';
else if NRTI_2 = 0 and NNRTI = 0 and PI = 1 and INSTI = 1 and Enh = 1 and EI = 0 and NRTI = 1 THEN regimen = 'NRTI+INSTI+PI';
else if NRTI_2 = 0 and NNRTI = 0 and PI = 1 and INSTI = 1 and Enh = 0 and EI = 0 and NRTI = 1 THEN regimen = 'NRTI+INSTI+PI';
else if NRTI_2 = 1 and NNRTI = 1 and INSTI = 0 and PI = 1 and Enh = 0 and EI = 0 THEN regimen = '2NRTI+NNRTI+PI';
else if NRTI_2 = 1 and NNRTI = 1 and INSTI = 0 and PI = 1 and Enh = 1 and EI = 0 THEN regimen = '2NRTI+NNRTI+PI';
else if NRTI_2 = 0 and NNRTI = 1 and PI = 1 and INSTI = 1 and Enh = 1 and EI = 0 and NRTI = 0 THEN regimen = 'NNRTI+INSTI+PI';
else if NRTI_2 = 0 and NNRTI = 1 and PI = 1 and INSTI = 1 and Enh = 0 and EI = 0 and NRTI = 0 THEN regimen = 'NNRTI+INSTI+PI';
else if NRTI_2 = 0 and NNRTI = 1 and PI = 0 and INSTI = 1 and Enh = 0 and EI = 0 and NRTI = 1 THEN regimen = 'NRTI+INSTI+NNRTI';
else if NRTI_2 = 0 and NRTI = 3 and INSTI = 0 and NNRTI = 0 and PI = 1 and Enh = 0 and EI = 0 THEN regimen = '3NRTI+PI';
else if NRTI_2 = 0 and NRTI = 3 and INSTI = 0 and NNRTI = 0 and PI = 1 and Enh = 1 and EI = 0 THEN regimen = '3NRTI+PI';
else if NRTI_2 = 0 and NRTI = 3 and INSTI = 0 and NNRTI = 1 and PI = 0 and Enh = 0 and EI = 0 THEN regimen = '3NRTI+NNRTI';
else if NRTI_2 = 0 and NRTI = 3 and INSTI = 1 and NNRTI = 0 and PI = 0 and Enh = 0 and EI = 0 THEN regimen = '3NRTI+INSTI';
else if NRTI_2 = 0 and NNRTI = 2 and PI = 0 and INSTI = 1 and Enh = 0 and EI = 0 and NRTI = 0 THEN regimen = '2NNRTI+INSTI';
ELSE regimen = 'Other';
run;

*Flag complete regimens (at least 3 medicines or dual regimens according to the Australian guideline);
data Hiv_only_disp2;
	set Hiv_only_disp2;
	if N_ARV>=3 or N_ARV=2 and regimen in ('2NRTI','INSTI+PI/Enh', 'NRTI+PI/Enh', 'NNRTI+INSTI', 'NNRTI+PI/Enh', 'EI-based','Etra-based') then C_ARV = 1;
	else C_ARV = 0;
run;

Title "Number of complete regimens - 30 days";
proc freq data= Hiv_only_disp2;
table C_ARV;
run;

Title "Incomplete dispensed regimens - 30 days";
proc freq data= Hiv_only_disp2;
table drugs_all ;
where C_ARV=0;
run;
title;

*Flag patients receiving only incomplete regimens;
PROC SQL; CREATE TABLE Incomplete AS
	SELECT DISTINCT pat_id, 
    SUM (C_ARV) AS N_complete 
	FROM Hiv_only_disp2
GROUP BY pat_id;
QUIT;

proc freq data = Incomplete;
table N_complete;
run;

proc print data = Incomplete;
where N_complete = 0;
run;

data Incomplete;
set Incomplete;
Incomplete = 1;
where N_complete = 0;
run; ****Exclude 86 patients with incomplete ART regimens during the whole observation period;

*Merging back into dispensings to exclude patients receiving only incomplete regimens; 
data Hiv_only_disp_complete;
	merge  Incomplete (in=a) Hiv_only_disp2 (in=b);
	by pat_id;
run;

proc freq data = Hiv_only_disp_complete;
table drugs_all;
where incomplete = 1;
run;

*Keep only those with at least one complete regimen;
data Hiv_only_disp_complete2;
	set Hiv_only_disp_complete;
	where Incomplete = . ;
run;

*Calculate age;
data Hiv_only_disp_complete2;
set Hiv_only_disp_complete2;
year = year(first_supp);
age = year - YEAR_BIRTH;
run;

data Hiv_only_disp_complete3;
set Hiv_only_disp_complete2;
where age ge 17;
run;


******************Go to program regimens check***************

After cleaning dispensings create some variables again:

***********************************************************************************
* Calculate first date of supply and first_supp_treat again because they can have changed to some patients where grouping on 30-day was incorrect;
proc contents data = up.Hiv_prevalent_disp_final; run;
PROC SORT data = up.Hiv_prevalent_disp_final;
by pat_id supp_date descending C_ARV;
run;

*First_supp_database;
data hiv_p (keep=pat_id first_supp first_supp_2013_2018);
	set up.Hiv_prevalent_disp_final; 
	by pat_id;
	first_supp_2013_2018 = supp_date;
    format first_supp_2013_2018 ddmmyy.;
	if first.pat_id then output;
run;

*Select from 2016 onward;
data Hiv_prevalent_disp_2016_2018;
set up.Hiv_prevalent_disp_final;
where '01Jan2016'd<=supp_date<='31Dec2018'd;
run;

data hiv_cohort_p2 (keep=pat_id first_supp first_supp2);
	set Hiv_prevalent_disp_2016_2018; 
	by pat_id;
	first_supp2 = supp_date;
    format first_supp2 ddmmyy.;
	if first.pat_id then output;
run;

*Merge back to include the first_supp2 date;
data Hiv_prevalent_disp_final2;
	merge hiv_cohort_p2 (in=a drop = first_supp) hiv_p (in=b drop = first_supp) Hiv_prevalent_disp_2016_2018 (in=c);
	by pat_id;
	if c;
run;

*Flag previous dispensing within 1 to 3y from index date;
data Hiv_previous_disp;
merge up.Hiv_prevalent_disp_final (in = a)  hiv_cohort_p2 (in=b drop = first_supp) hiv_p (in=c drop = first_supp);
by pat_id;
if a;
run;

data Hiv_previous_disp2; 
set Hiv_previous_disp;
prev_disp = first_supp2 - supp_date; 
time_on_art = first_supp2 - first_supp_2013_2018;
where first_supp2 not eq . ;
run;* prev_disp we eant only disp before first_supp (ie.e, positive values);

*previous switch;
data Hiv_previous_disp_c;
set Hiv_previous_disp2;
where C_ARV = 1 and 0 <= prev_disp <= 1080;
run;

data Hiv_previous_disp_c;
	set Hiv_previous_disp_c;
	by pat_id;
	prev_regimen = lag(regimen);
    if first.pat_id then do prev_regimen = '';Regimen_switch = 0;end;  
	else if prev_regimen ne regimen THEN Regimen_switch = 1;
	else if prev_regimen = regimen THEN Regimen_switch = 0;
  run;

proc sort data = Hiv_previous_disp_c;
by pat_id prev_disp; run;

data Hiv_previous_switch;
set Hiv_previous_disp_c;
where Regimen_switch = 1;
run;

PROC SQL; CREATE TABLE N_previous_switch AS
	SELECT DISTINCT pat_id, 
	SUM (Regimen_switch) AS N_prev_switch
	FROM Hiv_previous_switch
GROUP BY pat_id;
QUIT;

*Select the date of the last switch;
data Hiv_previous_switch_p (keep = pat_id prev_disp regimen prev_regimen regimen_switch supp_date drugs_all);
set Hiv_previous_switch;
if first.pat_id then output;
by pat_id;
run;

data Hiv_previous_switch_p2 (drop = prev_disp regimen prev_regimen regimen_switch supp_date drugs_all);
set Hiv_previous_switch_p;
flag_prev_reg = regimen_switch;
prev_reg_supp_date = supp_date;
time_on_regimen = prev_disp;
run;



*To be merged;
data up.Hiv_prevalent_traj_2016_2017_3;
merge up.Hiv_prevalent_traj_2016_2017 (in=a) Hiv_previous_switch_p2 (in = b) N_previous_switch (in=c) hiv_p (in = d keep=pat_id first_supp_2013_2018);
by pat_id;
if a;
run;

data up.Hiv_prevalent_traj_2016_2017_3;
set up.Hiv_prevalent_traj_2016_2017_3;
format prev_reg_supp_date DATE9.; 
time_on_art = first_supp2 - first_supp_2013_2018;
if flag_prev_reg = 1 then time_on_regimen2 = time_on_regimen;
if flag_prev_reg = . then time_on_regimen2 = time_on_art;
if time_on_regimen2 = 0 and flag_prev_reg = . then status2 =0;
if time_on_regimen2 = 0 and flag_prev_reg = 1 then status2 =1;
if 0 < time_on_regimen2 <= 180 then status2 = 1;
if time_on_regimen2 > 180 then status2 = 2;
run;


data up.Hiv_prevalent_traj_2016_2017_3;
set up.Hiv_prevalent_traj_2016_2017_3;
if time_on_art = 0 then status3 =0;
if 0 < time_on_art <= 180 then status3 = 1;
if time_on_art > 180 then status3 = 2;
run;

data up.Hiv_prevalent_traj_2016_2017_3;
set up.Hiv_prevalent_traj_2016_2017_3;
if time_on_art <= 180 then time_therapy = 0;
if time_on_art > 180 then time_therapy = 1;
run;

data up.Hiv_prevalent_traj_2016_2017_3;
set up.Hiv_prevalent_traj_2016_2017_3;
if time_on_art = 0 then naive_time =1;
if time_on_art ne 0 then naive_time =0;
run;



****FLag previous dispensing;
PROC SQL; CREATE TABLE flag_1y AS
	SELECT DISTINCT pat_id, 
    count (distinct supp_date) AS Prev_1y_disp 
	FROM Hiv_previous_disp
    WHERE 0 < prev_diff <= 360
    GROUP BY pat_id;
QUIT;

PROC SQL; CREATE TABLE flag_2y AS
	SELECT DISTINCT pat_id, 
    count (distinct supp_date) AS Prev_2y_disp 
	FROM Hiv_previous_disp2
    WHERE 0 < prev_disp <= 720
    GROUP BY pat_id;
QUIT;

PROC SQL; CREATE TABLE flag_3y AS
	SELECT DISTINCT pat_id, 
    count (distinct supp_date) AS Prev_3y_disp 
	FROM Hiv_previous_disp2
    WHERE 0 < prev_disp <= 1080
    GROUP BY pat_id;
QUIT;

proc freq data = flag_3y;
table Prev_3y_disp; run;

PROC SQL; CREATE TABLE flag_all AS
	SELECT DISTINCT pat_id, 
    count (distinct supp_date) AS Prev_all_disp 
	FROM Hiv_previous_disp
    WHERE 0 < prev_disp
    GROUP BY pat_id;
QUIT;

proc freq data = up.Hiv_prevalent_traj_2016_2017_2;
table naive naive_2y naive_3y;
run;


proc sql;
select count (distinct pat_id)
from Hiv_prevalent_disp_final2
where '01Jan2016'd<=first_supp2<='31Dec2017'd;
quit; *2137;

****************************************Merge with pat characteristics;
PROC SQL; CREATE TABLE up.Hiv_prevalent_disp_2016_2017 AS
	SELECT * FROM Hiv_prevalent_disp_final2 A   
		LEFT JOIN patients.patient_ids B ON A.pat_id=B.pat_id
        LEFT JOIN flag_3y E ON A.pat_id=E.pat_id
		LEFT JOIN flag_all F ON A.pat_id=F.pat_id;
QUIT;

proc sql;
select count (distinct pat_id)
from up.Hiv_prevalent_disp_2016_2017
where '01Jan2016'd<=first_supp2<='31Dec2017'd;
quit;

*Remove people initiating after Dec 2017;
data Hiv_prevalent_disp_final5;
set up.Hiv_prevalent_disp_2016_2017;
year = year(first_supp2);
age = year - YEAR_BIRTH;
where '01Jan2016'd<=first_supp2<='31Dec2017'd;
run;

proc freq  data = Hiv_prevalent_disp_final5;
table first_supp2 supp_date;
run;

proc sort data = Hiv_prevalent_disp_final5;
by pat_id supp_date descending C_ARV;
run;

* Correct flag STR; *If wanted to know how many received STR plus another antiretroviral come back here;
proc FREQ data = Hiv_prevalent_disp_final5;
table drugs_all;
where STR =1;
RUN;

data Hiv_prevalent_disp_final5;
set Hiv_prevalent_disp_final5;
if STR = 1 and drugs_all not in ('ABACAVIR/DOLUTEGRAVIR/LAMIVUDINE','COBICISTAT/ELVITEGRAVIR/EMTRICITABINE/TENOFOVIR', 'COBICISTAT/ELVITEGRAVIR/EMTRICITABINE/TENOFOVIR ALAFENAMIDE', 
'EFAVIRENZ/EMTRICITABINE/TENOFOVIR', 'EMTRICITABINE/RILPIVIRINE/TENOFOVIR', 'EMTRICITABINE/RILPIVIRINE/TENOFOVIR ALAFENAMIDE')
then STR =.;
RUN;


*flag expecienced and naive;
data Hiv_prevalent_disp_final6;
set Hiv_prevalent_disp_final5;
if  Prev_1y_disp ge 1 then naive = 0;
if  missing (Prev_1y_disp) then naive = 1;
if  Prev_2y_disp ge 1 then naive_2y = 0;
if  missing (Prev_2y_disp) then naive_2y  = 1;
if  Prev_3y_disp ge 1 then naive_3y = 0;
if  missing (Prev_3y_disp) then naive_3y  = 1;
if  Prev_all_disp ge 1 then naive_ALL = 0;
if  missing (Prev_all_disp) then naive_ALL  = 1;
run;

proc freq data= Hiv_prevalent_disp_final6;
table naive naive_2y naive_3y naive_ALL;
run;

*Keep only dispensings up to 1 year of follow-up;
data Hiv_prevalent_disp_final7;
set Hiv_prevalent_disp_final6;
disp_diff = supp_date - first_supp2;
if disp_diff <= 365;
run;

*Calculate the last dispensing of the period of a complete regimen;
proc sort data = Hiv_prevalent_disp_final7; *To get the last dispensing data;
by pat_id supp_date descending C_ARV;
run;

*Last dispensing;
data last_disp (keep = pat_id supp_date);
	set Hiv_prevalent_disp_final7;
	by pat_id;
	if last.pat_id then output;
run;

*Merge;
data Hiv_prevalent_disp_final7;
merge Hiv_prevalent_disp_final7 (in = a) last_disp (in=b rename=(supp_date=last_supp_art));
by pat_id;
if a;
run;

*Now check if they have a dispensing register for any other medicine;

********Dispensings of other medicines******;
data IDs_included (keep=pat_id);
set Hiv_prevalent_disp_final7;
if first.pat_id then output;
by pat_id;
run;

data haart_codes (keep = pbs_code art);
set pub.haart_codesv2;
art = 1;
run;

%MACRO other_meds;

%DO year=2015 %TO 2018;

PROC SQL; CREATE TABLE y&year.supply AS
	SELECT * FROM PBS10.y&year.supply A
		INNER JOIN IDs_included B
			ON A.pat_id=B.pat_id
		FULL OUTER JOIN haart_codes C
			ON A.item_code=C.pbs_code
		WHERE '1Jan2015'd<=supp_date<='31Dec2018'd; 
QUIT;
%END;
 
DATA other_meds; SET y2015supply; RUN;
%DO year=2016 %TO 2018;
	PROC APPEND DATA=y&year.supply BASE=other_meds FORCE; RUN;
%END;

%MEND; 

%other_meds;

*All medicines used in the OW;
data up.all_meds_2015_2018;
set other_meds;
run;

*Only other meds in the OW;
data up.other_meds_2015_2018;
set other_meds;
where art = .;
run;

proc sort data = up.all_meds_2015_2018;
	by pat_id supp_date;
run;

*Last dispensing of any drug;
data last_disp_any (keep = pat_id supp_date);
	set up.all_meds_2015_2018;
	by pat_id;
	if last.pat_id then output;
run;

*Merge;
data Hiv_prevalent_disp_final8;
merge Hiv_prevalent_disp_final7 last_disp_any (rename=(supp_date=last_supp_any));
by pat_id;
run;

*Flag death based com the combination of original death flag and absence of other dispensing in the follow-up;
data Hiv_prevalent_disp_final8;
set Hiv_prevalent_disp_final8;
diff_supp_any = last_supp_any - first_supp2;
death_flag = year(first_supp2)<=YEAR_DEATH<=year(last_supp_any);
if (death_flag = 1 and diff_supp_any <=360) then death = 1;
if (death_flag = 1 and diff_supp_any >360) then death = 0;
if missing(YEAR_DEATH) or death_flag = 0 then death=0;
run;

proc print data = Hiv_prevalent_disp_final8;
var pat_id first_supp2 drugs_all YEAR_DEATH last_supp_any diff_supp_any death;
where death =1;
run;

proc sql;
select count(distinct pat_id)
from Hiv_prevalent_disp_final8
where DEATH =1;
quit; *15 deaths during 1 year of follow-up;

*DON'T exclude those who died within the follow-up;


*Add use of other medicines in the prior six months of the index date (first_supp2 = baseline);

****Categories****;
proc sort data = Hiv_prevalent_disp_final8; by pat_id; run;
proc sort data = up.other_meds_2015_2018; by pat_id; run;

data IDs_first_disp (keep = pat_id first_supp2);
set Hiv_prevalent_disp_final8; 
if first.pat_id then output;
by pat_id;
run;

data all_previous_disp;
merge up.other_meds_2015_2018 (in = a) IDs_first_disp (in=b);
by pat_id;
if b;
run;

data all_previous_disp;
set all_previous_disp;
diff = first_supp2 - supp_date;
run;

data all_previous_disp2; *Dispensing on the prior 6 months;
set all_previous_disp;
where 0<=diff <=180;
run;

proc sort data = all_previous_disp2; by ITEM_CODE; run;

data up.all_previous_disp_2015_2018;
merge all_previous_disp2 (in=a) s85.Pbsdrugmap (Keep = ITEM_CODE drug_name ATC_CODE in=b);
by ITEM_CODE;
if a;
run;

*Check what they acctualy used;
data med_used;
set up.all_previous_disp_2015_2018;
where ATC_CODE like 'N05A%' OR ATC_CODE like 'N06A%' OR ATC_CODE like 'N05B%' OR ATC_CODE like 'C03%' OR ATC_CODE like 'C07%' OR ATC_CODE like 'C08%' OR ATC_CODE like 'C09%' OR ATC_CODE like 'A10%' OR ATC_CODE like 'C10%';
run;

PROC sql;
create table med_used2 as
select distinct drug_name, ATC_CODE
FROM med_used
order by ATC_CODE;
QUIT;

proc print data = med_used2;
run;

*Flag categories of medicines based on ATC and dispensings of at least 2 medicines in the prior six months;

proc sql;
create table Ids_antipsychotics as
select distinct pat_id,
count (distinct supp_date) as N_disp
from up.all_previous_disp_2015_2018
where ATC_CODE like 'N05A%'
GROUP BY pat_id
HAVING calculated N_disp >=2;
quit;

data Ids_antipsychotics;
set Ids_antipsychotics;
N05A = 1;
run;

proc sql;
create table Ids_antidepressants as
select distinct pat_id,
count (distinct supp_date) as N_disp
from up.all_previous_disp_2015_2018
where ATC_CODE like 'N06A%'
GROUP BY pat_id
HAVING calculated N_disp >=2;
quit;

data Ids_antidepressants;
set Ids_antidepressants;
N06A = 1;
run;

proc sql;
create table Ids_ANXIOLYTICS as
select distinct pat_id,
count (distinct supp_date) as N_disp
from up.all_previous_disp_2015_2018
where ATC_CODE like 'N05B%'
GROUP BY pat_id
HAVING calculated N_disp >=2;
quit;

data Ids_ANXIOLYTICS;
set Ids_ANXIOLYTICS;
N05B = 1;
run;

proc sql;
create table Ids_ANTIHYPERTENSIVES as
select distinct pat_id,
count (distinct supp_date) as N_disp
from up.all_previous_disp_2015_2018
where ATC_CODE like 'C03%' OR ATC_CODE like 'C07%' OR ATC_CODE like 'C08%' OR ATC_CODE like 'C09%' OR ATC_CODE EQ 'C10BX03'
GROUP BY pat_id
HAVING calculated N_disp >=2;
quit;

data Ids_ANTIHYPERTENSIVES;
set Ids_ANTIHYPERTENSIVES;
C23789 = 1;
run;

proc sql;
create table Ids_DIABETES as
select distinct pat_id,
count (distinct supp_date) as N_disp
from up.all_previous_disp_2015_2018
where ATC_CODE like 'A10%'
GROUP BY pat_id
HAVING calculated N_disp >=2;
quit;

data Ids_DIABETES;
set Ids_DIABETES;
A10 = 1;
run;

proc sql;
create table Ids_LIPID as
select distinct pat_id,
count (distinct supp_date) as N_disp
from up.all_previous_disp_2015_2018
where ATC_CODE like 'C10%' or ATC_CODE like 'A10BH%'
GROUP BY pat_id
HAVING calculated N_disp >=2;
quit;

data Ids_LIPID;
set Ids_LIPID;
C10 = 1;
run;

proc sql;
create table Ids_LIPID2 as
select distinct pat_id,
count (distinct supp_date) as N_disp
from up.all_previous_disp_2015_2018
where ATC_CODE like 'C10%' or ATC_CODE like 'A10BH%'
GROUP BY pat_id
HAVING calculated N_disp >=1 and ;
quit;

data Ids_LIPID;
set Ids_LIPID;
C10 = 1;
run;

*Merge with data set;

DATA Hiv_prevalent_disp_final9;
MERGE Hiv_prevalent_disp_final8 (in=a) Ids_antidepressants (drop = N_disp in=b) Ids_ANXIOLYTICS(drop = N_disp in=c) Ids_LIPID (drop = N_disp in=d) 
Ids_DIABETES (drop = N_disp in=e) Ids_ANTIHYPERTENSIVES (drop = N_disp in = f) Ids_antipsychotics (drop = N_disp in=G);
by pat_id;
if a;
run;

DATA Hiv_prevalent_disp_final9;
set Hiv_prevalent_disp_final9;
if N05A=1 or N05B=1 or N06A=1 then Mental = 1;
else Mental = 0;
run;

*Create variables;

data Hiv_prevalent_disp_final9;
set Hiv_prevalent_disp_final9;
length age_range $6.;
if age < 21 then age_range = '<21';
else if 21 <= age <= 30 then age_range = "21_30";
else if  31 <= age <= 40 then age_range = "31_40";
else if  41 <= age <= 50 then age_range = "41_50";
else if  51 <= age <= 60 then age_range = "51_60";
else if  61 <= age <= 70 then age_range = "61_70";
else age_range = '>70';
run;

data Hiv_prevalent_disp_final10 (drop = flag_100 diff_supp_any date_100);
set Hiv_prevalent_disp_final9;
run;

*Select one year of follow-up (Already selected, did not change numbers);
data Hiv_prevalent_disp_final11;
set Hiv_prevalent_disp_final10;
where disp_diff <= 365;
run; *10085;

*IDS;
proc sort data = Hiv_prevalent_disp_final11;
by pat_id supp_date descending C_ARV;
run;

data Hiv_prevalent_p;
set Hiv_prevalent_disp_final11;
if first.pat_id then output;
by pat_id;
run;

*DATA SET TO CALCULATE SWITCHES;
data HIV_prevalent_disp_final_c; *;
set Hiv_prevalent_disp_final11; 
where C_ARV = 1;* FROM 10086 TO 9898 ;
run; 

proc freq data = HIV_prevalent_disp_final_c;
table drugs_all;
where STR=1;
RUN;

*Flag switch to another regimen STR to MTR only within the time frame of the follow-up (1 year);
data HIV_prevalent_disp_final_c;
set HIV_prevalent_disp_final_c;
if missing(STR)then STR = 0;
run;

proc sort data = HIV_prevalent_disp_final_c;
by pat_id supp_date;
run;

*Flag switches;

data HIV_prevalent_disp_final_c;
	set HIV_prevalent_disp_final_c;
	by pat_id;
	prev_STR = lag(STR);
    if first.pat_id then do; prev_STR = .;Switch = 0;end; 
	else if prev_STR ne STR THEN Switch = 1;
	else if prev_STR = STR THEN Switch = 0;
  run;

data HIV_prevalent_disp_final_c;
	set HIV_prevalent_disp_final_c;
	by pat_id;
	prev_regimen = lag(regimen);
    if first.pat_id then do prev_regimen = '';Regimen_switch = 0;end;  
	else if prev_regimen ne regimen THEN Regimen_switch = 1;
	else if prev_regimen = regimen THEN Regimen_switch = 0;
  run;

data HIV_prevalent_disp_final_c;
	set HIV_prevalent_disp_final_c;
	by pat_id;
	prev_drugs_all = lag(drugs_all);
    if first.pat_id then do prev_drugs_all = '';drugs_all_switch = 0; end;
	else if prev_drugs_all ne drugs_all THEN drugs_all_switch = 1;
    else if prev_drugs_all = drugs_all THEN drugs_all_switch = 0;
 run;

PROC SQL; CREATE TABLE Switch_P AS
	SELECT DISTINCT pat_id, 
    SUM (Switch) AS N_Switch, 
	SUM (Regimen_switch) AS N_Regimen_switch,
	SUM (drugs_all_switch) AS N_drugs_all_switch
	FROM HIV_prevalent_disp_final_c
GROUP BY pat_id;
QUIT;

PROC SQL; CREATE TABLE Switch_P AS
	SELECT DISTINCT pat_id, 
	SUM (Regimen_switch) AS N_Regimen_switch
	FROM HIV_prevalent_disp_final_c
GROUP BY pat_id;
QUIT;

proc freq data = Switch_P;
table N_Regimen_switch;
run;

*Check;
proc print data = HIV_prevalent_disp_final_c;
var pat_id supp_date drugs_all regimen STR prev_STR switch prev_regimen Regimen_switch prev_drugs_all drugs_all_switch;
where pat_id = 141725372;
run;

data switch_regimens (keep = pat_id supp_date concat);
set HIV_prevalent_disp_final_c;
concat=catx(' to ', prev_regimen, regimen); 
where Regimen_switch = 1; 
run;

proc sort data = switch_regimens; by pat_id supp_date; run;

*Merge to patients characteristics table;

data switch_reg;
set switch_regimens;
if first.pat_id then output;
by pat_id;
run;

data Hiv_prevalent_complete_p;
merge Hiv_prevalent_p (in = a) switch_p (in = b) switch_reg (rename = (supp_date= reg_switch_day)in = c);
by pat_id;
if a;
run;

data Hiv_prevalent_complete_p;
set Hiv_prevalent_complete_p;
if N_switch ge 1 then STR_switch = 1;
if N_switch eq 0 then STR_switch = 0;
if N_Regimen_switch ge 1 then N_Reg_switch = 1;
if N_Regimen_switch eq 0 then N_Reg_switch = 0;
if N_drugs_all_switch ge 1 then N_drugs_switch = 1;
if N_drugs_all_switch eq 0 then N_drugs_switch = 0;
run;

****************************************************************
                           SELECTED COHORT 
****************************************************************
Those being dispensed regimens with 3 or more antiretrovirals;

data up.Hiv_c_p_3arv_2016_2017;
set Hiv_prevalent_complete_p;
where N_ARV>2;
run; *2042;

proc freq data = up.Hiv_c_p_3arv_2016_2017;
table year naive naive_2y naive_3y naive_ALL;
run;

proc freq data = up.Hiv_c_p_3arv_2016_2017; table regimen; where  naive = 1; run;
proc freq data = up.Hiv_c_p_3arv_2016_2017; table regimen; where  naive_2y = 1; run;
proc freq data = up.Hiv_c_p_3arv_2016_2017; table regimen; where  naive_3y = 1; run;
proc freq data = up.Hiv_c_p_3arv_2016_2017; table regimen; where  naive_ALL = 1; run;

*********************LTFU**********************************
All medicines used in the OW;
data up.all_meds_2015_2018;
set other_meds;
run;

data follow;
merge up.all_meds_2015_2018 (in = a) included_ids (in=b);
by pat_id;
if b;
run;

data follow2;
set follow;
IF (first_supp_treat)<=supp_date<(first_supp2+360) THEN OUTPUT; 
run;

proc sql;
select count(distinct pat_id)
from follow2;
quit; *2042;

proc sql;create table other_med as
select pat_id, count(distinct pat_id) as Other_med
from follow2
where art = .
group by pat_id;
quit; *1703 patients receiving other drugs (1703/2042 = 83%)in the follow-up + 30 days 
(1687/2042, 82.6%, in 360 days);


*All medicines used in the OW;
proc sort data = follow2; by pat_id supp_date; run; 

data last_disp_follow (keep = pat_id supp_date); *if last pat_id I will get the last disp;
	set follow2;
	by pat_id;
	if last.pat_id then output;
run;

data HIV_prevalent_traj_2016_2017_4;
merge up.HIV_prevalent_traj_2016_2017_3 (in=a) last_disp_follow (in=b rename=(supp_date=lastsup_anyfollow));
by pat_id;
if a;
run;

*Follow-up date;
data HIV_prevalent_traj_2016_2017_4;
set HIV_prevalent_traj_2016_2017_4;
time = lastsup_anyfollow - first_supp2;
run;

proc freq data = HIV_prevalent_traj_2016_2017_4;
table time;
run;

data HIV_prevalent_traj_2016_2017_4; *Does not work, inly if you select a higher flw, for example 390 days;
set HIV_prevalent_traj_2016_2017_4;
if death = 1 or time ge 360 then follow_y =1;
if death = 0 and time lt 360 then follow_y =0;
run;

proc freq data = HIV_prevalent_traj_2016_2017_4; table follow_y; run; *99% lost;


data HIV_prevalent_traj_2016_2017_5; *last 2 months;
set HIV_prevalent_traj_2016_2017_4;
if death = 1 or time ge 300 then follow_y2 =1;
else if death = 0 and time lt 300 then follow_y2 =0;
run;

proc freq data = HIV_prevalent_traj_2016_2017_5;
table follow_y2;
run; *(20.8% of LTFU);



data up.HIV_prevalent_traj_2016_2017_5;
merge HIV_prevalent_traj_2016_2017_5 (in=a) other_med (in=b);
by pat_id;
if a;
run;

data up.HIV_prevalent_traj_2016_2017_5;
set up.HIV_prevalent_traj_2016_2017_5;
if missing(other_med) then other_med=0;
run;
data up.HIV_prevalent_traj_2016_2017_5; *last 3 months;
set up.HIV_prevalent_traj_2016_2017_5;
if death = 1 or time ge 270 then follow_y3 =1;
else if death = 0 and time lt 270 then follow_y3 =0;
run;

proc freq data = up.HIV_prevalent_traj_2016_2017_5;
table follow_y3;
run;
proc freq data = up.HIV_prevalent_traj_2016_2017_5;
table follow_y3*other_med;
run;


data up.HIV_prevalent_traj_2016_2017_5;
set up.HIV_prevalent_traj_2016_2017_5;
if follow_y3= 0 and other_med = 1 then LTFU = 1;
if follow_y3= 0 and other_med = 0 then LTFU = 0;
if follow_y3= 1 then LTFU = 0;
run;

proc freq data = up.HIV_prevalent_traj_2016_2017_5;
table LTFU;
run;

data up.HIV_prevalent_traj_2016_2017_5;
set up.HIV_prevalent_traj_2016_2017_5;
if follow_y2= 0 and other_med = 1 then LTFU2 = 1;
if follow_y2= 0 and other_med = 0 then LTFU2 = 0;
if follow_y2= 1 then LTFU2 = 0;
run;

proc freq data = up.HIV_prevalent_traj_2016_2017_5;
table LTFU2;
run;


*;

*If consider the next month after follow up*;

data follow3;
set follow;
IF (first_supp_treat)<=supp_date<(first_supp2+390) THEN OUTPUT; 
run;

proc sql;
select count(distinct pat_id)
from follow3;
quit; *2042;

proc sql;
select count(distinct pat_id)
from follow3
where art = .;
quit; *1703 patients receiving other drugs (1703/2042 = 83%)in the follow-up + 30 days 


*All medicines used in the OW;
proc sort data = follow3; by pat_id supp_date; run; 

data last_disp_follow (keep = pat_id supp_date); *if last pat_id I will get the last disp;
	set follow3;
	by pat_id;
	if last.pat_id then output;
run;

data HIV_prevalent_traj_2016_2017_6;
merge up.HIV_prevalent_traj_2016_2017_3 (in=a) last_disp_follow (in=b rename=(supp_date=lastsup_anyfollow));
by pat_id;
if a;
run;

*Follow-up date;
data HIV_prevalent_traj_2016_2017_6;
set HIV_prevalent_traj_2016_2017_6;
time = lastsup_anyfollow - first_supp2;
run;

proc freq data = HIV_prevalent_traj_2016_2017_6;
table time;
run;

data HIV_prevalent_traj_2016_2017_6; *Does not work, only if you select a higher flw, for example 390 days, but LTFU still high (10%);
set HIV_prevalent_traj_2016_2017_6;
if death = 1 or time ge 360 then follow_y =1;
if death = 0 and time lt 360 then follow_y =0;
run;

proc freq data = HIV_prevalent_traj_2016_2017_6; table follow_y; run; *(38%ltfu);


data HIV_prevalent_traj_2016_2017_7;
set HIV_prevalent_traj_2016_2017_6;
if death = 1 or time ge 300 then follow_y2 =1;
if death = 0 and time lt 300 then follow_y2 =0;
run;

proc freq data = HIV_prevalent_traj_2016_2017_7;
table follow_y2;
run;

*How to adjust for LTFU in this case? 










*****************************************************************
                      ADHERENCE CALCULATIONS
*****************************************************************
In accordance to Leslie et al., (2009);

*Use original dispensing data with one drug per row of included patients;
proc sort data = up.haart_hiv4c;
by pat_id supp_date drug_name;
run;

proc sort data = up.Hiv_c_p_3arv_2016_2017;
by pat_id;
run;

data included_ids (keep = pat_id first_supp2 first_supp_treat death);
set up.Hiv_c_p_3arv_2016_2017;
if missing(first_supp_treat) then first_supp_treat = first_supp2; *28 patients where the first_supp2 not eq first_supp,due to correcting the first disp;
run;

data HIV_prev_disp;
merge up.haart_hiv4c (in = a) included_ids (in=b);
by pat_id;
if b;
run;

DATA temp;
SET HIV_prev_disp;
month = month(supp_date);
year = year(supp_date);
where year = 2018;
RUN;

proc sgplot data=temp;
vbar month;
run;

*Number of patients per year;

proc sql;
select count(distinct pat_id)
from HIV_prev_disp
where '01Jan2016'd<=supp_date<='31Dec2016'd;
quit; *from 1911 to 1889;

proc sql;
select count(distinct pat_id)
from HIV_prev_disp
where '01Jan2017'd<=supp_date<='31Dec2017'd;
quit; *from 1963 to 1938 to 1997 (increased time 6 months);

***********************
*Restrict dispensings to 360 days of up, since the first_supp_treat to account for days when the first medicine of the regimen was dispensed;

data HIV_prev_disp2;
set HIV_prev_disp;
IF (first_supp_treat)<=supp_date<(first_supp2+360) THEN OUTPUT; 
run;

proc sort data=HIV_prev_disp2; by pat_ID drug_name supp_date; 
run; 
 
proc transpose data = HIV_prev_disp2 out=fill_dates (drop=_name_) prefix = supp_date; 
by pat_ID drug_name; 
var supp_date; 
run; 
 
proc transpose data = HIV_prev_disp2 out=days_supply (drop=_name_) prefix = dur; 
by pat_ID drug_name; 
var duration; 
run; 
 
data both; 
merge fill_dates days_supply; 
by pat_ID drug_name; 
run; 

*Adjust observation window for adherence calculations per patient;
data both2;
merge both (in=a) included_ids (in=b);
by pat_id;
start_dt=first_supp2; 
end_dt=first_supp2+360;
days=end_dt-start_dt;
format start_dt end_dt DATE9.; 
run;

*Days covered for each medicine, accounting for early reffils and reffils in the same day;
data pdc; 
set both2; 
array daydummy(360) day1-day360; 
array filldates(*) supp_date1 - supp_date12; 
array days_supply(*) dur1 - dur12;

do u=2 to 12 while (filldates(u) ^= .);       
if filldates(u)<filldates(u-1)+days_supply(u-1)       
then filldates[u]=filldates(u-1)+days_supply(u-1);     
end; 

do ii=1 to 360; daydummy(ii)=0;
end; 

do ii=1 to 360; 
do i = 1 to dim(filldates) while (filldates(i) ^=.);       
if filldates(i)<= start_dt + ii -1 <= filldates(i)+ days_supply(i)-1          
then daydummy(ii)=1;         
end;  
end; 
 
drop i ii u; 

run;

PROC TRANSPOSE DATA=pdc OUT=pdc2;
	BY pat_id start_dt;
	VAR day1-day360;
	where drug_name not in ('RITONAVIR', 'COBICISTAT');
RUN;

*Day covered as overlapping dispensing of at least 3 meds, excluding ritonavir and cobicistat that are enhancers;
DATA pdc3; 
SET pdc2;

	day=INPUT(SUBSTR(_name_,4,4),4.); 
	month=CEIL(day/30);
	coverage_ARV= sum (of col1-col9);
	if coverage_ARV ge 3 then covered_regimen = 1;
	if coverage_ARV lt 3 then covered_regimen = 0;
	DROP _name_; RENAME col1=daily_use1 col2=daily_use2 col3=daily_use3 col4=daily_use4 col5=daily_use5 col6=daily_use6 col7=daily_use7 col8=daily_use8 col9=daily_use9;
RUN;

PROC SQL; CREATE TABLE pdc4 AS	
	SELECT DISTINCT pat_id, start_dt, month,
		SUM(covered_regimen)/30 AS month_pdc
	FROM pdc3 GROUP BY pat_id, month; 
QUIT;

PROC SQL; CREATE TABLE total_pdc AS	
	SELECT DISTINCT pat_id, start_dt, 
		SUM(covered_regimen)/360 AS p_pdc
	FROM pdc3 GROUP BY pat_id; 
QUIT;

*Cut-offs of adherence;
data total_pdc; 
set total_pdc;
if p_pdc >=.95 then Adh_95 = 1;
if p_pdc <.95 then Adh_95 = 0;
if p_pdc >=.80 then Adh_80 = 1;
if p_pdc <.80 then Adh_80 = 0;
run;

proc freq data = total_pdc order = freq;
table Adh_80 Adh_95 /binomial;
run;

proc means data = total_pdc; var p_pdc; run;
proc means data = total_pdc p25 p50 p75;
var p_pdc;
run;



*Monthly adherence to trajectory;
data HIV_pdc_monthly; 
set pdc4;
if month_pdc >=.95 then Adh_95 = 1;
if month_pdc <.95 then Adh_95 = 0;
if month_pdc >=.80 then Adh_80 = 1;
if month_pdc <.80 then Adh_80 = 0;
run;

title 'Frequency of adherent people (80%) by month';
proc sgplot data=HIV_pdc_monthly;
vbar month;
where Adh_80=1;
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly out=month (DROP=_name_) prefix=month_;
	by pat_id; 
    var month; 
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly out=Adh95 (DROP=_name_) prefix=Adh95_;
	by pat_id; 
    var Adh_95; 
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly out=Adh80 (DROP=_name_) prefix=Adh80_;
	by pat_id; 
    var Adh_80; 
run;

Data Traj_95_80; 
merge month Adh80 Adh95 included_ids (keep = pat_id death);
BY pat_id; 
Run;

****************Trajectory PDC 80**************************;

PROC TRAJ DATA= Traj_95_80 OUTPLOT=op_stat_ms OUTSTAT=os_stat_ms OUT=of_stat_ms OUTEST=oe_stat_ms ci95m;
	ID pat_id;
	VAR Adh80_1-Adh80_12;
	INDEP month_1-month_12;
	MODEL logit;
	ORDER 2 2 2;
	RUN;

%trajplot(op_stat_ms,os_stat_ms,"Adherence to cART - PDC 80%","Trajectory model","Percent","Month");
%trajplotnew(op_stat_ms,os_stat_ms,"Adherence to cART - PDC 80%","Trajectory model","Percent","Month");




proc export data = op_stat_ms outfile = 'Z:\PBS 10% Sample\Projects\HAART (JC)\op_stat_ms80_201909.csv' dbms = csv REPLACE;  run;


*Model selection;
* 3 3  -8084.7

* 2 2 2 BIC = -8054.0 *selected
* 3 3 1 BIC = -8027
* 1 3 3 BIC = -8027.24
*3 1 3 BIC = -8022
* 2 3 2 BIC = -8021.5
* 3 3 2 BIC = -7974.9
* 2 2 3 BIC = -8008
* 3 2 1 BIC = -8006
* 2 3 3 BIC = -8006

 BIC = -7996.8
BIC = -7994.8

does not run, CONVERGE PROBLEM OR GRAPH
*3 3 3 
*1 2 3

* 3 2 2 
* 3 2 3
 
*Check probabilies of belonging to each group;
proc univariate data=of_stat_ms noprint; where group=1; histogram grp1prb;run;
proc means data=of_stat_ms; var grp1prb; where group=1;run;
proc means data=of_stat_ms p50 p25 p75; var grp1prb; where group=1;run;

proc univariate data=of_stat_ms noprint; where group=2; histogram grp2prb;run;
proc means data=of_stat_ms; var grp2prb; where group=2;run;
proc means data=of_stat_ms p50 p25 p75; var grp2prb; where group=2;run;

proc univariate data=of_stat_ms noprint; where group=3; histogram grp3prb;run;
proc means data=of_stat_ms; var grp3prb; where group=3;run;
proc means data=of_stat_ms p50 p25 p75; var grp3prb; where group=3;run;

*Select groups to merge back into patients characteristics table;
data groups80 (keep = pat_id group);
set of_stat_ms;
run;

proc sort data = groups80; by pat_id; run;

proc sort data = up.Hiv_c_p_3arv_2016_2017;  by pat_id; run;

data up.HIV_prevalent_traj_2016_2017;
merge up.Hiv_c_p_3arv_2016_2017 (in = a) total_pdc (in=b) Traj_95_80 (in=c) groups80 (rename=(group=group80) in=d);
by pat_id;
if a;
run;

proc freq data = up.HIV_prevalent_traj_2016_2017;
table group80;
run;

data up.HIV_prevalent_traj_2016_2017(DROP = COL1 COL2 COL3 COL4 COL5 COL6 COL7 COL8 COL9 treat);
set up.HIV_prevalent_traj_2016_2017;
run;

**********************************************GAPS********************************************
*Count gap days without a complete cART regimen;

*Create gaps flag;
data gap_calculation_3arv;
set Pdc3;
by pat_id;
if first.pat_id then group = 1; 
else if covered_regimen =0 and lag(covered_regimen) =1 or covered_regimen =1 and lag(covered_regimen) =0 THEN group+1;
run;

*group by patient only for those with gaps;
proc sql;
create table gap_calculation_3arv2 as
select distinct pat_id, group, count (group) as gap
from gap_calculation_3arv
group by pat_id, group
having covered_regimen =0;
quit;

*select highest gap;
proc sort data = gap_calculation_3arv2; by pat_id descending gap; run;

*ids and highest gap;
data ids_gap (keep= pat_id gap);
set gap_calculation_3arv2;
if first.pat_id then output;
by pat_id;
run;

*************************Gaps within the first and last supp date of a complete regimen within one year ***************;

proc contents data=HIV_prevalent_disp_final_c; run;

proc sort data = HIV_prevalent_disp_final_c; by pat_id supp_date; run; 

data last_supp_Cart_year (keep = pat_id supp_date);
set HIV_prevalent_disp_final_c;
if last.pat_id then output;
by pat_id;
run;

data gap_medicine;
merge HIV_prev_disp2 (in=a) last_supp_Cart_year(in=b rename=(supp_date=last_art_y));
by pat_id;
if a;
run;

data gap_medicine (keep = pat_id days);
set gap_medicine;
end_dt=last_art_y+1;
days=end_dt-first_supp2;
run;

data gap_medicine;
merge gap_medicine Pdc3;
by pat_id;
run;

data gap_medicine;
set gap_medicine;
where day<=days;
run; 

data gap_calculation_arv3b;
set gap_medicine;
by pat_id;
if first.pat_id then group = 1; 
else if covered_regimen =0 and lag(covered_regimen) =1 or covered_regimen =1 and lag(covered_regimen) =0 THEN group+1;
run;

proc sql;
create table gap_calculation_arv3b2 as
select distinct pat_id, group, count (group) as gap
from gap_calculation_arv3b
group by pat_id, group
having covered_regimen =0;
quit;

proc sort data = gap_calculation_3arv2; by pat_id descending gap; run;
proc sort data = gap_calculation_arv3b2; by pat_id descending gap; run;

data ids_gap_b (keep= pat_id gap);
set gap_calculation_arv3b2;
if first.pat_id then output;
by pat_id;
run;

data up.all_gaps_3arv_2016_2017;
merge ids_gap (in=a) ids_gap_b (in=b rename=(gap=gap_art));
by pat_id;
if a;
run;

*update patient characteristics with gaps table;
PROC CONTENTS DATA = up.HIV_prevalent_traj_2016_2017 ;run;

data up.HIV_prevalent_traj_2016_2017;
merge up.HIV_prevalent_traj_2016_2017 (in=a)  up.all_gaps_3arv_2016_2017 (in=b);
by pat_id;
if a;
run;

*Create single dispensing and update patient characteristics table;
proc sql;  CREATE TABLE Number_single_disp AS
select distinct pat_id, 
Count (distinct Supp_date) as N_dispensings
from HIV_prev_disp2
GROUP BY pat_id
having N_dispensings eq 1;
quit;


data up.HIV_prevalent_traj_2016_2017;
merge up.HIV_prevalent_traj_2016_2017 (in=a) Number_single_disp (in = b) ;
by pat_id;
if a;
run;

proc freq data = up.HIV_prevalent_traj_2016_2017 order = freq;
table drugs_all;
run;


*Create dummies for groups and adjust missings; 
data up.HIV_prevalent_traj_2016_2017; set up.HIV_prevalent_traj_2016_2017;
LENGTH PHARMACY_STATE1 $8.;
if group80 eq 1 then group1_80=1;
if group80 in (2,3) then group1_80=0;
if group80 eq 2 then group2_80=1;
if group80 in (1,3) then group2_80=0;
if group80 eq 3 then group3_80=1;
if group80 in (1,2) then group3_80=0;
if missing(N06A) then N06A =0;
if missing(N05A) then N05A =0;
if missing(Mental) then Mental =0;
if missing(N05B) then N05B =0;
if missing(C23789) then C23789 =0;
if missing(C10) then C10 =0;
if missing(A10) then A10 =0;
if missing(STR) then STR =0;
if regimen eq '2NRTI + NNRTI' then regimen1= 0;
if regimen in ('2NRTI + INSTI') then regimen1= 1;
if regimen in ('2NRTI + PI') then regimen1= 2;
if regimen not in ('2NRTI + NNRTI', '2NRTI + INSTI','2NRTI + PI') then regimen1 = 4;
if PHARMACY_STATE in ('ACT','NSW')then PHARMACY_STATE1 = 'NSW/ACT';
if PHARMACY_STATE in ('QLD','NT')then PHARMACY_STATE1 = 'QLD/NT';
if PHARMACY_STATE in ('SA','WA')then PHARMACY_STATE1 = 'WA/SA';
if PHARMACY_STATE in ('TAS','VIC')then PHARMACY_STATE1 = 'TAS/VIC';
if 17 <= age <= 30 then age_range1 = "18_30";
if 31 <= age <= 40 then age_range1 = "31_40";
if 41 <= age <= 50 then age_range1 = "41_50";
if 51 <= age <= 60 then age_range1 = "51_60";
if 61 <= age <= 81 then age_range1 = "60+";
if regimen eq ('2NRTI + NNRTI') then regimen2= 0;
else if regimen eq ('2NRTI + INSTI') then regimen2= 1;
else if regimen eq ('2NRTI + PI') then regimen2= 2;
else if regimen in ('EI-based', 'Etra-based') then regimen2 = 3;
else if regimen in ('NRTI+INSTI+PI', 'NRTI+INSTI+NNRTI') then regimen2 =4;
else if regimen in ('3NRTI+NNRTI', '3NRTI+PI')then regimen2 =5;
else regimen2 = 6;
if 17<=age<=34 then age_range2=0;
if 35<=age<=44 then age_range2=1;
if 45<=age<=54 then age_range2=2;
if 55<=age then age_range2=3;
if Mental = 1 or C23789 = 1 or C10 = 1 or A10 = 1 then prior_med = 1;
if missing(prior_med) then prior_med = 0;
if gap_art>30 then gap_art_dic=1;
if gap_art=<30 then gap_art_dic=0;
if gap>30 then gap_dic=1;
if gap=<30 then gap_dic=0;
if N_dispensings eq 1 then single = 1;
if N_dispensings = . then single = 0;
run;

proc freq data = up.HIV_prevalent_traj_2016_2017;
table group80;
run;

proc freq data = up.HIV_prevalent_traj_2016_2017;
table year;
where naive_all =  1; 
run;

proc means data = up.HIV_prevalent_traj_2016_2017 p25 p50 p75;
var p_pdc;
class regimen2;
run;

proc freq data = up.HIV_prevalent_traj_2016_2017;
table Adh_80*regimen2;
run;


data up.HIV_prevalent_traj_2016_2017_3;
set up.HIV_prevalent_traj_2016_2017_3;
if missing (flag_prev_reg) then flag_prev_reg = 0;
run;

proc freq data = up.HIV_prevalent_traj_2016_2017_3;
table flag_prev_reg;
run;

data up.HIV_prevalent_traj_2016_2017_3;
set up.HIV_prevalent_traj_2016_2017_3;
if regimen eq ('2NRTI + NNRTI') then regimen3= 0;
else if regimen eq ('2NRTI + INSTI') then regimen3= 1;
else if regimen eq ('2NRTI + PI') then regimen3= 2;
else if regimen in ('EI-based', 'Etra-based') then regimen3 = 3;
else if regimen in ('NRTI+INSTI+PI', 'NRTI+INSTI+NNRTI') then regimen3 =4;
else regimen3 = 6;
run;


proc freq data =  up.HIV_prevalent_traj_2016_2017_3 order = freq;
table drugs_all;
run;

*************************************************************
LOGISTIC REGRESSION OF THE PROB. BELONGING TO EACH TRAJ GROUP
*************************************************************;

*Evaluate correlations: No or very weak (0 to 0.05), Weak (>0.05 to 0.10), Moderate (>0.10 to 0.15), Strong (>0.15 to 0.25), and Very Strong (>0.25). 
Available from: (2018). User's guide to correlation coefficients. Turkish journal of emergency medicine, 18(3), 91-93. doi:10.1016/j.tjem.2018.08.001;

proc freq data = up.HIV_prevalent_traj_2016_2017 order = freq;
table STR*regimen2 /chisq; output out=ChiSqData n nmiss pchi lrchi;
RUN; *Cramer's V   0.54 Do not include STR;

proc freq data = up.HIV_prevalent_traj_2016_2017 order = freq;
table age_range2*C10 /chisq; output out=ChiSqData n nmiss pchi lrchi;
RUN; *Cramer's V 0.36 ;

proc freq data = up.HIV_prevalent_traj_2016_2017 order = freq;
table age_range2*C10 / FISHER; RUN; *Fisher's Exact Test <0.001;

proc freq data = up.HIV_prevalent_traj_2016_2017 order = freq;
table age_range2*C23789 /chisq; output out=ChiSqData n nmiss pchi lrchi;
run; *Cramer's V 0.3793;

proc freq data = up.HIV_prevalent_traj_2016_2017 order = freq;
table age_range2*C23789 /FISHER; RUN; *Fisher's Exact Test <0.001;

proc freq data = up.HIV_prevalent_traj_2016_2017 order = freq;
table age_range2*A10 /chisq; output out=ChiSqData n nmiss pchi lrchi;
run; *Cramer's V 0.1110;

proc freq data = up.HIV_prevalent_traj_2016_2017 order = freq;
table age_range2*A10 /FISHER; RUN; *Fisher's Exact Test <0.001;

proc freq data = up.HIV_prevalent_traj_2016_2017 order = freq;
table age_range2*Mental /chisq; output out=ChiSqData n nmiss pchi lrchi; RUN; *Cramer's V 0.1376;

proc freq data = up.HIV_prevalent_traj_2016_2017 order = freq;
table regimen2*naive_all /FISHER; RUN; *Fisher's Exact Test <0.001;
proc freq data = up.HIV_prevalent_traj_2016_2017_3 order = freq;
table regimen2*naive_3y /chisq; output out=ChiSqData n nmiss pchi lrchi; run; *Cramer's V 0.2954 *Strong but will check VIF and tolerance;


proc freq data = up.HIV_prevalent_traj_2016_2017_3 order = freq;
table flag_prev_reg*naive_3y /chisq; output out=ChiSqData n nmiss pchi lrchi; run; *Cramer's V 0.2486 *Strong but will check VIF and tolerance;

proc freq data = up.HIV_prevalent_traj_2016_2017_3 order = freq;
table regimen3*group80 /FISHER; RUN; *Fisher's Exact Test <0.001;


data up.HIV_prevalent_traj_2016_2017_3;
set up.HIV_prevalent_traj_2016_2017_3;
if naive_all = 1 then status = 0;
if naive_all = 0 and flag_prev_reg = 0 then status = 1;
if naive_all = 0 and flag_prev_reg = 1 then status = 2;
run;


proc means data =up.HIV_prevalent_traj_2016_2017_3 p50;
var time_on_regimen;
class adh_80;
run;
proc freq data = up.HIV_prevalent_traj_2016_2017_3;
table status*Adh_80;
run;
proc freq data = up.HIV_prevalent_traj_2016_2017_3;
table status2*Adh_80;
run;
proc freq data = up.HIV_prevalent_traj_2016_2017_3;
table status3*Adh_80;
run;
proc freq data = up.HIV_prevalent_traj_2016_2017_3;
table n_prev_switch;
run;

data up.HIV_prevalent_traj_2016_2017_3;
set up.HIV_prevalent_traj_2016_2017_3;
if missing(N_prev_switch) then  n_sw = 0;
if N_prev_switch = 1 then  n_sw = 1;
if N_prev_switch > 1 then  n_sw = 2;
run;

data up.HIV_prevalent_traj_2016_2017_3;
set up.HIV_prevalent_traj_2016_2017_3;
if 




proc freq data = up.HIV_prevalent_traj_2016_2017_3;
table status3;
run;


*Select models with mental, based on clinical relevance AND minimum AIC

GROUP 2 - Higher adherence; 

*Model 1; 
ods graphics on;
PROC LOGISTIC DATA=up.HIV_prevalent_traj_2016_2017_3 descending; 
CLASS PATIENT_SEX (ref='F') mental(ref='0') regimen3 (ref='1') status2 (ref='2')
/PARAM=glm;
MODEL group2_80 = PATIENT_SEX age mental regimen3 status2
/link=logit;
oddsratio PATIENT_SEX/diff=ref;
oddsratio age/diff=ref;
ODDSRATIO regimen3 /DIFF=ref;
ODDSRATIO mental /DIFF=ref;
ODDSRATIO status2 /DIFF=ref;
RUN;
*AIC with mental: 2499.319 ods graphics on and plots(only)=(roc(id=obs) effect);

ods graphics on;
PROC LOGISTIC DATA=up.HIV_prevalent_traj_2016_2017_3 descending; 
CLASS PATIENT_SEX (ref='F') mental(ref='0') regimen3 (ref='1') status (ref='2')
/PARAM=glm;
MODEL group2_80 = PATIENT_SEX age mental regimen3 status
/link=logit;
oddsratio PATIENT_SEX/diff=ref;
oddsratio age/diff=ref;
ODDSRATIO regimen3 /DIFF=ref;
ODDSRATIO mental /DIFF=ref;
ODDSRATIO status /DIFF=ref;
RUN;

ods graphics on;
PROC LOGISTIC DATA=up.HIV_prevalent_traj_2016_2017_3 descending; 
CLASS PATIENT_SEX (ref='F') mental(ref='0') regimen3 (ref='1') naive_all (ref='0') time_therapy (ref='0')
/PARAM=glm;
MODEL group2_80 = PATIENT_SEX age mental regimen3 naive_all time_therapy
/link=logit;
oddsratio PATIENT_SEX/diff=ref;
oddsratio age/diff=ref;
ODDSRATIO regimen3 /DIFF=ref;
ODDSRATIO mental /DIFF=ref;
ODDSRATIO naive_all /DIFF=ref;
ODDSRATIO time_therapy /DIFF=ref;
RUN;






ods graphics on;
PROC LOGISTIC DATA=up.HIV_prevalent_traj_2016_2017_3 descending; 
CLASS PATIENT_SEX (ref='F') mental(ref='0') regimen1 (ref='1') status2 (ref='2')
/PARAM=glm;
MODEL group2_80 = PATIENT_SEX age mental regimen1 status2
/link=logit;
oddsratio PATIENT_SEX/diff=ref;
oddsratio age/diff=ref;
ODDSRATIO regimen1 /DIFF=ref;
ODDSRATIO mental /DIFF=ref;
ODDSRATIO status2 /DIFF=ref;
RUN;
*AIC with mental: 2499.319 ods graphics on and plots(only)=(roc(id=obs) effect);




ods graphics on; *which prior swicth;
PROC LOGISTIC DATA=up.HIV_prevalent_traj_2016_2017_3 descending; 
CLASS PATIENT_SEX (ref='F') mental(ref='0') regimen3 (ref='1') status2 (ref='2') n_sw  (ref='0')
/PARAM=glm;
MODEL group2_80 = PATIENT_SEX mental age regimen3 status2 n_sw
/link=logit;
oddsratio PATIENT_SEX/diff=ref;
oddsratio age;
ODDSRATIO regimen3 /DIFF=ref;
ODDSRATIO mental /DIFF=ref;
ODDSRATIO status2 /DIFF=ref;
ODDSRATIO n_sw /DIFF=ref;
RUN;



ods graphics on; *which n prior swicth;
PROC LOGISTIC DATA=up.HIV_prevalent_traj_2016_2017_3 descending; 
CLASS PATIENT_SEX (ref='F') mental(ref='0') regimen3 (ref='1') status2 (ref='2') flag_prev_reg  (ref='0')
/PARAM=glm;
MODEL group2_80 = PATIENT_SEX mental age regimen3 status2 flag_prev_reg
/link=logit;
oddsratio PATIENT_SEX/diff=ref;
oddsratio age;
ODDSRATIO regimen3 /DIFF=ref;
ODDSRATIO mental /DIFF=ref;
ODDSRATIO status2 /DIFF=ref;
ODDSRATIO flag_prev_reg /DIFF=ref;
RUN;


ods graphics on; *Time on cART and prior switch;
PROC LOGISTIC DATA=up.HIV_prevalent_traj_2016_2017_3 descending; 
CLASS PATIENT_SEX (ref='F') mental(ref='0') regimen3 (ref='1') status3 (ref='2') flag_prev_reg  (ref='0')
/PARAM=glm;
MODEL group2_80 = PATIENT_SEX mental age regimen3 status3 flag_prev_reg
/link=logit;
oddsratio PATIENT_SEX/diff=ref;
oddsratio age;
ODDSRATIO regimen3 /DIFF=ref;
ODDSRATIO mental /DIFF=ref;
ODDSRATIO status3 /DIFF=ref;
ODDSRATIO flag_prev_reg /DIFF=ref;
RUN;
*AIC with mental: 2499.319 ods graphics on and plots(only)=(roc(id=obs) effect);

ods graphics on; *Time on cART and n prior switch;
PROC LOGISTIC DATA=up.HIV_prevalent_traj_2016_2017_3 descending; 
CLASS PATIENT_SEX (ref='F') mental(ref='0') regimen3 (ref='1') status3 (ref='2') n_sw  (ref='0')
/PARAM=glm;
MODEL group2_80 = PATIENT_SEX mental age regimen3 status3 n_sw
/link=logit;
oddsratio PATIENT_SEX/diff=ref;
oddsratio age;
ODDSRATIO regimen3 /DIFF=ref;
ODDSRATIO mental /DIFF=ref;
ODDSRATIO status3 /DIFF=ref;
ODDSRATIO n_sw /DIFF=ref;
RUN;


data up.HIV_prevalent_traj_2016_2017_3;
set up.HIV_prevalent_traj_2016_2017_3;
if PATIENT_SEX = 'F' then sex = 0;
if PATIENT_SEX = 'M' then sex = 1;
run;

proc reg data=up.HIV_prevalent_traj_2016_2017_2; *Model 1;
      model group2_80 = SEX age mental regimen2 status
            / tol vif collin; *Tol and vif are acceptable Tol >0.2 and vif close to 1; :)
   run;

*GROUP 1 - Intermediate adherence;

*Model 1;
ods graphics on;
PROC LOGISTIC DATA=up.HIV_prevalent_traj_2016_2017_3 descending; 
CLASS PATIENT_SEX (ref='F') mental(ref='0') regimen3 (ref='1') status2 (ref='2')
/PARAM=glm;
MODEL group1_80 = PATIENT_SEX age mental regimen3 status2
/link=logit;
oddsratio PATIENT_SEX/diff=ref;
oddsratio age/diff=ref;
ODDSRATIO regimen3 /DIFF=ref;
ODDSRATIO mental /DIFF=ref;
ODDSRATIO status2 /DIFF=ref;
RUN;

*GROUP3 - Early non adherence;

*Model 1;
ods graphics on;
PROC LOGISTIC DATA=up.HIV_prevalent_traj_2016_2017_3 descending; 
CLASS PATIENT_SEX (ref='F') mental(ref='0') regimen3 (ref='1') status2 (ref='2')
/PARAM=glm;
MODEL group3_80 = PATIENT_SEX age mental regimen3 status2
/link=logit;
oddsratio PATIENT_SEX/diff=ref;
oddsratio age/diff=ref;
ODDSRATIO regimen3 /DIFF=ref;
ODDSRATIO mental /DIFF=ref;
ODDSRATIO status2 /DIFF=ref;
RUN;
 *Quasi-complete separation of data points detected  AIC = 369.493



***********;
proc sql;
create table prevalent_regimens as
select distinct regimen, drugs_all, count (drugs_all) as count
from up.HIV_prevalent_traj_2016_2017_3;
group by regimen, drugs_all;
quit;

proc freq data = up.HIV_prevalent_traj_2016_2017 order = freq;
table drugs_all;
run;

proc export data = prevalent_regimens outfile = 'Z:\PBS 10% Sample\Projects\HAART (JC)\prevalent_regimens_092019.csv' dbms = csv replace; run;

************************************************
SENSITIVITY ANALYSIS - one ART 
***********************************************;


DATA pdc3_s1; 
SET pdc2;

	day=INPUT(SUBSTR(_name_,4,4),4.); 
	month=CEIL(day/30);
	coverage_ARV= sum (of col1-col9);
	if coverage_ARV ge 1 then covered_regimen = 1;
	if coverage_ARV lt 1 then covered_regimen = 0;
	DROP _name_; RENAME col1=daily_use1 col2=daily_use2 col3=daily_use3 col4=daily_use4 col5=daily_use5 col6=daily_use6 col7=daily_use7 col8=daily_use8 col9=daily_use9;
RUN;

PROC SQL; CREATE TABLE pdc4_s1 AS	
	SELECT DISTINCT pat_id, start_dt, month,
		SUM(covered_regimen)/30 AS month_pdc
	FROM pdc3_s1 GROUP BY pat_id, month; 
QUIT;

PROC SQL; CREATE TABLE total_pdc_s1 AS	
	SELECT DISTINCT pat_id, start_dt, 
		SUM(covered_regimen)/360 AS p_pdc
	FROM pdc3_s1 GROUP BY pat_id; 
QUIT;

data total_pdc_s1; 
set total_pdc_s1;
if p_pdc >=.95 then Adh_95 = 1;
if p_pdc <.95 then Adh_95 = 0;
if p_pdc >=.80 then Adh_80 = 1;
if p_pdc <.80 then Adh_80 = 0;
run;

proc freq data = total_pdc_s1 order = freq;
table Adh_80/ binomial;
run;
proc means data = total_pdc_s1 p25 p50 p75;
var P_PDC;
run;

data HIV_pdc_monthly_s1; 
set pdc4_s1;
if month_pdc >=.95 then Adh_95 = 1;
if month_pdc <.95 then Adh_95 = 0;
if month_pdc >=.80 then Adh_80 = 1;
if month_pdc <.80 then Adh_80 = 0;
run;


PROC TRANSPOSE DATA=Hiv_pdc_monthly_s1 out=month_s1 (DROP=_name_) prefix=month_;
	by pat_id; 
    var month; 
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly_s1 out=Adh95_s1 (DROP=_name_) prefix=Adh95_;
	by pat_id; 
    var Adh_95; 
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly_s1 out=Adh80_s1 (DROP=_name_) prefix=Adh80_;
	by pat_id; 
    var Adh_80; 
run;

Data Traj_95_80_s1; 
merge month_s1 Adh80_s1 Adh95_s1;
BY pat_id; 
Run;

PROC TRAJ DATA= Traj_95_80_s1 OUTPLOT=op_stat_ms_s1 OUTSTAT=os_stat_ms_s1 OUT=of_stat_ms_s1 OUTEST=oe_stat_ms_s1;
	ID pat_id;
	VAR Adh80_1-Adh80_12;
	INDEP month_1-month_12;
	MODEL logit;
	ORDER 2 2 1;
RUN;

%trajplot(op_stat_ms_s1,os_stat_ms_s1,"Adherence to cART sensitivity","Trajectory model","Percent","Month");

*2 2 2  BIC= -7947 - too low third group (<1%)
*2 3 2  BIC= -7900.74 - too low third group (<1%)
* 2 2 1  -7828.19 *Selected
*3 3 1 -7749.54
*3 3 2 BIC= -7786.77;


proc export data = op_stat_ms_s outfile = 'Z:\PBS 10% Sample\Projects\HAART (JC)\op_stat_ms_s1.csv' dbms = csv replace; run;

proc univariate data=of_stat_ms_s noprint; where group=1; histogram grp1prb;run;
proc means data=of_stat_ms_s; var grp1prb; where group=1;run;
proc means data=of_stat_ms_s p50 p25 p75; var grp1prb; where group=1;run;

proc univariate data=of_stat_ms_s noprint; where group=2; histogram grp2prb;run;
proc means data=of_stat_ms_s; var grp2prb; where group=2;run;
proc means data=of_stat_ms_s p50 p25 p75; var grp2prb; where group=2;run;

proc univariate data=of_stat_ms_s noprint; where group=3; histogram grp3prb;run;
proc means data=of_stat_ms_s; var grp3prb; where group=3;run;
proc means data=of_stat_ms_s p50 p25 p75; var grp3prb; where group=3;run;


************************************************
SENSITIVITY ANALYSIS - two ART at the same time;
***********************************************;
 
DATA pdc3_sensitivity; 
SET pdc2;

	day=INPUT(SUBSTR(_name_,4,4),4.); 
	month=CEIL(day/30);
	coverage_ARV= sum (of col1-col9);
	if coverage_ARV ge 2 then covered_regimen = 1;
	if coverage_ARV lt 2 then covered_regimen = 0;
	DROP _name_; RENAME col1=daily_use1 col2=daily_use2 col3=daily_use3 col4=daily_use4 col5=daily_use5 col6=daily_use6 col7=daily_use7 col8=daily_use8 col9=daily_use9;
RUN;

PROC SQL; CREATE TABLE pdc4_sensitivity AS	
	SELECT DISTINCT pat_id, start_dt, month,
		SUM(covered_regimen)/30 AS month_pdc
	FROM pdc3_sensitivity GROUP BY pat_id, month; 
QUIT;

PROC SQL; CREATE TABLE total_pdc_sensitivity AS	
	SELECT DISTINCT pat_id, start_dt, 
		SUM(covered_regimen)/360 AS p_pdc
	FROM pdc3_sensitivity GROUP BY pat_id; 
QUIT;

data total_pdc_sensitivity; 
set total_pdc_sensitivity;
if p_pdc >=.95 then Adh_95 = 1;
if p_pdc <.95 then Adh_95 = 0;
if p_pdc >=.80 then Adh_80 = 1;
if p_pdc <.80 then Adh_80 = 0;
run;

proc freq data = total_pdc_sensitivity;
table Adh_80 Adh_95 P_PDC;
run;
proc means data = total_pdc_sensitivity p25 p50 p75;
var P_PDC;
run;

data HIV_pdc_monthly_s; 
set pdc4_sensitivity;
if month_pdc >=.95 then Adh_95 = 1;
if month_pdc <.95 then Adh_95 = 0;
if month_pdc >=.80 then Adh_80 = 1;
if month_pdc <.80 then Adh_80 = 0;
run;


PROC TRANSPOSE DATA=Hiv_pdc_monthly_s out=month_s (DROP=_name_) prefix=month_;
	by pat_id; 
    var month; 
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly_s out=Adh95_s (DROP=_name_) prefix=Adh95_;
	by pat_id; 
    var Adh_95; 
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly_s out=Adh80_s (DROP=_name_) prefix=Adh80_;
	by pat_id; 
    var Adh_80; 
run;

Data Traj_95_80_s; 
merge month_s Adh80_s Adh95_s;
BY pat_id; 
Run;

PROC TRAJ DATA= Traj_95_80_s OUTPLOT=op_stat_ms_s OUTSTAT=os_stat_ms_s OUT=of_stat_ms_s OUTEST=oe_stat_ms_s;
	ID pat_id;
	VAR Adh80_1-Adh80_12;
	INDEP month_1-month_12;
	MODEL logit;
	ORDER 2 2 1;
RUN;

%trajplot(op_stat_ms_s,os_stat_ms_s,"Adherence to cART sensitivity","Trajectory model","Percent","Month");

*2 2 2  BIC= -7947 - too low third group (<1%)
*2 3 2  BIC= -7900.74 - too low third group (<1%)
* 2 2 1  -7828.19 *Selected
*3 3 1 -7749.54
*3 3 2 BIC= -7786.77;


proc export data = op_stat_ms_s outfile = 'Z:\PBS 10% Sample\Projects\HAART (JC)\op_stat_ms_sensitivity.csv' dbms = csv replace; run;

proc univariate data=of_stat_ms_s noprint; where group=1; histogram grp1prb;run;
proc means data=of_stat_ms_s; var grp1prb; where group=1;run;
proc means data=of_stat_ms_s p50 p25 p75; var grp1prb; where group=1;run;

proc univariate data=of_stat_ms_s noprint; where group=2; histogram grp2prb;run;
proc means data=of_stat_ms_s; var grp2prb; where group=2;run;
proc means data=of_stat_ms_s p50 p25 p75; var grp2prb; where group=2;run;

proc univariate data=of_stat_ms_s noprint; where group=3; histogram grp3prb;run;
proc means data=of_stat_ms_s; var grp3prb; where group=3;run;
proc means data=of_stat_ms_s p50 p25 p75; var grp3prb; where group=3;run;

************************************************
SENSITIVITY ANALYSIS - three ART at the same time (counting RITONAVIR);
***********************************************;
 PROC TRANSPOSE DATA=pdc OUT=pdc2_R;
	BY pat_id start_dt;
	VAR day1-day360;
	where drug_name not in ('COBICISTAT');
RUN;

DATA pdc3_sensitivity_R; 
SET pdc2_R;

	day=INPUT(SUBSTR(_name_,4,4),4.); 
	month=CEIL(day/30);
	coverage_ARV= sum (of col1-col9);
	if coverage_ARV ge 3 then covered_regimen = 1;
	if coverage_ARV lt 3 then covered_regimen = 0;
	DROP _name_; RENAME col1=daily_use1 col2=daily_use2 col3=daily_use3 col4=daily_use4 col5=daily_use5 col6=daily_use6 col7=daily_use7 col8=daily_use8 col9=daily_use9;
RUN;

PROC SQL; CREATE TABLE pdc4_sensitivity_R AS	
	SELECT DISTINCT pat_id, start_dt, month,
		SUM(covered_regimen)/30 AS month_pdc
	FROM pdc3_sensitivity_R GROUP BY pat_id, month; 
QUIT;

PROC SQL; CREATE TABLE total_pdc_sensitivity_R AS	
	SELECT DISTINCT pat_id, start_dt, 
		SUM(covered_regimen)/360 AS p_pdc
	FROM pdc3_sensitivity_R GROUP BY pat_id; 
QUIT;

data total_pdc_sensitivity_R; 
set total_pdc_sensitivity_R;
if p_pdc >=.95 then Adh_95 = 1;
if p_pdc <.95 then Adh_95 = 0;
if p_pdc >=.80 then Adh_80 = 1;
if p_pdc <.80 then Adh_80 = 0;
run;

proc freq data = total_pdc_sensitivity_R;
table Adh_80 Adh_95 P_PDC;
run;
proc means data = total_pdc_sensitivity_R p25 p50 p75;
var P_PDC;
run;
 *VERY SIMILAR!!! DON'T SHOW;

************************************************
SENSITIVITY ANALYSIS - 3 days grace period;
*********************************************** 

Considering the grace period;

proc sort data = gap_calculation_3arv;
  by pat_id group;
run;

data up.grace_3arv;
  set gap_calculation_3arv;
  count + 1;
  by pat_id group;
  if first.group then count = 1;
run;

*Adjusting covered days;
data up.grace_3days_3ARV;
  set up.grace_3arv;
  if covered_regimen =1 then adj_daily_use = 1;
  if covered_regimen =0 and count<=3 then adj_daily_use = 1;
  if covered_regimen =0 and count>3 then adj_daily_use = 0;
run;

PROC SQL; CREATE TABLE HIV_pdc_total_grace3 AS	
	SELECT DISTINCT pat_id,
		SUM(adj_daily_use) AS days_covered
	FROM up.grace_3days_3ARV GROUP BY pat_id; 
QUIT;

data HIV_pdc_total_grace3;
set HIV_pdc_total_grace3;
p_dayscovered = days_covered/360;
P_100 = p_dayscovered*100;
if p_dayscovered >=.95 then Adh_95 = 1;
if p_dayscovered <.95 then Adh_95 = 0;
if p_dayscovered >=.80 then Adh_80 = 1;
if p_dayscovered <.80 then Adh_80 = 0;
label P_100='Proportion of days covered (%)'; 
run;

proc freq data = HIV_pdc_total_grace3 order = freq;
table Adh_80 /binomial;
run;

proc means data = HIV_pdc_total_grace3 p25 p50 p75;
var P_100;
run;


PROC SQL; CREATE TABLE pdc4_grace3 AS	
	SELECT DISTINCT pat_id, start_dt, month,
		SUM(adj_daily_use)/30 AS month_pdc
	FROM up.grace_3days_3ARV GROUP BY pat_id, month; 
QUIT;

data HIV_pdc_monthly_grace3; 
set pdc4_grace3;
if month_pdc >=.95 then Adh_95 = 1;
if month_pdc <.95 then Adh_95 = 0;
if month_pdc >=.80 then Adh_80 = 1;
if month_pdc <.80 then Adh_80 = 0;
run;

PROC TRANSPOSE DATA=HIV_pdc_monthly_grace3 out=month_grace3 (DROP=_name_) prefix=month;
	by pat_id; 
    var month; 
run;

PROC TRANSPOSE DATA=HIV_pdc_monthly_grace3 out=Adh95_grace3 (DROP=_name_) prefix=Adh95_;
	by pat_id; 
    var Adh_95; 
run;

PROC TRANSPOSE DATA=HIV_pdc_monthly_grace3 out=Adh80_grace3 (DROP=_name_) prefix=Adh80_;
	by pat_id; 
    var Adh_80; 
run;

Data Traj_95_80_grace3; 
merge month_grace3 Adh80_grace3 Adh95_grace3;
BY pat_id; 
Run;

******************pdc80% *****************************************************************;

PROC TRAJ DATA= Traj_95_80_grace3 OUTPLOT=op_stat3 OUTSTAT=os_stat3 OUT=of_stat3 OUTEST=oe_stat3;
	ID pat_id;
	VAR Adh80_1-Adh80_12;
	INDEP month1-month12;
	MODEL logit;
	ORDER 2 2 2;
RUN;

%trajplot(op_stat3,os_stat3,"Adherence 80% - Grace 3 days","Trajectory model","Percent","Month");

*Model selection;

*3 1 3 BIC= -7526.36 (Does not create file for ploting)

* 3 2 -7456.8
* 3 3 BIC = -7416.76

* 2 2 2 BIC = -7399.28 *SELECTED
* 2 3 2  BIC= -7334.74


*3 2 1 BIC= -7455.77 (Does not create file for ploting)
*3 2 2 BIC= -7460.8 (3 group too low)  
*3 3 2 BIC= -7534.0
*2 2 2 BIC= -7518.8
*3 3 1  BIC= -7479.4;


proc export data = op_stat3 outfile = 'Z:\PBS 10% Sample\Projects\HAART (JC)\op_stat_grace.csv' dbms = csv replace; run;

proc univariate data=of_stat3 noprint; where group=1; histogram grp1prb;run;
proc means data=of_stat3; var grp1prb; where group=1;run;
proc means data=of_stat3 p50 p25 p75; var grp1prb; where group=1;run;

proc univariate data=of_stat3 noprint; where group=2; histogram grp2prb;run;
proc means data=of_stat3; var grp2prb; where group=2;run;
proc means data=of_stat3 p50 p25 p75; var grp2prb; where group=2;run;

proc univariate data=of_stat3 noprint; where group=3; histogram grp3prb;run;
proc means data=of_stat3; var grp3prb; where group=3;run;
proc means data=of_stat3 p50 p25 p75; var grp3prb; where group=3;run;







**********************************THE END********************************;

data up.HIV_prevalent_traj;
set up.HIV_prevalent_traj;
if Mental or C10 or A10 or C23789 = 1 then other_med=1;
if missing(other_med) then other_med = 0;
run;

proc freq data = up.HIV_prevalent_traj;
table gap_art_dic;
run;

proc freq data = up.HIV_prevalent_traj;
table N_Reg_switch*group80 / chisq;
run;

proc freq data = up.HIV_prevalent_traj;
table Adh_80*group80 / FISHER;
run;

proc freq data = up.HIV_prevalent_traj;
table Adh_80*group80 / chisq;
run;

proc freq data = up.HIV_prevalent_traj;
table Adh_80*PHARMACY / FISHER;
run;


proc npar1way data=up.HIV_prevalent_traj WILCOXON;
	 class group80;
	 var P_PDC;
	run;

proc npar1way data=up.HIV_prevalent_traj MEDIAN;
	 class group1_80;
	 var P_PDC;
	run;

proc npar1way data=up.HIV_prevalent_traj;
	 class group80;
	 var P_PDC;
	run;


proc freq data = up.HIV_prevalent_traj;
table PHARMACY_STATE*group80 / CHSQ;
run;

proc freq data = up.HIV_prevalent_traj;
table PHARMACY_STATE1*group80 / CHSQ;
run;

proc freq data = up.HIV_prevalent_traj;
table PHARMACY_STATE*Adh_80 / FISHER;
run;

proc freq data = up.HIV_prevalent_traj;
table PHARMACY_STATE1*Adh_80 / CHSQ;
run;

proc freq data = up.HIV_prevalent_traj;
table PHARMACY_STATE1*group80 / CHSQ;
run;


****************************************************************
                      ADHERENCE CALCULATIONS - WITHOUT THOSE LTFU
*****************************************************************
In accordance to Leslie et al., (2009);

*Use original dispensing data with one drug per row of included patients;
proc sort data = up.haart_hiv4c;
by pat_id supp_date drug_name;
run;

proc sort data = up.Hiv_c_p_3arv_2016_2017;
by pat_id;
run;

data included_sens (keep = pat_id first_supp2 first_supp_treat death);
set up.HIV_prevalent_traj_2016_2017_5;
if missing(first_supp_treat) then first_supp_treat = first_supp2; 
where LTFU = 0;
run;

data HIV_prev_disp;
merge up.haart_hiv4c (in = a) included_sens (in=b);
by pat_id;
if b;
run;


***********************
*Restrict dispensings to 360 days of up, since the first_supp_treat to account for days when the first medicine of the regimen was dispensed;

data HIV_prev_disp2;
set HIV_prev_disp;
IF (first_supp_treat)<=supp_date<(first_supp2+360) THEN OUTPUT; 
run;

proc sort data=HIV_prev_disp2; by pat_ID drug_name supp_date; 
run; 
 
proc transpose data = HIV_prev_disp2 out=fill_dates (drop=_name_) prefix = supp_date; 
by pat_ID drug_name; 
var supp_date; 
run; 
 
proc transpose data = HIV_prev_disp2 out=days_supply (drop=_name_) prefix = dur; 
by pat_ID drug_name; 
var duration; 
run; 
 
data both; 
merge fill_dates days_supply; 
by pat_ID drug_name; 
run; 

*Adjust observation window for adherence calculations per patient;
data both2;
merge both (in=a) included_ids (in=b);
by pat_id;
start_dt=first_supp2; 
end_dt=first_supp2+360;
days=end_dt-start_dt;
format start_dt end_dt DATE9.; 
run;

*Days covered for each medicine, accounting for early reffils and reffils in the same day;
data pdc; 
set both2; 
array daydummy(360) day1-day360; 
array filldates(*) supp_date1 - supp_date12; 
array days_supply(*) dur1 - dur12;

do u=2 to 12 while (filldates(u) ^= .);       
if filldates(u)<filldates(u-1)+days_supply(u-1)       
then filldates[u]=filldates(u-1)+days_supply(u-1);     
end; 

do ii=1 to 360; daydummy(ii)=0;
end; 

do ii=1 to 360; 
do i = 1 to dim(filldates) while (filldates(i) ^=.);       
if filldates(i)<= start_dt + ii -1 <= filldates(i)+ days_supply(i)-1          
then daydummy(ii)=1;         
end;  
end; 
 
drop i ii u; 

run;

PROC TRANSPOSE DATA=pdc OUT=pdc2;
	BY pat_id start_dt;
	VAR day1-day360;
	where drug_name not in ('RITONAVIR', 'COBICISTAT');
RUN;

*Day covered as overlapping dispensing of at least 3 meds, excluding ritonavir and cobicistat that are enhancers;
DATA pdc3; 
SET pdc2;

	day=INPUT(SUBSTR(_name_,4,4),4.); 
	month=CEIL(day/30);
	coverage_ARV= sum (of col1-col9);
	if coverage_ARV ge 3 then covered_regimen = 1;
	if coverage_ARV lt 3 then covered_regimen = 0;
	DROP _name_; RENAME col1=daily_use1 col2=daily_use2 col3=daily_use3 col4=daily_use4 col5=daily_use5 col6=daily_use6 col7=daily_use7 col8=daily_use8 col9=daily_use9;
RUN;

PROC SQL; CREATE TABLE pdc4 AS	
	SELECT DISTINCT pat_id, start_dt, month,
		SUM(covered_regimen)/30 AS month_pdc
	FROM pdc3 GROUP BY pat_id, month; 
QUIT;

PROC SQL; CREATE TABLE total_pdc AS	
	SELECT DISTINCT pat_id, start_dt, 
		SUM(covered_regimen)/360 AS p_pdc
	FROM pdc3 GROUP BY pat_id; 
QUIT;

*Cut-offs of adherence;
data total_pdc; 
set total_pdc;
if p_pdc >=.95 then Adh_95 = 1;
if p_pdc <.95 then Adh_95 = 0;
if p_pdc >=.80 then Adh_80 = 1;
if p_pdc <.80 then Adh_80 = 0;
run;

proc freq data = total_pdc;
table Adh_80 Adh_95;
run;

proc means data = total_pdc; var p_pdc; run;
proc means data = total_pdc p25 p50 p75;
var p_pdc;
run;



*Monthly adherence to trajectory;
data HIV_pdc_monthly; 
set pdc4;
if month_pdc >=.95 then Adh_95 = 1;
if month_pdc <.95 then Adh_95 = 0;
if month_pdc >=.80 then Adh_80 = 1;
if month_pdc <.80 then Adh_80 = 0;
run;

title 'Frequency of adherent people (80%) by month';
proc sgplot data=HIV_pdc_monthly;
vbar month;
where Adh_80=1;
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly out=month (DROP=_name_) prefix=month_;
	by pat_id; 
    var month; 
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly out=Adh95 (DROP=_name_) prefix=Adh95_;
	by pat_id; 
    var Adh_95; 
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly out=Adh80 (DROP=_name_) prefix=Adh80_;
	by pat_id; 
    var Adh_80; 
run;

Data Traj_95_80; 
merge month Adh80 Adh95 included_ids (keep = pat_id death);
BY pat_id; 
Run;

****************Trajectory PDC 80**************************;

PROC TRAJ DATA= Traj_95_80 OUTPLOT=op_stat_ms OUTSTAT=os_stat_ms OUT=of_stat_ms OUTEST=oe_stat_ms ci95m;
	ID pat_id;
	VAR Adh80_1-Adh80_12;
	INDEP month_1-month_12;
	MODEL logit;
	ORDER 3 3 3;
	RUN;

%trajplot(op_stat_ms,os_stat_ms,"Adherence to cART - PDC 80%","Trajectory model","Percent","Month");
%trajplotnew(op_stat_ms,os_stat_ms,"Adherence to cART - PDC 80%","Trajectory model","Percent","Month");


****************************************************************
                      ADHERENCE CALCULATIONS - WITHOUT THOSE LTFU
*****************************************************************
In accordance to Leslie et al., (2009);

*Use original dispensing data with one drug per row of included patients;
proc sort data = up.haart_hiv4c;
by pat_id supp_date drug_name;
run;

proc sort data = up.Hiv_c_p_3arv_2016_2017;
by pat_id;
run;

data included_sens (keep = pat_id first_supp2 first_supp_treat death);
set up.HIV_prevalent_traj_2016_2017_5;
if missing(first_supp_treat) then first_supp_treat = first_supp2; 
where LTFU2 = 0;
run;

data HIV_prev_disp;
merge up.haart_hiv4c (in = a) included_sens (in=b);
by pat_id;
if b;
run;


***********************
*Restrict dispensings to 360 days of up, since the first_supp_treat to account for days when the first medicine of the regimen was dispensed;

data HIV_prev_disp2;
set HIV_prev_disp;
IF (first_supp_treat)<=supp_date<(first_supp2+360) THEN OUTPUT; 
run;

proc sort data=HIV_prev_disp2; by pat_ID drug_name supp_date; 
run; 
 
proc transpose data = HIV_prev_disp2 out=fill_dates (drop=_name_) prefix = supp_date; 
by pat_ID drug_name; 
var supp_date; 
run; 
 
proc transpose data = HIV_prev_disp2 out=days_supply (drop=_name_) prefix = dur; 
by pat_ID drug_name; 
var duration; 
run; 
 
data both; 
merge fill_dates days_supply; 
by pat_ID drug_name; 
run; 

*Adjust observation window for adherence calculations per patient;
data both2;
merge both (in=a) included_sens (in=b);
by pat_id;
start_dt=first_supp2; 
end_dt=first_supp2+360;
days=end_dt-start_dt;
format start_dt end_dt DATE9.; 
run;

*Days covered for each medicine, accounting for early reffils and reffils in the same day;
data pdc; 
set both2; 
array daydummy(360) day1-day360; 
array filldates(*) supp_date1 - supp_date12; 
array days_supply(*) dur1 - dur12;

do u=2 to 12 while (filldates(u) ^= .);       
if filldates(u)<filldates(u-1)+days_supply(u-1)       
then filldates[u]=filldates(u-1)+days_supply(u-1);     
end; 

do ii=1 to 360; daydummy(ii)=0;
end; 

do ii=1 to 360; 
do i = 1 to dim(filldates) while (filldates(i) ^=.);       
if filldates(i)<= start_dt + ii -1 <= filldates(i)+ days_supply(i)-1          
then daydummy(ii)=1;         
end;  
end; 
 
drop i ii u; 

run;

PROC TRANSPOSE DATA=pdc OUT=pdc2;
	BY pat_id start_dt;
	VAR day1-day360;
	where drug_name not in ('RITONAVIR', 'COBICISTAT');
RUN;

*Day covered as overlapping dispensing of at least 3 meds, excluding ritonavir and cobicistat that are enhancers;
DATA pdc3; 
SET pdc2;

	day=INPUT(SUBSTR(_name_,4,4),4.); 
	month=CEIL(day/30);
	coverage_ARV= sum (of col1-col9);
	if coverage_ARV ge 3 then covered_regimen = 1;
	if coverage_ARV lt 3 then covered_regimen = 0;
	DROP _name_; RENAME col1=daily_use1 col2=daily_use2 col3=daily_use3 col4=daily_use4 col5=daily_use5 col6=daily_use6 col7=daily_use7 col8=daily_use8 col9=daily_use9;
RUN;

PROC SQL; CREATE TABLE pdc4 AS	
	SELECT DISTINCT pat_id, start_dt, month,
		SUM(covered_regimen)/30 AS month_pdc
	FROM pdc3 GROUP BY pat_id, month; 
QUIT;

PROC SQL; CREATE TABLE total_pdc AS	
	SELECT DISTINCT pat_id, start_dt, 
		SUM(covered_regimen)/360 AS p_pdc
	FROM pdc3 GROUP BY pat_id; 
QUIT;

*Cut-offs of adherence;
data total_pdc; 
set total_pdc;
if p_pdc >=.95 then Adh_95 = 1;
if p_pdc <.95 then Adh_95 = 0;
if p_pdc >=.80 then Adh_80 = 1;
if p_pdc <.80 then Adh_80 = 0;
run;

proc freq data = total_pdc;
table Adh_80 Adh_95;
run;

proc means data = total_pdc; var p_pdc; run;
proc means data = total_pdc p25 p50 p75;
var p_pdc;
run;



*Monthly adherence to trajectory;
data HIV_pdc_monthly; 
set pdc4;
if month_pdc >=.95 then Adh_95 = 1;
if month_pdc <.95 then Adh_95 = 0;
if month_pdc >=.80 then Adh_80 = 1;
if month_pdc <.80 then Adh_80 = 0;
run;

title 'Frequency of adherent people (80%) by month';
proc sgplot data=HIV_pdc_monthly;
vbar month;
where Adh_80=1;
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly out=month (DROP=_name_) prefix=month_;
	by pat_id; 
    var month; 
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly out=Adh95 (DROP=_name_) prefix=Adh95_;
	by pat_id; 
    var Adh_95; 
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly out=Adh80 (DROP=_name_) prefix=Adh80_;
	by pat_id; 
    var Adh_80; 
run;

Data Traj_95_80; 
merge month Adh80 Adh95 included_sens (keep = pat_id death);
BY pat_id; 
Run;

****************Trajectory PDC 80**************************;

PROC TRAJ DATA= Traj_95_80 OUTPLOT=op_stat_ms OUTSTAT=os_stat_ms OUT=of_stat_ms OUTEST=oe_stat_ms ci95m;
	ID pat_id;
	VAR Adh80_1-Adh80_12;
	INDEP month_1-month_12;
	MODEL logit;
	ORDER 3 3 3;
	RUN;

%trajplot(op_stat_ms,os_stat_ms,"Adherence to cART - PDC 80%","Trajectory model","Percent","Month");
%trajplotnew(op_stat_ms,os_stat_ms,"Adherence to cART - PDC 80%","Trajectory model","Percent","Month");


proc print data = up.hiv_prevalent_traj_2016_2017_5;
var pat_id first_supp2 last_supp_art last_supp_any lastsup_anyfollow;
run;


***********************
*Calculate adherence between first and last cart dispensing, those with only one dispensing were classified as non-adherent?;

*Use original dispensing data with one drug per row of included patients;
proc sort data = up.haart_hiv4c;
by pat_id supp_date drug_name;
run;

proc sort data = up.Hiv_c_p_3arv_2016_2017;
by pat_id;
run;

data included_ids (keep = pat_id first_supp2 first_supp_treat death);
set up.Hiv_c_p_3arv_2016_2017;
if missing(first_supp_treat) then first_supp_treat = first_supp2; 
run;

data HIV_prev_disp;
merge up.haart_hiv4c (in = a) included_ids (in=b);
by pat_id;
if b;
run;

data HIV_prev_disp_follow;
set HIV_prev_disp;
IF (first_supp_treat)<=supp_date<=(first_supp2+360) THEN OUTPUT; 
run;

data last_supp_art_y (keep = pat_id supp_date);
set HIV_prev_disp_follow;
if last.pat_id then output;
by pat_id;
run;

data HIV_prev_disp_follow2;
merge HIV_prev_disp_follow (in=a) last_supp_art_y(in=b rename=(supp_date=last_art_y));
by pat_id;
if a;
run;

data HIV_prev_disp_follow3;
set HIV_prev_disp_follow2;
where supp_date <= last_art_y;
run;

proc sort data=HIV_prev_disp_follow3; by pat_ID drug_name supp_date; 
run; 
 
proc transpose data = HIV_prev_disp_follow3 out=fill_dates_y (drop=_name_) prefix = supp_date; 
by pat_ID drug_name; 
var supp_date; 
run; 
 
proc transpose data = HIV_prev_disp_follow3 out=days_supply_y (drop=_name_) prefix = dur; 
by pat_ID drug_name; 
var duration; 
run; 
 
data both_y; 
merge fill_dates_y days_supply_y; 
by pat_ID drug_name; 
run; 

*Adjust observation window for adherence calculations per patient;
data both2_y;
merge both_y (in=a) last_supp_art_y(in=b rename=(supp_date=last_art_y)) included_ids (in=c);
by pat_id;
start_dt=first_supp2; 
end_dt=last_art_y;
days=end_dt-start_dt;
format start_dt end_dt DATE9.;
if c; 
run;

*Days covered for each medicine, accounting for early reffils and reffils in the same day;
data pdc_y; 
set both2_y; 
array daydummy(360) day1-day360; 
array filldates(*) supp_date1 - supp_date12; 
array days_supply(*) dur1 - dur12;

do u=2 to 12 while (filldates(u) ^= .);       
if filldates(u)<filldates(u-1)+days_supply(u-1)       
then filldates[u]=filldates(u-1)+days_supply(u-1);     
end; 

do ii=1 to 360; daydummy(ii)=0;
end; 

do ii=1 to 360; 
do i = 1 to dim(filldates) while (filldates(i) ^=.);       
if filldates(i)<= start_dt + ii -1 <= filldates(i)+ days_supply(i)-1          
then daydummy(ii)=1;         
end;  
end; 
 
drop i ii u; 

run;

PROC TRANSPOSE DATA=pdc_y OUT=pdc2_y;
	BY pat_id start_dt;
	VAR day1-day360;
	where drug_name not in ('RITONAVIR', 'COBICISTAT');
RUN;

*Day covered as overlapping dispensing of at least 3 meds, excluding ritonavir and cobicistat that are enhancers;
DATA pdc3_y; 
SET pdc2_y;

	day=INPUT(SUBSTR(_name_,4,4),4.); 
	month=CEIL(day/30);
	coverage_ARV= sum (of col1-col9);
	if coverage_ARV ge 1 then covered_regimen = 1;
	if coverage_ARV lt 1 then covered_regimen = 0;
	DROP _name_; RENAME col1=daily_use1 col2=daily_use2 col3=daily_use3 col4=daily_use4 col5=daily_use5 col6=daily_use6 col7=daily_use7 col8=daily_use8 col9=daily_use9;
RUN;


data pdc4_y;
merge pdc3_y last_supp_art_y(in=b rename=(supp_date=last_art_y));
by pat_id;
days=last_art_y-start_dt;
run;

data pdc5_y;
set pdc4_y;
where day<=days;
run; 

PROC SQL; CREATE TABLE pdc6_y AS	
	SELECT DISTINCT pat_id, start_dt, month,
		SUM(covered_regimen)/30 AS month_pdc
	FROM pdc5_y GROUP BY pat_id, month; 
QUIT;

PROC SQL; CREATE TABLE total_covered_days AS	
	SELECT DISTINCT pat_id, start_dt, 
		SUM(covered_regimen) AS covered_days
	FROM pdc5_y GROUP BY pat_id; 
QUIT;

*Cut-offs of adherence;
data total_pdc_y; 
merge total_covered_days (drop = start_dt) up.Hiv_prevalent_traj_2016_2017_5 last_supp_art_y(in=b rename=(supp_date=last_art_y));
by pat_id;
run;

data total_pdc_y;
set total_pdc_y;
days_observed = last_art_y - first_supp2;
run;

proc freq data = total_pdc_y;
table days_observed;
run;

proc print data = total_pdc_y;
var pat_id;
where days_observed = 0;
run;

data total_pdc_y;
set total_pdc_y;
if days_observed = 0 then do;
   pdc2 = 0;
   end;
else if  days_observed > 0 then do;
    pdc2 = covered_days/days_observed;
end;
if pdc2 >=.80 then adh2=1;
if pdc2 <.80 then adh2=0;
run;

proc freq data = total_pdc_y order=freq;
table adh2/ BINOMIAL;
where days_observed not eq 0;
run;


proc freq data = total_pdc_y order=freq;
table adh2/ BINOMIAL;
run;

proc freq data = total_pdc_y;
table pdc2;
run;

proc means data = total_pdc_y; var pdc2; where days_observed not eq 0; run;
proc means data = total_pdc_y p25 p50 p75;
var pdc2;
where days_observed not eq 0;
run;



*Monthly adherence to trajectory;
data HIV_pdc_monthly; 
set pdc6_y;
if month_pdc >=.80 then Adh_80 = 1;
if month_pdc <.80 then Adh_80 = 0;
run;

title 'Frequency of adherent people (80%) by month';
proc sgplot data=HIV_pdc_monthly;
vbar month;
where Adh_80=1;
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly out=month (DROP=_name_) prefix=month_;
	by pat_id; 
    var month; 
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly out=Adh80 (DROP=_name_) prefix=Adh80_;
	by pat_id; 
    var Adh_80; 
run;

Data Traj_80; 
merge month (in = a) Adh80 (in=b) included_ids (in = c keep = pat_id first_supp2 death) last_supp_art_y(in=d rename=(supp_date=last_art_y));
BY pat_id; 
if a;
Run;

Data Traj_80; set Traj_80; 
time = last_art_y - first_supp2; run;



proc freq data = Traj_80;
table Adh80_1-Adh80_12;
run;
****************Trajectory PDC 80**************************;

PROC TRAJ DATA= Traj_80 OUTPLOT=op_stat_ms OUTSTAT=os_stat_ms OUT=of_stat_ms OUTEST=oe_stat_ms ci95m;
	ID pat_id;
	VAR Adh80_1-Adh80_12;
	INDEP month_1-month_12;
	MODEL logit;
	risk time;
	ORDER 3 3 3;
	RUN;

%trajplot(op_stat_ms,os_stat_ms,"Adherence to cART - PDC 80%","Trajectory model","Percent","Month");
%trajplotnew(op_stat_ms,os_stat_ms,"Adherence to cART - PDC 80%","Trajectory model","Percent","Month");

proc sql;  
select distinct pat_id, 
Count (distinct Supp_date) as N_dispensings
from HIV_prev_disp_follow
GROUP BY pat_id
having N_dispensings eq 1;
quit;

proc sql;  
select distinct pat_id, 
Count (distinct Supp_date) as N_dispensings
from HIV_prev_disp_follow3
GROUP BY pat_id
having N_dispensings eq 1;
quit;


proc print data = HIV_prev_disp_follow;
var pat_id supp_date drug_name first_supp2 first_supp_treat;
where pat_id in (1521754497, 2729589988, 7394257141, 9089663415, 9234971096, 4171719016);
run;






**********************************************************************************************************;
order=freq;
table adh2 adh_check / BINOMIAL;
run;

data up.Hiv_prevalent_traj_2016_2017_5;
set Hiv_prevalent_traj_2016_2017_5;
if pdc >=.80 then adh_80=1;
if pdc <.80 then adh_80=0;
run;

proc freq data = up.Hiv_prevalent_traj_2016_2017_5 order=freq;
table adh_80 / BINOMIAL;
run;

***********************************************************************
Censor patients on the last disp + supp_dys;


data last_day (keep = pat_id day);
set Pdc3;
by pat_id;
if last.PAT_ID then output;
where covered_regimen = 1;
run;

data pdc3b;
merge pdc3 last_day(in=b rename=(day=last_day));
by pat_id;
run;

data pdc3c;
set pdc3b;
where day<=last_day;
run; 


PROC SQL; CREATE TABLE pdc4 AS	
	SELECT DISTINCT pat_id, start_dt, month,
		SUM(covered_regimen)/30 AS month_pdc
	FROM pdc3c GROUP BY pat_id, month; 
QUIT;

PROC SQL; CREATE TABLE total_pdc AS	
	SELECT DISTINCT pat_id, start_dt, 
		SUM(covered_regimen)/last_day AS p_pdc
	FROM pdc3c GROUP BY pat_id; 
QUIT;

*Cut-offs of adherence;
data total_pdc; 
set total_pdc;
if p_pdc >=.80 then Adh_80 = 1;
if p_pdc <.80 then Adh_80 = 0;
run;

proc freq data = total_pdc order = freq;
table Adh_80 /binomial;
run;

proc means data = total_pdc; var p_pdc; run;
proc means data = total_pdc p25 p50 p75;
var p_pdc;
run;



*Monthly adherence to trajectory;
data HIV_pdc_monthly; 
set pdc4;
if month_pdc >=.80 then Adh_80 = 1;
if month_pdc <.80 then Adh_80 = 0;
run;

title 'Frequency of adherent people (80%) by month';
proc sgplot data=HIV_pdc_monthly;
vbar month;
where Adh_80=1;
run;

PROC TRANSPOSE DATA=Hiv_pdc_monthly out=month (DROP=_name_) prefix=month_;
	by pat_id; 
    var month; 
run;


PROC TRANSPOSE DATA=Hiv_pdc_monthly out=Adh80 (DROP=_name_) prefix=Adh80_;
	by pat_id; 
    var Adh_80; 
run;

Data Traj_80; 
merge month Adh80 included_ids (keep = pat_id death);
BY pat_id; 
Run;

****************Trajectory PDC 80**************************;

PROC TRAJ DATA= Traj_80 OUTPLOT=op_stat_ms OUTSTAT=os_stat_ms OUT=of_stat_ms OUTEST=oe_stat_ms ci95m;
	ID pat_id;
	VAR Adh80_1-Adh80_12;
	INDEP month_1-month_12;
	MODEL logit;
	ORDER 2 2 2;
	RUN;

%trajplot(op_stat_ms,os_stat_ms,"Adherence to cART - PDC 80%","Trajectory model","Percent","Month");

*2 2 2  BIC= -7464.00 (N=23893)
* 3 3 3 -7399.50
* 3 2 1  -7411.39
* 3 2 2   -7436.2
-7453.5;

proc freq data = up.HIV_prevalent_traj_2016_2017_3;
table age;
run;

proc means data = up.HIV_prevalent_traj_2016_2017_3 p50 p25 p75;
var p_pdc;
class group80;
run;




