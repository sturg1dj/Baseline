/*LIBNAME AB "F:\Datasets\HCUP\California SID 2007_2011\2010";*/
/**/
/**/
/*PROC SURVEYSELECT DATA=AB.CA_SID_2010_ALL N=5000 out=Sample;RUN;*/
/**/
/**/
/*DATA SAMPLE;*/
/*SET SAMPLE;*/
/**/
/*	HOSP = HOSPID;*/
/*	PRIMARYDX = DX1;*/
/*	IF FEMALE = 1 THEN SEX = 'F';*/
/*	ELSE IF FEMALE = 0 THEN SEX = 'M';*/
/**/
/**/
/*KEEP KEY HOSP PRIMARYDX SEX LOS PAY1 PL_CBSA RACE RACE_X CM_DM AGE HOMELESS;*/
/*RUN;*/
/**/
/*PROC FREQ DATA=SAMPLE;*/
/*TABLES HOSP PRIMARYDX SEX PAY1 PL_CBSA RACE RACE_X CM_DM HOMELESS;*/
/*RUN;*/


%MACRO BASELINE(DATA=,
                VARS=,
			   GROUP=,
              LEVELS=,
                 OUT=,
         OUTLocation=,
			 OUTType=,
			    MODE=);
OPTIONS MLOGIC MPRINT;

PROC SQL noprint;
SELECT DISTINCT
       &GROUP,
	   COUNT(DISTINCT &GROUP)
  INTO :GRP_LEV SEPARATED BY ' ',
       :GRP_LEV_CNT 
  FROM &DATA
 WHERE &GROUP NE '';
QUIT;

DATA PARSE;
LENGTH STRING J $1000. TEST $5. LEVELS 8 TYPE ORDER 3;
STRING = "&VARS";

I=1;
DO UNTIL(J = ' ');
	J = SCAN(STRING,I,')');
	VAR = COMPRESS(SCAN(J,1,'('));
	IF FIND(UPCASE(J),'TEST') NE 0 THEN TEST = COMPRESS(SCAN(SUBSTR(UPCASE(J),FIND(UPCASE(J),'TEST')+5,10),1,' '));ELSE TEST = '';
	IF FIND(UPCASE(J),'TYPE') NE 0 THEN TYPE = INPUT(COMPRESS(SCAN(SUBSTR(UPCASE(J),FIND(UPCASE(J),'TYPE')+5,10),1,' ')),3.);ELSE TYPE = '';
	IF FIND(UPCASE(J),'LEVELS') NE 0 THEN LEVELS = INPUT(COMPRESS(SCAN(SUBSTR(UPCASE(J),FIND(UPCASE(J),'LEVELS')+7,10),1,' ')),8.);ELSE LEVELS = .;
	IF FIND(UPCASE(J),'ORDER') NE 0 THEN ORDER = INPUT(COMPRESS(SCAN(SUBSTR(UPCASE(J),FIND(UPCASE(J),'ORDER')+6,10),1,' ')),3.);ELSE ORDER = .;

	OUTPUT;
	I + 1;
END;

RUN;

PROC SQL noprint;
SELECT VAR
  INTO :VARS SEPARATED BY ' '
  FROM PARSE
 WHERE VAR NE '';
QUIT;

%LET VARNUM = %SYSFUNC(COUNTW("&VARS"));
%PUT &VARNUM;

/*Grab Some metadata*/
PROC CONTENTS DATA=&DATA out=OUTCONTENT noprint;
RUN;

PROC SQL;
CREATE TABLE OUTCONTENT AS
SELECT A.NAME,
       A.LENGTH,
	   A.LABEL,
	   A.TYPE,
	   B.TYPE AS TYPE_FL
  FROM OUTCONTENT A,
       PARSE B
 WHERE UPCASE(A.NAME) = UPCASE(B.VAR);
 QUIT;




/*Creating datasets for later*/
DATA _VARS;
LENGTH NAME $255.;
SET _NULL_;
RUN;

DATA CAT;
LENGTH NAME $255.;
SET _NULL_;
RUN;

DATA CONT;
LENGTH NAME $255.;
SET _NULL_;
RUN;


/*Counting the number of variables of interest*/
%LET CNT1 = %SYSFUNC(countw(&VARS,' '));


/*Setting up the do loop through the variable list*/
%DO I = 1 %TO &CNT1;
	%LET VAR = %SCAN(&VARS.,&I.,' ');

/*Getting count of levels to determine type of numeric*/
	
PROC SQL;
CREATE TABLE _A AS
SELECT DISTINCT
       "&VAR" AS NAME,
       COUNT(DISTINCT &VAR) as LEVELS
  FROM &DATA;
QUIT;

/*Table where we keep variable information regarding levels*/
DATA _VARS;
SET _VARS _A;
RUN;
%END;

/*Combining level counts with type for the categoricals that have many levels*/
PROC SQL;
CREATE TABLE _VARS1 AS
SELECT A.NAME,
       A.LEVELS,
	   B.TYPE,
	   B.TYPE_FL
  FROM _VARS A,
       OUTCONTENT B
 WHERE UPCASE(A.NAME) = UPCASE(B.NAME);
QUIT;

/*If the number of levels < than the minimum we defined  OR the type is string then they are lumped into categorical*/
DATA CONT CAT;
SET _VARS1;
	IF LEVELS < &LEVELS OR TYPE = 2 OR TYPE_FL = 2 THEN OUTPUT CAT;
	ELSE OUTPUT CONT;
RUN;

PROC SQL noprint;
SELECT COUNT(*)
  INTO :CATROW
  FROM CAT;
SELECT COUNT(*)
  INTO :CONTROW
  FROM CONT;
QUIT;

/*Puts the continuous in their own variable, we are done with those for now*/

%IF &CONTROW GE 1 %THEN %DO;

PROC SQL noprint;
SELECT NAME
  INTO :CONTINUOUS SEPARATED BY " "
 FROM CONT;
QUIT;

%END;

%IF &CONTROW GE 1 %THEN %DO;

/*Two categories: those categoricals that are numeric and those that are not.*/
PROC SQL noprint;
SELECT NAME
  INTO :Categorical1 SEPARATED BY " "
 FROM CAT
WHERE TYPE = 1;
QUIT;

PROC SQL noprint;
SELECT NAME
  INTO :Categorical2 SEPARATED BY " "
 FROM CAT
WHERE TYPE = 2;
QUIT;

/*Counts for both of those groups*/
PROC SQL noprint;
SELECT COUNT(*)
  INTO :CNT2
  FROM CAT
WHERE TYPE = 1;
QUIT;

PROC SQL noprint;
SELECT COUNT(*)
  INTO :CNT3
  FROM CAT
WHERE TYPE = 2;
QUIT;

/*First doing the string categoricals*/

%IF &CNT3 GE 1 %THEN %DO;

DATA CAT2;
SET &DATA;
LENGTH LEVEL VARIABLE $32.;
ARRAY CATVAR[&CNT3] &CATEGORICAL2;
DO J = 1 TO &CNT3;
	VARIABLE = SCAN("&Categorical2",J,' ');
	LEVEL = CATVAR[j];
	OUTPUT;
END;

KEEP VARIABLE LEVEL;
RUN;

%END;

%ELSE %DO;
DATA CAT2;
SET _NULL_;
RUN;
%END;

%IF &CNT2 GE 1 %THEN %DO;
/*Now doing numeric, need the type to match and the arrays cannot handle different types*/
DATA CAT3;
SET &DATA;
LENGTH LEVEL VARIABLE $32.;
ARRAY CATVAR[&CNT2] &CATEGORICAL1;
DO J = 1 TO &CNT2;
	VARIABLE = SCAN("&Categorical1",J,' ');
	LEVEL = COMPRESS(INPUT(CATVAR[j],$255.));
	LEVRAW = CATVAR[j];
	OUTPUT;
END;

KEEP VARIABLE LEVEL LEVRAW;
RUN;

%END;

%ELSE %DO;
DATA CAT3;
SET _NULL_; 
RUN;
%END;

DATA CATCOMB;
SET CAT2 CAT3;
IF LEVRAW = . THEN LEVRAW = 0;
RUN;

/*Combining the results and defining the variable references*/
PROC SQL noprint;
CREATE TABLE CAT_ALL AS
SELECT DISTINCT 
       VARIABLE,
	   LEVEL,
	   LEVRAW,
	   COMPRESS(VARIABLE||"(REF='"||LEVEL||"')") AS CLASS /*<-----Creates the class variable reference*/
  FROM CATCOMB
WHERE LEVEL NE ''
GROUP BY VARIABLE
ORDER BY 1,3,2;
QUIT;

DATA CAT_ALL;
SET CAT_ALL;
BY VARIABLE;
	IF FIRST.VARIABLE NE 1 THEN DELETE;
RUN;

%END;


%GLOBAL MODEL CLASS; /*These are the macro variables we will use in our model*/

PROC SQL noprint;
SELECT VARIABLE,
       CLASS
  INTO :VARIABLE separated by ' ',
       :class separated by  ' '
  FROM CAT_ALL;
QUIT;

%LET MODEL = &VARIABLE &CONTINUOUS;

/*Delete all tables except the working table we are using*/
/*PROC DATASETS noprint;*/
/*DELETE CAT CAT2 CAT3 CAT_ALL _VARS _VARS1 CAT CONT OUTCONTENT _A;*/
/*RUN;*/

/**************************************
* Testing Time
**************************************/
PROC SQL;
CREATE TABLE CONT_TESTS AS
SELECT A.VAR,
       A.Test
  FROM PARSE A,
       CONT B
  WHERE upcase(A.VAR) = upcase(B.NAME);
QUIT;

/*****************************
* Determine Which Test To
* Perform
*****************************/

DATA C;
SET CONT_TESTS;
IF TEST = 'C' OR TEST = '';
RUN;

/*Pull out variable names, and labeled variables*/
PROC SQL noprint;
SELECT VAR,
       COMPRESS('SKEW_'||VAR),
	   COMPRESS('KURT_'||VAR),
	   COUNT(*) 
  INTO :UNI SEPARATED BY ' ',
       :SKEW Separated BY ' ',
	   :KURT SEPARATED BY ' ',
	   :CNT_VAR
  FROM C;
QUIT;

/*PROC UNIVARIATE TO DETERMINE SKEWNES AND KURTOSIS*/
PROC UNIVARIATE DATA=&DATA NORMAL plot noprint;
	VAR &UNI;
	OUTPUT OUT=OUTUNI KURT=&KURT SKEW=&SKEW;
RUN;

PROC TRANSPOSE DATA=OUTUNI OUT=TRNS;
RUN;

/*Make Table Usable*/
PROC SQL;
CREATE TABLE NORMAL AS
SELECT DISTINCT
       COMPRESS(SCAN(_LABEL_,2,',')) AS VAR,
       MAX(CASE WHEN COMPRESS(SCAN(_LABEL_,1,',')) = 'skewness' THEN COL1 ELSE . END) AS SKEWNESS,
	   MAX(CASE WHEN COMPRESS(SCAN(_LABEL_,1,',')) = 'kurtosis' THEN COL1 ELSE . END) AS KURTOSIS
  FROM TRNS
GROUP BY COMPRESS(SCAN(_LABEL_,2,','));
QUIT;

/*Using the -2 to 2 range to determind normality*/
DATA NORMAL;
SET NORMAL;
	IF -2 LT SKEWNESS LT 2 THEN TEST='T';
	ELSE TEST = 'W';
RUN;

/*Return results back to set with test determination*/
PROC SQL noprint;
CREATE TABLE CONT_TESTS AS
SELECT A.VAR,
       CASE WHEN B.TEST = '' THEN A.TEST ELSE B.TEST END AS TEST
  FROM CONT_TESTS A
LEFT OUTER JOIN NORMAL B ON A.VAR = B.VAR;
QUIT;

/*Separate those out between T dist and Non-parametric*/
DATA W T;
SET CONT_TESTS;
IF TEST = 'T' THEN OUTPUT T;
ELSE OUTPUT W;
RUN;

/*Pulling more variables and labeled variables*/

PROC SQL noprint;
SELECT COUNT(*)
  INTO :WCNT
  FROM W;
QUIT;

%IF &WCNT GT 0 %THEN %DO;


PROC SQL noprint;
SELECT VAR,
       COMPRESS('MED_'||VAR),
	   COMPRESS('IQR_'||VAR),
	   COMPRESS('NMISS_'||VAR)
  INTO :WILCOX separated by ' ',
       :MEDIAN separated by ' ',
	   :IQR separated by ' ',
	   :NMISS_A separated by ' '
  FROM W;
QUIT;

/*Median and IQR*/
PROC MEANS DATA=&DATA noprint;
VAR &WILCOX;
CLASS &GROUP;
OUTPUT OUT=MED_OUT MEDIAN(&WILCOX)=&MEDIAN QRANGE(&WILCOX)=&IQR nmiss(&WILCOX)=&NMISS_A;
RUN;

/*Statistical Test*/
PROC NPAR1WAY DATA=&DATA wilcoxon noprint;
CLASS &GROUP;
VAR &WILCOX;
OUTPUT OUT=WIL(KEEP=_VAR_ P_KW) WILCOXON;
RUN; 

/*A for All*/
DATA MED_OUT;
SET MED_OUT;

	IF &GROUP = '' THEN &GROUP = 'A';
RUN;

/*Next few parts is just transposing and combining*/

PROC TRANSPOSE DATA=MED_OUT OUT=TRNS_DAT;
ID &GROUP;
RUN;

DATA TRNS_DAT;
SET TRNS_DAT;
	VAR = SCAN(_NAME_,2,'_');
	STAT = SCAN(_NAME_,1,'_');
RUN;

PROC SQL;
CREATE TABLE WILCOX_OUT AS
SELECT A.VAR,
       COMPRESS(PUT(A.A,comma20.))||'('||COMPRESS(PUT(B.A,comma20.))||')' AS All,
       %DO Z = 1 %TO &GRP_LEV_CNT;
	   COMPRESS(PUT(A.%SCAN(&GRP_LEV,&Z,' '),comma20.))||'('||COMPRESS(PUT(B.%SCAN(&GRP_LEV,&Z,' '),comma20.))||')' AS %SCAN(&GRP_LEV,&Z,' '),
	   %END;
	   "Median (IQR)" AS LABEL,
	   P_KW AS pValue format=pvalue7.4,
	   'Wilcoxon' AS Test,
	   D.A AS NMISS
  FROM TRNS_DAT A,
       TRNS_DAT B,
	   TRNS_DAT D,
	   WIL C
 WHERE A.VAR = B.VAR = C._VAR_
   AND A.STAT='IQR'
   AND B.STAT='MED'
   AND D.STAT='NMISS';
QUIT;	

%END;

%IF &WCNT = 0 %THEN %DO;

DATA WILCOX_OUT;
SET _NULL_;
RUN;

%END;
/*End of Wilcoxon Section*/

/*Start Ttest/ANOVA section*/

PROC SQL noprint;
SELECT COUNT(*)
  INTO :TCNT
  FROM T;
QUIT;

%IF &TCNT GT 0 %THEN %DO;

PROC SQL noprint;
SELECT VAR,
       COUNT(*)
  INTO :VART separated by ' ',
       :tcnt
  FROM t
 WHERE VAR NE ' ';
QUIT;

DATA TOUT_P;
SET _NULL_;
RUN;

%DO Y = 1 %TO &TCNT;



	PROC GLM DATA=&DATA OUTSTAT=TOUT(WHERE=(_TYPE_ = 'SS3')) noprint;
		CLASS &GROUP;
		MODEL %SCAN(&VART.,&Y.,' ') = &GROUP;
	RUN;
	QUIT;

	DATA TOUT_P;
	SET TOUT_P TOUT;
	RUN;

PROC SQL noprint ;
SELECT VAR,
       COMPRESS('MEAN_'||VAR),
	   COMPRESS('STD_'||VAR),
	   COMPRESS('NMISS_'||VAR)
  INTO :TTEST separated by ' ',
       :MEAN separated by ' ',
	   :STD separated by ' ',
	   :NMISS_2 separated by ' '
  FROM T;
QUIT;

/*Mean and STD*/
PROC MEANS DATA=&DATA noprint;
	VAR &TTEST;
	CLASS &GROUP;
	OUTPUT OUT=MEAN_OUT MEAN(&TTEST)=&MEAN STD(&TTEST)=&STD NMISS(&TTEST)=&NMISS_2;
RUN;

DATA MEAN_OUT;
SET MEAN_OUT;

	IF &GROUP = '' THEN &GROUP = 'A';
RUN;

/*Next few parts is just transposing and combining*/

PROC TRANSPOSE DATA=MEAN_OUT OUT=TRNS_DAT2;
ID &GROUP;
RUN;

DATA TRNS_DAT2;
SET TRNS_DAT2;
	VAR = SCAN(_NAME_,2,'_');
	STAT = SCAN(_NAME_,1,'_');
RUN;

PROC SQL;
CREATE TABLE TTEST_OUT AS
SELECT A.VAR,
       COMPRESS(PUT(A.A,comma20.2))||'('||COMPRESS(PUT(B.A,comma20.2))||')' AS All,
       %DO Z = 1 %TO &GRP_LEV_CNT;
	   COMPRESS(PUT(A.%SCAN(&GRP_LEV,&Z,' '),comma20.2))||'('||COMPRESS(PUT(B.%SCAN(&GRP_LEV,&Z,' '),comma20.2))||')' AS %SCAN(&GRP_LEV,&Z,' '),
	   %END;
	   "Mean (STD)" AS LABEL,
	   PROB AS pValue format=pvalue7.4,
	   CASE
		WHEN &GRP_LEV_CNT GT 2 THEN 'ANOVA' ELSE 'TTest' END AS Test,
		D.A AS NMISS
  FROM TRNS_DAT2 A,
       TRNS_DAT2 B,
	   TRNS_DAT2 D,
	   tout_p C
 WHERE A.VAR = B.VAR = C._NAME_ = D.VAR
   AND A.STAT='STD'
   AND B.STAT='MEAN'
   AND D.STAT = 'NMISS';
QUIT;

%END;

%IF &TCNT = 0 %THEN %DO;

DATA TTEST_OUT;
SET _NULL_;
RUN;

%END;

DATA OUT_CONT;
LENTGH VAR LABEL ALL &&GRP_LEV TEST $100.;
SET TTEST_OUT WILCOX_OUT;

	LEVELS = VAR;

RUN;
%END;

PROC SQL noprint;
SELECT NAME,
       COUNT(DISTINCT NAME)
  INTO :CATVAR separated by ' ',
       :CATCNT
  FROM CAT;
QUIT;

DATA CAT_COMBINE;
SET _NULL_;
RUN;


%DO I = 1 %TO &CATCNT;

%LET NEWVAR = %SCAN(&CATVAR,&I,' ');

PROC FREQ DATA=&DATA noprint;
TABLES &NEWVAR &NEWVAR*&GROUP / nopercent norow chisq fisher out=FREQOUT OUTPCT ;
OUTPUT OUT=TESTSCAT CHISQ FISHER;
RUN;


PROC FREQ DATA=&DATA noprint;
TABLES &NEWVAR / nopercent norow chisq fisher out=FRETOT;
OUTPUT OUT=MISSING NMISS;
RUN;

DATA FRETOT;
SET FRETOT;
&GROUP = 'A';
PCT_COL = Percent;
RUN;

DATA MISSING;
SET MISSING;
VAR = "&NEWVAR";
RUN;

DATA FREQOUT;
SET FREQOUT FRETOT;

TOTALS = COMPRESS(PUT(COUNT,comma10.))||'('||COMPRESS(PUT(PCT_COL,6.1))||')';

RUN;

DATA TESTSCAT;
SET TESTSCAT;
	VAR = "&NEWVAR";
	FISHER = XP2_FISH;
	CHISQ = P_PCHI;
	FORMAT FISHER CHISQ PVALUE6.4;
KEEP FISHER CHISQ VAR;
RUN;

PROC SORT DATA=FREQOUT;
	BY &NEWVAR;
RUN;

PROC TRANSPOSE DATA=FREQOUT OUT=_TRNSCAT;*(WHERE=(_NAME_='TOTALS'));
	BY &NEWVAR;
	VAR TOTALS;
	ID SEX;
RUN;

DATA _TRNSCAT;
LENGTH VAR $32.;
	SET _TRNSCAT;
	VAR = "&NEWVAR";
	LEVELS = &NEWVAR;
RUN;

PROC SQL;
CREATE TABLE _AAA AS
SELECT A.*,
       B.FISHER,
	   B.CHISQ,
	   C.NMiss,
	   D.TEST
  FROM _TRNSCAT(DROP=_NAME_ &NEWVAR) A,
       TESTSCAT B,
	   MISSING C,
	   PARSE D
 WHERE A.VAR = B.VAR = C.VAR = D.VAR
   AND A.LEVELS NE '';
QUIT;       
       
       
DATA CAT_COMBINE;
SET CAT_COMBINE _AAA;

RUN; 



%END;

DATA CAT_COMBINE;
LENGTH TEST $32.;
SET CAT_COMBINE;

LABEL = 'Total(%)';

IF UPCASE(TEST) = 'F' AND FISHER NE . THEN DO;
	pValue = FISHER;
	Test = 'Fisher';
END;
ELSE IF UPCASE(TEST) = 'C' THEN DO;
	pValue = chisq;
	Test='Chi-Square';
END;

ELSE IF TEST NE . AND UPCASE(TEST) NOT IN ('C' 'F') THEN DO;
	PUT 'WARNING: Invalid test chosen. Will use Chi Square.';
	pValue = chisq;
	Test='Chi-Square';
END;
ELSE DO;
	pValue = chisq; 
	Test='Chi-Square';
END;

ALL=A;

DROP A FISHER CHISQ;
RUN;

PROC SQL noprint;
SELECT DISTINCT &GROUP,
       COUNT(DISTINCT &GROUP)
  INTO :GROUP_LEVS separated by  ' ',
       :GRP_LEV_CNT
  FROM &DATA
 WHERE &GROUP NE ''
ORDER BY 2;
QUIT;

DATA ALL_COMBINE;
LENGTH VAR LEVELS LABEL ALL &GROUP_LEVS TEST $45. PVALUE  NMISS 8;
SET CAT_COMBINE out_cont;

RUN;

PROC SQL;
CREATE TABLE RPT AS
SELECT A.*,
       B.ORDER,
	   CATX('|',A.VAR,COMPRESS(PUT(PVALUE,10.5)),NMISS,A.TEST) AS KEY
  FROM ALL_COMBINE A,
       PARSE B
 WHERE A.VAR = B.VAR;
QUIT;



PROC REPORT DATA=RPT out=TST;
COLUMNS KEY LEVELS LABEL ALL
%DO K = 1 %TO &GRP_LEV_CNT;
	%SCAN(&GROUP_LEVS,&K.,' ') 
%END; PValue Test NMISS;
	DEFINE KEY / GROUP NOPRINT;
	DEFINE LEVELS / DISPLAY;
	DEFINE LABEL / DISPLAY;
	DEFINE ALL / 'All' DISPLAY;

%DO L = 1 %TO &GRP_LEV_CNT;
	DEFINE %SCAN(&GROUP_LEVS,&L.,' ') / DISPLAY; 
%END;
	DEFINE PValue / 'pValue' COMPUTED;
	DEFINE NMISS / 'Missing' COMPUTED;
	BREAK BEFORE KEY / SKIP;
	COMPUTE BEFORE KEY;
		IF _BREAK_ = 'KEY' THEN DO;
	
			LEVELS = SCAN(KEY,1,'|');
			PValue = INPUT(SCAN(KEY,2,'|'),10.5);
			Test = SCAN(KEY,4,'|');
			NMISS = INPUT(SCAN(KEY,3,'|'),8.);
		END;

	ENDCOMP;
	
		
RUN;

DATA TST2;
SET TST;
IF _BREAK_ NE 'KEY' THEN DO;
	PVALUE = .;
	TEST = ' ';
	NMISS = .;
END;

"P-Value"n = COMPRESS(PUT(PVALUE,pvalue6.4));
"Test Performed"n = Test;
Missing = COMPRESS(PUT(NMISS,8.));

IF "P-Value"n = '.' THEN "P-Value"n = ' ';
IF Missing = '.' THEN Missing = ' '; 

DROP KEY _BREAK_ PVALUE TEST NMISS;
RUN;

OPTIONS orientation=landscape nodate nonumber;
ODS EXCEL FILE='F:\CSPH_Projects\IRBNotApplicable_NonResearchActivities\Prjct_DanSturgeon_NonResearch\WorkFldr_DSturgeon(DS52)\TESTING.XLSX' style=seaside;
PROC PRINT DATA=TST2 noobs;
RUN;
ODS EXCEL CLOSE;

%MEND;

/*%BASELINE(DATA=CHK5,*/
/*                VARS=RACE2(LEVELS=3) AGE(TYPE=1 TEST=C ORDER=3) Insurance(TEST=C ORDER = 4) LOS(TEST=C ORDER=6),*/
/*			   GROUP=SEX,*/
/*              LEVELS=10,*/
/*                 OUT=,*/
/*         OUTLocation=,*/
/*			 OUTType=,*/
/*			    MODE=);*/

/*DATA CHK5;*/
/*LENGTH RACE2 Insurance $32.;*/
/*SET SAMPLE;*/
/*IF RACE LE 3;*/
/**/
/*IF RACE = 1 THEN RACE2 = 'White';*/
/*ELSE IF RACE = 2 THEN RACE2 = 'Black';*/
/*ELSE IF RACE = 3 THEN RACE2 = 'Hispanic';*/
/*ELSE DELETE;*/
/**/
/*IF PAY1 = 1 THEN INSURANCE = 'Private';*/
/*ELSE IF PAY1 = 2 THEN INSURANCE = 'Medicare';*/
/*ELSE IF PAY1 = 3 THEN INSURANCE = 'Medicaid';*/
/*ELSE INSURANCE = 'Other';*/
/**/
/*IF KEY = '000006201010014923' THEN RACE2 = '';*/
/**/
/*RUN;*/
/**/
/*PROC MEANS DATA=CHK5 nmiss;*/
/*VAR INSURANCE RACE2;*/
/*OUTPUT OUT=chk3 NMISS=nmiss;*/
/*RUN;*/
/**/
/*PROC CONTENTS DATA=CHK5 out=zzz;RUN;*/
