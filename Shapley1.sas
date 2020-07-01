LIBNAME in "C:\Users\Dan Vu\Desktop\Shapley value\data";
PROC IMPORT OUT=in.data
	    DATAFILE= "C:\Users\Dan Vu\Desktop\Shapley value\data\org2.csv" DBMS=csv Replace;
		DELIMITER=",";
		GETNAMES=YES;
RUN;

%let rep_data_in = C:\Users\Dan Vu\Desktop\Shapley value\data;
libname in "&rep_data_in.";
%let indepvar = q1a q1b q1c q2 q3a q3b q3c q4a q4b; /*Independent Variables*/
%let depvar = Satisfaction; /*dependent Variable*/
%let Size = 150; /*Orignal size = 553 obs*/
%let NumSamples = 100;       
%let num = 1;


proc surveyselect data=org2 NOPRINT seed=1
     out=BootCases(rename=(Replicate=SampleID))
     method=urs              /* resample with replacement */
     sampsize=&Size
     reps=&NumSamples;       /* generate NumSamples bootstrap resamples */
run;

DATA BootcasesCV;
SET BootCases;
KEEP &indepvar.;
RUN;


ods html file = '/app/OFFICE/COMPORTEMENT/Dan/exportdata/553_2.html' ;

PROC MEANS data=BootcasesCV mean std cv;
ods output summary = Cof;
RUN;


DATA CofV;
SET Cof (KEEP=_NUMERIC_);
ARRAY VAR(*) _NUMERIC_;
MED= MEDIAN(of VAR(*));
RUN;

PROC PRINT DATA = CofV LABEL;
VAR MED;
LABEL MED = "MEDIAN OF COEFFICIENTS";
RUN;

%macro BC(sample = &num);
DATA BootCasesn&num.;
SET BootCases (WHERE = (SampleID = &num.));
RUN;
%mend;
%BC(sample = &num);

%macro Shapley;
/*Count No of Independent Variable*/
%local nb word;
%let nb=1;
%let word=%qscan(&indepvar,&nb,%str( ));
%do %while(&word ne);
%let nb=%eval(&nb+1);
%let word=%qscan(&indepvar,&nb,%str( ));
%end;
%let nb=%eval(&nb-1);
/*%put &=nvar.;
%put &=word.;*/


DATA _temp_;
SET BootCasesn&num;
	KEEP &indepvar &depvar;
RUN;

/*Rename Variables*/
DATA _temp_;
SET  _temp_;
RENAME
%do i=1 %to &nb;
%let var&i=%sysfunc(scan(&indepvar,&i,' '));
&&var&i =x&i %end ;;
RUN;

/*Reg with selection=rsquare*/
ods graphics on;
PROC REG DATA=_temp_ corr all;
	ods select  Corr NObs ANOVA ParameterEstimates SubsetSelSummary CollinDiag;
	ods output SubsetSelSummary=RsquareValues Corr=Corr;
	MODEL &depvar=x1-x&nb. / tol vif collin SELECTION=RSQUARE;
RUN;
ods graphics off;

DATA Corr (RENAME=(Satisfaction = Ryj));
SET Corr;
	KEEP Variable Satisfaction;
	IF Variable = "Satisfaction" then delete;
RUN;

DATA RsquareValues;
SET RsquareValues;
	DROP Dependent Model control Modelindex;
RUN;

DATA Null_Model;
	NumInModel =0;
	Rsquare=0;
	VarsInModel='x0';
RUN;

PROC APPEND BASE=RsquareValues DATA=null_model;
RUN;

PROC SORT DATA=RsquareValues;
	BY NumInModel VarsInModel;
RUN;


/*Shapley Value computation*/
%do i=1 %to &nb;
 DATA R2_with_&i.( RENAME=(RSquare=Rsquare_with VarsInModel=x_with))
      R2_without_&i. (DROP =NumInModel RENAME=(RSquare=Rsquare_without VarsInModel=x_without));
	 SET RsquareValues;
	 IF FIND(VarsInModel,"x&i") GT 0 THEN OUTPUT R2_with_&i.;
	 ELSE OUTPUT R2_without_&i.;
 RUN;

 DATA R2_delta_&i.;
 SET  R2_with_&i.;
 SET  R2_without_&i. ;
 RUN;
 DATA R2_delta_&i.;
 SET  R2_delta_&i.;
 	KEEP x&i. NumInModel;
 	x&i.=Rsquare_with-Rsquare_without;
 RUN;
%end;

 DATA R2_delta;
 	MERGE %do i=1 %to &nb; 
	R2_delta_&i. %end; ;
 RENAME
 %do i=1 %to &nb;
 %let var&i=%sysfunc(SCAN(&indepvar,&i,' '));
 	x&i=&&var&i  %end ; ;
 RUN;

PROC IML;
 USE R2_delta;
 READ ALL VAR {&indepvar} INTO indvar;
 READ ALL VAR {NumInModel} INTO NIM;
 p=ncol(indvar); /*p = n*/
 q=nrow(indvar);
 sv=j(q,p,0);
 do j=1 to p;
   do i=1 to q;
  	do m=1 to NIM[i,];
     	gamma=0;
     	w=fact(NIM[i,]-1)#(fact(p-(NIM[i,]-1)-1)); /*# = multiplication */
	 											/*fact(NIM[i,]-1) = k!
	 											p = n*/
  	w=w/fact(p);
     	gamma=gamma+w; /*ve dau*/
  	sv[i,j]=gamma#indvar[i,j]; /*delta la ve sau*/
   	end;
   end;
 end;
sv=SV[+,]`; /*[+,] = reduction des lignes par la somme; ` = transpose*/
RSquare=sv[+,];
R2Share = sv/RSquare*100;
Question={&indepvar};
Cname={SV Var};
Share={"Share(%)"};
Total = "100%";
PRINT   SV [rowname=Question colname=Cname] ;
PRINT RSquare ;
PRINT   R2Share [rowname=Question colname=Share] ;
PRINT Total;
CREATE SV var {Question sv};
APPEND; 
CLOSE SV;
QUIT;

PROC DATASETS lib=work
nolist kill;
QUIT;
RUN;

%MEND Shapley;
%Shapley;

ods html close ;
