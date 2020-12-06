FILENAME REFFILE '/folders/myfolders/AVD_AIVD_FLAN/FLAN.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=WORK.IMPORT2;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.IMPORT2; RUN;

DATA DADOS1;
    SET WORK.IMPORT2;
    KEEP FLAN_cod SEXO2 HIPERTENSAO2 DIABETE2 ESTADO_CIVIL_COD2 IMC_COD2 IDADE_COD2 RENDA_COD2 ESCOLARIDADE_COD2;
    IF (REC2=1) THEN OUTPUT;
RUN;

PROC FREQ DATA=dados1;
RUN;

proc freq data=DADOS1;
tables flan_cod*sexo2 / chisq;
run;
proc freq data=DADOS1;
tables flan_cod*hipertensao2 / chisq;
run;
proc freq data=DADOS1;
tables flan_cod*diabete2 / chisq;
run;
proc freq data=DADOS1;
tables flan_cod*estado_civil_cod2 / chisq;
run;
proc freq data=DADOS1;
tables flan_cod*imc_cod2 / chisq;
run;
proc freq data=DADOS1;
tables flan_cod*idade_cod2 / chisq;
run;
proc freq data=DADOS1;
tables flan_cod*renda_cod2 / chisq;
run;
proc freq data=DADOS1;
tables flan_cod*escolaridade_cod2 / chisq;
run;
DATA DADOS3;
    SET WORK.IMPORT2;
    KEEP FLAN_COD IDADE2 RENDA2 IMC2;
    IF (REC2=1) THEN OUTPUT;
RUN;
proc sgplot data=DADOS3;
vbox IDADE2/ category=flan_cod;
 run;
proc sgplot data=DADOS3;
vbox IMC2/ category=flan_cod;
 run;
proc sgplot data=DADOS3;
vbox RENDA2/ category=flan_cod;
 run;