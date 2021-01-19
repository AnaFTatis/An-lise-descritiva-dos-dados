#Determinacao da qualidade de vida dos idosos segundo a escala FLANGAN 

rm(list=ls(all=T))

library(readxl)
dados_flan = "FLAN.xlsx"
dados2 = read_xlsx(path = dados_flan, sheet=1)
dados2 = as.data.frame(dados2)
attach(dados2)

#Variaveis qualitativas
#Tabelas de frequencia
library(expss)

sexo2<- subset(x=dados2, subset = rec2 == 1, select = "Sexo2"); attach(sexo2)
fre(sexo2)

escolaridade2<- subset(x=dados2, subset = rec2 == 1, select = "Escolaridade2" ); attach(escolaridade2)
fre(escolaridade2)

escolaridade_cod2<- subset(x=dados2, subset = rec2 == 1, select = "Escolaridade_cod2" ); attach(escolaridade_cod2)
fre(escolaridade_cod2)

estado_civil2<- subset(x=dados2, subset = rec2 == 1, select = "Estado_civil2" ); attach(estado_civil2)

fre(estado_civil2)

estado_civil_cod2<- subset(x=dados2, subset = rec2 == 1, select = "Estado_civil_cod2" ); attach(estado_civil_cod2)
fre(na.omit(estado_civil_cod2))

trabalha2<- subset(x=dados2, subset = rec2 == 1, select = "Trabalha2" ); attach(trabalha2)
fre(trabalha2)

aposentado2<- subset(x=dados2, subset = rec2 == 1, select = "Aposentado2"); attach(aposentado2) 
fre(aposentado2)

hipertensao2<- subset(x=dados2, subset = rec2 == 1, select = "Hipertensao2"); attach(hipertensao2)
fre(hipertensao2)

diabete_mellitus2<- subset(x=dados2, subset = rec2 == 1, select = "Diabete2"); attach(diabete_mellitus2) 
fre(diabete_mellitus2)

doenca_cardiaca2<- subset(x=dados2, subset = rec2 == 1, select = "Cardiaca2"); attach(doenca_cardiaca2) 
fre(doenca_cardiaca2)

disfuncao_tireoide2<- subset(x=dados2, subset = rec2 == 1, select = "Tireoide2"); attach(disfuncao_tireoide2)
fre(disfuncao_tireoide2)

colesterol_alto2<- subset(x=dados2, subset = rec2 == 1, select = "Colesterol2"); attach(colesterol_alto2)
fre(colesterol_alto2)

osteoporose2<- subset(x=dados2, subset = rec2 == 1, select = "Osteoporose2"); attach(osteoporose2)
fre(osteoporose2)

depressao2<- subset(x=dados2, subset = rec2 == 1, select = "Depressao2"); attach(depressao2)
fre(depressao2)

alzheimer2<- subset(x=dados2, subset = rec2 == 1, select = "Alzheimer2"); attach(alzheimer2)
fre(alzheimer2)

#FLAN - 70 parcelas vazias 

flan<- subset(x=dados2, subset = rec2 == 1, select = "FLAN"); attach(flan)
median(FLAN)
summary(FLAN)
corte <- cut(dados2$FLAN, c(0, 85.5, 110),
             labels = c("Abaixo de 85,5", "Igual ou maior que 85,5"))
summary(corte)
flan_cod<- subset(x=dados2,subset = rec2 == 1, select = "FLAN_cod");attach(flan_cod)
fre(FLAN_cod)

#Variaveis quantitativas continuas

imc2<- subset(x=dados2, subset = rec2 == 1, select = "IMC2"); attach(imc2)
imc_cod2<- subset(x=dados2, subset = rec2 == 1, select = "IMC_cod2"); attach(imc_cod2)
fre(IMC_cod2)

renda2<- subset(x=dados2, subset = rec2 == 1, select = "Renda2"); attach(renda2)
renda_cod2<- subset(x=dados2, subset = rec2 == 1, select = "Renda_cod2"); attach(renda_cod2)
fre(renda_cod2)

idade2<- subset(x=dados2, subset = rec2 == 1, select = "Idade2"); attach(idade2)
idade_cod2<- subset(x=dados2, subset = rec2 == 1, select = "Idade_cod2"); attach(idade_cod2)

#TABELAS DE DUPLA ENTRADA
library(gmodels)

CrossTable(FLAN_cod, Sexo2,prop.r=T, prop.c=T, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Escolaridade2,prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Escolaridade_cod2,prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(Estado_civil_cod2, FLAN_cod,prop.r=F, prop.c=T, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Trabalha2, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Aposentado2, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Hipertensao2, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Diabete2, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Osteoporose2, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Cardiaca2, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Tireoide2, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Depressao2, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Alzheimer2, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Colesterol2, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Idade_cod2, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, Renda_cod2, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(FLAN_cod, IMC_cod2, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

#Box-plots

boxplot(formula=IMC2~FLAN_cod, ylab="IMC", xlab="Qualidade de vida")
boxplot(formula=Idade2~FLAN_cod, ylab="Idade", xlab="Qualidade de vida")
boxplot(formula=Renda2~FLAN_cod, ylab="Renda", xlab="Qualidade de vida")

#Teste qui-quadrado de Pearson 

(freq.flan.sexo<-table(FLAN_cod,Sexo2));summary(freq.flan.sexo)

(freq.flan.escol<-table(FLAN_cod,Escolaridade_cod2));summary(freq.flan.escol)

(freq.flan.idade<-table(FLAN_cod,Idade_cod2));summary(freq.flan.idade)

(freq.flan.trab<-table(FLAN_cod,Trabalha2));summary(freq.flan.trab)

(freq.flan.apo<-table(FLAN_cod,Aposentado2));summary(freq.flan.apo)

(freq.flan.ren<-table(FLAN_cod,Renda_cod2));summary(freq.flan.ren)

(freq.flan.imc<-table(FLAN_cod,IMC_cod2));summary(freq.flan.imc)

(freq.flan.est<-table(FLAN_cod,Estado_civil_cod2));summary(freq.flan.est)

(freq.flan.hip<-table(FLAN_cod,Hipertensao2));summary(freq.flan.hip)

(freq.flan.dm<-table(FLAN_cod,Diabete2));summary(freq.flan.dm)

(freq.flan.ost<-table(FLAN_cod,Osteoporose2));summary(freq.flan.ost)

(freq.flan.dc<-table(FLAN_cod,Cardiaca2));summary(freq.flan.dc)

(freq.flan.al<-table(FLAN_cod,Alzheimer2));summary(freq.flan.al)
chisq.test(FLAN_cod, Alzheimer2, correct = T)

(freq.flan.dp<-table(FLAN_cod,Depressao2));summary(freq.flan.dp)
chisq.test(FLAN_cod, Depressao2, correct = T)

(freq.flan.ti<-table(FLAN_cod,Tireoide2));summary(freq.flan.ti)

(freq.flan.col<-table(FLAN_cod,Colesterol2));summary(freq.flan.col)

