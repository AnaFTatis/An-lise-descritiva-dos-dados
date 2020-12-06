#Determinacao de dependencia dos idosos segundo as escalas AVD 
#(atividades de vida diaria) e AIVD (atividades instrumentais de vida diaria).

rm(list=ls(all=T))

library(readxl)
dados_avd_aivd = "AVD_AIVD.xlsx"
dados = read_xlsx(path = dados_avd_aivd, sheet=1)
dados = as.data.frame(dados)
attach(dados)

#Variaveis qualitativas
#Tabelas de frequencia
library(expss)

sexo<- subset(x=dados, subset = rec == 1, select = "Sexo"); attach(sexo)
fre(sexo)

escolaridade<- subset(x=dados, subset = rec == 1, select = "Escolaridade" ); attach(escolaridade)
fre(escolaridade)

escolaridade_cod<- subset(x=dados, subset = rec == 1, select = "Escolaridade_cod" ); attach(escolaridade_cod)
fre(escolaridade_cod)

estado_civil<- subset(x=dados, subset = rec == 1, select = "Estado_civil" ); attach(estado_civil)
fre(estado_civil)

estado_civil_cod<- subset(x=dados, subset = rec == 1, select = "Estado_civil_cod" ); attach(estado_civil_cod)
fre(estado_civil_cod)

trabalha<- subset(x=dados, subset = rec == 1, select = "Trabalha" ); attach(trabalha)
fre(trabalha)

aposentado<- subset(x=dados, subset = rec == 1, select = "Aposentado"); attach(aposentado) 
fre(aposentado)

hipertensao<- subset(x=dados, subset = rec == 1, select = "Hipertensao"); attach(hipertensao)
fre(hipertensao)

diabete_mellitus<- subset(x=dados, subset = rec == 1, select = "Diabete"); attach(diabete_mellitus) 
fre(diabete_mellitus)

doenca_cardiaca<- subset(x=dados, subset = rec == 1, select = "Cardiaca"); attach(doenca_cardiaca) 
fre(doenca_cardiaca)

disfuncao_tireoide<- subset(x=dados, subset = rec == 1, select = "Tireoide"); attach(disfuncao_tireoide)
fre(disfuncao_tireoide)

colesterol_alto<- subset(x=dados, subset = rec == 1, select = "Colesterol"); attach(colesterol_alto)
fre(colesterol_alto)

osteoporose<- subset(x=dados, subset = rec == 1, select = "Osteoporose"); attach(osteoporose)
fre(osteoporose)

depressao<- subset(x=dados, subset = rec == 1, select = "Depressao"); attach(depressao)
fre(depressao)

alzheimer<- subset(x=dados, subset = rec == 1, select = "Alzheimer"); attach(alzheimer)
fre(alzheimer)

#AVD E AIVD - 79 parcelas vazias 

avd<- subset(x=dados, subset = rec == 1, select = "AVD"); attach(avd)
fre(avd)

aivd<- subset(x=dados, subset = rec == 1, select = "AIVD"); attach(aivd)
fre(aivd)

#Variaveis quantitativas continuas

imc<- subset(x=dados, subset = rec == 1, select = "IMC"); attach(imc)
imc_cod<- subset(x=dados, subset = rec == 1, select = "IMC_cod"); attach(imc_cod)
fre(imc_cod)

renda<- subset(x=dados, subset = rec == 1, select = "Renda"); attach(renda)
renda_cod<- subset(x=dados, subset = rec == 1, select = "Renda_cod"); attach(renda_cod)
fre(renda_cod)

idade<- subset(x=dados, subset = rec == 1, select = "Idade"); attach(idade)
idade_cod<- subset(x=dados, subset = rec == 1, select = "Idade_cod"); attach(idade_cod)
fre(idade_cod)

#TABELAS DE DUPLA ENTRADA
library(gmodels)

CrossTable(AVD, Sexo,prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Sexo,prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Escolaridade,prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Escolaridade,prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Escolaridade_cod,prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Escolaridade_cod,prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(Estado_civil, AVD,prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(Estado_civil, AIVD,prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Estado_civil_cod, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Estado_civil_cod, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Trabalha, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Trabalha, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Aposentado, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Aposentado, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Hipertensao, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Hipertensao, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Diabete, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Diabete, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Osteoporose, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Osteoporose, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Cardiaca, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Cardiaca, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Tireoide, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Tireoide, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Depressao, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Depressao, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Alzheimer, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Alzheimer, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Colesterol, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Colesterol, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Idade_cod, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Idade_cod, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, Renda_cod, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, Renda_cod, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

CrossTable(AVD, IMC_cod, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)
CrossTable(AIVD, IMC_cod, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

#Box-plots

boxplot(formula=IMC~AVD, ylab="IMC", xlab="Atividades de Vida Diária")
boxplot(formula=Idade~AVD, ylab="Idade", xlab="Atividades de Vida Diária")
boxplot(formula=Renda~AVD, ylab="Renda", xlab="Atividades de Vida Diária")
boxplot(formula=IMC~AIVD, ylab="IMC", xlab="Atividades Instrumentais de Vida Diária")
boxplot(formula=Idade~AIVD, ylab="Idade", xlab="Atividades de Vida Diária")
boxplot(formula=Renda~AIVD, ylab="Renda", xlab="Atividades de Vida Diária")

#Teste qui-quadrado de Pearson 

(freq.avd.sexo<-table(AVD,Sexo));summary(freq.avd.sexo)
chisq.test(AVD,Sexo, correct = F)
(freq.aivd.sexo<-table(AIVD,Sexo));summary(freq.aivd.sexo)

(freq.avd.escol<-table(AVD,Escolaridade_cod));summary(freq.avd.escol)
chisq.test(AVD, Escolaridade_cod, correct = T)
(freq.aivd.escol<-table(AIVD,Escolaridade_cod));summary(freq.aivd.escol)

(freq.avd.idade<-table(AVD,Idade_cod));summary(freq.avd.idade)
(freq.aivd.idade<-table(AIVD,Idade_cod));summary(freq.aivd.idade)

(freq.avd.trab<-table(AVD,Trabalha));summary(freq.avd.trab)
chisq.test(AVD,Trabalha, correct = T)
(freq.aivd.trab<-table(AIVD,Trabalha));summary(freq.aivd.trab)
chisq.test(AIVD,Trabalha, correct = T)

(freq.avd.apo<-table(AVD,Aposentado));summary(freq.avd.apo)
chisq.test(AVD, Aposentado, correct = T)
(freq.aivd.apo<-table(AIVD,Aposentado));summary(freq.aivd.apo)
chisq.test(AIVD, Aposentado, correct = T)

(freq.avd.ren<-table(AVD,Renda_cod));summary(freq.avd.ren)
(freq.aivd.ren<-table(AIVD,Renda_cod));summary(freq.aivd.ren)

(freq.avd.imc<-table(AVD,IMC_cod));summary(freq.avd.imc)
(freq.aivd.imc<-table(AIVD,IMC_cod));summary(freq.aivd.imc)

(freq.avd.est<-table(AVD,Estado_civil_cod));summary(freq.avd.est)
(freq.aivd.est<-table(AIVD,Estado_civil_cod));summary(freq.aivd.est)

(freq.avd.hip<-table(AVD,Hipertensao));summary(freq.avd.hip)
(freq.aivd.hip<-table(AIVD,Hipertensao));summary(freq.aivd.hip)

(freq.avd.dm<-table(AVD,Diabete));summary(freq.avd.dm)
(freq.aivd.dm<-table(AIVD,Diabete));summary(freq.aivd.dm)

(freq.avd.ost<-table(AVD,Osteoporose));summary(freq.avd.ost)
chisq.test(AVD, Osteoporose, correct = T)
(freq.aivd.ost<-table(AIVD,Osteoporose));summary(freq.aivd.ost)
chisq.test(AIVD, Osteoporose, correct = T)

(freq.avd.dc<-table(AVD,Cardiaca));summary(freq.avd.dc)
chisq.test(AVD, Cardiaca, correct = T)
(freq.aivd.dc<-table(AIVD,Cardiaca));summary(freq.aivd.dc)
chisq.test(AIVD, Cardiaca, correct = T)

(freq.avd.al<-table(AVD,Alzheimer));summary(freq.avd.al)
chisq.test(AVD, Alzheimer, correct = T)
(freq.aivd.al<-table(AIVD,Alzheimer));summary(freq.aivd.al)
chisq.test(AVD, Alzheimer, correct = T)

(freq.avd.dp<-table(AVD,Depressao));summary(freq.avd.dp)
chisq.test(AVD, Depressao, correct = T)
(freq.aivd.dp<-table(AIVD,Depressao));summary(freq.aivd.dp)
chisq.test(AVD, Depressao, correct = T)

(freq.avd.ti<-table(AVD,Tireoide));summary(freq.avd.ti)
chisq.test(AVD, Tireoide, correct = T)
(freq.aivd.ti<-table(AIVD,Tireoide));summary(freq.aivd.ti)
chisq.test(AIVD, Tireoide, correct = T)

(freq.avd.col<-table(AVD,Colesterol));summary(freq.avd.col)
chisq.test(AVD, Colesterol, correct = T)
(freq.aivd.col<-table(AIVD,Colesterol));summary(freq.aivd.col)
chisq.test(AIVD, Colesterol, correct = T)

