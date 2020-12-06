#Descritiva dados Eliseu

rm(list=ls(all=T))

library(lattice)
library(expss)
library(robustbase)
library(randomcoloR)
library(readxl)
banco = "dados_eliseu_descritiva.xlsx"
banco.geral = read_xlsx(path = banco,sheet=1)
banco.geral = as.data.frame(banco.geral)
attach(banco.geral)
str(banco.geral)

#Variaveis qualitativas

#Sexo - Femninino(2) e Masculino(1) 

#Frequencia simples e frenquencia relativa 

sexo<- subset(x=banco.geral, subset = replicate == 1, select = "sexo")
fre(sexo)

#Grafico de barras e grafico de pizza

graficos<- function(x) {X11(); par(mfrow=c(1,2));freq<- table(x);(porcentagem<-round(100*freq/sum(freq), 0)); pie(freq, labels=paste(porcentagem, "%") ,main="Grafico de setores", radius = 2);k<-200; barplot(freq, col= randomColor(k), ylab="Frequencia", xlab=substitute(x), main = "Grafico de barras")}
graficos(sexo)

#Grafico de barras e grafico de pizza com dados faltantes (NA)

graficos.NA<- function(x) {X11(); par(mfrow=c(1,2));freqx.NA<-table(x, useNA ="ifany"); names(freqx.NA)[is.na(names(freqx.NA))] <- "NA"; porcentagemx.NA<-round(100*freqx.NA/sum(freqx.NA), 0); pie(freqx.NA, labels=paste(porcentagemx.NA, "%"), radius = 2 ,main="Grafico de setores");k<-200; barplot(freqx.NA, col= randomColor(k), ylab="Frequencia", xlab=substitute(x), main = "Grafico de barras")}
graficos.NA(sexo)

#Escolaridade

#Frequencia simples e frenquencia relativa 

escolaridade<- subset(x=banco.geral, subset = replicate == 1, select = "escola" )
fre(escolaridade)

#Grafico de barras e grafico de pizza

graficos(escolaridade)

#Grafico de barras e grafico de pizza com dados faltantes (NA)

graficos.NA(escolaridade)

#Variaveis quantitativas continuas: idade, peso, altura, imc,	kcal,	
#proteinas, lipideos, carboidratos, fibras totais,	calcio, magnesio,
#fosforo,	ferro, sodio, potassio, cobre, zinco,	vitamina a, vitamina B1,	
#vitamina B2, niacina, vitamina B6, vitamina B12, vitamina C e gordura saturada

#Idade, altura, peso, imc. 

iapimc<- subset(banco.geral, subset = replicate == 1, select = c(idadeanos, altura, peso, imc))
iapimc1<- subset(iapimc, subset = sexo == "Homens", select = c(idadeanos, altura, peso, imc)) 
iapimc2<- subset(iapimc, subset = sexo == "Mulheres", select = c(idadeanos, altura, peso, imc))

#Medidas descritivas de posicao e dispersao

medidas.de.posicao<- function(x) { respostas<- list(resumo=summary(x), quantis=fivenum(x)); return(respostas) }
medidas.de.dispersao<- function(x) {respostas<- list(variancia=var(x), desvio_padrao =sd(x), CV=100*(sd(x)/mean(x))); return(respostas)}

lapply(iapimc, function(x) medidas.de.posicao(x))
lapply(iapimc, function(x) medidas.de.posicao(na.omit(x)))
lapply(iapimc, function(x) medidas.de.dispersao(na.omit(x)))

lapply(iapimc1, function(x) medidas.de.posicao(x))
lapply(iapimc1, function(x) medidas.de.posicao(na.omit(x)))
lapply(iapimc1, function(x) medidas.de.dispersao(na.omit(x)))

lapply(iapimc2, function(x) medidas.de.posicao(x))
lapply(iapimc2, function(x) medidas.de.posicao(na.omit(x)))
lapply(iapimc2, function(x) medidas.de.dispersao(na.omit(x)))

#Macronutrientes: proteina, energia(kcal), fribras, carboidratos, saturado, lipideos. 
#Corte de kcal entre 300 e 6000

corte <- cut(banco.geral$kcal, c(0, 300, 6000, 10000),
             labels = c("Abaixo de 300", " Entre 300 e 6000 ", "Acima de 6000"))
summary(corte)

Macro<- subset(banco.geral, select = c(ptna, kcal, fibra_total, cho, saturado, lip))
Macro1<- subset(banco.geral, subset = sexo == "Homens", select = c(ptna, kcal, fibra_total, cho, saturado, lip)) 
Macro2<- subset(banco.geral, subset = sexo == "Mulheres", select = c(ptna, kcal, fibra_total, cho, saturado, lip)) 

#Medidas descritivas de posicao e dispersao

lapply(Macro, function(x) medidas.de.posicao(na.omit(x)))
lapply(Macro, function(x) medidas.de.dispersao(na.omit(x)))

lapply(Macro1, function(x) medidas.de.posicao(na.omit(x)))
lapply(Macro1, function(x) medidas.de.dispersao(na.omit(x)))

lapply(Macro2, function(x) medidas.de.posicao(na.omit(x)))
lapply(Macro2, function(x) medidas.de.dispersao(na.omit(x)))

#Microtrientes.vit: vitaminas (A, B1, B2, B6, B12, C, niacina). 

Micro.vit<- subset(banco.geral, select = c(vit_a, vit_b1, vit_b2, vit_b6, vit_b12, vit_c, niacina))
Micro.vit1<- subset(banco.geral, subset = sexo == "Homens", select = c(vit_a, vit_b1, vit_b2, vit_b6, vit_b12, vit_c, niacina)) 
Micro.vit2<- subset(banco.geral, subset = sexo == "Mulheres", select = c(vit_a, vit_b1, vit_b2, vit_b6, vit_b12, vit_c, niacina)) 

#Medidas descritivas de posicao e dispersao

lapply(Micro.vit, function(x) medidas.de.posicao(x))
lapply(Micro.vit, function(x) medidas.de.dispersao(x))

lapply(Micro.vit1, function(x) medidas.de.posicao(x))
lapply(Micro.vit1, function(x) medidas.de.dispersao(x))

lapply(Micro.vit2, function(x) medidas.de.posicao(x))
lapply(Micro.vit2, function(x) medidas.de.dispersao(x))

#Microtrientes: calcio, magnesio, ferro, zinco, cobre, fosforo, sodio, potassio,
#folato. 

Micro<- subset(banco.geral, select = c(calcio, mg, ferro, zn, cobre, fosforo, sodio, potassio, dfe))
Micro1<- subset(banco.geral, subset = sexo == "Homens", select = c(calcio, mg, ferro, zn, cobre, fosforo, sodio, potassio, dfe)) 
Micro2<- subset(banco.geral, subset = sexo == "Mulheres", select = c(calcio, mg, ferro, zn, cobre, fosforo, sodio, potassio, dfe)) 

#Medidas descritivas de posicao e dispersao

lapply(Micro, function(x) medidas.de.posicao(x))
lapply(Micro, function(x) medidas.de.dispersao(x))

lapply(Micro1, function(x) medidas.de.posicao(x))
lapply(Micro1, function(x) medidas.de.dispersao(x))

lapply(Micro2, function(x) medidas.de.posicao(x))
lapply(Micro2, function(x) medidas.de.dispersao(x))


#Graficos
#Histograma
#Idada, peso, altura e IMC

X11(); par(mfcol=c(2,4));h<- function(x) {(A<- max(x) - min(x)); (k<- sqrt(length(x))); (k<- ceiling(k)); (h<- A/k); (h<- round(h,2)); (int<- c(min(x) + seq(0,k+1,1)*h)); (resumo<- hist(x, breaks=int, include.lowest = F, right = F)); hist(x, breaks=int, col= "green", xlab= substitute(x) , ylab="Frequencia", main="Histograma",include.lowest = F, right = F, xlim=c(min(resumo$breaks)-h/2, max(resumo$breaks)+h/2)) };h(na.omit(iapimc$idade)); h(na.omit(iapimc$altura)); h(na.omit(iapimc$peso)); h(na.omit(iapimc$imc))

#Macronutrientes
X11(); par(mfcol=c(2,6));h(ptna); h(cho); h(fibra_total); h(saturado); h(lip); h(na.omit(kcal))

#Micronutrientes - 1
X11(); par(mfcol=c(2,7));h(vit_a); h(vit_b1); h(vit_b2); h(vit_b12); h(niacina); h(vit_b6); h(vit_c)

#Micronutrientes - 2
X11(); par(mfrow=c(3,6));h(calcio); h(ferro); h(mg); h(zn); h(cobre); h(fosf); h(sodio); h(potassio); h(dfe)

#box-plot ajustado 

#Idade, peso, altura e IMC
X11(); par(mfrow=c(2,2)); adjbox(idadeanos ~ sexo, data = banco.geral, subset = replicate == 1, main = "Box-plot ajustado", xlab = "Idade(anos)");adjbox(peso ~ sexo, data = banco.geral, subset = replicate == 1, main = "Box-plot ajustado", xlab = "Peso(Kg)");adjbox(altura ~ sexo, data = banco.geral, subset = replicate == 1, main = "Box-plot ajustado", xlab = "Altura(m)");adjbox(imc ~ sexo, data = banco.geral,  subset = replicate == 1, main = "Box-plot ajustado", xlab = "IMC")

#Macronutrientes
X11(); par(mfcol=c(2,3)); adjbox(ptna ~ sexo, data = banco.geral, main = "Proteínas (g)");adjbox(cho ~ sexo, data = banco.geral, main = "Carboidratos (g)");adjbox(fibra_total ~ sexo, data = banco.geral, main = "Fibras totais (g)");adjbox(saturado ~ sexo, data = banco.geral, main = "Gordura saturada (g)");adjbox(lip ~ sexo, data = banco.geral, main = "Lipídeos (g)"); adjbox(kcal ~ sexo, data = banco.geral, main = "Energia (kcal)")

#Micronutrientes - 1
X11(); par(mfcol=c(2,4));adjbox(vit_a ~ sexo, data = banco.geral, main = "Vitamina A (mcg)");adjbox(vit_b1 ~ sexo, data = banco.geral, main = "Vitamina B1 (mg)");adjbox(vit_b2 ~ sexo, data = banco.geral, main = "Vitamina B2 (mg)");adjbox(vit_b12 ~ sexo, data = banco.geral, main = "Vitamina B12 (mcg)");adjbox(niacina ~ sexo, data = banco.geral, main = "Niacina (mg)");adjbox(vit_b6 ~ sexo, data = banco.geral, main = "Vitamina B6 (mg)");adjbox(vit_c ~ sexo, data = banco.geral, main = "Vitamina C (mg)"); adjbox(calcio ~ sexo, data = banco.geral, main = "Cálcio (mg)")

#Micronutrientes 2
X11(); par(mfcol=c(2,4));adjbox(mg ~ sexo, data = banco.geral, main = "Magnésio (mg)");adjbox(ferro ~ sexo, data = banco.geral, main = "Ferro (mg)");adjbox(zn ~ sexo, data = banco.geral, main = "Zinco (mg)");adjbox(cobre ~ sexo, data = banco.geral, main = "Cobre (mcg)");adjbox(fosforo ~ sexo, data = banco.geral, main = "Fósforo (mg)");adjbox(sodio ~ sexo, data = banco.geral, main = "Sódio (mg)");adjbox(potassio ~ sexo, data = banco.geral, main = "Potássio (mg)")

#xy

#Macronutrientes
par(mfcol=c(2,3))
xyplot(kcal ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Energia (kcal)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(ptna ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Proteína (g)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(fibra_total ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Fibra total (g)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(cho ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Carboidratos (g)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(saturado ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Gordura Saturada (g)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(lip ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Lipídeos (g)", col = 1, type = "l", scales = list(y = list(relation = 'free')))

#Micronutrientes 1
xyplot(vit_a ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Vitamina A (mcg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(vit_b1 ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Vitamina B1 (mg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(vit_b2 ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Vitamina B2 (mg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(vit_b12 ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Vitamina B12 (mcg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(vit_b6 ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Vitamina B6 (mg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(vit_c ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Vitamina C (mg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(niacina ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Niacina (mg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(dfe ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Folato", col = 1, type = "l", scales = list(y = list(relation = 'free')))

#Micronutrientes 2
xyplot(calcio ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Cálcio (mg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(mg ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Magnésio (mg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(zn ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Zinco (mg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(ferro ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Ferro (mg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(cobre ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Cobre (mcg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(fosforo ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Fósforo (mg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(sodio ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Sódio (mg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(potassio ~ replicate|factor(sexo), group = id...1, data = banco.geral, xlab = "Recordatório", ylab = "Consumo de Potássio (mg)", col = 1, type = "l", scales = list(y = list(relation = 'free')))
