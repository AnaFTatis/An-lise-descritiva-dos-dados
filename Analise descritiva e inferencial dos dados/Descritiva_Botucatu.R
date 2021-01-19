#Descritiva dados Botucatu

rm(list=ls(all=T))

library(lattice)
library(expss)
library(robustbase)
library(randomcoloR)
library(readxl)
banco = "dados_botucatu_descritiva.xlsx"
banco.dados = read_xlsx(path = banco,sheet=1)
banco.dados = as.data.frame(banco.dados)
attach(banco.dados)
str(banco.dados)

#Sexo - Femninino/Masculino

#Frequencia simples e frenquencia relativa

sexo<- subset(x=banco.dados, subset = rec == 1, select = "Gender")
fre(sexo)

#Grafico de barras e grafico de pizza

graficos<- function(x) {X11(); par(mfrow=c(1,2));freq<- table(x);(porcentagem<-round(100*freq/sum(freq), 0)); pie(freq, labels=paste(porcentagem, "%") ,main="Grafico de setores", radius = 2);k<-200; barplot(freq, col= randomColor(k), ylab="Frequencia", xlab=substitute(x), main = "Grafico de barras")}

graficos(sexo)

#Idade codificada

#Frequencia simples e frenquencia relativa

idade_c<- subset(x=banco.dados, subset = rec == 1, select = "idade_cod")
fre(idade_c)

#Grafico de barras e grafico de pizza

graficos(idade_c)

#Escolaridade - analfabeto/fundamental/medio/superior

#Frequencia simples e frenquencia relativa

escolaridade<- subset(x=banco.dados, subset = rec == 1, select = "escolcod" )
fre(escolaridade)

#Grafico de barras e grafico de pizza

graficos(escolaridade)

#IMC codificado

#Frequencia simples e frenquencia relativa

imc_c<- subset(x=banco.dados, subset = rec == 1, select = "IMC_cod" )
fre(imc_c)

#Grafico de barras e grafico de pizza

graficos(imc_c)

#Estado civil - casados/divorciados e separados/solteiros/viuvos

#Frequencia simples e frenquencia relativa

estado_civíl<- subset(x=banco.dados, subset = rec == 1, select = "estcivil" )
fre(estado_civíl)

#Grafico de barras e grafico de pizza

graficos(estado_civíl)

#Ocupacao - trablhando e aposentados

#Frequencia simples e frenquencia relativa

trabalhando<- subset(x=banco.dados, subset = rec == 1, select = "trabalha" ); aposentados<- subset(x=banco.dados, subset = rec == 1, select = "aposentado")
fre(trabalhando)
fre(aposentados)

#Grafico de barras e grafico de pizza

graficos(trabalhando)
graficos(aposentados)

#Doencas - hipertensao/diabete/doenca cardiaca/disfuncoes da tireoide/
#colesterol alto/osteoporose/depressao/alzheimer

#Frequencia simples e frenquencia relativa

hipertenção<- subset(x=banco.dados, subset = rec == 1, select = "hipert"); diabete_mellitus<- subset(x=banco.dados, subset = rec == 1, select = "dm"); doença_cardíaca<- subset(x=banco.dados, subset = rec == 1, select = "dcard"); disfunção_tireóide<- subset(x=banco.dados, subset = rec == 1, select = "tireoide"); colesterol_alto<- subset(x=banco.dados, subset = rec == 1, select = "colest"); osteoporose<- subset(x=banco.dados, subset = rec == 1, select = "osteop"); depressão<- subset(x=banco.dados, subset = rec == 1, select = "depressao"); alzheimer<- subset(x=banco.dados, subset = rec == 1, select = "alzheimer")
fre(hipertenção)
fre(diabete_mellitus)
fre(doença_cardíaca)
fre(disfunção_tireóide)
fre(colesterol_alto)
fre(osteoporose)
fre(depressão)
fre(alzheimer)

#Grafico de barras e grafico de pizza

graficos(hipertenção)
graficos(diabete_mellitus)
graficos(doença_cardíaca)
graficos(disfunção_tireóide)
graficos(colesterol_alto)
graficos(osteoporose)
graficos(depressão)
graficos(alzheimer)

#Circunferência Abdominal (CA) codificada

#Frequencia simples e frequencia relativa

ca_c<- subset(banco.dados, rec == 1, select = "CA_COD")
fre(ca_c)

#Grafico de barras e grafico de pizza

graficos(ca_c)

#Idade, estatura, peso e IMC

iepimc<- subset(banco.dados, subset = rec == 1, select = c(idade, estatura, peso, IMC))
iepimc.m<- subset(iepimc, subset = sexo == "Mulheres", select = c(idade, estatura, peso, IMC))
iepimc.h<- subset(iepimc, subset = sexo == "Homens", select = c(idade, estatura, peso, IMC))

#Medidas descritivas de posicao e dispersao

medidas.de.posicao<- function(x) {respostas<- list(resumo=summary(x), quantis=fivenum(x)); return(respostas) }
medidas.de.dispersao<- function(x) {respostas<- list(variancia=var(x), desvio_padrao =sd(x), CV=100*(sd(x)/mean(x))); return(respostas)}

lapply(iepimc, function(x) medidas.de.posicao(x))
lapply(iepimc, function(x) medidas.de.dispersao(x))

lapply(iepimc.m, function(x) medidas.de.posicao(x))
lapply(iepimc.m, function(x) medidas.de.dispersao(x))

lapply(iepimc.h, function(x) medidas.de.posicao(x))
lapply(iepimc.h, function(x) medidas.de.dispersao(x))

#Cosumo total (gramas)

consumo_total<- subset(banco.dados, subset = rec == 1, select = "Tg")
consumo_total_m<- subset(consumo_total, subset = sexo == "Mulheres", select = "Tg")
consumo_total_h<- subset(consumo_total, subset = sexo == "Homens", select = "Tg")

#Medidas descritivas de posicao e dispersao

lapply(consumo_total, function(x) medidas.de.posicao(x))
lapply(consumo_total, function(x) medidas.de.dispersao(x))

lapply(consumo_total_h, function(x) medidas.de.posicao(x))
lapply(consumo_total_h, function(x) medidas.de.dispersao(x))

lapply(consumo_total_m, function(x) medidas.de.posicao(x))
lapply(consumo_total_m, function(x) medidas.de.dispersao(x))

#Macronutrientes: proteina, proteina vegetal, proteina animal, gordura total, gordura polisaturda,
#energia, fribras, carboidratos, gordura saturada, colesterol, gordura monosaturada.

Macro<- subset(banco.dados, select = c(prot, protanimal, protvegetal, fibra, energia, cho, gorduratotal, saturadosg, monog, polig, colsterol))
Macro.m<- subset(banco.dados, subset = Gender == "Mulheres", select = c(prot, protanimal, protvegetal, fibra, energia, cho, gorduratotal, saturadosg, monog, polig, colsterol))
Macro.h<- subset(banco.dados, subset = Gender == "Homens", select = c(prot, protanimal, protvegetal, fibra, energia, cho, gorduratotal, saturadosg, monog, polig, colsterol))

#Medidas descritivas de posicao e dispersao

lapply(Macro, function(x) medidas.de.posicao(x))
lapply(Macro, function(x) medidas.de.dispersao(x))

lapply(Macro.m, function(x) medidas.de.posicao(x))
lapply(Macro.m, function(x) medidas.de.dispersao(x))

lapply(Macro.h, function(x) medidas.de.posicao(x))
lapply(Macro.h, function(x) medidas.de.dispersao(x))

#proporção - proteinas, carboidratos e gordura

p<- subset(banco.dados, select = c(pprot, pcho, pgord))
p.m<- subset(banco.dados, subset = Gender == "Mulheres", select = c(pprot, pcho, pgord))
p.h<- subset(banco.dados, subset = Gender == "Homens", select = c(pprot, pcho, pgord))

lapply(p, function(x) medidas.de.posicao(x))
lapply(p, function(x) medidas.de.dispersao(x))

lapply(p.m, function(x) medidas.de.posicao(x))
lapply(p.m, function(x) medidas.de.dispersao(x))

lapply(p.h, function(x) medidas.de.posicao(x))
lapply(p.h, function(x) medidas.de.dispersao(x))

#Microtrientes: vitaminas (A, D, E, K, B1, B2, B3, acido pantenoico, B6, folato,
#B12, C).

Micro.vit<- subset(banco.dados, select = c(vitamcg, vitd, vite, vitk, vitc, vitb1, vitb2, vitb3, acpant, vitb6, folato, vitb12))
Micro.vit.m<- subset(banco.dados, subset = Gender == "Mulheres", select = c(vitamcg, vitd, vite, vitk, vitc, vitb1, vitb2, vitb3, acpant, vitb6, folato, vitb12))
Micro.vit.h<- subset(banco.dados, subset = Gender == "Homens", select = c(vitamcg, vitd, vite, vitk, vitc, vitb1, vitb2, vitb3, acpant, vitb6, folato, vitb12))

#Medidas descritivas de posicao e dispersao

lapply(Micro.vit, function(x) medidas.de.posicao(x))
lapply(Micro.vit, function(x) medidas.de.dispersao(x))

lapply(Micro.vit.m, function(x) medidas.de.posicao(x))
lapply(Micro.vit.m, function(x) medidas.de.dispersao(x))

lapply(Micro.vit.h, function(x) medidas.de.posicao(x))
lapply(Micro.vit.h, function(x) medidas.de.dispersao(x))

#Microtrientes: calcio, magnesio, ferro, zinco, cobre, fosforo, selenio, sodio,
#potassio, manganes.

Micro<- subset(banco.dados, select = c(calcio, mg, ferro, zinco, cobre, fosforo, sodio, potassio, Manganes, selenio))
Micro.m<- subset(banco.dados, subset = Gender == "Mulheres", select = c(calcio, mg, ferro, zinco, cobre, fosforo, sodio, potassio, Manganes, selenio))
Micro.h<- subset(banco.dados, subset = Gender == "Homens", select = c(calcio, mg, ferro, zinco, cobre, fosforo, sodio, potassio, Manganes, selenio))

#Medidas descritivas de posicao e dispersao

lapply(Micro, function(x) medidas.de.posicao(x))
lapply(Micro, function(x) medidas.de.dispersao(na.omit(x)))

lapply(Micro.m, function(x) medidas.de.posicao(x))
lapply(Micro.m, function(x) medidas.de.dispersao(x))

lapply(Micro.h, function(x) medidas.de.posicao(x))
lapply(Micro.h, function(x) medidas.de.dispersao(x))

#Graficos
#Histograma

#Idade, estatura, peso e IMC
X11(); par(mfcol=c(2,4)); h<- function(x) {(A<- max(x) - min(x)); (k<- sqrt(length(x))); (k<- ceiling(k)); (h<- A/k); (h<- round(h,2)); (int<- c(min(x) + seq(0,k+1,1)*h)); (resumo<- hist(x, breaks=int, include.lowest = F, right = F)); hist(x, breaks=int, col= "green", xlab= substitute(x) , ylab="Frequencia", main="Histograma",include.lowest = F, right = F, xlim=c(min(resumo$breaks)-h/2, max(resumo$breaks)+h/2))};h(iepimc$idade); h(iepimc$estatura); h(iepimc$peso); h(iepimc$IMC)

#Cosumo total
X11(); h(Tg)

#Macronutrientes - 1
X11(); par(mfcol=c(2,6)); h(energia); h(prot); h(protanimal); h(protvegetal); h(fibra); h(cho)

#Macronutrientes - 2
X11(); par(mfcol=c(2,5)); h(gorduratotal); h(saturadosg); h(monog); h(polig); h(colsterol)

#proproção
X11(); par(mfcol=c(2,3)); h(pprot); h(pcho); h(pgord)

#Micronutrientes - vit 1
X11(); par(mfcol=c(2,5)); h(vitamcg); h(vitk); h(vite); h(vitd); h(vitc)

#Micronutrientes - vit 2
X11(); par(mfrow=c(2,7)); h(vitb1); h(vitb2); h(vitb3); h(acpant); h(vitb6); h(folato); h(vitb12)

#Micronutrientes - 1
X11(); par(mfcol=c(2,5)); h(calcio); h(fosforo); h(mg); h(ferro); h(zinco)

#Micronutrientes - 2
X11(); par(mfrow=c(2,5)); h(cobre); h(selenio); h(sodio); h(potassio); h(Manganes)

#box-plot ajustado

#Idade, estatura, peso e IMC 
X11(); par(mfrow=c(1,4)); adjbox(idade ~ Gender, data = banco.dados, subset = rec == 1, main = "Idade");adjbox(peso ~ Gender, data = banco.dados, subset = rec == 1, main = "Peso");adjbox(estatura ~ Gender, data = banco.dados, subset = rec == 1, main = "Estatura");adjbox(IMC ~ Gender, data = banco.dados,  subset = rec == 1, main = "IMC")

#consumo total
X11(); adjbox(Tg ~ Gender, data = banco.dados, subset = rec == 1, main = "Consumo total (gramas)")

#Macronutrientes
X11(); par(mfcol=c(2,6)); adjbox(energia ~ Gender, data = banco.dados, main = "Energia");adjbox(prot ~ Gender, data = banco.dados, main = "Porteínas");adjbox(protanimal ~ Gender, data = banco.dados, main = "Proteína animal");adjbox(protvegetal ~ Gender, data = banco.dados, main = "Proteína vegetal");adjbox(fibra ~ Gender, data = banco.dados, main = "Fibras"); adjbox(cho ~ Gender, data = banco.dados, main = "Carboidratos");adjbox(gorduratotal ~ Gender, data = banco.dados, main = "Gordura total"); adjbox(saturadosg ~ Gender, data = banco.dados, main = "Gordura Saturada"); adjbox(monog ~ Gender, data = banco.dados, main = "Gordura monosaturada");adjbox(polig ~ Gender, data = banco.dados, main = "Gordura polisaturada"); adjbox(colsterol ~ Gender, data = banco.dados, main = "Colesterol")

#Proporção
X11();par(mfrow=c(1,3)); adjbox(pprot ~ Gender, data = banco.dados, main = "Proporção de proteínas"); adjbox(pcho ~ Gender, data = banco.dados, main = "Proporção de carboidratos");adjbox(pgord ~ Gender, data = banco.dados, main = "Proporção de gorduras")

#Micronutrientes - vit
X11(); par(mfcol=c(2,6)); adjbox(vitamcg ~ Gender, data = banco.dados, main = "Vitamina A");adjbox(vitk ~ Gender, data = banco.dados, main = "Vitamina K");adjbox(vite ~ Gender, data = banco.dados, main = "Vitamina E");adjbox(vitd ~ Gender, data = banco.dados, main = "Vitamina D");adjbox(vitc ~ Gender, data = banco.dados, main = "Vitamina C"); adjbox(vitb1 ~ Gender, data = banco.dados, main = "Vitamina B1");adjbox(vitb2 ~ Gender, data = banco.dados, main = "Vitamina B2");adjbox(vitb3 ~ Gender, data = banco.dados, main = "Vitamina B3");adjbox(acpant ~ Gender, data = banco.dados, main = "Ácido pantenoico");adjbox(vitb6 ~ Gender, data = banco.dados, main = "Vitamina B6");adjbox(folato ~ Gender, data = banco.dados, main = "Folato"); adjbox(vitb12 ~ Gender, data = banco.dados, main = "Vitamina B12")

#Micronutrientes
X11(); par(mfcol=c(2,5)); adjbox(calcio ~ Gender, data = banco.dados, main = "Cálcio");adjbox(mg ~ Gender, data = banco.dados, main = "Magnésio");adjbox(ferro ~ Gender, data = banco.dados, main = "Ferro");adjbox(zinco ~ Gender, data = banco.dados, main = "Zinco");adjbox(cobre ~ Gender, data = banco.dados, main = "Cobre");adjbox(fosforo ~ Gender, data = banco.dados, main = "Fósforo");adjbox(sodio ~ Gender, data = banco.dados, main = "Sódio");adjbox(potassio ~ Gender, data = banco.dados, main = "Potássio"); adjbox(selenio ~ Gender, data = banco.dados, main = "Selênio");adjbox(Manganes ~ Gender, data = banco.dados, main = "Manganês")

#xy

#Consumo total
xyplot(Tg ~ rec|factor(Gender), group = id, data = banco.dados, xlab="Recordatório", ylab = "Consumo total (gramas)", col = 1, type = "l", scales = list(y = list(relation = 'free'), x=list(at=c(1,2,3))))

#Macronutrientes
xyplot(energia ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Energia", col = 1, type = "l", scales = list(y = list(relation = 'free'), x=list(at=c(1,2,3))))
xyplot(prot ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Proteínas", col = 1, type = "l", scales = list(y = list(relation = 'free'), x=list(at=c(1,2,3))))
xyplot(protanimal ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Proteína animal", col = 1, type = "l", scales = list(y = list(relation = 'free'), x=list(at=c(1,2,3))))
xyplot(protvegetal ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Proteína vegetal", col = 1, type = "l", scales = list(y = list(relation = 'free'), x=list(at=c(1,2,3))))
xyplot(fibra ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Fibras", col = 1, type = "l", scales = list(y = list(relation = 'free'), x=list(at=c(1,2,3))))
xyplot(cho ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Carboidratos", col = 1, type = "l", scales = list(y = list(relation = 'free'), x=list(at=c(1,2,3))))
xyplot(gorduratotal ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Gordura total", col = 1, type = "l", scales = list(y = list(relation = 'free'), x=list(at=c(1,2,3))))
xyplot(saturadosg ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Gordura saturada", col = 1, type = "l", scales = list(y = list(relation = 'free'), x=list(at=c(1,2,3))))
xyplot(monog ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Gordura monosaturada", col = 1, type = "l", scales = list(y = list(relation = 'free'), x=list(at=c(1,2,3))))
xyplot(polig ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Gordura polisaturada", col = 1, type = "l", scales = list(y = list(relation = 'free'), x=list(at=c(1,2,3))))
xyplot(colsterol ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Colesterol", col = 1, type = "l", scales = list(y = list(relation = 'free'), x=list(at=c(1,2,3))))

#Proporção
xyplot(pprot ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Proporção de proteínas", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(pcho ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Proporção de carboidratos", col = 1, type = "l", scales = list(y = list(relation = 'free')))
xyplot(pgord ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Proporção de gorduras", col = 1, type = "l", scales = list(y = list(relation = 'free')))

#Micronutrientes
xyplot(vitamcg ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Vitamina A", col = 1, type = "l", scales = list(y = list(relation = 'free'), x=list(at=c(1,2,3))))
xyplot(vite ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Vitamina E", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(vitk ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Vitamina K", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(vitd ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Vitamina D", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(vitc ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Vitamina C", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(vitb1 ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Vitamina B1", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(vitb2 ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Vitamina B2", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(vitb3 ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Vitamina B3", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(acpant ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Ácido pantenoico", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(vitb6 ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Vitamina B6", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(folato ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Folato", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(vitb12 ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Vitamina B12", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(calcio ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Cálcio", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(fosforo ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Fósforo", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(mg ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Magnésio", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(ferro ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Ferro", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(zinco ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Zinco", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(cobre ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Cobre", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(selenio ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Selênio", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(sodio ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Sódio", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(potassio ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Potássio", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))
xyplot(Manganes ~ rec|factor(Gender), group = id, data = banco.dados, xlab = "Recordatório", ylab = "Mangenês", col = 1, type = "l", scales = list(y = list(relation = 'free'),x=list(at=c(1,2,3))))

