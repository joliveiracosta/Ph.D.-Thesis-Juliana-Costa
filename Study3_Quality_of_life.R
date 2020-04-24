#######################################
# Estudo 2 
# Qualidade de vida relacionada à saúde entre pacientes infectados pelo HIV 
# iniciando tratamento no Brasil na era de medicamentos em dose fixa combinada
# Autores: 
# Juliana de Oliveira Costa, Maria das Graças Braga Ceccato, Sallie Anne Pearson, 
# Palmira de Fátima Bonolo, Micheline Rosa Silveira, Francisco de Assis Acurcio
# Contato: juliana.olic@gmail.com
#######################################

# Carregar pacotes
if(!require(installr)) {
install.packages("installr"); require(installr)} #load / install+load installr
updateR()
if(!require(nlme)){install.packages("nlme")}
if(!require(lattice)){install.packages("lattice")}
install.packages("mctest")
library(mctest)
library(readxl)
library(car)
library(compareGroups)
library(ggplot2)
library(knitr)
library(nlme)
install.packages("MASS")
library(MASS)

# Leitura dos dados
setwd("C:\\Users\\z5202391\\Dropbox\\Doutorado_Juliana\\Artigo2\\Banco")
dados <- read_excel("Banco_HEM_CTA_CTR_BASAL_QV_20181127.xlsx")

# Checando importação
names(dados)
head(dados)
dim(dados)
str(dados)

###############PREPARANDO VARIAVEIS#####################

##########BANCO QUALIDADE DE VIDA

dados$CLASSE_ECONOMICA <- recode(dados$RENDA_ABEP_faixa, "0=0; 1=1; 2=1; 3=2; 4=2; 5=2")

QV <- c("IDADE","Idade_dic","IDADE_FX1","IDADE_FX2","Civil_dic",
        "MORAJUNTO","FILHO", "PLANO","EMPREGO","FUMA","STR_DTG_MM",
        "Escol_faixa","SEXO","Raca_dic","Risco_HIV", "Risco_HIV_dic",  
        "CLASSE_ECONOMICA",  "ALCOOL", "Alcool_atual", "Ansiedade_dic", "Depressao_dic","DOENCA", "DOENCADIC","TPOSITIVO",
        "ESQUEMA3","Serviço_dic","DROGAS_ILICITAS", "T_TRAT_SICLOM", "Local_TARV","T_TRAT_SICLOM_Dic","CRENCA","Tempo_diag", "Tdiag_dic", "Interval_diag_TARV",
        "TPOSITIVO_dic","Fumar_vida",
         "ORIENTA_ENTENDE","Adesao_dic","DIFICULDADE_DIC",
         "REACOESADVSOMA_dic", "QTD_RA", "QTD_RA_Dic", "QTD_RA_Dic2", "TROCA", "Ano", "Risco_HIV",
        "CD4_DIC3", "CD4_DIC",  "CV_100MIL","CD4_ENT_DIC", "CV_ENTRE_DIC","WHO_dic", "WHO_dic3", "Late", "Ans_Dep", "Sex_risk",
        "INDICE_EQ5D", "Q1","Q2","Q3","Q4","Q5","Q6","QV_geral",
        "Dominio1_FIS","Dominio2_PSI","Dominio3_IND","Dominio4_SOC",
        "Dominio5_AMB", "Dominio6_ESP", "STR_DTG", "STR_MM", "DTG_MM")

dados2 <- dados[,QV]
dados3 <- dados2[dados2$STR_DTG_MM != "2", ] #Exclui pacientes MTR (n = 40)
dadosQV <- dados3[dados3$T_TRAT_SICLOM>=14, ] #Exclui pacientes com menos de 14 dias de exposição à TARV

###Sugrupo de variáveis tipo factor (categóricas)

dadosQV$ALCOOL <- recode(dadosQV$ALCOOL, "1=1; 2=0")
dadosQV$ALCOOL <-factor(dadosQV$ALCOOL, labels = c("No","Yes"))
dadosQV$Alcool_atual <- factor(dadosQV$Alcool_atual, labels = c("No", "Yes"))
dadosQV$Ano <- factor(dadosQV$Ano, labels = c("2015", "2016", "2017"))
dadosQV$Adesao_dic <- factor(dadosQV$Adesao_dic, labels = c("No","Yes"))
dadosQV$Ansiedade_dic <- factor(dadosQV$Ansiedade_dic, labels = c("No","Yes"))
dadosQV$CD4_DIC3 <- factor(dadosQV$CD4_DIC3, labels = c(">500 cells/mm3", "200-500 cells/mm3","<200 cells/mm3"))
dadosQV$CD4_ENT_DIC <- factor(dadosQV$CD4_ENT_DIC, labels = c(">500 cells/mm3", "200-500 cells/mm3","<200 cells/mm3"))
dadosQV$Civil_dic <- factor(dadosQV$Civil_dic, labels = c("single/divorced/widowed","married/stable union"))
dadosQV$CLASSE_ECONOMICA <- factor (dadosQV$CLASSE_ECONOMICA, labels = c("D-E","C","A-B")) 
#dadosQV$CV_DIC <- factor(dadosQV$CV_DIC, labels = c("Undetectable","Detectable"))
dadosQV$CV_ENTRE_DIC <- factor(dadosQV$CV_ENTRE_DIC, labels = c("Undetectable","Detectable"))
dadosQV$CV_100MIL <- factor(dadosQV$CV_100MIL, labels = c("Until 100.000 copies","More than 100.000 copies"))
dadosQV$CRENCA <- recode(dadosQV$CRENCA, "1=1;2=0;9=NA")
dadosQV$CRENCA <- factor(dadosQV$CRENCA, labels = c("No","Yes"))
dadosQV$Depressao_dic <- factor(dadosQV$Depressao_dic, labels = c("No","Yes"))
dadosQV$DIFICULDADE_DIC <- factor(dadosQV$DIFICULDADE_DIC, labels = c("Easy","Difficult"))
dadosQV$DOENCADIC <- factor(dadosQV$DOENCADIC, labels = c("No","Yes"))
dadosQV$DROGAS_ILICITAS <- factor(dadosQV$DROGAS_ILICITAS, labels = c("No","Yes"))
dadosQV$EMPREGO <- recode(dadosQV$EMPREGO, "1=1; 2=0")
dadosQV$EMPREGO <- factor(dadosQV$EMPREGO, labels = c("No","Yes"))
dadosQV$Escol_faixa <- factor(dadosQV$Escol_faixa, labels = c("Until 9","10-12","13 or more"))
dadosQV$ESQUEMA3 <- factor(dadosQV$ESQUEMA3, labels = c("ITRNN","IIN", "IP"))
dadosQV$FILHO <- recode(dadosQV$FILHO, "1=1; 2=0")
dadosQV$FILHO <- factor(dadosQV$FILHO, labels = c("No","Yes"))
dadosQV$FUMA <- recode(dadosQV$FUMA, "1=1; 2=0")
dadosQV$FUMA <- factor(dadosQV$FUMA, labels = c("No","Yes"))
dadosQV$Fumar_vida <- factor(dadosQV$Fumar_vida, labels = c("No","Yes"))
dadosQV$Idade_dic <- factor(dadosQV$Idade_dic, labels = c("Until 33 years","More than 33 years"))
dadosQV$IDADE_FX1 <- factor(dadosQV$IDADE_FX1, labels = c("16-20 years","21-30 years","31-40 years","41-50 years","51 years or more"))        
dadosQV$IDADE_FX2 <- factor(dadosQV$IDADE_FX2, labels = c("16-24 years","25-34 years","35-44 years","45-54 years","55 years or more")) 
dadosQV$MORAJUNTO <- recode(dadosQV$MORAJUNTO, "1=1; 2=0")
dadosQV$MORAJUNTO <- factor(dadosQV$MORAJUNTO, labels = c("No","Yes"))
dadosQV$ORIENTA_ENTENDE <- recode(dadosQV$ORIENTA_ENTENDE, "1=0; 0=1")
dadosQV$ORIENTA_ENTENDE <- factor(dadosQV$ORIENTA_ENTENDE, labels = c("No","Yes"))
dadosQV$PLANO <- recode(dadosQV$PLANO, "1=1; 2=0")
dadosQV$PLANO <- factor(dadosQV$PLANO, labels = c("No","Yes"))
dadosQV$QTD_RA_Dic <- factor(dadosQV$QTD_RA_Dic, labels = c("0-5","More than 5"))
dadosQV$QTD_RA_Dic2 <- factor(dadosQV$QTD_RA_Dic2, labels = c("0-3","More than 3"))
dadosQV$Raca_dic <- factor(dadosQV$Raca_dic, labels = c("Non-Brown","Brown"))
dadosQV$REACOESADVSOMA_dic <- factor(dadosQV$REACOESADVSOMA_dic, labels = c("No","Yes"))
dadosQV$Risco_HIV <- factor(dadosQV$Risco_HIV, labels = c("Heterossexual woman","Heteossexual men", "MSM", "IDU", "Unknown"))
dadosQV$Risco_HIV_dic <- factor(dadosQV$Risco_HIV_dic, labels = c("Non-MSM","MSM"))
dadosQV$Serviço_dic <- factor(dadosQV$Serviço_dic, labels = c("I","II", "III"))
dadosQV$SEXO <- recode(dadosQV$SEXO, "1=1; 2=0")
dadosQV$SEXO <- factor(dadosQV$SEXO, labels = c("Female","Male"))
dadosQV$Local_TARV <-factor(dadosQV$Local_TARV, labels = c("Outpatient","Inpatient"))
dadosQV$T_TRAT_SICLOM_Dic <- factor(dadosQV$T_TRAT_SICLOM_Dic, labels = c("Until 60 days","More than 60 days"))
dadosQV$T_TRAT_SICLOM_mo <- dadosQV$T_TRAT_SICLOM/30
dadosQV$TPOSITIVO_dic <- factor(dadosQV$TPOSITIVO_dic, labels = c("Until 6 months","More than 6 months"))
dadosQV$TROCA <- recode(dadosQV$TROCA, "1=1; 2=0")
dadosQV$TROCA <- factor(dadosQV$TROCA, labels = c("No","Yes"))
dadosQV$STR_DTG_MM <- factor (dadosQV$STR_DTG_MM, labels = c("STR","DTG"))
dadosQV$STR_DTG <- factor (dadosQV$STR_DTG, labels = c("STR","DTG"))
dadosQV$Q1 <- factor(dadosQV$Q1, labels =c("no problem","some problems"))
dadosQV$Q2 <- factor(dadosQV$Q2, labels =c("no problem","some problems","extreme problems"))
dadosQV$Q3 <- factor(dadosQV$Q3, labels =c("no problem","some problems","extreme problems"))
dadosQV$Q4 <- factor(dadosQV$Q4, labels =c("no problem","some problems","extreme problems"))
dadosQV$Q5 <- factor(dadosQV$Q5, labels =c("no problem","some problems","extreme problems"))
dadosQV$WHO_dic3 <- factor(dadosQV$WHO_dic3, labels =c("A","B","C"))
dadosQV$WHO_dic <- factor(dadosQV$WHO_dic, labels =c("Non-AIDS","AIDS"))
dadosQV$Tdiag_dic <- factor(dadosQV$Tdiag_dic, labels =c("<=6 mo",">6 mo"))
dadosQV$CD4_DIC <- factor(dadosQV$CD4_DIC, labels =c("=<200",">200"))
dadosQV$Ans_Dep <- factor(dadosQV$Ans_Dep, labels =c("No","Yes"))

levels(dadosQV$CD4_DIC3)<-c(levels(dadosQV$CD4_DIC3),"Missing") 
dadosQV$CD4_DIC3[is.na(dadosQV$CD4_DIC3)] <- "Missing" 

levels(dadosQV$CV_100MIL)<-c(levels(dadosQV$CV_100MIL),"Missing") 
dadosQV$CV_100MIL[is.na(dadosQV$CV_100MIL)] <- "Missing" 


write.csv(dadosQV, "Banco_QV.csv")

by (data = dados$QTD_RA, INDICES = dados$STR_DTG_MM, FUN = summary)
summary(dados$QTD_RA)

by (data = dadosQV$T_TRAT_SICLOM, INDICES = dadosQV$STR_DTG_MM, FUN = summary)
by (data = dadosQV$T_TRAT_SICLOM, INDICES = dadosQV$STR_DTG_MM, FUN = sd)


comp.time <- compareGroups(dadosQV$STR_DTG_MM ~ dadosQV$T_TRAT_SICLOM, method = 4) 
comp.time 
comp.time.comptab <- createTable(comp.time,show.all = TRUE)  
print(comp.time.comptab, header.labels = c(p.overall = "p-value", all = "Total"))

####DESCRITIVA CATEGORICAS EXPLICATIVAS

dadosQV.comp <- compareGroups(dadosQV$STR_DTG_MM ~ dadosQV$SEXO +
                                dadosQV$Risco_HIV + dadosQV$Risco_HIV_dic + dadosQV$Idade_dic + dadosQV$IDADE_FX1 + dadosQV$IDADE_FX2 +
                                dadosQV$Civil_dic + dadosQV$Raca_dic + dadosQV$Escol_faixa + dadosQV$MORAJUNTO + 
                                dadosQV$FILHO + dadosQV$PLANO + dadosQV$CLASSE_ECONOMICA + 
                                dadosQV$EMPREGO + 
                                dadosQV$CRENCA+ dadosQV$FUMA + dadosQV$Fumar_vida + dadosQV$ALCOOL + dadosQV$Alcool_atual +
                                dadosQV$DROGAS_ILICITAS + 
                                dadosQV$CD4_DIC3 + dadosQV$WHO_dic3 + dadosQV$WHO_dic + dadosQV$CV_100MIL + dadosQV$DOENCADIC + 
                                dadosQV$TPOSITIVO_dic + dadosQV$Ansiedade_dic + dadosQV$Depressao_dic + dadosQV$Ans_Dep + dadosQV$Local_TARV + dadosQV$Ano +
                                dadosQV$ESQUEMA3 + dadosQV$T_TRAT_SICLOM_Dic + dadosQV$Adesao_dic +
                                dadosQV$DIFICULDADE_DIC + dadosQV$REACOESADVSOMA_dic + dadosQV$QTD_RA_Dic + dadosQV$QTD_RA_Dic2 +
                                dadosQV$TROCA + dadosQV$Serviço_dic)

#Resultados da descritiva da população
dadosQV.comp 

#Monta tabela da descritiva e exporta
dadosQV.comptab <- createTable(dadosQV.comp,show.all = TRUE)  
print(dadosQV.comptab, header.labels = c(p.overall = "p-value", all = "Total"))
export2word(dadosQV.comptab, file = "Descritiva_STR_DTG_14.doc" )

#Consulta valores faltantes
missingTable(dadosQV.comp) 

100*prop.table(table(dadosQV$DOENCADIC))

#####DESCRITIVA CONTÍNUAS

summary(dadosQV$T_TRAT_SICLOM)
sd(dadosQV$T_TRAT_SICLOM, na.rm = T)
by (data= dadosQV$T_TRAT_SICLOM, INDICES = dadosQV$STR_DTG_MM , FUN=summary) 
by (data= dadosQV$T_TRAT_SICLOM, INDICES = dadosQV$STR_DTG_MM , FUN=sd) 

summary(dadosQV$TPOSITIVO)
sd(dadosQV$TPOSITIVO, na.rm = T)

summary(dadosQV$IDADE)
sd(dadosQV$IDADE, na.rm = T)

summary(dadosQV$Tempo_diag)
sd(dadosQV$Tempo_diag, na.rm = T)

summary(dadosQV$Interval_diag_TARV)
sd(dadosQV$Interval_diag_TARV, na.rm = T)
by (data= dadosQV$Interval_diag_TARV, INDICES = dadosQV$STR_DTG_MM , FUN=summary) #Nao usei


by (data= dados$Tempo_diag, INDICES = dados$STR_DTG_MM , FUN=summary)


names(dados)

summary(dados$IDADE)
sd(dados$IDADE, na.rm = T)

dadosQV.comp2 <- compareGroups(dadosQV$STR_DTG_MM ~ dadosQV$Risco_HIV + dadosQV$WHO_dic3) 
dadosQV.comp2 ##Resultados da descritiva da população

dadosQV.comp2tab <- createTable(dadosQV.comp2,show.all = TRUE)  #Montar tabela da descritiva

print(dadosQV.comp2tab, header.labels = c(p.overall = "p-value", all = "Total"))

export2word(dadosQV.comp2tab, file = "Descritiva2.doc" )

####VARIAVEL RESPOSTA EQ5D

dadosQV.EQ <- compareGroups(dadosQV$STR_DTG_MM ~ 
                                dadosQV$Q1 + dadosQV$Q2 + 
                                dadosQV$Q3 + dadosQV$Q4 +
                                dadosQV$Q5, method = 4)

#Resultados da descritiva da população
dadosQV.EQ 

#Montar tabela da descritiva
dadosQV.EQ <- createTable(dadosQV.EQ,show.all = TRUE, digits=2)  
print(dadosQV.EQ, header.labels = c(p.overall = "p-value", all = "Total"))
export2word(dadosQV.EQ, file = "DescritivaEQ_STR_DTG_14.doc" )

#Consulta valores faltantes NAO TEM
missingTable(dadosQV.EQ) 


####VARIAVEIS RESPOSTA QUALIDADE DE VIDA

# Checa distribuicao
hist(dadosQV$QV_geral)
shapiro.test(dadosQV$QV_geral)

# Compara grupos
dadosQV.cont <- compareGroups(dadosQV$STR_DTG_MM ~ 
                                dadosQV$INDICE_EQ5D + dadosQV$Q6 +
                                dadosQV$QV_geral +
                                dadosQV$Dominio1_FIS + dadosQV$Dominio2_PSI +
                                dadosQV$Dominio3_IND + dadosQV$Dominio4_SOC +
                                dadosQV$Dominio5_AMB + dadosQV$Dominio6_ESP + dadosQV$T_TRAT_SICLOM_mo),
                              method = 4) #method 4 ele usa shapiro para ver se é noral e aplica
                                          #o teste mais adequado
                               
##Resultados da descritiva da população de variaveis da QV
dadosQV.cont 

#Monta tabela da descritiva, exporta e checa dados faltantes
dadosQV.conttab <- createTable(dadosQV.cont,show.all = TRUE)  
print(dadosQV.conttab, header.labels = c(p.overall = "p-value", all = "Total"))
export2word(dadosQV.conttab, file = "QV_Descritiva_str_dtg_14.doc" )
missingTable(dadosQV.cont)

####### Checa diferença de QV entre unidades do estudo

dadosQV.serv <- compareGroups(dadosQV$Serviço_dic ~ 
                                dadosQV$INDICE_EQ5D + dadosQV$Q6 +
                                dadosQV$QV_geral +
                                dadosQV$Dominio1_FIS + dadosQV$Dominio2_PSI +
                                dadosQV$Dominio3_IND + dadosQV$Dominio4_SOC +
                                dadosQV$Dominio5_AMB + dadosQV$Dominio6_ESP,method = 4)

## Resultados da QV por servico
dadosQV.serv

# Monta tabela da descritiva, exporta e checa dados faltantes
dadosQV.serv <- createTable(dadosQV.serv,show.all = TRUE)  
print(dadosQV.serv, header.labels = c(p.overall = "p-value", all = "Total"))
export2word(dadosQV.serv, file = "QV_Servico.doc" )

####Gráfico de correlação variando por intercepto
xyplot(INDICE_EQ5D ~ Q6|as.factor(Serviço_dic),data=dadosQV,
       type=c("p","g","r"),col="dark blue",col.line="black",lwd=2,
       xlab="Escala Visual Analógica",
       ylab="Utilidade")

xyplot(INDICE_EQ5D ~ QV_geral|as.factor(Serviço_dic),data=dadosQV,
       type=c("p","g","r"),col="dark blue",col.line="black",lwd=2,
       xlab="Qualidade de vida geral WHOQOL-HIV BREF",
       ylab="Utilidade")

xyplot(Q6 ~ QV_geral|as.factor(Serviço_dic),data=dadosQV,
       type=c("p","g","r"),col="dark blue",col.line="black",lwd=2,
       xlab="Qualidade de vida geral WHOQOL-HIV BREF",
       ylab="Escala Visual Analógica")

####REGRESSAO LINEAR BINARIA EQ5D e categóricas

summary(dadosQV$INDICE_EQ5D)
hist(dadosQV$INDICE_EQ5D)
shapiro.test(dadosQV$INDICE_EQ5D)
#Histograma e shapiro <0,05 indicam que a o índice QV não tem comportamento normal

summary(dadosQV$Q6)
hist(dadosQV$Q6)
shapiro.test(dadosQV$Q6)
#Histograma e shapiro <0,05 indicam que a EVA não tem comportamento normal

###Verifica diferenca por meio de boxplot

par(mfrow = c(1, 1))
boxplot(dadosQV$INDICE_EQ5D ~ dadosQV$Serviço_dic,xlab="Seviço",ylab="Índice EQ5D")
abline(h=mean(dadosQV$INDICE_EQ5D),col="red",lwd=2)

par(mfrow = c(1, 1))
boxplot(dadosQV$Q6 ~ dadosQV$Serviço_dic,xlab="Seviço",ylab="VAS")
abline(h=mean(dadosQV$Q6), col="red",lwd=2)
     
par(mfrow = c(1, 1))
boxplot(dadosQV$QV_geral ~ dadosQV$Serviço_dic,xlab="Seviço",ylab="QV geral")
abline(h=mean(dadosQV$QV_geral), col="red",lwd=2)  
       

#### Usando pacote lme
?nlme

## Modelo 1 (modelo vazio)  
Null.Model<-lme(INDICE_EQ5D ~ 1,random = ~1|Serviço_dic,
                control=list(opt="optim"), method = "ML",
                na.action = "na.omit",data = dadosQV)
summary(Null.Model)
VarCorr(Null.Model)[,1]   #Variancia do intercepto (random effects) 
                          #Variabilidade entre os servicos (1,44%)

## Modelo Single-level-individual analysis
Single.ind.model<- lm(INDICE_EQ5D ~ 1, method = "ML",
                      na.action = "na.omit",data=dadosQV) 
summary(Single.ind.model)
summary(Single.ind.model)$sigma^2   #Variabilidade entre indivíduos (1,46%)

## Modelo Single-level-ecological analysis
EQ5D.serv <- aggregate(dadosQV$INDICE_EQ5D,list(dadosQV$Serviço_dic),FUN=mean)
names(EQ5D.serv) <- c("Serviço","EQ5D.media")
head(EQ5D.serv)

Single.eco.model<- lm(EQ5D.media ~ 1, data=EQ5D.serv) 
summary(Single.eco.model)
summary(Single.eco.model)$sigma^2   #Variabilidade entre áreas (0,04%)

Single.eco.model<- lm(INDICE_EQ5D ~ 1,data=dadosQV) 

#Calculando ICC  (intra-class correlation)  (Table 1)
VarCorr(Null.Model)  #ICC = variancia entre pessoas/ variaria entre pessoas + aglomerados
#Variancia do intercepto (random effects) 
#Variabilidade entre os servicos 
0.0143549027 / ( 0.0143549027 +  0.11981195) 

# =  0.1069929,  10.7% da variância total é explicada
# pela variabilidade entre Serviços                                               

# Determinando se variação do intercepto entre areas é significante
Null.gls<-gls(INDICE_EQ5D ~ 1, control=list(opt="optim"), 
              method = "ML",
              na.action = "na.omit",
              data=dadosQV)
anova(Null.gls, Null.Model) 
## p-value = 0.1559 - A variação no intercepto entre serviços não é relevante, porém, opto manter pq a univariada do EQ5D com serviço mostra diferença entre grupos e 
                      #Boxplot evidencia distribuição diferente entre serviços

#Teste colinearidade para variáveis categóricas
#Cramer's V Calculado a partir do teste qui quadrado

cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}

#Testar pares de variáveis:

names(dadosQV)

#Sexo
cv.test(dadosQV$SEXO, dadosQV$IDADE) #0.324
cv.test(dadosQV$SEXO, dadosQV$Civil_dic) 
cv.test(dadosQV$SEXO, dadosQV$Raca_dic) 
cv.test(dadosQV$SEXO, dadosQV$Escol_faixa) #0.27
cv.test(dadosQV$SEXO, dadosQV$MORAJUNTO) 
cv.test(dadosQV$SEXO, dadosQV$FILHO) #0.29
cv.test(dadosQV$SEXO, dadosQV$PLANO) 
cv.test(dadosQV$SEXO, dadosQV$CLASSE_ECONOMICA) 
cv.test(dadosQV$SEXO, dadosQV$EMPREGO) 
cv.test(dadosQV$SEXO, dadosQV$CRENCA) 
cv.test(dadosQV$SEXO, dadosQV$FUMA) 
cv.test(dadosQV$SEXO, dadosQV$Alcool_atual) 
cv.test(dadosQV$SEXO, dadosQV$DROGAS_ILICITAS) 
cv.test(dadosQV$SEXO, dadosQV$Risco_HIV_dic) #0.31
cv.test(dadosQV$SEXO, dadosQV$WHO_dic) 
cv.test(dadosQV$SEXO, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$SEXO, dadosQV$Depressao_dic) 
cv.test(dadosQV$SEXO, dadosQV$Ans_Dep) 
cv.test(dadosQV$SEXO, dadosQV$DOENCADIC) 
cv.test(dadosQV$SEXO, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$SEXO, dadosQV$CD4_DIC) 
cv.test(dadosQV$SEXO, dadosQV$CV_100MIL)
cv.test(dadosQV$SEXO, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$SEXO, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$SEXO, dadosQV$Adesao_dic) 
cv.test(dadosQV$SEXO, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$SEXO, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$SEXO, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$SEXO, dadosQV$TROCA) 

#Idade
cv.test(dadosQV$Idade_dic, dadosQV$Civil_dic) 
cv.test(dadosQV$Idade_dic, dadosQV$Raca_dic)
cv.test(dadosQV$Idade_dic, dadosQV$Escol_faixa) 
cv.test(dadosQV$Idade_dic, dadosQV$MORAJUNTO) 
cv.test(dadosQV$Idade_dic, dadosQV$FILHO) #0.31
cv.test(dadosQV$Idade_dic, dadosQV$PLANO) 
cv.test(dadosQV$Idade_dic, dadosQV$CLASSE_ECONOMICA) 
cv.test(dadosQV$Idade_dic, dadosQV$EMPREGO) 
cv.test(dadosQV$Idade_dic, dadosQV$CRENCA) 
cv.test(dadosQV$Idade_dic, dadosQV$FUMA) 
cv.test(dadosQV$Idade_dic, dadosQV$Alcool_atual) 
cv.test(dadosQV$Idade_dic, dadosQV$DROGAS_ILICITAS) 
cv.test(dadosQV$Idade_dic, dadosQV$Risco_HIV_dic) 
cv.test(dadosQV$Idade_dic, dadosQV$WHO_dic) 
cv.test(dadosQV$Idade_dic, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$Idade_dic, dadosQV$Depressao_dic) 
cv.test(dadosQV$Idade_dic, dadosQV$DOENCADIC) 
cv.test(dadosQV$Idade_dic, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$Idade_dic, dadosQV$CD4_DIC) 
cv.test(dadosQV$Idade_dic, dadosQV$CV_100MIL) 
cv.test(dadosQV$Idade_dic, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$Idade_dic, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$Idade_dic, dadosQV$Adesao_dic) 
cv.test(dadosQV$Idade_dic, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$Idade_dic, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$Idade_dic, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$Idade_dic, dadosQV$TROCA) 

#Civil
cv.test(dadosQV$Civil_dic, dadosQV$Raca_dic) 
cv.test(dadosQV$Civil_dic, dadosQV$Escol_faixa) 
cv.test(dadosQV$Civil_dic, dadosQV$MORAJUNTO) 
cv.test(dadosQV$Civil_dic, dadosQV$FILHO) 
cv.test(dadosQV$Civil_dic, dadosQV$PLANO) 
cv.test(dadosQV$Civil_dic, dadosQV$CLASSE_ECONOMICA) 
cv.test(dadosQV$Civil_dic, dadosQV$EMPREGO) 
cv.test(dadosQV$Civil_dic, dadosQV$CRENCA) 
cv.test(dadosQV$Civil_dic, dadosQV$FUMA) 
cv.test(dadosQV$Civil_dic, dadosQV$Alcool_atual) 
cv.test(dadosQV$Civil_dic, dadosQV$DROGAS_ILICITAS) 
cv.test(dadosQV$Civil_dic, dadosQV$Risco_HIV_dic) 
cv.test(dadosQV$Civil_dic, dadosQV$WHO_dic) 
cv.test(dadosQV$Civil_dic, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$Civil_dic, dadosQV$Depressao_dic) 
cv.test(dadosQV$Civil_dic, dadosQV$DOENCADIC) 
cv.test(dadosQV$Civil_dic, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$Civil_dic, dadosQV$CD4_DIC) 
cv.test(dadosQV$Civil_dic, dadosQV$CV_100MIL) 
cv.test(dadosQV$Civil_dic, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$Civil_dic, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$Civil_dic, dadosQV$Adesao_dic) 
cv.test(dadosQV$Civil_dic, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$Civil_dic, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$Civil_dic, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$Civil_dic, dadosQV$TROCA) 

#Raça
cv.test(dadosQV$Raca_dic, dadosQV$Escol_faixa) 
cv.test(dadosQV$Raca_dic, dadosQV$MORAJUNTO) 
cv.test(dadosQV$Raca_dic, dadosQV$FILHO) 
cv.test(dadosQV$Raca_dic, dadosQV$PLANO) 
cv.test(dadosQV$Raca_dic, dadosQV$CLASSE_ECONOMICA) 
cv.test(dadosQV$Raca_dic, dadosQV$EMPREGO) 
cv.test(dadosQV$Raca_dic, dadosQV$CRENCA) 
cv.test(dadosQV$Raca_dic, dadosQV$FUMA) 
cv.test(dadosQV$Raca_dic, dadosQV$Alcool_atual) 
cv.test(dadosQV$Raca_dic, dadosQV$DROGAS_ILICITAS) 
cv.test(dadosQV$Raca_dic, dadosQV$Risco_HIV_dic)
cv.test(dadosQV$Raca_dic, dadosQV$WHO_dic) 
cv.test(dadosQV$Raca_dic, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$Raca_dic, dadosQV$Depressao_dic) 
cv.test(dadosQV$Raca_dic, dadosQV$DOENCADIC) 
cv.test(dadosQV$Raca_dic, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$Raca_dic, dadosQV$CD4_DIC) 
cv.test(dadosQV$Raca_dic, dadosQV$CV_100MIL) 
cv.test(dadosQV$Raca_dic, dadosQV$STR_DTG_MM)
cv.test(dadosQV$Raca_dic, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$Raca_dic, dadosQV$Adesao_dic) 
cv.test(dadosQV$Raca_dic, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$Raca_dic, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$Raca_dic, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$Raca_dic, dadosQV$TROCA) 

#Escolaridade
cv.test(dadosQV$Escol_faixa, dadosQV$MORAJUNTO) 
cv.test(dadosQV$Escol_faixa, dadosQV$FILHO) #0.38
cv.test(dadosQV$Escol_faixa, dadosQV$PLANO) #0.27
cv.test(dadosQV$Escol_faixa, dadosQV$CLASSE_ECONOMICA) #0.34
cv.test(dadosQV$Escol_faixa, dadosQV$EMPREGO) #0.25
cv.test(dadosQV$Escol_faixa, dadosQV$CRENCA) 
cv.test(dadosQV$Escol_faixa, dadosQV$FUMA) 
cv.test(dadosQV$Escol_faixa, dadosQV$Alcool_atual) 
cv.test(dadosQV$Escol_faixa, dadosQV$DROGAS_ILICITAS) 
cv.test(dadosQV$Escol_faixa, dadosQV$Risco_HIV_dic) #0.30
cv.test(dadosQV$Escol_faixa, dadosQV$WHO_dic) 
cv.test(dadosQV$Escol_faixa, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$Escol_faixa, dadosQV$Depressao_dic) 
cv.test(dadosQV$Escol_faixa, dadosQV$DOENCADIC) 
cv.test(dadosQV$Escol_faixa, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$Escol_faixa, dadosQV$CD4_DIC) 
cv.test(dadosQV$Escol_faixa, dadosQV$CV_100MIL) 
cv.test(dadosQV$Escol_faixa, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$Escol_faixa, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$Escol_faixa, dadosQV$Adesao_dic) 
cv.test(dadosQV$Escol_faixa, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$Escol_faixa, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$Escol_faixa, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$Escol_faixa, dadosQV$TROCA) 

#MORA JUNTO
cv.test(dadosQV$MORAJUNTO, dadosQV$FILHO) 
cv.test(dadosQV$MORAJUNTO, dadosQV$PLANO) 
cv.test(dadosQV$MORAJUNTO, dadosQV$CLASSE_ECONOMICA) 
cv.test(dadosQV$MORAJUNTO, dadosQV$EMPREGO) 
cv.test(dadosQV$MORAJUNTO, dadosQV$CRENCA) 
cv.test(dadosQV$MORAJUNTO, dadosQV$FUMA) 
cv.test(dadosQV$MORAJUNTO, dadosQV$Alcool_atual) 
cv.test(dadosQV$MORAJUNTO, dadosQV$DROGAS_ILICITAS) 
cv.test(dadosQV$MORAJUNTO, dadosQV$Risco_HIV_dic) 
cv.test(dadosQV$MORAJUNTO, dadosQV$WHO_dic) 
cv.test(dadosQV$MORAJUNTO, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$MORAJUNTO, dadosQV$Depressao_dic) 
cv.test(dadosQV$MORAJUNTO, dadosQV$DOENCADIC) 
cv.test(dadosQV$MORAJUNTO, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$MORAJUNTO, dadosQV$CD4_DIC) 
cv.test(dadosQV$MORAJUNTO, dadosQV$CV_100MIL) 
cv.test(dadosQV$MORAJUNTO, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$MORAJUNTO, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$MORAJUNTO, dadosQV$Adesao_dic) 
cv.test(dadosQV$MORAJUNTO, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$MORAJUNTO, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$MORAJUNTO, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$MORAJUNTO, dadosQV$TROCA) 

#Filhos
cv.test(dadosQV$FILHO, dadosQV$PLANO) 
 cv.test(dadosQV$FILHO, dadosQV$CLASSE_ECONOMICA)
cv.test(dadosQV$FILHO, dadosQV$EMPREGO) 
cv.test(dadosQV$FILHO, dadosQV$CRENCA) 
cv.test(dadosQV$FILHO, dadosQV$FUMA) 
cv.test(dadosQV$FILHO, dadosQV$Alcool_atual) 
cv.test(dadosQV$FILHO, dadosQV$DROGAS_ILICITAS) 
cv.test(dadosQV$FILHO, dadosQV$Risco_HIV_dic) #0.38
cv.test(dadosQV$FILHO, dadosQV$WHO_dic) 
cv.test(dadosQV$FILHO, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$FILHO, dadosQV$Depressao_dic) 
cv.test(dadosQV$FILHO, dadosQV$DOENCADIC) 
cv.test(dadosQV$FILHO, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$FILHO, dadosQV$CD4_DIC) 
cv.test(dadosQV$FILHO, dadosQV$CV_100MIL) 
cv.test(dadosQV$FILHO, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$FILHO, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$FILHO, dadosQV$Adesao_dic) 
cv.test(dadosQV$FILHO, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$FILHO, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$FILHO, dadosQV$QTD_RA_Dic)
cv.test(dadosQV$FILHO, dadosQV$TROCA) 

#Plano 
cv.test(dadosQV$PLANO, dadosQV$CLASSE_ECONOMICA) #0.24
cv.test(dadosQV$PLANO, dadosQV$EMPREGO) #0.17
cv.test(dadosQV$PLANO, dadosQV$CRENCA) 
cv.test(dadosQV$PLANO, dadosQV$FUMA) 
cv.test(dadosQV$PLANO, dadosQV$Alcool_atual) 
cv.test(dadosQV$PLANO, dadosQV$DROGAS_ILICITAS) 
cv.test(dadosQV$PLANO, dadosQV$Risco_HIV_dic) 
cv.test(dadosQV$PLANO, dadosQV$WHO_dic) 
cv.test(dadosQV$PLANO, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$PLANO, dadosQV$Depressao_dic) 
cv.test(dadosQV$PLANO, dadosQV$DOENCADIC) 
cv.test(dadosQV$PLANO, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$PLANO, dadosQV$CD4_DIC) 
cv.test(dadosQV$PLANO, dadosQV$CV_100MIL) 
cv.test(dadosQV$PLANO, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$PLANO, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$PLANO, dadosQV$Adesao_dic) 
cv.test(dadosQV$PLANO, dadosQV$DIFICULDADE_DIC)
cv.test(dadosQV$PLANO, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$PLANO, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$PLANO, dadosQV$TROCA)

#Socioeconômica
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$EMPREGO) #0.22
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$CRENCA) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$FUMA)
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$Alcool_atual) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$DROGAS_ILICITAS) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$Risco_HIV_dic) #0.20
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$WHO_dic) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$Depressao_dic) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$DOENCADIC) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$CD4_DIC) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$CV_100MIL) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$Adesao_dic) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$CLASSE_ECONOMICA, dadosQV$TROCA)

#eMPREGO
cv.test(dadosQV$EMPREGO, dadosQV$CRENCA) 
cv.test(dadosQV$EMPREGO, dadosQV$FUMA) 
cv.test(dadosQV$EMPREGO, dadosQV$Alcool_atual) 
cv.test(dadosQV$EMPREGO, dadosQV$DROGAS_ILICITAS) 
cv.test(dadosQV$EMPREGO, dadosQV$Risco_HIV_dic) 
cv.test(dadosQV$EMPREGO, dadosQV$WHO_dic) 
cv.test(dadosQV$EMPREGO, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$EMPREGO, dadosQV$Depressao_dic) 
cv.test(dadosQV$EMPREGO, dadosQV$DOENCADIC) 
cv.test(dadosQV$EMPREGO, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$EMPREGO, dadosQV$CD4_DIC) 
cv.test(dadosQV$EMPREGO, dadosQV$CV_100MIL) 
cv.test(dadosQV$EMPREGO, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$EMPREGO, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$EMPREGO, dadosQV$Adesao_dic) 
cv.test(dadosQV$EMPREGO, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$EMPREGO, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$EMPREGO, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$EMPREGO, dadosQV$TROCA) 


#cRENÇA
cv.test(dadosQV$CRENCA, dadosQV$FUMA) 
cv.test(dadosQV$CRENCA, dadosQV$Alcool_atual) 
cv.test(dadosQV$CRENCA, dadosQV$DROGAS_ILICITAS) 
cv.test(dadosQV$CRENCA, dadosQV$Risco_HIV_dic) 
cv.test(dadosQV$CRENCA, dadosQV$WHO_dic) 
cv.test(dadosQV$CRENCA, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$CRENCA, dadosQV$Depressao_dic) 
cv.test(dadosQV$CRENCA, dadosQV$DOENCADIC) 
cv.test(dadosQV$CRENCA, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$CRENCA, dadosQV$CD4_DIC) 
cv.test(dadosQV$CRENCA, dadosQV$CV_100MIL) 
cv.test(dadosQV$CRENCA, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$CRENCA, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$CRENCA, dadosQV$Adesao_dic) 
cv.test(dadosQV$CRENCA, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$CRENCA, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$CRENCA, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$CRENCA, dadosQV$TROCA) 

#FUMA
cv.test(dadosQV$FUMA, dadosQV$Alcool_atual) 
cv.test(dadosQV$FUMA, dadosQV$DROGAS_ILICITAS) #0.22
cv.test(dadosQV$FUMA, dadosQV$Risco_HIV_dic) 
cv.test(dadosQV$FUMA, dadosQV$WHO_dic) 
cv.test(dadosQV$FUMA, dadosQV$Ansiedade_dic)
cv.test(dadosQV$FUMA, dadosQV$Depressao_dic) 
cv.test(dadosQV$FUMA, dadosQV$DOENCADIC) 
cv.test(dadosQV$FUMA, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$FUMA, dadosQV$CD4_DIC) 
cv.test(dadosQV$FUMA, dadosQV$CV_100MIL) 
cv.test(dadosQV$FUMA, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$FUMA, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$FUMA, dadosQV$Adesao_dic) 
cv.test(dadosQV$FUMA, dadosQV$DIFICULDADE_DIC)
cv.test(dadosQV$FUMA, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$FUMA, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$FUMA, dadosQV$TROCA) 

#aLCOOL
cv.test(dadosQV$Alcool_atual, dadosQV$DROGAS_ILICITAS) 
cv.test(dadosQV$Alcool_atual, dadosQV$Risco_HIV_dic) 
cv.test(dadosQV$Alcool_atual, dadosQV$WHO_dic) 
cv.test(dadosQV$Alcool_atual, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$Alcool_atual, dadosQV$Depressao_dic) 
cv.test(dadosQV$Alcool_atual, dadosQV$DOENCADIC) 
cv.test(dadosQV$Alcool_atual, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$Alcool_atual, dadosQV$CD4_DIC) 
cv.test(dadosQV$Alcool_atual, dadosQV$CV_100MIL) 
cv.test(dadosQV$Alcool_atual, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$Alcool_atual, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$Alcool_atual, dadosQV$Adesao_dic)
cv.test(dadosQV$Alcool_atual, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$Alcool_atual, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$Alcool_atual, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$Alcool_atual, dadosQV$TROCA) 

#Drogas
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$Risco_HIV_dic) 
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$WHO_dic) 
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$Depressao_dic) 
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$DOENCADIC) 
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$CD4_DIC) 
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$CV_100MIL) 
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$Adesao_dic) 
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$DIFICULDADE_DIC)
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$DROGAS_ILICITAS, dadosQV$TROCA) 


#Risco HIV
cv.test(dadosQV$Risco_HIV_dic, dadosQV$WHO_dic) 
cv.test(dadosQV$Risco_HIV_dic, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$Risco_HIV_dic, dadosQV$Depressao_dic) 
cv.test(dadosQV$Risco_HIV_dic, dadosQV$DOENCADIC)
cv.test(dadosQV$Risco_HIV_dic, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$Risco_HIV_dic, dadosQV$CD4_DIC) 
cv.test(dadosQV$Risco_HIV_dic, dadosQV$CV_100MIL) 
cv.test(dadosQV$Risco_HIV_dic, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$Risco_HIV_dic, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$Risco_HIV_dic, dadosQV$Adesao_dic)
cv.test(dadosQV$Risco_HIV_dic, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$Risco_HIV_dic, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$Risco_HIV_dic, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$Risco_HIV_dic, dadosQV$TROCA) 

#WHO dic
cv.test(dadosQV$WHO_dic, dadosQV$Ansiedade_dic) 
cv.test(dadosQV$WHO_dic, dadosQV$Depressao_dic) 
cv.test(dadosQV$WHO_dic, dadosQV$DOENCADIC) 
cv.test(dadosQV$WHO_dic, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$WHO_dic, dadosQV$CD4_DIC3) #0.35
cv.test(dadosQV$WHO_dic, dadosQV$CV_100MIL) #0.22
cv.test(dadosQV$WHO_dic, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$WHO_dic, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$WHO_dic, dadosQV$Adesao_dic)
cv.test(dadosQV$WHO_dic, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$WHO_dic, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$WHO_dic, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$WHO_dic, dadosQV$TROCA) 

#Ansiedade
cv.test(dadosQV$Ansiedade_dic, dadosQV$Depressao_dic) #0.34

#cOMORBIDADES
cv.test(dadosQV$DOENCADIC, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$DOENCADIC, dadosQV$CD4_DIC)
cv.test(dadosQV$DOENCADIC, dadosQV$CV_100MIL) 
cv.test(dadosQV$DOENCADIC, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$DOENCADIC, dadosQV$T_TRAT_SICLOM_Dic)
cv.test(dadosQV$DOENCADIC, dadosQV$Adesao_dic) 
cv.test(dadosQV$DOENCADIC, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$DOENCADIC, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$DOENCADIC, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$DOENCADIC, dadosQV$TROCA) 

#Tempo diagnóstico
cv.test(dadosQV$TPOSITIVO_dic, dadosQV$CD4_DIC) 
cv.test(dadosQV$TPOSITIVO_dic, dadosQV$CV_100MIL) 
cv.test(dadosQV$TPOSITIVO_dic, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$TPOSITIVO_dic, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$TPOSITIVO_dic, dadosQV$Adesao_dic) 
cv.test(dadosQV$TPOSITIVO_dic, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$TPOSITIVO_dic, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$TPOSITIVO_dic, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$TPOSITIVO_dic, dadosQV$TROCA) 

#CD4
cv.test(dadosQV$CD4_DIC, dadosQV$CV_100MIL) #0.22
cv.test(dadosQV$CD4_DIC, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$CD4_DIC, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$CD4_DIC, dadosQV$Adesao_dic) 
cv.test(dadosQV$CD4_DIC, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$CD4_DIC, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$CD4_DIC, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$CD4_DIC, dadosQV$TROCA) 

#CV
cv.test(dadosQV$CV_100MIL, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$CV_100MIL, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$CV_100MIL, dadosQV$Adesao_dic) 
cv.test(dadosQV$CV_100MIL, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$CV_100MIL, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$CV_100MIL, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$CV_100MIL, dadosQV$TROCA) 

#Regimen
cv.test(dadosQV$STR_DTG_MM, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$STR_DTG_MM, dadosQV$Adesao_dic)
cv.test(dadosQV$STR_DTG_MM, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$STR_DTG_MM, dadosQV$REACOESADVSOMA_dic)
cv.test(dadosQV$STR_DTG_MM, dadosQV$QTD_RA_Dic)
cv.test(dadosQV$STR_DTG_MM, dadosQV$TROCA)

#Tempo de tratamento
cv.test(dadosQV$T_TRAT_SICLOM_Dic, dadosQV$Adesao_dic) 
cv.test(dadosQV$T_TRAT_SICLOM_Dic, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$T_TRAT_SICLOM_Dic, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$T_TRAT_SICLOM_Dic, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$T_TRAT_SICLOM_Dic, dadosQV$TROCA) 

cv.test(dadosQV$STR_DTG_MM, dadosQV$REACOESADVSOMA_dic)

#Adesao
cv.test(dadosQV$Adesao_dic, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$Adesao_dic, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$Adesao_dic, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$Adesao_dic, dadosQV$TROCA) 

#Dificuldade
cv.test(dadosQV$DIFICULDADE_DIC, dadosQV$REACOESADVSOMA_dic) 
cv.test(dadosQV$DIFICULDADE_DIC, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$DIFICULDADE_DIC, dadosQV$TROCA) 

#RA
cv.test(dadosQV$REACOESADVSOMA_dic, dadosQV$QTD_RA_Dic) 
cv.test(dadosQV$REACOESADVSOMA_dic, dadosQV$QTD_RA_Dic2) 
cv.test(dadosQV$REACOESADVSOMA_dic, dadosQV$TROCA) 

#dadosQV$QTD_RA_Dic2
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$IDADE) #0.2449564
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$Civil_dic) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$Raca_dic) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$Escol_faixa) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$MORAJUNTO)
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$FILHO) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$PLANO) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$CLASSE_ECONOMICA)
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$EMPREGO) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$CRENCA)
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$FUMA)
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$Alcool_atual) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$DROGAS_ILICITAS)
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$Ans_Dep) #0.3070133
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$DOENCADIC) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$CV_100MIL) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$Late) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$Local_TARV) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$ESQUEMA3) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$STR_DTG_MM) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$Adesao_dic)
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$DIFICULDADE_DIC) 
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$REACOESADVSOMA_dic) #0.2623839
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$QTD_RA_Dic) #0.4545846
cv.test(dadosQV$QTD_RA_Dic2, dadosQV$TROCA) 

#Ans_dep
cv.test(dadosQV$Ans_Dep, dadosQV$IDADE) #0.25
cv.test(dadosQV$Ans_Dep, dadosQV$Civil_dic) 
cv.test(dadosQV$Ans_Dep, dadosQV$Raca_dic) 
cv.test(dadosQV$Ans_Dep, dadosQV$Escol_faixa) 
cv.test(dadosQV$Ans_Dep, dadosQV$MORAJUNTO) 
cv.test(dadosQV$Ans_Dep, dadosQV$FILHO) 
cv.test(dadosQV$Ans_Dep, dadosQV$PLANO) 
cv.test(dadosQV$Ans_Dep, dadosQV$CLASSE_ECONOMICA) 
cv.test(dadosQV$Ans_Dep, dadosQV$EMPREGO) 
cv.test(dadosQV$Ans_Dep, dadosQV$CRENCA) 
cv.test(dadosQV$Ans_Dep, dadosQV$FUMA)
cv.test(dadosQV$Ans_Dep, dadosQV$Alcool_atual) 
cv.test(dadosQV$Ans_Dep, dadosQV$DROGAS_ILICITAS) 
cv.test(dadosQV$Ans_Dep, dadosQV$Risco_HIV_dic) 
cv.test(dadosQV$Ans_Dep, dadosQV$WHO_dic) 
cv.test(dadosQV$Ans_Dep, dadosQV$DOENCADIC) 
cv.test(dadosQV$Ans_Dep, dadosQV$TPOSITIVO_dic) 
cv.test(dadosQV$Ans_Dep, dadosQV$CV_100MIL) 
cv.test(dadosQV$Ans_Dep, dadosQV$ESQUEMA3) 
cv.test(dadosQV$Ans_Dep, dadosQV$STR_DTG_MM)
cv.test(dadosQV$Ans_Dep, dadosQV$T_TRAT_SICLOM_Dic) 
cv.test(dadosQV$Ans_Dep, dadosQV$Adesao_dic) 
cv.test(dadosQV$Ans_Dep, dadosQV$DIFICULDADE_DIC)

#####Univariada EQ5D

#Sexo
comp.sexo <-lme(INDICE_EQ5D ~ SEXO, random=~1|Serviço_dic, method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.sexo)
#Entra (p= 0.069)

#Risco_sexo
comp.rsexo <-lme(INDICE_EQ5D ~ Risco_HIV_dic, random=~1|Serviço_dic, method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.rsexo)

#Idade contínua
comp.idade <-lme(INDICE_EQ5D ~ IDADE,  random=~1|Serviço_dic, method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.idade) #Entra (p=0.0361)

#Civil
comp.civil <-lme(INDICE_EQ5D ~ Civil_dic, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.civil) #Entra (p=0.0792)

#Raça
comp.raca <-lme(INDICE_EQ5D ~ Raca_dic, random=~1|Serviço_dic,method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.raca) #Entra (p=0.178)

#Escolaridade
comp.escol <-lme(INDICE_EQ5D ~ Escol_faixa, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.escol) #Entra (p<0.05)

#Mora junto
comp.mora <-lme(INDICE_EQ5D ~ MORAJUNTO, random=~1|Serviço_dic,method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.mora) #Entra (p<0.05)

#Filho
comp.filho <-lme(INDICE_EQ5D ~ FILHO, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.filho) #Entra (p<0.05)

#Plano
comp.plano <-lme(INDICE_EQ5D ~ PLANO, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.plano) #Entra (p<0.05)

#Classe
comp.classe <-lme(INDICE_EQ5D ~ CLASSE_ECONOMICA, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.classe) #Entra (p<0.05)

#Emprego
comp.emprego <-lme(INDICE_EQ5D ~ EMPREGO, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.emprego) #Entra (p<0.05)

#Crença
comp.crenca <-lme(INDICE_EQ5D ~ CRENCA, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.crenca) #Entra (p=0.058)

#Fuma
comp.fuma <-lme(INDICE_EQ5D ~ FUMA, random=~1|Serviço_dic,method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.fuma) #Entra (p=0.013)

#Alcool atual
comp.alcool2 <-lme(INDICE_EQ5D ~ Alcool_atual, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.alcool2) #Entra (p<0.05)

#Drogas ilícitas
comp.drogas <-lme(INDICE_EQ5D ~ DROGAS_ILICITAS, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.drogas) # Não entra (p=0.363)

#Risco HIV
comp.risco <-lme(INDICE_EQ5D ~ Risco_HIV_dic, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.risco) #Entra (p<0.05)

#Ansiedade
comp.ans <-lme(INDICE_EQ5D ~ Ansiedade_dic, random=~1|Serviço_dic,method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.ans) #Entra (p<0.05)

#Depressão
comp.dep <-lme(INDICE_EQ5D ~ Depressao_dic, random=~1|Serviço_dic,method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.dep) #Entra (p<0.05)

#Ansiedade ou depressão 
comp.ansdep <-lme(INDICE_EQ5D ~ Ans_Dep, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.ansdep) #Entra (p<0.05)

#Comorbidades
comp.comorb <-lme(INDICE_EQ5D ~ DOENCADIC, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.comorb) #Entra (p<0.05)

#Classificação clínica
comp.cc <-lme(INDICE_EQ5D ~ WHO_dic, random=~1|Serviço_dic,method = "ML",
              na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.cc) #Entra (p=0.091)

#CD4 inicial
comp.cd4 <-lme(INDICE_EQ5D ~  CD4_DIC3, random=~1|Serviço_dic,method = "ML",
              na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.cd4) #Entra (p=0.091)

#CV inicial
comp.cv <-lme(INDICE_EQ5D ~ CV_100MIL, random=~1|Serviço_dic,method = "ML",
              na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.cv) #Não entra (p=0.427)

#Tempo diagnóstico
comp.td <-lme(INDICE_EQ5D ~ TPOSITIVO_dic, random=~1|Serviço_dic,method = "ML",
              na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.td) #Não entra (p=0.4365)

#cART
comp.cART <-lme(INDICE_EQ5D ~ STR_DTG_MM, random=~1|Serviço_dic,method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.cART) #Ajuste (p>0.264)

#Tempo tratamento
comp.ttrat <-lme(INDICE_EQ5D ~ T_TRAT_SICLOM_mo, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.ttrat) # Não Entra (p=0.884)

#Adesao
comp.adesao <-lme(INDICE_EQ5D ~ Adesao_dic, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.adesao) # Não Entra (p=0.228)

#Dificuldade
comp.dif <-lme(INDICE_EQ5D ~ DIFICULDADE_DIC, random=~1|Serviço_dic,method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.dif) #Entra (p<0.05)

#RA
comp.RA <-lme(INDICE_EQ5D ~ REACOESADVSOMA_dic, random=~1|Serviço_dic,method = "ML",
              na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.RA) #Entra (p<0.05)

#cd4
comp.cd4 <- lme(INDICE_EQ5D ~ CD4_DIC, random=~1|Serviço_dic,method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.cd4) #Entra (p<0.05)

#TROCA
comp.troc <- lme(INDICE_EQ5D ~ TROCA, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.troc) #Entra (p<0.05)

#QTD REACAo 
comp.qtdra <- lme(INDICE_EQ5D ~ QTD_RA_Dic2, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp.qtdra) #Entra (p<0.05)


#####Multivariada EQ5D

#Entraram as desbalanceadas no baseline (aids, tempor trat, Ra), esquema e as com p valor <0.20 na univariada
# Sairam CLASSE_ECONOMICA + EMPREGO + PLANO por serem correlacionadas com escolaridade, categoria de risco por relacionar com sexo, 
#e filho por correlacionar com idade, sexo e escolaridade. Reacao adversao ao inves de qtd de reacao adversa. Tirei mora junto 

mlm_eq <- c("INDICE_EQ5D", "Serviço_dic", "WHO_dic", "CD4_DIC3", "T_TRAT_SICLOM", "T_TRAT_SICLOM_mo", "DIFICULDADE_DIC", 'REACOESADVSOMA_dic', "STR_DTG_MM", "SEXO",
            "IDADE", "Civil_dic","Escol_faixa", "CRENCA", "FUMA", "Alcool_atual","Ans_Dep", "DOENCADIC", "CLASSE_ECONOMICA", 
            "EMPREGO", "PLANO", "FILHO", "MORAJUNTO")

dados_eq <-dadosQV[,mlm_eq]
dados_mlm.eq <-  na.exclude(dados_eq) #remove NA

Model.1a <-lme(INDICE_EQ5D ~ WHO_dic + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 SEXO + IDADE + Civil_dic + Escol_faixa + CRENCA + FUMA + Alcool_atual + Ans_Dep  + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.1a)

#Sai IDADE
Model.2a <-lme(INDICE_EQ5D ~ WHO_dic + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 SEXO + Civil_dic + Escol_faixa + CRENCA + FUMA + Alcool_atual + Ans_Dep  + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.2a)


#Sai SEXO
Model.3a <-lme(INDICE_EQ5D ~ WHO_dic + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 Civil_dic + Escol_faixa + CRENCA + FUMA + Alcool_atual + Ans_Dep  + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.3a)

#Sai crença
Model.4a <-lme(INDICE_EQ5D ~ WHO_dic + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 Civil_dic + Escol_faixa + FUMA + Alcool_atual + Ans_Dep  + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.4a)
#        AIC       BIC   logLik
#-625.1935 -567.6714 327.5968


#TESTE INCLUSAO DE VARIAVEIS

#1 
Model.1a_cc <-lme(INDICE_EQ5D ~ WHO_dic + T_TRAT_SICLOM_mo + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 SEXO + IDADE + Civil_dic + Escol_faixa + CRENCA + FUMA + Alcool_atual + Ans_Dep  + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dados_mlm.eq, control=list(opt="optim"))
summary(Model.1a_cc)

step.model <- stepAIC(Model.1a_cc, direction = "backward", 
                      trace = TRUE)
summary(step.model)
#       AIC       BIC  logLik
# -618.406 -572.7083 321.203

#2
Model.2a_cc <-lme(INDICE_EQ5D ~ CD4_DIC3 + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                    SEXO + IDADE + Civil_dic + Escol_faixa + CRENCA + FUMA + Alcool_atual + Ans_Dep  + DOENCADIC, #parte fixa
                  random=~1|Serviço_dic,                                                          #parte aleatoria
                  method = "ML",
                  na.action = "na.omit",data = dados_mlm.eq, control=list(opt="optim"))
summary(Model.2a_cc)

step.model2 <- stepAIC(Model.2a_cc, direction = "backward", 
                      trace = TRUE)
summary(step.model2)
#AIC       BIC  logLik
#-618.406 -572.7083 321.203
                               Value  Std.Error  DF  t-value p-value
DIFICULDADE_DICDifficult      -0.0169910 0.01190991 321 -1.42663  0.1547
REACOESADVSOMA_dicYes         -0.0361425 0.01526427 321 -2.36778  0.0185
Civil_dicmarried/stable union  0.0307158 0.01352136 321  2.27165  0.0238
Escol_faixa10-12               0.0258670 0.01426348 321  1.81351  0.0707
Escol_faixa13 or more          0.0524896 0.01479820 321  3.54703  0.0004
FUMAYes                       -0.0335604 0.01231030 321 -2.72620  0.0068
Alcool_atualYes                0.0427334 0.01156870 321  3.69388  0.0003
Ans_DepYes                    -0.0774558 0.01129524 321 -6.85739  0.0000
DOENCADICYes                  -0.0346741 0.01405668 321 -2.46673  0.0142


anova(Model.1a_cc,Model.2a_cc)

##Modelo final EQ5-5D 
VarCorr(Model.4a)
#ICC 
1.303989e-07 / ( 1.303989e-07 +  8.620064e-03) # 1.512714e-05 

#Verifica ajuste do modelo
standardized_4a = as.data.frame(scale(Model.4a$residuals))
standardized_4a = standardized_4a$fixed
fitted = scale(fitted.values(Model.4a))

qqnorm(standardized_4a)  #Normalidade apenas dos resíduos da parte fixa
abline(0,1,col="red",lwd=2)

hist(standardized_5a) #Histograma dos resíduos parte fixa

#####Univariada VAS

#Sexo
comp2.sexo <-lme(Q6 ~ SEXO, random=~1|Serviço_dic, method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.sexo) #Não entra (p= 0.4468)

#Risco_sexo
comp2.rsexo <-lme(Q6 ~ Risco_HIV_dic, random=~1|Serviço_dic, method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.rsexo) #Não entra(0,389)

#Idade contínua
comp2.idade <-lme(Q6 ~ IDADE,  random=~1|Serviço_dic, method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.idade) #Não entra (p=0.519)

#Civil
comp2.civil <-lme(Q6 ~ Civil_dic, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.civil) #Não entra (p=0.366)

#Raça
comp2.raca <-lme(Q6 ~ Raca_dic, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.raca) #Não entra (p=0.216)

#Escolaridade
comp2.escol <-lme(Q6 ~ Escol_faixa, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.escol) #Entra (p<0.05)

#Mora junto
comp2.mora <-lme(Q6 ~ MORAJUNTO, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.mora) #Não entra (0,235)

#Filho
comp2.filho <-lme(Q6 ~ FILHO, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.filho) #Entra (p=0,155)

#Plano
comp2.plano <-lme(Q6 ~ PLANO, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.plano) #Não Entra (p=0,552)

#Classe
comp2.classe <-lme(Q6 ~ CLASSE_ECONOMICA, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.classe) #Entra (p<0.05)

#Emprego
comp2.emprego <-lme(Q6 ~ EMPREGO, random=~1|Serviço_dic,method = "ML",
                    na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.emprego) #Entra (p<0.05)

#Crença
comp2.crenca <-lme(Q6 ~ CRENCA, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.crenca) #Entra (p=0.0718)

#Fuma
comp2.fuma <-lme(Q6 ~ FUMA, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.fuma) #Não Entra (p=0,241)

#Alcool atual
comp2.alcool2 <-lme(Q6 ~ Alcool_atual, random=~1|Serviço_dic,method = "ML",
                    na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.alcool2) #Entra (p<0.05)

#Drogas ilícitas
comp2.drogas <-lme(Q6 ~ DROGAS_ILICITAS, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.drogas) # Não entra (p=0.582)

#Risco HIV
comp2.risco <-lme(Q6 ~ Risco_HIV_dic, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.risco) #Não Entra (p=0,389)

#Ansiedade
comp2.ans <-lme(Q6 ~ Ansiedade_dic, random=~1|Serviço_dic,method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.ans) #Entra (p<0.05)

#Depressão
comp2.dep <-lme(Q6 ~ Depressao_dic, random=~1|Serviço_dic,method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.dep) #Entra (p<0.05)

#Ansiedade ou depressão 
comp2.ansdep <-lme(Q6 ~ Ans_Dep, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.ansdep) #Entra (p<0.05)

#Comorbidades
comp2.comorb <-lme(Q6 ~ DOENCADIC, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.comorb) #Entra (p=0.054)

#Classificação clínica
comp2.cc <-lme(Q6 ~ WHO_dic, random=~1|Serviço_dic,method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.cc) #Entra (p=0.0025)

#CD4 inicial
comp2.cd4 <-lme(Q6 ~  CD4_DIC3, random=~1|Serviço_dic,method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.cd4) #Entra (p=0.013)

#CV inicial
comp2.cv <-lme(Q6 ~ CV_100MIL, random=~1|Serviço_dic,method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.cv) #Não entra (p=0.675)

#Tempo diagnóstico
comp2.td <-lme(Q6 ~ TPOSITIVO_dic, random=~1|Serviço_dic,method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.td) #Não entra (p=0.0469)

#cART
comp2.cART <-lme(Q6 ~ STR_DTG_MM, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.cART) #Ajuste (p=0,100)

#Tempo tratamento
comp2.ttrat <-lme(Q6 ~ T_TRAT_SICLOM_mo, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.ttrat) # Ajuste (p=0.0447)

#Adesao
comp2.adesao <-lme(Q6 ~ Adesao_dic, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.adesao) # Não Entra (p=0.635)

#Dificuldade
comp2.dif <-lme(Q6 ~ DIFICULDADE_DIC, random=~1|Serviço_dic,method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.dif) #Entra (p<0.05)

#RA
comp2.RA <-lme(Q6 ~ REACOESADVSOMA_dic, random=~1|Serviço_dic,method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.RA) #Entra (p<0.05)

#cd4
comp2.cd4 <- lme(Q6 ~ CD4_DIC, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.cd4) #Entra (p<0.05)

#TROCA
comp2.troc <- lme(Q6 ~ TROCA, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.troc) #Entra (p=0.09)

#QTD REACAo 
comp2.qtdra <- lme(Q6 ~ QTD_RA_Dic2, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp2.qtdra) #Entra (p<0.05)

#####Multivariada VAS

#Entraram as desbalanceadas no baseline (aids, tempor trat, Ra), esquema e as com p valor <0.20 na univariada
# Sairam CLASSE_ECONOMICA + EMPREGO por serem correlacionadas com escolaridade, categoria de risco por relacionar com sexo, 
#e filho por correlacionar com escolaridade. Reacao adversao ao inves de qtd de reacao adversa. aIDS ao inves de cd4

mlm_vas <- c("Q6", "Serviço_dic", "WHO_dic", "CD4_DIC3", "T_TRAT_SICLOM_mo", "DIFICULDADE_DIC", 'REACOESADVSOMA_dic', "STR_DTG_MM",
            "Civil_dic","Escol_faixa", "CRENCA", "FUMA", "Alcool_atual","Ans_Dep", "TPOSITIVO_dic","DOENCADIC", "CLASSE_ECONOMICA", 
            "EMPREGO", "PLANO", "FILHO")

dados_vas <-dadosQV[,mlm_vas]
dados_mlm.vas <-  na.exclude(dados_vas) #remove NA


Model.1b <-lme(Q6 ~ WHO_dic + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 Escol_faixa + CRENCA + Alcool_atual + TPOSITIVO_dic +  Ans_Dep  + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.1b)

#sAI ESCOLARIDADE
Model.2b <-lme(Q6 ~ WHO_dic + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 CRENCA + Alcool_atual + TPOSITIVO_dic +  Ans_Dep  + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.2b)

#sAI Tempo diag
Model.3b <-lme(Q6 ~ WHO_dic + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 CRENCA + Alcool_atual + Ans_Dep  + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.3b)

#sAI Co-morb
Model.4b <-lme(Q6 ~ WHO_dic + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 CRENCA + Alcool_atual + Ans_Dep, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.4b)
anova()

#sAI Crenca
Model.6b <-lme(Q6 ~ WHO_dic + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 Alcool_atual + Ans_Dep, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.6b)

#sAI alcool
Model.7b <-lme(Q6 ~ WHO_dic + T_TRAT_SICLOM_mo + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 Ans_Dep, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.7b)
#AIC    BIC    logLik
#2866.313 2900.8 -1424.157

#Teste melhor ajuste
#1
Model.1b_cc <-lme(Q6 ~ WHO_dic + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 Escol_faixa + CRENCA + Alcool_atual + TPOSITIVO_dic +  Ans_Dep  + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dados_mlm.vas, control=list(opt="optim"))
summary(Model.1b_cc)

step.model1b <- stepAIC(Model.1b_cc, direction = "backward", 
                       trace = TRUE)
summary(step.model1b)
#AIC      BIC    logLik
#2788.113 2822.359

#2
Model.2b_cc <-lme(Q6 ~ CD4_DIC3 + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                    Escol_faixa + CRENCA + Alcool_atual + TPOSITIVO_dic +  Ans_Dep  + DOENCADIC, #parte fixa
                  random=~1|Serviço_dic,                                                          #parte aleatoria
                  method = "ML",
                  na.action = "na.omit",data = dados_mlm.vas, control=list(opt="optim"))
summary(Model.2b_cc)

step.model2b <- stepAIC(Model.2b_cc, direction = "backward", 
                        trace = TRUE)
summary(step.model2b)
#AIC      BIC    logLik
#2788.325 2830.181



##Modelo final VAS
VarCorr(Model.7b)
#ICC 
1.450808e-07 / ( 1.450808e-07 +  8.512049e-03) # 1.704388e-05 

#Verifica ajuste do modelo
standardized_7b = as.data.frame(scale(Model.7b$residuals))
standardized_7b = standardized_7b$fixed
fitted = scale(fitted.values(Model.7b))

qqnorm(standardized_7b)  #Normalidade apenas dos resíduos da parte fixa
abline(0,1,col="red",lwd=2)

hist(standardized_7b) #Histograma dos resíduos parte fixa

plot(fitted, standardized_7b)
abline(0,0,col="red",lwd=2)
abline(v=0)

#####Univariada QV_geral
#Sexo
comp3.sexo <-lme(QV_geral ~ SEXO, random=~1|Serviço_dic, method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.sexo) #Entra (p= 0.003)

#Risco_sexo
comp3.rsexo <-lme(QV_geral ~ Risco_HIV_dic, random=~1|Serviço_dic, method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.rsexo) #Nao entra (p=0.296)

#Idade contínua
comp3.idade <-lme(QV_geral ~ IDADE,  random=~1|Serviço_dic, method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.idade) #Entra (p=0.4473)

#Civil
comp3.civil <-lme(QV_geral ~ Civil_dic, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.civil) #Entra (p=0.04)

#Raça
comp3.raca <-lme(QV_geral ~ Raca_dic, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.raca) # Nao Entra (p=0.649)

#Escolaridade
comp3.escol <-lme(QV_geral ~ Escol_faixa, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.escol) #Entra (p<0.05)

#Mora junto
comp3.mora <-lme(QV_geral ~ MORAJUNTO, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.mora) #Nao entra (p=0.340)

#Filho
comp3.filho <-lme(QV_geral ~ FILHO, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.filho) #Entra (p=0.183)

#Plano
comp3.plano <-lme(QV_geral ~ PLANO, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.plano) #Nao entra (p=0.433)

#Classe
comp3.classe <-lme(QV_geral ~ CLASSE_ECONOMICA, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.classe) #Entra (p<0.05)

#Emprego
comp3.emprego <-lme(QV_geral ~ EMPREGO, random=~1|Serviço_dic,method = "ML",
                    na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.emprego) #Entra (p<0.05)

#Crença
comp3.crenca <-lme(QV_geral ~ CRENCA, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.crenca) #Nao entra (p=0.646)

#Fuma
comp3.fuma <-lme(QV_geral ~ FUMA, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.fuma) #Entra (p<0.05)

#Alcool atual
comp3.alcool2 <-lme(QV_geral ~ Alcool_atual, random=~1|Serviço_dic,method = "ML",
                    na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.alcool2) #Nao Entra (p=0.648)

#Drogas ilícitas
comp3.drogas <-lme(QV_geral ~ DROGAS_ILICITAS, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.drogas) # Não entra (p=0.349)

#Risco HIV
comp3.risco <-lme(QV_geral ~ Risco_HIV_dic, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.risco) #Nao entra (p=0.296)

#Ansiedade
comp3.ans <-lme(QV_geral ~ Ansiedade_dic, random=~1|Serviço_dic,method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.ans) #Entra (p<0.05)

#Depressão
comp3.dep <-lme(QV_geral ~ Depressao_dic, random=~1|Serviço_dic,method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.dep) #Entra (p<0.05)

#Ansiedade ou depressão 
comp3.ansdep <-lme(QV_geral ~ Ans_Dep, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.ansdep) #Entra (p<0.05)

#Comorbidades
comp3.comorb <-lme(QV_geral ~ DOENCADIC, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.comorb) #Entra (p<0.05)

#Classificação clínica
comp3.cc <-lme(QV_geral ~ WHO_dic, random=~1|Serviço_dic,method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.cc) #Ajuste (p=0.900)

#CD4 inicial
comp3.cd4 <-lme(QV_geral ~  CD4_DIC3, random=~1|Serviço_dic,method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.cd4) #Nao Entra (p=0.435)

#CV inicial
comp3.cv <-lme(QV_geral ~ CV_100MIL, random=~1|Serviço_dic,method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.cv) #Entra (p=0.09)

#Tempo diagnóstico
comp3.td <-lme(QV_geral ~ TPOSITIVO_dic, random=~1|Serviço_dic,method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.td) #Não entra (p=0.832)

#cART
comp3.cART <-lme(QV_geral ~ STR_DTG_MM, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.cART) #Ajuste (p>0.259)

#Tempo tratamento
comp3.ttrat <-lme(QV_geral ~ T_TRAT_SICLOM_mo, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.ttrat) #Ajuste (p=0.354)

#Adesao
comp3.adesao <-lme(QV_geral ~ Adesao_dic, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.adesao) # Não Entra (p=0.311)

#Dificuldade
comp3.dif <-lme(QV_geral ~ DIFICULDADE_DIC, random=~1|Serviço_dic,method = "ML",
                na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.dif) #Entra (p<0.05)

#RA
comp3.RA <-lme(QV_geral ~ REACOESADVSOMA_dic, random=~1|Serviço_dic,method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.RA) #Entra (p<0.05)

#cd4
comp3.cd4 <- lme(QV_geral ~ CD4_DIC, random=~1|Serviço_dic,method = "ML",
                 na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.cd4) #Entra (p<0.05)

#TROCA
comp3.troc <- lme(QV_geral ~ TROCA, random=~1|Serviço_dic,method = "ML",
                  na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.troc) #Nao entra (p=0.203)

#QTD REACAo 
comp3.qtdra <- lme(QV_geral ~ QTD_RA_Dic2, random=~1|Serviço_dic,method = "ML",
                   na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(comp3.qtdra) #Entra (p<0.07)


#####Multivariada QV geral

#Entraram as desbalanceadas no baseline (aids, tempor trat, Ra), esquema e as com p valor <0.20 na univariada
# Sairam CLASSE_ECONOMICA + EMPREGO por serem correlacionadas com escolaridade, categoria de risco por relacionar com sexo, 
#e filho por correlacionar com escolaridade. Reacao adversao ao inves de qtd de reacao adversa. aIDS ao inves de cd4

mlm_QV_geral <- c("QV_geral", "Serviço_dic", "WHO_dic", "CD4_DIC3", "T_TRAT_SICLOM_mo", "DIFICULDADE_DIC", 'REACOESADVSOMA_dic', "STR_DTG_MM",
             "Civil_dic", "SEXO", "Escol_faixa", "FUMA", "CV_100MIL", "Alcool_atual","Ans_Dep", "DOENCADIC", "CLASSE_ECONOMICA", 
             "EMPREGO", "FILHO")

dados_QVg <-dadosQV[,mlm_QV_geral]
dados_mlm.QVg <-  na.exclude(dados_QVg) #remove NA

Model.1c <-lme(QV_geral ~ WHO_dic + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 SEXO + Civil_dic + Escol_faixa + FUMA + CV_100MIL +  Ans_Dep  + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.1c)

#Sai sexo
Model.2c <-lme(QV_geral ~ WHO_dic + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 Civil_dic + Escol_faixa + FUMA + CV_100MIL +  Ans_Dep  + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.2c)

#Sai CV
Model.3c <-lme(QV_geral ~ WHO_dic + T_TRAT_SICLOM_mo + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 Civil_dic + Escol_faixa + FUMA + Ans_Dep + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.3c)

#Sai CV
Model.3c <-lme(QV_geral ~ WHO_dic + T_TRAT_SICLOM_mo + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 Civil_dic + Escol_faixa + FUMA + Ans_Dep + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               na.action = "na.omit",data = dadosQV, control=list(opt="optim"))
summary(Model.3c)
#AIC    BIC    logLik
#1672.112 1725.8 -822.0562

#Teste
#1
Model.1c_cc <-lme(QV_geral ~ WHO_dic + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                 SEXO + Civil_dic + Escol_faixa + FUMA + CV_100MIL +  Ans_Dep  + DOENCADIC, #parte fixa
               random=~1|Serviço_dic,                                                          #parte aleatoria
               method = "ML",
               data = dados_mlm.QVg, control=list(opt="optim"))
summary(Model.1c_cc)

step.model1c <- stepAIC(Model.1c_cc, direction = "backward", 
                        trace = TRUE)
summary(step.model1c)
#AIC      BIC   logLik
#1633.196 1671.337

#2
Model.2c_cc <-lme(QV_geral ~ CD4_DIC3 + T_TRAT_SICLOM + DIFICULDADE_DIC + REACOESADVSOMA_dic + STR_DTG_MM +
                    SEXO + Civil_dic + Escol_faixa + FUMA + CV_100MIL +  Ans_Dep  + DOENCADIC, #parte fixa
                  random=~1|Serviço_dic,                                                          #parte aleatoria
                  method = "ML",
                  data = dados_mlm.QVg, control=list(opt="optim"))
summary(Model.2c_cc)

step.model2c <- stepAIC(Model.2c_cc, direction = "backward", 
                        trace = TRUE)
summary(step.model2c)