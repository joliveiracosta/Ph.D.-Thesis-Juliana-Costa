#######################################
# Estudo 3 
# Análise de custo-efetividade de esquemas antirretrovirais na perspectiva do Sistema Único de Saúde 
# Autores: 
# Juliana de Oliveira Costa, Willings Botha, Sallie Anne Pearson, 
# Maria das Graças Braga Ceccato, Francisco de Assis Acurcio
# Contato: juliana.olic@gmail.com
#######################################

# Carregar pacotes

library(readxl)
library(ggplot2)
library(car)
library(plotly)
library(compareGroups)
library(Publish)
install.packages("Hmisc")
library(Hmisc)
install.packages("BCEA")
library("BCEA")
library(boot)
if(!require(Rmisc)){install.packages("Rmisc")}
library(Rmisc)
library(devtools)
library(easyGgplot2)
install.packages("margins")
library("margins")
install.packages("stats")
?power
#library(tidyverse)
library("sandwich")

#Leitura dos dados
setwd("C:\\Users\\z5202391\\Dropbox\\Doutorado_Juliana\\Artigo2\\Bancos_dados_scrip")


################################### Análise descritiva ################################################
data <- read_excel("juliana_19052017_reduzido.xlsx")

names(data)

var <- c("Sexo","IDADE_CAT", "CC_IN", "HepatiteBC", "V39", "int_ano_anterior_dic", "Inicio_TARV_local", "ANO_TARV", "IDADE","V7","Censura",
         "TARV_INITRA_CLASSE_DIC", "TARV_INITRA", "TARV_INITRA_CLASSE", "V47_STR", "v47_STR_dic", "tempo_diag_dic", "regitro_RA", "TROCA_ESQ","ESQ_TROCA", "CLASSE_TROCA", "Nao_adesao1_6m",
         "Nao_adesao1_12m", "int_acp_dic", "int_acp_2", "EST_CIVIL", "RACA", "DIG_SMI", "V32", "V33", "V6", "V9", "TEMPO_ACP_TOTAL","TEMPO_ACP_1_12M","OBITO_DIC", "int_acp_2_dic",
         "Risco_HIV_dic", "Risco_HIV", "DROGAS_ATUAL", "ESCOL", "V25", "V26", "EFT_12M", "EFT_6M", "DROGAS", "CV_IN_dic", "CD4_IN_dic", "CD4_IN_DIC_R", "EFT_12M_C2", "EFT_6M_C2", "Cost_IPW_USD")

dados <- data[,var]

# mudando a categoria de base adesao
dados$Nao_adesao1_6m <- recode(dados$Nao_adesao1_6m, "0=1; 1=0")
dados$Nao_adesao1_12m <- recode(dados$Nao_adesao1_12m, "0=1; 1=0")

###Descritiva dos grupos

#Muda a codificação dos grupos
dados$V47_STR <- recode(dados$V47_STR, "1=2; 2=1")

#Proporção de pacientes por grupo
table(dados$V47_STR)
100*prop.table(table(dados$V47_STR))

#Idade por grupo
by (data = dados$IDADE, INDICES = dados$V47_STR, FUN = summary)

summary(dados$IDADE)
sd(dados$IDADE)

summary(dados$TEMPO_ACP_1_12M)
sd(dados$TEMPO_ACP_1_12M)

#Tempo de acompanhamento por grupo
by (data = dados$TEMPO_ACP_1_12M, INDICES = dados$V47_STR, FUN = summary)
tempoacp_cmp <- compareGroups(dados$V47_STR ~ dados$TEMPO_ACP_1_12M + dados$IDADE)
tempoacp_cmp

dados.tempoacp_cmp <- createTable(tempoacp_cmp,show.n=TRUE, digits = 2, show.all = TRUE)  #Montar tabela da descritiva
print(dados.tempoacp_cmp, na.print=TRUE, header.labels = c(p.overall = "p-value", all = "Total"))

#Classe de medicamentos por grupo
by (data = dados$TARV_INITRA_CLASSE, INDICES = dados$V47_STR, FUN = summary)

#Variáveis categóricas, transformar em factor

dados[] <- lapply(dados,factor)

dados.comp <- compareGroups(dados$V47_STR ~ 
                              dados$Sexo + dados$IDADE_CAT + dados$EST_CIVIL + dados$ESCOL + dados$V7 + dados$RACA + dados$V6 + dados$V9 + dados$Risco_HIV + dados$Risco_HIV_dic +
                              dados$V25 + dados$V26 + dados$DROGAS + dados$V32 + dados$V33 + dados$DROGAS_ATUAL +
                              dados$V39 + dados$DIG_SMI + dados$CC_IN + dados$CV_IN_dic + dados$CD4_IN_dic +
                              dados$HepatiteBC + dados$int_ano_anterior_dic + dados$int_acp_dic + 
                              dados$tempo_diag_dic + dados$Inicio_TARV_local + dados$ANO_TARV + dados$TARV_INITRA + dados$TARV_INITRA_CLASSE + dados$CLASSE_TROCA +
                              dados$regitro_RA + dados$TROCA_ESQ + dados$OBITO_DIC + dados$int_acp_2_dic + dados$Censura +
                              dados$Nao_adesao1_6m + dados$Nao_adesao1_12m)


dados.comp ##Resultados da descritiva da população
dados.comptab <- createTable(dados.comp,show.n=TRUE, show.all = TRUE)  #Montar tabela da descritiva
print(dados.comptab, na.print=TRUE, header.labels = c(p.overall = "p-value", all = "Total"))
missingTable(dados.comp) #Consulta valores faltantes

# Salvar a descritiva
export2word(dados.comptab, file = "Descritiva.doc" )

#Comparar a efetividade (Intention-to-treat, EFT cenário 2 - Dados faltantes = falha)

eft_12m_c2 <-compareGroups(dados$V47_STR ~ dados$EFT_12M_C2 + dados$EFT_12M)
eft_12m_c2tab <- createTable(eft_12m_c2,show.n=TRUE, show.all = TRUE)
print(eft_12m_c2tab, na.print=TRUE, header.labels = c(p.overall = "p-value", all = "Total"))
missingTable(eft_12m_c2) #Consulta valores faltantes

#Intervalo de confiança para efetividade ITT
ci.mean(dados$EFT_12M_C2*100)
by (data = dados$EFT_12M_C2*100, INDICES = dados$V47_STR, FUN = ci.mean)

##########################################################################################################
#Banco dados de custos
data.custo <- read_excel("Custo_TARV_ITT_E_REAL.xlsx")
names(data.custo)

#mudando a categoria de base V47_STR
data.custo$V47_STR <- recode(data.custo$V47_STR, "1=2; 2=1")

#Conferindo custos por grupo 
by (data.custo$Custo_total_RS, INDICES= data.custo$V47_STR, sum)
by (data.custo$Custo_total_real, INDICES= data.custo$V47_STR, sum) #TARV - OK
by (data.custo$Custo_NON_ART_reais, INDICES= data.custo$V47_STR, sum) #Outros med - OK
by (data.custo$Custo_INT_IPCA_reais, INDICES= data.custo$V47_STR, sum, na.rm=TRUE) # Hosp. - OK
by (data.custo$CUSTO_CD4_reais, INDICES= data.custo$V47_STR, sum, na.rm=TRUE) #cd4 - ok
by (data.custo$Custo_CV_REAIS, INDICES= data.custo$V47_STR, sum, na.rm=TRUE) #CV - OK
by (data.custo$CUSTO_GN_reais, INDICES= data.custo$V47_STR, sum, na.rm=TRUE) #GN - ok
by (data.custo$Custo_exame, INDICES= data.custo$V47_STR, sum, na.rm=TRUE) #Outros exames - OK
by (data.custo$Custo_AMB_reais, INDICES= data.custo$V47_STR, sum, na.rm=TRUE) # AMB - OK 
by (data.custo$Custo_ESP_reais, INDICES= data.custo$V47_STR, sum, na.rm=TRUE) #ESP - OK
by (data.custo$Custo_HD_reais, INDICES= data.custo$V47_STR, sum, na.rm=TRUE) # HD - OK 
by (data.custo$Custo_ADT_reais, INDICES= data.custo$V47_STR, sum, na.rm=TRUE) #ADT - OK

by (data.custo$Custo_total_USD, INDICES= data.custo$V47_STR, sum)

by (data.custo$Cost_IPW_USD, INDICES= data.custo$V47_STR, sum, na.rm=TRUE)

sum (data.custo$Cost_IPW_USD, na.rm=TRUE)
summary(data.custo$Cost_IPW_USD, na.rm=TRUE)
sd(data.custo$Cost_IPW_USD, na.rm=TRUE)

sum (data.custo$Custo_total_USD)

###Descritiva de recursos utilizados pelos pacientes
recursos <- compareGroups(data.custo$V47_STR ~ data.custo$AMB_TOTAL2 + data.custo$ESP_TOTAL2 + data.custo$ADT + data.custo$HD_total + data.custo$N_disp
                          + data.custo$N_hosp + data.custo$Sum_Days_int + data.custo$N_CD4 + data.custo$N_CV + data.custo$N_GN + data.custo$N_Exam + data.custo$N_non_HAART)
                          #,method = 4) #SE FOR TROCAR PARA NAO PARAMETRICO #method 4 ele usa shapiro para ver se é noral e aplica o teste mais adequado

recursos

recursos.tab <- createTable(recursos, digits=1, show.n=TRUE, show.all = TRUE)  #Montar tabela da descritiva
print(recursos.tab, na.print=TRUE, header.labels = c(p.overall = "p-value", all = "Total"))
export2word(recursos.tab, file = "Recursos2.doc" )
missingTable(dados.comp) #Consulta valores faltantes

###Descritiva de custos por categoria
custos <- compareGroups(data.custo$V47_STR ~ data.custo$Custo_TARV_REAL_USS + data.custo$CUSTO_NON_ARV_USS + data.custo$Custo_AMB_USD + data.custo$Custo_HD_USD
                        + data.custo$Custo_ADT_USD + data.custo$Custo_ESP_USD + data.custo$CUSTO_CD4_USD + data.custo$Custo_CV_USD + data.custo$CUSTO_GN_USD + 
                          data.custo$Custo_exame_USD + data.custo$Custo_INT_USS + data.custo$Custo_total_USD + data.custo$Cost_IPW_USD)
                              #,method = 4) #SE FOR TROCAR PARA NAO PARAMETRICO #method 4 ele usa shapiro para ver se é noral e aplica o teste mais adequado

custos
custos.tab <- createTable(custos,digits=0, show.n=TRUE, show.all = TRUE)  #Montar tabela da descritiva
print(custos.tab, na.print=TRUE, header.labels = c(p.overall = "p-value", all = "Total"))
export2word(custos.tab, file = "Custos2.doc" )

###Gráfico composição de custos por categoria

#Carrega dados
graf2 <- read_excel("composicao.xlsx") 

g <- ggplot(data=graf2, aes(x=graf2$Category, y=graf2$Percentage, fill=Group)) +
  geom_bar(stat="identity", color="black", position=position_dodge())

#Formata
g <- g + labs(title="Distribution of total costs per category", x="Category", y = "Percentage")+
  scale_fill_manual(values=c('grey32','grey57','lightgrey')) +
  theme_classic()

#Plota
g

##################################Bootstraping

names(data.custo)

#subgroups
ce_STR <- data.custo[data.custo$V47_STR =="0",]
ce_MTR_SC <- data.custo[data.custo$V47_STR =="1",]
ce_MTR_other <- data.custo[data.custo$V47_STR =="2",]

######################Custo-efetividade
names(data.custo)
names(bt_cost_eff)

#Custo médio por grupo
mean_total.costs <- c(mean(ce_STR$Cost_IPW_USD),mean(ce_MTR_SC$Cost_IPW_USD),mean(ce_MTR_other$Cost_IPW_USD))
mean_total.costs

#Efetividade média por grupo
mean_eff <- c(mean(ce_STR$EFT_12M_C2),mean(ce_MTR_SC$EFT_12M_C2),mean(ce_MTR_other$EFT_12M_C2))
mean_eff

#Razão de custo-efetividade
ratios <- mean_total.costs/mean_eff
ratios

# Intervalo de confiança das razões de custo efetividade (obtido pelo bootstraping, abaixo)
R0 <- (bt_cost_eff$c0/ bt_cost_eff$e0)
quantile(R0, c(.025, .975))

R1 <- (bt_cost_eff$c1/ bt_cost_eff$e1)
quantile(R1, c(.025, .975))

R2 <- (bt_cost_eff$c2/ bt_cost_eff$e2)
quantile(R2, c(.025, .975))

#ICER

Overall <- (mean(data.custo$Cost_IPW_USD)/mean(data.custo$EFT_12M_C2))


I_cost <- c(mean(ce_MTR_SC$Cost_IPW_USD) - mean(ce_STR$Cost_IPW_USD),mean(ce_MTR_other$Cost_IPW_USD) - mean(ce_STR$Cost_IPW_USD))
I_cost

I_cost_round <- c(470, 1604)

I_eff <- c(mean(ce_MTR_SC$EFT_12M_C2) - mean(ce_STR$EFT_12M_C2),  mean(ce_MTR_other$EFT_12M_C2) - mean(ce_STR$EFT_12M_C2))
I_eff

I_eff_round <- c(0.024, 0.039)

ICER <- I_cost/I_eff
ICER 

ICER_round <- I_cost_round/I_eff_round
ICER_round

###Bootstrap por grupo, bootstrap custos e efetividade ao mesmo tempo (por paciente sorteado)

bp <- function(x,B) {
  n <- nrow(x)
  res <- data.frame(mean_cost = numeric(B), mean_eft = numeric(B))
  # do the bootstrap
  for(b in 1:B) {
    bt_ind <- sample(1:n, size = n, replace = TRUE)
    x_bt <- x[bt_ind,]
    res$mean_cost[b] <- mean(x_bt$Cost_IPW_USD)
    res$mean_eft[b] <- mean(x_bt$EFT_12M_C2)
    
  }
  # returna resultados 
  res
}

#Salva bootstraping
bt_STR <- bp(ce_STR,1000)
bt_MTR_SC <-bp(ce_MTR_SC,1000)
bt_MTR_Other <-bp(ce_MTR_other, 1000)

#Agrupar resultados e salvar bootstraping
bt_cost_eff <- cbind(bt_STR, bt_MTR_SC, bt_MTR_Other)
write.csv(bt_cost_eff, "Bootstrap.results.csv")

#Carregar resultados
bt_cost_eff <- read.csv("Bootstrap.results.csv")

#Alterar nome das colunas e ordenar para rodar BCEA
colnames(bt_cost_eff) <- c("obs","c0","e0","c1","e1", "c2", "e2") #usar "obs" somente se os dados estiverem salvos em csv. 
order <- c("e1","e2","e0","c1", "c2","c0")
bt_cost.eff <- bt_cost_eff[,order]

####BCEA
#Tranformar a data frame em matrix, segundo o pacote exige
e <- bt_cost.eff[,c(1:3)] 
c <- bt_cost.eff[,c(4:6)] 
class(e) #data frame
class(c) #data frame

e <- as.matrix(bt_cost.eff[,c(1:3)]) 
c <- as.matrix(bt_cost.eff[,c(4:6)]) 
class(e) #matrix
class(c) #matrix

treats <-c("Multiple-tablet regimens - Same components (MTR-SC)", "Multiple-tablet regimens - Other components (MTR-Other)","Single-tablet regimen (STR)") 


###Intervalo de confiança

#Diferença de custo MTR-SC e STR
bt_cost_eff$c1_c0 <- bt_cost_eff$c1 - bt_cost_eff$c0
ic1 <- bt_cost_eff$c1_c0
ic1_order <-sort(ic1)
ic1_order[26]
ic1_order[975]
quantile(bt_cost_eff$c1_c0, c(.025, .975)) #Método de posição e quartil geram números parecidos, mas o de posição é mais adequado. 
histogram(ic1)
mean(ic1)

#Diferença de efetividade de MTR-SC e STR
bt_cost_eff$e1_e0 <- bt_cost_eff$e1 - bt_cost_eff$e0
ie1 <- bt_cost_eff$e1_e0
ie1_order <-sort(ie1)
ie1_order[26]
ie1_order[975]
histogram(ie1)

#Diferença de custo MTR-OTHER e STR
bt_cost_eff$c2_c0 <- bt_cost_eff$c2 - bt_cost_eff$c0
ic2 <- bt_cost_eff$c2_c0
ic2_order <-sort(ic2)
ic2_order[26]
ic2_order[975]
histogram(ic2)

#Diferença de efetividade de MTR-OTHER e STR
bt_cost_eff$e2_e0 <- bt_cost_eff$e2 - bt_cost_eff$e0
ie2 <- bt_cost_eff$e2_e0
ie2_order <-sort(ie2)
ie2_order[26]
ie2_order[975]
histogram(ie2)

#ICER MTR-SC e STR (Calcular pelo método de caixa)
bt_cost_eff$ICER1 <- bt_cost_eff$c1_c0/bt_cost_eff$e1_e0
CE1 <- bt_cost_eff$ICER1
CE1_order <-sort(CE1)
CE1_order[26]
CE1_order[975]
histogram(CE1)

#ICER MTR-OTHER e STR
bt_cost_eff$ICER2 <- bt_cost_eff$c2_c0/bt_cost_eff$e2_e0
CE2 <- bt_cost_eff$ICER2
CE2_order <-sort(CE2)
CE2_order[26]
CE2_order[975]
histogram(CE2)


#Cost-effectiveness Plane

write.csv(bt_cost_eff, "montar_grafico.csv")

grafico <- read.csv("grafico.csv") #Abre arquivo formatado para gráfico

p <- ggplot2.scatterplot(data=grafico, xName='delta_e',yName='delta_c',
                         groupName="Group", backgroundColor="white",
                         groupColors=c("grey","grey30"))

p<- ggplot2.customize(p, mainTitle="Cost-effectiveness plane",
                     xtitle="Incremental effectiveness", ytitle="Incremental costs", xlim = c(-0.22,0.22), ylim= c(-500, 5000), 
                     removePanelGrid = TRUE,removePanelBorder = TRUE,legendPosition = "right")

p <- p + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)  + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x= element_text(hjust = 0.5))                   

print(p) 

###Cost-effectiveness plane using the package BCEA

m <-bcea(e,c,ref=3,interventions=treats,Kmax=50000) #Roda a análise com dados do bootstrap
summary(m)

#Padrão (STR - MTR-SC) e (STR - MTR-Other). 
#Inverter o padrão para plotar (MTR-SC - STR) e (MTR-Other - STR)
m2=m
m2$delta.e=-m2$delta.e
m2$delta.c=-m2$delta.c
m2$ref=m$comp
m2$comp=m$ref

#Plotar gráficos
ce.plot <- ceplane.plot(m2,graph="ggplot2", wtp=50000, ICER.size =2) # Plota Other vs STR
ce.plot$layers <- ce.plot$layers[-c(1,2,7)] 
ce.plot
ce.plot2 <- ggplot2.customize(ce.plot, xtitle="Incremental effectiveness", ytitle="Incremental costs", xlim = c(-0.20,0.22), ylim= c(-500, 5000), 
                  removePanelGrid = TRUE,removePanelBorder = TRUE, legendPosition = "top")
ce.plot2
ce.plot3 <- ce.plot2 + theme(axis.title.x= element_text(hjust = 0.5))                   
ce.plot3


###Cost-effectiveness acceptability curve
mce=multi.ce(m)
png(file="Curve.png",units="cm",width=18,height=14, res=600)
mce.plot(mce,pos="topright", color=c("seagreen", "tomato", "blue"),  type="l") 
dev.off()

?mce.plot

###Net-benefit
ib.plot(m)
eib.plot(m,pos="topleft") 


###############################################################################################################################
############Análise de sensibilidade

# Ajuste de efetividade e de custos pelas diferencas no baseline (p.10)
####idade, escolaridade, raca, trabalho, risco, tempo diagnóstico####

#Abrir novamente o banco e não aplicar factor a todas as variáveis

rm(dados)
data <- read_excel("juliana_19052017_reduzido.xlsx")
var <- c("ESCOL","RACA", "IDADE", "V6", "Risco_HIV_dic", "tempo_diag_dic","EFT_12M_C2", "V47_STR", "v47_STR_dic",  "Cost_IPW_USD")
dados <- data[,var]
dados$V47_STR <- recode(dados$V47_STR, "1=2; 2=1")

dados$ESCOL <- factor(dados$ESCOL, labels = c("<8", ">=8"))
dados$RACA <- factor(dados$RACA, labels = c("Non-brown", "Brown"))
dados$V6 <- factor(dados$V6, labels = c("Yes", "No"))
dados$Risco_HIV_dic <- factor(dados$Risco_HIV_dic, labels = c("Non-MSM", "MSM"))
dados$tempo_diag_dic <- factor(dados$tempo_diag_dic, labels = c("<=60", ">60"))
dados$EFT_12M_C2 <- factor(dados$EFT_12M_C2, labels = c("Detectable", "Undetectable"))
dados$V47_STR <- factor(dados$V47_STR, labels = c("STR","MTR-Same", "MTR-Other"))
dados$v47_STR_dic <- factor(dados$v47_STR_dic, labels= c("STR", "OTHER"))
summary(dados)
names(data)

dados2 <-  na.exclude(dados) #remove NA

#Regressão logística multivariada
m2 <-glm(EFT_12M_C2 ~ V47_STR + IDADE + ESCOL + RACA + V6 + Risco_HIV_dic + tempo_diag_dic,
         family=binomial(link="logit"), data=dados)

summary(m2)

#Salva valores preditos
dados2$predictions <- predict(m1, type = "response")
summary(dados2$predictions)
by (dados2$predictions, INDICES= dados2$V47_STR, mean)

mm2 <- margins(m2)
summary(mm2)
plot(mm2)

#Intervalo de confiança preditos
by (dados2$predictions*100, INDICES= dados2$V47_STR, ci.mean)

###Ajuste de custos por categorias desbanlanceadas no baseline (p.10)

#Regressão logística multivariada de custo.
#Modelo final de acordo com scrip no STATA (inclui Park test)
#Exporta base de dados para STATA

write.dta(dados, "C:\\Users\\z5202391\\Dropbox\\Doutorado_Juliana\\Artigo3\\Bancos_dados_scrip\\dados.dta")
write.dta(dados2, "C:\\Users\\z5202391\\Dropbox\\Doutorado_Juliana\\Artigo3\\Bancos_dados_scrip\\dados2.dta")

#Após seleção, rodei aqui para fazer bootstraping
modelo_gamma <- glm(Cost_IPW_USD ~ V47_STR +IDADE + ESCOL + RACA + V6 + Risco_HIV_dic + tempo_diag_dic,
                    family=Gamma(link=power(0.65)), data = dados2)

summary(modelo_gamma)
comp.model <- margins(modelo_gamma)
summary(comp.model$`dydx_V47_STRMTR-Same`)

#Salva valores preditos para aplicar Park test. Não usar valores preditos e tirar média. Dif entre grupos pelo comando "margins".
#Tentei rodar à mão o Park test aqui, mas não consegui replicar o teste robusto do STATA =( Ver scrip STATA

modelo_teste <- glm(Cost_IPW_USD ~ V47_STR +IDADE + ESCOL + RACA + V6 + Risco_HIV_dic + tempo_diag_dic,
                    family=Gamma(link="log", data = dados2)
                    dados2$predictions <- predict(modelo_teste, type = "response")
                    dados2$predictions_ln <- log(dados2$predictions)
                    dados2$residuals <- dados2$Cost_IPW_USD - dados2$predictions
                    dados2$res_2 <- (dados2$residuals)^2
                    
Park_test <- glm(dados2$res_2 ~ dados2$predictions_ln, family=Gamma(link="log")) #Coef = 2 = gamma
sandwich(Park_test)
vcovHC(Park_test, type = "HC1") 

###########################Fazer bootstrap de diferenças de efetividade e de deferença de custo. Ambos ajustados pelo baseline

bp <- function(x,B) {
  n <- nrow(x)
  res <- data.frame(mean_cost = numeric(B), mean_eft = numeric(B))
  # do the bootstrap
  for(b in 1:B) {
    bt_ind <- sample(1:n, size = n, replace = TRUE)
    x_bt <- x[bt_ind,]
    res$mean_cost[b] <- mean(x_bt$Cost_IPW_USD)
    res$mean_eft[b] <- mean(x_bt$predictions)
    
  }
  # return bootstrap sample of costs and effectiveness
  res
}


?family
bt_MTR_SC <- bp(STR2,1000)
bt_MTR_SCp <-bp(MTR_SAME2,1000)
bt_MTR_Otherp <-bp(MTR_Other2, 1000)

mm2$`dydx_V47_STRMTR-Same`
summary(comp.model$`dydx_V47_STRMTR-Same`)

#bind results
bt_cost_effp <- cbind(bt_STRp, bt_MTR_SCp, bt_MTR_Otherp)
write.csv(bt_cost_effp, "Bootstrap.results_PRED.csv")

#Carrega resultados
bt_cost_effp <- read.csv("Bootstrap.results_PRED.csv")

#Ordena colunas
colnames(bt_cost_effp) <- c("obs","c0","e0","c1","e1", "c2", "e2")
order <- c("e1","e2","e0","c1", "c2","c0")
bt_cost.effp <- bt_cost_effp[,order]

#Diferença de custos 
mean_total.costs2 <- c(mean(STR2$Cost_IPW_USD),mean(MTR_SAME2$Cost_IPW_USD),mean(MTR_Other2$Cost_IPW_USD))
mean_total.costs2

#Diferença efetividade
mean_eff2 <- c(mean(STR2$predictions),mean(MTR_SAME2$predictions),mean(MTR_Other2$predictions))
mean_eff2

#Razão de custo-efetividade
ratios2 <- mean_total.costs/mean_eff
ratios2

#Intervalo de confiança da Ração de custo-efetividade ajustada 

R02 <- (bt_cost_effp$c0/ bt_cost_effp$e0)
quantile(R02, c(.025, .975))

R12 <- (bt_cost_effp$c1/ bt_cost_effp$e1)
quantile(R12, c(.025, .975))

R22 <- (bt_cost_effp$c2/ bt_cost_effp$e2)
quantile(R22, c(.025, .975))

#ICER

I_cost2 <- c(mean(MTR_SAME2$Cost_IPW_USD) - mean(STR2$Cost_IPW_USD),mean(MTR_Other2$Cost_IPW_USD) - mean(STR2$Cost_IPW_USD))
I_cost2

I_eff2 <-c(mean(MTR_SAME2$predictions) - mean(STR2$predictions),  mean(MTR_Other2$predictions) - mean(STR2$predictions))
I_eff2

ICER2 <- I_cost/I_eff
ICER2 


###Intervalo de confiança

#Diferença de custo MTR-SC e STR
bt_cost_effp$e1_e0 <- bt_cost_effp$e1 - bt_cost_effp$e0
quantile(bt_cost_effp$e1_e0, c(.025, .975))

#Diferença de efetividade MTR-SC e STR
bt_cost_effp$c1_c0 <- bt_cost_effp$c1 - bt_cost_effp$c0
quantile(bt_cost_effp$c1_c0, c(.025, .975))

#Diferença de custo MTR-Other e STR
bt_cost_effp$c2_c0 <- bt_cost_effp$c2 - bt_cost_effp$c0
quantile(bt_cost_effp$c2_c0, c(.025, .975))

#Diferença de efetividade
bt_cost_effp$e2_e0 <- bt_cost_effp$e2 - bt_cost_effp$e0
quantile(bt_cost_effp$e2_e0, c(.025, .975))

#ICER MTR-SC e STR
bt_cost_effp$ICER1 <- bt_cost_effp$c1_c0/bt_cost_effp$e1_e0
quantile(bt_cost_effp$ICER1, c(.025, .975)) 

#ICER MTR-Other e STR
bt_cost_effp$ICER2 <- bt_cost_effp$c2_c0/bt_cost_effp$e2_e0
quantile(bt_cost_effp$ICER2, c(.025, .975)) 
