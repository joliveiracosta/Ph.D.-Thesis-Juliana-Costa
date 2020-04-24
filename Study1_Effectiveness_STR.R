#######################################
# Estudo 1 
# Costa, J.O. et al,. (2018). Efetividade da terapia antirretroviral na era de medicamentos em dose fixa combinada. 
# Revista de Saúde Pública, 52, 87. Epub 14 de novembro de 2018.https://dx.doi.org/10.11606/s1518-8787.2018052000399
# Contato: juliana.olic@gmail.com
#######################################

# Carregar pacotes
library(readxl)
library(car)
library(pROC)
library(mice)
library(ggplot2)

# Leitura dos dados
setwd(diretorio)
data <- read_excel("Banco.xlsx")
var <- c("Sexo","IDADE_CAT", "CC_IN", "HepatiteBC", "V39", "int_ano_anterior_dic", "Inicio_TARV_local", "ANO_TARV",
         "TARV_INITRA_CLASSE_DIC", "v47_STR_dic", "tempo_diag_dic", "regitro_RA", "TROCA_ESQ", "Nao_adesao1_6m",
         "Nao_adesao1_12m", "int_acp_dic", "EST_CIVIL", "RACA", "DIG_SMI", "V32", "V33", "V6", "V9", 
         "Risco_HIV_dic", "DROGAS_ATUAL", "ESCOL", "V25", "V26", "EFT_12M", "EFT_6M", "DROGAS", "CV_IN_dic", "CD4_IN_DIC_R")

dados <- data[,var]
dados[] <- lapply(dados,factor)


# mudando a categoria de base adesao
dados$Nao_adesao1_6m <- recode(dados$Nao_adesao1_6m, "0=1; 1=0")
dados$Nao_adesao1_12m <- recode(dados$Nao_adesao1_12m, "0=1; 1=0")


################################### ANALISE DE CASOS COMPLETOS ################################################

##### MODELOS UNIVARIARIADOS
### EFETIVIDADE EM 6 MESES
ef6m <- matrix(ncol=6,nrow=30)

# Sexo
m <- glm(EFT_6M ~ Sexo, data=dados, family=binomial(link="logit"))
ef6m[1,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                     exp(confint(m)[2,2]), summary(m)$coefficients[2,4])
# Idade
rm(m)
m <- glm(EFT_6M ~ IDADE_CAT, data=dados, family=binomial(link="logit"))
ef6m[2,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                     exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Aids
rm(m)
m <- glm(EFT_6M ~ CC_IN, data=dados, family=binomial(link="logit"))
ef6m[3,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                     exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Hepatite B ou C
rm(m)
m <- glm(EFT_6M ~ HepatiteBC, data=dados, family=binomial(link="logit"))
ef6m[4,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                     exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Doença mental
rm(m)
m <- glm(EFT_6M ~ V39, data=dados, family=binomial(link="logit"))
ef6m[5,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                     exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Internacao ano anterior
rm(m)
m <- glm(EFT_6M ~ int_ano_anterior_dic, data=dados, family=binomial(link="logit"))
ef6m[6,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                     exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Local de inicio da TARV
rm(m)
m <- glm(EFT_6M ~ Inicio_TARV_local, data=dados, family=binomial(link="logit"))
ef6m[7,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                     exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Ano de inicio da TARV
rm(m)
m <- glm(EFT_6M ~ ANO_TARV, data=dados, family=binomial(link="logit"))
ef6m[8,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                     exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Esquema TARV inicial (IP vs. ITRNN)
rm(m)
m <- glm(EFT_6M ~ TARV_INITRA_CLASSE_DIC, data=dados, family=binomial(link="logit"))
ef6m[9,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                     exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Esquema TARV inicial (MMD)
rm(m)
m <- glm(EFT_6M ~ v47_STR_dic, data=dados, family=binomial(link="logit"))
ef6m[10,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                     exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Tempo entre diag. e TARV
rm(m)
m <- glm(EFT_6M ~ tempo_diag_dic, data=dados, family=binomial(link="logit"))
ef6m[11,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Registro de reacao adversa
rm(m)
m <- glm(EFT_6M ~ regitro_RA, data=dados, family=binomial(link="logit"))
ef6m[12,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Troca esquema
rm(m)
m <- glm(EFT_6M ~ TROCA_ESQ, data=dados, family=binomial(link="logit"))
ef6m[13,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Adesao em 6 meses
rm(m)
m <- glm(EFT_6M ~ Nao_adesao1_6m, data=dados, family=binomial(link="logit"))
ef6m[14,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Internacoes durante o acompanhamento
rm(m)
m <- glm(EFT_6M ~ int_acp_dic, data=dados, family=binomial(link="logit"))
ef6m[15,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Estado civil
rm(m)
m <- glm(EFT_6M ~ EST_CIVIL, data=dados, family=binomial(link="logit"))
ef6m[16,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Raca
rm(m)
m <- glm(EFT_6M ~ RACA, data=dados, family=binomial(link="logit"))
ef6m[17,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Diagnostico de doenca mental severa
rm(m)
m <- glm(EFT_6M ~ DIG_SMI, data=dados, family=binomial(link="logit"))
ef6m[18,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Tabaco atualmente
rm(m)
m <- glm(EFT_6M ~ V32, data=dados, family=binomial(link="logit"))
ef6m[19,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Alcool atualmente
rm(m)
m <- glm(EFT_6M ~ V33, data=dados, family=binomial(link="logit"))
ef6m[20,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Se trabalha
rm(m)
m <- glm(EFT_6M ~ V6, data=dados, family=binomial(link="logit"))
ef6m[21,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Se tem filhos
rm(m)
m <- glm(EFT_6M ~ V9, data=dados, family=binomial(link="logit"))
ef6m[22,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Risco HIV
rm(m)
m <- glm(EFT_6M ~ Risco_HIV_dic, data=dados, family=binomial(link="logit"))
ef6m[23,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# uso de drogas atualmente
rm(m)
m <- glm(EFT_6M ~ DROGAS_ATUAL, data=dados, family=binomial(link="logit"))
ef6m[24,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Escolaridade
rm(m)
m <- glm(EFT_6M ~ ESCOL, data=dados, family=binomial(link="logit"))
ef6m[25,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Tabaco alguma vez na vida
rm(m)
m <- glm(EFT_6M ~ V25, data=dados, family=binomial(link="logit"))
ef6m[26,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Alcool alguma vez na vida
rm(m)
m <- glm(EFT_6M ~ V26, data=dados, family=binomial(link="logit"))
ef6m[27,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Drogas alguma vez na vida
rm(m)
m <- glm(EFT_6M ~ DROGAS, data=dados, family=binomial(link="logit"))
ef6m[28,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Carga viral inicial
rm(m)
m <- glm(EFT_6M ~ CV_IN_dic, data=dados, family=binomial(link="logit"))
ef6m[29,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# CD4 inicial
rm(m)
m <- glm(EFT_6M ~ CD4_IN_DIC_R, data=dados, family=binomial(link="logit"))
ef6m[30,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])
rm(m)

# Tabela univariada
ef6m[,2:6] <- round(ef6m[,2:6], digits = 6)
ef6m[,1] <- rbind("Sexo", "Idade", "Aids", "Hepatite B ou C", "Transtorno mental", "Internação ano anterior",
                  "Local TARV","Ano TARV", "TARV (IP vs ITRNN)", "TARV (MMD)", "Tempo diag/TARV", "Registro RA",
                  "Troca de esquema", "Adesão 6 meses", "Internação no acomp.", 
                  "Estado civil", "Raça", "Transtorno mental grave", "Tabaco atualmente", "Alcool atualmente",
                  "Trabalha", "Tem filhos", "Risco HIV", "Drogas atualmente", "Escolaridade", "Tabaco na vida",
                  "Alcool na vida", "Drogas na vida", "CV inicial", "CD4 inicial")
ef6m <- as.data.frame(ef6m)
names(ef6m) <- c("Variável", "Beta", "Odds Ratio", "LI", "LS","Valor-p")

# Exportando tabela univariada
write.table(ef6m,"ef6m.csv",sep=";")


### EFETIVIDADE EM 12 MESES
ef12m <- matrix(ncol=6,nrow=31)

# Sexo
m <- glm(EFT_12M ~ Sexo, data=dados, family=binomial(link="logit"))
ef12m[1,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])
# Idade
rm(m)
m <- glm(EFT_12M ~ IDADE_CAT, data=dados, family=binomial(link="logit"))
ef12m[2,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Aids
rm(m)
m <- glm(EFT_12M ~ CC_IN, data=dados, family=binomial(link="logit"))
ef12m[3,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Hepatite B ou C
rm(m)
m <- glm(EFT_12M ~ HepatiteBC, data=dados, family=binomial(link="logit"))
ef12m[4,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Doença mental
rm(m)
m <- glm(EFT_12M ~ V39, data=dados, family=binomial(link="logit"))
ef12m[5,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Internacao ano anterior
rm(m)
m <- glm(EFT_12M ~ int_ano_anterior_dic, data=dados, family=binomial(link="logit"))
ef12m[6,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Local de inicio da TARV
rm(m)
m <- glm(EFT_12M ~ Inicio_TARV_local, data=dados, family=binomial(link="logit"))
ef12m[7,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Ano de inicio da TARV
rm(m)
m <- glm(EFT_12M ~ ANO_TARV, data=dados, family=binomial(link="logit"))
ef12m[8,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Esquema TARV inicial (IP vs. ITRNN)
rm(m)
m <- glm(EFT_12M ~ TARV_INITRA_CLASSE_DIC, data=dados, family=binomial(link="logit"))
ef12m[9,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                      exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Esquema TARV inicial (MMD)
rm(m)
m <- glm(EFT_12M ~ v47_STR_dic, data=dados, family=binomial(link="logit"))
ef12m[10,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Tempo entre diag. e TARV
rm(m)
m <- glm(EFT_12M ~ tempo_diag_dic, data=dados, family=binomial(link="logit"))
ef12m[11,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Registro de reacao adversa
rm(m)
m <- glm(EFT_12M ~ regitro_RA, data=dados, family=binomial(link="logit"))
ef12m[12,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Troca esquema
rm(m)
m <- glm(EFT_12M ~ TROCA_ESQ, data=dados, family=binomial(link="logit"))
ef12m[13,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Adesao em 12 meses
rm(m)
m <- glm(EFT_12M ~ Nao_adesao1_12m, data=dados, family=binomial(link="logit"))
ef12m[14,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Internacoes durante o acompanhamento
rm(m)
m <- glm(EFT_12M ~ int_acp_dic, data=dados, family=binomial(link="logit"))
ef12m[15,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Estado civil
rm(m)
m <- glm(EFT_12M ~ EST_CIVIL, data=dados, family=binomial(link="logit"))
ef12m[16,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Raca
rm(m)
m <- glm(EFT_12M ~ RACA, data=dados, family=binomial(link="logit"))
ef12m[17,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Diagnostico de doenca mental severa
rm(m)
m <- glm(EFT_12M ~ DIG_SMI, data=dados, family=binomial(link="logit"))
ef12m[18,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Tabaco atualmente
rm(m)
m <- glm(EFT_12M ~ V32, data=dados, family=binomial(link="logit"))
ef12m[19,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Alcool atualmente
rm(m)
m <- glm(EFT_12M ~ V33, data=dados, family=binomial(link="logit"))
ef12m[20,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Se trabalha
rm(m)
m <- glm(EFT_12M ~ V6, data=dados, family=binomial(link="logit"))
ef12m[21,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Se tem filhos
rm(m)
m <- glm(EFT_12M ~ V9, data=dados, family=binomial(link="logit"))
ef12m[22,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Risco HIV
rm(m)
m <- glm(EFT_12M ~ Risco_HIV_dic, data=dados, family=binomial(link="logit"))
ef12m[23,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Uso de drogas atualmente
rm(m)
m <- glm(EFT_12M ~ DROGAS_ATUAL, data=dados, family=binomial(link="logit"))
ef12m[24,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Escolaridade
rm(m)
m <- glm(EFT_12M ~ ESCOL, data=dados, family=binomial(link="logit"))
ef12m[25,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Tabaco alguma vez na vida
rm(m)
m <- glm(EFT_12M ~ V25, data=dados, family=binomial(link="logit"))
ef12m[26,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Alcool alguma vez na vida
rm(m)
m <- glm(EFT_12M ~ V26, data=dados, family=binomial(link="logit"))
ef12m[27,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Efetividade em 6 meses
rm(m)
m <- glm(EFT_12M ~ EFT_6M, data=dados, family=binomial(link="logit"))
ef12m[28,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Drogas alguma vez na vida
rm(m)
m <- glm(EFT_12M ~ DROGAS, data=dados, family=binomial(link="logit"))
ef12m[29,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# Carga viral inicial
rm(m)
m <- glm(EFT_12M ~ CV_IN_dic, data=dados, family=binomial(link="logit"))
ef12m[30,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

# CD4 inicial
rm(m)
m <- glm(EFT_12M ~ CD4_IN_DIC_R, data=dados, family=binomial(link="logit"))
ef12m[31,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                       exp(confint(m)[2,2]), summary(m)$coefficients[2,4])

rm(m)

# Tabela univariada efetividade 12 meses

ef12m[,2:6] <- round(ef12m[,2:6], digits = 6)
ef12m[,1] <- rbind("Sexo", "Idade", "Aids", "Hepatite B ou C", "Transtorno mental", "Internação ano anterior",
                   "Local TARV","Ano TARV", "TARV (IP vs ITRNN)", "TARV (MMD)", "Tempo diag/TARV", "Registro RA",
                   "Troca de esquema", "Adesão 12 meses", "Internação no acomp.", 
                   "Estado civil", "Raça", "Transtorno mental grave", "Tabaco atualmente", "Alcool atualmente",
                   "Trabalha", "Tem filhos", "Risco HIV", "Drogas atualmente", "Escolaridade", "Tabaco na vida",
                   "Alcool na vida", "Efetividade 6 meses", "Drogas na vida", "CV inicial", "CD4 inicial")
ef12m <- as.data.frame(ef12m)
names(ef12m) <- c("Variável", "Beta", "Odds Ratio", "LI", "LS","Valor-p")

# Exportando tabela univariada
write.table(ef12m,"ef12m.csv",sep=";")


#### ANALISE MULTIVARIADA
### EFETIVIDADE 6 MESES

m <- glm(EFT_6M ~ Sexo + CC_IN + V39 + int_ano_anterior_dic + Inicio_TARV_local+
           ANO_TARV + v47_STR_dic + tempo_diag_dic + TROCA_ESQ + Nao_adesao1_6m + 
           RACA + V32 + DROGAS + CV_IN_dic + CD4_IN_DIC_R,
         family=binomial(link="logit"), data = dados)
summary(m) # remove: tempo_diag_dic
rm(m)

m <- glm(EFT_6M ~ Sexo + CC_IN + V39 + int_ano_anterior_dic + Inicio_TARV_local+
           ANO_TARV + v47_STR_dic + TROCA_ESQ + Nao_adesao1_6m + 
           RACA + V32 + DROGAS + CV_IN_dic + CD4_IN_DIC_R,
         family=binomial(link="logit"), data = dados)
summary(m) # remove: Inicio_TARV_local
rm(m)

m <- glm(EFT_6M ~ Sexo + CC_IN + V39 + int_ano_anterior_dic + 
           ANO_TARV + v47_STR_dic + TROCA_ESQ + Nao_adesao1_6m + 
           RACA + V32 + DROGAS + CV_IN_dic + CD4_IN_DIC_R,
         family=binomial(link="logit"), data = dados)
summary(m) # remove: v47_STR_dic
rm(m)

m <- glm(EFT_6M ~ Sexo + CC_IN + V39 + int_ano_anterior_dic + 
           ANO_TARV + TROCA_ESQ + Nao_adesao1_6m + 
           RACA + V32 + DROGAS + CV_IN_dic + CD4_IN_DIC_R,
         family=binomial(link="logit"), data = dados)
summary(m) # remove: V39
rm(m)

m <- glm(EFT_6M ~ Sexo + CC_IN + int_ano_anterior_dic + 
           ANO_TARV + TROCA_ESQ + Nao_adesao1_6m + 
           RACA + V32 + DROGAS + CV_IN_dic + CD4_IN_DIC_R,
         family=binomial(link="logit"), data = dados)
summary(m) # remove: int_ano_anterior_dic
rm(m)

m <- glm(EFT_6M ~ Sexo + CC_IN + ANO_TARV + TROCA_ESQ + Nao_adesao1_6m + 
           RACA + V32 + DROGAS + CV_IN_dic + CD4_IN_DIC_R,
         family=binomial(link="logit"), data = dados)
summary(m) # remove: ANO_TARV
rm(m)

m <- glm(EFT_6M ~ Sexo + CC_IN + TROCA_ESQ + Nao_adesao1_6m + 
           RACA + V32 + DROGAS + CV_IN_dic + CD4_IN_DIC_R,
         family=binomial(link="logit"), data = dados)
summary(m) # remove: RACA
rm(m)

m <- glm(EFT_6M ~ Sexo + CC_IN + TROCA_ESQ + Nao_adesao1_6m + 
           V32 + DROGAS + CV_IN_dic + CD4_IN_DIC_R,
         family=binomial(link="logit"), data = dados)
summary(m) # remove: DROGAS
rm(m)

m <- glm(EFT_6M ~ Sexo + CC_IN + TROCA_ESQ + Nao_adesao1_6m + 
           V32 + CV_IN_dic + CD4_IN_DIC_R,
         family=binomial(link="logit"), data = dados)
summary(m) # remove: CD4_IN_DIC_R
rm(m)

m <- glm(EFT_6M ~ Sexo + CC_IN + TROCA_ESQ + Nao_adesao1_6m + 
           V32 + CV_IN_dic,
         family=binomial(link="logit"), data = dados)
summary(m) # remove: Sexo
rm(m)

m <- glm(EFT_6M ~ CC_IN + TROCA_ESQ + Nao_adesao1_6m + V32 + CV_IN_dic,
         family=binomial(link="logit"), data = dados)
summary(m) # MODELO FINAL

multi6m <- matrix(ncol=6, nrow=5)
for(i in 1:5){
    multi6m[i,2:6] <- cbind(coef(m)[i+1],exp(coef(m)[i+1]),exp(confint(m)[i+1,1]), 
         exp(confint(m)[i+1,2]), summary(m)$coefficients[i+1,4])
}
multi6m[,2:6] <- round(multi6m[,2:6], digits=6)
multi6m[,1] <- rbind("Aids","Troca esquema", "Adesão 6 meses", "Tabaco atualmente", "CV inicial")
multi6m <- as.data.frame(multi6m)
names(multi6m) <- c("Variável", "Beta", "Odds Ratio", "LI", "LS","Valor-p")

# Exportando modelo final 6 meses
write.table(multi6m, "multi6m.csv", sep=";")

# Curva ROC
curva <- roc(m$y ~ fitted(m))
plot(curva)
auc(curva)

# vetor de betas
beta_acc6m <- coef(m)

rm(m)
rm(multi6m)
rm(curva)


### EFETIVIDADE 12 MESES

m <- glm(EFT_12M ~ Sexo + CC_IN + V39 + tempo_diag_dic + TROCA_ESQ + Nao_adesao1_12m +
           V32 + Risco_HIV_dic + DROGAS_ATUAL + V25 + EFT_6M + DROGAS + CV_IN_dic + CD4_IN_DIC_R,
         family = binomial(link = "logit"), data = dados)
summary(m) # remove: V25
rm(m)

m <- glm(EFT_12M ~ Sexo + CC_IN + V39 + tempo_diag_dic + TROCA_ESQ + Nao_adesao1_12m +
           V32 + Risco_HIV_dic + DROGAS_ATUAL + EFT_6M + DROGAS + CV_IN_dic + CD4_IN_DIC_R,
         family = binomial(link = "logit"), data = dados)
summary(m) # remove: V39
rm(m)

m <- glm(EFT_12M ~ Sexo + CC_IN + tempo_diag_dic + TROCA_ESQ + Nao_adesao1_12m +
           V32 + Risco_HIV_dic + DROGAS_ATUAL + EFT_6M + DROGAS + CV_IN_dic + CD4_IN_DIC_R,
         family = binomial(link = "logit"), data = dados)
summary(m) # remove: CC_IN
rm(m)

m <- glm(EFT_12M ~ Sexo + tempo_diag_dic + TROCA_ESQ + Nao_adesao1_12m +
           V32 + Risco_HIV_dic + DROGAS_ATUAL + EFT_6M + DROGAS + CV_IN_dic + CD4_IN_DIC_R,
         family = binomial(link = "logit"), data = dados)
summary(m) # remove: TROCA_ESQ
rm(m)

m <- glm(EFT_12M ~ Sexo + tempo_diag_dic + Nao_adesao1_12m +
           V32 + Risco_HIV_dic + DROGAS_ATUAL + EFT_6M + DROGAS + CV_IN_dic + CD4_IN_DIC_R,
         family = binomial(link = "logit"), data = dados)
summary(m) # remove: CD4_IN_DIC_R
rm(m)

m <- glm(EFT_12M ~ Sexo + tempo_diag_dic + Nao_adesao1_12m +
           V32 + Risco_HIV_dic + DROGAS_ATUAL + EFT_6M + DROGAS + CV_IN_dic,
         family = binomial(link = "logit"), data = dados)
summary(m) # remove: Sexo
rm(m)

m <- glm(EFT_12M ~ tempo_diag_dic + Nao_adesao1_12m + V32 + Risco_HIV_dic + DROGAS_ATUAL +
           EFT_6M + DROGAS + CV_IN_dic, family = binomial(link = "logit"), data = dados)
summary(m) # remove: DROGAS
rm(m)

m <- glm(EFT_12M ~ tempo_diag_dic + Nao_adesao1_12m + V32 + Risco_HIV_dic + DROGAS_ATUAL +
           EFT_6M + CV_IN_dic, family = binomial(link = "logit"), data = dados)
summary(m) # remove: V32
rm(m)

m <- glm(EFT_12M ~ tempo_diag_dic + Nao_adesao1_12m + Risco_HIV_dic + DROGAS_ATUAL +
           EFT_6M + CV_IN_dic, family = binomial(link = "logit"), data = dados)
summary(m) # remove: Nao_adesao1_12m
rm(m)

m <- glm(EFT_12M ~ tempo_diag_dic + Risco_HIV_dic + DROGAS_ATUAL +
           EFT_6M + CV_IN_dic, family = binomial(link = "logit"), data = dados)
summary(m) # remove: tempo_diag_dic
rm(m)

m <- glm(EFT_12M ~ Risco_HIV_dic + DROGAS_ATUAL + EFT_6M + CV_IN_dic,
         family = binomial(link = "logit"), data = dados)
summary(m) # remove: Risco_HIV_dic
rm(m)

m <- glm(EFT_12M ~ DROGAS_ATUAL + EFT_6M + CV_IN_dic,
         family = binomial(link = "logit"), data = dados)
summary(m) # remove: CV_IN_dic
rm(m)

m <- glm(EFT_12M ~ DROGAS_ATUAL + EFT_6M,
         family = binomial(link = "logit"), data = dados)
summary(m) # MODELO FINAL


multi12m <- matrix(ncol=6, nrow=2)
multi12m[1,2:6] <- cbind(coef(m)[2],exp(coef(m)[2]),exp(confint(m)[2,1]), 
                        exp(confint(m)[2,2]), summary(m)$coefficients[2,4])
multi12m[2,2:6] <- cbind(coef(m)[3],exp(coef(m)[3]),exp(confint(m)[3,1]), 
                        exp(confint(m)[3,2]), summary(m)$coefficients[3,4])


multi12m[,2:6] <- round(multi12m[,2:6], digits=6)
multi12m[,1] <- rbind("Drogas atualmente", "Efetividade em 6 meses")
multi12m <- as.data.frame(multi12m)
names(multi12m) <- c("Variável", "Beta", "Odds Ratio", "LI", "LS","Valor-p")

# Exportando tabela multivariada 12 meses
write.table(multi12m, "multi12m.csv", sep=";")

# curva ROC
curva <- roc(m$y ~ fitted(m))
plot(curva)
auc(curva)

#vetor de betas
beta_acc12m <- coef(m)

rm(m)
rm(multi12m)
rm(curva)

################################## IMPUTACAO MULTIPLA ################################################

var1 <- var[var != "CD4_IN_DIC_R"]    # nao incluir CD4 inicial. Correlacionado à variável Aids (0 missing)
dados1 <- data[,var1]
dados1[] <- lapply(dados1,factor)

# mudando a categoria de base adesao
dados1$Nao_adesao1_6m <- recode(dados1$Nao_adesao1_6m, "0=1; 1=0")
dados1$Nao_adesao1_12m <- recode(dados1$Nao_adesao1_12m, "0=1; 1=0")

# imputacao
impData <- mice(data=dados1, m=20, printFlag=FALSE, method="logreg", seed=3791011)
impData$visitSequence

#### UNIVARIADA (APENAS PARA OS DADOS IMPUTADOS)

ef.6m <- matrix(nrow=12,ncol=5)
ef.12m <- matrix(nrow=13,ncol=5)

### EFETIVIDADE 6 MESES

# efetividade 6m ~ escolaridade
m <- with(data=impData, glm(EFT_6M ~ ESCOL, family = binomial(link="logit")))
mod <- pool(m)
ef.6m[1,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 6m ~ raca/cor
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_6M ~ RACA, family = binomial(link="logit")))
mod <- pool(m)
ef.6m[2,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 6m ~ estado civil
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_6M ~ EST_CIVIL, family = binomial(link="logit")))
mod <- pool(m)
ef.6m[3,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 6m ~ trabalho
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_6M ~ V6, family = binomial(link="logit")))
mod <- pool(m)
ef.6m[4,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 6m ~ filhos
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_6M ~ V9, family = binomial(link="logit")))
mod <- pool(m)
ef.6m[5,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 6m ~ cv inicial
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_6M ~ CV_IN_dic, family = binomial(link="logit")))
mod <- pool(m)
ef.6m[6,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 6m ~ tabagismo vida
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_6M ~ V25, family = binomial(link="logit")))
mod <- pool(m)
ef.6m[7,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 6m ~ alcool vida
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_6M ~ V26, family = binomial(link="logit")))
mod <- pool(m)
ef.6m[8,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 6m ~ drogas ilicitas vida
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_6M ~ DROGAS, family = binomial(link="logit")))
mod <- pool(m)
ef.6m[9,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 6m ~ tabagismo atual
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_6M ~ V32, family = binomial(link="logit")))
mod <- pool(m)
ef.6m[10,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 6m ~ alcool atual
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_6M ~ V33, family = binomial(link="logit")))
mod <- pool(m)
ef.6m[11,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 6m ~ droga ilicita atual
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_6M ~ DROGAS_ATUAL, family = binomial(link="logit")))
mod <- pool(m)
ef.6m[12,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

### Efetividade 12 meses

# efetividade 12m ~ escolaridade
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_12M ~ ESCOL, family = binomial(link="logit")))
mod <- pool(m)
ef.12m[1,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 12m ~ raca/cor
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_12M ~ RACA, family = binomial(link="logit")))
mod <- pool(m)
ef.12m[2,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 12m ~ estado civil
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_12M ~ EST_CIVIL, family = binomial(link="logit")))
mod <- pool(m)
ef.12m[3,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 12m ~ trabalho
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_12M ~ V6, family = binomial(link="logit")))
mod <- pool(m)
ef.12m[4,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 12m ~ filhos
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_12M ~ V9, family = binomial(link="logit")))
mod <- pool(m)
ef.12m[5,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 12m ~ cv inicial
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_12M ~ CV_IN_dic, family = binomial(link="logit")))
mod <- pool(m)
ef.12m[6,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 12m ~ tabagismo vida
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_12M ~ V25, family = binomial(link="logit")))
mod <- pool(m)
ef.12m[7,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 12m ~ alcool vida
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_12M ~ V26, family = binomial(link="logit")))
mod <- pool(m)
ef.12m[8,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 12m ~ drogas ilicitas vida
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_12M ~ DROGAS, family = binomial(link="logit")))
mod <- pool(m)
ef.12m[9,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 12m ~ tabagismo atual
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_12M ~ V32, family = binomial(link="logit")))
mod <- pool(m)
ef.12m[10,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 12m ~ alcool atual
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_12M ~ V33, family = binomial(link="logit")))
mod <- pool(m)
ef.12m[11,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 12m ~ droga ilicita atual
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_12M ~ DROGAS_ATUAL, family = binomial(link="logit")))
mod <- pool(m)
ef.12m[12,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

# efetividade 12m ~ efetividade 6m
rm(m)
rm(mod)
m <- with(data=impData, glm(EFT_12M ~ EFT_6M, family = binomial(link="logit")))
mod <- pool(m)
ef.12m[13,] <- cbind(summary(mod)[2,1], exp(summary(mod)[2,1]), exp(summary(mod)[2,6]), exp(summary(mod)[2,7]), summary(mod)[2,5])

rm(m)
rm(mod)


## EFETIVIDADE EM 6 MESES
# Tabela formatada
var6m <- c("Escolaridade", "Raça", "Estado Civil", "Trabalho", "Filhos", "CV inicial", "Tabaco na vida",
           "Álcool vida", "Drogas na vida", "Tabaco atualmente", "Álcool atualmente", "Drogas atualmente")
ef.6m <- cbind(var6m, round(ef.6m, digits=6))
ef.6m <- as.data.frame(ef.6m)
names(ef.6m) <- c("Variável", "Beta", "Odds Ratio", "LI", "LS","Valor-p")

# Exporta tabela
write.table(ef.6m, "ef6m_imp.csv", sep=";")


## EFETIVIDADE EM 12 MESES
# Tabela formatada
var12m <- c("Escolaridade", "Raça", "Estado Civil", "Trabalho", "Filhos", "CV inicial", "Tabaco na vida",
           "Álcool vida", "Drogas na vida", "Tabaco atualmente", "Álcool atualmente", "Drogas atualmente",
           "Efetividade 6 meses")
ef.12m <- cbind(var12m, round(ef.12m, digits=6))
ef.12m <- as.data.frame(ef.12m)
names(ef.12m) <- c("Variável", "Beta", "Odds Ratio", "LI", "LS","Valor-p")

# Exporta tabela
write.table(ef.12m, "ef12m_imp.csv", sep=";")


##### ANALISE MULTIVARIADA
#### EFETIVIDADE EM 6 MESES
rm(m,mod)
m <- with(data=impData, glm(EFT_6M ~ Sexo + CC_IN + V39 + int_ano_anterior_dic + Inicio_TARV_local+
                              ANO_TARV + v47_STR_dic + tempo_diag_dic + TROCA_ESQ + Nao_adesao1_6m + 
                              RACA + V32 + DROGAS + CV_IN_dic, family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: v47_STR_dic

rm(m,mod)
m <- with(data=impData, glm(EFT_6M ~ Sexo + CC_IN + V39 + int_ano_anterior_dic + Inicio_TARV_local+
                              ANO_TARV + tempo_diag_dic + TROCA_ESQ + Nao_adesao1_6m + 
                              RACA + V32 + DROGAS + CV_IN_dic, family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: Inicio_TARV_local

rm(m,mod)
m <- with(data=impData, glm(EFT_6M ~ Sexo + CC_IN + V39 + int_ano_anterior_dic +
                              ANO_TARV + tempo_diag_dic + TROCA_ESQ + Nao_adesao1_6m + 
                              RACA + V32 + DROGAS + CV_IN_dic, family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: tempo_diag_dic

rm(m,mod)
m <- with(data=impData, glm(EFT_6M ~ Sexo + CC_IN + V39 + int_ano_anterior_dic +
                              ANO_TARV + TROCA_ESQ + Nao_adesao1_6m + 
                              RACA + V32 + DROGAS + CV_IN_dic, family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: int_ano_anterior_dic

rm(m,mod)
m <- with(data=impData, glm(EFT_6M ~ Sexo + CC_IN + V39 +
                              ANO_TARV + TROCA_ESQ + Nao_adesao1_6m + 
                              RACA + V32 + DROGAS + CV_IN_dic, family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: ANO_TARV

rm(m,mod)
m <- with(data=impData, glm(EFT_6M ~ Sexo + CC_IN + V39 + TROCA_ESQ + Nao_adesao1_6m + 
                              RACA + V32 + DROGAS + CV_IN_dic, family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: DROGAS

rm(m,mod)
m <- with(data=impData, glm(EFT_6M ~ Sexo + CC_IN + V39 + TROCA_ESQ + Nao_adesao1_6m + 
                              RACA + V32 + CV_IN_dic, family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: V39

rm(m,mod)
m <- with(data=impData, glm(EFT_6M ~ Sexo + CC_IN + TROCA_ESQ + Nao_adesao1_6m + 
                              RACA + V32 + CV_IN_dic, family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: Sexo

rm(m,mod)
m <- with(data=impData, glm(EFT_6M ~ CC_IN + TROCA_ESQ + Nao_adesao1_6m + 
                              RACA + V32 + CV_IN_dic, family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: RACA

rm(m,mod)
m <- with(data=impData, glm(EFT_6M ~ CC_IN + TROCA_ESQ + Nao_adesao1_6m + 
                              V32 + CV_IN_dic, family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # MODELO FINAL


# Tabela multivariada modelo final
ef6m_multi <- matrix(ncol=5,nrow=5)
for (i in 1:5){
  ef6m_multi[i,] <- cbind(summary(mod)[i+1,1], exp(summary(mod)[i+1,1]), exp(summary(mod)[i+1,6]), exp(summary(mod)[i+1,7]), summary(mod)[i+1,5])
}
var6m_multi <- c("Aids", "Troca esquema", "Adesao 6 meses", "Tabaco atualmente", "CV inicial")
ef6m_multi <- cbind(var6m_multi, round(ef6m_multi, digits=6))
ef6m_multi <- as.data.frame(ef6m_multi)
names(ef6m_multi) <- c("Variável", "Beta", "Odds Ratio", "LI", "LS","Valor-p")

# Exportando tabela
write.table(ef6m_multi, "ef6multi.csv", sep=";")

# Curva ROC
c <- matrix(ncol=1,nrow=20)
for(i in 1:20){
  p <- predict(m$analyses[[i]],type="response")
  a <- roc(dados1$EFT_6M ~ p)
  c[i,] <- a$auc
}
mean(c)

# Vetor de betas
beta_im6m <- summary(mod)[,1]


#### EFETIVIDADE EM 12 MESES

rm(mod,m)
m <- with(data=impData, glm(EFT_12M ~ Sexo + CC_IN + V39 + tempo_diag_dic + TROCA_ESQ + Nao_adesao1_12m +
                              V32 + Risco_HIV_dic + DROGAS_ATUAL + V25 + EFT_6M + DROGAS + CV_IN_dic,
                            family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: V25

rm(mod,m)
m <- with(data=impData, glm(EFT_12M ~ Sexo + CC_IN + V39 + tempo_diag_dic + TROCA_ESQ + Nao_adesao1_12m +
                              V32 + Risco_HIV_dic + DROGAS_ATUAL + EFT_6M + DROGAS + CV_IN_dic,
                            family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: V39

rm(mod,m)
m <- with(data=impData, glm(EFT_12M ~ Sexo + CC_IN + tempo_diag_dic + TROCA_ESQ + Nao_adesao1_12m +
                              V32 + Risco_HIV_dic + DROGAS_ATUAL + EFT_6M + DROGAS + CV_IN_dic,
                            family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: TROCA_ESQ

rm(mod,m)
m <- with(data=impData, glm(EFT_12M ~ Sexo + CC_IN + tempo_diag_dic + Nao_adesao1_12m +
                              V32 + Risco_HIV_dic + DROGAS_ATUAL + EFT_6M + DROGAS + CV_IN_dic,
                            family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: DROGAS

rm(mod,m)
m <- with(data=impData, glm(EFT_12M ~ Sexo + CC_IN + tempo_diag_dic + Nao_adesao1_12m +
                              V32 + Risco_HIV_dic + DROGAS_ATUAL + EFT_6M + CV_IN_dic,
                            family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: V32

rm(mod,m)
m <- with(data=impData, glm(EFT_12M ~ Sexo + CC_IN + tempo_diag_dic + Nao_adesao1_12m +
                              Risco_HIV_dic + DROGAS_ATUAL + EFT_6M + CV_IN_dic,
                            family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: CC_IN

rm(mod,m)
m <- with(data=impData, glm(EFT_12M ~ Sexo + tempo_diag_dic + Nao_adesao1_12m +
                              Risco_HIV_dic + DROGAS_ATUAL + EFT_6M + CV_IN_dic,
                            family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: CV_IN_dic

rm(mod,m)
m <- with(data=impData, glm(EFT_12M ~ Sexo + tempo_diag_dic + Nao_adesao1_12m +
                              Risco_HIV_dic + DROGAS_ATUAL + EFT_6M,
                            family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # remove: Sexo

rm(mod,m)
m <- with(data=impData, glm(EFT_12M ~ tempo_diag_dic + Nao_adesao1_12m +
                              Risco_HIV_dic + DROGAS_ATUAL + EFT_6M,
                            family = binomial(link="logit")))
mod <- pool(m)
round(summary(mod), digits=6) # MODELO FINAL

# Tabela multivariada 12 meses
ef12m_multi <- matrix(ncol=5,nrow=5)
for (i in 1:5){
  ef12m_multi[i,] <- cbind(summary(mod)[i+1,1], exp(summary(mod)[i+1,1]), exp(summary(mod)[i+1,6]), exp(summary(mod)[i+1,7]), summary(mod)[i+1,5])
}
var12m_multi <- c("Tempo entre diag e TARV", "Adesão 12 meses", "Risco HIV", "Drogas atualmente", "Efetividade 6 meses")
ef12m_multi <- cbind(var12m_multi, round(ef12m_multi, digits=6))
ef12m_multi <- as.data.frame(ef12m_multi)
names(ef12m_multi) <- c("Variável", "Beta", "Odds Ratio", "LI", "LS","Valor-p")

# Exportando tabela
write.table(ef12m_multi, "ef12multi.csv", sep=";")

# Curva ROC
c <- matrix(ncol=1,nrow=20)
for(i in 1:20){
  p <- predict(m$analyses[[i]],type="response")
  a <- roc(dados1$EFT_6M ~ p)
  c[i,] <- a$auc
}
mean(c)

# Vetor de betas
beta_im12m <- summary(mod)[,1]


###### COMPARACAO PREDITOS
#### EFETIVIDADE EM 6 MESES

# Individuos sem NA
data_6m <- data[,c("CC_IN", "TROCA_ESQ", "Nao_adesao1_6m", "V32", "CV_IN_dic")]
data_6m <- data_6m[complete.cases(data_6m),]
data_6m$Nao_adesao1_6m <- recode(data_6m$Nao_adesao1_6m, "0=1; 1=0")
data_6m <- as.matrix(data_6m)
data_6m <- cbind(rep(1, times=290),data_6m)

# Preditos
pred_acc6m <- exp(data_6m%*%as.matrix(beta_acc6m))/(1 + exp(data_6m%*%as.matrix(beta_acc6m)))
pred_im6m <- exp(data_6m%*%as.matrix(beta_im6m))/(1 + exp(data_6m%*%as.matrix(beta_im6m)))

pred6m <- as.data.frame(cbind(pred_acc6m, pred_im6m))
names(pred6m) <- c("ACC", "IM")
ggplot(data = pred6m, aes(IM, ACC)) + coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, col="blue", size=1) +
  geom_point(size = 2) +
  ggtitle("Valores Preditos", "Efetividade em 6 meses")


# Correlacao de pearson
cor(pred_acc6m, pred_im6m)


#### EFETIVIDADE EM 12 MESES

# Individuos sem NA
data_12m <- data[,c("tempo_diag_dic", "Nao_adesao1_12m", "Risco_HIV_dic", "DROGAS_ATUAL", "EFT_6M")]
data_12m <- data_12m[complete.cases(data_12m),]
data_12m$Nao_adesao1_12m <- recode(data_12m$Nao_adesao1_12m, "0=1; 1=0")
data_12m <- as.matrix(data_12m)
data_12m <- cbind(rep(1, times=259),data_12m)
data_12m1 <- data_12m[,c(1,5,6)]

# Preditos
pred_acc12m <- exp(data_12m1%*%as.matrix(beta_acc12m))/(1 + exp(data_12m1%*%as.matrix(beta_acc12m)))
pred_im12m <- exp(data_12m%*%as.matrix(beta_im12m))/(1 + exp(data_12m%*%as.matrix(beta_im12m)))

pred12m <- as.data.frame(cbind(pred_acc12m, pred_im12m))
names(pred12m) <- c("ACC", "IM")
ggplot(data = pred12m, aes(IM, ACC)) + coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  geom_abline(intercept = 0, slope = 1, col="blue", size=1) +
  geom_point(size = 2) +
  ggtitle("Valores Preditos", "Efetividade em 12 meses")

# Correlacao de pearson
cor(pred_acc12m, pred_im12m)

