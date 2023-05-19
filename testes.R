############## Base de óbitos ########################

## Sexo
prop.test(1200,2825,0.5,correct=F) 
prop.test(1625,2825,0.5,correct=F, alternative = c("greater")) 

## Comorbidade
prop.test(2354,2559,0.5,correct=F)
prop.test(2354,2559,0.5,correct=F, alternative = c("greater"))  



##### Faixa etaria

# var = read.csv("C:/Users/Gabi/Google Drive/TCC PUC 2021/datasetqui.csv")

prop.test(2004, 2825, 0.11, correct=F) 
prop.test(2004, 2825, 0.11, correct=F, alternative = c("greater"))  

############# Base de contaminados #####################


### Necessidade de UTI

prop.test(2269, 31531, 0.5, correct=F) 
prop.test(2269, 31531, 0.5, correct=F, alternative = c("less"))   


### Necessidade de Internação

prop.test(22575, 32868, 0.5, correct=F) 
prop.test(22575, 32868, 0.5, correct=F, alternative = c("less"))   
prop.test(22575, 32868, 0.5, correct=F, alternative = c("greater")) 


### Sexo

prop.test(24051, 45719, 0.5, correct=F) 
prop.test(24051, 45719, 0.5, correct=F, alternative = c("greater"))   


### Comorbidade

prop.test(7263, 17094, 0.5, correct=F) 
prop.test(7263, 17094, 0.5, correct=F, alternative = c("greater"))  

### Faixa etaria

prop.test(19979, 44847, 0.29, correct=F) 
prop.test(19979, 44847, 0.29, correct=F, alternative = c("greater"))  

######################### Qui-quadrado de independência #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr") 
library(dplyr)
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)
if(!require(psych)) install.packages("psych") 
library(psych)
if(!require(corrplot)) install.packages("corrplot") 
library(corrplot)

########################### UTI x Sexo ###########################

UTI_Sexo = read.csv("C:/Users/Gabi/Google Drive/TCC PUC 2021/UTI_SEXO.csv", stringsAsFactors = T)

#View(UTI_Sexo)
names(UTI_Sexo)

summary(UTI_Sexo$SEXO)
summary(UTI_Sexo$ï..UTI)

UTI_Sexo$SEXO <- factor(UTI_Sexo$SEXO,
                             levels = c("FEMININO",
                                        "MASCULINO"))

tabela <- table(UTI_Sexo$ï..UTI, UTI_Sexo$SEXO)
tabela

## Realização do modelo

quiqua2 <- chisq.test(tabela)
quiqua2

# Análise das frequências esperadas
# Pressuposto: frequências esperadas > 5

quiqua2$expected

N <- count(UTI_Sexo[1])

100*(360.7656/N)

#Não atingiu, usar fisher

fisher.test(tabela)

########################### Internação x Sexo ###########################

INTERNACAO_SEXO = read.csv("C:/Users/Gabi/Google Drive/TCC PUC 2021/INTERNACAO_SEXO.csv", stringsAsFactors = T)

#View(INTERNACAO_SEXO)
names(INTERNACAO_SEXO)

summary(INTERNACAO_SEXO$SEXO)
summary(INTERNACAO_SEXO$ï..INTERNACAO)



INTERNACAO_SEXO$SEXO <- factor(INTERNACAO_SEXO$SEXO,
                        levels = c("FEMININO",
                                   "MASCULINO"))

tabela <- table(INTERNACAO_SEXO$ï..INTERNACAO, INTERNACAO_SEXO$SEXO)
tabela

## Realização do modelo

quiqua2 <- chisq.test(tabela)
quiqua2

# Análise das frequências esperadas
# Pressuposto: frequências esperadas > 5

quiqua2$expected

N <- count(INTERNACAO_SEXO[1])

100*(3953.546/N)

########################### UTI x Comorbidade ###########################

UTI_COMORBIDADE = read.csv("C:/Users/Gabi/Google Drive/TCC PUC 2021/UTI_COMORBIDADE.csv", stringsAsFactors = T)

#View(UTI_COMORBIDADE)
names(UTI_COMORBIDADE)

summary(UTI_COMORBIDADE$COMORBIDADE)
summary(UTI_COMORBIDADE$ï..UTI)



UTI_COMORBIDADE$COMORBIDADE <- factor(UTI_COMORBIDADE$COMORBIDADE,
                               levels = c("SIM",
                                          "NAO"))

tabela <- table(UTI_COMORBIDADE$COMORBIDADE, UTI_COMORBIDADE$ï..UTI)
tabela

## Realização do modelo

quiqua2 <- chisq.test(tabela)
quiqua2

# Análise das frequências esperadas
# Pressuposto: frequências esperadas > 5

quiqua2$expected

N <- count(UTI_COMORBIDADE[1])

100*(738.6829/N)

#Não atingiu, usar fisher

fisher.test(tabela)

########################### Internação x Comorbidade ###########################

INTERNACAO_COMORBIDADE = read.csv("C:/Users/Gabi/Google Drive/TCC PUC 2021/INTERNACAO_COMORBIDADE.csv", stringsAsFactors = T)

#View(INTERNACAO_COMORBIDADE)
names(INTERNACAO_COMORBIDADE)

summary(INTERNACAO_COMORBIDADE$COMORBIDADE)
summary(INTERNACAO_COMORBIDADE$ï..INTERNACAO)



INTERNACAO_COMORBIDADE$COMORBIDADE <- factor(INTERNACAO_COMORBIDADE$COMORBIDADE,
                                      levels = c("SIM",
                                                 "NAO"))

tabela <- table(INTERNACAO_COMORBIDADE$COMORBIDADE, INTERNACAO_COMORBIDADE$ï..INTERNACAO)
tabela

## Realização do modelo

quiqua2 <- chisq.test(tabela)
quiqua2

# Análise das frequências esperadas
# Pressuposto: frequências esperadas > 5

quiqua2$expected

N <- count(UTI_COMORBIDADE[1])

100*(3185.348/N)








