#################################################################
### processamento e analise dos dados relacionados a figura 3 ###

#pacotes requisitados
library(tidyverse)
library(readxl)
library(rstatix)
library(DHARMa)
library(flexplot)
library(report)
library(outliers)

### importando os data sets ####

# dams data set
comp_data_dams <- read_xlsx("Data/Model_comparison/MODEL DATA_DAMS_COMP.xlsx")
str(comp_data_dams)

# transformando as variaveis infection e genotype em fatores
comp_data_dams$Genotype <- as.factor(comp_data_dams$Genotype)
comp_data_dams$Infection <- as.factor(comp_data_dams$Infection)
comp_data_dams$PTD <- as.factor(comp_data_dams$PTD)

# o grupo BNTac precisar ser o fator de referencia
comp_data_dams$Genotype <- relevel(comp_data_dams$Genotype, ref = "BNTac")
levels(comp_data_dams$Genotype)

# peripheral parasitemia data set
comp_parasitemia <- read_xlsx("Data/Model_comparison/PARASITEMIA_COMP.xlsx")
str(comp_parasitemia)

# transformando as variaveis genotype E preg em fatores
comp_parasitemia$Genotype <- as.factor(comp_parasitemia$Genotype)
comp_parasitemia$Preg <- as.factor(comp_parasitemia$Preg)

#o fator BNTac precisar ser o fator de referencia
comp_parasitemia$Genotype <- as.factor(comp_parasitemia$Genotype)
comp_parasitemia$Genotype <- relevel(comp_parasitemia$Genotype, ref = "BNT")
levels(comp_parasitemia$Genotype)

# fetus data set
comp_data_weight <- read_xlsx("Data/Model_comparison/MODEL DATA_WEIGHT_COMP.xlsx")
str(model_data_weight)

# transformando as variaveis infection e genotype em fatores
comp_data_weight$Genotype <- as.factor(comp_data_weight$Genotype)
comp_data_weight$Infection <- as.factor(comp_data_weight$Infection)

# o grupo BNTac precisar ser o fator de referencia
comp_data_weight$Genotype <- relevel(comp_data_weight$Genotype, ref = "BNTac")
levels(comp_data_weight$Genotype)


### analise da infecção*genotipo sobre RBC ###############################

#visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= RBC, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()


#modelo glm
glm_rbc <- glm(RBC~Infection*Genotype, data = comp_data_dams)

#diagnostico do modelo
residuals_rbc <- simulateResiduals(fittedModel = glm_rbc)
plot(residuals_rbc)

visualize(glm_rbc)

# estimates
summary(glm_rbc)
report(glm_rbc)

# multiplas comparações do modelo
#mtc_rbc <- emmeans(glm_rbc, pairwise~Infection*Genotype)
#summary(mtc_rbc)

### analise do percentual de hematocrito (HCT) ############################

# visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= HCT, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()

#visualisando a normalidade da variavel
ggplot(data = comp_data_dams, aes(sample= HCT, color= Infection))+
  stat_qq()+
  stat_qq_line(aes(group= Infection), color= "black")+
  facet_wrap(~Genotype)+
  theme_bw()

# modelo glm
glm_hct <- glm(HCT~Infection*Genotype, data = comp_data_dams)

#diagnostico do modelo
residuals_hct <- simulateResiduals(fittedModel = glm_hct)
plot(residuals_hct)

visualize(glm_hct)

# estimates
summary(glm_hct)
report(glm_hct)

#multiplas comparções do modelo
mtc_hct <- emmeans(glm_hct, pairwise~Infection*Genotype)
summary(mtc_hct)

### analise da contagem de hemoglobina no sangue (HGB) ###################

# visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= HGB, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()

# moidelo glm
glm_hgb <- glm(HGB~Infection*Genotype, data = comp_data_dams)

#diagnostico do modelo
residuals_hgb <- simulateResiduals(fittedModel = glm_hgb)
plot(residuals_hgb)

visualize(glm_hgb)

# estimates
summary(glm_hgb)
report(glm_hgb)

#multiplas comparações para o modelo
mtc_hgb <- emmeans(glm_hgb, pairwise ~ Infection*Genotype)
summary(mtc_hgb)

### analise da porcentagem de pre-termo ####################################

# filtrando as variaveis em uma nova tabela
comp_ptd_data

#calculando o numero de observações por grupo
n_ptd <- comp_data_dams %>%
  group_by(Genotype, Infection) %>%
  count(Infection, PTD)
n_ptd

#modelo glm
glm_ptd <- glm(PTD~Infection+Genotype, data = comp_data_dams,
               family = "binomial")

#diagnostico do modelo
residuals_ptd <- simulateResiduals(fittedModel = glm_ptd)
plot(residuals_ptd)

# estimates
summary(glm_ptd)

#multiplas comparações
mtc_ptd <- emmeans(glm_ptd, pairwise~Infection+Genotype)
summary(mtc_ptd)

### analise do percentual de parasitemia no sangue periferico #############

# visualizando o dado
ggplot(comp_parasitemia, aes(x= Genotype, y= Parasitemia, fill= Preg))+
  geom_boxplot()

#modelo glm
glm_parasitemia <- glm(Parasitemia~Preg*Genotype, data = comp_parasitemia)
summary(glm_parasitemia)

#diagnóstico do modelo por glm
residuals_para <- simulateResiduals(fittedModel = glm_parasitemia)
plot(residuals_para)

visualize(glm_parasitemia)

#multiplas comparações do modelo
mtc_parasitemia <- emmeans(glm_parasitemia, pairwise~Preg*Genotype)
summary(mtc_parasitemia)

### pb18s mRNA expression ########################################

#selecionando as variaveis especificas
pb18s_data <- comp_data_weight %>%
  select(Genotype, Fetus_ID, Infection, pb18s) %>%
  filter(Infection == "YES" & !is.na(pb18s))
str(pb18s_data)

#observando o dado
boxplot(pb18s~Genotype, data= pb18s_data)

#modelo glm
glm_pb18s <- glm(pb18s~Genotype, data = pb18s_data)
summary(glm_pb18s)

#diagnostico do modelo
residuals_pb18s <- simulateResiduals(fittedModel = glm_pb18s)
plot(residuals_pb18s)
