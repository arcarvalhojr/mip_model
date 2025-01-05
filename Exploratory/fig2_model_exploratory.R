#################################################################
### processamento e analise dos dados relacionados a figura 2 ###

#pacotes solicitados para as analises
library(tidyverse)
library(readxl)
library(writexl)
library(car)
library(lme4)
library(lmerTest)
library(DHARMa)
library(emmeans)
library(outliers)

### importando o data set

# fetus data set
model_data_weight <- read_xlsx("Data/Model/MODEL DATA_WEIGHT.xlsx")
str(model_data_weight)
#a variavel Infecção precisa ser categorica
model_data_weight$Infection <- as.character(model_data_weight$Infection)
str(model_data_weight)


### percentis fetos #######################################################

#filtrando apenas dois grupos de infecção
model_percentil_fw <- model_data_weight %>%
  filter(Infection %in% c(0, 8) & Fetal_weight) %>%
  select(Mice_ID, Fetus_ID, Infection, Fetal_weight)
str(model_percntil_fw)

#calculando o 5º percentil
percentil_5 <- quantile(model_percentil_fw$Fetal_weight[
  model_percentil_fw$Infection == "0"], probs = 0.05)

#Criando uma coluna de classificação (abaixo ou acima do 5º percentil)
model_percentil_fw <- model_percentil_fw %>% 
  mutate(percentil = ifelse(Fetal_weight <= percentil_5, "below", "under"))
str(model_percentil_fw)

# Salvando como .xlsx
write_xlsx(model_percentil_fw, "Data/Model/model_percentil_fw.xlsx")

#modelo glmm
glmm_fw <- glmer(factor(percentil)~Infection+(1|Mice_ID),
                    data= model_percentil_fw, family = binomial(link = "logit"))
summary(glmm_fw)

#diagnostico do modelo
residuals_percentil <- simulateResiduals(fittedModel = glmm_fw)
plot(residuals_percentil)

