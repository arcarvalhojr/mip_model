#####################################################################
### processamento e analise dos dados relacionados a figura sup 1 ###

# pacotes solicitados para as analises
library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(DHARMa)
library(flexplot)


### importando os data sets

# dams data set
model_data_dams <- read_xlsx("Data/Model/MODEL DATA_DAMS.xlsx")
str(model_data_dams)
#a variavel Infecção precisa ser categorica
model_data_dams$Infection <- as.factor(model_data_dams$Infection) 
str(model_data_dams)

# fetus data set
model_data_weight <- read_xlsx("Data/Model/MODEL DATA_WEIGHT.xlsx")
str(model_data_weight)
#a variavel Infecção precisa ser categorica
model_data_weight$Infection <- as.character(model_data_weight$Infection)
str(model_data_weight)

# vascular space data set
model_vasc_space <- read_xlsx("Data/Model/MODEL_VASC_SPACE.xlsx") 
str(model_vasc_space) 
#a variavel Infecção precisa ser categorica
model_vasc_space$Infection <- as.character(model_vasc_space$Infection) 
model_vasc_space$Analysis <- as.character(model_vasc_space$Analysis)
str(model_vasc_space)


#### analise da infecção sobre a contagem de eritrocitos (RBC)###########

#visualizando o dado
ggplot(model_data_dams, aes(x= Infection, y= RBC, fill= Infection)) +
  geom_boxplot()

#modelo glm
glm_rbc <- glm(RBC~Infection, data = model_data_dams)

#diagnóstico do modelo por glm
residuals_rbc <- simulateResiduals(fittedModel = glm_rbc)
plot(residuals_rbc)

visualize(glm_rbc)

# estimates
summary(glm_rbc)


#### analise da infecção sobre o percentual de hematocrito (HCT) ##########

#visualizando o dado
ggplot(model_data_dams, aes(x= Infection, y= HCT, fill= Infection)) +
  geom_boxplot()

#modelo glm
glm_hct <- glm(HCT~Infection, data = model_data_dams)

#diagnóstico do modelo por glm
residuals_hct <- simulateResiduals(fittedModel = glm_hct)
plot(residuals_hct)

visualize(glm_hct)

# estimates
summary(glm_hct)


#### analise da infecção sobre a contagem de hemoglobina (HGB) ##########

#visualizando o dado
ggplot(model_data_dams, aes(x= Infection, y= HGB, fill= Infection)) +
  geom_boxplot()


#modelo glm
glm_hgb <- glm(HGB~Infection, data= model_data_dams)


#diagnóstico do modelo por glm
residuals_hgb <- simulateResiduals(fittedModel = glm_hgb)
plot(residuals_hgb)

visualize(glm_hgb)

# estimates
summary(glm_hgb)


#### analise da contagem de leucocitos (WBC) ############################

#visualizando o dado
ggplot(model_data_dams, aes(x= Infection, y= WBC, fill= Infection)) +
  geom_boxplot()


#modelo glm
glm_wbc <- glm(WBC~Infection, data= model_data_dams)

#diagnóstico do modelo por glm
residuals_wbc <- simulateResiduals(fittedModel = glm_wbc)
plot(residuals_wbc)

visualize(glm_wbc)

# estimates
summary(glm_wbc)


### linfocitos ##########################################################

#visualizando o dado
ggplot(model_data_dams, aes(x= Infection, y= Lymph, fill= Infection)) +
  geom_boxplot()

#modelo glm
glm_lymph <- glm(Lymph~Infection, data = model_data_dams)


#diagnóstico do modelo por glm
residuals_lymph <- simulateResiduals(fittedModel = glm_lymph)
plot(residuals_lymph)

visualize(glm_lymph)

# estimates
summary(glm_lymph)

### Granulocitos ########################################################

#visualizando o dado
ggplot(model_data_dams, aes(x= Infection, y= Gran, fill= Infection)) +
  geom_boxplot()

#modelo glm
glm_gran <- glm(Gran~Infection, data = model_data_dams)

#diagnóstico do modelo por glm
residuals_gran <- simulateResiduals(fittedModel = glm_gran)
plot(residuals_gran)

visualize(glm_gran)

# estimates
summary(glm_gran)

### Monocitos ########################################################

#visualizando o dado
ggplot(model_data_dams, aes(x= Infection, y= Mon, fill= Infection)) +
  geom_boxplot()


#modelo glm
glm_mon <- glm(Mon~Infection, data = model_data_dams)

#diagnóstico do modelo por glm
residuals_mon <- simulateResiduals(fittedModel = glm_mon)
plot(residuals_mon)

visualize(glm_mon)

# estimates
summary(glm_mon)

### analise de peso das placentas ##################################

#visualisando os dados
ggplot(model_data_weight, aes(x= Infection, y= Placenta_weight, fill= Infection)) +
  geom_boxplot()


#modelo lmm
lmm_pw <- lmer(Placenta_weight~Infection+Litter_size+(1|Mice_ID),
               data= model_data_weight)


#diagnostico do modelo
residuals_pw <- simulateResiduals(fittedModel = lmm_pw)
plot(residuals_pw)

# estimates
summary(lmm_pw)


### Analise do espaço vascular ########################################


# visualisando o dado
ggplot(model_vasc_space, aes(x= Infection, y= Area, fill = Infection))+
  geom_boxplot()+
  facet_wrap(~Analysis)+
  theme_classic()

#filtrando a analise 3 (media)
vasc_space_data <- model_vasc_space %>%
  filter(Analysis == "3")
str(vasc_space_data)

#visualisando o dado
ggplot(vasc_space_data, aes(x= Infection, y= Area, fill = Infection))+
  geom_boxplot()+
  theme_classic()


#modelo lmm
lmm_vasc_space <- lmer(Area~Infection+(1|Mice_ID), data= vasc_space_data)

#diagnostico do modelo
residuals_vasc_space <- simulateResiduals(fittedModel = lmm_vasc_space)
plot(residuals_vasc_space)

# estimates
summary(lmm_vasc_space)
