#####################################################################
### processamento e analise dos dados relacionados a figura sup 2 ###

# pacotes solicitados para as analises
library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(DHARMa)
library(flexplot)
library(outliers)

### importando os data sets #######################

# dams data set
comp_data_dams <- read_xlsx("Data/Model_comparison/MODEL DATA_DAMS_COMP.xlsx")
str(comp_data_dams)

# transformando as variaveis infection e genotipo em fatores
comp_data_dams$Genotype <- as.factor(comp_data_dams$Genotype)
comp_data_dams$Infection <- as.factor(comp_data_dams$Infection)

# o grupo BNTac precisar ser o fator de referencia
comp_data_dams$Genotype <- relevel(comp_data_dams$Genotype, ref = "BNTac")
levels(comp_data_dams$Genotype)

# fetus data set
comp_data_weight <- read_xlsx("Data/Model_comparison/MODEL DATA_WEIGHT_COMP.xlsx")
str(model_data_weight)

# transformando as variaveis infection e genotype em fatores
comp_data_weight$Genotype <- as.factor(comp_data_weight$Genotype)
comp_data_weight$Infection <- as.factor(comp_data_weight$Infection)

# o grupo BNTac precisar ser o fator de referencia
comp_data_weight$Genotype <- relevel(comp_data_weight$Genotype, ref = "BNTac")
levels(comp_data_weight$Genotype)


### analise da infecção e do genotipo sobre o peso do baço ###############

#visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= Spleen_w, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()


#modelo GLM
glm_sw <- glm(Spleen_w~Infection*Genotype, data= comp_data_dams)


#diagnostico do modelo
residuals_sw <- simulateResiduals(fittedModel = glm_sw)
plot(residuals_sw)

# estimates
summary(glm_sw)

### analise da contagem de leucocitos (WBC) ##########################

#visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= WBC, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()


#verificando outliers
outs_comp_dams <- comp_data_dams %>%
  group_by(Genotype, Infection) %>%
  summarise(
    grubbs_pvalue = grubbs.test(WBC)$p.value,
    outlier = ifelse(grubbs_pvalue < 0.05, WBC[which.max(
      abs(WBC - mean(WBC)))], NA))
outs_comp_dams

#removendo outliers
no_outs <- comp_data_dams %>%
  filter(!(Genotype == "BJ" & Infection == "YES" & WBC == 15.2)) %>%
  dplyr::select(Genotype, Mice_ID, Infection, WBC)


#visualizando o dado
ggplot(no_outs, aes(x= Infection, y= WBC, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()


#modelo glm
glm_wbc <- glm(WBC~Infection*Genotype, data = no_outs)


#diagnostico do modelo
residuals_wbc <- simulateResiduals(fittedModel = glm_wbc)
plot(residuals_wbc)

#estimates
summary(glm_wbc)

### linfocitos ###################################################

#visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= LIN, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()

#modelo glm
glm_lin <- glm(LIN~Infection*Genotype, data = comp_data_dams)


#diagnostico do modelo
residuals_lin <- simulateResiduals(fittedModel = glm_lin)
plot(residuals_lin)

# estimates
summary(glm_lin)

## Granulocitos #################################################

#visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= GRAN, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()


#modelo glm
glm_gran <- glm(GRAN~Infection*Genotype, data = comp_data_dams)


#diagnostico do modelo
residuals_gran <- simulateResiduals(fittedModel = glm_gran)
plot(residuals_gran)

# estimates
summary(glm_gran)


### Monocitos #########################################################

#visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= MON, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()

#modelo glm
glm_mon <- glm(MON~Infection*Genotype, data = comp_data_dams)


#diagnostico do modelo
residuals_mon <- simulateResiduals(fittedModel = glm_mon)
plot(residuals_mon)

# estimates
summary(glm_mon)

### peso da placenta ################################################

# visualizando o dado
ggplot(comp_data_weight, aes(x= Infection, y= Placenta_weight, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()

# Verificação e extração de outliers
outs_comp <- comp_data_weight %>%
  group_by(Genotype, Infection) %>%
  summarise(
    grubbs_pvalue = grubbs.test(Placenta_weight)$p.value,
    outlier_value = ifelse(grubbs_pvalue < 0.05,
                           Placenta_weight[which.max(abs(Placenta_weight - mean(Placenta_weight)))],
                           NA_real_)) %>%
  filter(!is.na(outlier_value)) %>%
  ungroup()

outs_comp

# modelo lmm
lmm_pw <- lmer(Placenta_weight~Infection*Genotype+(1|Mice_ID),
               data = comp_data_weight)

# diagnostico do modelo
residuals_pw <- simulateResiduals(fittedModel = lmm_pw)
plot(residuals_pw)


# estimates
summary(lmm_pw)
