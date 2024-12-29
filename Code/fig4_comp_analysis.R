#################################################################
### processamento e analise dos dados relacionados a figura 4 ###

# pacotes solicitados para as analises
library(tidyverse)
library(readxl)
library(writexl)
library(lme4)
library(lmerTest)
library(DHARMa)
library(flexplot)
library(emmeans)
library(report)
library(outliers)

### importando o data set ###############

# fetus data set
comp_data_weight <- read_xlsx("Data/Model_comparison/MODEL DATA_WEIGHT_COMP.xlsx")
str(model_data_weight)

# transformando as variaveis infection e genotype em fatores
comp_data_weight$Genotype <- as.factor(comp_data_weight$Genotype)
comp_data_weight$Infection <- as.factor(comp_data_weight$Infection)
comp_data_weight$Stillbirth <- as.factor(comp_data_weight$Stillbirth)
comp_data_weight$Mice_ID <- as.factor(comp_data_weight$Mice_ID)

# o grupo BNTac precisar ser o fator de referencia
comp_data_weight$Genotype <- relevel(comp_data_weight$Genotype, ref = "BNTac")
levels(comp_data_weight$Genotype)


### Analise de peso dos fetos ########################################

# visualisando o dado
ggplot(comp_data_weight, aes(x= Genotype, y= Fetal_weight, fill= Infection))+
  geom_boxplot()

# modelo lmm
lmm_fw <- lmer(Fetal_weight~Infection*Genotype+(1|Mice_ID),
               data= comp_data_weight)


#diagnostico do modelo
residuals_fw <- simulateResiduals(fittedModel = lmm_fw)
plot(residuals_fw)

visualize(lmm_fw)

# estimates
summary(lmm_fw)
report(lmm_fw)

#verificando outliers
outs_comp_dams <- comp_data_weight %>%
  group_by(Genotype, Infection) %>%
  summarise(
    grubbs_pvalue = grubbs.test(Fetal_weight)$p.value,
    outlier = ifelse(grubbs_pvalue < 0.05, Fetal_weight[which.max(
      abs(Fetal_weight - mean(Fetal_weight)))], NA))
outs_comp_dams

#removendo outliers
no_outs_fw <- comp_data_weight %>%
  filter(!(Genotype == "BNTac" & Infection == "YES" & Fetal_weight == 0.7092)) %>%
  dplyr::select(Genotype, Mice_ID, Fetus_ID, Infection, Fetal_weight, Litter_size)


#multiplas comparações do modelo
mtc_fw <- emmeans(lmm_fw, pairwise~Infection*Genotype)
summary(mtc_fw)

### analise do racio #################################################

# visualisando o dado
ggplot(comp_data_weight, aes(x= Genotype, y= FW_PW, fill= Infection))+
  geom_boxplot()

# modelo lmm
lmm_ratio <- lmer(FW_PW~Infection*Genotype+(1|Mice_ID), data= comp_data_weight)


#diagnostico do modelo
residuals_pw <- simulateResiduals(fittedModel = lmm_ratio)
plot(residuals_pw)

visualize(lmm_ratio)

# estimates
summary(lmm_ratio)


### percentis fetos #####################################################

# selecionando as variaveis e removendo NAs
comp_percentil_fw <- comp_data_weight %>%
  select(Genotype, Mice_ID, Fetus_ID, Infection, Fetal_weight) %>%
  filter(!is.na(Fetal_weight))

# calculando o 5º percentil
percentil_5 <- comp_percentil_fw %>%
  filter(Infection == "NO") %>%
  group_by(Genotype) %>%
  summarize(percentil_5 = quantile(Fetal_weight, probs = 0.05,
                                   na.rm = TRUE))

# Criando uma coluna de classificação (abaixo ou acima do 5º percentil)
comp_percentil_fw <- comp_percentil_fw %>%
  left_join(percentis_5, by = "Genotype") %>% 
  mutate(percentil = ifelse(Fetal_weight <= percentil_5, "below", "under"))


# Salvando como .xlsx
write_xlsx(comp_percentil_fw, "Data/Model_comparison/comp_percentil_fw.xlsx")

# modelo glmm
glmm_fw <- glmer(factor(percentil)~Infection*Genotype+(1|Mice_ID),
                 data = comp_percentil_fw, family = "binomial")


# diagnostico do modelo
residuals_percentil <- simulateResiduals(fittedModel = glmm_fw)
plot(residuals_percentil)

# estimates
summary(glmm_fw)

#multiplas comparações do modelo
mtc_percentil <- emmeans(glmm_fw, pairwise~Infection*Genotype)
summary(mtc_percentil)

### espaço vascular ####################################################
