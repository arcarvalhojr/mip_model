#################################################################
### processamento e analise dos dados relacionados a figura 5 ###

# pacotes solicitados para as analises
library(tidyverse)
library(readxl)
library(writexl)
library(lme4)
library(lmerTest)
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

# o grupo BNTac precisar ser o fator de referencia
comp_data_dams$Genotype <- relevel(comp_data_dams$Genotype, ref = "BNTac")
levels(comp_data_dams$Genotype)

# fetus data set
comp_data_weight <- read_xlsx("Data/Model_comparison/MODEL DATA_WEIGHT_COMP.xlsx")
str(model_data_weight)

# transformando as variaveis infection e genotype em fatores
comp_data_weight$Genotype <- as.factor(comp_data_weight$Genotype)
comp_data_weight$Infection <- as.factor(comp_data_weight$Infection)
comp_data_weight$Mice_ID <- as.factor(comp_data_weight$Mice_ID)

# o grupo BNTac precisar ser o fator de referencia
comp_data_weight$Genotype <- relevel(comp_data_weight$Genotype, ref = "BNTac")
levels(comp_data_weight$Genotype)


### slecionando variaveis especificas ################################
cytok_data_dams <- comp_data_dams %>%
  select(Genotype, Mice_ID, Infection, IL10_sp, MCP1_sp, IFN_sp, TNF_sp,
         MCP1_sr, IFN_sr, TNF_sr)

cytok_data_placenta <- comp_data_weight %>%
  select(Genotype, Mice_ID, Infection, MCP1, IFN)


#### MCP-1 soro #######################################################

#visualizando o dado
ggplot(cytok_data_dams, aes(x= Infection, y= MCP1_sr, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()


# Verificação e extração de outliers
outs_mcp1_sr <- cytok_data_dams %>%
  group_by(Genotype) %>%
  summarise(
    grubbs_pvalue = grubbs.test(MCP1_sr)$p.value,
    outlier_value = ifelse(grubbs_pvalue < 0.05,
                           MCP1_sr[which.max(abs(MCP1_sr - mean(MCP1_sr)))],
                           NA_real_)) %>%
  filter(!is.na(outlier_value)) %>%
  ungroup()

outs_mcp1_sr

#calculando o numero de observações
n_mcp1_sr <- cytok_data_dams %>%
  filter(!is.na(MCP1_sr)) %>%
  group_by(Genotype, Infection) %>%
  summarise(observacoes= sum(!is.na(MCP1_sr)))
n_mcp1_sr

# modelo glm
glm_mcp1_sr <- glm(MCP1_sr~Genotype, data = cytok_data_dams,
                   family = Gamma(link = "identity"))

# diagnostico do modelo
residuals_mcp1_sr <- simulateResiduals(fittedModel = glm_mcp1_sr)
plot(residuals_mcp1_sr)

visualize(glm_mcp1_sr)

# esyimates
summary(glm_mcp1_sr)


### MCP-1 baço #######################################################

#visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= MCP1_sp, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()


#teste de hipotese
wilcox_mcp1_sp <- wilcox.test(MCP1_sp~Genotype, data = cytok_data_dams,
                              na.action = na.omit)
print(wilcox_mcp1_sp)

# modelo glm
glm_mcp1_sp <- glm(MCP1_sp~Genotype, data = cytok_data_dams,
                   family = Gamma(link = "identity"))

# diagnostico do modelo
residuals_mcp1_sp <- simulateResiduals(fittedModel = glm_mcp1_sp)
plot(residuals_mcp1_sp)

visualize(glm_mcp1_sp)

# estimates
summary(glm_mcp1_sp)

#### TNF soro #####################################################

#visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= TNF_sr, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()

# modelo glm
glm_tnf_sr <- glm(TNF_sr~Genotype, data = cytok_data_dams,
                   family = Gamma(link = "identity"))

# diagnostico do modelo
residuals_tnf_sr <- simulateResiduals(fittedModel = glm_tnf_sr)
plot(residuals_tnf_sr)

visualize(glm_tnf_sr)

# estimates
summary(glm_tnf_sr)


#teste de hipotese
wilcox_tnf_sr <- wilcox.test(TNF_sr~Genotype, data = cytok_data_dams,
                             na.action = na.omit)
print(wilcox_tnf_sr)

####### TNF baço#####################################################

#visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= TNF_sp, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()

# modelo glm
glm_tnf_sp <- glm(TNF_sp~Genotype, data = cytok_data_dams,
                  family = Gamma(link = "identity"))

# diagnostico do modelo
residuals_tnf_sp <- simulateResiduals(fittedModel = glm_tnf_sp)
plot(residuals_tnf_sp)

visualize(glm_tnf_sp)

# estimates
summary(glm_tnf_sp)

#teste de hipotese
wilcox_tnf_sp <- wilcox.test(TNF_sp~Genotype, data = cytok_data_dams,
                             na.action = na.omit)
print(wilcox_tnf_sp)

####### IFN soro#####################################################

#visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= IFN_sr, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()

# modelo glm
glm_ifn_sr <- glm(IFN_sr~Genotype, data = cytok_data_dams,
                  family = Gamma(link = "identity"))

# diagnostico do modelo
residuals_ifn_sr <- simulateResiduals(fittedModel = glm_ifn_sr)
plot(residuals_ifn_sr)

visualize(glm_ifn_sr)

# estimates
summary(glm_ifn_sr)

#teste de hipotese
wilcox_ifn_sr <- wilcox.test(IFN_sr~Genotype, data = cytok_data_dams,
                             na.action = na.omit)
print(wilcox_ifn_sr)

####### IFN baço#####################################################

#visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= IFN_sp, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()

# modelo glm
glm_ifn_sp <- glm(IFN_sp~Genotype, data = cytok_data_dams,
                  family = Gamma(link = "identity"))

# diagnostico do modelo
residuals_ifn_sp <- simulateResiduals(fittedModel = glm_ifn_sp)
plot(residuals_ifn_sp)

visualize(glm_ifn_sp)

# estimates
summary(glm_ifn_sp)

#teste de hipotese
wilcox_ifn_sp <- wilcox.test(IFN_sp~Genotype, data = cytok_data_dams,
                             na.action = na.omit)
print(wilcox_ifn_sp)


####### IL-10 baço#####################################################

#visualizando o dado
ggplot(comp_data_dams, aes(x= Infection, y= IL10_sp, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()

#modelo glm
glm_il10 <- glm(IL10_sp~Infection*Genotype, data = cytok_data_dams)



#diagnostico do modelo
residuals_il10 <- simulateResiduals(fittedModel = glm_il10)
plot(residuals_il10)

visualize(glm_il10)

# estimates
summary(glm_il10)


####### MCP-1 placenta###################################################

#visualizando o dado
ggplot(comp_data_weight, aes(x= Infection, y= MCP1, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()


#modelo glm
glm_mcp1_placenta <- glm(MCP1 ~ Infection * Genotype, data = cytok_data_placenta)


#diagnostico do modelo
residuals_mcp1_placenta <- simulateResiduals(fittedModel = glm_mcp1_placenta)
plot(residuals_mcp1_placenta)

visualize(glm_mcp1_placenta)

# estimates
summary(glm_mcp1_placenta)


####### IFN placenta###################################################

#visualizando o dado
ggplot(comp_data_weight, aes(x= Infection, y= IFN, fill= Infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~Genotype)+
  theme_bw()


#modelo glm
glm_ifn_placenta <- glm(IFN ~ Genotype, data = cytok_data_placenta)
summary(glm_ifn)

#diagnostico do modelo
residuals_ifn_placenta <- simulateResiduals(fittedModel = glm_ifn_placenta)
plot(residuals_ifn_placenta)

visualize(glm_ifn_placenta)

# estimates
summary(glm_ifn_placenta)
