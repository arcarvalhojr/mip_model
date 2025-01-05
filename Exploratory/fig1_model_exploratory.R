#################################################################
### processamento e analise dos dados relacionados a figura 1 ###

#pacotes solicitados para as analises
library(tidyverse)
library(readxl)
library(writexl)
library(lme4)
library(lmerTest)
library(DHARMa)
library(performance)
library(flexplot)
library(brglm2)

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
model_data_weight$Infection <- as.factor(model_data_weight$Infection)
str(model_data_weight)

# dams peripheral parasitemia data set
model_parasitemia <- read_xlsx("Data/Model/MODEL_PARASITEMIA.xlsx")
str(model_parasitemia)
#a variavel Infecção precisa ser categorica
model_parasitemia$Infection <- as.factor(model_parasitemia$Infection)
str(model_parasitemia)


#### analise da infecção sobre o peso do baço ############################

# visualizando o dado
ggplot(model_data_dams, aes(x= Infection, y= Spleen_w, fill= Infection)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.8),
              size= 3, show.legend = FALSE)

  
#modelo glm
glm_sw <- glm(Spleen_w~Infection, data = model_data_dams)

#diagnóstico do modelo por glm
residuals_sw <- simulateResiduals(fittedModel = glm_sw)
plot(residuals_sw)

visualize(glm_sw)

# estimates
summary(glm_sw)

#### analise do percentual de parasitemia no sangue periferico ###############

#visualizando o dado
ggplot(model_parasitemia, aes(x= Preg, y= Parasitemia, fill= Infection))+
  geom_boxplot(outlier.shape = NA, position = position_dodge(width= 0.8)) +
  geom_jitter(position = position_jitterdodge(
    jitter.width = 0.2, dodge.width = 0.8), size= 2.5)


#modelo glm
glm_parasitemia <- glm(Parasitemia~Infection+Preg, data = model_parasitemia,
                       family = Gamma(link = "identity"))

#diagnóstico do modelo por glm
residuals_para <- simulateResiduals(fittedModel = glm_parasitemia)
plot(residuals_para)

check_model(glm_parasitemia)

visualize(glm_parasitemia)

# estimates
summary(glm_parasitemia)


#### analise da porcentagem de pre-termo #################################

#visualizando o dado
ggplot(model_data_dams, aes(x= Infection, fill= PTD)) +
  geom_bar()

#modelo glm
glm_ptd <- glm(factor(PTD) ~ Infection, data = model_data_dams,
               family = binomial, method = "brglmFit")


# diagnostico do modelo
residuals_ptd <- simulateResiduals(fittedModel = glm_ptd)
plot(residuals_ptd)

# estimates
summary(glm_ptd)

### Analise de peso dos fetos ###########################################

#visualisando o dado
ggplot(model_data_weight, aes(x= Infection, y= Fetal_weight, fill= Infection)) +
  geom_boxplot(show.legend = FALSE)

#modelo lmm
lmm_fw <- lmer(Fetal_weight~Infection+Litter_size+Placenta_weight+(1|Mice_ID),
               data= model_data_weight)

#diagnostico do modelo
residuals_fw <- simulateResiduals(fittedModel = lmm_fw)
plot(residuals_fw)

visualize(lmm_fw)

# estimates
summary(lmm_fw)

#verificando outliers
outs_model_fw <- model_data_weight %>%
  group_by(Infection) %>%
  summarise(
    grubbs_pvalue = grubbs.test(Fetal_weight)$p.value,
    outlier = ifelse(grubbs_pvalue < 0.05, Fetal_weight[which.max(
      abs(Fetal_weight - mean(Fetal_weight)))], NA))
outs_model_fw

### correlação entre o peso dos fetos e da placenta ######################

#filtrando apenas dois grupos de infecção
model_cor_pw_fw <- model_data_weight %>%
  filter(Infection %in% c(0, 8) & Fetal_weight & Placenta_weight) %>%
  dplyr::select(Mice_ID, Fetus_ID, Infection, Fetal_weight, Placenta_weight)
str(model_cor_pw_fw)

# Salvando como .xlsx
write_xlsx(model_cor_pw_fw, "Data/Model/model_cor_pw_fw.xlsx")

# analise de correlação
cor_test <- model_cor_pw_fw %>%
  group_by(Infection) %>%
  summarise(
    correlation = cor.test(Placenta_weight,
                           Fetal_weight, method = "spearman")$estimate,
    p_value = cor.test(Placenta_weight,
                       Fetal_weight, method = "spearman")$p.value)

print(cor_test)

### analise do racio ####################################################

# visualisandoo dado
ggplot(model_data_weight, aes(x= Infection, y= FW_PW, fill= Infection)) +
  geom_boxplot()

#modelo lmm
lmm_ratio <- lmer(FW_PW~Infection+(1|Mice_ID), data= model_data_weight)

#diagnostico do modelo
residuals_ratio <- simulateResiduals(fittedModel = lmm_ratio)
plot(residuals_ratio)

# estimates
summary(lmm_ratio)

### analise da viabilidade fetal #######################################

# visualizando o dado
ggplot(model_data_weight, aes(x= Infection, fill= Stillbirth)) +
  geom_bar(position = position_dodge(width = 1))

#modelo GLMM
glm_stb <- glmer(factor(Stillbirth)~Infection+(1|Mice_ID),
                 data= model_data_weight, family = "binomial")

#diagnostico do modelo
residuals_stb <- simulateResiduals(fittedModel = glm_stb)
plot(residuals_stb)

# estimates
summary(glm_stb)

### quantificação de pb18s ################################################

#selecionando as variaveis especificas e removendo o grupo n infectado
model_pb18s_data <- model_data_weight %>%
  dplyr::select(Fetus_ID, Infection, pb18s) %>%
  filter(Infection != "0" & !is.na(pb18s))
str(pb18s_data)

# Salvando como .xlsx
write_xlsx(model_pb18s_data, "Data/Model/model_pb18s_data.xlsx")

# visualisando o dado
ggplot(model_pb18s_data, aes(x= Infection, y= pb18s, fill= Infection)) +
  geom_boxplot()

#modelo glm
glm_pb18s <- glm(pb18s~Infection, data = model_pb18s_data,
                 family = Gamma(link = "log"))


#diagnostico do modelo
residuals_pb18s <- simulateResiduals(fittedModel = glm_pb18s)
plot(residuals_pb18s)

check_model(glm_pb18s)

visualize(glm_pb18s)

# estimates
summary(glm_pb18s)


######### criando uma tabela com os resultados das analises ################

# requisaitando o pacote
library(gtsummary)
library(kableExtra)

# guardando os resultados em uma lista
models <- list("Spleen weight" = glm_sw,
               "Fetal weight" = lmm_fw,
               "Fetal/placental weight ratio" = lmm_ratio,
               "Pre-term birth" = glm_ptd,
               "Stillbirth" = glm_stb)

models_parasitemia <- list("Peripheral parasitemia" = glm_parasitemia,
                           "Pb18s mRNA expression" = glm_pb18s)

# definindo o nome das variaveis
coef_map <- c("Infection2" = "PbNK65+",
              "Infection4" = "PbNK65++",
              "Infection8" = "PbNK65+++",
              "Litter_size" = "Litter size",
              "Placenta_weight" = "Placental weight",
              "(Intercept)" = "Intercept")


coef_map_parasitemia <- c()


# formatando a tabela com modelsummary
table_ms <- modelsummary(models,
             coef_map = coef_map,
             gof_map = c("nobs"),
             statistic = "conf.int",
             stars = TRUE,
             output = "data.frame")


kbl(table_ms, booktabs = TRUE, escape = FALSE) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  add_footnote("CI: Confidence Interval. Stars indicate significance: *** p<0.001, ** p<0.01, * p<0.05", notation = "symbol")




# formatando tabelas com gtsummary
model_data_dams |>
  select(Infection, Spleen_w, RBC, HCT, HGB) |>
  tbl_summary(by = Infection)

# Gerando tabelas de regressão para cada modelo
tables <- lapply(models, function(model) {
  tbl_regression(model, exponentiate = FALSE)
})

# Empilhando as tabelas em uma única tabela
final_table <- tbl_merge(
  tbls = tables, # Apenas `tables`, sem envolver com `list()`
  tab_spanner = names(models) # Nomes das tabelas
) %>%
  modify_header(label ~ "**Predictors**") %>%  # Nomes das variáveis em negrito
  modify_fmt_fun(
    p.value = function(x) style_pvalue(x, digits = 3, italic = TRUE)  # P-value em itálico
  ) %>%
  bold_labels() %>%  # Deixa os rótulos das variáveis em negrito
  italicize_levels()  # Deixa os níveis dos fatores em itálico
final_table
