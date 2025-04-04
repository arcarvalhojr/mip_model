######## Exploratory analysis of data related to figure 1 ##########


# import libraries --------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(lme4)         # For fitting linear and mixed-effects models
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks (R², outliers, assumptions, etc.)
library(flexplot)     # For visual diagnostics and exploratory data analysis



# import clean data -------------------------------------------------------

# dams data set
model_dams <- readRDS("Data/Clean_data/model_dams.rds")

# fetus data set
model_weights <- readRDS("Data/Clean_data/model_weights.rds")

# dams peripheral parasitemia data set
model_parasitemia <- readRDS("Data/Clean_data/model_parasitemia.rds")

# pb18s data set
model_pb18s <- readRDS("Data/Clean_data/model_pb18s_clean.rds")

# Analysis of the effect of infection on spleen weight --------------------

# looking the data
ggplot(model_dams, aes(x = infection, y = spleen_w, fill = infection)) +
  geom_boxplot(show.legend = FALSE) 

  
# glm model
glm_sw <- glm(spleen_w ~ infection, data = model_dams)

# model diagnostic
simulateResiduals(fittedModel = glm_sw, plot = TRUE)

visualize(glm_sw, plot = "residuals")

check_outliers(glm_sw, method = "cook")


# Analyse of the effect of pregnancy on susceptibility to infection --------

# looking the data
ggplot(model_parasitemia, aes(x = preg, y = parasitemia, fill = infection))+
  geom_boxplot()

# glm model
#it doesn't seem like there's an interaction, but let's check it anyway

glm_parasitemia_1 <- glm(parasitemia ~ infection + preg,
                         data = model_parasitemia,
                         family = Gamma(link = "identity"))

glm_parasitemia_2 <- glm(parasitemia ~ infection * preg,
                         data = model_parasitemia,
                         family = Gamma(link = "identity"))

anova(glm_parasitemia_1, glm_parasitemia_2)

#yep, there's no interaction

# model diagnostic
simulateResiduals(fittedModel = glm_parasitemia_1, plot = TRUE)

visualize(glm_parasitemia_1, plot = "residuals")

check_outliers(glm_parasitemia_1, method = "cook")


# Analyse of infection on fetal weight ------------------------------------

# looking the data
ggplot(model_weights, aes(x = infection,y = fetal_weight, fill = infection)) +
  geom_boxplot(show.legend = FALSE)

# lmm models
lmm_fw <- lmer(fetal_weight ~ infection + (1|mice_id), data= model_weights)

# with litter size
lmm_fw1 <- lmer(fetal_weight ~ infection + litter_size + (1|mice_id),
                data = model_weights)

visualize(lmm_fw1, plot = "model", 
          formula = fetal_weight ~ litter_size + mice_id | infection,
          sample = 20)  # looking the model
#it seems like litter size impact fetal weight

# with placental weight
lmm_fw2 <- lmer(fetal_weight ~ infection + litter_size + placenta_weight + 
                  (1|mice_id), data = model_weights)

visualize(lmm_fw2, plot = "model", 
          formula = fetal_weight ~ placenta_weight + mice_id | infection,
          sample = 20)
#also seems that pw impact fetal weight

# comparing the fit of the models
compare_performance(lmm_fw, lmm_fw1, lmm_fw2, rank = TRUE, verbose = FALSE)

# comparing the effect of infection
lmm_fw_full <- lmer(fetal_weight ~ infection + litter_size + placenta_weight + 
                      (1|mice_id), data = model_weights)

lmm_fw_reduced <- lmer(fetal_weight ~ litter_siz e + placenta_weight +
                         (1|mice_id), data = model_weights)

anova(lmm_fw_full, lmm_fw_reduced)

# model diagnostic
visualize(lmm_fw_full, plot = "residuals")

simulateResiduals(fittedModel = lmm_fw_full, plot = TRUE)

check_outliers(lmm_fw_full, method = "cook")


# Analyse of infection on fetal/placental weight ratio --------------------

# looking the data
model_weights %>% 
  filter(!is.na(ratio)) %>% 
  ggplot(aes(x = infection, y = ratio, fill = infection)) +
  geom_boxplot(show.legend = FALSE)

# lmm models
lmm_ratio <- lmer(ratio ~ infection + (1|mice_id), 
                  data = model_weights)


# with litter size
lmm_ratio1 <- lmer(ratio ~ infection + litter_size + (1|mice_id), 
                   data = model_weights)

visualize(lmm_ratio1, plot = "model", 
          formula = ratio ~ litter_size + mice_id | infection,
          sample = 20)

# comparing the fit of the models
compare_performance(lmm_ratio, lmm_ratio1,
                    rank = TRUE, verbose = FALSE)

# comparing the effect of infection
lmm_ratio_full <- lmer(ratio ~ infection + litter_size + (1|mice_id), 
                       data = model_weights)

lmm_ratio_reduced <- lmer(ratio ~ litter_size + (1|mice_id), 
                          data = model_weights)

anova(lmm_ratio_full, lmm_ratio_reduced)

# model diagnostic
simulateResiduals(fittedModel = lmm_ratio_full, plot = TRUE)

check_outliers(lmm_ratio_full, method = "cook")

#looks bad, deviations detected

# let's try without litter size
lmm_ratio_2 <- lmer(ratio ~ infection + (1|mice_id), 
                   data = model_weights)

# model diagnostic
simulateResiduals(fittedModel = lmm_ratio_2, plot = TRUE)

visualize(lmm_ratio_2, plot = "residuals")

check_outliers(lmm_ratio_2, method = "cook")

#better


# Analyse of infection on fetal viability ---------------------------------

# looking the data
ggplot(model_weights, aes(x = infection, fill = alive)) +
  geom_bar(position = position_dodge(width = 1))

# glmm model
glm_stb <- glmer(factor(alive) ~ infection + (1|mice_id),
                 data = model_weights, family = "binomial")

# model diagnostic
simulateResiduals(fittedModel = glm_stb, plot = TRUE)


# Analyse of placental pb18s relative expression ---------------------------

# looking the data
ggplot(model_pb18s, aes(x = infection, y = log(pb18s), fill = infection)) +
  geom_boxplot(show.legend = FALSE)

# glm model
glm_pb18s <- glm(pb18s ~ infection, data = model_pb18s,
                 family = Gamma(link = "log"))


# model diagnostic
simulateResiduals(fittedModel = glm_pb18s, plot = TRUE)

check_outliers(glm_pb18s, method = "cook")

#outlier detected

# removing outs
outs_pb18s <- check_outliers(glm_pb18s, method = "cook")

model_pb18s_clean <- model_pb18s %>%
  slice(-which(outs_pb18s))

# glm model without outs
glm_pb18s_2 <- glm(pb18s ~ infection, data = model_pb18s_clean,
                 family = Gamma(link = "log"))

simulateResiduals(fittedModel = glm_pb18s_2, plot = TRUE)

# saving clean data
#saveRDS(model_pb18s_clean, "Data/Clean_data/model_pb18s_clean.rds")
