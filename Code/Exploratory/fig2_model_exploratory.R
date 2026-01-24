######## Exploratory analysis of data related to figure 2 ##########


# import libraries --------------------------------------------------------

library(tidyverse)  # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(lme4)       # For fitting linear and mixed-effects models
library(lmerTest)   # Provide p-value for mixed-effects models estimated with the lmer()
library(DHARMa)     # For residual diagnostics using simulated residuals
library(flexplot)     # For visual diagnostics and exploratory data analysis


# import clean data -------------------------------------------------------

# fetus data
model_weights <- readRDS("Data/Clean_data/model_weights.rds")


# Analyse of infection on fetal weight ------------------------------------

# looking the data
ggplot2::ggplot(
  model_weights, aes(x = infection,y = fetal_weight, fill = infection)) +
  geom_boxplot(show.legend = FALSE)

model_weights %>% 
  dplyr::group_by(infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(fetal_weight)),
                   n_obs_NA = sum(is.na(fetal_weight)))

# lmm models
# base model
lmm_fw <- lmerTest::lmer(
  fetal_weight ~ infection + (1|mice_id), data= model_weights)

# with litter size
lmm_fw1 <- lmerTest::lmer(fetal_weight ~ infection + litter_size + (1|mice_id),
                data = model_weights)

flexplot::visualize(lmm_fw1, plot = "model", 
          formula = fetal_weight ~ litter_size + mice_id | infection,
          sample = 20)  # looking the model

performance::compare_performance(lmm_fw, lmm_fw1, rank = TRUE, verbose = FALSE)

anova(lmm_fw, lmm_fw1)

#it seems like litter size impact fetal weight in all groups

# with placental weight
lmm_fw2 <- lmerTest::lmer(
  fetal_weight ~ infection + litter_size + placenta_weight + 
                  (1|mice_id), data = model_weights)

flexplot::visualize(lmm_fw2, plot = "model", 
          formula = fetal_weight ~ placenta_weight + mice_id | infection,
          sample = 20)

# comparing the fit of the models
performance::compare_performance(lmm_fw1, lmm_fw2, rank = TRUE, verbose = FALSE)

#also seems that pw impact fetal weight


# comparing the effect of infection
lmm_fw_full <- lmerTest::lmer(
  fetal_weight ~ infection + litter_size + placenta_weight + 
                      (1|mice_id), data = model_weights)

lmm_fw_reduced <- lmerTest::lmer(fetal_weight ~ litter_size + placenta_weight +
                         (1|mice_id), data = model_weights)

anova(lmm_fw_full, lmm_fw_reduced)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_fw_full, plot = TRUE)

flexplot::visualize(lmm_fw_full, plot = "residuals")

performance::check_outliers(lmm_fw_full, method = "cook")

performance::check_model(lmm_fw_full)

# model output
performance::model_performance(lmm_fw_full)

summary(lmm_fw_full)

# Analyse of infection on placental weight --------------------------------

# Looking the data
ggplot2::ggplot(
  model_weights, aes(x= infection, y= placenta_weight, fill= infection)) +
  geom_boxplot(show.legend = FALSE)


model_weights %>% 
  dplyr::group_by(infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(placenta_weight)),
                   n_obs_NA = sum(is.na(placenta_weight)))

# lmm model
#base model
lmm_pw <- lmerTest::lmer(placenta_weight ~ infection + (1|mice_id),
                     data= model_weights)

#with litter size
lmm_pw_1 <- lmerTest::lmer(placenta_weight ~ infection + litter_size + (1|mice_id),
               data= model_weights)

performance::compare_performance(lmm_pw, lmm_pw_1,
                                 rank = TRUE, verbose = FALSE)

anova(lmm_pw, lmm_pw_1)

# compare the effect of infection
lmm_pw_full <- lmerTest::lmer(
  placenta_weight ~ infection + (1|mice_id), data = model_weights)

lmm_pw_reduced <- lmerTest::lmer(
  placenta_weight ~  + (1|mice_id), data = model_weights)

anova(lmm_pw_full, lmm_pw_reduced)

#no effect for infection on placental weight

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_pw_full, plot = TRUE)

flexplot::visualize(lmm_pw_full, plot = "residuals")

performance::check_model(lmm_pw_full)

performance::check_outliers(lmm_pw_full, method = "cook")

# model output
performance::model_performance(lmm_pw_full)

summary(lmm_pw_full)

# Analyse of infection on fetal/placental weight ratio --------------------

# looking the data
model_weights %>% 
  ggplot2::ggplot(aes(x = infection, y = ratio, fill = infection)) +
  geom_boxplot(show.legend = FALSE)


model_weights %>% 
  dplyr::group_by(infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(ratio)),
                   n_obs_NA = sum(is.na(ratio)))

# lmm model
#base model
lmm_ratio_1 <- lmerTest::lmer(ratio ~ infection + (1|mice_id), 
                  data = model_weights)


#with litter size
lmm_ratio_2 <- lmerTest::lmer(ratio ~ infection + litter_size + (1|mice_id), 
                   data = model_weights)

flexplot::visualize(lmm_ratio_2, plot = "model", 
          formula = ratio ~ litter_size + mice_id | infection,
          sample = 20)

# comparing the fit of the models
performance::compare_performance(lmm_ratio_1, lmm_ratio_2,
                    rank = TRUE, verbose = FALSE)

anova(lmm_ratio_1, lmm_ratio_2)


# compare the effect of infection
lmm_ratio_full <- lmerTest::lmer(ratio ~ infection + litter_size + (1|mice_id), 
                       data = model_weights)

lmm_ratio_reduced <- lmerTest::lmer(ratio ~ litter_size + (1|mice_id), 
                          data = model_weights)

anova(lmm_ratio_full, lmm_ratio_reduced)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_ratio_full, plot = TRUE)

flexplot::visualize(lmm_ratio_full, plot = "residuals")

performance::check_outliers(lmm_ratio_full, method = "cook")

performance::check_model(lmm_ratio_full)


#looks bad, deviations detected
# let's try without litter size

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_ratio_1, plot = TRUE)

flexplot::visualize(lmm_ratio_1, plot = "residuals")

performance::check_outliers(lmm_ratio_1, method = "cook")

performance::check_model(lmm_ratio_1)

#better

# model output
performance::model_performance(lmm_ratio_1)

summary(lmm_ratio_1)

# Analysis of infection on the chances of "fgr" ---------------------------

model_fgr <- model_weights %>%
  dplyr::mutate(percentile_5 = quantile(fetal_weight[infection == "pbnk65-"],
                                 probs = 0.05, na.rm = TRUE)) %>%  # getting 5º percentile
  
  dplyr::mutate(
    percentile = ifelse(fetal_weight >= percentile_5,
                        "within_range", "below_range")) %>% # classification by the 5º percentile 
  dplyr::mutate(
    percentile = factor(percentile, levels = c(
      "within_range", "below_range"))) %>%  
  dplyr::select(mice_id, fetus_id, infection, litter_size, placenta_weight, fetal_weight,
                percentile_5, percentile) 


# Saving data
#saveRDS(model_fgr, "Data/Clean_data/model_fgr.rds")


# Looking the data
ggplot2::ggplot(model_fgr, aes(x = infection, fill = percentile)) +
  geom_bar(position = position_dodge())

model_fgr %>% 
  dplyr::count(percentile, by = infection)

# glmm model
#base
glmm_fgr <- lme4::glmer(percentile ~ infection + (1|mice_id),
                    data = model_fgr, family = "binomial")

#with litter size
glmm_fgr_1 <- lme4::glmer(percentile ~ infection + litter_size + (1|mice_id),
                          data = model_fgr, family = "binomial")

#compare fits
performance::compare_performance(glmm_fgr, glmm_fgr_1,
                                 rank = TRUE, verbose = FALSE)

anova(glmm_fgr, glmm_fgr_1)

#with placenta weight
glmm_fgr_2 <- lme4::glmer(percentile ~ infection + placenta_weight + (1|mice_id),
                          data = model_fgr, family = "binomial")

#singularity detected, maybe overfiting

# compare the effect of infection
glmm_fgr_full <- lme4::glmer(percentile ~ infection + (1|mice_id),
                         data = model_fgr, family = "binomial")

glmm_fgr_reduced <- lme4::glmer(percentile ~ + (1|mice_id),
                             data = model_fgr, family = "binomial")


anova(glmm_fgr_full, glmm_fgr_reduced)

#there is an effect for infection

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glmm_fgr_full, plot = TRUE)

performance::check_predictions(glmm_fgr_full) %>% 
  plot(type = "discrete_both")

# model output
performance::model_performance(glmm_fgr_full)

summary(glmm_fgr_full)

# Analyse of infection on fetal viability ---------------------------------

# looking the data
ggplot2::ggplot(model_weights, aes(x = infection, fill = alive)) +
  geom_bar(position = position_dodge(width = 1))


model_weights %>% 
  dplyr::filter(!is.na(alive)) %>% 
  dplyr::count(alive, by = infection)

# glmm model
#base model
glmm_stb <- lme4::glmer(alive ~ infection + (1|mice_id),
                 data = model_weights, family = "binomial")

#with litter size
glmm_stb_1 <- lme4::glmer(alive ~ infection + litter_size + (1|mice_id),
                        data = model_weights, family = "binomial")

# compare fits
performance::compare_performance(glmm_stb, glmm_stb_1, 
                                 rank = TRUE, 
                                 verbose = FALSE)

anova(glmm_stb, glmm_stb_1)

#with placenta weight
glmm_stb_2 <- lme4::glmer(
  alive ~ infection + litter_size + placenta_weight + (1|mice_id),
  data = model_weights, family = "binomial")

# compare fits
performance::compare_performance(glmm_stb_1, glmm_stb_2, 
                                 rank = TRUE, 
                                 verbose = FALSE)

#model without placenta weight fits better

# compare the effect of infection
glmm_stb_full <- lme4::glmer(alive ~ infection + litter_size + (1|mice_id),
  data = model_weights, family = "binomial")

glmm_stb_reduced <- lme4::glmer(alive ~ litter_size + (1|mice_id),
  data = model_weights, family = "binomial")

anova(glmm_stb_full, glmm_stb_reduced)

#no effect 

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glmm_stb_full, plot = TRUE)

performance::check_predictions(glmm_stb_full) %>% 
  plot(type = "discrete_both")

# model output
performance::model_performance(glmm_stb_full)

summary(glmm_stb_full)


# Analyse of infection on placental vascular space area -------------------

# Looking the data
ggplot2::ggplot(
  model_weights, aes(x= infection, y= vascular_space, fill = infection))+
  geom_boxplot(show.legend = FALSE)


model_weights %>% 
  dplyr::group_by(infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(vascular_space)),
                   n_NA = sum(is.na(vascular_space)))

#many Nas. Not all placentas had good quality H&E slides for analysis


# lmm model
lmm_vasc_space <- lmerTest::lmer(
  vascular_space ~ infection + (1|mice_id), data= model_weights)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_vasc_space, plot = TRUE)

flexplot::visualize(lmm_vasc_space, plot = "residuals")

performance::check_model(lmm_vasc_space)

performance::check_outliers(lmm_vasc_space, method = "cook")

# model output
performance::model_performance(lmm_vasc_space)

summary(lmm_vasc_space)


# Save models output ------------------------------------------------------

models_list <- dplyr::tibble(
  model_name = c("fetal weight", "placental weight", "fw/pw ratio",
                 "fgr", "fetal viability", "vascular space"),
  model_output = list(lmm_fw_full, lmm_pw_full, lmm_ratio_1,
                      glmm_fgr_full, glmm_stb_full, lmm_vasc_space)
)


#saveRDS(models_list, "Code/Results/fig2_model_outputs.rds")
