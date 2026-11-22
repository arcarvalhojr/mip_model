######## Exploratory analysis of data related to figure 2 ##########


# import libraries --------------------------------------------------------

library(tidyverse)  # For data manipulation and visualization 
library(lmerTest)   # For fitting mixed-effects models (with p-value)
library(DHARMa)     # For residual diagnostics using simulated residuals
library(flexplot)   # For visual diagnostics 
library(emmeans)    # For multiple testing correction

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

# lmm model

#dams age at start (Gd0.5) and litter size are covariates that can influence fw

lmm_fw <- lmerTest::lmer(
  fetal_weight ~ infection + litter_size + age_at_start +
                            (1|mice_id), data = model_weights)

# looking the model
flexplot::visualize(lmm_fw, plot = "model", 
          formula = fetal_weight ~ litter_size + mice_id | infection,
          sample = 20)


flexplot::visualize(lmm_fw, plot = "model", 
                    formula = fetal_weight ~ age_at_start + mice_id | infection,
                    sample = 20)


# comparing the effect of infection in the model
lmm_fw_full <- lmm_fw

lmm_fw_reduced <- lmerTest::lmer(
  fetal_weight ~ litter_size + age_at_start +
                         (1|mice_id), data = model_weights)

anova(lmm_fw_full, lmm_fw_reduced)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_fw_full, plot = TRUE)

flexplot::visualize(lmm_fw_full, plot = "residuals")

performance::check_collinearity(lmm_fw_full)

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
lmm_pw <- lmerTest::lmer(
  placenta_weight ~ infection + litter_size + age_at_start +
    (1|mice_id), data= model_weights)


# looking the model
flexplot::visualize(lmm_pw, plot = "model", 
                    formula = placenta_weight ~ litter_size + mice_id | infection,
                    sample = 20)


flexplot::visualize(lmm_pw, plot = "model", 
                    formula = placenta_weight ~ age_at_start + mice_id | infection,
                    sample = 20)


# compare the effect of infection
lmm_pw_full <- lmm_pw

lmm_pw_reduced <- lmerTest::lmer(
  placenta_weight ~ litter_size + age_at_start +
    (1|mice_id), data = model_weights)


anova(lmm_pw_full, lmm_pw_reduced)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_pw_full, plot = TRUE)

flexplot::visualize(lmm_pw_full, plot = "residuals")

performance::check_collinearity(lmm_pw_full)


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
lmm_ratio <- lmerTest::lmer(
  ratio ~ infection + litter_size + age_at_start +
    (1|mice_id), data = model_weights)


flexplot::visualize(lmm_ratio, plot = "model", 
          formula = ratio ~ litter_size + mice_id | infection,
          sample = 20)

flexplot::visualize(lmm_ratio, plot = "model", 
                    formula = ratio ~ age_at_start + mice_id | infection,
                    sample = 20)


# compare the effect of infection
lmm_ratio_full <- lmm_ratio

lmm_ratio_reduced <- lmerTest::lmer(
  ratio ~ litter_size + age_at_start +
    (1|mice_id), data = model_weights)

anova(lmm_ratio_full, lmm_ratio_reduced)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_ratio_full, plot = TRUE)

flexplot::visualize(lmm_ratio_full, plot = "residuals")

performance::check_collinearity(lmm_ratio_full)

# model output
performance::model_performance(lmm_ratio_full)

summary(lmm_ratio_full)


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
  dplyr::select(mice_id, fetus_id, infection, litter_size, age_at_start,
                placenta_weight, fetal_weight, percentile_5, percentile) 


# Saving data
#saveRDS(model_fgr, "Data/Clean_data/model_fgr.rds")


# Looking the data
ggplot2::ggplot(model_fgr, aes(x = infection, fill = percentile)) +
  geom_bar(position = position_dodge())

model_fgr %>% 
  dplyr::count(percentile, by = infection)

# glmm model
glmm_fgr <- lme4::glmer(
  percentile ~ infection + litter_size + age_at_start +
    (1|mice_id), data = model_fgr, family = "binomial")


# compare the effect of infection
glmm_fgr_full <- glmm_fgr

glmm_fgr_reduced <- lme4::glmer(
  percentile ~ litter_size + age_at_start +
    (1|mice_id), data = model_fgr, family = "binomial")


anova(glmm_fgr_full, glmm_fgr_reduced)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glmm_fgr_full, plot = TRUE)

performance::check_predictions(glmm_fgr_full) %>% 
  plot(type = "discrete_both")

performance::check_collinearity(glmm_fgr_full)

# model output
performance::model_performance(glmm_fgr_full)

summary(glmm_fgr_full)


# Analyse of infection on fetal viability ---------------------------------

# looking the data
ggplot2::ggplot(model_weights, aes(x = infection, fill = fetal_viability)) +
  geom_bar(position = position_dodge(width = 1))


model_weights %>% 
  dplyr::filter(!is.na(fetal_viability)) %>% 
  dplyr::count(fetal_viability, by = infection)

# glmm model
glmm_fv <- lme4::glmer(
  fetal_viability ~ infection + litter_size + age_at_start +
    (1|mice_id), data = model_weights, family = "binomial")


# compare the effect of infection
glmm_fv_full <- glmm_fv

glmm_fv_reduced <- lme4::glmer(
  fetal_viability ~ litter_size + age_at_start +
    (1|mice_id), data = model_weights, family = "binomial")

anova(glmm_fv_full, glmm_fv_reduced)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glmm_fv_full, plot = TRUE)

performance::check_predictions(glmm_fv_full) %>% 
  plot(type = "discrete_both")

performance::check_collinearity(glmm_fv_full)

# model output
performance::model_performance(glmm_fv_full)

summary(glmm_fv_full)


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
  vascular_space ~ infection + litter_size + age_at_start +
    (1|mice_id), data= model_weights)


# compare the effect of infection
lmm_vs_full <- lmm_vasc_space

lmm_vs_reduced <- lmerTest::lmer(
  vascular_space ~ litter_size + age_at_start +
    (1|mice_id), data= model_weights)

anova(lmm_vs_full, lmm_vs_reduced)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_vs_full, plot = TRUE)

flexplot::visualize(lmm_vs_full, plot = "residuals")

performance::check_collinearity(lmm_vs_full)


# model output
performance::model_performance(lmm_vs_full)

summary(lmm_vs_full)


# Save models output ------------------------------------------------------

# function to get contrasts and correct for multiple testing
emmeans::emm_options(lmer.df = "satterthwaite")  # same as lmerTest

dunnett_infection <- function(m) {
  cont <- emmeans::contrast(emmeans::emmeans(m, ~ infection),
                            method = "trt.vs.ctrl", ref = 1)
  raw <- summary(cont, adjust = "none")
  adj <- summary(cont, adjust = "dunnett")
  
  tibble::tibble(
    term    = gsub("^\\(|\\)$", "", sub(" - .*", "", raw$contrast)),
    p.value = raw$p.value,
    q.value = adj$p.value
  )
}


models_list <- dplyr::tibble(
  model_name = c("fetal weight", "placental weight", "fw/pw ratio",
                 "fgr", "fetal viability", "vascular space"),
  model_output = list(lmm_fw_full, lmm_pw_full, lmm_ratio_full,
                      glmm_fgr_full, glmm_fv_full, lmm_vs_full)
) %>% 
  dplyr::mutate(contrasts = purrr::map(model_output, dunnett_infection))


#saveRDS(models_list, "Code/Results/fig2_model_outputs.rds")
