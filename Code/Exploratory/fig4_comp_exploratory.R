######## Exploratory analysis of data related to figure 4 ##########


# import libraries --------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization
library(lme4)         # For fitting mixed-effects models
library(lmerTest)     # Provide p-value for mixed-effects models estimated with the lmer()
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks
library(flexplot)     # For visual diagnostics


# import clean data -------------------------------------------------------

# fetus data set
comp_weights <- readRDS("Data/Clean_data/comp_weights.rds")


# analyse of infection*genotype on fetus weight --------------------------

# looking the data
ggplot2::ggplot(comp_weights,
                aes(x = genotype, y = fetal_weight, fill= infection)) +
  geom_boxplot()


comp_weights %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(fetal_weight)),
                   n_obs_NA = sum(is.na(fetal_weight)))

# lmm model
lmm_fw <- lmerTest::lmer(
  fetal_weight ~ infection * genotype + litter_size + age_at_start +
    (1|mice_id), data = comp_weights)


# looking the model
flexplot::visualize(lmm_fw, plot = "model", 
          formula = fetal_weight ~ litter_size + infection | genotype,
          sample = 37)

flexplot::visualize(lmm_fw, plot = "model", 
                    formula = fetal_weight ~ age_at_start + infection | genotype,
                    sample = 37)



# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_fw, plot = TRUE)

flexplot::visualize(lmm_fw, plot = "residuals")

performance::check_collinearity(lmm_fw)


# model output
performance::model_performance(lmm_fw)

summary(lmm_fw)


# analyse of infection*genotype on placental weight -----------------------

# Looking the data
ggplot2::ggplot(comp_weights,
                aes(x= infection, y= placenta_weight, fill= infection)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ genotype)

comp_weights %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(placenta_weight)),
                   n_NA = sum(is.na(placenta_weight)))

# lmm model
lmm_pw <- lmerTest::lmer(
  placenta_weight ~ infection * genotype + litter_size + age_at_start +
    (1|mice_id), data = comp_weights)


# looking the model
flexplot::visualize(lmm_pw, plot = "model", 
                    formula = placenta_weight ~ litter_size + infection | genotype,
                    sample = 20)

flexplot::visualize(lmm_pw, plot = "model", 
                    formula = placenta_weight ~ age_at_start + infection | genotype,
                    sample = 20)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_pw, plot = TRUE)

flexplot::visualize(lmm_pw, plot = "residuals")

performance::check_collinearity(lmm_pw)

# model output
performance::model_performance(lmm_pw)

summary(lmm_pw)


# analyse of infection*genotype on fw/pw weight ratio ---------------------

# looking the data
ggplot2::ggplot(comp_weights,
                aes(x = genotype, y = ratio, fill = infection)) +
  geom_boxplot()

comp_weights %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(ratio)),
                   n_obs_NA = sum(is.na(ratio)))


# lmm models
lmm_ratio <- lmerTest::lmer(
  ratio ~ infection * genotype + litter_size + age_at_start +
    (1|mice_id), data= comp_weights)


flexplot::visualize(lmm_ratio, plot = "model", 
                    formula = ratio ~ litter_size + infection | genotype,
                    sample = 100)

flexplot::visualize(lmm_ratio, plot = "model", 
                    formula = ratio ~ age_at_start + infection | genotype,
                    sample = 100)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_ratio, plot = TRUE)

flexplot::visualize(lmm_ratio, plot = "residuals")

performance::check_collinearity(lmm_ratio)


# model output
performance::model_performance(lmm_ratio)

summary(lmm_ratio)


# analyse of infection*genotype on the chances of fgr ---------------------

# get fw 10º percentile
comp_percentile_fw <- comp_weights %>%
  dplyr::filter(!is.na(fetal_weight)) %>%
  dplyr::group_by(genotype) %>% 
  dplyr::mutate(percentile_10 = quantile(fetal_weight[infection == "no"],
                                 probs = 0.1, na.rm = TRUE)) %>%
  dplyr::mutate(percentile = ifelse(
    fetal_weight >= percentile_10, "within_range", "below_range")) %>% #classification by the 10º percentile
  dplyr::select(
    mice_id, fetus_id, genotype, litter_size, age_at_start,
    infection, fetal_weight, percentile_10, percentile) %>% 
  dplyr::mutate(
    percentile = factor(percentile, levels = c(
      "within_range", "below_range")))

comp_percentile_fw %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::count(percentile)


# glmm model
glmm_fgr <- lme4::glmer(
  percentile ~ infection * genotype + litter_size + age_at_start +
    (1|mice_id), data = comp_percentile_fw, family = "binomial")


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glmm_fgr, plot = TRUE)

performance::check_predictions(glmm_fgr) %>% 
  plot(type = "discrete_both")

performance::check_collinearity(glmm_fgr)

# saving data set
#saveRDS(comp_percentile_fw, "Data/Clean_data/comp_fgr_clean.rds")

# model output
performance::model_performance(glmm_fgr)

summary(glmm_fgr)


# analyse of infection*genotype on placental sinusoides area --------------

# looking the data
ggplot2::ggplot(comp_weights,
                aes(x = genotype, y = vascular_space, fill = infection)) +
  geom_boxplot()

comp_weights %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(vascular_space)),
                   n_obs_NA = sum(is.na(vascular_space)))


# lmm model
lmm_vs <- lmerTest::lmer(
  vascular_space ~ infection * genotype + litter_size + age_at_start +
    (1|mice_id), data = comp_weights)


flexplot::visualize(lmm_vs, plot = "model", 
                    formula = vascular_space ~ litter_size + infection | genotype,
                    sample = 100)

flexplot::visualize(lmm_vs, plot = "model", 
                    formula = vascular_space ~ age_at_start + infection | genotype,
                    sample = 100)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_vs, plot = TRUE)

flexplot::visualize(lmm_vs, plot = "residuals")

performance::check_collinearity(lmm_vs)


# model output
performance::model_performance(lmm_vs)

summary(lmm_vs)


# analyse of infection*genotype on fetal viability ------------------------

# Looking the data
ggplot2::ggplot(comp_weights, aes(x= infection, fill= fetal_viability)) +
  geom_bar(position = position_dodge()) +
  facet_wrap(~ genotype)

# Count the n of observations per group
comp_weights %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(fetal_viability)),
                   n_NA = sum(is.na(fetal_viability)))


# glmm model
glmm_fv <- lme4::glmer(
  fetal_viability ~ infection * genotype + litter_size + age_at_start +
    (1|mice_id), data = comp_weights, family = "binomial")

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glmm_fv, plot = TRUE)

performance::check_predictions(glmm_fv) %>% 
  plot(type = "discrete_both")

performance::check_collinearity(glmm_fv)

# model output
performance::model_performance(glmm_fv)

summary(glmm_fv)


# save model outputs ------------------------------------------------------

models_list <- dplyr::tibble(
  model_name = c("fetal weight", "placental weight", "fw/pw ratio",
                "vascular space", "fgr", "fetal viability"),
  model_output = list(lmm_fw, lmm_pw, lmm_ratio,
                      lmm_vs, glmm_fgr, glmm_fv)
)


#saveRDS(models_list, "Code/Results/fig4_model_outputs.rds")
