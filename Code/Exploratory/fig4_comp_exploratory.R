######## Exploratory analysis of data related to figure 4 ##########


# import libraries --------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(lme4)         # For fitting linear and mixed-effects models
library(lmerTest)     # Provide p-value for mixed-effects models estimated with the lmer()
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks (R², outliers, assumptions, etc.)
library(flexplot)     # For visual diagnostics and exploratory data analysis



# import clean data -------------------------------------------------------

# fetus data set
comp_weights <- readRDS("Data/Clean_data/comp_weights.rds")


# analyse of infection*genotype on fetus weight --------------------------

# looking the data
ggplot2::ggplot(comp_weights,
                aes(x = genotype, y = fetal_weight, fill= infection))+
  geom_boxplot()


comp_weights %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(fetal_weight)),
                   n_obs_NA = sum(is.na(fetal_weight)))

# lmm models
#base model
lmm_fw <- lmerTest::lmer(fetal_weight ~ infection * genotype + (1|mice_id),
               data = comp_weights)

# looking the model
flexplot::visualize(lmm_fw, plot = "model", 
                    formula = fetal_weight ~ infection | genotype,
                    sample = 37)

#with litter size
lmm_fw1 <- lmerTest::lmer(
  fetal_weight ~ infection * genotype + litter_size + (1|mice_id),
                data= comp_weights)

# looking the model
flexplot::visualize(lmm_fw1, plot = "model", 
          formula = fetal_weight ~ litter_size + infection | genotype,
          sample = 37)

# comparing the fit of the models
performance::compare_performance(lmm_fw, lmm_fw1,
                                 rank = TRUE, verbose = FALSE)
anova(lmm_fw, lmm_fw1)

#no effect for litter size on fetal weight in the model

#with placental weight
lmm_fw2 <- lmerTest::lmer(
  fetal_weight ~ infection * genotype + placenta_weight + (1|mice_id),
                data= comp_weights)

# looking the data
flexplot::visualize(lmm_fw2, plot = "model", 
          formula = fetal_weight ~ placenta_weight + infection | genotype,
          sample = 37)

# comparing the fit os the models
performance::compare_performance(lmm_fw, lmm_fw2,
                                 rank = TRUE, verbose = FALSE)

#seems like the base (simple) model is better

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_fw, plot = TRUE)

flexplot::visualize(lmm_fw, plot = "residuals")

performance::check_model(lmm_fw)

#don't look bad

performance::check_outliers(lmm_fw, method = "cook")

#outliers detected

# removing outliers
comp_fw_clean <- comp_weights %>% 
  dplyr::filter(!is.na(fetal_weight)) %>% 
  dplyr::select(mice_id, fetus_id, genotype, infection, fetal_weight) 
  

outs_fw <- performance::check_outliers(lmm_fw, method = "cook")

comp_fw_clean <- comp_fw_clean %>% 
  dplyr::slice(-which(outs_fw))

# Looking data without outs
ggplot2::ggplot(comp_fw_clean,
                aes(x = genotype, y = fetal_weight, fill= infection))+
  geom_boxplot()

comp_fw_clean %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(fetal_weight)),
                   n_obs_NA = sum(is.na(fetal_weight)))

# lmm model without outs
lmm_fw3 <- lmerTest::lmer(fetal_weight ~ infection * genotype + (1|mice_id),
                data = comp_fw_clean)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_fw3, plot = TRUE)

flexplot::visualize(lmm_fw3, plot = "residuals")

performance::check_model(lmm_fw3)

# saving fetal weight clean data
#saveRDS(comp_fw_clean, "Data/Clean_data/comp_fw_clean.rds")


# model output
performance::model_performance(lmm_fw3)

summary(lmm_fw3)


# analyse of infection*genotype on placental weight -----------------------

# Looking the data
ggplot2::ggplot(comp_weights,
                aes(x= infection, y= placenta_weight, fill= infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

comp_weights %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(placenta_weight)),
                   n_NA = sum(is.na(placenta_weight)))

# lmm model
#base model
lmm_pw <- lmerTest::lmer(placenta_weight ~ infection * genotype + (1|mice_id),
                         data = comp_weights)

#with litter size
lmm_pw1 <- lmerTest::lmer(
  placenta_weight ~ infection * genotype + litter_size + (1|mice_id),
  data = comp_weights)

# looking the model
flexplot::visualize(lmm_pw1, plot = "model", 
                    formula = placenta_weight ~ litter_size + infection | genotype,
                    sample = 20)

# compare fits
performance::compare_performance(lmm_pw, lmm_pw1, rank = TRUE, verbose = FALSE)

anova(lmm_pw, lmm_pw1)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_pw1, plot = TRUE)

flexplot::visualize(lmm_pw1, plot = "residuals")

performance::check_model(lmm_pw1)

performance::check_outliers(lmm_pw1, method = "cook")

#outs detected

# remove outs (replace for NA)
comp_pw_clean <- comp_weights %>% 
  dplyr::select(mice_id, fetus_id, genotype, infection,
                placenta_weight, litter_size) %>% 
  tidyr::drop_na()

outs_pw <- performance::check_outliers(lmm_pw1, method = "cook")

comp_pw_clean <- comp_pw_clean %>% 
  dplyr::slice(-which(outs_pw))

# llm model
lmm_pw2 <- lmerTest::lmer(
  placenta_weight ~ infection * genotype + litter_size + (1|mice_id),
  data = comp_pw_clean)

# model diagnostic
simulateResiduals(fittedModel = lmm_pw2, plot = TRUE)

flexplot::visualize(lmm_pw2, plot = "residuals")

performance::check_model(lmm_pw2)


# saving hematology clean data
#saveRDS(comp_pw_clean, "Data/Clean_data/comp_pw_clean.rds")

# model output
performance::model_performance(lmm_pw2)

summary(lmm_pw2)


# analyse of infection*genotype on fw/pw weight ratio ---------------------

# looking the data
ggplot2::ggplot(comp_weights,
                aes(x = genotype, y = ratio, fill = infection))+
  geom_boxplot()

comp_weights %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(ratio)),
                   n_obs_NA = sum(is.na(ratio)))

#many NAs. This is because of many NA on placenta weight

# lmm models
#base model
lmm_ratio <- lmerTest::lmer(ratio ~ infection * genotype + (1|mice_id),
                  data= comp_weights)

#with litter size
lmm_ratio1 <- lmerTest::lmer(
  ratio ~ infection * genotype + litter_size + (1|mice_id),
                        data= comp_weights)

flexplot::visualize(lmm_ratio1, plot = "model", 
                    formula = ratio ~ litter_size + infection | genotype,
                    sample = 100)

# compare fits
performance::compare_performance(lmm_ratio, lmm_ratio1,
                                 rank = TRUE,
                                 verbose = FALSE)

anova(lmm_ratio, lmm_ratio1)

#base model is better

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_ratio, plot = TRUE)

flexplot::visualize(lmm_ratio, plot = "residuals")

performance::check_model(lmm_ratio)

performance::check_outliers(lmm_ratio, method = "cook")

#outlier detected

# removing outliers
comp_ratio_clean <- comp_weights %>%
  dplyr::select(mice_id, fetus_id, genotype, infection, ratio) %>% 
  dplyr::filter(!is.na(ratio))

outs_ratio <- performance::check_outliers(lmm_ratio, method = "cook")

comp_ratio_clean <- comp_ratio_clean %>%
  dplyr::slice(-which(outs_ratio))

# Looking data without outs
ggplot2::ggplot(comp_ratio_clean,
                aes(x = genotype, y = ratio, fill = infection))+
  geom_boxplot()

# lmm model without outs
lmm_ratio2 <- lmerTest::lmer(ratio ~ infection * genotype + (1|mice_id),
                   data= comp_ratio_clean)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_ratio2, plot = TRUE)

flexplot::visualize(lmm_ratio2, plot = "residuals")

performance::check_model(lmm_ratio2)

# saving ratio clean data
#saveRDS(comp_ratio_clean, "Data/Clean_data/comp_ratio_clean.rds")


# model output
performance::model_performance(lmm_ratio2)

summary(lmm_ratio2)

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
    mice_id, fetus_id, genotype, infection, fetal_weight,
    percentile_10, percentile) %>% 
  dplyr::mutate(
    percentile = factor(percentile, levels = c(
      "within_range", "below_range")))

comp_percentile_fw %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::count(percentile)


# glmm model
glmm_fgr <- lme4::glmer(percentile ~ infection * genotype + (1|mice_id),
                 data = comp_percentile_fw, family = "binomial")


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glmm_fgr, plot = TRUE)

performance::check_predictions(glmm_fgr) %>% 
  plot(type = "discrete_both")

performance::check_model(glmm_fgr)

# saving data set
#saveRDS(comp_percentile_fw, "Data/Clean_data/comp_fgr_clean.rds")

# model output
performance::model_performance(glmm_fgr)

summary(glmm_fgr)

# analyse of infection*genotype on placental sinusoides area --------------


# analyse of infection*genotype on fetal viability ------------------------

# Looking the data
ggplot2::ggplot(comp_weights, aes(x= infection, fill= alive)) +
  geom_bar(position = position_dodge()) +
  facet_wrap(~ genotype)

# Count the n of observations per group
comp_weights %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(alive)),
                   n_NA = sum(is.na(alive)))


# glmm model
glmm_stb <- lme4::glmer(alive ~ infection * genotype + (1|mice_id),
                        data = comp_weights,
                        family = "binomial")

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glmm_stb,
                          plot = TRUE)

performance::check_predictions(glmm_stb) %>% 
  plot(type = "discrete_both")

performance::check_model(glmm_stb)

# model output
performance::model_performance(glmm_stb)

summary(glmm_stb)


# save model outputs ------------------------------------------------------

models_list <- dplyr::tibble(
  model_name = c("fetal weight", "placental weight", "fw/pw ratio",
                 "fgr", "fetal viability"),
  model_output = list(lmm_fw3, lmm_pw2, lmm_ratio2,
                      glmm_fgr, glmm_stb)
)


#saveRDS(models_list, "Code/Results/fig4_model_outputs.rds")
