######## Exploratory analysis of data related to figure 4 ##########


# import libraries --------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(lme4)         # For fitting linear and mixed-effects models
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

# lmm models
#base model
lmm_fw <- lme4::lmer(fetal_weight ~ infection * genotype + (1|mice_id),
               data = comp_weights)

# looking the model
flexplot::visualize(lmm_fw, plot = "model", 
                    formula = fetal_weight ~ infection | genotype,
                    sample = 37)


#with litter size
lmm_fw1 <- lme4::lmer(
  fetal_weight ~ infection * genotype + litter_size + (1|mice_id),
                data= comp_weights)

# looking the model
flexplot::visualize(lmm_fw1, plot = "model", 
          formula = fetal_weight ~ litter_size + genotype | infection,
          sample = 37)

# comparing the fit os the models
performance::compare_performance(lmm_fw, lmm_fw1,
                                 rank = TRUE, verbose = FALSE)
anova(lmm_fw, lmm_fw1)

#no effect for litter size on fetal weight in the model

#with placental weight
lmm_fw2 <- lme4::lmer(
  fetal_weight ~ infection * genotype + placenta_weight + (1|mice_id),
                data= comp_weights)

# looking the data
flexplot::visualize(lmm_fw2, plot = "model", 
          formula = fetal_weight ~ placenta_weight + genotype | infection,
          sample = 37)

# comparing the fit os the models
performance::compare_performance(lmm_fw, lmm_fw2,
                                 rank = TRUE, verbose = FALSE)

#seems like the base (simple) model is better

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_fw, plot = TRUE)

flexplot::visualize(lmm_fw, plot = "residuals")

#heterocedasticity detected but visually don't look bad

performance::check_outliers(lmm_fw, method = "cook")

#outliers detected

# removing outliers
comp_fw_clean <- comp_weights %>% 
  dplyr::filter(!is.na(fetal_weight)) %>% 
  dplyr::select(mice_id, fetus_id, genotype, infection, fetal_weight) 
  

outs_fw <- check_outliers(lmm_fw, method = "cook")

comp_fw_clean <- comp_fw_clean %>% 
  dplyr::slice(-which(outs_fw))

# Looking data without outs
ggplot2::ggplot(comp_fw_clean,
                aes(x = genotype, y = fetal_weight, fill= infection))+
  geom_boxplot()

# lmm model without outs
lmm_fw3 <- lme4::lmer(fetal_weight ~ infection * genotype + (1|mice_id),
                data = comp_fw_clean)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_fw3, plot = TRUE)

flexplot::visualize(lmm_fw3, plot = "residuals")

# saving fetal weight clean data
#saveRDS(comp_fw_clean, "Data/Clean_data/comp_fw_clean.rds")


# analyse of infection*genotype on fw/pw weight ratio ---------------------

# looking the data
ggplot2::ggplot(comp_weights,
                aes(x = genotype, y = ratio, fill = infection))+
  geom_boxplot()

# lmm models
#base model
lmm_ratio <- lme4::lmer(ratio ~ infection * genotype + (1|mice_id),
                  data= comp_weights)

#with litter size
lmm_ratio1 <- lme4::lmer(
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

performance::check_outliers(lmm_ratio, method = "cook")

# removing outliers
comp_ratio_clean <- comp_weights %>%
  dplyr::select(mice_id, fetus_id, genotype, infection, ratio) %>% 
  dplyr::filter(!is.na(ratio))

outs_ratio <- check_outliers(lmm_ratio, method = "cook")

comp_ratio_clean <- comp_ratio_clean %>%
  dplyr::slice(-which(outs_ratio))

# Looking data without outs
ggplot2::ggplot(comp_ratio_clean,
                aes(x = genotype, y = ratio, fill = infection))+
  geom_boxplot()

# lmm model without outs
lmm_ratio2 <- lme4::lmer(ratio ~ infection * genotype + (1|mice_id),
                   data= comp_ratio_clean)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_ratio2, plot = TRUE)

flexplot::visualize(lmm_ratio2, plot = "residuals")

# saving ratio clean data
#saveRDS(comp_ratio_clean, "Data/Clean_data/comp_ratio_clean.rds")


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

# glmm model
glmm_fw <- lme4::glmer(percentile ~ infection * genotype + (1|mice_id),
                 data = comp_percentile_fw, family = "binomial")


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glmm_fw, plot = TRUE)

# saving data set
#saveRDS(comp_percentile_fw, "Data/Clean_data/comp_fgr_fw.rds")


# analyse of infection*genotype on placental sinusoides area --------------

