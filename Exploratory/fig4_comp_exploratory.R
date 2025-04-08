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



# analyse of infection*genotype on fetaus weight --------------------------

# looking the data
ggplot(comp_weights, aes(x = genotype, y = fetal_weight, fill= infection))+
  geom_boxplot()

# lmm models
lmm_fw <- lmer(fetal_weight ~ infection * genotype + (1|mice_id),
               data = comp_weights)

# with litter size
lmm_fw1 <- lmer(fetal_weight ~ infection * genotype + litter_size + (1|mice_id),
                data= comp_weights)

# looking the model
visualize(lmm_fw1, plot = "model", 
          formula = fetal_weight ~ litter_size + genotype | infection,
          sample = 37)

# comparing the fit os the models
compare_performance(lmm_fw, lmm_fw1, rank = TRUE, verbose = FALSE)
anova(lmm_fw, lmm_fw1)

#no effect for litter size on fetal weight in the model

# with placental weight
lmm_fw2 <- lmer(fetal_weight ~ infection * genotype + placenta_weight + (1|mice_id),
                data= comp_weights)

# looking the data
visualize(lmm_fw2, plot = "model", 
          formula = fetal_weight ~ placenta_weight + genotype | infection,
          sample = 37)

# comparing the fit os the models
compare_performance(lmm_fw, lmm_fw2, rank = TRUE, verbose = FALSE)

#seems like simple model is better

# model diagnostic
simulateResiduals(fittedModel = lmm_fw, plot = TRUE)

visualize(lmm_fw, plot = "residuals")

#homoscedasticity detected but visually don't look bad

check_outliers(lmm_fw, method = "cook")

# removing outliers
outs_fetalweight <- check_outliers(lmm_fw, method = "cook")

comp_fw_clean <- comp_weights %>% 
  select(mice_id, fetus_id, genotype, infection, fetal_weight) %>% 
  slice(-which(outs_fetalweight)) %>% 
  filter(!is.na(fetal_weight))

ggplot(comp_fw_clean, aes(x = genotype, y = fetal_weight, fill= infection))+
  geom_boxplot()

# lmm model without outs
lmm_fw3 <- lmer(fetal_weight ~ infection * genotype + (1|mice_id),
                data = comp_fw_clean)

# model diagnostic
simulateResiduals(fittedModel = lmm_fw3, plot = TRUE)

visualize(lmm_fw3, plot = "residuals")

# saving fetal weitgh clean data
#saveRDS(comp_fw_clean, "Data/Clean_data/comp_fw_clean.rds")


# analyse of infection*genotype on fw/pw weight ratio ---------------------

# looking the data
ggplot(comp_weights, aes(x = genotype, y = ratio, fill = infection))+
  geom_boxplot()

# lmm models
lmm_ratio <- lmer(ratio ~ infection * genotype + (1|mice_id),
                  data= comp_weights)

# model diagnostic
simulateResiduals(fittedModel = lmm_ratio, plot = TRUE)

visualize(lmm_ratio, plot = "residuals")

check_outliers(lmm_ratio, method = "cook")

# removing outliers
outs_ratio <- check_outliers(lmm_ratio, method = "cook")

comp_ratio_clean <- comp_weights %>%
  select(mice_id, fetus_id, genotype, infection, ratio) %>% 
  slice(- which(outs_ratio)) %>% 
  filter(!is.na(ratio))

ggplot(comp_ratio_clean, aes(x = genotype, y = ratio, fill = infection))+
  geom_boxplot()

# lmm model without outs
lmm_ratio2 <- lmer(ratio ~ infection * genotype + (1|mice_id),
                   data= comp_ratio_clean)

# model diagnostic
simulateResiduals(fittedModel = lmm_ratio2, plot = TRUE)

visualize(lmm_ratio2, plot = "residuals")

# saving ratio clean data
#saveRDS(comp_ratio_clean, "Data/Clean_data/comp_ratio_clean.rds")



# analyse of infection*genotype on the chances of fgr ---------------------

# getting percentile data set
comp_percentile_fw <- comp_weights %>%
  group_by(genotype) %>% 
  mutate(percentile_10 = quantile(fetal_weight[infection == "no"],
                                 probs = 0.1, na.rm = TRUE)) %>%  # getting 10º percentile
  select(mice_id, fetus_id, genotype, infection, fetal_weight, percentile_10) %>%
  filter(!is.na(fetal_weight)) %>% 
  mutate(percentile = ifelse(fetal_weight >= percentile_10, "above", "below"))  #classification by the 10º percentile  


# glmm model
glmm_fw <- glmer(factor(percentile) ~ infection * genotype + (1|mice_id),
                 data = comp_percentile_fw, family = "binomial")


# model diagnostic
#simulateResiduals(fittedModel = glmm_fw, plot = TRUE)

# saving data set
saveRDS(comp_percentile_fw, "Data/Clean_data/comp_percentile_fw.rds")


# analyse of infection*genotype on placental sinusoides area --------------

