######## Exploratory analysis of data related to figure 2 ##########


# import libraries --------------------------------------------------------

library(tidyverse)  # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(lme4)       # For fitting linear and mixed-effects models
library(DHARMa)     # For residual diagnostics using simulated residuals



# import clean data -------------------------------------------------------

# fetus data set
model_weights <- readRDS("Data/Clean_data/model_weights.rds")

model_percentile_fw <- model_weights %>%
  mutate(percentile_5 = quantile(fetal_weight[infection == 0],
                                 probs = 0.05, na.rm = TRUE)) %>%  # getting 5º percentile
  filter(infection %in% c(0, 8)) %>%
  select(mice_id, fetus_id, infection, fetal_weight, percentile_5) %>%
  mutate(percentile = ifelse(fetal_weight >= percentile_5, "above", "below"))  #classification by the 5º percentile  


# Saving data set
saveRDS(model_percentile_fw, "Data/Clean_data/model_percentile_fw.rds")



# Analysis of infection on the chances of "fgr" ---------------------------

# glmm model
glmm_fw <- glmer(factor(percentile) ~ infection + (1|mice_id),
                    data = model_percentile_fw, family = "binomial")


# model diagnostic
simulateResiduals(fittedModel = glmm_fw, plot = TRUE)

