### Exploratory analysis of data related to suplementary fig 2 ###


# Import library ----------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks (R², outliers, assumptions, etc.)
library(flexplot)     # For visual diagnostics and exploratory data analysis


# Import data -------------------------------------------------------------

# fetus data
model_weights <- readRDS("Data/Clean_data/model_weights.rds")



# Analyse of infection on placental vascular space area -------------------

# Looking the data
ggplot2::ggplot(
  model_weights, aes(x= infection, y= vascular_space, fill = infection))+
  geom_boxplot(show.legend = FALSE)


# lmm model
lmm_vasc_space <- lme4::lmer(
  vascular_space ~ infection + (1|mice_id), data= model_weights)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_vasc_space, plot = TRUE)

flexplot::visualize(lmm_vasc_space, plot = "residuals")

performance::check_outliers(lmm_vasc_space, method = "cook")
