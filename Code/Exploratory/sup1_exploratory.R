### Exploratory analysis of data related to suplementary fig 1 ###


# Import library ----------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks (R², outliers, assumptions, etc.)
library(flexplot)     # For visual diagnostics and exploratory data analysis


# Import data --------------------------------------------------------------

# dams data 
model_dams <- readRDS("Data/Clean_data/model_dams.rds")


# Analyse of infection on erithrocytes count ------------------------------

# Looking the data
ggplot2::ggplot(model_dams, aes(x= infection, y= rbc, fill= infection)) +
  geom_boxplot(show.legend = FALSE)

# glm model
glm_rbc <- glm(rbc ~ infection, data = model_dams, family = Gamma(link = "identity"))

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_rbc, plot = TRUE)

flexplot::visualize(glm_rbc, plot = "residuals")

performance::check_outliers(glm_rbc, method = "cook")

#that are already few observations so we will not consider remove outs


# Analyse of infection on hematocrit --------------------------------------

# Looking the data
ggplot2::ggplot(model_dams, aes(x= infection, y= hct, fill= infection)) +
  geom_boxplot(show.legend = FALSE)

# glm model
glm_hct <- glm(hct ~ infection, data = model_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_hct, plot = TRUE)

performance::check_outliers(glm_hct, method = "cook")


# Analyse of infection on hemoglobin --------------------------------------

# Looking the data
ggplot2::ggplot(model_dams, aes(x= infection, y= hgb, fill= infection)) +
  geom_boxplot(show.legend = FALSE)


# glm model
glm_hgb <- glm(hgb ~ infection, data= model_dams)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_hgb, plot = TRUE)

performance::check_outliers(glm_hgb, method = "cook")

#that are already few observations so we will not consider remove outs

