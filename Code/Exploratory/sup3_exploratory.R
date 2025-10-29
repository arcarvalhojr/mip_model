### Exploratory analysis of data related to supplementary figure 3 ###


# Import libraries --------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(DHARMa)       # For residual diagnostics using simulated residuals
library(flexplot)     # For visual diagnostics and exploratory data analysis
library(performance)  # For model performance checks (R², outliers, assumptions, etc.)


# Import data -------------------------------------------------------------

# dams data set
comp_dams <- readRDS("Data/Clean_data/comp_dams.rds")


# analyse of infection*genotype on spleen weight --------------------------

# Looking the data
ggplot2::ggplot(comp_dams, aes(x= infection, y= spleen_w, fill= infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~genotype)+
  theme_bw()


#  glm model
glm_sw <- glm(spleen_w ~ infection * genotype, data= comp_dams)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_sw, plot = TRUE)

flexplot::visualize(glm_sw, plot = "residuals")

performance::check_outliers(glm_sw, method = "cook")


# analyse of infection*genotype on rbc ------------------------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = rbc, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# glm model
glm_rbc <- glm(rbc ~ infection * genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_rbc, plot = TRUE)

flexplot::visualize(glm_rbc, plot = "residuals")

performance::check_outliers(glm_rbc, method = "cook")


# analyse of infection*genotype on hct ------------------------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = hct, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# glm model
glm_hct <- glm(hct ~ infection * genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_hct, plot = TRUE)

flexplot::visualize(glm_hct, plot = "residuals")

performance::check_outliers(glm_hct, method = "cook")


# analyse of infection*genotype on hgb ------------------------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = hgb, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# glm model
glm_hgb <- glm(hgb ~ infection * genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_hgb, plot = TRUE)

flexplot::visualize(glm_hgb, plot = "residuals")

performance::check_outliers(glm_hgb, method = "cook")

