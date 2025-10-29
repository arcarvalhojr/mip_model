### Exploratory analysis of data related to supplementary figure 3 ###

# Import libraries --------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(DHARMa)       # For residual diagnostics using simulated residuals
library(flexplot)     # For visual diagnostics and exploratory data analysis
library(performance)  # For model performance checks (R², outliers, assumptions, etc.)


# Import data -------------------------------------------------------------

# fetus data set
comp_weights <- readRDS("Data/Clean_data/comp_weights.rds")

# dams data set
comp_dams <- readRDS("Data/Clean_data/comp_dams.rds")


# analyse of infection*genotype on fetal viability ------------------------

# Looking the data
ggplot2::ggplot(comp_weights, aes(x= infection, fill= alive)) +
  geom_bar(position = position_dodge()) +
  facet_wrap(~ genotype)

# Count the n of observations per group
comp_weights %>% 
  dplyr::group_by(genotype) %>% 
  dplyr::filter(!is.na(alive)) %>% 
  dplyr::count(alive, by = infection)


# glmm model
glmm_stb <- lme4::glmer(factor(alive) ~ infection * genotype + (1|mice_id),
                  data = comp_weights,
                  family = "binomial")

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glmm_stb,
                                   plot = TRUE)


# analyse of infection*genotype on placental weight -----------------------

# Looking the data
ggplot2::ggplot(comp_weights,
                aes(x= infection, y= placenta_weight, fill= infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# lmm model
#base model
lmm_pw <- lme4::lmer(placenta_weight ~ infection * genotype + (1|mice_id),
               data = comp_weights)

#with litter size
lmm_pw1 <- lme4::lmer(
  placenta_weight ~ infection * genotype + litter_size + (1|mice_id),
                     data = comp_weights)

# looking the model
flexplot::visualize(lmm_pw1, plot = "model", 
                    formula = placenta_weight ~ litter_size + mice_id | infection,
                    sample = 20)

# compare fits
performance::compare_performance(lmm_pw, lmm_pw1, rank = TRUE, verbose = FALSE)

anova(lmm_pw, lmm_pw1)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = lmm_pw1, plot = TRUE)

flexplot::visualize(lmm_pw1, plot = "residuals")

performance::check_outliers(lmm_pw, method = "cook")


# analyse of infection*genotype on leucocytes count -----------------------

# Looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = wbc, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ genotype) + 
  theme_bw()

# glm model
glm_wbc <- glm(wbc ~ infection * genotype, data = comp_dams)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_wbc, plot = TRUE)

performance::check_outliers(glm_wbc, method = "cook")

#outlier detected

# removing outs
comp_wbc_clean <- comp_dams %>% 
  dplyr::select(genotype, mice_id, infection, wbc) %>% 
  dplyr::filter(!is.na(wbc))


outs_wbc <- performance::check_outliers(glm_wbc, method = "cook")

comp_wbc_clean <- comp_wbc_clean %>% 
  dplyr::slice(-which(outs_wbc))

# Looking data without outs
ggplot2::ggplot(comp_wbc_clean, aes(x = infection, y = wbc, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ genotype) + 
  theme_bw()

# glm model
glm_wbc1 <- glm(wbc ~ infection * genotype, data = comp_wbc_clean)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_wbc1, plot = TRUE)


# analyse of infection*genotype on limphocytes ----------------------------

# Looking the data
ggplot2::ggplot(comp_dams, aes(x= infection, y= lymph, fill= infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# glm model
glm_lymph <- glm(lymph ~ infection * genotype, data = comp_dams)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_lymph, plot = TRUE)

performance::check_outliers(glm_lymph, method = "cook")


# analyse of infection*genotype on granulocytes ---------------------------

# Looking the data
ggplot2::ggplot(comp_dams, aes(x= infection, y= gran, fill= infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# glm model
glm_gran <- glm(gran ~ infection * genotype, data = comp_dams)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_gran, plot = TRUE)

performance::check_outliers(glm_gran, method = "cook")


# analyse of infection*genotype on momocytes ------------------------------

# Looking the data
ggplot2::ggplot(comp_dams, aes(x= infection, y= mon, fill= infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# glm model
glm_mon <- glm(mon ~ infection * genotype, data = comp_dams)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_mon, plot = TRUE)

performance::check_outliers(glm_mon, method = "cook")
