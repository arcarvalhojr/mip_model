### Exploratory analysis of data related to suplementary fig 2 ###


# Import library ----------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(lmerTest)   # Provide p-value for mixed-effects models estimated with the lmer()
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks (R², outliers, assumptions, etc.)
library(flexplot)     # For visual diagnostics and exploratory data analysis


# Import data -------------------------------------------------------------

# dams data set
comp_dams <- readRDS("Data/Clean_data/comp_dams.rds")

# fetus data set
comp_weights <- readRDS("Data/Clean_data/comp_weights.rds")

# analyse of infection*genotype on the chances of ptd ---------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, fill = ptd)) +
  geom_bar(position = position_dodge()) +
  facet_wrap(~ genotype) 


comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::count(ptd)

# glm model
glm_ptd <- glm(ptd ~ infection * genotype, data = comp_dams,
               family = "binomial")

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_ptd, plot = TRUE)

performance::check_predictions(glm_ptd) %>% 
  plot(type = "discrete_both")

performance::check_model(glm_ptd)

# model output
performance::model_performance(glm_ptd)

summary(glm_ptd)


# analyse of infection*genotype on spleen weight --------------------------

# Looking the data
ggplot2::ggplot(comp_dams, aes(x= infection, y= spleen_w, fill= infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~genotype)+
  theme_bw()

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(spleen_w)),
                   n_NA = sum(is.na(spleen_w)))


#  glm model
glm_sw <- glm(spleen_w ~ infection * genotype, data= comp_dams)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_sw, plot = TRUE)

flexplot::visualize(glm_sw, plot = "residuals")

performance::check_model(glm_sw)

performance::check_outliers(glm_sw, method = "cook")

# model output
performance::model_performance(glm_sw)

summary(glm_sw)


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

# removing outs (replace for NA) 
comp_leucocyte_clean <- comp_dams %>% 
  dplyr::select(genotype, mice_id, infection, wbc, lymph, gran, mon) %>% 
  tidyr::drop_na()


outs_wbc <- performance::check_outliers(glm_wbc, method = "cook")

comp_leucocyte_clean <- comp_leucocyte_clean %>% 
  dplyr::mutate(wbc = if_else(outs_wbc, NA_real_, wbc))

# Looking data without outs
ggplot2::ggplot(comp_leucocyte_clean,
                aes(x = infection, y = wbc, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ genotype) + 
  theme_bw()


# glm model
glm_wbc1 <- glm(wbc ~ infection * genotype, data = comp_leucocyte_clean)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_wbc1, plot = TRUE)

performance::check_model(glm_wbc1)

# model output
performance::model_performance(glm_wbc1)

summary(glm_wbc1)

# analyse of infection*genotype on limphocytes ----------------------------

# Looking the data
ggplot2::ggplot(comp_leucocyte_clean,
                aes(x= infection, y= lymph, fill= infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

comp_leucocyte_clean %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(lymph)),
                   n_NA = sum(is.na(lymph)))

# glm model
glm_lymph <- glm(lymph ~ infection * genotype, data = comp_leucocyte_clean)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_lymph, plot = TRUE)

performance::check_model(glm_lymph)

performance::check_outliers(glm_lymph, method = "cook")

# model output
performance::model_performance(glm_lymph)

summary(glm_lymph)

# analyse of infection*genotype on granulocytes ---------------------------

# Looking the data
ggplot2::ggplot(comp_leucocyte_clean,
                aes(x= infection, y= gran, fill= infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

comp_leucocyte_clean %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(gran)),
                   n_NA = sum(is.na(gran)))

# glm model
glm_gran <- glm(gran ~ infection * genotype, data = comp_leucocyte_clean)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_gran, plot = TRUE)

performance::check_model(glm_gran)

performance::check_outliers(glm_gran, method = "cook")


# model output
performance::model_performance(glm_gran)

summary(glm_gran)

# analyse of infection*genotype on momocytes ------------------------------

# Looking the data
ggplot2::ggplot(comp_leucocyte_clean,
                aes(x= infection, y= mon, fill= infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

comp_leucocyte_clean %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(mon)),
                   n_NA = sum(is.na(mon)))

# glm model
glm_mon <- glm(mon ~ infection * genotype, data = comp_leucocyte_clean)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_mon, plot = TRUE)

performance::check_model(glm_mon)

performance::check_outliers(glm_mon, method = "cook")


# saving clean data
#saveRDS(comp_leucocyte_clean, "Data/Clean_data/comp_leucocyte_clean.rds")

# model output
performance::model_performance(glm_mon)

summary(glm_mon)


# save model output -------------------------------------------------------

model_list <- dplyr::tibble(
  model_name = c("ptd", "spleen weight", "leucocytes",
                 "lymphocytes", "granulocytes", "monocytes"),
  model_output = list(glm_ptd, glm_sw, glm_wbc1,
                      glm_lymph, glm_gran, glm_mon)
)

#saveRDS(model_list, "Code/Results/supfig2_model_outputs.rds")
