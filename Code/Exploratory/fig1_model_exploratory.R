######## Exploratory analysis of data related to figure 1 ##########


# import libraries --------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks (R², outliers, assumptions, etc.)
library(flexplot)     # For visual diagnostics and exploratory data analysis

# import clean data -------------------------------------------------------

# dams data set
model_dams <- readRDS("Data/Clean_data/model_dams.rds")

# fetus data set
model_weights <- readRDS("Data/Clean_data/model_weights.rds")

# dams peripheral parasitemia data set
model_parasitemia <- readRDS("Data/Clean_data/model_parasitemia.rds")


# Analysis of the effect of infection on spleen weight --------------------

# looking the data
ggplot2::ggplot(
  model_dams, aes(x = infection, y = spleen_w, fill = infection)) +
  geom_boxplot(show.legend = FALSE) 

  
model_dams %>%
  dplyr::group_by(infection) %>%
  dplyr::summarise(n_spleen = sum(!is.na(spleen_w)))


# glm model
glm_sw <- glm(spleen_w ~ infection, data = model_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_sw, plot = TRUE)

flexplot::visualize(glm_sw, plot = "residuals")

performance::check_outliers(glm_sw, method = "cook")

# model output
performance::model_performance(glm_sw)

summary(glm_sw)

# Analyse of the effect of pregnancy on susceptibility to infection -------

# looking the data
ggplot2::ggplot(
  model_parasitemia, aes(x = preg, y = parasitemia, fill = infection))+
  geom_boxplot()


model_parasitemia %>%
  dplyr::group_by(infection, preg) %>%
  dplyr::summarise(n_parasitemia = sum(!is.na(parasitemia)))

#it doesn't seem like there's an interaction, but let's check it anyway

ggplot2::ggplot(
  model_parasitemia, aes(x = parasitemia)) +
  geom_histogram(bins = 10)

#don't follow a normal distribution too

# glm model
glm_parasitemia_1 <- glm(parasitemia ~ infection + preg,
                         data = model_parasitemia,
                         family = Gamma(link = "identity"))

glm_parasitemia_2 <- glm(parasitemia ~ infection * preg,
                         data = model_parasitemia,
                         family = Gamma(link = "identity"))

anova(glm_parasitemia_1, glm_parasitemia_2)

#yep, there's no interaction

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_parasitemia_1, plot = TRUE)

flexplot::visualize(glm_parasitemia_1, plot = "residuals")

performance::check_outliers(glm_parasitemia_1, method = "cook")

# model output
performance::model_performance(glm_parasitemia_1)

summary(glm_parasitemia_1)

# Analyse of placental pb18s relative expression ---------------------------

# looking the data
ggplot2::ggplot(
  model_weights, aes(x = infection, y = log(pb18s), fill = infection)) +
  geom_boxplot(show.legend = FALSE)


model_weights %>%
  dplyr::group_by(infection) %>%
  dplyr::filter(infection != "pbnk65-") %>% 
  dplyr::summarise(n_obs = n(),
                   n_pb18s = sum(!is.na(pb18s)),
                   n_NA = sum(is.na(pb18s)))

#there are a lot o NA!
#not all placentas were used for pb18s quantification and
#not all placentas used for rna extraction worked.
#control group (non-infected) placentas were not used for this experiment.

# glm model
glm_pb18s <- glm(pb18s ~ infection, data = model_weights,
                 family = Gamma(link = "log"))


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_pb18s, plot = TRUE)

performance::check_outliers(glm_pb18s, method = "cook")

#outlier detected

# removing outs
model_pb18s_clean <- model_weights %>%
  dplyr::select(mice_id, fetus_id, infection, pb18s) %>% 
  tidyr::drop_na()


outs_pb18s <- check_outliers(glm_pb18s, method = "cook")

model_pb18s_clean <- model_pb18s_clean %>%
  dplyr::slice(-which(outs_pb18s))


# looking the data without outs
ggplot2::ggplot(
  model_pb18s_clean, aes(x = infection, y = log(pb18s), fill = infection)) +
  geom_boxplot(show.legend = FALSE)


model_pb18s_clean %>%
  dplyr::group_by(infection) %>%
  dplyr::filter(infection != "pbnk65-") %>% 
  dplyr::summarise(n_pb18s = sum(!is.na(pb18s)))


# glm model without outs
glm_pb18s_2 <- glm(pb18s ~ infection, data = model_pb18s_clean,
                 family = Gamma(link = "log"))

DHARMa::simulateResiduals(fittedModel = glm_pb18s_2, plot = TRUE)

# saving clean data
#saveRDS(model_pb18s_clean, "Data/Clean_data/model_pb18s_clean.rds")


# model output
performance::model_performance(glm_pb18s_2)

summary(glm_pb18s_2)

# Save model outputs ------------------------------------------------------

models_list <- list(
  model_glm_sw = glm_sw,
  model_glm_parasitemia = glm_parasitemia_1,
  model_glm_pb18s = glm_pb18s_2
)

#saveRDS(models_list, "Code/Results/Model_outputs/fig1_model_outputs.rds")