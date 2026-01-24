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


outs_pb18s <- performance::check_outliers(glm_pb18s, method = "cook")

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

# Analyse of infection on erithrocytes count ------------------------------

# Looking the data
ggplot2::ggplot(model_dams, aes(x= infection, y= rbc, fill= infection)) +
  geom_boxplot(show.legend = FALSE)


model_dams %>% 
  dplyr::group_by(infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(rbc)),
                   n_NA = sum(is.na(rbc)))


# glm model
glm_rbc <- glm(rbc ~ infection, data = model_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_rbc, plot = TRUE)

flexplot::visualize(glm_rbc, plot = "residuals")

performance::check_model(glm_rbc)

performance::check_outliers(glm_rbc, method = "cook")

#outlier detected

# remove outs (replace for NA)
model_hemato_clean <- model_dams %>% 
  dplyr::select(mice_id, infection, rbc, hct, hgb) %>% 
  tidyr::drop_na()

outs_rbc <- performance::check_outliers(glm_rbc, method = "cook")

model_hemato_clean <- model_hemato_clean %>% 
  dplyr::mutate(rbc = if_else(outs_rbc, NA_real_, rbc))


# Looking the data without out
ggplot2::ggplot(model_hemato_clean,
                aes(x= infection, y= rbc, fill= infection)) +
  geom_boxplot(show.legend = FALSE)

# glm model
glm_rbc_1 <- glm(rbc ~ infection, data = model_hemato_clean)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_rbc_1, plot = TRUE)

flexplot::visualize(glm_rbc_1, plot = "residuals")

performance::check_model(glm_rbc_1)

# model output
performance::model_performance(glm_rbc_1)

summary(glm_rbc_1)

# Analyse of infection on hematocrit --------------------------------------

# Looking the data
ggplot2::ggplot(model_dams, aes(x= infection, y= hct, fill= infection)) +
  geom_boxplot(show.legend = FALSE)

# glm model
glm_hct <- glm(hct ~ infection, data = model_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_hct, plot = TRUE)

performance::check_model(glm_hct)

performance::check_outliers(glm_hct, method = "cook")

# model output
performance::model_performance(glm_hct)

summary(glm_hct)

# Analyse of infection on hemoglobin --------------------------------------

# Looking the data
ggplot2::ggplot(model_dams, aes(x= infection, y= hgb, fill= infection)) +
  geom_boxplot(show.legend = FALSE)


# glm model
glm_hgb <- glm(hgb ~ infection, data= model_dams)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_hgb, plot = TRUE)

performance::check_model(glm_hgb)

performance::check_outliers(glm_hgb, method = "cook")

#outlier detected

# remove out (replace for NA)

outs_hgb <- performance::check_outliers(glm_hgb, method = "cook")

model_hemato_clean <- model_hemato_clean %>% 
  dplyr::mutate(hgb = if_else(outs_hgb, NA_real_, hgb))


# Looking the data without out
ggplot2::ggplot(model_hemato_clean,
                aes(x= infection, y= hgb, fill= infection)) +
  geom_boxplot(show.legend = FALSE)

# glm model
glm_hgb_1 <- glm(hgb ~ infection, data = model_hemato_clean)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_hgb_1, plot = TRUE)

flexplot::visualize(glm_hgb_1, plot = "residuals")

performance::check_model(glm_hgb_1)

# model output
performance::model_performance(glm_hgb_1)

summary(glm_hgb_1)


# saving hematology clean data
#saveRDS(model_hemato_clean, "Data/Clean_data/model_hemato_clean.rds")

# Save model outputs ------------------------------------------------------

models_list <- dplyr::tibble(
  model_name = c("spleen weight", "peripheral parasitemia",
                 "pb18s expression", "erytrocytes",
                 "hematocrite", "hemoglobin"),
  model_output = list(glm_sw, glm_parasitemia_1, glm_pb18s_2,
                      glm_rbc_1, glm_hct, glm_hgb_1)
)


#saveRDS(models_list, "Code/Results/fig1_model_outputs.rds")
