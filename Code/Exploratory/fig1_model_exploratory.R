######## Exploratory analysis of data related to figure 1 ##########


# import libraries --------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks
library(emmeans)      # For multiple testing correction

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
  dplyr::summarise(n_spleen = sum(!is.na(spleen_w)),
                   n_NA = sum(is.na(spleen_w)))


# glm model
glm_sw <- glm(spleen_w ~ infection, data = model_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_sw, plot = TRUE)


# model output
performance::model_performance(glm_sw)

summary(glm_sw)


# Analyse of the effect of pregnancy on susceptibility to infection -------

# looking the data
ggplot2::ggplot(
  model_parasitemia, aes(x = preg, y = parasitemia, fill = infection)) +
  geom_boxplot()


model_parasitemia %>%
  dplyr::summarise(n_para = sum(!is.na(parasitemia)),
                   n_NA = sum(is.na(parasitemia)),
                   .by = c(infection, preg))

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


# model output
performance::model_performance(glm_parasitemia_1)

summary(glm_parasitemia_1)


# Analyse of placental pb18s relative expression ---------------------------

# looking the data
model_weights %>% 
  dplyr::filter(!is.na(pb18s)) %>% 
  ggplot2::ggplot(
  aes(x = infection, y = log(pb18s), fill = infection)) +
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


# model output
performance::model_performance(glm_pb18s)

summary(glm_pb18s)


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


#trying to adjust family and link

# glm model with gamma family
glm_rbc_2 <- glm(rbc ~ infection, family = Gamma(link = "identity"),
                 data = model_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_rbc_2, plot = TRUE)

#better, gamma family improve the residuals diagnostic

# model output
performance::model_performance(glm_rbc_2)

summary(glm_rbc_2)


# Analyse of infection on hematocrit --------------------------------------

# Looking the data
ggplot2::ggplot(model_dams, aes(x= infection, y= hct, fill= infection)) +
  geom_boxplot(show.legend = FALSE)

model_dams %>% 
  dplyr::group_by(infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(hct)),
                   n_NA = sum(is.na(hct)))

# glm model
glm_hct <- glm(hct ~ infection, data = model_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_hct, plot = TRUE)

# glm model with gamma family
glm_hct_2 <- glm(hct ~ infection, family = Gamma(link = "identity"),
                 data = model_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_hct_2, plot = TRUE)

#didn't improve residuals diagnostic. keeping with gaussian glm

# model output
performance::model_performance(glm_hct)

summary(glm_hct)


# Analyse of infection on hemoglobin --------------------------------------

# Looking the data
ggplot2::ggplot(model_dams, aes(x= infection, y= hgb, fill= infection)) +
  geom_boxplot(show.legend = FALSE)

model_dams %>% 
  dplyr::group_by(infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(hgb)),
                   n_NA = sum(is.na(hgb)))


# glm model
glm_hgb <- glm(hgb ~ infection, data= model_dams)


# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_hgb, plot = TRUE)

# model output
performance::model_performance(glm_hgb)

summary(glm_hgb)


# Save model outputs ------------------------------------------------------

# function to get contrasts and correct for multiple testing
dunnett_infection <- function(m) {
  cont <- emmeans::contrast(emmeans::emmeans(m, ~ infection),
                            method = "trt.vs.ctrl", ref = 1)
  raw <- summary(cont, adjust = "none")
  adj <- summary(cont, adjust = "dunnett")
  
  tibble::tibble(
    term    = gsub("^\\(|\\)$", "", sub(" - .*", "", raw$contrast)),
    p.value = raw$p.value,
    q.value = adj$p.value
  )
}

# create list with models output
models_list <- dplyr::tibble(
  model_name = c("spleen weight", "peripheral parasitemia",
                 "pb18s expression", "erythrocytes",
                 "hematocrite", "hemoglobin"),
  model_output = list(glm_sw, glm_parasitemia_1, glm_pb18s,
                      glm_rbc_2, glm_hct, glm_hgb)
) %>% 
  dplyr::mutate(contrasts = purrr::map(model_output, dunnett_infection))


#saveRDS(models_list, "Code/Results/fig1_model_outputs.rds")
