######## Exploratory analysis of data related to figure 3 ##########


# import libraries --------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks


# import clean data -------------------------------------------------------

# dams data set
comp_dams <- readRDS("Data/Clean_data/comp_dams.rds") 

# peripheral parasitemia data set
comp_parasitemia <- readRDS("Data/Clean_data/comp_parasitemia.rds")

# pb18s data set
comp_weights <- readRDS("Data/Clean_data/comp_weights.rds")


# analyse of pregnancy*genotype on peripheral parasitemia -----------------

# looking the data
ggplot2::ggplot(comp_parasitemia,
                aes(x = genotype, y = parasitemia, fill = preg)) +
  geom_boxplot()


comp_parasitemia %>% 
  dplyr::group_by(genotype, preg) %>% 
  dplyr::summarise(n_obs = sum(!is.na(parasitemia)),
                   percent_NA = mean(is.na(parasitemia)))

# glm model
glm_parasitemia <- glm(parasitemia ~ preg * genotype, data = comp_parasitemia)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_parasitemia, plot = TRUE)


# model output
performance::model_performance(glm_parasitemia)

summary(glm_parasitemia)


# analyse of genotype on placental pb18s ----------------------------------

# looking the data
ggplot2::ggplot(comp_weights,
                aes(x = genotype, y = log(pb18s), fill = genotype)) +
  geom_boxplot(show.legend = FALSE)

comp_weights %>% 
  dplyr::group_by(genotype) %>% 
  dplyr::summarise(n_obs = sum(!is.na(pb18s)),
                   n_NA = sum(is.na(pb18s)))

#just few placentas were used for this analysis


# glm model
glm_pb18s <- glm(pb18s ~ genotype, family = Gamma(link = "log"),
                 data = comp_weights)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_pb18s, plot = TRUE)


# model output
performance::model_performance(glm_pb18s)

summary(glm_pb18s)


# analyse of infection*genotype on rbc ------------------------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = rbc, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ genotype)

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(rbc)),
                   n_NA = sum(is.na(rbc)))

# glm model
glm_rbc <- glm(rbc ~ infection * genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_rbc, plot = TRUE)


# model output
performance::model_performance(glm_rbc)

summary(glm_rbc)


# analyse of infection*genotype on hct ------------------------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = hct, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ genotype)

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(hct)),
                   n_NA = sum(is.na(hct)))

# glm model
glm_hct <- glm(hct ~ infection * genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_hct, plot = TRUE)


# model output
performance::model_performance(glm_hct)

summary(glm_hct)


# analyse of infection*genotype on hgb ------------------------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = hgb, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ genotype)

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(hgb)),
                   n_NA = sum(is.na(hgb)))

# glm model
glm_hgb <- glm(hgb ~ infection * genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_hgb, plot = TRUE)


# model output
performance::model_performance(glm_hgb)

summary(glm_hgb)


# Save models output ------------------------------------------------------

models_list <- dplyr::tibble(
  model_name = c("peripheral parasitemia", "pb18s expression",
                  "erythrocytes", "hematocrite", "hemoglobin"),
  model_output = list(glm_parasitemia, glm_pb18s, glm_rbc, glm_hct, glm_hgb)
)


#saveRDS(models_list, "Code/Results/fig3_model_outputs.rds")
#saveRDS(glm_parasitemia, "Code/Results/fig3_parasitemia_output.rds")

