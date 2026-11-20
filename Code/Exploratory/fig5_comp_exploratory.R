######## Exploratory analysis of data related to figure 5 ##########


# import library ----------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks 

# import clean data -------------------------------------------------------

# cytokines in damns data set
comp_dams <- readRDS("Data/Clean_data/comp_dams.rds")

# cytokines in fetus data set
comp_weights <- readRDS("Data/Clean_data/comp_weights.rds")


# analyse of infection*genotype on levels of serum mcp1 -------------------

# looking the data
ggplot2::ggplot(comp_dams,
                aes(x = infection, y = mcp1_sr, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ genotype) +
  theme_bw()

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(mcp1_sr)),
                   n_obs_NA = sum(is.na(mcp1_sr)))

#only detected in infected damns

# glm model
glm_mcp1_sr <- glm(mcp1_sr ~ genotype, data = comp_dams)


glm_mcp1_sr <- glm(mcp1_sr ~ genotype, family = Gamma(link = "identity"),
                   data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_mcp1_sr, plot = TRUE)

performance::check_heteroscedasticity(glm_mcp1_sr)


# model output
performance::model_performance(glm_mcp1_sr)

summary(glm_mcp1_sr)


# analyse of infection*genotype on levels of serum ifn --------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = ifn_sr, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ genotype) +
  theme_bw()

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(ifn_sr)),
                   n_obs_NA = sum(is.na(ifn_sr)))

#also only detected in infected damns

# glm model
glm_ifn_sr <- glm(ifn_sr ~ genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_ifn_sr, plot = TRUE)

performance::check_heteroscedasticity(glm_ifn_sr)


# model output
performance::model_performance(glm_ifn_sr)

summary(glm_ifn_sr)


# analyse of infection*genotype on levels of serum tnf --------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = tnf_sr, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ genotype) +
  theme_bw()


comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(tnf_sr)),
                   n_obs_NA = sum(is.na(tnf_sr)))

#there was no cytokines detected in serum of non-inf dams

# glm model
glm_tnf_sr <- glm(tnf_sr ~ genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_tnf_sr, plot = TRUE)

performance::check_heteroscedasticity(glm_tnf_sr)


# model output
performance::model_performance(glm_tnf_sr)

summary(glm_tnf_sr)


# analyse of infection*genotype on levels of spleen mcp1 ------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = mcp1_sp, fill= infection)) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ genotype) +
  theme_bw()

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(mcp1_sp)),
                   n_obs_NA = sum(is.na(mcp1_sp)))

#also only detected ck levels in infected damns

# glm model
glm_mcp1_sp <- glm(mcp1_sp ~ genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_mcp1_sp, plot = TRUE)

performance::check_heteroscedasticity(glm_mcp1_sp)

# model output
performance::model_performance(glm_mcp1_sp)

summary(glm_mcp1_sp)


# analyse of infection*genotype on levels of spleen ifn -------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = ifn_sp, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ genotype) +
  theme_bw()

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(ifn_sp)),
                   n_obs_NA = sum(is.na(ifn_sp)))

# glm model
glm_ifn_sp <- glm(ifn_sp ~ genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_ifn_sp, plot = TRUE)

performance::check_heteroscedasticity(glm_ifn_sp)

# model output
performance::model_performance(glm_ifn_sp)

summary(glm_ifn_sp)


# analyse of infection*genotype on levels of spleen tnf -------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = tnf_sp, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ genotype) +
  theme_bw()

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(tnf_sp)),
                   n_obs_NA = sum(is.na(tnf_sp)))

# glm model
glm_tnf_sp <- glm(tnf_sp ~ genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_tnf_sp, plot = TRUE)

performance::check_heteroscedasticity(glm_tnf_sp)


# model output
performance::model_performance(glm_tnf_sp)

summary(glm_tnf_sp)


# analyse of infection*genotype on levels of spleen il10 ------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = il10_sp, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ genotype) +
  theme_bw()

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(il10_sp)),
                   n_obs_NA = sum(is.na(il10_sp)))

#ck present in all groups

# glm model
glm_il10_sp <- glm(il10_sp ~ infection * genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_il10_sp, plot = TRUE)


# model output
performance::model_performance(glm_il10_sp)

summary(glm_il10_sp)


# analyse of infection*genotype on levels of placenta mcp1 ----------------

# looking the data
ggplot2::ggplot(comp_weights,
                aes(x = infection, y = mcp1, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ genotype) +
  theme_bw()

comp_weights %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(mcp1)),
                   n_obs_NA = sum(is.na(mcp1)))

#many NAs; not all placentas were used for cytokine quantification

# glm model
glm_mcp1_pl <- glm(mcp1 ~ infection * genotype, data = comp_weights)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_mcp1_pl, plot = TRUE)


# model output
performance::model_performance(glm_mcp1_pl)

summary(glm_mcp1_pl)


# analyse of infection*genotype on levels of placenta ifn -----------------

# looking the data
ggplot2::ggplot(comp_weights, aes(x = infection, y = ifn, fill = infection)) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ genotype) +
  theme_bw()

comp_weights %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(ifn)),
                   n_obs_NA = sum(is.na(ifn)))

#only in infected dams

# glm model
glm_ifn_pl <- glm(ifn ~ genotype, data = comp_weights)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_ifn_pl, plot = TRUE)

performance::check_heteroscedasticity(glm_ifn_pl)

# model output
performance::model_performance(glm_ifn_pl)

summary(glm_ifn_pl)


# save model outpput ------------------------------------------------------

models_list <- dplyr::tibble(
  model_name = c("serum mcp-1", "serum ifn", "serum tnf",
                 "spleen mcp-1", "spleen ifn", "spleen tnf", "spleen il-10",
                 "placenta mcp-1", "placenta ifn"),
  model_output = list(glm_mcp1_sr, glm_ifn_sr, glm_tnf_sr, glm_mcp1_sp,
                      glm_ifn_sp, glm_tnf_sp, glm_il10_sp, glm_mcp1_pl,
                      glm_ifn_pl)
)


#saveRDS(models_list, "Code/Results/fig5_model_outputs.rds")


# pivot data for visualization --------------------------------------------

# damns
comp_dams_ctk_long <- comp_dams %>% 
  dplyr::select(genotype, mice_id, infection, mcp1_sr, ifn_sr, tnf_sr,
                mcp1_sp, ifn_sp, tnf_sp, il10_sp) %>% 
  tidyr::pivot_longer(
    cols = ends_with(c("sp", "sr")),
    names_to = c("cytokine", "region"),
    names_sep = "_",
    values_to = "value",
    values_drop_na = TRUE
  ) 

# placenta
comp_placenta_ctk_long <- comp_weights %>% 
  dplyr::select(genotype, mice_id, infection, mcp1, ifn) %>% 
  tidyr::pivot_longer(
    cols = c(mcp1, ifn),
    names_to = "cytokine",
    values_to = "value",
    values_drop_na = TRUE
  ) %>% 
  dplyr::mutate(region = "pl")

# bind tables
comp_cytok_long <- comp_dams_ctk_long %>% 
  dplyr::bind_rows(comp_placenta_ctk_long) %>% 
  dplyr::mutate(cytokine = factor(cytokine,
                                  levels = c("mcp1", "ifn", "tnf", "il10"))) %>% 
  dplyr::mutate(region = factor(region,
                                levels = c("sr", "sp", "pl")))

dplyr::glimpse(comp_cytok_long)

# save data
#saveRDS(comp_cytok_long, "Data/Clean_data/comp_cytok_long.rds")
