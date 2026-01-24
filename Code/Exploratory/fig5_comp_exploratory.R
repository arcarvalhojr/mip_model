######## Exploratory analysis of data related to figure 5 ##########


# import library ----------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks (R², outliers, assumptions, etc.)
library(flexplot)     # For visual diagnostics and exploratory data analysis

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

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_mcp1_sr, plot = TRUE)

flexplot::visualize(glm_mcp1_sr, plot = "residuals")

performance::check_model(glm_mcp1_sr)

performance::check_outliers(glm_mcp1_sr, method = "cook")

#outlier detected

# removing outlier
comp_mcp1_sr_clean <- comp_dams %>% 
  dplyr::select(genotype, mice_id, infection, mcp1_sr) %>% 
  dplyr::filter(!is.na(mcp1_sr))

outs_mcp1_sr <- performance::check_outliers(glm_mcp1_sr, method = "cook")

comp_mcp1_sr_clean <- comp_mcp1_sr_clean %>% 
  dplyr::slice(-which(outs_mcp1_sr))

# looking the data without outs
ggplot2::ggplot(comp_mcp1_sr_clean,
                aes(x = infection, y = mcp1_sr, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# glm model without outs
glm_mcp1_sr1 <- glm(mcp1_sr ~ genotype, data = comp_mcp1_sr_clean)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_mcp1_sr1, plot = TRUE)

flexplot::visualize(glm_mcp1_sr1, plot = "residuals")

# model output
performance::model_performance(glm_mcp1_sr1)

summary(glm_mcp1_sr1)

# analyse of infection*genotype on levels of serum ifn --------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = ifn_sr, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
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

flexplot::visualize(glm_ifn_sr, plot = "residuals")

performance::check_model(glm_ifn_sr)

performance::check_outliers(glm_ifn_sr, method = "cook")

#outlier detected

# removing outs
comp_ifn_sr_clean <- comp_dams %>% 
  dplyr::select(mice_id, genotype, infection, ifn_sr) %>% 
  dplyr::filter(!is.na(ifn_sr))

outs_ifn_sr <- performance::check_outliers(glm_ifn_sr, method = "cook")

comp_ifn_sr_clean <- comp_ifn_sr_clean %>% 
  dplyr::slice(-which(outs_ifn_sr))

# glm model without outs
glm_ifn_sr1 <- glm(ifn_sr ~ genotype, data = comp_ifn_sr_clean)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_ifn_sr1, plot = TRUE)

flexplot::visualize(glm_ifn_sr1)

# model output
performance::model_performance(glm_ifn_sr1)

summary(glm_ifn_sr1)

# analyse of infection*genotype on levels of serum tnf --------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = tnf_sr, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()


comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(tnf_sr)),
                   n_obs_NA = sum(is.na(tnf_sr)))

#apparently there was not cytokines detected on non-inf dams serum

# glm model
glm_tnf_sr <- glm(tnf_sr ~ genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_tnf_sr, plot = TRUE)

flexplot::visualize(glm_tnf_sr)

performance::check_outliers(glm_tnf_sr, method = "cook")

#outlier detected

# removing outs
comp_tnf_sr_clean <- comp_dams %>% 
  dplyr::select(mice_id, genotype, infection, tnf_sr) %>% 
  dplyr::filter(!is.na(tnf_sr))

outs_tnf_sr <- performance::check_outliers(glm_tnf_sr, method = "cook")

comp_tnf_sr_clean <- comp_tnf_sr_clean %>% 
  dplyr::slice(-which(outs_tnf_sr))

# glm model without outs
glm_tnf_sr1 <- glm(tnf_sr ~ genotype, data = comp_tnf_sr_clean)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_tnf_sr1, plot = TRUE)

flexplot::visualize(glm_tnf_sr1)

# model output
performance::model_performance(glm_tnf_sr1)

summary(glm_tnf_sr1)

# analyse of infection*genotype on levels of spleen mcp1 ------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = mcp1_sp, fill= infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(mcp1_sp)),
                   n_obs_NA = sum(is.na(mcp1_sp)))

#also only detected levels in infected damns

# glm model
glm_mcp1_sp <- glm(mcp1_sp ~ genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_mcp1_sp, plot = TRUE)

flexplot::visualize(glm_mcp1_sp)

performance::check_outliers(glm_mcp1_sp, method = "cook")

# model output
performance::model_performance(glm_mcp1_sp)

summary(glm_mcp1_sp)

# analyse of infection*genotype on levels of spleen ifn -------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = ifn_sp, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(ifn_sp)),
                   n_obs_NA = sum(is.na(ifn_sp)))

# glm model
glm_ifn_sp <- glm(ifn_sp ~ genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_ifn_sp, plot = TRUE)

flexplot::visualize(glm_ifn_sp)

performance::check_outliers(glm_ifn_sp, method = "cook")

# model output
performance::model_performance(glm_ifn_sp)

summary(glm_ifn_sp)

# analyse of infection*genotype on levels of spleen tnf -------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = tnf_sp, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(tnf_sp)),
                   n_obs_NA = sum(is.na(tnf_sp)))

# glm model
glm_tnf_sp <- glm(tnf_sp ~ genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_tnf_sp, plot = TRUE)

flexplot::visualize(glm_tnf_sp)

performance::check_outliers(glm_tnf_sp, method = "cook")

#outlier detected

# removing outs
comp_tnf_sp_clean <- comp_dams %>% 
  select(mice_id, genotype, infection, tnf_sp) %>% 
  filter(!is.na(tnf_sp))

outs_tnf_sp <- performance::check_outliers(glm_tnf_sp, method = "cook")


comp_tnf_sp_clean <- comp_tnf_sp_clean %>% 
  dplyr::slice(-which(outs_tnf_sp))


# glm model
glm_tnf_sp1 <- glm(tnf_sp ~ genotype, data = comp_tnf_sp_clean)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_tnf_sp1, plot = TRUE)


# model output
performance::model_performance(glm_tnf_sp1)

summary(glm_tnf_sp1)

# analyse of infection*genotype on levels of spleen il10 ------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, y = il10_sp, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

comp_dams %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(il10_sp)),
                   n_obs_NA = sum(is.na(il10_sp)))


# glm model
glm_il10_sp <- glm(il10_sp ~ infection * genotype, data = comp_dams)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_il10_sp, plot = TRUE)

flexplot::visualize(glm_il10_sp, plot = "residuals")

performance::check_model(glm_il10_sp)

performance::check_outliers(glm_il10_sp, method = "cook")

# model output
performance::model_performance(glm_il10_sp)

summary(glm_il10_sp)

# analyse of infection*genotype on levels of placenta mcp1 ----------------

# looking the data
ggplot2::ggplot(comp_weights,
                aes(x = infection, y = mcp1, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

comp_weights %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(mcp1)),
                   n_obs_NA = sum(is.na(mcp1)))

#many NAs. not all placentas were used for cytokine quantification

# glm model
glm_mcp1_pl <- glm(mcp1 ~ infection * genotype, data = comp_weights,
                   family = Gamma(link = "identity"))

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_mcp1_pl, plot = TRUE)

flexplot::visualize(glm_mcp1_pl, plot = "residuals")

performance::check_model(glm_mcp1_pl)

performance::check_outliers(glm_mcp1_pl, method = "cook")


# model output
performance::model_performance(glm_mcp1_pl)

summary(glm_mcp1_pl)

# analyse of infection*genotype on levels of placenta ifn -----------------

# looking the data
ggplot2::ggplot(comp_weights, aes(x = infection, y = ifn, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

comp_weights %>% 
  dplyr::group_by(genotype, infection) %>% 
  dplyr::summarise(n_obs = sum(!is.na(ifn)),
                   n_obs_NA = sum(is.na(ifn)))

# glm model
glm_ifn_pl <- glm(ifn ~ genotype, data = comp_weights)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_ifn_pl, plot = TRUE)

flexplot::visualize(glm_ifn_pl, plot = "residuals")

performance::check_model(glm_ifn_pl)

performance::check_outliers(glm_ifn_pl, method = "cook")

# model output
performance::model_performance(glm_ifn_pl)

summary(glm_ifn_pl)

# save model outpput ------------------------------------------------------

models_list <- dplyr::tibble(
  model_name = c("serum mcp-1", "serum ifn", "serum tnf",
                 "spleen mcp-1", "spleen ifn", "spleen tnf", "spleen il-10",
                 "placenta mcp-1", "placenta ifn"),
  model_output = list(glm_mcp1_sr1, glm_ifn_sr1, glm_tnf_sr1, glm_mcp1_sp,
                      glm_ifn_sp, glm_tnf_sp1, glm_il10_sp, glm_mcp1_pl,
                      glm_ifn_pl)
)


#saveRDS(models_list, "Code/Results/fig5_model_outputs.rds")


# pivot data for visualization --------------------------------------------

comp_tnf_sp_clean <- comp_tnf_sp_clean %>% 
  dplyr::select(mice_id, tnf_sp)

comp_mcp1_sr_clean <- comp_mcp1_sr_clean %>% 
  dplyr::select(mice_id, mcp1_sr)

comp_ifn_sr_clean <- comp_ifn_sr_clean %>% 
  dplyr::select(mice_id, ifn_sr)

comp_tnf_sr_clean <- comp_tnf_sr_clean %>% 
  dplyr::select(mice_id, tnf_sr)

# damns
comp_dams_ctk_long <- comp_dams %>% 
  dplyr::select(genotype, mice_id, infection, mcp1_sp, ifn_sp, il10_sp) %>% 
  dplyr::left_join(comp_tnf_sp_clean, by = "mice_id") %>% 
  dplyr::left_join(comp_mcp1_sr_clean, by = "mice_id") %>% 
  dplyr::left_join(comp_ifn_sr_clean, by = "mice_id") %>% 
  dplyr::left_join(comp_tnf_sr_clean, by = "mice_id") %>% 
  tidyr::pivot_longer(
    cols = ends_with(c("sp", "sr")),
    names_to = c("cytokine", "region"),
    names_sep = "_",
    values_to = "value",
    values_drop_na = TRUE
  ) 

# placenta
comp_placenta_ctk_long <- comp_weights %>% 
  dplyr::select(mice_id, genotype, infection, mcp1, ifn) %>% 
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
