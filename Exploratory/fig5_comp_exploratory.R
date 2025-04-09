######## Exploratory analysis of data related to figure 5 ##########


# import library ----------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(lme4)         # For fitting linear and mixed-effects models
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks (R², outliers, assumptions, etc.)
library(flexplot)     # For visual diagnostics and exploratory data analysis



# import clean data -------------------------------------------------------

# cytokines in damns data set
comp_ctk_damns <- readRDS("Data/Clean_data/comp_ctk_dams.rds")

# cytokines in fetus data set
comp_ctk_placentas <- readRDS("Data/Clean_data/comp_ctk_placentas.rds")



# analyse of infection*genotype on levels of serum mcp1 -------------------

# looking the data
ggplot(comp_ctk_damns, aes(x = infection, y = mcp1_sr, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

#only detected in infected damns

# removing NAs
comp_mcp1_sr_clean <- comp_ctk_damns %>% 
  select(mice_id, genotype, infection, mcp1_sr) %>% 
  filter(!is.na(mcp1_sr))

# glm model
glm_mcp1_sr <- glm(mcp1_sr ~ genotype, data = comp_mcp1_sr_clean)

# model diagnostic
simulateResiduals(fittedModel = glm_mcp1_sr, plot = TRUE)

visualize(glm_mcp1_sr, plot = "residuals")

check_outliers(glm_mcp1_sr, method = "cook")

# removing outlier
outs_mcp1_sr <- check_outliers(glm_mcp1_sr, method = "cook")

comp_mcp1_sr_clean <- comp_mcp1_sr_clean %>% 
  slice(-which(outs_mcp1_sr))

# looking the data without outs
ggplot(comp_mcp1_sr_clean, aes(x = infection, y = mcp1_sr, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# glm model without outs
glm_mcp1_sr2 <- glm(mcp1_sr ~ genotype, data = comp_mcp1_sr_clean)

# model diagnostic
simulateResiduals(fittedModel = glm_mcp1_sr2, plot = TRUE)

visualize(glm_mcp1_sr2, plot = "residuals")



# analyse of infection*genotype on levels of serum ifn --------------------

# looking the data
ggplot(comp_ctk_damns, aes(x = infection, y = ifn_sr, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# removing NAs
comp_ifn_sr_clean <- comp_ctk_damns %>% 
  select(mice_id, genotype, infection, ifn_sr) %>% 
  filter(!is.na(ifn_sr))

# glm model
glm_ifn_sr <- glm(ifn_sr ~ genotype, data = comp_ifn_sr_clean)

# model diagnostic
simulateResiduals(fittedModel = glm_ifn_sr, plot = TRUE)

visualize(glm_ifn_sr)

check_outliers(glm_ifn_sr, method = "cook")

# removing outs
outs_ifn_sr <- check_outliers(glm_ifn_sr, method = "cook")

comp_ifn_sr_clean <- comp_ifn_sr_clean %>% 
  slice(-which(outs_ifn_sr))

# glm model without outs
glm_ifn_sr2 <- glm(ifn_sr ~ genotype, data = comp_ifn_sr_clean)

# model diagnostic
simulateResiduals(fittedModel = glm_ifn_sr2, plot = TRUE)

visualize(glm_ifn_sr2)


# analyse of infection*genotype on levels of serum ifn --------------------

# looking the data
ggplot(comp_ctk_damns, aes(x = infection, y = tnf_sr, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# removing NAs
comp_tnf_sr_clean <- comp_ctk_damns %>% 
  select(mice_id, genotype, infection, tnf_sr) %>% 
  filter(!is.na(tnf_sr))

# glm model
glm_tnf_sr <- glm(tnf_sr ~ genotype, data = comp_tnf_sr_clean)

# model diagnostic
simulateResiduals(fittedModel = glm_tnf_sr, plot = TRUE)

visualize(glm_tnf_sr)

check_outliers(glm_tnf_sr, method = "cook")

# removing outs
outs_tnf_sr <- check_outliers(glm_tnf_sr, method = "cook")

comp_tnf_sr_clean <- comp_tnf_sr_clean %>% 
  slice(-which(outs_tnf_sr))

# glm model without outs
glm_tnf_sr2 <- glm(tnf_sr ~ genotype, data = comp_tnf_sr_clean)

# model diagnostic
simulateResiduals(fittedModel = glm_tnf_sr2, plot = TRUE)

visualize(glm_tnf_sr2)


# analyse of infection*genotype on levels of spleen mcp1 ------------------

# looking the data
ggplot(comp_ctk_damns, aes(x = infection, y = mcp1_sp, fill= infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

#also only detected levels in infected damns

# removing NAs
comp_mcp1_sp_clean <- comp_ctk_damns %>% 
  select(mice_id, genotype, infection, mcp1_sp) %>% 
  filter(!is.na(mcp1_sp))

# glm model
glm_mcp1_sp <- glm(mcp1_sp ~ genotype, data = comp_mcp1_sp_clean)

# model diagnostic
simulateResiduals(fittedModel = glm_mcp1_sp, plot = TRUE)

visualize(glm_mcp1_sp)

check_outliers(glm_mcp1_sp, method = "cook")


# analyse of infection*genotype on levels of spleen ifn -------------------

# looking the data
ggplot(comp_ctk_damns, aes(x = infection, y = ifn_sp, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# removing NAs
comp_ifn_sp_clean <- comp_ctk_damns %>% 
  select(mice_id, genotype, infection, ifn_sp) %>% 
  filter(!is.na(ifn_sp))

# glm model
glm_ifn_sp <- glm(ifn_sp ~ genotype, data = comp_ifn_sp_clean)

# model diagnostic
simulateResiduals(fittedModel = glm_ifn_sp, plot = TRUE)

visualize(glm_ifn_sp)

check_outliers(glm_ifn_sp, method = "cook")


# analyse of infection*genotype on levels of spleen tnf -------------------

# looking the data
ggplot(comp_ctk_damns, aes(x = infection, y = tnf_sp, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# removing NAs
comp_tnf_sp_celan <- comp_ctk_damns %>% 
  select(mice_id, genotype, infection, tnf_sp) %>% 
  filter(!is.na(tnf_sp))

# glm model
glm_tnf_sp <- glm(tnf_sp ~ genotype, data = comp_tnf_sp_celan)

# model diagnostic
simulateResiduals(fittedModel = glm_tnf_sp, plot = TRUE)

visualize(glm_tnf_sp)

check_outliers(glm_tnf_sp, method = "cook")


# analyse of infection*genotype on levels of spleen il10 ------------------

# looking the data
ggplot(comp_ctk_damns, aes(x = infection, y = il10_sp, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# glm model
glm_il10_sp <- glm(il10_sp ~ infection * genotype, data = comp_ctk_damns)

# model diagnostic
simulateResiduals(fittedModel = glm_il10_sp, plot = TRUE)

visualize(glm_il10_sp)

check_outliers(glm_il10_sp, method = "cook")


# analyse of infection*genotype on levels of placenta mcp1 ----------------

# looking the data
ggplot(comp_ctk_placentas, aes(x = infection, y = mcp1, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# glm model
glm_mcp1_pl <- glm(mcp1 ~ infection * genotype, data = comp_ctk_placentas)

# model diagnostic
simulateResiduals(fittedModel = glm_mcp1_pl, plot = TRUE)

visualize(glm_mcp1_pl)

check_outliers(glm_mcp1_pl, method = "cook")


# analyse of infection*genotype on levels of placenta ifn -----------------

# looking the data
ggplot(comp_ctk_placentas, aes(x = infection, y = ifn, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# removing NAs
comp_ifn_pl_clean <- comp_ctk_placentas %>% 
  select(mice_id, fetus_id, genotype, infection, ifn) %>% 
  filter(!is.na(ifn))

# glm model
glm_ifn_pl <- glm(ifn ~ genotype, data = comp_ifn_pl_clean)

# model diagnostic
simulateResiduals(fittedModel = glm_ifn_pl, plot = TRUE)

visualize(glm_ifn_pl)

check_outliers(glm_ifn_pl, method = "cook")

