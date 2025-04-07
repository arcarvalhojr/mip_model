######## Exploratory analysis of data related to figure 3 ##########


# import libraries --------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks (R², outliers, assumptions, etc.)


### importando os data sets ####

# import clean data -------------------------------------------------------

# dams data set
comp_dams <- readRDS("Data/Clean_data/comp_dams.rds") 

# hematology data set
comp_hematology <- readRDS("Data/Clean_data/comp_hemato_clean.rds")

# peripheral parasitemia data set
comp_parasitemia <- readRDS("Data/Clean_data/comp_parasitemia.rds")

# pb18s data set
comp_pb18s <- readRDS("Data/Clean_data/comp_pb18s.rds")

# analyse of infection*genotype on rbc ------------------------------------

# looking the data
ggplot(comp_hematology, aes(x = infection, y = rbc, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# glm model
glm_rbc <- glm(rbc ~ infection * genotype, data = comp_hematology)

# model diagnostic
simulateResiduals(fittedModel = glm_rbc, plot = TRUE)

check_outliers(glm_rbc, method = "cook")


# analyse of infection*genotype on hct ------------------------------------

# looking the data
ggplot(comp_hematology, aes(x = infection, y = hct, fill = nfection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# glm model
glm_hct <- glm(hct ~ infection * genotype, data = comp_hematology)

# model diagnostic
simulateResiduals(fittedModel = glm_hct, plot = TRUE)

check_outliers(glm_hct, method = "cook")


# analyse of infection*genotype on hgb ------------------------------------

# looking the data
ggplot(comp_hematology, aes(x = infection, y = hgb, fill = infection))+
  geom_boxplot(show.legend = FALSE)+
  facet_wrap(~ genotype)+
  theme_bw()

# glm model
glm_hgb <- glm(hgb ~ infection * genotype, data = comp_hematology)

# model diagnostic
simulateResiduals(fittedModel = glm_hgb, plot = TRUE)

check_outliers(glm_hgb, method = "cook")


# analyse of infection*genotype on the chances of ptd ---------------------

# looking the data
ggplot(comp_dams, aes(x = infection, fill = ptd)) +
  geom_bar(position = position_dodge()) +
  facet_wrap(~ genotype) 

# glm model
glm_ptd <- glm(factor(ptd) ~ infection * genotype, data = comp_dams,
               family = "binomial")

# model diagnostic
simulateResiduals(fittedModel = glm_ptd, plot = TRUE)


# analyse of pregnancy*genotype on peripheral parasitemia -----------------

# looking the data
ggplot(comp_parasitemia, aes(x = genotype, y = parasitemia, fill = preg))+
  geom_boxplot()

# glm model
glm_parasitemia <- glm(parasitemia ~ preg * genotype, data = comp_parasitemia)

# model diagnostic
simulateResiduals(fittedModel = glm_parasitemia, plot = TRUE)

check_outliers(glm_parasitemia, method = "cook")

# removing outs
outs_parasitemia <- check_outliers(glm_parasitemia, method = "cook")

comp_parasitemia <- comp_parasitemia %>%
  slice(-which(outs_parasitemia))

# glm model without outs
glm_parasitemia_2 <- glm(parasitemia ~ preg * genotype, data = comp_parasitemia)

simulateResiduals(fittedModel = glm_parasitemia_2, plot = TRUE)

# saving clean data
#saveRDS(comp_parasitemia, "Data/Clean_data/comp_parasitemia.rds")


# analyse of genotype on placental pb18s ----------------------------------

# looking the data
ggplot(comp_pb18s, aes(x = genotype, y = log(pb18s), fill = genotype)) +
  geom_boxplot() +
  theme_bw()

# glm model
glm_pb18s <- glm(pb18s ~ genotype, family = Gamma(link = "log"),
                 data = comp_pb18s)

# model diagnostic
simulateResiduals(fittedModel = glm_pb18s, plot = TRUE)

check_outliers(glm_pb18s, method = "cook")

# removing outs
outs_pb18s <- check_outliers(glm_pb18s, method = "cook")

comp_pb18s <- comp_pb18s %>%
  slice(-which(outs_pb18s))

# looking the data without outs
ggplot(comp_pb18s, aes(x = genotype, y = log(pb18s), fill = genotype)) +
  geom_boxplot() +
  theme_bw()

# glm model without outs
glm_pb18s_2 <- glm(pb18s ~ genotype, family = Gamma(link = "log"),
                         data = comp_pb18s)

simulateResiduals(fittedModel = glm_pb18s_2, plot = TRUE)

# saving clean data
#saveRDS(comp_pb18s, "Data/Clean_data/comp_pb18s.rds")
