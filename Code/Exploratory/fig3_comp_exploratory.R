######## Exploratory analysis of data related to figure 3 ##########


# import libraries --------------------------------------------------------

library(tidyverse)    # For data manipulation and visualization (ggplot2, dplyr, etc.)
library(DHARMa)       # For residual diagnostics using simulated residuals
library(performance)  # For model performance checks (R², outliers, assumptions, etc.)


# import clean data -------------------------------------------------------

# dams data set
comp_dams <- readRDS("Data/Clean_data/comp_dams.rds") 

# peripheral parasitemia data set
comp_parasitemia <- readRDS("Data/Clean_data/comp_parasitemia.rds")

# pb18s data set
comp_weights <- readRDS("Data/Clean_data/comp_weights.rds")


# analyse of infection*genotype on the chances of ptd ---------------------

# looking the data
ggplot2::ggplot(comp_dams, aes(x = infection, fill = ptd)) +
  geom_bar(position = position_dodge()) +
  facet_wrap(~ genotype) 

# glm model
glm_ptd <- glm(factor(ptd) ~ infection * genotype, data = comp_dams,
               family = "binomial")

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_ptd, plot = TRUE)


# analyse of pregnancy*genotype on peripheral parasitemia -----------------

# looking the data
ggplot2::ggplot(comp_parasitemia,
                aes(x = genotype, y = parasitemia, fill = preg))+
  geom_boxplot()

# glm model
glm_parasitemia <- glm(parasitemia ~ preg * genotype, data = comp_parasitemia)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_parasitemia, plot = TRUE)

performance::check_outliers(glm_parasitemia, method = "cook")

#outlier detected

# removing outs
outs_parasitemia <- performance::check_outliers(
  glm_parasitemia, method = "cook")

comp_parasitemia <- comp_parasitemia %>%
  dplyr::slice(-which(outs_parasitemia))

# glm model without outs
glm_parasitemia_2 <- glm(
  parasitemia ~ preg * genotype, data = comp_parasitemia)

DHARMa::simulateResiduals(fittedModel = glm_parasitemia_2, plot = TRUE)

# saving clean data
#saveRDS(comp_parasitemia, "Data/Clean_data/comp_parasitemia.rds")


# analyse of genotype on placental pb18s ----------------------------------

# looking the data
ggplot2::ggplot(comp_weights,
                aes(x = genotype, y = log(pb18s), fill = genotype)) +
  geom_boxplot() +
  theme_bw()

# glm model
glm_pb18s <- glm(pb18s ~ genotype, family = Gamma(link = "log"),
                 data = comp_weights)

# model diagnostic
DHARMa::simulateResiduals(fittedModel = glm_pb18s, plot = TRUE)

performance::check_outliers(glm_pb18s, method = "cook")

#outlier detected

# removing outs
comp_pb18s_clean <- comp_weights %>% 
  dplyr::select(genotype, mice_id, fetus_id, infection, pb18s) %>% 
  tidyr::drop_na()

outs_pb18s <- performance::check_outliers(glm_pb18s, method = "cook")  
  
comp_pb18s_clean <- comp_pb18s_clean %>%
  dplyr::slice(-which(outs_pb18s))

# looking the data without outs
ggplot2::ggplot(comp_pb18s_clean,
                aes(x = genotype, y = log(pb18s), fill = genotype)) +
  geom_boxplot() +
  theme_bw()

# glm model without outs
glm_pb18s_2 <- glm(pb18s ~ genotype, family = Gamma(link = "log"),
                         data = comp_pb18s_clean)

DHARMa::simulateResiduals(fittedModel = glm_pb18s_2, plot = TRUE)

# saving clean data
#saveRDS(comp_pb18s_clean, "Data/Clean_data/comp_pb18s_clean.rds")
