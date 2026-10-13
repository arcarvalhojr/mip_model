#### Clean data for mip model analysis ###################################

# Here the raw data for damns and fetus related analysis will be cleaned and
# saved into .rds files.

# import libraries --------------------------------------------------------
library(tidyverse)
library(readxl)


# Import data  ------------------------------------------------------------
raw_data_dams <- readxl::read_xlsx("Data/Raw_data/model_data_dams.xlsx")
raw_data_fetus <- readxl::read_xlsx("Data/Raw_data/model_data_fetus.xlsx")
raw_model_parasitemia <- readxl::read_xlsx(
  "Data/Raw_data/model_parasitemia.xlsx")

# data related to inf and non-inf dams ------------------------------------

glimpse(raw_data_dams)

# padronize variables to lowercase
# assign the real names for variable infection
# control group doesn't have parasitemia

infection_name_map = c(
  "0" = "pbnk65-",
  "2" = "pbnk65+",
  "4" = "pbnk65++",
  "8" = "pbnk65+++"
)

model_dams <- raw_data_dams %>%
  dplyr::mutate(mice_id = tolower(mice_id)) %>% 
  dplyr::mutate(infection = recode(infection, !!!infection_name_map)) %>% 
  dplyr::mutate(infection = factor(
    infection, levels = c("pbnk65-", "pbnk65+", "pbnk65++", "pbnk65+++"))) %>%
  dplyr::mutate(ptd = as.factor(ptd))
  

dplyr::glimpse(model_dams)

#saveRDS(model_dams, "Data/Clean_data/model_dams.rds")


# Parasitemia data for preg and non-preg infected dams --------------------

glimpse(raw_model_parasitemia)

# assign the real names for variable infection

parasitemia_inf_name_map <- c(
  "2" = "pbnk65+",
  "4" = "pbnk65++",
  "8" = "pbnk65+++"
)

model_parasitemia <- raw_model_parasitemia %>% 
  dplyr::mutate(infection = recode(
    infection, !!!parasitemia_inf_name_map
  )) %>% 
  dplyr::mutate(infection = factor(
    infection, levels = c("pbnk65+", "pbnk65++", "pbnk65+++"))) %>% 
  dplyr::mutate(preg = as.factor(preg))

#saveRDS(model_parasitemia, "Data/Clean_data/model_parasitemia.rds")

# data related to fetus and placentas ------------------------------------

glimpse(raw_data_fetus)

# assign the real names for variable infection
# variable fetal_viability should have "yes" as the reference level

model_weights <- raw_data_fetus %>%
  dplyr::mutate(dplyr::across(c(mice_id, fetus_id), tolower)) %>% 
  dplyr::mutate(infection = recode(infection, !!!infection_name_map)) %>% 
  dplyr::mutate(infection = factor(
    infection, levels = c("pbnk65-", "pbnk65+", "pbnk65++", "pbnk65+++"))) %>%
  dplyr::mutate(ptd = as.factor(ptd)) %>% 
  dplyr::mutate(fetal_viability = factor(
    fetal_viability, levels = c("yes", "no")))

#saveRDS(model_weights, "Data/Clean_data/model_weights.rds")
