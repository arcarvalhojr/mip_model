#### Clean data for mip comparrison model analysis ###########################

# Here the raw data for damns and fetus related analysis will be cleaned and
# saved into .rds files.

# import libraries --------------------------------------------------------
library(tidyverse)
library(readxl)


# Import data -------------------------------------------------------------
raw_data_dams <- readxl::read_xlsx("Data/Raw_data/comp_data_dams.xlsx")
raw_comp_parasitemia <- readxl::read_xlsx("Data/Raw_data/comp_parasitemia.xlsx")
raw_data_fetus <- readxl::read_xlsx("Data/Raw_data/comp_data_fetus.xlsx")

# Data related to inf and non-inf dams ------------------------------------

dplyr::glimpse(raw_data_dams)

# control group doesn't have parasitemia, replace NA with 0
# padronize genotype names to lowercase
# b6 ntac should be the reference level for analysis


# comp dams
comp_dams <- raw_data_dams %>% 
  dplyr::mutate(parasitemia = if_else(
    is.na(parasitemia) & infection == "no", 0, parasitemia
  )) %>%
  dplyr::mutate(genotype = tolower(genotype)) %>% 
  dplyr::mutate(genotype = factor(genotype, levels = c("bntac", "bj")))
  

saveRDS(comp_dams, "Data/Clean_data/comp_dams.rds")



# Parasitemia data for preg and non-preg infected dams --------------------

dplyr::glimpse(raw_comp_parasitemia)

# padronize genotype names to lowercase
# b6 ntac should be the reference level for analysis

comp_parasitemia <- raw_comp_parasitemia %>% 
  dplyr::mutate(genotype = tolower(genotype)) %>% 
  dplyr::mutate(genotype = factor(genotype, levels = c("bnt", "bj")))


saveRDS(comp_parasitemia, "Data/Clean_data/comp_parasitemia.rds")

# data related to fetus and placentas -------------------------------------

glimpse(raw_data_fetus)

# padronize genotype names to lowercase
# b6 ntac should be the reference level for analysis

# comp fetus and placenta weights
comp_weights <- raw_data_fetus %>% 
  dplyr::mutate(genotype = tolower(genotype)) %>% 
  dplyr::mutate(genotype = factor(genotype ,levels = c("bntac", "bj")))
  

saveRDS(comp_weights, "Data/Clean_data/comp_weights.rds")
