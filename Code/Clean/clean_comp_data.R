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

# control group doesn't have parasitemia
# padronize variables to lowercase
# b6 ntac should be the reference level for analysis


# comp dams
comp_dams <- raw_data_dams %>% 
  dplyr::mutate(dplyr::across(c(genotype, mice_id), tolower)) %>% 
  dplyr::mutate(genotype = factor(genotype, levels = c("bntac", "bj"))) %>% 
  dplyr::mutate(dplyr::across(c(infection, ptd), as.factor))
  

#saveRDS(comp_dams, "Data/Clean_data/comp_dams.rds")


# Parasitemia data for preg and non-preg infected dams --------------------

dplyr::glimpse(raw_comp_parasitemia)

# padronize variables to lowercase
# b6 ntac should be the reference level for analysis

comp_parasitemia <- raw_comp_parasitemia %>% 
  dplyr::mutate(dplyr::across(c(genotype, mice_id), tolower)) %>% 
  dplyr::mutate(
    genotype = dplyr::recode(genotype, "bnt" = "bntac"), 
    genotype = factor(genotype, levels = c("bntac", "bj")),
    preg = as.factor(preg)
    )


#saveRDS(comp_parasitemia, "Data/Clean_data/comp_parasitemia.rds")

# data related to fetus and placentas -------------------------------------

glimpse(raw_data_fetus)

# padronize variables to lowercase
# b6 ntac should be the reference level for analysis
# variable fetal_viability should have "yes" as the reference level

# comp fetus and placenta weights
comp_weights <- raw_data_fetus %>% 
  dplyr::mutate(dplyr::across(c(genotype, mice_id, fetus_id), tolower)) %>% 
  dplyr::mutate(genotype = factor(genotype ,levels = c("bntac", "bj"))) %>% 
  dplyr::mutate(fetal_viability = factor(
    fetal_viability, levels = c("yes", "no"))) %>% 
  dplyr::mutate(dplyr::across(c(infection, ptd), as.factor)) 
  
  

#saveRDS(comp_weights, "Data/Clean_data/comp_weights.rds")
