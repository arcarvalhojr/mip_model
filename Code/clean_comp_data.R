#### Clean data for mip comparrison model analysis ###########################


# import libraries --------------------------------------------------------
library(tidyverse)
library(readxl)


# datas related to dams ---------------------------------------------------
raw_data_dams <- read_xlsx("Data/Raw_data/comp_data_dams.xlsx")
glimpse(raw_data_dams)

#genotype bntac should be the reference level
raw_data_dams$genotype <- factor(raw_data_dams$genotype,
                                    levels = c("BNTac", "BJ"))
levels(raw_data_dams$genotype)

# comp dams
comp_dams <- raw_data_dams %>% 
  select(genotype, mice_id, infection, ptd, spleen_w)

saveRDS(comp_dams, "Data/Clean_data/comp_dams.rds")

# comp hematology dams
comp_hemato_clean <- raw_data_dams %>% 
  select(genotype, mice_id, infection, rbc, hct, hgb,
         wbc, lymph, mon, gran) %>% 
  drop_na()

saveRDS(comp_hemato_clean, "Data/Clean_data/comp_hemato_clean.rds")

# comp cytokines dams
comp_ctk_dams <- raw_data_dams %>% 
  select(genotype, mice_id, infection, mcp1_sp, ifn_sp, tnf_sp, il10_sp,
         mcp1_sr, ifn_sr, tnf_sr)

saveRDS(comp_ctk_dams, "Data/Clean_data/comp_ctk_dams.rds")

# datas related to fetus and placentas ------------------------------------
raw_data_fetus <- read_xlsx("Data/Raw_data/comp_data_fetus.xlsx")
glimpse(raw_data_fetus)

#genotype bntac should be the reference level
raw_data_fetus$genotype <- factor(raw_data_fetus$genotype,
                                 levels = c("BNTac", "BJ"))
levels(raw_data_fetus$genotype)

# comp fetus and placenta weights
comp_weights <- raw_data_fetus %>% 
  select(genotype, mice_id, fetus_id, ini_weight, litter_size, infection,
         fetal_weight, placenta_weight, ratio, alive)

saveRDS(comp_weights, "Data/Clean_data/comp_weights.rds")

# comp pb18s
comp_pb18s <- raw_data_fetus %>% 
  select(genotype, fetus_id, infection, pb18s) %>% 
  filter(!is.na(pb18s))

saveRDS(comp_pb18s, "Data/Clean_data/comp_pb18s.rds")

# comp cytokines placentas
comp_ctk_placentas <- raw_data_fetus %>% 
  select(genotype, mice_id, fetus_id, infection, mcp1, ifn)

saveRDS(comp_ctk_placentas, "Data/Clean_data/comp_ctk_placentas.rds")
