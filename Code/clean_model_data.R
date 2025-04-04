#### Clean data for mip model analysis ###################################


# import libraries --------------------------------------------------------
library(tidyverse)
library(readxl)

# datas related to dams ---------------------------------------------------
raw_data_dams <- read_xlsx("Data/Raw_data/model_data_dams.xlsx") 
glimpse(raw_data_dams)

#infection should be character
raw_data_dams$infection <- as.character(raw_data_dams$infection)
glimpse(raw_data_dams)


# model dams
model_dams <- raw_data_dams %>% 
  select(mice_id, infection, ptd, spleen_w)

saveRDS(model_dams, "Data/Clean_data/model_dams.rds")

# model hematology dams
model_hemato_clean <- raw_data_dams %>% 
  select(mice_id, infection, rbc, hct, hgb, lymph, mon, gran) %>% 
  drop_na()

saveRDS(model_hemato_clean, "Data/Clean_data/model_hemato_clean.rds")

# model cytokines dams
model_ctk_dams <- raw_data_dams %>% 
  select(mice_id, infection, mcp1_sr, ifn_sr, tnf_sr)

saveRDS(model_ctk_dams, "Data/Clean_data/model_ctk_dams.rds")

# model parasitemia dams
model_parasitemia <- read_xlsx("Data/Raw_data/model_parasitemia.xlsx")
glimpse(model_parasitemia)

#infection should be character
model_parasitemia$infection <- as.character(model_parasitemia$infection)

saveRDS(model_parasitemia, "Data/Clean_data/model_parasitemia.rds")

# datas related to fetus and placentas ------------------------------------
raw_data_fetus <- read_xlsx("Data/Raw_data/model_data_fetus.xlsx")
glimpse(raw_data_fetus)

#infection should be character
raw_data_fetus$infection <- as.character(raw_data_fetus$infection)
glimpse(raw_data_fetus)

# model weights
model_weights <- raw_data_fetus %>% 
  select(mice_id, fetus_id, ini_weight, litter_size, infection, fetal_weight,
         placenta_weight, ratio)

saveRDS(model_weights, "Data/Clean_data/model_weights.rds")

# model pb18s data
model_pb18s_clean <- raw_data_fetus %>% 
  select(fetus_id, infection, pb18s) %>% 
  filter(!is.na(pb18s))

saveRDS(model_pb18s_clean, "Data/Clean_data/model_pb18s_clean.rds")

# model vascular space (sinusoides) data
model_sinusoides_clean <- raw_data_fetus %>% 
  select(fetus_id, infection, vascular_space) %>% 
  filter(!is.na(vascular_space))

saveRDS(model_sinusoides_clean, "Data/Clean_data/model_sinusoides_clean.rds")
