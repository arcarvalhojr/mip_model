### Figure plots of data related to figure 1 ###


# Import libraries --------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(broom)
library(ggpubr)

# import clean data -------------------------------------------------------

# dams data set
model_dams <- readRDS("Data/Clean_data/model_dams.rds")

# fetus data set
model_pb18s <- readRDS("Data/Clean_data/model_pb18s_clean.rds")

# dams peripheral parasitemia data set
model_parasitemia <- readRDS("Data/Clean_data/model_parasitemia.rds")

# hematologic parameters data set
model_hematology <- readRDS("Data/Clean_data/model_hemato_clean.rds")


# import models outputs
models_output <- readRDS("Code/Results/fig1_model_outputs.rds")


# spleen weight -----------------------------------------------------------

# get model output and p-value
glm_sw = models_output %>% 
  dplyr::filter(model_name == "spleen weight") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]] %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(
    group1 = "pbnk65-",
    group2 = str_remove(term, "infection"),
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  )

# create the plot
ggplot2::ggplot(model_dams, aes(x= infection, y= spleen_w, fill= infection)) +
  stat_summary(fun.data = ggpubr::mean_sd, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE) + #add std errorbars
  stat_summary(fun= mean, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") + #add the mean
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, shape= 21, size= 6,
                               color= "black") + #add datapoints in each group
  labs(x= NULL, y= "Spleen Weight (mg)") +
  scale_fill_manual(values= c("#636363", "#fcae91", "#fb6a4a", "#cb181d")) + #add custom colors (colorblind safe)
  scale_y_continuous(limits = c(50, 450), breaks = seq(50, 450, by= 100),
                     expand = c(0,0)) + #customize axis limits
  theme_classic() + #add classic theme with white background
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) + #change text font size
  ggpubr::stat_pvalue_manual(
    glm_sw, 
    label = "p_label", 
    y.position = c(370, 400, 430),
    tip.length = 0.01,
    label.size = 5)
 
ggsave("Images/Plots/model_spleen.png", width = 7, height = 5) #save image


# peripheral parasitemia --------------------------------------------------

# get model output and p-value
glm_para = models_output %>% 
  dplyr::filter(model_name == "peripheral parasitemia") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]] %>%
  broom::tidy() %>% 
  mutate(
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  )

# create a df with the p-value text label 
parasitemia_stat <- data.frame(
  preg = "no",             
  infection = "pbnk65+",  
  label = "Pregnancy effect: *p*<0.001"
)

# create the plot
ggplot2::ggplot(model_parasitemia, aes(x= preg, y= parasitemia, fill= preg)) +
  stat_summary(fun.data = ggpubr::mean_sd, geom= "errorbar",
               color= "black", width= 0.3) +
  stat_summary(show.legend = FALSE, fun = mean, geom = "crossbar",
               width= 0.5, color= "black") +
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, shape= 21,
                               size= 5, color= "black") +
  facet_grid(~ infection) +
  labs(x= "Pregnancy", y= "Peripheral parasitemia (%)") +
  scale_x_discrete(breaks = c("no", "yes"),
                   labels = c("no", "yes")) +
  scale_fill_manual(values = c("#ffffff", "#cb181d")) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by= 3),
                     expand = c(0,0)) +
  theme_classic() +
  theme(#strip.background = element_blank(),
        strip.text = element_text(size= 14),
        axis.title.y = element_text(size= 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) +
  ggtext::geom_richtext(
    data = parasitemia_stat, 
    aes(label = label, y = 13),
    fill = NA,          
    label.color = NA,   
    size = 5,           
    hjust = 0.25           
  )

ggsave("Images/Plots/model_parasitemia.png", width = 8, height = 5)


# placental parasitemia (pb18s expression) --------------------------------

# get model output and p-value
glm_pb18s = models_output %>% 
  dplyr::filter(model_name == "pb18s expression") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]] %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(
    group1 = "pbnk65+",
    group2 = str_remove(term, "infection"),
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  )

# create the plot
ggplot2::ggplot(model_pb18s, aes(x= infection, y= pb18s, fill= infection)) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") +
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, size = 5,
                               shape= 21, color= "black") +
  labs(x= NULL, y = bquote("pb18s mRNA (" * 2^{-Delta * Delta * Ct} * ")")) +
  scale_fill_manual(values= c("#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_log10() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) +
  ggpubr::stat_pvalue_manual(
    glm_pb18s, 
    label = "p_label", 
    y.position = c(4, 4.5),
    tip.length = 0.01,
    label.size = 5)

ggsave("Images/Plots/model_pb18s.png", width= 6, height= 5)


# erithrocytes (RBC) ------------------------------------------------------

# get model output and p-value
glm_rbc = models_output %>% 
  dplyr::filter(model_name == "erytrocytes") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]] %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(
    group1 = "pbnk65-",
    group2 = str_remove(term, "infection"),
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  )

# create the plot
ggplot2::ggplot(model_hematology, aes(x= infection, y= rbc, fill= infection)) +
  stat_summary(fun.data = ggpubr::mean_sd, geom= "errorbar", color= "black",
               width= 0.3, show.legend = FALSE) + 
  stat_summary(fun = mean, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") + 
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, shape= 21,
                               size= 6, color= "black") + 
  labs(x= NULL, y = bquote("Red blood cells (x 10"^6*"/"*mu*"L)")) +
  scale_fill_manual(values = c("#636363","#fcae91", "#fb6a4a", "#cb181d")) + 
  scale_y_continuous(limits = c(4, 10), breaks = seq(4, 10, by= 2),
                     expand = c(0,0)) + 
  theme_classic() + 
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16)) +
  ggpubr::stat_pvalue_manual(
    glm_rbc, 
    label = "p_label", 
    y.position = c(8.6, 9.1, 9.6),
    tip.length = 0.01,
    label.size = 5)


ggsave("Images/Plots/model_rbc.png", width = 7, height = 5) 


# hematocrite (HCT) -------------------------------------------------------

# get model output and p-value
glm_hct = models_output %>% 
  dplyr::filter(model_name == "hematocrite") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]] %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(
    group1 = "pbnk65-",
    group2 = str_remove(term, "infection"),
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  )

# create the plot 
ggplot2::ggplot(model_hematology, aes(x= infection, y= hct, fill= infection)) +
  stat_summary(fun.data = ggpubr::mean_sd, geom= "errorbar", color= "black",
               width= 0.3, show.legend = FALSE) +
  stat_summary(fun = mean, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") +
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, shape= 21,
                               size= 6, color= "black") +
  labs(x= NULL, y= "Hematocrit (%)") +
  scale_fill_manual(values = c("#636363", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(limits = c(23, 43), breaks = seq(23, 43, by= 5),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15)) +
  ggpubr::stat_pvalue_manual(
    glm_hct, 
    label = "p_label", 
    y.position = c(39.2, 40.6, 42),
    tip.length = 0.01,
    label.size = 5)
  

ggsave("Images/Plots/model_hct.png", width = 7, height = 5)


# hemoglobin (HGB) --------------------------------------------------------

# get model output and p-value
glm_hgb = models_output %>% 
  dplyr::filter(model_name == "hemoglobin") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]] %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(
    group1 = "pbnk65-",
    group2 = str_remove(term, "infection"),
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  )

# create the plot
ggplot2::ggplot(model_hematology, aes(x= infection, y= hgb, fill= infection)) +
  stat_summary(fun.data = ggpubr::mean_sd, geom= "errorbar", color= "black",
               width= 0.3, show.legend = FALSE) +
  stat_summary(fun = mean, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") +
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, shape= 21,
                               size= 6, color= "black") +
  labs(x= NULL, y = "Hemoglobin (g/dL)") +
  scale_fill_manual(values = c("#636363", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(limits = c(8, 16), breaks = seq(8, 16, by= 2),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) +
  ggpubr::stat_pvalue_manual(
    glm_hgb, 
    label = "p_label", 
    y.position = c(13.7, 14.5, 15.3),
    tip.length = 0.01,
    label.size = 5)

ggsave("Images/Plots/model_hgb.png", width = 7, height = 5)

