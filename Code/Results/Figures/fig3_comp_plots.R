### Figure plots of data related to figure 3 ###


# Import libraries --------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(broom)
library(ggpubr)

# import clean data -------------------------------------------------------

# dams data set
comp_dams <- readRDS("Data/Clean_data/comp_dams.rds")

# parasitemia data set
comp_parasitemia <- readRDS("Data/Clean_data/comp_parasitemia_clean.rds")

# pb18s data set
comp_pb18s <- readRDS("Data/Clean_data/comp_pb18s_clean.rds")

# import models outputs
models_output <- readRDS("Code/Results/fig3_model_outputs.rds")

# import parasitemia model output
parasitemia_output <- readRDS("Code/Results/fig3_parasitemia_output.rds") 


# erithrocytes (RBC) ------------------------------------------------------

# get model output and p-value
model_rbc = models_output %>% 
  dplyr::filter(model_name == "erithrocytes") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]]

# get p-values for the substrain groups
comps_rbc <- model_rbc %>% 
  emmeans::emmeans(~ infection | genotype) %>% # Compara infecção PARA CADA genótipo
  emmeans::contrast("revpairwise") %>%        # "Infected - Non-infected"
  broom::tidy() %>%
  mutate(
    group1 = "no",  
    group2 = "yes",
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE),
    x_pos = as.numeric(factor(genotype, levels = c("bntac", "bj"))),
    xmin = x_pos - 0.2,
    xmax = x_pos + 0.2
  )

# get p-value for the interaction term
glm_rbc <- model_rbc %>% 
  broom::tidy() %>% 
  dplyr::filter(stringr::str_detect(term, ":")) %>% 
  dplyr::pull(p.value) %>% 
  data.frame(
    infection = "no",
    genotype = "bntac",
    label = paste0("Interaction: ", 
                   scales::pvalue(., accuracy = 0.001, add_p = TRUE))
  )

# create plot
ggplot2::ggplot(comp_dams, aes(x= genotype, y= rbc, fill= infection)) +
  stat_summary(fun.data = ggpubr::mean_sd, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= mean, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  ggbeeswarm::geom_beeswarm(shape= 21, size= 4.5, color= "black",
                dodge.width = 0.8, cex = 4) +
  labs(x= NULL, y= bquote("Red blood cells (x 10"^6*"/"*mu*"L)")) +
  scale_x_discrete(breaks= c("bntac", "bj"),
                   labels= c("C57BL/6NTac",
                             "C57BL/6J")) +
  scale_fill_manual(name= NULL,
                    breaks = c("no", "yes"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#005AB5", "#DC3220")) +
  scale_y_continuous(limits = c(2, 14), breaks = seq(2, 14, by= 3),
                     expand = c(0,0)) +
  theme_classic() +
  theme(legend.direction = "horizontal",
        legend.position = c(0.75, 0.975),
        legend.background = element_blank(),
        legend.text = element_text(size= 13),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 17),
        axis.text.x = element_text(size= 18)) +
  ggpubr::stat_pvalue_manual(
    comps_rbc, 
    xmin = "xmin",
    xmax = "xmax",
    label = "p_label", 
    y.position = c(11.7, 11.7),
    tip.length = 0.01,
    label.size = 5) +
  ggtext::geom_richtext(
    data = glm_rbc, 
    aes(label = label, y = 13.5),
    fill = NA,          
    label.color = NA,   
    size = 5,           
    hjust = 0.85           
  )
 

ggsave("Images/Plots/comp_rbc.png", width = 7, height = 5)



# hematocrity (HCT) -------------------------------------------------------

# get model output and p-value
model_hct = models_output %>% 
  dplyr::filter(model_name == "hematocrite") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]]

# get p-values for the substrain groups
comps_hct <- model_hct %>% 
  emmeans::emmeans(~ infection | genotype) %>% # Compara infecção PARA CADA genótipo
  emmeans::contrast("revpairwise") %>%        # "Infected - Non-infected"
  broom::tidy() %>%
  mutate(
    group1 = "no",  
    group2 = "yes",
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE),
    x_pos = as.numeric(factor(genotype, levels = c("bntac", "bj"))),
    xmin = x_pos - 0.2,
    xmax = x_pos + 0.2
  )

# get p-value for the interaction term
glm_hct <- model_hct %>% 
  broom::tidy() %>% 
  dplyr::filter(stringr::str_detect(term, ":")) %>% 
  dplyr::pull(p.value) %>% 
  data.frame(
    infection = "no",
    genotype = "bntac",
    label = paste0("Interaction: ", 
                   scales::pvalue(., accuracy = 0.001, add_p = TRUE))
  )

# create the plot
ggplot2::ggplot(comp_dams, aes(x= genotype, y= hct, fill= infection)) +
  stat_summary(fun.data = ggpubr::mean_sd, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= mean, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  ggbeeswarm::geom_beeswarm(shape= 21, size= 4.5, color= "black",
                dodge.width = 0.8, cex = 4, show.legend = FALSE) +
  labs(x= NULL, y= "Hematocrit (%)") +
  scale_x_discrete(breaks= c("bntac", "bj"),
                   labels= c("C57BL/6NTac",
                             "C57BL/6J")) +
  scale_fill_manual(name= NULL,
                    breaks = c("no", "yes"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#005AB5", "#DC3220")) +
  scale_y_continuous(limits = c(10, 70), breaks = seq(10, 70, by= 15),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 17),
        axis.text.x = element_text(size= 18)) +
  ggpubr::stat_pvalue_manual(
    comps_hct, 
    xmin = "xmin",
    xmax = "xmax",
    label = "p_label", 
    y.position = c(58, 58),
    tip.length = 0.01,
    label.size = 5) +
  ggtext::geom_richtext(
    data = glm_hct, 
    aes(label = label, y = 67),
    fill = NA,          
    label.color = NA,   
    size = 5,           
    hjust = 0.85           
  )


ggsave("Images/Plots/comp_hct.png", width = 7, height = 5)



# hemoglobin (HGB) --------------------------------------------------------

# get model output and p-value
model_hgb = models_output %>% 
  dplyr::filter(model_name == "hemoglobin") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]]

# get p-values for the substrain groups
comps_hgb <- model_hgb %>% 
  emmeans::emmeans(~ infection | genotype) %>% 
  emmeans::contrast("revpairwise") %>%        
  broom::tidy() %>%
  mutate(
    group1 = "no",  
    group2 = "yes",
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE),
    x_pos = as.numeric(factor(genotype, levels = c("bntac", "bj"))),
    xmin = x_pos - 0.2,
    xmax = x_pos + 0.2
  )

# get p-value for the interaction term
glm_hgb <- model_hgb %>% 
  broom::tidy() %>% 
  dplyr::filter(stringr::str_detect(term, ":")) %>% 
  dplyr::pull(p.value) %>% 
  data.frame(
    infection = "no",
    genotype = "bntac",
    label = paste0("Interaction: ", 
                   scales::pvalue(., accuracy = 0.001, add_p = TRUE))
  )

# create the plot
ggplot2::ggplot(comp_dams, aes(x= genotype, y= hgb, fill= infection)) +
  stat_summary(fun.data = ggpubr::mean_sd, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= mean, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  ggbeeswarm::geom_beeswarm(shape= 21, size= 4.5, color= "black",
                dodge.width = 0.8, cex = 4, show.legend = FALSE) +
  labs(x= NULL, y= "Hemoglobin (g/dL)") +
  scale_x_discrete(breaks= c("bntac", "bj"),
                   labels= c("C57BL/6NTac",
                             "C57BL/6J")) +
  scale_fill_manual(name= NULL,
                    breaks = c("no", "yes"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#005AB5", "#DC3220")) +
  scale_y_continuous(limits = c(3, 23), breaks = seq(3, 23, by= 5),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size= 17),
        axis.text.x = element_text(size= 18)) +
  ggpubr::stat_pvalue_manual(
    comps_hgb, 
    xmin = "xmin",
    xmax = "xmax",
    label = "p_label", 
    y.position = c(19, 19),
    tip.length = 0.01,
    label.size = 5) +
  ggtext::geom_richtext(
    data = glm_hgb, 
    aes(label = label, y = 22),
    fill = NA,          
    label.color = NA,   
    size = 5,           
    hjust = 0.85           
  )


ggsave("Images/Plots/comp_hgb.png", width = 7, height = 5)



# peripheral parasitemia --------------------------------------------------

# get p-values for the substrain groups
comps_parasitemia <- parasitemia_output %>% 
  emmeans::emmeans(~ preg | genotype) %>% 
  emmeans::contrast("revpairwise") %>%        
  broom::tidy() %>%
  mutate(
    group1 = "no",  
    group2 = "yes",
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE),
    x_pos = as.numeric(factor(genotype, levels = c("bnt", "bj"))),
    xmin = x_pos - 0.2,
    xmax = x_pos + 0.2
  )

# get p-value for the interaction term
glm_parasitemia <- parasitemia_output %>% 
  broom::tidy() %>% 
  dplyr::filter(stringr::str_detect(term, ":")) %>% 
  dplyr::pull(p.value) %>% 
  data.frame(
    preg = "no",
    genotype = "bnt",
    label = paste0("Interaction: ", 
                   scales::pvalue(., accuracy = 0.001, add_p = TRUE))
  )

# create the plot
ggplot2::ggplot(comp_parasitemia, aes(x= genotype, y= parasitemia, fill= preg)) +
  stat_summary(fun.data = ggpubr::mean_sd, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= mean, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  ggbeeswarm::geom_beeswarm(shape= 21, size= 4, color= "black",
                            dodge.width = 0.8, cex = 4) +
  labs(x= NULL, y= "Peripheral parasitemia (%)") +
  scale_x_discrete(breaks= c("bnt", "bj"),
                   labels= c("C57BL/6NTac",
                             "C57BL/6J")) +
  scale_fill_manual(name= NULL,
                    breaks = c("no", "yes"),
                    values= c("#ffffff", "#DC3220"),
                    labels= c("Non-pregnant", "Pregnant"))+
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by= 3),
                     expand = c(0,0)) +
  theme_classic() +
  theme(legend.direction = "horizontal",
        legend.position = c(0.25, 0.975),
        legend.background = element_blank(),
        legend.text = element_text(size= 13),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size= 17)) +
  ggpubr::stat_pvalue_manual(
    comps_parasitemia, 
    xmin = "xmin",
    xmax = "xmax",
    label = "p_label", 
    y.position = c(8.3, 11),
    tip.length = 0.01,
    label.size = 5) +
  ggtext::geom_richtext(
    data = glm_parasitemia, 
    aes(label = label, y = 10.5),
    fill = NA,          
    label.color = NA,   
    size = 5,           
    hjust = 0.85           
  )


ggsave("Images/Plots/comp_parasitemia.png", width = 7, height = 5)



# pb18s relative expression -----------------------------------------------

# get model output and p-value
glm_pb18s = models_output %>% 
  dplyr::filter(model_name == "placental parasitemia") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]] %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(
    group1 = "bntac",
    group2 = str_remove(term, "genotype"),
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  )       

# create the plot
ggplot2::ggplot(comp_pb18s, aes(x= genotype, y= pb18s, fill= genotype)) +
  stat_summary(fun= mean, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") +
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, size = 4.5,
                               shape= 21, color= "black") +
  labs(x= NULL, y= bquote("pb18s mRNA (" * 2^{-Delta * Delta * Ct} * ")")) +
  scale_fill_manual(values= c("#cccccc", "#636363")) +
  scale_x_discrete(breaks= c("bntac", "bj"),
                   labels= c("C57BL/6NTac",
                             "C57BL/6J")) +
  scale_y_log10() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 17),
        axis.text.x = element_text(size= 18)) +
  ggpubr::stat_pvalue_manual(
    glm_pb18s, 
    label = "p_label", 
    y.position = c(3),
    tip.length = 0.01,
    label.size = 5)

ggsave("Images/Plots/comp_pb18s.png", width= 6, height= 5)
