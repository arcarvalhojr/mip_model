### Figure plots of data related to sup figure 2 ###


# Import libraries --------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(broom)
library(ggpubr)
library(emmeans)

# import clean data -------------------------------------------------------

# dams data set
comp_dams <- readRDS("Data/Clean_data/comp_dams.rds")

# leucocytes data set
comp_leucocytes <- readRDS("Data/Clean_data/comp_leucocyte_clean.rds")

# import models outputs
models_output <- readRDS("Code/Results/supfig2_model_outputs.rds")



# pre-term delivery (PTD) -------------------------------------------------


# create contingency table
ptd_contingency_tbl <- comp_dams %>%
  dplyr::group_by(infection, genotype, ptd) %>%
  dplyr::summarise(count= n()) %>%
  dplyr::mutate(percent= (count / sum(count)) *100) %>%
  dplyr::mutate(text_label = ifelse(ptd == "yes",
                             paste0(count, "/", sum(count)), "")) %>%
  dplyr::select(genotype, infection, ptd, percent, text_label)

# create plot
ggplot2::ggplot(
  ptd_contingency_tbl, aes(x= infection, y= percent, fill= ptd)) +
  geom_col(width= 0.8, color= "black") +
  facet_wrap(~ genotype, labeller = as_labeller(c(
    bntac= "C57BL/6NTac", bj= "C57BL/6J"))) +
  labs(x= "Infection", y= "Percentage of the fetus (%)") +
  scale_x_discrete(breaks= c("no", "yes"),
                   labels= c("No", "Yes")) +
  scale_fill_manual(name= NULL,
                    breaks = c("no", "yes"),
                    values= c("#ffffff", "black"),
                    labels= c("C-section", "Pre-term delivery")) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 105, by= 20),
                     expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size= 16),
        legend.position = "top",
        legend.text = element_text(size= 13),
        axis.title = element_text(size= 20),
        axis.text.y = element_text(size= 17),
        axis.text.x = element_text(size= 18)) +
  geom_text(aes(label = text_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 5)

ggsave("Images/Plots/comp_ptd.png", width = 7, height = 5)




# spleen weight -----------------------------------------------------------

# get model output and p-value
model_sw = models_output %>% 
  dplyr::filter(model_name == "spleen weight") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]]

# get p-values for the substrain groups
comps_sw <- model_sw %>% 
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
glm_sw <- model_sw %>% 
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
ggplot2::ggplot(comp_dams, aes(x= genotype, y= spleen_w, fill= infection)) +
  stat_summary(fun.data = ggpubr::mean_sd, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= mean, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  ggbeeswarm::geom_beeswarm(shape= 21, size= 4.5, color= "black",
                            dodge.width = 0.8, cex = 4) +
  labs(x= NULL, y= "Spleen weight (mg)") +
  scale_x_discrete(breaks= c("bntac", "bj"),
                   labels= c("C57BL/6NTac",
                             "C57BL/6J")) +
  scale_fill_manual(name= NULL,
                    breaks = c("no", "yes"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#005AB5", "#DC3220")) +
  scale_y_continuous(limits = c(50, 450), breaks = seq(50, 450, by= 100),
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
    comps_sw, 
    xmin = "xmin",
    xmax = "xmax",
    label = "p_label", 
    y.position = c(340, 340),
    tip.length = 0.01,
    label.size = 5) +
  ggtext::geom_richtext(
    data = glm_sw, 
    aes(label = label, y = 440),
    fill = NA,          
    label.color = NA,   
    size = 5,           
    hjust = 0.85           
  )


ggsave("Images/Plots/comp_sw.png", width = 7, height = 5)



# leucocytes --------------------------------------------------------------

# get model output and p-value
model_wbc = models_output %>% 
  dplyr::filter(model_name == "leucocytes") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]]

# get p-values for the substrain groups
comps_wbc <- model_wbc %>% 
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
glm_wbc <- model_wbc %>% 
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
ggplot2::ggplot(comp_leucocytes, aes(x= genotype, y= wbc, fill= infection)) +
  stat_summary(fun.data = ggpubr::mean_sd, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= mean, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  ggbeeswarm::geom_beeswarm(shape= 21, size= 4.5, color= "black",
                            dodge.width = 0.8, cex = 4, show.legend = FALSE) +
  labs(x= NULL, y= bquote("Leucocytes (x 10"^3*"/"*mu*"L)")) +
  scale_x_discrete(breaks= c("bntac", "bj"),
                   labels= c("C57BL/6NTac",
                             "C57BL/6J")) +
  scale_fill_manual(name= NULL,
                    breaks = c("no", "yes"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#005AB5", "#DC3220")) +
  scale_y_continuous(limits = c(0.5, 6.5), breaks = seq(0.5, 6.5, by= 2),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 17),
        axis.text.x = element_text(size= 18)) +
  ggpubr::stat_pvalue_manual(
    comps_wbc, 
    xmin = "xmin",
    xmax = "xmax",
    label = "p_label", 
    y.position = c(4.5, 4.5),
    tip.length = 0.01,
    label.size = 5) +
  ggtext::geom_richtext(
    data = glm_wbc, 
    aes(label = label, y = 6),
    fill = NA,          
    label.color = NA,   
    size = 5,           
    hjust = 0.85           
  )


ggsave("Images/Plots/comp_wbc.png", width = 7, height = 5)



# lin, gran and monocytes -------------------------------------------------

# leucocitos data set long
cells_long <- comp_leucocytes %>%
  dplyr::select(genotype, mice_id, infection, lymph, gran, mon) %>%
  tidyr::pivot_longer(cols = c(lymph, gran, mon),
               names_to = c("cells"),
               values_to = c("values")) %>%
  dplyr::filter(!is.na(values)) %>%
  dplyr::mutate(cells = factor(cells,
                        levels = c("lymph", "gran", "mon")))


models_cells <- models_output %>%
  dplyr::filter(model_name %in% c("lymphocytes",
                                  "granulocytes",
                                  "monocytes"))

stats_cells <- models_cells %>%
  dplyr::mutate(
    cells = dplyr::case_when(
      model_name == "lymphocytes" ~ "lymph",
      model_name == "granulocytes" ~ "gran",
      model_name == "monocytes" ~ "mon"
    ),
    stats = purrr::map(
      model_output,
      ~ emmeans::emmeans(.x, ~ infection | genotype) %>%
        emmeans::contrast("revpairwise") %>%
        broom::tidy()
    )
  ) %>%
  tidyr::unnest(stats) %>%
  mutate(
    group1 = "no",
    group2 = "yes",
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  ) %>% 
  left_join(
    cells_long %>%
      group_by(cells) %>%
      summarise(y.position = max(values, na.rm = TRUE) * 1),
    by = "cells"
  )

stats_cells <- stats_cells %>%
  mutate(
    x_pos = as.numeric(factor(genotype, levels = c("bntac", "bj"))),
    xmin = x_pos - 0.2,
    xmax = x_pos + 0.2
  )


# labels
cells_labels <- c(
  "lymph" = "Lymphocytes", 
  "gran" = "Granulocytes", 
  "mon" = "Monocytes"
)


ggplot2::ggplot(cells_long, aes(x= genotype, y= values, fill= infection)) +
  stat_summary(fun.data = ggpubr::mean_sd, geom = "errorbar", color= "black", 
               width= 0.3, position = position_dodge(0.8),
               show.legend = FALSE) +
  stat_summary(fun= mean, show.legend = FALSE, geom = "crossbar", 
               width= 0.5, color= "black", position = position_dodge(0.8)) +
  geom_quasirandom(show.legend = FALSE, shape= 21, size= 4, color= "black",
                   dodge.width = 0.8) +
  facet_wrap(~cells, scales = "free_y", labeller = as_labeller(cells_labels)) +
  labs(x= NULL, y= "Cells (%)") +
  scale_fill_manual(name = NULL,
                    breaks = c("no", "yes"),
                    values = c("#005AB5", "#DC3220")) +
  scale_x_discrete(breaks= c("bntac", "bj"),
                   labels= c("C57BL/6NTac",
                             "C57BL/6J")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 20),
    axis.text = element_text(size= 18),
    strip.text = element_markdown(size = 16)) +
  ggpubr::stat_pvalue_manual(
    stats_cells,
    label = "p_label",
    xmin = "xmin",
    xmax = "xmax",
    y.position = "y.position",
    tip.length = 0.01,
    label.size = 5
  )


ggsave("Images/Plots/comp_leucocytes.png", width = 16, height = 4)
