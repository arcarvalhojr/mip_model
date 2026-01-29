### Figure plots of data related to figure 2 ###


# Import libraries --------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(broom.mixed)
library(ggpubr)

# import clean data -------------------------------------------------------

# fetus data set
model_weights <- readRDS("Data/Clean_data/model_weights.rds")

# fgr data set
model_fgr <- readRDS("Data/Clean_data/model_fgr.rds")


# import models outputs
models_output <- readRDS("Code/Results/fig2_model_outputs.rds")


# fetal weight ------------------------------------------------------------

# get model output and p-value
lmm_fw = models_output %>% 
  dplyr::filter(model_name == "fetal weight") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]] %>%
  broom.mixed::tidy(effects = "fixed") %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    group1 = "pbnk65-",
    group2 = stringr::str_remove(term, "infection"),
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  ) %>% 
  dplyr::filter(stringr::str_detect(term, "infection"))

# create the plot
ggplot2::ggplot(
  model_weights, aes(x= infection, y= fetal_weight, fill= infection)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width= 0.6) + 
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, size = 2.5) +
  labs(x= NULL, y= "Fetal weight (g)") +
  scale_fill_manual(values= c("#636363", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(limits= c(0.4, 1.6), breaks= seq(0.4, 1.6, by= 0.3),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) +
  ggpubr::stat_pvalue_manual(
    lmm_fw, 
    label = "p_label", 
    y.position = c(1.37, 1.45, 1.53),
    tip.length = 0.01,
    label.size = 5)

ggsave("Images/Plots/model_fw.png", width= 7, height= 5)


# placental weight --------------------------------------------------------

# get model output and p-value
lmm_pw = models_output %>% 
  dplyr::filter(model_name == "placental weight") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]] %>%
  broom.mixed::tidy(effects = "fixed") %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    group1 = "pbnk65-",
    group2 = stringr::str_remove(term, "infection"),
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  )

# create plot
ggplot2::ggplot(
  model_weights, aes(x= infection, y= placenta_weight, fill= infection)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width= 0.6) + 
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, size = 2.5) +
  labs(x= NULL, y= "Placental weight (mg)") +
  scale_fill_manual(values= c("#cccccc", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(limits= c(20, 170), breaks= seq(20, 170, by= 30),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) +
  ggpubr::stat_pvalue_manual(
    lmm_pw, 
    label = "p_label", 
    y.position = c(140, 150, 160),
    tip.length = 0.01,
    label.size = 5)

ggsave("Images/Plots/model_pw.png", width= 7, height= 5)


# fw/pw ratio -------------------------------------------------------------

# get model output and p-value
lmm_ratio = models_output %>% 
  dplyr::filter(model_name == "fw/pw ratio") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]] %>%
  broom.mixed::tidy(effects = "fixed") %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    group1 = "pbnk65-",
    group2 = stringr::str_remove(term, "infection"),
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  )

# create the plot
ggplot2::ggplot(
  model_weights, aes(x= infection, y= ratio, fill= infection)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width= 0.6) +
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, size = 2.5) +
  labs(x= NULL, y= "Fw/Pw ratio (g/g)") +
  scale_fill_manual(values= c("#636363", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(limits= c(5, 25), breaks= seq(5, 25, by= 5),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) +
  ggpubr::stat_pvalue_manual(
    lmm_ratio, 
    label = "p_label", 
    y.position = c(20, 22, 24),
    tip.length = 0.01,
    label.size = 5)

ggsave("Images/Plots/model_ratio.png", width= 7, height= 5)


# fetal growth restriction (FGR) ------------------------------------------

# get model output and p-value
model_fgr = models_output %>% 
  dplyr::filter(model_name == "fgr") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]]

# get predicted probabilities
emm_fgr <- emmeans::emmeans(
  model_fgr,
  ~ infection,
  type = "response"
)

# make a df
emm_fgr_df <- as.data.frame(emm_fgr) %>% 
  mutate(
    infection = factor(infection, levels = c("pbnk65-", "pbnk65+",
                                             "pbnk65++", "pbnk65+++"))
  )

# get model output and p-value
glmm_fgr <- model_fgr %>% 
  broom.mixed::tidy(effects = "fixed") %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    group1 = "pbnk65-",
    group2 = stringr::str_remove(term, "infection"),
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  )

#  dplyr::filter(term == "infectionpbnk65+++")


# create predicted probability  plot
ggplot(emm_fgr_df, aes(x = infection, y = prob, color = infection)) +
  geom_point(position = position_dodge(width = 0.8), size = 4.5,
             show.legend = FALSE) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.8), width = 0.2,
                size = 1, show.legend = FALSE) +
  scale_color_manual(values = c("#636363", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(x = NULL, y = "Predicted probability") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 17),
        axis.text.x = element_text(size= 18)) +
  ggpubr::stat_pvalue_manual(
    glmm_fgr, 
    label = "p_label", 
    y.position = c(0.3, 0.55, 0.8),
    tip.length = 0.01,
    label.size = 5)

ggsave("Images/Plots/model_proba_fgr.png", width= 10, height= 7)


# create fetal weights density plot
ggplot2::ggplot(model_fgr, aes(x = fetal_weight, color= infection)) +
  annotate("rect", xmin = -Inf, xmax = model_fgr$percentile_5, 
           ymin = 0, ymax = Inf, fill = "grey90", alpha = 0.5) +
  annotate("segment", x = 0.55, xend = 0.6, y = 3.5, yend = 3.5, 
           linetype = "dashed", color = "black", size = 0.8) +
  annotate("text", x = 0.62, y = 3.5, label = "5th percentile", 
           color = "black", hjust = 0, size = 5) +
  geom_density(linewidth = 1) +
  labs(x = "Fetal weight (g)", y = "Density") +
  scale_color_manual(name = NULL,
                    values = c("#636363", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(limits = c(0.5, 1.5), breaks = seq(0.5, 1.5, by = 0.2),
                     expand = c(0.02, 0)) +
  geom_vline(xintercept = model_fgr$percentile_5,
             linetype = "dashed", size = 0.8) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.text = element_text(size= 13),
        legend.position = c(0.975, 0.975),
        legend.justification = c(1, 1), 
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16))


ggsave("Images/Plots/density_fw.png", width= 12, height= 7)


# create stacked bar plot

# # create contingency table
# fgr_contingency_tbl <- model_fgr %>%
#   dplyr::filter(infection %in% c("pbnk65-", "pbnk65+++")) %>% 
#   dplyr::group_by(infection, percentile) %>%
#   dplyr::summarise(count= n()) %>%
#   dplyr::mutate(percent= (count / sum(count)) *100) %>%
#   dplyr::mutate(text_label = ifelse(percentile == "below_range",
#                              paste0(count, "/", sum(count)), "")) %>%
#   dplyr::mutate(percentile = factor(percentile, levels = c("below_range", "within_range"))) %>% 
#   dplyr::select(infection, percentile, percent, text_label)
# 
# 
# # create plot
# 
# # create a df with the p-value text label 
# fgr_stat <- data.frame(
#   infection = "pbnk65+++",  
#   label = "p=0.028"
# )
# 
# ggplot2::ggplot(
#   fgr_contingency_tbl, aes(x= infection, y= percent, fill= percentile)) +
#   geom_col(width= 0.9, color= "black") +
#   labs(x= NULL, y= "Percentage of the fetus (%)") +
#   scale_fill_manual(name= NULL,
#                     breaks = c("below_range", "within_range"),
#                     values= c("black", "#ffffff"),
#                     labels= c("Below range", "Within range")) +
#   scale_y_continuous(limits = c(0, 105), breaks = seq(0, 105, by= 20),
#                      expand = c(0,0)) +
#   theme_classic()+
#   theme(legend.text = element_text(size= 13),
#         axis.title.y = element_text(size= 20),
#         axis.text.y = element_text(size= 16),
#         axis.text.x = element_text(size= 16)) +
#   geom_text(aes(label = text_label),
#             position = position_stack(vjust = 0.5),
#             color = "white",
#             size = 5) +
#   ggtext::geom_richtext(
#     data = fgr_stat, 
#     aes(label = label, y = 103),
#     fill = NA,          
#     label.color = NA,   
#     size = 5,           
#     hjust = 0.5           
#   )
#   
# ggsave("Images/Plots/stacked_fw.png", width= 7, height= 7)


# fetal viability ---------------------------------------------------------

# get model output and p-value
glmm_stb = models_output %>% 
  dplyr::filter(model_name == "fetal viability") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]] %>%
  broom.mixed::tidy(effects = "fixed") %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    group1 = "pbnk65-",
    group2 = stringr::str_remove(term, "infection"),
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  ) %>% 
  dplyr::filter(stringr::str_detect(term, "infection"))

# create contingency table
stb_contingency_tbl <- model_weights %>%
  group_by(infection, alive) %>%
  summarise(count= n()) %>%
  mutate(percent= (count / sum(count)) *100) %>%
  mutate(text_label = ifelse(alive == "no",
                             paste0(count, "/", sum(count)), "")) %>%
  select(infection, alive, percent, text_label)


# create plot

# create a df with the p-value text label 
stb_stat <- data.frame(
  infection = c("pbnk65+", "pbnk65++", "pbnk65+++"),  
  label = c("p=0.648", "p=0.506", "p=0.061")
)

ggplot2::ggplot(stb_contingency_tbl, aes(x= infection, y= percent, fill= alive)) +
  geom_col(width= 0.9, color= "black") +
  labs(x= NULL, y= "Percentage of the fetus (%)") +
  scale_fill_manual(name= NULL,
                    breaks = c("no", "yes"),
                    values= c("black", "#ffffff"),
                    labels= c("Non-viable", "Viable")) +
  scale_y_continuous(limits = c(0, 100.5), breaks = seq(0, 100.5, by= 20),
                     expand = c(0,0)) +
  theme_classic()+
  theme(legend.position = "top",
        legend.text = element_text(size= 13),
        axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) +
  geom_text(aes(label = text_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 5) 

  # ggtext::geom_richtext(
  #   data = stb_stat, 
  #   aes(label = label, y = 103),
  #   fill = NA,          
  #   label.color = NA,   
  #   size = 5,           
  #   hjust = 0.5           
  # )

ggsave("Images/Plots/model_stb.png", width= 7, height= 5)


# placental vascular space ------------------------------------------------

# get model output and p-value
lmm_vasc_space = models_output %>% 
  dplyr::filter(model_name == "vascular space") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]] %>%
  broom.mixed::tidy(effects = "fixed") %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    group1 = "pbnk65-",
    group2 = stringr::str_remove(term, "infection"),
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  )

# create plot
ggplot2::ggplot(
  model_weights, aes(x= infection, y= vascular_space, fill= infection)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width= 0.6) + 
  geom_quasirandom(show.legend = FALSE, size = 2.5) +
  labs(x= NULL, y= "Placental vascular space (%)") +
  scale_fill_manual(values= c("#636363", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(limits= c(25, 65), breaks= seq(25, 65, by= 10),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) +
  ggpubr::stat_pvalue_manual(
    lmm_vasc_space, 
    label = "p_label", 
    y.position = c(55, 58, 61),
    tip.length = 0.01,
    label.size = 5)

ggsave("Images/Plots/model_vasc_space.png", width= 7, height= 5)

