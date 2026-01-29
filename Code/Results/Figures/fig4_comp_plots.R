### Figure plots of data related to figure 4 ###


# Import libraries --------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(broom)
library(ggpubr)
library(emmeans)

# import clean data -------------------------------------------------------

# fetus data set
comp_weights <- readRDS("Data/Clean_data/comp_weights.rds")

# fetus weight data set
comp_fw <- readRDS("Data/Clean_data/comp_fw_clean.rds")

# placenta weight data set
comp_pw <- readRDS("Data/Clean_data/comp_pw_clean.rds")

# fw/pw ratio data set
comp_ratio <- readRDS("Data/Clean_data/comp_ratio_clean.rds")

# fgr data set
comp_fgr <- readRDS("Data/Clean_data/comp_fgr_clean.rds")


# import models outputs
models_output <- readRDS("Code/Results/fig4_model_outputs.rds")
 


# fetal weight ------------------------------------------------------------

# get model output and p-value
model_fw = models_output %>% 
  dplyr::filter(model_name == "fetal weight") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]]

# get p-values for the substrain groups
comps_fw <- model_fw %>% 
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
lmm_fw <- model_fw %>% 
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
ggplot2::ggplot(comp_fw, aes(x= genotype, y= fetal_weight, fill= infection)) +
  geom_boxplot(outlier.shape = NA, width= 0.8) +
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, size = 1.5,
                               dodge.width = 0.8) +
  labs(x= NULL, y= "Fetal weight (g)") +
  scale_x_discrete(breaks= c("bntac", "bj"),
                   labels= c("C57BL/6NTac",
                             "C57BL/6J")) +
  scale_fill_manual(name= NULL,
                    breaks = c("no", "yes"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#005AB5", "#DC3220")) +
  scale_y_continuous(limits = c(0.7, 1.7), breaks = seq(0.7, 1.7, by= 0.2),
                     expand = c(0,0)) +
  theme_classic() +
  theme(legend.direction = "horizontal",
        legend.position = c(0.8, 0.97),
        legend.text = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 17)) +
  ggpubr::stat_pvalue_manual(
    comps_fw, 
    xmin = "xmin",
    xmax = "xmax",
    label = "p_label", 
    y.position = c(1.54, 1.35),
    tip.length = 0.01,
    label.size = 5) +
  ggtext::geom_richtext(
    data = lmm_fw, 
    aes(label = label, y = 1.66),
    fill = NA,          
    label.color = NA,   
    size = 5,           
    hjust = 0.85           
  )


ggsave("Images/Plots/comp_fw.png", width = 7, height = 5)


# placental weight --------------------------------------------------------

# get model output and p-value
model_pw = models_output %>% 
  dplyr::filter(model_name == "placental weight") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]]

# get p-values for the substrain groups
comps_pw <- model_pw %>% 
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
lmm_pw <- model_pw %>% 
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
ggplot2::ggplot(comp_pw, aes(x= genotype, y= placenta_weight, fill= infection)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width= 0.8) +
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, size = 1.5,
                               dodge.width = 0.8, cex = 4) +
  labs(x= NULL, y= "Placental weight (mg)") +
  scale_x_discrete(breaks= c("bntac", "bj"),
                   labels= c("C57BL/6NTac",
                             "C57BL/6J")) +
  scale_fill_manual(name= NULL,
                    breaks = c("no", "yes"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#005AB5", "#DC3220"))+
  scale_y_continuous(limits = c(25, 225), breaks = seq(25, 225, by= 50),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 17),
        axis.text.x = element_text(size = 18)) +
  ggpubr::stat_pvalue_manual(
    comps_pw, 
    xmin = "xmin",
    xmax = "xmax",
    label = "p_label", 
    y.position = c(190, 190),
    tip.length = 0.01,
    label.size = 5) +
  ggtext::geom_richtext(
    data = lmm_pw, 
    aes(label = label, y = 220),
    fill = NA,          
    label.color = NA,   
    size = 5,           
    hjust = 0.85           
  )


ggsave("Images/Plots/comp_pw.png", width = 7, height = 5)



# fw/pw ratio -------------------------------------------------------------

# get model output and p-value
model_ratio = models_output %>% 
  dplyr::filter(model_name == "fw/pw ratio") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]]

# get p-values for the substrain groups
comps_ratio <- model_ratio %>% 
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
lmm_ratio <- model_ratio %>% 
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
ggplot2::ggplot(comp_ratio, aes(x= genotype, y= ratio, fill= infection)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width= 0.8) +
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, size = 1.5,
                               dodge.width = 0.8) +
  labs(x= NULL, y= "Fw/Pw ratio (g/g)") +
  scale_x_discrete(breaks= c("bntac", "bj"),
                   labels= c("C57BL/6NTac",
                             "C57BL/6J")) +
  scale_fill_manual(name= NULL,
                    breaks = c("no", "yes"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#005AB5", "#DC3220")) +
  scale_y_continuous(limits = c(4, 20), breaks = seq(4, 20, by= 4),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 17),
        axis.text.x = element_text(size = 18)) +
  ggpubr::stat_pvalue_manual(
    comps_ratio, 
    xmin = "xmin",
    xmax = "xmax",
    label = "p_label", 
    y.position = c(16, 16),
    tip.length = 0.01,
    label.size = 5) +
  ggtext::geom_richtext(
    data = lmm_ratio, 
    aes(label = label, y = 19),
    fill = NA,          
    label.color = NA,   
    size = 5,           
    hjust = 0.85           
  )


ggsave("Images/Plots/comp_ratio.png", width = 7, height = 5)



# fetal growth restriction (FGR) ------------------------------------------

# get model output 
model_fgr = models_output %>% 
  dplyr::filter(model_name == "fgr") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]]

# create OR table
or_data <- broom.mixed::tidy(model_fgr, conf.int = TRUE, exponentiate = TRUE) %>%
  dplyr::filter(effect == "fixed", term != "(Intercept)") %>%
  dplyr::mutate(
    term = factor(term, levels = c("infectionyes:genotypebj",
                                   "genotypebj",
                                   "infectionyes")),
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  )


# create forest plot
ggplot2::ggplot(or_data, aes(x = estimate, y = term, color = term)) +
  geom_point(size = 3.5, show.legend = FALSE) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.15, size = 1, show.legend = FALSE
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  scale_x_log10(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000),
    labels = c("0.0001", "0.001", "0.01", "0.1", "1", "10", "100", "1000")
  ) +
  scale_y_discrete(breaks = c("infectionyes",
                              "genotypebj",
                              "infectionyes:genotypebj"),
                   labels = c("BNTac inf vs BJ inf",
                              "BNTac vs BJ",
                              "Interaction (Inf x Gen)")) +
  scale_color_manual(values = c("#DC3220", "#005AB5", "#005AB5")) +
  labs(
    x = "Odds ratio (Log scale)",
    y = NULL
  ) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size= 17),
        axis.text.x = element_text(size= 17)) +
  geom_text(aes(label = p_label, x = estimate * 3),
            vjust = 1.5,
            size = 6,
            show.legend = FALSE
  ) +
  geom_text(aes(x = estimate, label = round(estimate, 2)),
    position = position_nudge(y = 0.175),
    size = 6,
    show.legend = FALSE
  )


ggsave("Images/Plots/or_fgr.png", width= 12, height= 7)


# create predicted probabilitie plot

# get predicted probabilities
emm_fgr <- emmeans::emmeans(
  model_fgr,
  ~ infection * genotype,
  type = "response"
)

# make a df
emm_fgr_df <- as.data.frame(emm_fgr) %>% 
  mutate(
    infection = factor(infection, levels = c("no", "yes")),
    genotype  = factor(genotype, levels = c("bntac", "bj"))
  )

# create plot
ggplot(emm_fgr_df, aes(x = infection, y = prob, color = infection)) +
  geom_point(position = position_dodge(width = 0.8), size = 4.5,
             show.legend = FALSE) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
    position = position_dodge(width = 0.8), width = 0.2,
    size = 1, show.legend = FALSE) +
  facet_wrap(~ genotype, labeller = as_labeller(c("bntac"= "C57BL/6NTac",
                                                  "bj"= "C57BL/6J"))) +
  scale_x_discrete(breaks= c("no", "yes"),
                   labels= c("No", "Yes")) +
  scale_color_manual(values = c("#005AB5", "#DC3220")) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(x = "Infection", y = "Predicted probability") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size= 16),
        axis.title = element_text(size = 20),
        axis.text.y = element_text(size= 17),
        axis.text.x = element_text(size= 18))


ggsave("Images/Plots/comp_proba_fgr.png", width= 8, height= 6)

# text_fgr <- data.frame(
#   genotype = "bntac", 
#   x_seg = 0.7, x_end = 0.75, 
#   x_text = 0.78, y_pos = 5, 
#   label = "10th percentile"
# ) %>% 
#   dplyr::mutate(genotype = factor(genotype, levels = c("bntac", "bj")))
# 
# # create fetal weights density plot
# ggplot2::ggplot(comp_fgr, aes(x = fetal_weight, color = infection, fill = infection, alpha = infection)) +
#   geom_density(linewidth = 1, show.legend = FALSE) +
#   facet_wrap(~ genotype, scales = "free",
#              labeller = as_labeller(c("bntac"= "C57BL/6NTac",
#                                       "bj"= "C57BL/6J"))) +
#   geom_segment(data = text_fgr, aes(x = x_seg, xend = x_end, y = y_pos, yend = y_pos),
#                linetype = "dashed", color = "black", linewidth = 0.6, inherit.aes = FALSE) +
#   geom_text(data = text_fgr, aes(x = x_text, y = y_pos, label = label),
#             color = "black", hjust = 0, size = 5, inherit.aes = FALSE) +
#   labs(x = "Fetal weight (g)", y = "Density") +
#   scale_color_manual(name = NULL,
#                     breaks = c("no", "yes"),
#                     values = c("#005AB5", "#DC3220"),
#                     labels = c("Non-infected", "Infected")) +
#   scale_fill_manual(breaks = c("no", "yes"),
#                     values = c("#005AB5", "#DC3220")) +
#   scale_alpha_manual(values = c("no" = 0.15, "yes" = 0.7), guide = "none") +
#   scale_y_continuous(expand = c(0.025,0)) +
#   scale_x_continuous(expand = c(0.05, 0)) +
#   geom_vline(data = comp_fgr, 
#              aes(xintercept = percentile_10),
#              linetype = "dashed", linewidth = 0.8) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         #legend.text = element_text(size= 12),
#         #legend.position = c(0.1025, 0.9),
#         strip.text = element_text(size= 16),
#         axis.title = element_text(size = 20),
#         axis.text = element_text(size = 18))
#   
# 
# 
# ggsave("Images/Plots/density_fw.png", width= 12, height= 7)


# # create stacked bar plot
# 
# # create contingency table
# fgr_contingency_tbl <- comp_fgr %>%
#   dplyr::group_by(genotype, infection, percentile) %>%
#   dplyr::summarise(count= n()) %>%
#   dplyr::mutate(percent= (count / sum(count)) *100) %>%
#   dplyr::mutate(text_label = ifelse(percentile == "below_range",
#                              paste0(count, "/", sum(count)), "")) %>%
#   dplyr::mutate(percentile = factor(percentile, 
#                                     levels = c("below_range", 
#                                                "within_range"))) %>% 
#   dplyr::select(genotype, infection, percentile, percent, text_label)
# 
# # get model output and p-value
# model_fgr = models_output %>% 
#   dplyr::filter(model_name == "fgr") %>% 
#   dplyr::pull(model_output) %>% 
#   .[[1]]
# 
# # get p-values for the substrain groups
# comps_fgr <- model_fgr %>% 
#   emmeans::emmeans(~ infection | genotype) %>% 
#   emmeans::contrast("revpairwise") %>%        
#   broom::tidy() %>%
#   mutate(
#     p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE),
#   )
# 
# fgr_p_labels <- comps_fgr %>% 
#   dplyr::mutate(
#     infection = "yes",
#   ) %>% 
#   dplyr::mutate(genotype = factor(genotype, levels = c("bntac", "bj"))) %>% 
#   dplyr::select(genotype, infection, p_label)
# 
# # get p-value for the interaction term
# glmm_fgr <- model_fgr %>% 
#   broom::tidy() %>% 
#   dplyr::filter(stringr::str_detect(term, ":")) %>% 
#   dplyr::pull(p.value) 
# 
# fgr_interaction <- paste0(
#   "Interaction: ", scales::pvalue(glmm_fgr, accuracy = 0.001, add_p = TRUE))
# 
# 
# # create plot
# fgr_plot <- ggplot2::ggplot(
#   fgr_contingency_tbl, aes(x= infection, y= percent, fill= percentile)) +
#   geom_col(width= 0.8, color= "black") +
#   facet_wrap(~ genotype,
#              labeller = as_labeller(c("bntac"= "C57BL/6NTac",
#                                       "bj"= "C57BL/6J"))) +
#   labs(x= "Infection", y= "Percentage of the fetus (%)") +
#   scale_fill_manual(name= NULL,
#                     breaks = c("below_range", "within_range"),
#                     values= c("black", "#ffffff"),
#                     labels= c("Below range", "Within range")) +
#   scale_x_discrete(breaks= c("no", "yes"),
#                    labels= c("No", "Yes")) +
#   scale_y_continuous(limits = c(0, 110), breaks = seq(0, 110, by= 20),
#                      expand = c(0,0)) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         strip.text = element_text(size= 16),
#         legend.position = "top",
#         legend.text = element_text(size= 13),
#         axis.title = element_text(size= 20),
#         axis.text.y = element_text(size= 17),
#         axis.text.x = element_text(size= 18)) +
#   geom_text(aes(label = text_label),
#             position = position_stack(vjust = 0.5),
#             color = "white",
#             size = 5) +
#   geom_text(data = fgr_p_labels, 
#             aes(x = infection, y = 103, label = p_label),
#             inherit.aes = FALSE, 
#             size = 5, vjust = 0)
# 
# fgr_plot <- cowplot::ggdraw(fgr_plot) +
#   cowplot::draw_label(fgr_interaction, x = 0.25, y = 0.9, size = 13)
# 
# 
# ggsave("Images/Plots/comp_stacked_fw.png", fgr_plot, width= 7, height= 5)



# fetal viability ---------------------------------------------------------

# get model output and p-value
model_stb = models_output %>% 
  dplyr::filter(model_name == "fetal viability") %>% 
  dplyr::pull(model_output) %>% 
  .[[1]]

# get p-values for the substrain groups
comps_stb <- model_stb %>% 
  emmeans::emmeans(~ infection | genotype) %>% 
  emmeans::contrast("revpairwise") %>%        
  broom::tidy() %>%
  mutate(
    group1 = "no",  
    group2 = "yes",
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE),
    genotype = factor(genotype, levels = c("bntac", "bj"))
  )


# get predicted probabilities
emm_stb <- emmeans::emmeans(
  model_stb,
  ~ infection * genotype,
  type = "response"
)

# make a df
emm_stb_df <- as.data.frame(emm_stb) %>% 
  mutate(
    infection = factor(infection, levels = c("no", "yes")),
    genotype  = factor(genotype, levels = c("bntac", "bj"))
  )


# create plot
ggplot(emm_stb_df, aes(x = infection, y = prob, color = infection)) +
  geom_point(position = position_dodge(width = 0.8), size = 4.5,
             show.legend = FALSE) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.8), width = 0.2,
                size = 1, show.legend = FALSE) +
  facet_wrap(~ genotype, labeller = as_labeller(c("bntac"= "C57BL/6NTac",
                                                  "bj"= "C57BL/6J"))) +
  scale_x_discrete(breaks= c("no", "yes"),
                   labels= c("No", "Yes")) +
  scale_color_manual(values = c("#005AB5", "#DC3220")) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(x = "Infection", y = "Predicted probability") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size= 16),
        axis.title = element_text(size = 20),
        axis.text.y = element_text(size= 17),
        axis.text.x = element_text(size= 18)) +
  ggpubr::stat_pvalue_manual(
    comps_stb, 
    label = "p_label", 
    y.position = c(0.75, 0.75),
    tip.length = 0.01,
    label.size = 5)


ggsave("Images/Plots/comp_proba_stb.png", width= 7, height= 5)



# # get p-values for the substrain groups
# comps_stb <- model_stb %>%
#   emmeans::emmeans(~ infection | genotype) %>%
#   emmeans::contrast("revpairwise") %>%
#   broom::tidy() %>%
#   mutate(
#     p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE),
#   )
# 
# stb_p_labels <- comps_stb %>%
#   dplyr::mutate(
#     infection = "yes",
#   ) %>%
#   dplyr::mutate(genotype = factor(genotype, levels = c("bntac", "bj"))) %>%
#   dplyr::select(genotype, infection, p_label)
# 
# # get p-value for the interaction term
# glmm_stb <- model_stb %>%
#   broom::tidy() %>%
#   dplyr::filter(stringr::str_detect(term, ":")) %>%
#   dplyr::pull(p.value)
# 
# stb_interaction <- paste0(
#   "Interaction: ", scales::pvalue(glmm_stb, accuracy = 0.001, add_p = TRUE))


# contingency table
stb_contingency_tbl <- comp_weights %>%
  dplyr::filter(!is.na(alive)) %>%
  dplyr::group_by(genotype, infection, alive) %>%
  dplyr::summarise(count= n()) %>%
  dplyr::mutate(percent= (count / sum(count)) *100) %>%
  dplyr::mutate(text_label = ifelse(alive == "no",
                             paste0(count, "/", sum(count)), "")) %>%
  dplyr::select(genotype, infection, alive, percent, text_label)


# stacked bar plot
ggplot2::ggplot(
  stb_contingency_tbl, aes(x= infection, y= percent, fill= alive)) +
  geom_col(width= 0.8, color= "black") +
  facet_wrap(~ genotype, labeller = as_labeller(c(
    bntac= "C57BL/6NTac", bj= "C57BL/6J"))) +
  labs(x= "Infection", y= "Percentage of the fetus (%)") +
  scale_x_discrete(breaks= c("no", "yes"),
                   labels= c("No", "Yes")) +
  scale_fill_manual(name= NULL,
                    breaks = c("no", "yes"),
                    values= c("black", "#ffffff"),
                    labels= c("Non-viable", "Viable")) +
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

  # geom_text(data = stb_p_labels,
  #           aes(x = infection, y = 103, label = p_label),
  #           inherit.aes = FALSE,
  #           size = 5, vjust = 0)

# stb_plot <- cowplot::ggdraw(stb_plot) +
#   cowplot::draw_label(stb_interaction, x = 0.25, y = 0.9, size = 13)


ggsave("Images/Plots/comp_stb.png", width= 7, height= 5)


### espaço vascular ######################################################



