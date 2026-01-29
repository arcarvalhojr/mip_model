### Figure plots of data related to figure 5 ###


# Import libraries --------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(broom.mixed)
library(ggpubr)

# import clean data -------------------------------------------------------

# cytokines data set
model_cytok <- readRDS("Data/Clean_data/comp_cytok_long.rds")


# import models outputs
models_output <- readRDS("Code/Results/fig5_model_outputs.rds")




# necessary function ------------------------------------------------------

# these functions were adapted from this answer by Claus Wilke:
# https://stackoverflow.com/questions/60332202/conditionally-fill-ggtext-text-boxes-in-facet-wrap
# https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/


element_textbox_highlight <- function(..., hi.labels = NULL, hi.fill = NULL,
                                      hi.col = NULL, hi.box.col = NULL, hi.family = NULL) {
  structure(
    c(element_textbox(...),
      list(hi.labels = hi.labels, hi.fill = hi.fill, hi.col = hi.col, hi.box.col = hi.box.col, hi.family = hi.family)
    ),
    class = c("element_textbox_highlight", "element_textbox", "element_text", "element")
  )
}

element_grob.element_textbox_highlight <- function(element, label = "", ...) {
  if (label %in% element$hi.labels) {
    element$fill <- element$hi.fill %||% element$fill
    element$colour <- element$hi.col %||% element$colour
    element$box.colour <- element$hi.box.col %||% element$box.colour
    element$family <- element$hi.family %||% element$family
  }
  NextMethod()
}



# extract p-values --------------------------------------------------------

# classify models 
models <- models_output %>%
  tidyr::separate(model_name, into = c("region_name", "cytokine_name"), sep = " ") %>%
  dplyr::mutate(
    region = case_when(
      region_name == "serum" ~ "sr",
      region_name == "spleen" ~ "sp",
      region_name == "placenta" ~ "pl"
    ),
    cytokine = case_when(
      cytokine_name == "mcp-1" ~ "mcp1",
      cytokine_name == "ifn" ~ "ifn",
      cytokine_name == "tnf" ~ "tnf",
      cytokine_name == "il-10" ~ "il10"
    ),
    model_type = case_when(
      region == "sp" & cytokine == "il10" ~ "interaction",
      region == "pl" & cytokine == "mcp1" ~ "interaction",
      TRUE ~ "genotype_only"
    )
  )

# get p-values for models without infection group
stats_genotype <- models %>%
  dplyr::filter(model_type == "genotype_only") %>%
  dplyr::mutate(
    stats = purrr::map(model_output, ~
                         emmeans::emmeans(.x, ~ genotype) %>%
                         emmeans::contrast("pairwise") %>%
                         broom::tidy()
    )
  ) %>%
  tidyr::unnest(stats) %>%
  dplyr::mutate(
    group1 = "bntac",
    group2 = "bj",
    p_label = scales::pvalue(p.value, accuracy = 0.001, add_p = TRUE)
  ) %>%
  dplyr::left_join(
    model_cytok %>%
      group_by(region, cytokine) %>%
      summarise(y.position = max(value, na.rm = TRUE) * 1.1),
    by = c("region", "cytokine")
  )

#get interaction p-value for groups
# stats_interaction <- models %>%
#   dplyr::filter(model_type == "interaction") %>%
#   dplyr::mutate(
#     stats = purrr::map(model_output, ~
#                          broom::tidy(.x) %>%
#                          filter(stringr::str_detect(term, ":"))
#     )
#   ) %>%
#   tidyr::unnest(stats) %>%
#   dplyr::mutate(
#     label = ifelse(
#       p.value < 0.001,
#       "**Interaction:** <i>p</i> &lt; 0.001",
#       glue::glue("Interaction: p={signif(p.value, 2)}")
#     )
#   )


# cytkokine grid plot -----------------------------------------------------

labels <- c(
  "mcp1" = "MCP-1", 
  "ifn" = "IFN-&gamma;", 
  "tnf" = "TNF",
  "il10" = "IL-10",
  "pl" = "PLACENTA",
  "sr" = "SORO",
  "sp" = "BAÇO"
)


cytok_labels <- c(
  "mcp1" = "MCP-1", 
  "ifn" = "IFN-&gamma;", 
  "tnf" = "TNF",
  "il10" = "IL-10"
)


# create the plot
ggplot2::ggplot(model_cytok, aes(x = genotype, y = value, fill = infection)) +
  stat_summary(fun.data = ggpubr::mean_sd, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(1)) + 
  stat_summary(fun= mean, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(1)) + 
  ggbeeswarm::geom_quasirandom(shape= 21, size= 4.5, color= "black",
                   dodge.width = 1) +
  facet_wrap(region ~ cytokine, scales = "free", drop = TRUE,
             labeller = as_labeller(labels)) +
  labs(x = NULL, y = "Cytokines") +
  scale_fill_manual(name= NULL,
                    breaks = c("no", "yes"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#005AB5", "#DC3220")) +
  scale_x_discrete(breaks= c("bntac", "bj"),
                   labels= c("C57BL/6NTac", "C57BL/6J")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        legend.text = element_text(size= 16),
        axis.title = element_text(size = 23),
        axis.text.y = element_text(size= 20),
        axis.text.x = element_text(size= 21),
        panel.spacing.y = unit(1.2, "lines"),
        strip.text = element_textbox_highlight(
          size = 16, face = "bold",
          fill = "white", box.color = "black", color = "black",
          halign = .5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
          padding = margin(5, 0, 3, 0), margin = margin(0, 1, 3, 1),
          hi.labels = cytok_labels,
          hi.fill = "#cccccc", hi.box.col = "black", hi.col = "black")) +
  ggpubr::stat_pvalue_manual(
    stats_genotype,
    label = "p_label",
    xmin = "group1",
    xmax = "group2",
    tip.length = 0.01,
    label.size = 5
  ) 


  # ggtext::geom_richtext(
  #   data = stats_interaction,
  #   aes(
  #     x = Inf,
  #     y = Inf,
  #     label = label
  #   ),
  #   inherit.aes = FALSE,
  #   hjust = 1.05,
  #   vjust = 1.3,
  #   size = 4.5,
  #   fill = NA,
  #   label.color = NA
  # )

        
ggsave("Images/Plots/comp_cytokines.png", width = 14, height = 13)  
  
