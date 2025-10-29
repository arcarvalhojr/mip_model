##############################################################
### criação dos graficos dos dados relacionados a figura 5 ###

# pacotes solicitados para as analises
library(tidyverse)
library(readxl)
library(ggtext)
library(ggbeeswarm)
library(ggsignif)


### importando os data sets ####

# dams data set
comp_data_dams <- read_xlsx("Data/Model_comparison/MODEL DATA_DAMS_COMP.xlsx")

# transformando a variavel genotype em fator
comp_data_dams$Genotype <- as.factor(comp_data_dams$Genotype)

# o grupo BNTac precisar ser o fator de referencia
comp_data_dams$Genotype <- relevel(comp_data_dams$Genotype, ref = "BNTac")
levels(comp_data_dams$Genotype)

# fetus data set
comp_data_weight <- read_xlsx("Data/Model_comparison/MODEL DATA_WEIGHT_COMP.xlsx")

# transformando as variaveis infection e genotype em fatores
comp_data_weight$Genotype <- as.factor(comp_data_weight$Genotype)

# o grupo BNTac precisar ser o fator de referencia
comp_data_weight$Genotype <- relevel(comp_data_weight$Genotype, ref = "BNTac")
levels(comp_data_weight$Genotype)


### slecionando variaveis especificas ################################
cytok_data_dams <- comp_data_dams %>%
  select(Genotype, Mice_ID, Infection, IL10_sp, MCP1_sp, IFN_sp, TNF_sp,
         MCP1_sr, IFN_sr, TNF_sr)

cytok_data_placenta <- comp_data_weight %>%
  select(Genotype, Mice_ID, Infection, MCP1, IFN)


# Organizando cytok_data_dams no formato longo e removendo NAs
cytok_data_dams_long <- cytok_data_dams %>%
  pivot_longer(
    cols = starts_with("IL10") | starts_with("MCP1") | starts_with("IFN") | starts_with("TNF"),
    names_to = c("Cytokine", "Region"),
    names_sep = "_",
    values_to = "Value",
    values_drop_na = TRUE)


# Organizando cytok_data_placenta no formato longo
cytok_data_placenta_long <- cytok_data_placenta %>%
  pivot_longer(
    cols = c(MCP1, IFN),
    names_to = "Cytokine",
    values_to = "Value",
    values_drop_na = TRUE) %>%
  mutate(Region = "Placenta")



# Unindo os dois datasets 
cytok_data_long <- bind_rows(cytok_data_dams_long,
                             cytok_data_placenta_long)


# Reordenando e redefinindo os nomes de cytokine
cytok_data_long$Cytokine <- factor(
  cytok_data_long$Cytokine,
  levels = c("MCP1", "IFN", "TNF", "IL10"),
  labels = c("MCP-1", "IFN", "TNF", "IL-10"))

# Redefinindo os nomes de Region
cytok_data_long$Region <- factor(
  cytok_data_long$Region,
  levels = c("Placenta", "sr", "sp"),
  labels = c("PLACENTA", "SERUM", "SPLEEN"))

cytok_data_long$Cytokine <- as.factor(cytok_data_long$Cytokine)
cytok_data_long$Cytokine <- factor(cytok_data_long$Cytokine,
                                        levels = c("MCP1","IFN","TNF","IL10"))
levels(cytok_data_long$Cytokine)


# criando uma função para calcular a mediana, iqr e os limites
median_iqr <- function(x)
{
  data.frame(
    y= median(x),
    ymin= quantile(x, 0.25),
    ymax= quantile(x, 0.75)
  )
}


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

# criando um painel paea visualização dos dados
ggplot(cytok_data_long, aes(x = Genotype, y = Value, fill = Infection)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(1)) + 
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(1)) + 
  geom_quasirandom(shape= 21, size= 3, color= "black",
                   dodge.width = 1) +
  facet_wrap(Cytokine ~ Region, scales = "free_y", drop = TRUE) +
  labs(x = NULL, y = "Cytokine Level") +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#91BFDB", "#F1A340")) +
  scale_x_discrete(breaks= c("BJ", "BNTac"),
                   labels= c("C57BL/6J", "C57BL/6NTac")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        legend.text = element_text(size= 16),
        axis.title = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15),
        strip.text = element_textbox_highlight(
          size = 13, face = "bold",
          fill = "white", box.color = "black", color = "black",
          halign = .5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
          padding = margin(5, 0, 3, 0), margin = margin(0, 1, 3, 1),
          hi.labels = c("MCP-1", "IFN", "TNF", "IL-10"),
          hi.fill = "#cccccc", hi.box.col = "black", hi.col = "black"))
        
ggsave("Plots/Comparison/comp_cytokines.tiff", width = 10, height = 9)  

