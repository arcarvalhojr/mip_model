### Figure plots of data related to figure 1 ###


# Import libraries --------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(ggsignif)
library(broom)
library(ggpubr)

# import clean data -------------------------------------------------------

# dams data set
model_dams <- readRDS("Data/Clean_data/model_dams.rds")

# fetus data set
model_pb18s <- readRDS("Data/Clean_data/model_pb18s_clean.rds")

# dams peripheral parasitemia data set
model_parasitemia <- readRDS("Data/Clean_data/model_parasitemia.rds")

# import models outputs
models_output <- readRDS("Code/Results/fig1_model_outputs.rds")

# funcyion to calculate the median iqr and limits
median_iqr <- function(x)
{
  data.frame(
    y= median(x),
    ymin= quantile(x, 0.25),
    ymax= quantile(x, 0.75)
  )
}


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


ggplot2::ggplot(model_dams,
                aes(x= infection, y= spleen_w, fill= infection)) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE) + #add std errorbars
  stat_summary(fun= mean, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") + #add the mean
  ggbeeswarm::geom_quasirandom(show.legend = FALSE, shape= 21, size= 6,
                               color= "black") + #add datapoints in each group
  labs(x= NULL, y= "Spleen Weight (mg)") +
  scale_fill_manual(values= c("#636363", "#fcae91", "#fb6a4a", "#cb181d")) + #add custom colors (colorblind safe)
  scale_y_continuous(limits = c(50, 450), breaks = seq(50, 450, by= 100),
                     expand = c(0,0)) + #customize axis limits
  theme_classic()+ #add classic theme with white background
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) + #change text font size
  ggpubr::stat_pvalue_manual(
    glm_sw, 
    label = "p_label", 
    y.position = c(370, 400, 430),
    tip.length = 0.01,
    label.size = 5)
 
ggsave("Plots/Caracterization/spleen.png", width = 6, height = 5) #save image


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


ggplot(model_parasitemia, aes(x= preg, y= parasitemia, fill= preg)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom= "errorbar", color= "black", width= 0.3) +
  stat_summary(show.legend = FALSE, fun = mean, geom = "crossbar",
               width= 0.5, color= "black") +
  geom_quasirandom(show.legend = FALSE, shape= 21, size= 4, color= "black") +
  facet_grid(~ infection) +
  labs(x= "Pregnancy", y= "Peripheral parasitemia (%)") +
  scale_x_discrete(breaks = c("no", "yes"),
                   labels = c("no", "yes")) +
  scale_fill_manual(values = c("#ffffff", "#cb181d")) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by= 3),
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size= 14),
        axis.title.y = element_text(size= 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) +
  ggtext::geom_richtext(
    data = parasitemia_stat, 
    aes(label = label, y = 13),
    fill = NA,          # Remove o fundo branco
    label.color = NA,   # Remove a borda da caixa
    size = 4,           # Tamanho da fonte
    hjust = 0.25           # Alinha à esquerda
  )

ggsave("Plots/Caracterization/peri_para.png", width = 6, height = 5)


# placental parasitemia (pb18s expression) --------------------------------

# get the number of obs
n_pb18s <- model_pb18s_data %>%
  filter(!is.na(pb18s)) %>%
  group_by(Infection) %>%
  summarise(observacoes= sum(!is.na(pb18s)))
n_pb18s

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


ggplot(model_pb18s, aes(x= infection, y= pb18s, fill= infection)) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") +
  geom_quasirandom(show.legend = FALSE, size = 5, shape= 21, color= "black")+
  labs(x= NULL, y= "pb18s mRNA (relative expression)") +
  scale_fill_manual(values= c("#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_log10() +
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) +
  ggpubr::stat_pvalue_manual(
    glm_pb18s, 
    label = "p_label", 
    y.position = c(4, 4.5),
    tip.length = 0.01,
    label.size = 5)

ggsave("Plots/Caracterization/pb18s.png", width= 6, height= 5)

### Peso dos fetos ##################################################

#computando o numero de observações da variavel
n_fw <- model_data_weight %>% 
  filter(!is.na(Mice_ID)) %>%
  group_by(Infection) %>%
  summarise(observacoes= n_distinct(Mice_ID)) #conta o número de valores únicos na variavel
n_fw

#customizando boxplot para visualização do peso dos fetos
ggplot(model_data_weight, aes(x= Infection, y= Fetal_weight, fill= Infection))+
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width= 0.6)+ 
  geom_quasirandom(show.legend = FALSE, size = 2.5)+
  labs(x= NULL, y= "Fetal weight (g)")+
  scale_x_discrete(breaks= c("0", "2", "4", "8"),
                   labels= c(("PbNK65 -\n(n= 5)"),
                             ("PbNK65 +\n(n= 5)"),
                             ("PbNK65 ++\n(n= 5)"),
                             ("PbNK65 +++\n(n= 5)")))+
  scale_fill_manual(values= c("#636363", "#fcae91", "#fb6a4a", "#cb181d"))+
  scale_y_continuous(limits= c(0.4, 1.6), breaks= seq(0.4, 1.6, by= 0.3),
                     expand = c(0,0))+
  theme_classic()+
  theme(axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15))+
  geom_signif(comparisons = list(c("0", "8")),
              annotations = c("*"),
              y_position = c(1.45),
              textsize = 6, tip_length = 0.03, vjust = 0.3)

ggsave("Plots/Caracterization/model_fw.tiff", width= 6, height= 5)


### Racio FW/PW #########################################################

#criando uma tabela com o n de observações por camundongo por grupo
n_ratio <- model_data_weight %>% 
  filter(!is.na(FW_PW)) %>%
  group_by(Infection) %>%
  summarise(observacoes= n_distinct(Mice_ID)) 
n_ratio

#customizando boxplot para visualização da razão FW/PW
ggplot(model_data_weight, aes(x= Infection, y= FW_PW, fill= Infection))+
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width= 0.6)+
  geom_quasirandom(show.legend = FALSE, size = 2.5)+
  labs(x= NULL, y= "Fw/Pw ratio (g/g)")+
  scale_x_discrete(breaks= c("0", "2", "4", "8"),
                   labels= c(("PbNK65 -\n(n= 5)"),
                             ("PbNK65 +\n(n= 5)"),
                             ("PbNK65 ++\n(n= 5)"),
                             ("PbNK65 +++\n(n= 5)")))+
  scale_fill_manual(values= c("#636363", "#fcae91", "#fb6a4a", "#cb181d"))+
  scale_y_continuous(limits= c(5, 21), breaks= seq(5, 21, by= 4),
                     expand = c(0,0))+
  theme_classic()+
  theme(axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15))

ggsave("Plots/Caracterization/model_ratio.tiff", width= 6, height= 5)

### Stillbirth (viabilidade fetal) ######################################

#computando o numero de observações da variavel
n_stb <- model_data_weight %>%
  filter(!is.na(Stillbirth)) %>%
  group_by(Infection, Stillbirth) %>%
  summarise(observacoes= sum(!is.na(Stillbirth)))
n_stb

# criando uma tabela de contingencia
stb_rel_abun <- model_data_weight %>%
  group_by(Infection, Stillbirth) %>%
  summarise(count= n()) %>%
  mutate(percent= (count / sum(count)) *100) %>%
  mutate(text_label = ifelse(Stillbirth == "NO",
                             paste0(count, "/", sum(count)), "")) %>%
  select(Infection, Stillbirth, percent, text_label)
stb_rel_abun

ggplot(stb_rel_abun, aes(x= Infection, y= percent, fill= Stillbirth)) +
  geom_col(width= 0.9, color= "black") +
  labs(x= NULL, y= "Percentage of the fetus (%)") +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    values= c("black", "#ffffff"),
                    labels= c("Non-viable", "Viable")) +
  scale_x_discrete(breaks= c("0", "2", "4", "8"),
                   labels= c("PbNK65-\n(n=40)",
                             "PbNK65+\n(n=38)",
                             "PbNK65++\n(n=37)",
                             "PbNK65+++\n(n=36)")) +
  scale_y_continuous(limits = c(0, 100.5), breaks = seq(0, 100.5, by= 20),
                     expand = c(0,0)) +
  theme_classic()+
  theme(legend.position = "top",
        legend.text = element_text(size= 13),
        axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15)) +
  geom_text(aes(label = text_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 5)

ggsave("Plots/Caracterization/model_stb.tiff", width= 6, height= 5)




