##############################################################
### criação dos graficos dos dados relacionados a figura 4 ###

# pacotes solicitados para as analises
library(tidyverse)
library(readxl)
library(ggbeeswarm)
library(ggsignif)
library(ggpubr)

### importando o data set ###############

# fetus data set
comp_data_weight <- read_xlsx("Data/Model_comparison/MODEL DATA_WEIGHT_COMP.xlsx")
str(model_data_weight)

# transformando as variaveis infection e genotype em fatores
comp_data_weight$Genotype <- as.factor(comp_data_weight$Genotype)
comp_data_weight$Infection <- as.factor(comp_data_weight$Infection)
comp_data_weight$Stillbirth <- as.factor(comp_data_weight$Stillbirth)
comp_data_weight$Mice_ID <- as.factor(comp_data_weight$Mice_ID)

# o grupo BNTac precisar ser o fator de referencia
comp_data_weight$Genotype <- relevel(comp_data_weight$Genotype, ref = "BNTac")
levels(comp_data_weight$Genotype)


### Peso dos fetos#########################################################

# criando uma tabela com o n de observações por camundongo por grupo
n_fw <- comp_data_weight %>% 
  filter(!is.na(Fetal_weight)) %>%
  group_by(Genotype, Infection) %>%
  summarise(observacoes= n_distinct(Mice_ID))
n_fw

# customizando boxplot para visualização do peso dos fetos
ggplot(comp_data_weight, aes(x= Genotype, y= Fetal_weight, fill= Infection))+
  geom_boxplot(outlier.shape = NA, width= 0.8)+
  geom_quasirandom(show.legend = FALSE, size = 1.5, dodge.width = 0.8)+
  labs(x= NULL, y= "Fetal weight (g)")+
  scale_x_discrete(breaks= c("BJ", "BNTac"),
                   labels= c("C57BL/6J\n(n=11-7)",
                             "C57BL/6NTac\n(n=11-8)"))+
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#91BFDB", "#F1A340"))+
  scale_y_continuous(limits = c(0.7, 1.7), breaks = seq(0.7, 1.7, by= 0.2),
                     expand = c(0,0))+
  theme_classic()+
  theme(legend.position = c(0.8, 0.9),
        legend.text = element_text(size = 13))+
  theme(axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))

 # geom_signif(y_position = c(1.6), xmin = c(0.8), 
  #            xmax = c(1.2), annotation = c("**"),
   #           textsize = 6, tip_length = 0.03, vjust = 0.3)

  
ggsave("Plots/Comparison/comp_fw.tiff", width = 6, height = 5)

### razão peso dos fetos pelo peso da placenta (racio)##################

# criando uma tabela com o n de observações por camundongo por grupo
n_ratio <- comp_data_weight %>% 
  filter(!is.na(FW_PW)) %>%
  group_by(Genotype, Infection) %>%
  summarise(observacoes= n_distinct(Mice_ID)) 
n_ratio

#customizando boxplot para visualização do racio
ggplot(comp_data_weight, aes(x= Genotype, y= FW_PW, fill= Infection))+
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width= 0.8)+
  geom_quasirandom(show.legend = FALSE, size = 1.5, dodge.width = 0.8,
                   cex = 4)+
  labs(x= NULL, y= "FW/PW ratio (g/g)")+
  scale_x_discrete(breaks= c("BJ", "BNTac"),
                   labels= c("C57BL/6J\n(n=10-4)",
                             "C57BL/6NTac\n(n=9-6)"))+
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#91BFDB", "#F1A340"))+
  scale_y_continuous(limits = c(2, 18), breaks = seq(2, 18, by= 4),
                     expand = c(0,0))+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16))

ggsave("Plots/Comparison/comp_ratio.tiff", width = 6, height = 5)


### grafico de densidade peso dos fetos#####################################

# importando o data set
comp_percentil_fw <- read_xlsx("Data/Model_comparison/comp_percentil_fw.xlsx")
# transformando as variaveis infection e genotype em fatores
comp_percentil_fw$Genotype <- as.factor(comp_percentil_fw$Genotype)
# o grupo BNTac precisar ser o fator de referencia
comp_percentil_fw$Genotype <- relevel(comp_percentil_fw$Genotype, ref = "BNTac")

#calculando o n de fetos por grupo
n_percentil <- comp_percentil_fw %>% 
  filter(!is.na(Fetal_weight)) %>%
  group_by(Genotype, Infection) %>%
  summarise(observacoes= sum(!is.na(Fetal_weight)))
n_percentil

ggplot(comp_percentil_fw, aes(x = Fetal_weight, color = Infection)) +
  geom_density(linewidth = 1) +
  facet_wrap(~Genotype, scales = "free",
             labeller = as_labeller(c("BJ"= "C57BL/6J\n(n=73-44)",
                                      "BNTac"= "C57BL/6NTac\n(n=89-59)"))) +
  labs(x = "Fetal weight (g)", y = "Density") +
  scale_color_manual(name = NULL,
                    breaks = c("NO", "YES"),
                    values = c("#91BFDB", "#F1A340"),
                    labels = c("Non-infected",
                               "Infected")) +
  scale_y_continuous(expand = c(0.025,0)) +
  scale_x_continuous(expand = c(0.05, 0)) +
  geom_vline(data = percentis_5, aes(xintercept = percentil_5), 
             linetype = "dashed", linewidth = 0.8) +
  theme_classic() +
  theme(legend.text = element_text(size= 12),
        legend.position = c(0.1025, 0.9),
        strip.background = element_blank(),
        strip.text = element_text(size= 15),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16))

ggsave("Plots/Comparison/density_fw.tiff", width= 8.5, height= 5)


### stacked bar peso dos fetos############################################

# Calculando proporções para cada grupo de infecção
rel_aboun_fw <- comp_percentil_fw %>%
  group_by(Genotype, Infection, percentil) %>%
  summarise(count= n()) %>%
  mutate(percent= (count / sum(count)) *100) %>%
  mutate(text_label = ifelse(percentil == "below",
                             paste0(count, "/", sum(count)), "")) %>%
  dplyr::select(Genotype, Infection, percentil, percent, text_label)
rel_aboun_fw

# stacked bar plot
ggplot(rel_aboun_fw, aes(x= Infection, y= percent, fill= percentil)) +
  geom_col(width= 0.8, color= "black") +
  facet_wrap(~Genotype,
             labeller = as_labeller(c("BJ"= "C57BL/6J",
                                      "BNTac"= "C57BL/6NTac"))) +
  labs(x= "Infection", y= "Percentage of the fetus (%)") +
  scale_fill_manual(name= NULL,
                    breaks = c("below", "under"),
                    values= c("black", "#ffffff"),
                    labels= c("Below 5th percentil", "Under 5th percentil")) +
  scale_x_discrete(breaks= c("NO", "YES"),
                   labels= c("No", "Yes")) +
  scale_y_continuous(limits = c(0, 103), breaks = seq(0, 103, by= 20),
                     expand = c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size= 15, margin = margin(b = 10)),
        legend.position = "top",
        legend.text = element_text(size= 13),
        axis.title = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) +
  geom_text(aes(label = text_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 5)

ggsave("Plots/Comparison/comp_stacked_fw.tiff", width= 7, height= 6)

### espaço vascular ######################################################