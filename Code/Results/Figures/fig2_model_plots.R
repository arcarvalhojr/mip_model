##############################################################
### criação dos graficos dos dados relacionados a figura 1 ###


# pacotes solicitados para visualisação dos dados
library(tidyverse)
library(readxl)
library(ggtext)
library(ggbeeswarm)
library(ggsignif)

# importando o data set
model_percentil_fw <- read_xlsx("Data/Model/model_percentil_fw.xlsx")
str(model_percentil_fw)



# fetal weight ------------------------------------------------------------

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


# placental weight --------------------------------------------------------

# criando uma tabela com o n de observações por camundongo por grupo
n_pw <- model_data_weight %>% 
  filter(!is.na(Placenta_weight)) %>%
  group_by(Infection) %>%
  summarise(observacoes= n_distinct(Mice_ID)) 
n_pw

#customizando boxplot para visualização do peso das placentas
ggplot(model_data_weight, aes(x= Infection, y= Placenta_weight, fill= Infection))+
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width= 0.6)+ 
  geom_quasirandom(show.legend = FALSE, size = 2.5)+
  labs(x= NULL, y= "Placental weight (mg)")+
  scale_x_discrete(breaks= c("0", "2", "4", "8"),
                   labels= c(("PbNK65 -\n(n= 5)"),
                             ("PbNK65 +\n(n= 5)"),
                             ("PbNK65 ++\n(n= 5)"),
                             ("PbNK65 +++\n(n= 5)")))+
  scale_fill_manual(values= c("#cccccc", "#fcae91", "#fb6a4a", "#cb181d"))+
  scale_y_continuous(limits= c(30, 150), breaks= seq(30, 150, by= 30),
                     expand = c(0,0))+
  theme_classic()+
  theme(axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15))

ggsave("Plots/Caracterization/model_pw.tiff", width= 6, height= 5)


# fw/pw ratio -------------------------------------------------------------

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


# fetal growth restriction (FGR) ------------------------------------------

# grafico de densidade do peso dos fetos

#calculando o n de fetos por grupo
n_fw <- model_percentil_fw %>% 
  filter(!is.na(Fetal_weight)) %>%
  group_by(Infection) %>%
  summarise(observacoes= sum(!is.na(Fetal_weight)))
n_fw

ggplot(model_percentil_fw, aes(x = Fetal_weight, color= Infection)) +
  geom_density(size= 1) +
  labs(x = "Fetal weight (g)", y = "Density") +
  scale_color_manual(name = NULL,
                    breaks = c("0", "8"),
                    values = c("#636363", "#cb181d"),
                    labels = c("PbNK65 - (n= 40)",
                               "PbNK65 +++ (n= 36)")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(limits = c(0.5, 1.5), breaks = seq(0.5, 1.5, by = 0.2),
                     expand = c(0.02, 0)) +
  geom_vline(xintercept = percentil_5, linetype = "dashed", size = 0.8) +
  theme_classic() +
  theme(legend.text = element_text(size= 13),
        legend.position = c(1, 1),
        legend.justification = c(1, 1), 
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16))

ggsave("Plots/Caracterization/density_fw.tiff", width= 7, height= 6)

## stacked bar peso dos fetos

#Criando uma tabela de contingencia
rel_aboun_fw <- model_percentil_fw %>%
  group_by(Infection, percentil) %>%
  summarise(count= n()) %>%
  mutate(percent= (count / sum(count)) *100) %>%
  mutate(text_label = ifelse(percentil == "below",
                             paste0(count, "/", sum(count)), "")) %>%
  select(Infection, percentil, percent, text_label)
rel_aboun_fw

#stacked bar plot
ggplot(rel_aboun_fw, aes(x= Infection, y= percent, fill= percentil)) +
  geom_col(width= 0.9, color= "black") +
  labs(x= NULL, y= "Percentage of the fetus (%)") +
  scale_fill_manual(name= NULL,
                    breaks = c("below", "under"),
                    values= c("black", "#ffffff"),
                    labels= c("Below 5th percentil", "Under 5th percentil")) +
  scale_x_discrete(breaks= c("0", "8"),
                   labels= c("PbNK65 -\n(n=40)",
                             "PbNK65 +++\n(n=36)")) +
  scale_y_continuous(limits = c(0, 103), breaks = seq(0, 103, by= 20),
                     expand = c(0,0)) +
  theme_classic()+
  theme(legend.text = element_text(size= 13),
        axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15)) +
  geom_text(aes(label = text_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 5) +
  geom_text(data=tibble(x=c(2), y=c(101)),
            aes(x=x, y=y, label= "*"), size= 8,
            inherit.aes = FALSE)

ggsave("Plots/Caracterization/stacked_fw.tiff", width= 6, height= 6)


# fetal viability ---------------------------------------------------------

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


# placental vascular space ------------------------------------------------

#filtrando a analise 3 (media)
vasc_space_data <- model_vasc_space %>%
  filter(Analysis == "3")
str(vasc_space_data)

# criando uma tabela com o n de observações por camundongo por grupo
n_vasc_space <- vasc_space_data %>% 
  filter(!is.na(Area)) %>%
  group_by(Infection) %>%
  summarise(observacoes= n_distinct(Mice_ID)) 
n_vasc_space

ggplot(vasc_space_data, aes(x= Infection, y= Area, fill= Infection))+
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width= 0.6)+ 
  geom_quasirandom(show.legend = FALSE, size = 2.5)+
  labs(x= NULL, y= "Placental weight (mg)")+
  scale_x_discrete(breaks= c("0", "2", "4", "8"),
                   labels= c(("PbNK65 -\n(n= 5)"),
                             ("PbNK65 +\n(n= 3)"),
                             ("PbNK65 ++\n(n= 4)"),
                             ("PbNK65 +++\n(n= 4)")))+
  scale_fill_manual(values= c("#cccccc", "#fcae91", "#fb6a4a", "#cb181d"))+
  scale_y_continuous(limits= c(25, 55), breaks= seq(25, 55, by= 10),
                     expand = c(0,0))+
  theme_classic()+
  theme(axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15))

ggsave("Plots/Caracterization/model_vasc_space.tiff", width= 6, height= 5)

