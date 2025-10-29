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

