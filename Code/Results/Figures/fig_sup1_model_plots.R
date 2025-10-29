#####################################################################
###  criação dos graficos dos dados relacionados a figura sup 1 #####

# pacotes solicitados para as analises
library(tidyverse)
library(readxl)
library(ggtext)
library(ggbeeswarm)
library(ggsignif)


### importando os data sets

# dams data set
model_data_dams <- read_xlsx("Data/Model/MODEL DATA_DAMS.xlsx")
str(model_data_dams)
#a variavel Infecção precisa ser categorica
model_data_dams$Infection <- as.character(model_data_dams$Infection) 
str(model_data_dams)

# fetus data set
model_data_weight <- read_xlsx("Data/Model/MODEL DATA_WEIGHT.xlsx")
str(model_data_weight)
#a variavel Infecção precisa ser categorica
model_data_weight$Infection <- as.character(model_data_weight$Infection)
str(model_data_weight)

# vascular space data set
model_vasc_space <- read_xlsx("Data/Model/MODEL_VASC_SPACE.xlsx") 
str(model_vasc_space) 
#a variavel Infecção precisa ser categorica
model_vasc_space$Infection <- as.character(model_vasc_space$Infection) 
model_vasc_space$Analysis <- as.character(model_vasc_space$Analysis)
str(model_vasc_space)

# criando uma função para calcular a mediana, iqr e os limites
median_iqr <- function(x)
{
  data.frame(
    y= median(x),
    ymin= quantile(x, 0.25),
    ymax= quantile(x, 0.75)
  )
}


#### contagem de RBC ###################################################

# computando o numero de observações da variavel
n_rbc <- model_data_dams %>%
  filter(!is.na(RBC)) %>%
  group_by(Infection) %>%
  summarise(observacoes= sum(!is.na(RBC)))
n_rbc

ggplot(model_data_dams, aes(x= Infection, y= RBC, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom= "errorbar", color= "black",
               width= 0.3, show.legend = FALSE) + #adicionando o iqr gerado pela função
  stat_summary(fun = median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") + #adicionando a mediana 
  geom_quasirandom(show.legend = FALSE, shape= 21, size= 6, color= "black")+ #adicionando os pontos
  labs(x= NULL, y= "Red blood cells (x10^6/uL)") +
  scale_x_discrete(breaks = c("0", "2", "4", "8"),
                   labels = c("PbNK65-\n(n=5)",
                              "PbNK65+\n(n=5)",
                              "PbNK65++\n(n=5)",
                              "PbNK65+++\n(n=4)")) + #trocando a legenda padrão e adicionando o n de observações
  scale_fill_manual(values = c("#cccccc","#fcae91", "#fb6a4a", "#cb181d")) + #customisando as cores
  scale_y_continuous(limits = c(4, 10), breaks = seq(4, 10, by= 2),
                     expand = c(0,0)) + #customisando os intervalos dos eixos
  theme_classic() + #adicionando o tema com fundo branco
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 15)) + #alterando o tamanho das legendas dos eixos
  geom_signif(comparisons = list(c("0", "4")),
              annotations = c("*"),
              y_position = c(9),
              textsize = 6, tip_length = 0.03, vjust = 0.3) #adicionando a estatistica 

ggsave("Plots/Caracterization/rbc.tiff", width = 6, height = 5) #salvando a imagem

#### percentual de HCT ###################################################

#computando o numero de observações da variavel
n_hct <- model_data_dams %>%
  filter(!is.na(HCT)) %>%
  group_by(Infection) %>%
  summarise(observacoes= sum(!is.na(HCT)))
n_hct

ggplot(model_data_dams, aes(x= Infection, y= HCT, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom= "errorbar", color= "black",
               width= 0.3, show.legend = FALSE) +
  stat_summary(fun = median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") +
  geom_quasirandom(show.legend = FALSE, shape= 21, size= 6, color= "black")+
  labs(x= NULL, y= "Hematocrit (%)") +
  scale_x_discrete(breaks = c("0", "2", "4", "8"),
                   labels = c("PbNK65-\n(n=5)",
                              "PbNK65+\n(n=5)",
                              "PbNK65++\n(n=5)",
                              "PbNK65+++\n(n=4)")) +
  scale_fill_manual(values = c("#cccccc", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(limits = c(23, 43), breaks = seq(23, 43, by= 5),
                     expand = c(0,0)) +
  theme_classic()+
  theme(axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15)) +
  geom_signif(comparisons = list(c("0", "4")),
              annotations = c("**"),
              y_position = c(40),
              textsize = 6, tip_length = 0.03, vjust = 0.3)

ggsave("Plots/Caracterization/hct.tiff", width = 6, height = 5)

#### contagem de HGB ##################################################

#criando um atabela com o numero de observações
n_hgb <- model_data_dams %>%
  filter(!is.na(HGB)) %>%
  group_by(Infection) %>%
  summarise(observacoes= sum(!is.na(HGB)))
n_hgb

ggplot(model_data_dams, aes(x= Infection, y= HGB, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom= "errorbar", color= "black",
               width= 0.3, show.legend = FALSE) +
  stat_summary(fun = median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") +
  geom_quasirandom(show.legend = FALSE, shape= 21, size= 6, color= "black")+
  labs(x= NULL, y= "Hemoglobin (g/dL)") +
  scale_x_discrete(breaks = c("0", "2", "4", "8"),
                   labels = c("PbNK65-\n(n=5)",
                              "PbNK65+\n(n=5)",
                              "PbNK65++\n(n=5)",
                              "PbNK65+++\n(n=4)")) +
  scale_fill_manual(values = c("#cccccc", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(limits = c(8, 16), breaks = seq(8, 16, by= 2),
                     expand = c(0,0)) +
  theme_classic()+
  theme(axis.title.y = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15)) +
  geom_signif(comparisons = list(c("0", "4")),
              annotations = c("*"),
              y_position = c(15),
              textsize = 6, tip_length = 0.03, vjust = 0.3)

ggsave("Plots/Caracterization/hgb.tiff", width = 6, height = 5)

### contagem de leucocitos (WBC) ###################################

# criando um atabela com o numero de observações
n_wbc <- model_data_dams %>%
  filter(!is.na(WBC)) %>%
  group_by(Infection) %>%
  summarise(observacoes= sum(!is.na(WBC)))
n_wbc


ggplot(model_data_dams, aes(x= Infection, y= WBC, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom= "errorbar", color= "black",
               width= 0.3, show.legend = FALSE) +
  stat_summary(fun = median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") +
  geom_quasirandom(show.legend = FALSE, shape= 21, size= 6, color= "black")+
  labs(x= NULL, y= "Leucocytes (x10^3/uL)") +
  scale_x_discrete(breaks = c("0", "2", "4", "8"),
                   labels = c("PbNK65-\n(n=5)",
                              "PbNK65+\n(n=5)",
                              "PbNK65++\n(n=5)",
                              "PbNK65+++\n(n=4)")) +
  scale_fill_manual(values = c("#cccccc", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by= 2),
                     expand = c(0.1,0)) +
  theme_classic()+
  theme(axis.title.y = element_text(size= 18),
        axis.text.y = element_text(size= 15),
        axis.text.x = element_text(size= 14.5)) +
  geom_signif(comparisons = list(c("0", "4")),
              annotations = c("*"),
              y_position = c(6),
              textsize = 6, tip_length = 0.03, vjust = 0.3)

ggsave("Plots/Caracterization/wbc.tiff", width = 6, height = 5)

### customização dot plot linfocitos ###################################


ggplot(model_data_dams, aes(x= Infection, y= Lymph, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") +
  geom_quasirandom(show.legend = FALSE, shape= 21, size= 6, color= "black")+
  labs(x= NULL, y= "Percentage of lymphocytes (%)") +
  scale_x_discrete(breaks = c("0", "2", "4", "8"),
                   labels= c("PbNK65-\n(n=5)",
                             "PbNK65+\n(n=5)",
                             "PbNK65++\n(n=5)",
                             "PbNK65+++\n(n=4)")) +
  scale_fill_manual(values= c("#cccccc", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(limits = c(30, 110), breaks = seq(30, 110, by= 20),
                     expand = c(0,0)) +
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15))+
  geom_signif(comparisons = list(c("0", "8")),
              annotations = c("**"),
              y_position = c(102),
              textsize = 6, tip_length = 0.03, vjust = 0.3)

ggsave("Plots/Caracterization/lymph.tiff", width = 6, height = 5)

### customização dot plot granulocitos ###############################


ggplot(model_data_dams, aes(x= Infection, y= Gran, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") +
  geom_quasirandom(show.legend = FALSE, shape= 21, size= 6, color= "black")+
  labs(x= NULL, y= "Percentage of Granulocytes (%)") +
  scale_x_discrete(breaks = c("0", "2", "4", "8"),
                   labels= c("PbNK65-\n(n=5)",
                             "PbNK65+\n(n=5)",
                             "PbNK65++\n(n=5)",
                             "PbNK65+++\n(n=4)")) +
  scale_fill_manual(values= c("#cccccc", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(limits = c(2, 82), breaks = seq(2, 82, by= 20),
                     expand = c(0,0)) +
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15))+
  geom_signif(comparisons = list(c("0", "8")),
              annotations = c("**"),
              y_position = c(70),
              textsize = 6, tip_length = 0.03, vjust = 0.3)

ggsave("Plots/Caracterization/gran.tiff", width = 6, height = 5)

### customização dot plot monocitos ##################################


ggplot(model_data_dams, aes(x= Infection, y= Mon, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black", 
               width= 0.3, show.legend = FALSE) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar", 
               width= 0.5, color= "black") +
  geom_quasirandom(show.legend = FALSE, shape= 21, size= 6, color= "black")+
  labs(x= NULL, y= "Percentage of Monocytes (%)") +
  scale_x_discrete(breaks = c("0", "2", "4", "8"),
                   labels= c("PbNK65-\n(n=5)",
                             "PbNK65+\n(n=5)",
                             "PbNK65++\n(n=5)",
                             "PbNK65+++\n(n=4)")) +
  scale_fill_manual(values= c("#cccccc", "#fcae91", "#fb6a4a", "#cb181d")) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by= 3),
                     expand = c(0,0)) +
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15))

ggsave("Plots/Caracterization/mon.tiff", width = 6, height = 5)

### Peso das placentas ###########################################

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

### espaço vascular ################################################

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
