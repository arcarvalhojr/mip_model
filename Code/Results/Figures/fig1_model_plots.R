##############################################################
### criação dos graficos dos dados relacionados a figura 1 ###

# pacotes solicitados para visualisação dos dados
library(tidyverse)
library(readxl)
library(ggtext)
library(ggbeeswarm)
library(ggsignif)

### importando os data sets #######################

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

# dams peripheral parasitemia data set
model_parasitemia <- read_xlsx("Data/Model/MODEL_PARASITEMIA.xlsx")
str(model_parasitemia)
#a variavel Infecção precisa ser categorica
model_parasitemia$Infection <- as.character(model_parasitemia$Infection)
str(model_parasitemia)

#criando uma função para calcular a mediana, iqr e os limites
median_iqr <- function(x)
{
  data.frame(
    y= median(x),
    ymin= quantile(x, 0.25),
    ymax= quantile(x, 0.75)
  )
}

#### peso do baço#######################################################

#computando o numero de observações da variavel
n_sw <- model_data_dams %>%
  filter(!is.na(Spleen_w)) %>%
  group_by(Infection) %>%
  summarise(observacoes= sum(!is.na(Spleen_w)))
n_sw

ggplot(model_data_dams, aes(x= Infection, y= Spleen_w, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE) + #adicionando o iqr gerado pela função
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") + #adicionando a mediana
  geom_quasirandom(show.legend = FALSE, shape= 21, size= 6, color= "black")+ #adicionando os pontos
  labs(x= NULL, y= "Spleen Weight (mg)") +
  scale_x_discrete(breaks = c("0", "2", "4", "8"),
                   labels= c("PbNK65-\n(n=5)",
                             "PbNK65+\n(n=5)",
                             "PbNK65++\n(n=5)",
                             "PbNK65+++\n(n=5)")) + #trocando a legenda padrão e adicionando o n de observações
  scale_fill_manual(values= c("#636363", "#fcae91", "#fb6a4a", "#cb181d")) + #customizando as cores
  scale_y_continuous(limits = c(50, 450), breaks = seq(50, 450, by= 100),
                     expand = c(0,0)) + #customisando os limites e intervalos dos eixos 
  theme_classic()+ #adicionando o tema com fundo branco
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15))+ #alterando o tamando das fontes das legendas
  geom_signif(comparisons = list(c("0", "2"), c("0", "4"), c("0", "8")),
              annotations = c("***", "***", "***"),
              y_position = c(360, 390, 420),
              textsize = 6, tip_length = 0.03, vjust = 0.3) #adicionando a estatistica 
 
ggsave("Plots/Caracterization/spleen.tiff", width = 6, height = 5) #salvando a imagem

## percentual de parasitemia periferica ####################################

#computando o numero de observações da variavel
n_peri_para <- model_parasitemia %>%
  filter(!is.na(Parasitemia)) %>%
  group_by(Preg, Infection) %>%
  summarise(observacoes= sum(!is.na(Parasitemia)))
n_peri_para


ggplot(model_parasitemia, aes(x= Preg, y= Parasitemia, fill= Preg)) +
  stat_summary(fun.data = median_iqr, geom= "errorbar", color= "black",
               width= 0.3) +
  stat_summary(show.legend = FALSE, fun = median, geom = "crossbar",
               width= 0.5, color= "black") +
  geom_quasirandom(show.legend = FALSE, shape= 21, size= 4, color= "black")+
  facet_grid(~Infection, labeller = as_labeller(c("2"= "PbNK65+\n(n=6-5)",
                                                  "4"= "PbNK65++\n(n=4-5)",
                                                  "8"= "PbNK65+++\n(n=5-5)")))+
  labs(x= "Pregnancy", y= "Peripheral parasitemia (%)") +
  scale_x_discrete(breaks = c("NO", "YES"),
                   labels = c("No", "Yes")) +
  scale_fill_manual(values = c("#ffffff", "#cb181d")) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by= 3),
                     expand = c(0,0)) +
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size= 14),
        axis.title.y = element_text(size= 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16))

ggsave("Plots/Caracterization/peri_para.tiff", width = 6, height = 5)


### porcentagem de pre termo #########################################

#computando o numero de observações da variavel
n_ptd <- model_data_dams %>%
  count(Infection, PTD)
n_ptd

# criando uma tabela de contingencia
ptd_rel_abun<- model_data_dams %>%
  group_by(Infection, PTD) %>%
  summarise(count= n()) %>%
  mutate(percent= (count / sum(count)) *100) %>%
  mutate(text_label = ifelse(PTD == "YES",
                             paste0(count, "/", sum(count)), "")) %>%
  dplyr::select(Infection, PTD, percent, text_label)
ptd_rel_abun

ggplot(ptd_rel_abun, aes(x= Infection, y= percent, fill= PTD)) +
  geom_col(width= 0.9, color= "black") +
  labs(x= NULL, y= "Percentage of the dams (%)") +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    values= c("#ffffff", "black"),
                    labels= c("Euthanasia", "Pre-term delivery")) +
  scale_x_discrete(breaks= c("0", "2", "4", "8"),
                   labels= c("PbNK65-\n(n=5)",
                             "PbNK65+\n(n=5)",
                             "PbNK65++\n(n=5)",
                             "PbNK65+++\n(n=5)")) +
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

ggsave("Plots/Caracterization/ptd.tiff", width = 6, height = 5)

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


### scatter plot e analise de correlação ##############################

# importando o dado
model_cor_pw_fw <- read_xlsx("Data/Model/model_cor_pw_fw.xlsx")
str(model_cor_pw_fw)


ggplot(model_cor_pw_fw, aes(x = Placenta_weight, y = Fetal_weight,
                            color = Infection)) +
  geom_point(size = 3) +  # Pontos de dispersão
  geom_smooth(method = "lm", aes(fill = Infection)) +  # Linha de regressão
  labs(x= "Placenta weight (mg)", y= "Fetal weight (g)")+
  scale_color_manual(name = NULL,
                     values = c("#636363","#cb181d"),
                     breaks = c("0", "8"),
                     labels = c("PbNK65 -", "PbNK65 +++"))+
  scale_fill_manual(name = NULL,
                    values = c("#636363","#cb181d"),
                    breaks = c("0", "8"),
                    labels = c("PbNK65 -", "PbNK65 +++"))+
  scale_y_continuous(limits= c(0.5, 1.3), breaks = seq(0.5, 1.3, by= 0.2),
                     expand = c(0,0))+
  scale_x_continuous(limits = c(50, 150), breaks = seq(50, 150, by= 25),
                     expand = c(0.025,0.025))+
  stat_cor(method = "spearman", label.x.npc = 0.6, 
           label.y.npc = 0.15)+ #analise de correlação
  theme_classic()+
  theme(legend.text = element_text(size= 11),
        axis.title.y = element_text(size= 20),
        axis.title.x = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16),
        legend.position = "top",
        legend.direction = "horizontal")

ggsave("Plots/Caracterization/model_cor_pw_fw.tiff", width= 6, height= 5)

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

### quantificação da sub unidade 18s de pb na placenta ##################

# importando o dado
model_pb18s_data <- read_xlsx("Data/Model/model_pb18s_data.xlsx")
str(model_pb18s_data)

#computando o numero de observações da variavel
n_pb18s <- model_pb18s_data %>%
  filter(!is.na(pb18s)) %>%
  group_by(Infection) %>%
  summarise(observacoes= sum(!is.na(pb18s)))
n_pb18s

ggplot(model_pb18s_data, aes(x= Infection, y= pb18s, fill= Infection)) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") +
  geom_quasirandom(show.legend = FALSE, size = 4, shape= 21, color= "black")+
  labs(x= NULL, y= "Pb18s mRNA (relative expression)") +
  scale_fill_manual(values= c("#fcae91", "#fb6a4a", "#cb181d")) +
  scale_x_discrete(breaks= c("2", "4", "8"),
                   labels= c("PbNK65+\n(n=7)",
                             "PbNK65++\n(n=10)",
                             "PbNK65+++\n(n=9)")) +
  scale_y_log10()+
  #scale_y_cut(breaks = c(10, 600), which = c(1, 2, 3))+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 15))

ggsave("Plots/Caracterization/pb18s.tiff", width= 6, height= 5)
