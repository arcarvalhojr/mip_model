#####################################################################
###  criação dos graficos dos dados relacionados a figura sup 2 #####

# pacotes solicitados para as analises
library(tidyverse)
library(readxl)
library(ggtext)
library(ggbeeswarm)
library(ggsignif)

### importando os data sets #######################

# dams data set
comp_data_dams <- read_xlsx("Data/Model_comparison/MODEL DATA_DAMS_COMP.xlsx")
str(comp_data_dams)

# transformando as variaveis infection e genotipo em fatores
comp_data_dams$Genotype <- as.factor(comp_data_dams$Genotype)
comp_data_dams$Infection <- as.factor(comp_data_dams$Infection)

# o grupo BNTac precisar ser o fator de referencia
comp_data_dams$Genotype <- relevel(comp_data_dams$Genotype, ref = "BNTac")
levels(comp_data_dams$Genotype)

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


#criando uma função para calcular a mediana, iqr e os limites
median_iqr <- function(x)
{
  data.frame(
    y= median(x),
    ymin= quantile(x, 0.25),
    ymax= quantile(x, 0.75)
  )
}


### peso do baço ######################################################

#computando o numero de observações da variavel
n_comp_sw <- comp_data_dams %>% 
  filter(!is.na(Spleen_w)) %>%
  group_by(Genotype, Infection) %>%
  summarise(observacoes= sum(!is.na(Spleen_w)))
n_comp_sw

#customizando scatter dot plot para visualização do peso do baço
ggplot(comp_data_dams, aes(x= Genotype, y= Spleen_w, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  geom_beeswarm(shape= 21, size= 4, color= "black", dodge.width = 0.8, cex = 4,
                show.legend = FALSE) +
  labs(x= NULL, y= "Spleen Weight (mg)") +
  scale_x_discrete(breaks= c("BJ", "BNTac"),
                   labels= c("C57BL/6J\n(n=10-8)",
                             "C57BL/6NTac\n(n=11-9)")) +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    values= c("#91BFDB", "#F1A340"),
                    labels= c("Non-infected", "Infected"))+
  scale_y_continuous(limits = c(50, 350), breaks = seq(50, 350, by= 100),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size= 16),
        axis.text.y = element_text(size= 16)) 

#  geom_signif(y_position = c(325,325), xmin = c(0.8,1.8), 
#              xmax = c(1.2,2.2), annotation = c("***","***"),
#              textsize = 6, tip_length = 0.03, vjust = 0.3) 

ggsave("Plots/Comparison/comp_sw.tiff", width = 6, height = 5)

### contagem de leucocitos (WBC) ######################################

#removendo outliers
no_outs <- comp_data_dams %>%
  filter(!(Genotype == "BJ" & Infection == "YES" & WBC == 15.2)) %>%
  dplyr::select(Genotype, Mice_ID, Infection, WBC)

#computando o numero de observações da variavel
n_comp_wbc <- no_outs %>% 
  filter(!is.na(WBC)) %>%
  group_by(Genotype, Infection) %>%
  summarise(observacoes= sum(!is.na(WBC)))
n_comp_wbc

#customizando scatter dot plot para visualização da contagem de WBC
ggplot(no_outs, aes(x= Genotype, y= WBC, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  geom_beeswarm(shape= 21, size= 4, color= "black",
                dodge.width = 0.8, cex = 4, show.legend = FALSE) +
  labs(x= NULL, y= "Leucocytes (10^3/uL)") +
  scale_x_discrete(breaks= c("BJ", "BNTac"),
                   labels= c("C57BL/6J\n(n=10-6)",
                             "C57BL/6NTac\n(n=11-9)")) +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#91BFDB", "#F1A340")) +
  scale_y_continuous(limits = c(0.6, 4.6), breaks = seq(0.6, 4.6, by= 1),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) 

#  geom_signif(y_position = c(4.2), xmin = c(0.8), 
#              xmax = c(1.2), annotation = c("**"),
#              textsize = 6, tip_length = 0.03, vjust = 0.3) 

ggsave("Plots/Comparison/comp_wbc.tiff", width = 6, height = 5)


### Linfocitos #######################################################

#computando o numero de observações da variavel
n_comp_lymph <- comp_data_dams %>% 
  filter(!is.na(LIN)) %>%
  group_by(Genotype, Infection) %>%
  summarise(observacoes= sum(!is.na(LIN)))
n_comp_lymph

#customizando scatter dot plot para visualização da porcentagem de linfocitos
ggplot(comp_data_dams, aes(x= Genotype, y= LIN, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  geom_beeswarm(shape= 21, size= 4, color= "black",
                dodge.width = 0.8, cex = 4, show.legend = FALSE) +
  labs(x= NULL, y= "Lymphocytes (%)") +
  scale_x_discrete(breaks= c("BJ", "BNTac"),
                   labels= c("C57BL/6J\n(n=10-7)",
                             "C57BL/6NTac\n(n=11-9)")) +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#91BFDB", "#F1A340")) +
  scale_y_continuous(limits = c(20, 100), breaks = seq(20, 100, by= 20),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) 

#  geom_signif(y_position = c(90, 90), xmin = c(0.8, 1.8), 
 #             xmax = c(1.2, 2.2), annotation = c("**", "**"),
#              textsize = 6, tip_length = 0.03, vjust = 0.3)

ggsave("Plots/Comparison/comp_lin.tiff", width = 6, height = 5)

### granulocitos #######################################################

#computando o numero de observações da variavel
n_comp_gran <- comp_data_dams %>% 
  filter(!is.na(GRAN)) %>%
  group_by(Genotype, Infection) %>%
  summarise(observacoes= sum(!is.na(GRAN)))
n_comp_gran

#customizando scatter dot plot para visualização da porcentagem de granulocitos
ggplot(comp_data_dams, aes(x= Genotype, y= GRAN, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  geom_beeswarm(shape= 21, size= 4, color= "black",
                dodge.width = 0.8, cex = 4, show.legend = FALSE) +
  labs(x= NULL, y= "Granulocytes (%)") +
  scale_x_discrete(breaks= c("BJ", "BNTac"),
                   labels= c("C57BL/6J\n(n=10-7)",
                             "C57BL/6NTac\n(n=11-9)")) +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#91BFDB", "#F1A340")) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by= 20),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) 

#  geom_signif(y_position = c(70, 70), xmin = c(0.8, 1.8), 
#              xmax = c(1.2, 2.2), annotation = c("**", "**"),
#              textsize = 6, tip_length = 0.03, vjust = 0.3)

ggsave("Plots/Comparison/comp_gran.tiff", width = 6, height = 5)

### monocitos #################################################

#computando o numero de observações da variavel
n_comp_mon <- comp_data_dams %>% 
  filter(!is.na(MON)) %>%
  group_by(Genotype, Infection) %>%
  summarise(observacoes= sum(!is.na(MON)))
n_comp_mon

#customizando scatter dot plot para visualização da porcentagem de monocitos
ggplot(comp_data_dams, aes(x= Genotype, y= MON, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  geom_beeswarm(shape= 21, size= 4, color= "black",
                dodge.width = 0.8, cex = 4, show.legend = FALSE) +
  labs(x= NULL, y= "Monocytes (%)") +
  scale_x_discrete(breaks= c("BJ", "BNTac"),
                   labels= c("C57BL/6J\n(n=10-7)",
                             "C57BL/6NTac\n(n=11-9)")) +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#91BFDB", "#F1A340")) +
  scale_y_continuous(limits = c(1, 13), breaks = seq(1, 13, by= 3),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) 

#  geom_signif(y_position = c(12.2, 12.2), xmin = c(0.8, 1.8), 
#              xmax = c(1.2, 2.2), annotation = c("**", "*"),
#              textsize = 6, tip_length = 0.03, vjust = 0.3)

ggsave("Plots/Comparison/comp_mon.tiff", width = 6, height = 5)

### Stillbirth (viabilidade fetal)#####################################

#selecionando as variaveis
stb_comp_data <- comp_data_weight %>%
  select(Genotype, Mice_ID, Fetus_ID, Infection, Stillbirth) %>%
  filter(!is.na(Stillbirth) & Stillbirth != "" & Stillbirth != "NA")

#calculando o numero de obvservações
n_stb <- stb_comp_data %>%
  filter(!is.na(Stillbirth)) %>%
  group_by(Genotype, Infection) %>%
  count(Stillbirth)
n_stb

#Calculando proporções para cada grupo de genotipo e infecção
stb_rel_abun<- stb_comp_data %>%
  group_by(Genotype, Infection, Stillbirth) %>%
  summarise(count= n()) %>%
  mutate(percent= (count / sum(count)) *100) %>%
  mutate(text_label = ifelse(Stillbirth == "NO",
                             paste0(count, "/", sum(count)), "")) %>%
  dplyr::select(Genotype, Infection, Stillbirth, percent, text_label)
stb_rel_abun

#stacked bar plot
ggplot(stb_rel_abun, aes(x= Infection, y= percent, fill= Stillbirth)) +
  geom_col(width= 0.8, color= "black") +
  facet_wrap(~Genotype, labeller = as_labeller(c(
    BNTac= "C57BL/6NTac", BJ= "C57BL/6J")))+
  labs(x= "Infection", y= "Percentage of the fetus (%)")+
  scale_x_discrete(breaks= c("NO", "YES"),
                   labels= c("No", "Yes")) +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    values= c("black", "#ffffff"),
                    labels= c("Non-viable", "Viable"))+
  scale_y_continuous(limits = c(0, 100.5), breaks = seq(0, 100.5, by= 20),
                     expand = c(0,0)) +
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size= 15),
        legend.position = "top",
        legend.text = element_text(size= 13),
        axis.title = element_text(size= 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) +
  geom_text(aes(label = text_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 5)

ggsave("Plots/Comparison/comp_stb.tiff", width= 7, height= 6)   


### Peso das placentas##################################################

#criando uma tabela com o n de observações por camundongo por grupo
n_pw_comp <- comp_data_weight %>% 
  filter(!is.na(Placenta_weight)) %>%
  group_by(Genotype, Infection) %>%
  summarise(observacoes= n_distinct(Mice_ID)) #conta o número de valores únicos na variavel
n_pw_comp

#customizando boxplot para visualização do peso das placentas
ggplot(comp_data_weight, aes(x= Genotype, y= Placenta_weight, fill= Infection))+
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, width= 0.8)+
  geom_quasirandom(show.legend = FALSE, size = 1.5, dodge.width = 0.8,
                   cex = 4)+
  labs(x= NULL, y= "Placental weight (mg)")+
  scale_x_discrete(breaks= c("BJ", "BNTac"),
                   labels= c("C57BL/6J\n(n=9-5)",
                             "C57BL/6NTac\n(n=9-6)"))+
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#91BFDB", "#F1A340"))+
  scale_y_continuous(limits = c(50, 250), breaks = seq(50, 250, by= 50),
                     expand = c(0,0))+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16))

ggsave("Plots/Comparison/comp_pw.tiff", width = 6, height = 5)
