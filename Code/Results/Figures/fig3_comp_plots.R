##############################################################
### criação dos graficos dos dados relacionados a figura 3 ###

# pacotes solicitados para visualisação dos dados
library(tidyverse)
library(readxl)
library(ggbeeswarm)
library(ggsignif)

# importando o data set 
comp_data_dams <- read_xlsx("Data/Model_comparison/MODEL DATA_DAMS_COMP.xlsx")
str(comp_data_dams)

# transformando as variaveis infection e genotipo em fatores
comp_data_dams$Genotype <- as.factor(comp_data_dams$Genotype)
comp_data_dams$Infection <- as.factor(comp_data_dams$Infection)

# o grupo BNTac precisar ser o fator de referencia
comp_data_dams$Genotype <- relevel(comp_data_dams$Genotype, ref = "BNTac")
levels(comp_data_dams$Genotype)

# peripheral parasitemia data set
comp_parasitemia <- read_xlsx("Data/Model_comparison/PARASITEMIA_COMP.xlsx")
str(comp_parasitemia)

# transformando as variaveis genotype E preg em fatores
comp_parasitemia$Genotype <- as.factor(comp_parasitemia$Genotype)
comp_parasitemia$Preg <- as.factor(comp_parasitemia$Preg)

#o fator BNTac precisar ser o fator de referencia
comp_parasitemia$Genotype <- as.factor(comp_parasitemia$Genotype)
comp_parasitemia$Genotype <- relevel(comp_parasitemia$Genotype, ref = "BNT")
levels(comp_parasitemia$Genotype)

#criando uma função para calcular a mediana, iqr e os limites min e max
median_iqr <- function(x)
{
  data.frame(
    y= median(x),
    ymin= quantile(x, 0.25),
    ymax= quantile(x, 0.75)
  )
}

### contagem de eritrócitos (RBC) #######################################

# computando o numero de observações da variavel
n_rbc <- comp_data_dams %>% 
  filter(!is.na(RBC)) %>%
  group_by(Genotype, Infection) %>%
  summarise(observacoes= sum(!is.na(RBC)))
n_rbc

# customizando scatter dot plot para visualização da contagem de RBC
ggplot(comp_data_dams, aes(x= Genotype, y= RBC, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  geom_beeswarm(shape= 21, size= 4, color= "black",
                dodge.width = 0.8, cex = 4, show.legend = FALSE) +
  labs(x= NULL, y= "Red blood cells (10^6/uL)") +
  scale_x_discrete(breaks= c("BJ", "BNTac"),
                   labels= c("C57BL/6J\n(n=10-7)",
                             "C57BL/6NTac\n(n=11-9)")) +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#91BFDB", "#F1A340")) +
  scale_y_continuous(limits = c(2, 14), breaks = seq(2, 14, by= 3),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16))
 
  # geom_signif(y_position = c(12), xmin = c(0.8), 
   #           xmax = c(1.2), annotation = c("*"),
    #          textsize = 6, tip_length = 0.03, vjust = 0.3)

ggsave("Plots/Comparison/comp_rbc.tiff", width = 6, height = 5)


### contagem de hematocrito (HCT) #################################

# computando o numero de observações da variavel
n_hct <- comp_data_dams %>% 
  filter(!is.na(HCT)) %>%
  group_by(Genotype, Infection) %>%
  summarise(observacoes= sum(!is.na(HCT)))
n_hct

# customizando scatter dot plot para visualização da contagem de HCT
ggplot(comp_data_dams, aes(x= Genotype, y= HCT, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  geom_beeswarm(shape= 21, size= 4, color= "black",
                dodge.width = 0.8, cex = 4, show.legend = FALSE) +
  labs(x= NULL, y= "Hematocrit (%)") +
  scale_x_discrete(breaks= c("BJ", "BNTac"),
                   labels= c("C57BL/6J\n(n=10-7)",
                             "C57BL/6NTac\n(n=11-9)")) +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#91BFDB", "#F1A340")) +
  scale_y_continuous(limits = c(10, 60), breaks = seq(10, 60, by= 10),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) 

#  geom_signif(y_position = c(56), xmin = c(0.80), 
#              xmax = c(1.2), annotation = c("*"),
#              textsize = 6, tip_length = 0.03, vjust = 0.3)

ggsave("Plots/Comparison/comp_hct.tiff", width = 6, height = 5)

### contagem de hemoglobina (HGB) ##################################

# computando o numero de observações da variavel
n_hgb <- comp_data_dams %>% 
  filter(!is.na(HGB)) %>%
  group_by(Genotype, Infection) %>%
  summarise(observacoes= sum(!is.na(HGB)))
n_hgb

# customizando scatter dot plot para visualização da contagem de HGB
ggplot(comp_data_dams, aes(x= Genotype, y= HGB, fill= Infection)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  geom_beeswarm(shape= 21, size= 4, color= "black",
                dodge.width = 0.8, cex = 4, show.legend = FALSE) +
  labs(x= NULL, y= "Hemoglobin (g/dL)") +
  scale_x_discrete(breaks= c("BJ", "BNTac"),
                   labels= c("C57BL/6J\n(n=10-7)",
                             "C57BL/6NTac\n(n=11-9)")) +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    labels= c("Non-infected", "Infected"),
                    values = c("#91BFDB", "#F1A340")) +
  scale_y_continuous(limits = c(3, 23), breaks = seq(3, 23, by= 5),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16)) 

#  geom_signif(y_position = c(20), xmin = c(0.80), 
#              xmax = c(1.2), annotation = c("*"),
#              textsize = 6, tip_length = 0.03, vjust = 0.3)

ggsave("Plots/Comparison/comp_hgb.tiff", width = 6, height = 5)

### porcentagem de pre termo (PTD) ####################################

# calculando o numero de observações por grupo
n_ptd <- comp_data_dams %>%
  group_by(Genotype, Infection) %>%
  count(Infection, PTD)
n_ptd

# criando uma tabela de contingencia
ptd_rel_abun<- comp_data_dams %>%
  group_by(Infection, Genotype, PTD) %>%
  summarise(count= n()) %>%
  mutate(percent= (count / sum(count)) *100) %>%
  mutate(text_label = ifelse(PTD == "YES",
                             paste0(count, "/", sum(count)), "")) %>%
  dplyr::select(Genotype, Infection, PTD, percent, text_label)
ptd_rel_abun

ggplot(ptd_rel_abun, aes(x= Infection, y= percent, fill= PTD)) +
  geom_col(width= 0.9, color= "black") +
  facet_wrap(~Genotype, labeller = as_labeller(c("BJ"= "C57BL/6J",
                                                 "BNTac"= "C57BL/6NTac"))) +
  scale_x_discrete(breaks = c("NO", "YES"), labels = c("No", "Yes")) +
  labs(x= "Infection", y= "Percentage of the dams (%)") +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    values= c("white", "black"),
                    labels= c("Euthanasia", "Pre-term delivery")) +
  scale_y_continuous(limits = c(0, 100.5), breaks = seq(0, 100.5, by= 20),
                     expand = c(0,0)) +
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size= 13),
        legend.text = element_text(size= 13),
        axis.title = element_text(size= 20),
        axis.text = element_text(size= 16)) +
  geom_text(aes(label = text_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 5)

ggsave("Plots/Comparison/comp_ptd.tiff", width = 6, height = 5)

### percentual de parasitemia periferica ##############################

# criando um atabela com o numero de observações
n_parasitemia <- comp_parasitemia %>%
  filter(!is.na(Parasitemia)) %>%
  group_by(Preg, Genotype) %>%
  summarise(observacoes= sum(!is.na(Parasitemia)))
n_parasitemia

ggplot(comp_parasitemia, aes(x= Genotype, y= Parasitemia, fill= Preg)) +
  stat_summary(fun.data = median_iqr, geom = "errorbar", color= "black",
               width= 0.3, show.legend = FALSE,
               position = position_dodge(0.8)) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black",
               position = position_dodge(0.8)) +
  geom_beeswarm(shape= 21, size= 4, color= "black", dodge.width = 0.8, cex = 4) +
  labs(x= NULL, y= "Peripheral parasitemia (%)") +
  scale_x_discrete(breaks= c("BJ", "BNT"),
                   labels= c("C57BL/6J\n(n=5-8)",
                             "C57BL/6NTac\n(n=4-9)")) +
  scale_fill_manual(name= NULL,
                    breaks = c("NO", "YES"),
                    values= c("#cccccc", "#F1A340"),
                    labels= c("Non-pregnant", "Pregnant"))+
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by= 3),
                     expand = c(0,0)) +
  theme_classic() +
  theme(legend.position = c(0.185, 0.925),
        legend.text = element_text(size= 13),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size= 16),
        axis.text.y = element_text(size= 16)) 

#  geom_signif(y_position = c(9,11), xmin = c(0.8,1.8), 
#              xmax = c(1.2,2.2), annotation = c("**","***"),
#              textsize = 6, tip_length = 0.03, vjust = 0.3) 

ggsave("Plots/Comparison/comp_parasitemia.tiff", width = 6, height = 5)

### Pb18s expression ###################################################
n_pb18s_comp <- pb18s_data %>%
  group_by(Genotype) %>%
  summarise(observacoes= n_distinct(pb18s))
n_pb18s_comp

ggplot(pb18s_data, aes(x= Genotype, y= pb18s, fill= Genotype)) +
  stat_summary(fun= median, show.legend = FALSE, geom = "crossbar",
               width= 0.5, color= "black") +
  geom_quasirandom(show.legend = FALSE, size = 4, shape= 21, color= "black")+
  labs(x= NULL, y= "Pb18s mRNA (relative expression)") +
  scale_fill_manual(values= c("#fcae91", "#cccccc")) +
  scale_x_discrete(breaks= c("BJ", "BNTac"),
                   labels= c("C57BL/6J\n(n=10)",
                             "C57BL/6NTac\n(n=13)")) +
  scale_y_log10()+
  theme_classic()+
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size= 16),
        axis.text.x = element_text(size= 16))

ggsave("Plots/Comparison/pb18s.tiff", width= 6, height= 5)
