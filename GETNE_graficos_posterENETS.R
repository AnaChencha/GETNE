#Elaborado por: Ana Elsa Huerta Hernández

#Gráficos para el congreso ENETS

library(readxl)
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(broom)
library(car)
library(rstatix)
library(ggpubr)
library(cutpointr)


GETNE <- read_xlsx("M:\\Proyecto NET\\GETNE\\Datasets\\Datasets_report.xlsx", sheet = "GETNE_DB",
                   col_names = TRUE)

GETNE_graficos <- GETNE %>% 
  filter(Cardiopatia_carcinoide == "No" | Cardiopatia_alDx == "No") %>% 
  mutate(Cardiopatia_carcinoide = case_when(Cardiopatia_carcinoide == "Si" ~ "CHD Yes", 
                                            Cardiopatia_carcinoide == "No" ~ "CHD No"),
         Cardiopatia_carcinoide = factor(Cardiopatia_carcinoide, levels = c("CHD No", "CHD Yes")))

Tabla_GETNE_TAC2 <- GETNE_graficos %>%
  select(Diametro_AD_TAC_BASAL, Diametro_AI_TAC_BASAL, Diametro_VD_TAC_BASAL, Diametro_VI_TAC_BASAL,
         RATIO_AD_AI_TAC_BASAL, RATIO_VD_VI_TAC_BASAL, Cardiopatia_carcinoide) %>% 
  tbl_summary(by = Cardiopatia_carcinoide, type = list(where(is.numeric) ~ "continuous",
                                                       all_dichotomous() ~ "categorical"),
              missing = "no") %>%
  add_p()
Tabla_GETNE_TAC2

View(GETNE_graficos)

str(GETNE_graficos)

GETNE_graficos %>%  count(Cardiopatia_carcinoide)

my_comparisons <- list(c("CHD No", "CHD Yes"))

setwd("M:/Proyecto NET/GETNE/Graficos_ENETS") #Fijar directorio
#dir.create("Graficos_ENETS/") #Para crear una carpeta

Basal_rv_lv_ratio <- 
  ggplot(GETNE_graficos, aes(x=Cardiopatia_carcinoide, y=RATIO_VD_VI_TAC_BASAL, fill = Cardiopatia_carcinoide))+
  geom_dotplot(binaxis="y", stackdir = "center", dotsize = 1)+
  geom_boxplot(width = 0.6, fill = "NA", outlier.color = "NA")+
  scale_fill_manual(values = c("gray", "orange"))+
  labs(title = "", x= "", y = "Basal RV/LV ratio")+
  stat_compare_means(comparisons = my_comparisons, size = 7.5, paired = FALSE, 
                     method = "t.test", label = "p.signif")+
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18, face = "bold"),   
        aspect.ratio = 1.5, legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = -0.5)) 

ggsave("Basal_rv_lv_ratio.png")

Basal_ra_la_ratio <- 
  ggplot(GETNE_graficos, aes(x=Cardiopatia_carcinoide, y=RATIO_AD_AI_TAC_BASAL, fill = Cardiopatia_carcinoide))+
  geom_dotplot(binaxis="y", stackdir = "center", dotsize = 1)+
  geom_boxplot(width = 0.6, fill = "NA", outlier.color = "NA")+
  scale_fill_manual(values = c("gray", "orange"))+
  labs(title = "", x= "", y = "Basal RA/LA ratio")+
  stat_compare_means(comparisons = my_comparisons, size = 7.5, method = "t.test", 
                     label = "p.signif", paired = FALSE)+
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18, face = "bold"),   
        aspect.ratio = 1.5, legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = -0.5))

ggsave("Basal_ra_la_ratio.png")

Basal_rv <- 
  ggplot(GETNE_graficos, aes(x=Cardiopatia_carcinoide, y=Diametro_VD_TAC_BASAL, fill = Cardiopatia_carcinoide))+
  geom_dotplot(binaxis="y", stackdir = "center", dotsize = 1)+
  geom_boxplot(width = 0.6, fill = "NA", outlier.color = "NA")+
  scale_fill_manual(values = c("gray", "orange"))+
  labs(title = "", x= "", y = "Basal RV diameter")+
  stat_compare_means(comparisons = my_comparisons, size = 7.5, method = "t.test", 
                     label = "p.signif", paired = FALSE)+
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18, face = "bold"),   
        aspect.ratio = 1.5, legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = -0.5))

ggsave("Basal_rv.png")

Basal_lv <-
ggplot(GETNE_graficos, aes(x=Cardiopatia_carcinoide, y=Diametro_VI_TAC_BASAL, fill = Cardiopatia_carcinoide))+
  geom_dotplot(binaxis="y", stackdir = "center", dotsize = 1)+
  geom_boxplot(width = 0.6, fill = "NA", outlier.color = "NA")+
  scale_fill_manual(values = c("gray", "orange"))+
  labs(title = "", x= "", y = "Basal LV diameter")+
  stat_compare_means(comparisons = my_comparisons, size = 7.5, method = "t.test", 
                     label = "p.signif", paired = FALSE)+
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18, face = "bold"),   
        aspect.ratio = 1.5, legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = -0.5))

ggsave("Basal_lv.png")


GETNE_wide <- read_xlsx("M:\\Proyecto NET\\GETNE\\Datasets\\Datasets_report.xlsx", sheet = "GETNE_wide",
                        col_names = TRUE)

GETNE_wide_filter <- GETNE_wide %>% 
  filter(Cardiopatia_alDx == "No") %>% 
  mutate(moment = factor(moment, levels = c("BASAL", "Dx_Cardiopatia", "CONTROL"),
                         labels = c("Basal", "CHD Dx", "Control")))



#Friedman test------

##Diametro VD-------

Friedman_diametro_VD <- GETNE_wide_filter %>% friedman_test(Diametro_VD ~ moment | Numero_registro)

pwc_Diametro_VD <- GETNE_wide_filter %>% 
  wilcox_test(Diametro_VD ~ moment, paired = TRUE, p.adjust.method = "bonferroni")

pwc_Diametro_VD <- pwc_Diametro_VD %>%  add_y_position()

##Diametro VI--------

Friedman_diametro_VI <- GETNE_wide_filter %>% friedman_test(Diametro_VI ~ moment | Numero_registro)

pwc_Diametro_VI <- GETNE_wide_filter %>% 
  wilcox_test(Diametro_VI ~ moment, paired = TRUE, p.adjust.method = "bonferroni")

pwc_Diametro_VI <- pwc_Diametro_VI %>%  add_y_position()

##Diametro AI-------

Friedman_diametro_AI <- GETNE_wide_filter %>% friedman_test(Diametro_AI ~ moment | Numero_registro)

pwc_Diametro_AI <- GETNE_wide_filter %>% 
  wilcox_test(Diametro_AI ~ moment, paired = TRUE, p.adjust.method = "bonferroni")

pwc_Diametro_AI <- pwc_Diametro_AI %>%  add_y_position()

##Diametro AD------

Friedman_diametro_AD <- GETNE_wide_filter %>% friedman_test(Diametro_AD ~ moment | Numero_registro)

pwc_Diametro_AD <- GETNE_wide_filter %>% 
  wilcox_test(Diametro_AD ~ moment, paired = TRUE, p.adjust.method = "bonferroni")

pwc_Diametro_AD <- pwc_Diametro_AD %>%  add_y_position()

##Ratio AD_AI------
Friedman_RATIO_AD_AI <- GETNE_wide_filter %>% friedman_test(RATIO_AD_AI ~ moment | Numero_registro)

pwc_RATIO_AD_AI <- GETNE_wide_filter %>% 
  wilcox_test(RATIO_AD_AI ~ moment, paired = TRUE, p.adjust.method = "bonferroni")

pwc_RATIO_AD_AI <- pwc_RATIO_AD_AI %>%  add_y_position()

##Ratio VD_VI---------
Friedman_RATIO_VD_VI <- GETNE_wide_filter %>% friedman_test(RATIO_VD_VI ~ moment | Numero_registro)

pwc_RATIO_VD_VI <- GETNE_wide_filter %>% 
  wilcox_test(RATIO_VD_VI ~ moment, paired = TRUE, p.adjust.method = "bonferroni")

pwc_RATIO_VD_VI <- pwc_RATIO_VD_VI %>%  add_y_position()

setwd("M:/Proyecto NET/GETNE/Graficos_ENETS")

Din_diametro_VD <- ggplot(GETNE_wide_filter, aes(x=moment, y=Diametro_VD, fill = moment))+
  geom_dotplot(binaxis="y", stackdir = "center", dotsize = 1)+
  geom_boxplot(width = 0.6, fill = "NA", outlier.color = "NA")+
  scale_fill_manual(values = c("orange", "orange3", "gold"))+
  geom_line(aes(group = Numero_registro), color = "gray70") +
  labs(title = "", x= "", y = "RV diameter",
       subtitle = get_test_label(Friedman_diametro_VD, detailed = FALSE))+
  stat_pvalue_manual(pwc_Diametro_VD, hide.ns = TRUE, size = 7.5) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18, face = "bold"),   
        aspect.ratio = 1.5, legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = -0.5),
        plot.subtitle = element_text(size = 20, face = "bold"))

ggsave("Din_diametro_VD.png")




Din_diametro_VI <- ggplot(GETNE_wide_filter, aes(x=moment, y=Diametro_VI, fill = moment))+
  geom_dotplot(binaxis="y", stackdir = "center", dotsize = 1)+
  geom_boxplot(width = 0.6, fill = "NA", outlier.color = "NA")+
  scale_fill_manual(values = c("orange", "orange3", "gold"))+
  geom_line(aes(group = Numero_registro), color = "gray70") +
  labs(title = "", x= "", y = "LV diameter",
       subtitle = get_test_label(Friedman_diametro_VI, detailed = FALSE))+
  stat_pvalue_manual(pwc_Diametro_VI, hide.ns = TRUE, size = 7.5) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18, face = "bold"),   
        aspect.ratio = 1.5, legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = -0.5),
        plot.subtitle = element_text(size = 20, face = "bold"))

ggsave("Din_diametro_VI.png")



Din_diametro_AI <- ggplot(GETNE_wide_filter, aes(x=moment, y=Diametro_AI, fill = moment))+
  geom_dotplot(binaxis="y", stackdir = "center", dotsize = 1)+
  geom_boxplot(width = 0.6, fill = "NA", outlier.color = "NA")+
  scale_fill_manual(values = c("orange", "orange3", "gold"))+
  geom_line(aes(group = Numero_registro), color = "gray70") +
  labs(title = "", x= "", y = "LA diameter",
       subtitle = get_test_label(Friedman_diametro_AI, detailed = FALSE))+
  stat_pvalue_manual(pwc_Diametro_AI, hide.ns = TRUE, size = 7.5) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18, face = "bold"),   
        aspect.ratio = 1.5, legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = -0.5),
        plot.subtitle = element_text(size = 20, face = "bold"))

ggsave("Din_diametro_AI.png")




Din_diametro_AD <- ggplot(GETNE_wide_filter, aes(x=moment, y=Diametro_AD, fill = moment))+
  geom_dotplot(binaxis="y", stackdir = "center", dotsize = 1)+
  geom_boxplot(width = 0.6, fill = "NA", outlier.color = "NA")+
  scale_fill_manual(values = c("orange", "orange3", "gold"))+
  geom_line(aes(group = Numero_registro), color = "gray70") +
  labs(title = "", x= "", y = "RA diameter",
       subtitle = get_test_label(Friedman_diametro_AD, detailed = FALSE))+
  stat_pvalue_manual(pwc_Diametro_AD, hide.ns = TRUE, size = 7.5) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18, face = "bold"),   
        aspect.ratio = 1.5, legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = -0.5),
        plot.subtitle = element_text(size = 20, face = "bold"))

ggsave("Din_diametro_AD.png")



Din_ratio_ad_ai <- ggplot(GETNE_wide_filter, aes(x=moment, y=RATIO_AD_AI, fill = moment))+
  geom_dotplot(binaxis="y", stackdir = "center", dotsize = 1)+
  geom_boxplot(width = 0.6, fill = "NA", outlier.color = "NA")+
  scale_fill_manual(values = c("orange", "orange3", "gold"))+
  geom_line(aes(group = Numero_registro), color = "gray70") +
  labs(title = "", x= "", y = "RA/LA ratio",
       subtitle = get_test_label(Friedman_RATIO_AD_AI, detailed = FALSE))+
  stat_pvalue_manual(pwc_RATIO_AD_AI, hide.ns = TRUE, size = 7.5) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18, face = "bold"),   
        aspect.ratio = 1.5, legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = -0.5),
        plot.subtitle = element_text(size = 20, face = "bold"))

ggsave("Din_ratio_ad_ai.png")




Din_ratio_vd_vi <- ggplot(GETNE_wide_filter, aes(x=moment, y=RATIO_VD_VI, fill = moment))+
  geom_dotplot(binaxis="y", stackdir = "center", dotsize = 1)+
  geom_boxplot(width = 0.6, fill = "NA", outlier.color = "NA")+
  scale_fill_manual(values = c("orange", "orange3", "gold"))+
  geom_line(aes(group = Numero_registro), color = "gray70") +
  labs(title = "", x= "", y = "RV/LV ratio",
       subtitle = get_test_label(Friedman_RATIO_VD_VI, detailed = FALSE))+
  stat_pvalue_manual(pwc_RATIO_VD_VI, hide.ns = TRUE, size = 7.5) +
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x = element_text(size = 18),  
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18, face = "bold"),   
        aspect.ratio = 1.5, legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = -0.5),
        plot.subtitle = element_text(size = 20, face = "bold"))

ggsave("Din_ratio_vd_vi.png")


GETNE_filter <- GETNE %>% 
  filter(Cardiopatia_carcinoide == "No" | Cardiopatia_alDx == "No") %>% 
  mutate(Cardiopatia_carcinoide = factor(Cardiopatia_carcinoide),
         Inclinacion_septo_ventricular_TAC_BASAL = factor(Inclinacion_septo_ventricular_TAC_BASAL))

CP2 <- cutpointr(GETNE_filter, RATIO_VD_VI_TAC_BASAL, Cardiopatia_carcinoide,
                 method = maximize_metric, metric = sum_sens_spec)

summary(CP2)

plot(CP2)

CP2_b <- cutpointr(GETNE_filter, RATIO_VD_VI_TAC_BASAL, Cardiopatia_carcinoide, Sexo,
                   boot_runs = 0, boot_stratify = TRUE, pos_class = "Si")

CP2_b %>%  select(subgroup, AUC, optimal_cutpoint, sum_sens_spec, acc, sensitivity, specificity, data)

summary(CP2_b)

plot(CP2_b)
