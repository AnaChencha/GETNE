#ANÁLISIS BASE DE DATOS PROYECTO GETNE
#Fecha análisis: 22/10/2025
#Elaborado por: Ana Elsa Huerta

#Cargar paquetes-----
install.packages("cutpointr")

library(readxl)
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(broom)
library(car)
library(rstatix)
library(ggpubr)
library(cutpointr)
library(here)


here()


#BD------

GETNE <- read_xlsx(
  here("Base cardiopatia carcinoide completa_analisis.xlsx"),
                   col_names = TRUE)


str(GETNE)
View(GETNE_DB[c("Numero_registro", "Cardiopatia_carcinoide", "Cardiopatia_alDx", "Diametro_AD_TAC_Cardiopatia",
                "Diametro_AD_TAC_Dx_Cardiopatia", "Diametro_AD_TAC_BASAL", "Inclinacion_septo_ventricular")])

GETNE_DB <- GETNE %>% 
  mutate(
    Sexo = factor(Sexo),
    Estado_general = factor(Estado_general),
    Metastasis_hepaticas = factor(Metastasis_hepaticas, labels = c("No", "Si")),
    Metastasis_peritoneales = factor(Metastasis_peritoneales, labels = c("No", "Si")),
    Metastasis_ganglionares = factor(Metastasis_ganglionares, labels = c("No", "Si")),
    Ki_67 = factor(Ki_67),
    Cromogranina_A_categ = factor(Cromogranina_A_categ, labels = c("normal", "elevada")),
    Inclinacion_septo_ventricular_TAC_BASAL = factor(Inclinacion_septo_ventricular_TAC_BASAL),
    Tiempo_CONTROL = factor(Tiempo_CONTROL),
    Inlinacion_septo_ventricular_TAC_CONTROL = factor(Inlinacion_septo_ventricular_TAC_CONTROL),
    Cardiopatia_carcinoide = factor(Cardiopatia_carcinoide),
    Cardiopatia_alDx = factor(Cardiopatia_alDx),
    Inclinacion_septo_ventricular_TAC_Dx_Cardiopatia = factor(Inclinacion_septo_ventricular_TAC_Dx_Cardiopatia),
    CX_VALVULAR = factor(CX_VALVULAR, labels = c("No", "Si")),
    
    #Lo siguiente es para generar unas columnas que me permitan comparar en general cardiopatia si vs no de los parámetros del TAC
    Diametro_AD_TAC_Cardiopatia = case_when(
      Cardiopatia_alDx == "Si" | Cardiopatia_carcinoide == "No" ~ Diametro_AD_TAC_BASAL,
      Cardiopatia_alDx == "No" & Cardiopatia_carcinoide == "Si" ~ Diametro_AD_TAC_Dx_Cardiopatia),
    Diametro_AI_TAC_Cardiopatia = case_when(
      Cardiopatia_alDx == "Si" | Cardiopatia_carcinoide == "No" ~ Diametro_AI_TAC_BASAL,
      Cardiopatia_alDx == "No" & Cardiopatia_carcinoide == "Si" ~ Diametro_AI_TAC_Dx_Cardiopatia),
    Diametro_VD_TAC_Cardiopatia = case_when(
      Cardiopatia_alDx == "Si" | Cardiopatia_carcinoide == "No" ~ Diametro_VD_TAC_BASAL,
      Cardiopatia_alDx == "No" & Cardiopatia_carcinoide == "Si" ~ Diametro_VD_TAC_Dx_Cardiopatia),
    Diametro_VI_TAC_Cardiopatia = case_when(
      Cardiopatia_alDx == "Si" | Cardiopatia_carcinoide == "No" ~ Diametro_VI_TAC_BASAL,
      Cardiopatia_alDx == "No" & Cardiopatia_carcinoide == "Si" ~ Diametro_VI_TAC_Dx_Cardiopatia),
    RATIO_AD_AI_TAC_Cardiopatia = case_when(
      Cardiopatia_alDx == "Si" | Cardiopatia_carcinoide == "No" ~ RATIO_AD_AI_TAC_BASAL,
      Cardiopatia_alDx == "No" & Cardiopatia_carcinoide == "Si" ~ RATIO_AD_AI_TAC_Dx_Cardiopatia),
    RATIO_VD_VI_TAC_Cardiopatia = case_when(
      Cardiopatia_alDx == "Si" | Cardiopatia_carcinoide == "No" ~ RATIO_VD_VI_TAC_BASAL,
      Cardiopatia_alDx == "No" & Cardiopatia_carcinoide == "Si" ~ RATIO_VD_VI_TAC_Dx_Cardiopatia),
    Inclinacion_septo_ventricular = case_when(
      Cardiopatia_alDx == "Si" | Cardiopatia_carcinoide == "No" ~ Inclinacion_septo_ventricular_TAC_BASAL,
      Cardiopatia_alDx == "No" & Cardiopatia_carcinoide == "Si" ~ Inclinacion_septo_ventricular_TAC_Dx_Cardiopatia)
         ) %>% 
  
  mutate(
    Dif_diametroAD_TAC = case_when(
      Tiempo_CONTROL == "post-diagnostico" ~ (Diametro_AD_TAC_Dx_Cardiopatia - 
                                                Diametro_AD_TAC_BASAL)),
    Dif_diametro_AI_TAC = case_when(
      Tiempo_CONTROL == "post-diagnostico" ~ (Diametro_AI_TAC_Dx_Cardiopatia - 
                                                Diametro_AI_TAC_BASAL)), 
    Dif_diametro_VD_TAC = case_when(
      Tiempo_CONTROL == "post-diagnostico" ~ (Diametro_VD_TAC_Dx_Cardiopatia - 
                                                Diametro_VD_TAC_BASAL)),
    Dif_diametro_VI_TAC = case_when(
      Tiempo_CONTROL == "post-diagnostico" ~ (Diametro_VI_TAC_Dx_Cardiopatia - 
                                                Diametro_VI_TAC_BASAL)),
    Dif_RATIO_AD_AI_TAC = case_when(
      Tiempo_CONTROL == "post-diagnostico" ~ (RATIO_AD_AI_TAC_Dx_Cardiopatia - 
                                                RATIO_AD_AI_TAC_BASAL)),
    Dif_RATIO_VD_VI_TAC = case_when(
      Tiempo_CONTROL == "post-diagnostico" ~ (RATIO_VD_VI_TAC_Dx_Cardiopatia - 
                                                RATIO_VD_VI_TAC_BASAL)),
    Dif_diametroAD_TAC_control = case_when(
      Tiempo_CONTROL == "post-diagnostico" ~ (Diametro_AD_TAC_Dx_Cardiopatia - 
                                                Diametro_AD_TAC_CONTROL)),
    Dif_diametro_AI_TAC_control = case_when(
      Tiempo_CONTROL == "post-diagnostico" ~ (Diametro_AI_TAC_Dx_Cardiopatia - 
                                                Diametro_AI_TAC_CONTROL)),
    Dif_diametro_VD_TAC_control = case_when(
      Tiempo_CONTROL == "post-diagnostico" ~ (Diametro_VD_TAC_Dx_Cardiopatia - 
                                                Diametro_VD_TAC_CONTROL)),
    Dif_diametro_VI_TAC_control = case_when(
      Tiempo_CONTROL == "post-diagnostico" ~ (Diametro_VI_TAC_Dx_Cardiopatia - 
                                                Diametro_VI_TAC_CONTROL)),
    Dif_RATIO_AD_AI_TAC_control = case_when(
      Tiempo_CONTROL == "post-diagnostico" ~ (RATIO_AD_AI_TAC_Dx_Cardiopatia - 
                                                RATIO_AD_AI_TAC_CONTROL)),
    Dif_RATIO_VD_VI_TAC_control = case_when(
      Tiempo_CONTROL == "post-diagnostico" ~ (RATIO_VD_VI_TAC_Dx_Cardiopatia - 
                                                RATIO_VD_VI_TAC_CONTROL))
    )



GETNE_control <- GETNE %>% 
  filter(Tiempo_CONTROL == "post-diagnostico") %>% 
  select(Numero_registro, Cardiopatia_alDx, Diametro_AD_TAC_BASAL, Diametro_AI_TAC_BASAL, Diametro_VD_TAC_BASAL, Diametro_VI_TAC_BASAL,
         RATIO_AD_AI_TAC_BASAL, RATIO_VD_VI_TAC_BASAL,
         Diametro_AD_TAC_Dx_Cardiopatia, Diametro_AI_TAC_Dx_Cardiopatia,
         Diametro_VD_TAC_Dx_Cardiopatia, Diametro_VI_TAC_Dx_Cardiopatia, RATIO_AD_AI_TAC_Dx_Cardiopatia,
         RATIO_VD_VI_TAC_Dx_Cardiopatia,
         Diametro_AD_TAC_CONTROL, Diametro_AI_TAC_CONTROL, Diametro_VD_TAC_CONTROL, Diametro_VI_TAC_CONTROL,
         RATIO_AD_AI_TAC_CONTROL, RATIO_VD_VI_TAC_CONTROL) %>%
  pivot_longer(cols = -c(Numero_registro, Cardiopatia_alDx), names_to = "Parameter", values_to = "value") %>% 
  separate(Parameter, into = c("Parameter", "moment"), sep = "TAC") %>% 
  mutate(Parameter = str_replace_all(Parameter, "_$", ""),
         moment = str_replace_all(moment, "^_", "")) 

GETNE_control_2 <- GETNE %>% 
  filter(Tiempo_CONTROL == "post-diagnostico") %>% 
  select(Numero_registro, Cardiopatia_alDx, Inclinacion_septo_ventricular_TAC_BASAL, Inclinacion_septo_ventricular_TAC_Dx_Cardiopatia,
         Inlinacion_septo_ventricular_TAC_CONTROL) %>% 
  pivot_longer(cols = -c(Numero_registro, Cardiopatia_alDx), names_to = "moment", values_to = "Inclinacion_septoventricular") %>% 
  mutate(moment = str_replace_all(moment, "Inclinacion_septo_ventricular_TAC_", ""),
         moment = str_replace_all(moment, "Inlinacion_septo_ventricular_TAC_", "")) 
  
  

GETNE_wide <- GETNE_control %>% 
  pivot_wider(names_from = Parameter, values_from = value) %>% 
  inner_join(GETNE_control_2, by =c("Numero_registro", "moment")) %>% 
  select(-Cardiopatia_alDx.y) %>% 
  rename(Cardiopatia_alDx = Cardiopatia_alDx.x)

GETNE_wide_filter <- GETNE_wide %>% 
  filter(Cardiopatia_alDx == "No")



##Guardar BD------

setwd("M:/Proyecto NET/GETNE")
#dir.create("Datasets/")

writexl::write_xlsx(list("GETNE_DB" = GETNE_DB, "GETNE_control" = GETNE_control, 
                         "GETNE_wide" = GETNE_wide),
                    "Datasets/Datasets_report.xlsx")

#Normalidad-----

##Todos--------

Normalidad_CCsi <- GETNE %>%
  filter(Cardiopatia_carcinoide == "Si") %>% 
  summarise(n = n(), 
            Diametro_AD_TAC_BASAL_n = shapiro.test(Diametro_AD_TAC_BASAL)$p.value, 
            Diametro_AI_TAC_BASAL_n = shapiro.test(Diametro_AI_TAC_BASAL)$p.value,
            Diametro_VD_TAC_BASAL_n = shapiro.test(Diametro_VD_TAC_BASAL)$p.value,
            Diametro_VI_TAC_BASAL_n = shapiro.test(Diametro_VI_TAC_BASAL)$p.value,
            RATIO_AD_AI_TAC_BASAL_n = shapiro.test(RATIO_AD_AI_TAC_BASAL)$p.value,
            RATIO_VD_VI_TAC_BASAL_n = shapiro.test(RATIO_VD_VI_TAC_BASAL)$p.value) %>%
  pivot_longer(!n, names_to = "parameter", values_to = "p.value")  %>%
  mutate(normality = case_when(p.value > 0.05 ~ "normal", 
                               p.value <= 0.05 ~ "not_normal")) %>%  print(n=50)

Normalidad_CCno <- GETNE %>%
  filter(Cardiopatia_carcinoide == "No") %>% 
  summarise(n = n(), 
            Diametro_AD_TAC_BASAL_n = shapiro.test(Diametro_AD_TAC_BASAL)$p.value, 
            Diametro_AI_TAC_BASAL_n = shapiro.test(Diametro_AI_TAC_BASAL)$p.value,
            Diametro_VD_TAC_BASAL_n = shapiro.test(Diametro_VD_TAC_BASAL)$p.value,
            Diametro_VI_TAC_BASAL_n = shapiro.test(Diametro_VI_TAC_BASAL)$p.value,
            RATIO_AD_AI_TAC_BASAL_n = shapiro.test(RATIO_AD_AI_TAC_BASAL)$p.value,
            RATIO_VD_VI_TAC_BASAL_n = shapiro.test(RATIO_VD_VI_TAC_BASAL)$p.value) %>%
  pivot_longer(!n, names_to = "parameter", values_to = "p.value")  %>%
  mutate(normality = case_when(p.value > 0.05 ~ "normal", 
                               p.value <= 0.05 ~ "not_normal")) %>%  print(n=50)


##CC_diagnóstico---------

Normalidad_CCsi_Dxno <- GETNE %>%
  filter(Cardiopatia_carcinoide == "Si" & Cardiopatia_alDx == "No") %>% 
  summarise(n = n(), 
            Diametro_AD_TAC_BASAL_n = shapiro.test(Diametro_AD_TAC_BASAL)$p.value, 
            Diametro_AI_TAC_BASAL_n = shapiro.test(Diametro_AI_TAC_BASAL)$p.value,
            Diametro_VD_TAC_BASAL_n = shapiro.test(Diametro_VD_TAC_BASAL)$p.value,
            Diametro_VI_TAC_BASAL_n = shapiro.test(Diametro_VI_TAC_BASAL)$p.value,
            RATIO_AD_AI_TAC_BASAL_n = shapiro.test(RATIO_AD_AI_TAC_BASAL)$p.value,
            RATIO_VD_VI_TAC_BASAL_n = shapiro.test(RATIO_VD_VI_TAC_BASAL)$p.value) %>%
  pivot_longer(!n, names_to = "parameter", values_to = "p.value")  %>%
  mutate(normality = case_when(p.value > 0.05 ~ "normal", 
                               p.value <= 0.05 ~ "not_normal")) %>%  print(n=50)


##CC_post-diagnóstico---------

Normalidad_CC_postdianostico <- GETNE %>%
  filter(Tiempo_CONTROL == "post-diagnostico") %>% 
  summarise(n = n(), 
            Dif_diametroAD_TAC_n = shapiro.test(Dif_diametroAD_TAC)$p.value, 
            Dif_diametro_AI_TAC_n = shapiro.test(Dif_diametro_AI_TAC)$p.value,
            Dif_diametro_VD_TAC_n = shapiro.test(Dif_diametro_VD_TAC)$p.value,
            Dif_diametro_VI_TAC_n = shapiro.test(Dif_diametro_VD_TAC)$p.value,
            Dif_RATIO_AD_AI_TAC_n = shapiro.test(Dif_RATIO_AD_AI_TAC)$p.value,
            Dif_RATIO_VD_VI_TAC_n = shapiro.test(Dif_RATIO_VD_VI_TAC)$p.value) %>%
  pivot_longer(!n, names_to = "parameter", values_to = "p.value")  %>%
  mutate(normality = case_when(p.value > 0.05 ~ "normal", 
                               p.value <= 0.05 ~ "not_normal")) %>%  print(n=50)

Normalidad_CC_postdianostico_control <- GETNE %>%
  filter(Tiempo_CONTROL == "post-diagnostico") %>% 
  summarise(n = n(), 
            Dif_diametroAD_TAC_control1 = shapiro.test(Dif_diametroAD_TAC_control)$p.value, 
            Dif_diametro_AI_TAC_control1 = shapiro.test(Dif_diametro_AI_TAC_control)$p.value,
            Dif_diametro_VD_TAC_control1 = shapiro.test(Dif_diametro_VD_TAC_control)$p.value,
            Dif_diametro_VI_TAC_control1 = shapiro.test(Dif_diametro_VI_TAC_control)$p.value,
            Dif_RATIO_AD_AI_TAC_control1 = shapiro.test(Dif_RATIO_AD_AI_TAC_control)$p.value,
            Dif_RATIO_VD_VI_TAC_control1 = shapiro.test(Dif_RATIO_VD_VI_TAC_control)$p.value) %>%
  pivot_longer(!n, names_to = "parameter", values_to = "p.value")  %>%
  mutate(normality = case_when(p.value > 0.05 ~ "normal", 
                               p.value <= 0.05 ~ "not_normal")) %>%  print(n=50)


GETNE %>% 
  filter(Cardiopatia_alDx == "No") %>% 
  summarize(Median_meses_al_diagnostico = median(Mes_Dx_Basal),
            min = min(Mes_Dx_Basal),
            max = max(Mes_Dx_Basal))


#ANOVA------

#Anova medidas repetidas

##Diametro AD------

#Para identificar outliers, tienen que ser extremos

###Ouliers-----
GETNE_wide_filter %>% 
  group_by(moment) %>% 
  identify_outliers(Diametro_AD) #no hay extreme outliers


###Residuales------

#Para extraer los residuales por grupo
res_moment_AD <- GETNE_wide_filter %>%
  group_by(moment) %>% 
  do({
    residualesAD_r <- lm(Diametro_AD ~ 1, data=.) 
    data.frame(residualesAD_r = residuals(residualesAD_r))
  })

###Normalidad-------

res_moment_AD %>% 
  group_by(moment) %>% 
  mutate(moment = factor(moment)) %>% 
  summarise(p_value_S = shapiro.test(residualesAD_r)$p.value) #no todos cumplen la condición de normalidad --- no paramétrico


###LeveneTest--------

leveneTest(Diametro_AD ~ moment, data = GETNE_wide_filter, center = median) #homogeneidad de varianzas

###Anova-repetidas------

anova_diametro_AD_r <- aov(Diametro_AD ~ moment + Error(Numero_registro/moment), data = GETNE_wide_filter)
summary(anova_diametro_AD_r) #Valor de P del anova de medidas repetidas
#residualesAD_r <- residuals(anova_diametro_AD_r) Esto es para obtener los residuales pero queremos por grupo


###Friedman--------
#Como nuestra variable tiene valores extremos y no sigue la normalidad en general hacemos el test de Friedman. 
#Dos maneras de hacer el test de Friedman, elegí la primera para el gráfico. 
Friedman_diametro_AD <- GETNE_wide_filter %>% 
  friedman_test(Diametro_AD ~ moment | Numero_registro)

#Friedman_diametro_AD <- friedman.test(Diametro_AD ~ moment | Numero_registro, data = GETNE_wide)


###Post-hoc---------
#Post-hoc comparaciones -- dos métodos para el mismo test

pairwise.wilcox.test(
  GETNE_wide_filter$Diametro_AD,
  GETNE_wide_filter$moment,
  paired = TRUE, 
  p.adjust.method = "bonferroni") #Se detectan diferencias entre el basal con Dx cardiopatia  (0.016) entre control con Dx de cardiopatía (0.042)

pwc_Diametro_AD <- GETNE_wide_filter %>% 
  wilcox_test(Diametro_AD ~ moment, paired = TRUE, p.adjust.method = "bonferroni")
pwc_Diametro_AD

##Diametro AI-----

###Ouliers-----

GETNE_wide_filter %>% 
  group_by(moment) %>% 
  identify_outliers(Diametro_AI) # no tiene outliers


###Residuales--------

#Para extraer los residuales por grupo
res_moment_AI <- GETNE_wide_filter %>% 
  group_by(moment) %>% 
  do({
    residualesAI_r <- lm(Diametro_AI ~ 1, data=.) 
    data.frame(residualesAI_r = residuals(residualesAI_r))
  })


###Normalidad----------

#Para hacer el test de shapiro wilk por grupo
res_moment_AI %>% 
  group_by(moment) %>% 
  mutate(moment = factor(moment)) %>% 
  summarise(p_value_S = shapiro.test(residualesAI_r)$p.value)   #todos cumplen con la condición de normalidad


###LeveneTest---------
leveneTest(Diametro_AI ~ moment, data = GETNE_wide_filter, center = median) #homogeneidad de varianzas

###Anova-repetidas------

anova_diametro_AI_r <- aov(Diametro_AI ~ moment + Error(Numero_registro/moment), data = GETNE_wide_filter)
summary(anova_diametro_AI_r) #no hay diferencias entre grupos por anova


##Diametro VD------

###Outliers--------

GETNE_wide_filter %>% 
  group_by(moment) %>% 
  identify_outliers(Diametro_VD) #no hay extreme outliers


###Residuales--------

#Para extraer los residuales por grupo
res_moment_VD <- GETNE_wide_filter %>% 
  group_by(moment) %>% 
  do({
    residualesVD_r <- lm(Diametro_VD ~ 1, data=.) 
    data.frame(residualesVD_r = residuals(residualesVD_r))
  })


###Normalidad--------
#Para hacer el test de shapiro wilk por grupo
res_moment_VD %>% 
  group_by(moment) %>% 
  mutate(moment = factor(moment)) %>% 
  summarise(p_value_S = shapiro.test(residualesVD_r)$p.value)   #no todos cumplen la condición de normalidad


###LeveneTest-------
leveneTest(Diametro_VD ~ moment, data = GETNE_wide_filter, center = median) #homogeneidad de varianzas

###Anova-repetidas------

anova_diametro_VD_r <- aov(Diametro_VD ~ moment + Error(Numero_registro/moment), data = GETNE_wide)
summary(anova_diametro_VD_r) #no hay diferencias entre grupos

###Friedman-------


Friedman_diametro_VD <- GETNE_wide_filter %>% 
  friedman_test(Diametro_VD ~ moment | Numero_registro) #Hay diferecias entre grupos


pairwise.wilcox.test(
  GETNE_wide_filter$Diametro_VD,
  GETNE_wide_filter$moment,
  paired = TRUE, 
  p.adjust.method = "bonferroni") #Basal con Dx de la cardiopatía son diferentes

##Diametro VI-----

###Outliers------

GETNE_wide_filter %>% 
  group_by(moment) %>% 
  identify_outliers(Diametro_VI) #no hay outliers


###Residuales--------
#Para extraer los residuales por grupo
res_moment_VI <- GETNE_wide_filter %>% 
  group_by(moment) %>% 
  do({
    residualesVI_r <- lm(Diametro_VI ~ 1, data=.) 
    data.frame(residualesVI_r = residuals(residualesVI_r))
  })


###Normalidad-------
#Para hacer el test de shapiro wilk por grupo
res_moment_VI %>% 
  group_by(moment) %>% 
  mutate(moment = factor(moment)) %>% 
  summarise(p_value_S = shapiro.test(residualesVI_r)$p.value)   #todos cumplen con la condición de normalidad


###LeveneTest--------
leveneTest(Diametro_VI ~ moment, data = GETNE_wide_filter, center = median) #homogeneidad de varianzas

###Anova-repetidas------

anova_diametro_VI_r <- aov(Diametro_VI ~ moment + Error(Numero_registro/moment), data = GETNE_wide_filter)
summary(anova_diametro_VI_r)  #no hay diferencias entre grupos

##RAtio VD_VI--------

###Outliers---------

GETNE_wide_filter %>% 
  group_by(moment) %>% 
  identify_outliers(RATIO_AD_AI) #no hay extreme outliers


###Residuales-------
#Para extraer los residuales por grupo
res_moment_AD_AI <- GETNE_wide_filter %>% 
  group_by(moment) %>% 
  do({
    residualesAD_AI_r <- lm(RATIO_AD_AI ~ 1, data=.) 
    data.frame(residualesAD_AI_r = residuals(residualesAD_AI_r))
  })


###Normalidad---------
#Para hacer el test de shapiro wilk por grupo
res_moment_AD_AI %>% 
  group_by(moment) %>% 
  mutate(moment = factor(moment)) %>% 
  summarise(p_value_S = shapiro.test(residualesAD_AI_r)$p.value)   #no todos cumplen la condición de normalidad


###LeveneTest-------

leveneTest(RATIO_AD_AI ~ moment, data = GETNE_wide_filter, center = median) #homogeneidad de varianzas


###Friedman-------
Friedman_RATIO_AD_AI <- GETNE_wide_filter %>% friedman_test(RATIO_AD_AI ~ moment | Numero_registro)

#Friedman_RATIO_AD_AI <- friedman.test(RATIO_AD_AI ~ moment | Numero_registro, data = GETNE_wide) #hay diferencias entre grupos

###Post-hoc--------
pwc_RATIO_AD_AI <- GETNE_wide_filter %>% 
  wilcox_test(RATIO_AD_AI ~ moment, paired = TRUE, p.adjust.method = "bonferroni") #Basal es dif a Dx Cardiopatía 0.01 y Control dif a Dx cardiopatía 0.038
pwc_RATIO_AD_AI


pairwise.wilcox.test(
  GETNE_wide_filter$RATIO_AD_AI,
  GETNE_wide_filter$moment,
  paired = TRUE, 
  p.adjust.method = "bonferroni") #Se detectan diferencias entre basal con Dx cardiopatía (0.004) y control con diagnóstico Cardiopatia (0.049)


##RAtio VD_VI--------

###Outliers---------

GETNE_wide_filter %>% 
  group_by(moment) %>% 
  identify_outliers(RATIO_VD_VI) ## Extreme outliers


###Residuales----------

#Para extraer los residuales por grupo
res_moment_VD_VI <- GETNE_wide_filter %>% 
  group_by(moment) %>% 
  do({
    residualesVD_VI_r <- lm(RATIO_VD_VI ~ 1, data=.) 
    data.frame(residualesVD_VI_r = residuals(residualesVD_VI_r))
  })


###Normalidad----------

#Para hacer el test de shapiro wilk por grupo
res_moment_VD_VI %>% 
  group_by(moment) %>% 
  mutate(moment = factor(moment)) %>% 
  summarise(p_value_S = shapiro.test(residualesVD_VI_r)$p.value)   #no todos cumplen la condición de normalidad

leveneTest(RATIO_VD_VI ~ moment, data = GETNE_wide_filter, center = median) #homogeneidad de varianzas


###Friedman--------
Friedman_RATIO_VD_VI <- GETNE_wide_filter %>% friedman_test(RATIO_VD_VI ~ moment | Numero_registro) #sale significativa

#Friedman_RATIO_VD_VI <- friedman.test(RATIO_VD_VI ~ moment | Numero_registro, data = GETNE_wide) #hay diferencias entre grupos

###Post-hoc---------
pwc_RATIO_VD_VI <- GETNE_wide_filter %>% 
  wilcox_test(RATIO_VD_VI ~ moment, paired = TRUE, p.adjust.method = "bonferroni")
pwc_RATIO_VD_VI


pairwise.wilcox.test(
  GETNE_wide$RATIO_VD_VI,
  GETNE_wide$moment,
  paired = TRUE, 
  p.adjust.method = "bonferroni") #no llega a haber diferencias significativas entre grupos, la diferencia mayor está entre control con Dx cardiopatia (0.05)


#Graficos: anova----

ggplot(GETNE_wide, aes(x= moment, y= Diametro_AI, group = Numero_registro))+
  geom_line(alpha = 0.3) +
  geom_point(size = 2, alpha = 0.6)+
  stat_summary(fun = mean, geom = "line", aes(group = 1),
               color = "red", size = 1.3)+
  stat_summary(fun = mean, geom = "point", color = "red", size =3)+
  theme_minimal()+
  labs( x = "Moment", y = "Diametro AI",
        title = "Evolución Diámetro AI (medias)", 
        subtitle = "LIneas grises: cada paciente, línea roja: media del grupo")

#Gráficos: Friedman -----

ggplot(GETNE_wide, aes(x= moment, y= Diametro_AD, group = Numero_registro))+
  geom_line(alpha = 0.4, color ="gray50") +
  geom_point(size = 3, alpha = 0.7, color= "gray50")+
  stat_summary(fun = median, geom = "line", aes(group = 1),
               color = "red", size = 1.5)+
  stat_summary(fun = median, geom = "point", color = "red", size =3)+
  theme_minimal()+
  labs( x = "Moment", y = "Diametro AD",
        title = "Evolución Diámetro AD (medianas)", 
        subtitle = "LIneas grises: cada paciente, línea roja: mediana del grupo")


pwc_Diametro_AD <- pwc_Diametro_AD %>%  add_y_position()

ggboxplot(GETNE_wide, x= "moment", y= "Diametro_AD", add = "jitter")+
  stat_pvalue_manual(pwc_Diametro_AD, hide.ns = TRUE) +
  labs(
    title = "Evolución Diámetro AD",
    subtitle = get_test_label(Friedman_diametro_AD, detailed = TRUE),
    caption = get_pwc_label(pwc_Diametro_AD))+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 18))

pwc_RATIO_AD_AI <- pwc_RATIO_AD_AI %>%  add_y_position()

ggboxplot(GETNE_wide, x= "moment", y= "RATIO_AD_AI", add = "jitter")+
  stat_pvalue_manual(pwc_RATIO_AD_AI, hide.ns = TRUE) +
  labs(
    title = "Evolución RATIO AD:AI",
    subtitle = get_test_label(Friedman_diametro_AD, detailed = TRUE),
    caption = get_pwc_label(pwc_RATIO_AD_AI)+
      theme(plot.title = element_text(size = 18, face = "bold"),
            axis.title = element_text(size = 18, face = "bold"),
            axis.text = element_text(size = 18)))

pwc_RATIO_VD_VI <- pwc_RATIO_VD_VI %>%  add_y_position()

ggboxplot(GETNE_wide, x= "moment", y= "RATIO_VD_VI", add = "jitter")+
  stat_pvalue_manual(pwc_RATIO_VD_VI, hide.ns = TRUE) +
  labs(
    title = "Evolución RATIO VD:VI",
    subtitle = get_test_label(Friedman_RATIO_VD_VI, detailed = TRUE),
    caption = get_pwc_label(pwc_RATIO_VD_VI)) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 18))



ggboxplot(x = "grupo", y = "Value",
          width = 0.4, add = "jitter", color = "grupo", 
          palette = c("#5dc1b9", "salmon"), add.params = list(size = 3.5))+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", size = 25,
                     paired = FALSE, label = "p.signif")+
  labs(x= "", y = "MCS Scale")+
  scale_y_continuous(limits = c(10,80), breaks = seq(10, 80, by = 20))+
  geom_hline(yintercept = 50, colour = "gray40", linetype = "dashed")+
  theme(panel.background=element_blank(),
        axis.line=element_line(color="black"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20, face = "bold"),
        aspect.ratio = 1.5, legend.position = "none",
        plot.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 14))

#Cutpointr--------
#Prueba determinación cut-off 
#Sensibilidad es la capacidad de detectar a los enfermos. 
#Especificidad es la capacidad de detectr a los sanos


GETNE_filter <- GETNE %>% 
  filter(Cardiopatia_carcinoide == "No" | Cardiopatia_alDx == "No")


CP <- cutpointr(GETNE, RATIO_VD_VI_TAC_BASAL, Cardiopatia_carcinoide,
                method = maximize_metric, metric = sum_sens_spec)

summary(CP)
plot(CP)


CP2 <- cutpointr(GETNE_filter, RATIO_VD_VI_TAC_BASAL, Cardiopatia_carcinoide,
                method = maximize_metric, metric = sum_sens_spec)

summary(CP2)
plot(CP2)

CP2_b <- cutpointr(GETNE_filter, RATIO_VD_VI_TAC_BASAL, Cardiopatia_carcinoide, Sexo,
                                    boot_runs = 1000, boot_stratify = TRUE, pos_class = "Si", direction = ">=")

CP2_b %>%  select(subgroup, optimal_cutpoint, sum_sens_spec, acc, sensitivity, specificity, data)

plot(CP2_b)


CP_VI <- cutpointr(GETNE, Diametro_VI_TAC_BASAL, Cardiopatia_carcinoide,
                method = maximize_metric, metric = sum_sens_spec, pos_class = "Si")

summary(CP_VI)
plot(CP_VI)
CP_VI$pos_class

CP_VI2 <- cutpointr(GETNE_filter, Diametro_VI_TAC_BASAL, Cardiopatia_carcinoide,
                method = maximize_metric, metric = sum_sens_spec, pos_class = "Si")

summary(CP_VI2)
plot(CP_VI2)

set.seed(12)
opt_cut_b <- cutpointr(GETNE_filter, RATIO_VD_VI_TAC_BASAL, Cardiopatia_carcinoide, boot_runs = 1000)

set.seed(12)
opt_cut_b2 <- cutpointr(GETNE_filter, Diametro_VI_TAC_BASAL, Cardiopatia_carcinoide, boot_runs = 1000,
                        pos_class = "Si")

summary(opt_cut_b2)


CP_b <- cutpointr(GETNE_filter, Diametro_VI_TAC_BASAL, Cardiopatia_carcinoide, Sexo,
                  boot_runs = 1000, boot_stratify = TRUE, pos_class = "Si")

CP_b %>%  select(subgroup, AUC, optimal_cutpoint, sum_sens_spec, acc, sensitivity, specificity, data)


CP_b2 <- cutpointr(GETNE_filter, Diametro_VI_TAC_BASAL, Cardiopatia_carcinoide, Sexo,
                  boot_runs = 0, boot_stratify = TRUE, pos_class = "Si")

CP_b2 %>%  select(subgroup, AUC, optimal_cutpoint, sum_sens_spec, acc, sensitivity, specificity, data)
plot(CP_b2)
#Para combinar los dos factores

##Score:combinado-----

str(GETNE_filter)

Multivariante1 <- glm(Cardiopatia_carcinoide ~ Diametro_VI_TAC_BASAL + Diametro_VD_TAC_BASAL, data = GETNE_filter, family = "binomial")

summary(Multivariante1)

Multivariante2 <- glm(Cardiopatia_carcinoide ~ RATIO_AD_AI_TAC_BASAL + RATIO_VD_VI_TAC_BASAL, data = GETNE_filter, family = "binomial")

summary(Multivariante2)

modelo <- glm(Cardiopatia_carcinoide ~ RATIO_VD_VI_TAC_BASAL + Diametro_VI_TAC_BASAL, data = GETNE_filter, family = "binomial")

summary(modelo)


modelo2 <- glm(Cardiopatia_carcinoide ~ RATIO_VD_VI_TAC_BASAL + Diametro_VI_TAC_BASAL + Inclinacion_septo_ventricular_TAC_BASAL, data = GETNE_filter, family = "binomial")

summary(modelo2)

#GETNE_filter <- GETNE_filter %>% 
  #mutate(variable_combinada = ((-1.4231)+RATIO_VD_VI_TAC_BASAL*6.7543) + (Diametro_VI_TAC_BASAL*-0.1117))

GETNE_filter$variable_combinada <- predict(modelo, type = "link") #para calcular una nueva variable a partir del modelo (GLM)

GETNE_filter$variable_combinada2 <- predict(modelo2, type = "link")

CP_variablecombinada <- cutpointr(GETNE_filter, variable_combinada, Cardiopatia_carcinoide,
                    method = maximize_metric, metric = sum_sens_spec, pos_class = "Si")

summary(CP_variablecombinada)
plot(CP_variablecombinada)

##Bootstrap----

#Usando Sexo como subgrupo
set.seed(12)
CP_variablecombinada_b <- cutpointr(GETNE_filter, variable_combinada, Cardiopatia_carcinoide, Sexo,
                                  boot_runs = 1000, boot_stratify = TRUE, pos_class = "Si", direction = ">=")

CP_variablecombinada_b %>%  select(subgroup, optimal_cutpoint, sum_sens_spec, acc, sensitivity, specificity, data)

plot(CP_variablecombinada_b)

CP_variablecombinada2 <- cutpointr(GETNE_filter, variable_combinada2, Cardiopatia_carcinoide,
                                  method = maximize_metric, metric = sum_sens_spec, pos_class = "Si")

summary(CP_variablecombinada2)
plot(CP_variablecombinada2)


