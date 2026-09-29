#ANÁLISIS BASE DE DATOS PROYECTO GETNE
#Elaborado por: Ana Elsa Huerta

#Cargar paquetes-----

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
library(survival)
library(lubridate)
library(ggsurvfit)
library(survminer)

#BBDD------

GETNE <- read_xlsx(
  here("Data", "Extra", "Base_cardiopatia_carcinoide_completa_analisis.xlsx"),
                   col_names = TRUE)

GETNE_2 <- read_xlsx(
  here("Data", "Extra", "Base_cardiopatia_carcinoide_completa_tratamientos.xlsx"),
  col_names = TRUE)

GETNE_2 <- GETNE_2 %>% 
  select(Numero_GETNE, exitus, fecha_exitus, OS_years, OS_mo, TtoStartDate_1_1, AnalogsCombination_1_1, Fin_tto_analogo)
  
GETNE_DB <- GETNE %>% 
  left_join(
    GETNE_2, by = "Numero_GETNE"
    ) %>% 
  mutate(
    Sexo = factor(Sexo),
    Estado_general = factor(Estado_general),
    Metastasis_hepaticas = factor(Metastasis_hepaticas, labels = c("No", "Si")),
    Metastasis_peritoneales = factor(Metastasis_peritoneales, labels = c("No", "Si")),
    Metastasis_ganglionares = factor(Metastasis_ganglionares, labels = c("No", "Si")),
    Ki_67 = factor(Ki_67),
    Grado_NET = if_else(Ki_67 == "<= 2%", "G1", "G2"),
    Cromogranina_A_categ = factor(Cromogranina_A_categ, labels = c("normal", "elevada")),
    Inclinacion_septo_ventricular_TAC_BASAL = factor(Inclinacion_septo_ventricular_TAC_BASAL),
    Tiempo_CONTROL = factor(Tiempo_CONTROL),
    Inlinacion_septo_ventricular_TAC_CONTROL = factor(Inlinacion_septo_ventricular_TAC_CONTROL),
    Cardiopatia_carcinoide = factor(Cardiopatia_carcinoide),
    Cardiopatia_alDx = factor(Cardiopatia_alDx),
    Inclinacion_septo_ventricular_TAC_Dx_Cardiopatia = factor(Inclinacion_septo_ventricular_TAC_Dx_Cardiopatia),
    CX_VALVULAR = factor(CX_VALVULAR, labels = c("No", "Si")),
    `5_HIAA` = as.numeric(`5_HIAA`),
    
    #refactorizar Ki-67 en dicotómica
    
    Ki_67_dic = case_when(
      Ki_67 == "<= 2%" ~ "<=10%",
      Ki_67 == "3 - 5%" ~ "<=10%",
      Ki_67 == "6 - 10%" ~ "<=10%",
      Ki_67 == "11 - 20%" ~ ">10%"
    ),
    
    #Cálculo de ratios
    
    RATIO_AD_AI_TAC_BASAL = round(Diametro_AD_TAC_BASAL / Diametro_AI_TAC_BASAL, 2),
    RATIO_VD_VI_TAC_BASAL = round(Diametro_VD_TAC_BASAL / Diametro_VI_TAC_BASAL, 2),
    RATIO_AD_AI_TAC_CONTROL = round(Diametro_AD_TAC_CONTROL / Diametro_AI_TAC_CONTROL, 2),
    RATIO_VD_VI_TAC_CONTROL = round(Diametro_VD_TAC_CONTROL / Diametro_VI_TAC_CONTROL, 2),
    RATIO_AD_AI_TAC_Dx_Cardiopatia = round(Diametro_AD_TAC_Dx_Cardiopatia / Diametro_AI_TAC_Dx_Cardiopatia, 2),
    RATIO_VD_VI_TAC_Dx_Cardiopatia = round(Diametro_VD_TAC_Dx_Cardiopatia / Diametro_VI_TAC_Dx_Cardiopatia, 2)
  ) %>% 
  
  #Renombrar `5_HIAA`
  
  rename(A5_HIAA = `5_HIAA`) %>% 
  
  #Lo siguiente es para generar unas columnas que me permitan comparar en general cardiopatia si vs no de los parámetros del TAC
  
  mutate(
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
      Cardiopatia_alDx == "No" & Cardiopatia_carcinoide == "Si" ~ Inclinacion_septo_ventricular_TAC_Dx_Cardiopatia),
    )%>% 
  
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
    ) %>% 
  
  #OS
  
  mutate(exitus = case_when(exitus == "1" ~ 1,
                            exitus == "2" ~ 0),
         Exitus_status = case_when(exitus == 1 ~ "Sí",
                                   exitus == 0 ~ "No"),
         Tratamiento_oncologico = case_when(
           AnalogsCombination_1_1 == "/Lanreótido autogel" ~ "Somatostatin Analogs",
           AnalogsCombination_1_1 == "Lanreótido autogel" ~ "Somatostatin Analogs",
           AnalogsCombination_1_1 == "Octreótido LAR" ~ "Somatostatin Analogs",
           AnalogsCombination_1_1 == "Octreótido LAR/-" ~ "Somatostatin Analogs",
           AnalogsCombination_1_1 == "Octreótido s.c./Lanreótido autogel" ~ "Somatostatin Analogs")
         ) 

GETNE_control <- GETNE_DB %>% 
  filter(Tiempo_CONTROL != "pre-diagnostico" | is.na(Tiempo_CONTROL)) %>% 
  select(Numero_registro, Cardiopatia_carcinoide, Cardiopatia_alDx, Diametro_AD_TAC_BASAL, Diametro_AI_TAC_BASAL, Diametro_VD_TAC_BASAL, Diametro_VI_TAC_BASAL,
         RATIO_AD_AI_TAC_BASAL, RATIO_VD_VI_TAC_BASAL,
         Diametro_AD_TAC_Dx_Cardiopatia, Diametro_AI_TAC_Dx_Cardiopatia,
         Diametro_VD_TAC_Dx_Cardiopatia, Diametro_VI_TAC_Dx_Cardiopatia, RATIO_AD_AI_TAC_Dx_Cardiopatia,
         RATIO_VD_VI_TAC_Dx_Cardiopatia,
         Diametro_AD_TAC_CONTROL, Diametro_AI_TAC_CONTROL, Diametro_VD_TAC_CONTROL, Diametro_VI_TAC_CONTROL,
         RATIO_AD_AI_TAC_CONTROL, RATIO_VD_VI_TAC_CONTROL) %>%
  pivot_longer(cols = -c(Numero_registro, Cardiopatia_alDx, Cardiopatia_carcinoide), names_to = "Parameter", values_to = "value") %>% 
  separate(Parameter, into = c("Parameter", "moment"), sep = "TAC") %>% 
  mutate(Parameter = str_replace_all(Parameter, "_$", ""),
         moment = str_replace_all(moment, "^_", "")) 

GETNE_control_2 <- GETNE_DB %>% 
  filter(Tiempo_CONTROL != "pre-diagnostico" | is.na(Tiempo_CONTROL)) %>% 
  select(Numero_registro, Cardiopatia_carcinoide, Cardiopatia_alDx, Inclinacion_septo_ventricular_TAC_BASAL, Inclinacion_septo_ventricular_TAC_Dx_Cardiopatia,
         Inlinacion_septo_ventricular_TAC_CONTROL) %>% 
  pivot_longer(cols = -c(Numero_registro, Cardiopatia_carcinoide, Cardiopatia_alDx), names_to = "moment", values_to = "Inclinacion_septoventricular") %>% 
  mutate(moment = str_replace_all(moment, "Inclinacion_septo_ventricular_TAC_", ""),
         moment = str_replace_all(moment, "Inlinacion_septo_ventricular_TAC_", "")) 
  
GETNE_wide <- GETNE_control %>% 
  pivot_wider(names_from = Parameter, values_from = value) %>% 
  inner_join(GETNE_control_2, by =c("Numero_registro", "moment")) %>% 
  select(-Cardiopatia_alDx.y, Cardiopatia_carcinoide.y) %>% 
  rename(Cardiopatia_alDx = Cardiopatia_alDx.x,
         Cardiopatia_carcinoide = Cardiopatia_carcinoide.x)

GETNE_wide_filter <- GETNE_wide %>% 
  filter(Cardiopatia_alDx == "No")


##Guardar BD------

writexl::write_xlsx(list("GETNE_DB" = GETNE_DB, "GETNE_control" = GETNE_control, 
                         "GETNE_wide" = GETNE_wide),
                    path = here("Data", "Datasets","Dataset_report.xlsx"))


