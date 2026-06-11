
#Summary tables------

##

##Not normal: Wilcox_test------

Tabla_GETNE_categorical <- GETNE %>% 
  select(HOSPITAL, Sexo, Edad, Estado_general, Cardiopatia_alDx, Localizacion_primario, Metastasis_hepaticas,
         Metastasis_peritoneales, Metastasis_ganglionares, Ki_67, Cromogranina_A_categ,
         Cardiopatia_carcinoide, CX_VALVULAR,
         Tratamiento_sistemico_recibido) %>% 
  tbl_summary(by = Cardiopatia_carcinoide, type = list(where(is.numeric) ~ "continuous",
                                                       all_dichotomous() ~ "categorical"),
              missing = "no") %>% 
  add_p() 
Tabla_GETNE_categorical

Tabla_GETNE_TAC <- GETNE %>% 
  select(Diametro_AD_TAC_BASAL, Diametro_AI_TAC_BASAL, Diametro_VD_TAC_BASAL, Diametro_VI_TAC_BASAL,
         RATIO_AD_AI_TAC_BASAL, RATIO_VD_VI_TAC_BASAL, Inclinacion_septo_ventricular_TAC_BASAL, 
         Cardiopatia_carcinoide) %>% 
  tbl_summary(by = Cardiopatia_carcinoide, type = list(where(is.numeric) ~ "continuous",
                                                       all_dichotomous() ~ "categorical"),
              missing = "no") %>% 
  add_p() %>% 
  add_difference()
Tabla_GETNE_TAC

##Normal: ttest----

Tabla_GETNE_todos_norm <- GETNE %>% 
  select(Diametro_AI_TAC_BASAL, Diametro_VD_TAC_BASAL, Diametro_VI_TAC_BASAL, Cardiopatia_carcinoide) %>% 
  tbl_summary(by = Cardiopatia_carcinoide, type = list(where(is.numeric) ~ "continuous",
                                                       all_dichotomous() ~ "categorical"),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(all_continuous() ~ "t.test") 
Tabla_GETNE_todos_norm

#Tablas para quitar a los que desarrollaron cardiopatia carcinoide después del diagnóstico, comparación no cardiopatía, si cardiopatía. 

#Not normal------
Tabla_GETNE_cardiopatia <- GETNE %>% 
  filter(Cardiopatia_carcinoide == "No" | Cardiopatia_alDx == "No") %>% 
  select(HOSPITAL, Sexo, Edad, Estado_general, Cardiopatia_alDx, Localizacion_primario, Metastasis_hepaticas,
         Metastasis_peritoneales, Metastasis_ganglionares, Ki_67, Cromogranina_A_categ,
         Cardiopatia_carcinoide, CX_VALVULAR,
         Tratamiento_sistemico_recibido) %>% 
  tbl_summary(by = Cardiopatia_carcinoide, type = list(where(is.numeric) ~ "continuous",
                                                       all_dichotomous() ~ "categorical"),
              missing = "no") %>% 
  add_p() 
Tabla_GETNE_cardiopatia


Tabla_GETNE_TAC2 <- GETNE %>%
  filter(Cardiopatia_carcinoide == "No" | Cardiopatia_alDx == "No") %>% 
  select(Diametro_AD_TAC_BASAL, Diametro_AI_TAC_BASAL, Diametro_VD_TAC_BASAL, Diametro_VI_TAC_BASAL,
         RATIO_AD_AI_TAC_BASAL, RATIO_VD_VI_TAC_BASAL, Inclinacion_septo_ventricular_TAC_BASAL, 
         Cardiopatia_carcinoide) %>% 
  tbl_summary(by = Cardiopatia_carcinoide, type = list(where(is.numeric) ~ "continuous",
                                                       all_dichotomous() ~ "categorical"),
              missing = "no") %>% 
  add_difference()%>% 
  add_p() 
Tabla_GETNE_TAC2

Tabla_GETNE_TAC2 <- GETNE %>%
  filter(Cardiopatia_carcinoide == "No" | Cardiopatia_alDx == "No") %>% 
  select(Diametro_AD_TAC_BASAL, Diametro_AI_TAC_BASAL, Diametro_VD_TAC_BASAL, Diametro_VI_TAC_BASAL,
         RATIO_AD_AI_TAC_BASAL, RATIO_VD_VI_TAC_BASAL, Inclinacion_septo_ventricular_TAC_BASAL, 
         Cardiopatia_carcinoide) %>% 
  tbl_summary(by = Cardiopatia_carcinoide, type = list(where(is.numeric) ~ "continuous",
                                                       all_dichotomous() ~ "categorical"),
              missing = "no") %>% 
  add_difference()
#add_p() 
Tabla_GETNE_TAC2


#Normal ttest 2--------

Tabla_GETNE_cardiopatia_norm <- GETNE %>% 
  filter(Cardiopatia_carcinoide == "No" | Cardiopatia_alDx == "No") %>% 
  select(Diametro_AD_TAC_BASAL, Diametro_AI_TAC_BASAL, Diametro_VI_TAC_BASAL, Cardiopatia_carcinoide) %>% 
  tbl_summary(by = Cardiopatia_carcinoide, type = list(where(is.numeric) ~ "continuous",
                                                       all_dichotomous() ~ "categorical2"),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(all_continuous() ~ "t.test") 
Tabla_GETNE_cardiopatia_norm


sig_GETNE_control <- GETNE_control %>%
  nest(data = -Parameter) %>%
  mutate(test = map(.x=data, ~wilcox.test(value~moment, data=.x, paired= TRUE) %>% tidy))%>%
  unnest(test) %>% 
  mutate(p.adjust = p.adjust(p.value, method = "BH")) %>% 
  arrange(p.value)
sig_GETNE_control 

#Evaluamos la normalidad y todas las diferencias son no normales. ESta es una tabla, todos con cardiopatía post-diagnóstico, 
#que tienen la muestra basal y el control post-diagnóstico de cardiopatía. 


tbl_getne_control <- GETNE_wide %>%
  filter(moment == "BASAL" | moment == "Dx_Cardiopatia") %>% 
  tbl_summary(by = moment, include = -Numero_registro, type = list(where(is.numeric) ~ "continuous")) %>% 
  add_p(test = list(
    all_continuous() ~ "paired.wilcox.test"), group = Numero_registro) %>% 
  add_difference()
tbl_getne_control

tbl_getne_control2 <- GETNE_wide %>%
  tbl_summary(by = moment, include = -Numero_registro, type = list(where(is.numeric) ~ "continuous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(test = list(
    all_continuous() ~ "paired.wilcox.test"), group = Numero_registro) %>% 
  add_difference()
tbl_getne_control2

tbl_getne_control3 <- GETNE_wide %>%
  filter(moment == "BASAL" | moment == "CONTROL") %>% 
  tbl_summary(by = moment, include = -Numero_registro, type = list(where(is.numeric) ~ "continuous")) %>% 
  add_p(test = list(
    all_continuous() ~ "paired.wilcox.test"), group = Numero_registro) %>% 
  add_difference()
tbl_getne_control3

tbl_getne_control4 <- GETNE_wide %>%
  filter(moment == "BASAL" | moment == "CONTROL") %>% 
  tbl_summary(by = moment, include = -Numero_registro, type = list(where(is.numeric) ~ "continuous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(test = list(
    all_continuous() ~ "paired.t.test"), group = Numero_registro) 
tbl_getne_control4

#Tablas----

##De acuerdo a la cardiopatía, incluyendo a los que tenian al diagnóstico

Tabla_GETNE_todos <- GETNE %>% 
  select(HOSPITAL, Sexo, Edad, Estado_general, Cardiopatia_alDx, Localizacion_primario, Metastasis_hepaticas,
         Metastasis_peritoneales, Metastasis_ganglionares, Ki_67, Cromogranina_A_categ,
         Diametro_AD_TAC_BASAL, Diametro_AI_TAC_BASAL, Diametro_VD_TAC_BASAL, Diametro_VI_TAC_BASAL,
         RATIO_AD_AI_TAC_BASAL, RATIO_VD_VI_TAC_BASAL, Inclinacion_septo_ventricular_TAC_BASAL, 
         Cardiopatia_carcinoide, CX_VALVULAR,
         Tratamiento_sistemico_recibido) %>% 
  tbl_summary(by = Cardiopatia_carcinoide, type = list(where(is.numeric) ~ "continuous",
                                                       all_dichotomous() ~ "categorical")) %>% 
  add_p()
Tabla_GETNE_todos

#Anova one-way----

anova_diametro_AD <- aov(Diametro_AD ~ moment, data = GETNE_wide)
resid_diamtero_AD <- residuals (anova_diametro_AD)
summary(anova_diametro_AD)
shapiro.test(resid_diamtero_AD)
leveneTest(Diametro_AD ~ moment, data = GETNE_wide) 

Tabla_GETNE_cardiopatia <- GETNE %>% 
  filter(Cardiopatia_carcinoide == "No" | Cardiopatia_alDx == "No") %>% 
  select(Sexo, Edad, Estado_general, Localizacion_primario, Metastasis_hepaticas,
         Metastasis_peritoneales, Metastasis_ganglionares, Ki_67, Cromogranina_A_categ,
         Diametro_AD_TAC_BASAL, Diametro_AI_TAC_BASAL, Diametro_VD_TAC_BASAL, Diametro_VI_TAC_BASAL,
         RATIO_AD_AI_TAC_BASAL, RATIO_VD_VI_TAC_BASAL, Inclinacion_septo_ventricular_TAC_BASAL, 
         Cardiopatia_carcinoide, CX_VALVULAR,
         Tratamiento_sistemico_recibido) %>% 
  tbl_summary(by = Cardiopatia_carcinoide, type = list(where(is.numeric) ~ "continuous",
                                                       all_dichotomous() ~ "categorical")) %>% 
  add_p()
Tabla_GETNE_cardiopatia

##Evaluando las diferencias entre los que desarrollaron cardiopatía carcinoide de forma posterior

Tabla_GETNE_cardiopatia_2 <- GETNE %>% 
  filter(Cardiopatia_carcinoide == "Si") %>% 
  select(Sexo, Estado_general, Cardiopatia_alDx, Localizacion_primario, Metastasis_hepaticas,
         Metastasis_peritoneales, Metastasis_ganglionares, Ki_67, Cromogranina_A_categ,
         Diametro_AD_TAC_BASAL, Diametro_AI_TAC_BASAL, Diametro_VD_TAC_BASAL, Diametro_VI_TAC_BASAL,
         RATIO_AD_AI_TAC_BASAL, RATIO_VD_VI_TAC_BASAL, Inclinacion_septo_ventricular_TAC_BASAL, 
         Cardiopatia_carcinoide, Diametro_AD_TAC_Dx_Cardiopatia, Diametro_AI_TAC_Dx_Cardiopatia,
         Diametro_VD_TAC_Dx_Cardiopatia, Diametro_VI_TAC_Dx_Cardiopatia, RATIO_AD_AI_TAC_Dx_Cardiopatia,
         RATIO_VD_VI_TAC_Dx_Cardiopatia, Inclinacion_septo_ventricular_TAC_Dx_Cardiopatia, CX_VALVULAR,
         Tratamiento_sistemico_recibido) %>% 
  tbl_summary(by = Cardiopatia_alDx, type = list(where(is.numeric) ~ "continuous",
                                                 all_dichotomous() ~ "categorical")) %>% 
  add_p()
Tabla_GETNE_cardiopatia_2


anova_diametro_AI <- aov(Diametro_AI ~ moment, data = GETNE_wide)
resid_diamtero_AI <- residuals (anova_diametro_AI)
shapiro.test(resid_diamtero_AI)
summary(anova_diametro_AI)