#### Tesis #####
##### Codigo 24: education plot#####

#rm (list=ls ()) # para vaciar el workspace donde vas a trabajar

### llamando a las librerias 
library (did)
library (dplyr)
library (ggplot2)
library (lfe)
library (PanelMatch)
library (readstata13)
library (readxl)
library (stargazer)
library (Synth)
library (tidyverse)
library (tidyr)
library (viridis)
library (ggthemes)
library (lubridate)
library (scales)
library (broom)
library (fixest)
library (patchwork)
library (here)


###Cargando base de datos
ed <- read_csv(here("data", "df_ed_2005_mun.csv"))

df <- read_csv(here("data", "df_final_general_year.csv"))


### revisamos que los nombres de los municipios coincidan
summary(ed$nom_mun %in% df$mun) ## 38 discrepancias:
ed$nom_mun[(ed$nom_mun %in% df$mun)==FALSE] ###son los que tienen acento

### quitamos acentos
ed_sa <- ed %>% 
  mutate(nom_mun = recode(nom_mun, 
                          "Álvaro Obregón" = "Alvaro Obregon",
                          "Apatzingán" = "Apatzingan",
                          "Briseñas" = "Brisenas",
                          "Carácuaro" = "Caracuaro",
                          "Coalcomán de Vázquez Pallares" = "Coalcoman",
                          "Copándaro" = "Copandaro",
                          "Cherán" = "Cheran",
                          "Chucándiro" = "Chucandiro",
                          "Erongarícuaro" = "Erongaricuaro",
                          "Ixtlán" = "Ixtlan",
                          "Jiménez" = "Jimenez",
                          "Juárez" = "Juarez",
                          "Maravatío" = "Maravatio",
                          "Lázaro Cárdenas" = "Lazaro Cardenas",
                          "Múgica" = "Mugica",
                          "Nocupétaro" = "Nocupetaro",
                          "Numarán" = "Numaran",
                          "Pajacuarán" = "Pajacuaran",
                          "Panindícuaro" = "Panindicuaro",
                          "Parácuaro" = "Paracuaro",
                          "Pátzcuaro" = "Patzcuaro",
                          "Peribán" = "Periban",
                          "Purépero" = "Purepero",
                          "Puruándiro" = "Puruandiro",
                          "Queréndaro" = "Querendaro",
                          "Cojumatlán de Régules" = "Cojumatlan de Regules",
                          "Tacámbaro" = "Tacambaro",
                          "Tancítaro" = "Tancitaro",
                          "Tangancícuaro" = "Tangancicuaro",
                          "Tarímbaro" = "Tarimbaro",
                          "Tingüindín" = "Tinguindin",
                          "Tiquicheo de Nicolás Romero" = "Tiquicheo",
                          "Tumbiscatío" = "Tumbiscatio",
                          "Yurécuaro" = "Yurecuaro",
                          "Zináparo" = "Zinaparo",
                          "Zinapécuaro" = "Zinapecuaro",
                          "Zitácuaro" = "Zitacuaro",
                          "José Sixto Verduzco" = "Jose Sixto Verduzco"
                          ))
summary(ed_sa$nom_mun %in% df$mun) ## 0 discrepancias

### hacemos el join
summary(df)
summary(ed_sa)

df_ul <- left_join(df, ed_sa, by = c("mun" = "nom_mun"))
df_ul

### convirtiendo variables a factores
summary(df_ul)
df_ul <- df_ul %>% 
  mutate(mun = factor(mun))
summary(df_ul)

###Plot

##creamos otro df
plot_data <- df_ul %>%
  mutate(ed_t = ifelse(upper == 1, "Arriba de la media", "Abajo de la media"),
         ed_t = factor(ed_t),
         pto_f = ifelse(pto == 1, "(b) A partir de 2011", " (a) Antes de 2011"),
         pto_f = factor(pto_f)) %>% 
  group_by(ed_t, pto_f) %>% 
  summarise(mean_gini = mean(gini_mean),
            se_gini = sd(gini_mean, na.rm = T) / sqrt(n()),
            upper = mean_gini + (-1.96 * se_gini),
            lower = mean_gini + (1.96 * se_gini))

summary(plot_data)


## el plot
(dd_plot_a <- ggplot(plot_data, aes(x = pto_f, y = mean_gini, color = ed_t)) +
    #geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
    geom_point() +
    geom_line(aes(group = ed_t)) +
    labs(#title = "Gini mean per education level",
         x = "Periodo", y = "Gini",
         color = "Nivel de Educación") + 
    scale_color_manual(values = c("brown1", "cornflowerblue")) +
    theme_clean()
)


ggsave("ddplot_ed.png", dd_plot_a, path = "graficas", dpi = "retina",
       width = 7, height = 3.5)

### hacemos lo mismo pero para tratados y control
##hacemos nuevas df
dful_trat <- df_ul %>% 
  filter(trat_2 == 1)
summary(dful_trat)

dful_cont <- df_ul %>% 
  filter(trat_2 == 0)
summary(dful_cont)

## tratados

#creamos otro df
plot_data1 <- dful_trat %>%
  mutate(ed_t = ifelse(upper == 1, "Arriba de la media", "Abajo de la media"),
         ed_t = factor(ed_t),
         pto_f = ifelse(pto == 1, "(b) A partir de 2011", " (a) Antes de 2011"),
         pto_f = factor(pto_f)) %>% 
  group_by(ed_t, pto_f) %>% 
  summarise(mean_gini = mean(gini_mean),
            se_gini = sd(gini_mean, na.rm = T) / sqrt(n()),
            upper = mean_gini + (-1.96 * se_gini),
            lower = mean_gini + (1.96 * se_gini))

summary(plot_data1)


# el plot
(dd_plot_t <- ggplot(plot_data1, aes(x = pto_f, y = mean_gini, color = ed_t)) +
    #geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
    geom_point() +
    geom_line(aes(group = ed_t)) +
    labs(#title = "Gini mean per education level",
         subtitle = "Grupo de Tratamiento",
         x = "Periodo", y = "Gini",
         color = "Nivel de Educación") + 
    scale_color_manual(values = c("brown1", "cornflowerblue")) +
    theme_clean()
)


ggsave("ddplot_ed_trat.png", dd_plot_t, path = "graficas", dpi = "retina",
       width = 7, height = 3.5)

##control
#creamos otro df
plot_data2 <- dful_cont %>%
  mutate(ed_t = ifelse(upper == 1, "Arriba de la media", "Abajo de la media"),
         ed_t = factor(ed_t),
         pto_f = ifelse(pto == 1, "(b) A partir de 2011", " (a) Antes de 2011"),
         pto_f = factor(pto_f)) %>% 
  group_by(ed_t, pto_f) %>% 
  summarise(mean_gini = mean(gini_mean),
            se_gini = sd(gini_mean, na.rm = T) / sqrt(n()),
            upper = mean_gini + (-1.96 * se_gini),
            lower = mean_gini + (1.96 * se_gini))

summary(plot_data)


# el plot
(dd_plot_c <- ggplot(plot_data2, aes(x = pto_f, y = mean_gini, color = ed_t)) +
    #geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
    geom_point() +
    geom_line(aes(group = ed_t)) +
    labs(#title = "Gini mean per education level",
         subtitle = "Grupo de Control",
         x = "Periodo", y = "Gini",
         color = "Nivel de Educación") + 
    scale_color_manual(values = c("brown1", "cornflowerblue")) +
    theme_clean()
)


ggsave("ddplot_ed_control.png", dd_plot_c, path = "graficas", dpi = "retina",
       width = 7, height = 3.5)

###juntando plots
edudd_plots <- (dd_plot_a / dd_plot_t / dd_plot_c)

ggsave("edu_dd_plots_atc.png", edudd_plots, path = "graficas", dpi = "retina", width = 7, height = 7)
