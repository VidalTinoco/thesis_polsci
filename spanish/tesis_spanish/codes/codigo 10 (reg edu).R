#### Tesis #####
##### Codigo 10: reg education #####

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
library (lme4)
library (here)
library (plm)



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
  mutate(mun = factor(mun),
         pto = factor(pto),
         trat_2 = factor(trat_2),
         upper = factor(upper),
         lower = factor(lower)
         )
summary(df_ul)


###Two-Way Fixed-Effects (TWFE) y errores agrupados por municipio
# summary(mod <- felm (gini_mean ~ (trat_2*pto) + upper | mun + year | 0 | mun, data=df_ul)) ### este ya no

### Efectos heterogeneos errores agrupados por municipios
summary(mod_ef_al <- felm(gini_mean ~ (trat_2*pto*upper) | 0 | 0 | mun, data=df_ul))


### Tabla
stargazer(mod_ef_al
          #, title = "TABLE V. Effects of avocado production on inequality"
          , omit.stat=c ("f", "ser", "aic", "bic", "ll")
          , dep.var.caption = ""
          , covariate.labels = c("Municipio Aguacatero", 
                                 "2011",
                                 "Educación",
                                 "Municipio Aguacatero * 2011",
                                 "Municipio Aguacatero * Educación",
                                 "2011 * Educación",
                                 "Municipio Aguacatero * 2011 * Educación")
          , dep.var.labels = c("Gini")
          , notes.append = F
          , notes.align = "l"
          , notes = c("Errores estándar robustos, agrupados por municipio, están en paréntesis.",
                      "Coeficientes significativamente diferentes de cero",
                      "están denotados por el siguiente sistema:",
                      "*10%, **5%, y ***1%.")
          , type="html", out="tablas/tabla5_reg_heteff_rename.html")

