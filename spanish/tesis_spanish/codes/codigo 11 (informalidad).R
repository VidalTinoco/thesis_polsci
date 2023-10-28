#### Tesis #####
##### Codigo 11: informalidad plots y exportadoras#####

# rm (list=ls ()) # para vaciar el workspace donde vas a trabajar

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
library (stringr)
library (here)


### Cargando bases de datos 
df <- read_xlsx(here("data", "informalidad_trim_mich.xlsx"))

inf_ag <- read_xlsx(here("data", "inegi_enoe_clean_mich.xlsx"))

### cambiamos trimestre por variable de fecha
class(df$trimestre)
df <- df %>% mutate(trimestre_date = as.Date(trimestre))
class(df$trimestre_date)


### cambiamos variables a factor
inf_ag <- inf_ag %>% 
  mutate(State = factor(State),
         Municipality = factor(Municipality),
         Group = factor(Group),
         Subgroup = factor(Subgroup),
         Formal_Informal = factor(`Classification of Formal and Informal Jobs of the First Activity`))
summary(inf_ag)

### exploramos variables y recodificamos

##Group
sort(unique(inf_ag$Group))

inf_ag <- inf_ag %>% mutate(Group = recode(Group,
                                           "Otros Trabajadores en Actividades AgrÃ­colas, Ganaderas, Forestales, Caza y Pesca, no Clasificados Anteriormente" = "Otros Trabajadores en Actividades Agricolas, Ganaderas, Forestales, Caza y Pesca, no Clasificados Anteriormente",
                                           "Trabajadores en Actividades AgrÃ­colas y Ganaderas" = "Trabajadores en Actividades Agricolas y Ganaderas"))

##Subgroup
sort(unique(inf_ag$Subgroup))

inf_ag <- inf_ag %>% mutate(Subgroup = recode(Subgroup,
                                              "Otros Trabajadores en Actividades AgrÃ­colas, Ganaderas, Forestales, Caza y Pesca, no Clasificados Anteriormente" = "Otros Trabajadores en Actividades Agricolas, Ganaderas, Forestales, Caza y Pesca, no Clasificados Anteriormente",
                                              "Trabajadores en Actividades AgrÃ­cola" = "Trabajadores en Actividades Agricola",
                                              "Trabajadores en Actividades Ganaderas y en la CrÃ­a de Animales" = "Trabajadores en Actividades Ganaderas y en la Cria de Animales",
                                              "Trabajadores que Combinan Actividades AgrÃ­colas con Ganaderas" = "Trabajadores que Combinan Actividades Agricolas con Ganaderas",
                                              "Trabajadores en Actividades AgrÃ­colas" = "Trabajadores en Actividades Agricolas"))

summary(inf_ag)

## municipio
sort(unique(inf_ag$Municipality))

inf_ag <- inf_ag %>% mutate(Municipality = recode(Municipality,
                                                  "Ã\u0081lvaro ObregÃ³n" = "Alvaro Obregon",
                                                  "ApatzingÃ¡n" = "Apatzingan",
                                                  "BriseÃ±as" = "Brisenas",
                                                  "CherÃ¡n" = "Cheran",
                                                  "CoalcomÃ¡n de VÃ¡zquez Pallares" = "Coalcoman",
                                                  "CojumatlÃ¡n de RÃ©gules" = "Cojumatlan",
                                                  "CopÃ¡ndaro" = "Copandaro",
                                                  "IxtlÃ¡n" = "Ixtlan",
                                                  "JimÃ©nez" = "Jimenez",
                                                  "JosÃ© Sixto Verduzco" = "Jose Sixto Verduzco",
                                                  "LÃ¡zaro CÃ¡rdenas" = "Lazaro Cardenas",
                                                  "MÃºgica" = "Mugica",
                                                  "MaravatÃ­o" = "Maravatio",
                                                  "NocupÃ©taro" = "Nocupetaro",
                                                  "NumarÃ¡n" = "Numaran",
                                                  "PÃ¡tzcuaro" = "Patzcuaro",
                                                  "PajacuarÃ¡n" = "Pajacuaran",
                                                  "PanindÃ­cuaro" = "Panindicuaro",
                                                  "ParÃ¡cuaro" = "Paracuaro",
                                                  "PeribÃ¡n" = "Periban",
                                                  "PurÃ©pero" = "Purepero",
                                                  "PuruÃ¡ndiro" = "Puruandiro",
                                                  "QuerÃ©ndaro" = "Querendaro",
                                                  "TacÃ¡mbaro" = "Tacambaro",
                                                  "TancÃ­taro" = "Tancitaro",
                                                  "TarÃ­mbaro" = "Tarimbaro",
                                                  "TangancÃ­cuaro" = "Tangancicuaro",
                                                  "TingÃ¼indÃ­n" = "Tinguindin",
                                                  "Tiquicheo de NicolÃ¡s Romero" = "Tiquicheo",
                                                  "YurÃ©cuaro" = "Yurecuaro",
                                                  "ZinÃ¡paro" = "Zinaparo",
                                                  "ZinapÃ©cuaro" = "Zinapecuaro",
                                                  "ZitÃ¡cuaro" = "Zitacuaro"))
summary(inf_ag)

### agregamos variable de tratamiento

##cargamos df
var_trat <- read_csv(here("data", "df_final_general_year.csv"))

##exploramos
summary(var_trat)

## seleccionamos variables de interes
var_trat <- var_trat %>% select(mun,
                                trat_2)
var_trat

## eliminamos repetidos
var_trat <- var_trat %>% 
  filter( !duplicated(mun))
var_trat

### revisamos que los nombres de los municipios coincidan
summary(inf_ag$Municipality %in% var_trat$mun) ## 4 discrepancias:
inf_ag$Municipality[(inf_ag$Municipality %in% var_trat$mun)==FALSE] ## Cojumatlan

### recodificamos cojumatlan en var_trat
var_trat <- var_trat %>% 
  mutate(mun = recode(mun, 
                      "Cojumatlan de Regules" = "Cojumatlan")) #ya quedo

### hacemos el join
inf_full <- left_join(inf_ag, var_trat, by = c("Municipality" = "mun"))

##revisamos
summary(inf_full) #quedo mamalona

### revisamos variable de trimestre
sort(unique(inf_full$Quarter)) ## hay que cambiarla a fecha

## hacemos dos variables con substring para year y quarter
inf_full <- inf_full %>% 
  mutate(year = substr(Quarter,1,4),
         q = substr(Quarter, 7, 7))
view(inf_full)

## recortamos hasta 2020
inf_full <- inf_full %>% 
  filter(year <= 2020)

## creamos variable de fecha con las dos ultimas variables que creamos
inf_full <- inf_full %>% 
  mutate(year = as.numeric(year),
         q = as.numeric(q),
         date = ymd(paste(year, q + 1, "01")) - 1)
summary(inf_full$date) #ya jala, se que no es por cuarto pero sirve igual






#### graficamos

###inf ag
summary(inf_full)

## cambiamos municipio a factor
inf_full <- inf_full %>% mutate(Municipality = factor(Municipality))
summary(inf_full)

## creamos variables de interes
inf_full <- inf_full %>% 
  group_by(Quarter,
           Formal_Informal) %>% 
  mutate(WF_FI_Qua = sum(Workforce)) %>% 
  ungroup


## borrador
(inf_00 <- ggplot (
  data=inf_full, 
  aes(x=date, y=WF_FI_Qua, color = Formal_Informal))
  + geom_line()
  + geom_point()
)


## poniendola wapa
(inf_ag <- ggplot (
  data=inf_full, 
  aes(x=date, y=WF_FI_Qua, color = Formal_Informal))
  + geom_line()
  + geom_point()
  + geom_vline(xintercept= ymd(20110101), linetype=2, show.legend = TRUE)
  + geom_text(aes(x = ymd(20110801), label="2011", y=25000), angle=0, color = "seagreen")
  + labs(title = "Workers per Type of Employment",
         x = "Quarter", y = "Workers", color = "Type of Employment",
         caption = "Source: ENOE")
  + scale_color_manual(labels = c("Formal Employment", "Informal Employment"), values = c("deepskyblue3", "firebrick1")) +
    theme_clean()
)

##la guardamos
ggsave("informality_agjobs.png", inf_ag, path = "graficas y mapas/Final/Employment", dpi = "retina", width = 7, height = 3)

## y si hacemos geom_smooth
(inf_ag <- ggplot (
  data=inf_full, 
  aes(x=date, y=WF_FI_Qua, color = Formal_Informal))
  + geom_smooth()
  + geom_point()
  + geom_vline(xintercept= ymd(20110101), linetype=2, show.legend = TRUE)
  + geom_text(aes(x = ymd(20110801), label="2011", y=25000), angle=0, color = "seagreen")
  + geom_vline(xintercept= ymd(20150401), linetype=2, show.legend = TRUE)
  + geom_text(aes(x = ymd(20151101), label="2015", y=0), angle=0, color = "red")
  + labs(#title = "Workers per Type of Employment",
         x = "Trimestre", y = "Empleados", color = "Tipo de Empleo")
  + scale_color_manual(labels = c("Empleo Formal", "Empleo Informal"), values = c("deepskyblue3", "firebrick1")) +
    theme_clean()
) ##se ve mas chida

##la guardamos
ggsave("informality_agjobs_workers_smooth.png", inf_ag, path = "graficas", dpi = "retina", width = 7, height = 3)


### ahora filtramos por trat
summary(inf_full$trat_2)

inf_ag_trat <- inf_full %>% filter(trat_2 == 1) 
inf_ag_cont <- inf_full %>% filter(trat_2 == 0)

## graficamos tratados
(inf_trat <- ggplot (
  data=inf_ag_trat, 
  aes(x=date, y=WF_FI_Qua, color = Formal_Informal))
  + geom_line()
  + geom_point()
  + geom_vline(xintercept= ymd(20110101), linetype=2, show.legend = TRUE)
  + geom_text(aes(x = ymd(20110801), label="2011", y=25000), angle=0, color = "seagreen")
  + labs(#title = "Type of Employment",
         subtitle = "Grupo de Tratamiento",
         x = "Trimestre", y = "Empleados", color = "Tipo de Empleo")
  + scale_color_manual(labels = c("Empleo Formal", "Empleo Informal"), values = c("deepskyblue3", "firebrick1")) +
    theme_clean()
)


##la guardamos
ggsave("informality_agjobs_treat.png", inf_trat, path = "graficas", dpi = "retina", width = 7, height = 3)


## graficamos control
(inf_cont <- ggplot (
  data=inf_ag_cont, 
  aes(x=date, y=WF_FI_Qua, color = Formal_Informal))
  + geom_line()
  + geom_point()
  + geom_vline(xintercept= ymd(20110101), linetype=2, show.legend = TRUE)
  + geom_text(aes(x = ymd(20110801), label="2011", y=25000), angle=0, color = "seagreen")
  + labs(title = "Type of Employment",
         subtitle = "Control Group",
         x = "Quarter", y = "Workers", color = "Type of Employment",
         caption = "Source: ENOE")
  + scale_color_manual(labels = c("Formal Employment", "Informal Employment"), values = c("deepskyblue3", "firebrick1")) +
    theme_clean()
)

##la guardamos
ggsave("informality_agjobs_cont.png", inf_cont, path = "graficas y mapas/Final", dpi = "retina", width = 7, height = 3)

### hacemos lo mismo que lo anterior pero con salarios promedio

## creamos variables de interes agrupando por quarter
inf_full <- inf_full %>% 
  group_by(Quarter,
           Formal_Informal) %>% 
  mutate(Wage_FI_Qua = mean(`Monthly Wage`, na.rm = TRUE)) %>% 
  ungroup
summary(inf_full$Wage_FI_Qua)


## borrador
(te_w_00 <- ggplot (
  data=inf_full, 
  aes(x=date, y=Wage_FI_Qua, color = Formal_Informal))
  + geom_line()
  + geom_point()
)

## poniendola wapa
(te_w_1 <- ggplot (
  data=inf_full, 
  aes(x=date, y=Wage_FI_Qua, color = Formal_Informal))
  + geom_line()
  + geom_point()
  + geom_vline(xintercept= ymd(20110101), linetype=2, show.legend = TRUE)
  + geom_text(aes(x = ymd(20110801), label="2011", y=2500), angle=0, color = "seagreen")
  + labs(title = "Mean Wage per Type of Employment",
         x = "Quarter", y = "Mean Wage", color = "Type of Employment",
         caption = "Source: ENOE")
  + scale_color_manual(labels = c("Formal Employment", "Informal Employment"), values = c("deepskyblue3", "firebrick1")) +
    theme_clean()
)

##la guardamos
ggsave("informality_agjobs_meanwage.png", te_w_1, path = "graficas y mapas/Final", dpi = "retina", width = 8, height = 3)

## y si hacemos geom_smooth
(te_w_1 <- ggplot (
  data=inf_full, 
  aes(x=date, y=Wage_FI_Qua, color = Formal_Informal))
  + geom_smooth()
  + geom_point()
  + geom_vline(xintercept= ymd(20110101), linetype=2, show.legend = TRUE)
  + geom_text(aes(x = ymd(20110801), label="2011", y=2500), angle=0, color = "seagreen")
  + labs(#title = "Mean Wage per Type of Employment",
         x = "Trimestre", y = "Salario Promedio", color = "Tipo de Empleo")
  + scale_color_manual(labels = c("Empleo Formal", "Empleo Informal"), values = c("deepskyblue3", "firebrick1")) +
    theme_clean()
)

##la guardamos
ggsave("informality_agjobs_meanwage_smooth.png", te_w_1, path = "graficas", dpi = "retina", width = 8, height = 3)

### ahora filtramos por trat
summary(inf_full$trat_2)

inf_ag_trat <- inf_full %>% filter(trat_2 == 1) 
inf_ag_cont <- inf_full %>% filter(trat_2 == 0)

## unidades tratadas
(te_w_trat <- ggplot (
  data=inf_ag_trat, 
  aes(x=date, y=Wage_FI_Qua, color = Formal_Informal))
  + geom_line()
  + geom_point()
  + geom_vline(xintercept= ymd(20110101), linetype=2, show.legend = TRUE)
  + geom_text(aes(x = ymd(20110801), label="2011", y=2500), angle=0, color = "seagreen")
  + labs(title = "Mean Wage per Type of Employment",
         subtitle = "Treatment",
         x = "Quarter", y = "Mean Wage", color = "Type of Employment",
         caption = "Source: ENOE")
  + scale_color_manual(labels = c("Formal Employment", "Informal Employment"), values = c("deepskyblue3", "firebrick1")) +
    theme_clean()
)

##la guardamos
ggsave("informality_agjobs_meanwage_trat.png", te_w_trat, path = "graficas y mapas/Final", dpi = "retina", width = 8, height = 3)

## unidades de control
(te_w_control <- ggplot (
  data=inf_ag_cont, 
  aes(x=date, y=Wage_FI_Qua, color = Formal_Informal))
  + geom_line()
  + geom_point()
  + geom_vline(xintercept= ymd(20110101), linetype=2, show.legend = TRUE)
  + geom_text(aes(x = ymd(20110801), label="2011", y=2500), angle=0, color = "seagreen")
  + labs(title = "Mean Wage per Type of Employment",
         subtitle = "Control",
         x = "Quarter", y = "Mean Wage", color = "Type of Employment",
         caption = "Source: ENOE")
  + scale_color_manual(labels = c("Formal Employment", "Informal Employment"), values = c("deepskyblue3", "firebrick1")) +
    theme_clean()
)

##la guardamos
ggsave("informality_agjobs_meanwage_cont.png", te_w_control, path = "graficas y mapas/Final", dpi = "retina", width = 8, height = 3)


##borrador
(inf_0 <- ggplot (
  data=df, 
    aes(x=trimestre_date, y=til_1))
  + scale_x_date(labels = date_format("%Y-%m-%d"))
  + geom_line()
  + geom_vline(xintercept = ymd(20110101), linetype = 2)
)

## ponienedola wapa
(inf <- ggplot (
  data=df, 
  aes(x=trimestre_date, y=til_1))
  + scale_x_date(labels = date_format("%Y-%m-%d"))
  + geom_line(color = "blue")
  + geom_vline(xintercept = ymd(20110101), linetype = 2, show.legend = TRUE, color = "red")
  + geom_text(aes(x = ymd(20111201), label="2011", y=68), angle=0, color = "black")
  + labs(#title = "Labor Informality Rate in Michoacan",
         x = "Trimestre", y = "Tasa de Informalidad Laboral")
  + theme_clean()
)

## guardandola
ggsave("labor_informality_rate_mich.png", inf, path = "graficas", dpi = "retina", width = 8, height = 3)

## borrador 2


