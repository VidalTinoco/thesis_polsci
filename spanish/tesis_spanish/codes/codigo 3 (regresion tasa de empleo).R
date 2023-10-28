#### Tesis #####
##### Codigo 3 regresion tasa de empleo #####

#rm (list=ls ()) # para vaciar el workspace donde vas a trabajar

### llamando a las librerias 
library (dplyr)
library (ggplot2)
library (lfe)
library (readxl)
library (stargazer)
library (tidyverse)
library (tidyr)
library (viridis)
library (ggthemes)
library (lubridate)
library (scales)
library (fixest)
library (ggiplot)
library (here)



### Cargando bases de datos 
df <- read_csv(here("data", "year_mun_workers_pob.csv"))

### cambiando variables a factor
df <- df %>% 
  mutate(mun = factor(mun),
         pto = factor(pto),
         trat_2 = factor(trat_2))

### corriendo regresion siendo el outcome log_tasa
  ##Two-Way Fixed-Effects (TWFE) y errores agrupados por municipio
summary(mod <- felm(log_tasa ~ (trat_2*pto) | mun + year | 0 | mun, data = df))

### Tabla
stargazer(mod
          #, title = "TABLE IV. Effects of avocado production on employment"
          , omit.stat=c ("f", "ser", "aic", "bic", "ll")
          , dep.var.caption = ""
          , covariate.labels = c("Municipio Aguacatero", "2011", "Municipio Aguacatero * 2011")
          , dep.var.labels = c("Log Tasa de Empleo por 100 hab.") 
          , notes.append = F
          , notes.align = "l"
          , notes = c("Errores estándar robustos,",
                      "agrupados por municipio,", 
                      "están en paréntesis.", 
                      "Incluye efectos fijos por municipio y tiempo.",
                      "Coeficientes significativamente diferentes de cero",
                      "están denotados por el siguiente sistema:",
                      "*10%, **5%, y ***1%.")
          , type="html", out="tablas/tabla2_regresion_nivelempleo_year.html")

### Comparando tendencias: Aguacateros vs No Aguacateros
##Grafica sin embellecer
df_na <- df %>% filter(trat_2 != "NA")
summary(df_na)

(ag0 <- ggplot (
  data=df_na %>% group_by(trat_2, year) %>% summarise(log_tasa = mean (log_tasa, na.rm=T))
  , aes(x=year, y=log_tasa, color=trat_2))
  + geom_vline(xintercept=2011, linetype=2)
  + geom_line()
) 

##Embelleciendo grafica
(ag <- ggplot (
  data=df_na %>% group_by(trat_2, year) %>% summarise(log_tasa = mean (log_tasa, na.rm=T))
  , aes(x=year, y=log_tasa, color=trat_2))
  + geom_vline(xintercept=2011, linetype=2, show.legend = TRUE)
  + geom_line()
  + geom_text(aes(x=2011.9, label="2011", y=1.75), angle=0, color = "red")
  + labs(#title = "Nivel de Empleo por Municipios Aguacateros y No Aguacateros",
         x = "Año", y = "Log Nivel de Empleo 
         por 1000 hab.", color = "Tratamiento")
  + scale_color_manual(labels = c("No Aguacateros", "Aguacateros"), values = c("tan3", "seagreen")) +
    theme_clean()
) 

ggsave("plot_empleo_trat_year.png", ag, path = "graficas", dpi = "retina",
       width = 9, height = 3.5)

### Event plot
# 
# ###modificando tipo de variables
# df <- df %>% 
#   mutate(trat_2_num = as.numeric(trat_2))
# class(df$trat_2_num)
# 
# ###Creando variable time_to_treat
# df <- df %>% 
#   mutate(time_to_treat = ifelse(trat_2_num == 1, year - 2011, 0))
# summary(df)
# 
# 
# ### Event study models
# mod_twfe = feols(log_tasa ~ i(time_to_treat, trat_2_num, ref = -1)|mun + year,  cluster = ~mun, data = df)
# summary(mod_twfe)
# 
# ### Event Plot
# event_plot_mix <- ggiplot(mod_twfe,
#                           aggr_eff = "post", aggr_eff.par = list(col = "blue"),
#                           main = 'Event Plot: Effect on Employment (TWFE)',
#                           xlab = 'Time to treatment',
#                           geom_style = 'ribbon',
#                           col = 'tomato1',
#                           theme = theme_clean())
# event_plot_mix
# 
# ggsave("event_plot_mix_employment(tabla2).png", event_plot_mix, path = "graficas y mapas/Final", dpi = "retina")













