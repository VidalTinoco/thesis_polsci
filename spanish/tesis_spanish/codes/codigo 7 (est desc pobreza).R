#### Tesis #####
##### Codigo 21: code master 7 (est desc pobreza)#####

#rm (list=ls ()) # para vaciar el workspace donde vas a trabajar

### llamando a las librerias 
library (dplyr)
library (ggplot2)
library (PanelMatch)
library (readstata13)
library (readxl)
library (Synth)
library (tidyverse)
library (tidyr)
library (viridis)
library (ggthemes)
library (lubridate)
library (scales)
library (stargazer)
library (patchwork)
library (here)

###Cargando base de datos agricolas
coneval <- read_xlsx(here("data", "concentrado_ind_de_pob_2020_clean.xlsx"))


### Cambiando variables a factores
coneval <- coneval %>% 
  mutate(Clave_entidad = factor(Clave_entidad),
         Entidad_federativa = factor(Entidad_federativa))
summary(coneval)  
coneval  

### filtramos mich
con_mich <- coneval %>% 
  filter(Clave_entidad == "16")
con_mich  

### revisamos variables que nos interesan
ls(con_mich)

### las seleccionamos
con_mich <- con_mich %>% 
  select(Clave_entidad,
         Entidad_federativa,
         PobExt_Porcentaje2010,
         PobExt_Porcentaje2015,
         PobExt_Porcentaje2020,
         Pobreza_Porcentaje2010,
         Pobreza_Porcentaje2015,
         Pobreza_Porcentaje2020,
         PobrMod_Porcentaje2010,
         PobMod_Porcentaje2015,
         PobMod_Porcentaje2020
  )

### cambiamos sentido del df
conmich_hor <- con_mich %>% 
  gather(key = "var",
         value = "porcentaje",
         PobExt_Porcentaje2010,
         PobExt_Porcentaje2015,
         PobExt_Porcentaje2020,
         Pobreza_Porcentaje2010,
         Pobreza_Porcentaje2015,
         Pobreza_Porcentaje2020,
         PobrMod_Porcentaje2010,
         PobMod_Porcentaje2015,
         PobMod_Porcentaje2020)

conmich_hor

### dividimos a tres df
pobext <- conmich_hor %>% 
  filter(var == "PobExt_Porcentaje2010" |
         var == "PobExt_Porcentaje2015" |
         var == "PobExt_Porcentaje2020")
pobext

pob <- conmich_hor %>% 
  filter(var == "Pobreza_Porcentaje2010" |
           var == "Pobreza_Porcentaje2015" |
           var == "Pobreza_Porcentaje2020")
pob

pobmod <- conmich_hor %>% 
  filter(var == "PobrMod_Porcentaje2010" |
           var == "PobMod_Porcentaje2015" |
           var == "PobMod_Porcentaje2020")


### hacemos los plots comparando years

##porcentaje pobreza extrema 
##graf simple
(gs <- ggplot(data = pobext,
                     aes(var, porcentaje)) +
    geom_point()
  
)

## poniendola wapa
pobext$var[pobext$var == "PobExt_Porcentaje2010"] <- "2010"
pobext$var[pobext$var == "PobExt_Porcentaje2015"] <- "2015"
pobext$var[pobext$var == "PobExt_Porcentaje2020"] <- "2020"
pobext

(pob_ext <- ggplot(data = pobext,
                   aes(var, porcentaje)) +
    geom_point(color = "seagreen",
               shape = 17,
               size = 2) +
    labs(title = "Percentage of the population in extreme poverty",
         subtitle = "Michoacan",
         x = "Year", y = "Percentaje in extreme poverty",
         caption = "Source: CONEVAL") +
    theme_clean()
)

ggsave("extpov_mich_year.png", pob_ext, path = "graficas y mapas/Final/Context", dpi = "retina")

##porcentaje en pobreza
pob
pob$var[pob$var == "Pobreza_Porcentaje2010"] <- "2010"
pob$var[pob$var == "Pobreza_Porcentaje2015"] <- "2015"
pob$var[pob$var == "Pobreza_Porcentaje2020"] <- "2020"
pob

(pob_plot <- ggplot(data = pob,
                   aes(var, porcentaje)) +
    geom_point(color = "seagreen",
               shape = 17,
               size = 2) +
    labs(title = "Percentage of the population in poverty",
         subtitle = "Michoacan",
         x = "Year", y = "Percentaje in poverty",
         caption = "Source: CONEVAL") +
    theme_clean()
)

ggsave("pov_mich_year.png", pob_plot, path = "graficas y mapas/Final/Context", dpi = "retina")

##porcentaje en pobreza moderada
pobmod
pobmod$var[pobmod$var == "PobrMod_Porcentaje2010"] <- "2010"
pobmod$var[pobmod$var == "PobMod_Porcentaje2015"] <- "2015"
pobmod$var[pobmod$var == "PobMod_Porcentaje2020"] <- "2020"
pobmod

(pob_mod <- ggplot(data = pobmod,
                    aes(var, porcentaje)) +
    geom_point(color = "seagreen",
               shape = 17,
               size = 2) +
    labs(title = "Percentage of the population in moderate poverty",
         subtitle = "Michoacan",
         x = "Year", y = "Percentaje in moderate poverty",
         caption = "Source: CONEVAL") +
    theme_clean()
)

ggsave("povmod_mich_year.png", pob_mod, path = "graficas y mapas/Final/Context", dpi = "retina")


### juntamos las graficas
pov_per_year <- (pob_ext / pob_mod / pob_plot)

ggsave("pov_year_emt.png", pov_per_year, path = "graficas y mapas/Final/Context", dpi = "retina", width = 7, height = 7)


  