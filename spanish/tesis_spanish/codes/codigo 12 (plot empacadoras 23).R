#### Tesis #####
##### Codigo 12: plot empacadoras#####

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
library (here)


### Cargando bases de datos 
df <- read_xlsx(here("data", "empacadoras_23.xlsx"))

### explorando
df
summary(df)


### ploteando
p1 <- ggplot(df) + 
  geom_histogram(aes(year_const)) +
  theme_clean()
p1

### creando una variable para marcar antes y despues de 2011
df <- df %>% 
  mutate(trat = ifelse(year_const >= 2011, "Entrada total al mercado EUA", "Antes de 2011"))

### volviendo a plotear
p2 <- ggplot(df, aes(year_const, fill = factor(trat))) +
  geom_histogram()
p2

### poniendola wapa
fp <- ggplot(df, aes(year_const, fill = factor(trat))) +
  geom_histogram() +
  scale_fill_manual(values = c("darkolivegreen3", "darkgreen")) +
   guides(fill=guide_legend(
     title='Periodo de Tratamiento')
     ) +
  xlab("Año") + ylab("Empresas Empacadoras de Aguacate") +
  #labs(caption = "Source: SIGER") +
  #ggtitle("Evocado Exporters per Year") +
  theme_clean()
fp


### la guardamos
ggsave("empacadoras_year.png", fp, path = "graficas", dpi = "retina", width = 7, height = 3)

