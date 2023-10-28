#### Tesis #####
##### Codigo 13: DAGs #####

rm (list=ls ()) # para vaciar el workspace donde vas a trabajar

### llamando a las librerias 
library(tidyverse)
library(ggdag)
library(ggthemes)

## layouts: ('star', 'circle', 'gem', 'dh', 'graphopt'OK, 'grid', 'mds', 'randomly', 'fr'OK, 'kk'OK, 'drl', 'lgl') ##

### Qué es lo que esperamos
hip_0 <- dagify(ineq ~ agric,
                labels = c("ineq" = "Desigualdad",
                           "agric" = "Comercio\n Agrícola"),
                outcome = "ineq",
                exposure = "agric",
                coords = list(x = c( "agric"= 1, "ineq" = 3),
                              y = c("agric" = 2, "ineq" = 2)))
d_hip_0 <- ggdag_status(hip_0, 
                        text = FALSE, 
                        #use_labels = "label",
                        #stylized = TRUE,
                        #check_overlap = TRUE,
                        layout = 'fr',
                        )

d_hip_0 <- d_hip_0 + theme_dag_blank(legend.position = "none") +
  scale_color_manual(values = c("seagreen", "dodgerblue2")) +
  geom_dag_label_repel(aes(label = label), colour = "black", show.legend = FALSE) +
  geom_dag_edges_arc(edge_color = "orange", curvature = 0)

ggsave("dag_hip0.png", d_hip_0, path = "graficas", dpi = "retina",
       width = 7, height = 3.5)


### literatura

## Primera parte

lit_1 <- dagify(ineq ~ val,
                val ~ prod + com,
                prod ~ dem,
                com ~ dem,
                dem ~ merk,
                labels = c("ineq" = "Desigualdad",
                           "merk" = "Tamaño\n Mercado",
                           "dem" = "Demanda",
                           "val" = "Valor\n Cultivos",
                           "prod" = "Producción",
                           "com" = "Comercio"),
                outcome = "ineq",
                exposure = "val",
                latent = "merk",
                coords = list(x = c("merk"= 1, "dem" = 2, "prod" = 3, "com" = 3, "val" = 4, "ineq" = 5),
                              y = c("merk" = 2, "dem" = 2, "prod" = 3, "com"= 1, "val" = 2, "ineq" = 2)
                              )
  
)

dag_l1 <- ggdag_status(lit_1, 
                        text = FALSE, 
                        #use_labels = "label",
                        #stylized = TRUE,
                        check_overlap = TRUE,
                        layout = 'fr',
)

dag_l1 <- dag_l1 +
  theme_dag_blank(legend.position = "none") +
  scale_color_manual(values = c("seagreen", "orange", "dodgerblue2")) +
  geom_dag_label_repel(aes(label = label), colour = "black", show.legend = FALSE)
  

ggsave("dag_lit1.png", dag_l1, path = "graficas", dpi = "retina",
       width = 7, height = 3.5)


### Resumen lit
res_lit <- dagify(ineq ~ sal,
                sal ~ com,
                com ~ prod + acc,
                prod ~ mod,
                labels = c("ineq" = "Desigualdad",
                           "sal" = "Salarios",
                           "com" = "Comercio\n Agricola",
                           "prod" = "Cap. de\n Producción",
                           "acc" = "Acceso a\n Mercados",
                           "mod" = "Modernización"),
                outcome = "ineq",
                exposure = "com",
                latent = "sal",
                coords = list(x = c("mod"= 1, "prod" = 2, "acc" = 2, "com" = 3, "sal" = 4, "ineq" = 5),
                              y = c("mod" = 3, "prod" = 3, "acc" = 1, "com"= 2, "sal" = 2, "ineq" = 2)
                )
                
)

dag_rs <- ggdag_status(res_lit, 
                       text = FALSE, 
                       #use_labels = "label",
                       #stylized = TRUE,
                       check_overlap = TRUE,
                       layout = 'fr',
)

dag_rs <- dag_rs +
  theme_dag_blank(legend.position = "none") +
  scale_color_manual(values = c("seagreen", "orange", "dodgerblue2")) +
  geom_dag_label_repel(aes(label = label), colour = "black", show.legend = FALSE)

ggsave("dag_rs.png", dag_rs, path = "graficas", dpi = "retina",
       width = 8, height = 3.5)

