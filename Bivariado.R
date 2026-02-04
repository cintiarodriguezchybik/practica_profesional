library(FactoMineR)
library(factoextra)
library(gplots)
library(cluster)
library(ggplot2)
library(dplyr)
library(janitor)
library(ggmosaic)
install.packages("factoextra")
install.packages("cluster")
install.packages("janitor")
install.packages("ggmosaic")
library(scales)
library(forcats)
library(tidyr)
#############################Relacion entre variables de aspecto personal##################

###Ingreso x Lugar.residencia

#agrupo las localidades menores en "Otros"
localidades_otros<-c("Albarellos","Carcaraña","Funes","Roldan","Las parejas","Villa Gobernador Galvez")

encuesta <- encuesta %>%
  mutate(Lugar.residencia.nuevo = case_when(
    Lugar.residencia %in% localidades_otros ~ "Otros",
    TRUE ~ Lugar.residencia),
    Lugar.residencia.nuevo = factor(Lugar.residencia.nuevo,
                                    levels = c("Zavalla", "Rosario", "Otros"))#reordeno las categorias
  )

#calculo las frecuencias y los porcentajes por año
porcentajes <- encuesta %>%
  count(Ingreso, Lugar.residencia.nuevo) %>%
  group_by(Ingreso) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()

ggplot_30<-
ggplot(porcentajes, aes(x = as.factor(Ingreso), y = porcentaje, 
                        fill = Lugar.residencia.nuevo)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +  # geom_col porque nyo puse los valores del eje y,position="fill" para porcentajes
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_discrete(breaks = c("Zavalla", "Rosario", "Otros")) +
  labs(x = "Año de Ingreso", 
       y = "Porcentaje",
       fill = "Lugar de\nresidencia") +
  theme_minimal() +
  theme_gray() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.3, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10))



###Ingreso x Tiene.trabajo

#calculo frecuencias y porcentajes 

porcentajes <- encuesta %>%
  count(Ingreso, Tiene.trabajo) %>%
  group_by(Ingreso) %>% #para que me haga el porcentaje dentro de cada año
  mutate(porcentaje = n / sum(n),
         etiqueta = ifelse(porcentaje >= 0.05,
                           scales::percent(porcentaje, accuracy = 1), 
                           "")
  ) %>%
  ungroup()%>%
  mutate(Tiene.trabajo=factor(Tiene.trabajo,levels=c("SÍ","NO"))) #para que aparezca primero SI

colores_2_categorias <- scales::hue_pal()(3)  # Extrae 2 colores de los 3 colores por defecto
#show_col(scales::hue_pal()(3))

ggplot_31<-
ggplot(porcentajes, aes(x = as.factor(Ingreso), y = porcentaje, 
                        fill = Tiene.trabajo)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +  # fill para porcentajes
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("SÍ" = colores_2_categorias[2],  # #F8766D (naranja/rojo)
                               "NO" = colores_2_categorias[1]), # #00BA38 (verde)
                    breaks = c("SÍ", "NO")) +
   labs(x = "Año de Ingreso", 
       y = "Porcentaje",
       fill = "¿Trabaja?") +
  theme_minimal() +
  theme_gray() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.3, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

###Ingreso x Personas.con.las.que.vive

porcentajes <- encuesta %>%
  count(Ingreso, Personas.con.las.que.vive) %>%
  group_by(Ingreso) %>% #para que me haga el porcentaje dentro de cada año
  mutate(porcentaje = n / sum(n),
         etiqueta = ifelse(porcentaje >= 0.05,
                           scales::percent(porcentaje, accuracy = 1), 
                           "")
  ) %>%
  ungroup()

ggplot_32<-
ggplot(porcentajes, aes(x = as.factor(Ingreso), y = porcentaje, 
                        fill = Personas.con.las.que.vive)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +  # fill para porcentajes
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Año de Ingreso", 
       y = "Porcentaje",
       fill = "¿Con quién vive?") +
  theme_minimal() +
  theme_gray() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.3, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

###Tiene.trabajo x Personas.con.las.que vive

porcentajes <- encuesta %>%
  count(Tiene.trabajo, Personas.con.las.que.vive) %>%
  group_by(Tiene.trabajo) %>% #para que me haga el porcentaje dentro de cada año
  mutate(porcentaje = n / sum(n),
         etiqueta = ifelse(porcentaje >= 0.05,
                           scales::percent(porcentaje, accuracy = 1), 
                           "")
  ) %>%
  ungroup()%>%
mutate(Tiene.trabajo = factor(Tiene.trabajo, levels = c("SÍ", "NO")))

ggplot_33<-
ggplot(porcentajes, aes(x =Tiene.trabajo, y = porcentaje, 
                        fill = Personas.con.las.que.vive)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +  # fill para porcentajes
  geom_text(aes(label = etiqueta),
                       
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  labs( x = "¿Trabaja?", 
       y = "Porcentaje",
       fill = "¿Con quién vive?") +
  theme_minimal() +
  theme_gray() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.3, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

####################Relacion entre variables de aspecto personal y aspecto academico##############################


##Ingreso x Condición.alcanzada.I

porcentajes <- encuesta %>%
  count(Ingreso, Condicion.alcanzada.I) %>%
  group_by(Ingreso) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()

colores_personalizados <- c(
  "Me anoté, pero no empecé a cursar." = "#F564E3",  
  "Libre (rendí las evaluaciones pero me fue mal)" = "#F8766D",  
  "Abandoné" = "#00BFC4",  
  "Promocioné" = "#00BA38",  
  "Regular" = "#619CFF" 
)

ggplot_34<-
ggplot(porcentajes, aes(x = as.factor(Ingreso),y=porcentaje, fill = Condicion.alcanzada.I)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_fill_manual(values=colores_personalizados,
                    labels = c("Me anoté, pero no empecé a cursar."= "No cursé",
                               "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
                               "Abandoné" = "Abandoné",
                               "Promocioné"="Promocioné ",
                               "Regular"="Regular"),
                    name="Condición\nalcanzada")+  #cambio el nombre que quiero en el fillname = "Condición" 
  labs(x = "Año de Ingreso", 
       y = "Porcentaje")+
  theme_minimal() +#elimina los elementos no escenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Ingreso x Condicion.alcanzada.II

porcentajes <- encuesta %>%
  count(Ingreso, Condicion.alcanzada.II) %>%
  group_by(Ingreso) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()

ggplot_35<-
ggplot(porcentajes, aes(x = as.factor(Ingreso),y=porcentaje, fill = Condicion.alcanzada.II)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_fill_manual(values=colores_personalizados,
                    labels = c("Me anoté, pero no empecé a cursar."= "No cursé",
                               "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
                               "Abandoné" = "Abandoné",
                               "Promocioné"="Promocioné",
                               "Regular"="Regular"),
                    name = "Condición\nalcanzada")+ 
  labs(      
       x = "Año de Ingreso", 
       y = "Porcentaje")+
  theme_minimal() +#elimina los elementos no escenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))


#Ingreso x Condicion.alcanzada.III

library(tidyr)

porcentajes <- encuesta %>%
  drop_na(Condicion.alcanzada.III) %>% #cargo la libreria tidy. elimina los 2 NA(valores faltantes) que hay en la var. 
  count(Ingreso, Condicion.alcanzada.III) %>%
  group_by(Ingreso) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Condicion.alcanzada.III=factor(Condicion.alcanzada.III,levels= c("Me anoté, pero no empecé a cursar.","Abandoné","Libre (rendí las evaluaciones pero me fue mal)","Promocioné" ,"Regular" ))) #para que aparezca en eso orden en el grafico 

ggplot_36<-
ggplot(porcentajes, aes(x = as.factor(Ingreso),y=porcentaje, fill = Condicion.alcanzada.III)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  scale_fill_manual(
    values = colores_personalizados,
    labels = c("Me anoté, pero no empecé a cursar."= "No cursé",
               "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
                "Abandoné" = "Abandoné",
               "Promocioné"="Promocioné",
               "Regular"="Regular"),
    name="Condición\nalcanzada")+  #cambio el nombre que quiero en el fill
   geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  
  labs(     
       x = "Año de Ingreso", 
       y = "Porcentaje")+
  theme_minimal() +#elimina los elementos no escenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

##Tiene.trabajo x Condición.alcanzada.I

porcentajes <- encuesta %>%
  count(Tiene.trabajo, Condicion.alcanzada.I) %>%
  group_by(Tiene.trabajo) %>% #para que me haga el porcentaje dentro de cada categ de Tiene.trabajo
  mutate(porcentaje = n / sum(n),
         etiqueta = ifelse(porcentaje >= 0.05,
                           scales::percent(porcentaje, accuracy = 1), 
                           "")
  ) %>%
  ungroup()%>%
  mutate(Tiene.trabajo=factor(Tiene.trabajo,levels=c("SÍ","NO"))) #para que aparezca primero SI

ggplot_37<-
ggplot(porcentajes, aes(x = as.factor(Tiene.trabajo), y = porcentaje, 
                        fill = Condicion.alcanzada.I)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +  # fill para porcentajes
  scale_fill_manual(
    values = colores_personalizados,
    labels = c("Me anoté, pero no empecé a cursar."= "No cursé",
               "Abandoné" = "Abandoné",
               "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
               "Promocioné"="Promocioné",
               "Regular"="Regular"),
    name="Condición\nalcanzada")+ 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "¿Trabaja?", 
       y = "Porcentaje",
       fill = "Condición\nalcanzada") +
  theme_minimal() +
  theme_gray() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

##Tiene.trabajo x Condición.alcanzada.II

porcentajes <- encuesta %>%
  count(Tiene.trabajo, Condicion.alcanzada.II) %>%
  group_by(Tiene.trabajo) %>% #para que me haga el porcentaje dentro de cada categ de Tiene.trabajo
  mutate(porcentaje = n / sum(n),
         etiqueta = ifelse(porcentaje >= 0.05,
                           scales::percent(porcentaje, accuracy = 1), 
                           "")
  ) %>%
  ungroup()%>%
  mutate(Tiene.trabajo=factor(Tiene.trabajo,levels=c("SÍ","NO"))) #para que aparezca primero SI

ggplot_38<-
ggplot(porcentajes, aes(x = as.factor(Tiene.trabajo), y = porcentaje, 
                        fill = Condicion.alcanzada.II)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +  # fill para porcentajes
  scale_fill_manual(
    values = colores_personalizados,
    labels = c("Me anoté, pero no empecé a cursar."= "No cursé",
               "Abandoné" = "Abandoné",
               "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
               "Promocioné"="Promocioné",
               "Regular"="Regular"),
    name="Condición\nalcanzada")+ 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "¿Trabaja?", 
       y = "Porcentaje",
       fill = "Condición\nalcanzada") +
  theme_minimal() +
  theme_gray() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

##Tiene.trabajo x Condición.alcanzada.III

porcentajes <- encuesta %>%
  drop_na(Condicion.alcanzada.III) %>% #elimina los NA de la variable
  count(Tiene.trabajo, Condicion.alcanzada.III) %>%
  group_by(Tiene.trabajo) %>% #para que me haga el porcentaje dentro de cada categ de Tiene.trabajo
  mutate(porcentaje = n / sum(n),
         etiqueta = ifelse(porcentaje >= 0.05,
                           scales::percent(porcentaje, accuracy = 1), 
                           "")
  ) %>%
  ungroup()%>%
  mutate(Tiene.trabajo=factor(Tiene.trabajo,levels=c("SÍ","NO"))) %>%#para que aparezca primero SI
  mutate(Condicion.alcanzada.III=factor(Condicion.alcanzada.III,levels= c("Me anoté, pero no empecé a cursar.","Abandoné","Libre (rendí las evaluaciones pero me fue mal)","Promocioné" ,"Regular" ))) #para que aparezca en eso orden en el grafico 

ggplot_39<-
ggplot(porcentajes, aes(x = as.factor(Tiene.trabajo), y = porcentaje, 
                        fill = Condicion.alcanzada.III)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +  # fill para porcentajes
  scale_fill_manual(
    values = colores_personalizados,
    labels = c("Me anoté, pero no empecé a cursar."= "No cursé",
               "Abandoné" = "Abandoné",
               "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
               "Promocioné"="Promocioné",
               "Regular"="Regular"),
    name="Condición\nalcanzada")+ 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "¿Trabaja?", 
       y = "Porcentaje",
       fill = "Condición\nalcanzada") +
  theme_minimal() +
  theme_gray() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

# Tiempo.traslado x Asistencia.consulta.I 

porcentajes <- encuesta %>%
  drop_na(Tiempo.traslado,Asistencia.consulta.I)%>% #elimina los NA de ambas var
  count(Tiempo.traslado,Asistencia.consulta.I) %>%
  group_by(Tiempo.traslado) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Tiempo.traslado=factor(Tiempo.traslado,levels=c("Menos de 1 h.","Entre 1 y 2 hs.","Entre 2 y 3 hs.","Más de 3 hs.")),
         Asistencia.consulta.I=factor(Asistencia.consulta.I,levels = c("NUNCA","A VECES","SÍ"))) #quiero ese orden

ggplot_40<-
ggplot(porcentajes, aes(x =as.factor(Tiempo.traslado),y=porcentaje, fill =Asistencia.consulta.I )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs(x = "Tiempo", 
       y = "Porcentaje",
       fill="Asistencia\n consulta")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

# Tiempo.traslado x Asistencia.consulta.II 

porcentajes <- encuesta %>%
  drop_na(Tiempo.traslado,Asistencia.consulta.II)%>% #elimina los NA de ambas var
  count(Tiempo.traslado,Asistencia.consulta.II) %>%
  group_by(Tiempo.traslado) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Tiempo.traslado=factor(Tiempo.traslado,levels=c("Menos de 1 h.","Entre 1 y 2 hs.","Entre 2 y 3 hs.","Más de 3 hs.")),
         Asistencia.consulta.II=factor(Asistencia.consulta.II,levels = c("NUNCA","A VECES","SÍ"))) #quiero ese orden

ggplot_41<-
ggplot(porcentajes, aes(x =as.factor(Tiempo.traslado),y=porcentaje, fill =Asistencia.consulta.II )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs( x = "Tiempo", 
       y = "Porcentaje",
       fill="Asistencia\n consulta")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

# Tiempo.traslado x Asistencia.consulta.III 

porcentajes <- encuesta %>%
  drop_na(Tiempo.traslado,Asistencia.consulta.III)%>% #elimina los NA de ambas var
  count(Tiempo.traslado,Asistencia.consulta.III) %>%
  group_by(Tiempo.traslado) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Tiempo.traslado=factor(Tiempo.traslado,levels=c("Menos de 1 h.","Entre 1 y 2 hs.","Entre 2 y 3 hs.","Más de 3 hs.")),
         Asistencia.consulta.III=factor(Asistencia.consulta.III,levels = c("NUNCA","A VECES","SÍ"))) #quiero ese orden

ggplot_42<-
ggplot(porcentajes, aes(x =as.factor(Tiempo.traslado),y=porcentaje, fill =Asistencia.consulta.III )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs(x = "Tiempo", 
       y = "Porcentaje",
       fill="Asistencia\n consulta")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))









#####################Relacion entre variables de aspecto academico##########################

#Condicion.alcanzada.IxCalidad.explicativa.docente.I

porcentajes <- encuesta %>%
  drop_na(Calidad.explicativa.docente.I,Condicion.alcanzada.I)%>% #elimina los NA de ambas var
  count( Condicion.alcanzada.I,Calidad.explicativa.docente.I) %>%
  group_by(Condicion.alcanzada.I) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
mutate(Calidad.explicativa.docente.I=factor(Calidad.explicativa.docente.I,levels= c("No me resultaron claras","Claras","Muy claras")))%>%
mutate(Condicion.alcanzada.I=factor(Condicion.alcanzada.I,levels=c("Libre (rendí las evaluaciones pero me fue mal)","Regular","Promocioné")))

ggplot_43<-
ggplot(porcentajes, aes(x =as.factor(Condicion.alcanzada.I ),y=porcentaje, fill =Calidad.explicativa.docente.I )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
    scale_x_discrete(
      labels = c(
        "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
        "Promocioné" = "Promocioné",
        "Regular" = "Regular")) +
      scale_fill_discrete(
    labels = c("Claras" = "Claras",
               "Muy claras" = "Muy claras",
               "No me resultaron claras" = "No claras"))+  #cambio el nombre que quiero en el fill
  labs( x = "Condición alcanzada", 
       y = "Porcentaje",
       fill="Explicaciones")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Condicion.alcanzada.II x Calidad.explicativa.docente.II

porcentajes <- encuesta %>%
  drop_na(Calidad.explicativa.docente.II,Condicion.alcanzada.II)%>% #elimina los NA de ambas var
  count( Condicion.alcanzada.II,Calidad.explicativa.docente.II) %>%
  group_by(Condicion.alcanzada.II) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
mutate(Calidad.explicativa.docente.II=factor(Calidad.explicativa.docente.II,levels= c("No me resultaron claras","Claras","Muy claras"))) %>%
mutate(Condicion.alcanzada.II=factor(Condicion.alcanzada.II,levels=c("Libre (rendí las evaluaciones pero me fue mal)","Regular","Promocioné")))

ggplot_44<-
ggplot(porcentajes, aes(x =as.factor(Condicion.alcanzada.II ),y=porcentaje, fill =Calidad.explicativa.docente.II )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c(
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
  scale_fill_discrete(
    labels = c("Claras" = "Claras",
               "Muy claras" = "Muy claras",
               "No me resultaron claras" = "No claras"))+  #cambio el nombre que quiero en el fill
  labs( x = "Condición alcanzada", 
       y = "Porcentaje",
       fill="Explicaciones")+
  theme_minimal() +#elimina los elementos no escenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Condicion.alcanzada.III x Calidad.explicativa.docente.III

porcentajes <- encuesta %>%
  drop_na(Calidad.explicativa.docente.III,Condicion.alcanzada.III)%>% #elimina los NA de ambas var
  count( Condicion.alcanzada.III,Calidad.explicativa.docente.III) %>%
  group_by(Condicion.alcanzada.III) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
mutate(Calidad.explicativa.docente.III=factor(Calidad.explicativa.docente.III,levels= c("No me resultaron claras","Claras","Muy claras")))%>%
  mutate(Condicion.alcanzada.III=factor(Condicion.alcanzada.III,levels=c("Libre (rendí las evaluaciones pero me fue mal)","Regular","Promocioné")))


ggplot_45<-
  ggplot(porcentajes, aes(x =as.factor(Condicion.alcanzada.III ),y=porcentaje, fill =Calidad.explicativa.docente.III )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c(
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
  scale_fill_discrete(
    labels = c("Claras" = "Claras",
               "Muy claras" = "Muy claras",
               "No me resultaron claras" = "No claras"))+  #cambio el nombre que quiero en el fill
  labs( x = "Condición alcanzada", 
       y = "Porcentaje",
       fill="Explicaciones")+
  theme_minimal() +#elimina los elementos no escenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#ContenidosI x Asistencia.consulta.I

porcentajes <- encuesta %>%
  drop_na(Contenidos.I,Asistencia.consulta.I)%>% #elimina los NA de ambas var
  count(Contenidos.I,Asistencia.consulta.I) %>%
  group_by(Contenidos.I) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Contenidos.I=factor(Contenidos.I,levels=c("Excesivo","Adecuado","Insuficiente")),
         Asistencia.consulta.I=factor(Asistencia.consulta.I,levels = c("NUNCA","A VECES","SÍ")))
ggplot_46<-         
ggplot(porcentajes, aes(x =as.factor(Contenidos.I ),y=porcentaje, fill =Asistencia.consulta.I )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs(      
       x = "Contenido", 
       y = "Porcentaje",
       fill="Asistencia\n consulta")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#ContenidosII x Asistencia.consulta.II

porcentajes <- encuesta %>%
  drop_na(Contenidos.II,Asistencia.consulta.II)%>% #elimina los NA de ambas var
  count(Contenidos.II,Asistencia.consulta.II) %>%
  group_by(Contenidos.II) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Contenidos.II=factor(Contenidos.II,levels=c("Excesivo","Adecuado","Insuficiente")),
         Asistencia.consulta.II=factor(Asistencia.consulta.II,levels = c("NUNCA","A VECES","SÍ")))

ggplot_47<-
ggplot(porcentajes, aes(x =Contenidos.II,y=porcentaje, fill =Asistencia.consulta.II )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs(      
       x = "Contenido", 
       y = "Porcentaje",
       fill="Asistencia\n consulta")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#ContenidosIII x Asistencia.consulta.III

porcentajes <- encuesta %>%
  drop_na(Contenidos.III,Asistencia.consulta.III)%>% #elimina los NA de ambas var
  count(Contenidos.III,Asistencia.consulta.III) %>%
  group_by(Contenidos.III) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Contenidos.III=factor(Contenidos.III,levels=c("Excesivo","Adecuado","Insuficiente")),
         Asistencia.consulta.III=factor(Asistencia.consulta.III,levels = c("NUNCA","A VECES","SÍ")))

ggplot_48<-
ggplot(porcentajes, aes(x =Contenidos.III,y=porcentaje, fill =Asistencia.consulta.III )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs(      
       x = "Contenido", 
       y = "Porcentaje",
       fill="Asistencia\n consulta")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Asistencia.consulta.I x Condicion.alcanzada.I

porcentajes <- encuesta %>%
  drop_na(Asistencia.consulta.I,Condicion.alcanzada.I)%>% #elimina los NA de ambas var
  count(Asistencia.consulta.I,Condicion.alcanzada.I) %>%
  group_by(Asistencia.consulta.I) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Asistencia.consulta.I=factor(Asistencia.consulta.I,levels = c("SÍ","A VECES","NUNCA")))

ggplot_49<-
  ggplot(porcentajes, aes(x =as.factor(Asistencia.consulta.I),y=porcentaje, fill =Condicion.alcanzada.I )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_fill_discrete(labels = c(
    "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
    "Promocioné" = "Promocioné",
    "Regular" = "Regular"))+
   labs(      
       x = "Asistencia a consulta", 
       y = "Porcentaje",
       fill="Condición\nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Asistencia.consulta.II x Condicion.alcanzada.II

porcentajes <- encuesta %>%
  drop_na(Asistencia.consulta.II,Condicion.alcanzada.II)%>% #elimina los NA de ambas var
  count(Asistencia.consulta.II,Condicion.alcanzada.II) %>%
  group_by(Asistencia.consulta.II) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Asistencia.consulta.II=factor(Asistencia.consulta.II,levels = c("SÍ","A VECES","NUNCA")))

ggplot_50<-
ggplot(porcentajes, aes(x =as.factor(Asistencia.consulta.II),y=porcentaje, fill =Condicion.alcanzada.II )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_fill_discrete(labels = c(
    "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
    "Promocioné" = "Promocioné",
    "Regular" = "Regular"))+
  labs(      
       x = "Asistencia a consulta", 
       y = "Porcentaje",
       fill="Condición\nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Asistencia.consulta.III x Condicion.alcanzada.III

porcentajes <- encuesta %>%
  drop_na(Asistencia.consulta.III,Condicion.alcanzada.III)%>% #elimina los NA de ambas var
  count(Asistencia.consulta.III,Condicion.alcanzada.III) %>%
  group_by(Asistencia.consulta.III) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Asistencia.consulta.III=factor(Asistencia.consulta.III,levels = c("SÍ","A VECES","NUNCA")))

ggplot_51<-
ggplot(porcentajes, aes(x =as.factor(Asistencia.consulta.III),y=porcentaje, fill =Condicion.alcanzada.III )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_fill_discrete(labels = c(
    "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
    "Promocioné" = "Promocioné",
    "Regular" = "Regular"))+
  labs(     
       x = "Asistencia a consulta", 
       y = "Porcentaje",
       fill="Condición\nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Condicion.cursado.I x Condicion.alcanzada.I

porcentajes <- encuesta %>%
  drop_na(Condicion.cursado.I,Condicion.alcanzada.I)%>% #elimina los NA de ambas var
  count(Condicion.cursado.I,Condicion.alcanzada.I) %>%
  group_by(Condicion.cursado.I) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Condicion.cursado.I=factor(Condicion.cursado.I,levels = c("Ingresante","Cambio de plan","Recursante")))

ggplot_52<-
ggplot(porcentajes, aes(x =as.factor(Condicion.cursado.I),y=porcentaje, fill =Condicion.alcanzada.I )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels=c("Ingresante"="Ingresante",
             "Cambio de plan"="cambio \n de plan",
             "Recursante"="Recursante"))+
  scale_fill_manual(
    values = colores_personalizados,
    labels = c("Abandoné" = "Abandoné",
               "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
               "Promocioné"="Promocioné",
               "Regular"="Regular"),
    name="Condición\nalcanzada")+ 
   labs(      
       x = "Condición cursado", 
       y = "Porcentaje",
       fill="Condición\nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"))

#Condicion.cursado.II x Condicion.alcanzada.II

porcentajes <- encuesta %>%
  drop_na(Condicion.cursado.II,Condicion.alcanzada.II)%>% #elimina los NA de ambas var
  count(Condicion.cursado.II,Condicion.alcanzada.II) %>%
  group_by(Condicion.cursado.II) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Condicion.cursado.II=factor(Condicion.cursado.II,levels = c("Ingresante","Cambio de plan","Recursante")))

ggplot_53<-
ggplot(porcentajes, aes(x =as.factor(Condicion.cursado.II),y=porcentaje, fill =Condicion.alcanzada.II )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels=c("Ingresante"="Ingresante",
             "Cambio de plan"="cambio \n de plan",
             "Recursante"="Recursante"))+
  scale_fill_manual(
    values = colores_personalizados,
    labels = c("Abandoné" = "Abandoné",
               "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
               "Promocioné"="Promocioné",
               "Regular"="Regular"),
    name="Condición\nalcanzada")+ 
  labs(      
       x = "Condición cursado", 
       y = "Porcentaje",
       fill="Condición\nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"))

#Condicion.cursado.III x Condicion.alcanzada.III

porcentajes <- encuesta %>%
  drop_na(Condicion.cursado.III,Condicion.alcanzada.III)%>% #elimina los NA de ambas var
  count(Condicion.cursado.III,Condicion.alcanzada.III) %>%
  group_by(Condicion.cursado.III) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Condicion.cursado.III=factor(Condicion.cursado.III,levels = c("Ingresante","Cambio de plan","Recursante")))%>%
  mutate(Condicion.alcanzada.III=factor(Condicion.alcanzada.III,levels= c("Me anoté, pero no empecé a cursar.","Abandoné","Libre (rendí las evaluaciones pero me fue mal)","Promocioné" ,"Regular" ))) #para que aparezca en eso orden en el grafico 

ggplot_54<-
ggplot(porcentajes, aes(x =as.factor(Condicion.cursado.III),y=porcentaje, fill =Condicion.alcanzada.III )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels=c("Ingresante"="Ingresante",
             "Cambio de plan"="cambio \n de plan",
             "Recursante"="Recursante"))+
  scale_fill_manual(
    values = colores_personalizados,
    labels = c("Me anoté, pero no empecé a cursar."= "No cursé",
               "Abandoné" = "Abandoné",
               "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
               "Promocioné"="Promocioné",
               "Regular"="Regular"),
    name="Condición\nalcanzada")+ 
  labs(      
       x = "Condición cursado", 
       y = "Porcentaje",
       fill="Condición\nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"))

# Recursos.utilizados.I x Condicion.alcanzada.I

porcentajes <- encuesta %>%
  drop_na(
    Recursos.utilizados.I,Condicion.alcanzada.I)%>% #elimina los NA de ambas var
  count(Recursos.utilizados.I,Condicion.alcanzada.I) %>%
  group_by(Recursos.utilizados.I) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Recursos.utilizados.I=factor(Recursos.utilizados.I,levels = c("Poco adecuados","Adecuados","Muy adecuados")))

ggplot_55<-
ggplot(porcentajes, aes(x =as.factor(Recursos.utilizados.I),y=porcentaje, fill =Condicion.alcanzada.I )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c(
      "Muy adecuados"="Muy\n adecuados",
      "Adecuados"="Adecuados",
      "Poco adecuados"="Poco\n adecuados"))+
      scale_fill_discrete(
    labels = c(
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
  labs(      
       x = "Recursos utilizados", 
       y = "Porcentaje",
       fill="Condición\nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"))

# Recursos.utilizados.II x Condicion.alcanzada.II

porcentajes <- encuesta %>%
  drop_na(Recursos.utilizados.II,Condicion.alcanzada.II)%>% #elimina los NA de ambas var
  count(Recursos.utilizados.II,Condicion.alcanzada.II) %>%
  group_by(Recursos.utilizados.II) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Recursos.utilizados.II=factor(Recursos.utilizados.II,levels = c("Poco adecuados","Adecuados","Muy adecuados")))

ggplot_56<-
ggplot(porcentajes, aes(x =as.factor(Recursos.utilizados.II),y=porcentaje, fill =Condicion.alcanzada.II )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c(
      "Muy adecuados"="Muy\n adecuados",
      "Adecuados"="Adecuados",
      "Poco adecuados"="Poco\n adecuados"))+
  scale_fill_discrete(
    labels = c(
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
  labs(     
       x = "Recursos utilizados", 
       y = "Porcentaje",
       fill="Condición\nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"))

# Recursos.utilizados.III x Condicion.alcanzada.III

porcentajes <- encuesta %>%
  drop_na(Recursos.utilizados.III,Condicion.alcanzada.III)%>% #elimina los NA de ambas var
  count(Recursos.utilizados.III,Condicion.alcanzada.III) %>%
  group_by(Recursos.utilizados.III) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Recursos.utilizados.III=factor(Recursos.utilizados.III,levels = c("Poco adecuados","Adecuados","Muy adecuados")))

ggplot_57<-
ggplot(porcentajes, aes(x =as.factor(Recursos.utilizados.III),y=porcentaje, fill =Condicion.alcanzada.III )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c(
      "Muy adecuados"="Muy\n adecuados",
      "Adecuados"="Adecuados",
      "Poco adecuados"="Poco\n adecuados"))+
  scale_fill_discrete(
    labels = c(
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
  labs(      
       x = "Recursos utilizados", 
       y = "Porcentaje",
       fill="Condición\nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"))

#Condicion.alcanzada.I x Contenidos.clase/parcial.I

porcentajes <- encuesta %>%
  drop_na(`Contenidos.clase/parcial.I`,Condicion.alcanzada.I)%>% #elimina los NA de ambas var
  count( Condicion.alcanzada.I,`Contenidos.clase/parcial.I`) %>%
  group_by(Condicion.alcanzada.I) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Condicion.alcanzada.I=factor(Condicion.alcanzada.I,levels= c("Promocioné","Regular","Libre (rendí las evaluaciones pero me fue mal)")), #para que aparezca en eso orden en el grafico 
         `Contenidos.clase/parcial.I`=factor(`Contenidos.clase/parcial.I`,levels = c("Algunos temas no los vimos en las clases.","Algunos temas los vimos poco en las clases.","Si, los vimos varias veces")))

ggplot_58<-         
ggplot(porcentajes, aes(x =as.factor(Condicion.alcanzada.I ),y=porcentaje, fill =`Contenidos.clase/parcial.I` )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c(
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
  scale_fill_discrete(
    labels = c( 
      "Algunos temas no los vimos en las clases."="Algunos no",
      "Algunos temas los vimos poco en las clases."="Si/Poco",
      "Si, los vimos varias veces"="Si"))+
      labs(title = "Matemática I",      
       x = "Condición", 
       y = "Porcentaje",
       fill="Contenidos")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Condicion.alcanzada.II x Contenidos.clase/parcial.II

porcentajes <- encuesta %>%
  drop_na(`Contenidos.clase/parcial.II`,Condicion.alcanzada.II)%>% #elimina los NA de ambas var
  count( Condicion.alcanzada.II,`Contenidos.clase/parcial.II`) %>%
  group_by(Condicion.alcanzada.II) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Condicion.alcanzada.II=factor(Condicion.alcanzada.II,levels= c("Promocioné","Regular","Libre (rendí las evaluaciones pero me fue mal)")), #para que aparezca en eso orden en el grafico 
         `Contenidos.clase/parcial.II`=factor(`Contenidos.clase/parcial.II`,levels = c("Algunos temas no los vimos en las clases.","Algunos temas los vimos poco en las clases.","Si, los vimos varias veces")))

ggplot_59<-
ggplot(porcentajes, aes(x =as.factor(Condicion.alcanzada.II),y=porcentaje, fill =`Contenidos.clase/parcial.II` )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c(
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
  scale_fill_discrete(
    labels = c( 
      "Algunos temas no los vimos en las clases."="Algunos no",
      "Algunos temas los vimos poco en las clases."="Si/Poco",
      "Si, los vimos varias veces"="Si"))+
  labs(title = "Biología",      
       x = "Condición", 
       y = "Porcentaje",
       fill="Contenidos")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Condicion.alcanzada.III x Contenidos.clase/parcial.III

porcentajes <- encuesta %>%
  drop_na(`Contenidos.clase/parcial.III`,Condicion.alcanzada.III)%>% #elimina los NA de ambas var
  count(Condicion.alcanzada.III,`Contenidos.clase/parcial.III`) %>%
  group_by(Condicion.alcanzada.III) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Condicion.alcanzada.III=factor(Condicion.alcanzada.III,levels= c("Promocioné","Regular","Libre (rendí las evaluaciones pero me fue mal)")), #para que aparezca en eso orden en el grafico 
         `Contenidos.clase/parcial.III`=factor(`Contenidos.clase/parcial.III`,levels = c("Algunos temas no los vimos en las clases.","Algunos temas los vimos poco en las clases.","Si, los vimos varias veces")))

ggplot_60<-
ggplot(porcentajes, aes(x =as.factor(Condicion.alcanzada.III),y=porcentaje, fill =`Contenidos.clase/parcial.III` )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c(
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
  scale_fill_discrete(
    labels = c( 
      "Algunos temas no los vimos en las clases."="Algunos no",
      "Algunos temas los vimos poco en las clases."="Si/Poco",
      "Si, los vimos varias veces"="Si"))+
  labs(title = "Química General e Inorgánica",      
       x = "Condición", 
       y = "Porcentaje",
       fill="Contenidos")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Contenidos.I x Condicion.alcanzada.I

porcentajes <- encuesta %>%
  drop_na(Contenidos.I,Condicion.alcanzada.I)%>% #elimina los NA de ambas var
  count(Contenidos.I,Condicion.alcanzada.I) %>%
  group_by(Contenidos.I) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Contenidos.I=factor(Contenidos.I,levels=c("Excesivo","Adecuado","Insuficiente")))%>%
  mutate(Condicion.alcanzada.I=factor(Condicion.alcanzada.I,levels=c("Libre (rendí las evaluaciones pero me fue mal)","Promocioné","Regular")))
         

ggplot_61<-
ggplot(porcentajes, aes(x =as.factor(Contenidos.I ),y=porcentaje, fill =Condicion.alcanzada.I )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  scale_fill_discrete(
    labels = c( 
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
       geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs(title = "Matemática I",      
       x = "Contenido", 
       y = "Porcentaje",
       fill="Condición \nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Contenidos.II x Condicion.alcanzada.II

porcentajes <- encuesta %>%
  drop_na(Contenidos.II,Condicion.alcanzada.II)%>% #elimina los NA de ambas var
  count(Contenidos.II,Condicion.alcanzada.II) %>%
  group_by(Contenidos.II) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Contenidos.II=factor(Contenidos.II,levels=c("Excesivo","Adecuado","Insuficiente")))%>%
  mutate(Condicion.alcanzada.II=factor(Condicion.alcanzada.II,levels=c("Libre (rendí las evaluaciones pero me fue mal)","Promocioné","Regular")))


ggplot_62<-
ggplot(porcentajes, aes(x =as.factor(Contenidos.II ),y=porcentaje, fill =Condicion.alcanzada.II )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  scale_fill_discrete(
    labels = c( 
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs(title = "Biología",      
       x = "Contenido", 
       y = "Porcentaje",
       fill="Condición \nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))


#Contenidos.III x Condicion.alcanzada.III

porcentajes <- encuesta %>%
  drop_na(Contenidos.III,Condicion.alcanzada.III)%>% #elimina los NA de ambas var
  count(Contenidos.III,Condicion.alcanzada.III) %>%
  group_by(Contenidos.III) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Contenidos.III=factor(Contenidos.III,levels=c("Excesivo","Adecuado","Insuficiente")))%>%
  mutate(Condicion.alcanzada.III=factor(Condicion.alcanzada.III,levels=c("Libre (rendí las evaluaciones pero me fue mal)","Promocioné","Regular")))


ggplot_63<-
ggplot(porcentajes, aes(x =as.factor(Contenidos.III ),y=porcentaje, fill =Condicion.alcanzada.III)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  scale_fill_discrete(
    labels = c( 
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs(title = "Química General e Inorgánica",      
       x = "Contenido", 
       y = "Porcentaje",
       fill="Condición \nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Calidad.explicativa.docente.I x Asistencia.consulta.I

porcentajes <- encuesta %>%
  drop_na(Calidad.explicativa.docente.I,Asistencia.consulta.I)%>% #elimina los NA de ambas var
  count(Calidad.explicativa.docente.I,Asistencia.consulta.I) %>%
  group_by(Calidad.explicativa.docente.I) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Calidad.explicativa.docente.I=factor(Calidad.explicativa.docente.I,levels= c("Muy claras","Claras","No me resultaron claras")),
         Asistencia.consulta.I=factor(Asistencia.consulta.I,levels = c("NUNCA","A VECES","SÍ")))

ggplot_64<-
ggplot(porcentajes, aes(x =as.factor(Calidad.explicativa.docente.I ),y=porcentaje, fill =Asistencia.consulta.I )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c("Muy claras" = "Muy claras",
               "Claras" = "claras", 
               "No me resultaron claras" = "No claras"))+ 
   labs(title = "Matemática I",      
       x = "Explicaciones", 
       y = "Porcentaje",
       fill="Asistencia\n consulta")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Calidad.explicativa.docente.II x Asistencia.consulta.II

porcentajes <- encuesta %>%
  drop_na(Calidad.explicativa.docente.II,Asistencia.consulta.II)%>% #elimina los NA de ambas var
  count(Calidad.explicativa.docente.II,Asistencia.consulta.II) %>%
  group_by(Calidad.explicativa.docente.II) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Calidad.explicativa.docente.II=factor(Calidad.explicativa.docente.II,levels= c("Muy claras","Claras","No me resultaron claras")),
         Asistencia.consulta.II=factor(Asistencia.consulta.II,levels = c("NUNCA","A VECES","SÍ")))

ggplot_65<-
ggplot(porcentajes, aes(x =as.factor(Calidad.explicativa.docente.II ),y=porcentaje, fill =Asistencia.consulta.II )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c("Muy claras" = "Muy claras",
               "Claras" = "claras", 
               "No me resultaron claras" = "No claras"))+ 
  labs(title = "Biología",      
       x = "Explicaciones", 
       y = "Porcentaje",
       fill="Asistencia\n consulta")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Calidad.explicativa.docente.IIIx Asistencia.consulta.III

porcentajes <- encuesta %>%
  drop_na(Calidad.explicativa.docente.III,Asistencia.consulta.III)%>% #elimina los NA de ambas var
  count(Calidad.explicativa.docente.III,Asistencia.consulta.III) %>%
  group_by(Calidad.explicativa.docente.III) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Calidad.explicativa.docente.III=factor(Calidad.explicativa.docente.III,levels= c("Muy claras","Claras","No me resultaron claras")),
         Asistencia.consulta.III=factor(Asistencia.consulta.III,levels = c("NUNCA","A VECES","SÍ")))

ggplot_66<-
ggplot(porcentajes, aes(x =as.factor(Calidad.explicativa.docente.III ),y=porcentaje, fill =Asistencia.consulta.III )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c("Muy claras" = "Muy claras",
               "Claras" = "claras", 
               "No me resultaron claras" = "No claras"))+ 
  labs(title = "Química General e Inorgánica",      
       x = "Explicaciones", 
       y = "Porcentaje",
       fill="Asistencia\n consulta")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Asistencia.particular.I x Calidad.explicativa.docente.I

porcentajes <- encuesta %>%
  drop_na(Calidad.explicativa.docente.I,Asistencia.particular.I)%>% #elimina los NA de ambas var
  count( Asistencia.particular.I,Calidad.explicativa.docente.I) %>%
  group_by(Asistencia.particular.I) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Calidad.explicativa.docente.I=factor(Calidad.explicativa.docente.I,levels= c("No me resultaron claras","Claras","Muy claras")),
         Asistencia.particular.I=factor(Asistencia.particular.I,levels = c("SÍ","NO")))

ggplot_67<-
ggplot(porcentajes, aes(x =as.factor( Asistencia.particular.I),y=porcentaje, fill =Calidad.explicativa.docente.I )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_fill_discrete(
    labels = c("Claras" = "Claras",
               "Muy claras" = "Muy claras",
               "No me resultaron claras" = "No claras"))+  #cambio el nombre que quiero en el fill
  labs(title = "Matemática I",      
       x = "¿Asiste a clases particulares?", 
       y = "Porcentaje",
       fill="Explicaciones")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Asistencia.particular.II x Calidad.explicativa.docente.II

porcentajes <- encuesta %>%
  drop_na(Calidad.explicativa.docente.II,Asistencia.particular.II)%>% #elimina los NA de ambas var
  count( Asistencia.particular.II,Calidad.explicativa.docente.II) %>%
  group_by(Asistencia.particular.II) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Calidad.explicativa.docente.II=factor(Calidad.explicativa.docente.II,levels= c("No me resultaron claras","Claras","Muy claras")),
         Asistencia.particular.II=factor(Asistencia.particular.II,levels = c("SÍ","NO")))

ggplot_68<-
ggplot(porcentajes, aes(x =as.factor( Asistencia.particular.II),y=porcentaje, fill =Calidad.explicativa.docente.II )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_fill_discrete(
    labels = c("Claras" = "Claras",
               "Muy claras" = "Muy claras",
               "No me resultaron claras" = "No claras"))+  #cambio el nombre que quiero en el fill
  labs(title = "Biología",      
       x = "¿Asiste a clases particulares?", 
       y = "Porcentaje",
       fill="Explicaciones")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Asistencia.particular.III x Calidad.explicativa.docente.III

porcentajes <- encuesta %>%
  drop_na(Calidad.explicativa.docente.III,Asistencia.particular.III)%>% #elimina los NA de ambas var
  count( Asistencia.particular.III,Calidad.explicativa.docente.III) %>%
  group_by(Asistencia.particular.III) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Calidad.explicativa.docente.III=factor(Calidad.explicativa.docente.III,levels= c("No me resultaron claras","Claras","Muy claras")),
         Asistencia.particular.III=factor(Asistencia.particular.III,levels = c("SÍ","NO")))

ggplot_69<-
ggplot(porcentajes, aes(x =as.factor( Asistencia.particular.III),y=porcentaje, fill =Calidad.explicativa.docente.III )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_fill_discrete(
    labels = c("Claras" = "Claras",
               "Muy claras" = "Muy claras",
               "No me resultaron claras" = "No claras"))+  #cambio el nombre que quiero en el fill
  labs(title = "Química General e Inorgánica",      
       x = "¿Asiste a clases particulares?", 
       y = "Porcentaje",
       fill="Explicaciones")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Rendimiento.consulta.I x Condicion.alcanzada.I

porcentajes <- encuesta %>%
  drop_na(Rendimiento.consulta.I,Condicion.alcanzada.I)%>% #elimina los NA de ambas var
  count(Rendimiento.consulta.I,Condicion.alcanzada.I) %>%
  group_by(Rendimiento.consulta.I) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Rendimiento.consulta.I=factor(Rendimiento.consulta.I,levels=c("Muy útiles","Útiles","Poco útiles")))

ggplot_70<-
ggplot(porcentajes, aes(x =as.factor(Rendimiento.consulta.I ),y=porcentaje, fill =Condicion.alcanzada.I )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  scale_fill_discrete(
    labels = c( 
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs(title = "Matemática I",      
       x = "Clases consulta", 
       y = "Porcentaje",
       fill="Condición \nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Rendimiento.consulta.II x Condicion.alcanzada.II

porcentajes <- encuesta %>%
  drop_na(Rendimiento.consulta.II,Condicion.alcanzada.II)%>% #elimina los NA de ambas var
  count(Rendimiento.consulta.II,Condicion.alcanzada.II) %>%
  group_by(Rendimiento.consulta.II) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Rendimiento.consulta.II=factor(Rendimiento.consulta.II,levels=c("Muy útiles","Útiles","Poco útiles")))

ggplot_71<-
ggplot(porcentajes, aes(x =as.factor(Rendimiento.consulta.II ),y=porcentaje, fill =Condicion.alcanzada.II )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  scale_fill_discrete(
    labels = c( 
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs(title = "Biología",      
       x = "Clases consulta", 
       y = "Porcentaje",
       fill="Condición \nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Rendimiento.consulta.III x Condicion.alcanzada.III

porcentajes <- encuesta %>%
  drop_na(Rendimiento.consulta.III,Condicion.alcanzada.III)%>% #elimina los NA de ambas var
  count(Rendimiento.consulta.III,Condicion.alcanzada.III) %>%
  group_by(Rendimiento.consulta.III) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Rendimiento.consulta.III=factor(Rendimiento.consulta.III,levels=c("Muy útiles","Útiles","Poco útiles")))

ggplot_72<-
ggplot(porcentajes, aes(x =as.factor(Rendimiento.consulta.III ),y=porcentaje, fill =Condicion.alcanzada.III )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  scale_fill_discrete(
    labels = c( 
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre",
      "Promocioné" = "Promocioné",
      "Regular" = "Regular")) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs(title = "Química General e Inorgánica",      
       x = "Clases consulta", 
       y = "Porcentaje",
       fill="Condición \nalcanzada")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

#Examen.final.I x Condicion.cursado.I

porcentajes <- encuesta %>%
  drop_na(Examen.final.I,Condicion.cursado.I)%>% #elimina los NA de ambas var
  count(Examen.final.I,Condicion.cursado.I) %>%
  group_by(Examen.final.I) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Examen.final.I=factor(Examen.final.I,levels = c("NO, porque Promocioné","SÍ","NO")),
         Condicion.cursado.I=factor(Condicion.cursado.I,levels = c("Recursante","Cambio de plan","Ingresante")))

ggplot_73<-
ggplot(porcentajes, aes(x =as.factor(Examen.final.I),y=porcentaje, fill =Condicion.cursado.I )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c(
      "NO, porque Promocioné" = "Promocioné",
      "SÍ" = "SÍ",
      "NO" = "NO")) +
  labs(title = "Matemática I",      
       x = "¿Rindió examen final?", 
       y = "Porcentaje",
       fill="Condición")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"))

#Examen.final.II x Condicion.cursado.II

porcentajes <- encuesta %>%
  drop_na(Examen.final.II,Condicion.cursado.II)%>% #elimina los NA de ambas var
  count(Examen.final.II,Condicion.cursado.II) %>%
  group_by(Examen.final.II) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Examen.final.II=factor(Examen.final.II,levels = c("NO, porque Promocioné","SÍ","NO")),
         Condicion.cursado.II=factor(Condicion.cursado.II,levels = c("Recursante","Cambio de plan","Ingresante")))
ggplot_74<-
ggplot(porcentajes, aes(x =as.factor(Examen.final.II),y=porcentaje, fill =Condicion.cursado.II )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c(
      "NO, porque Promocioné" = "Promocioné",
      "SÍ" = "SÍ",
      "NO" = "NO")) +
  labs(title = "Biología",      
       x = "¿Rindió examen final?", 
       y = "Porcentaje",
       fill="Condición")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"))

#Examen.final.III x Condicion.cursado.III

porcentajes <- encuesta %>%
  drop_na(Examen.final.III,Condicion.cursado.III)%>% #elimina los NA de ambas var
  count(Examen.final.III,Condicion.cursado.III) %>%
  group_by(Examen.final.III) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Examen.final.III=factor(Examen.final.III,levels = c("NO, porque Promocioné","SÍ","NO")),
         Condicion.cursado.III=factor(Condicion.cursado.III,levels = c("Recursante","Cambio de plan","Ingresante")))

ggplot_75<-
ggplot(porcentajes, aes(x =as.factor(Examen.final.III),y=porcentaje, fill =Condicion.cursado.III )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c(
      "NO, porque Promocioné" = "Promocioné",
      "SÍ" = "SÍ",
      "NO" = "NO")) +
  labs(title = "Química General e Inorgánica",      
       x = "¿Rindió examen final?", 
       y = "Porcentaje",
       fill="Condición")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"))

# Condicion.alcanzada.I x Consignas.final.I

porcentajes <- encuesta %>%
  drop_na(Consignas.final.I,Condicion.alcanzada.I)%>% #elimina los NA de ambas var
  count(Condicion.alcanzada.I,Consignas.final.I) %>%
  group_by(Condicion.alcanzada.I) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
    ) %>%
  ungroup()%>%
  mutate(Consignas.final.I=factor(Consignas.final.I,levels=c("Poco claras","Claras","Muy claras")),
         Condicion.alcanzada.I=factor(Condicion.alcanzada.I,levels=c("Regular","Libre (rendí las evaluaciones pero me fue mal)")))                                                



ggplot_76<-
ggplot(porcentajes, aes(x =as.factor(Condicion.alcanzada.I ),y=porcentaje, fill =Consignas.final.I )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
   geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  scale_x_discrete(
    labels = c( 
            "Regular" = "Regular",
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre" )) +
  labs(title = "Matemática I",      
       x = "Condición alcanzada",
       y = "Porcentaje",
       fill="Consignas \ndel final")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

# Condicion.alcanzada.II x Consignas.final.II

porcentajes <- encuesta %>%
  drop_na(Condicion.alcanzada.II,Consignas.final.II)%>% #elimina los NA de ambas var
  count(Condicion.alcanzada.II,Consignas.final.II) %>%
  group_by(Condicion.alcanzada.II) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Consignas.final.II=factor(Consignas.final.II,levels=c("Poco claras","Claras","Muy claras")),
         Condicion.alcanzada.II=factor(Condicion.alcanzada.II,levels=c("Regular","Libre (rendí las evaluaciones pero me fue mal)"))) 

ggplot_77<-
ggplot(porcentajes, aes(x =as.factor(Condicion.alcanzada.II ),y=porcentaje, fill =Consignas.final.II )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  scale_x_discrete(
    labels = c( 
      "Regular" = "Regular",
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre" )) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs(title = "Biología",      
       x = "Condición alcanzada",
       y = "Porcentaje",
       fill="Consignas \n del final")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

# Condicion.alcanzada.III x Consignas.final.III

porcentajes <- encuesta %>%
  drop_na(Condicion.alcanzada.III,Consignas.final.III)%>% #elimina los NA de ambas var
  count(Condicion.alcanzada.III,Consignas.final.III) %>%
  group_by(Condicion.alcanzada.III) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05,
                      scales::percent(porcentaje, accuracy = 1), #convierte el decimal en porcentaje y con accuracy lo redondea sin decimal
                      "")
  ) %>%
  ungroup()%>%
  mutate(Consignas.final.III=factor(Consignas.final.III,levels=c("Poco claras","Claras","Muy claras")),
         Condicion.alcanzada.III=factor(Condicion.alcanzada.III,levels=c("Regular","Libre (rendí las evaluaciones pero me fue mal)"))) 

ggplot_78<-
ggplot(porcentajes, aes(x =as.factor(Condicion.alcanzada.III),y=porcentaje, fill =Consignas.final.III )) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) + #fill:hace que la barra sume 100% 
  scale_x_discrete(
    labels = c( 
      "Regular" = "Regular",
      "Libre (rendí las evaluaciones pero me fue mal)" = "Libre" )) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            size = 4, 
            color = "white", 
            fontface = "bold") +
  scale_y_continuous(labels = percent_format()) + #formatea el eje y, de decimales lo convierte a porcentaje
  labs(title = "Química General e Inorgánica",      
       x = "Condición alcanzada",
       y = "Porcentaje",
       fill="Consignas \n del final")+
  theme_minimal() +#elimina los elementos no esenciales del gráfico
  theme_gray()+ #fondo gris por defecto
  theme(axis.text.x = element_text(hjust = 0.5),#centra los valores en el eje x
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(hjust = 0.5,size=10))

