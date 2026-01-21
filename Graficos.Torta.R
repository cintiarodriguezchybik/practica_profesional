#Busco la ruta del archivo de excel

file.choose()
ruta_archivo<-"C:\\Users\\PC\\Downloads\\Encuesta Plan 2023 (Respuestas)(1).xlsx"

#Leo el archivo de Excel

install.packages("readxl")
library(readxl)
encuesta<-read_excel("C:\\Users\\PC\\Downloads\\Encuesta Plan 2023 (Respuestas)(1).xlsx")

#Cargo la librería correspondiente para hacer los gráficos
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(tidyverse)


########################### MATEMÁTICA ############################################################

#Cortaderas.grupal.I

datos_barras <- encuesta %>%
  mutate(Cortaderas.grupal.I = factor(case_when(  
    Cortaderas.grupal.I == "SI" ~ "Sí",
    Cortaderas.grupal.I == "NO" ~ "No",
    is.na(Cortaderas.grupal.I)  ~ "Ns / Nc"),#clasifico los valores en 3 categorias
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.grupal.I)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))#calculo los porcentajes y creo etiquetas
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 15
nuevos_datos[3, "porcentaje"] <- 16.5
nuevos_datos[3, "etiqueta"] <- "15\n(16.5%)"
nuevos_datos[1,4]<-"43\n(47.2%)"
nuevos_datos[2,4]<-"33\n(36.3%)"

colores <- c("#FFFF00", "#FFC125", "#E15759") 

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.grupal.I)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas:\n ¿Se trabajó en forma grupal?",
       subtitle = "Incluyendo los valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)


  

#Cortaderas.individual.I

datos_barras <- encuesta %>%
  mutate(Cortaderas.individual.I = factor(case_when(
    Cortaderas.individual.I == "SI" ~ "Sí",
    Cortaderas.individual.I == "NO" ~ "No",
    is.na(Cortaderas.individual.I)  ~ "Ns / Nc"),
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.individual.I)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 23
nuevos_datos[3, "porcentaje"] <- 25.3
nuevos_datos[3, "etiqueta"] <- "23\n(25.3%)"
nuevos_datos[1,4]<-"33\n(36.3%)"
nuevos_datos[2,4]<-"35\n(38.4%)"

colores <- c("#FFFF00", "#FFC125", "#E15759") 

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.individual.I)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas:\n ¿Se trabajó en forma individual?",
       subtitle = "Incluyendo valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)

#Cortaderas.tarea.I

datos_barras <- encuesta %>%
  mutate(Cortaderas.tarea.I = factor(case_when(
    Cortaderas.tarea.I == "SI" ~ "Sí",
    Cortaderas.tarea.I == "NO" ~ "No",
    is.na(Cortaderas.tarea.I)  ~ "Ns / Nc"),
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.tarea.I)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 25
nuevos_datos[3, "porcentaje"] <- 27.5
nuevos_datos[3, "etiqueta"] <- "25\n(27.5%)"
nuevos_datos[1,4]<-"34\n(37.4%)"
nuevos_datos[2,4]<-"32\n(35.1%)"

colores <- c("#FFFF00", "#FFC125", "#E15759") 

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.tarea.I)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas:\n ¿Se trabajó como tarea?",
       subtitle = "Incluyendo valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)

#Cortaderas.aplicada.I

datos_barras <- encuesta %>%
  mutate(Cortaderas.aplicada.I = factor(case_when(
    Cortaderas.aplicada.I == "SI" ~ "Sí",
    Cortaderas.aplicada.I == "NO" ~ "No",
    is.na(Cortaderas.aplicada.I)  ~ "Ns / Nc"),
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.aplicada.I)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 21
nuevos_datos[3, "porcentaje"] <- 23.1
nuevos_datos[3, "etiqueta"] <- "21\n(23.1%)"
nuevos_datos[1,4]<-"40\n(44.0%)"
nuevos_datos[2,4]<-"30\n(32.9%)"

colores <- c("#FFFF00", "#FFC125", "#E15759") 

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.aplicada.I)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas: ¿Se aplicaron\n los conceptos dados en Matemática?",
       subtitle = "Incluyendo valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)

#Cortaderas.razonada.I

datos_barras <- encuesta %>%
  mutate(Cortaderas.razonada.I = factor(case_when(
    Cortaderas.razonada.I == "SI" ~ "Sí",
    Cortaderas.razonada.I == "NO" ~ "No",
    is.na(Cortaderas.razonada.I)  ~ "Ns / Nc"),
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.razonada.I)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 21
nuevos_datos[3, "porcentaje"] <- 23.1
nuevos_datos[3, "etiqueta"] <- "21\n(23.1%)"
nuevos_datos[1,4]<-"43\n(47.2%)"
nuevos_datos[2,4]<-"27\n(29.7%)"

colores <- c("#FFFF00", "#FFC125", "#E15759") 

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.razonada.I)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas:¿Permitió entender mejor\n la Matemática y su utilidad en la agronomía?",
       subtitle = "Incluyendo valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)

############################# BIOLOGÍA ###########################################################

#Cortaderas.grupal.II

datos_barras <- encuesta %>%
  mutate(Cortaderas.grupal.II = factor(case_when(  
    Cortaderas.grupal.II == "SI" ~ "Sí",
    Cortaderas.grupal.II == "NO" ~ "No",
    is.na(Cortaderas.grupal.II)  ~ "Ns / Nc"),#clasifico los valores en 3 categorias
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.grupal.II)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))#calculo los porcentajes y creo etiquetas
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 19
nuevos_datos[3, "porcentaje"] <- 21.1
nuevos_datos[3, "etiqueta"] <- "19\n(21.1%)"
nuevos_datos[1,4]<-"37\n(41.1%)"
nuevos_datos[2,4]<-"34\n(37.8%)"

colores <- c("green", "#228B22", "#E15759") 

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.grupal.II)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas:\n ¿Se trabajó en forma grupal?",
       subtitle = "Incluyendo los valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)




#Cortaderas.individual.II

datos_barras <- encuesta %>%
  mutate(Cortaderas.individual.II = factor(case_when(
    Cortaderas.individual.II == "SI" ~ "Sí",
    Cortaderas.individual.II == "NO" ~ "No",
    is.na(Cortaderas.individual.II)  ~ "Ns / Nc"),
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.individual.II)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 29
nuevos_datos[3, "porcentaje"] <- 32.2
nuevos_datos[3, "etiqueta"] <- "29\n(32.2%)"
nuevos_datos[1,4]<-"14\n(15.6%)"
nuevos_datos[2,4]<-"47\n(52.2%)"

colores <- c("green", "#228B22", "#E15759")

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.individual.II)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas:\n ¿Se trabajó en forma individual?",
       subtitle = "Incluyendo valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)

#Cortaderas.tarea.II

datos_barras <- encuesta %>%
  mutate(Cortaderas.tarea.II = factor(case_when(
    Cortaderas.tarea.II == "SI" ~ "Sí",
    Cortaderas.tarea.II == "NO" ~ "No",
    is.na(Cortaderas.tarea.II)  ~ "Ns / Nc"),
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.tarea.II)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 25
nuevos_datos[3, "porcentaje"] <- 27.8
nuevos_datos[3, "etiqueta"] <- "25\n(27.8%)"
nuevos_datos[1,4]<-"19\n(21.1%)"
nuevos_datos[2,4]<-"46\n(51.1%)"

colores <- c("green", "#228B22", "#E15759")

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.tarea.II)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas:\n ¿Se trabajó como tarea?",
       subtitle = "Incluyendo valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)

#Cortaderas.aplicada.II

datos_barras <- encuesta %>%
  mutate(Cortaderas.aplicada.II = factor(case_when(
    Cortaderas.aplicada.II == "SI" ~ "Sí",
    Cortaderas.aplicada.II == "NO" ~ "No",
    is.na(Cortaderas.aplicada.II)  ~ "Ns / Nc"),
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.aplicada.II)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 25
nuevos_datos[3, "porcentaje"] <- 27.8
nuevos_datos[3, "etiqueta"] <- "25\n(27.8%)"
nuevos_datos[1,4]<-"27\n(30.0%)"
nuevos_datos[2,4]<-"38\n(42.2%)"

colores <- c("green", "#228B22", "#E15759")

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.aplicada.II)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas: ¿Se aplicaron\n los conceptos dados en Matemática?",
       subtitle = "Incluyendo valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)

#Cortaderas.razonada.II

datos_barras <- encuesta %>%
  mutate(Cortaderas.razonada.II= factor(case_when(
    Cortaderas.razonada.II == "SI" ~ "Sí",
    Cortaderas.razonada.II == "NO" ~ "No",
    is.na(Cortaderas.razonada.II)  ~ "Ns / Nc"),
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.razonada.II)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 27
nuevos_datos[3, "porcentaje"] <- 30.0
nuevos_datos[3, "etiqueta"] <- "27\n(30.0%)"
nuevos_datos[1,4]<-"25\n(27.8%)"
nuevos_datos[2,4]<-"38\n(42.2%)"

colores <- c("green", "#228B22", "#E15759")

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.razonada.II)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas:¿Permitió entender mejor\n la Matemática y su utilidad en la agronomía?",
       subtitle = "Incluyendo valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)


################################## QUÍMICA GENERAL E INORGÁNICA #####################################

#Cortaderas.grupal.III

datos_barras <- encuesta %>%
  mutate(Cortaderas.grupal.III = factor(case_when(  
    Cortaderas.grupal.III == "SI" ~ "Sí",
    Cortaderas.grupal.III == "NO" ~ "No",
    is.na(Cortaderas.grupal.III)  ~ "Ns / Nc"),#clasifico los valores en 3 categorias
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.grupal.III)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))#calculo los porcentajes y creo etiquetas
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 25
nuevos_datos[3, "porcentaje"] <-31.3 
nuevos_datos[3, "etiqueta"] <- "25\n(31.3%)"
nuevos_datos[1,4]<-"24\n(30.0%)"
nuevos_datos[2,4]<-"31\n(38.7%)"

colores <- c("pink", "#FF69B4", "#E15759") 

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.grupal.III)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas:\n ¿Se trabajó en forma grupal?",
       subtitle = "Incluyendo los valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)




#Cortaderas.individual.III

datos_barras <- encuesta %>%
  mutate(Cortaderas.individual.III = factor(case_when(
    Cortaderas.individual.III == "SI" ~ "Sí",
    Cortaderas.individual.III == "NO" ~ "No",
    is.na(Cortaderas.individual.III)  ~ "Ns / Nc"),
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.individual.III)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 28
nuevos_datos[3, "porcentaje"] <- 35.0
nuevos_datos[3, "etiqueta"] <- "28\n(35.0%)"
nuevos_datos[1,4]<-"13\n(16.3%)"
nuevos_datos[2,4]<-"39\n(48.7%)"

colores <- c("pink", "#FF69B4", "#E15759") 

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.individual.III)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas:\n ¿Se trabajó en forma individual?",
       subtitle = "Incluyendo valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)

#Cortaderas.tarea.III

datos_barras <- encuesta %>%
  mutate(Cortaderas.tarea.III = factor(case_when(
    Cortaderas.tarea.III == "SI" ~ "Sí",
    Cortaderas.tarea.III == "NO" ~ "No",
    is.na(Cortaderas.tarea.III)  ~ "Ns / Nc"),
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.tarea.III)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 20
nuevos_datos[3, "porcentaje"] <- 25.0
nuevos_datos[3, "etiqueta"] <- "20\n(25.0%)"
nuevos_datos[1,4]<-"38\n(47.5%)"
nuevos_datos[2,4]<-"22\n(27.5%)"

colores <- c("pink", "#FF69B4", "#E15759") 

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.tarea.III)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas:\n ¿Se trabajó como tarea?",
       subtitle = "Incluyendo valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)

#Cortaderas.aplicada.III

datos_barras <- encuesta %>%
  mutate(Cortaderas.aplicada.III = factor(case_when(
    Cortaderas.aplicada.III == "SI" ~ "Sí",
    Cortaderas.aplicada.III == "NO" ~ "No",
    is.na(Cortaderas.aplicada.III)  ~ "Ns / Nc"),
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.aplicada.III)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 24
nuevos_datos[3, "porcentaje"] <- 30.0
nuevos_datos[3, "etiqueta"] <- "24\n(30.0%)"
nuevos_datos[1,4]<-"28\n(35.0%)"
nuevos_datos[2,4]<-"28\n(35.0%)"

colores <- c("pink", "#FF69B4", "#E15759") 

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.aplicada.III)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas: ¿Se aplicaron\n los conceptos dados en Matemática?",
       subtitle = "Incluyendo valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)

#Cortaderas.razonada.III

datos_barras <- encuesta %>%
  mutate(Cortaderas.razonada.III= factor(case_when(
    Cortaderas.razonada.III == "SI" ~ "Sí",
    Cortaderas.razonada.III== "NO" ~ "No",
    is.na(Cortaderas.razonada.III)  ~ "Ns / Nc"),
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.razonada.III)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 26
nuevos_datos[3, "porcentaje"] <- 32.5
nuevos_datos[3, "etiqueta"] <- "26\n(32.5%)"
nuevos_datos[1,4]<-"26\n(32.5%)"
nuevos_datos[2,4]<-"28\n(35.0%)"

colores <- c("pink", "#FF69B4", "#E15759") 

ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.razonada.III)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4, fontface = "bold",lineheight = 0.8) +
  labs(title = "Las Cortaderas:¿Permitió entender mejor\n la Matemática y su utilidad en la agronomía?",
       subtitle = "Incluyendo valores faltantes",
       fill = "Respuesta") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)

######################################CORTADERAS INTEGRADA##############################

#Cortaderas.general
datos_barras <- encuesta %>%
  mutate(Cortaderas.general= factor(case_when(
    Cortaderas.general == "SÍ" ~ "Sí",
    Cortaderas.general== "NO" ~ "No",
    is.na(Cortaderas.general)  ~ "Ns / Nc"),
    levels = c("Sí", "No", "Ns / Nc") )) %>% # Orden personalizado
  count(Cortaderas.general)%>%
  mutate( porcentaje = n/sum(n)*100,etiqueta = paste0(n, "\n(", round(porcentaje, 1), "%)"))
datos_barras

#Reemplazo los valores de la fila de valores faltantes porque hay 1 estudiantes que no debe responder(no es valor faltante)
nuevos_datos <- datos_barras
nuevos_datos[3, "n"] <- 7
nuevos_datos[3, "porcentaje"] <- 8.97
nuevos_datos[3, "etiqueta"] <- "8,97%\n(7)"
nuevos_datos[1,4]<-"50%\n(39)"
nuevos_datos[2,4]<-"41,03%\n(32)"

colores <- c("skyblue1", "darkviolet", "#E15759") 

ggplot_general<-
ggplot(nuevos_datos, aes(x = "", y = n, fill = Cortaderas.general)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 3, fontface = "bold",lineheight = 0.8) +
  labs(fill = "Respuesta") +
  theme_void() +
  theme(legend.title = element_text(face = "bold")) +
  scale_fill_manual(values = colores)
