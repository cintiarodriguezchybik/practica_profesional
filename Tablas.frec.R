library(dplyr)



########TABLAS FRECUENCIAS CON TOTALES#####################

# Lugar.residencia x Ingreso  

localidades_otros<-c("Albarellos","Carcaraña","Funes","Roldan","Las parejas","Villa Gobernador Galvez")

encuesta <- encuesta %>%
  mutate(Lugar.residencia.nuevo = case_when(
    Lugar.residencia %in% localidades_otros ~ "Otros",
    TRUE ~ Lugar.residencia),
    Lugar.residencia.nuevo = factor(Lugar.residencia.nuevo,
                                    levels = c("Zavalla", "Rosario", "Otros"))#reordeno las categorias
  )

tabla1 <- table(encuesta$Lugar.residencia.nuevo,encuesta$Ingreso)
tabla1_con_totales <- addmargins(tabla1) # Agregar totales marginales
print(tabla1_con_totales) # Mostrar la tabla

                       #test chi cuadrado

Ingreso_agrupado <- ifelse(encuesta$Ingreso %in% c("2022", "2023"), "2022-2023", "2024")#agrupo ambos años para cumplir supuestos

tabla1_agrupado <- table(encuesta$Lugar.residencia.nuevo,Ingreso_agrupado)#tabla con ingresos agrupados
tabla1_agrupado_totales<-addmargins(tabla1_agrupado)#con totales
chi.1<-chisq.test(tabla1_agrupado)
print(chi.2) 

# Tiene.trabajo x Ingreso

encuesta$Tiene.trabajo <- factor(encuesta$Tiene.trabajo, levels = c("SÍ", "NO")) #CAMBIO EL ORDEN

tabla2 <- table(encuesta$Tiene.trabajo,encuesta$Ingreso)
tabla2_con_totales <- addmargins(tabla2) # Agregar totales marginales
print(tabla2_con_totales) # Mostrar la tabla

                     #test chi cuadrado

Ingreso_agrupado <- ifelse(encuesta$Ingreso %in% c("2022", "2023"), "2022-2023", "2024")#agrupo ambos años para cumplir supuestos

tabla2_agrupado <- table(encuesta$Tiene.trabajo,Ingreso_agrupado)#tabla con ingresos agrupados
tabla2_agrupado_totales<-addmargins(tabla2_agrupado)#con totales
chi.2<-chisq.test(tabla2_agrupado)
print(chi.2)

# Personas.con.las.que.vive x Ingreso

tabla3 <- table(encuesta$Personas.con.las.que.vive,encuesta$Ingreso)
tabla3_con_totales <- addmargins(tabla3) # Agregar totales marginales
print(tabla3_con_totales) # Mostrar la tabla

                        #test chi cuadrado

Ingreso_agrupado <- ifelse(encuesta$Ingreso %in% c("2022", "2023"), "2022-2023", "2024")#agrupo ambos años para cumplir supuestos

tabla3_agrupado <- table(encuesta$Personas.con.las.que.vive,Ingreso_agrupado)#tabla con ingresos agrupados
tabla3_agrupado_totales<-addmargins(tabla3_agrupado)#con totales
chi.3<-chisq.test(tabla3_agrupado)
print(chi.3)

# Personas.con.las.que.vive x Tiene.trabajo

tabla4 <- table(encuesta$Personas.con.las.que.vive,encuesta$Tiene.trabajo)
tabla4_con_totales <- addmargins(tabla4) # Agregar totales marginales
print(tabla4_con_totales) # Mostrar la tabla

                   #test chi cuadrado

chi.4<-chisq.test(tabla4)
print(chi.4)


# Condicion.alcanzada.I x Ingreso

tabla5 <- table(encuesta$Condicion.alcanzada.I,encuesta$Ingreso)
tabla5_con_totales <- addmargins(tabla5) # Agregar totales marginales
print(tabla5_con_totales) # Mostrar la tabla
rownames(tabla5_con_totales)[rownames(tabla5_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
                      
#test chi cuadrado

Ingreso_agrupado <- ifelse(encuesta$Ingreso %in% c("2022", "2023"), "2022-2023", "2024")#agrupo ambos años para cumplir supuestos
Condicion.alcanzada.I_agrupado<-ifelse(encuesta$Condicion.alcanzada.I %in% c("Abandoné","Libre (rendí las evaluaciones pero me fue mal)"),"Logrado","No logrado")

tabla5_agrupado <- table(Condicion.alcanzada.I_agrupado,Ingreso_agrupado)#tabla con ingresos agrupados y Condicion.alcanzada.I agrupada
tabla5_agrupado_totales<-addmargins(tabla5_agrupado)#con totales
chi.5<-chisq.test(tabla5_agrupado)
print(chi.5)


# Condicion.alcanzada.II x Ingreso

tabla6 <- table(encuesta$Condicion.alcanzada.II,encuesta$Ingreso)
tabla6_con_totales <- addmargins(tabla6) # Agregar totales marginales
print(tabla6_con_totales) # Mostrar la tabla
rownames(tabla6_con_totales)[rownames(tabla6_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"

                        #test chi cuadrado

Ingreso_agrupado <- ifelse(encuesta$Ingreso %in% c("2022", "2023"), "2022-2023", "2024")#agrupo ambos años para cumplir supuestos
Condicion.alcanzada.II_agrupado<-ifelse(encuesta$Condicion.alcanzada.II %in% c("Abandoné","Libre (rendí las evaluaciones pero me fue mal)"),"No logrado","Logrado")

tabla6_agrupado <- table(Condicion.alcanzada.II_agrupado,Ingreso_agrupado)#tabla con ingresos agrupados y Condicion.alcanzada.I agrupada
tabla6_agrupado_totales<-addmargins(tabla6_agrupado)#con totales
chi.6<-chisq.test(tabla6_agrupado)
print(chi.6)



# Condicion.alcanzada.III x Ingreso

encuesta$Condicion.alcanzada.III <- factor(encuesta$Condicion.alcanzada.III, levels = c("Me anoté, pero no empecé a cursar.","Abandoné","Libre (rendí las evaluaciones pero me fue mal)","Promocioné" ,"Regular" )) #CAMBIO EL ORDEN

tabla7 <- table(encuesta$Condicion.alcanzada.III,encuesta$Ingreso)
tabla7_con_totales <- addmargins(tabla7) # Agregar totales marginales
print(tabla7_con_totales) # Mostrar la tabla
rownames(tabla7_con_totales)[rownames(tabla7_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"

                    #test chi cuadrado

Ingreso_agrupado <- ifelse(encuesta$Ingreso %in% c("2022", "2023"), "2022-2023", "2024")#agrupo ambos años para cumplir supuestos
Condicion.alcanzada.III_agrupado<-ifelse(encuesta$Condicion.alcanzada.III %in% c("Abandoné","Libre (rendí las evaluaciones pero me fue mal)","Me anoté, pero no empecé a cursar."),"No logrado","Logrado")

tabla7_agrupado <- table(Condicion.alcanzada.III_agrupado,Ingreso_agrupado)#tabla con ingresos agrupados y Condicion.alcanzada.I agrupada
tabla7_agrupado_totales<-addmargins(tabla7_agrupado)#con totales
chi.7<-chisq.test(tabla7_agrupado)
print(chi.7)

# Condicion.alcanzada.I x Tiene.trabajo

tabla8 <- table(encuesta$Condicion.alcanzada.I,encuesta$Tiene.trabajo)
tabla8_con_totales <- addmargins(tabla8) # Agregar totales marginales
print(tabla8_con_totales) # Mostrar la tabla
rownames(tabla8_con_totales)[rownames(tabla8_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
                     #test chi cuadrado

Condicion.alcanzada.I_agrupado<-ifelse(encuesta$Condicion.alcanzada.I %in% c("Abandoné","Libre (rendí las evaluaciones pero me fue mal)"),"No logrado","Logrado")

tabla11_agrupado <- table(Condicion.alcanzada.I_agrupado,encuesta$Tiene.trabajo)#Condicion.alcanzada.I agrupada
tabla11_agrupado_totales<-addmargins(tabla11_agrupado)#con totales
chi.11<-chisq.test(tabla11_agrupado)
print(chi.11)

# Condicion.alcanzada.II x Tiene.trabajo

tabla9 <- table(encuesta$Condicion.alcanzada.II,encuesta$Tiene.trabajo)
tabla9_con_totales <- addmargins(tabla9) # Agregar totales marginales
print(tabla9_con_totales) # Mostrar la tabla
rownames(tabla9_con_totales)[rownames(tabla9_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
                 #test chi cuadrado

Condicion.alcanzada.II_agrupado<-ifelse(encuesta$Condicion.alcanzada.II %in% c("Abandoné","Libre (rendí las evaluaciones pero me fue mal)"),"No logrado","Logrado")

tabla12_agrupado <- table(Condicion.alcanzada.II_agrupado,encuesta$Tiene.trabajo)#Condicion.alcanzada.II agrupada
tabla12_agrupado_totales<-addmargins(tabla12_agrupado)#con totales
chi.12<-chisq.test(tabla12_agrupado)
print(chi.12)

# Condicion.alcanzada.III x Tiene.trabajo

tabla10 <- table(encuesta$Condicion.alcanzada.III,encuesta$Tiene.trabajo)
tabla10_con_totales <- addmargins(tabla10) # Agregar totales marginales
print(tabla10_con_totales) # Mostrar la tabla
rownames(tabla10_con_totales)[rownames(tabla10_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
rownames(tabla10_con_totales)[rownames(tabla10_con_totales) == "Me anoté, pero no empecé a cursar."]<-"No cursé"
               
                              #test chi cuadrado

encuesta<- encuesta %>%
  filter(!is.na(Condicion.alcanzada.III))#elimino los NA 

Condicion.alcanzada.III_agrupado<-ifelse(encuesta$Condicion.alcanzada.III %in% c("Me anoté, pero no empecé a cursar.","Abandoné","Libre (rendí las evaluaciones pero me fue mal)"),"No logrado","Logrado")

tabla13_agrupado <- table(Condicion.alcanzada.III_agrupado,encuesta$Tiene.trabajo)#Condicion.alcanzada.III agrupada
tabla13_agrupado_totales<-addmargins(tabla13_agrupado)#con totales
chi.13<-chisq.test(tabla13_agrupado)
print(chi.13)

# Asistencia.consulta.I x Tiempo.traslado

tabla11 <- table(encuesta$Asistencia.consulta.I,encuesta$Tiempo.traslado)
tabla11_con_totales <- addmargins(tabla11) # Agregar totales marginales
print(tabla11_con_totales) # Mostrar la tabla
colnames(tabla11_con_totales)[colnames(tabla11_con_totales) =="Entre 1 y 2 hs."]<-"1-2hs."
colnames(tabla11_con_totales)[colnames(tabla11_con_totales) =="Entre 2 y 3 hs."]<-"2-3hs."
colnames(tabla11_con_totales)[colnames(tabla11_con_totales) =="Menos de 1 h."]<-"<1h."
colnames(tabla11_con_totales)[colnames(tabla11_con_totales) =="Más de 3 hs."]<-">3hs." 
                           
# Asistencia.consulta.II x Tiempo.traslado

tabla12 <- table(encuesta$Asistencia.consulta.II,encuesta$Tiempo.traslado)
tabla12_con_totales <- addmargins(tabla12) # Agregar totales marginales
print(tabla12_con_totales) # Mostrar la tabla
colnames(tabla12_con_totales)[colnames(tabla12_con_totales) =="Entre 1 y 2 hs."]<-"1-2hs."
colnames(tabla12_con_totales)[colnames(tabla12_con_totales) =="Entre 2 y 3 hs."]<-"2-3hs."
colnames(tabla12_con_totales)[colnames(tabla12_con_totales) =="Menos de 1 h."]<-"<1h."
colnames(tabla12_con_totales)[colnames(tabla12_con_totales) =="Más de 3 hs."]<-">3hs." 

# Asistencia.consulta.III x Tiempo.traslado

tabla13 <- table(encuesta$Asistencia.consulta.III,encuesta$Tiempo.traslado)
tabla13_con_totales <- addmargins(tabla13) # Agregar totales marginales
print(tabla13_con_totales) # Mostrar la tabla
colnames(tabla13_con_totales)[colnames(tabla13_con_totales) =="Entre 1 y 2 hs."]<-"1-2hs."
colnames(tabla13_con_totales)[colnames(tabla13_con_totales) =="Entre 2 y 3 hs."]<-"2-3hs."
colnames(tabla13_con_totales)[colnames(tabla13_con_totales) =="Menos de 1 h."]<-"<1h."
colnames(tabla13_con_totales)[colnames(tabla13_con_totales) =="Más de 3 hs."]<-">3hs." 

# Condicion.alcanzada.I x Calidad.explicativa.docente.I

encuesta$Condicion.alcanzada.I <- factor(encuesta$Condicion.alcanzada.I, levels = c("Libre (rendí las evaluaciones pero me fue mal)","Regular","Promocioné")) #CAMBIO EL ORDEN

tabla14 <- table(encuesta$Calidad.explicativa.docente.I,encuesta$Condicion.alcanzada.I)
tabla14_con_totales <- addmargins(tabla14) # Agregar totales marginales
print(tabla14_con_totales) # Mostrar la tabla
colnames(tabla14_con_totales)[colnames(tabla14_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
rownames(tabla14_con_totales)[rownames(tabla14_con_totales) == "No me resultaron claras"] <- "No claras"
 
                   #test chi cuadrado

Condicion.alcanzada.I_agrupado<-ifelse(encuesta$Condicion.alcanzada.I %in% c("Libre (rendí las evaluaciones pero me fue mal)"),"No logrado","Logrado")

tabla17_agrupado <- table(encuesta$Calidad.explicativa.docente.I,Condicion.alcanzada.I_agrupado)#Condicion.alcanzada.I agrupada
tabla17_agrupado_totales<-addmargins(tabla17_agrupado)#con totales
chi.17<-chisq.test(tabla17_agrupado)
print(chi.17)

# Condicion.alcanzada.II x Calidad.explicativa.docente.II

encuesta$Condicion.alcanzada.II <- factor(encuesta$Condicion.alcanzada.II, levels = c("Libre (rendí las evaluaciones pero me fue mal)","Regular","Promocioné")) #CAMBIO EL ORDEN

tabla15 <- table(encuesta$Calidad.explicativa.docente.II,encuesta$Condicion.alcanzada.II)
tabla15_con_totales <- addmargins(tabla15) # Agregar totales marginales
print(tabla15_con_totales) # Mostrar la tabla
colnames(tabla15_con_totales)[colnames(tabla15_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
rownames(tabla15_con_totales)[rownames(tabla15_con_totales) == "No me resultaron claras"] <- "No claras"

                  #test chi cuadrado

Condicion.alcanzada.II_agrupado<-ifelse(encuesta$Condicion.alcanzada.II %in% c("Libre (rendí las evaluaciones pero me fue mal)"),"No logrado","Logrado")

tabla18_agrupado <- table(encuesta$Calidad.explicativa.docente.II,Condicion.alcanzada.II_agrupado)#Condicion.alcanzada.I agrupada
tabla18_agrupado_totales<-addmargins(tabla18_agrupado)#con totales
chi.18<-chisq.test(tabla18_agrupado)
print(chi.18)

# Condicion.alcanzada.III x Calidad.explicativa.docente.III

encuesta$Condicion.alcanzada.III <- factor(encuesta$Condicion.alcanzada.III, levels = c("Libre (rendí las evaluaciones pero me fue mal)","Regular","Promocioné")) #CAMBIO EL ORDEN

tabla16 <- table(encuesta$Calidad.explicativa.docente.III,encuesta$Condicion.alcanzada.III)
tabla16_con_totales <- addmargins(tabla16) # Agregar totales marginales
print(tabla16_con_totales) # Mostrar la tabla
colnames(tabla16_con_totales)[colnames(tabla16_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
rownames(tabla16_con_totales)[rownames(tabla16_con_totales) == "No me resultaron claras"] <- "No claras"

                    #test chi cuadrado

Condicion.alcanzada.III_agrupado<-ifelse(encuesta$Condicion.alcanzada.III %in% c("Libre (rendí las evaluaciones pero me fue mal)"),"No logrado","Logrado")

tabla19_agrupado <- table(encuesta$Calidad.explicativa.docente.III,Condicion.alcanzada.III_agrupado)#Condicion.alcanzada.I agrupada
tabla19_agrupado_totales<-addmargins(tabla19_agrupado)#con totales
chi.19<-chisq.test(tabla19_agrupado)
print(chi.19)

#Contenidos.I x Asistencia.consulta.I

encuesta$Contenidos.I<-factor(encuesta$Contenidos.I,levels=c("Excesivo","Adecuado","Insuficiente"))
encuesta$Asistencia.consulta.I<-factor(encuesta$Asistencia.consulta.I,levels=c("NUNCA","A VECES","SÍ"))

tabla17 <- table(encuesta$Asistencia.consulta.I,encuesta$Contenidos.I)
tabla17_con_totales <- addmargins(tabla17) # Agregar totales marginales
print(tabla17_con_totales) # Mostrar la tabla

#Contenidos.II x Asistencia.consulta.II

encuesta$Contenidos.II<-factor(encuesta$Contenidos.II,levels=c("Excesivo","Adecuado","Insuficiente"))
encuesta$Asistencia.consulta.II<-factor(encuesta$Asistencia.consulta.II,levels=c("NUNCA","A VECES","SÍ"))

tabla18 <- table(encuesta$Asistencia.consulta.II,encuesta$Contenidos.II)
tabla18_con_totales <- addmargins(tabla18) # Agregar totales marginales
print(tabla18_con_totales) # Mostrar la tabla

#Contenidos.III x Asistencia.consulta.III

encuesta$Contenidos.III<-factor(encuesta$Contenidos.III,levels=c("Excesivo","Adecuado","Insuficiente"))
encuesta$Asistencia.consulta.III<-factor(encuesta$Asistencia.consulta.III,levels=c("NUNCA","A VECES","SÍ"))

tabla19 <- table(encuesta$Asistencia.consulta.III,encuesta$Contenidos.III)
tabla19_con_totales <- addmargins(tabla19) # Agregar totales marginales
print(tabla19_con_totales) # Mostrar la tabla

# Asistencia.consulta.I x Condicion.alcanzada.I 

encuesta$Asistencia.consulta.I<-factor(encuesta$Asistencia.consulta.I,levels=c("SÍ","A VECES","NUNCA"))

tabla20 <- table(encuesta$Condicion.alcanzada.I,encuesta$Asistencia.consulta.I)
tabla20_con_totales <- addmargins(tabla20) # Agregar totales marginales
print(tabla20_con_totales) # Mostrar la tabla
rownames(tabla20_con_totales)[rownames(tabla20_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"

                      #test chi cuadrado

Condicion.alcanzada.I_agrupado<-ifelse(encuesta$Condicion.alcanzada.I %in% c("Libre (rendí las evaluaciones pero me fue mal)"),"No logrado","Logrado")

tabla23_agrupado <- table(Condicion.alcanzada.I_agrupado,encuesta$Asistencia.consulta.I)#Condicion.alcanzada.I agrupada
tabla23_agrupado_totales<-addmargins(tabla23_agrupado)#con totales
chi.23<-chisq.test(tabla23_agrupado)
print(chi.23)



# Asistencia.consulta.II x Condicion.alcanzada.II 

encuesta$Asistencia.consulta.II<-factor(encuesta$Asistencia.consulta.II,levels=c("SÍ","A VECES","NUNCA"))

tabla21 <- table(encuesta$Condicion.alcanzada.II,encuesta$Asistencia.consulta.II)
tabla21_con_totales <- addmargins(tabla21) # Agregar totales marginales
print(tabla21_con_totales) # Mostrar la tabla
rownames(tabla21_con_totales)[rownames(tabla21_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
                         #test chi cuadrado

Condicion.alcanzada.II_agrupado<-ifelse(encuesta$Condicion.alcanzada.II %in% c("Libre (rendí las evaluaciones pero me fue mal)"),"No logrado","Logrado")

tabla24_agrupado <- table(Condicion.alcanzada.II_agrupado,encuesta$Asistencia.consulta.II)#Condicion.alcanzada.I agrupada
tabla24_agrupado_totales<-addmargins(tabla24_agrupado)#con totales
chi.24<-chisq.test(tabla24_agrupado)
print(chi.24)

# Asistencia.consulta.III x Condicion.alcanzada.III

encuesta$Asistencia.consulta.III<-factor(encuesta$Asistencia.consulta.III,levels=c("SÍ","A VECES","NUNCA"))

tabla22 <- table(encuesta$Condicion.alcanzada.III,encuesta$Asistencia.consulta.III)
tabla22_con_totales <- addmargins(tabla22) # Agregar totales marginales
print(tabla22_con_totales) # Mostrar la tabla
rownames(tabla22_con_totales)[rownames(tabla22_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
                               #test chi cuadrado

Condicion.alcanzada.III_agrupado<-ifelse(encuesta$Condicion.alcanzada.III %in% c("Libre (rendí las evaluaciones pero me fue mal)"),"No logrado","Logrado")

tabla19_agrupado <- table(Condicion.alcanzada.III_agrupado,encuesta$Asistencia.consulta.III)#Condicion.alcanzada.I agrupada
tabla19_agrupado_totales<-addmargins(tabla19_agrupado)#con totales
chi.19<-chisq.test(tabla19_agrupado)
print(chi.19)

# Condicion.cursado.I x Condicion.alcanzada.I

encuesta$Condicion.cursado.I <- factor(encuesta$Condicion.cursado.I, levels = c("Recursante","Cambio de plan", "Ingresante" )) #CAMBIO EL ORDEN

tabla23 <- table(encuesta$Condicion.alcanzada.I,encuesta$Condicion.cursado.I)
tabla23_con_totales <- addmargins(tabla23) # Agregar totales marginales
print(tabla23_con_totales) # Mostrar la tabla
rownames(tabla23_con_totales)[rownames(tabla23_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
# Condicion.cursado.II x Condicion.alcanzada.II

encuesta$Condicion.cursado.II <- factor(encuesta$Condicion.cursado.II, levels = c("Recursante","Cambio de plan", "Ingresante" )) #CAMBIO EL ORDEN

tabla24 <- table(encuesta$Condicion.alcanzada.II,encuesta$Condicion.cursado.II)
tabla24_con_totales <- addmargins(tabla24) # Agregar totales marginales
print(tabla24_con_totales) # Mostrar la tabla
rownames(tabla24_con_totales)[rownames(tabla24_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"

# Condicion.cursado.III x Condicion.alcanzada.III

encuesta$Condicion.cursado.III <- factor(encuesta$Condicion.cursado.III, levels = c("Recursante","Cambio de plan", "Ingresante" )) #CAMBIO EL ORDEN

tabla25 <- table(encuesta$Condicion.alcanzada.III,encuesta$Condicion.cursado.III)
tabla25_con_totales <- addmargins(tabla25) # Agregar totales marginales
print(tabla25_con_totales) # Mostrar la tabla
rownames(tabla25_con_totales)[rownames(tabla25_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"

#Condicion.alcanzada.I x Recursos.utilizados.I

encuesta$Recursos.utilizados.I <- factor(encuesta$Recursos.utilizados.I, levels = c("Muy adecuados","Adecuados", "Poco adecuados" )) #CAMBIO EL ORDEN

tabla26 <- table(encuesta$Condicion.alcanzada.I,encuesta$Recursos.utilizados.I)
tabla26_con_totales <- addmargins(tabla26) # Agregar totales marginales
print(tabla26_con_totales) # Mostrar la tabla
rownames(tabla26_con_totales)[rownames(tabla26_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"

#Condicion.alcanzada.II x Recursos.utilizados.II

encuesta$Recursos.utilizados.II <- factor(encuesta$Recursos.utilizados.II, levels = c("Muy adecuados","Adecuados", "Poco adecuados" )) #CAMBIO EL ORDEN

tabla27 <- table(encuesta$Condicion.alcanzada.II,encuesta$Recursos.utilizados.II)
tabla27_con_totales <- addmargins(tabla27) # Agregar totales marginales
print(tabla27_con_totales) # Mostrar la tabla
rownames(tabla27_con_totales)[rownames(tabla27_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"

#Condicion.alcanzada.III x Recursos.utilizados.III

encuesta$Recursos.utilizados.III <- factor(encuesta$Recursos.utilizados.III, levels = c("Muy adecuados","Adecuados", "Poco adecuados" )) #CAMBIO EL ORDEN

tabla28 <- table(encuesta$Condicion.alcanzada.III,encuesta$Recursos.utilizados.III)
tabla28_con_totales <- addmargins(tabla28) # Agregar totales marginales
print(tabla28_con_totales) # Mostrar la tabla
rownames(tabla28_con_totales)[rownames(tabla28_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"

#Contenido.clase/parcial.I x Condicion.alcanzada.I

encuesta$Condicion.alcanzada.I <- factor(encuesta$Condicion.alcanzada.I, levels = c("Regular","Promocioné","Libre (rendí las evaluaciones pero me fue mal)")) #CAMBIO EL ORDEN
encuesta$`Contenidos.clase/parcial.I` <- factor(encuesta$`Contenidos.clase/parcial.I`, levels = c("Algunos temas no los vimos en las clases.","Algunos temas los vimos poco en las clases.","Si, los vimos varias veces")) #CAMBIO EL ORDEN

tabla29 <- table(encuesta$`Contenidos.clase/parcial.I`,encuesta$Condicion.alcanzada.I)
tabla29_con_totales <- addmargins(tabla29) # Agregar totales marginales
print(tabla29_con_totales) # Mostrar la tabla
colnames(tabla29_con_totales)[colnames(tabla29_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
rownames(tabla29_con_totales)[rownames(tabla29_con_totales) == "Algunos temas no los vimos en las clases."] <- "Algunos no"
rownames(tabla29_con_totales)[rownames(tabla29_con_totales) == "Algunos temas los vimos poco en las clases."] <- "Si/poco"
rownames(tabla29_con_totales)[rownames(tabla29_con_totales) == "Si, los vimos varias veces"] <- "Si"
 
#Contenido.clase/parcial.II x Condicion.alcanzada.II

encuesta$Condicion.alcanzada.II <- factor(encuesta$Condicion.alcanzada.II, levels = c("Regular","Promocioné","Libre (rendí las evaluaciones pero me fue mal)")) #CAMBIO EL ORDEN
encuesta$`Contenidos.clase/parcial.II` <- factor(encuesta$`Contenidos.clase/parcial.II`, levels = c("Algunos temas no los vimos en las clases.","Algunos temas los vimos poco en las clases.","Si, los vimos varias veces")) #CAMBIO EL ORDEN

tabla30 <- table(encuesta$`Contenidos.clase/parcial.II`,encuesta$Condicion.alcanzada.II)
tabla30_con_totales <- addmargins(tabla30) # Agregar totales marginales
print(tabla30_con_totales) # Mostrar la tabla
colnames(tabla30_con_totales)[colnames(tabla30_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
rownames(tabla30_con_totales)[rownames(tabla30_con_totales) == "Algunos temas no los vimos en las clases."] <- "Algunos no"
rownames(tabla30_con_totales)[rownames(tabla30_con_totales) == "Algunos temas los vimos poco en las clases."] <- "Si/poco"
rownames(tabla30_con_totales)[rownames(tabla30_con_totales) == "Si, los vimos varias veces"] <- "Si"


#Contenido.clase/parcial.III x Condicion.alcanzada.III

encuesta$Condicion.alcanzada.III <- factor(encuesta$Condicion.alcanzada.III, levels = c("Regular","Promocioné","Libre (rendí las evaluaciones pero me fue mal)")) #CAMBIO EL ORDEN
encuesta$`Contenidos.clase/parcial.III` <- factor(encuesta$`Contenidos.clase/parcial.III`, levels = c("Algunos temas no los vimos en las clases.","Algunos temas los vimos poco en las clases.","Si, los vimos varias veces")) #CAMBIO EL ORDEN

tabla31 <- table(encuesta$`Contenidos.clase/parcial.III`,encuesta$Condicion.alcanzada.III)
tabla31_con_totales <- addmargins(tabla31) # Agregar totales marginales
print(tabla31_con_totales) # Mostrar la tabla
colnames(tabla31_con_totales)[colnames(tabla31_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
rownames(tabla31_con_totales)[rownames(tabla31_con_totales) == "Algunos temas no los vimos en las clases."] <- "Algunos no"
rownames(tabla31_con_totales)[rownames(tabla31_con_totales) == "Algunos temas los vimos poco en las clases."] <- "Si/poco"
rownames(tabla31_con_totales)[rownames(tabla31_con_totales) == "Si, los vimos varias veces"] <- "Si"


# Condicion.alcanzada.I x Contenidos.I

encuesta$Condicion.alcanzada.I <- factor(encuesta$Condicion.alcanzada.I, levels = c("Libre (rendí las evaluaciones pero me fue mal)","Regular","Promocioné")) #CAMBIO EL ORDEN
encuesta$Contenidos.I<-factor(encuesta$Contenidos.I,levels=c("Excesivo","Adecuado","Insuficiente"))

tabla32 <- table(encuesta$Condicion.alcanzada.I,encuesta$Contenidos.I)
tabla32_con_totales <- addmargins(tabla32) # Agregar totales marginales
print(tabla32_con_totales) # Mostrar la tabla
rownames(tabla32_con_totales)[rownames(tabla32_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"

# Condicion.alcanzada.II x Contenidos.II

encuesta$Condicion.alcanzada.II <- factor(encuesta$Condicion.alcanzada.II, levels = c("Libre (rendí las evaluaciones pero me fue mal)","Regular","Promocioné")) #CAMBIO EL ORDEN
encuesta$Contenidos.II<-factor(encuesta$Contenidos.II,levels=c("Excesivo","Adecuado","Insuficiente"))

tabla33 <- table(encuesta$Condicion.alcanzada.II,encuesta$Contenidos.II)
tabla33_con_totales <- addmargins(tabla33) # Agregar totales marginales
print(tabla33_con_totales) # Mostrar la tabla
rownames(tabla33_con_totales)[rownames(tabla33_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"

                    #test chi cuadrado

Condicion.alcanzada.II_agrupado<-ifelse(encuesta$Condicion.alcanzada.II %in% c("Libre (rendí las evaluaciones pero me fue mal)"),"No logrado","Logrado")

tabla35_agrupado <- table(Condicion.alcanzada.II_agrupado,encuesta$Contenidos.II)#Condicion.alcanzada.I agrupada
tabla35_agrupado_totales<-addmargins(tabla35_agrupado)#con totales
chi.35<-chisq.test(tabla35_agrupado)
print(chi.35)

# Condicion.alcanzada.III x Contenidos.III

encuesta$Condicion.alcanzada.III <- factor(encuesta$Condicion.alcanzada.III, levels = c("Libre (rendí las evaluaciones pero me fue mal)","Regular","Promocioné")) #CAMBIO EL ORDEN
encuesta$Contenidos.III<-factor(encuesta$Contenidos.III,levels=c("Excesivo","Adecuado","Insuficiente"))

tabla34 <- table(encuesta$Condicion.alcanzada.III,encuesta$Contenidos.III)
tabla34_con_totales <- addmargins(tabla34)# Agregar totales marginales
print(tabla34_con_totales) # Mostrar la tabla
rownames(tabla34_con_totales)[rownames(tabla34_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"

                        #test chi cuadrado

Condicion.alcanzada.III_agrupado<-ifelse(encuesta$Condicion.alcanzada.III %in% c("Libre (rendí las evaluaciones pero me fue mal)"),"No logrado","Logrado")

tabla3.5_agrupado <- table(Condicion.alcanzada.III_agrupado,encuesta$Contenidos.III)#Condicion.alcanzada.I agrupada
tabla3.5_agrupado_sin.ins<-tabla3.5_agrupado[,-3] #elimino la categoria "insuficiente" por baja frecuencia
tabla3.5_agrupado_totales<-addmargins(tabla3.5_agrupado_sin.ins)#con totales
chi.3.5<-chisq.test(tabla3.5_agrupado_sin.ins)
print(chi.3.5)

# Asistencia.consulta.I x Calidad.explicativa.docente.I 

encuesta$Asistencia.consulta.I<-factor(encuesta$Asistencia.consulta.I,levels=c("NUNCA","A VECES","SÍ"))
encuesta$Calidad.explicativa.docente.I<-factor(encuesta$Calidad.explicativa.docente.I,levels = c("Muy claras","Claras","No me resultaron claras"))

tabla35 <- table(encuesta$Asistencia.consulta.I,encuesta$Calidad.explicativa.docente.I)
tabla35_con_totales <- addmargins(tabla35) # Agregar totales marginales
print(tabla35_con_totales) # Mostrar la tabla
colnames(tabla35_con_totales)[colnames(tabla35_con_totales) == "No me resultaron claras"] <- "No claras"

# Asistencia.consulta.II x Calidad.explicativa.docente.II 

encuesta$Asistencia.consulta.II<-factor(encuesta$Asistencia.consulta.II,levels=c("NUNCA","A VECES","SÍ"))
encuesta$Calidad.explicativa.docente.II<-factor(encuesta$Calidad.explicativa.docente.II,levels = c("Muy claras","Claras","No me resultaron claras"))

tabla36 <- table(encuesta$Asistencia.consulta.II,encuesta$Calidad.explicativa.docente.II)
tabla36_con_totales <- addmargins(tabla36) # Agregar totales marginales
print(tabla36_con_totales) # Mostrar la tabla
colnames(tabla36_con_totales)[colnames(tabla36_con_totales) == "No me resultaron claras"] <- "No claras"

                     #test chi cuadrado

Asistencia.consulta.II_agrupado<-ifelse(encuesta$Asistencia.consulta.II %in% c("A VECES","SÍ"),"Asiste","NUNCA")

tabla37_agrupado <- table(Asistencia.consulta.II_agrupado,encuesta$Calidad.explicativa.docente.II)#Condicion.alcanzada.I agrupada
tabla37_agrupado_totales<-addmargins(tabla3.5_agrupado_sin.ins)#con totales
chi.37<-chisq.test(tabla37_agrupado)
print(chi.37)

# Asistencia.consulta.III x Calidad.explicativa.docente.III 

encuesta$Asistencia.consulta.III<-factor(encuesta$Asistencia.consulta.III,levels=c("NUNCA","A VECES","SÍ"))
encuesta$Calidad.explicativa.docente.III<-factor(encuesta$Calidad.explicativa.docente.III,levels = c("Muy claras","Claras","No me resultaron claras"))

tabla37 <- table(encuesta$Asistencia.consulta.III,encuesta$Calidad.explicativa.docente.III)
tabla37_con_totales <- addmargins(tabla37) # Agregar totales marginales
print(tabla37_con_totales) # Mostrar la tabla
colnames(tabla37_con_totales)[colnames(tabla37_con_totales) == "No me resultaron claras"] <- "No claras"

                    #test chi cuadrado

chi.38<-chisq.test(tabla38)
print(chi.38)

# Calidad.explicativa.docente.I x Asistencia.particular.I

encuesta$Calidad.explicativa.docente.I<-factor(encuesta$Calidad.explicativa.docente.I,levels = c("No me resultaron claras","Claras","Muy claras"))
encuesta$Asistencia.particular.I<-factor(encuesta$Asistencia.particular.I,levels=c("SÍ","NO"))

tabla38 <- table(encuesta$Calidad.explicativa.docente.I,encuesta$Asistencia.particular.I)
tabla38_con_totales <- addmargins(tabla38) # Agregar totales marginales
print(tabla38_con_totales) # Mostrar la tabla
rownames(tabla38_con_totales)[rownames(tabla38_con_totales) == "No me resultaron claras"] <- "No claras"

                    #test chi cuadrado

chi.39<-chisq.test(tabla39)
print(chi.39)

# Calidad.explicativa.docente.II x Asistencia.particular.II

encuesta$Calidad.explicativa.docente.II<-factor(encuesta$Calidad.explicativa.docente.II,levels = c("No me resultaron claras","Claras","Muy claras"))
encuesta$Asistencia.particular.II<-factor(encuesta$Asistencia.particular.II,levels=c("SÍ","NO"))

tabla39 <- table(encuesta$Calidad.explicativa.docente.II,encuesta$Asistencia.particular.II)
tabla39_con_totales <- addmargins(tabla39) # Agregar totales marginales
print(tabla39_con_totales) # Mostrar la tabla
rownames(tabla39_con_totales)[rownames(tabla39_con_totales) == "No me resultaron claras"] <- "No claras"

# Calidad.explicativa.docente.III x Asistencia.particular.III

encuesta$Calidad.explicativa.docente.III<-factor(encuesta$Calidad.explicativa.docente.III,levels = c("No me resultaron claras","Claras","Muy claras"))
encuesta$Asistencia.particular.III<-factor(encuesta$Asistencia.particular.III,levels=c("SÍ","NO"))

tabla40 <- table(encuesta$Calidad.explicativa.docente.III,encuesta$Asistencia.particular.III)
tabla40_con_totales <- addmargins(tabla40) # Agregar totales marginales
print(tabla40_con_totales) # Mostrar la tabla
rownames(tabla40_con_totales)[rownames(tabla40_con_totales) == "No me resultaron claras"] <- "No claras"

                     #test chi cuadrado

chi.41<-chisq.test(tabla41)
print(chi.41)

# Condicion.alcanzada.I x Rendimiento.consulta.I

encuesta$Condicion.alcanzada.I <- factor(encuesta$Condicion.alcanzada.I, levels = c("Libre (rendí las evaluaciones pero me fue mal)","Promocioné" ,"Regular" )) #CAMBIO EL ORDEN
encuesta$Rendimiento.consulta.I<-factor(encuesta$Rendimiento.consulta.I,levels=c("Muy útiles","Útiles","Poco útiles"))

tabla41<- table(encuesta$Condicion.alcanzada.I,encuesta$Rendimiento.consulta.I)
tabla41_con_totales <- addmargins(tabla41) # Agregar totales marginales
print(tabla41_con_totales) # Mostrar la tabla
rownames(tabla41_con_totales)[rownames(tabla41_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"

# Condicion.alcanzada.II x Rendimiento.consulta.II

encuesta$Condicion.alcanzada.II <- factor(encuesta$Condicion.alcanzada.II, levels = c("Libre (rendí las evaluaciones pero me fue mal)","Promocioné" ,"Regular" )) #CAMBIO EL ORDEN
encuesta$Rendimiento.consulta.II<-factor(encuesta$Rendimiento.consulta.II,levels=c("Muy útiles","Útiles","Poco útiles"))

tabla42 <- table(encuesta$Condicion.alcanzada.II,encuesta$Rendimiento.consulta.II)
tabla42_con_totales <- addmargins(tabla42) # Agregar totales marginales
print(tabla42_con_totales) # Mostrar la tabla
rownames(tabla42_con_totales)[rownames(tabla42_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"

# Condicion.alcanzada.III x Rendimiento.consulta.III

encuesta$Condicion.alcanzada.III <- factor(encuesta$Condicion.alcanzada.III, levels = c("Libre (rendí las evaluaciones pero me fue mal)","Promocioné" ,"Regular" )) #CAMBIO EL ORDEN
encuesta$Rendimiento.consulta.III<-factor(encuesta$Rendimiento.consulta.III,levels=c("Muy útiles","Útiles","Poco útiles"))

tabla43 <- table(encuesta$Condicion.alcanzada.III,encuesta$Rendimiento.consulta.III)
tabla43_con_totales <- addmargins(tabla43) # Agregar totales marginales
print(tabla43_con_totales) # Mostrar la tabla
rownames(tabla43_con_totales)[rownames(tabla43_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"


# Consignas.final.I x Condicion.alcanzada.I

encuesta$Condicion.alcanzada.I <- factor(encuesta$Condicion.alcanzada.I, levels = c("Regular","Libre (rendí las evaluaciones pero me fue mal)")) #CAMBIO EL ORDEN
encuesta$Consignas.final.I<-factor(encuesta$Consignas.final.I,levels=c("Poco claras","Claras","Muy claras"))

tabla44 <- table(encuesta$Consignas.final.I,encuesta$Condicion.alcanzada.I)
tabla44_con_totales <- addmargins(tabla44) # Agregar totales marginales
colnames(tabla44_con_totales)[colnames(tabla44_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
print(tabla44_con_totales) # Mostrar la tabla



# Consignas.final.II x Condicion.alcanzada.II

encuesta$Condicion.alcanzada.II <- factor(encuesta$Condicion.alcanzada.II, levels = c("Regular" ,"Libre (rendí las evaluaciones pero me fue mal)")) #CAMBIO EL ORDEN
encuesta$Consignas.final.II<-factor(encuesta$Consignas.final.II,levels=c("Poco claras","Claras","Muy claras"))

tabla45 <- table(encuesta$Consignas.final.II,encuesta$Condicion.alcanzada.II)
tabla45_con_totales <- addmargins(tabla45) # Agregar totales marginales
colnames(tabla45_con_totales)[colnames(tabla45_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
print(tabla45_con_totales) # Mostrar la tabla



# Consignas.final.III x Condicion.alcanzada.III

encuesta$Condicion.alcanzada.III <- factor(encuesta$Condicion.alcanzada.III, levels = c("Regular" ,"Libre (rendí las evaluaciones pero me fue mal)")) #CAMBIO EL ORDEN
encuesta$Consignas.final.III<-factor(encuesta$Consignas.final.III,levels=c("Poco claras","Claras","Muy claras"))

tabla46 <- table(encuesta$Consignas.final.III,encuesta$Condicion.alcanzada.III)
tabla46_con_totales <- addmargins(tabla46) # Agregar totales marginales
colnames(tabla46_con_totales)[colnames(tabla46_con_totales) == "Libre (rendí las evaluaciones pero me fue mal)"] <- "Libre"
print(tabla46_con_totales) # Mostrar la tabla

