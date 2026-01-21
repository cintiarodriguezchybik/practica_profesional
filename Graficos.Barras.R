#Busco la ruta del archivo de excel
file.choose()
ruta_archivo<-"C:\\Users\\PC\\Downloads\\Encuesta Plan 2023 (Respuestas)(1).xlsx"

#Leo el archivo de Excel
install.packages("readxl")

library(readxl)
encuesta<-read_excel("C:\\Users\\PC\\Downloads\\Encuesta Plan 2023 (Respuestas)(1).xlsx")
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

#####Ingreso
tabla.ordenada1<-factor(encuesta$Ingreso,levels=c("2024","2023","2022"))#ordeno las categorias como aparecen en la encuesta
table1<-table(tabla.ordenada1) # Frecuencias absolutas
prop.table(table(table1)) # Frecuencias relativas
porcentajes1<-round(prop.table(table1)*100,digits=1) #Porcentajes

frame1<- data.frame("Año de ingreso" = names(table1),
                    "Frecuencia" = as.numeric(table1),
                    "Porcentaje" = gsub("\\.", ",", 
                                        sprintf("%.1f",round(as.numeric(porcentajes1),digits =  1)))) #tabla de frec.absolutas y porcentajes con comas

ggplot_1<-
ggplot(frame1, aes(x =Año.de.ingreso, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
   ylim(0,100)+
  geom_text(aes(label =paste0(frame1$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame1$Porcentaje%in% c("72,8", "23,9"),1.2,-0.2),fontface="bold")+ 
  labs(x = "Año",
       y = "Porcentaje de estudiantes")
       

#####Lugar.residencia
tabla.ordenada2<-factor(encuesta$Lugar.residencia,levels=c("Zavalla","Rosario","Albarellos","Carcaraña","Funes","Roldan","Las parejas","Villa Gobernador Galvez")) 
tabla2<-table(tabla.ordenada2) # Frecuencias absolutas
prop.table(tabla2)# Frecuencias relativas
porcentajes2<-round(prop.table(tabla2)*100,1) #Porcentajes

frame2<- data.frame("Lugar de residencia" = names(tabla2),
                    "Frecuencia" = as.numeric(tabla2),
                    "Porcentaje" = gsub("\\.", ",", 
                                        sprintf("%.1f",round(as.numeric(porcentajes2), 1)))) #tabla de frec.absolutas y porcentajes

ggplot_2<-
ggplot(frame2, aes(x =Lugar.de.residencia, y =as.numeric(gsub(",", ".", Porcentaje)))) +
       geom_bar(stat = "identity", fill = "steelblue",color="black") +
       ylim(0,100)+
       geom_text(aes(label =paste0(frame2$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame2$Porcentaje%in% c("51,1","41,3"),1.2,-0.2),fontface="bold")+
       scale_x_discrete(limits = levels(tabla.ordenada2),labels=c("Villa Gobernador Galvez"="VGG"))+
       labs(x = "Localidad",
                y = "Porcentaje de estudiantes") 


#####Personas.con.las.que.vive
tabla.ordenada3<-factor(encuesta$Personas.con.las.que.vive,levels=c("Sólo/a","Con mi familia","Con amigos/as"))
tabla3<-table(tabla.ordenada3) # Frecuencias absolutas
prop.table(tabla3) # Frecuencias relativas
porcentajes3<-round(prop.table(tabla3)*100,1) #Porcentajes

frame3<- data.frame("Personas con las que vive" = names(tabla3),
                    "Frecuencia" = as.numeric(tabla3),
                    "Porcentaje" = gsub("\\.", ",", 
                                        sprintf("%.1f",round(as.numeric(porcentajes3), 1)) ))#tabla de frec.absolutas y porcentaje

ggplot_3<-
ggplot(frame3, aes(x =Personas.con.las.que.vive, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  scale_x_discrete(limits = levels(tabla.ordenada3))+
  ylim(0,100)+
  geom_text(aes(label =paste0(frame3$Porcentaje,"%\n(",Frecuencia,")")),vjust = 1.2,fontface="bold")+
  labs(x = "Personas con las que vive",
       y = "Porcentaje de estudiantes") 

####Tiempo.traslado
tabla.ordenada4<-factor(encuesta$Tiempo.traslado,levels=c("Menos de 1 h.","Entre 1 y 2 hs.","Entre 2 y 3 hs.","Más de 3 hs."))
tabla4<-table(tabla.ordenada4) # Frecuencias absolutas
prop.table(tabla4) # Frecuencias relativas
porcentajes4<-round(prop.table(tabla4)*100,1) #Porcentajes

frame4<- data.frame("Tiempo de traslado" = names(tabla4),
                    "Frecuencia" = as.numeric(tabla4),
                    "Porcentaje" = gsub("\\.", ",", 
                                        sprintf("%.1f",round(as.numeric(porcentajes4), 1)))) #tabla de frec.absolutas y porcentajes

frame4$Porcentaje <- gsub(",0$", "", frame4$Porcentaje) #elimino el decimal en 17,0%

ggplot_4<-
ggplot(frame4, aes(x =Tiempo.de.traslado, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  scale_x_discrete(limits = levels(tabla.ordenada4))+
  ylim(0,100)+
  geom_text(aes(label =paste0(frame4$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame4$Porcentaje%in% c("53,4","26,1","17"),1.1,-0.2),fontface="bold")+
  labs(x = "Tiempo",
       y = "Porcentaje de estudiantes") 


#Tiene.trabajo
tabla.ordenada5<-factor(encuesta$Tiene.trabajo,levels=c("SÍ","NO"))#ordeno las categorias como aparecen en la encuesta
table5<-table(tabla.ordenada5) # Frecuencias absolutas
prop.table(table(table5)) # Frecuencias relativas
porcentajes5<-round(prop.table(table5)*100,digits=1) #Porcentajes

frame5<- data.frame("Tiene trabajo" = names(table5),
                    "Frecuencia" = as.numeric(table5),
                    "Porcentaje" = gsub("\\.", ",", 
                                        sprintf("%.1f",round(as.numeric(porcentajes5),digits =  1)))) #tabla de frec.absolutas y porcentajes con comas

ggplot_5<-
ggplot(frame5, aes(x =Tiene.trabajo, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  scale_x_discrete(limits = levels(tabla.ordenada5))+
  ylim(0,100)+
  geom_text(aes(label =paste0(frame5$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.1,fontface="bold")+
  labs(x = "¿Trabajan?",
       y = "Porcentaje de estudiantes") 

#Horas.semanales.de.trabajo
table6<-table(encuesta$Horas.semanales.de.trabajo) # Frecuencias absolutas
prop.table(table6) # Frecuencias relativas
porcentajes6<-round(prop.table(table6)*100,1) #Porcentajes

frame6<- data.frame("Horas de trabajo" = names(table6),
                    "Frecuencia" = as.numeric(table6),
                    "Porcentaje" = gsub("\\.", ",", 
                                        sprintf("%.1f",round(as.numeric(porcentajes6), 1)))) #tabla de frec.absolutas y porcentajes

ggplot_6<-
ggplot(frame6, aes(x =Horas.de.trabajo, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  ylim(0,100)+
  geom_text(aes(label =paste0(frame6$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.1,fontface="bold")+
  labs(x = "Horas",
       y = "Porcentaje de estudiantes") 

##################MATEMATICA##################################################################################

#Condicion.cursado.I
tabla.ordenada7<-factor(encuesta$Condicion.cursado.I,levels=c("Ingresante","Cambio de plan","Recursante"))
table7<-table(tabla.ordenada7) # Frecuencias absolutas
prop.table(table7) # Frecuencias relativas
porcentajes7<-round(prop.table(table7)*100,1) #Porcentajes

frame7<- data.frame("Condicion de cursado" = names(table7),
                    "Frecuencia" = as.numeric(table7),
                    "Porcentaje" = gsub("\\.", ",", 
                                        sprintf("%.1f",round(as.numeric(porcentajes7), 1)))) #tabla de frec.absolutas y porcentajes

G1.1<-ggplot(frame7, aes(x =Condicion.de.cursado, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  ylim(0,100)+
  geom_text(aes(label =paste0(frame7$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame7$Porcentaje%in% "73,9",1.1,-0.2),size=2.5,fontface="bold")+
  scale_x_discrete(limits = levels(tabla.ordenada7),labels=c("Ingresante"="Ingresante","Cambio de plan"="Cambio\n de plan","Recursante"="Recursante"))+
  labs(title = "Matemática I",
       x = "  ",
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Matemática I
  theme(axis.text.x = element_text(size = 6,angle = 45,face="bold"))


#Condicion.alcanzada.I

tabla.ordenada8<-factor(encuesta$Condicion.alcanzada.I,levels=c("Regular","Promocioné","Libre (rendí las evaluaciones pero me fue mal)","Abandoné","Me anoté, pero no empecé a cursar."))
table8<-table(tabla.ordenada8) # Frecuencias absolutas
prop.table(table(table8)) # Frecuencias relativas
porcentajes8<-round(prop.table(table8)*100,1) #Porcentajes

frame8<- data.frame("Condicion alcanzada I" = names(table8),
                    "Frecuencia" = as.numeric(table8),
                    "Porcentaje" = gsub("\\.", ",", 
                                        sprintf("%.1f",round(as.numeric(porcentajes8), 1)))) #tabla de frec.absolutas y porcentajes
frame8$Porcentaje <- gsub(",0$", "", frame8$Porcentaje) #elimino el decimal en 0,0%


G2.1<-ggplot(frame8, aes(x =Condicion.alcanzada.I, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada8),labels=c("Regular"="Regular","Promocioné"="Promocionó","Libre (rendí las evaluaciones pero me fue mal)"="Libre","Abandoné"="Abandonó","Me anoté, pero no empecé a cursar."="No cursó"))+
  geom_text(aes(label =paste0(frame8$Porcentaje,"%\n(",Frecuencia,")")),size=2.5,fontface="bold",vjust=ifelse(frame8$Porcentaje%in% 1,1.2,-0.2))+
  labs(title = "Matemática I",
       x = " ",
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Matemática I
  theme(axis.text.x = element_text(size =6,angle = 45,face="bold"))


#ContenidosI
tabla.ordenada9<-factor(encuesta$Contenidos.I,levels=c("Excesivo","Adecuado","Insuficiente")) #ordeno categorías como aparecen en la encuesta
table9<-table(tabla.ordenada9) # Frecuencias absolutas
table9
prop.table(table(table9)) # Frecuencias relativas
porcentajes9<-round(prop.table(table9)*100,1) #Porcentajes

frame9<- data.frame("ContenidosI" = names(table9),
                    "Frecuencia" = as.numeric(table9),
                    "Porcentaje" =gsub("\\.", ",", 
                                       sprintf("%.1f", round(as.numeric(porcentajes9), 1)))) #tabla de frec.absolutas y porcentajes
frame9$Porcentaje <- gsub(",0$", "", frame9$Porcentaje) #elimino el decimal en 10,0%

G3.1<-ggplot(frame9, aes(x =ContenidosI, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  ylim(0,100)+
  geom_text(aes(label =paste0(frame9$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame9$Porcentaje%in% "86,7",1.2,-0.2),size=2.5,fontface="bold")+
  scale_x_discrete(limits = levels(tabla.ordenada9))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Matemática I",
       x = "  ",
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo matematica
  theme(axis.text.x = element_text(size = 6,angle = 45,face="bold"))


#Calidad.explicativa.docente.I
tabla.ordenada10<-factor(encuesta$Calidad.explicativa.docente.I,levels=c("Muy claras","Claras","No me resultaron claras"))
table10<-table(tabla.ordenada10) # Frecuencias absolutas
prop.table(table(table10)) # Frecuencias relativas
porcentajes10<-round(prop.table(table10)*100,1) #Porcentajes

frame10<- data.frame("Calidad explicativaI" = names(table10),
                    "Frecuencia" = as.numeric(table10),
                    "Porcentaje" = gsub("\\.", ",", 
                                        sprintf("%.1f",round(as.numeric(porcentajes10), 1)))) #tabla de frec.absolutas y porcentajes

G4.1<-ggplot(frame10, aes(x =Calidad.explicativaI, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  ylim(0,100)+
  geom_text(aes(label =paste0(frame10$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame10$Porcentaje%in% c("22,5","66,3"),1.2,-0.2),size=2.5,fontface="bold")+
  scale_x_discrete(limits = levels(tabla.ordenada10),labels=c("Muy claras"="Muy\n claras","Claras"="Claras","No me resultaron claras"="No\n claras"))+
  labs(title = "Matemática I ",
       x = " ",
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo matem.
  theme(axis.text.x = element_text(size = 6,face="bold"))



#Recusrsos.utilizados.I
tabla.ordenada11<-factor(encuesta$Recursos.utilizados.I,levels=c("Muy adecuados","Adecuados","Poco adecuados"))
table11<-table(tabla.ordenada11) # Frecuencias absolutas
prop.table(table(table11)) # Frecuencias relativas
porcentajes11<-round(prop.table(table11)*100,1) #Porcentajes

frame11<- data.frame("Recursos utilizados.I" = names(table11),
                     "Frecuencia" = as.numeric(table11),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes11), 1)))) #tabla de frec.absolutas y porcentajes

G5.1<-ggplot(frame11, aes(x =Recursos.utilizados.I, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame11$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame11$Porcentaje%in% c("32,2","61,1"),1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada11),labels=c("Muy adecuados"="Muy\n adecuados","Adecuados"="Adecuados","Poco adecuados"="Poco\n adecuados"))+
  labs(title = "Matemática I ",
       x = " ",
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Matemática
  theme(axis.text.x = element_text(size = 6,angle = 45,face="bold"))



#Contenidos.clase/parcial.I
tabla.ordenada12<-factor(encuesta$`Contenidos.clase/parcial.I`,levels=c("Si, los vimos varias veces","Algunos temas los vimos poco en las clases.","Algunos temas no los vimos en las clases.","No,el examen parcial fue muy distinto a lo dado en clase"))
table12<-table(tabla.ordenada12) # Frecuencias absolutas
prop.table(table(table12)) # Frecuencias relativas
porcentajes12<-round(prop.table(table12)*100,1) #Porcentajes

frame12<- data.frame("Contenidos clase/parcial I" = names(table12),
                     "Frecuencia" = as.numeric(table12),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes12), 1)) ))#tabla de frec.absolutas y porcentajes
frame12$Porcentaje <- gsub(",0$", "", frame12$Porcentaje) #elimino el decimal en 17,0%

G6.1<-ggplot(frame12, aes(x =Contenidos.clase.parcial.I, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame12$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame12$Porcentaje%in% 1,1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada12),labels=c("Si, los vimos varias veces"="SI","Algunos temas los vimos poco en las clases."="SI,\n dados\n poco","Algunos temas no los vimos en las clases."="algunos\n no","No,el examen parcial fue muy distinto a lo dado en clase"="NO"))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Matemática I",
       x = " ",
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo matematica
  theme(axis.text.x = element_text(size = 6,face="bold"))


#Consignas.parcial.I
tabla.ordenada13<-factor(encuesta$Consignas.parcial.I,levels=c("Muy claras","Claras","Poco claras"))
table13<-table(tabla.ordenada13) # Frecuencias absolutas
prop.table(table(table13)) # Frecuencias relativas
porcentajes13<-round(prop.table(table13)*100,1) #Porcentajes

frame13<- data.frame("Consignas parcial I" = names(table13),
                     "Frecuencia" = as.numeric(table13),
                     "Porcentaje" =gsub("\\.", ",", 
                                        sprintf("%.1f", round(as.numeric(porcentajes13), 1)))) #tabla de frec.absolutas y porcentajes

G7.1<-ggplot(frame13, aes(x =Consignas.parcial.I, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame13$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame13$Porcentaje%in% c("22,2","72,2"),1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada13),labels=c("Muy claras"="Muy\n claras","Claras"="Claras","Poco claras"="Poco\n claras"))+
  labs(title = "Matemática I",
       x = " ",
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo matematica
  theme(axis.text.x = element_text(size = 6,face="bold"))


#Tiempo.examen.I
tabla.ordenada14<-factor(encuesta$Tiempo.examen.I,levels=c("Adecuado","Insuficiente"))
table14<-table(tabla.ordenada14) # Frecuencias absolutas
prop.table(table(table14)) # Frecuencias relativas
porcentajes14<-round(prop.table(table14)*100,1) #Porcentajes

frame14<- data.frame("Tiempo examen I" = names(table14),
                     "Frecuencia" = as.numeric(table14),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes14), 1)))) #tabla de frec.absolutas y porcentajes

G8.1<-ggplot(frame14, aes(x =Tiempo.examen.I, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame14$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame14$Porcentaje%in% "91,1",1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada14))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Matemática I",
       x = " ",
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo matematica
  theme(axis.text.x = element_text(size = 6,face="bold"))



#Asistencia.consulta.I
tabla.ordenada15<-factor(encuesta$Asistencia.consulta.I,levels=c("SÍ","A VECES","NUNCA"))
table15<-table(tabla.ordenada15) # Frecuencias absolutas
prop.table(table(table15)) # Frecuencias relativas
porcentajes15<-round(prop.table(table15)*100,1) #Porcentajes

frame15<- data.frame("Asistencia consulta I" = names(table15),
                     "Frecuencia" = as.numeric(table15),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes15), 1)))) #tabla de frec.absolutas y porcentajes

G9.1<-ggplot(frame15, aes(x =Asistencia.consulta.I, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame15$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.2,size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada15))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Matemática I ",
       x = " ",
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo matematica
  theme(axis.text.x = element_text(size = 6,face="bold"))

#Rendimiento.consulta.I
tabla.ordenada16<-factor(encuesta$Rendimiento.consulta.I,levels=c("Muy útiles","Útiles","Poco útiles"))
table16<-table(tabla.ordenada16) # Frecuencias absolutas
prop.table(table(table16)) # Frecuencias relativas
porcentajes16<-round(prop.table(table16)*100,1) #Porcentajes

frame16<- data.frame("Rendimiento consulta I" = names(table16),
                     "Frecuencia" = as.numeric(table16),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes16), 1)))) #tabla de frec.absolutas y porcentajes


G10.1<-ggplot(frame16, aes(x =Rendimiento.consulta.I, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame16$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame16$Porcentaje%in% c("36,2","56,5"),1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada16),labels=c("Muy útiles"="Muy\n útiles","Útiles"="Útiles","Poco útiles"="Poco\n útiles"))+
  labs(title = "Matemática I",
       x = " ",
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo matematica
  theme(axis.text.x = element_text(size = 6,face="bold"))

#Asistencia.particular.I
tabla.ordenada17<-factor(encuesta$Asistencia.particular.I,levels=c("SÍ","NO"))
table17<-table(tabla.ordenada17) # Frecuencias absolutas
prop.table(table(table17)) # Frecuencias relativas
porcentajes17<-round(prop.table(table17)*100,1) #Porcentajes

frame17<- data.frame("Asistencia particular I" = names(table17),
                     "Frecuencia" = as.numeric(table17),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes17), 1)))) #tabla de frec.absolutas y porcentajes
frame17$Porcentaje <- gsub(",0$", "", frame17$Porcentaje) #elimino el decimal en 44,0%

G11.1<-ggplot(frame17, aes(x =Asistencia.particular.I, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame17$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame17$Porcentaje%in% c(44,56),1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada17))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Matemática I",
       x = " ",
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo matematica
  theme(axis.text.x = element_text(size = 6,face="bold"))

#Examen.final.I
tabla.ordenada18<-factor(encuesta$Examen.final.I,levels=c("SÍ","NO, porque Promocioné","NO"))
table18<-table(tabla.ordenada18) # Frecuencias absolutas
prop.table(table(table18)) # Frecuencias relativas
porcentajes18<-round(prop.table(table18)*100,1) #Porcentajes

frame18<- data.frame("Examen final I" = names(table18),
                     "Frecuencia" = as.numeric(table18),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes18), 1))) )#tabla de frec.absolutas y porcentajes
frame18$Porcentaje <- gsub(",0$", "", frame18$Porcentaje) #elimino el decimal en 30,0%

G12.1<-ggplot(frame18, aes(x =Examen.final.I, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame18$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.2,size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada18),labels=c("SÍ"="Si","NO, porque Promocioné"="No\n(promocionó)","NO"="No"))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Matemática I ",
       x = " ",
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo matematica
  theme(axis.text.x = element_text(size = 6,face="bold"))

#Consignas.final.I
tabla.ordenada19<-factor(encuesta$Consignas.final.I,levels=c("Muy claras","Claras","Poco claras"))
table19<-table(tabla.ordenada19) # Frecuencias absolutas
prop.table(table(table19)) # Frecuencias relativas
porcentajes19<-round(prop.table(table19)*100,1) #Porcentajes

frame19<- data.frame("Consignas final I" = names(table19),
                     "Frecuencia" = as.numeric(table19),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes19), 1)))) #tabla de frec.absolutas y porcentajes
frame19$Porcentaje <- gsub(",0$", "", frame19$Porcentaje) #elimino el decimal en 20,0%

G13.1<-ggplot(frame19, aes(x =Consignas.final.I, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame19$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.2,size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada19),labels=c("Muy claras"="Muy\n claras","Claras"="Claras","Poco claras"="Poco\n claras"))+
  labs(title = "Matemática I",
       x = " ",
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo matematica
  theme(axis.text.x = element_text(size = 6,face="bold"))


#Tiempo.examen.final.I
tabla.ordenada20<-factor(encuesta$Tiempo.examen.final.I,levels=c("Adecuado","Insuficiente"))
table20<-table(tabla.ordenada20) # Frecuencias absolutas
prop.table(table(table20)) # Frecuencias relativas
porcentajes20<-round(prop.table(table20)*100,1) #Porcentajes

frame20<- data.frame("Tiempo examen final I" = names(table20),
                     "Frecuencia" = as.numeric(table20),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes20), 1)))) #tabla de frec.absolutas y porcentajes
frame20$Porcentaje <- gsub(",0$", "", frame20$Porcentaje) #elimino el decimal en 90,0%

G14.1<-ggplot(frame20, aes(x =Tiempo.examen.final.I, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame20$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame20$Porcentaje%in% "90",1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada20))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Matemática I",
       x = " ", 
       y = "Porcentaje de estudiantes")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo matematica
  theme(axis.text.x = element_text(size = 6,face="bold"))



##################Biología##################################################################################

#Condicion.cursado.II
tabla.ordenada21<-factor(encuesta$Condicion.cursado.II,levels=c("Ingresante","Cambio de plan","Recursante"))
table21<-table(tabla.ordenada21) # Frecuencias absolutas
prop.table(table21) # Frecuencias relativas
porcentajes21<-round(prop.table(table21)*100,1) #Porcentajes

frame21<- data.frame("Condicion de cursado II" = names(table21),
                    "Frecuencia" = as.numeric(table21),
                    "Porcentaje" = gsub("\\.", ",", 
                                        sprintf("%.1f",round(as.numeric(porcentajes21), 1)))) #tabla de frec.absolutas y porcentajes
frame21$Porcentaje <- gsub(",0$", "", frame21$Porcentaje) #elimino el decimal en 11,0%

G1.2<-ggplot(frame21, aes(x =Condicion.de.cursado.II, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  scale_x_discrete(limits = levels(tabla.ordenada21),labels=c("Ingresante"="Ingresante","Cambio de plan"="Cambio\n de plan","Recursante"="Recursante"))+
  ylim(0,100)+
  geom_text(aes(label =paste0(frame21$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame21$Porcentaje%in% "81,3",1.2,-0.2),size = 2.5,fontface="bold")+
  labs(title = "Biología",
       x = "Condición cursado",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+
  theme(axis.text.x = element_text(size = 6,angle=45,face="bold"))



#Condicion.alcanzada.II

tabla.ordenada22<-factor(encuesta$Condicion.alcanzada.II,levels=c("Regular","Promocioné","Libre (rendí las evaluaciones pero me fue mal)","Abandoné","Me anoté, pero no empecé a cursar."))
table22<-table(tabla.ordenada22) # Frecuencias absolutas
prop.table(table(table22)) # Frecuencias relativas
porcentajes22<-round(prop.table(table22)*100,1) #Porcentajes

frame22<- data.frame("Condicion alcanzada II" = names(table22),
                    "Frecuencia" = as.numeric(table22),
                    "Porcentaje" = gsub("\\.", ",", 
                                        sprintf("%.1f",round(as.numeric(porcentajes22), 1)))) #tabla de frec.absolutas y porcentajes
frame22$Porcentaje <- gsub(",0$", "", frame22$Porcentaje) #elimino el decimal en 0,0%

G2.2<-ggplot(frame22, aes(x =Condicion.alcanzada.II, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame22$Porcentaje,"%\n(",Frecuencia,")")),size=2.5,fontface="bold",vjust=ifelse(frame22$Porcentaje%in% 1,1.2,-0.2))+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada22),labels=c("Regular"="Regular","Promocioné"="Promocionó","Libre (rendí las evaluaciones pero me fue mal)"="Libre","Abandoné"="Abandonó","Me anoté, pero no empecé a cursar."="No cursó"))+
  labs(title = "Biología ",
       x = "Condición alcanzada",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Biologia
  theme(axis.text.x = element_text(size = 6,angle = 45,face="bold"))



#ContenidosII
tabla.ordenada23<-factor(encuesta$Contenidos.II,levels=c("Excesivo","Adecuado","Insuficiente")) #ordeno categorías como aparecen en la encuesta
table23<-table(tabla.ordenada23) # Frecuencias absolutas
prop.table(table(table23)) # Frecuencias relativas
porcentajes23<-round(prop.table(table23)*100,1) #Porcentajes


frame23<- data.frame("ContenidosII" = names(table23),
                    "Frecuencia" = as.numeric(table23),
                    "Porcentaje" = gsub("\\.", ",", 
                                        sprintf("%.1f",round(as.numeric(porcentajes23), 1)))) #tabla de frec.absolutas y porcentajes

G3.2<-ggplot(frame23, aes(x =ContenidosII, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame23$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame23$Porcentaje%in% c("22,2","68,9"),1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada23))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Biología ",
       x = "Cantidad de contenidos",
       y = "  ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo biologia
  theme(axis.text.x = element_text(size = 6,angle = 45,face="bold"))


#Calidad.explicativa.docente.II
tabla.ordenada24<-factor(encuesta$Calidad.explicativa.docente.II,levels=c("Muy claras","Claras","No me resultaron claras"))
table24<-table(tabla.ordenada24) # Frecuencias absolutas
prop.table(table(table24)) # Frecuencias relativas
porcentajes24<-round(prop.table(table24)*100,1) #Porcentajes

frame24<- data.frame("Calidad explicativaII" = names(table24),
                     "Frecuencia" = as.numeric(table24),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes24), 1)))) #tabla de frec.absolutas y porcentajes

G4.2<-ggplot(frame24, aes(x =Calidad.explicativaII, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame24$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame24$Porcentaje%in% c("53,3","34,4"),1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada24),labels=c("Muy claras"="Muy\n claras","Claras"="Claras","No me resultaron claras"="No\n claras"))+
  labs(title = "Biología ",
       x = "Explicaciones",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo biologia
  theme(axis.text.x = element_text(size = 6,face="bold"))


#Recusrsos.utilizados.II
tabla.ordenada25<-factor(encuesta$Recursos.utilizados.II,levels=c("Muy adecuados","Adecuados","Poco adecuados"))
table25<-table(tabla.ordenada25) # Frecuencias absolutas
prop.table(table(table25)) # Frecuencias relativas
porcentajes25<-round(prop.table(table25)*100,1) #Porcentajes

frame25<- data.frame("Recursos utilizados.II" = names(table25),
                     "Frecuencia" = as.numeric(table25),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes25), 1)))) #tabla de frec.absolutas y porcentajes

G5.2<-ggplot(frame25, aes(x =Recursos.utilizados.II, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame25$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.2,size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada11),labels=c("Muy adecuados"="Muy\n adecuados","Adecuados"="Adecuados","Poco adecuados"="Poco\n adecuados"))+
  labs(title = "Biología ",
       x = "Recursos",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo biologia
  theme(axis.text.x = element_text(size = 6,angle = 45,face="bold"))



#Contenidos.clase/parcial.II
tabla.ordenada26<-factor(encuesta$`Contenidos.clase/parcial.II`,levels=c("Si, los vimos varias veces","Algunos temas los vimos poco en las clases.","Algunos temas no los vimos en las clases.","No,el examen parcial fue muy distinto a lo dado en clase"))
table26<-table(tabla.ordenada26) # Frecuencias absolutas
prop.table(table(table26)) # Frecuencias relativas
porcentajes26<-round(prop.table(table26)*100,1) #Porcentajes

frame26<- data.frame("Contenidos clase/parcial II" = names(table26),
                     "Frecuencia" = as.numeric(table26),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes26), 1)))) #tabla de frec.absolutas y porcentajes
frame26$Porcentaje <- gsub(",0$", "", frame26$Porcentaje) #elimino el decimal en 0,0%

G6.2<-ggplot(frame26, aes(x =Contenidos.clase.parcial.II, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame26$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame26$Porcentaje%in% 1,1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada26),labels=c("Si, los vimos varias veces"="SI","Algunos temas los vimos poco en las clases."="SI,\n dados\n poco","Algunos temas no los vimos en las clases."="algunos\n no","No,el examen parcial fue muy distinto a lo dado en clase"="NO"))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Biología",
       x = "¿Mismos contenidos?",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo biologia
  theme(axis.text.x = element_text(size = 6,face="bold")) 




#Consignas.parcial.II
tabla.ordenada27<-factor(encuesta$Consignas.parcial.II,levels=c("Muy claras","Claras","Poco claras"))
table27<-table(tabla.ordenada27) # Frecuencias absolutas
prop.table(table(table27)) # Frecuencias relativas
porcentajes27<-round(prop.table(table27)*100,1) #Porcentajes

frame27<- data.frame("Consignas parcial II" = names(table27),
                  "Frecuencia" = as.numeric(table27),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes27), 1)))) #tabla de frec.absolutas y porcentajes

G7.2<-ggplot(frame27, aes(x =Consignas.parcial.II, y =as.numeric(gsub(",", ".", Porcentaje))))+
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame27$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame27$Porcentaje%in% c("23,3","64,4"),1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada27),labels=c("Muy claras"="Muy\n claras","Claras"="Claras","Poco claras"="Poco\n claras"))+
  labs(title = "Biología",
       x = "Consignas",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo biologia
  theme(axis.text.x = element_text(size = 6,face="bold"))

#Tiempo.examen.II
tabla.ordenada28<-factor(encuesta$Tiempo.examen.II,levels=c("Adecuado","Insuficiente"))
table28<-table(tabla.ordenada28) # Frecuencias absolutas
prop.table(table(table28)) # Frecuencias relativas
porcentajes28<-round(prop.table(table28)*100,1) #Porcentajes

frame28<- data.frame("Tiempo examen II" = names(table28),
                     "Frecuencia" = as.numeric(table28),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes28), 1)))) #tabla de frec.absolutas y porcentajes

G8.2<-ggplot(frame28, aes(x =Tiempo.examen.II, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame28$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame28$Porcentaje%in% "94,4",1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada28))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Biología",
       x = "Tiempo",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo biologia
  theme(axis.text.x = element_text(size = 6,face="bold"))



#Asistencia.consulta.II
tabla.ordenada29<-factor(encuesta$Asistencia.consulta.II,levels=c("SÍ","A VECES","NUNCA"))
table29<-table(tabla.ordenada29) # Frecuencias absolutas
prop.table(table(table29)) # Frecuencias relativas
porcentajes29<-round(prop.table(table29)*100,1) #Porcentajes

frame29<- data.frame("Asistencia consulta II" = names(table29),
                     "Frecuencia" = as.numeric(table29),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes29), 1)))) #tabla de frec.absolutas y porcentajes

G9.2<-ggplot(frame29, aes(x =Asistencia.consulta.II, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame29$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.2,size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada29))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Biología ",
       x = "Asistencia",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo biologia
  theme(axis.text.x = element_text(size = 6,face="bold"))

#Rendimiento.consulta.II
tabla.ordenada30<-factor(encuesta$Rendimiento.consulta.II,levels=c("Muy útiles","Útiles","Poco útiles"))
table30<-table(tabla.ordenada30) # Frecuencias absolutas
prop.table(table(table30)) # Frecuencias relativas
porcentajes30<-round(prop.table(table30)*100,1) #Porcentajes

frame30<- data.frame("Rendimiento consulta II" = names(table30),
                     "Frecuencia" = as.numeric(table30),
                    "Porcentaje" = gsub("\\.", ",", 
                                        sprintf("%.1f",round(as.numeric(porcentajes30), 1)))) #tabla de frec.absolutas y porcentajes

G10.2<-ggplot(frame30, aes(x =Rendimiento.consulta.II, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame30$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame30$Porcentaje%in% c("52,1","35,4"),1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada30),labels=c("Muy útiles"="Muy\n útiles","Útiles"="Útiles","Poco útiles"="Poco\n útiles"))+
  labs(title = "Biología ",
       x = "Rendimiento",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo biologia
  theme(axis.text.x = element_text(size = 6,face="bold"))



#Asistencia.particular.II
tabla.ordenada31<-factor(encuesta$Asistencia.particular.II,levels=c("SÍ","NO"))
table31<-table(tabla.ordenada31) # Frecuencias absolutas
prop.table(table(table31)) # Frecuencias relativas
porcentajes31<-round(prop.table(table31)*100,1) #Porcentajes

frame31<- data.frame("Asistencia particular II" = names(table31),
                     "Frecuencia" = as.numeric(table31),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes31), 1)))) #tabla de frec.absolutas y porcentajes

G11.2<-ggplot(frame31, aes(x =Asistencia.particular.II, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame31$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame31$Porcentaje%in% "97,8",1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada31))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Biología ",
       x = "Asistencia",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo biologia
  theme(axis.text.x = element_text(size = 6,face="bold"))

#Examen.final.II
tabla.ordenada32<-factor(encuesta$Examen.final.II,levels=c("SÍ","NO, porque Promocioné","NO"))
table32<-table(tabla.ordenada32) # Frecuencias absolutas
prop.table(table(table32)) # Frecuencias relativas
porcentajes32<-round(prop.table(table32)*100,1) #Porcentajes

frame32<- data.frame("Examen final II" = names(table32),
                     "Frecuencia" = as.numeric(table32),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes32), 1)))) #tabla de frec.absolutas y porcentajes
frame32$Porcentaje <- gsub(",0$", "", frame32$Porcentaje) #elimino el decimal en 10,0%

G12.2<-ggplot(frame32, aes(x =Examen.final.II, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame32$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame32$Porcentaje%in% c("43,3","46,7"),1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada32),labels=c("SÍ"="Si","NO, porque Promocioné"="No\n(promocionó)","NO"="No"))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Biología",
       x = "Asistencia",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo biologia
  theme(axis.text.x = element_text(size = 6,face="bold"))

#Consignas.final.II
tabla.ordenada33<-factor(encuesta$Consignas.final.II,levels=c("Muy claras","Claras","Poco claras"))
table33<-table(tabla.ordenada33) # Frecuencias absolutas
prop.table(table(table33)) # Frecuencias relativas
porcentajes33<-round(prop.table(table33)*100,1) #Porcentajes

frame33<- data.frame("Consignas final II" = names(table33),
                     "Frecuencia" = as.numeric(table33),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes33), 1)) ))#tabla de frec.absolutas y porcentajes
frame33$Porcentaje <- gsub(",0$", "", frame33$Porcentaje) #elimino el decimal en 59,0%

G13.2<-ggplot(frame33, aes(x =Consignas.final.II, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame33$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.2,size=2.5,fontface="bold")+ 
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada33),labels=c("Muy claras"="Muy\n claras","Claras"="Claras","Poco claras"="Poco\n claras"))+
  labs(title = "Biología ",
       x = "Consignas",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo biologia
  theme(axis.text.x = element_text(size = 6,face="bold"))


#Tiempo.examen.final.II
tabla.ordenada34<-factor(encuesta$Tiempo.examen.final.II,levels=c("Adecuado","Insuficiente"))
table34<-table(tabla.ordenada34) # Frecuencias absolutas
prop.table(table(table34)) # Frecuencias relativas
porcentajes34<-round(prop.table(table34)*100,1) #Porcentajes

frame34<- data.frame("Tiempo examen final II" = names(table34),
                     "Frecuencia" = as.numeric(table34),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes34), 1)))) #tabla de frec.absolutas y porcentajes

G14.2<-ggplot(frame34, aes(x =Tiempo.examen.final.II, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame34$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame34$Porcentaje%in% "92,3",1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada34))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Biología ",
       x = "Tiempo",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo biologia
  theme(axis.text.x = element_text(size = 6,face="bold"))

########QUÍMICA GENERAL E INORGÁNICA#######################################################################

#Condicion.cursado.III
tabla.ordenada35<-factor(encuesta$Condicion.cursado.III,levels=c("Ingresante","Cambio de plan","Recursante"))
table35<-table(tabla.ordenada35) # Frecuencias absolutas
prop.table(table35) # Frecuencias relativas
porcentajes35<-round(prop.table(table21)*100,1) #Porcentajes

frame35<- data.frame("Condicion de cursado III" = names(table35),
                     "Frecuencia" = as.numeric(table35),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes35), 1)))) #tabla de frec.absolutas y porcentajes
frame35$Porcentaje <- gsub(",0$", "", frame35$Porcentaje) #elimino el decimal en 11,0%

G1.3<-ggplot(frame35, aes(x =Condicion.de.cursado.III, y =as.numeric(gsub(",", ".", Porcentaje))))  +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  scale_x_discrete(limits = levels(tabla.ordenada35),labels=c("Ingresante"="Ingresante","Cambio de plan"="Cambio\n de plan","Recursante"="Recursante"))+
  geom_text(aes(label =paste0(frame35$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame35$Porcentaje%in% "81,3",1.2,-0.2),size = 2.5,fontface="bold")+
  ylim(0,100)+
  labs(title = "Química G. e I.",
       x = "  ",
       y = "  ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+
  theme(axis.text.x = element_text(size = 6,angle = 45,face="bold"))


#Condicion.alcanzada.III

tabla.ordenada36<-factor(encuesta$Condicion.alcanzada.III,levels=c("Regular","Promocioné","Libre (rendí las evaluaciones pero me fue mal)","Abandoné","Me anoté, pero no empecé a cursar."))
table36<-table(tabla.ordenada36) # Frecuencias absolutas
prop.table(table(table36)) # Frecuencias relativas
porcentajes36<-round(prop.table(table36)*100,1) #Porcentajes

frame36<- data.frame("Condicion alcanzada III" = names(table36),
                     "Frecuencia" = as.numeric(table36),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes36), 1)))) #tabla de frec.absolutas y porcentajes
frame36$Porcentaje <- gsub(",0$", "", frame36$Porcentaje) #elimino el decimal en 50,0%

G2.3<-ggplot(frame36, aes(x =Condicion.alcanzada.III, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame36$Porcentaje,"%\n(",Frecuencia,")")),size=2.5,fontface="bold",vjust=ifelse(frame36$Porcentaje%in% 1,1.2,-0.2))+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada36),labels=c("Regular"="Regular","Promocioné"="Promocionó","Libre (rendí las evaluaciones pero me fue mal)"="Libre","Abandoné"="Abandonó","Me anoté, pero no empecé a cursar."="No cursó"))+
  labs(title = "Química G. e I. ",
       x = " ",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Quimica
  theme(axis.text.x = element_text(size = 6,angle = 45,face="bold"))


#ContenidosIII
tabla.ordenada37<-factor(encuesta$Contenidos.III,levels=c("Excesivo","Adecuado","Insuficiente")) #ordeno categorías como aparecen en la encuesta
table37<-table(tabla.ordenada37) # Frecuencias absolutas
prop.table(table(table37)) # Frecuencias relativas
porcentajes37<-round(prop.table(table37)*100,1) #Porcentajes

frame37<- data.frame("ContenidosIII" = names(table37),
                     "Frecuencia" = as.numeric(table37),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes37), 1)))) #tabla de frec.absolutas y porcentaje

G3.3<-ggplot(frame37, aes(x =ContenidosIII, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame37$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame37$Porcentaje%in% c("74,4","23,1"),1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada37))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Química G. e I.",
       x = "  ",
       y = "  ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Quimica
  theme(axis.text.x = element_text(size = 6,angle = 45,face="bold"))



#Calidad.explicativa.docente.III
tabla.ordenada38<-factor(encuesta$Calidad.explicativa.docente.III,levels=c("Muy claras","Claras","No me resultaron claras"))
table38<-table(tabla.ordenada38) # Frecuencias absolutas
prop.table(table(table38)) # Frecuencias relativas
porcentajes38<-round(prop.table(table38)*100,1) #Porcentajes

frame38<- data.frame("Calidad explicativaIII" = names(table38),
                     "Frecuencia" = as.numeric(table38),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes38), 1)) ))#tabla de frec.absolutas y porcentajes

G4.3<-ggplot(frame38, aes(x =Calidad.explicativaIII, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame38$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.2,size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada38),labels=c("Muy claras"="Muy\n claras","Claras"="Claras","No me resultaron claras"="No\n claras"))+
  labs(title = "Química G. e I. ",
       x = " ",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Quimica
  theme(axis.text.x = element_text(size = 6,face="bold"))


#Recusrsos.utilizados.III
tabla.ordenada39<-factor(encuesta$Recursos.utilizados.III,levels=c("Muy adecuados","Adecuados","Poco adecuados"))
table39<-table(tabla.ordenada39) # Frecuencias absolutas
prop.table(table(table39)) # Frecuencias relativas
porcentajes39<-round(prop.table(table25)*100,1) #Porcentajes

frame39<- data.frame("Recursos utilizados.III" = names(table39),
                     "Frecuencia" = as.numeric(table39),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes39), 1)))) #tabla de frec.absolutas y porcentajes

G5.3<-ggplot(frame39, aes(x =Recursos.utilizados.III, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame39$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.2,size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada11),labels=c("Muy adecuados"="Muy\n adecuados","Adecuados"="Adecuados","Poco adecuados"="Poco\n adecuados"))+
  labs(title = " Química G. e I.",
       x = " ",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Quimica
  theme(axis.text.x = element_text(size = 6,angle = 45,face="bold"))



#Contenidos.clase/parcial.III
tabla.ordenada40<-factor(encuesta$`Contenidos.clase/parcial.III`,levels=c("Si, los vimos varias veces","Algunos temas los vimos poco en las clases.","Algunos temas no los vimos en las clases.","No,el examen parcial fue muy distinto a lo dado en clase"))
table40<-table(tabla.ordenada40) # Frecuencias absolutas
prop.table(table(table40)) # Frecuencias relativas
porcentajes40<-round(prop.table(table40)*100,1) #Porcentajes

frame40<- data.frame("Contenidos clase/parcial III" = names(table40),
                     "Frecuencia" = as.numeric(table40),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes40), 1)))) #tabla de frec.absolutas y porcentajes
frame40$Porcentaje <- gsub(",0$", "", frame40$Porcentaje) #elimino el decimal en 50,0%

G6.3<-ggplot(frame40, aes(x =Contenidos.clase.parcial.III, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame40$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame40$Porcentaje%in% 1,1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada26),labels=c("Si, los vimos varias veces"="SI","Algunos temas los vimos poco en las clases."="SI,\n dados\n poco","Algunos temas no los vimos en las clases."="algunos\n no","No,el examen parcial fue muy distinto a lo dado en clase"="NO"))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Química G. e I. ",
       x = " ",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Quimica
  theme(axis.text.x = element_text(size = 6,face="bold"))




#Consignas.parcial.III
tabla.ordenada41<-factor(encuesta$Consignas.parcial.III,levels=c("Muy claras","Claras","Poco claras"))
table41<-table(tabla.ordenada41) # Frecuencias absolutas
prop.table(table(table41)) # Frecuencias relativas
porcentajes41<-round(prop.table(table41)*100,1) #Porcentajes

frame41<- data.frame("Consignas parcial III" = names(table41),
                     "Frecuencia" = as.numeric(table41),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes41), 1)))) #tabla de frec.absolutas y porcentajes

G7.3<-ggplot(frame41, aes(x =Consignas.parcial.III, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame41$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.2,size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada41),labels=c("Muy claras"="Muy\n claras","Claras"="Claras","Poco claras"="Poco\n claras"))+
  labs(title = "Química G. e I.",
       x = " ",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Quimica
  theme(axis.text.x = element_text(size = 6,face="bold"))


#Tiempo.examen.III
tabla.ordenada42<-factor(encuesta$Tiempo.examen.III,levels=c("Adecuado","Insuficiente"))
table42<-table(tabla.ordenada42) # Frecuencias absolutas
prop.table(table(table42)) # Frecuencias relativas
porcentajes42<-round(prop.table(table42)*100,1) #Porcentajes

frame42<- data.frame("Tiempo examen III" = names(table42),
                     "Frecuencia" = as.numeric(table42),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes42), 1)))) #tabla de frec.absolutas y porcentajes

G8.3<-ggplot(frame42, aes(x =Tiempo.examen.III, y =as.numeric(gsub(",", ".", Porcentaje))))+
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame42$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame42$Porcentaje%in% "89,7",1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada28))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Química G. e I ",
       x = " ",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Quimica
  theme(axis.text.x = element_text(size = 6,face="bold"))



#Asistencia.consulta.III
tabla.ordenada43<-factor(encuesta$Asistencia.consulta.III,levels=c("SÍ","A VECES","NUNCA"))
table43<-table(tabla.ordenada43) # Frecuencias absolutas
prop.table(table(table43)) # Frecuencias relativas
porcentajes43<-round(prop.table(table43)*100,1) #Porcentajes

frame43<- data.frame("Asistencia consulta III" = names(table43),
                     "Frecuencia" = as.numeric(table43),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes43), 1)))) #tabla de frec.absolutas y porcentajes

G9.3<-ggplot(frame43, aes(x =Asistencia.consulta.III, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame43$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.2,size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada43))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Química G. e I. ",
       x = " ",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Quimica
  theme(axis.text.x = element_text(size = 6,face="bold"))



#Rendimiento.consulta.III
tabla.ordenada44<-factor(encuesta$Rendimiento.consulta.III,levels=c("Muy útiles","Útiles","Poco útiles"))
table44<-table(tabla.ordenada44) # Frecuencias absolutas
prop.table(table(table44)) # Frecuencias relativas
porcentajes44<-round(prop.table(table44)*100,1) #Porcentajes

frame44<- data.frame("Rendimiento consulta III" = names(table44),
                     "Frecuencia" = as.numeric(table44),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes44), 1)))) #tabla de frec.absolutas y porcentajes

G10.3<-ggplot(frame44, aes(x =Rendimiento.consulta.III, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame44$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame44$Porcentaje%in% c("28,3","63,3"),1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada44),labels=c("Muy útiles"="Muy\n útiles","Útiles"="Útiles","Poco útiles"="Poco\n útiles"))+
  labs(title = "Química G. e I. ",
       x = " ",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Quimica
  theme(axis.text.x = element_text(size = 6,face="bold"))




#Asistencia.particular.III
tabla.ordenada45<-factor(encuesta$Asistencia.particular.III,levels=c("SÍ","NO"))
table45<-table(tabla.ordenada45) # Frecuencias absolutas
prop.table(table(table45)) # Frecuencias relativas
porcentajes45<-round(prop.table(table45)*100,1) #Porcentajes

frame45<- data.frame("Asistencia particular III" = names(table45),
                     "Frecuencia" = as.numeric(table45),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes45), 1)))) #tabla de frec.absolutas y porcentajes

G11.3<-ggplot(frame45, aes(x =Asistencia.particular.III, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame45$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.2,size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada45))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Química G. e I. ",
       x = " ",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Quimica
  theme(axis.text.x = element_text(size = 6,face="bold"))


#Examen.final.III
tabla.ordenada46<-factor(encuesta$Examen.final.III,levels=c("SÍ","NO, porque Promocioné","NO"))
table46<-table(tabla.ordenada46) # Frecuencias absolutas
prop.table(table(table46)) # Frecuencias relativas
porcentajes46<-round(prop.table(table46)*100,1) #Porcentajes

frame46<- data.frame("Examen final III" = names(table46),
                     "Frecuencia" = as.numeric(table46),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes46), 1)))) #tabla de frec.absolutas y porcentajes

G12.3<-ggplot(frame46, aes(x =Examen.final.III, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame46$Porcentaje,"%\n(",Frecuencia,")")),vjust=ifelse(frame46$Porcentaje%in% c("42,3","56,4"),1.2,-0.2),size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada46),labels=c("SÍ"="Si","NO, porque Promocioné"="No\n(promocionó)","NO"="No"))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Química G. e I. ",
       x = " ",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Quimica
  theme(axis.text.x = element_text(size = 6,face="bold"))

#Consignas.final.III
tabla.ordenada47<-factor(encuesta$Consignas.final.III,levels=c("Muy claras","Claras","Poco claras"))
table47<-table(tabla.ordenada47) # Frecuencias absolutas
prop.table(table(table47)) # Frecuencias relativas
porcentajes47<-round(prop.table(table47)*100,1) #Porcentajes

frame47<- data.frame("Consignas final III" = names(table47),
                     "Frecuencia" = as.numeric(table47),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes47), 1)))) #tabla de frec.absolutas y porcentajes

G13.3<-ggplot(frame47, aes(x =Consignas.final.III, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame47$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.2,size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada47),labels=c("Muy claras"="Muy\n claras","Claras"="Claras","Poco claras"="Poco\n claras"))+
  labs(title = "Química G. e I.",
       x = " ",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Quimica
  theme(axis.text.x = element_text(size = 6,face="bold"))

#Tiempo.examen.final.III
tabla.ordenada48<-factor(encuesta$Tiempo.examen.final.III,levels=c("Adecuado","Insuficiente"))
table48<-table(tabla.ordenada48) # Frecuencias absolutas
prop.table(table(table48)) # Frecuencias relativas
porcentajes48<-round(prop.table(table48)*100,1) #Porcentajes

frame48<- data.frame("Tiempo examen final III" = names(table48),
                     "Frecuencia" = as.numeric(table48),
                     "Porcentaje" = gsub("\\.", ",", 
                                         sprintf("%.1f",round(as.numeric(porcentajes48), 1)))) #tabla de frec.absolutas y porcentajes

G14.3<-ggplot(frame48, aes(x =Tiempo.examen.final.III, y =as.numeric(gsub(",", ".", Porcentaje)))) +
  geom_bar(stat = "identity", fill = "steelblue",color="black") +
  geom_text(aes(label =paste0(frame48$Porcentaje,"%\n(",Frecuencia,")")),vjust=1.2,size=2.5,fontface="bold")+
  ylim(0,100)+
  scale_x_discrete(limits = levels(tabla.ordenada48))+ #ordeno categorías como aparecen en la encuesta
  labs(title = "Química G. e I. ",
       x = " ",
       y = " ")+
  theme(plot.title = element_text(size = 8.5,face = "bold"))+ #tamaño del titulo Quimica
  theme(axis.text.x = element_text(size = 6,face="bold"))


#########################Gráficos en paneles################################################

#Condicion.cursado

grid.arrange(G1.1, G1.2, G1.3, ncol = 3)
                                        
#Condicion.alcanzada

grid.arrange(G2.1, G2.2, G2.3, ncol = 3)

#Contenidos

grid.arrange(G3.1, G3.2, G3.3, ncol = 3)

#Calidad.explicativa.docente

grid.arrange(G4.1, G4.2, G4.3, ncol = 3)

#Recursos.utilizados

grid.arrange(G5.1, G5.2, G5.3, ncol = 3)

#Contenidos.clase.parcial

grid.arrange(G6.1, G6.2, G6.3, ncol = 3)

#Consignas.parcial

grid.arrange(G7.1, G7.2, G7.3, ncol = 3)

#Tiempo.examen

grid.arrange(G8.1, G8.2, G8.3, ncol = 3)

#Asistencia.consulta

grid.arrange(G9.1, G9.2, G9.3, ncol = 3)

#Rendimiento.consulta

grid.arrange(G10.1, G10.2, G10.3, ncol = 3)

#Asistencia particular

grid.arrange(G11.1, G11.2, G11.3, ncol = 3)

#Examen.final

grid.arrange(G12.1, G12.2, G12.3, ncol = 3)

#Consignas.final

grid.arrange(G13.1, G13.2, G13.3, ncol = 3)

#Tiempo.examen.final

grid.arrange(G14.1, G14.2, G14.3, ncol = 3)
