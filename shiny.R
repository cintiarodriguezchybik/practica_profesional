#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
 
 
  
  library(readxl)
  library(shiny)
  library(gridExtra)
  library(ggplot2)
  library(dplyr)
  library(grid)
  
  encuesta<-read_excel("C:\\Users\\PC\\Desktop\\Trabajo Final Cintia Sabrina Rodriguez Chybik\\Trabajo Final Cintia Sabrina Rodriguez Chybik\\Base de datos depurada.xlsx")
  
  tipo_analisis<-c("UNIVARIADO","BIVARIADO")
  
   ui <- fluidPage(
     
     titlePanel(
       div(
         style = "text-align: center; padding: 20px 0;",
         h1(icon("chart-column"), " ANÁLISIS EXPLORATORIO INTERACTIVO", 
            style = "color: #1B5E20;"),
         h4("Encuesta del Plan de Estudios - Ingeniería Agronómica"),
         tags$hr(style = "width: 50%; border-color: #4CAF50;"),
         p("Herramienta desarrollada para la Comisión de Seguimiento (COSPE IA)", 
           style = "color: #666; font-size: 14px;")
       )
     ),
    
     radioButtons("analisis",label = "TIPO DE ANÁLISIS",tipo_analisis,inline=TRUE),
    
     conditionalPanel(condition = "input.analisis == 'UNIVARIADO'",
         selectInput("variable",label = "SELECCIONE LA VARIABLE",
                    choices=c(" " = "", #para que inicialmente aparezca vacío el selector
                              "Ingreso a la carrera"="Ingreso",
                              "Lugar de residencia"= "Lugar.residencia",
                              "Personas con las que vive"="Personas.con.las.que.vive",
                              "Tiempo traslado a la facultad"="Tiempo.traslado",
                              "Condición laboral"="Tiene.trabajo", 
                              "Horas semanales de trabajo"="Horas.semanales.de.trabajo",
                              "Condición de cursado"="Condicion.cursado",
                              "Condición alcanzada"="Condicion.alcanzada",
                              "Cantidad de contenidos por clase"="Contenidos",
                              "Calidad explicativa del docente"="Calidad.explicativa.docente",
                              "Recursos utilizados en clase"="Recursos.utilizados", 
                              "Relación contenidos clase/parcial"="Contenidos.clase.parcial",
                              "Calidad de las consignas del parcial"="Consignas.parcial",
                              "Tiempo del exámen parcial"="Tiempo.examen",
                              "Frecuencia de asistencia a clases consulta"="Asistencia.consulta",
                              "Rendimiento de las clases consulta"="Rendimiento.consulta",
                              "Asistencia clases particulares"="Asistencia.particular",
                              "Asistencia al examen final"="Examen.final",
                              "Calidad de las consignas del examen final"="Consignas.final",
                              "Tiempo límite dado para examen final"="Tiempo.examen.final",
                              "Cortaderas integración general"="Cortaderas.general"),
                  selected = "", width = "400px"),plotOutput("grafico", height = "300px", width = "700")),
     
  
       conditionalPanel(condition = "input.analisis == 'BIVARIADO'",
                        fluidRow(column(7,  
                         selectInput("analisis_bivariado", 
                                      label = "ANÁLISIS ENTRE VARIABLES:",
                                       choices = c(" " = "",
                                                   "De aspecto personal" = "personal",
                                                   "De aspecto personal y académico" = "personal_academico",
                                                   "De aspecto académico" = "academico"),
                                       selected = "",width = "400px")),
                         column(5,  
                          uiOutput("selector_bivariado")
                         )
                        ),
                                      
                          uiOutput("layout_bivariado")
                        )
   )
   
   
    server <- function(input, output, session) {
      
      output$grafico <- renderPlot({
        
        if(input$analisis=="UNIVARIADO"){
        
         req(input$variable)
         if(input$variable=="Ingreso"){
           ggplot_1 + labs(title = "Año de ingreso a la carrera") +
             theme(plot.title = element_text(size=15,face = "bold"),
                            axis.title = element_text(size = 17),
                            axis.text = element_text(size = 15))
         
           } else if(input$variable=="Lugar.residencia"){
           ggplot_2 + labs(title = "Localidad de residencia de los estudiantes") +
               theme(plot.title = element_text(size=15,face = "bold"),
                          axis.title = element_text(size = 17),
                          axis.text = element_text(size = 15))
           
         } else if (input$variable=="Personas.con.las.que.vive"){
           ggplot_3 + labs(title = "Personas con las que viven los estudiantes") +
             theme(plot.title = element_text(size=15,face = "bold"),
                            axis.title = element_text(size = 17),
                            axis.text = element_text(size = 15))
      
         } else if (input$variable == "Tiempo.traslado") {
           ggplot_4 +labs(title = "Tiempo diario de traslado ida y vuelta a la facultad") +
             theme(plot.title = element_text(size=15,face = "bold"),
                           axis.title = element_text(size = 17),
                           axis.text = element_text(size = 15))
           
         } else if (input$variable == "Tiene.trabajo") {
           ggplot_5 + labs(title = "Condición laboral de los estudiantes") +
             theme(plot.title = element_text(size=15,face = "bold"),
                            axis.title = element_text(size = 17),
                            axis.text = element_text(size = 15))
           
         } else if (input$variable == "Horas.semanales.de.trabajo") {
           ggplot_6 + labs(title = "Cantidad de horas semanales dedicadas al trabajo") +
             theme(plot.title = element_text(size=15,face = "bold"),
                            axis.title = element_text(size = 17),
                            axis.text = element_text(size = 15))
           
         } else if (input$variable=="Condicion.cursado"){
           grid.arrange(G1.1, G1.2, G1.3, ncol = 3,top=textGrob("Condición de cursado por asignatura",gp = gpar(fontsize = 15, fontface = "bold"),x=0.05,hjust=0))
         
         } else if (input$variable=="Condicion.alcanzada"){
           grid.arrange(G2.1, G2.2, G2.3, ncol = 3,top=textGrob("Condición alcanzada por asignatura",x=0.05,hjust=0,gp = gpar(fontsize = 15, fontface = "bold")))
     
         } else if (input$variable=="Contenidos"){
           
           grid.arrange(G3.1, G3.2, G3.3, ncol = 3,top=textGrob("Contenido dado por clase por asignatura",x=0.05,hjust=0,gp = gpar(fontsize = 15, fontface = "bold")))
         
         } else if (input$variable=="Calidad.explicativa.docente"){
         
           grid.arrange(G4.1, G4.2, G4.3, ncol = 3,top=textGrob("Explicaciones del docente por asignatura",x=0.05,hjust=0,gp = gpar(fontsize = 15, fontface = "bold")))    
  
         } else if (input$variable=="Recursos.utilizados"){
           
           grid.arrange(G5.1, G5.2, G5.3, ncol = 3,top=textGrob("Recursos (material) utilizados en clases por asignatura",x=0.05,hjust=0,gp = gpar(fontsize = 15, fontface = "bold")))
           
         } else if (input$variable=="Contenidos.clase.parcial"){ 
           
           grid.arrange(G6.1, G6.2, G6.3, ncol = 3,top=textGrob("En el parcial, ¿se tomaron los mismos contenidos dados en clase?",x=0.05,hjust=0,gp = gpar(fontsize = 15, fontface = "bold")))
           
         } else if (input$variable=="Consignas.parcial"){ 
           
           grid.arrange(G7.1, G7.2, G7.3, ncol = 3,top=textGrob("Calidad de las consignas del parcial por asignatura",x=0.05,hjust=0,gp = gpar(fontsize = 15, fontface = "bold")))
           
         } else if (input$variable=="Tiempo.examen"){
           
           grid.arrange(G8.1, G8.2, G8.3, ncol = 3,top=textGrob("Tiempo dado para resolver el parcial por asignatura",x=0.05,hjust=0,gp = gpar(fontsize = 15, fontface = "bold")))
           
         } else if (input$variable=="Asistencia.consulta"){ 
           
           grid.arrange(G9.1, G9.2, G9.3, ncol = 3,top=textGrob("Asistencia a las consultas por asignatura",x=0.05,hjust=0,gp = gpar(fontsize = 15, fontface = "bold")))
           
         } else if (input$variable=="Rendimiento.consulta"){
           
           grid.arrange(G10.1, G10.2, G10.3, ncol = 3,top=textGrob("Rendimiento de las consultas por asignatura",x=0.05,hjust=0,gp = gpar(fontsize = 15, fontface = "bold"))) 
           
         } else if (input$variable=="Asistencia.particular"){
           
           grid.arrange(G11.1, G11.2, G11.3, ncol = 3,top=textGrob("Asistencia a particular por asignatura",x=0.05,hjust=0,gp = gpar(fontsize = 15, fontface = "bold")))
           
         } else if (input$variable=="Examen.final"){
           
           grid.arrange(G12.1, G12.2, G12.3, ncol = 3,top=textGrob("Asistencia al examen final por asignatura",x=0.05,hjust=0,gp = gpar(fontsize = 15, fontface = "bold")))
           
         } else if (input$variable=="Consignas.final"){ 
           
           grid.arrange(G13.1, G13.2, G13.3, ncol = 3,top=textGrob("Calidad de las consignas del examen final por asignatura",x=0.05,hjust=0,gp = gpar(fontsize = 15, fontface = "bold")))
           
         } else if (input$variable=="Tiempo.examen.final"){
           
           grid.arrange(G14.1, G14.2, G14.3, ncol = 3,top=textGrob("Tiempo límite dado para responder el examen final por asignatura",x=0.05,hjust=0,gp = gpar(fontsize = 15, fontface = "bold")))
          
         } else if (input$variable=="Cortaderas.general"){
           ggplot_general+labs(title = "Caso 'Las Cortaderas': ¿Permitió integrar\n los contenidos de las asignaturas de primer año?") +
             theme(plot.title = element_text(size=15,face = "bold")
                   )}
        
        }
      }
      )
      
      output$selector_bivariado <- renderUI({
        req(input$analisis_bivariado)
        req(input$analisis_bivariado != "")  # Solo mostrar si se seleccionó una categoría
       
         # Definir las opciones según la categoría seleccionada
        opciones <- switch(input$analisis_bivariado,#switch:selector por caso
                           "personal" = c(" " = "",
                                          "Lugar de residencia X Ingreso a la carrera"="Lugar.residenciaxIngreso",
                                          "Condición laboral X Ingreso a la carrera"="Tiene.trabajoxIngreso",
                                          "Personas con las que vive X Ingreso a la carrera"="Personas.con.las.que.vivexIngreso",
                                          "Personas con las que vive X Condición laboral"="Personas.con.las.que.vivexTiene.trabajo"), 
                           "personal_academico" = c(" " = "",
                                          "Condición alcanzada X Ingreso a la carrera"="Condicion.alcanzadaxIngreso",
                                          "Condición alcanzada X Condición laboral"="Condicion.alcanzadaxTiene.trabajo",
                                          "Frecuencia a consulta X Tiempo de traslado"="Asistencia.consultaxTiempo.traslado"),
                            "academico"=c(" "="",
                                          "Calidad explicativa docente X Condición alcanzada"="Calidad.explicativa.docentexCondicion.alcanzada",
                                          "Frecuencia a consulta X Cantidad de contenidos"="Asistencia.consulta×Contenidos", 
                                          "Condición alcanzada X Frecuencia a consultas"="Condicion.alcanzada×Asistencia.consulta",
                                          "Condición alcanzada X Condición de cursado"="Condicion.alcanzadaxCondicion.cursado",
                                          "Condición alcanzada X Recursos utilizados en clase"="Condición.alcanzadaxRecursos.utilizado",
                           	              "Relación de contenidos clase/parcial X Condición alcanzada"="Contenidos.clase.parcialxCondicion.alcanzada",  
                                          "Condición alcanzada X Cantidad de contenidos"="Condicion.alcanzada x Contenidos", 
                           	              "Frecuencia a consulta X Calidad explicativa del docente"="Asistencia.consultaxCalidad.explicativa.docente", 
                                          "Calidad explicativa docente X Asistencia clases particulares"="Calidad.explicativa.docentexAsistencia.particular", 
                                          "Condición alcanzada X Rendimiento clases consulta"="Condicion.alcanzadaxRendimiento.consulta", 
                                          "Condición alcanzada X Calidad de las consignas del final"="Condicion.alcanzadaxConsignas.final" )
                           )
                           
                           # Crear el selector 
                           selectInput("variables_indep", 
                                       label = "SELECCIONE EL CRUCE DE VARIABLES:",
                                       choices = opciones,
                                       selected = "", 
                                       width = "420px")
      })
    
      
    # Output para el layout dinámico del análisis bivariado.Layout dinámico para gráficos y tablas 
      output$layout_bivariado <- renderUI({
        req(input$variables_indep)  # Esperar a que se seleccione una relación
        req(input$variables_indep != "")  # No mostrar si está vacío
        
        opciones_personales <- c(
          "Lugar.residenciaxIngreso",
          "Tiene.trabajoxIngreso",
          "Personas.con.las.que.vivexIngreso", 
          "Personas.con.las.que.vivexTiene.trabajo"
        )
        
        opciones_asignaturas<-c(
          "Condicion.alcanzadaxIngreso",
          "Condicion.alcanzadaxTiene.trabajo",
          "Asistencia.consultaxTiempo.traslado",
          "Calidad.explicativa.docentexCondicion.alcanzada",
          "Asistencia.consulta×Contenidos", 
          "Condicion.alcanzada×Asistencia.consulta",
          "Condicion.alcanzadaxCondicion.cursado",
          "Condición.alcanzadaxRecursos.utilizado",
          "Contenidos.clase.parcialxCondicion.alcanzada",  
          "Condicion.alcanzada x Contenidos", 
          "Asistencia.consultaxCalidad.explicativa.docente", 
          "Calidad.explicativa.docentexAsistencia.particular", 
          "Condicion.alcanzadaxRendimiento.consulta",
          "Condicion.alcanzadaxConsignas.final"
          )
        
        if(input$variables_indep %in% opciones_personales) {
          fluidRow(
            column(7, plotOutput("grafico_bivariado", height = "500px")),
            column(5, tableOutput("tabla_bivariado"))
                   )
          
        }else if(input$variables_indep %in% opciones_asignaturas){
          tagList(
            # Grid.arrange con los 3 gráficos
            plotOutput("grafico_grid_arrange", height = "250px", width = "100%"),
            
            # 3 Tablas debajo
            fluidRow(
              column(4, 
                     div(style = "text-align: center; margin: 10px 0;"), 
                     div(style = "font-size: 12px;",
                     tableOutput("tabla_academica1"))
              ),
              column(4,
                     div(style = "text-align: center; margin: 10px 0;"),
                     div(style = "font-size: 12px;",
                     tableOutput("tabla_academica2"))
              ),
              column(4,
                     div(style = "text-align: center; margin: 10px 0;"),
                     div(style = "font-size: 12px;",
                     tableOutput("tabla_academica3"))
              )
            )
          )
        }
      })
      
      # GRÁFICOS PARA OPCIONES PERSONALES (individuales)
      output$grafico_bivariado <- renderPlot({
        req(input$variables_indep)
        
        if(input$variables_indep == "Lugar.residenciaxIngreso") {
          ggplot_30+labs(title = "Lugar de residencia según Año de ingreso a la carrera")+theme(plot.title = element_text(hjust = 0)) 
        } else if(input$variables_indep == "Tiene.trabajoxIngreso") {
          ggplot_31+labs(title = "Condición laboral según Año de ingreso a la carrera")+theme(plot.title = element_text(hjust = 0)) 
        } else if(input$variables_indep == "Personas.con.las.que.vivexIngreso") {
          ggplot_32+labs(title = "Personas con las que viven según Año de ingreso a la carrera")+theme(plot.title = element_text(hjust = 0)) 
        } else if(input$variables_indep == "Personas.con.las.que.vivexTiene.trabajo") {
          ggplot_33+labs(title = "Personas con las que viven según Condición laboral")+theme(plot.title = element_text(hjust = 0)) 
        }
      })
      
      # GRÁFICO GRID.ARRANGE PARA OPCIONES ACADÉMICAS
      output$grafico_grid_arrange <- renderPlot({
        req(input$variables_indep)
        
        if(input$variables_indep == "Condicion.alcanzadaxIngreso") {
          grid.arrange(ggplot_34, ggplot_35, ggplot_36, ncol = 3)
                                      
        } else if(input$variables_indep == "Condicion.alcanzadaxTiene.trabajo") {
          grid.arrange(ggplot_37, ggplot_38, ggplot_39, ncol = 3)
                       
        } else if(input$variables_indep == "Asistencia.consultaxTiempo.traslado") {
          grid.arrange(ggplot_40, ggplot_41, ggplot_42, ncol = 3)
          
        } else if(input$variables_indep == "Calidad.explicativa.docentexCondicion.alcanzada") {
          grid.arrange(ggplot_43, ggplot_44, ggplot_45, ncol = 3)
                       
        } else if(input$variables_indep == "Asistencia.consulta×Contenidos") {
          grid.arrange(ggplot_46,ggplot_47, ggplot_48 , ncol = 3)
          
        } else if(input$variables_indep == "Condicion.alcanzada×Asistencia.consulta") {
          grid.arrange(ggplot_49,ggplot_50, ggplot_51 , ncol = 3)
          
        } else if(input$variables_indep == "Condicion.alcanzadaxCondicion.cursado") {
          grid.arrange(ggplot_52,ggplot_53, ggplot_54 , ncol = 3)
          
        } else if(input$variables_indep == "Condición.alcanzadaxRecursos.utilizado") {
          grid.arrange(ggplot_55,ggplot_56, ggplot_57 , ncol = 3)
        
        } else if(input$variables_indep == "Contenidos.clase.parcialxCondicion.alcanzada") {
          grid.arrange(ggplot_58,ggplot_59, ggplot_60 , ncol = 3)
          
        } else if(input$variables_indep =="Condicion.alcanzada x Contenidos") {
          grid.arrange(ggplot_61,ggplot_62, ggplot_63 , ncol = 3)
        
        } else if(input$variables_indep =="Asistencia.consultaxCalidad.explicativa.docente") {
          grid.arrange(ggplot_64,ggplot_65, ggplot_66 , ncol = 3)
          
        } else if(input$variables_indep == "Calidad.explicativa.docentexAsistencia.particular") {
          grid.arrange(ggplot_67,ggplot_68, ggplot_69 , ncol = 3)
  
        } else if(input$variables_indep == "Condicion.alcanzadaxRendimiento.consulta" ) {
          grid.arrange(ggplot_70,ggplot_71, ggplot_72 , ncol = 3)
          
        } else if(input$variables_indep == "Condicion.alcanzadaxConsignas.final") {
          grid.arrange(ggplot_76,ggplot_77, ggplot_78 , ncol = 3)  
           
      }})  
          
              # TABLAS PARA OPCIONES PERSONALES
      
      output$tabla_bivariado <- renderTable({
        req(input$variables_indep)
        
        if(input$variables_indep == "Lugar.residenciaxIngreso") {
          as.data.frame.matrix(tabla1_con_totales)
        } else if(input$variables_indep == "Tiene.trabajoxIngreso") {
          as.data.frame.matrix(tabla2_con_totales)
        } else if(input$variables_indep == "Personas.con.las.que.vivexIngreso") {
          as.data.frame.matrix(tabla3_con_totales)
        } else if(input$variables_indep == "Personas.con.las.que.vivexTiene.trabajo") {
          as.data.frame.matrix(tabla4_con_totales)
        }
      }, rownames = TRUE, digits = 0)
      
      # TABLAS PARA OPCIONES ACADÉMICAS
      output$tabla_academica1 <- renderTable({
        req(input$variables_indep)
        
        switch(input$variables_indep,
               "Condicion.alcanzadaxIngreso" = as.data.frame.matrix(tabla5_con_totales),
               "Condicion.alcanzadaxTiene.trabajo" = as.data.frame.matrix(tabla8_con_totales),
               "Asistencia.consultaxTiempo.traslado" = as.data.frame.matrix(tabla11_con_totales),
               "Calidad.explicativa.docentexCondicion.alcanzada" = as.data.frame.matrix(tabla14_con_totales),
               "Asistencia.consulta×Contenidos"= as.data.frame.matrix(tabla17_con_totales), 
               "Condicion.alcanzada×Asistencia.consulta"= as.data.frame.matrix(tabla20_con_totales),
               "Condicion.alcanzadaxCondicion.cursado"= as.data.frame.matrix(tabla23_con_totales),
               "Condición.alcanzadaxRecursos.utilizado"= as.data.frame.matrix(tabla26_con_totales),
               "Contenidos.clase.parcialxCondicion.alcanzada"= as.data.frame.matrix(tabla29_con_totales),  
               "Condicion.alcanzada x Contenidos"= as.data.frame.matrix(tabla32_con_totales), 
               "Asistencia.consultaxCalidad.explicativa.docente"= as.data.frame.matrix(tabla35_con_totales), 
               "Calidad.explicativa.docentexAsistencia.particular"= as.data.frame.matrix(tabla38_con_totales), 
               "Condicion.alcanzadaxRendimiento.consulta"= as.data.frame.matrix(tabla41_con_totales), 
               "Condicion.alcanzadaxConsignas.final"= as.data.frame.matrix(tabla44_con_totales),
               data.frame(Información = "Tabla Asignatura 1")
        )
      }, rownames = TRUE, digits = 0)
      
      output$tabla_academica2 <- renderTable({
        req(input$variables_indep)
        switch(input$variables_indep,
               "Condicion.alcanzadaxIngreso" = as.data.frame.matrix(tabla6_con_totales),
               "Condicion.alcanzadaxTiene.trabajo" = as.data.frame.matrix(tabla9_con_totales),
               "Asistencia.consultaxTiempo.traslado" = as.data.frame.matrix(tabla12_con_totales),
               "Calidad.explicativa.docentexCondicion.alcanzada" = as.data.frame.matrix(tabla15_con_totales),
               "Asistencia.consulta×Contenidos"= as.data.frame.matrix(tabla18_con_totales), 
               "Condicion.alcanzada×Asistencia.consulta"= as.data.frame.matrix(tabla21_con_totales),
               "Condicion.alcanzadaxCondicion.cursado"= as.data.frame.matrix(tabla24_con_totales),
               "Condición.alcanzadaxRecursos.utilizado"= as.data.frame.matrix(tabla27_con_totales),
               "Contenidos.clase.parcialxCondicion.alcanzada"= as.data.frame.matrix(tabla30_con_totales),  
               "Condicion.alcanzada x Contenidos"= as.data.frame.matrix(tabla33_con_totales), 
               "Asistencia.consultaxCalidad.explicativa.docente"= as.data.frame.matrix(tabla36_con_totales), 
               "Calidad.explicativa.docentexAsistencia.particular"= as.data.frame.matrix(tabla39_con_totales), 
               "Condicion.alcanzadaxRendimiento.consulta"= as.data.frame.matrix(tabla42_con_totales), 
               "Condicion.alcanzadaxConsignas.final"= as.data.frame.matrix(tabla45_con_totales),
               data.frame(Información = "Tabla Asignatura 2")
        )
      }, rownames = TRUE, digits = 0)
      
      output$tabla_academica3 <- renderTable({
        req(input$variables_indep)
        
        switch(input$variables_indep,
               "Condicion.alcanzadaxIngreso" = as.data.frame.matrix(tabla7_con_totales),
               "Condicion.alcanzadaxTiene.trabajo" = as.data.frame.matrix(tabla10_con_totales),
               "Asistencia.consultaxTiempo.traslado" = as.data.frame.matrix(tabla13_con_totales),
               "Calidad.explicativa.docentexCondicion.alcanzada" = as.data.frame.matrix(tabla16_con_totales),
               "Asistencia.consulta×Contenidos"= as.data.frame.matrix(tabla19_con_totales), 
               "Condicion.alcanzada×Asistencia.consulta"= as.data.frame.matrix(tabla22_con_totales),
               "Condicion.alcanzadaxCondicion.cursado"= as.data.frame.matrix(tabla25_con_totales),
               "Condición.alcanzadaxRecursos.utilizado"= as.data.frame.matrix(tabla28_con_totales),
               "Contenidos.clase.parcialxCondicion.alcanzada"= as.data.frame.matrix(tabla31_con_totales),  
               "Condicion.alcanzada x Contenidos"= as.data.frame.matrix(tabla34_con_totales), 
               "Asistencia.consultaxCalidad.explicativa.docente"= as.data.frame.matrix(tabla37_con_totales), 
               "Calidad.explicativa.docentexAsistencia.particular"= as.data.frame.matrix(tabla40_con_totales), 
               "Condicion.alcanzadaxRendimiento.consulta"= as.data.frame.matrix(tabla43_con_totales), 
               "Condicion.alcanzadaxConsignas.final"= as.data.frame.matrix(tabla46_con_totales),
               data.frame(Información = "Tabla Asignatura 3")
        )
      }, rownames = TRUE, digits = 0)
      
    }
    
    
      
     shinyApp(ui, server)

     
     
      
      
              
       
          
       
     
    
   

        