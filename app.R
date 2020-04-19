.libPaths("C:/Program Files/R/R-3.6.3/library" )

library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(tidyverse)
library(stringr)
library(plotly)
library(vctrs)
library(sf)
library(summarytools)



#Las principales bases de datos están guardadas en el directorio correspondiente como:
# "base_basica_victimas" Para la base que incluye los datos de victimas años 2010 a 2018
# "base_comu" Para los datos de las comunas, cada fila representando una comuna en un año dado.
# "base_comu_global" Para los datos de las comunas desagregados por año. Cada fila una comuna. Tomamos medias.
# "mapa" Es la base de datos geoespaciales ( objeto sf)

### PARTE FUERA DE UI/SERVER  ###

#Cargamos los datos
base_basica  <- readRDS( "base_victimas" )
base_comu    <- readRDS( "base_comu" )
base_comu_gl <- readRDS( "base_comu_global" )
mapa         <- readRDS( "datosmapaMOD" )

nombre_bonito <- function(str){
  #Esta función transforma nombres de variables a nombres más legibles para que aparezcan en las gráficas
  
  if(!is.character(str))
    "Error. La funcion necesita un string para funcionar" %>% print()
  v <- c(
    "Comuna","comuna",
    "Lugar del delito","lugar",
    "Año","año",
    "Mes","mes",
    "Día de la semana","diasem",
    "Día del mes","diames",
    "Día del año","dia365",
    "Hora exacta","horadec",
    "Hora del día","hora24",
    "Detenido","detenido",
    "Sexo","sexo",
    "Nivel de estudios","estudios",
    "Edad","edad",
    "Superficie", "superficie",
    "Delito","nombre_delito",
    "Franja horaria", "franja",
    "Población","poblacion",
    "Densidad","densidad",
    "Mujeres(%)","mujeres",
    "Mortalidad Infantil", "mort.infantil",
    "Natalidad","natalidad",
    "Participación en programas sociales","prog.social",
    "Participación en programas culturales","prog.cult",
    "Asistencia escolar(%)","asist.escolar",
    "Retiro escolar (educación básica)(%)","reb",
    "Retiro escolar (educación media)(%)","rem",
    "Área verde por habitante","a.verdes",
    "Pobreza(%)","pobreza",
    "Oddshombres","oddshombres",
    "Violación","violaciones",
    "Homicidio","homicidio",
    "Hurtos","hurto",
    "Ingresos propios permanentes","ipp",
    "Lesiones","lesiones",
    "Tasa de hurto por cada 10.000 habitantes", "hurtos",
    "Tasa de lesiones por cada 10.000 habitantes", "Lesiones",
    "Tasa de otros robos con fuerza por cada 10.000 habitantes", "otros_robos",
    "Tasa de robo con intimidacion por cada 10.000 habitantes", "intimidacion",         
    "Tasa de robo con violencia por cada 10.000 habitantes", "violencia" ,        
    "Tasa de robo en lugar habitado por cada 10.000 habitantes", "habitado"  ,      
    "Tasa de robo en lugar no habitado por cada 10.000 habitantes", "no_habitado" ,    
    "Tasa de robo por Sorpresa por cada 10.000 habitantes","sorpresa",   
    "Tasa de violaciones por cada 10.000 habitantes","violacones",  
    "Tasa de robo de vehículo por cada 10.000 habitantes","vehículo"          , 
    "Tasa de robo objeto de o desde vehículo por cada 10.000 habitantes", "objeto_veh"  ,
    "Tasa de homicidio por cada 10.000 habitantes", "Homicidio"
  )
  
  
  for( i in seq(str))
    for( j in seq(v))
      if ((str[i] ==v[j]) && (j%%2==0))  
        str[i] <- v[j-1]
  str
}



###PARTE DATOS BÁSICOS FUERA UI/SERVER

opc_victimas <- names(base_basica) %>%
  setdiff(c("comuna", "horadec"))
opc_comu <- names(base_comu) %>% 
  setdiff(c("comuna", "año", "superficie", "densidad" ))


###FIN PARTE DATOS BÁSICOS FUERA UI/SERVER
 

# PESTAÑA TIPO DELITO FUERA SERVER #

base <- base_basica
vector_graf <- base$nombre_delito %>%
  unique() %>% 
  vec_cast(character())
l_gr <- vector_graf %>%
  length()
vector_graf[l_gr+1] <- "Seleccionar todos los delitos"



#me quedo sólo con las variables que nos interesan para esta pestaña
base_redu_graf <-base %>% 
  select(diasem, hora24, nombre_delito, edad, estudios, sexo, diames, año, mes)
opc_vars_por_tipo_delito <- setdiff(base_redu_graf %>%
                                      names(),c("nombre_delito")) %>% 
                          nombre_bonito()
# FIN TIPO DELITO FUERA SERVER #


# PESTAÑA MAPA CALOR FUERA DE SERVER #

base_mc<-base_basica %>% select(comuna, nombre_delito, hora24, 
                                diasem, diames, dia365, mes, año, franja)

opciones_mc<-base_mc %>% names()  #Opciones para los dos últimos deslizadores. 
opciones_mc<-opciones_mc[(opciones_mc!="comuna" ) & (opciones_mc!="nombre_delito")]

# FIN PESTAÑA MAPA CALOR FUERA DE SERVER #

# PESTAÑA MATRIZ DE CORRELACIÓN FUERA DE SERVER #

nombres_delito<- base_basica$nombre_delito %>% unique() %>% as.character()
base_comu_min <- base_comu %>% select(-nombres_delito)
base_comu_min <- base_comu_min %>% select(-comuna, -año, -superficie, -poblacion)
#Para los NA
mat_cor <- base_comu_min %>%
  cor(use = "pairwise.complete.obs") %>% 
  round(2) %>%
  as_tibble( rownames = "Variable")
mat_cor <- pivot_longer(mat_cor, cols = 2:length(mat_cor), names_to = "Variable_2") 

names(mat_cor) <- c("Variable_1", "Variable_2", "Valor")

f_cor <- function(n){
  factor <- .85+n/(2*28)
}

# FIN PESTAÑA MATRIZ DE correlacion FUERA DE SERVER #

###PESTAÑA MAPAS SANTIAGO FUERA UI/SERVER ####

mapa <- readRDS("datosmapaMOD")

names(mapa) <- mapa %>% 
  names() %>% 
  nombre_bonito()
#Hace que se muestre mar rapido, pero deforma el mapa.
mapa <-mapa #%>% st_simplify(preserveTopology = TRUE, dTolerance = 0.05)

opciones_mapa_santiago <- setdiff(names(mapa),
                                  c("Comuna", "OBJECTID", "SHAPE_LENG", "SHAPE_AREA", "geometry"))

for (i in 1:length(mapa)){
  if(names(mapa)[i] %>%  is.element(opciones_mapa_santiago))
    mapa[[i]]<- round(mapa[[i]],2)
}

###Fin pestaña mapa santiago fuera UI/SERVER


### FIN PARTE FUERA UI/SERVER ###


ui <- fluidPage(
  theme = shinytheme("sandstone"),
  navbarPage("",
             tabPanel("Presentación",
                      
                      mainPanel( 
                        
                        h1(tags$b("Datos de criminalidad "),
                           align= "center"),
                        h1(tags$b("Región Metropolitana de Santiago"),
                           align= "center"),
                        
                        br(),
                        
                        p("Esta aplicación es una herramienta para la
                          visualización de datos de criminalidad en la",
                          strong("Región Metropolitana de Santiago"), 
                          ", Chile. ",
                          br(),
                          "Tanto como para desarrollar la aplicación como
                          para el trabajo de tratamiendo de datos previo, hemos utilizado el lenguaje de programación ",
                          tags$a(href = "https://www.r-project.org/", "R"),
                          " junto con el entorno de desarrollo integrado ",
                          tags$a(href = "https://rstudio.com/products/rstudio/ ", "RStudio"),
                          " . Además, la aplicación ha sido desarrollada usando el paquete ",
                          tags$a(href = "https://shiny.rstudio.com/", "Shiny"), 
                          ". Tanto los datos como el código que han sido utilizados
                          para crear esta aplicación pueden ser descargados desde ",
                          tags$a(href = "https://drive.google.com/open?id=1cPp1D2r8oKPUymeSMqzK6wGKN0ZFe-93",
                                 "este enlace"), ".",
                          br(),
                          br(),
                          "La aplicación se divide en varias pestañas a las que se puede acceder desde el panel superior.
                          A continuación se describe
                          brevemente cada una de las pestañas."
                        ),
                        
                        br(),
                        
                        h5(tags$b("Pestaña", tags$i("Datos Básicos"),":")), 
                        p("En esta pestaña se hace una descripción básica de los
                          datos con los que hemos trabajado.",
                          "Está formada por dos subpestañas, una para los 
                          datos de las víctimas, y otra para los datos de las comunas"),
                        br(),
                        h5(tags$b("Pestaña", tags$i("Por tipo de delito"),":")),
                        p("En esta pestaña se permite al usuario seleccionar una variable y  uno o varios delitos.
                          Se muestra a continuación una gráfica del número de los delitos seleccionados frente a la variable elegida."),
                        br(),
                        h5(tags$b("Pestaña", tags$i("Mapa de calor"),":")),
                        p("Esta pestaña permite elegir dos variables temporales
                        y un tipo de delito. A continuación
                          cruza las variables temporales y muestra, valiéndose 
                          de un gradiente de color, la distribución
                          temporal de los casos ocurridos del delito elegido. 
                          Deslizando el ratón por encima de cada celda se
                          accede al conteo de casos."),
                        br(),
                        h5(tags$b("Pestaña",
                                  tags$i("Matriz de correlación"),
                                  ":")),
                        p("Permite seleccionar las variables 
                        de datos de las comunas que se desee y crea
                        una matriz de correlación.
                          Cada casilla indica, mediante un gradiente de color el coeficiente de Pearson de las variables respectivas."),
                        br(),
                        h5(tags$b("Pestaña", tags$i("Mapas comunales"), ":")),
                        p("Seleccionada una variable de datos de las comunas, permite ver su distribución espacial en la Región Metropolitana mediante un mapa. 
                          El valor de la variable seleccionada se puede ver desplazando el ratón por encima de ella  o consultando la tabla anexa."),
                        br(),
                        
                        fluidRow(
                          column(1,
                                 br(),
                                 img(src="https://upload.wikimedia.org/wikipedia/en/c/c1/University_of_Valencia_seal.png",
                                     height = 110)
                          ),
                          column(3,
                                 img(src="https://www.uv.es/recursos/fatwirepub/ccurl/853/449/CAP_MATES_vl.png",
                                     height = 170)
                          )
                        ),
                        
                        p(
                          strong("Autor:"),
                          br(),
                          strong("Pascual Esteban Briz")
                        )
                        
                        
                      )
             ),
             navbarMenu("Datos básicos",
                        tabPanel("Víctimas",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       inputId ="var_vict",
                                       label="Seleccione la variable", 
                                       choices = opc_victimas,
                                       selected = opc_victimas[1]
                                     )
                                     
                                   ),
                                   mainPanel(
                                     htmlOutput("t_vict"),
                                     tableOutput("medidas_vict"),
                                     plotlyOutput("gr_db_vict")
                                     
                                   )
                                 )),
                        tabPanel("Comunas",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       inputId ="var_comu",
                                       label="Seleccione la variable", 
                                       choices = opc_comu,
                                       selected = "mujeres"
                                     )
                                     
                                   ),
                                   mainPanel(
                                     
                                     htmlOutput("t_comu"),
                                     tableOutput("medidas_comu"),
                                     plotlyOutput("gr_db_comu")
                                     
                                     
                                   )
                                 ))),
             
             tabPanel("Por tipo de delito",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Seleccione las opciones que desee."),
                          
                          selectInput(
                            inputId ="variable_graf",
                            label="Seleccione la variable", 
                            choices = opc_vars_por_tipo_delito,
                            selected = "Edad"
                          ),
                          
                          selectInput(
                            inputId = "delito_graf" ,
                            label="Seleccione delito", 
                            choices = vector_graf,
                            selected="hurto",
                            multiple = TRUE
                          ),
                          selectInput(
                            inputId = "opc_graf",
                            label = "Seleccione tipo de gráfica",
                            choices = c("Diagrama de barras", "Diagrama de líneas"),
                            selected = "Diagrama de barras"
                          ),
                          actionButton(
                            inputId = "boton_graf",
                            label="Actualice la gráfica con los datos seleccionados"
                          )
                          
                          
                        ),
                        mainPanel( 
                          plotOutput("graf_pestaña_por_delito"),
                          DT::dataTableOutput("base_mostrar")
                        )
                      )),
             
             tabPanel("Mapa de Calor",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Crea un mapa de calor según el número de delitos
               en una determinada comuna a lo largo de una semana"),
                          
                          selectInput(
                            inputId ="comuna_elegida",
                            label="Seleccione comuna", 
                            choices =  c("Todas las comunas", base_mc$comuna %>% levels() ),
                            selected = "NULL"
                          ),
                          
                          selectInput(
                            inputId = "delito_elegido" ,
                            label="Seleccione delito", 
                            choices = c("Todos los delitos", base_mc$nombre_delito %>% levels() ),
                            selected="NULL"
                          ),
                          
                          selectInput(
                            inputId = "varx_mc" ,
                            label="Seleccione variable para el eje horizontal", 
                            choices = opciones_mc,  
                            selected="hora24"
                          ),
                          
                          selectInput(
                            inputId = "vary_mc" ,
                            label="Seleccione variable para el eje vertical", 
                            choices = opciones_mc,  
                            selected="diasem"      
                          ),                       
                          
                          actionButton(
                            inputId = "boton_mapa_calor",
                            label=" Pulse para actualizar el mapa"
                          )
                        ),
                        mainPanel(
                          textOutput(
                            "texto_seleccion") %>% h3(),
                          plotlyOutput("mapa_calor"),
                          textOutput("txt_vars_mc") %>% h3()
                        )
                      )
             ),
             tabPanel( "Matriz de correlación",
                       titlePanel("Matriz de correlación"),
                       sidebarLayout(
                         sidebarPanel(
                           helpText("Crea una matriz de correlación. Permite elegir las variables que nos interesen."),
                           
                           selectInput(
                             inputId ="vars_corr",
                             label="Seleccione las variables", 
                             choices =  c("Todas las variables", base_comu_min %>% names()),
                             selected = "NULL",
                             multiple= TRUE
                           ),
                           
                           actionButton(
                             inputId = "boton_corr",
                             label=" Pulse para actualizar la matriz de correlación"
                           )
                           
                         ),
                         mainPanel( 
                           plotOutput("matriz_corr",  
                                      width = 900,
                                      dblclick = "plot1_dblclick",
                                      brush = brushOpts(
                                        id = "plot1_brush",
                                        resetOnNew = TRUE)
                           ),
                           DT::dataTableOutput("base_p_cor")
                         )
                       )
             ),
             tabPanel("Mapas Comunales",
                      ###UI PESTAÑA MAPAS
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput(
                            inputId = "var_mapa",
                            label = "Elija la variable a mostrarse",
                            choices = opciones_mapa_santiago,
                            selected = (opciones_mapa_santiago)[1] ),
                          actionButton(
                            inputId="boton_mapa",
                            label = "Pulse para actualizar"),
                          helpText ("El mapa puede tardar algunos segundos en aparecer")
                        ),
                        mainPanel(
                          tags$h4("Mapa de la Región Metropolitana de Santiago"),
                          tags$h5("(52 Comunas)"),
                          box(
                            plotlyOutput( "mapa_mostrar"),                            width= 12
                          ),
                          DT::dataTableOutput( "dt_mapa")
                        )
                      )
                      ## FIN UI PESTAÑA MAPAS##
             )
  ))



server <- function(input, output) {
  
  ### PESTAÑA DATOS BÁSICOS ###
  
  
  #Parte víctimas
  
  dbn <- eventReactive(input$var_vict,
                       {
                         for(i in seq(base_basica))
                         {
                           if(names(base_basica)[i] == input$var_vict)
                             n<-i
                         } 
                         print(n)
                         n
                         
                       })
  
  
  
  resumen_vict<-eventReactive( input$var_vict,
                               {
                                 
                                 
                                 if (  is.factor(base_basica[[dbn()]]) ){
                                   
                                   data <- base_basica[[dbn()]]
                                   l <- base_basica[[dbn()]] %>% levels %>% length
                                   
                                   v <- numeric(l)
                                   
                                   f <-  base_basica[[dbn()]] %>% freq()
                                   for(i in 1:l)
                                     v[i]<- f[i,2]
                                   
                                   v <- v %>% as.matrix() %>% t() %>% as_tibble()
                                   names(v)<- levels(base_basica[[dbn()]])
                                 }
                                 
                                 if( is.numeric(base_basica[[dbn()]])){
                                   
                                   data <- base_basica[[dbn()]]
                                   
                                   
                                   v <- numeric(7)
                                   v[1] <- mean(data, na.rm=TRUE)
                                   v[2] <- sd(data, na.rm=TRUE)
                                   v[3:7] <- quantile(data, na.rm=TRUE)
                                   v <- v %>% as.matrix() %>% t() %>% as_tibble()
                                   names(v) <- c("Media", "Desviación Típica", "Mínimo", "Cuartil 25%", "Mediana", "Cuartil 75%", "Máximo")  
                                 }
                                 
                                 
                                 v
                                 
                               })
  
  output$t_vict<- renderUI({
    print("entra en renderUI")
    
    if (  is.factor(base_basica[[dbn()]]) ){
      print(dbn())
      t_cond <- "Esta variable es categórica."
      t_cond2 <- "A continuación se muestra una tabla con la frecuencia de cada categoría. Le acompaña
                   una gráfica de su distribución del número de delitos por categoría."
      
      
    }else if( is.numeric(base_basica[[dbn()]])){
      
      t_cond <- "Esta variable toma valores enteros."
      t_cond2<- " A continuación se muestra una tabla con los datos de la media, desviación típica 
                  y cuantiles de la variable. Le acompaña una gráfica que muestra el número de 
                  delitos para cada valor de la variable"
    }
    
    
    
    t <-paste0("La variable seleccionada es ", strong(nombre_bonito(input$var_vict)), ". ", t_cond, "\n", t_cond2)
    HTML(t) %>% h5()
  })
  
  
  base_vict <- eventReactive(input$var_vict,{
    df <- base_basica %>% 
      group_by(!!as.name(input$var_vict)) %>%
      count()
    
    df
  })
  
  
  output$gr_db_vict <- renderPlotly({
    
    
    print("entra al render")
    g <- base_vict() %>% 
          ggplot(aes(x = !!as.name(input$var_vict), y = n)) +
          geom_col()     +
          ylab("")+
          xlab(nombre_bonito(input$var_vict)) 
 
    
    g <- g %>% ggplotly()
  })
  
  #Parte comunas
  
  resumen_comu<-eventReactive( input$var_comu,
                               {
                                 for(i  in seq(base_comu))
                                   if(names(base_comu)[i] == input$var_comu)
                                     data <- base_comu[[i]]
                                   
                                   
                                   v <- numeric(7)
                                   v[1] <- mean(data, na.rm=TRUE)
                                   v[2] <- sd(data, na.rm=TRUE)
                                   v[3:7] <- quantile(data, na.rm=TRUE)
                                   v <- v %>% as.matrix() %>% t() %>% as_tibble()
                                   names(v) <- c("Media", "Desviación Típica", "Mínimo", "Cuartil 25%", "Mediana", "Cuartil 75%", "Máximo")  
                                   
                                   v
                               })
  
  output$medidas_vict <- renderTable(resumen_vict())
  
  
  
  output$t_comu <- renderUI({
    
    t <-paste0("La variable seleccionada es ", strong(nombre_bonito(input$var_comu)), 
               ". A continuación se muestra una tabla con su media, desviación típica y cuantiles.
               Le acompaña una gráfica donde cada punto representa el valor para una comuna en un año.")
    HTML(t) %>% h4()
  })
  
  output$medidas_comu <- renderTable(resumen_comu())
  
  
  
  output$gr_db_comu <- renderPlotly({
    
    gr <-base_comu %>%
      ggplot(aes(x = año, y = !!as.name(input$var_comu)))+
      geom_point()+
      aes(group = comuna)
    gr %>% ggplotly()
    
  })
  ### FIN PESTAÑA DATOS BÁSICOS ###
 
  
  ### PESTAÑA TIPO DELITO ###
  
  
  base_pestaña_graf<-eventReactive(
    input$boton_graf,
    {
      
      df <- base_redu_graf
      
      names(df) <- df %>% 
        names() %>% 
        nombre_bonito()
      
      if ( !( vector_graf[l_gr+1] %>% is.element(input$delito_graf) )  && (length(input$delito_graf )== 1)){
        df <- df %>% filter( Delito==input$delito_graf)
        df <- df %>% 
          group_by( !!as.name(input$variable_graf ), Delito ) %>%
          count()
        
      }else if( vector_graf[l_gr+1] %>% is.element(input$delito_graf) ){
        df <- df %>% 
          group_by( !!as.name(input$variable_graf), Delito) %>%
          count()
      }else{
        df <- df %>% 
          filter(Delito %in% 
                   input$delito_graf) %>%
          group_by(!!as.name(input$variable_graf), Delito) %>%
          count()
      }
      
      
      
      df
      
    })
  
  graf_por_delito<-eventReactive(
    base_pestaña_graf(),
    { 
      if (!( vector_graf[l_gr+1] %>% is.element(input$delito_graf) ) && (length(input$delito_graf) == 1)  ){
        
        if (input$opc_graf == "Diagrama de barras"){
        gr <-ggplot(data = base_pestaña_graf() , mapping = aes(x = !!as.name(input$variable_graf), y = n)) +
          geom_col()+ggtitle(input$delito_graf %>%
                               paste("por") %>% 
                               paste(input$variable_graf)) +
          theme(plot.title = element_text(hjust = 0.5))+
          labs( y = "Casos ocurridos",
                x = input$variable_graf,
                title = "")
        } else{
          gr <-ggplot(data = base_pestaña_graf() , mapping = aes(x = !!as.name(input$variable_graf),
                                                                 y = n,
                                                                 group = Delito)) +
            geom_line(size = 1)+ggtitle(input$delito_graf %>%
                                 paste("por") %>% 
                                 paste(input$variable_graf)) +
            theme(plot.title = element_text(hjust = 0.5))+
            labs( y = "Casos ocurridos",
                  x = input$variable_graf,
                  title = "")
          
        }
      }
      else 
      { 
        if (input$opc_graf == "Diagrama de barras"){
         gr<-ggplot(data = base_pestaña_graf(),
                     mapping = aes(x= !!as.name(input$variable_graf),
                                   y=n, 
                                   fill= Delito)) +
            guides(fill=guide_legend(title="Delitos seleccionados:"))+
            geom_col( show.legend = TRUE)+
            labs( y = "Casos ocurridos",
                  x = input$variable_graf,
                  title = "")
        }
       else{
        gr<-ggplot(data = base_pestaña_graf(),
                   mapping = aes(x= !!as.name(input$variable_graf),
                                 y=n, 
                                 color = Delito,
                                 group = Delito)) +
          guides(fill=guide_legend(title="Delitos seleccionados:"))+
          geom_line( show.legend = TRUE, size = 1)+
          labs( y = "Casos ocurridos",
                x = input$variable_graf,
                title = "")
       }
      }  
      
      gr
    })
  
  output$graf_pestaña_por_delito <- renderPlot(graf_por_delito())
  
  output$base_mostrar <- DT::renderDataTable(
    base_pestaña_graf(), 
    rownames = FALSE, 
    class = 'cell-border stripe', 
    style = 'bootstrap'
  )
  
  
  ### FIN PESTAÑA TIPO DELITO ###
  
  ### PESTAÑA MAPA DE CALOR ###
  
  texto_mc<-reactiveValues(
    texto1="",
    texto2=""
  )
  
  observeEvent(
    input$boton_mapa_calor ,
    { 
      if ((input$comuna_elegida == "Todas las comunas") && (input$delito_elegido == "Todos los delitos")){
        
        texto_mc$texto1 <- "Se muestra el mapa de calor para todos los delitos y todas las comunas" %>% print()
        
      } else if ( input$comuna_elegida == "Todas las comunas"){
        
        texto_mc$texto1 <- paste( "Se muestra el mapa de calor para todas las comunas y para el delito", input$delito_elegido)
        
      } else if( input$delito_elegido == "Todos los delitos"){
        
        texto_mc$texto1 <- paste( "Se muestra el mapa de calor para la comuna", tolower(input$comuna_elegida), "y para todos los delitos")
        
      } else{
        
        texto_mc$texto1 <- paste("Se muestra el mapa de calor para la comuna", tolower(input$comuna_elegida), "y para el delito", input$delito_elegido, ".")
        
      }
      
      x<- input$varx_mc
      y<- input$vary_mc
      z<-c(x,y)
      
      z<-case_when(
        z== "hora24" ~"Hora del día",
        z== "diasem" ~"Día de la semana",
        z== "diames" ~"Día del mes",
        z== "dia365" ~"Día del año",
        z== "mes" ~"Mes",
        z== "año" ~"Año",
        z== "franja" ~"Franja horaria"
      )
      
      texto_mc$texto2<-paste0("Las variables mostradas son '", z[1], "' en el eje horizontal y '", z[2], "' en el eje vertical.")
    })
  
  output$texto_seleccion <- renderText(texto_mc$texto1)
  output$txt_vars_mc     <- renderText(texto_mc$texto2)
  
  
  ####Construyo la base con la que trabaja el mapa de calor a partir de los inputs elegidos.
  base_elegida<- eventReactive(
    input$boton_mapa_calor, 
    {
      if ( (input$comuna_elegida == "Todas las comunas") && (input$delito_elegido == "Todos los delitos") ){
        
        dfs <- base_mc %>% 
          group_by( !!as.name(input$vary_mc), !!as.name(input$varx_mc) ) %>% 
          count()
      } else if ((input$comuna_elegida != "Todas las comunas") && (input$delito_elegido == "Todos los delitos") ){
        
        dfs <- base_mc %>% 
          filter( comuna == input$comuna_elegida ) %>% 
          group_by(!!as.name(input$vary_mc),!!as.name(input$varx_mc)) %>% 
          count()
      } else {
        
        dfs <- base_mc %>% 
          filter( nombre_delito == input$delito_elegido) 
        
        if ( input$comuna_elegida == "Todas las comunas" ){
          dfs<- dfs %>% 
            group_by(!!as.name(input$vary_mc),!!as.name(input$varx_mc)) %>% 
            count()
        } else{
          
          dfs<- dfs %>% 
            filter( comuna == input$comuna_elegida) %>% 
            group_by(!!as.name(input$vary_mc),!!as.name(input$varx_mc)) %>%
            count()
        }
      }
      dfs 
    })
  
  
  mc<-eventReactive( #Separo en dos partes usando eventReactive para evitar que se muestre un mensaje de error al cambiar de variables.
    base_elegida(),
    {
      
      str1 <-input$varx_mc %>% nombre_bonito()
      str2 <-input$vary_mc %>% nombre_bonito()
      l1 <- paste(sep="", str1, ":")
      l2 <- paste(sep="", str2, ":")
      
      p <- base_elegida() %>% 
        ggplot( aes(x = !!as.name(input$varx_mc), y = !!as.name(input$vary_mc), fill = base_elegida()[[length(base_elegida())]],
                    text = paste(
                      l1, !!as.name(input$varx_mc), "\n",
                      l2, !!as.name(input$vary_mc), "\n",
                      "Casos:", base_elegida()[[length(base_elegida())]],
                      sep = " ")
        )) +
        geom_tile() +
        theme(axis.text.x = element_text(angle = 90,  vjust = 0.6), 
              legend.title = element_blank(),
              legend.position="top",
              legend.direction="horizontal",
              legend.key.width=unit(2, "cm"),
              legend.key.height=unit(0.25, "cm"),
              legend.spacing =unit(-0.5,"cm"),
              panel.spacing=element_blank()) +
        labs(x = "", y = "", title = "") +
        scale_fill_gradient(low = "white", high = "#17146b")+
        labs(fill = "Casos acumulados")
      p %>% 
        ggplotly(tooltip = "text")
    })
  output$mapa_calor<-  renderPlotly({
    print(mc())
  } )
  
  
  ### FIN PESTAÑA MAPA DE CALOR ###
  
  ### PESTAÑA MATRIZ CORRELACIÓN ###
  
  rv_corr<-reactiveValues(num = 0)
  
  #Base pestaña
  base_corr<-eventReactive(
    input$boton_corr,
    
    
    {
      if("Todas las variables" %>% is.element(input$vars_corr)){
        df <- mat_cor
      } else{
        df <- mat_cor %>% filter( Variable_1 %>% is.element(input$vars_corr))
        df <- df      %>% filter( Variable_2 %>% is.element(input$vars_corr))
      }
      
      #Hago que los nombres tengan un mejor aspecto
      df <- df %>% 
        mutate(Variable_1 = Variable_1 %>% nombre_bonito) %>% 
          mutate(Variable_2 = Variable_2 %>% nombre_bonito)
      
      #A continuación hago los nombres de los delitos un poquito más cortos, para que se muestren mejor.
      trozo1 <- "Tasa de "
      trozo2 <- " por cada 10.000 habitantes"
      
      for (i in seq(df$Variable_1)){
        if(df$Variable_1[i] %>% str_detect(trozo1)){
          #df$Variable_1[i] <- df$Variable_1[i] %>% str_replace(trozo1, "")
          df$Variable_1[i] <- df$Variable_1[i] %>% str_replace(trozo2, "")
        }
        if(df$Variable_2[i] %>% str_detect(trozo1)){
          #df$Variable_2[i] <- df$Variable_2[i] %>% str_replace(trozo1, "")
          df$Variable_2[i] <- df$Variable_2[i] %>% str_replace(trozo2, "")
        }
      }
      df
    })
  
  observeEvent(
    input$boton_corr,
    { 
      if("Todas las variables" %>% is.element(input$vars_corr)){
        rv_corr$num <- base_corr()$Variable_1 %>% unique() %>% length()
      } else{
        rv_corr$num <- input$vars_corr %>% length()
      }   
    })
  
  
  rango <- reactiveValues(x = NULL, y = NULL) #Los creo para el zoom
  
  
  output$matriz_corr <- renderPlot(
    ggplot(data = base_corr(), aes(x=Variable_1, y=Variable_2, fill=Valor)) + 
      geom_tile()+
      theme(  axis.text.x = element_text(
        angle = 45,
        vjust = 1,
        size = f_cor(rv_corr$num)*(4 + 52/rv_corr$num),
        hjust = 1),
        axis.text.y = element_text(
          size= f_cor(rv_corr$num)*(4 + 52/rv_corr$num)
        )
      )+
      scale_fill_gradient2( low = "blue", high = "red", mid = "white",  
                            midpoint = 0, limit = c(-1,1), space = "Lab", 
                            name="Coeficiente de\nCorrelación\nde Pearson" )+
      labs( x ="", y = "") +
      coord_fixed(xlim = rango$x, ylim = rango$y, expand = FALSE)
    
  )
  
  #Trozo para que funcione el zoom
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      rango$x <- c(brush$xmin, brush$xmax)
      rango$y <- c(brush$ymin, brush$ymax)
      
    } else {
      rango$x <- NULL
      rango$y <- NULL
    }
  })
  
  #Renderizo la base de datos con la que trabajamos en cada paso para mostrarla junto a la matriz
  
  output$base_p_cor <- DT::renderDataTable( base_corr(), filter="top" )
  
  
  ### FIN MATRIZ CORRELACIÓN ###
  
  
  ###SERVER PESTAÑA MAPAS
  
  base_pestaña_mapas <- eventReactive(
    input$boton_mapa,
    {
      df <- base_comu_gl
      names(df)<- df %>% 
        names %>% 
        nombre_bonito()
      
      df <- df %>% 
        select( Comuna, input$var_mapa) 
      df <- df %>% 
        mutate( Ranking = rank(desc(df[[2]]), ties.method = "min"))
      
      df
    })
  output$dt_mapa <- DT::renderDataTable({
    base_pestaña_mapas() %>%
      datatable() %>% 
      formatRound(2,2)
  })
  
  mapa_ly<- eventReactive(
    input$boton_mapa, 
    {
      gr <- mapa %>% ggplot() +
        geom_sf( aes( fill = !!as.name(input$var_mapa),
                      text= paste0("Comuna: ", Comuna, "\n",
                                   input$var_mapa, ": ", !!as.name(input$var_mapa))
        ))+
        scale_fill_continuous() +
        theme_bw() +
        theme(axis.text.x=element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())+
        scale_fill_viridis_c()
      
      gr <- gr %>%
        ggplotly(tooltip = "text") 
      gr
    })
  
  output$mapa_mostrar <- renderPlotly({
    mapa_ly()
  })
  
  ###FIN SERVER PESTAÑA MAPAS
  
}

shinyApp(ui = ui, server = server) 

