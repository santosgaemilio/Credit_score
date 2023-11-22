
library(readr)
library(dplyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)
library(factoextra)
library(shiny)
library(shinyjs)


# Define UI for application that draws a histogram
fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"), tags$script(src="app.js"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@100;200;300;400;500&display=swap",
              rel = "stylesheet")
  ),
  useShinyjs(),
  div(style="padding: 0px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="Análisis Crediticio"
      )
  ),
  navbarPage(id = "navbar", title = div(id = "logo", img(src = "Picture2.png", 
                                                         height = "35px", id = "logo-img")),
             tabPanel("",
                      #Cuerpo mainpage
                      tags$div(
                        id = "insertedHTML",
                        HTML('<div id = "main">
        <div id="main-page-title">
            <div id = "main-content">
                <div id = "main-title">
                    <h1>Análisis Crediticio</h1>
                    <img src="main_bedu.png" id = "main-logo">
                </div>
                <p style = "font-weight: 400;
                font-size: larger;">Perfil de Usuarios Óptimos para Créditos</p>
            </div>
        </div>
        <br>
        <div id = "main-page-body">
            <div id = "body-content" class= "row">
                <div class="column">
                    <h2>Elevator pitch</h1>
                    <p style = "font-size: large;">Una plataforma web donde se puede analizar el puntaje crediticio de los usuarios pueden adquirir créditos.</p>
                </div>
                <div class="column" style="padding-top: 0%;">
                    <h2 style="text-align: left;">Oportunidad de mercado</h1>
                    <b><p>Según la Encuesta Nacional de Inclusión Financiera (ENIF):</p></b>
                    <li>En 2021, el 32.7% de las personas de 18 a 70 años tienen al menos un crédito formal, lo que representa 27.4 millones de personas.</li>
                </div>              
            </div>
            
            <div id = "body-sblock" style="color: #ffffff;">
                <div id = "problema-title"> 
                    <h2  id = "problema-text">Problema a solucionar</h2>
                    <img src="cross.png" alt="" id="problema-logo">
                </div>
                
                <p style="font-size: large; padding: 2%;">Para una gran cantidad de usuarios les es difícil definir si cuentan un puntaje atractivo para la autorización de créditos bancarios.</p>
            </div>  
        </div>
    </div>')
                      ),
                      value = "main"),
             tabPanel("Nuestro Dataset",
                      sidebarLayout(
                        sidebarPanel(
                          div(
                            class = "graphs",
                            h3("Resumen"),
                            img(src = "resumen.png", height = "30px",  class = "title-graph")),
                          selectInput("IGoptions","",c("Columnas","Registros y Nulos")),
                          uiOutput("IG")
                        ),
                        mainPanel(
                          h2("Vista preliminar"),
                          br(),
                          dataTableOutput("credit_dt")
                        )
                      )
            ),
             tabPanel("Análisis Univariado", 
                      sidebarLayout(
                        sidebarPanel(
                          div(
                            class = "graphs",
                            h3("Gráficas"),
                            img(src = "graph.png", height = "30px",  class = "title-graph")),                          
                          selectInput("AUoptions","",c("Distribución de Género",
                                                       "Distribución de clientes existentes",
                                                       "Distribución de perfil laboral",
                                                       "Distribución de ingresos",
                                                       "Distribución de puntuación crediticia",
                                                       "Distribuciíon del monto de préstamo")),
                          uiOutput("AU.Desc")
                        ),
                        mainPanel( id = "AU-G",
                          uiOutput("AU")
                        )
                      )
                      ),
              tabPanel("Relación entre columnas",
                       sidebarLayout(
                         sidebarPanel(
                           div(
                             class = "graphs",
                             h3("Gráficas"),
                             img(src = "graph.png", height = "30px",  class = "title-graph")),
                           selectInput("ABoptions","",c("Distribución de Género",
                                                        "Distribución de clientes existentes",
                                                        "Distribución de perfil laboral",
                                                        "Distribución de ingresos",
                                                        "Distribución de puntuación crediticia",
                                                        "Distribuciíon del monto de préstamo")),
                           uiOutput("AB.Desc")
                         ),
                         mainPanel(
                           uiOutput("AB")
                         )
                       )
                       )
            )
)
