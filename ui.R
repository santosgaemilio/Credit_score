
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
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", href = "styles.css"), tags$script(src="app.js")),
  div(style="padding: 0px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="Análisis Crediticio"
      )
  ),
  navbarPage(id = "navbar", title = div(id = "logo", img(src = "Picture2.png", 
                                                         height = "35px", id = "logo-img")),
             tabPanel("","Main Page",value = "main"),
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
