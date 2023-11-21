
library(readr)
library(dplyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)
library(factoextra)
library(shiny)
library(shinyjs)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # ------------------------ DATASET ----
    df <- read.csv("www/credit_data.csv")
    selected_columns <- df %>%
      select("Age", "Occupation", "Income", "Credit.Score") 
  # ------------ INFORMACION GENERAL-----
    
    output$credit_dt <- renderDataTable(
      {head(selected_columns,n = 10)},
      options = list(aLengthMenu = c(5,25,50),
                     iDisplayLength = 5))
    
    output$IG <- renderUI({
      if (input$IGoptions == "Registros y Nulos") {
        tagList(
          strong("Cantidad de registros:"),
          textOutput("IG.NoReg"),
          strong("Cantidad de nulos:"),
          textOutput("IG.NoNull"),
          strong("Porcentaje de nulos:"),
          textOutput("IG.NullPer"),
          br(),
          strong("Todos los nulos se encuentran en la columna 'Occupation'"))
      }else{
        tagList(
          strong("Columnas"),
          verbatimTextOutput("IG.Col")
        )
      }
    })
    
    n.rows <- nrow(df)
    n.nulls <- sum(is.na(df))
    p.nulls <- (n.nulls / n.rows) * 100
    output$IG.NoReg <- renderText(n.rows)
    output$IG.NoNull <- renderText(n.nulls)
    output$IG.NullPer <- renderText({
      paste0(round(p.nulls, 2), "%")
    })
    output$IG.Col <- renderPrint(
      {str(df)}
    )
    shinyjs::onclick("logo",  updateTabsetPanel(session, inputId="navbar", selected="main"))
    
  # ---- ANALISIS UNIVARIADO --------
    output$AU <- renderUI({
      switch(
        input$AUoptions,
        "Distribución de Género" = {
          plotOutput("GDistGen")
        },
        "Distribución de clientes existentes" = {
          plotOutput("GDclient")
        },
        "Distribución de perfil laboral" = {
          plotOutput("GDperfil")
        },
        "Distribución de ingresos" = {
          plotOutput("GDingresos")
        },
        "Distribución de puntuación crediticia" = {
          plotOutput("GDpuntuacion")
        },
        "Distribuciíon del monto de préstamo" = {
          plotOutput("GDmontoPrestamo")
        }
      )
    })
    
    output$AU.Desc <- renderUI({
      switch(
        input$AUoptions,
        "Distribución de Género" = {
          p("Virtualmente la misma cantidad de mujeres (47.57%) y hombres (47.43%) con un pequeño
            porcentaje de género no especificado (4.98%)")
        },
        "Distribución de clientes existentes" = {
          p("La mayoría de los clientes que pidieron un préstamo fueron nuevos 
            clientes",strong("(62.15% vs 37.84%)"))
        },
        "Distribución de perfil laboral" = {
          tagList(
            p(strong("Freelancer:")," 8.08%"),
            p(strong("Salaried:")," 48.61%"),
            p(strong("Self-Employed:")," 30.14%"),
            p(strong("Student:")," 6.61%"),
            p(strong("Unemployed:")," 6.52%")
          )
        },
        "Distribución de ingresos" = {
          tagList(
            strong("Promedio:"),
            textOutput("Ingresos.Mean"),
            strong("Desviación estándar"),
            textOutput("Ingresos.SD"),
            strong("Min:"),
            textOutput("Ingresos.Min"),
            strong("Max:"),
            textOutput("Ingresos.Max")
          )
        },
        "Distribución de puntuación crediticia" = {
          p("Varía de ", strong("300 a 850,"),"lo que es típico para las puntuaciones de crédito. 
            La mediana es de", strong("584,"),"indicando que la mitad de las puntuaciones está 
            por debajo de 584 y la otra mitad por encima.")
        },
        "Distribuciíon del monto de préstamo" = {
          p("Varía entre ", strong("$5,294 y $150,000,"), "con una mediana de ", strong("$111,263,"), "lo que 
            sugiere que la mayoría de los préstamos son de cantidades significativas.")
        }
      )
    })
    
    output$Ingresos.Mean <- renderText(paste0("$ ",round(mean(df$Income),3)))
    output$Ingresos.SD <- renderText(paste0("$ ", round(sd(df$Income),3)))
    output$Ingresos.Min <- renderText(paste0("$ ", min(df$Income)))
    output$Ingresos.Max <- renderText(paste0("$ ", max(df$Income)))
    
  # -----------------------Plots-------------------
    output$GDistGen <- renderPlot({
      ggplot(df, aes(x = Gender)) + 
        geom_bar(fill = "#FF6A39", color = "black") +
        theme_minimal() +
        labs(title = "Distribución de Género", x = "Género", y = "Frecuencia") +
        theme(axis.text.x = element_text(size = 12),  
              axis.text.y = element_text(size = 12))
    })
    
    output$GDclient <- renderPlot({
      ggplot(df, aes(x = Existing.Customer)) + 
        geom_bar(fill = "#FF6A39", color = "black") +
        theme_minimal() +
        labs(title = "Distribución de Clientes Existentes", x = "Cliente Existente", y = "Frecuencia")
    })
    output$GDperfil <- renderPlot({
      ggplot(df, aes(x = Employment.Profile)) + 
        geom_bar(fill = "#FF6A39", color = "black") +
        theme_minimal() +
        labs(title = "Distribución de Perfil Laboral", x = "Perfil Laboral", y = "Frecuencia")
    })
    output$GDingresos <- renderPlot({
      ggplot(df, aes(x = Income)) + 
        geom_histogram(bins = 30, fill = "#FF6A39", color = "black") +
        theme_minimal() +
        labs(title = "Distribución de Ingresos", x = "Ingresos", y = "Frecuencia")
    })
    output$GDpuntuacion <- renderPlot({
      ggplot(df, aes(x = Credit.Score)) + 
        geom_histogram(bins = 30, fill = "#FF6A39", color = "black") +
        theme_minimal() +
        labs(title = "Distribución de la Puntuación de Crédito", x = "Puntuación de Crédito", y = "Frecuencia")
    })
    output$GDmontoPrestamo <- renderPlot({
      ggplot(df, aes(x = Loan.Amount)) + 
        geom_histogram(bins = 30, fill = "#FF6A39", color = "black") +
        theme_minimal() +
        labs(title = "Distribución del Monto del Préstamo", x = "Monto del Préstamo", y = "Frecuencia")
    })
  # --------- RELACION ENTRE C ------
    numeric.df <- select(df, Age, Income, 
                           Credit.Score,Credit.History.Length, 
                           Number.of.Existing.Loans, Loan.Amount, 
                           Loan.Tenure, LTV.Ratio, Profile.Score)  
    

}
